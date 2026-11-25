/* =====================================================================
   应用外壳（App Shell）—— 一处管住"把指令送进会话"这件事
   ---------------------------------------------------------------------
   为什么需要一个外壳层：
     这件事以前散在业务代码里：先试任务通道、再等注入通道、都不通就把
     原文摆出来让人自己复制。结果有三个具体毛病 ——
       1. 每条链路各自判断"连上没有"，判据还不一样，一处假阳性就把另一条
          能用的通道整个绕过去（之前任务通道 isConnected() 恒为真，就是它）。
       2. 送失败之后没有后续。人得自己复制粘贴，而指令里带着请求号、
          产物清单、命令行，手抄必错。
       3. 送成功了也不等于做完了。宿主收了消息只说明"发出去了"，
          助手有没有真去做、有没有回写，没人盯。
   这个外壳把三件事一次解决：
     注入（两条通道，判据只在这里）· 入队（失败不丢）· 复查（每小时，
     没回写就再发一次，直到回写或到重试上限）。
   业务代码只调 AppShell.inject(text, meta)，不再自己碰通道。
   ===================================================================== */
(function () {
  'use strict';

  var RETRY_MS = 3600 * 1000;   // 每小时复查一次（与后端 INJECT_RETRY_SECS 对齐）
  var getRetryContext = function () { return null; };
  var retrying = false;
  var MAX_ATTEMPTS = 6;         // 与后端 MAX_INJECT_ATTEMPTS 对齐
  var SEND_WAIT_MS = 8000;      // 单次投递最多等这么久

  var started = false;
  var timer = null;
  var inflightKey = null;
  var inflightCurrent = null;
  var inflight = null;          // 单飞：同一时刻只跑一次投递，避免重复发
  var lastStatus = { pending: 0, due: 0, items: [] };

  /* ---------------- 通道判定：只认真握手 ---------------- */

  // 注入通道（compose）。instance() 由网关 bootstrap 建好，
  // 只有真的嵌在会话里、宿主发过 connect 才会有实例。
  function composeInstance() {
    try {
      var api = window.CatsCoAnnotations;
      if (!api || typeof api.instance !== 'function') return null;
      return api.instance() || null;
    } catch (e) { return null; }
  }

  // 任务通道（cloud-html-artifact 的 bridge）。只认宿主主动握手。
  //   判据必须是自己听到 connect 事件，不能用 isConnected()：
  //   那个实现只要父页说过话就置位，在这套发布方式下恒为真。
  var taskHandshakeAt = 0;
  // 只记前 8 条来自父窗口的消息：origin + type。
  //   用途是回答"宿主到底在不在说话"。一条都没有 = 父页从没跟应用通信；
  //   有消息但 origin 不在白名单 = 消息被 SDK 丢了。两种处置完全不同。
  var parentMsgOrigins = [];
  try {
    window.addEventListener('message', function (ev) {
      try {
        if (ev.source === window.parent && window.parent !== window && parentMsgOrigins.length < 8) {
          parentMsgOrigins.push(String(ev.origin) + ':' + String((ev.data && ev.data.type) || '?'));
        }
      } catch (e) {}
    });
  } catch (e) {}
  try {
    window.addEventListener('message', function (ev) {
      var isParent = false;
      try { isParent = (ev.source === window.parent) && (window.parent !== window); }
      catch (e) { isParent = false; }
      if (!isParent) return;
      var d = ev.data;
      if (d && typeof d === 'object' && d.type === 'catsco.artifact.host.connect.v1') {
        taskHandshakeAt = Date.now();
      }
    });
  } catch (e) {}

  function taskApi() {
    var h = window.catscoArtifactHost;
    if (!h || typeof h.requestTask !== 'function') return null;
    if (!taskHandshakeAt) return null;
    return h;
  }

  function inFrame() {
    try { return window.parent !== window; } catch (e) { return true; }
  }

  /* ---------------- 投递 ---------------- */

  function sendViaCompose(inst, text, meta) {
    if (!stillCurrent(meta)) return Promise.resolve(recordChanged());
    return new Promise(function (resolve) {
      var settled = false;
      var ackTimer = null;
      function finish(r) {
        if (settled) return;
        settled = true;
        if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
        resolve(r);
      }
      try {
        inst.onComposeAck(function (ack) {
          finish(ack && ack.accepted
            ? { ok: true, via: 'compose', code: 'accepted' }
            : { ok: false, via: 'compose',
                code: (ack && ack.reason) || 'unknown',
                why: '平台没收下：' + ((ack && ack.reason) || '未知原因') });
        });
      } catch (e) {}
      var sent = false;
      try { if (!stillCurrent(meta)) { finish(recordChanged()); return; } sent = inst.proposeCompose({ text: text }); } catch (e) { sent = false; }
      if (!sent) {
        finish({ ok: false, via: 'compose', code: 'send_failed', why: '交给平台时没成功。' });
        return;
      }
      // 回执也可能永远不来（通道半断），所以自带超时，不无限等。
      ackTimer = setTimeout(function () {
        finish({ ok: false, via: 'compose', code: 'ack_timeout', why: '12 秒没回执。' });
      }, 12000);
    });
  }

  function sendViaTask(api, text, meta) {
    if (!stillCurrent(meta)) return Promise.resolve(recordChanged());
    var payload = { text: String(text || '') };
    if (meta && meta.mode === 'server') payload.mode = 'server';
    var intent = (meta && meta.intent) || 'assistant.connect.v1';
    return api.requestTask(intent, payload).then(function (t) {
      return { ok: true, via: 'task', code: 'accepted', task: t || null };
    }).catch(function (e) {
      var code = (e && e.code) || 'task_failed';
      return { ok: false, via: 'task', code: code, why: (e && e.message) || '任务通道没收下。' };
    });
  }

  // 一条通道失败之后，第二条还值不值得试。
  //   值：平台没收下、交出去没回执、任务没连上 —— 这些是"这条路堵了"，不是"内容有问题"。
  //   不值：正文是空的（换条路也一样）。
  function worthSecondTry(r) {
    if (!r || r.ok) return false;
    return ['send_failed', 'ack_timeout', 'unknown', 'not_connected',
      'task_failed', 'host_not_connected', 'request_timeout'].indexOf(r.code) >= 0;
  }

  /* 一次投递尝试：两条通道都试，谁通走谁。
     ------------------------------------------------------------------
     原来这里只认第一条能用的通道，一旦它"看起来在"就死等它的回执：
       · compose 实例在、但宿主其实已经断了 → 干等 12 秒，第二条通道一次没试；
       · 两条都不在时，不管真实原因是什么，why 一律写"宿主的注入通道没有建立"，
         于是"页面压根没拿到 SDK""独立标签页""宿主没发握手"三种完全不同的
         情况，在面板上长得一模一样，人只能反复点同一个按钮。
     现在：先试 compose（不需要用户激活，定时复查也能用），
     它失败且属于"这条路堵了"就立刻转任务通道（要新鲜点击，手动点是有的）；
     两条都不成时，如实按真实原因给码和下一步。 */
  function attemptSend(text, meta) {
    if (!stillCurrent(meta)) return Promise.resolve(recordChanged());
    var inst = composeInstance();
    var api = taskApi();

    if (inst) {
      return sendViaCompose(inst, text, meta).then(function (r) {
        if (!stillCurrent(meta)) return recordChanged();
        if (r.ok || !worthSecondTry(r)) return r;
        if (!api) {
          // 注入通道交不出去、任务通道又没连上 —— 这时只说"交给平台时没成功"
          //   等于没说：两条路都断了这件事必须讲出来，人才知道该去修通道。
          return { ok: false, via: 'compose', code: r.code || 'send_failed',
            why: '注入通道交不出去（' + (r.why || r.code || '未知原因')
              + '），任务通道也没连上。' };
        }
        return sendViaTask(api, text, meta).then(function (r2) {
          return r2.ok ? r2 : (r2.why ? r2 : r);
        });
      });
    }
    if (api) return sendViaTask(api, text, meta);

    // 两个都不在：可能是宿主还没握手完，给一小段时间等它。
    return new Promise(function (resolve) {
      var t0 = Date.now();
      (function poll() {
        if (!stillCurrent(meta)) { resolve(recordChanged()); return; }
        var i2 = composeInstance();
        if (i2) { sendViaCompose(i2, text, meta).then(resolve); return; }
        var a2 = taskApi();
        if (a2) { sendViaTask(a2, text, meta).then(resolve); return; }
        if (Date.now() - t0 >= SEND_WAIT_MS) {
          resolve(noChannelWhy());
          return;
        }
        setTimeout(poll, 250);
      })();
    });
  }

  // 两条都不通时，到底是哪一种不通 —— 这三种的处理方式完全不同，不能混着说。
  function noChannelWhy() {
    var hasSdk = !!(window.CatsCoAnnotations && typeof window.CatsCoAnnotations.instance === 'function');
    var hasBridge = !!(window.catscoArtifactHost && typeof window.catscoArtifactHost.requestTask === 'function');
    if (!inFrame()) {
      return { ok: false, code: 'standalone',
        why: '这是独立标签页，没有会话通道。请从会话里打开这个应用再点。' };
    }
    if (!hasSdk && !hasBridge) {
      return { ok: false, code: 'no_sdk',
        why: '页面没拿到平台的会话脚本（网关没注入）。请关掉这个应用面板重开一次。' };
    }
    return { ok: false, code: 'not_connected',
      why: '页面在会话里，但宿主没发握手：注入通道和任务通道都没建立。'
        + '关掉这个应用面板重开一次；还不行就是平台那侧没连上，需要平台侧处理。' };
  }

  /* ---------------- 后端队列 ---------------- */

  // 基路径必须在脚本执行时就定下来。
  //   踩过的坑：写成"调用时读 document.currentScript"，而真正的调用都发生在
  //   页面交互之后 —— 那时 currentScript 已是 null，拼出来的地址会退化成
  //   站点根路径（/api/inject），观感就是"接口 404、队列永远空"。
  var BASE = (function () {
    try {
      var me = document.currentScript && document.currentScript.src;
      if (me) return new URL(me, location.href).pathname.replace(/[^/]*$/, '');
    } catch (e) {}
    var path = location.pathname || '/';
    if (path.charAt(path.length - 1) !== '/') path = path.slice(0, path.lastIndexOf('/') + 1);
    return path;
  })();

  function apiUrl(p) {
    return BASE + String(p == null ? '' : p).replace(/^\/+/, '');
  }

  function postJSON(p, body) {
    return fetch(apiUrl(p), {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body || {})
    }).then(function (r) {
      return r.json().then(function (body) {
        if (!r.ok) {
          var err = new Error((body && (body.error || body.message || body.code)) || ('HTTP ' + r.status));
          err.body = body; err.status = r.status;
          throw err;
        }
        return body;
      });
    });
  }

  function getJSON(p) {
    return fetch(apiUrl(p)).then(function (r) { return r.json(); });
  }

  // Every transport entry goes through this gate, including legacy queue recovery.
  // Queue payloads are untrusted previews: only the server may recover their full text.
  function failure(e, stage) {
    var body = (e && e.body) || e || {};
    return { ok: false, queued: false, code: body.code || 'instruction_prepare_failed',
      error: body.error || body.message || '完整任务文件准备失败。',
      why: body.error || body.why || body.message || '完整任务文件准备失败。',
      stage: body.stage || stage || 'prepare',
      http_status: body.http_status || (e && (e.status || e.http_status)) || 0 };
  }

  function recordChanged() {
    return {ok:false, code:'record_changing', why:'记录已切换，已停止投递。', stage:'record_guard', queued:false};
  }

  function stillCurrent(meta) {
    return !meta || typeof meta.isCurrent !== 'function' || meta.isCurrent();
  }

  function prepareText(text, meta) {
    meta = meta || {};
    var request = { session_id: meta.analysisId || '', req_id: meta.reqId || '',
      text: meta.itemId ? '' : String(text || ''), purpose: meta.purpose || 'execute',
      // 按钮名必须一起送上去：进会话的那段正文由服务端生成，
      //   前端拼的标题只落进 instruction.txt，会话首行看不到。
      origin: meta.originLabel || '' };
    if (meta.itemId) request.item_id = meta.itemId;
    return postJSON('api/instruction/prepare', request).then(function (r) {
      if (!r || !r.ok) return failure(r, 'prepare');
      var absolute = function (p) { return typeof p === 'string' && p.charAt(0) === '/' && !/[\r\n\0]/.test(p); };
      var identities = typeof r.session_id === 'string' && !!r.session_id &&
        typeof r.req_id === 'string' && (request.purpose === 'query' || !!r.req_id);
      // item_id recovery is authoritative: old req_id is NOT a reason to reject it.
      if (!absolute(r.path) || !absolute(r.context_path) ||
          typeof r.message !== 'string' || !r.message.trim() || r.message.length >= 950 ||
          r.message.indexOf(r.path) < 0 || r.message.indexOf(r.context_path) < 0 ||
          (r.instruction_id !== undefined && (typeof r.instruction_id !== 'string' || !r.instruction_id.trim())) ||
          !identities || (!meta.itemId && (r.session_id !== request.session_id || r.req_id !== request.req_id))) {
        return failure({code:'instruction_prepare_invalid', error:'完整任务文件未通过核验。'}, 'prepare_validate');
      }
      return {ok:true, text:r.message, analysisId:r.session_id, reqId:r.req_id, instructionId:r.instruction_id || ''};
    }).catch(function (e) { return failure(e, 'prepare'); });
  }

  function prepareAndSend(text, meta) {
    return prepareText(text, meta).then(function (p) {
      if (!p.ok) return p;
      if (!stillCurrent(meta)) return recordChanged();
      // Keep the routing topic distinct from the analysis record identity.
      var sendMeta = Object.assign({}, meta, {analysisId:p.analysisId, reqId:p.reqId, instructionId:p.instructionId});
      // 先入队，再投递。
      //   为什么顺序要反过来：投递这一步活在浏览器里，回执没回来之前页面一刷新、
      //   面板一关，整个 Promise 就随文档销毁，队列那一步也一并没了 ——
      //   人看到"已交给助手"，会话里却一条都没有，事后查不到任何投递尝试。
      //   指令包在 prepare 阶段已经落在本机，入队是幂等的、不依赖通道；
      //   先把它写进队列，即使这次投递半路夭折，retryDue 每小时还会补发。
      //   补发这条路（meta.itemId 有值）不再入队：那条本来就在队列里，
      //   再写一次只会多出一条重复条目，回写后要人工清两条。
      var preQueue = meta.itemId
        ? Promise.resolve({ ok: true, id: meta.itemId })
        : queueIt(p.text, sendMeta);
      return preQueue.then(function (q) {
        return attemptSend(p.text, sendMeta).then(function (r) {
          r.preparedText = p.text;
          r.preparedMeta = sendMeta;
          r.queuedFirst = !!(q && q.ok);
          r.queueId = (q && q.id) || '';
          return r;
        });
      });
    });
  }

  function retryMeta(it) {
    return {itemId:it.id, session:it.session || '', analysisId:it.analysis_id || it.session_id || '',
      reqId:it.req_id || '', purpose:it.purpose || 'execute', step:it.step || '',
      originLabel:it.origin || ''};
  }

  function queueIt(text, meta) {
    if (!stillCurrent(meta)) return Promise.resolve(recordChanged());
    var payload = {
      action: 'queue',
      text: String(text || ''),
      session: (meta && meta.session) || '',
      analysis_id: (meta && meta.analysisId) || '',
      purpose: (meta && meta.purpose) || 'execute',
      req_id: (meta && meta.reqId) || '',
      step: (meta && meta.step) || '',
      reason: (meta && meta.reason) || '',
      // 按钮名随队列一起存，补发时才有出处可写。
      origin: (meta && meta.originLabel) || ''
    };
    if (meta && meta.instructionId) payload.instruction_id = meta.instructionId;
    return postJSON('api/inject', payload).then(function (r) {
      return { ok: true, id: (r && r.item && r.item.id) || '', retrySecs: (r && r.retry_secs) || 3600 };
    }).catch(function () {
      return { ok: false, id: '', retrySecs: 3600 };
    });
  }

  function reportAttempt(id, ok, code, why) {
    if (!id) return Promise.resolve(null);
    return postJSON('api/inject', { action: 'attempt', id: id, ok: !!ok, code: code || '', why: why || '' })
      .catch(function () { return null; });
  }

  /* ---------------- 通道自检与留痕 ---------------- */

  /* 把"页面这一侧到底看到了什么"报给服务端。
     为什么必须在页面里做：注入失败发生在浏览器内，服务端完全看不见。
     光看"通道没连上"三个字，分不清是没拿到 SDK、独立标签页、还是宿主没握手；
     而父页地址（referrer）只有页面自己知道，它正是判断
     "宿主域在不在白名单"的唯一依据。没有它，只能一直猜。 */
  function channelReport() {
    var inst = composeInstance();
    // 父窗口真实地址：平台的 iframe 常把 Referer 清空（referrerpolicy），
    //   单看 referrer 会永远是空。Chrome 的 location.ancestorOrigins 能直接读出
    //   祖先链上的 origin —— 它是判断"宿主域在不在 SDK 白名单"的唯一可靠来源。
    var ancestors = [];
    try {
      var ao = window.location.ancestorOrigins;
      if (ao) for (var i = 0; i < ao.length; i++) ancestors.push(String(ao[i]));
    } catch (e) {}
    // URL 里有没有平台发下来的绑定票据，是判断"面板到底有没有把这次打开
    //   绑到一个会话上"的直接证据：有 aob_/open_ref 说明平台认了这次打开；
    //   什么都没有，说明它只是把页面塞进 iframe，没打算跟应用对话。
    //   只记参数名和票据前缀，不记完整值。
    var qkeys = [], hasOpenRef = false;
    try {
      var sp = new URLSearchParams(location.search);
      sp.forEach(function (v, k) {
        qkeys.push(k);
        if (/aob_|open_ref|openRef/.test(k + '=' + v)) hasOpenRef = true;
      });
    } catch (e) {}
    return {
      referrer: String(document.referrer || ''),
      ancestors: ancestors.join(' '),
      url_path: String(location.pathname || ''),
      url_query_keys: qkeys.join(','),
      url_has_open_ref: hasOpenRef,
      // 本页在浏览器里的真实 origin。这一项是判"iframe 是不是被隔离"的铁证：
      //   正常跨域框里它是 https://artifact.catsco.cc；
      //   若面板给 iframe 加了 sandbox 却没有 allow-same-origin，它会变成 "null"，
      //   而宿主按精确 origin 发来的握手消息会被浏览器直接丢弃 ——
      //   表现就是"面板说已绑定，应用一条消息都收不到"。
      doc_origin: (function () { try { return String(window.origin || ''); } catch (e) { return 'ERR'; } })(),
      is_secure: !!window.isSecureContext,
      parent_msgs: parentMsgOrigins.join(' '),
      parent_origin: (function () {
        try {
          var r = document.referrer || '';
          return r ? new URL(r).origin : '';
        } catch (e) { return ''; }
      })(),
      in_frame: inFrame(),
      has_sdk: !!(window.CatsCoAnnotations && typeof window.CatsCoAnnotations.instance === 'function'),
      has_instance: !!inst,
      has_bridge: !!(window.catscoArtifactHost && typeof window.catscoArtifactHost.requestTask === 'function'),
      task_handshake: !!taskHandshakeAt,
      ua: String(navigator.userAgent || '').slice(0, 160)
    };
  }

  function reportDiag(code, why) {
    var body = channelReport();
    body.action = 'diag';
    body.code = code || '';
    body.why = why || '';
    return postJSON('api/inject', body).catch(function () { return null; });
  }

  /* ---------------- 对外主入口 ---------------- */

  /**
   * 把一段文本送进当前会话。
   * meta: { session, reqId, step, mode, intent, silent }
   * 返回 { ok, via, code, why, queued, queueId }
   *   ok=true  表示已交给宿主（真正做成还要看回写）
   *   ok=false 且 queued=true 表示已入队，外壳会按小时复查补发
   */
  function inject(text, meta) {
    meta = meta || {};
    var body = String(text || '');
    if (!body.trim()) {
      return Promise.resolve({ ok: false, code: 'empty_text', why: '正文是空的。' });
    }
    // 单飞：并发调用合并成同一次投递，避免同一条指令发两遍。
    var key = JSON.stringify([body, meta.analysisId || '', meta.reqId || '', meta.session || '', meta.purpose || 'execute', meta.step || '', meta.mode || '', meta.intent || '']);
    if (inflight) {
      if (inflightKey === key && stillCurrent(meta) && inflightCurrent === meta.isCurrent) return inflight;
      return Promise.resolve({ok:false, queued:false, code:'send_in_progress', why:'另一条指令正在投递，请稍后重试。'});
    }
    if (!stillCurrent(meta)) return Promise.resolve(recordChanged());
    inflightKey = key;
    inflightCurrent = meta.isCurrent;

    inflight = prepareAndSend(body, meta).then(function (r) {
      if (!r.preparedText) return r; // prepare failure must never queue the original.
      if (!stillCurrent(meta)) return recordChanged();
      meta = r.preparedMeta || meta;
      body = r.preparedText;
      if (r.ok) {
        // 队列已在投递之前写好（见 prepareAndSend），这里只记这次投递的结果。
        //   真正做成的判据仍然是"有没有回写"，所以队列条目保留到回写为止。
        return reportAttempt(r.queueId, true, r.code, '').then(function () {
          refreshStatus();
          return { ok: true, via: r.via, code: r.code, queued: !!r.queuedFirst, queueId: r.queueId };
        });
      }
      // 失败必须留痕，否则下一次还是只能看同一句"没连上"。
      reportDiag(r.code || 'not_connected', r.why || '');
      return reportAttempt(r.queueId, false, r.code, r.why).then(function () {
        refreshStatus();
        // 通道不通不等于这件事没交出去。
        //   指令已经在队列里（投递之前就写好了），助手能取走执行 —— 那是同一件事的另一条路。
        //   所以把"已入信箱"单独标出来，让状态与事实一致。
        return {
          ok: false, via: r.via || '', code: r.code || 'not_connected',
          why: r.why || '', queued: !!r.queuedFirst, queueId: r.queueId,
          relayed: !!r.queuedFirst
        };
      });
    }).then(function (out) {
      inflight = null; inflightKey = null; inflightCurrent = null;
      return out;
    }, function (e) {
      inflight = null; inflightKey = null; inflightCurrent = null;
      return failure(e, 'send');
    });

    return inflight;
  }

  /* ---------------- 每小时复查 + 补发 ---------------- */

  function normalizeTopic(topic) {
    var value = String(topic || '').trim();
    return value.indexOf('grp_') === 0 ? 'cc_group_' + value : value;
  }

  function retryDue() {
    var ctx = getRetryContext();
    if (inflight || retrying || !ctx || !ctx.analysisId || !ctx.session) return Promise.resolve(null);
    retrying = true;
    return getJSON('api/inject/due').then(function (r) {
      if (inflight || !ctx.isCurrent()) return null;
      var it = ((r && r.items) || []).find(function (item) {
        return (item.analysis_id || item.session_id) === ctx.analysisId
          && normalizeTopic(item.session) === normalizeTopic(ctx.session);
      });
      if (!it) return null;
      if (Number(it.attempts || 0) >= MAX_ATTEMPTS) {
        return reportAttempt(it.id, false, 'max_attempts', '已达重试上限，需要人工处理');
      }
      var meta = retryMeta(it);
      meta.isCurrent = function () { return !inflight && ctx.isCurrent(); };
      return prepareAndSend('', meta).then(function (res) {
        if (res.code === 'record_changing') return res;
        return reportAttempt(it.id, res.ok, res.code, res.why).then(function () {
          refreshStatus();
          return res;
        });
      });
    }).catch(function (e) { return failure(e, 'retry_due'); })
      .finally(function () { retrying = false; });
  }

  // 只把队列概况取回来存在内存里；界面上不再有对应按钮和面板。
  function refreshStatus() {
    return getJSON('api/inject').then(function (r) {
      lastStatus = r || lastStatus;
      return lastStatus;
    }).catch(function () { return lastStatus; });
  }


  function start() {
    if (started) return;
    started = true;
    // 打开、刷新和回到页面都只读状态，只有定时检查才会补发。
    timer = setInterval(function () { retryDue(); refreshStatus(); }, RETRY_MS);
    document.addEventListener('visibilitychange', function () {
      if (document.visibilityState === 'visible') refreshStatus();
    });
    window.addEventListener('focus', refreshStatus);
    refreshStatus();
    // 打开就自报一次现场：通道通不通、父页是谁。
    //   为什么不等失败才报：失败时人未必点自检，日志就永远是空的；
    //   而"打开这一刻通道是什么状态"才是判断问题的关键证据。
    setTimeout(function () {
      var r = channelReport();
      reportDiag(r.has_instance ? 'page_load_ok' : 'page_load_no_channel',
        '打开页面时自动上报');
    }, 1500);
  }

  window.AppShell = {
    inject: inject,
    diag: function () { return channelReport(); },
    reportDiag: reportDiag,
    retryDue: retryDue,
    setRetryContext: function (read) { getRetryContext = read; },
    refreshStatus: refreshStatus,
    status: function () { return lastStatus; },
    channels: function () {
      return {
        inFrame: inFrame(),
        compose: !!composeInstance(),
        task: !!taskApi(),
        taskHandshakeAt: taskHandshakeAt
      };
    },
    start: start
  };

  // 外壳自己起来，不依赖业务代码记得初始化。
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
