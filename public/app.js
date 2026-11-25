/* =====================================================================
   个性化分析助手 · 页面逻辑
   后端是本机的一个小服务（serve.R），同源同进程起在本机回环地址上。
   所以：不需要浏览器目录权限、不受 iframe 限制、能真的调 R / Python。
   所有重活都在服务端：列目录、跑命令、收产物。
   ===================================================================== */
'use strict';
const $ = (s, r) => (r || document).querySelector(s);
const $$ = (s, r) => Array.prototype.slice.call((r || document).querySelectorAll(s));
const esc = s => String(s === null || s === undefined ? '' : s)
  .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
  .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
const arr = x => Array.isArray(x) ? x : (x === null || x === undefined ? [] : [x]);
const nowTime = () => new Date().toLocaleTimeString('zh-CN', { hour12: false });

function fmtSize(n) {
  if (n === null || n === undefined || n === '') return '';
  n = Number(n);
  if (isNaN(n)) return '';
  if (n < 1024) return n + ' B';
  if (n < 1048576) return (n / 1024).toFixed(0) + ' KB';
  if (n < 1073741824) return (n / 1048576).toFixed(1) + ' MB';
  return (n / 1073741824).toFixed(2) + ' GB';
}
function toast(msg, kind) {
  let host = $('#toast');
  const el = document.createElement('div');
  el.className = 'toastmsg' + (kind ? ' ' + kind : '');
  el.textContent = msg;
  host.appendChild(el);
  setTimeout(() => el.remove(), kind === 'bad' ? 8000 : 4000);
}

const S = {
  env: null, runDir: null, runDirRemote: false, pipeline: null, gates: null, steps: [], warns: [],
  gateState: {}, gateOpen: {}, dirtyParams: {},
  artifacts: [], envCheck: null, running: false, blocked: false, step: null, code: null,
  browseDir: null, browseParent: null, pollTimer: null,
  history: null, recentDirs: [], scripts: null, openScript: null,
  scriptDraft: null, scriptTab: 'list', incoming: [], queue: [], activeNav: null,
  scriptCheck: null, scriptBackups: null,
  // 「保存并识别」顺带自动注入会话的结果。
  //   常驻显示，不靠 toast —— toast 一闪就没了，而"这次到底有没有交给助手"
  //   是要一直摆在那让人核对的信息。
  autoInject: null, autoInjectBusy: false, autoInjectAt: 0,
  // 「保存并识别」正在执行中，以及上一次开始的时间。
  //   为什么需要两个字段，而不是一个"忙碌"就够：
  //     ① 提交中按钮不禁用 —— 实测同一帧点两下会发两次请求，
  //        第二发的请求号覆盖第一发，前一次的识别结果回来就被判成旧请求，白跑一趟。
  //     ② 点完焦点还留在按钮上 —— 焦点在按钮上时，空格和回车等同于再点一次，
  //        人没碰鼠标、只是翻个页，就会触发一次保存并识别。
  //        实测：空格 1 次、回车 1 次、双击 2 次，全部打到 /api/place/commit。
  //   所以既要进程内锁，也要时间窗：两者挡的不是同一种误触。
  commitBusy: false, commitAt: 0,
  // 本次「保存并识别」的请求号（服务端生成）。
  //   它随注入文本一起交给助手，助手回写时原样带回，
  //   页面据此判断"手上的结果是这一次跑出来的"还是上一轮的旧数据。
  commitReq: null, agentHandoffPath: null,
  // 步骤槽的请求号（本次派给执行端的那条活的号）。
  //   为什么要和 commitReq 分开：以前只有一个号，步骤下发会把它覆盖，
  //   识别结论就被判成"不是你这次的旧数据"，页面永远停在识别中。
  stepReq: null,
  // 勾选要一起跑的环节。用数组而不是只存一个，是因为要按流程顺序批量发。
  picked: [],
  // 多选模式：关着的时候卡片上不显示勾选框，界面和以前一模一样。
  //   为什么默认关：勾选框常驻会让每张卡都多一个控件，日常单点跑反而更费眼。
  //   点「多选」才进入，进去之后才有全选和批量发送。
  pickMode: false,
  // 注释依据卡当前停在哪一页：server / upload / lit。
  refTab: null,
  // 批量运行时的游标：当前跑到勾选列表里的第几个，以及整批的结论。
  //   为什么要有它：四个环节连着发出去，中途任何一个被闸门拦下，
  //   后面的就不能再发了，否则会连着一串 409。
  batch: null,
  // 体检结果的新鲜度：stale=true 表示有结果、但不属于本次请求。
  envStale: false, staleEnv: null,
  // 「保存并识别」之后，识别结论处在哪一步。
  //   为什么要这个状态：服务器模式下识别不由本机执行 ——
  //   本机连不到内网，这一趟必须由助手代跑，结论要等它写回才有。
  //   没有这个状态，页面就会在助手还没动的时候，把"本机连不上服务器"
  //   当成识别结果摆出来（读起来就是"识别不了"），而其实什么都还没开始。
  //   null=还没点过  waiting=已交给助手、等写回  ready=结论已到  timeout=还没回来
  identifyPhase: null,
  // 决策：跑到那一步才生成的问题，以及人已经答过的。
  //   这里放的是"服务端真的生成过的条目"，不是一份预置清单。
  //   没有条目 = 还没走到需要问人的地方，不代表"都定好了"。
  decisions: null,
  // 「保存并识别」的请求号（服务端生成）。
  // 服务器模式下"这一步交给助手"的结果。
  //   为什么要常驻：toast 一闪就没了，而"到底有没有送进会话"是要能核对的；
  //   送失败时还得把指令原文摆出来，让人可以自己复制发一次。
  delegated: null,
  // 助手回写的每步执行状态（服务器模式）。step id → {status, code, log, imported_at}
  remoteSteps: {},
  // 流程进度（阶段 + 环节 + 耗时 + 质检结论），来自 /api/flow。
  flow: null,
  // 处理过程里每个阶段被手动展开(0)/收起(1)过没有；没表态才按"跑着就展开"算
  flowOpen: {},
  flowTimer: null,
  waitTimer: null, waitReq: null, waitTries: 0, waitSince: 0, staleEnvAt: '',
  execTasks: [],
  // ---- 多份分析记录 -------------------------------------------------
  //   这一页不再只有「这一次」。每次分析是一份记录，页面停在其中一份上。
  //   sessionId：现在看的是哪一份（刷新后靠它回到原处）。
  //   collapsed：识别结论里哪几个小节被收起了，按记录存在服务端。
  //   sessions：历史分析列表。
  //   为什么要落服务端而不是 localStorage：换一台电脑、换个浏览器打开，
  //   看到的东西必须一致；localStorage 只在这台浏览器上有。
  sessionId: null, sessions: [], collapsed: {}, readonly: false,
  // ---- 管理员与两个隔离开关 -----------------------------------------
  //   admin：/api/admin/policy 的返回（is_admin + 两个开关）
  //   policy：最近一次读到的开关状态，页面用它说明"现在是什么状态"
  //   为什么页面要缓存它：状态行和只读提示都要用；
  //   每次都现取会让页面在请求间隙闪一下"不知道隔离开没开"。
  admin: null, policy: null,
  // ---- 我是在哪个会话里被打开的 -------------------------------------
  //   应用挂公网子路径后，自己看不出宿主会话。而注入指令要落到对的那个会话，
  //   助手也得知道回写到哪儿 —— 所以启动时向本机后端问一次（后端去问网关）。
  //   topicId 形如 cc_group_grp_5813；拿不到就是 null，此时不猜、如实说没拿到。
  identity: null, topicId: null,
};


// A generation identifies a viewing lifetime, including A -> B -> A.
let recordGeneration = 0;
let sessionChanging = false;
const recordTimers = new Set();
const recordContext = () => recordGeneration;
const recordCurrent = record => record === recordGeneration;
if (window.AppShell && typeof window.AppShell.setRetryContext === 'function') {
  window.AppShell.setRetryContext(() => {
    if (sessionChanging || S.readonly) return null;
    const generation = recordContext();
    const analysisId = S.sessionId;
    const session = S.topicId;
    return {analysisId, session, isCurrent: () => recordCurrent(generation) && !sessionChanging
      && !S.readonly && S.sessionId === analysisId && S.topicId === session};
  });
}
async function recordAwait(promise, __recordScope) {
  try {
    const result = await promise;
    if (!recordCurrent(__recordScope)) throw new Error('记录已切换');
    return result;
  } catch (e) {
    if (!recordCurrent(__recordScope)) throw new Error('记录已切换');
    throw e;
  }
}
function recordInterval(callback, ms) {
  const __recordScope = recordContext();
  const timer = setInterval(() => {
    if (recordCurrent(__recordScope) && !sessionChanging) callback();
  }, ms);
  recordTimers.add(timer);
  return timer;
}
function resetRecordState() {
  stopPolling(); stopWaitWriteback();
  recordTimers.forEach(clearInterval); recordTimers.clear();
  if (collapsedSaveTimer) clearTimeout(collapsedSaveTimer);
  collapsedSaveTimer = null;
  retryStepId = null; busyForce = null; refPickerGo = null; refPickStepId = '';
  lastInvokeLabel = '';
  const keep = new Set(['env', 'identity', 'topicId', 'sessions', 'sessionId']);
  Object.keys(S).forEach(key => { if (!keep.has(key)) delete S[key]; });
  Object.assign(S, {
    runDir:null, runDirRemote:false, pipeline:null, gates:null, steps:[], warns:[],
    gateState:{}, gateOpen:{}, dirtyParams:{}, dirtySteps:{}, artifacts:[],
    envCheck:null, running:false, blocked:false, step:null, code:null,
    history:null, recentDirs:[], scripts:null, openScript:null, scriptDraft:null,
    scriptTab:'list', incoming:[], queue:[], scriptCheck:null, scriptBackups:null,
    autoInject:null, autoInjectBusy:false, autoInjectAt:0, commitReq:null, agentHandoffPath:null, stepReq:null,
    picked:[], pickMode:false, refTab:null, batch:null, envStale:false, staleEnv:null,
    identifyPhase:null, decisions:null, delegated:null, remoteSteps:{}, flow:null,
    flowOpen:{}, flowTimer:null, pollTimer:null, waitTimer:null, waitTries:0,
    waitReq:null, waitSince:0, staleEnvAt:'', execTasks:[], collapsed:{}, identified:false,
    needsRemote:false, references:null, variants:{}, variantsLoaded:null, lastQuery:{},
    place:null, placeDraft:null, lastLogLen:0, readonly:false,
    // 注释依据两个按钮里圈在哪一个：saved=已确定选择，edit=修改选择。
    //   默认 saved：之前定过的依据如果还完整，就不该让人以为要重选。
    annotRefMode:'saved'
  });
  ['monitorTimes','logBox','logHint','delegatedHost','flowHost','stepsHost','artifactsHost'].forEach(id => {
    const el = document.getElementById(id); if (el) { el.textContent = ''; el.innerHTML = ''; }
  });
  ['dlgRetry','dlgRef','dlgArtifacts'].forEach(closeDlg);
}
function currentDelegated() {
  return S.delegated && S.delegated.sessionId === S.sessionId ? S.delegated : null;
}

/* ---------------- 与服务端说话 ---------------- */
/* API 基址：从这个页面自己的位置推出来，不写死。
   为什么必须这样：
     同一个页面可能挂在两种位置 ——
       本机直连：   http://127.0.0.1:18980/            （根路径）
       云端应用：   https://…/<应用身份名>/          （子路径）
     如果写死 fetch('/api/env')，在云端会请求到 /api/env ——
     那是站点根下的路径，不属于这个应用，返回 404，
     页面就显示「连不上本地服务」。而服务其实是好的。

   优先用「本脚本自己的 src」推，因为那最直接反映应用装在哪；
   取不到时退回 location.pathname。 */
const API_BASE = (() => {
  try {
    const me = document.currentScript && document.currentScript.src;
    if (me) return new URL(me, location.href).pathname.replace(/[^/]*$/, '');
  } catch (e) { /* 退回下面 */ }
  let p = location.pathname || '/';
  if (!p.endsWith('/')) p = p.slice(0, p.lastIndexOf('/') + 1);
  return p;
})();
const apiUrl = p => API_BASE + String(p == null ? '' : p).replace(/^\/+/, '');
async function api(path, opts) {
  if (sessionChanging && !/^\/api\/(sessions(?:\/(new|switch|state))?|place)(?:\?|$)/.test(path)) {
    throw new Error('正在切换分析记录');
  }
  const __recordScope = recordContext();
  const recordBound = !(opts && opts.recordBound === false);
  let sid = S.sessionId;
  const explicit = new URL(path, 'http://local').searchParams.get('session');
  if (explicit) sid = explicit;
  else if (opts && opts.body) {
    try { sid = JSON.parse(opts.body).session_id || sid; } catch (e) { /* non-JSON body */ }
  }
  if (recordBound && sid && !/^\/api\/sessions\/(new|switch)(?:\?|$)/.test(path)) {
    const bound = new URL(path, 'http://local');
    if (!bound.searchParams.has('session')) bound.searchParams.set('session', sid);
    path = bound.pathname + bound.search;
    if (opts && opts.body && (!opts.headers || opts.headers['Content-Type'] === 'application/json')) {
      const body = JSON.parse(opts.body);
      if (!body.session_id) body.session_id = sid;
      opts = Object.assign({}, opts, { body: JSON.stringify(body) });
    }
  }
  const url = apiUrl(path);
  let res;
  try {
    res = await fetch(url, opts);
  } catch (e) {
    // 把实际请求的地址带出来。没有这一句，下次再出问题只能看到
    // 「Failed to fetch」，看不出是路径错了还是服务没起。
    throw new Error('请求失败（' + url + '）：' + (e && e.message ? e.message : e));
  }
  const text = await res.text();
  if (!recordCurrent(__recordScope)) throw new Error('记录已切换');
  let body = null;
  try { body = JSON.parse(text); } catch { /* 非 JSON */ }
  if (!res.ok) {
    // 把响应体一起带出来：业务层拒绝（比如"上一步没过质检"）说的是有用的话，
    //   只丢一条 message 出去，调用方就分不清是"被流程拦下"还是"服务坏了"。
    const err = new Error((body && body.error) || text.slice(0, 200) || ('HTTP ' + res.status + ' @ ' + url));
    err.status = res.status;
    err.body = body;
    throw err;
  }
  return body;
}
const getJSON = p => api(p);
const postJSON = (p, obj) => api(p, {
  method: 'POST', headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify(obj || {}),
});

/* ---------------- 词表 ---------------- */
const SRC_LABEL = {
  official: { t: '照官方', c: 'src-official', why: '官方脚本里就是这么写的' },
  instance: { t: '本机适配', c: 'src-instance', why: '为在这台机器做的适配' },
  user: { t: '你定的', c: 'src-user', why: '你亲自指定的' },
  gate: { t: '等你定', c: 'src-gate', why: '要你确认' },
  unspecified: { t: '未标明', c: 'src-unspecified', why: '' },
};
const srcTag = s => {
  const i = SRC_LABEL[s] || SRC_LABEL.unspecified;
  return '<span class="srctag ' + i.c + '" title="' + esc(i.why) + '">' + esc(i.t) + '</span>';
};

const PARAM_LABEL = {
  species: ['物种', '小鼠 / 人 / 大鼠'],
  nfeatures: ['高变基因数', '挑多少基因做降维'],
  dims: ['降维维度', '用前多少个主成分'],
  pcs_doublet: ['去双用的维度', '去双用的主成分数'],
  pN: ['人工双细胞比例', '生成假细胞的比例，官方 0.25'],
  qc_min_features: ['质控下限', '每细胞最少基因数'],
  qc_max_features: ['质控上限', '超过视为异常'],
  qc_mt: ['线粒体上限', '高于视为将死细胞'],
  res1: ['第一次聚类分辨率', '值越大分群越细'],
  res2: ['第二次聚类分辨率', '筛群之后再分一次'],
  seed: ['随机种子', '固定后结果可复现'],
  min_gene_per_type_for_test: ['判定类型的最少基因数', ''],
  rscript_path: ['R 运行程序', '留空就用系统里的'],
  r_lib_paths: ['R 包目录', '多个用逗号分隔'],
  python_path: ['Python 运行程序', '出 PPT 要用'],
  expected_seurat_major: ['Seurat 大版本', '官方用 v4'],
  memory_gb: ['本机内存', 'GB'],
  indir: ['数据目录', '每个样本一个子目录'],
  outdir: ['输出目录', '图、PPT、zip 都放这里'],
  samples_file: ['样本分组表', '两列：样本名 + 分组名'],
  reference_table: ['文献 marker 表', '判定细胞类型的依据'],
  lit_dict: ['marker 词典', '与文献表二选一'],
};
const plabel = k => PARAM_LABEL[k] || [k, ''];

/* 产物清单的环节顺序与中文名。
   ---------------------------------------------------------------------
   为什么前端自己存一份：接口给的 step_title 里 11.report 的 zip 写作
   「打包交付物」，而产物面板要按流程显示「12.打包产物」；出图与富集共用
   10.figs，也得靠目录再分一次。这份表只管显示顺序与标题，不改任何数据。
   顺序与 /api/flow 的 stages 一致，序号与步骤卡片上的编号对得上。 */
const ART_STAGES = [
  ['doublet', '逐样本去双与质控'], ['merge', '合并与批次校正'],
  ['markers', '一次聚类 marker'], ['filter', '筛群与剔样本后重聚类'],
  ['final', '终版比例统计'],
  ['annotate', '细胞类型注释'],
  ['figs', '出图'], ['enrich', 'GO 富集'],
  ['extract', '提取报告关键指标'],
  ['ppt', '生成交付 PPT'], ['pack', '打包产物']
];

const STEP_ICON = {
  env: '①', decide: '②', precheck: '③', pipeline1: '④',
  pipeline2: '⑤', plots: '⑥',
  // ppt / pack 曾经也在这张表里写着 ⑦ ⑧。
  //   那是服务器档只有 8 步时的编号。现在的流程有 12 步，
  //   这两步排在第 11、12 位，用写死的字符就会和前面按序号算出来的
  //   1..10 撞号：页面末尾出现 ⑦⑧，读起来像只有 8 步。
  //   编号一律按实际顺序算（见渲染处 i + 1），表里只留流程之外的那几步。
};

/* =====================================================================
   启动
   ---------------------------------------------------------------------
   首屏（2026-11-21 改）：
   实测打开应用要停 5-6 秒「正在连接本地服务…」，期间主体全白。
   根因：boot() 先等 /api/env，再串行等 /api/place、/api/whoami、
   /api/sessions/state —— 每个接口在服务端都要过一次网关身份查询（超时 4 秒）；
   而 #main 要等 /api/env 回来才 display:block，状态行文案又只有 renderHead()
   会替换，renderHead 要等 boot() 全走完。
   所以这里做三件事：进来先画一屏；每推进一步就把状态行改成真实的那一步；
   哪一步失败就写清是哪一步、什么原因，不再用「连接失败」一句话糊过去。
   ===================================================================== */
const BOOT_STEP_CN = {
  connect: '连接本机服务',
  identity: '校验身份',
  place: '读取运行位置',
  records: '读取分析记录',
  run: '读取运行目录',
};

// 首屏失败停在哪个环节。renderHeadSubline() 会一直把它带出来 ——
//   否则 renderHead 一跑，失败原因就被「分析名称：X」盖掉，人再也看不到是哪儿出的问题。
//   后续真的把运行目录读回来时由 loadRun() 清掉。
let bootFail = null;

const bootReason = err => {
  const t = String((err && err.message) || err || '未知原因').replace(/\s+/g, ' ').trim();
  return t.length > 80 ? (t.slice(0, 80) + '…') : t;
};

/* 进度：正在做哪一步。 */
function setBootStatus(key) {
  const sub = $('#subline');
  if (sub) sub.textContent = '正在' + (BOOT_STEP_CN[key] || key) + '…';
}

/* 失败：哪一步、什么原因。 */
function setBootFail(key, err) {
  bootFail = (BOOT_STEP_CN[key] || key) + '失败：' + bootReason(err);
  const sub = $('#subline');
  if (sub) sub.textContent = bootFail;
}

/* 进来先画一屏：顶栏本来就是静态 DOM，这里只把几处空着的位置填上，
   不新造一套骨架 HTML。主体先给一行占位，等各块真正渲染时被覆盖
   （renderPlace() 会重写 #placeHost）。 */
function bootSkeleton() {
  bootFail = null;
  const main = $('#main');
  if (main) main.style.display = 'block';
  setBootStatus('connect');
  const tally = $('#tally');
  if (tally) tally.innerHTML = '<span class="pill plain">正在加载</span>';
  const hint = $('#whoamiHint');
  if (hint) hint.textContent = '正在确认身份…';
  const ph = $('#placeHost');
  if (ph && !ph.innerHTML.trim()) {
    ph.innerHTML = '<div class="card card-pad"><div class="small">正在连接本机服务，读取运行环境…</div></div>';
  }
}

/* 没有运行目录时的主体：先把外壳画出来，再明说缺什么、去哪补。
   这里刻意不调 renderRun() 整块重画 —— 那一趟会连带调 renderFlow、
   renderVarPanels 等一串依赖运行目录的东西，没有目录时它们只会输出
   "进度读取失败"这类吓人的话，而且会把别处的状态一起改写。
   只改这几处最直接的地方，其余保持原样。 */
function renderNoRunDir() {
  const rs = $('#runState');
  if (rs) { rs.className = 'runstate'; rs.textContent = '还没定运行目录'; }
  const hint = $('#runHint');
  if (hint) hint.textContent = '未设运行目录：先在「运行环境配置」里保存并识别，再回来跑。';
  const flow = $('#flowHost');
  if (flow) flow.textContent = '未设运行目录，还没有进度可读。';
  const steps = $('#stepsHost');
  if (steps && !arr(S.steps).length) {
    steps.innerHTML = '<div class="card card-pad"><b>未设运行目录</b>'
      + '<div class="small" style="margin-top:6px">这份分析还没定结果写到哪。'
      + '先在「运行环境配置」里保存并识别，再回来跑。</div></div>';
  }
  // 一键全流程没有目录时不能点：这一条 renderRun 里也有，但那条路这里没走。
  const runAll = $('#btnRunAll');
  if (runAll) runAll.disabled = true;
}

async function boot() {
  const __recordScope = recordContext();
  // 首屏这几步里第一步失败的原因要留住：后面的成功不该把它抹掉。
  //   只在全部走通时才清空，否则「校验身份失败」会被后来的「就绪」盖成正常。
  let firstFail = null;
  const fail = (key, err) => {
    if (!firstFail) firstFail = true;
    setBootFail(key, err);
  };
  try {

  // 任何 await 之前先画一屏，别让人对着白屏等。
  bootSkeleton();

  setBootStatus('connect');
  try {
    S.env = await recordAwait(getJSON('/api/env'), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    $('#bootErr').style.display = 'block';
    $('#bootErrMsg').textContent = String(e.message || e);
    fail('connect', e);
    // 占位还写着"正在连接"，会跟下面的报错自相矛盾，一并收掉。
    const ph = $('#placeHost');
    if (ph) ph.innerHTML = '<div class="card card-pad"><div class="small">本机服务没连上，原因见上方提示。</div></div>';
    const tl = $('#tally');
    if (tl) tl.innerHTML = '<span class="pill err">没连上</span>';
    return;
  }
  bindEvents();
  // 不再有「欢迎页 / 主界面」两态。
  //   以前没选目录就先给一页引导，选完才进主界面；现在第一步本身就
  //   是「选机器 + 选路径」，所以直接进主界面，由第一块负责这件事。
  $('#main').style.display = 'block';
  // 顺序保持原样（先 place 再 identity）：这两个函数自己就把网络错误收进了
  //   返回值、不抛异常，改成并行只会多一个失败点，换不来时间 ——
  //   本机服务是单进程事件循环，两个请求并行到服务端照样排队。
  setBootStatus('place');
  try {
    await recordAwait(loadPlace(), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    fail('place', e);
  }
  // loadPlace 读失败不抛，只把原因放在 S.place.ok === false 里，这里照样说出来。
  if (S.place && S.place.ok === false) fail('place', S.place.error || '读不到运行位置设置');
  setBootStatus('identity');
  try {
    // 先认清"我在哪个会话里"。拿不到也不阻塞页面，只是不显示会话号。
    await recordAwait(loadIdentity(), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    fail('identity', e);
  }
  // 身份读不到不上报成失败：本机模式、未登录时 /api/whoami 本来就会回 ok=false，
  //   那是正常形态（renderIdentity 会写「未登录 / 无凭据」），
  //   把它算成启动失败会让人以为页面坏了。
  // 先恢复「我在哪一份分析里」。
  //   这一条是需求的根：刷新之后页面必须停在原处，而不是回到空白。
  //   所以顺序是：拿到服务端的记录 -> 决定看哪一份 -> 再按它设置各项状态。
  setBootStatus('records');
  try {
    await recordAwait(loadSessionsState(), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    fail('records', e);
  }
  // 记录读失败时 loadSessionsState 不抛，只把原因记在 sessionsError 里，这里照样说出来。
  if (S.sessionsError) fail('records', S.sessionsError);
  // 前面都过了才清掉失败标记；任何一步没过，状态行就一直停在那个原因上。
  if (!firstFail) bootFail = null;
  // 就绪：状态行换成「分析名称：X」，格式与原来完全一致（同一段代码）。
  renderHeadSubline();
  setStageVisible(S.identified);
  renderPlace();
  renderSessionsBar();
  renderSidenav();
  renderJargon();
  await recordAwait(renderIdentify(), __recordScope);
  // 有识别结果就把闸门与运行区也画出来，让整页回到上次的样子。
  //   以前这里无条件清空，于是刷新后「需要你决定的事」和「跑分析」全消失。
  // 没有可用的运行目录（本来就没有，或者读失败）时，下面补一次主体说明。
  let needNoRunChrome = false;
  if (S.runDir) {
    setBootStatus('run');
    try { await recordAwait(loadRun(), __recordScope); } catch (e) {
      if (!recordCurrent(__recordScope)) return;
      fail('run', e);
      needNoRunChrome = true;
    }
  } else {
    needNoRunChrome = true;
  }

  switchTab('cur');
  // 补说明必须放在 switchTab 之后：switchTab 会走 renderAll -> renderRun，
  //   把提示改回「未设运行目录」一句话、把步骤格子清空。先写后写顺序反了，
  //   这段说明就白写了。
  if (needNoRunChrome) { renderHead(); renderSessionsBar(); renderNoRunDir(); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 位置没确认时，隐藏「需要你决定的事」「跑分析」。
   这两块都要求先知道数据在哪，否则摆出来只是噪音。 */
function setStageVisible(on) {
  ['secGates', 'secRun'].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = on && curTab === 'cur' ? 'block' : 'none';
  });
  const monitor = $('#secMonitor');
  if (monitor) monitor.style.display = S.sessionId && curTab === 'cur' ? 'block' : 'none';
  if (S.sessionId) startPolling(); else stopPolling();
}

function bindEvents() {
  const bh = $('#btnHistory'); if (bh) bh.addEventListener('click', () => { renderHistory(); openDlg('dlgHistory'); });
  const bn = $('#btnNewSession'); if (bn) bn.addEventListener('click', () => newSession());
  // 产物下载回到顶栏：她说那个按钮原来在上方，被删掉后页面里还有一句
  //   「在产物下载里看全部」，指不到地方。现在放在「新建分析」左边。
  const ba = $('#btnArtifacts'); if (ba) ba.addEventListener('click', () => { refreshArtifacts(); openDlg('dlgArtifacts'); });
  // 权限管理：只有应用所属机器人的主人看得到这个按钮。
  //   按钮本身不是权限边界 —— 真正拦人的是后端 /api/admin/policy 的 owner 判定；
  //   这里隐藏只是不给普通用户一个点开就被拒的入口。
  const badm = $('#btnAdmin'); if (badm) badm.addEventListener('click', openAdminDialog);
  // 顶部三个页签：当前分析 / 历史分析 / 脚本数据库。
  //   三页互斥显示，点中的那个亮起来；不再用弹窗，也不用"返回"按钮。
  $$('#tabs .tab').forEach(tb => {
    tb.addEventListener('click', () => switchTab(tb.getAttribute('data-tab')));
  });
  const br = $('#btnRefreshArt'); if (br) br.addEventListener('click', refreshArtifacts);
  // 全部取回：把清单里所有「能在应用里看」的产物一次性登记给执行端。
  //   逐个点太慢，而且人不知道哪些值得取 —— 这个按钮就是"把能看的都取回来"。
  const bfa = $('#btnFetchAllArt'); if (bfa) bfa.addEventListener('click', fetchAllArtifacts);
  // 生成交付 PPT：素材已在本机时直接生成，不等服务器。
  //   注意不要绑 btnBuildPpt —— 那个现在是产物面板的页签按钮，
  //   点它是切面板，不是生成 PPT。生成走 btnBuildPptGen（只在本地模式出现）。
  const bpp = $('#btnBuildPptGen'); if (bpp) bpp.addEventListener('click', buildPpt);
  // 打包交付物：图已取回本机、PPT 也已在本机生成时，打包不必再绕一趟服务器。
  //   流程定义里 pack 是「服务器上跑」的环节，服务器模式下要点到最后一环、
  //   还要等回写；而这件事本质是本地压个 zip，绕过去只多一次等待和一个失败点。
  const bpk = $('#btnBuildPack'); if (bpk) bpk.addEventListener('click', buildPack);
  const ra = $('#btnRunAll'); if (ra) ra.addEventListener('click', () => runAllWithOverwriteConfirm());
  // 勾选的环节按流程顺序一起跑。以前只能一环一环点：
  //   跑到第 6 环停下，剩下 4 环要点四次、等四轮。
  //   这里做的是「按顺序串行发」而不是并发 —— 同一条链上并发写产物会互相覆盖。
  const rsel = $('#btnRunSel'); if (rsel) rsel.addEventListener('click', runPickedSteps);
  // 多选开关：点一下才让卡片上出现勾选框。
  //   默认关着是有意的 —— 平时单点跑一环，多一个控件反而碍眼；
  //   要批量的时候再进这个模式，界面才变成"选几个、一起跑"。
  const bpm = $('#btnPickMode'); if (bpm) bpm.addEventListener('click', togglePickMode);
  const bpa = $('#btnPickAll'); if (bpa) bpa.addEventListener('click', togglePickAll);
  const bss = $('#btnStop'); if (bss) bss.addEventListener('click', stopRun);
  const du = $('#dlgUp'); if (du) du.addEventListener('click', () => { if (S.browseParent) browseTo(S.browseParent); });
  const duse = $('#dlgUse'); if (duse) duse.addEventListener('click', () => { if (S.browseDir) chooseDir(S.browseDir); });
}

/* =====================================================================
   多份分析记录
   ---------------------------------------------------------------------
   一次分析 = 一份记录。页面停在其中一份上，刷新不动它。
   三个入口各管一件事，不要互相兼职：
     新建分析        开一份空白记录（同名复制设置，不复制结论）
     历史分析        列出所有记录，点一份就把整页切过去
     存档与步骤回滚   对当前这一份做存档 / 退回 / 标记从某步重跑
   ===================================================================== */

/* 读回「我在哪一份分析里」。这是刷新后能回到原处的唯一依据。 */
async function loadSessionsState(targetId = null, bindCurrent = true) {
  const __recordScope = recordContext();
  try {

  let d = null;
  const selectedId = targetId || (bindCurrent ? S.sessionId : null);
  const statePath = selectedId
    ? '/api/sessions/state?id=' + encodeURIComponent(String(selectedId))
    : '/api/sessions/state';
  try { d = await recordAwait(api(statePath, {recordBound: false}), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
  }
  if (!d || !d.ok) {
    S.recordStateError = selectedId ? '暂时无法读取这份分析的识别结论，请刷新重试。' : '';
    // No current selection must not hide the list. A failed history read must
    // be distinguished from a genuinely empty history, without borrowing state.
    try {
      const listing = await recordAwait(api('/api/sessions', {recordBound:false}), __recordScope);
      if (!listing || !listing.ok || listing.identified === false)
        throw new Error((listing && listing.error) || '历史记录暂时无法读取');
      S.sessions = arr(listing.sessions);
      S.sessionsError = '';
    } catch (e) {
      if (!recordCurrent(__recordScope)) return;
      S.sessionsError = String(e.message || e);
    }
    renderIdentity();
    return;
  }
  S.sessionsError = '';
  S.recordStateError = '';
  S.sessions = arr(d.sessions);
  S.sessionId = d.viewing_session_id || (d.session && d.session.id) || selectedId || d.current || null;
  const s = d.session || null;
  S.readonly = !!(s && s.can_write === false);
  S.collapsed = (s && s.collapsed) || {};

  // 记录里存的识别结果 = 这一份当时识别出来的东西。
  //   为什么不用 /api/envcheck：那个回答的是"最近一次识别是什么"，
  //   在多份记录并存之后就是错的 —— 打开一份旧记录会看到新记录的数字。
  const ident = s && s.identify;
  // 两个槽位分开读：pending_req 是识别在等的号，pending_step_req 是步骤在等的号。
  //   老服务端没有新字段时回退到 pending_req，语义与旧版一致。
  const pending = d.pending_identify_req !== undefined ? d.pending_identify_req : (d.pending_req || null);
  S.stepReq = d.pending_step_req || null;
  // ---- 识别状态一律以服务端结论为准 --------------------------------
  //   为什么不能在这里自己算（2026-11-22，gina 报「会话 1 已回写、会话 2 仍显示识别中」）：
  //     原来判据是「记录里的识别号 == 我手上这个页面的请求号」。
  //     手上那个号只属于点过按钮的那一个标签页，换会话打开同一条记录时手上没有号，
  //     同一个问题就被答成两种结论 —— 记录里明明有回写，另一个会话却显示"识别中"。
  //   服务端现在只按记录本身给出四种结论（ready/waiting/stale/none），
  //   任何会话看到的都一样。这里只负责把它翻译成页面状态，不再自行推断。
  const identStatus = String(d.identify_status || '');
  const ssWaiting = (identStatus === 'waiting');
  const ssStale = (identStatus === 'stale');
  const waiting = ssWaiting;
  S.identifyStatus = identStatus || null;
  S.identifyStaleHours = d.identify_stale_hours || null;
  S.identifyResultAt = d.identify_result_at || null;
  if (ident && ident.result && !waiting) {
    S.envCheck = ident.result;
    S.identified = true;
    // 「这一份是不是服务器模式」以记录自己记下的为准（ident.mode），
    //   不看 placeDraft —— 那要等下面几行才灌进来，顺序反了就会判成本机，
    //   于是「自动判定」被画成"还没跑"，而服务器模式下它本来就随识别一起回来。
    S.needsRemote = (ident.mode === 'server')
      || ((s && s.mode) === 'server');
    S.commitReq = ident.req_id || null;
    S.identifyPhase = 'ready';
    S.envStale = false;
  } else {
    // 这一份还没识别过：不要拿别的记录的结论填进来。
    S.envCheck = null;
    S.identified = false;
    S.identifyPhase = null;
    S.commitReq = null;
    S.needsRemote = !!(s && s.mode === 'server');
  }
  if (s && s.place) {
    S.placeDraft = placeDraftFromServer(s.place);
  }
  if (s && s.run_dir) {
    S.runDir = s.run_dir;
    S.runDirRemote = (s.mode === 'server');
  }
  // 有未回的识别请求：把等待恢复起来，让页面继续自己等。
  //   超期的那种（服务端判 stale，请求挂着 24 小时仍没回来）不走"一直在等"：
  //   页面继续显示"识别中"只会让人以为它还在跑。改成 timeout，说清等的是哪个号、
  //   等了多久，并把重跑入口摆出来。
  if (waiting) {
    S.commitReq = pending;
    S.identified = true;
    S.needsRemote = (s && s.mode === 'server') || (ident && ident.mode === 'server');
    S.identifyPhase = 'waiting';
    startWaitWriteback();
  } else if (ssStale) {
    S.identified = true;
    S.needsRemote = (s && s.mode === 'server') || (ident && ident.mode === 'server');
    S.identifyPhase = 'timeout';
    stopWaitWriteback();
  }
  // 会话切换后不能沿用上一份记录的识别号。
  //   这条号只在"点过保存并识别的那一次"里有意义；换到另一份记录还留着它，
  //   后面 fetchEnvcheck 就会拿别的记录的号去比新鲜度，结论永远是"不是这次的"。
  if (!waiting && !ssStale && identStatus === 'ready') {
    S.commitReq = d.identify_result_req || (ident && ident.req_id) || null;
  }

  // 顶部状态行在这里重画一次。
  //   为什么必须放在这里而不是只放在初始化里：状态行的名字取自 S.sessions
  //   与 S.sessionId，而这两样正是本函数刚写完的。初始化时 loadIdentity 先跑、
  //   loadSessionsState 后跑，于是状态行渲染时列表还是空的 ——
  //   结果就是新建或切记录之后，标题一直停在「分析记录：未选择」。
  //   本函数是新建、切换、刷新三条路的共同收口，放这里一次覆盖三条。
  renderIdentity();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 当前这份记录的一行摘要。刷新后它还在，用户一眼知道自己在哪。 */
function renderSessionsBar() {
  const host = $('#sessbar'); if (!host) return;
  const cur = S.sessions.find(s => s.id === S.sessionId) || null;
  if (!cur) {
    host.innerHTML = '<span class="sessempty">还没有分析记录。</span>';
    return;
  }
  let badge = '';
  if (cur.has_identify) {
    const v = String(cur.verdict || '');
    const cls = v === 'ready' ? 'ok' : (v === 'blocked' ? 'bad' : 'warn');
    const txt = cur.verdict_cn || (v === 'ready' ? '可以跑' : (v === 'blocked' ? '跑不了' : '有提醒'));
    badge = '<span class="idbadge ' + cls + '">' + esc(txt) + '</span>';
  } else {
    badge = '<span class="idbadge warn">还没识别</span>';
  }
  host.innerHTML = '<span class="sesscur">' + (S.readonly ? '只读查看：' : '当前分析：') + '<b>' + esc(cur.name || cur.id) + '</b></span>'
    + badge
    + (cur.run_dir ? '<span class="sesspath">' + esc(cur.run_dir) + '</span>' : '')
    + '<span class="small">' + esc(String(cur.created_at || '').replace('T', ' '))
    + (cur.n_archives ? '　存档 ' + cur.n_archives + ' 份' : '') + '</span>'
    + '<span class="spacer"></span>'
    + (S.readonly
      ? '<span class="small">' + (isolationOff('write')
          ? '别人也能改（管理员已关修改隔离）'
          : '只读：只能看，不能改。要协作请让管理员关掉修改隔离') + '</span>'
      : '<button class="btn" data-sess-list="1">切换</button>');
}

/* 隔离开关的当前值。拿不到就按"开着"算 —— 从严那一侧，
   免得因为一次读失败在页面上显示成"谁都能改"。 */
function isolationOff(which) {
  const p = S.policy;
  if (!p) return false;
  if (which === 'read') return p.record_isolation === false;
  return p.write_isolation === false;
}

/* 新建分析：设置从「来源记录」继承，结果目录可以在这里改。
   为什么不再照抄全局设置：全局设置只是这台机器上一次保存的样子，
   它可能来自另一份记录 —— 包括不属于当前账号的记录，只要页面切过去看过。
   照抄等于把那份记录的路径搬进新记录。所以这里显式带两个东西：
   source_session_id（当前正在看的这份记录，服务端当场校验归属）
   和 run_dir（这次要用的独立结果目录）。
   取消对话框就不新建：以前 prompt 返回 null 会被当成空串，等于点了取消也建。 */
async function newSession() {
  if (sessionChanging) return;

  let name = null;
  try { name = prompt('这次分析叫什么？', ''); } catch (e) { name = null; }
  if (name === null) { toast('已取消，没有新建。', 'info'); return; }
  name = String(name).trim();

  const cur = S.place || {};
  const curRd = (cur.server && cur.server.work_root) || (cur.local && cur.local.path) || '';
  let rd = null;
  try { rd = prompt('这次分析的结果目录（服务器上的独立目录）', curRd); } catch (e) { rd = null; }
  if (rd === null) { toast('已取消，没有新建。', 'info'); return; }
  rd = String(rd).trim();

  sessionChanging = true;
  recordGeneration += 1;
  recordTimers.forEach(clearInterval); recordTimers.clear();
  stopPolling(); stopWaitWriteback();
  const __recordScope = recordContext();
  try {

  try {
    const body = { name: name, run_dir: rd };
    if (S.sessionId) body.source_session_id = S.sessionId;
    const r = await recordAwait(postJSON('/api/sessions/new', body), __recordScope);
    if (!r || !r.ok) { sessionChanging = false; toast('新建失败：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    // 先把界面上属于「上一次」的东西清掉，再去读新记录。
    //   不清的话，新记录的第一次渲染会短暂显示上一份的结论。
    resetRecordState();
    S.sessionId = (r.session && r.session.id) || r.id || null;
    await recordAwait(loadPlace(), __recordScope);
    S.collapsed = {};
    stopWaitWriteback();
    await recordAwait(loadSessionsState(), __recordScope);
    sessionChanging = false;
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    toast('已新建：' + ((r.session && r.session.name) || '') + '。填好设置后点「保存并识别」。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 sessionChanging = false; toast('新建失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 历史分析：每份记录一行，点一行就把整页切过去。 */
function renderSessions() {
  const host = $('#sessionsHost'), note = $('#sessNote');
  if (!host) return;
  const list = arr(S.sessions);
  if (note) note.textContent = list.length ? ('共 ' + list.length + ' 份') : '';
  const warning = S.sessionsError
    ? '<div class="small" role="alert">历史记录加载失败：' + esc(S.sessionsError)
      + ' 请稍后刷新；这不表示分析记录已被删除。</div>' : '';
  if (!list.length) {
    host.innerHTML = warning || '<div class="small">还没有分析记录。</div>';
    return;
  }
  let h = warning + '<table class="libtbl"><thead><tr>'
    + '<th>分析</th><th>创建</th><th>跑在哪</th><th>识别结论</th><th>存档</th><th>运行摘要</th><th>操作</th>'
    + '</tr></thead><tbody>';
  list.forEach(s => {
    const cur = s.id === S.sessionId;
    const readonly = s.readonly === true || s.can_write === false;
    const runSummary = window.runSummaries && window.runSummaries.get(String(s.id));
    const whereFull = s.mode === 'server'
      ? ('服务器 ' + (s.run_dir || '')) : (s.run_dir || '本机');
    const whereShort = s.mode === 'server'
      ? ('服务器 ' + (String(s.run_dir || '').split('/').filter(Boolean).pop() || ''))
      : whereFull;
    const v = String(s.verdict || '');
    const vtxt = s.has_identify
      ? '<span class="histtag" style="background:var(--done-soft);color:var(--done)">' + esc(s.verdict_cn || v || '已识别') + '</span>'
      : '<span class="histtag">还没识别</span>';
    h += '<tr' + (cur ? ' class="sessrow-cur"' : '') + '>'
      + '<td><b>' + esc(s.name || s.id) + '</b>'
      + (cur ? ' <span class="small">（当前）</span>' : '')
      + (s.note ? '<div class="note">' + esc(s.note) + '</div>' : '') + '</td>'
      // 日期单独不折行：挤窄时会断成「2026-11-」+「03 08:58:2」+「0」三行。
      + '<td class="pth nw">' + esc(String(s.created_at || '').replace('T', ' ')) + '</td>'
      // 「跑在哪」原来整条绝对路径塞进一格，窄窗口下折成三四行，
      //   整行被它撑得老高，看起来像多了好几个回车。
      //   现在只留末段目录名，单行省略，完整路径放 title 里，鼠标停上去能看全。
      + '<td class="pth"><div class="ellip" title="' + esc(whereFull) + '">' + esc(whereShort) + '</div></td>'
      + '<td>' + vtxt + '</td>'
      + '<td class="nw">' + (s.n_archives || 0) + ' 份</td>'
      + '<td class="ac-history-summary" data-run-summary="' + esc(s.id) + '">' + esc(runSummary ? (runSummary.sentence || '状态待核实') : '状态尚未读取') + '</td>'
      + '<td>' + (cur ? '<span class="small">' + (readonly ? '只读查看' : '正在看') + '</span>'
                      : readonly
                        ? '<button class="btn btn-sm" data-sess-view="' + esc(s.id) + '">只读查看</button>'
                        : '<button class="btn btn-sm" data-sess-switch="' + esc(s.id) + '">切换</button>') + '</td>'
      + '</tr>';
  });
  h += '</tbody></table>';
  host.innerHTML = h;
}

async function switchSession(id) {
  if (sessionChanging) return;
  sessionChanging = true;
  recordGeneration += 1;
  recordTimers.forEach(clearInterval); recordTimers.clear();
  stopPolling(); stopWaitWriteback();
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/sessions/switch', { id: id }), __recordScope);
    if (!r || !r.ok) { sessionChanging = false; toast('切不过去：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    resetRecordState();
    S.readonly = false;
    S.sessionId = (r.session && r.session.id) || id;
    stopWaitWriteback();
    await recordAwait(loadPlace(), __recordScope);
    await recordAwait(loadSessionsState(), __recordScope);
    sessionChanging = false;
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    if (S.runDir) await recordAwait(loadRun(), __recordScope);
    closeDlg('dlgSessions');
    toast('已切到：' + ((r.session && r.session.name) || id), 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 sessionChanging = false; toast('切换失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* Shared records are opened through their explicit id. This never calls the
   switch endpoint, so the server-side current record stays untouched. */
async function openReadOnlySession(id) {
  if (sessionChanging || !id) return;
  sessionChanging = true;
  recordGeneration += 1;
  recordTimers.forEach(clearInterval); recordTimers.clear();
  stopPolling(); stopWaitWriteback();
  const __recordScope = recordContext();
  try {
    resetRecordState();
    await recordAwait(loadSessionsState(String(id), false), __recordScope);
    if (!S.readonly) throw new Error('这条记录不是只读共享记录');
    await recordAwait(loadPlace(), __recordScope);
    await recordAwait(loadRun(), __recordScope);
    sessionChanging = false;
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    renderSessions();
    closeDlg('dlgSessions');
    toast('已打开共享记录（只读，不改变当前记录）', 'info');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    sessionChanging = false;
    toast('打开共享记录失败：' + e.message, 'bad');
  }
}

async function returnToCurrentRecord() {
  if (sessionChanging) return;
  sessionChanging = true;
  recordGeneration += 1;
  recordTimers.forEach(clearInterval); recordTimers.clear();
  stopPolling(); stopWaitWriteback();
  const __recordScope = recordContext();
  try {
    resetRecordState();
    await recordAwait(loadSessionsState(null, false), __recordScope);
    await recordAwait(loadPlace(), __recordScope);
    if (S.runDir) await recordAwait(loadRun(), __recordScope);
    sessionChanging = false;
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    renderSessions();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    sessionChanging = false;
    toast('回到当前记录失败：' + e.message, 'bad');
  }
}

/* ---------------- 弹窗 ---------------- */
function openDlg(id) { const el = document.getElementById(id); if (el) el.style.display = 'flex'; }
function closeDlg(id) { const el = document.getElementById(id); if (el) el.style.display = 'none'; }

/* 「再次交给助手处理」：已完成步骤的入口。
   ---------------------------------------------------------------------
   为什么不能直接复用"交给助手去跑"：
     这一步已经有结果了。用户点它不是首次执行，通常是为了
     重跑、只补某一部分、或者纠正上次跑偏的地方。
     直接甩出同一条旧指令，等于把上次的问题原样再发一遍。
   所以先把已完成的事实摆出来，再让他写清这次要什么。 */
let retryStepId = null;

/* 同一步骤是否已有任务在跑。
   ---------------------------------------------------------------------
   为什么要加这道拦截（2026-10-28）：
     执行端是"领到就提交"，同一类型的作业没有互斥。
     如果终版还在跑，人又点「按原样重跑」，队列里会多一条，
     执行端会立刻领走并再提交一次 —— 两个作业写同一个 8.final 目录，
     产物互相覆盖，最后留下的可能是半截结果。
     这不是理论风险：当天就差点发生。
   拦截不等于禁止：把后果摊开说清楚，人要强行再来一次仍然可以。 */
function busyOf(stepId) {
  // 必须比对分析记录。
  //   /api/exec 是"执行端视角"的接口，返回的是整台应用的队列，不按记录过滤。
  //   原来这里只比 step 和 status，于是另一条分析记录的同名步骤在排队时，
  //   当前记录也会被判成"这一步正在跑"：点不动、建不了新的、还反复提示排队中。
  //   两条记录的结果目录不同，同时跑根本不会写同一个文件，没有任何理由互斥。
  const sid = String(S.sessionId || '');
  if (!sid) return undefined;
  return arr(S.execTasks).find(t => t
    && String(t.session_id || '') === sid
    && String(t.step) === String(stepId)
    && (String(t.status) === 'running' || String(t.status) === 'queued'));
}

let busyForce = null;

// 上一次点下去那个按钮自己写的字。
//   注入会话的标题必须等于她看到的那行字，不能是另一套内部说法。
//   取不到就退回空串，由调用点给一个中性名字。
let lastInvokeLabel = '';

// 取一次就清空。
//   为什么要清（2026-11-23）：这个值是全局的，只在点 [data-run] / [data-req]
//   卡片时赋值，之后一直留着。于是先点了「交给助手去跑」，再点另一个完全
//   不相干的按钮，那个按钮的注入标题会被写成上一次那张卡片的字 ——
//   人看到「明明我点的按钮不叫这个」。
//   清掉之后，谁没自己赋值就只能拿到空串，由调用点给中性名字，
//   不会再借到别人的名字。
function takeInvokeLabel() {
  const v = String(lastInvokeLabel || '').trim();
  lastInvokeLabel = '';
  return v;
}

function showBusySheet(stepId, t, onForce) {
  const title = stepTitleCN(stepId) || stepId;
  const isRun = String(t.status) === 'running';
  const stCn = isRun ? '正在执行' : '正在排队';
  let h = '<div class="sh-p">「' + esc(title) + '」这一步已经有一个任务在' + esc(stCn) + '。</div>';
  h += '<div class="sh-p" style="margin-top:10px">现在再交一次，会有<b>两个作业同时写同一个产物目录</b>，'
    + '结果互相覆盖 —— 后跑完的那个可能把先跑完的改掉或删掉。'
    + '这不是警告而已，是同目录写入的必然结果。</div>';
  h += '<table class="sh-t" style="margin-top:12px">'
    + '<tr><th>任务</th><td>' + esc(String(t.id || '')) + '</td></tr>'
    + '<tr><th>状态</th><td>' + esc(stCn)
    + (t.job_id ? '（作业 ' + esc(String(t.job_id)) + '）' : '') + '</td></tr>'
    + '<tr><th>开始</th><td>' + esc(String(t.claimed_at || t.created_at || '').replace('T', ' ')) + '</td></tr>'
    + '<tr><th>最近报信</th><td>' + esc(String(t.last_seen || '').replace('T', ' ') || '还没报过') + '</td></tr>'
    + '</table>';
  h += '<div class="sh-p" style="margin-top:12px">建议等它跑完、结果写回之后再说。想看进度，点「查询结果」。</div>';
  // 无论排队中还是在跑，都要给一个人工作废的入口。
  //   为什么（2026-11-22，gina 报"只能改代码才能继续"）：
  //     最常见的死法不是"排着没人领"，而是"领取了、再没动静" ——
  //     执行端死了，任务停在 running。原来 running 连按钮都不给，
  //     界面上一个能点的东西都没有，人只能去改 state/exec_tasks.json。
  //     现在服务端自己判断：执行端还活着且还在报信，才拒一次并告诉你可以强制作废。
  if (!isRun) {
    h += '<div class="sh-p" style="margin-top:10px;color:#8a5a00">'
      + '如果它已经排了很久、执行端也没领走，可以把它作废，让后面的环节能派出去。'
      + '作废不改产物、不删记录，只把这条标成失败并写清原因。</div>';
  } else {
    h += '<div class="sh-p" style="margin-top:10px;color:#8a5a00">'
      + '如果执行端早就没了、这条只是停在"在跑"，可以强制结束它。'
      + '强制结束只改状态、不删记录；如果那个作业其实还在跑，它的产物仍可能落地。</div>';
  }
  const foot = '<button class="btn" data-busy-go="' + esc(stepId) + '">仍然再交一次</button>'
    + '<button class="btn" data-exec-cancel="' + esc(String(t.id || ''))
    + '">' + (isRun ? '强制结束这条任务' : '作废这条排队任务') + '</button>';
  const __recordScope = recordContext();
  busyForce = onForce ? () => { if (recordCurrent(__recordScope) && !sessionChanging) onForce(); } : null;
  sheet('这一步正在跑：' + title, h, foot);
}

/* 作废一条排队中的执行任务。
   判据由服务端给：只允许 queued、只允许本记录、还在报信的拒绝。
   前端不自己判断"该不该允许"，只把服务端的拒绝原样说出来。 */
async function cancelExecTask(execId, force) {
  if (sessionChanging || !execId) return;
  const __recordScope = recordContext();
  try {
    const r = await recordAwait(postJSON('/api/exec/cancel',
      { id: String(execId), force: !!force }), __recordScope);
    if (r && !r.ok) {
      // 服务端说"可能真的还在跑，但你可以强制作废" —— 再问一次，别替人决定。
      //   以前这种情形只回一句错误，人除了改文件没有别的路。
      if (r.can_force || r.code === 'exec_task_still_alive') {
        const ok = window.confirm(
          String(r.error || '这条可能还在跑。') + '\n\n仍然要作废吗？\n'
          + '作废只改状态、不删记录；如果那个作业其实还在跑，它的产物仍可能落地。');
        if (ok) return cancelExecTask(execId, true);
        return;
      }
      closeSheet();
      toast('没作废成：' + (r.error || r.code || '未知原因'), 'bad');
      return;
    }
    closeSheet();
    toast('已作废 ' + String(execId) + '，后面的环节可以派了。', 'good');
    await recordAwait(refreshExecTasks(), __recordScope);
    await recordAwait(loadRun(), __recordScope);
    renderAll();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    toast('作废失败：' + String((e && e.message) || e), 'bad');
  }
}

function openRetryDlg(id, force) {
  // 同一步骤还在跑时，先把后果摊开说清，不让人以为"点一下只是排个队"。
  const busy = busyOf(id);
  if (busy && !force) {
    showBusySheet(id, busy, function () { openRetryDlg(id, true); });
    return;
  }
  const sg = stageOf(id);
  const step = (S.steps || []).find(x => x.id === id) || {};
  const rs = remoteStep(id);
  retryStepId = id;

  $('#retryTitle').textContent = '再次交给助手：' + (step.title || (sg && sg.title) || id);
  $('#retryNote').textContent = sg && sg.status === 'done' ? '这一步已完成' : '';

  // 已完成的事实先摊开：不看这些就决定重跑什么，等于凭印象。
  const presentation = retryStatusPresentation(sg, rs);
  const bits = [presentation.label];
  if (sg && sg.ended_at) bits.push(presentation.time + ' ' + String(sg.ended_at).replace('T', ' '));
  const dur = sg && sg.status !== 'handoff_unconfirmed' && !(sg.status === 'failed' && !sg.ended_at) ? fmtDur(sg.started_at, sg.ended_at) : '';
  if (dur) bits.push('用了 ' + dur);
  if (sg && sg.source_host) bits.push('跑在 ' + sg.source_host);
  if (rs && rs.exec_pc) bits.push('经 ' + rs.exec_pc);
  if (sg && arr(sg.artifacts).length) bits.push('产物 ' + arr(sg.artifacts).length + ' 项');

  let dh = '';
  if (bits.length) {
    dh += '<div class="rz ' + presentation.cls + '" style="margin:0"><span class="mk">' + presentation.mark + '</span><span>'
      + esc(bits.join('　·　')) + '</span></div>';
  }
  const arts = sg ? arr(sg.artifacts) : [];
  if (arts.length) {
    dh += '<div style="margin-top:8px" class="sdarts">'
      + arts.slice(0, 8).map(a => '<span class="a">' + esc(a) + '</span>').join('')
      + (arts.length > 8 ? '<span class="a">…共 ' + arts.length + ' 项</span>' : '')
      + '</div>';
  }
  if (rs && rs.log) {
    dh += '<pre class="sdlog" style="margin-top:8px">' + esc(String(rs.log).slice(0, 600)) + '</pre>';
  }
  $('#retryDone').innerHTML = dh;

  $('#retryReq').value = '';
  $('#retryHint').textContent = '';
  openDlg('dlgRetry');
  const __recordScope = recordContext();
  setTimeout(function () {
    if (!recordCurrent(__recordScope)) return;
    const el = $('#retryReq');
    if (el) el.focus();
  }, 50);
}

document.addEventListener('click', function (e) {
  if (!e.target || e.target.id !== 'btnRetryGo') return;
  const id = retryStepId;
  if (!id) return;
  const note = ($('#retryReq') && $('#retryReq').value || '').trim();
  closeDlg('dlgRetry');
  retryStepId = null;
  // 直接把要求带进这一次执行，不再走一遍"先关框再重问"。
  runStep(id, note);
});

document.addEventListener('click', function (e) {
  const c = e.target.closest('[data-dlgclose]');
  if (c) { closeDlg(c.getAttribute('data-dlgclose')); return; }
  // 权限管理的两个开关按钮。统一在这里接，弹窗内容是动态渲染的，
  //   在渲染时逐个绑监听会随重渲染丢掉。
  const tg = e.target.closest('[data-admin-toggle]');
  if (tg) {
    toggleAdminSwitch(tg.getAttribute('data-admin-toggle'), tg.getAttribute('data-admin-target'));
    return;
  }
  // 点遮罩关闭（只在点到弹窗背景本身时，避免误关）
  if (e.target.classList && e.target.classList.contains('modal')) {
    closeDlg(e.target.id);
  }
});

/* =====================================================================
   目录选择
   ===================================================================== */
async function openDirDialog() {
  const __recordScope = recordContext();
  try {

  closeDlg('dlgDir');
  openDlg('dlgDir');
  const start = (S.placeDraft && S.placeDraft.local_path)
    || (S.env && S.env.home) || null;
  await recordAwait(browseTo(start), __recordScope);

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}
async function browseTo(dir) {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(getJSON('/api/fs' + (dir ? ('?dir=' + encodeURIComponent(dir)) : '')), __recordScope);
    S.browseDir = r.dir; S.browseParent = r.parent;
    $('#crumb').textContent = r.dir;
    const rows = [];
    if (r.parent) rows.push('<div class="lrow" data-go="' + esc(r.parent) + '"><span class="ic">↑</span><span>上一层</span></div>');
    arr(r.drives).forEach(d => {
      rows.push('<div class="lrow" data-go="' + esc(d) + '"><span class="ic">▣</span><span>' + esc(d) + '</span></div>');
    });
    arr(r.dirs).forEach(d => {
      rows.push('<div class="lrow" data-go="' + esc(joinPath(r.dir, d.name)) + '"><span class="ic">▸</span><span>' + esc(d.name) + '</span></div>');
    });
    arr(r.files).slice(0, 80).forEach(f => {
      rows.push('<div class="lrow file"><span class="ic">·</span><span>' + esc(f.name) + '</span><span class="sz">' + fmtSize(f.size) + '</span></div>');
    });
    const host = $('#listing');
    host.innerHTML = rows.join('') || '<div class="emptyart">空文件夹，可以直接当运行目录。</div>';
    $$('[data-go]', host).forEach(el => el.addEventListener('click', () => browseTo(el.getAttribute('data-go'))));
    $('#dlgHint').textContent = '当前：' + r.dir;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    $('#listing').innerHTML = '<div class="emptyart">读不了这个目录：' + esc(e.message) + '</div>';
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}
function joinPath(dir, name) {
  const sep = dir.indexOf('\\') >= 0 ? '\\' : '/';
  return String(dir).replace(/[\\/]+$/, '') + sep + name;
}
/* 在弹窗里选中一个目录。
   注意：这里**不再**直接把它设成运行目录，只写进草稿。
   真正的落地发生在「保存选择」——那一步才会去建配置、跑识别。
   以前是选完就立刻生效，于是「选了目录」和「开始干活」两件事
   被合成了一件，用户看不出自己到哪一步了。 */
async function chooseDir(dir) {
  const __recordScope = recordContext();
  try {

  closeDlg('dlgDir');
  S.placeDraft = S.placeDraft || placeDraftFromServer();
  S.placeDraft.mode = 'local';
  S.placeDraft.local_path = dir;
  renderPlace();
  toast('已选中：' + dir + '。确认后点「保存选择」。', 'good');

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   载入
   ===================================================================== */
async function loadRun() {
  const __recordScope = recordContext();
  try {

  const r = await recordAwait(getJSON('/api/run-dir'), __recordScope);
  if (!r.runDir) {
    // 没有运行目录不等于"什么都不用画"。
    //   以前这里直接 return，调用方后面那些渲染全不执行 —— 刷新后顶栏名字、
    //   会话栏都停在上一次的样子，主体一片空白，看起来像页面没加载完。
    //   现在先把外壳画出来，主体明说「未设运行目录」，再去补那句话。
    if (!recordCurrent(__recordScope)) return;
    // 这里不清 bootFail：读成功只是"确实没有目录"，
    //   不该顺手把启动时记下的失败原因（比如身份没校验成）抹成正常。
    renderHead();
    renderSessionsBar();
    renderNoRunDir();
    return;
  }
  bootFail = null;
  S.runDir = r.runDir;
  S.pipeline = r.pipeline || null;
  S.gates = r.gates || null;
  S.steps = r.steps || [];
  S.dirtyParams = {}; S.dirtySteps = {}; S.gateState = {}; S.gateOpen = {};
  arr(S.gates && S.gates.gates).forEach((g, i) => {
    if (!g.id) g.id = 'g' + i;
    const st = (g.status === 'answered') ? 'answered' : (g.status === 'auto' ? 'auto' : 'pending');
    S.gateState[g.id] = {
      status: st,
      answer: g.answer === undefined ? null : g.answer,
      answer_detail: g.answer_detail === undefined ? null : g.answer_detail,
    };
    S.gateOpen[g.id] = (st === 'pending');
  });
  const ss = r.stepState || {};
  S.running = (ss.status === 'running');
  S.step = ss.step || null;
  S.code = (ss.code === undefined ? null : ss.code);

  // 这里以前会去设 #needDir 的 display。那个节点在改版时已经删掉了，
  //   于是取到 null、赋 style 直接抛 TypeError。
  //   它又被 commitPlace 的 try/catch 接住，表现成「点了保存、弹一句失败、
  //   页面原地不动」—— 看起来像后端没成功，其实后端早就成功了。
  //   教训：删 DOM 时要一并清掉引用，不能只删 HTML。
  const mainEl = $('#main');
  if (mainEl) mainEl.style.display = 'block';
  renderAll();
  // 服务器模式必须常驻轮询。
  //   原来只有「本机在跑」才开轮询，服务器模式下本机永远不在跑，
  //   于是页面打开后只取一次状态，之后再也不刷新 ——
  //   助手在服务器上推进、回写，页面一动不动，人只能靠问才知道进展。
  if (S.sessionId) startPolling();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 权限管理：读开关、改开关。
   两个开关的含义写死在文案里，因为"关闭隔离"这件事必须让人看清楚
   代价是什么，不然点一下就会把所有人的记录变成互相可见。
   页面只负责显示和提交；判定全在后端（/api/admin/policy）。 */
async function loadAdmin() {
  const __recordScope = recordContext();
  try {
    const r = await api('/api/admin/policy', { recordBound: false });
    if (!recordCurrent(__recordScope)) return;
    S.admin = r || null;
    S.policy = (r && r.policy) || null;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    S.admin = { ok: false, error: String(e.message || e) };
  }
  const btn = $('#btnAdmin');
  if (btn) btn.style.display = (S.admin && S.admin.is_admin) ? '' : 'none';
  renderSessionsBar();
  return S.admin;
}

function openAdminDialog() {
  renderAdminBody();
  openDlg('dlgAdmin');
  loadAdmin().then(() => { renderAdminBody(); });
}

function adminSwitchRow(id, title, on, note) {
  // note 为空时不渲染那一段。
  //   gina 2026-11-19 把面板里那几行说明整块划掉了：她要的是一个开关本身，
  //   不是一段解释。留着空 <p> 还会多出一行空白。
  const n = String(note || '');
  return '<div class="card card-pad" style="margin-bottom:10px">'
    + '<div style="display:flex;align-items:center;gap:10px;flex-wrap:wrap">'
    + '<b>' + esc(title) + '</b>'
    + '<span class="small">' + (on ? '已开启（各看各的）' : '已关闭（互相可见）') + '</span>'
    + '<span class="spacer"></span>'
    + '<button class="btn ' + (on ? 'primary' : '') + '" data-admin-toggle="' + esc(id) + '"'
    + ' data-admin-target="' + (on ? 'false' : 'true') + '">'
    + (on ? '关闭隔离' : '打开隔离') + '</button>'
    + '</div>'
    + (n ? ('<p class="small" style="margin:8px 0 0">' + esc(n) + '</p>') : '')
    + '</div>';
}

function renderAdminBody() {
  const host = $('#adminBody'), note = $('#adminNote');
  if (!host) return;
  const a = S.admin;
  if (!a) { host.innerHTML = '<div class="small">正在读权限设置…</div>'; return; }
  if (!a.ok && !a.is_admin) {
    host.innerHTML = '<div class="small">读不到权限设置：' + esc(a.error || '未知原因') + '</div>';
    return;
  }
  // 非主人：不给任何可操作的开关（gina 2026-11-19）。
  //   她的原话：「应该只有账号主人可以做到」；并且接受退一步的方案：
  //   「非主人账号点击关闭是无效的，弹出无效的弹窗」。
  //   这里两层都做：面板里不渲染任何按钮，后端 POST 也已经 403 ——
  //   前端不给入口只是不让人点了才被拒，真正的边界在后端。
  if (!a.is_admin) {
    if (note) note.textContent = '';
    host.innerHTML = '<div class="rz bad" style="margin:0"><span class="mk">!</span>'
      + '<span><b>无效操作</b>：这个入口只对应用所属机器人的主人开放。'
      + '当前账号（' + esc((a.me && (a.me.username || a.me.uid || a.me.owner_key)) || '未识别') + '）不是主人，'
      + '不能查看或修改隔离设置。</span></div>';
    return;
  }
  const p = a.policy || S.policy || { record_isolation: true, write_isolation: true };
  // 上次是谁改的：只写日志不够，人打开面板就该看到上一手。
  if (note) {
    const who = (p.updated_by_username && p.updated_by_username !== '')
      ? p.updated_by_username
      : ((p.updated_by && p.updated_by !== '') ? p.updated_by : '');
    note.textContent = p.updated_at
      ? ('上次修改 ' + String(p.updated_at).replace('T', ' ') + (who ? (' · ' + who) : ''))
      : '还没改过';
  }
  // 只留一个开关：记录隔离（gina 2026-11-19）。
  //   她的原话：「截图1里红框的部分都去掉，就留下一个，因为你现在修改权限隔离
  //   实际上是不存在的」。那个开关不产生真实效果，摆着会让人以为改了就有用。
  //   后端 write_isolation 仍保持原值（默认开启），只是不再给一个假的旋钮。
  //   说明段落也一并去掉 —— 红框覆盖的就是这几行字。
  host.innerHTML = adminSwitchRow('record_isolation', '记录隔离（用户记录互相可见）',
    p.record_isolation !== false, '');
}

async function toggleAdminSwitch(id, target) {
  const want = target === true || target === 'true';
  const label = id === 'record_isolation' ? '记录隔离' : '修改权限隔离';
  if (!want && !confirm('确定关闭「' + label + '」？\n关闭后，任何已登录用户都能' +
      (id === 'record_isolation' ? '看到全部记录' : '修改别人的记录和应用代码') + '。')) return;
  try {
    const r = await api('/api/admin/policy', {
      recordBound: false,
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ [id]: want })
    });
    if (!r || !r.ok) { toast('没改成：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    S.admin = Object.assign({}, S.admin, { policy: r.policy });
    S.policy = r.policy;
    renderAdminBody();
    renderSessionsBar();
    toast(label + (want ? '已开启' : '已关闭') + '。', 'good');
  } catch (e) {
    toast('没改成：' + e.message, 'bad');
  }
}

function renderAll() {
  const record = recordContext();
  renderHead();
  renderPlace();
  // 注释依据：换组织/物种/细胞类型范围就要换一份表，
  //   所以它是"每次分析自己选一份"的资产，不是内置常量。
  loadReferences();
  // 先拉脚本清单（只取数据），再渲染识别结论 ——
  //   脚本明细并进识别结果的第三段，不再单独占一个区块。
  renderStageScripts().then(() => {
    if (!recordCurrent(record)) return;
    renderIdentify();
  });
  renderTaskLog();
  renderGates();
  // 决策是"服务端生成过的条目"，必须单独取一次。
  //   不取就会按空渲染，看起来像"都定好了" —— 那正是要避免的误读。
  loadDecisions();
  renderRun();
  renderSettings();
  renderHistory();
  renderSidenav();
  refreshArtifacts();
  // 处理过程要单独取一次：它是「跑到哪个环节、花了多久」，
  //   和上面那些渲染各管一件事，不塞进其中任何一个里。
  loadFlow();
  // 权限管理：既决定「权限管理」按钮出不出现，也决定状态行怎么说只读。
  //   不只在打开弹窗时读 —— 只读提示要跟着每次渲染走。
  loadAdmin();
  // 脚本数据库是页面里的一节，不是弹窗 —— 打开页面就要有内容，
  //   不能等人点了按钮才第一次去读。
  renderLib();
  // 顶部页签互斥：不属于当前页的区块一律藏起来。
  //   不写这一段，首次打开会把三页的内容一起铺出来 —— 看起来像三个页面拼在一起。
  const inTab = (TABS[curTab] || TABS.cur).secs;
  $$('#main section.sec').forEach(sec => {
    if (inTab.indexOf(sec.id) < 0) sec.style.display = 'none';
  });
}
/* 闸门按段位分两组。
   A = 跑主流程之前就得定（数据在哪、注释依据、质控门槛、读入前剔样本）
   B = 一次聚类之后才定（筛群、剔样本、低置信门槛）
   为什么要分：B 那几条的依据（簇大小、样本分布、marker）只有跑到一次聚类才产生。
   混在一起显示，人会以为「现在就得答」，而这时他手上没有任何材料。 */
/* =====================================================================
   决策：跑到那一步才生成，人答了才算定
   ---------------------------------------------------------------------
   这是这一版改动最大的一处。原来的模型是：
     初始化时把九条问题一次性抄进 run-dir/gates.json → 页面读文件 →
     读不到就整块隐藏。
   两个后果都很实在：
     · 服务器模式下 run-dir 在服务器上，本机读不到 → 那一栏直接消失，
       人连被问一次的机会都没有；
     · 就算读到了，那也是一份提前写好的清单，不是"处理到这里才该问的问题"。
   现在反过来：清单由服务端在走到某一步时按当时的真实数据生成，
   生成的同时发进会话；页面只显示"已经生成过的"。
   没走到，就没有条目 —— 空的不是故障，是还没到这里。
   ===================================================================== */
function decItems() { return arr(S.decisions && S.decisions.items); }
function decPendingFor(stepId) {
  return decItems().filter(x => String(x.status || '') === 'asked'
    && String(x.step || '') === String(stepId));
}
/* 某一步卡在哪些待答问题上（页面用它决定按钮能不能点、提示什么） */
function blockingTitles(stepId) {
  return decPendingFor(stepId).map(x => x.title || x.key);
}
function pendingDecisions() {
  return decItems().filter(x => String(x.status || '') === 'asked');
}

/* =====================================================================
   注释依据（参考表 / marker 词典）
   ---------------------------------------------------------------------
   为什么要有这一块，而不是内置一份"官方表"：
     参考表是按组织、按物种、按细胞类型范围定的。换一批数据就换一份依据。
     内置一份能跑通这一轮，下一批就废 —— 而用错依据注释出来的结果，
     格式是对的、内容是错的，事后极难发现。
   所以这里做三件事：上传、说明用途、按分析记录选用。
   ===================================================================== */
function fmtBytes(n) {
  n = Number(n) || 0;
  if (n < 1024) return n + ' B';
  if (n < 1048576) return (n / 1024).toFixed(0) + ' KB';
  return (n / 1048576).toFixed(1) + ' MB';
}

async function loadReferences() {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(getJSON('/api/reference/list'
      + (S.sessionId ? ('?session=' + encodeURIComponent(S.sessionId)) : '')), __recordScope);
    S.references = (r && r.ok) ? r : null;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 S.references = null; }
  renderReferences();
  renderPlace();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 页面上不再铺开「注释依据」这一整节。
   ---------------------------------------------------------------------
   为什么删：服务器上的参考表是检测运行环境时顺手罗列出来的，属于"背景材料"，
   不是"此刻要人做的决定"。以前它整节常驻在页面上（列表 + 上传表单 + 查文献表单），
   人一进来就先看到一堆和自己当前这步无关的表，真正的选择入口反而在弹出卡里，
   两套界面同一件事，改一处漏一处。
   现在只剩一个入口：点「注释」那一步的运行按钮，由后端 409 need_reference
   弹出卡片，在卡片里切 服务器已有 / 上传 / 查文献 三页。
   这里保留函数只是为了别的地方调用它时不至于报错。 */
function renderReferences() {
  const note = $('#refNote');
  if (note) note.textContent = '';
}

/* 被闸门拦住时弹出来的选择面板。
   ---------------------------------------------------------------------
   为什么单独一个面板而不是让人自己去下面那一节找：
     人点的是这一张卡上的按钮，视线就在这儿。把他弹到另一个区块，
     等于让他自己回忆"刚才那件事要用哪份表"。选择就该出现在被打断的地方。
   三条路都在面板里，选完立刻继续刚才那次提交 —— 不用再点一次。 */
let refPickerGo = null;
let refPickStepId = '';

function openRefPicker(stepId, onReady, prefMode) {
  const record = recordContext();
  refPickerGo = onReady ? () => { if (recordCurrent(record) && !sessionChanging) onReady(); } : null;
  if (stepId) refPickStepId = stepId;
  // 打开时默认停在哪一页：
  //   ① 事先在服务器信息里选过（prefMode）就听它的 —— 那个决定已经做过，
  //      卡片只是把结果落实，不该又让人从别的页开始翻；
  //   ② 否则服务器上已经有表就看服务器那页；
  //   ③ 都没有就落到上传页 —— 让人一进来就看到能立刻做的事。
  const items0 = arr(S.references && S.references.items);
  const hasSrv = items0.some(x => String(x.source || '') === 'server');
  if (prefMode === 'upload') S.refTab = 'upload';
  else if (prefMode === 'lit') S.refTab = 'lit';
  else if (prefMode === 'path' || prefMode === 'none') S.refTab = 'server';
  else if (!S.refTab) S.refTab = hasSrv ? 'server' : 'upload';
  renderRefPicker();
}

/* 三页签的选择卡。
   ---------------------------------------------------------------------
   为什么改成页签而不是三个散落按钮：
     以前三个入口分别在页面不同区块，点"服务器上找"会被弹到下面的
     注释依据整节去，人得自己滚回去找文件输入框。三件事本质是同一件
     事（这份依据从哪来）的三个答案，就该在同一张卡里切。
   三页各自的动作都留在这张卡里完成，不跳走：
     服务器已有 -> 列出现成的表，点一行就选用
     上传       -> 选文件、填说明、传完自动选用
     查文献     -> 填物种组织，发一句话给助手，让它去建词典 */
function renderRefPicker() {
  // 卡片的标题要用"是哪一步把我拦下来的"。以前这里直接写 stepId，
  //   而 stepId 只是 openRefPicker 的形参，本函数根本看不到 —— 渲染到
  //   最后一行就抛 ReferenceError，整张卡画不出来，页面看起来像"什么都没发生"。
  //   现在统一从 refPickStepId 取，它由 openRefPicker 存下来。
  const stepId = refPickStepId;
  const items = arr(S.references && S.references.items);
  const chosen = String((S.references && S.references.chosen) || '');
  const srv = items.filter(x => String(x.source || '') === 'server');
  const up = items.filter(x => String(x.source || '') !== 'server');
  const tab = S.refTab || 'server';
  // 这批数据是什么物种、什么组织：判"这份表适不适用"必须要有它。
  //   空着只能如实说判不了，不能默认"应该没问题"。
  const spNow = (S.references && S.references.species) || '';
  const tsNow = (S.references && S.references.tissue) || '';

  /* 这张表自己标了什么 —— 只陈述，不给"能不能用"的结论。
     2026-11-04 改：原来这里显示「跨物种，可用」「适用于这批数据」，
     等于替人做了一次适用性判断。而这件事本来该由人定：
     跨物种注释在研究里是常规做法（移植实验里人源鼠源细胞共存、
     人鼠多数判定基因是同源基因只差大小写），不存在需要拦截的风险。
     现在只把表标注的物种/组织如实摆出来，用不用由人选；选了就严格执行。 */
  const apTag = (it) => {
    const v = String(it.applicable || '');
    if (v === 'ok') return '<span class="aptag ok">与这批数据标注一致</span>';
    if (v === 'noted') return '<span class="aptag">标注范围不同（照用）</span>';
    if (v === 'unlabeled' || v === 'unknown') return '<span class="aptag">未标注范围</span>';
    return '';
  };
  const row = (it) => '<button class="refpick-item' + (String(it.id) === chosen ? ' on' : '')
    + '" data-refpick="' + esc(it.id) + '">'
    + '<span class="refpick-main">'
    + '<b>' + esc(it.name) + '</b>'
    + '<span class="mut">' + fmtBytes(it.bytes)
    + (it.species ? '　' + esc(it.species) : '')
    + (it.tissue ? '　' + esc(it.tissue) : '')
    + '　' + apTag(it)
    + '</span>'
    + (it.server_path ? '<span class="refpick-path">' + esc(it.server_path) + '</span>' : '')
    + (it.note ? '<span class="mut">' + esc(it.note) + '</span>' : '')
    + '</span>'
    + '<span class="refpick-go">' + (String(it.id) === chosen ? '在用' : '选用') + '</span>'
    + '</button>';

  const tabBtn = (id, label, n) => '<button class="reftab' + (tab === id ? ' on' : '')
    + '" data-reftab="' + id + '">' + label
    + (n ? '<span class="reftab-n">' + n + '</span>' : '') + '</button>';

  let h = '<div class="reftabs">'
    + tabBtn('server', '服务器已有', srv.length)
    + tabBtn('upload', '上传', up.length)
    + tabBtn('lit', '查文献建表', 0)
    + '</div>';

  if (chosen) {
    const cur = items.find(x => String(x.id) === chosen);
    h += '<div class="rz ok" style="margin:12px 0 0"><span class="mk">✓</span><span>'
      + '本次用：<b>' + esc((cur && cur.name) || chosen) + '</b></span></div>';
  } else {
    h += '<div class="rz warn" style="margin:12px 0 0"><span class="mk">!</span><span>'
      + '还没选注释依据</span></div>';
  }

  // 这批数据的物种、组织：判"这份表适不适用"必须要有它，所以跟选择放在同一张卡里。
  //   页面上原来另有一整节「注释依据」，已经删掉（选择只在点「注释」时弹这张卡），
  //   这一行就是它唯一的入口 —— 空着只报"判不了"，不默认"应该没问题"。
  h += '<div class="sh-h2">这批数据</div>'
    + '<div class="refrow">'
    + '<input type="text" id="dataSpeciesIn" placeholder="物种（例如 Mouse）" value="' + esc(spNow) + '">'
    + '<input type="text" id="dataTissueIn" placeholder="组织（例如 Kidney）" value="' + esc(tsNow) + '">'
    + '<button class="btn btn-sm" data-refspecies="1">记下</button>'
    + '</div>';

  if (tab === 'server') {
    h += '<div class="sh-h2">服务器上已有的注释依据</div>';
    if (srv.length) {
      h += '<div class="refpick-list">' + srv.map(row).join('') + '</div>';
      h += '<div class="btnrow" style="margin-top:10px">'
        + '<button class="btn btn-sm" data-refscan="1">重新扫一遍</button>'
        + '<span class="small" id="scanMsg"></span></div>';
    } else {
      h += '<div class="sh-p">还没列过。点下面这一下，让助手到服务器上把可能的'
        + '注释依据表列出来（只读，不改任何文件）。</div>'
        + '<div class="btnrow"><button class="btn primary" data-refscan="1">让助手去列一遍</button>'
        + '<span class="small" id="scanMsg"></span></div>';
    }
  } else if (tab === 'upload') {
    h += '<div class="sh-h2">上传一份注释依据</div>'
      + '<input type="file" id="refFile" class="filein">'
      + '<div class="refrow"><input type="text" id="refNoteIn" placeholder="用途说明（必填，写清适用什么组织/物种）"></div>'
      + '<div class="refrow">'
      + '<input type="text" id="refSpeciesIn" placeholder="物种">'
      + '<input type="text" id="refTissueIn" placeholder="组织">'
      + '</div>'
      + '<div class="btnrow" style="margin-top:8px">'
      + '<button class="btn primary" data-refup="1">上传并选用</button>'
      + '<span class="small" id="refMsg"></span></div>';
    if (up.length) {
      h += '<div class="sh-h2">已上传的</div><div class="refpick-list">' + up.map(row).join('') + '</div>';
    }
  } else {
    h += '<div class="sh-h2">让助手查文献建一份 marker 词典</div>'
      + '<div class="sh-p">适用于服务器上没有现成参考表的情况。'
      + '助手会按物种组织检索文献，抽出 marker 组成词典，再交给注释脚本。'
      + '这一步只读文献，不动数据。</div>'
      + '<div class="refrow">'
      + '<input type="text" id="litSpeciesIn" placeholder="物种（必填，例如 Mouse）">'
      + '<input type="text" id="litTissueIn" placeholder="组织（必填，例如 Kidney）">'
      + '</div>'
      + '<div class="btnrow" style="margin-top:8px">'
      + '<button class="btn primary" data-reflit="1">发要求给助手去查</button>'
      + '<span class="small" id="litMsg"></span></div>';
  }

  // key 固定：三个页签是同一条选择的三个答案，切换只换这张卡里的内容，
  //   卡本身（位置、宽高、层数、遮罩）一次定了就不动。
  //   以前这里每切一次页签就新建一层浮层，旧的不关，于是看起来像三个弹窗。
  sheet('注释依据 · ' + (stepTitleCN(stepId) || stepId || '注释'), h, null, { key: 'refpicker' });
}


/* 让助手去服务器上找一遍已有的参考表。
   为什么这件事必须它去做：应用在公网这一侧，看不到对方内网磁盘。
   它只读不写 —— 找不到就如实说没找到，不许拿目录里不相干的东西凑数。 */
async function askRefScan() {
  const __recordScope = recordContext();
  try {

  const msg = $('#scanMsg') || $('#refPickMsg');
  if (msg) msg.textContent = '正在把这件事发到会话…';
  try {
    const r = await recordAwait(postJSON('/api/reference/scan-request', { session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) { if (msg) msg.textContent = '没能发起：' + ((r && r.error) || '未知'); return; }
    if (msg) msg.textContent = r.queued ? '已发出，等回写。' : '已生成，未送进会话。';
    toast('已发出。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msg) msg.textContent = '失败：' + String(e.message || e);
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function doRefUpload() {
  const __recordScope = recordContext();
  try {

  const inp = $('#refFile'), msg = $('#refMsg');
  const f = inp && inp.files && inp.files[0];
  if (!f) { if (msg) msg.textContent = '先选一个文件。'; return; }
  const note = ($('#refNoteIn') && $('#refNoteIn').value || '').trim();
  if (!note) { if (msg) msg.textContent = '填一下用途说明。'; return; }
  if (msg) msg.textContent = '正在上传…';
  try {
    // 读成 base64：xlsx 是二进制，用 text() 会损坏内容。
    const buf = await recordAwait(f.arrayBuffer(), __recordScope);
    let bin = '';
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < bytes.length; i += 1) bin += String.fromCharCode(bytes[i]);
    const b64 = btoa(bin);
    const r = await recordAwait(postJSON('/api/reference/upload', {
      name: f.name, b64: b64, note: note,
      species: ($('#refSpeciesIn') && $('#refSpeciesIn').value || '').trim(),
      tissue: ($('#refTissueIn') && $('#refTissueIn').value || '').trim(),
    }), __recordScope);
    if (!r || !r.ok) { if (msg) msg.textContent = '上传失败：' + ((r && r.error) || '未知'); return; }
    if (msg) msg.textContent = '已上传。';
    // 传完自动选为本次分析用的那一份：上传这个动作本身就表示"这次要用它"。
    const picked = await recordAwait(postJSON('/api/reference/choose', { id: r.item.id, session_id: S.sessionId || '' }), __recordScope);
    if (!picked || !picked.ok) throw new Error((picked && picked.error) || '选用失败');
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft.ref_mode = 'upload';
    await recordAwait(saveRefChoice(), __recordScope);
    toast('参考表已上传，并设为本次分析使用。', 'good');
    await recordAwait(loadReferences(), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msg) msg.textContent = '失败：' + String(e.message || e);
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 在选择面板里选了一份：先设为本次使用，然后继续刚才被打断的那次提交。
   为什么自动继续：人是点了"跑这一步"才被拦的，选完还让他回去再点一次，
   等于把一件事拆成两步、还容易忘。 */
async function pickRefAndGo(id) {
  const __recordScope = recordContext();
  try {

  const el = document.getElementById('stSheet');
  try {
    const r = await recordAwait(postJSON('/api/reference/choose', { id: id, session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) { toast('选用失败：' + ((r && r.error) || ''), 'bad'); return; }
    await recordAwait(loadReferences(), __recordScope);
    if (el && el.parentNode) el.parentNode.removeChild(el);
    const go = refPickerGo; refPickerGo = null;
    if (typeof go === 'function') { toast('依据已定，继续跑这一步。', 'good'); go(); }
    else toast('已设为本次分析的注释依据。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    toast('选用失败：' + String(e.message || e), 'bad');
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 记下这批数据的物种与组织。
   为什么必须有这一步：判"这份表适不适用"要比对两边的物种组织。
   数据侧的物种组织不登记，每份表都只能报"判不了"——
   那等于把判断又推回给人自己猜。 */
async function saveDataScope() {
  const __recordScope = recordContext();
  try {

  const sp = ($('#dataSpeciesIn') && $('#dataSpeciesIn').value || '').trim();
  const ts = ($('#dataTissueIn') && $('#dataTissueIn').value || '').trim();
  if (!sp && !ts) { toast('物种和组织至少填一项。', 'bad'); return; }
  try {
    const r = await recordAwait(postJSON('/api/sessions/update', {
      id: S.sessionId || '', species: sp, tissue: ts,
    }), __recordScope);
    if (!r || !r.ok) { toast('没记上：' + ((r && r.error) || ''), 'bad'); return; }
    toast('已记下。', 'good');
    await recordAwait(loadReferences(), __recordScope);
    if (document.getElementById('stSheet')) openRefPicker(refPickStepId, refPickerGo);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('没记上：' + String(e.message || e), 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function chooseRef(id) {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/reference/choose', { id: id, session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) { toast('选用失败：' + ((r && r.error) || ''), 'bad'); return; }
    toast('已设为本次分析的注释依据。', 'good');
    await recordAwait(loadReferences(), __recordScope);
    // 同时登记一条注释版本候选。
    //   为什么：注释这一步要"跑几版、挑一版"。选定依据本身就是"这一版用什么表"，
    //   不登记的话，换表再跑一次会把上一版覆盖掉，人就没得比了。
    const it = arr(S.references && S.references.items).find(x => String(x.id) === String(id));
    try {
      await recordAwait(postJSON('/api/variants', {
        action: 'save', stage: 'annotate',
        item: {
          params: { reference: id },
          label: '依据：' + ((it && it.name) || id),
          status: 'pending',
          outdir: '9.annot',
          note: it && it.note ? String(it.note).slice(0, 200) : ''
        }
      }), __recordScope);
      await recordAwait(loadVariants(true), __recordScope);
      renderVarPanels();
    } catch (e2) {
    if (!recordCurrent(__recordScope)) return;
 /* 登记失败不影响选用本身 */ }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('选用失败：' + String(e.message || e), 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 让助手去查文献建词典。
   填物种组织是为了检索更准，不是资格审核。
   跨物种也能用（人鼠 marker 多为同源基因，比对时统一成大写）。
   2026-11-04 明确一点：这条路只允许"检索已发表文献并逐条标出处"，
   不允许助手凭记忆拼一份出来。记忆不是出处；一份没来源的词典
   比没有词典更危险 —— 它看起来像依据，事后没人会再去核。 */
async function askLitDict() {
  const __recordScope = recordContext();
  try {

  const msg = $('#litMsg');
  const sp = ($('#litSpeciesIn') && $('#litSpeciesIn').value || '').trim();
  const ts = ($('#litTissueIn') && $('#litTissueIn').value || '').trim();
  if (!sp || !ts) {
    if (msg) msg.textContent = '物种和组织都要填。';
    return;
  }
  if (msg) msg.textContent = '正在把这件事发到会话…';
  try {
    const r = await recordAwait(postJSON('/api/reference/litdict', {
      species: sp, tissue: ts, session_id: S.sessionId || '',
    }), __recordScope);
    if (!r || !r.ok) { if (msg) msg.textContent = '没能发起：' + ((r && r.error) || '未知'); return; }
    if (msg) msg.textContent = r.queued ? '已发出，等回写。' : '已生成，未送进会话。';
    toast('已发出。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msg) msg.textContent = '失败：' + String(e.message || e);
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 选完文件就上传，并立刻把它显示出来。
   为什么不再放进那张三页签的卡片里：来源人已经在页面上选过了，
   再弹一次等于把同一个决定问两遍；而且卡片传完就关，
   页面上看不到"到底传上了没有"——实测她就是在这一步卡住的。 */
function pickRefFile() {
  let inp = document.getElementById('refFileQuick');
  if (!inp) {
    inp = document.createElement('input');
    inp.type = 'file';
    inp.id = 'refFileQuick';
    inp.style.display = 'none';
    document.body.appendChild(inp);
    inp.addEventListener('change', function () {
      const f = inp.files && inp.files[0];
      if (f) uploadRefFile(f);
      inp.value = '';
    });
  }
  inp.click();
}

async function uploadRefFile(f) {
  const __recordScope = recordContext();
  try {

  const msg = document.getElementById('refQuickMsg');
  const say = function (x) { if (msg) msg.textContent = x; };
  say('正在上传 ' + f.name + '…');
  try {
    const buf = await recordAwait(f.arrayBuffer(), __recordScope);
    let bin = '';
    const bytes = new Uint8Array(buf);
    for (let i = 0; i < bytes.length; i += 1) bin += String.fromCharCode(bytes[i]);
    const d = S.placeDraft || {};
    const r = await recordAwait(postJSON('/api/reference/upload', {
      name: f.name, b64: btoa(bin),
      note: (d.ref_note || '').trim() || f.name,
      species: (S.references && S.references.species) || '',
      tissue: (S.references && S.references.tissue) || '',
    }), __recordScope);
    if (!r || !r.ok) { say('上传失败：' + ((r && r.error) || '未知')); return; }
    if (!S.sessionId) {
      // Library upload does not create a record or pick an unrelated history item.
      S.pendingRef = Object.assign({}, r.item, { id: r.item.id, name: f.name, bytes: f.size });
      renderPlace();
      return;
    }
    const c = await recordAwait(postJSON('/api/reference/choose', {
      id: r.item.id, session_id: S.sessionId,
    }), __recordScope);
    if (!c || !c.ok) { say('传上去了，但没能设为本次使用：' + ((c && c.error) || '未知')); return; }
    // 选择落到服务端：不然刷新一下又回到"没选"。
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft.ref_mode = 'upload';
    await recordAwait(saveRefChoice(), __recordScope);
    await recordAwait(loadReferences(), __recordScope);
    renderPlace();
    toast('已上传并设为本次使用：' + f.name, 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    say('上传失败：' + String(e.message || e));
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 把注释依据这一组选择写回服务端（不触发识别）。 */
async function saveRefChoice() {
  const __recordScope = recordContext();
  try {

  const d = S.placeDraft || {};
  if (d.mode !== 'server') return;
  try {
    const result = await recordAwait(postJSON('/api/place', {
      mode: d.mode,
      session_id: S.sessionId || undefined,
      server: {
        ref_mode: d.ref_mode || '',
        ref_path: d.ref_path || '',
        ref_note: d.ref_note || '',
        ref_archive: d.ref_archive || '',
      },
    }), __recordScope);
    if (!result || !result.ok) throw new Error((result && result.error) || '保存失败');
    S.place = result;
    return result;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    toast('注释依据没有保存：' + e.message, 'bad');
    throw e;
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 文献库：只送存档号，到注释环节由助手按号去取。
   为什么不在页面里查：应用够不到那个库，装出来的"查询结果"只能是假的。 */
async function askLitByArchive() {
  const __recordScope = recordContext();
  try {

  const msg = document.getElementById('refLitMsg');
  const say = function (x) { if (msg) msg.textContent = x; };
  const d = S.placeDraft || {};
  const ano = String(d.ref_archive || '').trim();
  if (!ano) { say('先填存档号。'); return; }
  say('正在保存…');
  try {
    await recordAwait(saveRefChoice(), __recordScope);
    say('已保存存档号 ' + ano + '，到注释环节检索。');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    say('失败：' + String(e.message || e));
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function loadDecisions() {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(getJSON('/api/decisions'
      + (S.sessionId ? ('?session=' + encodeURIComponent(S.sessionId)) : '')), __recordScope);
    S.decisions = (r && r.ok) ? r : null;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    S.decisions = null;
  }
  renderGates();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 一张待答卡：问题、为什么问、可选项、依据。
   选项用单列按钮而不是下拉：这些选项要连"依据"一起看才好选。 */
function decCard(it, idx) {
  const opts = arr(it.options);
  const sug = String(it.suggested === null || it.suggested === undefined ? '' : it.suggested);
  let h = '<div class="gcard" data-dec="' + esc(it.id) + '">';
  h += '<div class="ghead"><div class="gnum">' + (idx || '·') + '</div>'
    + '<div class="gtit"><h3>' + esc(it.title || it.key) + '</h3>'
    + (it.question ? '<div class="gwhy">' + esc(it.question) + '</div>' : '') + '</div></div>';
  h += '<div class="gbody">';
  if (it.why) h += '<div class="decwhy">' + esc(it.why) + '</div>';
  h += '<div class="decopts">' + opts.map(o => {
    const v = String(o.value === null || o.value === undefined ? '' : o.value);
    return '<label class="decopt"><input type="radio" name="dec-' + esc(it.id) + '" value="' + esc(v) + '"'
      + (v === sug ? ' checked' : '') + '>'
      + '<span>' + esc(o.label || v) + (v === sug ? '<i>建议</i>' : '') + '</span></label>';
  }).join('') + '</div>';
  // 选"我自己指定"这一类需要写具体值
  h += '<input class="decin" type="text" data-dec-detail="' + esc(it.id)
    + '" placeholder="要补充就写在这里（例如簇号 24,25）">';
  if (it.evidence && it.evidence.kind === 'clusters' && arr(it.evidence.flagged).length) {
    h += '<div class="sh-h2">依据：被标出来的簇</div>'
      + '<table class="sh-t">' + arr(it.evidence.flagged).map(c => '<tr>'
        + '<th>簇 ' + esc(c.cluster) + '</th><td>' + esc(String(c.n)) + ' 细胞　Mock '
        + esc(String(c.mock_pct)) + '% / MP ' + esc(String(c.mp_pct)) + '%'
        + (c.flags ? '<br><span class="mut">' + esc(c.flags) + '</span>' : '') + '</td></tr>').join('')
      + '</table>';
  } else if (it.evidence && it.evidence.note) {
    h += '<div class="mut" style="margin-top:8px">' + esc(it.evidence.note) + '</div>';
  }
  h += '</div>';
  h += '<div class="gfoot">'
    + '<button class="btn primary" data-dec-confirm="' + esc(it.id) + '">确定</button>'
    + (sug ? '<button class="btn" data-dec-sug="' + esc(it.id) + '">用建议值</button>' : '')
    + '</div></div>';
  return h;
}

/* 决策对话框：把某一步的待答卡摆出来，人自己选、自己点确定。
   为什么需要它（2026-11-13 实测）：页面上的「需要你决定的事」整块已删，
   renderGates 只剩空壳并提前 return，于是 pending 只有计数、
   没有任何可点的地方 —— 阶段卡按钮写着"先定 1 件"却带 disabled，
   人在浏览器里点不动，Playwright 点它也只会超时。流程死在这一步。
   这里不自动采用建议值：选项要人点，确定才写回，
   写回仍走既有的 POST /api/decisions/answer。 */
function openDecisionDlg(stepId) {
  const list = decPendingFor(stepId);
  if (!list.length) {
    toast('这一步没有待答的问题了。', 'good');
    renderRun();
    return;
  }
  let body = '';
  list.forEach(function (it, i) {
    body += decCard(it, i + 1);
  });
  body += '<div class="mut" style="margin-top:10px">'
    + '这里只把问题摆出来。选哪一项由你点，确定之后才写回记录。</div>';
  const foot = '<button class="btn" data-sheet-close="1">先不答</button>';
  sheet('这一步需要你先定 ' + list.length + ' 件', body, foot, { wide: true });
  // 不预选任何一项。
  //   这张卡带 suggested，decCard 会把建议值预勾上；在本对话框里那样做，
  //   人一路点"确定"就等于替自己采纳了建议 —— 而这里的口径是
  //   "选哪一项由人点"。所以打开时把预勾全部清掉，人必须自己点一下。
  try {
    const top = SHEET_STACK.length ? SHEET_STACK[SHEET_STACK.length - 1].el : null;
    if (top) top.querySelectorAll('input[type=radio]:checked').forEach(function (el) { el.checked = false; });
  } catch (e) {}
}

function decDoneCard(it) {
  const opts = arr(it.options).filter(o => String(o.value) === String(it.answer));
  let lab = opts.length ? String(opts[0].label || opts[0].value) : String(it.answer === null ? '—' : it.answer);
  if (it.answer_detail) lab += '（' + String(it.answer_detail) + '）';
  return '<div class="gcard done"><div style="display:flex;gap:10px;align-items:center;padding:11px 18px;flex-wrap:wrap">'
    + '<span class="tag done">已定</span><b>' + esc(it.title || it.key) + '</b>'
    + '<span class="small" style="flex:1;min-width:0">' + esc(lab) + '</span>'
    + '<span class="mut">' + esc(String(it.answered_at || '').replace('T', ' ').slice(5, 16)) + '</span>'
    + '</div></div>';
}

function renderGates() {
  // 「需要你决定的事」这个板块已经删掉（她要的是"该问的时候弹窗问、答案写回记录"，
  //   不是常驻一页）。这里留一个空壳，是因为别处还会调它 ——
  //   直接删函数会让那些调用点抛错，而它们本身没有错。
  const host = $('#gatesHost'), note = $('#gatesNote');
  if (!host) return;
  const sec = document.getElementById('secGates');
  const pend = pendingDecisions();
  const done = decItems().filter(x => String(x.status || '') === 'answered');

  // 没有任何生成过的条目：整块收起来。
  //   这里刻意不再写"都定好了，可以跑" —— 那句话在"还没生成"的时候
  //   是最误导的一句：它把"没人问过"说成了"都定完了"。
  if (!pend.length && !done.length) {
    if (host) host.innerHTML = '';
    if (note) note.textContent = '';
    if (sec) sec.style.display = 'none';
    renderHead();
    return;
  }
  if (sec) sec.style.display = 'block';

  let html = '';
  if (pend.length) {
    html += '<div class="sech">要你先定<span class="aside">' + pend.length + ' 件</span></div>';
    pend.forEach((it, i) => { html += decCard(it, i + 1); });
  }
  if (done.length) {
    html += '<details class="stgfold" style="margin-top:18px"><summary>已经定下来的'
      + '<span class="cnt">' + done.length + ' 件</span></summary>'
      + '<div style="padding:10px 12px">' + done.map(decDoneCard).join('') + '</div></details>';
  }
  if (host) host.innerHTML = html;
  if (note) note.textContent = pend.length ? (pend.length + ' 件等你定') : '';
  renderHead();
}

/* 确定：把选中的值写回服务端，再重画。
   没答的还留着，答过的进"已经定下来的"。 */
async function answerDecision(id, useSuggested) {
  const __recordScope = recordContext();
  try {

  const it = decItems().find(x => String(x.id) === String(id));
  if (!it) return;
  let val = useSuggested ? String(it.suggested === null || it.suggested === undefined ? '' : it.suggested) : '';
  if (!val) {
    const el = document.querySelector('input[name="dec-' + id + '"]:checked');
    if (el) val = el.value;
  }
  if (!val) { toast('先选一个', 'bad'); return; }
  const dEl = document.querySelector('[data-dec-detail="' + id + '"]');
  const detail = dEl ? String(dEl.value || '').trim() : '';
  try {
    const r = await recordAwait(postJSON('/api/decisions/answer', {
      id: id, answer: val, detail: detail, session_id: S.sessionId || '',
    }), __recordScope);
    if (!r || !r.ok) { toast('没存上：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    toast('已记下。这一步可以跑了。', 'good');
    await recordAwait(loadDecisions(), __recordScope);
    // 从决策对话框里答完，要把它关掉再重画。
    //   不关的话，人答完了对话框还挂着，看起来像没生效；
    //   而且下一张卡如果还有别的待答项，会叠在旧内容上。
    if (SHEET_STACK.length) closeSheet();
    renderRun();
    // 这一步的待答问题都答完了：把对话框收掉、让按钮恢复成"可以跑"。
    if (!decPendingFor((decItems().find(x => String(x.id) === String(id)) || {}).step || '').length) {
      renderGates();
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    toast('没存上：' + String(e.message || e), 'bad');
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}
/* 顶栏状态条。
   只留「现在能不能跑」和「还差几件」这两类信息。
   以前会显示「还没设置文件」「已答复 N」「未保存的改动 N 处」——
   前两个是过程噪音（配置在保存选择时自动生成、答复进度在闸门区看得到），
   第三个暴露的是实现细节而不是用户要关心的事，都删掉了。 */
/* 状态行只写「分析名称：X」这一件事。
   单独拆出来是因为启动阶段要提前写它一次（那时主体还没画完），
   而失败原因又不能被这一句盖掉 —— 两处调同一段代码，格式才不会漂。 */
function renderHeadSubline() {
  const sub = $('#subline');
  if (!sub) return;
  if (bootFail) { sub.textContent = bootFail; return; }
  const cur = (S.sessions || []).find(x => x.id === S.sessionId) || null;
  const nm = cur && cur.name ? cur.name : (S.sessionId ? S.sessionId : '');
  sub.innerHTML = '分析名称：<b>' + esc(nm || '还没选') + '</b>';
}

function renderHead() {
  // 状态行区分分析归属与回复路由。
  //   工作路径删掉了 —— 那条路径又长又没人看，还把这行撑成两行。
  renderHeadSubline();
  const pA = pendingDecisions().length;
  let h = '';
  if (!S.runDir) h += '<span class="pill plain">未设运行目录，运行状态未知</span>';
  else if (pA > 0) h += '<span class="pill wait"><span class="n">' + pA + '</span> 件要先定</span>';
  else if (decItems().length) h += '<span class="pill ok">可以开跑</span>';
  const t = $('#tally');
  if (t) t.innerHTML = h;
}

/* ---------------- 环境体检 ---------------- */
/* =====================================================================
   需要你决定的事
   ---------------------------------------------------------------------
   用户的原话：「我现在看有非常非常多的选择需要选，但是我其实不知道这些
   东西具体对应哪些环节，然后为什么一定要在前置进行选择。」
   所以每张卡片要回答三件事，而且只用三行：
     1. 这是什么（标题）
     2. 为什么现在就得定（一句话，不是一段）
     3. 选哪个（选项，说明它到底在选什么）
   依据数据默认折叠 —— 它是支撑材料，不是主内容。
   ===================================================================== */

/* 闸门标题改成人话。
   配置里的 title/question 是给脚本看的，直接用会有「gates.json」
   「nFeature」这种词，用户的原话是「这个 json 我不理解是什么意思」。 */
const GATE_UI = {
  data_indir: {
    t: '原始数据在哪个目录',
    why: '选过位置就自动填。',
    opts: { auto: ['用第一步选好的路径', ''], keep: ['先放着，等数据拷过来再说', '暂不推进主流程'] },
  },
  reference_source: {
    t: '判细胞类型用什么依据',
    why: '注释必须有外部依据。',
    hint: '三选一。',
    opts: {
      reference_table: ['用一份文献参考表', '论文补充材料里的 marker 表'],
      lit_dict: ['用一份 marker 词典', 'JSON，可由文献检索生成'],
      skip: ['这次不做注释', '注释图与富集图会缺'],
    },
  },
  qc_threshold: {
    t: '质控下限取多少',
    why: '低于它会被丢掉。官方有 300／400／500 三个数。',
    opts: {
      '300': ['300（照官方脚本里写的）', '官方交付最低 401，实际没用这个值'],
      '400': ['400（对齐官方交付数据实测）', '与官方交付最低 401 吻合'],
      '500': ['500（官方注释里提到的）', '官方脚本里这行被注释掉，没执行'],
    },
  },
  read_exclusion: {
    t: '读入前要不要剔样本',
    why: '实验设计决定，脚本不替你判断。',
    opts: { none: ['不剔除', ''], exclude: ['要剔除', '勾选样本'] },
  },
  cluster_filter: {
    t: '一次聚类后剔哪些簇',
    why: '只对本次有效，照抄会删错。',
    hint: '下面三选一。',
    opts: {
      none: ['都不剔', '全部进入后续'],
      by_name: ['按细胞类型名保留', '填类型名，其余剔除；需先有注释'],
      by_cluster: ['按簇号保留', '填簇号；换一次运行就全变'],
    },
  },
  post_cluster_exclusion: {
    t: '聚类后要不要剔样本',
    why: '这是实验设计决定，脚本不替你判断。',
    opts: { none: ['不剔除', ''], exclude: ['要剔除', '勾选具体哪个样本'] },
  },
  lowconfidence: {
    t: '多小的簇算低置信',
    why: '细胞太少，统计不可靠。',
    opts: {
      flag_only: ['只标记，不定阈值', '标低置信，不命名'],
      threshold: ['按细胞数定一个门槛', '低于门槛的簇标为低置信'],
    },
  },
};
const gateUI = g => GATE_UI[g.id] || {};
const gateTitle = g => (gateUI(g).t || g.title || g.id);
const gateWhy = g => (gateUI(g).why || g.question || '');

/* 选项文案：优先用我们改写过的，没写的才回落到配置里的 */
function gateOptionLabel(g, o) {
  const m = (gateUI(g).opts || {})[o.value];
  return m ? m[0] : (o.label || o.value);
}
function gateOptionDesc(g, o) {
  const m = (gateUI(g).opts || {})[o.value];
  if (m) return m[1];
  return o.desc || '';
}

function findGate(id) { return arr(S.gates && S.gates.gates).filter(g => g.id === id)[0]; }

function evTable(ev, gid) {
  if (!ev) return '<div class="small">这一项没有附带依据数据。</div>';
  const cap = ev.caption ? '<div class="evcap">' + esc(ev.caption) + '</div>' : '';
  if (ev.type === 'kv') {
    const items = arr(ev.items);
    if (!items.length) return '<div class="small">暂无数据</div>';
    return '<div class="evbox">' + cap + '<dl class="kvlist">' + items.map(it =>
      '<dt>' + esc(it.k || '') + '</dt><dd>' + esc(it.v || '') + '</dd>').join('') + '</dl></div>';
  }
  const cols = arr(ev.columns), rows = arr(ev.rows);
  if (!cols.length && !rows.length) {
    return '<div class="small">暂无数据</div>';
  }
  const LIMIT = 8;
  const key = 'ev_' + gid;
  const showAll = !!S[key];
  const list = showAll ? rows : rows.slice(0, LIMIT);
  let numCol = -1;
  cols.forEach((c, i) => { if (String(c).indexOf('细胞数') >= 0 || String(c).indexOf('占比') >= 0) numCol = i; });
  const th = cols.map(c => '<th>' + esc(c) + '</th>').join('');
  const tr = list.map(r => '<tr>' + cols.map((c, i) => {
    const cell = r[i] === undefined ? '' : r[i];
    return '<td' + (numCol === i ? ' class="num"' : '') + '>' + esc(cell) + '</td>';
  }).join('') + '</tr>').join('');
  const more = rows.length > LIMIT
    ? '<div class="morewrap"><button class="btn ghost" data-evmore="' + esc(gid) + '">'
    + (showAll ? '收起来' : '展开全部 ' + rows.length + ' 行') + '</button></div>'
    : '';
  return '<div class="evbox">' + cap + '<table class="ev"><thead><tr>' + th + '</tr></thead><tbody>' + tr + '</tbody></table>' + more + '</div>';
}

function detailSpec(g, val) {
  const id = g.id;
  if (id === 'cluster_filter') {
    if (val === 'by_name') return { type: 'text', label: '要保留的细胞类型名', ph: '例如：PT-S1, PT-S2, LOH, DCT', hint: '用逗号分隔' };
    if (val === 'by_cluster') return { type: 'text', label: '要保留的簇号', ph: '例如：0,1,2,13', warn: '簇号只对本次有效，换一次就全变。' };
  }
  if ((id === 'read_exclusion' || id === 'post_cluster_exclusion') && val === 'exclude') {
    return { type: 'samples', label: '勾选要剔除的样本' };
  }
  if (val && val !== 'none' && val !== 'auto' && val !== 'keep' && g.needs_detail === true && !/^\d+$/.test(String(val))) {
    return { type: 'text', label: '具体内容', ph: '填清楚' };
  }
  return { type: 'none' };
}

function optionsHTML(g) {
  const st = S.gateState[g.id] || {};
  const opts = arr(g.options);
  if (!opts.length) return '';
  return '<div class="opts">' + opts.map(o => {
    const sel = String(st.answer) === String(o.value);
    const sug = String(g.suggested) === String(o.value);
    const risk = /by_cluster|force_native/.test(String(o.value));
    const desc = gateOptionDesc(g, o);
    return '<label class="opt' + (sel ? ' sel' : '') + '" data-gopt="' + esc(g.id) + '" data-val="' + esc(o.value) + '">'
      + '<span class="dot"></span><span class="optmain"><span class="optlab">' + esc(gateOptionLabel(g, o))
      + (sug ? '<span class="badge-sug">建议</span>' : '')
      + (risk ? '<span class="badge-sug" style="background:#fdf3ec;color:#9d3c0d">有风险</span>' : '')
      + '</span>' + (desc ? '<span class="optdesc">' + esc(desc) + '</span>' : '') + '</span></label>';
  }).join('') + '</div>';
}

function detailHTML(g) {
  const st = S.gateState[g.id] || {};
  const spec = detailSpec(g, st.answer);
  if (spec.type === 'none') return '';
  const v = st.answer_detail;
  let inner = '';
  if (spec.type === 'text') {
    const val = v ? (Array.isArray(v) ? v.join(', ') : String(v)) : '';
    inner = '<div class="blocklabel">' + esc(spec.label) + '</div>'
      + '<input type="text" data-gdetail="' + esc(g.id) + '" value="' + esc(val) + '" placeholder="' + esc(spec.ph || '') + '">'
      + (spec.hint ? '<div class="hintline">' + esc(spec.hint) + '</div>' : '')
      + (spec.warn ? '<div class="warnbox">' + esc(spec.warn) + '</div>' : '');
  } else if (spec.type === 'samples') {
    const all = arr(g.sample_options);
    let chosen = arr(v).map(String);
    if (!Array.isArray(v) && typeof v === 'string' && v) chosen = v.split(/[,，]/).map(s => s.trim());
    inner = '<div class="blocklabel">' + esc(spec.label) + '</div>';
    if (!all.length) {
      inner += '<input type="text" data-gdetail="' + esc(g.id) + '" value="' + esc(chosen.join(', ')) + '" placeholder="填样本名，逗号分隔">'
        + '<div class="hintline">聚类后自动列出样本名。</div>';
    } else {
      inner += '<div class="sampgrid">' + all.map(s => {
        const on = chosen.indexOf(String(s)) >= 0;
        return '<label class="sampchk' + (on ? ' on' : '') + '"><input type="checkbox" data-gsamp="' + esc(g.id)
          + '" value="' + esc(s) + '"' + (on ? ' checked' : '') + '>' + esc(s) + '</label>';
      }).join('') + '</div>';
    }
  }
  return '<div class="detail">' + inner + '</div>';
}

/* 一张待决定卡片 */
function gateCard(g, idx) {
  const st = S.gateState[g.id] || { status: 'pending' };
  const hint = gateUI(g).hint || '';
  const head = '<div class="ghead"><div class="gnum">' + (idx || '·') + '</div>'
    + '<div class="gtit"><h3>' + esc(gateTitle(g)) + '</h3>'
    + (gateWhy(g) ? '<div class="gwhy">' + esc(gateWhy(g)) + '</div>' : '') + '</div></div>';

  return '<div class="gcard">' + head + '<div class="gbody">'
    + (hint ? '<div class="blocklabel">' + esc(hint) + '</div>' : '')
    + optionsHTML(g) + detailHTML(g)
    + (g.note ? '<div class="notebox">' + esc(g.note) + '</div>' : '')
    + (g.evidence ? '<details class="stgfold" style="margin-top:12px"><summary>依据数据'
        + '<span class="cnt">' + (String(g.id).indexOf('cluster') >= 0 ? '簇大小 / 样本分布 / marker' : '供判断用')
        + '</span></summary><div style="padding:12px 16px">' + evTable(g.evidence, g.id) + '</div></details>' : '')
    + '</div>'
    + '<div class="gfoot"><button class="btn primary" data-gconfirm="' + esc(g.id) + '">确定</button>'
    + '<button class="btn" data-greco="' + esc(g.id) + '">用建议值</button></div></div>';
}

/* 已定下来的一条：压成一行，需要改再展开 */
function gateDone(g) {
  const st = S.gateState[g.id] || {};
  if (st.status === 'pending') return '';
  const found = arr(g.options).filter(o => String(o.value) === String(st.answer));
  let lab = found.length ? gateOptionLabel(g, found[0]) : String(st.answer === null ? '—' : st.answer);
  if (st.answer_detail) {
    const d = Array.isArray(st.answer_detail) ? st.answer_detail.join(', ') : String(st.answer_detail);
    if (d) lab += '（' + d + '）';
  }
  const auto = st.status === 'auto';
  return '<div class="gcard done"><div style="display:flex;gap:10px;align-items:center;padding:11px 18px;flex-wrap:wrap">'
    + '<span class="tag ' + (auto ? 'auto' : 'done') + '">' + (auto ? '自动' : '已定') + '</span>'
    + '<b>' + esc(gateTitle(g)) + '</b>'
    + '<span class="small" style="flex:1;min-width:0">' + esc(lab) + '</span>'
    + '<button class="btn ghost" data-gopen="' + esc(g.id) + '">改</button>'
    + '</div></div>';
}

/* ---------------- 运行区 ---------------- */
// 服务器模式下，某一步在服务器上到哪了。
//   数据来源只有一个：助手跑完回写的 state/remote_steps.json。
//   本机看不到服务器上的进程，所以这里不回退去猜 —— 没回写就是没回写。
function remoteStep(id) {
  return (S.remoteSteps && S.remoteSteps[id]) ? S.remoteSteps[id] : null;
}
function stepBadge(id) {
  // Server writebacks are authoritative for remote runs; never let a stale local
  // stepState overwrite the stage status shown by the run board.
  if (S.needsRemote) {
    const rs = remoteStep(id);
    if (rs && ['done', 'failed', 'running', 'delegated', 'stopped'].includes(rs.status)) {
      const labels = { done: ['done', '已完成'], failed: ['failed', '服务器上没跑通'],
        running: ['running', '服务器上运行中'], delegated: ['running', '已交给助手'], stopped: ['failed', '已停止'] };
      // 已验收优先：库里这一环已经有 verified，就别再让一次没回写的
      //   delegated 把卡片挂成黄色。实测 p46899a8a-V1：annotate 与 enrich
      //   都已有 verified 行，remote_steps 里却还留着 delegated，
      //   卡片一直是黄的，人以为这步没跑完。
      //   真失败（failed/stopped）仍然优先，不能被更早的验收盖掉。
      const sgB = stageOf(id);
      const flowDoneB = !!(sgB && sgB.status === 'done');
      if (flowDoneB && rs.status !== 'failed' && rs.status !== 'stopped') {
        return { cls: 'done', txt: '已完成' };
      }
      return { cls: labels[rs.status][0], txt: labels[rs.status][1] };
    }
  }
  // 退回标记优先：被作废的步骤要先显示「待重跑」，
  //   否则用户刚退回、卡片还写着「已完成」，会以为退回没生效。
  const hs = (S.history && S.history.steps) ? S.history.steps[id] : null;
  if (hs && hs.state === 'stale') return { cls: 'failed', txt: '待重跑（已退回）' };
  if (S.blocked && S.step === id) return { cls: 'failed', txt: '没跑：还有事没定' };
  if (S.running && S.step === id) return { cls: 'running', txt: '正在跑…' };
  if (S.running && S.step === 'all') return { cls: 'running', txt: '在队列里' };
  if (S.step === id && S.code === 0) return { cls: 'done', txt: '已完成' };
  if (S.step === id && S.code !== 0 && S.code !== null) return { cls: 'failed', txt: '失败（退出码 ' + S.code + '）' };
  if (S.step === 'all' && S.code === 0) return { cls: 'done', txt: '全流程已完成' };
  return { cls: '', txt: '' };
}
/* 每个步骤下面挂它自己的环节与耗时。
   ---------------------------------------------------------------------
   为什么并到卡片里：以前「处理过程」是页面下方另一块独立清单，
   同一件事有两个说法 —— 上面卡片写"已完成"，下面清单里同一阶段还挂着"进行中"，
   看的人不知道信哪个。并进来之后，一个步骤只有一处说法。
   返回 HTML 片段；阶段对不上就返回空串，不凭空造环节。 */
function stageOf(id) {
  return arr(S.flow && S.flow.stages).find(x => x.id === id) || null;
}

function renderStageInline(sg, stepId) {
  if (!sg) return '';
  let h = '';

  // 环节条：一环一格。
  //   这一版按 gina 的要求简化到底：
  //     ① 只区分「完成」和「失败」两种可见状态，其余一律不挂符号 ——
  //        未回写、进行中这两种原来用 ○ 和 ! 表示，符号本身没人认得，
  //        再加一行图例去解释它，等于用第二段文字弥补第一段的含糊。
  //        现在不需要解释：看得见勾就是完成，看得见叉就是失败，别的什么都不画。
  //     ② 失败的格子可以点，点开就是这一环的失败原因。
  //        原因藏在 title 属性里等于没有 —— 触屏没有悬停。
  //     ③ 图例和「卡在这一环」那两行都删掉了：它们是解释上一版的符号，
  //        符号不在了，解释也没有存在的理由。
  const subs = arr(sg.subs);
  if (subs.length) {
    h += '<div class="stsubs">';
    subs.forEach((sb, i) => {
      const st = sb.status;
      const sd = fmtDur(sb.started_at, sb.ended_at);
      const live = !!sd && !sb.ended_at;
      const sdTxt = sd ? (live ? '已过 ' + sd : sd) : '';
      const name = esc(sb.title) + (sdTxt ? ' <i>' + esc(sdTxt) + '</i>' : '');
      if (st === 'failed') {
        h += '<button class="stsub bad" data-subfail="' + esc(stepId || sg.id || '')
          + '|' + i + '">✕ ' + name + '</button>';
      } else if (st === 'done') {
        h += '<span class="stsub ok">✓ ' + name + '</span>';
      } else {
        // 未回写 / 进行中 / 还没开始：不挂符号，也不解释。
        h += '<span class="stsub plain">' + name + '</span>';
      }
    });
    h += '</div>';
  }
  return h;
}

/* 这一步已经产出的文件：不论跑完没跑完都摆出来。
   为什么必须这样：
     产物原来只在「任务完成详情」里出现，而那块只在 done 时才渲染。
     这次的结果是「去双已经跑完、七个 rds 都在」，页面却什么都不显示 ——
     人只能以为没产出。产物是这件事的唯一凭据，不该藏在完成态后面。 */
/* 产物清单不再摆在卡片上。
   ---------------------------------------------------------------------
   gina 的原话：卡片上那串文件名是从上往下堆的罗列，"我感觉不好"。
   而且产物本来就该是"点开详情才看"的东西 —— 卡片是让人判断"这一步
   什么状态、要不要动手"，不是用来列文件的。
   所以这里保留函数名与语义（有没有产物、几项），但不在卡片正文渲染。 */
function stageArtCount(sg, rs) {
  const arts = sg ? arr(sg.artifacts) : [];
  return arts.length;
}

/* 通用浮层：卡片上的「任务完成详情」和失败的环节都走它。
   为什么自己建而不是用 index.html 里的 modal：
     这两处的内容是随步骤变的，塞进固定 DOM 会变成又一处要同步的地方。
     建一次、用完删掉。 */
/* 浮层可以叠：后开的盖在前面那个上面，关掉只回到上一级。
   为什么必须叠：「任务完成详情」里点缩略图看大图，关大图应该回到详情，
   而不是把详情一起关掉 —— 以前两者共用一个 #stSheet，点开大图就把详情替换掉了，
   关掉就什么都不剩，等于把人踢出流程。 */
const SHEET_STACK = [];

function sheet(title, bodyHTML, footHTML, opts) {
  const o = opts || {};
  // ---- 同一张卡就地换内容，不叠新的一层 ----
  //   gina 2026-11-19：「修改选择」里注释依据的三个选项变成三个弹窗，
  //   切一次页签就多一层，她说「切换就是切换，弹窗大小都不要变」。
  //   根因是每次重画都新建浮层、旧的不关。需要就地更新的卡带一个 key，
  //   已开着就只换标题和正文，尺寸、遮罩、层数都不动。
  if (o.key) {
    const hit = SHEET_STACK.filter(x => x.key === o.key);
    if (hit.length) {
      const top = hit[hit.length - 1];
      const hd = top.el.querySelector('.sheet-h b');
      const bd = top.el.querySelector('.sheet-b');
      if (hd) hd.textContent = title;
      if (bd) bd.innerHTML = bodyHTML;
      top.title = title;
      const ft = top.el.querySelector('.sheet-f');
      if (footHTML) {
        if (ft) ft.innerHTML = footHTML;
        else {
          const d = document.createElement('div');
          d.className = 'sheet-f';
          d.innerHTML = footHTML;
          top.el.querySelector('.sheet-box').appendChild(d);
        }
      } else if (ft && ft.parentNode) {
        ft.parentNode.removeChild(ft);
      }
      return top.el;
    }
  }
  const box = document.createElement('div');
  // 第一层保留 id=stSheet：页面里不少地方按这个 id 找浮层（刷新内容、判断开没开）。
  //   第二层起换成 stSheet2/3…，这样"关掉最上面一层"不会误伤下面那层。
  box.id = SHEET_STACK.length ? ('stSheet' + (SHEET_STACK.length + 1)) : 'stSheet';
  box.className = 'sheet' + (SHEET_STACK.length ? ' sheet-lv2' : '');
  if (o.wide) box.classList.add('sheet-wide');
  box.innerHTML = '<div class="sheet-box">'
    + '<div class="sheet-h"><b>' + esc(title) + '</b>'
    + '<button class="btn btn-sm" data-sheet-close="1">关闭</button></div>'
    + '<div class="sheet-b">' + bodyHTML + '</div>'
    + (footHTML ? '<div class="sheet-f">' + footHTML + '</div>' : '')
    + '</div>';
  box.addEventListener('click', function (e) {
    if (e.target === box || e.target.closest('[data-sheet-close]')) {
      closeSheet(box);
    }
  });
  // Esc 关最上面那一层，浅层不受影响
  box.addEventListener('keydown', function (e) {
    if (e.key === 'Escape') closeSheet(box);
  });
  document.body.appendChild(box);
  SHEET_STACK.push({ el: box, title: title, key: o.key || '' });
  // 顶部工具条：层数 >1 时给出「返回」，让人知道自己在第几层
  const hd = box.querySelector('.sheet-h');
  if (hd && SHEET_STACK.length > 1) {
    const back = document.createElement('button');
    back.className = 'btn btn-sm';
    back.textContent = '← 返回';
    back.addEventListener('click', function () { closeSheet(box); });
    hd.insertBefore(back, hd.querySelector('b'));
  }
  return box;
}

/* 关掉指定的那一层；关的是最上层时，下面那层自动恢复可见 */
function closeSheet(box) {
  const target = box || (SHEET_STACK.length ? SHEET_STACK[SHEET_STACK.length - 1].el : null);
  if (!target) return;
  const i = SHEET_STACK.findIndex(x => x.el === target);
  if (i >= 0) SHEET_STACK.splice(i, 1);
  if (target.parentNode) target.parentNode.removeChild(target);
}

/* 任务完成详情：一个可点的按钮，点开才看内容。
   ---------------------------------------------------------------------
   原来它是一个 <details> 折叠块，卡片上还要先铺一段"已产出 8 项 + 8 个
   文件名"。gina 的要求是：详情本身做成按钮，产物放进详情里。
   于是卡片只剩状态和动作，核对用的材料全部进这一层。 */
/* 详情面板开着时，取回完成后就地重画一次。
   为什么需要：详情里的格子是按当次状态生成的静态 HTML，取回成功只重画了
   「产物下载」面板，详情里那些格子会一直停在“正在取回…”，
   看起来像取回失败。只重画最上面那一层，且只在它确实是详情时动手。 */
function refreshOpenStepDetail() {
  const top = SHEET_STACK.length ? SHEET_STACK[SHEET_STACK.length - 1] : null;
  if (!top || !top.el) return false;
  const head = top.el.querySelector('.sheet-h b');
  const title = head ? String(head.textContent || '') : '';
  if (title.indexOf('任务完成详情') !== 0) return false;
  const stepId = S.detailCtx && S.detailCtx.stepId;
  if (!stepId) return false;
  const body = top.el.querySelector('.sheet-b');
  const keep = body ? body.scrollTop : 0;
  closeSheet(top.el);
  openStepDetail(stepId);
  const next = SHEET_STACK.length ? SHEET_STACK[SHEET_STACK.length - 1] : null;
  const nb = next && next.el ? next.el.querySelector('.sheet-b') : null;
  if (nb && keep) nb.scrollTop = keep;
  return true;
}

function openStepDetail(stepId) {
  const sg = stageOf(stepId);
  const rs = remoteStep(stepId);
  const title = stepTitleCN(stepId) || stepId;
  if (!sg && !rs) {
    sheet(title, '<div class="sh-p">这一步还没有任何回写记录。</div>');
    return;
  }
  const st = (sg && sg.status) || (rs && rs.status) || '';
  const code = (rs && rs.code !== null && rs.code !== undefined) ? rs.code
    : (sg && sg.code !== null && sg.code !== undefined ? sg.code : null);
  const arts = sg ? arr(sg.artifacts) : [];
  const log = String((sg && sg.exec_error) || (sg && sg.log) || (rs && rs.log) || '');
  const args = arr(sg && sg.args);
  const deps = arr(sg && sg.deps);

  // 记下这次详情属于哪条记录、哪个环节 —— 「提问」要把这些一起发进会话，
  //   否则助手只知道有个文件名，不知道是这批数据、这一步的产物。
  const curRec = arr(S.sessions).find(x => x.id === S.sessionId) || null;
  S.detailCtx = {
    stepId: stepId,
    title: title,
    recordName: curRec && curRec.name ? String(curRec.name) : '',
    runDir: S.runDir || '',
    roles: {}
  };

  // 她说这张卡要能回答四件事：吃了什么、按什么参数、怎么跑的、产出什么。
  //   以前只有一张状态表和一份文件名清单 —— 文件名看不出结果对不对，
  //   要判断必须跳到「产物下载」再找一遍，那条路还得先取回。
  //   所以结果直接摆在这里：图给缩略图、表格能点开预览，两者都能下载。
  let h = '';

  // ---- ① 输入的数据 ----
  h += '<div class="dtsec"><div class="dtsec-t">输入的数据</div>';
  if (deps.length) {
    h += '<div class="depfiles">' + deps.map(d => '<span class="depf">' + esc(String(d)) + '</span>').join('') + '</div>';
  } else {
    h += '<div class="sh-p">这一步没有登记输入文件（可能直接在服务器上读原始数据）。</div>';
  }
  h += '</div>';

  // ---- ② 输入的参数 ----
  h += '<div class="dtsec"><div class="dtsec-t">输入的参数</div>';
  if (args.length) {
    h += '<pre class="sh-log">' + esc(args.join(' ')) + '</pre>';
  } else {
    h += '<div class="sh-p">这一步没有登记运行参数。</div>';
  }
  h += '</div>';

  // ---- ③ 处理的情况 ----
  h += '<div class="dtsec"><div class="dtsec-t">处理的情况</div>';
  const rows = [];
  rows.push(['状态', (ST_CN[st] || st || '未知') + (code === null ? '' : '（退出码 ' + code + '）')]);
  const cf2 = (sg && sg.conflict) || (rs && rs.conflict) || '';
  if (cf2) {
    rows.push(['注意', '这条记录自相矛盾：状态字段写「已完成」，但' + cf2
      + '。应用按正文为准，不把它当成已完成。']);
  }
  if (sg && sg.started_at) rows.push(['开始', String(sg.started_at).replace('T', ' ')]);
  if (sg && sg.ended_at) {
    const d = fmtDur(sg.started_at, sg.ended_at);
    rows.push(['结束', String(sg.ended_at).replace('T', ' ') + (d ? '（用了 ' + d + '）' : '')]);
  } else if (sg && sg.started_at && !['failed','rejected','done'].includes(sg.status)) {
    rows.push(['至今', '还没结束，已过 ' + fmtDur(sg.started_at, '')]);
  }
  if (sg && sg.imported_at) rows.push(['回写于', String(sg.imported_at).replace('T', ' ')]);
  const host = (sg && sg.source_host) || (rs && rs.source_host) || '';
  if (host) rows.push(['跑在哪', host + ((rs && rs.exec_pc) ? ' ／ 经 ' + rs.exec_pc : '')]);
  if (sg && sg.review) {
    const vd = sg.review.verdict || (sg.review.ok ? 'pass' : 'fail');
    const vtxt = vd === 'pass' ? '通过' : (vd === 'unknown' ? '判不了' : '没过');
    const bad = [];
    (sg.review.checks || []).forEach(c => {
      if (c && c.ok === false) bad.push((c.name || '') + (c.detail ? '：' + c.detail : ''));
    });
    rows.push([sg.started_at && sg.review.imported_at && sg.review.imported_at < sg.started_at ? '历史质检（上次）' : '质检', '应用自检' + vtxt
      + (sg.review.imported_at ? '　' + String(sg.review.imported_at).replace('T', ' ') : '')
      + (bad.length ? '　' + bad.join('；') : '')]);
  }
  h += '<table class="sh-t">';
  rows.forEach(r => { h += '<tr><th>' + esc(r[0]) + '</th><td>' + esc(r[1]) + '</td></tr>'; });
  h += '</table>';
  if (log) h += '<pre class="sh-log" style="margin-top:8px">' + esc(log) + '</pre>';
  h += '</div>';

  // ---- ④ 输出的结果 ----
  //   图给缩略图（点开放大、能下载）；表格给「预览」按钮（点开是表 + 下载）。
  //   没取回的文件不能假装能看：按状态给「取回」按钮，点一下登记取回。
  const IMG = /\.(png|jpg|jpeg|gif|svg)$/i;
  const TBL = /\.(xls|xlsx|csv|tsv)$/i;
  const mirroredSet = {};
  arr(S.artifacts).forEach(a => { if (a && a.mirrored) mirroredSet[String(a.rel)] = a; });

  // 只显示这一步自己的产物。
  //   以前这里直接摊 sg.artifacts，而回写里常把整轮的文件都带上，
  //   于是"去双"的详情里能看到注释的图和别的环节的表 —— 她说"不要乱放图"。
  //   现在按环节对应的目录前缀过滤；前缀表跟流程的落盘目录一一对应。
  const STAGE_DIR = {
    env: [], doublet: ['1.doublet/'], merge: ['3.merge/'],
    markers: ['5.markers/'], filter: ['7.after_drop/'], final: ['8.final/'],
    fullmark: ['9.full/'], diff: ['9.full/'], annotate: ['9.annot/'],
    figs: ['10.figs/'], enrich: ['10.figs/4.diff_enrich/'],
    ppt: ['11.report/'], pack: ['11.report/']
  };
  const dirs = STAGE_DIR[stepId] || [];
  const inStage = rel => {
    const r = String(rel || '');
    if (!dirs.length) return true;
    // 出图与富集同属 10.figs：富集那一步只认 4.diff_enrich，出图则排除它，
    //   否则两边会各显示一份相同的文件，看着像重复出了两遍。
    if (stepId === 'enrich') return r.startsWith('10.figs/4.diff_enrich/');
    if (stepId === 'figs') return r.startsWith('10.figs/') && !r.startsWith('10.figs/4.diff_enrich/');
    if (stepId === 'ppt' || stepId === 'pack') {
      return dirs.some(d => r.startsWith(d)) && /\.(pptx|zip)$/i.test(r);
    }
    return dirs.some(d => r.startsWith(d));
  };
  const mine = arr(S.artifacts).filter(a => inStage(String(a && a.rel ? a.rel : a)));
  // 本地清单里没有（比如执行端还没列过），退回回写里带的那份，行为不变。
  const useArts = mine.length ? mine.map(a => a.rel) : arts.map(a => String(a && a.rel ? a.rel : a));

  // 先把分项算清楚，再拼 HTML（顺序颠倒会引用到还没声明的变量，整张卡就画不出来）。
  const imgs = [], tbls = [], pdfs = [], others = [];
  useArts.forEach(a => {
    const rel = String(a && a.rel ? a.rel : a);
    if (IMG.test(rel)) imgs.push(rel);
    else if (TBL.test(rel)) tbls.push(rel);
    else if (/\.pdf$/i.test(rel)) pdfs.push(rel);
    else others.push(rel);
  });

  // 同一张图常同时存 png 与 pdf，那是同一个产物，不是两张图。
  //   以前按文件数报"图片 6 张"，看着像凭空多出三张 —— 她说"产物完全是混乱的"。
  //   现在按文件名合并成一组：缩略图只出一次，格式写在名字下面。
  const imgStems = {};
  imgs.forEach(rel => { imgStems[rel.replace(/\.[^.]+$/, '')] = 1; });
  const lonePdf = [];
  pdfs.forEach(rel => {
    const stem = rel.replace(/\.[^.]+$/, '');
    if (imgStems[stem]) imgs.push(rel);
    else lonePdf.push(rel);
  });
  lonePdf.forEach(rel => others.push(rel));

  const figGroups = {};
  imgs.forEach(rel => {
    const m = rel.match(/^(.*)\.([A-Za-z0-9]+)$/);
    const stem = m ? m[1] : rel;
    const ext = m ? m[2].toLowerCase() : '';
    (figGroups[stem] = figGroups[stem] || []).push({ rel: rel, ext: ext });
  });
  const figStems = Object.keys(figGroups);
  // 计数按合并后的口径报：一张图 + 一份表 + 一个文件 = 三项，和下面的分项对得上。
  const outCount = figStems.length + tbls.length + others.length;

  h += '<div class="dtsec"><div class="dtsec-t">输出的结果'
    + ' <span class="dtsec-c">' + outCount + ' 项</span></div>';
  if (!useArts.length) {
    h += '<div class="sh-p">这一步没有登记产物。</div>';
  } else {
    if (figStems.length) {
      h += '<div class="dtgrp">图片 ' + figStems.length + ' 张</div><div class="thumbs">';
      figStems.slice(0, 16).forEach(stem => {
        const group = figGroups[stem];
        // 缩略图优先用能直接在浏览器里显示的格式
        const viewable = group.find(g => /^(png|jpg|jpeg|gif|svg)$/.test(g.ext));
        const pick = viewable || group[0];
        const has = !!mirroredSet[pick.rel];
        const nm = stem.split('/').pop();
        const exts = group.map(g => g.ext.toUpperCase()).join(' · ');
        if (has && viewable) {
          h += '<button class="thumb" data-bigimg="' + esc(pick.rel) + '" title="' + esc(stem) + '">'
            + '<img src="' + apiUrl('api/artifact/file') + '?session='
            + encodeURIComponent(S.sessionId || '') + '&rel=' + encodeURIComponent(pick.rel) + '" alt="' + esc(nm) + '">'
            + '<span class="thumb-n">' + esc(nm) + '</span>'
            + '<span class="thumb-f">' + esc(exts) + '</span></button>';
        } else {
          const ai = arr(S.artifacts).find(x => String(x.rel || '') === String(pick.rel));
          const rk = ai ? (Number(ai.fetch_rank) || 0) : 0;
          const waitLabel = rk > 0 ? ('排队第 ' + rk + ' 位') : '还没取回';
          h += '<button class="thumb empty" data-artget="' + esc(pick.rel) + '" title="点一下把它取回来">'
            + '<span class="thumb-wait">' + esc(waitLabel) + '</span>'
            + '<span class="thumb-n">' + esc(nm) + '</span>'
            + '<span class="thumb-f">' + esc(exts) + '</span></button>';
        }
      });
      h += '</div>';
      if (figStems.length > 16) {
        h += '<div class="sh-p">还有 ' + (figStems.length - 16) + ' 张，在「产物下载」里看全部。</div>';
      }
    }

    if (tbls.length) {
      h += '<div class="dtgrp">表格 ' + tbls.length + ' 份</div><div class="dtlist">';
      tbls.forEach(rel => {
        const has = !!mirroredSet[rel];
        const nm = rel.split('/').pop();
        const tdl = apiUrl('api/artifact/download') + '?session=' + encodeURIComponent(S.sessionId || '')
          + '&rel=' + encodeURIComponent(rel);
        h += '<div class="dtrow"><span class="dtrow-n">' + esc(nm) + '</span>'
          + (has
            ? '<button class="btn btn-sm" data-tableview="' + esc(rel) + '">预览</button>'
              + '<a class="btn btn-sm" href="' + tdl + '">下载</a>'
            : '<button class="btn btn-sm" data-artget="' + esc(rel) + '">取回</button>')
          + '</div>';
      });
      h += '</div>';
    }

    if (others.length) {
      h += '<div class="dtgrp">其它文件 ' + others.length + ' 份</div><div class="dtlist">';
      others.slice(0, 20).forEach(rel => {
        const has = !!mirroredSet[rel];
        // 已取回不能只写一行字：它就在本机了，这里却没有下载入口，
        //   人得再去产物面板翻一遍。能下载就直接给下载。
        const dl = apiUrl('api/artifact/download') + '?session=' + encodeURIComponent(S.sessionId || '')
          + '&rel=' + encodeURIComponent(rel);
        h += '<div class="dtrow"><span class="dtrow-n">' + esc(rel) + '</span>'
          + (has
            ? '<a class="btn btn-sm" href="' + dl + '">下载</a>'
            : '<button class="btn btn-sm" data-artget="' + esc(rel) + '">取回</button>')
          + '</div>';
      });
      h += '</div>';
    }
  }
  h += '</div>';

  // ---- ⑤ 这一步的多套参数与结果 ----
  //   她说"筛群的参数依据最终要显示到任务完成详情里"。
  //   所以候选列表从卡片搬到这里：详情本来就是"这一步到底怎么跑的、出了什么"，
  //   几套参数与各自的输出，正属于这里。
  S.detailVariantStage = '';
  if (stepId === 'filter' || stepId === 'annotate') {
    S.detailVariantStage = stepId;
    h += '<div class="dtsec" id="detailVars"><div class="dtsec-t">参数与候选结果'
      + '<span class="dtsec-c" id="detailVarsCount"></span></div>'
      + '<div id="detailVarsBody"><div class="small">正在读…</div></div></div>';
  }

  const box0 = sheet('任务完成详情 · ' + title, h);
  if (S.detailVariantStage) {
    // 弹窗建好之后再拉候选：避免把 /api/variants 的等待塞进拼串里。
    fillDetailVars(box0);
  }
}

/* 把候选列表填进详情里的那一段；顺带在标题上写数量 */
async function fillDetailVars(box) {
  const __recordScope = recordContext();
  try {

  const b = (box || document).querySelector('#detailVarsBody');
  const cnt = (box || document).querySelector('#detailVarsCount');
  const stage = S.detailVariantStage;
  if (!b || !stage) return;
  await recordAwait(loadVariants(true), __recordScope);
  const v = (S.variants && S.variants[stage]) || { items: [], chosen: '' };
  const items = v.items || [];
  if (cnt) cnt.textContent = items.length ? (items.length + ' 套') : '';
  if (!items.length) {
    b.innerHTML = '<div class="sh-p">这一步还没跑过带参数的版本。'
      + (stage === 'filter'
        ? '点卡片上的运行按钮，会先让你定 dims 与 resolution。'
        : '先用上面那张卡选一份注释依据。')
      + '</div>';
    return;
  }
  b.innerHTML = variantListHTML(stage, items, v.chosen || '');

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 详情里的选用：选完就地刷新那一段，不关弹窗 */
async function chooseVariantInDetail(stage, id) {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/variants', { action: 'choose', stage: stage, id: id, session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) { toast('选用失败：' + ((r && r.error) || ''), 'bad'); return; }
    await recordAwait(loadVariants(true), __recordScope);
    const box = document.getElementById('stSheet');
    await recordAwait(fillDetailVars(box), __recordScope);
    if (id) {
      const it = arr((S.variants && S.variants[stage] || {}).items).find(x => String(x.id) === String(id));
      const label = it ? (it.label || it.id) : id;
      const txt = ['【来自 个性化分析助手 · 选定结果】',
        S.topicId ? ('回复路由会话号：' + S.topicId) : '',
        '',
        '环节 ' + stage + ' 已经定了一套：' + label,
        it && it.outdir ? ('产物目录：' + it.outdir) : '',
        '',
        '这里只同步已选参数，请只读确认；不要因此提交或重跑任何作业。其它候选保留不删。'].filter(Boolean).join('\n');
      try { await recordAwait(injectToSession(txt, function () {}, 'query', {step:stage, reqId:''}, '同步已选参数（详情）'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 /* 通道不通不影响选定 */ }
      toast('已选定：' + label, 'good');
    } else {
      toast('已取消选用。', '');
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('选用失败：' + String(e.message || e), 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 大图：点缩略图看大图，右上角可下载。
   为什么不在原卡片里放大：卡片宽度有限，图一放大就把文字挤跑；
   单独一层能占满屏，也能顺手把下载按钮放在手边。 */
/* 看大图。参数里带上"同一环节的全部图"，这样才能左右翻。
   为什么要在这一层翻：她说"一个环节如果有多张图，希望有左右切换"——
   看完一张要关掉、再点下一张，核对一组图时非常费手。 */
function openBigImage(rel, list) {
  const imgs = arr(list).filter(Boolean);
  if (!imgs.length) imgs.push(rel);
  let idx = Math.max(0, imgs.indexOf(rel));

  function html() {
    const cur = imgs[idx];
    const src = apiUrl('api/artifact/file') + '?session=' + encodeURIComponent(S.sessionId || '')
      + '&rel=' + encodeURIComponent(cur);
    const dl = apiUrl('api/artifact/download') + '?session=' + encodeURIComponent(S.sessionId || '')
      + '&rel=' + encodeURIComponent(cur);
    const multi = imgs.length > 1;
    let h = '<div class="bigwrap">'
      + (multi && idx > 0
          ? '<button class="bignav prev" data-bignav="-1" title="上一张">‹</button>' : '')
      + '<img src="' + src + '" alt="">'
      + (multi && idx < imgs.length - 1
          ? '<button class="bignav next" data-bignav="1" title="下一张">›</button>' : '')
      + '</div>';
    h += '<div class="bigbar">'
      + '<span class="bigpath" title="' + esc(cur) + '">' + esc(cur) + '</span>'
      + (multi ? '<span class="bigcount">第 ' + (idx + 1) + ' / ' + imgs.length + ' 张</span>' : '')
      + '<button class="btn" data-bigask="1">提问</button>'
      + '<a class="btn primary" href="' + dl + '">下载这张图</a>'
      + '<button class="btn" data-sheet-close="1">关闭看大图</button>'
      + '</div>';
    // 「提问」点开才出现输入框：平时不占地方，要用时就在图下面。
    h += '<div class="askbox" id="askBox" style="display:none">'
      + '<textarea id="askInput" class="l1ask" placeholder="针对这张图想问什么？例如：这几个样本的线粒体比例是不是偏高"></textarea>'
      + '<div class="btnrow" style="margin-top:8px">'
      + '<button class="btn primary" data-bigaskgo="1">连同这张图和它的信息发进会话</button>'
      + '<span class="small" id="askMsg"></span></div></div>';
    return h;
  }

  const box = sheet(String(rel).split('/').pop(), html(), null, { wide: true });
  box.classList.add('sheet-big');

  function redraw() {
    const b = box.querySelector('.sheet-b');
    if (b) b.innerHTML = html();
    const hd = box.querySelector('.sheet-h b');
    if (hd) hd.textContent = String(imgs[idx]).split('/').pop();
  }

  box.addEventListener('click', function (e) {
    const nav = e.target.closest('[data-bignav]');
    if (nav) {
      const d = Number(nav.getAttribute('data-bignav'));
      const nx = idx + d;
      if (nx >= 0 && nx < imgs.length) { idx = nx; redraw(); }
      return;
    }
    if (e.target.closest('[data-bigask]')) {
      const b = box.querySelector('#askBox');
      if (b) {
        const on = b.style.display !== 'none';
        b.style.display = on ? 'none' : '';
        if (!on) box.querySelector('#askInput')?.focus();
      }
      return;
    }
    if (e.target.closest('[data-bigaskgo]')) {
      const q = box.querySelector('#askInput');
      askAboutArtifact(imgs[idx], q ? q.value : '', box.querySelector('#askMsg'));
      return;
    }
  });

  // 键盘左右也能翻：核对一组图时比点按钮快
  box.addEventListener('keydown', function (e) {
    if (e.key === 'ArrowLeft' && idx > 0) { idx -= 1; redraw(); }
    else if (e.key === 'ArrowRight' && idx < imgs.length - 1) { idx += 1; redraw(); }
  });
}

/* 针对一张图/一份文件提问：把图、它的信息、人的问题一起发进会话。
   为什么要把这些信息都拼进去：助手看不到这个页面，
   只说"这张图"它不知道是哪一张；只说路径它也判断不了在哪个环节出的、
   该不该夸样本比较 —— 环节与记录决定了解读的分寸。 */
async function askAboutArtifact(rel, question, msgEl) {
  const __recordScope = recordContext();
  try {

  const q = String(question || '').trim();
  if (!q) { if (msgEl) msgEl.textContent = '先写一句想问什么。'; return; }
  const ctx = S.detailCtx || {};
  const stageId = ctx.stepId || '';
  const stageName = ctx.title || (stageId ? (stepTitleCN(stageId) || stageId) : '');
  const recName = ctx.recordName || '';
  const runDir = ctx.runDir || S.runDir || '';
  const role = ctx.roles && ctx.roles[rel] ? ctx.roles[rel] : '';
  const full = runDir ? (runDir.replace(/\/+$/, '') + '/' + rel) : rel;

  const L = [];
  L.push('【来自 个性化分析助手 · 针对一张结果提问】');
  if (S.topicId) L.push('回复路由会话号：' + S.topicId + '（回答请送回这里）');
  L.push('');
  L.push('我的问题：' + q);
  L.push('');
  L.push('这张结果的信息：');
  if (recName) L.push('  处理记录：' + recName);
  if (stageName) L.push('  所属环节：' + stageName + (stageId ? '（' + stageId + '）' : ''));
  L.push('  文件：' + rel);
  L.push('  服务器路径：' + full);
  if (role) L.push('  作用：' + role);
  L.push('');
  L.push('如果回答需要看图和表本身，请到上面的路径读取；读不到就明说读不到。');

  const text = L.join('\n');
  if (msgEl) msgEl.textContent = '正在发进会话…';
  try {
    const r = await recordAwait(injectToSession(text, function () {}, 'query', undefined, '就这份产物提问'), __recordScope);
    if (msgEl) {
      msgEl.textContent = r.ok ? '已发进会话，等它回答。'
        : ('没发出去：' + (r.why || r.code || '原因不明') + (r.queued ? '（已转存队列）' : '（未发送、未入队）'));
    }
    if (r.ok) toast('已发进会话。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msgEl) msgEl.textContent = '发送失败：' + String(e.message || e);
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 点失败的环节：看这一环为什么失败。
   这是 gina 明确要的交互 —— 失败不靠一串符号和一行图例去解释，
   点那个叉就能看到原因。所以原因本身必须是可以被点开的东西。 */
function openSubFail(stepId, idx) {
  const sg = stageOf(stepId);
  const subs = sg ? arr(sg.subs) : [];
  const sb = subs[Number(idx)];
  const title = stepTitleCN(stepId) || stepId;
  if (!sb) {
    sheet('失败原因', '<div class="sh-p">没有这一环的记录。</div>');
    return;
  }
  const task = currentStepTask(stepId, sg.exec_id);
  const note = String(sb.note || '').trim();
  const reason = note && note !== '回写为完成' ? note
    : (sg.exec_status === 'failed' && sg.exec_error)
      || (task && task.status === 'failed' && task.error) || '';
  const fallback = errKeyLines(String((sg && sg.log) || '')).join('\n');
  // 报错弹窗也按"这是什么 / 报错原文 / 同一阶段的日志"分层，
  //   和任务完成详情同一套版式 —— 她说这些弹窗"太简朴，看不出细节"。
  let h = '';
  h += '<div class="dtsec dtsec-bad">'
    + '<div class="dtsec-t">哪一环失败了</div>'
    + '<div class="failhead"><b>' + esc(sb.title || '未命名环节') + '</b>'
    + '<span class="failtag">失败</span>'
    + (sb.started_at ? '<span class="failtime">开始 ' + esc(String(sb.started_at).replace('T', ' ')) + '</span>' : '')
    + (sb.ended_at ? '<span class="failtime">结束 ' + esc(String(sb.ended_at).replace('T', ' ')) + '</span>' : '')
    + '</div></div>';

  h += '<div class="dtsec"><div class="dtsec-t">报错原文</div>';
  if (reason) {
    h += '<pre class="sh-log fail-log">' + esc(reason) + '</pre>';
  } else {
    h += '<div class="sh-p">这一环没有写失败原因。点卡片上的「再报一次」，让它把报错取回来。</div>';
  }
  h += '</div>';

  if (fallback && fallback !== reason) {
    h += '<div class="dtsec"><div class="dtsec-t">同一阶段日志里的报错行</div>'
      + '<pre class="sh-log">' + esc(fallback) + '</pre></div>';
  }
  // 整步日志放在最后且默认收起：多数时候上面两段已经够定位，
  //   但真要查上下文时不能没有。
  const fullLog = String((sg && sg.log) || '');
  if (fullLog) {
    h += '<details class="faillogmore"><summary>整步执行记录（点开）</summary>'
      + '<pre class="sh-log">' + esc(fullLog) + '</pre></details>';
  }
  sheet('失败原因 · ' + title, h);
}

/* 任务完成详情：改成一个按钮，内容在浮层里。
   ---------------------------------------------------------------------
   原来是 <details> 折叠块，卡片上还要先铺一段"已产出 8 项 + 一串文件名"。
   gina 的要求是：详情做成可点的按钮，产物放进详情里面。
   所以卡片上只留一句「查看详情」，核对材料全部进 openStepDetail()。 */
function renderStageDone(sg, rs, stepId) {
  if (!sg && !rs) return '';
  const arts = sg ? arr(sg.artifacts) : [];
  const label = '任务完成详情' + (arts.length ? '（产物 ' + arts.length + ' 项）' : '');
  return '<button class="btn btn-sm stmore" data-stepdetail="' + esc(stepId) + '">'
    + esc(label) + '</button>';
}

/* 失败卡：把"为什么不行"直接摊在卡片上。
   ---------------------------------------------------------------------
   这是上一版最要命的地方：报错只在中间对话里，卡片上只有一句
   「服务器上没跑通（退出码 1）」。人必须在两个窗口之间自己对照，
   才知道到底怎么了。日志一直在手上，只是被"只在完成态渲染"挡掉了。
   现在失败也渲染，而且不是折叠的 —— 失败时最需要的那句话不该要点开。 */
function errKeyLines(txt) {
  const s = String(txt || '');
  if (!s) return [];
  const out = [];
  const push = function (x) {
    const t = String(x || '').replace(/\s+/g, ' ').trim();
    if (!t) return;
    // 去重不是"一模一样才跳"：同一句报错常常既出现在「报错原文：」整段里，
    //   又在后面单独成句再出现一次。子串关系也算重复，否则卡片上会念两遍。
    for (let i = 0; i < out.length; i += 1) {
      if (out[i] === t || out[i].indexOf(t) >= 0 || t.indexOf(out[i]) >= 0) return;
    }
    if (out.length < 4) out.push(t);
  };
  // ① 优先取回写里明写的「报错原文：」之后那一段 —— 那是最贴近机器的原话。
  const m = s.match(/报错原文[：:]\s*([^。\n]{4,160})/);
  if (m) push(m[1].replace(/\s*\/\s*/g, ' ／ '));
  // ② 再按句子找带错误特征的片段（R 的 Error、退出码、缺失产物）。
  s.split(/[\n；;。]/).forEach(function (seg) {
    if (/Error:|Execution halted|command not found|退出码|缺失|没跑通|失败/.test(seg)) push(seg);
  });
  return out;
}

function renderStageFail(sg, rs) {
  // Failure details belong in the step-detail sheet, not inline in the grid card.
  return '';
}

/* 卡片里的"细节"折叠区。
   把每小环进度与失败原因一起收进去：三列布局下卡片很窄，
   常驻展开会把一行三个环节拉成三根长条，扫视时反而看不出哪个在跑、哪个失败。
   失败时默认展开，因为出错不必再点一下。 */
function stageFoldHTML(sg, rs, stepId) {
  const hasDetails = !!(sg || rs);
  if (!hasDetails) return '';
  const failed = (rs && rs.status === 'failed') || (sg && sg.status === 'failed');
  const nSub = arr(sg && sg.subs).length;
  const label = failed ? '查看失败原因' : (nSub ? '查看环节详情（' + nSub + ' 项）' : '查看环节详情');
  return '<button class="btn btn-sm stmore" data-stepdetail="' + esc(stepId) + '">' + esc(label) + '</button>';
}

function stageAcceptance(stage, remote) {
  if (!stage || stage.conflict || (remote && remote.conflict)) return false;
  if (['queued', 'running', 'failed', 'stopped'].includes(String(stage.exec_status || ''))) return false;
  const review = stage.review;
  return stage.status === 'done' && !!review && review.ok === true && review.verdict === 'pass';
}
function stageCoveredAcceptance(stage, flow) {
  if (!stage || stage.covered !== true || !stage.cover_by || stage.conflict) return false;
  if (['queued', 'running', 'failed', 'stopped'].includes(String(stage.exec_status || ''))) return false;
  const remote = remoteStep(stage.id);
  if (remote && remote.conflict) return false;
  const upstream = arr(flow && flow.stages).find(x => x.id === stage.cover_by);
  return stageAcceptance(upstream, upstream && remoteStep(upstream.id));
}
function coveredStageCount(flow) {
  return arr(flow && flow.stages).filter(stage => stageCoveredAcceptance(stage, flow)).length;
}
function acceptedStageCount(flow) {
  return arr(flow && flow.stages).filter(stage =>
    stageAcceptance(stage, remoteStep(stage.id)) || stageCoveredAcceptance(stage, flow)).length;
}
function retryStatusPresentation(stage, remote) {
  if (stage && ['queued', 'running'].includes(String(stage.exec_status || stage.status)))
    return {label: '本次任务正在执行', cls: 'warn', mark: '…', time: '上次结束于'};
  if (stageAcceptance(stage, remote))
    return {label: '已验收', cls: 'ok', mark: '✓', time: '完成于'};
  if ((stage && stage.conflict) || (remote && remote.conflict))
    return {label: '执行与结果记录有冲突', cls: 'warn', mark: '!', time: '结束于'};
  if ((stage && (stage.status === 'failed' || stage.exec_status === 'failed')) || (remote && remote.status === 'failed'))
    return {label: '上次执行失败', cls: 'fail', mark: '×', time: '结束于'};
  if (stage && stage.review && stage.review.ok === false)
    return {label: '结果验收未通过', cls: 'warn', mark: '!', time: '结束于'};
  if ((stage && stage.status === 'done') || (remote && remote.status === 'done'))
    return {label: '已收到结果，等待验收', cls: 'warn', mark: '…', time: '结束于'};
  if ((stage && (stage.status === 'stopped' || stage.exec_status === 'stopped')) || (remote && remote.status === 'stopped'))
    return {label: '上次任务已停止', cls: 'warn', mark: '!', time: '结束于'};
  return {label: '尚无已验收结果', cls: 'warn', mark: '!', time: '结束于'};
}

function stageAction(sg) {
  const coverId = String(sg && sg.cover_by || '');
  const active = !!(sg && ['queued','running'].includes(sg.exec_status || sg.status));
  return {coverId, active, selectable: !coverId && !active,
    detailId: coverId || (active ? sg.id : '')};
}
function selectableStep(id) { return stageAction(stageOf(id)).selectable; }

function renderRun() {
  // 待答的决策按"这一步"数：整页按钮该不该禁用、提示几件，都用它。
  const pA = pendingDecisions().length;
  const pB = 0;
  // 服务器模式：这一步不在本机跑，但按钮不能是死的。
  //   点了就把这步变成一条任务交给助手，状态显示"交给谁了、在等什么"。
  const remote = !!S.needsRemote;
  // 「跑分析」里只列真正在跑分析的那几步。
  //   环境体检 / 自动判定 / 容量预检 这三步不属于这里：它们是「保存并识别」
  //   那一趟顺带做完的事（用户的原话是「为什么都在跑分析这个环节里」）。
  const PRE = ['env', 'decide', 'precheck'];
  const runSteps = S.steps.filter(s => PRE.indexOf(s.id) < 0);

  $('#stepsHost').innerHTML = runSteps.map((s, i) => {
    const b = stepBadge(s.id);
    const sg = stageOf(s.id);
    const blockers = blockingTitles(s.id);
    const blockedNow = blockers.length > 0;
    const rs = remoteStep(s.id);
    const dl = (currentDelegated() && S.delegated.step === s.id) ? S.delegated : null;

    // 卡片底色跟着阶段真实状态走，不只跟本机运行状态走。
    //   服务器模式本机从来不会 running，只看本机状态的话，跑完的卡片也不会变色。
    let cls = b.cls;
    // 已验收优先于 remote_steps 的残留状态。
    //   实测 p46899a8a-V1：annotate 在库里已 verified，但 remote_steps 还挂着
    //   一次没回写的 delegated，卡片就一直显示黄色。真失败（failed/stopped）
    //   仍然优先，不能因为更早有 verified 就把后续失败盖掉。
    const flowDone = !!(sg && sg.status === 'done');
    if (remote && rs && ['done', 'failed', 'running', 'delegated', 'stopped'].includes(rs.status)) {
      if (flowDone && rs.status !== 'failed' && rs.status !== 'stopped') cls = 'done';
      else cls = rs.status === 'done' ? 'done' : (rs.status === 'failed' || rs.status === 'stopped' ? 'failed' : 'running');
    } else if (!cls && sg) {
      if (sg.status === 'done') cls = 'done';
      else if (sg.status === 'running' || sg.status === 'delegated') cls = 'running';
      else if (sg.status === 'failed') cls = 'failed';
    }

    let stateTxt = b.txt;
    if (sg) {
      const sm = FLOW_ST_CN[sg.status] || [sg.status || '', ''];
      if (!stateTxt) stateTxt = sm[0];
    }
    // ---- 优先级：执行队列事实 > 回写记录 > 聊天投递 --------------------
    //   实测踩到的坑：作业 60739 已经在 master01 上跑，
    //   /api/flow 也返回 doublet running + exec_id，
    //   可页面先按 rs.status='delegated' 写成"已交给助手"，
    //   再被 rs.display_status='handoff_unconfirmed' 覆盖成"交接未确认"，
    //   最后又被 dl.ok=true 覆盖成"已送进会话"。
    //   三条旧判据一路盖掉最新的事实，人看到的是三手前的结论。
    //   现在改成：只要有 exec_id/exec_status，就以它为准，下面几条一律不再改。
    const exStatus = sg && sg.exec_status ? String(sg.exec_status) : '';
    const exId = sg && sg.exec_id ? String(sg.exec_id) : '';
    const exJob = sg && sg.exec_job ? String(sg.exec_job) : '';
    if (exId || exStatus) {
      if (exStatus === 'running') {
        stateTxt = '执行端正在跑' + (exJob ? '（作业 ' + exJob + '）' : '') + '，等它回写';
        cls = 'running';
      } else if (exStatus === 'queued') {
        // queued 只能说"已入队"。集群是否已提交由执行端 claim 之后才知道，
        //   这里写成"已提交作业"就是在替执行端宣布它还没做的事。
        stateTxt = '已入队，等执行端取走提交' + (exId ? '（' + exId + '）' : '');
        cls = 'running';
      } else if (exStatus === 'done') {
        stateTxt = '执行端已完成，等回写确认';
        cls = 'running';
      } else if (exStatus === 'failed') {
        stateTxt = sg.exec_error ? (/目标端版本与包内不一致/.test(sg.exec_error) ? '报告脚本版本不一致，尚未提交计算；请先同步版本，再重试' : '执行失败，点击查看失败原因') : '执行端报失败，尚待回写说明';
        cls = 'failed';
      }
      if (rs && rs.status === 'done') {
        stateTxt = '已在服务器上跑成'
          + (rs.imported_at ? '（' + String(rs.imported_at).replace('T', ' ') + '）' : '');
        cls = 'done';
      }
    } else if (remote) {
      if (rs && rs.status === 'running') stateTxt = '助手在服务器上跑';
      else if (rs && rs.status === 'delegated') stateTxt = rs.display_status === 'handoff_unconfirmed' ? '交接未确认，尚无执行回写' : '已交给助手，等它动手';
      else if (rs && rs.status === 'done') stateTxt = '已在服务器上跑成'
        + (rs.imported_at ? '（' + String(rs.imported_at).replace('T', ' ') + '）' : '');
      else if (rs && rs.status === 'failed') stateTxt = '服务器上没跑通'
        + (rs.code === null || rs.code === undefined ? '' : '（退出码 ' + rs.code + '）');
      // relayed：会话通道断了，但指令已落进本机队列，助手会取走执行。
      //   这两种必须分开写：一个是"没交出去"，一个是"换条路交出去了"。
      const executionRecorded = rs && ['running', 'done', 'failed', 'stopped'].includes(rs.status);
      if (!executionRecorded && dl && dl.ok === false) stateTxt = dl.relayed
        ? '已放进助手队列，会取走执行' : '没送进会话';
      if (!executionRecorded && dl && dl.ok === true) stateTxt = '已送进会话，助手会接着做';
    }
    // 「交接未确认」只在完全没有任何执行记录时才成立。
    //   有 exec_id 就说明作业已经派出去了，这句话与事实相反。
    if (!exId && !exStatus &&
        ((sg && sg.status === 'handoff_unconfirmed') || (rs && rs.display_status === 'handoff_unconfirmed'))) {
      if (!(dl && dl.ok === true)) { stateTxt = '交接未确认，尚无执行回写'; cls = 'warnc'; }
    }

    // 状态字段说"完成"、回写正文说"没跑"——这种记录不能画成绿的。
    //   gina 2026-10-28 就是被这一条卡住的：详情里状态写已完成，
    //   底下执行记录原文却是「不写成完成」。同一个窗口里两句话打架，
    //   人只能得出"这应用不可信"。这里以正文为准显示，并说明矛盾在哪。
    const conflict = (rs && rs.conflict) || (sg && sg.conflict) || '';
    if (conflict) {
      stateTxt = '记录自相矛盾：状态字段说完成，回写正文说没跑';
      cls = 'warnc';
    }

    // 已被上游覆盖的步骤：显示"已完成"，并说明东西在哪。
    //   这两步（全量 marker / 组间差异）要的产出，终版已经算出来了。
    //   原来它们显示"还没跑"、点了报"脚本不存在"，看起来像流程断了一截。
    const cov = stageCoveredAcceptance(sg, S.flow);
    if (cov) {
      stateTxt = '已完成（由「' + coverTitle(sg.cover_by) + '」一并算出，不重算）';
      cls = 'done';
    }

    // 回写完成和验收通过分别显示；新一次作业在运行时不沿用旧的通过结论。
    const accepted = stageAcceptance(sg, rs);
    const activeExecution = ['queued', 'running'].includes(exStatus);
    const resultAwaitingReview = !conflict && !activeExecution && !cov && !accepted &&
      ((rs && rs.status === 'done') || (rs && rs.code === 0 && sg && sg.review && sg.review.ok === false));
    if (conflict) {
      stateTxt = '执行与结果记录有冲突，请查看详情'; cls = 'warnc';
    } else if (!activeExecution && cov) {
      stateTxt = '已验收（产物由「' + coverTitle(sg.cover_by) + '」提供）';
      cls = 'done';
    } else if (!activeExecution && accepted) {
      stateTxt = '已验收'
        + (sg.review.imported_at ? '（' + String(sg.review.imported_at).replace('T', ' ') + '）' : '');
      cls = 'done';
    } else if (resultAwaitingReview) {
      stateTxt = sg && sg.review && sg.review.ok === false
        ? '计算已结束，结果验收未通过；查看详情处理' : '已收到结果，等待验收';
      cls = 'warnc';
    } else if (!rs && !exId && (!sg || sg.status === 'todo')) {
      stateTxt = '尚无运行记录';
    }

    // 已完成过就换个说法：不是"再开一次"，而是"带着新要求再来一次"。
    //   这两件事对人是不同的：前者像首次执行，后者才说明这一步已有结果，
    //   点它是为了重跑或补做，所以要给个地方写清楚"这次要怎样"。
    //   这一版把说法写死成动作本身（"带新要求重跑"），不再用两种近义说法
    //   让人猜区别；失败态明确写"按原样重跑"，因为失败后要的是同一件事再来一次。
    const finished = (sg && sg.status === 'done') || (rs && rs.status === 'done');
    const failed = (sg && sg.status === 'failed') || (rs && rs.status === 'failed');
    // 已被上游覆盖的步骤（全量 marker / 组间差异）：产出随终版一并生成。
    //   它们确实有产出，但不该有"独立跑一次"这个动作 —— flow_execution
    //   会明确拒绝 covered 步骤，按钮写成能跑就是骗人。
    //   给的是"看上游那一步的进度"，因为真正决定这两步结果的是终版。
    const action = stageAction(sg);
    const coverId = action.coverId;
    if (coverId && !cov && !conflict) {
      stateTxt = '随「' + coverTitle(coverId) + '」一并生成，等待结果验收';
      cls = '';
    }
    let btnLabel;
    if (coverId) btnLabel = '查看「' + coverTitle(coverId) + '」进度';
    else if (action.active) btnLabel = '查看运行进度';
    else if (resultAwaitingReview) btnLabel = '查看验收详情';
    else if (blockedNow) btnLabel = '先定 ' + blockers.length + ' 件';
    else if (finished) btnLabel = '带新要求重跑';
    else if (failed) btnLabel = '按原样重跑这一环';
    else if (remote) btnLabel = '交给助手去跑';
    else btnLabel = (S.running && S.step === s.id) ? '运行中' : '开始这一步';

    const tip = blockedNow
      ? '还差：' + blockers.slice(0, 3).join('；') + (blockers.length > 3 ? ' 等' : '')
      : (finished ? '这一步已有结果，点开可以写清这次要它怎么做'
        : (failed ? '这一步失败过，点开可以写清要它怎么修再跑' : ''));

    // 被待答问题挡住时，按钮改成"打开决策对话框"。
    //   为什么必须改这一处：页面上的「需要你决定的事」整块已经删掉
    //   （她要的是该问的时候弹窗问），renderGates 只剩空壳、提前 return，
    //   于是 pending 只有计数、没有任何可点的地方 —— 按钮写着"先定 N 件"
    //   却带着 disabled，人点不动，流程当场死在这一步。实测就是这么卡的。
    const btnAttr = action.detailId ? ' data-stepdetail="' + esc(action.detailId) + '"'
      : resultAwaitingReview ? ' data-stepdetail="' + esc(s.id) + '"' : blockedNow
      ? ' data-decopen="' + esc(s.id) + '"'
      : ((finished || failed)
        ? ' data-req="' + esc(s.id) + '"'
        : ' data-run="' + esc(s.id) + '"');

    // 卡片只留四行：标题、一行状态、环节格、按钮组。
    //   原来这里还有：一段步骤说明（"簇大小、簇 top15 marker、供筛群判断用"）、
    //   一行状态、一行环节、一行"已产出 N 项 + 一串文件名"、一个折叠的详情。
    //   gina 的原话是"很多解释性文字都是这样从上往下的罗列，我感觉不好" ——
    //   这些内容各自都有更合适的位置：说明进详情，产物进详情，符号不再解释。
    const dur = sg && sg.status !== 'handoff_unconfirmed' && !(sg.status === 'failed' && !sg.ended_at) ? fmtDur(sg.started_at, sg.ended_at) : '';
    const live = !!dur && sg && !sg.ended_at;
    const arts = sg ? arr(sg.artifacts) : [];
    const meta = [stateTxt];
    if (dur) meta.push(live ? ('已过 ' + dur) : dur);
    if (arts.length) meta.push('产物 ' + arts.length + ' 项');

    // 按钮组：四个动作同一套样式，只有主行动带 primary。
    //   上一次是"带新要求重跑"用 btn、另外两个用 btn-sm，并排时高度和边框都不一样。
    // 被决策挡住时按钮必须可点：它就是打开决策卡的那个入口。
    const btnDisabled = (!blockedNow && (S.running && !remote && !finished)) ? ' disabled' : '';
    let btns = '<button class="btn stbtn' + (finished || failed ? '' : ' primary') + '"' + btnAttr
      + btnDisabled + '>' + esc(btnLabel) + '</button>';
    btns += '<button class="btn stbtn" data-steppoke="' + esc(s.id) + '"'
      + ((remote && !coverId) ? '' : ' disabled')
      + ' title="' + (coverId ? '这一步没有自己的执行记录，随上游一并生成' : '请执行端再报一次回写') + '"'
      + '>再报一次</button>';
    btns += '<button class="btn stbtn stmore" data-stepdetail="' + esc(s.id) + '">任务完成详情</button>';

    return '<div class="stcard ' + cls + '">'
      + '<h4>'
      // 勾选框：用来"选几个环节按顺序一起跑"。
      //   以前只能一环一环点，四个环节点四次、每次等一轮。
      //   勾在这里不改原有单点行为：不勾就是原来的按钮。
      + ((S.pickMode && action.selectable) ? '<input type="checkbox" class="stpick" data-mpick="' + esc(s.id) + '"'
        + (S.picked && S.picked.indexOf(s.id) >= 0 ? ' checked' : '')
        + ' title="勾选后可让它和其它环节按顺序一起跑">' : '')
      + '<span class="stnum">' + (STEP_ICON[s.id] || (i + 1)) + '</span>' + esc(s.title)
      + (blockedNow ? '<span class="stgate">还差 ' + blockers.length + ' 件</span>' : '') + '</h4>'
      + '<div class="stmeta">' + esc(meta.join('　·　')) + '</div>'
      // 细节（每小环的进度、失败原因）收进折叠区。
      //   她说"一行三个环节，内容多的就点展开"：三列布局下卡片本来就窄，
      //   把这些内容常驻摊开会把每张卡撑成一整屏，三个并排就变成三根长条。
      //   失败时默认展开 —— 出错了不需要再点一下才看得到原因。
      + stageFoldHTML(sg, rs, s.id)
      // 参数与候选结果的位置：筛群的多套参数落在这里。
      //   放在按钮上方，因为"先定参数、再点跑"才是顺序。
      //   注释依据不放进这个异步面板：那个面板要等一次网络往返才填内容，
      //   而卡片每 5 秒整块重建一次，中间那段空档就成了"按钮一直在闪"。
      //   改由 annotRefBarHTML() 跟卡片同步生成，天生不会缺一段。
      + (s.id === 'filter' ? '<div class="varpanel" id="varpanel-' + esc(s.id) + '"></div>' : '')
      + (s.id === 'annotate' ? annotRefBarHTML() : '')
      + '<div class="stbtns">' + btns + '</div>'
      + '<div class="stqmsg" id="stq-' + esc(s.id) + '"></div>'
      + '</div>';
  }).join('');

  const rsHost = $('#runState');
  if (remote) {
    const observed = Object.values(S.remoteSteps || {});
    const counts = {};observed.forEach(x => {const name = ST_CN[x.status] || '状态未知';counts[name] = (counts[name] || 0) + 1;});
    const dl = currentDelegated();
    // 执行队列里有没有正在跑/在排的：这一层比"回写状态"更新，
    //   它是执行端自己的记录，不是助手转述的。有就先用它说一句实话，
    //   免得整行还停在"已交接，待执行确认"这种三手前的结论。
    const exRows = [];
    (S.flow && arr(S.flow.stages) || []).forEach(x => {
      if (!x || !x.exec_id) return;
      exRows.push(x);
    });
    const exRun = exRows.filter(x => String(x.exec_status) === 'running');
    const exQue = exRows.filter(x => String(x.exec_status) === 'queued');
    if (exRun.length) {
      const a = exRun[0];
      rsHost.className = 'runstate running';
      rsHost.textContent = '执行端正在跑：' + (a.title || a.id)
        + (a.exec_job ? '（作业 ' + a.exec_job + '）' : '')
        + '；其余阶段以回写为准';
    } else if (exQue.length) {
      const a = exQue[0];
      rsHost.className = 'runstate running';
      rsHost.textContent = '已入队：' + (a.title || a.id) + '，等执行端取走提交';
    } else if (S.flow && S.flow.progress) {
      const total = Number(S.flow.progress.total || 0);
      const accepted = acceptedStageCount(S.flow);
      const failed = arr(S.flow.stages).filter(x => x.status === 'failed');
      rsHost.className = 'runstate';
      rsHost.textContent = '已完成 ' + accepted + ' / ' + total + ' 个阶段'
        + (coveredStageCount(S.flow) ? '，其中 ' + coveredStageCount(S.flow) + ' 项由上游产物验收覆盖' : '')
        + (failed.length ? '；' + failed.length + ' 个阶段需处理，详见对应卡片' : '；后续状态见各步骤');
    } else if (observed.length) {
      rsHost.className = 'runstate'; rsHost.textContent = '验收进度尚未读取 · '
        + Object.entries(counts).map(([name,n]) => name + ' ' + n + ' 阶段').join('；');
    }
    else if (dl && dl.ok === false) { rsHost.className = 'runstate failed'; rsHost.textContent = ''; }
    else if (dl && dl.ok === true) { rsHost.className = 'runstate running'; rsHost.textContent = '已交给助手：' + dl.title; }
    else { rsHost.className = 'runstate'; rsHost.textContent = '服务器模式 · 以接收回写为准'; }
  }
  else if (S.running) { rsHost.className = 'runstate running'; rsHost.textContent = '正在跑' + (S.step === 'all' ? '（全流程）' : '：' + S.step); }
  else if (S.blocked) { rsHost.className = 'runstate failed'; rsHost.textContent = '被拦住了：还有事没定'; }
  else if (S.code === 0 && S.step) { rsHost.className = 'runstate done'; rsHost.textContent = '上一步成功'; }
  else if (S.code !== null && S.code !== undefined && S.code !== 0 && S.step) { rsHost.className = 'runstate failed'; rsHost.textContent = '上一步没跑通（退出码 ' + S.code + '）'; }
  else { rsHost.className = 'runstate'; rsHost.textContent = S.runDir ? '空闲' : '还没定运行目录'; }

  // 没有运行目录就不算就绪：连跑到哪、结果写哪都还没定。
  const ready = !!S.runDir;
  $('#btnRunAll').disabled = (S.running && !remote) || pA > 0 || pB > 0 || !ready;
  updateRunSelBtn();
  $('#btnStop').style.display = (S.running && !remote) ? 'inline-block' : 'none';

  $('#runHint').textContent = !ready ? '未设运行目录'
    : (remote ? ''
      : (pA > 0 ? ('待定 ' + pA) : (pB > 0 ? ('待定 ' + pB) : '')));
  renderDelegated();
  renderRunCtl();
  renderFlow();
  // 参数与候选结果单独铺一次：它们要读 /api/variants，
  //   不能塞进上面那个同步拼串里（那里 await 不了）。
  renderVarPanels();
}

/* =====================================================================
   处理过程：环节清单 + 进度 + 耗时
   ---------------------------------------------------------------------
   为什么要这一块：
     以前页面上只有「这一步跑没跑成」一格状态。用户看不到中途跑到哪、
     每个环节花了多久 —— 而"卡在合并那一步已经四十分钟"和"什么都没开始"
     在旧界面上长得一样。
   数据来源：
     阶段与环节清单来自服务端 /api/flow（它读 engine/flow_spec.R，唯一出处），
     进度来自执行端的环节级回写。
   显示原则：
     只显示真的发生过的：环节没回写就写「未回写」，不替它画成完成。
     耗时只用工整的两个时间戳算；只有一个时间戳就写「计时中」。
   ===================================================================== */
const FLOW_ST_CN = {
  todo: ['无回写，状态未知', ''], running: ['回写为运行中', 'run'], done: ['回写为完成', 'ok'],
  failed: ['回写为失败', 'bad'], unknown: ['未回写，状态未知', 'warn'], delegated: ['已交出', 'run'],
  stopped: ['回写为停止', 'warn'], handoff_unconfirmed: ['交接未确认，尚无执行回写', 'warn'],
  // 申报 done 但判定器拒过。必须与 done 分开显示：
  //   合成一句"完成"会让人以为这步过了，还会顺手点下一步。
  //   颜色按警告处理，不按成功。
  rejected: ['完成申报未通过，待核', 'warn'],
};
const REVIEW_CN = {
  yes: ['在服务器上跑', 'ok'], no: ['不在目标服务器上', 'bad'], unknown: ['没有机器信息', 'warn'],
};

function fmtDur(a, b) {
  if (!a) return '';
  const t1 = Date.parse(String(a).replace(' ', 'T'));
  const t2 = b ? Date.parse(String(b).replace(' ', 'T')) : Date.now();
  if (isNaN(t1) || isNaN(t2)) return '';
  let ms = t2 - t1;
  if (ms < 0) ms = 0;
  const sec = Math.round(ms / 1000);
  if (sec < 60) return sec + ' 秒';
  const min = Math.floor(sec / 60);
  if (min < 60) return min + ' 分 ' + (sec % 60) + ' 秒';
  const hr = Math.floor(min / 60);
  return hr + ' 时 ' + (min % 60) + ' 分';
}
/* 时钟显示：必须是看表那一刻的本地时间。
   ---------------------------------------------------------------------
   以前这里是直接切字符串 d.slice(11,16)。两个来源的时间格式不同，
   切出来的结果就不同：
     · 后端回写的时间是「2026-10-28T09:04:39」（本机时区，没有 Z），切对了；
     · 页面自己生成的时间是 new Date().toISOString()，结尾带 Z ——
       那是 UTC，切出来的 01:33 比墙上的 09:33 早八小时。
   于是「已查 01:33」这种字看起来像另一个世界的时间。
   现在统一按 Date 解析再取本地时分：带 Z 的自动换算，不带的当本地，
   两种都能显示成看表那一刻。 */
function fmtClock(x) {
  if (!x) return '';
  const raw = String(x).replace(' ', 'T');
  const d = new Date(raw);
  if (isNaN(d.getTime())) {
    // 解析不了就退回去切字符串，不显示成空 —— 宁可粗糙也别让人以为没查到。
    return raw.length >= 16 ? raw.slice(11, 16) : raw;
  }
  const p = n => (n < 10 ? '0' + n : '' + n);
  return p(d.getHours()) + ':' + p(d.getMinutes());
}

/* 流程层面的提醒：只留"每个步骤自己说不出来"的那几条。
   ---------------------------------------------------------------------
   为什么只留这几条：
     环节与耗时已经并进每个步骤卡片了（见 renderStageInline）。
     同一份清单再在页面下方完整画一遍，就会出现"上面说跑完、下面说在跑"
     这种自相矛盾的画面 —— 那正是这次要消掉的东西。
     这里只保留三类卡片放不下的信息：
       · 停滞提示（哪一步多久没动静了）
       · 这套脚本压根没有的阶段
       · 每一步都要过的那三关（全局规则）
   */
function renderFlow() {
  const host = $('#flowHost'); if (!host) return;
  const f = S.flow;
  if (!f) { host.textContent = '本次进度读取失败或尚未读取，状态未知；不代表任务已停止。'; return; }
  if (!arr(f.stages).length) { host.textContent = '尚无阶段进度数据，不能据此判断任务是否启动。'; return; }

  const st = f.selfcheck || {};
  let h = '<div class="flowwrap">';

  // 进度条：把三件事分开数，别糊成一个「完成 5/10」。
  //   以前只数 status==='done'，于是被上游覆盖的步骤（fullmark / diff）
  //   永远算不进去，任何 X/10 都必然偏低 —— 人看到会以为流程没跑，
  //   其实那两步的产出已经由终版一并算出来了。
  const pg = f.progress;
  if (pg && pg.total) {
    const pending = arr(f.stages).filter(x => !stageAcceptance(x, remoteStep(x.id)) && !stageCoveredAcceptance(x, f));
    const active = pending.filter(x => x.status === 'running').map(x => x.title || x.id);
    const waiting = pending.filter(x => x.status !== 'running').map(x => x.title || x.id);
    const pendingText = [active.length ? ('回写为运行中：' + active.join('、')) : '',
      waiting.length ? ('无完成回写或状态待确认：' + waiting.join('、')) : ''].filter(Boolean).join('；');
    h += '<div class="progline">'
      + '<b>已完成 ' + acceptedStageCount(f) + ' / ' + pg.total + '</b>'
      + '　<span class="dt">回写为完成 ' + pg.done + ' 步'
      + (coveredStageCount(f) ? '，其中 ' + coveredStageCount(f) + ' 项由上游产物验收覆盖' : '')
      + '</span>'
      + (pendingText ? '<details class="progmiss"><summary>查看无完成回写的阶段（' + pending.length + '）</summary>' + esc(pendingText) + '</details>' : '')
      + '</div>';
  }

  // 自检不过要先说：流程定义和引擎对不上时，后面所有进度都不可信。
  if (st && st.ok === false) {
    h += '<div class="rz fail" style="margin:8px 0"><span class="mk">×</span><span><b>流程定义自检没过</b>'
      + ' <span class="dt">' + esc(arr(st.problems).slice(0, 3).join('；')) + '</span></span></div>';
  }

  // 对账：同一份阶段名单在几个文件里各写一份，差在哪、该改哪一段。
  //   放在这一页顶部，因为它回答的就是「为什么这一步不动」——
  //   人是在看这张表时发现问题的，答案必须在同一屏。
  // 对账：同一份阶段名单在几个文件里各写一份，差在哪、该改哪一段。
  //   放在这一页顶部，因为它回答的就是「为什么这一步不动」——
  //   人是在看这张表时发现问题的，答案必须在同一屏。
  //
  //   2026-11-03 改：这个框原来只要有任何一条 issue 就画，标题固定写
  //   「定义对不上」。于是 0 处不一致时，它照样顶着"对不上"的标题挂在
  //   页面最上面 —— 人看到的第一反应就是"没修好"。而它下面列的两条，
  //   一条是另一个入口的旧名单（设计如此，本应用不用它），
  //   一条是服务器引擎快照还没上报（等执行端，不是错）。
  //   两种都不是"定义对不上"。把它们和真正的 fail 混在一个框里，
  //   真出问题时那条反而被淹没。
  //
  //   现在的分档：
  //     fail > 0            → 红框「定义对不上」，只列 fail
  //     fail = 0，有 unknown → 一行中性灰字「有 N 项没法核」，可展开
  //     fail = 0，只剩 legacy → 不显示（它的说明已经在流程表对应环节上）
  const ct = f.contract;
  if (ct && ct.issues && ct.issues.length) {
    const bad = ct.issues.filter(x => x.level === 'fail');
    const unk = ct.issues.filter(x => x.level === 'unknown');
    const leg = ct.issues.filter(x => x.level === 'legacy');

    if (bad.length) {
      h += '<div class="rz fail ctbox">'
        + '<div><span class="mk">×</span>'
        + '<b>定义对不上</b> <span class="dt">' + bad.length + ' 处不一致'
        + (unk.length ? '，' + unk.length + ' 处还没法核' : '')
        + '</span></div><div class="ctlist">';
      bad.forEach(x => {
        h += '<div class="ctrow fail">'
          + '<div class="ctwhere">' + esc(x.where) + '</div>'
          + '<div class="ctwhat">' + esc(x.what) + '</div>'
          + '<div class="ctfix">' + esc(x.fix) + '</div>'
          + '</div>';
      });
      h += '</div></div>';
    } else if (unk.length) {
      // 没有不一致，只是有事实没核到。这不是告警，是一条待办。
      h += '<div class="ctnote">'
        + '<span class="dt">另有 ' + unk.length + ' 项还没法核：'
        + esc(unk.map(x => x.what).join('；'))
        + '</span></div>';
    }
    // legacy 不显示：它是"另一个入口的名单没覆盖本流程"，属于正确事实，
    //   不是待修项。写在这里只会让人以为有活没干完。
    void leg;
  }

  // 停滞提示：回答「助手说它在跑、然后就没消息了」。
  const stall = flowStall(f);
  if (stall) {
    h += '<div class="rz warn flowstall"><span class="mk">!</span><span>'
      + '<b>进度待确认：' + esc(stall.dur) + ' 未更新</b>'
      + ' <span class="dt">「' + esc(stall.title) + '」最后一次动静是 '
      + esc(fmtClock(stall.at)) + '。仅凭没有新回写，不能判断进程已停止。'
      + '可查现有结果，或请求执行端确认。</span></span>'
      + '<div class="btnrow" style="margin-top:6px">'
      + '<button class="btn" data-flowpoke="' + esc(stall.stage) + '">让它现在报一次</button>'
      + '<button class="btn" data-flowcheck="1">查一次结果</button></div></div>';
  }

  // 流程里本来就没有的阶段，如实说"没有"，不摆空按钮。
  const miss = arr(f.missing);
  if (miss.length) {
    h += '<div class="small" style="margin-top:8px">这套脚本没有：'
      + esc(miss.join('、')) + '（服务器上没对应脚本）</div>';
  }

  const gates = arr(f.gates);
  if (gates.length) {
    h += '<div class="flowgates"><span class="small">每一步都要过这三关：</span>'
      + gates.map(g => '<span class="fgtag" title="' + esc(g.how || '') + '">'
        + esc(g.title) + '</span>').join('') + '</div>';
  }
  h += '</div>';

  host.innerHTML = h;
}

/* 判断是否停滞：只看已经发生的事实。
   判据：某一阶段状态是 running，且它自身与它所有环节里最新的时间戳
        距现在超过阈值。阈值给 6 分钟 —— 短于它，正常跑一步也会被误报。
   没有 running 的阶段就不算停滞：那不是"卡住"，是"还没开始"。

   2026-11-03 加了两条排除（审核第 4 条）：
   ① covered（已由上游覆盖）—— 这一步本来就不该跑，报"停滞"是假警报。
      fullmark 就是这样被挂了 133 小时：它被标成"已由终版覆盖"，
      却因为状态残留着 delegated 而一直被算作"卡住"。
   ② 已经有失败结论的 —— 那不是"在等"，是"已经停了"。
      这种情况要报的是"失败"，走失败那条线，不是停滞这条线。
   两类都排除后，停滞提示才只在"真的有人在跑却没消息"时出现。
   假警报的代价不是多看一眼，是人会开始忽略所有警报。 */
const FLOW_STALL_MIN = 6;
function flowStall(f) {
  let best = null;
  arr(f && f.stages).forEach(sg => {
    if (sg.covered) return;                       // ① 已覆盖，不该跑
    if (sg.status !== 'running') return;
    if (sg.status === 'failed') return;           // ② 已有失败结论
    const subs = arr(sg.subs);
    if (subs.some(sb => sb.status === 'failed')) return;  // 子项已判失败
    const cands = [sg.started_at, sg.imported_at || '', sg.updated_at || '',
                   // 执行端心跳也算"有动静"。
                   //   一个样本要跑十几分钟，期间本来就不会有 step/import 回写；
                   //   只看回写时间就会把正常在跑报成"1 小时 31 分钟未更新"。
                   //   实测就是这条误报。心跳在更新，就说明它活着。
                   sg.exec_last_seen || ''];
    subs.forEach(sb => { cands.push(sb.started_at, sb.ended_at); });
    let latest = 0;
    cands.forEach(x => {
      if (!x) return;
      const t = Date.parse(String(x).replace(' ', 'T'));
      if (!isNaN(t) && t > latest) latest = t;
    });
    if (!latest) return;
    const mins = (Date.now() - latest) / 60000;
    if (mins < FLOW_STALL_MIN) return;
    if (!best || mins > best.mins) {
      const mm = Math.floor(mins);
      best = {
        stage: sg.id, title: sg.title, at: new Date(latest).toISOString(), mins,
        dur: mm < 60 ? (mm + ' 分钟') : (Math.floor(mm / 60) + ' 小时 ' + (mm % 60) + ' 分钟'),
      };
    }
  });
  return best;
}

/* 「让它现在报一次」：把一条短指令送进会话，要求它立刻回写当前进度。
   为什么要这一条：执行端跑起来之后不会自己回来报信，「我等它通知我」
   在真实链路里不成立。人总得有个办法去问一声 —— 这就是那个办法。
   送出去的仍然是一条短指令（带 /api/brief 地址），不占通道长度。 */
/* 单步「查询结果」：查完必须说出一句结论。
   ---------------------------------------------------------------------
   以前这个动作只会重画一次页面，再把一个 UTC 时分写在旁边，
   人点了看不到任何变化，只有一个和墙上的钟对不上的时间。
   现在按这一步自己的数据回答四件事，答不出来的就明写"没记录"：
     ① 现在什么状态；
     ② 最后一次回写是什么时候、谁写的；
     ③ 产物几项、缺哪几项（拿流程定义里的应有清单对）；
     ④ 这次查询发生在什么时刻（本地时分，带秒，免得看不出是不是新查的）。
   查不出新东西也算一种结论：说清"和上次一样，没有新回写"。 */
const ST_CN = {
  done: '回写为完成', failed: '回写为失败', running: '回写为运行中', delegated: '已交接，待执行确认',
  stopped: '回写为停止', todo: '无回写，状态未知', unknown: '状态未知',
};

async function queryStep(id) {
  const __recordScope = recordContext();
  try {

  const box = document.getElementById('stq-' + id);
  const title = stepTitleCN(id) || id;
  if (box) box.innerHTML = '<span class="qwait">正在查「' + esc(title) + '」…</span>';
  // 先把这一份状态真的重新取回来，再说话。不先刷新就说结论，等于念旧账。
  try {
    const st = await recordAwait(getJSON('/api/status'), __recordScope);
    if (st && Array.isArray(st.steps)) {
      const m = {};
      st.steps.forEach(s => { if (s && s.id) m[s.id] = s; });
      S.remoteSteps = m;
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 /* 取不到就按手上的说，并如实标注 */ }
  await recordAwait(loadFlow(), __recordScope);
  const tAt = new Date();
  const p = n => (n < 10 ? '0' + n : '' + n);
  const nowTxt = p(tAt.getHours()) + ':' + p(tAt.getMinutes()) + ':' + p(tAt.getSeconds());

  const sg = stageOf(id);
  const rs = remoteStep(id);
  if (!sg && !rs) {
    if (box) box.innerHTML = '<span class="qbad">查完了：这一步还没有任何回写记录。'
      + '点了它也没用，先「交给助手去跑」。</span><span class="qat">' + nowTxt + '</span>';
    return;
  }
  const stKey = (sg && sg.status) || (rs && rs.status) || '';
  const stTxt = ST_CN[stKey] || stKey || '未知';
  const code = (rs && rs.code !== null && rs.code !== undefined) ? rs.code
    : (sg && sg.code !== null && sg.code !== undefined ? sg.code : null);
  const imp = String((sg && sg.imported_at) || (rs && rs.imported_at) || '').replace('T', ' ');
  const arts = sg ? arr(sg.artifacts) : [];
  const subs = sg ? arr(sg.subs) : [];
  const badSubs = subs.filter(x => x.status === 'failed');

  const bits = [];
  bits.push('状态：<b>' + esc(stTxt) + '</b>' + (code === null ? '' : '（退出码 ' + esc(String(code)) + '）'));
  bits.push(imp ? ('最后回写：' + esc(imp)) : '最后回写：没记录');
  bits.push(arts.length
    ? ('产物：' + arts.length + ' 项' + (stKey === 'failed' ? '（失败态下这只是已有/应有清单，不等于全部落盘）' : ''))
    : '产物：这项没有清单回写');
  if (badSubs.length) bits.push('卡在：' + esc(badSubs.map(x => x.title).join('、')));

  const changed = (S.lastQuery && S.lastQuery[id]) ? S.lastQuery[id] : null;
  const sig = [stKey, code, imp, arts.length].join('|');
  S.lastQuery = S.lastQuery || {};
  const same = changed === sig;
  S.lastQuery[id] = sig;

  if (box) {
    box.innerHTML = '<div class="qbox">'
      + '<div class="qline">' + bits.join('　·　') + '</div>'
      + '<div class="qnote">' + (same
        ? '和上一次查的一样：这段时间没有新的回写。'
        : '这是刚取回来的，和上一次不一样。')
      + '</div>'
      + '</div><span class="qat">' + nowTxt + '</span>';
  }
  if (stKey === 'failed' || stKey === 'running') {
    // 失败或在跑的时候，顺手把那一步的卡片按新状态重画一遍，
    //   免得查询结论说"没跑通"、卡片还挂着旧的样子。
    renderRun();
    const again = document.getElementById('stq-' + id);
    if (again) again.innerHTML = box ? box.innerHTML : '';
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function pokeFlow(stageId, originLabel = '查进度') {
  const __recordScope = recordContext();
  try {

  await recordAwait(loadFlow(), __recordScope);
  const binding = stepNotificationBinding(stageId);
  if (!binding.reqId) {
    toast('这一步暂无可查询的任务编号。请查看步骤卡片和执行队列。', 'info');
    return;
  }
  const brief = '/api/brief?step=' + encodeURIComponent(stageId)
    + (S.sessionId ? '&session=' + encodeURIComponent(S.sessionId) : '')
    + (binding.reqId ? '&req=' + encodeURIComponent(binding.reqId) : '');
  const txt = '【来自 个性化分析助手】\n'
    + '要办的事：「' + (stepTitleCN(stageId) || stageId) + '」现在跑到哪了，请只读观察后说明。\n'
    + '1. 到服务器上看这一阶段的实际进程与最后写出的文件，不要凭记忆回答。\n'
    + '2. 只读汇报进度；查进度用 GET /api/flow?session=<分析记录号>（只读、校验归属），'
    + '或直接看页面上的状态。\n'
    + '3. 不要手工 POST /api/step/import。这一环的作业由执行端回写，'
    + '你手工写会和它争同一份记录，写出来的状态谁对谁错没人能判。\n'
    + '4. 如果已经停了或失败了，把原因原样说明，不要重跑、不要换参数、不要 qsub。\n'
    + '完整流程说明在 ' + brief + ' （GET 取 text 字段）。';
  // purpose 必须是 observe：这是一条"看一眼、说一句"的通知。
  //   以前传默认 execute，prepare 会按执行任务去重建正文、申请回写凭据、
  //   抢占输出目录 —— 只为问一句进度，却把执行条件全备齐了。
  //   而且它要求助手手工回写，正好和执行端争同一份记录。
  const r = await recordAwait(injectToSession(txt, function () {}, 'observe', binding,
    originLabel + ' · ' + (stepTitleCN(stageId) || stageId)), __recordScope);
  // 卡片上也留一句：toast 会消失，而人点完按钮还站在这一张卡上。
  const box = document.getElementById('stq-' + stageId);
  if (box) {
    // handed：已交给宿主或已放进本机队列，两种都算交出去了。
    //   以前只认 r.ok，队列那条路明明接通了也画成红色失败。
    const handed = !!(r && (r.ok || r.relayed));
    box.innerHTML = handed
      ? '<span class="qok">已让它报一次（' + esc(fmtClock(new Date().toISOString()))
        + (r && r.ok ? ' 已送入会话' : ' 已进入消息队列') + '）。进行中的作业可在执行队列查看。</span>'
      : '<span class="qbad">没送进会话：'
        + esc(String((r && (r.why || r.code)) || '通道没连上').replace(/[。．.]+$/, ''))
        + '。可等会话通道恢复后再点一次。</span>';
  }
  if (r && (r.ok || r.relayed)) toast(r.ok ? '已向会话发送进度查询。' : '会话通道未连通，进度查询已存入消息队列。', 'info');
  else toast('没送进会话：' + String((r && (r.why || r.code)) || '通道没连上').replace(/[。．.]+$/, ''), 'bad');

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

function stepTitleCN(id) {
  const hit = arr(S.flow && S.flow.stages).find(x => x.id === id);
  return hit ? hit.title : '';
}

/* 覆盖这一步的上游叫什么。
   只用来把"已由「终版比例统计」一并算出"这句话说完整，
   取不到就退回 id，不编一个好听的名字。 */
function coverTitle(id) {
  if (!id) return '';
  return stepTitleCN(id) || String(id);
}

async function loadFlow() {
  const __recordScope = recordContext();
  try {

  try { S.flow = await recordAwait(getJSON('/api/flow'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 S.flow = null; }
  renderFlow();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

// 服务器模式下"这一步交给谁了"要说清楚，而不是只弹一条会消失的提示。
//   失败时把指令原文摆出来，人可以自己复制发一次 —— 功能不押在一条握手上。
/* 常驻推进条：不藏在弹窗、不等停滞才出现。
   为什么要有：
     原来「让它报一次」「查一次结果」只在检测到停滞时才画出来，
     没停滞就一个入口都没有 —— 而"我想知道现在到底怎么样"恰恰是最常见的需求。
     人只能去问助手，那就等于应用没起作用。
   这里固定给三个动作，任何时刻都在：
     查一次结果 / 让它报一次 / 复制进度询问原文 */
/* 执行队列状态：人点了之后，页面上必须能看见"已排队 / 执行中"。
   为什么单列一个请求：这条链路是"点一下→工作电脑取走→服务器执行"，
   它和 flow 的进度是两个来源，混在一起渲染会互相覆盖。 */
async function refreshExecTasks() {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(getJSON('/api/exec'), __recordScope);
    S.execTasks = arr(r && r.items);
    S.execCounts = (r && r.counts) || null;
    S.execStuck = (r && r.stuck_count) || 0;
    S.execWorker = (r && r.worker) || null;
    S.execReclaim = (r && r.reclaim) || null;
    S.execLoadErr = '';
    renderRunCtl();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 /* 读不到就保持原样，不遮住已有进度 */
    // 但必须记下这次失败：看板要能回答"队列到底读没读到"，
    //   一直静默失败会让人以为队列真的空着。
    S.execLoadErr = String((e && e.message) || e || '读取失败');
    renderRunCtl(); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 顶部那条推进条已经撤掉。
   ---------------------------------------------------------------------
   原来它写「最近完成：X　下一步：Y　产物 N 项」并挂两个按钮
   （查一次结果 / 让它报一次）。gina 的判断是冗余，这个判断是对的：
     · "最近完成/下一步" 每张步骤卡上自己就写着状态和顺序，重复一遍；
     · 那两个按钮现在每张卡上都有，而且是针对"这一步"的，比顶部那条
       只认"当前阶段"更准；
     · "产物 N 项" 已经进「任务完成详情」。
   留着它只会让人在两处读到同一件事，还要判断哪一处是准的。 */
function renderRunCtl() {
  // 同一块看板要出现在两个地方：
  //   #runCtl      「跑分析」区块顶部（点完按钮就在眼前）
  //   #queueHost   「分析运行看板」区块顶部（没识别也能看，专门查队列用）
  //   两处内容必须一模一样 —— 各写一份迟早会不一致，人就会在两处读到不同的结论。
  const targets = ['#runCtl', '#queueHost'].map(sel => $(sel)).filter(Boolean);
  if (!targets.length) return;
  const paint = html => targets.forEach(el => { el.innerHTML = html; });

  // 读不到队列时必须说出来。
  //   以前是静默保留上一次的样子，于是"接口挂了"和"队列空着"长得一模一样，
  //   人只会以为活没派出去，反复点按钮。
  if (S.execLoadErr) {
    paint('<div class="rz fail"><span class="mk">×</span><span>'
      + '<b>队列读不到</b> <span class="dt">' + esc(S.execLoadErr) + '</span></span></div>');
    return;
  }

  const w = S.execWorker || {};
  const c = S.execCounts || {};
  const r = S.execReclaim || null;
  const items = arr(S.execTasks);
  const live = items.filter(t => ['queued', 'running'].includes(String(t.status)));
  const stuck = items.filter(t => t && t.stuck);
  const n = k => (c[k] === undefined || c[k] === null ? 0 : c[k]);

  const atText = v => v ? String(v).replace('T', ' ').slice(5, 16) : '';
  const agoText = secs => {
    if (secs === null || secs === undefined) return '';
    const s = Number(secs);
    if (!isFinite(s) || s < 0) return '';
    if (s < 90) return Math.round(s) + ' 秒前';
    if (s < 5400) return Math.round(s / 60) + ' 分钟前';
    return (s / 3600).toFixed(1) + ' 小时前';
  };

  let h = '<div class="qboard">';

  // ---- 一句话总述：现在这台机器到底能不能干活 --------------------
  //   三个事实必须同时成立才叫"能跑"：执行端在线、队列里没有卡住的、回收器在跑。
  //   缺任何一个都要直接说出来，不能让人从下面几行自己去拼。
  const workerOk = !!w.online;
  // 心跳会骗人：进程每分钟报活，却两天没领过一件活。
  //   所以"能干活"要看 serving（服务端已按最后领活时间算过），不能只看 online。
  const servingOk = w.serving === undefined ? workerOk : !!w.serving;
  const spinIdle = !!w.spin_idle;
  const reclaimOk = !!(r && r.alive);
  const blockOk = stuck.length === 0;
  const headline = (servingOk && reclaimOk && blockOk)
    ? { cls: 'ok', mk: '✓', t: '队列正常',
        d: n('running') + ' 条在跑、' + n('queued') + ' 条在排。' }
    : (!workerOk
      ? { cls: 'fail', mk: '×', t: '没有执行端在线',
          d: '排队的任务不会有人领取。要先在工作电脑上启动执行端。' }
      : (spinIdle
        ? { cls: 'fail', mk: '×', t: '执行端在报心跳，但它不领活',
            d: '它在空转：心跳每分钟到，队列却没人取。'
              + (w.last_claim_at
                ? ('最后一次领活 ' + String(w.last_claim_at).replace('T', ' ').slice(0, 16)
                   + '，' + (agoText(w.last_claim_age_secs) || '很久以前') + '。')
                : '从来没有领过任何任务。')
              + ' 应用已发出启动/安装请求，需要在那台电脑上重装并重启执行端。' }
        : (!reclaimOk
          ? { cls: 'warn', mk: '!', t: '回收器没在跑',
              d: '失联任务不会被自动收尾，队列里的状态可能停在旧样子。' }
          : { cls: 'warn', mk: '!', t: '有 ' + stuck.length + ' 条排队任务没人认领',
              d: '它们会一直挡住后续环节。确认不会再跑了就作废掉。' })));
  h += '<div class="rz ' + headline.cls + '"><span class="mk">' + headline.mk + '</span>'
    + '<span><b>' + esc(headline.t) + '</b> <span class="dt">' + esc(headline.d) + '</span></span></div>';

  // ---- 计数条 ----------------------------------------------------
  h += '<div class="qcount">'
    + '<span><i class="qc q-run"></i>在跑 <b>' + n('running') + '</b></span>'
    + '<span><i class="qc q-que"></i>排队 <b>' + n('queued') + '</b></span>'
    + '<span><i class="qc q-done"></i>完成 <b>' + n('done') + '</b></span>'
    + '<span><i class="qc q-fail"></i>失败 <b>' + n('failed') + '</b></span>'
    + '</div>';

  // ---- 执行端 ----------------------------------------------------
  //   agoSecs：把服务端给的 "2026-11-22T20:17:05" 换成"多少秒之前"。
  //   自己算而不是用服务端算，是因为这一栏是"现在"快照，页面停留久了也要准。
  const agoSecs = v => {
    if (!v) return null;
    const ms = Date.parse(String(v).replace(' ', 'T'));
    if (!isFinite(ms)) return null;
    return Math.max(0, Math.round((Date.now() - ms) / 1000));
  };
  let workerHtml = '<span class="qk">执行端</span>';
  if (workerOk) {
    workerHtml += '<span class="qv ok">在线</span><span class="qd">'
      + esc(w.by || '未知') + '　最近报信 '
      + esc(agoText(agoSecs(w.at)) || '—')
      + '（' + esc(atText(w.at) || '—') + '）</span>';
  } else if (w.at) {
    workerHtml += '<span class="qv bad">离线</span><span class="qd">最后一次报信 '
      + esc(atText(w.at)) + '，' + esc(agoText(agoSecs(w.at)) || '时间不明')
      + '。排队的任务不会有人领取。</span>';
  } else {
    workerHtml += '<span class="qv bad">离线</span><span class="qd">'
      + '从来没有报过信。这台应用还没有任何执行端连过。</span>';
  }
  h += '<div class="qline">' + workerHtml + '</div>';

  // ---- 启动/安装请求 ------------------------------------------------
  //   检测不到能用的执行端时，应用会主动发一条"去那台电脑装并启动"的请求。
  //   这一行回答两件事：有没有发过、发给了哪台电脑。
  //   没有这一行，"我已经发过请求了"这句话在页面上找不到凭据。
  const boot = (w && w.bootstrap) || null;
  if (boot && Number(boot.count || 0) > 0) {
    h += '<div class="qline"><span class="qk">启动请求</span>'
      + '<span class="qv' + (servingOk ? ' ok' : ' bad') + '">已发 ' + Number(boot.count) + ' 次</span>'
      + '<span class="qd">最近一次 ' + esc(atText(boot.requested_at) || '—')
      + (boot.exec_pc ? ('，目标电脑 ' + esc(boot.exec_pc)) : '')
      + (servingOk ? '。执行端已在正常领活。' : '。等那台电脑把执行端装好并启动，这里会自动变成正常。')
      + '</span></div>';
  }

  // 心跳在、却从不领活：这是最难自己发现的一种。
  //   实测形态：心跳每分钟一次、claim 动作全量为 0。
  //   心跳只能证明进程活着，证明不了它在看这个队列。
  if (workerOk && n('queued') > 0 && n('running') === 0) {
    const oldest = live.filter(t => String(t.status) === 'queued')
      .map(t => Number(t.queued_minutes || 0)).sort((a, b) => b - a)[0];
    if (oldest >= 10) {
      h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
        + '<b>执行端在线，但排队最久的那条已经等了 ' + Math.round(oldest) + ' 分钟没人领。</b> '
        + '<span class="dt">心跳只证明进程活着，不证明它在轮询这个队列。'
        + '前一步在跑时可以一直排；但若前面没有任何在跑的任务，就说明它没在取活。</span></span></div>';
    }
  }

  // ---- 回收器 ----------------------------------------------------
  //   为什么要显示它：它坏死时不会有任何报错冒到页面上，只表现成"那条一直不动"。
  //   把最后一次扫描时间摆出来，"没人回收"和"没人接活"才分得开。
  h += '<div class="qline">'
    + '<span class="qk">回收器</span>'
    + (r
      ? ((r.alive
        ? '<span class="qv ok">在跑</span>'
        : '<span class="qv bad">没在跑</span>')
        + '<span class="qd">'
        + (r.last_scan_at
          ? ('上次扫描 ' + esc(atText(r.last_scan_at)) + '，'
             + esc(agoText(r.last_scan_age_secs)))
          : '还没有扫描记录')
        + (r.expect_secs ? ('　（每 ' + Math.round(Number(r.expect_secs) / 60) + ' 分钟一次）') : '')
        + '</span>')
      : '<span class="qv">未知</span><span class="qd">这次没拿到回收器状态</span>')
    + '</div>';

  // 最近回收了什么：这是"我做的动作有没有生效"的唯一凭据。
  if (r && arr(r.recent).length) {
    h += '<div class="qrecent">';
    arr(r.recent).slice(0, 3).forEach(x => {
      const kd = String(x.kind || '');
      const label = kd === 'manual_cancel' ? '人工作废'
        : (kd === 'scan_error' ? '扫描出错'
          : ('自动回收 ' + (Number(x.reaped || 0)) + ' 条'));
      const ids = arr(x.ids).map(String).filter(Boolean);
      const when = String(x.at || '').replace('T', ' ').slice(5, 16);
      h += '<div class="qz"><span class="qt">' + esc(when) + '</span>'
        + '<span class="qn">' + esc(label) + '</span>'
        + (ids.length ? '<span class="qi">' + esc(ids.slice(0, 3).join('、')) + '</span>' : '')
        + (x.error ? '<span class="qi">' + esc(String(x.error)) + '</span>' : '')
        + '</div>';
    });
    h += '</div>';
  }

  // ---- 未结束的任务明细 ------------------------------------------
  //   只列 queued 和 running：完成和失败的历史没有"要不要动手"这个问题，
  //   全列出来会把要判断的那两行淹掉。
  if (live.length) {
    h += '<div class="qlist">';
    live.slice(0, 8).forEach(t => {
      const st = String(t.status);
      const isRun = st === 'running';
      const title = stepTitleCN(t.step) || t.step || '—';
      // 这条是不是当前记录的任务，必须标出来。
      //   /api/exec 是执行端视角的接口，返回的是整台应用的队列，不按记录过滤。
      //   不标的话，人会把别的分析记录的排队任务当成自己的，
      //   然后对着一条不属于自己的任务反复找原因。
      const mine = !!S.sessionId && String(t.session_id || '') === String(S.sessionId);
      const mins = t.queued_minutes === null || t.queued_minutes === undefined ? null : Number(t.queued_minutes);
      h += '<div class="qitem' + (t.stuck ? ' stuck' : '') + '">'
        + '<span class="qs ' + (isRun ? 'run' : 'que') + '">' + (isRun ? '在跑' : '排队') + '</span>'
        + '<span class="qname">' + esc(title) + '</span>'
        + (mine ? '' : '<span class="qother">别的记录 ' + esc(String(t.session_id || '').slice(-9)) + '</span>')
        + '<span class="qmeta">' + esc(String(t.id || '')) + '</span>'
        + (isRun
          ? (t.claimed_at ? '<span class="qmeta">领取于 ' + esc(atText(t.claimed_at)) + '</span>' : '')
          : (mins !== null && mins !== undefined ? '<span class="qmeta">已等 ' + mins + ' 分钟</span>' : ''))
        + (t.stuck ? '<span class="qflag">没人认领</span>' : '')
        // 在跑的也要给作废入口。
        //   为什么（2026-11-22，gina 报"只能改代码才能继续"）：
        //     原来只给排队中的那条按钮，正在跑的连按钮都没有。
        //     可最常见的死法恰恰是"领取了、再没动静"——执行端死了，任务停在 running。
        //     那种情况下界面上一个能点的东西都没有，人只能去改 state/exec_tasks.json。
        //   服务端会自己判断：执行端还活着、也还在报信，才拒一次并告诉你可以强制作废。
        + '<button class="btn btn-sm qcancel" data-exec-cancel="' + esc(String(t.id || ''))
        + '">' + (isRun ? '强制结束' : '作废') + '</button>'
        + '</div>';
    });
    if (live.length > 8) {
      h += '<div class="qmore">还有 ' + (live.length - 8) + ' 条未列出。</div>';
    }
    h += '</div>';
  } else if (n('total') > 0) {
    h += '<div class="qidle">队列里没有在跑或在排的任务。'
      + '（历史 ' + n('done') + ' 条完成、' + n('failed') + ' 条失败）</div>';
  } else {
    h += '<div class="qidle">队列是空的，还没有派过任何任务。</div>';
  }
  const lastFailure = S.sessionId && items.filter(t => t.session_id === S.sessionId && t.status === 'failed')
    .sort((a, b) => String(b.finished_at || b.created_at || '').localeCompare(String(a.finished_at || a.created_at || '')))[0];
  if (lastFailure) {
    h += '<div class="qidle"><b>当前分析最近一次失败：</b>'
      + esc(stepTitleCN(lastFailure.step) || lastFailure.step || '') + ' '
      + esc(lastFailure.id || '')
      + (lastFailure.error ? ' · ' + esc(lastFailure.error) : '') + '</div>';
  }

  h += '<div class="qfoot">'
    + '<button class="btn btn-sm" data-exec-refresh="1">刷新队列</button>'
    + '<span class="small">页面开着时每 5 秒自己更新一次；关掉页面后回收仍按上面那个频率照常跑。</span>'
    + '</div>';

  h += '</div>';
  paint(h);
}

/* =====================================================================
   同一环节的多套结果：筛群参数组合 / 注释版本
   ---------------------------------------------------------------------
   为什么这两个环节要特殊处理：
     它们不是"跑一遍就完"，而是要人看过才敢往下走。
       · 筛群：dims 与 resolution 决定分成多少个簇。resolution 选 0.5 / 0.8 / 0.9
         会得到不同粒度，必须把几套都跑出来对比，人挑一套最合适的往下用。
       · 注释：一份参考表只给一种判读，换一份表就是另一套结论。
         也要并排看，挑最像的那一版。
   所以这里做三件事：
     1) 参数面板：把 dims / resolution 做成可选可多选
     2) 候选列表：每跑一套就登记一条，状态与输出目录都写清
     3) 选定：人在列表里点一套，写入记录；下游只认这一套
   ===================================================================== */
const RES_PRESETS = ['0.3', '0.5', '0.6', '0.8', '0.9', '1.0'];

function variantCache() {
  S.variants = S.variants || {};
  return S.variants;
}

async function loadVariants(force) {
  const __recordScope = recordContext();
  try {

  if (!force && S.variantsLoaded === S.sessionId) return variantCache();
  try {
    const d = await recordAwait(getJSON('/api/variants' + (S.sessionId ? ('?session=' + encodeURIComponent(S.sessionId)) : '')), __recordScope);
    S.variants = (d && d.stages) || {};
    S.variantsLoaded = S.sessionId || '';
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    S.variants = S.variants || {};
  }
  return variantCache();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 筛群参数面板 + 候选列表 */
function filterPanelHTML(items, chosen) {
  const dimsVal = (S.filterDims || '1:20');
  const picked = arr(S.filterRes);
  const chips = RES_PRESETS.map(r =>
    '<button class="rchip' + (picked.indexOf(r) >= 0 ? ' on' : '') + '" data-res="' + esc(r) + '">'
    + esc(r) + '</button>').join('');

  let h = '<div class="vpanel">'
    + '<div class="vpanel-t">筛群参数</div>'
    + '<div class="vrow"><label>dims</label>'
    + '<input id="filterDims" class="vinput" value="' + esc(dimsVal) + '" placeholder="例如 1:20">'
    + '<span class="vhint">取多少维做近邻图。官方脚本给的是 1:20。</span></div>'
    + '<div class="vrow"><label>resolution</label><div class="rchips">' + chips + '</div></div>'
    + '<div class="vhint">本次运行一套参数，确认后提交计算。</div>'
    + '<div class="btnrow" style="margin-top:7px">'
    + '<button class="btn btn-sm primary" data-filter-go="1">按这套参数运行</button>'
    + '<span class="small" id="filterMsg"></span></div>'
    + '</div>';

  h += variantListHTML('filter', items, chosen);
  return h;
}

/* 通用候选列表 */
function variantListHTML(stage, items, chosen) {
  if (!arr(items).length) return '';
  let h = '<div class="vpanel"><div class="vpanel-t">候选结果 '
    + '<span class="vcount">' + items.length + ' 套</span></div><div class="vlist">';
  items.forEach(it => {
    const on = String(it.id) === String(chosen);
    const st = String(it.status || 'pending');
    const stCn = st === 'done' ? '已出结果' : (st === 'failed' ? '没跑通' : '等回写');
    const stCls = st === 'done' ? 'ok' : (st === 'failed' ? 'bad' : 'wait');
    const params = it.params || {};
    const plabel = it.label
      || (stage === 'filter'
        ? ('dims ' + (params.dims || '?') + ' · resolution ' + (params.resolution || '?'))
        : (params.reference || it.id));
    h += '<div class="vitem' + (on ? ' on' : '') + '">'
      + '<div class="vtop"><b>' + esc(plabel) + '</b>'
      + '<span class="vtag ' + stCls + '">' + esc(stCn) + '</span>'
      + (on ? '<span class="vtag on">下游用它</span>' : '') + '</div>'
      + (it.outdir ? '<div class="vpath">' + esc(it.outdir) + '</div>' : '')
      + (it.note ? '<div class="vnote">' + esc(it.note) + '</div>' : '')
      + '<div class="btnrow" style="margin-top:6px">'
      + (on
          ? '<button class="btn btn-sm" data-var-unchoose="' + esc(stage) + '">取消选用</button>'
          : '<button class="btn btn-sm primary" data-var-choose="' + esc(stage) + '|' + esc(it.id) + '">选这一套</button>')
      + '</div></div>';
  });
  h += '</div></div>';
  return h;
}

/* 注释依据的两个按钮：已确定选择 / 修改选择。
   ---------------------------------------------------------------------
   为什么单独一个同步函数（2026-11-18）：
     以前这里是一块 async 面板，要等一次 /api/variants 往返才填内容，
     而卡片每 5 秒整块重建一次 —— 重建后到返回前那段空档里按钮不存在，
     所以人看到的是"按钮一直在闪，时有时无"。
     改成跟卡片同一趟同步生成，逻辑上不可能缺那一段。
   两个按钮是什么意思（按人话说）：
     · 已确定选择：这一步该用哪份依据，之前就定过了，直接用它。
       点它就表示"照这份跑"，圈亮在这一个上。
     · 修改选择：还没定、或者想换一份。点它才弹三选一
       （服务器上已有 / 我上传一份 / 文献库），跟环境识别那边同一套。
       选完并配好，圈就移到这一个上，另一个灭。
   亮着的那一个就是"这次跑会用的"。没配好的时候两个都不亮，
   并且写明缺什么 —— 不给一个看不出来能不能跑的状态。 */
function annotRefState() {
  const d = S.placeDraft || {};
  const rm = String(d.ref_mode || '');
  const items = arr(S.references && S.references.items);
  const chosen = String((S.references && S.references.chosen) || '');
  const cur = items.filter(x => String(x.id || '') === chosen)[0] || null;
  if (rm === 'path') {
    const p0 = String(d.ref_path || '').trim();
    return { mode: 'path', ready: !!p0, label: p0 ? ('服务器路径 ' + p0) : '还没填服务器上的路径' };
  }
  if (rm === 'upload') {
    const nm = cur ? String(cur.name || cur.id || '') : (S.pendingRef ? String(S.pendingRef.name || '') : '');
    return { mode: 'upload', ready: !!nm, label: nm ? ('已上传 ' + nm) : '还没上传文件' };
  }
  if (rm === 'lit') {
    const a0 = String(d.ref_archive || '').trim();
    return { mode: 'lit', ready: !!a0, label: a0 ? ('文献库存档号 ' + a0) : '还没填存档号' };
  }
  if (rm === 'none') return { mode: 'none', ready: true, label: '这一轮不需要注释依据' };
  return { mode: '', ready: false, label: '还没选注释依据' };
}

function annotRefBarHTML() {
  const st = annotRefState();
  // 默认圈在「已确定选择」上：只要之前定过的依据还完整，就不该让人以为要重选。
  //   显式点过「修改选择」才把圈移过去；改完再移回「已确定选择」。
  const editing = String(S.annotRefMode || '') === 'edit';
  const savedOn = st.ready && !editing;
  const editOn = editing;
  const tip = st.ready
    ? (editing ? '改完并保存后，圈会回到「已确定选择」' : '这一步会按上面这份依据跑')
    : ('还差一步：' + st.label + ' — 点「修改选择」去填');
  return '<div class="annotrefbar" style="margin-top:6px">'
    + '<div class="btnrow">'
    + '<button class="btn btn-sm' + (savedOn ? ' primary' : '') + '"'
    + ' data-annot-ref-mode="saved" title="沿用之前定好的注释依据">已确定选择</button>'
    + '<button class="btn btn-sm' + (editOn ? ' primary' : '') + '"'
    + ' data-annot-ref-mode="edit" title="换一份或第一次选">修改选择</button>'
    + '</div>'
    + '<div class="small" style="margin-top:5px;color:#5a6068">' + esc(tip) + '</div>'
    + '</div>';
}

async function renderVarPanels() {
  const __recordScope = recordContext();
  try {

  const fp = $('#varpanel-filter');
  if (!fp) return;
  await recordAwait(loadVariants(true), __recordScope);
  // 卡片里不再摆参数与候选列表。
  //   她的原话：「筛群的参数依据是要点击运行之后弹出的，并且最终显示到任务完成详情里」。
  //   卡片上常驻一块参数框，一来把卡片撑高（三列布局里最怕这个），
  //   二来让人以为"不填参数这步就跑不了"，实际它只在运行时才需要定。
  if (fp) fp.innerHTML = '';

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 点「跑这一步」时，若这一步需要多套参数，先弹参数卡。
   为什么放在运行这一刻：dims 与 resolution 是"这一次怎么跑"的输入，
   不是这一步的常驻状态。放在卡片上，人一进来就以为要先填，
   其实是点了跑才需要决定。 */
function openFilterParams() {
  const picked = arr(S.filterRes);
  const dimsVal = S.filterDims || '1:20';
  const chips = RES_PRESETS.map(r =>
    '<button class="rchip' + (picked.indexOf(r) >= 0 ? ' on' : '') + '" data-res="' + esc(r) + '">'
    + esc(r) + '</button>').join('');
  let h = '<div class="vpanel" style="margin-top:0">'
    + '<div class="vrow"><label>dims</label>'
    + '<input id="filterDims" class="vinput" value="' + esc(dimsVal) + '" placeholder="例如 1:20">'
    + '</div>'
    + '<div class="vhint" style="margin-bottom:8px">取多少维做近邻图。官方脚本给的是 1:20。</div>'
    + '<div class="vrow"><label>resolution</label><div class="rchips">' + chips + '</div></div>'
    + '<div class="vhint">本次运行一套参数，结果可在「任务完成详情」查看。</div>'
    + '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn primary" data-filter-go="1">按这套参数运行</button>'
    + '<span class="small" id="filterMsg"></span></div>'
    + '</div>';
  sheet('筛群参数 · 筛群与剔样本后重聚类', h);
}

/* 参数卡里点确认：先登记每套候选，再走正常的提交路径。
   为什么不再自己拼一条指令：提交本来就有统一入口（runStep），
   它负责闸门、忙闲、服务器模式转交、回写跟踪。自己再发一条，
   会和它重复提交同一步，也绕开了那些检查。
   参数作为"这次的要求"随提交带过去，助手照它跑几组。 */
async function confirmFilterParams() {
  const __recordScope = recordContext();
  if (S.filterSubmitting) return;
  S.filterSubmitting = true;
  const submitButton = document.querySelector('[data-filter-go]');
  if (submitButton) submitButton.disabled = true;
  try {

  const dimsIn = $('#filterDims');
  if (dimsIn) S.filterDims = dimsIn.value;
  const res = arr(S.filterRes);
  const msg = $('#filterMsg');
  if (res.length !== 1) { if (msg) msg.textContent = '请选择一套 resolution。'; return; }
  if (msg) msg.textContent = '正在登记…';

  const note = await recordAwait(registerFilterVariants(), __recordScope);
  if (SHEET_STACK.length) closeSheet();
  await recordAwait(runStep('filter', note), __recordScope);

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    const message = e && e.message ? e.message : '参数保存或提交失败，请查看步骤状态后重试。';
    const msg = $('#filterMsg');
    if (msg) msg.textContent = message;
    toast(message, 'bad');
  } finally {
    S.filterSubmitting = false;
    if (submitButton) submitButton.disabled = false;
  }
}

/* 触发多组参数：为每个 resolution 各登记一套候选，并发一条指令说明清楚 */
/* 把这次要跑的几组参数登记下来，并返回一段"这次的要求"文本。
   只登记、不发送 —— 发送统一走 runStep，避免重复提交同一步。
   为什么要登记：几组结果要能在「任务完成详情」里并排看到、并挑一套往下用，
   所以每组必须先有自己的身份（id / 参数 / 输出目录）。 */
async function registerFilterVariants() {
  const scope = recordContext();
  const dimsIn = $('#filterDims');
  const dims = String(dimsIn ? dimsIn.value : S.filterDims || '').trim();
  const res = arr(S.filterRes);
  if (!dims || res.length !== 1) throw new Error('请填写 dims，并选择一套 resolution。');
  const r = String(res[0]);
  const had = arr((S.variants && S.variants.filter || {}).items);
  const same = had.find(x => x && x.params && String(x.params.dims) === dims
    && String(x.params.resolution) === r);
  const saved = await recordAwait(postJSON('/api/variants', {
    action: 'save', stage: 'filter',
    item: { id: same ? same.id : '', params: {dims: dims, resolution: r},
      label: 'dims ' + dims + ' · resolution ' + r, status: 'pending', outdir: '7.after_drop' }
  }), scope);
  if (!saved || !saved.ok) throw new Error('参数未保存，尚未提交计算。');
  const choices = arr(saved.items).filter(x => x && x.params
    && String(x.params.dims) === dims && String(x.params.resolution) === r);
  if (choices.length !== 1 || !choices[0].id) throw new Error('参数记录不唯一，尚未提交计算。');
  const selected = await recordAwait(postJSON('/api/variants', {
    action: 'choose', stage: 'filter', id: choices[0].id
  }), scope);
  if (!selected || !selected.ok || selected.chosen !== choices[0].id)
    throw new Error('参数未选用，尚未提交计算。');
  await recordAwait(loadVariants(true), scope);
  return '本次使用一套重聚类参数：dims=' + dims + '，resolution=' + r
    + '；剔除范围按本次已保存的筛群决定，输出目录为7.after_drop。';
}

function renderDelegated() {
  const host = $('#delegatedHost'); if (!host) return;
  const dl = currentDelegated();
  if (!dl) { host.innerHTML = ''; return; }
  if (dl.mode === 'observe' && dl.execId) {
    const notice = dl.ok === true ? '观察通知已送达当前会话。'
      : dl.ok === false ? '观察通知未送达。' + (dl.why ? ' ' + dl.why : '') + ' 作业状态可在步骤卡片和执行队列查看。'
      : '正在向当前会话发送观察通知。作业状态可在步骤卡片和执行队列查看。';
    host.innerHTML = '<div class="rz warn"><span class="mk">i</span><span>' + esc(notice) + '</span></div>';
    return;
  }
  // relayed 是"会话通道没连上，但指令已经进了本机队列"，我取走执行就等于交出去了。
  //   以前把它和真失败画成同一个红色，人看到"没送进"只能以为坏了、反复重点。
  const relayed = !!(dl && dl.relayed);
  const cls = dl.ok === true ? 'ok' : (relayed ? 'warn' : (dl.ok === false ? 'fail' : 'warn'));
  const mk = dl.ok === true ? '✓' : (relayed ? '!' : (dl.ok === false ? '×' : '·'));
  host.innerHTML = '<div class="rz ' + cls + '"><span class="mk">' + mk + '</span><span><b>'
    + esc(dl.title) + '</b> <span class="dt">'
    + esc(dl.ok === true
      ? '已送进会话，它会接着做。'
      : (relayed
        ? ('会话通道没连上，这段已经放进助手队列'
           + (dl.queueId ? '（' + dl.queueId + '）' : '')
           + '，我会取走执行，不用你再发一次。')
        : (dl.ok === false
          ? ('没送进：' + String(dl.why || '通道没连上').replace(/[。．.]+$/, '')
             + '。原文在下面，可自己发一次。')
          : '正在送…')))
    + '</span></span></div>'
    + (dl.ok === false && !relayed && dl.text
      ? '<details style="margin-top:6px"><summary class="small">指令原文（可复制）</summary>'
        + '<pre class="log" style="max-height:220px">' + esc(dl.text) + '</pre></details>'
      : '')
    // 全文一直在应用里可取：通道短不等于指令短。
    //   人想核对"到底交给它什么"，点开就是那一条完整指令。
    + (dl.text
      ? '<details style="margin-top:6px"><summary class="small">它拿到的完整指令（' 
        + esc(String(dl.text).length) + ' 字）</summary>'
        + '<pre class="log" style="max-height:260px">' + esc(dl.text) + '</pre></details>'
      : '');
}
async function initConfig() {
  if (sessionChanging) return;
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/config', { init: true }), __recordScope);
    toast('已生成：' + arr(r.written).join('、'), 'good');
    await recordAwait(loadRun(), __recordScope);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('生成失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}
// 把已经定下的事拼成一句话交给助手。
//   为什么必须带上：这些决定（筛群门槛、剔哪个样本、注释依据）是人在应用里
//   一个个点出来的，助手在服务器上看不到页面。不带过去，它只能自己猜一个值 ——
//   那正是"同一份数据两种跑法"的来源。
function decidedSummary() {
  const gs = arr(S.gates && S.gates.gates);
  const out = [];
  gs.forEach(g => {
    const st = S.gateState[g.id] || {};
    if (st.status !== 'answered' && st.status !== 'auto') return;
    let v = st.answer;
    if (v === null || v === undefined || v === '') return;
    if (Array.isArray(v)) v = v.join('、');
    let d = st.answer_detail;
    if (Array.isArray(d)) d = d.join('、');
    out.push((g.title || g.id) + ' = ' + v + (d && String(d).trim() ? '（' + d + '）' : '')
      + (st.status === 'auto' ? '［系统建议值］' : ''));
  });
  return out.join('；');
}

/* 勾选批量跑：按钮状态与提示。
   为什么单独一个函数：勾选发生在 change 事件里，而按钮禁用还要跟着
   「在跑」「闸门没过」这些全局状态走 —— 两处各写一份必然不一致。 */
function updateRunSelBtn() {
  const btn = $('#btnRunSel');
  const hint = $('#runSelHint');
  const pm = $('#btnPickMode');
  const pa = $('#btnPickAll');
  const on = !!S.pickMode;
  if (pm) {
    pm.textContent = on ? '退出多选' : '多选';
    pm.classList.toggle('primary', on);
  }
  if (btn) btn.style.display = on ? '' : 'none';
  if (pa) pa.style.display = on ? '' : 'none';
  if (!btn) return;
  S.picked = arr(S.picked).filter(selectableStep);
  const picked = S.picked;
  const n = picked.length;
  btn.disabled = n === 0 || S.running || !S.runDir;
  btn.textContent = n ? ('按顺序跑勾选的 ' + n + ' 环') : '按顺序跑勾选的';
  if (pa) {
    const runnable = (S.steps || []).filter(s => ['env', 'decide', 'precheck'].indexOf(s.id) < 0 && selectableStep(s.id));
    const allOn = runnable.length > 0 && runnable.every(s => picked.indexOf(s.id) >= 0);
    pa.textContent = allOn ? '取消全选' : ('全选 ' + runnable.length + ' 环');
  }
  if (hint) {
    if (!on) hint.textContent = '';
    else if (!n) hint.textContent = '勾几个环节，按流程顺序一起发。';
    else if (S.batch) hint.textContent = S.batch.note || '';
    else hint.textContent = '会按流程顺序一环一环发，不并发。';
  }
}

/* 进入 / 退出多选模式。
   退出时清空勾选 —— 留着勾选但看不见，下次进来会莫名其妙多选几环。 */
function togglePickMode() {
  S.pickMode = !S.pickMode;
  if (!S.pickMode) S.picked = [];
  updateRunSelBtn();
  renderRun();
}

/* 全选 / 取消全选。只选真正在跑分析的那些环节，
   环境体检与预检不算 —— 它们属于「保存并识别」那一趟。 */
function togglePickAll() {
  const PRE = ['env', 'decide', 'precheck'];
  const runnable = (S.steps || []).filter(s => PRE.indexOf(s.id) < 0 && selectableStep(s.id)).map(s => s.id);
  const cur = Array.isArray(S.picked) ? S.picked : [];
  const allOn = runnable.length > 0 && runnable.every(id => cur.indexOf(id) >= 0);
  S.picked = allOn ? [] : runnable.slice();
  updateRunSelBtn();
  renderRun();
}

/* 按流程顺序跑勾选的环节。
   为什么不并发：同一条链上两个环节同时写产物会互相覆盖，
   而且闸门是「上一步 verified 才能开下一步」，并发发出去后面几条必然 409。
   所以这里串行发：一条发完再发下一条，被拦下就停，并把原因说清。 */
async function runPickedSteps() {
  if (sessionChanging) return;
  const __recordScope = recordContext();
  try {

  const picked = arr(S.picked).filter(selectableStep);
  S.picked = picked.slice();
  if (!picked.length) return;
  // 按应用给的流程顺序排，不按勾选先后 —— 勾的先后不代表该跑的先后。
  const order = (S.flow && Array.isArray(S.flow.order)) ? S.flow.order : [];
  const seq = picked.slice().sort((a, b) => {
    const ia = order.indexOf(a), ib = order.indexOf(b);
    return (ia < 0 ? 999 : ia) - (ib < 0 ? 999 : ib);
  });
  S.batch = { seq: seq, i: 0, note: '准备发第 1 / ' + seq.length + ' 环…' };
  updateRunSelBtn();
  for (let k = 0; k < seq.length; k++) {
    const id = seq[k];
    if (!selectableStep(id)) continue;
    S.batch = { seq: seq, i: k, note: '正在发第 ' + (k + 1) + ' / ' + seq.length + ' 环：' + stepCN(id) };
    updateRunSelBtn();
    try {
      const body = { step: id, gate_note: decidedSummary() };
      const r = await recordAwait(postJSON('/api/run', body), __recordScope);
      if (!r || r.ok === false) {
        // 被闸门或决策挡住：停在这里，后面的不再发。
        //   继续发只会得到一串同样的 409，看不出真正卡在哪一环。
        S.batch = { seq: seq, i: k, stop: true,
                    note: '停在第 ' + (k + 1) + ' 环（' + stepCN(id) + '）：'
                      + String((r && (r.error || r.code)) || '被拦下') };
        updateRunSelBtn();
        toast('停在第 ' + (k + 1) + ' 环：' + stepCN(id), 'bad');
        renderRun();
        return;
      }
      if (r.delegated) {
        const sendTxt = r.session_text || r.instruction || '';
        // 已入队时服务端回 mode=observe，进会话的那条只是"看着"。
        //   把 purpose 一起带过去，队列与回执里都留得下这个标记。
        await recordAwait(injectToSession(sendTxt, function () {},
          r.mode === 'observe' ? 'observe' : 'execute', {step:id, reqId:r.req_id || r.request_id || ''},
          // 这条路径就是「一键全流程」，标题固定写它自己。
          //   不能读全局的 lastInvokeLabel：那是上一张卡片的字，
          //   借过来就会变成"我点的是全流程，会话里却写着别的东西"。
          '一键全流程'), __recordScope);
      }
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;

      S.batch = { seq: seq, i: k, stop: true,
                  note: '第 ' + (k + 1) + ' 环发送失败：' + String(e.message || e) };
      updateRunSelBtn();
      renderRun();
      return;
    }
    await recordAwait(new Promise(res => setTimeout(res, 1200)), __recordScope);
  }
  S.batch = { seq: seq, i: seq.length, done: true,
              note: '勾选的 ' + seq.length + ' 环都发出去了，等各自回写。' };
  updateRunSelBtn();
  toast('勾选的 ' + seq.length + ' 环都发出去了。', 'good');
  renderRun();

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

function runAllWithOverwriteConfirm() {
  if (sessionChanging) return;
  const ok = window.confirm('重新运行全流程会覆盖当前运行目录中的结果。是否从第一环重新开始？');
  if (!ok) return;
  const req = 'chain-' + Date.now() + '-' + Math.random().toString(36).slice(2, 10);
  runStep('all', null, true, { overwrite: true, req_id: req });
}

async function runStep(id, reqNote, force, extra = {}) {
  if (sessionChanging) return;
  const __recordScope = recordContext();
  try {

  const originLabel = extra.originLabel || takeInvokeLabel();
  if (S.running && !S.needsRemote && !extra.overwrite) return;
  // 同一步骤已有任务在跑：先说清后果，别让人以为只是排个队。
  //   这条对「按原样重跑」和「交给助手去跑」都生效 ——
  //   两个按钮都会真的提交一次作业，覆盖风险和入口无关。
  const busy = busyOf(id);
  if (busy && !S.needsRemote && !force && !extra.overwrite) {
    showBusySheet(id, busy, function () { runStep(id, reqNote, true, {...extra, originLabel}); });
    return;
  }
  S.running = true; S.step = id; S.code = null; S.lastLogLen = 0;
  renderRun();
  // 注意：这里不先 startPolling()。
  //   以前是先轮询再发请求，第一次轮询跑在请求之前，读到的还是"没开始"的
  //   旧状态 —— wasRunning=true、running=false，于是立刻弹一句"没跑通"。
  //   服务器模式下更明显：那一步本来就是"交出去、等回写"，页面不该自己判失败。
  try {
    const body = { step: id, gate_note: decidedSummary() };
    if (extra && extra.overwrite) body.overwrite = true;
    if (extra && extra.req_id) body.req_id = String(extra.req_id);
    // 再次交给助手时用户写的具体要求，单独一个字段往后传。
    //   不和 gate_note 混：一个是流程闸门的答案，一个是这次追加的要求，
    //   混在一起事后分不清哪句是当初定的。
    if (reqNote && String(reqNote).trim()) body.req_note = String(reqNote).trim();
    const r = await recordAwait(postJSON('/api/run', body), __recordScope);
    if (r && r.duplicate_dispatch) {
      S.running = false;
      renderRun();
      toast('已找到这一步的作业' + (r.exec_id ? '（' + r.exec_id + '）' : '') + '，请在执行队列查看。', 'info');
      startPolling();
      return;
    }
    // 服务器模式：这一步不由本机执行，服务端把它变成一条任务交回来。
    //   这里必须真的发出去 —— 只显示"请到助手那边跑"等于按钮是死的，
    //   用户点了只会看到一条报错。发出去之后状态才从"空闲"变成"已交给助手"。
    if (r && r.delegated) {
      const delegated = {sessionId: S.sessionId, step: id, title: r.title || id, at: Date.now(), ok: null, why: '',
        text: r.instruction || '', mode: r.mode || 'execute', execQueued: !!r.exec_queued,
        execId: r.exec_id || '', execJob: r.exec_job || ''};
      S.delegated = delegated;
      if (delegated.execQueued) {
        S.running = false;
        startPolling();
      }
      renderRun();
      // 进会话的只用短的那一段：长指令会超过通道载荷上限，被平台判无效。
      //   短的那段带着 /api/brief 的地址，助手按地址取全文，一条不落地照做。
      //   已入队时那段是"只观察"的，purpose 一起带过去，队列里也留得下标记。
      const sendTxt = r.session_text || r.instruction || '';
      const purpose = r.mode === 'observe' ? 'observe' : 'execute';
      const stepLabel = stepTitleCN(id) || (id === 'all' ? '一键全流程' : r.title || id);
      const title = originLabel && originLabel !== stepLabel ? originLabel + ' · ' + stepLabel : stepLabel;
      const res = await recordAwait(injectToSession(sendTxt, function () {}, purpose,
        {step:id, reqId:r.req_id || r.request_id || ''}, title), __recordScope);
      // 这一步的作业在服务端就已经排进执行队列了（r.delegated 本身就是"已入队"）。
      //   注入会话只是顺带通知助手，不是被执行的前提。
      //   以前把注入结果当成败判据：会话通道一断，页面就报"没送进"，人以为活没派出去，
      //   实际作业已经躺在队列里、执行端每分钟都会来领。
      if (S.delegated !== delegated) return;
      delegated.ok = !!res.ok;
      delegated.why = res.ok ? '' : (res.why || res.code || '');
      // 文案必须说清"谁在跑"：作业已入队时，跑的是执行端，不是助手。
      //   以前无论哪种情况都说"已交给助手"，于是人在会话里看到助手又要跑一遍，
      //   以为这是正常的第二步 —— 实际那是重复提交。
      if (S.delegated.execQueued && r.mode === 'observe') {
        toast('作业已排进执行队列（' + (r.exec_id || '已登记') + '），执行端会提交跑；'
          + '会话那边只让它观察进度。' + (res.ok ? '' : '会话通知没发出去，不影响这条作业。'), 'good');
      } else if (res.ok) {
        toast('已交给助手：' + S.delegated.title + '。跑完会写回。', 'good');
      } else if (S.delegated.execQueued) {
        toast('已排进执行队列，执行端会自动取走跑。会话通知没发出去，不影响这条作业。', 'good');
      } else {
        toast('指令没能送进会话：' + String(res.why || res.code || '通道没连上').replace(/[。．.]+$/, '')
          + (res.queued ? '。已入队，每小时自动重试，回写后停止。' : '。原文在下面，可自己发。'), 'bad');
      }
      if (!delegated.execQueued) S.running = false;
      renderRun();
      // 交接出去之后继续盯着：助手回写的那一刻，这一栏要自己变。
      //   这里只是"等结果"，不是"在跑" —— 所以 pollStatus 对服务器模式走另一条判据。
      startPolling();
      return;
    }
    toast('已开始：' + id, 'good');
    if (r && r.steps) S.queue = r.steps;
    startPolling();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    S.running = false;
    // 被质检闸门拦下不是"跑失败"：那一步根本没开始跑。
    //   分开来说，人才能判断该补质检还是该修脚本。
    if (e && e.status === 409 && e.body && e.body.blocked_review) {
      S.delegated = null;
      renderRun();
      toast(e.body.error, 'bad');
      loadFlow();
      return;
    }
    // 被"要你先定注释依据"拦住：同样不是失败，是流程在等人选依据。
    //   上一版没这道拦截，点了就直接注入会话，人根本没机会选 —— 这正是
    //   gina 看到的"没有弹出任何选择，直接就注入会话了"。
    if (e && e.status === 409 && e.body && e.body.code === 'need_reference') {
      S.delegated = null;
      renderRun();
      await recordAwait(loadReferences(), __recordScope);
      const pm = (e.body && e.body.pref_mode) || '';
      // 已经选过来源就照它办，不再弹那张三页签的卡：
      //   上传 -> 直接开文件框；文献库 -> 直接按存档号交给助手去取；
      //   服务器路径 -> 打开服务器那一页（只有这种情况真的需要人挑一行）。
      //   什么都没选（包括"先不定"）才弹卡片，让人当场定一次。
      if (pm === 'upload') {
        pickRefFile();
        return;
      }
      if (pm === 'lit') {
        const ano = String((e.body && e.body.ref_archive) || '');
        if (ano) {
          const r2 = await recordAwait(postJSON('/api/reference/litdict', {
            ref_archive: ano,
            species: (S.references && S.references.species) || '',
            tissue: (S.references && S.references.tissue) || '',
            session_id: S.sessionId || '',
          }), __recordScope);
          toast(r2 && r2.ok ? ('已交给助手按存档号 ' + ano + ' 去取。')
                            : ('没能发起检索：' + ((r2 && r2.error) || '未知')), 
                r2 && r2.ok ? 'good' : 'bad');
          return;
        }
        toast('这次走文献库，但还没填存档号。到「运行位置」里填上再来。', 'bad');
        return;
      }
      openRefPicker(id, function () { runStep(id, reqNote, true); }, pm);
      return;
    }

    // 被"要你先定一件事"拦住：这不是失败，是流程在等人。
    //   以前没有这一种，一律被当成"跑失败"，于是看起来像脚本出错。
    if (e && e.status === 409 && e.body && e.body.code === 'need_decision') {
      S.delegated = null;
      renderRun();
      await recordAwait(loadDecisions(), __recordScope);
      const n = (e.body.items || []).length || e.body.pending || 1;
      toast('这一步要你先定 ' + n + ' 件事，问题已经发到会话里了。', 'bad');
      const sec = document.getElementById('secGates');
      if (sec) sec.scrollIntoView({ behavior: 'smooth', block: 'start' });
      return;
    }
    S.code = -1;
    renderRun();
    toast(e.message, 'bad');
    stopPolling();
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}
async function stopRun() {
  if (sessionChanging) return;
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/stop', {}), __recordScope);
    toast(r.msg || '已发出停止指令', '');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('停不下来：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* DB observations carry provenance and attempt identity; neither historical
   snapshots nor transport errors prove current process liveness. The steps
   fallback supports older API responses without manufacturing stdout. */
function renderRunLog(st, failed = false) {
  const box = $('#logBox'), hint = $('#logHint');
  if (!box || !hint) return;
  const remote = !!((st && st.remote) || S.needsRemote);
  const failNote = $('#logFailNote'), fold = $('#logFold');
  // 失败态没有正文可展开：折叠标题（含「加载失败 · 运行日志」那一段）
  // 整块收起，只留一行说明，避免同一个失败理由出现两遍。
  const showFold = (on) => { if (fold) fold.hidden = !on; };
  if (failed) {
    showFold(false);
    if (failNote) {
      failNote.hidden = false;
      failNote.textContent = '本次未能读取运行日志，请稍后重试。读取失败不代表任务未启动或已停止。';
    }
    return;
  }
  showFold(true);
  if (failNote) { failNote.hidden = true; failNote.textContent = ''; }
  const times = $('#monitorTimes');
  if (times) {
    times.hidden = false;
    times.textContent = '最新接收回写：' + ((st && st.latest_received_at) || '未知（无本次接收记录）')
      + ' · 本次读取：' + ((st && st.log_read_at) || '服务器未提供读取时间');
  }
  if (st && Array.isArray(st.log_entries)) {
    const provenance = {callback:'执行端回写', history_snapshot:'历史回写快照（非实时 stdout）', local_snapshot:'本机日志内容快照'};
    const states = {delegated:'已交接，待执行确认', running:'回写为运行中', done:'回写为完成（非验证通过）', failed:'回写为失败', stopped:'回写为停止'};
    hint.textContent = '数据库日志 · 页面最近最多 200 条（当前 ' + st.log_entries.length + ' 条）；下载为全量数据库观察，含多次完整内容快照，非去重 stdout';
    box.textContent = st.log_entries.length ? st.log_entries.map(x =>
      '阶段：' + (x.step_id || '未知') + (x.sub_id ? (' / ' + x.sub_id) : '')
      + ' · ' + (states[x.status] || '状态未知') + ' · req_id：' + (x.req_id || '未知')
      + '\n来源：' + (provenance[x.provenance] || '未知') + ' / ' + (x.source || '未提供')
      + '\n来源时间：' + (x.source_at || '未知') + ' · 接收时间：' + (x.received_at || '未知')
      + ' · 入库时间：' + (x.observed_at || '未知')
      + '\n' + (x.content ? (x.content.length > 40000 ? '（页面仅显示正文末尾；完整内容见下载）\n' + x.content.slice(-40000) : x.content) : '本次未提供日志正文。')).join('\n\n')
      : '没有已持久化的日志观察记录；不能据此判断任务是否启动。';
    return;
  }
  const atBottom = box.scrollTop + box.clientHeight >= box.scrollHeight - 30;
  const text = String((st && st.log) || '');
  if (remote) {
    const rows = arr(st && st.steps).filter(x => x && x.id);
    const statusNames = Object.assign({}, ST_CN, { delegated: '已交接，待执行确认', running: '回写为运行中' });
    const imported = rows.filter(x => x.imported_at).length;
    hint.textContent = rows.length ? ('执行记录 · ' + imported + ' 个阶段有回写，' + (rows.length - imported) + ' 个待确认')
      : (text ? '远程回写日志' : '暂无远程回写');
    box.textContent = rows.length ? rows.map(x => {
      const label = x.display_status === 'handoff_unconfirmed' ? '交接未确认' : (statusNames[x.status] || '状态未知');
      return '阶段：' + (x.title || x.id) + ' · ' + label
        + (x.imported_at ? ('\n回写时间：' + x.imported_at) : ('\n交接时间：' + (x.delegated_at || '未提供') + '；尚无执行端回写'))
        + (x.source_host ? (' · 来源：' + x.source_host) : '')
        + '\n' + (String(x.log || '').trim() ? String(x.log) : '此阶段尚未提供日志正文。');
    }).join('\n\n') : (text || '尚未收到这份分析的远程执行回写；这不代表服务器未启动。');
  } else {
    hint.textContent = text ? ('本机进程 · ' + text.split('\n').length + ' 行') : '暂无本机日志';
    box.textContent = text || '当前没有可读取的本机进程日志；远程执行记录由执行端回写，不是本机标准输出。';
  }
  if (atBottom) box.scrollTop = box.scrollHeight;
}

/* ---------------- 轮询状态 ---------------- */
let monitorFlight = null;
function startPolling() {
  if (S.pollTimer || !S.sessionId || document.hidden) return;
  pollStatus();
  S.pollTimer = recordInterval(pollStatus, 5000);
}
function stopPolling() {
  if (S.pollTimer) { clearInterval(S.pollTimer); recordTimers.delete(S.pollTimer); S.pollTimer = null; }
}
document.addEventListener('visibilitychange', () => {
  if (document.hidden) stopPolling(); else startPolling();
});
async function pollStatus() {
  const scope = recordContext();
  if (!S.sessionId || sessionChanging || document.hidden || monitorFlight === scope) return;
  monitorFlight = scope;
  try {
    // Both panels refresh for every valid record, not just claimed running tasks.
    // 队列看板一起刷：执行端上下线、排队积压、卡住的任务都会在这 5 秒里变，
    //   只是"点了才刷新"的话，人看到的永远是点下去那一刻的样子。
    await Promise.all([refreshStatusOnce(), loadFlow(), refreshExecTasks()]);
  } finally {
    if (monitorFlight === scope) monitorFlight = null;
  }
}
async function downloadRunLog() {
  const scope = recordContext(), sid = S.sessionId;
  if (!sid || sessionChanging) return;
  try {
    const response = await recordAwait(fetch(apiUrl('/api/run/logs/download?session=' + encodeURIComponent(sid)),
      {credentials:'same-origin'}), scope);
    if (!response.ok) throw new Error('日志下载失败（HTTP ' + response.status + '）');
    const blob = await recordAwait(response.blob(), scope);
    const url = URL.createObjectURL(blob), a = document.createElement('a');
    a.href = url; a.download = 'run-log-' + sid.replace(/[^a-zA-Z0-9_-]/g, '_') + '.txt';
    a.click(); setTimeout(() => URL.revokeObjectURL(url), 0);
  } catch (e) { if (recordCurrent(scope)) toast(e.message, 'bad'); }
}
document.addEventListener('click', e => { if (e.target.closest('#btnDownloadRunLog')) downloadRunLog(); });
async function refreshStatusOnce() {
  const __recordScope = recordContext();
  try {

  try {
    const st = await recordAwait(getJSON('/api/status'), __recordScope);
    renderRunLog(st);
    S.running = st.remote ? (st.status === 'running' || st.has_running === true || arr(st.steps).some(x => x.status === 'running')) : st.status === 'running';
    S.blocked = st.status === 'blocked';
    S.step = st.step || null;
    S.code = st.code === undefined ? null : st.code;
    renderRun();
    // 服务器模式：每步到哪了只能靠助手回写，所以顺手把这份状态收下来。
    if (Array.isArray(st.steps)) {
      const m = {};
      st.steps.forEach(s => { if (s && s.id) m[s.id] = s; });
      S.remoteSteps = m;
      renderRun();
    }
  } catch {
    if (!recordCurrent(__recordScope)) return;
    renderRunLog(null, true); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* ---------------- 设置表 ---------------- */
function renderSettings() {
  const host = $('#settingsHost'), note = $('#settingsNote');
  if (!S.pipeline) {
    host.innerHTML = '<div style="color:#7c857f">这个运行目录里还没有 pipeline.json。'
      + '<div class="btnrow" style="margin-top:12px"><button class="btn primary" id="btnInit2">生成一份默认设置</button></div></div>';
    note.textContent = '';
    const b = $('#btnInit2'); if (b) b.addEventListener('click', initConfig);
    return;
  }
  const groups = [['params', '分析参数'], ['io', '文件位置'], ['environment', '本机环境']];
  let total = 0, gate = 0, html = '';
  groups.forEach(([sec, title]) => {
    const obj = S.pipeline[sec] || {};
    const keys = Object.keys(obj);
    if (!keys.length) return;
    const rows = keys.map(k => {
      const node = obj[k];
      const val = (node && typeof node === 'object' && 'value' in node) ? node.value : node;
      const src = (node && typeof node === 'object' && node.source) ? node.source : 'unspecified';
      const locked = !!(node && typeof node === 'object' && node.locked);
      const why = (node && typeof node === 'object' && node.why) ? node.why : '';
      total++; if (src === 'gate') gate++;
      const L = plabel(k);
      const changed = !!S.dirtyParams[sec + '.' + k];
      const vstr = (val === null || val === undefined) ? '' : (Array.isArray(val) ? val.join(', ') : String(val));
      return '<tr class="' + (changed ? 'changed' : '') + '">'
        + '<td class="pname">' + esc(L[0]) + (L[1] ? '<small>' + esc(L[1]) + '</small>' : '')
        + (why ? '<small style="color:#a1620a">' + esc(why) + '</small>' : '') + '</td>'
        + '<td class="pval"><input type="text" data-pinput="' + esc(sec + '.' + k) + '" value="' + esc(vstr)
        + '"' + (locked ? ' disabled' : '') + ' placeholder="待填"></td>'
        + '<td>' + srcTag(src) + '</td>'
        + '<td><button class="lockbtn' + (locked ? ' on' : '') + '" data-plock="' + esc(sec + '.' + k) + '">'
        + (locked ? '已锁定' : '可改') + '</button></td>'
        + '<td>' + (src === 'gate' ? '<button class="goto" data-goto="' + esc(k) + '">去答复</button>' : '') + '</td>'
        + '</tr>';
    }).join('');
    html += '<div class="blocklabel" style="margin-top:' + (html ? '20px' : '0') + '">' + esc(title) + '</div>'
      + '<table class="pt"><thead><tr><th>项目</th><th>当前值</th><th>谁定的</th><th>能不能改</th><th></th></tr></thead><tbody>'
      + rows + '</tbody></table>';
  });
  host.innerHTML = html || '<div style="color:#7c857f">没有可显示的设置。</div>';
  note.textContent = total + ' 项' + (gate ? ('，其中 ' + gate + ' 项等你定') : '');
}

/* ---------------- 保存 ---------------- */
function buildPipelineOut() {
  const p = JSON.parse(JSON.stringify(S.pipeline || {}));
  Object.keys(S.dirtyParams).forEach(key => {
    const parts = key.split('.'), sec = parts[0], k = parts.slice(1).join('.');
    if (!p[sec]) p[sec] = {};
    if (!p[sec][k] || typeof p[sec][k] !== 'object') p[sec][k] = { value: null };
    p[sec][k].source = 'user';
  });
  if (!p.case) p.case = {};
  p.case.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
  return p;
}
function buildGatesOut() {
  const g = JSON.parse(JSON.stringify(S.gates || { gates: [] }));
  g.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
  g.gates = arr(g.gates).map(one => {
    const st = S.gateState[one.id] || {};
    if (st.status === 'answered' && st.answer !== null && st.answer !== undefined) {
      one.status = 'answered'; one.answer = st.answer; one.answer_detail = st.answer_detail;
      one.answered_at = one.answered_at || new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
      one.answered_by = 'user';
    } else if (st.status === 'auto') {
      one.status = 'auto';
      if (one.answer === undefined || one.answer === null) one.answer = st.answer;
    } else { one.status = 'pending'; }
    return one;
  });
  return g;
}
async function saveAll() {
  if (sessionChanging) return;
  const __recordScope = recordContext();
  try {

  if (!S.runDir) { toast('先选运行目录。', 'bad'); return; }
  const payload = {};
  if (S.pipeline) payload['pipeline.json'] = buildPipelineOut();
  if (S.gates) payload['gates.json'] = buildGatesOut();
  if (!Object.keys(payload).length) { toast('还没有可保存的内容。', 'bad'); return; }
  try {
    const r = await recordAwait(postJSON('/api/config', payload), __recordScope);
    // 服务器模式：配置要落在服务器的结果目录里，本机写不了。
    //   服务端把这件活变成一条指令交回来，这里必须真的发出去 ——
    //   不然「保存我的设置」在服务器模式下就是一句空话。
    if (r && r.delegated) {
      S.delegated = { sessionId: S.sessionId, step: 'config', title: '生成配置文件', at: Date.now(), ok: null, why: '', text: r.instruction || '' };
      renderRun();
      const res = await recordAwait(injectToSession(r.instruction || '', function () {}, undefined, undefined, '保存我的设置'), __recordScope);
      S.delegated.ok = !!res.ok;
      S.delegated.why = res.ok ? '' : (res.why || res.code || '');
      renderRun();
      if (res.ok) toast('已交给助手。', 'good');
      else toast('设置没能送进会话：' + (res.why || res.code || '通道没连上')
        + (res.queued ? '。已入队，每小时自动重试。' : '。原文已摆在下面。'), 'bad');
      return;
    }
    S.dirtyParams = {};
    if (payload['gates.json']) {
      S.gates = payload['gates.json'];
      arr(S.gates.gates).forEach(g => {
        const st = (g.status === 'answered') ? 'answered' : (g.status === 'auto' ? 'auto' : 'pending');
        S.gateState[g.id] = {
          status: st,
          answer: g.answer === undefined ? null : g.answer,
          answer_detail: g.answer_detail === undefined ? null : g.answer_detail,
        };
      });
    }
    renderAll();
    toast('已保存。', 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('保存失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* ---------------- 产物 ---------------- */
/* 请求把某个产物从服务器取回来。
   为什么要点一下才取：取文件要过内网，全量同步几十兆不现实；
   而且人往往只想看某一张图。 */ 
async function requestArtifact(rel, btn) {
  const __recordScope = recordContext();
  try {

  const old = btn ? btn.textContent : '';
  const item = arr(S.artifacts).find(a => String(a.rel || '') === String(rel));
  if (item && ['pending', 'running'].includes(item.fetch_state)) {
    toast('已在取回队列中，可点击刷新查看进度。', 'good'); return;
  }
  if (btn) { btn.disabled = true; btn.textContent = '登记中…'; }
  try {
    const r = await recordAwait(postJSON('/api/artifact/request', {
      rel: rel, session_id: S.sessionId || '',
      bytes: item && Number.isFinite(Number(item.size)) ? Number(item.size) : null,
      confirm: true,
    }), __recordScope);
    if (!r || !r.ok) {
      toast('登记失败：' + ((r && r.error) || ''), 'bad');
      if (btn) { btn.disabled = false; btn.textContent = old; }
      return;
    }
    if (r.ready) { toast('已经取回了。', 'good'); await recordAwait(refreshArtifacts(), __recordScope); return; }
    toast('已登记，等执行端取回（一般一两分钟内）。', 'good');
    if (item) { item.fetch_state = 'pending'; item.fetch_error = ''; }
    if (btn) btn.textContent = '正在取回…';
    // 轮询一次：取回后这行会自己变成「打开 / 查看」。
    let n = 0;
    const timer = recordInterval(async () => {
  const __recordScope = recordContext();
  try {

      n += 1;
      await recordAwait(refreshArtifacts(), __recordScope);
      // 取回完成后就地重画详情：否则详情里的格子会一直停在“正在取回…”。
      const cur0 = arr(S.artifacts).find(a => String(a.rel || '') === String(rel));
      if (cur0 && cur0.mirrored) refreshOpenStepDetail();
      const still = document.querySelector('[data-artget="' + rel.replace(/"/g, '\\"') + '"]');
      const current = arr(S.artifacts).find(a => String(a.rel || '') === String(rel));
      if (!still || (current && current.mirrored) ||
          (current && current.fetch_state === 'error') || n >= 40) {
        clearInterval(timer);
      }
    
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}, 6000);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    toast('登记失败：' + String(e.message || e), 'bad');
    if (btn) { btn.disabled = false; btn.textContent = old; }
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 表格预览：把前若干行渲染成表。
   为什么不在应用里直接塞个 Excel 组件：这些 .xls 多半是制表符分隔的文本，
   真正需要的是"看一眼对不对"，不是编辑。 */ 
async function openArtPreview(rel) {
  const __recordScope = recordContext();
  try {

  sheet('查看表格 · ' + String(rel).split('/').pop(), '<div class="sh-p">正在读…</div>');
  try {
    const r = await recordAwait(getJSON('/api/artifact/preview?session=' + encodeURIComponent(S.sessionId || '')
      + '&rel=' + encodeURIComponent(rel)), __recordScope);
    if (!r || !r.ok) {
      sheet('查看表格 · ' + String(rel).split('/').pop(),
        '<div class="sh-p">' + esc((r && r.error) || '读不出来') + '</div>');
      return;
    }
    const cols = arr(r.columns);
    let h = '<div class="sh-p">' + esc(rel)
      + (r.note ? '　' + esc(r.note) : '') + '</div>';
    h += '<div class="tblwrap"><table class="dtbl"><thead><tr>';
    cols.forEach(c => { h += '<th>' + esc(c) + '</th>'; });
    h += '</tr></thead><tbody>';
    const cells = arr(r.rows);
    const nrow = cells.length ? cells[0].length : 0;
    for (let i = 0; i < nrow; i += 1) {
      h += '<tr>';
      for (let j = 0; j < cols.length; j += 1) {
        const v = cells[j] ? cells[j][i] : '';
        h += '<td>' + esc(v == null ? '' : String(v)) + '</td>';
      }
      h += '</tr>';
    }
    h += '</tbody></table></div>';
    // 看完表必须能直接下载：预览只是核对，交付要的是文件本身。
    h += '<div class="btnrow" style="margin-top:12px">'
      + '<button class="btn" data-tblask="1">提问</button>'
      + '<a class="btn primary" href="' + apiUrl('api/artifact/download')
      + '?session=' + encodeURIComponent(S.sessionId || '')
      + '&rel=' + encodeURIComponent(rel) + '">下载这份表格</a>'
      + '<span class="small">' + esc(rel) + '</span></div>';
    // 和大图同一套：点「提问」才出现输入框
    h += '<div class="askbox" id="askBox" style="display:none">'
      + '<textarea id="askInput" class="l1ask" placeholder="针对这份表想问什么？例如：这里的细胞比例和上游统计对得上吗"></textarea>'
      + '<div class="btnrow" style="margin-top:8px">'
      + '<button class="btn primary" data-tblaskgo="1">连同这份表和它的信息发进会话</button>'
      + '<span class="small" id="askMsg"></span></div></div>';
    const box = sheet('查看表格 · ' + String(rel).split('/').pop(), h, null, { wide: true });
    box.classList.add('sheet-tbl');
    box.addEventListener('click', function (e) {
      if (e.target.closest('[data-tblask]')) {
        const b = box.querySelector('#askBox');
        if (b) {
          const on = b.style.display !== 'none';
          b.style.display = on ? 'none' : '';
          if (!on) box.querySelector('#askInput')?.focus();
        }
        return;
      }
      if (e.target.closest('[data-tblaskgo]')) {
        const q = box.querySelector('#askInput');
        askAboutArtifact(rel, q ? q.value : '', box.querySelector('#askMsg'));
      }
    });
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    sheet('查看表格 · ' + String(rel).split('/').pop(),
      '<div class="sh-p">读不出来：' + esc(String(e.message || e)) + '</div>');
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 全部取回：把清单里所有「能在应用里看」的产物一次登记给执行端。
   为什么要有这个按钮：产物有一百来项，逐个点「取回并查看」不现实；
   而人真正想要的是"能看的都拿回来，然后我翻一遍"。
   已经取回的会跳过，重复点不会重复搬。 */
async function fetchAllArtifacts() {
  const __recordScope = recordContext();
  try {

  const note = $('#artActionNote');
  const btn = $('#btnFetchAllArt');
  if (!S.artifacts || !S.artifacts.length) { toast('还没有产物清单，先刷新。', 'bad'); return; }
  const todo = S.artifacts.filter(a => a.viewable && !a.mirrored
    && !['pending', 'running'].includes(a.fetch_state)
    && !/\.(?:pptx|zip|rds|tar|gz|xz)$/i.test(a.rel || ''));
  if (!todo.length) {
    toast('没有待取回的小图表；PPT 和结果包请单独点击取回。', 'good');
    if (note) note.textContent = '没有待取回的项。';
    return;
  }
  if (btn) { btn.disabled = true; btn.textContent = '登记中…'; }
  if (note) note.textContent = '正在登记 ' + todo.length + ' 项…';
  let ok = 0;
  for (const a of todo) {
    try {
      const r = await recordAwait(postJSON('/api/artifact/request', {
        rel: a.rel, session_id: S.sessionId || '', bytes: a.size || null,
      }), __recordScope);
      if (r && r.ok) ok += 1;
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 /* 单条失败不打断整批 */ }
  }
  if (note) note.textContent = '已登记 ' + ok + ' / ' + todo.length + ' 项，等执行端取回。取回后再点刷新。';
  toast('已登记 ' + ok + ' 项，等取回。', 'good');
  if (btn) { btn.disabled = false; btn.textContent = '全部取回'; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 生成交付 PPT：素材在本机时直接生成，不等服务器。
   为什么放在本机做：以前要下发到服务器跑，缺 python / 缺模板 / 素材不齐
   任何一条都会让按钮变成死路。素材取回之后本机就能出，
   生成完直接登记成产物，点下载即可。 */
/* 打包交付物。
   为什么单独一条本机通道：pack 在流程定义里是「服务器上跑」的环节，
   服务器模式下要点到最后一环、还要等它回写。可这件事本质是本地压个 zip ——
   图已经取回本机、PPT 也已经在本机生成，绕一趟服务器只多一次等待和一个失败点。
   缺什么不硬失败，但必须把「带了什么、没带什么」如实写在结果里：
   静默打一个只有图的包，人会以为交付物齐了。 */
async function buildPack() {
  if (S.runDirRemote) { closeDlg('dlgArtifacts'); openStepDetail('pack'); return; }
  const __recordScope = recordContext();
  try {

  const note = $('#artActionNote');
  const btn = $('#btnBuildPack');
  if (btn) { btn.disabled = true; btn.textContent = '打包中…'; }
  if (note) note.textContent = '正在把本机素材压成一个 zip…';
  try {
    const r = await recordAwait(postJSON('/api/pack/build', { session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) {
      const why = (r && (r.error || r.code)) || '';
      if (note) note.textContent = '没打成包：' + why;
      toast('没打成包：' + why, 'bad');
      return;
    }
    const inc = arr(r.included);
    const lack = arr(r.lacking);
    if (note) {
      note.textContent = '已打包 ' + (r.out_rel || 'analysis_results.zip')
        + '（' + fmtSize(r.bytes) + '）'
        + '　含 ' + inc.length + ' 项'
        + (lack.length ? '　没带：' + lack.join('、') : '');
    }
    toast('打包完成，可以下载了。', 'good');
    await recordAwait(refreshArtifacts(), __recordScope);
    if (r.download_url) window.location.href = apiUrl(r.download_url.replace(/^\//, ''));
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (note) note.textContent = '打包失败：' + String(e.message || e);
    toast('打包失败：' + String(e.message || e), 'bad');
  } finally {
    if (!recordCurrent(__recordScope)) return;

    if (btn) { btn.disabled = false; btn.textContent = '打包交付物'; }
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function buildPpt() {
  // 服务器模式下这个按钮叫「查看各环节产物」：它要办的事是把清单摊开，
  //   而不是跳到 PPT 单步的详情里 —— 后者只回答一步，问的是"各环节"。
  if (S.runDirRemote) { revealStageList(); return; }
  const __recordScope = recordContext();
  try {

  const note = $('#artActionNote');
  const btn = $('#btnBuildPpt');
  if (btn) { btn.disabled = true; btn.textContent = '生成中…'; }
  if (note) note.textContent = '正在用本机素材生成 PPT，大约二十秒…';
  try {
    const r = await recordAwait(postJSON('/api/ppt/build', { session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) {
      const why = (r && (r.error || r.code)) || '';
      if (note) note.textContent = '没生成出来：' + why;
      toast('没生成出来：' + why, 'bad');
      return;
    }
    const miss = arr(r.missing_dirs);
    const skip = arr(r.skipped);
    if (note) {
      note.textContent = '已生成 ' + (r.out_rel || 'analysis_report.pptx')
        + '（' + fmtSize(r.bytes) + '）'
        + (miss.length ? '　缺目录：' + miss.join('、') : '')
        + (skip.length ? '　跳过 ' + skip.length + ' 项，见下方说明' : '');
    }
    toast('PPT 已生成，可以下载了。', 'good');
    await recordAwait(refreshArtifacts(), __recordScope);
    // 生成完直接把下载递出去，不用人再找。
    if (r.download_url) window.location.href = apiUrl(r.download_url.replace(/^\//, ''));
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (note) note.textContent = '生成失败：' + String(e.message || e);
    toast('生成失败：' + String(e.message || e), 'bad');
  } finally {
    if (!recordCurrent(__recordScope)) return;

    if (btn) { btn.disabled = false; btn.textContent = '生成交付 PPT'; }
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}


/* 等待中的文案：把“排第几位”写出来。
   为什么不再只写“正在取回…”：队列里可能积压几十项，只写这三个字，
   人看不出是在第 1 位还是第 59 位，只能反复点、反复切页面。 */
function fetchWaitLabel(a) {
  const rank = Number(a && a.fetch_rank) || 0;
  return rank > 0 ? ('正在取回 · 第 ' + rank + ' 位') : '正在取回…';
}

function artifactActions(a) {
  const rel = String(a.rel || '');
  const query = '?session=' + encodeURIComponent(S.sessionId || '') + '&rel=' + encodeURIComponent(rel);
  const download = '<a class="btn btn-sm" href="' + apiUrl('api/artifact/download')
    + query + '">下载</a>';
  if (a.mirrored) {
    if (a.view_kind === '表格') return '<button class="btn btn-sm" data-artprev="'
      + esc(rel) + '">查看</button>' + download;
    if (/\.(?:png|jpe?g|svg|pdf|html|txt|md)$/i.test(rel))
      return '<a class="btn btn-sm" target="_blank" rel="noopener" href="'
        + apiUrl('api/artifact/file') + query + '">打开</a>' + download;
    return download;
  }
  if (['pending', 'running'].includes(a.fetch_state))
    return '<button class="btn btn-sm" disabled data-artget="' + esc(rel) + '">' + fetchWaitLabel(a) + '</button>';
  const canFetch = a.fetchable === true || a.viewable === true;
  if (canFetch) {
    const delivery = /\.(?:pptx|zip|rds|tar|gz|xz)$/i.test(rel) || a.too_big;
    const label = a.fetch_state === 'error' ? '重试取回' : (delivery ? '取回供下载' : '取回并查看');
    return '<button class="btn btn-sm" data-artget="' + esc(rel) + '">' + label + '</button>'
      + (a.fetch_state === 'error' ? '<span class="artsize" title="' + esc(a.fetch_error || '')
        + '">取回失败：' + esc(a.fetch_error || '请重试或查看运行日志') + '</span>' : '');
  }
  return '<span class="artsize">' + (a.too_big ? '超过取回上限，需从服务器取' : '需从服务器取') + '</span>';
}

/* 把产物面板里的环节清单摊开并滚到可见处。
   为什么单独一个函数：清单按环节折叠着，168 个图的目录收起来时
   点「查看各环节产物」看不到任何东西，等于按钮没反应。 */
function revealStageList() {
  const host = $('#artHost');
  if (!host) return;
  host.querySelectorAll('details.artdir').forEach(el => { el.open = true; });
  const first = host.querySelector('.artsum') || host;
  if (first && first.scrollIntoView) first.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

async function refreshArtifacts() {
  const __recordScope = recordContext();
  try {

  if (!S.runDir) return;
  try {
    const r = await recordAwait(getJSON('/api/artifacts'), __recordScope);
    S.artifacts = r.items || [];
    const note = $('#artNote');
    const host = $('#artHost');
    // 服务器模式：清单只能来自执行端的回写，应用自己 ls 不到服务器磁盘。
    //   所以"空"有两种完全不同的意思，必须分开说 ——
    //   混成一句「还没有产物」，人就会以为真的白跑了。
    const remote = !!r.remote;
    // 页签「查看各环节产物」的文案是固定的，这里不再按模式改写它 ——
    //   以前服务器模式下它写的是「查看 PPT 步骤」，改名后又被这里覆盖回去，
    //   于是页签上永远是旧文案。
    const pptGen = $('#btnBuildPptGen');
    const packButton = $('#btnBuildPack');
    // 生成 PPT / 打包这两件事只有本机素材才做得了；服务器模式隐藏，不摆死按钮。
    if (pptGen) pptGen.style.display = remote ? 'none' : '';
    if (packButton) packButton.style.display = remote ? 'none' : '';
    const listed = r.listed !== false;

    if (note) {
      note.textContent = (r.outdir ? ('来自 ' + r.outdir) : '')
        + (remote && r.listed_at ? ('　列出时间 ' + String(r.listed_at).replace('T', ' ')) : '');
    }

    if (!S.artifacts.length) {
      if (remote && !listed) {
        host.innerHTML = '<div class="emptyart">还没让执行端列过产物清单。'
          + '服务器上的文件在应用这台机器上看不到，需要执行端列一次并回写。</div>';
      } else if (remote) {
        host.innerHTML = '<div class="emptyart">执行端列过了，这个目录下确实没有可下载的产物。</div>';
      } else {
        host.innerHTML = '<div class="emptyart">还没有产物。</div>';
      }
      return;
    }

    // 按环节分组，不再按目录名字母序。
    //   以前 keys.sort() 得到的是 1.doublet、10.figs、11.report、3.merge…
    //   既有排序错觉，标题又只有文件夹名，认不出这是流程的第几步。
    //   现在按流程 1..12 排，标题写环节中文名，序号与卡片上的编号一致。
    const stageOfArt = a => {
      const rel = String(a.rel || '');
      // 出图与富集共用 10.figs，只能靠子目录分开，否则第 9、10 环混成一份。
      if (rel.indexOf('10.figs/4.diff_enrich/') === 0) return 'enrich';
      if (rel.indexOf('10.figs/') === 0) return 'figs';
      // 同一份 11.report 里 zip 是打包产物，pptx 与 manifest 属于 PPT 那一步。
      if (/\.zip$/i.test(rel)) return 'pack';
      if (rel.indexOf('11.report/') === 0) return 'ppt';
      return String(a.step || '');
    };
    const groups = {};
    S.artifacts.forEach(a => {
      const k = stageOfArt(a);
      (groups[k] = groups[k] || []).push(a);
    });
    const keys = ART_STAGES.map(x => x[0]).filter(k => (groups[k] || []).length);
    // 认不出属于哪一环的不硬塞，单独列在最后并写明没归上。
    const unknown = Object.keys(groups).filter(k => keys.indexOf(k) < 0 && groups[k].length);
    let h = '<div class="artsum">' + S.artifacts.length + ' 个产物，' + keys.length + ' 个环节'
      + (remote ? '　<b>在服务器上，不在本机</b>' : '') + '</div>';
    h += '<div class="artlist">';
    const drawArtGroup = (key, title) => {
      const list = groups[key] || [];
      // 文件夹名仍然留着：取回、排错时对路径比对人话有用。
      const dirs = Array.from(new Set(list.map(a => {
        const rel = String(a.rel || '');
        return rel.indexOf('/') >= 0 ? rel.split('/')[0] : '（根目录）';
      })));
      const idx = ART_STAGES.findIndex(x => x[0] === key);
      h += '<details class="artdir"><summary>'
        + '<span class="artno">' + (idx >= 0 ? (idx + 1) : '·') + '</span>'
        + '<span class="artdirn">' + esc(title) + '</span>'
        + '<span class="artdirf">' + esc(dirs.join('、')) + '</span>'
        + '<span class="cnt">' + list.length + ' 个</span></summary>';
      h += list.map(a => {
        const rel = String(a.rel || '');
        const nm = rel.split('/').pop();
        // 能看的就直接点开：图与 PDF 浏览器内联打开，表格开预览层。
        //   还没取回的，点一下登记「我要看这个」，执行端下一轮取回来。
        //   太大或类型不支持（rds 之类）就明说，不摆点了没反应的按钮。
        const act = artifactActions(a);
        return '<div class="artrow"><span class="artkind ' + esc(a.kind) + '">' + esc(a.kind) + '</span>'
          + '<span class="artname" title="' + esc(rel) + '">' + esc(nm) + '</span>'
          + '<span class="artsize">' + fmtSize(a.size) + '</span>'
          + act
          + '</div>';
      }).join('');
      h += '</details>';
    };
    keys.forEach(k => drawArtGroup(k, (ART_STAGES.find(x => x[0] === k) || [])[1] || k));
    unknown.forEach(k => {
      const first = String(((groups[k] || [])[0] || {}).rel || '');
      drawArtGroup(k, '未归到环节（' + (first.indexOf('/') >= 0 ? first.split('/')[0] : '根目录') + '）');
    });
    h += '</div>';
    // Refresh must not collapse the folder the user is browsing.
    const openDirs = new Set(Array.from(host.querySelectorAll('details.artdir[open]'))
      .map(el => el.querySelector('.artdirn')?.textContent));
    const scrollPositions = [];
    for (let el = host; el; el = el.parentElement) {
      if (el.scrollHeight > el.clientHeight) scrollPositions.push([el, el.scrollTop]);
    }
    host.innerHTML = h;
    host.querySelectorAll('details.artdir').forEach(el => {
      el.open = openDirs.has(el.querySelector('.artdirn')?.textContent);
    });
    scrollPositions.forEach(([el, top]) => { el.scrollTop = top; });
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    $('#artHost').innerHTML = '<div class="emptyart">读产物列表失败：' + esc(e.message) + '</div>';
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   事件委托
   ===================================================================== */
document.addEventListener('click', e => {
  const t = e.target;

  const opt = t.closest('[data-gopt]');
  if (opt) {
    const gid = opt.getAttribute('data-gopt'), val = opt.getAttribute('data-val');
    const st = S.gateState[gid];
    if (st) {
      const old = st.answer;
      st.answer = val;
      if (String(old) !== String(val)) {
        const sp = detailSpec(findGate(gid), val);
        if (sp.type === 'none') st.answer_detail = null;
        if (sp.type === 'samples') st.answer_detail = [];
      }
    }
    renderGates();
    return;
  }
  const evb = t.closest('[data-evmore]');
  if (evb) { const k = 'ev_' + evb.getAttribute('data-evmore'); S[k] = !S[k]; renderGates(); return; }

  /* ---- 决策卡：确定 / 用建议值 ---- */
  const dc = t.closest('[data-dec-confirm]');
  if (dc) { answerDecision(dc.getAttribute('data-dec-confirm'), false); return; }
  const dsg = t.closest('[data-dec-sug]');
  if (dsg) { answerDecision(dsg.getAttribute('data-dec-sug'), true); return; }

  const go = t.closest('[data-gopen]');
  if (go) { const id = go.getAttribute('data-gopen'); S.gateState[id].status = 'pending'; S.gateOpen[id] = true; renderGates(); return; }

  const rc = t.closest('[data-greco]');
  if (rc) {
    const id = rc.getAttribute('data-greco'); const g = findGate(id);
    if (g && g.suggested !== undefined && g.suggested !== null) {
      const st = S.gateState[id];
      st.answer = g.suggested;
      const sp = detailSpec(g, g.suggested);
      if (sp.type === 'text') st.answer_detail = null;
      if (sp.type === 'samples') st.answer_detail = [];
      renderGates();
    }
    return;
  }
  const cf = t.closest('[data-gconfirm]');
  if (cf) {
    const id = cf.getAttribute('data-gconfirm'); const g = findGate(id); const st = S.gateState[id];
    if (!st.answer && st.answer !== 0) { toast('先选一项。', 'bad'); return; }
    const sp = detailSpec(g, st.answer);
    if (sp.type === 'text') {
      const v = st.answer_detail;
      const vs = Array.isArray(v) ? v.join(', ') : (v === null || v === undefined ? '' : String(v));
      if (!vs.trim()) { toast('还要填内容。', 'bad'); return; }
      const parts = vs.split(/[,，]/).map(x => x.trim()).filter(Boolean);
      st.answer_detail = parts.length === 1 ? parts[0] : parts;
    }
    if (sp.type === 'samples' && !arr(st.answer_detail).length) { toast('请至少勾选一个样本。', 'bad'); return; }
    st.status = 'answered';
    S.gateOpen[id] = false;
    syncGateToSettings(g, st);
    renderGates(); renderRun(); renderSettings();
    toast('记下了，记得保存。', 'good');
    return;
  }
  const lk = t.closest('[data-plock]');
  if (lk) {
    const key = lk.getAttribute('data-plock'); const parts = key.split('.');
    const sec = parts[0], k = parts.slice(1).join('.');
    const node = S.pipeline[sec][k];
    if (node && typeof node === 'object') node.locked = !node.locked;
    renderSettings();
    return;
  }
  const gt = t.closest('[data-goto]');
  if (gt) {
    const want = gt.getAttribute('data-goto');
    const hit = arr(S.gates && S.gates.gates).filter(g =>
      g.id === want || (g.id || '').indexOf(want) >= 0 || (want || '').indexOf(g.id) >= 0)[0];
    if (hit) {
      S.gateOpen[hit.id] = true; renderGates();
      const el = document.querySelector('[data-gconfirm="' + hit.id + '"]');
      if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      toast('已经跳到那一项了。', '');
    } else { toast('跑过一次聚类才出现。', 'bad'); }
    return;
  }
  // 被待答问题挡住的那一步：按钮打开决策对话框。
  //   这是 pending 唯一的人手入口，不能省。
  const dco = t.closest('[data-decopen]');
  if (dco) { openDecisionDlg(dco.getAttribute('data-decopen')); return; }
  const rn = t.closest('[data-run]');
  if (rn) {
    const rid = rn.getAttribute('data-run');
    // 记下这个按钮自己写的字，注入会话时原样当标题。
    //   为什么不再用固定的"跑这一步"（2026-11-22，gina 报「明明我点的按钮不叫这个」）：
    //     卡片按钮在不同状态下写的是"开始这一步 / 交给助手去跑 / 带新要求重跑 /
    //     按原样重跑这一环"，而注入标题一直写死"跑这一步" —— 两边对不上，
    //     人就会怀疑自己点错了。标题必须等于她看到的那行字。
    lastInvokeLabel = (rn.textContent || '').trim();
    // 筛群这一步先弹参数：dims 与 resolution 决定分成多少簇，
    //   跑一次要花很久，参数定错了就是白跑。
    if (rid === 'filter') { openFilterParams(); return; }
    runStep(rid);
    return;
  }
  // 已完成过的步骤：不是"再开一次"，而是"带着新要求再来一次"。
  //   所以先弹一个框问清这次要什么，而不是直接甩出同一条旧指令。
  const rq = t.closest('[data-req]');
  if (rq) {
    const rqid = rq.getAttribute('data-req');
    lastInvokeLabel = (rq.textContent || '').trim();
    // 已跑过的筛群，入口是「带新要求重跑」而不是「运行」——
    //   两条入口都要先弹参数卡，否则改参数这件事在重跑路径上就漏了。
    if (rqid === 'filter') { openFilterParams(); return; }
    openRetryDlg(rqid);
    return;
  }
  // 警告浮层里的"仍然再交一次"：人已经看过覆盖后果，照他的意思做。
  const bg = t.closest('[data-busy-go]');
  if (bg) {
    const sid2 = bg.getAttribute('data-busy-go');
    const f = busyForce; busyForce = null;
    const old = document.getElementById('stSheet');
    if (old && old.parentNode) old.parentNode.removeChild(old);
    if (typeof f === 'function') f();
    else runStep(sid2, null, true);
    return;
  }
  // 作废一条排队中没人认领的执行任务。
  //   它出现在两个地方：步骤卡上"这一步在跑"那张浮层里（和"仍然再交一次"并排），
  //   以及队列看板每一行末尾（那里才是"卡住了"最常见的发现处）。
  //   两处走同一个处理函数，判据由服务端给。
  const xc = t.closest('[data-exec-cancel]');
  if (xc) { cancelExecTask(xc.getAttribute('data-exec-cancel')); return; }
  // 队列看板上手动刷新一次：人怀疑"是不是没更新"时，要给一个能自己确认的动作。
  if (t.closest('[data-exec-refresh]')) { refreshExecTasks(); return; }
  // 参考表：上传 / 选用
  if (t.closest('[data-refup]')) { doRefUpload(); return; }
  if (t.closest('[data-reflit]')) { askLitDict(); return; }
  if (t.closest('[data-refscan]')) { askRefScan(); return; }
  // 注释依据卡的三页签：切页不重新拉数据，只重画这一张卡。
  const rt = t.closest('[data-reftab]');
  if (rt) {
    S.refTab = rt.getAttribute('data-reftab');
    renderRefPicker();
    return;
  }
  const rp = t.closest('[data-refpick]');
  if (rp) { pickRefAndGo(rp.getAttribute('data-refpick')); return; }
  if (t.closest('[data-refspecies]')) { saveDataScope(); return; }
  const ag = t.closest('[data-artget]');
  if (ag) { requestArtifact(ag.getAttribute('data-artget'), ag); return; }
  /* ---- 任务完成详情：看大图 / 预览表格 ---- */
  const bi = t.closest('[data-bigimg]');
  if (bi) {
    // 把这一组图一起带过去：大图里才能左右翻，不用退出再点下一张。
    const host = bi.closest('.thumbs');
    const list = host ? [...host.querySelectorAll('[data-bigimg]')]
      .map(x => x.getAttribute('data-bigimg')) : [];
    openBigImage(bi.getAttribute('data-bigimg'), list);
    return;
  }
  const tv = t.closest('[data-tableview]');
  if (tv) { openArtPreview(tv.getAttribute('data-tableview')); return; }
  const ap = t.closest('[data-artprev]');
  if (ap) { openArtPreview(ap.getAttribute('data-artprev')); return; }
  const ru = t.closest('[data-refuse]');
  if (ru) { chooseRef(ru.getAttribute('data-refuse')); return; }

  /* ---- 筛群：多选 resolution ---- */
  const resBtn = t.closest('[data-res]');
  if (resBtn) {
    const v = resBtn.getAttribute('data-res');
    S.filterRes = arr(S.filterRes);
    S.filterRes = [v];
    const dimsIn = $('#filterDims');
    if (dimsIn) S.filterDims = dimsIn.value;
    // 直接改被点那个钮的选中态。
    //   为什么不重绘整块面板：参数卡是弹层，不是卡片里那块区域；
    //   重绘卡片里的空占位不会让弹层里的按钮变样，点了就像没反应。
    document.querySelectorAll('[data-res]').forEach(x =>
      x.classList.toggle('on', x.getAttribute('data-res') === v));
    const msg = $('#filterMsg');
    if (msg) msg.textContent = '';
    return;
  }
  if (t.closest('[data-filter-go]')) { confirmFilterParams(); return; }

  /* ---- 注释依据：两个按钮各管一件事 ---- */
  const arm = t.closest('[data-annot-ref-mode]');
  if (arm) {
    const which = String(arm.getAttribute('data-annot-ref-mode') || '');
    if (which === 'saved') {
      // “已确定选择”：就是“照之前定的那份跑”。
      //   不重弹选择卡（重弹等于把同一个决定问第二遍），
      //   只把圈移回它身上，并确认那份依据还完整。
      S.annotRefMode = 'saved';
      const st = annotRefState();
      if (!st.ready) { toast('还没配好：' + st.label + '。点「修改选择」去填。', 'bad'); renderRun(); return; }
      renderRun();
      toast('这一步将按上面那份依据跑。', 'good');
      return;
    }
    // “修改选择”：先把圈移过来，再弹三选一。
    S.annotRefMode = 'edit';
    renderRun();
    const dm = (S.placeDraft && S.placeDraft.ref_mode) || '';
    openRefPicker('annotate', function () { S.annotRefMode = 'saved'; renderRun(); }, dm);
    return;
  }
  if (t.closest('[data-annot-ref]')) {
    // 页面上已经选过来源时，这个按钮就只做那一个问题的事。
    //   没选过才打开三选一的卡片（这时弹卡不是重复提问，是第一次问）。
    const dm = (S.placeDraft && S.placeDraft.ref_mode) || '';
    if (dm === 'upload') { pickRefFile(); return; }
    if (dm === 'lit') { askLitByArchive(); return; }
    openRefPicker('annotate', null);
    return;
  }

  /* ---- 候选结果：选一套 / 取消选用 ---- */
  const vc = t.closest('[data-var-choose]');
  if (vc) {
    const parts = String(vc.getAttribute('data-var-choose') || '').split('|');
    chooseVariantInDetail(parts[0], parts.slice(1).join('|'));
    return;
  }
  const vu = t.closest('[data-var-unchoose]');
  if (vu) { chooseVariantInDetail(vu.getAttribute('data-var-unchoose'), ''); return; }
});

/* 选一套候选作为下游输入。
   为什么必须有这一步：筛群与注释都会产出好几套，跑完之后到底用哪一套往下走，
   只有人能定。不选就往下跑，下游会拿到"不知道哪一套"的对象 —— 那正是要避免的。
   选完同时把结果告诉会话，让助手照这套继续。 */
async function chooseVariant(stage, id) {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/variants', { action: 'choose', stage: stage, id: id, session_id: S.sessionId || '' }), __recordScope);
    if (!r || !r.ok) { toast('选用失败：' + ((r && r.error) || ''), 'bad'); return; }
    await recordAwait(loadVariants(true), __recordScope);
    renderVarPanels();
    if (id) {
      const it = arr((S.variants && S.variants[stage] || {}).items).find(x => String(x.id) === String(id));
      const label = it ? (it.label || it.id) : id;
      const txt = ['【来自 个性化分析助手 · 选定结果】',
        S.topicId ? ('回复路由会话号：' + S.topicId) : '',
        '',
        '环节 ' + stage + ' 已经定了一套：' + label,
        it && it.outdir ? ('产物目录：' + it.outdir) : '',
        '',
        '这里只同步已选参数，请只读确认；不要因此提交或重跑任何作业。其它候选保留不删。'].filter(Boolean).join('\n');
      try { await recordAwait(injectToSession(txt, function () {}, 'query', {step:stage, reqId:''}, '同步已选参数'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 /* 通道不通不影响选定本身 */ }
      toast('已选定：' + label, 'good');
    } else {
      toast('已取消选用。', '');
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('选用失败：' + String(e.message || e), 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

document.addEventListener('input', e => {
  const t = e.target;
  if (t.matches('[data-pinput]')) {
    const key = t.getAttribute('data-pinput');
    const parts = key.split('.'), sec = parts[0], k = parts.slice(1).join('.');
    const raw = t.value;
    let parsed;
    if (raw === '') parsed = null;
    else if (/^-?\d+(\.\d+)?$/.test(raw.trim())) parsed = Number(raw.trim());
    else if (raw.indexOf(',') >= 0) parsed = raw.split(/[,，]/).map(x => x.trim()).filter(Boolean);
    else parsed = raw;
    if (!S.pipeline[sec]) S.pipeline[sec] = {};
    const node = S.pipeline[sec][k];
    if (node && typeof node === 'object') { node.value = parsed; node.source = 'user'; }
    else S.pipeline[sec][k] = { value: parsed, source: 'user' };
    S.dirtyParams[key] = true;
    const tr = t.closest('tr'); if (tr) tr.classList.add('changed');
    renderHead();
    return;
  }
  if (t.matches('[data-gdetail]')) {
    S.gateState[t.getAttribute('data-gdetail')].answer_detail = t.value;
  }
});

document.addEventListener('change', e => {
  const t = e.target;
  // 勾选要一起跑的环节。勾/取消都只是改这份清单，
  //   不在这里发请求 —— 发请求的是「按顺序跑勾选的」那个按钮。
  if (t.matches('[data-mpick]')) {
    const id = t.getAttribute('data-mpick');
    if (!Array.isArray(S.picked)) S.picked = [];
    if (t.checked && selectableStep(id)) { if (S.picked.indexOf(id) < 0) S.picked.push(id); }
    else { S.picked = S.picked.filter(x => x !== id); }
    updateRunSelBtn();
    return;
  }
  if (t.matches('[data-gsamp]')) {
    const gid = t.getAttribute('data-gsamp'), v = t.value;
    let cur = arr(S.gateState[gid].answer_detail).map(String);
    if (t.checked) { if (cur.indexOf(v) < 0) cur.push(v); } else { cur = cur.filter(x => x !== v); }
    S.gateState[gid].answer_detail = cur;
    const lab = t.closest('.sampchk'); if (lab) lab.className = 'sampchk' + (t.checked ? ' on' : '');
  }
});

/* 把闸门答复同步进设置项，让脚本直接就能读到 */
function syncGateToSettings(g, st) {
  if (!S.pipeline) return;
  const d = st.answer_detail;
  const byId = {
    data_indir(v, dd) {
      const path = (dd === null || dd === undefined) ? '' : (Array.isArray(dd) ? dd[0] : String(dd).trim());
      if (v === 'keep' || !path) return;
      S.pipeline.io = S.pipeline.io || {};
      S.pipeline.io.indir = { value: path, source: 'user', why: '由本人确认（助手第 1 项）' };
      S.dirtyParams['io.indir'] = true;
    },
    reference_source(v, dd) {
      const path = (dd === null || dd === undefined) ? '' : (Array.isArray(dd) ? dd[0] : String(dd).trim());
      S.pipeline.io = S.pipeline.io || {};
      if (v === 'skip') {
        S.pipeline.io.reference_table = { value: null, source: 'user', why: '本次跳过注释' };
        S.pipeline.io.lit_dict = { value: null, source: 'user', why: '本次跳过注释' };
      } else if (v === 'lit_dict' && path) {
        S.pipeline.io.lit_dict = { value: path, source: 'user', why: '由本人确认' };
        S.pipeline.io.reference_table = { value: null, source: 'user', why: '改用 marker 词典' };
      } else if (v === 'reference_table' && path) {
        S.pipeline.io.reference_table = { value: path, source: 'user', why: '由本人确认' };
        S.pipeline.io.lit_dict = { value: null, source: 'user', why: '改用文献参考表' };
      } else return;
      S.dirtyParams['io.reference_table'] = true;
      S.dirtyParams['io.lit_dict'] = true;
    },
    qc_threshold(v) {
      if (v === 'keep') return;
      S.pipeline.params = S.pipeline.params || {};
      S.pipeline.params.qc_min_features = { value: Number(v), source: 'user', why: '由本人确认（助手第 1 项）' };
      S.dirtyParams['params.qc_min_features'] = true;
    },
    cluster_filter(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.cluster_filter || {};
      node.source = 'user';
      if (v === 'none') { node.mode = 'none'; node.by_name_keep = []; node.by_cluster_keep = []; }
      if (v === 'by_name') {
        node.mode = 'by_name';
        node.by_name_keep = Array.isArray(dd) ? dd : (dd ? String(dd).split(/[,，]/).map(s => s.trim()) : []);
      }
      if (v === 'by_cluster') { node.mode = 'by_cluster'; node.reason = '由本人确认'; }
      node.reason = node.reason || '由本人确认';
      S.pipeline.steps.cluster_filter = node;
      S.dirtySteps['cluster_filter'] = true;
    },
    read_exclusion(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.read_exclusion || {};
      node.source = 'user';
      node.mode = (v === 'none') ? 'none' : 'exclude';
      node.samples = (v === 'none') ? [] : arr(dd).map(String);
      S.pipeline.steps.read_exclusion = node;
      S.dirtySteps['read_exclusion'] = true;
    },
    post_cluster_exclusion(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.post_cluster_exclusion || {};
      node.source = 'user';
      node.mode = (v === 'none') ? 'none' : 'exclude';
      node.samples = (v === 'none') ? [] : arr(dd).map(String);
      S.pipeline.steps.post_cluster_exclusion = node;
      S.dirtySteps['post_cluster_exclusion'] = true;
    },
    lowconfidence(v) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.lowconfidence || {};
      node.source = 'user';
      if (v === 'flag_only') { node.mode = 'flag_only'; node.min_cells = node.min_cells || 200; }
      else { node.mode = 'threshold'; node.min_cells = Number(v); }
      S.pipeline.steps.lowconfidence = node;
      S.dirtySteps['lowconfidence'] = true;
    },
    memory(v) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.memory || {};
      node.source = 'user'; node.mode = v;
      S.pipeline.steps.memory = node;
      S.dirtySteps['memory'] = true;
    },
    seurat_version(v) {
      S.pipeline.params = S.pipeline.params || {};
      S.pipeline.params.expected_seurat_major = { value: (v === 'v5_allow' ? 5 : 4), source: 'user' };
      S.dirtyParams['params.expected_seurat_major'] = true;
    },
  };
  const fn = byId[g.id];
  if (fn) { try { fn(st.answer, d); } catch { /* 忽略 */ } }
}

/* 术语卡：默认收起，想看了点一下才展开。
   用户的原话是「这些专有名词，我要专门做一个卡片，放在最下方，
   也是做一个按钮，当用户想看的时候就看就行了」——所以它不进主流程，
   不占视线，只在需要时打开。 */
const GLOSS_LIST = [
  ['入口锁', 'guard',
   '脚本开头的一把锁。这个脚本如果不是从规定的唯一入口启动的，它就直接拒绝运行，并说明原因。'
   + '目的只有一个：保证每次运行都被记录在案。被绕过偷偷跑掉的脚本，事后查不出用了什么参数。'],
  ['指纹', 'md5',
   '一串由文件内容算出来的字符。内容改一个字，它就完全不同。它不保密，只回答一个问题：'
   + '这个文件还是不是我登记时的那个。'],
  ['台账', 'ledger',
   '一本只加不改的记录本。每次运行都追加一条：什么时候跑的、用了哪些参数、退出码多少、'
   + '产物指纹是什么。事后可以核对到底用了哪一套脚本。'],
  ['白名单', 'whitelist',
   '一份登记表，写着这条流程允许用哪些脚本、各自指纹是多少。交付前会拿实际用过的脚本去比对，'
   + '对不上就不许交付。'],
  ['闸门', 'gate',
   '必须由人来定的事，比如质控下限取多少、剔除哪些簇。没答完，脚本会直接拒绝启动，'
   + '并告诉你还差哪几件。'],
  ['快照', 'snapshot',
   '把当时的设置（参数和你的答复）存一份，随时可以退回去。'],
  ['待重跑', 'stale',
   '这一步的结果做过，但你退回了更早的一步，它现在不算数了，需要重跑。'],
];

/* =====================================================================
   脚本数据库
   ---------------------------------------------------------------------
   她要看的是「库里到底有什么」，不是结论摘要：
     · 当前库是什么结构（三层，每层存什么、在哪执行）
     · 每层里有哪些条目，条目的状态与位置
     · 条目里具体是哪几个文件，点开能看正文
   所以这里按层铺开，条目不合并、不省略 —— 省略了就看不出一层缺什么。
   ===================================================================== */
const LIB_LAYER_CN = { L1_callable: 'L1 可调用库', L2_server: 'L2 环境探测', L3_app: 'L3 应用自带' };
const LIB_STATUS_CN = { ready: '可用', pending_import: '未导入', draft: '草稿' };

/* =====================================================================
   脚本数据库：独立一页
   ---------------------------------------------------------------------
   为什么不跟其它区块挤在同一页：
     它是一份资料，看的时候要一直展开、滚动、比对；挤在分析页里，
     往下滚两屏就会撞上"跑分析"，人的注意力被拉回去。
     所以进去时只留它，返回时原样恢复 —— 顺便把侧栏也收起来，
     避免点侧栏又跳回别处。
   ===================================================================== */
/* =====================================================================
   顶部三个页签：当前分析 / 历史分析 / 脚本数据库
   ---------------------------------------------------------------------
   以前「历史分析」和「脚本数据库」一个是弹窗、一个是隐藏其它区块的伪页面，
   两种做法都别扭：弹窗一关就没了，伪页面还得配一个"返回"按钮。
   现在三页并列，点哪个显示哪个，当前页的按钮亮起来 —— 位置不会丢。
   当前分析页才显示「新建分析 / 存档与步骤回滚」：它们只对这一页有意义。
   ===================================================================== */
const TABS = {
  cur: { secs: ['secPlace', 'secRun', 'secMonitor'] },
  hist: { secs: ['secHist'] },
  lib: { secs: ['secLib'] }
};
let curTab = 'cur';

function switchTab(key) {
  const cfg = TABS[key];
  if (!cfg) return;
  curTab = key;
  const parallel = document.getElementById('parallelDashboard');
  if (parallel) parallel.style.display = key === 'cur' ? '' : 'none';
  $$('#main section.sec').forEach(sec => {
    sec.style.display = cfg.secs.indexOf(sec.id) >= 0 ? '' : 'none';
  });
  // Stage visibility is derived from this record, never restored from another tab.
  setStageVisible(S.identified);
  // 页签高亮 + 动作按钮只在这一页出现
  $$('#tabs .tab').forEach(tb => {
    tb.classList.toggle('on', tb.getAttribute('data-tab') === key);
  });
  const acts = $('#tabActs');
  if (acts) acts.style.display = (key === 'cur') ? '' : 'none';
  const nav = $('#sidenav');
  if (nav) nav.style.display = (key === 'cur') ? '' : 'none';

  if (key === 'lib') renderLib();
  if (key === 'hist') { renderSessions(); }
  if (key === 'cur') { renderAll(); }
  window.scrollTo({ top: 0 });
}

async function renderLib() {
  const __recordScope = recordContext();
  try {

  const host = $('#libHost'), note = $('#libNote');
  if (!host) return;
  host.innerHTML = '<div class="small">正在读库…</div>';

  let lib = null, arch = null;
  try { lib = await recordAwait(getJSON('/api/library'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  try { arch = await recordAwait(getJSON('/api/script-archive'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}

  const records = arr(arch && arch.records);
  const recById = {};
  records.forEach(r => { recById[r.id] = r; });
  const skills = arr(lib && lib.entries).filter(e => String(e.kind || '') === 'skill');
  if (note) note.textContent = skills.length + ' 个 skill · ' + records.length + ' 次记录';

  // 环节中文名（L2/L3 都用它当标题）
  const STEP_CN = {
    env: '环境体检', doublet: '逐样本去双与质控', merge: '合并与批次校正',
    markers: '一次聚类 marker', filter: '筛群与剔样本后重聚类',
    final: '终版比例统计', annotate: '细胞类型注释',
    enrich: 'GO 富集', figs: '出图', extract: '提取报告关键指标',
    ppt: '生成交付 PPT', pack: '打包交付物',
    // 历史步骤：仅用于回放旧记录时显示标题，不参与流程顺序与计数
    fullmark: '全量 marker（历史）', diff: '组间差异（历史）'
  };
  const stepName = id => STEP_CN[id] || id || '未归类';

  // 环节顺序跟流程走，没登记的排后面
  //   fullmark / diff 已于 2026-11-21 从流程摘除（它们从来没有可执行体，
  //   产物其实是 final 产的）。这里不再列出，否则流程视图里会重新冒出来。
  //   旧记录的 step_id 仍然存在，靠 STEP_CN 兜底显示名字。
  const ORDER = ['env', 'doublet', 'merge', 'markers', 'filter', 'final', 'fullmark',
                 'annotate', 'diff', 'figs', 'enrich', 'extract', 'ppt', 'pack'];
  // 「按记录」只列从环境体检起的正式环节（gina 2026-11-19）。
  //   她的原话：「只需要留下从环境体检开始的脚本内容」。
  //   未归类的、以及不属于流程的条目（例如只在索引里留了个名字的）
  //   不再铺进来 —— 那些既不构成一次处理的流程，也读不到正文。
  const STAGE_ORDER_FROM_ENV = ['env', 'doublet', 'merge', 'markers', 'filter', 'final',
                                'fullmark', 'annotate', 'diff', 'figs', 'enrich',
                                'extract', 'ppt', 'pack'];
  const stepRank = id => {
    const k = ORDER.indexOf(id);
    return k < 0 ? 99 : k;
  };

  let h = '';

  // ============ L1 可调用库 ============
  // L1 里面还分两小层，按"这套脚本从哪来"排：
  //   ① Seurat 官方那套脚本（甲方给的第一手）
  //   ② 我们 AI 补写的 skill
  // 原来的「服务器上的脚本」那一层指向 R3-1026，而那条记录已被移除
  //   （gina 2026-11-19：她要求把 R2-1022 与 R3-1026 都去掉）。
  //   留着一个指向不存在记录的层，页面会显示成空壳，比不显示更误导。
  //   现在服务器上实际在用的脚本改为随每条分析记录自动登记到「按记录」里。
  const l1defs = [
    { key: 'seurat', name: 'Seurat 官方脚本',
      hint: '甲方最初交付的那套脚本，逐样本去双、合并聚类、出 marker 都靠它。',
      recId: 'R1-official',
      gap: '这套只收进了甲方交付过的那些文件，实际在他们服务器上可能还有别的。' },
    { key: 'skill', name: 'AI 补写的 skill',
      hint: '我们自己写的技能，服务器没有的部分靠它补。',
      recId: '',
      gap: '' }
  ];

  h += '<details class="libfold" open><summary>'
    + '<span class="libfold-n">L1</span>'
    + '<b>可调用库</b>'
    + '<span class="libfold-c">2 类 · ' + skills.length + ' 个 skill</span>'
    + '</summary><div class="libfold-b">';

  l1defs.forEach(def => {
    const isSkill = def.key === 'skill';
    let items = [];
    if (isSkill) {
      items = skills.map(sk => ({
        name: String(sk.skill_name || sk.title || sk.id),
        files: arr(sk.files),
        openable: false
      }));
    } else {
      const rec = recById[def.recId];
      const seen = {};
      items = arr(rec && rec.files)
        .filter(f => f.name && /\.(R|r|py|sh)$/.test(f.name))
        .filter(f => { if (seen[f.name]) return false; seen[f.name] = 1; return true; })
        .map(f => ({ name: f.name, record: def.recId, rel: f.rel, openable: true,
                     exists: f.exists !== false }));
    }

    // 她要求前两层也能折叠：默认收起，点开才铺文件。
    //   十几个文件名一次摊开，页面立刻被撑满，反而看不到下面还有两层。
    //   同时把标题下那句解释和底部那段"列不全"的说明都删了 ——
    //   她要的是清单，不是旁白；列不全这件事，看数量就知道。
    h += '<details class="l1box"' + (isSkill ? ' open' : '') + '>'
      + '<summary class="l1h"><b>' + esc(def.name) + '</b>'
      + '<span class="l1c">' + items.length + (isSkill ? ' 个' : ' 个脚本') + '</span>'
      + '</summary><div class="l1body">';

    if (isSkill) {
      items.forEach(it => {
        h += '<details class="libsub"><summary>'
          + '<span class="libsub-n">' + esc(it.name) + '</span>'
          + '<span class="scrtag aski">AI skill</span>'
          + '<span class="libsub-c">' + it.files.length + ' 个文件</span>'
          + '</summary><div class="libsub-b">';
        if (!it.files.length) {
          h += '<div class="small">这个 skill 没登记文件。</div>';
        } else {
          it.files.forEach(f => {
            const nm = String(f.name || '').split('/').pop();
            if (f.exists === false) {
              h += '<span class="libfile off">' + esc(nm) + ' <span class="libfile-n">缺</span></span>';
            } else {
              h += '<button class="libfile" data-lib-open="' + esc(f.path) + '">' + esc(nm)
                + (f.bytes ? '<span class="libfile-n">' + fmtSize(f.bytes) + '</span>' : '') + '</button>';
            }
          });
        }
        h += '</div></details>';
      });
    } else {
      if (!items.length) {
        h += '<div class="small">这一层还没有取回任何脚本。</div>';
      } else {
        h += '<div class="l1files">';
        items.forEach(it => {
          // 索引里有、存档里没有的，不给按钮：点开只会是空的。
          //   她的问题就是这一类 —— 列得出来但读不到。
          if (it.exists === false) {
            h += '<span class="libfile off" title="索引里有登记，存档目录里没有这个文件">'
              + esc(it.name) + ' <span class="libfile-n">缺</span></span>';
            return;
          }
          h += '<button class="libfile" data-arc-open="' + esc(it.record) + '|' + esc(it.rel) + '">'
            + esc(it.name) + '</button>';
        });
        h += '</div>';
      }
      // 只留调研入口，不再配一段解释。
      if (def.gap) {
        h += '<div class="l1gap-a">'
          + '<button class="btn btn-sm primary" data-l1probe="' + esc(def.key) + '">让助手去调研</button>'
          + '<span class="small" data-l1msg="' + esc(def.key) + '"></span>'
          + '</div>';
      }
    }
    h += '</div></details>';
  });
  h += '</div></details>';

  // ============ L2 按环节 ============
  // 同一个环节跨记录汇总：这个环节历史上用过哪些脚本、各成功几次、哪几次记录用过。
  const byStage = {};
  records.forEach(rec => {
    arr(rec.files).forEach(f => {
      const st = String(f.stage || '') || '未归类';
      const key = String(f.name || '');
      byStage[st] = byStage[st] || {};
      const cur = byStage[st][key] || { name: key, records: [], ok: 0, fail: 0, refs: [] };
      cur.records.push(rec.id);
      cur.ok += Number(f.used_ok || 0);
      cur.fail += Number(f.used_fail || 0);
      // 只要有一条记录里这个文件是真能读的，L2 这行就给按钮。
      //   以前是"最后一条覆盖"，于是最后一条恰好没正文时，
      //   明明别处有可读副本，这一行却会变成不可点。
      cur.refs.push({ record: rec.id, rel: f.rel, exists: f.exists !== false });
      if (f.exists !== false && !cur.live) { cur.live = { record: rec.id, rel: f.rel }; }
      byStage[st][key] = cur;
    });
  });
  const stageKeys = Object.keys(byStage).sort((a, b) => stepRank(a) - stepRank(b));

  h += '<details class="libfold"><summary>'
    + '<span class="libfold-n">L2</span>'
    + '<b>按环节</b>'
    + '<span class="libfold-c">' + stageKeys.length + ' 个环节</span>'
    + '</summary><div class="libfold-b">'
    + '<div class="libhint">同一个环节，历史上用过哪些脚本。点脚本看正文；'
    + '后面写着它出自哪几次记录、成功过几次。</div>';
  stageKeys.forEach(st => {
    const list = Object.values(byStage[st]).sort((a, b) => b.ok - a.ok || a.name.localeCompare(b.name));
    h += '<details class="libsub"><summary>'
      + '<span class="libsub-n">' + esc(stepName(st)) + '</span>'
      + '<span class="libsub-c">' + list.length + ' 个脚本</span>'
      + '</summary><div class="libsub-b">';
    list.forEach(it => {
      const ref = it.refs[it.refs.length - 1];
      const live = it.live || null;
      h += '<div class="librow">';
      // 有一个可读副本就给按钮，一个都没有就标缺并说明。
      if (live) {
        h += '<button class="libfile" data-arc-open="' + esc(live.record) + '|' + esc(live.rel) + '">'
          + esc(it.name) + '</button>';
      } else {
        h += '<span class="libfile off" title="只在索引里登记过，存档目录里没有正文">'
          + esc(it.name) + ' <span class="libfile-n">缺</span></span>';
      }
      h += '<span class="librow-m">'
        + '记录 ' + esc(it.records.join('、'))
        + '　成功 ' + it.ok + ' 次' + (it.fail ? '　退回 ' + it.fail + ' 次' : '')
        + '</span></div>';
    });
    h += '</div></details>';
  });
  h += '</div></details>';

  // ============ L3 按记录 ============
  h += '<details class="libfold"><summary>'
    + '<span class="libfold-n">L3</span>'
    + '<b>按记录</b>'
    + '<span class="libfold-c">' + records.length + ' 次</span>'
    + '</summary><div class="libfold-b">'
    + '<div class="libhint">每一次处理各用了一套脚本。点开一次记录，按环节看那次选的是哪一个。</div>';
  records.forEach(rec => {
    const files = arr(rec.files);
    // 一次记录里一个环节只留一个：同名重复的按名字去重
    const perStage = {};
    files.forEach(f => {
      const st = String(f.stage || '') || '未归类';
      if (!perStage[st]) perStage[st] = f;
    });
    // 只列「环境体检」及其后的环节（gina 2026-11-19）。
    //   以前把未归类的、以及不属于正式流程的条目也铺进来，她要求
    //   「只需要留下从环境体检开始的脚本内容」。
    //   env 固定排第一：那是每次真跑的第一段，其余按流程顺序。
    const sts = Object.keys(perStage)
      .filter(st => STAGE_ORDER_FROM_ENV.indexOf(st) >= 0)
      .sort((a, b) => stepRank(a) - stepRank(b));
    // 这条记录到底能读几份：0 份就在标题上明说，不要让人点进去才发现全空。
    const nLive = files.filter(f => f.exists !== false).length;
    const recTag = nLive === 0
      ? '<span class="libsub-c bad">这次没有任何脚本正文</span>'
      : (nLive < files.length
        ? '<span class="libsub-c">可读 ' + nLive + ' / ' + files.length + ' 份</span>'
        : '<span class="libsub-c">' + files.length + ' 份均可读</span>');
    // 缺正文的份数：用来决定给不给「补取正文」。
    const nMiss = files.length - nLive;
    // 右侧下载：按这条记录汇编一份 ipynb。
    //   为什么放在记录这一层而不是每个脚本一行：她要的是「这一次处理
    //   用的整脚本合集」，一个脚本一个 ipynb 反而没法看。
    const canDl = nLive > 0;
    const dlBtn = canDl
      ? '<button class="btn btn-sm" data-rec-nb="' + esc(rec.id) + '"'
        + ' title="按这条记录登记过的脚本汇编一份 ipynb">下载 ipynb</button>'
      : '<button class="btn btn-sm" disabled title="这条记录没有可读的脚本正文">下载 ipynb</button>';
    // 补取正文（gina 2026-11-20）。
    //   有脚本只在服务器上、本机没有同版本副本时，光标「缺」没有出路。
    //   这个按钮把缺的那些路径交给执行端去读回来，md5 对上才收进存档。
    const fetchBtn = nMiss > 0
      ? '<button class="btn btn-sm" data-rec-body="' + esc(rec.id) + '"'
        + ' title="让执行端按原路径把这些脚本读回来（md5 校验一致才收）">补取正文 ' + nMiss + '</button>'
      : '';
    h += '<div class="librec">'
      + '<details class="libsub"><summary>'
      + '<span class="libsub-n">' + esc(rec.name || rec.id) + '</span>'
      + '<span class="libsub-c">' + esc(rec.when || '') + ' · ' + sts.length + ' 个环节</span>'
      + recTag
      + '</summary><div class="libsub-b">';
    sts.forEach(st => {
      const f = perStage[st];
      h += '<div class="librow">'
        + '<span class="librow-k">' + esc(stepName(st)) + '</span>'
        + (f.exists === false
          ? '<span class="libfile off" title="存档目录里没有正文">' + esc(f.name)
            + ' <span class="libfile-n">缺</span></span>'
          : '<button class="libfile" data-arc-open="' + esc(rec.id) + '|' + esc(f.rel) + '">'
            + esc(f.name) + '</button>')
        + '<span class="librow-m">' + (f.used_ok ? '成功 ' + f.used_ok + ' 次' : '') + '</span>'
        + '</div>';
    });
    h += '</div></details>'
      + '<div class="librec-dl">' + dlBtn + '</div>'
      + '</div>';
  });
  h += '</div></details>';

  host.innerHTML = h;

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   L1 某一层列不全时，让助手去把这一层调研清楚
   ---------------------------------------------------------------------
   为什么不直接发一条固定指令：
     要查什么、按什么口径查，只有人自己知道。
     固定指令发出去，助手查回来的往往不是她要的那一份。
   所以这里先让人写要求，再连同机器与目录一起注入会话 ——
   参数由应用给（她不用重复填），要求由她写。
   ===================================================================== */
/* 只留一句示例，不再预填整段指令。
   为什么：预填一整段（含回写接口、字段名）她根本不会逐字看，
   反而要花力气判断"这堆是不是都得改"。示例句给个方向就够，
   真正要发出去的固定部分（只读、回写接口、参数）由代码在后面补，
   不占她的注意力。 */
const L1_PROBE = {
  seurat: {
    name: 'Seurat 官方脚本',
    eg: '把这批数据用到的 Seurat 官方脚本全找出来，只读不改',
    tail: '清单回写 POST /api/script-scan，source 填 seurat。'
  },
  server: {
    name: '服务器上的脚本',
    eg: '把这台服务器上实际在用的分析脚本全找出来，只读不改',
    tail: '清单回写 POST /api/script-scan，source 填 server。'
  }
};

function openL1Probe(key) {
  const cfg = L1_PROBE[key];
  if (!cfg) return;
  // 面板只留一个输入框：一句示例放在 placeholder 里，不是预填正文。
  //   预填正文会让人以为那些字都要保留、都要改；placeholder 只是提示方向。
  const h = '<textarea id="l1Ask" class="l1ask" placeholder="' + esc(cfg.eg) + '"></textarea>'
    + '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn primary" data-l1go="' + esc(key) + '">发进会话</button>'
    + '<span class="small" id="l1GoMsg"></span></div>';
  sheet(cfg.name, h);
}

async function sendL1Probe(key) {
  const __recordScope = recordContext();
  try {

  const cfg = L1_PROBE[key];
  const msg = $('#l1GoMsg');
  const el = $('#l1Ask');
  // 没填就用示例句当要求 —— 她点开就想发的时候，不该被"先写点什么"拦住。
  const typed = el ? String(el.value || '').trim() : '';
  const ask = typed || cfg.eg;

  const d = S.placeDraft || placeDraftFromServer() || {};
  const L = [];
  L.push('【来自 个性化分析助手 · 脚本数据库调研】');
  if (S.topicId) L.push('回复路由会话号：' + S.topicId + '（结果请送回这里）');
  L.push('');
  L.push('要办的事：' + ask);
  L.push('');
  L.push('现场信息（应用已填好，不用问人）：');
  if ((d.mode || 'local') === 'server') {
    L.push('  目标服务器：' + (d.host || '（未填）') + ':' + (d.port || 22));
    L.push('  登录账号：' + (d.user || '（未填）'));
    if (d.exec_pc) L.push('  执行电脑：' + d.exec_pc);
    if (d.script_root) L.push('  脚本目录：' + d.script_root);
    if (d.data_root) L.push('  数据目录：' + d.data_root);
    if (d.run_dir) L.push('  结果目录：' + d.run_dir);
  } else {
    L.push('  本机运行，目录：' + (d.local_path || S.runDir || '（未填）'));
  }
  L.push('  口令不在文本里，从应用本机配置读。');
  L.push('');
  L.push('红线：只读，不改、不删服务器上任何文件。');
  L.push('找不到就如实说没找到，不要拿名字相近的脚本凑数。');
  if (cfg.tail) L.push(cfg.tail);

  const text = L.join('\n');
  if (msg) msg.textContent = '正在发进会话…';
  try {
    const r = await recordAwait(injectToSession(text, function () {}, 'query', undefined, '找脚本'), __recordScope);
    if (msg) msg.textContent = r.ok ? '已发进会话，等它回报。' : ('没发出去：' + (r.why || r.code || '原因不明') + (r.queued ? '（已转存队列）' : '（未发送、未入队）'));
    if (r.ok) {
      const old = document.getElementById('stSheet');
      if (old && old.parentNode) old.parentNode.removeChild(old);
      toast('已发进会话。调研结果写回后，这一页会补上。', 'good');
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msg) msg.textContent = '发送失败：' + String(e.message || e);
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 打开存档里的脚本：记录 id + 相对路径 */
/* 按一条脚本库记录下载 ipynb。
   ---------------------------------------------------------------------
   她要的是「这一次处理用的整脚本合集」，所以按记录汇编一份，
   不是每个脚本一个 ipynb。生成与格式都在后端做（用户要求跟 zjz 机器上
   那份模板一致：一级标题 + 章节编号｜中文名），前端只负责触发下载。
   没有可读正文时后端会明确报错，这里把那句话原样显示，不吞掉。 */
async function downloadRecordNotebook(recordId, btn) {
  const __recordScope = recordContext();
  try {
    const old = btn ? btn.textContent : '';
    if (btn) { btn.disabled = true; btn.textContent = '正在生成…'; }
    try {
      // 地址必须走 apiUrl()：应用挂在 /<应用身份名>/ 这个子路径下，
      //   写死 "/api/..." 会打到站点根，被网关挡掉，浏览器只报 "Failed to fetch"。
      //   gina 2026-11-20 报的「下载 ipynb 报错」就是这个原因。
      const resp = await fetch(apiUrl('/api/script-archive/notebook?record=' + encodeURIComponent(recordId)),
        { credentials: 'same-origin' });
      if (!resp.ok) {
        let msg = '生成失败';
        try {
          const j = await resp.json();
          msg = (j && (j.error || j.code)) || msg;
        } catch (e) { /* 非 JSON，保持通用文案 */ }
        toast('ipynb：' + msg, 'bad');
        return;
      }
      const blob = await resp.blob();
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = String(recordId).replace(/[^A-Za-z0-9._-]/g, '_') + '.ipynb';
      document.body.appendChild(a);
      try { a.click(); } finally { a.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
      toast('这份 ipynb 已交给浏览器下载。', 'good');
    } finally {
      if (btn) { btn.disabled = false; btn.textContent = old || '下载 ipynb'; }
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    toast('ipynb：' + String(e.message || e), 'bad');
  }
}

/* 补取正文：让执行端按原路径把缺的脚本读回来。
   ---------------------------------------------------------------------
   为什么会有"缺正文"：分析跑在服务器上，脚本常常只存在 /share/home/... 那个
   工作目录里。执行端回写时只给了路径和指纹，没给正文；本机又没有同版本副本
   （服务器那份被改过，md5 与随应用分发的不一致），所以只能先登记。
   这个按钮就是把那些路径登记给执行端，取回来 md5 对上才收进存档 ——
   不拿一份"差不多"的文件顶替，也不凭空生成。 */
async function requestRecordBodies(recordId, btn) {
  const __recordScope = recordContext();
  const old = btn ? btn.textContent : '';
  if (btn) { btn.disabled = true; btn.textContent = '正在登记…'; }
  try {
    const r = await api('/api/script-archive/fetch-body', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ record: recordId })
    });
    if (!r || !r.ok) { toast('补取正文：' + ((r && r.error) || '没能登记'), 'bad'); return; }
    toast(r.note || ('已登记 ' + (r.queued || 0) + ' 个脚本等待取回。'),
          (r.queued || 0) > 0 ? 'good' : 'bad');
    if ((r.queued || 0) > 0) {
      // 取回是异步的：过几秒刷新一次列表，正文到位后「缺」会自己消失。
      //   函数名是 renderLib（本页脚本库列表的入口），写成别的名字只会静默失败。
      setTimeout(() => { try { renderLib(); } catch (e) { /* 列表没开就算了 */ } }, 4000);
    }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    toast('补取正文：' + String(e.message || e), 'bad');
  } finally {
    if (btn) { btn.disabled = false; btn.textContent = old || '补取正文'; }
  }
}

/* 打开脚本库里登记过的一个文件（L1 的 skill 文件走这里）。
   ---------------------------------------------------------------------
   gina 2026-11-20：「AI 补写的 skill 里面的脚本不可点击查看」。
   原因是点击处调用了 openLibFile()，但这个函数从来没定义过 ——
   点了只会抛 ReferenceError，页面上表现为"按钮没反应"。
   现在补上实现：走 /api/lib-read（只放行库表登记过的路径），
   复用脚本弹窗显示正文。 */
async function openLibFile(path) {
  const __recordScope = recordContext();
  const t = $('#scriptTitle');
  if (t) t.textContent = String(path || '').split('/').pop();
  const b = $('#scriptBody');
  if (b) b.innerHTML = '<div class="small">正在读…</div>';
  openDlg('dlgScript');
  try {
    const d = await recordAwait(getJSON('/api/lib-read?path=' + encodeURIComponent(path)), __recordScope);
    if (!d || !d.ok) {
      if (b) b.innerHTML = '<div class="small">读不到：' + esc((d && d.error) || '未知原因') + '</div>';
      return;
    }
    let h = '<div class="scrmeta">'
      + '<span><span class="k">文件</span> <b>' + esc(d.name || '') + '</b></span>'
      + '<span><span class="k">大小</span> ' + fmtSize(d.size)
      + '　<span class="k">改于</span> ' + esc(d.mtime || '') + '</span></div>';
    h += '<pre class="scriptcode">' + esc(d.content || '') + '</pre>';
    if (b) b.innerHTML = h;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    if (b) b.innerHTML = '<div class="small">读不到：' + esc(String(e.message || e)) + '</div>';
  }
}

async function openArchiveFile(record, rel) {
  const __recordScope = recordContext();
  try {

  const t = $('#scriptTitle'); if (t) t.textContent = String(rel).split('/').pop();
  const b = $('#scriptBody');
  if (b) b.innerHTML = '<div class="small">正在读…</div>';
  openDlg('dlgScript');
  try {
    const d = await recordAwait(getJSON('/api/archive-read?record=' + encodeURIComponent(record)
      + '&rel=' + encodeURIComponent(rel)), __recordScope);
    if (!d || !d.ok) {
      if (b) b.innerHTML = '<div class="small">读不到：' + esc((d && d.error) || '') + '</div>';
      return;
    }
    let h = '<div class="scrmeta"><span><span class="k">记录</span> <b>' + esc(record) + '</b></span>'
      + '<span><span class="k">存档路径</span> ' + esc(rel) + '</span>'
      + '<span><span class="k">大小</span> ' + fmtSize(d.size)
      + '　<span class="k">改于</span> ' + esc(d.mtime || '') + '</span></div>';
    h += '<pre class="scriptcode">' + esc(d.content || '') + '</pre>';
    if (b) b.innerHTML = h;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (b) b.innerHTML = '<div class="small">读不到：' + esc(e.message) + '</div>';
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

function renderJargon() {
  const host = $('#jargonHost'); if (!host) return;
  // 名词解释不再是页面里的一节，改成右上角按钮弹出的面板：
  //   它是查阅用的，不该占正文位置，也不该让目录多出一项。
  let h = '<dl class="kvlist">';
  GLOSS_LIST.forEach(function (it) {
    h += '<dt>' + esc(it[0]) + '</dt><dd>' + esc(it[2]) + '</dd>';
  });
  h += '</dl>';
  host.innerHTML = h;
  const n = $('#jargonNote'); if (n) n.textContent = GLOSS_LIST.length + ' 个词';
}

/* boot() 的调用在文件末尾 */

/* =====================================================================
   第一步 · 运行位置
   ---------------------------------------------------------------------
   这一块取代了旧的「欢迎页 + 顶栏选目录 + 数据在哪台机器上」三处。
   为什么合并：那三处在做同一件事（决定数据在哪），却分散在三个地方，
   用户要在页面上来回找。现在只有一处，而且是第一件事。

   本机 / 服务器 两种模式，字段全部可编辑（预填但不写死）：
     预填是因为这些值通常就那一套，填一次省事；
     可改是因为不同人可能连不同的机器、用不同的账号。
   ===================================================================== */
S.place = null;      // /api/place 的返回
S.placeDraft = null; // 界面上正在编辑、还没保存的值

async function loadPlace() {
  const __recordScope = recordContext();
  try {

  try { S.place = await recordAwait(getJSON('/api/place'), __recordScope); }
  catch (e) {
    if (!recordCurrent(__recordScope)) return;
 S.place = { ok: false, error: String(e.message || e) }; }
  return S.place;

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 我在哪个会话里 —— 由本机后端代问网关（浏览器直连网关会被 CSP 拦下）。
   失败不抛：这不是页面能用的前提，只是"知道该把指令送到哪儿"的加分项。
   拿不到会话号时如实显示"没拿到"，不编一个出来。 */
async function loadIdentity() {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(getJSON('/api/whoami'), __recordScope);
    S.identity = r || null;
    S.topicId = (r && r.topic_id) || null;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    S.identity = { ok: false, error: String(e.message || e) };
    S.topicId = null;
  }
  renderIdentity();
  return S.identity;

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

function renderIdentity() {
  const el = document.getElementById('whoamiHint');
  if (!el) return;
  const r = S.identity;
  if (!r) { el.textContent = ''; return; }
  const cur = arr(S.sessions).find(s => s.id === S.sessionId) || null;
  const named = cur && cur.name ? String(cur.name) : String(S.sessionId || '未选择');
  const tid = S.topicId ? String(S.topicId) : '';
  const tail = tid ? (tid.length > 12 ? tid.slice(0, 12) + '…' : tid) : '';
  el.title = '状态属于分析记录「' + named + '」（' + (S.sessionId || '未选择')
    + '）；回复路由只决定消息去向，其他聊天也可在授权后接续同一分析，无须召回原聊天。'
    + (tid ? '当前回复路由：' + tid + '。' : '');
  if (tid) {
    el.textContent = '分析记录：' + named + '　回复路由：' + tail;
    el.style.color = '';
    return;
  }
  const route = r.ok && r.identity && r.identity.authenticated === false
    ? (r.saw_cookie || r.saw_auth ? '未登录' : '无凭据') : '未知';
  el.textContent = '分析记录：' + named + '　回复路由：' + route;
  el.style.color = '#8a8f8b';
}

/* 把服务端读到的值灌进草稿。每次「重新载入」都重置草稿，
   避免用户改了没保存、界面却像已经生效了。
   可选传一份 place（来自分析记录）：切记录时要把那一份记下的机器与路径
   恢复成当前设置 —— 否则页面显示的是这次分析的目录、实际动手用的是上一次的。 */
function placeDraftFromServer(place) {
  const p = place || S.place || {};
  const srv = p.server || {};
  const rt = srv.runtime || {};
  const loc = p.local || {};
  return {
    mode: p.mode || 'local',
    // 兼容两种返回：local.path（新接口）和顶层 local_path（备用）。
    //   接口字段名一旦对不上，界面会显示成空路径，而用户刚填过的内容
    //   看起来像是被清掉了 —— 这类「看起来没保存成功」的问题最难查。
    local_path: loc.path || p.local_path || '',
    host: srv.host || '',
    port: srv.port || 22,
    user: srv.user || '',
    // 执行电脑：哪台工作电脑能连上这台服务器。
    //   存它的理由见 buildInstructionText 里那段说明：指令里写清名字，
    //   执行端就不必逐台试连。第一次跑成功后应用自动填上，也能手改。
    exec_pc: srv.exec_pc || '',
    password: '',          // 口令永远不从服务端取回：服务端根本不会回传它
    has_password: !!srv.has_password,
    data_root: srv.data_root || '',
    script_root: srv.script_root || '',
    work_root: srv.work_root || '',
    rscript: rt.rscript || '',
    // R 包目录与动态库路径：以前这两个字段根本读不到页面里，
    //   于是「保存并识别」提交时又把它们丢了（提交载荷里只有 rscript/queue/node），
    //   配置里明明写着 /share/apps/R/R-4.1.2-4/lib64/R/library，
    //   探针却用默认 Rscript + 默认库路径去 library()，
    //   十个包全部报 dyn.load / "could not be loaded" —— 页面显示的就是这个。
    //   现在原样读出来、原样提交、原样写进指令，三处都不许再丢。
    r_lib_path: rt.lib_path || '',
    ld_library_path: rt.ld_library_path || '',
    queue: rt.queue || '',
    node_constraint: rt.node_constraint || '',
    label: srv.label || '',
    // 注释依据：在这一节就先定，不必等到注释那一步才问。
    //   为什么提前：以前只在点「注释」时才问，于是前面每一环都在
    //   "不知道有没有参考表"的状态下跑，执行端就去服务器上扫一遍全盘。
    //   提前定下来，绝大多数情况连扫都不用扫。
    ref_mode: srv.ref_mode || '',
    ref_path: srv.ref_path || '',
    ref_note: srv.ref_note || '',
    // 文献库那边的存档号：选"文献库"时只填号，注释环节由助手按号去取。
    ref_archive: srv.ref_archive || '',
  };
}

function renderPlace() {
  const host = $('#placeHost'); if (!host) return;
  if (!S.placeDraft) S.placeDraft = placeDraftFromServer();
  const d = S.placeDraft;
  // 「已识别」只看本次打开之后有没有点过保存，不看配置文件里存的值。
  //   否则刷新后按钮旁会写「已识别过」，可页面明明是刚打开、什么都还没跑。
  const ready = !!S.identified;
  const note = $('#placeNote');

  let h = '<div class="pl-card">';

  // ---- 两台机器二选一 ----
  h += '<div class="pl-modes">'
    + '<label class="pl-mode' + (d.mode === 'local' ? ' sel' : '') + '" data-plmode="local">'
    + '<span class="mk"></span><span><b>本机</b><span>数据在这台电脑上</span></span></label>'
    + '<label class="pl-mode' + (d.mode === 'server' ? ' sel' : '') + '" data-plmode="server">'
    + '<span class="mk"></span><span><b>服务器</b><span>数据在远端服务器上</span></span></label>'
    + '</div>';

  if (d.mode === 'local') {
    // 「本机」= 跑这个应用的这台机器，不是使用者自己的电脑。
    //   以前这里只放一个「选择文件夹」按钮，人会以为浏览框里是自己的 Windows 盘 ——
    //   实际看到的是应用所在机器的目录，两边根本不是一台机器。
    //   与其让人自己撞明白，不如直接说清，并给出正确做法（用整包在自己电脑上跑）。
    h += '<div class="rz warn" style="margin-bottom:12px"><span class="mk">!</span><span>'
      + '<b>「本机」= 跑这个应用的机器</b> '
      + '<span class="dt">要跑在自己电脑上，用整包在本机起服务。</span>'
      + '</span></div>';
    h += '<div class="pl-field"><label>工作路径</label>'
      + '<input type="text" data-plf="local_path" value="' + esc(d.local_path)
      + '" placeholder="例如 /data/run1"></div>';
    if (d.local_path) {
      h += '<div class="small" style="margin-left:132px">'
        + (S.place && S.place.local_ok === false
            ? '<span style="color:#a8631a">路径读不到。</span>'
            : '产物都写在这个目录。')
        + '</div>';
    }
  } else {
    h += '<div class="pl-field"><label>IP / 主机名</label>'
      + '<input type="text" class="mono" data-plf="host" value="' + esc(d.host) + '" placeholder="例如 10.1.1.2"></div>';
    h += '<div class="pl-field"><label>端口</label>'
      + '<input type="text" class="mono" data-plf="port" value="' + esc(d.port) + '" placeholder="22"></div>';
    h += '<div class="pl-field"><label>账号</label>'
      + '<input type="text" class="mono" data-plf="user" value="' + esc(d.user) + '" placeholder="你的登录账号"></div>';
    h += '<div class="pl-field"><label>密码</label>'
      + '<input type="password" data-plf="password" value="" placeholder="'
      + esc(d.has_password ? '已保存（留空则不修改）' : '填登录密码') + '"></div>';
    h += '<div class="pl-field"><label>数据目录</label>'
      + '<input type="text" data-plf="data_root" value="' + esc(d.data_root)
      + '" placeholder="服务器上放 matrix 的目录"></div>';
    // 脚本目录与数据目录是两个位置。
    //   交付过来的脚本通常和数据放在同一个父目录下（这里就是
    //   .../6sample_data_script/ 里面既有 matrix 也有各步骤脚本），
    //   把它们塞进一个字段，就一定会漏掉其中一个。
    h += '<div class="pl-field"><label>脚本目录</label>'
      + '<input type="text" data-plf="script_root" value="' + esc(d.script_root)
      + '" placeholder="服务器上放分析脚本的目录"></div>';
    h += '<div class="pl-field"><label>结果目录</label>'
      + '<input type="text" data-plf="work_root" value="' + esc(d.work_root)
      + '" placeholder="服务器上写结果的目录"></div>';
    // 执行电脑：内网服务器通常只有一台工作电脑连得上。
    //   以前这一项不存在，指令里只能写「在能连上这台服务器的电脑上执行」，
    //   执行端看不到会话选中的是哪一台，只能逐台试 —— 试到连不上的那台，
    //   就得把连接超时一直等完。第一次识别成功后这里会自动填上。
    h += '<div class="pl-field"><label>执行电脑</label>'
      + '<input type="text" data-plf="exec_pc" value="' + esc(d.exec_pc)
      + '" placeholder="能连上服务器的电脑，识别后自动填"></div>';
    h += '<div class="small" style="margin-left:132px">留空就要挨台试，会慢。</div>';

    // ---- 注释依据：先选来源，再按来源给出对应的那一个动作 ----
    //   三选一在页面上就定下来，后面的按钮只做这一件事：
    //     服务器上已有 -> 填路径
    //     我上传一份   -> 直接开文件框
    //     文献库       -> 填存档号
    //   不再在点按钮时弹"服务器 / 上传 / 查文献"那张卡：同一个决定问两遍，
    //   人第二次看到只会以为上一次没生效。
    const rmNow = d.ref_mode || '';
    // 本次选定的那一份：从 /api/reference/list 的结果里取，
    //   不另存一份状态 —— 两处各记一份，早晚对不上。
    const refItemsP = arr(S.references && S.references.items);
    const refChosenP = String((S.references && S.references.chosen) || '');
    const refCur = refItemsP.filter(function (x) {
      return String(x.id || '') === refChosenP;
    })[0] || null;
    const refOpt = (val, title) => '<button class="pl-refopt'
      + (rmNow === val ? ' sel' : '')
      + '" data-plfref="' + val + '"><b>' + title + '</b></button>';
    h += '<div style="margin-top:16px;padding-top:14px;border-top:1px solid #e6ebf2">'
      + '<div class="pl-field pl-field-hd" style="margin-bottom:8px"><label>注释依据</label></div>'
      + '<div class="pl-refopts">'
      + refOpt('path', '服务器上已有')
      + refOpt('upload', '我上传一份')
      + refOpt('lit', '文献库')
      + '</div>';

    if (rmNow === 'path') {
      h += '<div class="pl-field" style="margin-top:10px"><label>表在服务器上的路径</label>'
        + '<input type="text" class="mono" data-plf="ref_path" value="' + esc(d.ref_path)
        + '" placeholder="/share/home/.../Supplementary_Table_5.xlsx"></div>';
    } else if (rmNow === 'upload') {
      h += '<div class="refrow" style="margin-top:10px">'
        + '<button class="btn btn-sm primary" data-refup-quick="1">选择文件并上传</button>'
        + '<span class="small" id="refQuickMsg">'
        + (S.pendingRef ? '已上传，待保存并识别创建记录后选用' : '') + '</span></div>';
    } else if (rmNow === 'lit') {
      h += '<div class="refrow" style="margin-top:10px">'
        + '<input type="text" class="mono" data-plf="ref_archive" value="' + esc(d.ref_archive)
        + '" placeholder="文献库存档号，例如 A-2024-0187">'
        + '<button class="btn btn-sm" data-reflit-go="1">保存存档号</button>'
        + '<span class="small" id="refLitMsg"></span></div>';
    }
    // 上传入库但尚未创建记录：保留文件信息，不冒充已经选用。
    if (S.pendingRef) {
      h += '<div class="rz" data-ref-pending="1" style="margin-top:10px"><span class="mk">↑</span><span>'
        + '已上传：<b>' + esc(S.pendingRef.name || S.pendingRef.id) + '</b>'
        + '　' + fmtBytes(S.pendingRef.bytes)
        + '<br><b>待绑定</b>：保存并识别创建本次记录后选用'
        + '</span></div>';
    }
    // 本次已经选定的那一份：常驻显示，不用去别处找它在哪。
    if (refCur) {
      h += '<div class="rz ok" style="margin-top:10px"><span class="mk">✓</span><span>'
        + '本次用：<b>' + esc(refCur.name || refCur.id) + '</b>'
        + (refCur.bytes ? '　' + fmtBytes(refCur.bytes) : '')
        + '</span></div>';
    }
    h += '<div class="pl-field" style="margin-top:10px"><label>一句话说明</label>'
      + '<input type="text" data-plf="ref_note" value="' + esc(d.ref_note)
      + '" placeholder="可选"></div>';
    h += '</div>';
    h += '<details class="stgfold" style="margin-top:14px"><summary>进阶（一般不用改）'
      + '<span class="cnt">R 程序 / 库路径 / 队列 / 节点</span></summary>'
      + '<div style="padding:10px 16px 16px">'
      + '<div class="pl-field"><label>R 程序</label>'
      + '<input type="text" class="mono" data-plf="rscript" value="' + esc(d.rscript) + '" placeholder="/share/apps/R/.../Rscript"></div>'
      // R 包目录 / 动态库路径：原来页面上根本没有这两栏，值是配置里写着的，
      //   用户改不了、保存时还会被清空，于是每次识别都用默认库路径跑，
      //   十个包全报加载失败。现在摆出来、可编辑、随提交一起存。
      + '<div class="pl-field"><label>R 包目录</label>'
      + '<input type="text" class="mono" data-plf="r_lib_path" value="' + esc(d.r_lib_path)
      + '" placeholder="/share/apps/R/R-4.1.2-4/lib64/R/library"></div>'
      + '<div class="pl-field"><label>动态库路径</label>'
      + '<input type="text" class="mono" data-plf="ld_library_path" value="' + esc(d.ld_library_path)
      + '" placeholder="有 Rcpp / hdf5 加载失败才需要填"></div>'
      + '<div class="pl-field"><label>队列</label>'
      + '<input type="text" class="mono" data-plf="queue" value="' + esc(d.queue) + '" placeholder="all.q"></div>'
      + '<div class="pl-field"><label>节点</label>'
      + '<input type="text" class="mono" data-plf="node_constraint" value="' + esc(d.node_constraint)
      + '" placeholder="hostname=master01"></div>'
      + '</div></details>';
  }

  // ---- 保存并识别 ----
  //   按钮永远叫「保存并识别」，不随状态改名。
  //   之前信息齐全后它会变成「重新识别」，于是这一页上找不到"保存"这个动作 ——
  //   用户的原话是「没有保存识别按钮啊」。动作名不该因为状态而消失。
  h += '<div class="pl-actions">'
    + '<button class="btn primary big" data-plcommit="1"'
    + (S.commitBusy ? ' disabled aria-busy="true"' : '') + '>'
    + (S.commitBusy ? '识别中…' : '保存并识别') + '</button>'
    + '<span class="small">' + (d.mode === 'server'
        ? '保存并交给助手去识别'
        : '保存并自动识别')
    + '</span>'
    + (ready ? '<span class="small pl-done">改完需重新识别</span>' : '')
    + '<span id="plMsg" class="small"></span>'
    + '</div>';

  // 「这次识别交给助手没有、结果回来没有」不再画在这里。
  //   它和识别结论是同一件事的两面，拆两处显示会各说一半：
  //   按钮下面说"已交给助手"，识别结论里却说"识别不了"。
  //   现在统一放在「识别结论」那一栏里，一处说话。

  h += '</div>';

  // ---- 没识别成功的项：直接列在这里，而不是让人去找 ----
  //   这些都是「原本该自动填上、这次没填上」的东西。
  //   它们不影响已经保存的设置，但会让人误以为环境已经全认出来了。
  // 服务器模式下，后端会带两条「本机连不上服务器 / 体检还没做」的提示。
  //   那不是识别失败，是这条链路的正常形态（识别本来就由助手代跑）。
  //   把它们列进「没能识别」，人会以为出错 —— 所以这里单独说清，不计入失败项。
  // 服务器模式下后端只会带一条「体检还没做、已交给助手」，
  //   它不是识别失败，也不该被列进「没能识别」。这里把它滤掉，只留真问题。
  //   （以前这里还多画一张卡解释"应用在云端所以读不到服务器"——
  //    那段话把链路讲反了：体检本来就不是应用自己去做的事，已删。）
  const isServerMode = (d.mode === 'server');
  const realWarns = isServerMode
    ? arr(S.warns).filter(function (x) {
        const t = String(x);
        return t.indexOf('环境体检还没做') < 0;
      })
    : arr(S.warns);
  const ws = realWarns;
  if (ws.length) {
    h += '<div class="pl-card" style="margin-top:12px">'
      + '<div class="rz warn" style="margin:0 0 8px"><span class="mk">!</span><span>'
      + '<b>这 ' + ws.length + ' 项没识别出来</b> '
      + '<span class="dt">设置已保存，这几项没自动填上。</span></span></div>'
      + '<div class="rzlist">'
      + ws.map(function (w, i) {
          return '<div class="rz"><span class="mk">' + (i + 1) + '</span><span>'
            + esc(w) + '</span></div>';
        }).join('')
      + '</div></div>';
  }

  host.innerHTML = h;

  note.textContent = ready ? '已确认' : '';
}

/* 弹窗选文件夹：复用同一个目录浏览弹窗，选完写回工作路径 */
function openPathPicker() {
  S.pickTarget = 'place';
  openDlg('dlgDir');
  const start = (S.placeDraft && S.placeDraft.local_path)
    || (S.env && S.env.home) || null;
  browseTo(start);
}

async function commitPlace() {
  if (sessionChanging) return;
  // 防误触两道闸。为什么两道都要：
  //   进程内锁挡"正在跑又点了一次"，时间窗挡"刚跑完连点两下"。
  //   没有这两道时，同一帧点两下会真的发出两次提交，两个请求号前后覆盖。
  if (S.commitBusy) return;
  if (S.commitAt && Date.now() - S.commitAt < 1500) return;
  S.commitBusy = true;
  S.commitAt = Date.now();
  // 点下去立刻把焦点挪走。
  //   为什么必须做：按钮执行完不会被移走，焦点留在上面，
  //   此后按一次空格或回车就等于再签一次字 —— 这正是"我没点鼠标却触发了"的形态。
  try {
    const b0 = document.querySelector('[data-plcommit]');
    if (b0 && typeof b0.blur === 'function') b0.blur();
  } catch (e) {}
  renderPlace();
  const __recordScope = recordContext();
  try {

  const d = S.placeDraft || {};
  const msg = $('#plMsg');
  if (msg) msg.textContent = '正在识别环境与脚本库…';
  // 上一轮自动注入的结论不留给这一轮 —— 否则「已交给助手」会一直挂着，
  //   让人以为这次也发出去了。
  S.autoInject = null;
  S.agentHandoffPath = null;
  // 上一轮的等待也要停掉：这一轮有新的请求号，旧的那次再写回来也不算数。
  stopWaitWriteback();
  S.identifyPhase = null;

  // 先把字段存下来，再提交。
  //   分两步是为了：即使识别那一步失败，用户填的东西也已经落地，
  //   不用重填一遍。
  const payload = {
    mode: d.mode,
    // 这次识别属于哪一份分析记录。
    //   带上它，回写的结果才能挂到"这一份"名下；
    //   不带的话，多份记录并存时结果会挂到当前那一份，可能不是用户在看的这份。
    session_id: S.sessionId || undefined,
    local_path: d.mode === 'local' ? d.local_path : undefined,
    server: d.mode === 'server' ? {
      host: d.host, port: d.port, user: d.user,
      exec_pc: d.exec_pc,
      password: d.password,          // 空串 = 不改口令，服务端按这个规则处理
      data_root: d.data_root, script_root: d.script_root, work_root: d.work_root,
      label: d.label,
      // 注释依据：不跟着这一次提交走，页面上的选择就只是"显示成选了"。
      //   以前这里确实漏掉了这四个字段，于是选了"上传"、点了保存，
      //   服务端仍然是空 —— 到注释环节又被问一次，或者干脆按"没定"处理。
      ref_mode: d.ref_mode || '', ref_path: d.ref_path || '',
      ref_note: d.ref_note || '', ref_archive: d.ref_archive || '',
      // R 包目录与动态库路径必须跟着提交走。
      //   原来这里只发 rscript/queue/node_constraint，服务端 place_clean_server
      //   又把 runtime 收成五个字段，于是配置里已有的 lib_path、
      //   ld_library_path 在这一步被清空 —— 之后无论谁去跑探针，
      //   都只能拿到一个"用默认库路径"的 R，十个包全报加载失败。
      runtime: {
        rscript: d.rscript,
        lib_path: d.r_lib_path || '',
        ld_library_path: d.ld_library_path || '',
        queue: d.queue, node_constraint: d.node_constraint,
      },
    } : undefined,
  };
  try {
    // 动手之前先按这一份现在的样子存一个存档。
    //   为什么放在这里：点「保存并识别」往往就是"我要改点什么"的时刻。
    //   先留一份当时的状态，改坏了能整份退回来 —— 而且这一步是自动的，
    //   不指望人记得手动点。
    if (S.sessionId) await recordAwait(archiveNow('before-identify', '自动存档'), __recordScope);
    // 一次请求就够：/api/place/commit 自己会先把设置落盘，再往下走。
    //   以前这里是「先 POST /api/place，再 POST /api/place/commit」两次往返 ——
    //   第一次的返回值没人用，纯粹白等一个来回（用户的原话是"为啥这么慢"）。
    //   分成两步的初衷是「保存」和「开始干活」两个动作分开，但那是**界面**上的
    //   两件事，不该落成两次网络往返：commit 内部本来就不做写入以外的事。
    const r = await recordAwait(postJSON('/api/place/commit', payload), __recordScope);
    if (!r.ok) throw new Error(r.error || '识别失败');
    S.place = r.place || S.place;
    S.placeDraft = placeDraftFromServer();
    // 本次会话已经识别过：按钮文案与「已识别」提示以它为准，
    // 不写进任何配置文件 —— 刷新之后要重新回到未识别状态。
    S.identified = true;
    // 本次请求号：后面注入的文本要带上它，助手回写时带回同一个号。
    S.commitReq = r.request_id || null;
    S.agentHandoffPath = r.agent_handoff_path || null;
    // 服务端可能顺手开了一份新记录（第一次点保存并识别时）——
    //   立刻接管它，否则这一次识别的结果就没有归属，刷新必丢。
    if (r.session_id && S.sessionId !== r.session_id) {
      S.sessionId = r.session_id;
    }
    await recordAwait(loadSessionsState(), __recordScope);
    // Only bind a pre-record upload to the record returned by this commit.
    // Switching records clears pendingRef and invalidates all awaited responses.
    if (S.pendingRef && r.session_id && S.sessionId === r.session_id) {
      const pending = S.pendingRef;
      try {
        const chosen = await recordAwait(postJSON('/api/reference/choose', {
          id: pending.id, session_id: r.session_id,
        }), __recordScope);
        if (!chosen || !chosen.ok) throw new Error('未能选用参考表');
        S.placeDraft.ref_mode = 'upload';
        await recordAwait(saveRefChoice(), __recordScope);
        S.pendingRef = null;
        toast('已将上传的参考表设为本次使用：' + pending.name, 'good');
      } catch (e) {
        if (!recordCurrent(__recordScope)) return;
        toast('分析已保存，参考表已上传但尚未选用，请在本记录中重新选用。', 'bad');
      }
    }
    renderSessionsBar();
    // 服务器模式：识别不在本机跑，所以这一轮拿不到数字。
    //   前端据此显示"等助手写回"，而不是把空结果画成"检查完了"。
    S.needsRemote = !!r.needs_remote;
    // 备份任务号：这次识别排给了谁，事后能查到。
    S.queuedCheck = r.queued_check || '';
    S.warns = arr(r.warnings).map(function (x) { return String(x); });
    // 结论什么时候出现，取决于「谁去跑识别」——
    //   本机模式：本机当场跑完，结论立刻可出。
    //   服务器模式：识别由助手在能连上服务器的电脑上做，结论要等它写回才有。
    //   这时不能拿"本机读不到服务器"当识别结果 —— 用户看到的就是"识别不了"，
    //   而其实什么都还没开始。
    const needDelegate = (d.mode === 'server') && !!r.needs_remote;
    S.identifyPhase = needDelegate ? 'waiting' : 'ready';
    // 这里只报「已保存」。真正交没交出去，要等下面 autoInjectIdentify 的结果。
    //   以前这里直接写「已保存并交给助手」，而注入是后面才发生的：
    //   通道没回执、页面中途刷新、loadRun 抛错，人看到的都是一句成功的绿字，
    //   于是「说交给助手了，可会话里什么都没有」。真实结果由 autoInjectIdentify
    //   在 finally 里按 S.autoInject 报，成功失败分开说。
    if (needDelegate) {
      toast('已保存，正在把识别请求交给助手…', 'good');
    } else if (S.warns.length) {
      toast('已保存，' + S.warns.length + ' 项没识别出来', 'bad');
    } else {
      toast('已保存，识别完成。', 'good');
    }
    await recordAwait(loadRun(), __recordScope);
    setStageVisible(true);
    renderAll();

    // 服务器模式下把「这次识别」交给助手，并开始等它写回 —— 和按钮同一步发生。
    //   本机模式不需要这一步：那边当场就跑完了。
    if (needDelegate) { autoInjectIdentify(); startWaitWriteback(); }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (msg) msg.textContent = '';
    toast('保存失败：' + e.message, 'bad');
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  } finally {
    // 无论成功、失败还是提前返回，锁都要解开，否则按钮永久卡在"识别中"。
    S.commitBusy = false;
    renderPlace();
  }
}

/* =====================================================================
   把「这次识别」交给助手（由「保存并识别」触发）
   ---------------------------------------------------------------------
   这一步不再有单独的手动入口：用户点一次「保存并识别」，
    设置落盘、本机识别跑完、剩下的交给助手 —— 一个动作做完。
   失败的形态要说清：
     · 通道不通（独立窗口 / 没 SDK）不算保存失败，设置已经存下了。
       这时把原文摆出来让人自己复制发送，功能不押在一条握手上。
     · 落盘失败也不影响识别结果，识别结论照样显示。
   ===================================================================== */
async function autoInjectIdentify() {
  const __recordScope = recordContext();
  try {

  // 上一轮如果卡住了，这里会永远 return，界面也就永远没反应。
  //   为什么会卡：以前 injectToSession 等平台回执没有超时，
  //   await 永不返回，finally 不执行，S.autoInjectBusy 就一直是 true。
  //   现在注入本身有 12 秒上限，这里再加一道保险：超过 30 秒还挂着，
  //   一律视为上一轮已失效，放行这一轮，不让界面被一次卡死拖住。
  if (S.autoInjectBusy) {
    const stuck = Date.now() - (S.autoInjectAt || 0);
    if (stuck < 30000) return;
    S.autoInjectBusy = false;
  }
  const d = S.placeDraft || placeDraftFromServer();
  const text = buildInstructionText(
    '在这台服务器上跑一次环境体检和脚本库识别：'
    + '看内存和磁盘够不够、R 包齐不齐、数据目录与脚本目录在不在，'
    + '再把服务器上已有的分析脚本列出来。'
    + '注释依据严格按下文用户所选来源处理，已上传时不扫描服务器寻找其他表。'
    + '结果写回应用，让「识别结论」这一栏显示真实执行结果；连接失败与回写鉴权失败分别说明，不得互相替代。');

  S.autoInjectBusy = true;
  S.autoInjectAt = Date.now();
  S.autoInject = null;
  renderPlace();
  try {
    const r = await recordAwait(injectToSession(text, function () {}, undefined, undefined, '保存并识别'), __recordScope);
    // relayed：通道没通，但指令已经落进本机队列 —— 助手能取走执行。
    //   这两种情况在页面上必须分开写：一个是"没发出去"，一个是"换条路交出去了"。
    S.autoInject = r.ok
      ? { ok: true, text: text }
      : { ok: false, code: r.code || 'failed', why: r.why || '', text: text,
          relayed: !!r.relayed, queueId: r.id || '' };
    // 落一份记录：应用本机留痕，便于事后核对"这次到底发出去没有"。
    //   失败也记，记的是失败原因。发送本身不依赖它，写不进去不影响结果。
    //   注入结果一并记上。为什么要记：注入这一步发生在浏览器里，
    //   服务端看不见平台回了什么。不记的话，事后只能看到"指令生成了"，
    //   分不清是平台没收、还是通道没连上、还是回执根本没来 ——
    //   三种情况的处理方式完全不同。记下来，下一次就有据可查。
    try {
      await recordAwait(postJSON('/api/instruct', {
        text: '（保存并识别时自动提交）' + text,
        inject: S.autoInject
          ? { ok: !!S.autoInject.ok,
              code: S.autoInject.code || (S.autoInject.ok ? 'accepted' : ''),
              why: S.autoInject.why || '' }
          : { ok: false, code: 'not_attempted', why: '没有走到注入这一步' },
      }), __recordScope);
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    S.autoInject = { ok: false, code: 'failed', why: String(e.message || e), text: text };
  } finally {
    if (!recordCurrent(__recordScope)) return;

    S.autoInjectBusy = false;
    renderPlace();
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   识别结果：环境体检 + 自动判定
   ---------------------------------------------------------------------
   这两块不是「需要用户做的决定」，是系统自己跑出来的结论。
   所以它们以结果形式贴在第一步下面，不列进待办清单、不要求确认。
   ===================================================================== */
function verdictBadge(v) {
  if (v === 'ready' || v === 'ok') return ['可以跑', 'ok'];
  if (v === 'blocked' || v === 'fail') return ['跑不了', 'fail'];
  return ['有提醒', 'warn'];
}

/* =====================================================================
   等助手把识别结果写回来
   ---------------------------------------------------------------------
   服务器模式下识别不在本机跑，页面能做的只有两件事：把请求交出去、等结果。
   所以这里不猜、不拿旧数据显示，只是隔几秒问一次「这一次请求的结果到了吗」。
   判据是请求号：只有和本次请求号对得上的结果，才算"这一次跑出来的"。
   到了才画结论；没到就一直显示"识别中"，并说明在等什么。
   ===================================================================== */
function stopWaitWriteback() {
  if (S.waitTimer) { clearInterval(S.waitTimer); S.waitTimer = null; }
}

async function fetchEnvcheckSafe() {
  const __recordScope = recordContext();
  try {

  try { return (await recordAwait(getJSON('/api/envcheck'), __recordScope)) || {}; } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 return {}; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 有请求号时以请求匹配及服务端 stale 判定为准，刷新页面不会开启新请求。
   没有请求号的旧本机结果仍使用等待起点，避免把旧写入当成新回写。 */
function ecResultTime(ec) {
  const r = (ec && ec.result) || {};
  const t = r.imported_at || r.generated_at || '';
  const ms = t ? Date.parse(String(t).replace(' ', 'T')) : 0;
  return { ms: Number.isFinite(ms) ? ms : 0, text: t };
}

function ecIsFresh(ec) {
  if (!ec || !ec.result || ec.stale) return false;
  if (S.commitReq) return (ec.result.req_id || ec.result_request) === S.commitReq;
  if (!S.waitSince) return true;
  const t = ecResultTime(ec);
  return !!t.ms && t.ms >= S.waitSince;
}

function ecAtText(ec) {
  const t = ecResultTime(ec);
  if (!t.text) return '';
  const d = String(t.text).replace('T', ' ');
  return d.length >= 16 ? (d.slice(5, 10) + ' ' + d.slice(11, 16)) : d;
}

function startWaitWriteback() {
  if (S.waitTimer && S.waitReq === S.commitReq) return;
  stopWaitWriteback();
  S.waitReq = S.commitReq;
  S.waitTries = 0;
  S.waitSince = Date.now();
  // 等到什么时候放弃。
  //   40 分钟（480 × 5 秒）：这不是"体检要跑 40 分钟"——
  //   一条 SSH 探针十几秒就完了。留的是**等待**：助手可能正在忙别的、
  //   或者这条指令正排在队列里。以前是 5 分钟，结果体检其实跑成了、
  //   结果也写回来了，页面却早已放弃、显示成超时，用户看到的就是
  //   「页面没有任何变化」。放弃得太早，等于把成功读成失败。
  const MAX_TRIES = 480;
  const tick = async function () {
  const __recordScope = recordContext();
  try {

    S.waitTries += 1;
    const ec = await recordAwait(fetchEnvcheckSafe(), __recordScope);
    if (ecIsFresh(ec)) {
      stopWaitWriteback();
      S.identifyPhase = 'ready';
      // 结果到了也顺便把记录状态读回来：这一次的结论要挂在这一份记录上，
      //   刷新之后才还在。
      try { await recordAwait(loadSessionsState(), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
      renderAll();
      toast('识别结果已写回。', 'good');
      return;
    }
    // 已经有结果但早于本次等待起点：不当成"到了"，只把旧结果的时间记住，
    //   页面显示"最新一份是几点"，人一眼能看出没有新写入。
    if (ec && ec.result && !ec.stale) S.staleEnvAt = ecAtText(ec);
    if (S.waitTries >= MAX_TRIES) {
      stopWaitWriteback();
      S.identifyPhase = 'timeout';
      renderAll();
    }
  
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
};
  S.waitTimer = recordInterval(tick, 5000);
  tick();
}

/* 手动查一次：不想干等的时候点一下 */
async function recheckIdentify() {
  const __recordScope = recordContext();
  try {

  const note = $('#idReMsg');
  if (note) note.textContent = '正在查…';
  const ec = await recordAwait(fetchEnvcheckSafe(), __recordScope);
  if (ecIsFresh(ec)) {
    stopWaitWriteback();
    S.identifyPhase = 'ready';
    renderAll();
    toast('本次结果已写回。', 'good');
    return;
  }
  // 没到就是没到。若手上有一份更早的，把它的时间说出来 ——
  //   "没有新结果"和"上次那份是 15:55 的"是两件事，后者才解释得清为什么没动。
  const at = ecAtText(ec);
  const msg = at
    ? ('还没回写。手上这份是 ' + at + ' 的旧结果。')
    : '还没回写，也没有任何结果。';
  if (note) note.textContent = msg;
  if (S.identifyPhase === 'timeout' || !S.identifyPhase) {
    S.staleEnvAt = at || '';
    renderAll();
  }
  // 重新开始等：人点了查询，说明还在意这一趟。
  if (S.identifyPhase === 'timeout') S.identifyPhase = 'waiting';

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 等结果这段时间里，识别结论这一栏长什么样。
   要点：只说"在等什么"，不下任何结论 —— 成功还是失败由助手跑出来的算。 */
function identifyWaitingHTML() {
  const pd = S.placeDraft || {};
  const where = (pd.mode === 'server')
    ? ('服务器 ' + (pd.host || '（未填地址）'))
    : '这台电脑（本机）';
  const to = S.identifyPhase === 'timeout';
  const ai = S.autoInject;

  let h = '<div class="pl-card">';
  h += '<div class="idhead">'
    + '<h3 class="idtitle">识别结论</h3>'
    + '<span class="idbadge warn">' + (to ? '还没回来' : '识别中') + '</span>'
    + '<span class="idwhere">' + esc(where) + '</span>'
    + '</div>';
  h += '<div class="small">其他聊天也可在授权范围内接续，无须召回原聊天。先查任务状态，避免重复执行；回写以分析记录和请求号为准。'
    + ' 分析记录：' + esc(S.sessionId || '未选择')
    + '；请求号：' + esc(S.commitReq || '未取得') + '。</div>';
  const oldAt = S.staleEnvAt || '';
  h += '<div class="idsum">' + esc(to
      ? ('还没收到回写 —— 这一栏不下结论。')
      : ('正在等助手到 ' + where + ' 上跑一次环境体检和脚本库识别。'))
    + '</div>';
  // 超期没回来：说清等的是哪一次、等了多久，并给一个重跑的口子。
  //   以前这里只有"识别中"和"还没回来"两种说法，人分不出
  //   "正在跑"和"早就没人管了"；而后者只会让这条记录一直停在识别中。
  if (to && S.identifyStatus === 'stale') {
    h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
      + '<b>这次识别请求已经等了 ' + esc(String(S.identifyStaleHours || '')) + ' 小时，没有回写。</b> '
      + '<span class="dt">请求号：' + esc(S.commitReq || '未取得')
      + '。它不是还在跑，是没人回来交结果。'
      + (S.identifyResultAt ? ('记录里最近一次成功识别是 ' + esc(String(S.identifyResultAt).replace('T', ' ')) + '。') : '')
      + '可以重新点「保存并识别」再来一次。</span></span></div>';
  }
  // 手上已有旧结果时把时间point出来：不说这句，人只会看到"识别中"不动，
  //   以为是卡住了，其实是这一次还没回写。
  if (oldAt) {
    h += '<div class="idsum" style="color:#8a5a00">手上这份结果是 '
      + esc(oldAt) + ' 的，不是这一次的。</div>';
  }

  // 交给助手这一步的实况：成了就报成，没交出去就把原文摆出来。
  let lv = 'warn', mk = '!', tx = '正在把这次识别交给助手…';
  if (S.autoInjectBusy) { lv = 'warn'; mk = '!'; tx = '正在把这次识别交给助手…'; }
  else if (ai && ai.ok) { lv = 'ok'; mk = '✓'; tx = '已交给助手'; }
  else if (ai && ai.relayed) {
    // 通道断着，但指令已经在本机队列里，助手能取走 —— 这不是失败。
    lv = 'warn'; mk = '!';
    tx = '平台会话通道没连上，已把这段放进助手队列'
      + (ai.queueId ? '（' + ai.queueId + '）' : '')
      + '，我会直接取走执行。';
  }
  else if (ai) {
    const soft = (ai.code === 'standalone' || ai.code === 'no_sdk');
    lv = soft ? 'warn' : 'fail'; mk = soft ? '!' : '×';
    tx = '这段没自动交给助手：' + (ai.why || ai.code || '');
  }
  h += '<div class="rz ' + lv + '" style="margin-top:10px"><span class="mk">' + mk + '</span><span>'
    + '<b>' + esc(tx) + '</b>'
    + (lv === 'ok' ? ' <span class="dt">在执行机上跑。</span>' : '')
    + '</span></div>';

  // 通道到底通不通，直接说出来，不让人对着"正在交…"干等。
  //   为什么要这一段：从左侧全局「应用」列表打开时，页面确实在 iframe 里，
  //   SDK 脚本也在，但平台没有建 instance —— 于是握手永远不会来，
  //   "正在把这次识别交给助手…"会一直挂着，看起来像卡死，其实是入口不对。
  //   实测两种入口表现完全不同：
  //     聊天右侧面板打开 -> has_instance=true，父页发 connect 事件，通道可用
  //     全局应用列表打开 -> has_instance=false，父页一条消息都没有
  //   这属于平台侧行为，本机不放宽 origin、不伪造握手。
  //   能做的只有一件：把"你应该从哪儿打开"直接写在脸上。
  let ch = null;
  try { ch = window.AppShell && window.AppShell.channels ? window.AppShell.channels() : null; } catch (e) { ch = null; }
  const noChannel = ch && !ch.compose;
  const stuckBusy = S.autoInjectBusy && (Date.now() - (S.autoInjectAt || 0)) > 45000;
  if (noChannel && !(ai && ai.ok)) {
    h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
      + '<b>当前页面没有接上会话通道。</b> '
      + '<span class="dt">平台没有在这个页面里建立会话连接，'
      + (ch.inFrame ? '页面在框里但宿主没发握手' : '页面是独立打开的')
      + '。请关掉这一页，到 Rachel 聊天里点右上角「打开产物」→ 应用 → 打开应用，'
      + '在那个面板里点「保存并识别」。从左侧全局应用列表打开不会连上。</span>'
      + '</span></div>';
  } else if (stuckBusy) {
    h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
      + '<b>这次还没交出去。</b> <span class="dt">已等 45 秒未收到宿主回执。'
      + '可点下面「再交给助手一次」，或先确认是不是从聊天右侧面板打开的。</span>'
      + '</span></div>';
  }

  if (ai && !ai.ok && !ai.relayed) {
    // 通道不通不是"识别失败"，设置已经存下了。把原文摆出来让人自己发。
    h += '<div style="margin-top:8px">'
      + '<div class="small" style="margin-bottom:4px">或复制这段发给任一已授权聊天的助手：</div>'
      + '<textarea readonly data-idraw="1" rows="7" '
      + 'class="idraw">'
      + esc(ai.text || '') + '</textarea>'
      + '<div class="btnrow" style="margin-top:6px">'
      + '<button class="btn" data-idcopy="1">复制这段</button>'
      + '<span class="small" id="idCopyMsg"></span></div></div>';
  }

  h += '<div class="btnrow" style="margin-top:12px">'
    + '<button class="btn" data-idrecheck="1">现在查一次结果</button>'
    + (to ? '<button class="btn" data-idreinject="1">再交给助手一次</button>' : '')
    + '<span class="small" id="idReMsg"></span></div>';

  if (to) {
    h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
      + '<b>等了 5 分钟还没到</b> <span class="dt">可能还在排队。'
      + '可点「现在查一次」或「再交给一次」。</span></span></div>';
  }

  h += '</div>';
  return h;
}

/* 「保存并识别」这一趟里，头三步各自走到哪了。
   为什么要这一条：
    环境体检 / 自动判定 / 容量预检 本来和识别是同一趟事（用户的原话是
    「为什么都在跑分析这个环节里」）。它们既然属于识别，进展就该在识别结论里
    说得清，而不是靠「跑分析」里三个按钮让人去点 —— 那三个按钮在服务器模式下
    点了只会得到一句"要去助手那边跑"。
   判据只用已经拿到的东西，不猜：
    体检：有 items 就是跑过了；带回 error 就是没跑成
    判定：本机模式下有 decision.json 才有；服务器模式的判定随识别一起回来
    预检：要等主流程上段之后才有依据，这里如实说"还没到" */
function identifyStepStrip(env, dec, envFailed) {
  const it = (id) => arr(env && env.items).find(x => x.id === id);
  const hasEnv = !!env && arr(env.items).length > 0;

  const steps = [];
  // ① 环境体检
  if (!hasEnv && !envFailed) {
    steps.push(['env', '环境体检', 'wait', '等识别结果回来']);
  } else if (envFailed) {
    steps.push(['env', '环境体检', 'fail', String(env.error || '这次没跑成')]);
  } else {
    const c = (env && env.counts) || {};
    steps.push(['env', '环境体检', (c.fail ? 'fail' : (c.warn ? 'warn' : 'done')),
      '通过 ' + (c.ok || 0) + ' 提醒 ' + (c.warn || 0) + ' 缺失 ' + (c.fail || 0)]);
  }
  // ② 自动判定
  if (dec && dec.verdict_cn) {
    const cls = (dec.path === 'blocked') ? 'fail' : ((dec.path === 'warn' || dec.verdict === 'warn') ? 'warn' : 'done');
    steps.push(['decide', '自动判定', cls, String(dec.verdict_cn)]);
  } else if (hasEnv) {
    // 服务器模式下判定由识别那一趟一并给出；这里如实说明它还没回来，
    //   而不是把它画成"不需要"。
    steps.push(['decide', '自动判定', 'wait',
      S.needsRemote ? '随识别结果一并回来' : '还没跑']);
  } else {
    steps.push(['decide', '自动判定', 'wait', '等环境体检先出结果']);
  }
  // ③ 容量预检
  steps.push(['precheck', '容量预检', 'wait', '上段跑完才有依据']);

  const mk = { done: '✓', warn: '!', fail: '×', wait: '○' };
  return '<div class="stepstrip">' + steps.map(s =>
    '<span class="stp ' + s[2] + '"><span class="mk">' + mk[s[2]] + '</span>'
    + '<b>' + esc(s[1]) + '</b> <span class="dt">' + esc(s[3]) + '</span></span>').join('')
    + '</div>';
}

/* =====================================================================
   识别结论
   ---------------------------------------------------------------------
   保存并识别之后，页面不能只多出几个脚本条目 —— 那等于什么都没说：
   看的人不知道该不该往下走、这些脚本从哪来、还缺什么。
   所以这里只写两块，正好对应她要问的两个问题：
     ① 这个板块服务器适不适合跑（环境 + 判断依据合成一段）
     ② 脚本汇总（按流程固定的每个环节，列这一步能用的脚本 + 来源标签）

   为什么从四段并成两块：
     原来的四段是「环境评估 / 该用哪一套脚本 / 应用自带的补充脚本 /
     应用自带的脚本 / 应用自带的工具」—— 同一件事在四五处各说一半，
     人得自己在脑子里拼起来；而且按「脚本从哪来」分块，
     回答不了「这一步到底跑哪个脚本」这个真问题。
     现在按「流程环节」分块，来源退化成每条脚本上的一个标签。

   每一块的标题可以点开收起。
     收起状态按分析记录保存在服务端，
     不放在浏览器里：刷新、换一台电脑打开，看到的还是你上次收成什么样。
   ===================================================================== */

/* 小节收纳状态。
   存两份：S.collapsed 是当前生效的，写回服务端由 persistCollapsed 做。
   键名用固定短名（env / pick / sup / tools），不要用下标 ——
   下标会随着「这次有没有这一段」而变，收起的就变成另一段了。

   实现用 <details>/<summary> 而不是自己拿 JS 开关 div：
   原生元素自带键盘可达、找到关键字高亮、打印时按需展开，
   而且标题与内容的结构关系是写在标签里的，不会因为重渲染错位。 */
const IDENT_SECS = ['env', 'scripts'];

/* 四段默认收起 —— 用户的原话是「能折叠的都默认折叠，除非这个流程正在跑」。
   但「默认收起」有一个必须让路的例外：这一段正对应着正在跑的东西。
   正在等结果的人最需要看到的就是那一段的实况，把它收起来等于让人去猜。
   所以：
     · 用户手动点开过 → 尊重用户（c[key] === 0 表示显式展开）
     · 用户手动收起过 → 尊重用户（c[key] === 1）
     · 都没表态过     → 正在跑的那一段展开，其余收起 */
const IDENT_SEC_STAGE = { env: 'env', scripts: 'figs' };

function identSecOpen(key) {
  const c = S.collapsed || {};
  if (c[key] === 0) return true;      // 人明确点开过
  if (c[key] === 1 || c[key] === true) return false;  // 人明确收起过
  // 没表态：看这一段是不是正在跑
  return identSecRunning(key);
}

/* 这一段是不是「正在跑」。
   判据只用已有的东西，不猜：
     · 本机模式：S.running 且当前步骤落在这一段管的阶段上
     · 服务器模式：对应阶段的回写状态是 running / delegated
   拿不到任何依据时返回 false —— 默认收起是基线，不能因为不知道就全展开。 */
function identSecRunning(key) {
  // 环境体检那一趟本来就是「等助手写回」，等的时候要看得见
  // 识别这一趟正在等助手写回时，前两段是要看的
  if (S.identifyPhase === 'waiting' && (key === 'env' || key === 'pick')) return true;
  const stage = IDENT_SEC_STAGE[key];
  if (!stage) return false;
  if (S.running && S.step === stage) return true;
  const rs = (S.remoteSteps || {})[stage];
  if (rs && (rs.status === 'running' || rs.status === 'delegated')) return true;
  return false;
}

/* 一段的标题（可点开收起）。asideHTML 是标题右侧那行灰字说明。 */
function identSecHead(key, titleHTML, asideHTML) {
  const open = identSecOpen(key);
  return '<details class="secblk secfold" data-ident-sec="' + esc(key) + '"'
    + (open ? ' open' : '') + '>'
    + '<summary class="sech"><span class="fmk" aria-hidden="true"></span>'
    + titleHTML + (asideHTML || '') + '</summary>';
}

/* 收起状态落盘。失败不提示、不阻断阅读 —— 它只是阅读习惯，
   写不进去下一次点开还是默认展开，不影响任何结论。 */
let collapsedSaveTimer = null;
function persistCollapsed() {
  if (!S.sessionId) return;
  if (collapsedSaveTimer) clearTimeout(collapsedSaveTimer);
  const __recordScope = recordContext();
  collapsedSaveTimer = setTimeout(function () {
    if (!recordCurrent(__recordScope) || sessionChanging) return;
    postJSON('/api/sessions/update', { id: S.sessionId, collapsed: S.collapsed || {} })
      .catch(function () {});
  }, 400);
}

/* 把已经渲染好的四个小节就地变成可折叠的。
   为什么用 DOM 转换、而不是在拼 HTML 时就把 <details> 写进去：
   这四段的正文是分好几处拼出来的（每段里还有各自的分支），
   在拼串阶段插标签要改五六处、每处都可能把 </div> 配错；
   而「哪个 div 是小节」在 DOM 里是一眼可辨的（.secblk 且直接子节点里有 .sech），
   转换一次就够，正文一个字都不用动 —— 否则这次改动会碰到整块渲染逻辑，
   而那块逻辑里刚修好的东西（失败态、覆盖率判断）不该被顺手碰一遍。

   用 <details>/<summary> 而不是自己拿 JS 开关 div：
   原生元素自带键盘可达、Ctrl+F 能找到收起的内容、打印时按需展开。 */
function bindIdentFolds(root, keys) {
  if (!root) return;
  const list = arr(keys);
  // 四段都在最外层那张卡里，所以先定位卡，再取它的直接子 .secblk ——
  //   用 :scope 锁住层级，免得哪天某段内部再出现一个 .secblk，
  //   把里面的也当成一段收起来。
  const card = root.querySelector('.pl-card') || root;
  const blks = $$(':scope > .secblk', card);
  blks.forEach(function (blk, i) {
    const head = blk.querySelector(':scope > .sech');
    const key = list[i];
    if (!head || !key) return;
    const d = document.createElement('details');
    d.className = 'secblk secfold';
    d.setAttribute('data-ident-sec', key);
    if (identSecOpen(key)) d.open = true;
    const sum = document.createElement('summary');
    sum.className = head.className;
    sum.innerHTML = '<span class="fmk" aria-hidden="true"></span>' + head.innerHTML;
    d.appendChild(sum);
    Array.prototype.slice.call(blk.childNodes).forEach(function (n) {
      if (n === head) return;
      d.appendChild(n);
    });
    blk.parentNode.replaceChild(d, blk);
    d.addEventListener('toggle', function () {
      S.collapsed = S.collapsed || {};
      // 展开记 0、收起记 1 —— 都得记住。
      //   以前展开是 delete，于是「人点开过」和「从没表态」分不出来，
      //   默认一改成收起，人点开的那一段下次刷新又被收回去。
      S.collapsed[key] = d.open ? 0 : 1;
      persistCollapsed();
    });
  });
}

async function renderIdentify() {
  const __recordScope = recordContext();
  try {

  const host = $('#identifyHost'); if (!host) return;
  // 判据是「这一次点过保存并识别」，不是「本机记没记住运行目录」。
  //   服务器模式下运行目录在服务器上，本机 state 里是空的；
  //   按 S.runDir 判会让整块识别结论直接消失 —— 点了保存却什么都没出现。
  if (!S.identified) {
    host.innerHTML = S.recordStateError ? '<div class="small">' + esc(S.recordStateError) + '</div>' : '';
    return;
  }
  // 服务器模式下，「保存并识别」只是把活交给助手，识别结论要等它写回才有。
  //   这段时间只显示"在等什么"。绝不在这里下结论：本机读不到服务器的内存和包，
  //   拿这份空缺当"识别不了"是错的 —— 那只是"还没开始"。
  if (S.identifyPhase === 'waiting' || S.identifyPhase === 'timeout') {
    host.innerHTML = identifyWaitingHTML();
    return;
  }

  let env = S.sessionId ? S.envCheck || null : null, dec = null, lib = null;
  let envStale = false, staleEnv = null, staleNote = '';
  // 先看「这一份记录自己识别出了什么」。
  //   多份记录并存之后，这个才是正确的问题 —— /api/envcheck 回答的是
  //   「最近一次识别是什么」，切到旧记录时会显示新记录的数字。
  //   记录里有结论就用它；没有才回退去问 /api/envcheck（老数据/本机模式）。
  let fromSession = !!env;
  if (S.sessionId && !fromSession) {
    try {
      const ss = await recordAwait(getJSON('/api/sessions/state?id=' + encodeURIComponent(S.sessionId)), __recordScope);
      const ident = ss && ss.session && ss.session.identify;
      if (ident && ident.result
          && (!S.commitReq || ident.req_id === S.commitReq)) {
        env = ident.result;
        S.commitReq = ident.req_id || S.commitReq;
        fromSession = true;
      }
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  }
  // 一律问一次：服务器模式下，/api/envcheck 读的是执行端回写的那份结果。
  //   只认「属于最近一次保存并识别」的结果。
  //   不属于的话，它只是上一轮跑出来的旧数据 —— 拿来当结论显示，
  //   会让人以为刚点的那次已经生效了，而助手可能根本还没动。
  if (!fromSession && (!S.sessionId || !S.needsRemote)) {
    try {
      const ec = await recordAwait(getJSON('/api/envcheck'), __recordScope);
      envStale = !!ec.stale || (!!S.commitReq && !ecIsFresh(ec));
      if (envStale) {
        staleEnv = ec.result || null;
        env = null;
      } else {
        env = ec.result || null;
      }
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  }
  if (!S.needsRemote) {
    try { dec = (await recordAwait(getJSON('/api/decision'), __recordScope)).result || null; } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  }
  try { lib = await recordAwait(getJSON('/api/library'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
}
  S.envCheck = env; S.decision = dec;
  S.envStale = envStale; S.staleEnv = staleEnv;
  // 本机模式下没拿到体检/判定结果就没什么可说的；
  //   但服务器模式本来就不该有这两个结果 —— 不能因此把整块清空。
  if (!S.needsRemote && !env && !dec) { host.innerHTML = ''; return; }

  // 脚本统计：环节数、脚本数、需要留意的个数
  //   只把「能独立跑的脚本」计入需要留意；lib/ 下的子模块本来就靠别人
  //   source 调用，不需要启动锁，把它们算进去会虚报一堆无关的数字。
  const stages = arr(S.stages);
  let nScript = 0, nFlag = 0;
  stages.forEach(st => arr(st.scripts).forEach(s => {
    nScript++;
    const isModule = String(s.role || '').indexOf('子模块') >= 0 || String(s.path || '').indexOf('/lib/') >= 0;
    if (!s.guarded && !isModule) nFlag++;
  }));

  // 应用自带的工具：库表里 B 类（分析绘图）——这些不放在服务器上，
  //   应用自己带着，服务器缺什么都不影响它们。
  //   注意：entries 里没有 category 字段（只有 id 前缀），按 'B-' 前缀取。
  //   之前按 e.category==='B' 过滤，结果一条都取不到，第 ④ 段永远是空的。
  const tools = arr(lib && lib.entries).filter(e => String(e.id || '').indexOf('B-') === 0);

  const blocked = !!(dec && dec.path === 'blocked');
  const warnOnly = !blocked && env && env.verdict === 'warn';
  // 助手写回来了，但带回的是"这次没跑成"（连不上、SSH 失败、拿不到数据……）。
  //   这种情况必须显示成失败，并原样带上它给的原因。
  //   不判这一条的话，空的体检项会被当成"0 项不通过"画成"全部通过"——
  //   把一次失败读成成功，比不显示更糟。
  const envFailed = !!env && (env.ok === false || !!env.error || !arr(env.items).length);

  // 说清是"哪台机器"：用户选的是服务器时，写「服务器 10.1.1.2」而不是含糊的
  //   「这台机器」—— 那句在服务器模式下读起来完全不知道指谁。
  const pd = S.placeDraft || {};
  const where = (pd.mode === 'server')
    ? ('服务器 ' + (pd.host || '（未填地址）'))
    : '这台电脑（本机）';

  let h = '<div class="pl-card">';

  // ---- 总判断 ----
  h += '<div class="idhead">'
    + '<h3 class="idtitle">识别结论</h3>'
    + '<span class="idbadge ' + ((S.needsRemote && !env) ? 'warn'
        : ((blocked || envFailed) ? 'bad' : (warnOnly ? 'warn' : 'ok'))) + '">'
    + ((S.needsRemote && !env) ? '待检查'
        : ((blocked || envFailed) ? '没跑成' : (warnOnly ? '能跑，有提醒' : '可以跑'))) + '</span>'
    + '<span class="idwhere">' + esc(where) + '</span>'
    + '</div>';
  // 本次识别里的头三步走到哪了。
  //   这三步是「保存并识别」那一趟顺带做完的（体检 -> 自动判定 -> 容量预检），
  //   所以它们的状态显示在这里，而不是摆成「跑分析」里的三个按钮 ——
  //   摆成按钮，人点了只会得到一句"服务器模式要去助手那边跑"。
  h += identifyStepStrip(env, dec, envFailed);
  h += '<div class="idsum">' + esc((S.needsRemote && !env)
      ? '服务器还没检查过，等结果回来。'
      : (envFailed
        ? ('这次没跑成：' + String(env.error || '回写里没有体检项')
           + '。下面第 ① 段是它给的原因。')
        : (blocked
        ? ('现在跑不了：' + (arr(dec.gaps)[0] || '有必需项缺失'))
        : (warnOnly
          ? (esc(where) + ' 可以跑，有几处要留意。')
          : (esc(where) + ' 可以跑完整流程。')))))
    + '</div>';

  // ---- ① 这个板块服务器适不适合跑 ----
  // 这一段只回答一个问题：这台机器能不能把整条流程跑完。
  //   原来分成「环境评估」+「该用哪一套脚本」两段，说的是同一件事的两面：
  //   能不能跑，取决于机器够不够 + 有没有脚本可用。
  //   拆两段看的人要把两处结论自己合起来，合成一段才是他真正要的那句话。
  h += '<div class="secblk"><div class="sech">这个板块服务器适不适合跑</div>';
  if (S.needsRemote && !env) {
    h += '<div class="rz warn"><span class="mk">!</span><span>'
      + '<b>这次识别已交给助手</b> '
      + '<span class="dt">结果写回来这里会换成真实数字。</span></span></div>';
  } else if (envFailed) {
    h += '<div class="rz fail"><span class="mk">×</span><span>'
      + '<b>这次没跑成</b> <span class="dt">'
      + esc(String(env.error || env.checked_via || '') || '回写里没有体检项。')
      + '</span></span></div>';
  } else if (env) {
    const c = env.counts || {};
    h += '<div class="idmeta">检查 ' + arr(env.items).length + ' 项：通过 ' + (c.ok || 0)
      + '　提醒 ' + (c.warn || 0) + '　缺失 ' + (c.fail || 0)
      + (env.generated_at ? '　（' + esc(String(env.generated_at).replace('T', ' ')) + '）' : '') + '</div>';
    const bad = arr(env.items).filter(it => it.status !== 'ok');
    if (bad.length) {
      h += '<div class="rzlist">' + bad.map(it =>
        '<div class="rz ' + (it.status === 'fail' ? 'fail' : 'warn') + '">'
        + '<span class="mk">' + (it.status === 'fail' ? '×' : '!') + '</span>'
        + '<span><b>' + esc(it.label) + '</b> <span class="dt">' + esc(it.detail) + '</span></span></div>').join('')
        + '</div>';
    } else {
      // 全部通过时也别只说一句"全部通过"：看的人想知道内存多大、磁盘剩多少、
      //   数据在不在、包齐不齐。把这些实际值列出来，结论才可信。
      const keyIds = ['mem','disk','r_version','data_root','script_root'];
      const keys = arr(env.items).filter(it => keyIds.indexOf(it.id) >= 0);
      if (keys.length) {
        h += '<div class="rzlist">' + keys.map(it =>
          '<div class="rz ok"><span class="mk">✓</span><span><b>' + esc(it.label)
          + '</b> <span class="dt">' + esc(it.detail) + '</span></span></div>').join('') + '</div>';
      }
      const npkg = arr(env.items).filter(it => String(it.id).indexOf('pkg_') === 0).length;
      h += '<div class="rz ok"><span class="mk">✓</span><span><b>R 包</b> '
        + '<span class="dt">' + npkg + ' 个必需包全部可加载</span></span></div>';
    }
  } else {
    h += '<div class="idmeta">这次没拿到体检结果。</div>';
  }

  // 判断依据：为什么说能跑 / 不能跑。以前它藏在「这次该用哪一套脚本」里，
  //   现在跟结论放在一起 —— 结论和依据分开两段，人只会信一半。
  if (env && env.script_lib && arr(env.script_lib.files).length) {
    const lb = env.script_lib;
    const covers = arr(lb.covers);
    const pk = arr(env.items).filter(it => String(it.id).indexOf('pkg_') === 0);
    const pkOk = pk.filter(it => it.status === 'ok').length;
    h += '<div class="idmeta" style="margin-top:10px">判断依据：服务器上已经有 '
      + arr(lb.files).length + ' 个现成脚本，覆盖 ' + covers.length + ' 个环节；'
      + '必需 R 包 ' + pkOk + '/' + pk.length + ' 个可加载。'
      + '有现成的就用现成的，不再下发一套。</div>';
    const NEED = ['装包检查', '去双细胞', '合并聚类', 'marker 鉴定', '细胞比例', '差异与富集', '出图', '出 PPT', '打包'];
    const miss = NEED.filter(x => !covers.some(cv => String(cv).indexOf(x.split(' ')[0]) >= 0 || x.indexOf(String(cv)) >= 0));
    if (miss.length) {
      h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
        + '<b>服务器脚本没覆盖的活</b> <span class="dt">' + esc(miss.join('、'))
        + '　由应用侧补上：见下面「脚本汇总」里标着 AI skill 的那几条。</span></span></div>';
    } else {
      h += '<div class="rz ok" style="margin-top:8px"><span class="mk">✓</span><span>'
        + '<b>没有缺口</b> <span class="dt">整条流程都能用服务器这套跑完。</span></span></div>';
    }
  } else if (S.needsRemote && !env) {
    h += '<div class="idmeta">脚本清单要等这一趟识别回来才有，这里不猜。</div>';
  }
  h += '</div>';

  // ---- ② 脚本汇总 ----
  // 按「流程固定环节」列，而不是按目录列。
  //   为什么：人问的是「这一步到底跑哪个脚本」，目录结构回答不了这个问题。
  //   环节顺序来自 flow_spec（唯一权威），不再单独维护一份。
  // 每条只给一个来源标签：
  //   服务器自带   脚本在服务器上（{script_root} / {work_root}），原样调用
  //   AI 优化      应用库里的低内存适配版（sc-pipeline，从服务器那套改出来的）
  //   AI 补充      应用自带、服务器没有的（出图 / PPT 那一侧）
  // 脚本汇总：一个环节一条，只有「来源标签 + 可点的脚本名」。
  //   她要的是"这一步跑哪个脚本"，不是脚本目录的说明书。
  //   作用、来源路径这些解释性内容全部挪进点开的那张卡里 ——
  //   列表上多写一行，十二个环节就是十二行噪音。
  const flowStages = arr(S.flow && S.flow.stages);
  // 由 skill 提供的脚本，来源一律写「AI skill」——说的是它由哪个技能产出。
  const TAG_CN = { server: '服务器自带', lib: '脚本库提取', aiopt: 'AI 优化',
                   aisup: 'AI 补充', askill: 'AI skill' };
  const TAG_CLS = { server: 'srvt', lib: 'libt', aiopt: 'aiot', aisup: 'aisu', askill: 'aski' };

  // 每个环节只认 flow_spec 登记的那一个脚本（唯一权威）。
  //   以前还并列了 lib/ 子模块和补充脚本，同一环节摆三四个文件，
  //   看的人反而不知道真正跑的是哪个。
  function oneScriptFor(st) {
    const raw = String(st.script || '');
    if (!raw) return null;
    const isEngine = /^\{engine\}\//.test(raw);
    const rel = raw.replace(/^\{engine\}\//, '');
    const isCmd = !/\.(R|r|py|json|sh|pl|jl)$/.test(rel);
    let tag = 'server';
    if (isEngine) {
      // sc-pipeline 那几条是从服务器那套改出来的 → AI 优化；
      //   其余（出图 / PPT）由对应 skill 提供 → AI skill。
      tag = /^sc-pipeline\//.test(rel) ? 'aiopt' : 'aisup';
      if (tag === 'aisup' && st.skill_exists) tag = 'askill';
    }
    // 作用只在点开的卡里显示，列表上不写。
    //   取法按可信度排：补充脚本登记的标题 > 环节自己的说明。
    const sup = arr(S.supplement).find(x => String(x.for_stage || '') === String(st.id));
    return {
      name: isCmd ? (rel + '（系统命令）') : rel.split('/').pop(),
      path: isCmd ? '' : rel,
      raw: raw,
      tag: tag,
      canOpen: isEngine && !isCmd,
      isCmd: isCmd,
      role: (sup && sup.title) || String(st.note || ''),
      // 只有真的由 skill 提供的脚本才记来源 skill；系统命令不记。
      bySkill: (isEngine && !isCmd && st.skill_exists) ? String(st.skill || '') : '',
      // AI 优化那几条是从服务器那套原生脚本改出来的，按她的要求在这一行注一句。
      from: tag === 'aiopt' ? '服务器版优化而来' : ''
    };
  }

  // 点开脚本时，作用与来源路径显示在内容上方。
  S.scrMeta = {};
  h += '<div class="secblk"><div class="sech">脚本汇总</div>';
  if (!flowStages.length) {
    h += '<div class="idmeta">没读到流程定义，这里暂时是空的。</div>';
  } else {
    flowStages.forEach((st, idx) => {
      const sp = oneScriptFor(st);
      const nm = String(st.title || st.id);
      h += '<div class="stgline">'
        + '<span class="stgnum">' + (idx + 1) + '</span>'
        + '<span class="stgname">' + esc(nm) + '</span>'
        + '<span class="stgscr">';
      if (!sp) {
        h += '<span class="small">这一环节没有登记脚本</span>';
      } else {
        // 服务器自带的脚本也要能点：点开看它的路径、参数、作用，
        //   和「这一步到底怎么调它」。内容本身在服务器上，
        //   应用这台机器读不到 —— 卡片里如实说明，并把能给的都给出。
        S.scrMeta[sp.raw] = { tag: TAG_CN[sp.tag] || sp.tag, raw: sp.raw, from: sp.from,
                              stage: nm, role: sp.role, args: arr(st.args),
                              skill: sp.bySkill || '' };
        h += '<button class="scrname" data-sw-act="open" data-rel="' + esc(sp.raw) + '">'
          + esc(sp.name) + '</button>';
        h += '<span class="scrtag ' + (TAG_CLS[sp.tag] || 'srvt') + '">'
          + esc(TAG_CN[sp.tag] || sp.tag) + '</span>';
        if (sp.from) h += '<span class="scrfrom">' + esc(sp.from) + '</span>';
        // skill 标签只在「这份脚本确实由那个 skill 提供」时才挂。
        //   打包那一步跑的是系统 zip 命令，它不属于 sc-ppt-kit；
        //   以前按环节的 skill 名一律挂上，就成了「服务器自带 + skill」并列，自相矛盾。
        if (sp.bySkill) h += '<span class="scrskill">skill · ' + esc(sp.bySkill) + '</span>';
      }
      h += '</span></div>';
    });
  }
  h += '</div>';

  h += '</div>';
  host.innerHTML = h;
  // 四个小节渲染完再包成可折叠：正文一个字不改，只换外壳。
  bindIdentFolds(host, IDENT_SECS);

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   这次新增的四件事：
     1. 右侧导航（点一下跳到对应环节）
     2. 历史记录 + 快照 + 回退
     3. 脚本工作台（看 / 换 / 传 / 审 / 改）
     4. 交给助手的任务队列
   同时把「守卫 / 指纹 / 台账」这类词改成点开就有解释的说法。
   ===================================================================== */

/* ---------------- 术语：鼠标停上去就有解释 ---------------- */
const GLOSS = {
  guard: '脚本开头的一把锁。不是从唯一入口启动的，它就拒绝运行 —— 保证每次运行都被记录在案。',
  md5: '一串由文件内容算出来的字符。内容改一个字，它就完全不同。用来判断文件有没有被改过。',
  ledger: '一本只加不改的记录本。每次运行都追加一条：时间、参数、退出码、产物指纹。',
  whitelist: '一份登记表，写着这条流程允许用哪些脚本、各自指纹是多少。交付前拿实际用的去比对。',
  gate: '必须由人决定的问题。没答完，脚本直接拒绝启动，并告诉你还差哪几件。',
  snapshot: '把当时的设置（参数和答复）存一份，随时可以退回去。',
  stale: '这一步的结果做过，但因为你退回了更早的一步，它现在不算数了，需要重跑。',
};
const GLOSS_WORD = {
  guard: '入口锁', md5: '指纹', ledger: '台账', whitelist: '白名单',
  gate: '闸门', snapshot: '快照', stale: '待重跑',
};
function gterm(key) {
  return '<span class="term" title="' + esc(GLOSS[key] || '') + '">?</span>';
}
function gword(key) {
  return esc(GLOSS_WORD[key] || key) + gterm(key);
}

/* =====================================================================
   一 · 欢迎页：最近用过的运行目录，随时切回来
   ===================================================================== */
function navSections() {
  return $$('#main section.sec[data-nav]').filter(sec => sec.style.display !== 'none');
}

function renderSidenav() {
  const nav = $('#sidenav'); if (!nav) return;
  const secs = navSections();
  if (!secs.length) { nav.className = 'sidenav'; return; }
  let h = '', lastGroup = null;
  secs.forEach(sec => {
    const g = sec.getAttribute('data-navgroup') || '';
    if (g && g !== lastGroup) { h += '<div class="snavgroup">' + esc(g) + '</div>'; lastGroup = g; }
    let cls = 'snavitem';
    if (sec.querySelector('.stcard.failed') || sec.querySelector('.chk.fail')) cls += ' s-err';
    else if (sec.querySelector('.stcard.running')) cls += ' s-warn';
    else if (sec.querySelector('.stcard.done')) cls += ' s-done';
    // title / aria-label 都写上。
    //   窄屏时导航收成只剩圆点，文字被 display:none 藏掉，
    //   那时按钮既看不到名字、读屏软件也读不出它是什么。
    const nm = sec.getAttribute('data-nav');
    h += '<button class="' + cls + '" data-navjump="' + esc(sec.id) + '"'
      + ' title="' + esc(nm) + '" aria-label="' + esc(nm) + '">'
      + '<span class="dot"></span><span class="tx">' + esc(nm) + '</span></button>';
  });
  h += '<div class="snavsep"></div><button class="snavitem" data-navjump="__top"'
    + ' title="回到顶部" aria-label="回到顶部">'
    + '<span class="dot"></span><span class="tx">回到顶部</span></button>';
  nav.innerHTML = h;
  nav.className = 'sidenav on';
  markActiveNav();
}

function jumpTo(id) {
  if (id === '__top') { window.scrollTo({ top: 0, behavior: 'smooth' }); return; }
  const el = document.getElementById(id);
  if (!el) return;
  const y = el.getBoundingClientRect().top + window.pageYOffset - 74;
  window.scrollTo({ top: y, behavior: 'smooth' });
}

function markActiveNav() {
  const nav = $('#sidenav'); if (!nav) return;
  const secs = navSections();
  if (!secs.length) return;
  let best = secs[0].id, bestD = Infinity;
  const mid = window.innerHeight * 0.35;
  secs.forEach(sec => {
    const d = Math.abs(sec.getBoundingClientRect().top - mid);
    if (d < bestD) { bestD = d; best = sec.id; }
  });
  $$('.snavitem', nav).forEach(b => {
    b.classList.toggle('active', b.getAttribute('data-navjump') === best);
  });
}
window.addEventListener('scroll', function () { if (S.runDir) markActiveNav(); }, { passive: true });

/* =====================================================================
   三 · 历史记录与回退
   ===================================================================== */
const STEP_CN = {
  env: '环境体检', decide: '自动判定', precheck: '容量预检', pipeline1: '主流程上段',
  pipeline2: '主流程下段', plots: '出图', ppt: '出 PPT', pack: '打包结果',
};
const stepCN = id => STEP_CN[id] || id;

const STATE_CN = {
  done: ['已完成', 'done'], failed: ['失败', 'failed'], running: ['正在跑', ''],
  blocked: ['被拦下', 'failed'], not_run: ['还没跑', ''], stale: ['待重跑', 'stale'],
};
function stateMeta(s) { return STATE_CN[s] || [s || '', '']; }

async function renderHistory() {
  const __recordScope = recordContext();
  try {

  const host = $('#historyHost'), note = $('#histNote');
  if (!host) return;
  // 这一栏有两层东西，别混：
  //   上面一半 = 这一次分析的存档（本轮新增，按记录存，切记录跟着变）
  //   下面一半 = 流程自己的台账快照与历次运行（一直在的东西，没动）
  // 为什么要分开：存档管的是"这次分析我保存过的几个状态"，台账管的是
  //   "每一步跑没跑成"。合成一个列表会让人分不清退回去的是设置还是运行。
  let arcs = [];
  if (S.sessionId) {
    try {
      const r = await recordAwait(getJSON('/api/sessions/archives?id=' + encodeURIComponent(S.sessionId)), __recordScope);
      arcs = arr(r && r.archives);
    } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 arcs = []; }
  }

  let hh = '<div class="blocklabel">存档</div>';
  if (!arcs.length) {
    hh += '<div class="small">还没有存档。</div>';
  } else {
    hh += '<table class="libtbl"><thead><tr><th>时间</th><th>说明</th><th>含什么</th><th>操作</th></tr></thead><tbody>';
    arcs.slice(0, 20).forEach(a => {
      hh += '<tr><td class="pth">' + esc(String(a.created_at || '').replace('T', ' ')) + '</td>'
        + '<td>' + esc(a.label || a.id) + (a.note ? '<div class="note">' + esc(a.note) + '</div>' : '') + '</td>'
        + '<td>步骤 ' + (a.n_steps || 0) + ' 步</td>'
        + '<td><button class="btn btn-sm" data-arc-restore="'
        + esc(a.id) + '">退回到这里</button></td></tr>';
    });
    hh += '</tbody></table>';
  }
  hh += '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn" data-arc-snap="1">存一个存档</button></div>';
  hh += '<div style="margin-top:16px;padding-top:14px;border-top:1px solid var(--line-soft)">'
    + '<div class="blocklabel">流程台账</div></div>';

  if (!S.runDir) {
    hh += '<div class="small">还没有运行目录。</div>';
    host.innerHTML = hh;
    return;
  }
  let d;
  try { d = await recordAwait(getJSON('/api/history'), __recordScope); }
  catch (e) {
    if (!recordCurrent(__recordScope)) return;
 host.innerHTML = hh + '<div class="small">读台账失败：' + esc(e.message) + '</div>'; return; }
  if (!d.ok) { host.innerHTML = hh + '<div class="small">' + esc(d.error || '台账不可用') + '</div>'; return; }
  S.history = d;

  const steps = d.steps || {}, order = arr(d.order);
  const snapshots = arr(d.snapshots), runs = arr(d.runs);
  note.textContent = runs.length ? ('台账 ' + runs.length + ' 次运行') : '还没有运行记录';

  let h = '';
  h += '<div class="blocklabel">走到哪一步</div>';
  h += '<div style="display:flex;flex-wrap:wrap;gap:6px;align-items:center;margin-bottom:6px">';
  order.forEach((id, i) => {
    const st = steps[id] || {};
    const meta = stateMeta(st.state);
    if (i) h += '<span class="chainer">→</span>';
    h += '<button class="chainitem ' + meta[1] + '" data-hist-rerun="' + esc(id) + '" '
      + 'title="' + esc((st.invalidated ? '已作废，需重跑｜' : '') + '上次：' + (st.time || '没跑过')) + '">'
      + '<span class="n">' + (i + 1) + '</span>' + esc(stepCN(id))
      + '<span style="opacity:.75">' + esc(meta[0]) + '</span></button>';
  });
  h += '</div>';

  if (d.mark && d.mark.from_step) {
    h += '<div class="warnbox">已标记：从「' + esc(stepCN(d.mark.from_step)) + '」往后需重跑'
      + (arr(d.mark.will_rerun).length ? '（' + esc(arr(d.mark.will_rerun).map(stepCN).join('、')) + '）' : '')
      + '。产物不删。</div>';
  }

  h += '<div class="blocklabel" style="margin-top:18px">' + gword('snapshot') + '</div>';
  if (!snapshots.length) {
    h += '<div class="small">还没有快照。</div>';
  } else {
    h += '<table class="libtbl"><thead><tr><th>时间</th><th>说明</th><th>含什么</th><th>操作</th></tr></thead><tbody>';
    snapshots.slice(0, 20).forEach(s => {
      h += '<tr><td class="pth">' + esc(s.created_at || '') + '</td>'
        + '<td>' + esc(s.label || s.id) + (s.note ? '<div class="note">' + esc(s.note) + '</div>' : '') + '</td>'
        + '<td>' + (s.has_pipeline ? '参数' : '—') + (s.has_gates ? ' + 答复' : '') + '</td>'
        + '<td><button class="btn btn-sm" data-hist-restore="'
        + esc(s.id) + '">退回到这里</button></td></tr>';
    });
    h += '</tbody></table>';
  }
  h += '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn" data-hist-snap="1">存一个快照</button>'
    + '<button class="btn" data-hist-refresh="1">刷新</button></div>';

  h += '<div class="blocklabel" style="margin-top:20px">历次运行</div>';
  if (!runs.length) {
    h += '<div class="small">还没有记录。</div>';
  } else {
    runs.slice(0, 12).forEach(r => {
      const when = String(r.started || '').replace('T', ' ');
      const verdict = r.ok
        ? '<span class="histtag" style="background:var(--done-soft);color:var(--done)">成功</span>'
        : (r.has_blocked
          ? '<span class="histtag" style="background:var(--wait-soft);color:var(--wait)">被拦下</span>'
          : '<span class="histtag" style="background:var(--err-soft);color:var(--err)">有失败</span>');
      h += '<div class="histrow"><div class="histwhen">' + esc(when) + '</div><div class="histmain">'
        + '<div>' + verdict + ' <span class="small">' + esc(String(r.run_id || '')) + '</span></div>'
        + '<div class="histsteps">' + arr(r.steps).map(s => {
            const st = steps[s] || {};
            const meta = stateMeta(st.state);
            const cls = st.state === 'done' ? 'done' : (st.state === 'failed' ? 'failed' : (st.state === 'stale' ? 'stale' : ''));
            return '<span class="hstep ' + cls + '">' + esc(stepCN(s)) + ' · ' + esc(meta[0]) + '</span>';
          }).join('') + '</div>'
        + (arr(r.set_ids).length ? '<div class="small" style="margin-top:4px">用到的脚本套：'
            + esc(arr(r.set_ids).join('、')) + '</div>' : '')
        + '</div></div>';
    });
  }

  host.innerHTML = hh + h;

  // 历史读完再刷一次导航与步骤卡片：
  //   renderAll 里这两个是先于历史渲染的，拿不到「待重跑」状态。
  //   不回头刷一次，用户退回后要手动刷新页面才看得到红点。
  if (S.runDir) { renderRun(); renderSidenav(); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function histSnapshot() {
  const __recordScope = recordContext();
  try {

  try {
    const r = await recordAwait(postJSON('/api/snapshot', { label: 'manual' }), __recordScope);
    toast(r.ok ? '已存快照：' + r.id : ('存不了：' + (r.error || '')), r.ok ? 'good' : 'bad');
    renderHistory();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('存快照失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 这次分析的存档：整份记下来（识别结论、填的位置、步骤状态）。
   与流程台账的快照不是一回事 —— 那份记的是 pipeline.json / gates.json。 */
async function archiveNow(label, note) {
  const __recordScope = recordContext();
  try {

  if (!S.sessionId) { toast('还没有分析记录。', 'bad'); return null; }
  try {
    const r = await recordAwait(postJSON('/api/sessions/archive', {
      id: S.sessionId,
      label: label || 'manual',
      note: note || '',
    }), __recordScope);
    if (!r || !r.ok) { toast('存档失败：' + ((r && r.error) || '未知原因'), 'bad'); return null; }
    if (!label || label === 'manual') toast('已存档：' + r.archive.id, 'good');
    // 存完立刻把列表刷出来。
    //   不刷的话，用户点完按钮看不到任何变化（列表里还是"还没有存档"），
    //   会以为没存上、再点一次，于是重复存。
    renderHistory();
    renderSessionsBar();
    return r.archive;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('存档失败：' + e.message, 'bad'); return null; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function archiveRestore(id) {
  const __recordScope = recordContext();
  try {

  if (!confirm('退回到 ' + id + '？\n当前状态会先存一份，产物不删。')) return;
  try {
    const r = await recordAwait(postJSON('/api/sessions/restore', { id: S.sessionId, archive: id }), __recordScope);
    if (!r || !r.ok) { toast('退不了：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    await recordAwait(loadSessionsState(), __recordScope);
    sessionChanging = false;
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    renderHistory();
    toast('已退回，当前状态另存为 ' + (r.kept || ''), 'good');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('退不回去：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function histRestore(id) {
  const __recordScope = recordContext();
  try {

  if (!confirm('退回快照 ' + id + '？\n当前状态会先存一份，产物不删。')) return;
  try {
    const r = await recordAwait(postJSON('/api/rollback', { mode: 'restore', snapshot: id }), __recordScope);
    if (r.ok) { toast('已退回。', 'good'); await recordAwait(loadRun(), __recordScope); }
    else toast('退不了：' + (r.error || ''), 'bad');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('回退失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function histMarkRerun(step) {
  const __recordScope = recordContext();
  try {

  const idx = arr(S.history && S.history.order).indexOf(step);
  const after = idx >= 0 ? arr(S.history.order).slice(idx).map(stepCN).join(' → ') : stepCN(step);
  if (!confirm('标记从「' + stepCN(step) + '」往后重跑？\n' + after + '\n产物不删。')) return;
  try {
    const r = await recordAwait(postJSON('/api/rollback', { mode: 'rerun', from_step: step }), __recordScope);
    if (r.ok) {
      toast('已标记：从「' + stepCN(step) + '」往后重跑。', 'good');
      renderHistory(); renderRun();
    } else toast('标记失败：' + (r.error || ''), 'bad');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('标记失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   脚本区：按环节折叠，每个脚本四个动作
   ---------------------------------------------------------------------
   为什么改成折叠 + 行内四按钮：
   之前把清单、解释、换一份、改一处全部平铺在一块，还带一大段说明文字。
   用户的实际反馈是「这个脚本工作台是干啥用的啊」——问题不在功能，
   在于它一上来就把所有东西摆出来，没有入口的层次。
   现在：默认只看到环节名和数量，点开才看脚本；
   每个脚本旁边就是能对它做的事，不用先读一段说明才知道哪个按钮管什么。
   ===================================================================== */
const CHECK_CN = {
  in_inventory: '在流程的脚本清单里',
  guard: '带有启动锁（不能被绕过单独运行）',
  ascii: '源码是纯英文（旧版 R + 中文 Windows 上才解析得了）',
  reads_config: '参数从配置文件读，没有写死在代码里',
  hardcoded: '没有明显的写死阈值',
  stop_on_fail: '出错就停，不会带着错往下算',
  writes_outdir: '结果写到输出目录',
  declares_stage: '头部写清了属于哪一段',
  parses: '代码语法正确，能正常解析',
};
const CHECK_MK = { pass: ['✓', 'pass'], warn: ['!', 'warn'], fail: ['✗', 'fail'], skip: ['–', 'skip'] };

function checksHTML(r) {
  let h = '';
  if (r.verdict === 'pass') {
    h += '<div class="pickbox ok"><span class="ic">✓</span><span class="tx"><b>可以用</b></span></div>';
  } else if (r.verdict === 'warn') {
    h += '<div class="pickbox warn"><span class="ic">!</span><span class="tx"><b>大体能用，有几点要注意</b></span></div>';
  } else {
    h += '<div class="pickbox no"><span class="ic">✗</span><span class="tx"><b>现在还不能用</b></span></div>';
  }
  h += '<div class="chklist">';
  arr(r.checks).forEach(c => {
    const mk = CHECK_MK[c.status] || ['?', 'skip'];
    h += '<div class="chk ' + mk[1] + '"><span class="mk">' + mk[0] + '</span><span>'
      + esc(CHECK_CN[c.id] || c.id)
      + (c.evidence ? ' <span class="ev">' + esc(c.evidence) + '</span>' : '')
      + '</span></div>';
  });
  h += '</div>';
  return h;
}

/* 一句话说清这个脚本能不能直接用。
   不再用「要看一眼」这种含糊说法 —— 用户原话是「我也看不懂什么叫要看一眼」。 */
function scriptVerdict(s) {
  if (s.guarded) return '<span class="flagline ok">可以用</span>';
  return '<span class="flagline no">建议先检查</span>';
}

async function renderStageScripts() {
  const __recordScope = recordContext();
  try {

  // 只负责取数据。渲染在 renderIdentify() 的第三段里做 ——
  //   用户明确指出：脚本明细应当属于识别结果，不该单独吊在下面。
  const host = $('#scriptsHost');
  let d;
  try { d = await recordAwait(getJSON('/api/stage-scripts'), __recordScope); }
  catch (e) {
    if (!recordCurrent(__recordScope)) return;
 S.stages = []; if (host) host.innerHTML = ''; return; }
  S.stages = (d && d.ok) ? arr(d.stages) : [];
  // 应用自带脚本的清单单独取一次：它回答"这一步到底跑哪个文件"。
  try {
    const sp = await recordAwait(getJSON('/api/supplement'), __recordScope);
    S.supplement = (sp && sp.ok) ? arr(sp.items) : [];
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 S.supplement = []; }
  if (host) host.innerHTML = '';   // 旧容器不再使用

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 脚本明细的 HTML（供识别结论第三段内联使用）
   list：要渲染的环节数组。不传就用全部环节。
     过滤放在调用处做 —— 这一段现在的语义是「补充脚本」，
     该显示哪几个环节是调用处按服务器脚本库的覆盖情况算出来的。 */
function scriptListHTML(list) {
  let h = '';
  arr(list === undefined ? S.stages : list).forEach(function (st) {
    h += '<details class="stgfold"><summary>'
      + '<span class="stgname">' + esc(st.name) + '</span>'
      + '<span class="skilltag" title="这一环节由这个 skill 负责">skill · ' + esc(st.skill) + '</span>'
      + '<span class="cnt">' + st.count + ' 个脚本</span>'
      + '</summary>';
    if (st.desc) h += '<div class="stgdesc">' + esc(st.desc) + '</div>';
    if (!st.count) {
      h += '<div class="stgempty">这一环节没找到脚本。</div>';
    } else {
      arr(st.scripts).forEach(function (sp) {
        h += '<div class="scriptrow">'
          + '<div class="sn"><b>' + esc(sp.name) + '</b>'
          + (sp.role ? '<span class="srole">' + esc(sp.role) + '</span>' : '')
          + '<div class="p">' + esc(sp.path) + '</div></div>'
          + scriptVerdict(sp)
          + '<div class="acts">'
          + '<button class="btn" data-sw-act="open" data-rel="' + esc(sp.path) + '">打开</button>'
          + '<button class="btn" data-sw-act="check" data-rel="' + esc(sp.path) + '">检查</button>'
          + '<button class="btn" data-sw-act="change" data-rel="' + esc(sp.path) + '">修改</button>'
          + '<button class="btn" data-sw-act="upload" data-rel="' + esc(sp.path) + '">上传</button>'
          + '</div></div>';
      });
    }
    h += '</details>';
  });
  return h;
}

/* ---------------- 动作一：打开（看内容） ---------------- */
async function actOpen(rawRel) {
  const __recordScope = recordContext();
  try {

  const meta = (S.scrMeta || {})[rawRel] || {};
  // 传进来的是流程登记里的原样路径，可能带 {engine} / {script_root} / {work_root}。
  //   带 {engine} 的读得到（脚本在应用这一侧）；
  //   另外两个在服务器上，本机没有实体 —— 不能假装读得到，也不能什么都不给。
  const isEngine = /^\{engine\}\//.test(rawRel);
  const rel = rawRel.replace(/^\{engine\}\//, '');
  S.curScript = isEngine ? rel : '';
  const ttl = $('#scriptTitle'); if (ttl) ttl.textContent = meta.name || rawRel.split('/').pop();
  const b = $('#scriptBody');
  openDlg('dlgScript');

  // ---- 卡片顶部：这一条到底是什么（列表上不写的解释都在这里）----
  function metaBlock(extra) {
    let h = '<div class="scrmeta">';
    if (meta.stage) h += '<span><span class="k">环节</span> <b>' + esc(meta.stage) + '</b></span>';
    if (meta.tag) h += '<span><span class="k">来源</span> <b>' + esc(meta.tag) + '</b>'
      + (meta.from ? '<span class="k">（' + esc(meta.from) + '）</span>' : '') + '</span>';
    if (meta.skill) h += '<span><span class="k">skill</span> <b>' + esc(meta.skill) + '</b></span>';
    h += '<span><span class="k">路径</span> ' + esc(meta.raw || rawRel) + '</span>';
    h += extra || '';
    h += '</div>';
    if (meta.role) h += '<div class="small" style="margin:-4px 0 12px">' + esc(meta.role) + '</div>';
    return h;
  }

  // ---- 服务器上的脚本：内容读不到，就把"怎么跑"讲清楚 ----
  if (!isEngine) {
    const src = /^\{script_root\}/.test(rawRel) ? '服务器脚本目录'
      : (/^\{work_root\}/.test(rawRel) ? '服务器结果目录' : '服务器');
    let h = metaBlock('<span><span class="k">位置</span> ' + esc(src) + '</span>');
    h += '<div class="rz warn" style="margin-bottom:12px"><span class="mk">!</span><span>'
      + '<b>这份脚本在服务器上</b> '
      + '<span class="dt">应用这台机器看不到服务器磁盘，所以这里没有它的代码正文。'
      + '下面写的是它在流程里怎么被调用。</span></span></div>';
    if (arr(meta.args).length) {
      h += '<div class="blocklabel">这一步传给它的参数</div>'
        + '<pre class="scriptcode">' + esc(arr(meta.args).join(' ')) + '</pre>';
    }
    h += '<div class="small" style="margin-top:12px">要看正文，让助手在服务器上读一次：'
      + '「cat ' + esc(rawRel) + '」。</div>';
    if (b) b.innerHTML = h;
    return;
  }

  // ---- 应用这一侧的脚本：正常读正文 ----
  if (b) b.innerHTML = '<div class="small">正在读…</div>';
  try {
    const d = await recordAwait(getJSON('/api/script-read?rel=' + encodeURIComponent(rel)), __recordScope);
    if (!d.ok) {
      if (b) b.innerHTML = metaBlock() + '<div class="small">读不到：' + esc(d.error || '') + '</div>';
      return;
    }
    S.curScriptInfo = d;
    let h = metaBlock('<span><span class="k">大小</span> ' + fmtSize(d.size)
      + '　<span class="k">改于</span> ' + esc(d.mtime || '') + '</span>');
    h += '<pre class="scriptcode">' + esc(d.content || '') + '</pre>';
    const bks = arr(d.backups);
    if (bks.length) {
      h += '<div class="blocklabel" style="margin-top:16px">历史版本</div>'
        + '<table class="libtbl"><thead><tr><th>时间</th><th>指纹</th><th>操作</th></tr></thead><tbody>'
        + bks.slice(0, 6).map(x => '<tr><td class="pth">' + esc(x.at) + '</td>'
          + '<td class="pth">' + esc(String(x.md5 || '').slice(0, 10)) + '</td>'
          + '<td>' + (x.legacy
              ? '<span class="small">早期存档，无法还原</span>'
              : '<button class="btn" data-sw-restore="' + esc(x.id) + '">还原</button>')
          + '</td></tr>').join('') + '</tbody></table>';
    }
    if (b) b.innerHTML = h;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    if (b) b.innerHTML = metaBlock() + '<div class="small">读不到：' + esc(e.message) + '</div>';
  }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* ---------------- 动作二：检查 ----------------
   机械检查在前端能立刻拿到；要读懂代码的判断交给助手。 */
async function actCheck(rel) {
  const __recordScope = recordContext();
  try {

  S.curScript = rel;
  const t = $('#checkTitle'); if (t) t.textContent = '检查 · ' + rel.split('/').pop();
  const b = $('#checkBody');
  if (b) b.innerHTML = '<div class="small">正在检查…</div>';
  openDlg('dlgCheck');
  try {
    const r = await recordAwait(getJSON('/api/script-review?rel=' + encodeURIComponent(rel)), __recordScope);
    if (!r.ok) { if (b) b.innerHTML = '<div class="small">检查失败：' + esc(r.error || '') + '</div>'; return; }
    let h = checksHTML(r);
    h += '<div style="margin-top:14px;padding-top:14px;border-top:1px solid var(--line-soft)">'
      + '<div class="small" style="margin-bottom:8px">是否能判的已在上面。'
      + '方法层面要读代码，交给助手：</div>'
      + '<div class="btnrow"><button class="btn primary" data-sw-ask="1">交给助手审阅</button>'
      + '<span id="askMsg" class="small"></span></div></div>';
    if (b) b.innerHTML = h;
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 if (b) b.innerHTML = '<div class="small">检查失败：' + esc(e.message) + '</div>'; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* ---------------- 动作三：修改（换一份 或 提改动要求） ---------------- */
function actChange(rel) {
  S.curScript = rel;
  const t = $('#changeTitle'); if (t) t.textContent = '修改 · ' + rel.split('/').pop();
  const b = $('#changeBody');
  if (b) b.innerHTML =
      '<div class="small" style="margin-bottom:12px">' + esc(rel) + '</div>'
    + '<div class="blocklabel">一、传新版本替换</div>'
    + '<input type="file" id="chgFile" class="filein">'
    + '<div class="btnrow"><button class="btn" data-sw-replace="1">用这份替换</button>'
    + '<span class="small">替换前自动备份。</span></div>'
    + '<div style="height:18px"></div>'
    + '<div class="blocklabel">二、写要求交给助手改</div>'
    + '<textarea class="idea" id="chgIdea" placeholder="例如：质控下限改成从配置读"></textarea>'
    + '<div class="btnrow" style="margin-top:8px">'
    + '<button class="btn primary" data-sw-change="1">把这条改动要求交给助手</button>'
    + '<span id="chgMsg" class="small"></span></div>';
  openDlg('dlgChange');
}

/* ---------------- 动作四：上传（传完自动送审） ---------------- */
function actUpload(rel) {
  S.curScript = rel;
  const t = $('#uploadTitle'); if (t) t.textContent = '上传 · ' + rel.split('/').pop();
  const b = $('#uploadBody');
  if (b) b.innerHTML =
      '<div class="small" style="margin-bottom:12px">'
      + '上传会替换 ' + esc(rel) + '，并自动送审。</div>'
    + '<input type="file" id="upFile" class="filein">'
    + '<div class="btnrow"><button class="btn primary" data-sw-uploadexec="1">上传并送审</button>'
    + '<span id="upMsg" class="small"></span></div>'
    + '<div id="upResult" style="margin-top:14px"></div>';
  openDlg('dlgUpload');
}

/* 执行「上传并送审」：先替换，再立刻跑一次检查并把结论直接显示出来。
   用户不需要自己去点「检查」——上传这个动作本身就包含审核。 */
async function doUploadAndReview() {
  const __recordScope = recordContext();
  try {

  const inp = $('#upFile');
  const f = inp && inp.files && inp.files[0];
  const msg = $('#upMsg'), out = $('#upResult');
  if (!f) { toast('先选一个文件。', 'bad'); return; }
  if (f.size > 2 * 1024 * 1024) { toast('文件太大（上限 2 MB）。', 'bad'); return; }
  if (msg) msg.textContent = '正在上传…';
  try {
    const content = await recordAwait(f.text(), __recordScope);
    const up = await recordAwait(postJSON('/api/script-upload', { name: f.name, content: content }), __recordScope);
    if (!up.ok) { if (msg) msg.textContent = '上传失败：' + (up.error || ''); return; }
    if (msg) msg.textContent = '正在替换并检查…';
    const put = await recordAwait(postJSON('/api/incoming', { action: 'adopt', id: up.id, rel: S.curScript, mode: 'replace' }), __recordScope);
    if (!put.ok) { if (msg) msg.textContent = '替换失败：' + (put.error || ''); return; }
    if (msg) msg.textContent = '已替换，检查结论如下';
    const rv = await recordAwait(getJSON('/api/script-review?rel=' + encodeURIComponent(S.curScript)), __recordScope);
    if (out) out.innerHTML = rv.ok ? checksHTML(rv)
      : '<div class="small">检查失败：' + esc(rv.error || '') + '</div>';
    renderStageScripts();
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 if (msg) msg.textContent = '失败：' + e.message; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* 用文件直接替换（修改弹窗的方式一） */
async function doReplaceWithFile() {
  const __recordScope = recordContext();
  try {

  const inp = $('#chgFile');
  const f = inp && inp.files && inp.files[0];
  if (!f) { toast('先选一个文件。', 'bad'); return; }
  if (f.size > 2 * 1024 * 1024) { toast('文件太大（上限 2 MB）。', 'bad'); return; }
  if (!confirm('用这份替换 ' + S.curScript + '？\n\n现在这份会自动备份，之后可以还原。')) return;
  try {
    const content = await recordAwait(f.text(), __recordScope);
    const up = await recordAwait(postJSON('/api/script-upload', { name: f.name, content: content }), __recordScope);
    if (!up.ok) { toast('上传失败：' + (up.error || ''), 'bad'); return; }
    const put = await recordAwait(postJSON('/api/incoming', { action: 'adopt', id: up.id, rel: S.curScript, mode: 'replace' }), __recordScope);
    if (!put.ok) { toast('替换失败：' + (put.error || ''), 'bad'); return; }
    toast('已替换。旧版本已备份。', 'good');
    closeDlg('dlgChange');
    renderStageScripts();
    actCheck(S.curScript);
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('替换失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function submitChange() {
  const __recordScope = recordContext();
  try {

  const ta = $('#chgIdea'); const idea = ta ? ta.value.trim() : '';
  const msg = $('#chgMsg');
  if (!idea) { toast('先写下想改什么。', 'bad'); return; }
  try {
    const r = await recordAwait(postJSON('/api/script-change', { rel: S.curScript, idea: idea }), __recordScope);
    if (r.ok) {
      if (msg) msg.textContent = '已交给助手。';
      toast('已交给助手。', 'good');
      closeDlg('dlgChange');
      renderTaskLog();
    } else { if (msg) msg.textContent = '提交失败：' + (r.error || ''); }
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 if (msg) msg.textContent = '提交失败：' + e.message; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function askReview() {
  const __recordScope = recordContext();
  try {

  const msg = document.getElementById('askMsg');
  if (msg) msg.textContent = '正在提交…';
  try {
    const r = await recordAwait(postJSON('/api/script-ask', { rel: S.curScript, question: S.lastIdea || '' }), __recordScope);
    if (r.ok) {
      if (msg) msg.textContent = '已交给助手，结论见任务记录。';
      toast('已交给助手审阅。', 'good');
      renderTaskLog();
    } else if (msg) msg.textContent = '提交失败：' + (r.error || '');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 if (msg) msg.textContent = '提交失败：' + e.message; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

async function restoreBackup(id) {
  const __recordScope = recordContext();
  try {

  if (!confirm('还原成这一版？\n\n当前这份会先备份。')) return;
  try {
    const r = await recordAwait(postJSON('/api/script-restore', { backup: id }), __recordScope);
    if (r.ok) { toast('已还原。', 'good'); closeDlg('dlgScript'); renderStageScripts(); }
    else toast('还原失败：' + (r.error || ''), 'bad');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('还原失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   交给助手做的事：以任务记录的形式挂在脚本区下面
   ---------------------------------------------------------------------
   之前它自己占一个区块，叫「交给助手的任务」——用户的原话是
   「这个又是什么东西啊」。它其实是脚本动作的副产品，放在动作旁边
   才讲得通；没任务时整块不出现。
   ===================================================================== */
const TASK_CN = {
  script_review: '脚本审阅', script_change: '脚本改动',
  review: '环节质检', stage: '跑某一环节',
};
const TASK_ST_CN = { pending: '等助手处理', running: '处理中', done: '已完成', failed: '失败' };

/* 把一段文本注入会话。
   这个应用以「隧道发布」挂到公网，网关会往页面注入 SDK
   （/_catsco/runtime/annotations-v1.js，挂成 window.CatsCoAnnotations）。
   它的语义就是：这段话作为一条消息出现在对话里，助手看得到。

   读源码得来的三条硬事实，写在这里免得下次又猜：
     1. 宿主连接是异步的，instance() 一开始返回 null，要等。
     2. 只有应用被嵌在会话里打开时才有宿主；独立窗口、直接开网址都没有。
     3. proposeCompose 的返回值只说明"有没有交出去"，
        真正成败要看 onComposeAck 的回执。 */
function sessionChannel() {
  const api = window.CatsCoAnnotations;
  if (!api || typeof api.instance !== 'function') return { state: 'no_sdk', api: null };

  // 读 SDK 源码得来的判据：它只在页面被嵌进另一个窗口时才挂握手监听
  //   （源码里那句 window.parent !== window 就是这道门槛）。
  //   所以"不在会话框里"必须先判出来 —— 否则页面会一直显示"正在连接"，
  //   让人干等一个永远不会来的握手。
  let inFrame = false;
  try { inFrame = window.parent !== window; } catch (e) { inFrame = true; }

  let inst = null;
  try { inst = api.instance(); } catch (e) { inst = null; }
  if (inst) return { state: 'connected', api: api, inst: inst, inFrame: inFrame };
  if (!inFrame) return { state: 'standalone', api: api, inst: null, inFrame: false };
  return { state: 'waiting', api: api, inst: null, inFrame: true };
}

/* 第二条路：平台任务通道（window.catscoArtifactHost）。
   为什么要有两条：compose 那条要宿主先发 happened 握手，而握手不来是
   平台侧的事，本机改不动。任务通道是平台给「页面点一下 → 变成一次真实
   会话」的正规入口，它自己会向宿主报到（bridge.ready），连上与否能直接问。
   两条都试，谁通走谁；两条都不通，就把指令存进本机队列，绝不白点。 */
/* 任务通道到底连上没有 —— 只认宿主主动发来的握手。
   ------------------------------------------------------------------
   为什么不能再用 isConnected()：
     bridge 的 handleMessage 只要"消息来自父页 + origin 在白名单"就会把
     connectedParentOrigin 置位，不管那条消息是什么类型。而网关发布的页面
     本来就会和父页来回发 annotations 的消息，于是 isConnected() 恒为 true。
   结果就是：任务请求交给了一个根本不收它的宿主，15 秒后超时
   （Artifact task request timed out），本该可用的注入通道还被它抢先绕开。
   所以这里改成自己听握手：只有真收到 catsco.artifact.host.connect.v1，
   才认为任务通道可用；听不到就老老实实走注入。 */
let taskHostHandshakeAt = 0;
try {
  window.addEventListener('message', function (ev) {
    let isParent = false;
    try { isParent = (ev.source === window.parent) && (window.parent !== window); } catch (e) { isParent = false; }
    if (!isParent) return;
    const d = ev.data;
    if (d && typeof d === 'object' && d.type === 'catsco.artifact.host.connect.v1') {
      taskHostHandshakeAt = Date.now();
    }
  });
} catch (e) {}

function hostBridge() {
  const h = window.catscoArtifactHost;
  if (!h || typeof h.requestTask !== 'function') return null;
  // 这里刻意不用 h.isConnected()：在这套发布方式下它是假阳性。
  return { api: h, connected: taskHostHandshakeAt > 0 };
}



function saveOutbox(text) {
  // 带上会话号：队列里的指令也是"从某个会话点出来的"，
  //   取出来重发时要能回到同一个会话，否则又变成不知道送去哪儿。
  return postJSON('/api/outbox', { text: String(text || ''), session: S.topicId || '' })
    .catch(function (e) { return { ok: false, error: String(e && e.message || e) }; });
}

function composeSend(inst, text, finish) {
  let ackTimer = null;
  try {
    inst.onComposeAck(function (ack) {
      if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
      finish(ack && ack.accepted
        ? { ok: true, via: 'compose' }
        : { ok: false, via: 'compose', code: (ack && ack.reason) || 'unknown',
            why: '平台没收下：' + ((ack && ack.reason) || '未知原因') + '。' });
    });
  } catch (e) {}
  let sent = false;
  try { sent = inst.proposeCompose({ text: text }); } catch (e) { sent = false; }
  if (!sent) {
    if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
    finish({ ok: false, via: 'compose', code: 'send_failed', why: '交给平台时没成功。' });
    return;
  }
  ackTimer = setTimeout(function () {
    ackTimer = null;
    finish({ ok: false, via: 'compose', code: 'ack_timeout',
      why: '12 秒没回执，通道可能已断。' });
  }, 12000);
}

/* 把一段文本注入会话。
   ---------------------------------------------------------------------
   现在这件事交给应用外壳（app-shell.js）统一做，这里只做一层薄适配：
     通道判定、失败入队、每小时复查补发、回写销项，全在外壳里。
   为什么收上去：以前这里自己判"连上没有"、自己决定失败后怎么办，
   判据和外壳不一致（任务通道 isConnected() 恒为真那次，就把本来能用的
   注入通道整个绕过去了）；而且送失败之后只剩"请自己复制"。
   调用点不用改：签名与返回结构保持原样。 */
function currentStepTask(step, execId) {
  if (!S.sessionId) return null;
  return arr(S.execTasks).find(x => x.step === step && x.session_id === S.sessionId
    && (!execId || x.id === execId)) || null;
}

function stepNotificationBinding(step) {
  const sg = stageOf(step);
  if (sg && sg.exec_id) return {step: step, reqId: sg.exec_req_id || ''};
  const rs = (S.remoteSteps || {})[step];
  return {step: step, reqId: rs && rs.req_id || ''};
}

async function injectToSession(text, onState, purpose, binding, originLabel) {
  const record = recordContext();
  if (sessionChanging) return Promise.resolve({ok:false,code:"record_changing"});
  text = String(text || '');
  // 正文第一行写清是哪个按钮发起的。
  //   会话里只看到一大段指令时，人和助手都分不清这次是「一键全流程」还是「跑这一步」，
  //   也没法和页面对账；回写时更无法判断这条通知属于哪个动作。
  //   格式与服务端生成的那段保持一致：【按钮名】独占第一行。
  //   两边都写成"【本次操作】xxx / 【xxx】"会让同一条指令出现两种标题。
  if (originLabel) text = '【' + String(originLabel) + '】\n' + text;
  // A step notification carries its own request; environment recognition is separate.
  const explicit = binding !== undefined;
  const reqId = explicit ? String(binding && binding.reqId || '') : (S.commitReq || '');
  if (explicit && purpose !== 'query' && !reqId) {
    return {ok:false, code:'notification_request_missing', stage:'prepare', queued:false,
      why:'尚未取得这一步的任务编号，未发送观察通知；计算状态请看步骤卡片。'};
  }
  const meta = {
    analysisId: S.sessionId || '', purpose: purpose || 'execute',
    session: S.topicId || '', reqId: reqId,
    step: explicit ? String(binding && binding.step || '') : ((S.delegated && S.delegated.step) || ''),
    mode: (S.placeDraft && S.placeDraft.mode) || 'server',
    intent: 'assistant.connect.v1',
    // 按钮名要随请求一起传下去：入队时存进队列条目，
    //   补发时靠它重建「【本次操作】」标题。只拼正文不传这个字段的话，
    //   队列里那条仍然没有出处，补发出去还是看不出是谁发起的。
    originLabel: originLabel || '',
    isCurrent: () => !sessionChanging && recordCurrent(record) &&
      meta.analysisId === (S.sessionId || '') && meta.session === (S.topicId || '') &&
      (explicit || meta.reqId === (S.commitReq || ''))
  };
  const shell = window.AppShell;
  if (!shell || typeof shell.inject !== 'function') {
    if (onState && recordCurrent(record)) onState('failed');
    return {ok:false, code:'shell_missing', stage:'prepare', queued:false,
      why:'注入外壳未加载，无法核验完整任务；未发送、未入队。'};
  }
  if (onState) onState('sending');
  return shell.inject(text, meta).then(function (r) {
    if (onState && recordCurrent(record)) onState(r && r.ok ? 'connected' : (r && r.stage ? 'failed' : 'waiting'));
    // ok=true 只代表"交给宿主了"，真正做成要看回写 —— 外壳会盯着这一条。
    if (r && r.ok) return { ok: true, via: r.via || 'shell', code: r.code || 'accepted' };
    return Object.assign({}, r || {}, {
      ok: false,
      via: (r && r.via) || 'shell',
      code: (r && r.code) || 'not_connected',
      queued: !!(r && r.queued),
      id: (r && r.queueId) || '',
      why: (r && (r.why || r.error)) || '两条通道都没连上。'
    });
  });
}

/* =====================================================================
   把「要做什么」和上面填的服务器信息拼成一段文本
   ---------------------------------------------------------------------
   这段文本会整段原样作为一条消息进对话，所以只放助手真需要的东西：
   要做的事 + 目标机器 + 几个目录。界面上那些说明文案一律不带。
   两条硬规矩：
     · 口令绝不写进文本。它由应用在本机保管，这里只留一句"从本机配置读"。
     · 平台对载荷有长度上限，超了会被拒；这里做一次上限保护。
   ===================================================================== */
function buildInstructionText(ask) {
  const d = S.placeDraft || placeDraftFromServer();
  const L = [];

  L.push('【来自 个性化分析助手】');
  // 归属和防旧回写字段放在正文前，避免长指令截断它们。
  if (S.sessionId) L.push('本次分析记录号：session_id=' + S.sessionId + '（回写必须原样带上 session_id）');
  if (S.commitReq) L.push('本次请求号：req_id=' + S.commitReq + '（回写必须原样带上 req_id，拒绝旧请求）');
  if (S.agentHandoffPath) L.push('回写凭据文件：' + S.agentHandoffPath + '；仅获授权应用本机读取token并放X-Agent-Token请求头，不输出、不放正文、不自签；不得拿测试属主令牌替代。');
  else L.push('未交接回写凭据：仅复用既有合法授权，不自签、不冒充用户、不绕过403。');
  L.push('Python可在已授权分析服务器本地运行，工作电脑可仅作SSH跳板，无需安装Python；本地探针用--transport local --host服务器实际hostname，先核对目标服务器身份；--notebook-out输出体检ipynb，不是全分析复现。');
  if (S.topicId) L.push('回复路由会话号：' + S.topicId + '（仅消息去向，不限制执行聊天）');
  L.push('其他聊天可在已有授权范围内接续同一分析，无须召回原聊天；先核对 session_id、req_id 和任务状态，已有执行或完成回写时不得重复执行。聊天不同不代表授权扩大，平台握手的 topic 一致性认证仍须保留。');
  L.push('体检失败也须回写：POST /api/envcheck/import，body 带 session_id、req_id、host、result；result 使用探针失败 JSON，保留非零退出码，不把失败当成功。');
  L.push('要办的事：' + String(ask == null ? '' : ask).trim());

  if ((d.mode || 'local') === 'server') {
    L.push('');
    L.push('目标服务器：' + (d.host || '（主机未填）') + ':' + (d.port || 22));
    L.push('登录账号：' + (d.user || '（账号未填）'));
    // 执行电脑写在最前面第二行，因为它决定了"这条指令在哪台机器上跑"。
    //   以前这里只有一句「在能连上这台服务器的电脑上执行」，执行端看不到
    //   会话选中的是哪一台，只能挨台试 —— 试到连不上的那台就得把连接超时
    //   等完（实测白等 23 秒）。第一次识别成功后应用会自动把名字填上。
    if (d.exec_pc) {
      L.push('执行电脑/SSH跳板：' + d.exec_pc + '（已有授权shell直达目标服务器时可在服务器本地运行探针，不试其他机器）');
    } else {
      L.push('执行电脑：未记录。请在能连上这台服务器的电脑上执行；'
        + '跑通一次后应用会把它的名字记下来，下次直接指名。');
    }
    if (d.data_root) L.push('数据目录：' + d.data_root);
    if (d.script_root) L.push('脚本目录：' + d.script_root);
    if (d.work_root) L.push('结果目录：' + d.work_root);
    if (d.rscript) L.push('R 程序：' + d.rscript);
    // R 包目录与动态库路径：这两行是这次识别能不能成的关键。
    //   只给 R 程序、不给库路径时，探针会用 R 自己的默认 .libPaths()，
    //   而分析包全装在这台机器指定的库里；结果就是十个包挨个报
    //   dyn.load / "package 'Rcpp' could not be loaded" —— 看起来像"包没装"，
    //   其实是"找错了库"。所以这里必须显式写出来，并直接给出探针参数，
    //   让执行端不用自己推。
    if (d.r_lib_path) {
      L.push('R 包目录（必须用它，不要用 R 默认库路径）：' + d.r_lib_path);
    } else {
      L.push('R 包目录：未填。这时探针只能用 R 默认库路径，'
        + '若包不装在默认库里，加载失败不代表包缺失 —— 先把库路径补上再判断。');
      L.push('   库路径可从应用运行目录的 config/server_profile.json 读取 '
        + 'server.runtime.lib_path 与 server.runtime.ld_library_path（只读，不修改该文件）。');
    }
    if (d.ld_library_path) {
      L.push('动态库路径（LD_LIBRARY_PATH，缺了会报 Rcpp could not be loaded）：'
        + d.ld_library_path);
    }
    if (d.queue) L.push('队列：' + d.queue);
    if (d.node_constraint) L.push('节点约束：' + d.node_constraint);
    L.push('');
    // 这一节是给执行端的做法说明，不是客套话。
    //   要解决的是「每次都得先探一遍路」这件事：应用自带的探针脚本已经把
    //   取哪些项、怎么判、输出什么形状都定好了，照着跑就是一次 SSH 取全。
    //   所以这里明确写清：用哪个脚本、哪些参数、不要先做别的探测。
    L.push('怎么做（照这个来，不要自己另写探路的步骤）：');
    L.push('1. 连接仅限本次授权的执行设备、host、user、port；优先复用已核验的合法连接配置或凭据提供者，先核对其目标与授权范围。'
      + '不因文件年龄或临时文件身份判定凭据失效；过期、撤销或目标不匹配时不得使用。不得跨账号或扫描任意脚本搜取秘密。'
      + 'config/server_profile.json 仅是应用内相对位置，不代表执行电脑路径；具备应用本机读取权限时才从应用运行目录读取，不编造路径、不向用户索要配置路径。'
      + '不在对话、命令参数或日志输出密码；不绕过权限或主机身份校验。'
      + '非交互任务中，未提交密码的默认密钥失败不能判定账号或密码不可用。若工具仅允许默认密钥或无交互且无法提供密码，'
      + '记录 actual_auth_method（实际认证方式）和 authentication_not_supplied（未提供所需认证），不得记为 password_invalid；不得盲重试。');
    if (d.exec_pc === 'PC-20250808YJEP' && d.host === '10.1.1.2' && Number(d.port || 22) === 22 && d.user === 'hhhong1') {
      L.push('已核验的本次连接入口：仅在获授权的 PC-20250808YJEP 上，核对 C:\\Users\\Administrator\\AppData\\Local\\Temp\\pa_ssh.py 仍存在且其连接目标为本次 host、port、user；用 engine/workpc_check.py --transport ssh --expected-device PC-20250808YJEP --connector-sha256 5653441dc0130770190012b24cbda124ec2c6dc5307312cc88ca6c0df2f53760 --legacy-connector 指向此现有脚本。探针将命令从私密标准输入交给旧程序，旧程序自行持有凭据；不复制凭据、不逐台安装，也不得将该路径用于其他设备或其他目标。入口不可用则回写真实失败。');
    } else {
      L.push('密码通道的执行接口：将已核验合法提供者取得的密码仅在本次子进程环境 SC_REMOTE_PWD 中传给 engine/workpc_check.py --transport ssh；不把密码放入命令参数、正文或日志，不持久保存新的副本。');
    }
    L.push('2. 应用自带探针 engine/workpc_check.py（与 step_plan 同一目录）。'
      + '可在已授权目标服务器shell用本地python运行，或经指定工作电脑SSH执行；取全下列各项：'
      + '内存 / CPU / /share 剩余 / R 版本 / 10 个必需 R 包 / 数据目录逐个符号链接'
      + ' / 脚本目录与每个脚本的作用 / 结果目录可写 / 队列与节点约束。');
    if (d.r_lib_path || d.ld_library_path) {
      L.push('3a. 探针必须显式带上库路径，不许省略、不许只给 R 程序：'
        + (d.r_lib_path ? (' --libs ' + d.r_lib_path) : '')
        + (d.ld_library_path ? (' --ldlib ' + d.ld_library_path) : '')
        + '。探针会把它导出成 R_LIBS_USER / LD_LIBRARY_PATH 再跑 library()。');
      L.push('   为什么这一条不能省：分析包装在指定库里，R 默认 .libPaths() 找不到它们，'
        + '缺库路径时十个包会齐刷刷报 dyn.load / "package \'Rcpp\' could not be loaded"，'
        + '读起来像"包没装"，实际是"找错了库"。所以先确认库路径进去了，再判断包在不在。');
    }
    L.push('3. 参数按上面那几个目录与 R 程序填；脚本目录填上面「脚本目录」那一项。'
      + '探针在这一层没看到脚本时会自己下钻到 script 子目录，'
      + '所以你照给的路径填即可，不需要先去看目录结构。');
    L.push('4. 凭据可用后不要另做探路：不要试哪台机器能连、' 
      + '不要 ls 目录看结构、不要在整个盘上 glob 找文件（实测一次全盘 glob 要 12 秒，'
      + '多数还是 0 结果）。直接按第 2 步跑探针；连不上就把这条当作结果写回，'
      + '不要换机器重试。');
    L.push('5. 把脚本输出的 JSON 回写应用：POST /api/envcheck/import，'
      + 'body 放 result（含失败JSON）、host、session_id、req_id；X-Agent-Token从上述私密交接文件读取。');
    // 第 6 条：注释依据。
    //   用户可以在「服务器信息」里先定：给路径 / 上传 / 查文献 / 不定。
    //   定了就照定的来，不再让人去找；没定就明说"这一步先跳过，注释时会问"。
    //   为什么不再扫全盘：2026-11-04 那轮现场写了三版扫描脚本，第一版全盘深扫
    //   撞 300 秒超时白等 5 分钟，接着又写三个 xlsx 解析脚本猜表头。
    //   而实际上用户手上通常就知道那份表在哪 —— 问一句比扫一遍快得多。
    const rm = String(d.ref_mode || '');
    if (rm === 'path' && d.ref_path) {
      L.push('6. 注释依据：用户已给出服务器上的路径 ' + d.ref_path);
      L.push('   只核对这一个文件（存在性、字节数、表头里的物种与组织），');
      L.push('   再用 POST /api/reference/scan 登记这一条，body 的 items 只放它。');
      L.push('   不要扫目录、不要 find、不要去找别的候选表 —— 用户已经指定了这一份。');
    } else if (rm === 'upload') {
      L.push('6. 注释依据：用户会自己上传一份到应用里，不需要你去服务器上找。');
      L.push('   这一步跳过，不要扫、不要 find。');
    } else if (rm === 'lit') {
      L.push('6. 注释依据：用户选了到注释环节由你查文献建一份，这一步先跳过。');
      L.push('   不要扫服务器、不要 find。');
    } else if (rm === 'none') {
      L.push('6. 注释依据：这一轮不需要，跳过。不要扫、不要 find。');
    } else {
      L.push('6. 注释依据：用户这次还没给。不要扫全盘、不要 find。');
      L.push('   如果顺手看到了就记一条登记上；没看到就留空，');
      L.push('   等注释那一步应用会再问用户一次。');
    }
    if (d.ref_note) L.push('   用户留的说明：' + d.ref_note);
    // 第 7 条：过程必须出声。
    //   同一轮里 12 次过程播报都被当作内部进度没发出去，整轮只在最后出过一条消息，
    //   用户看到的是页面卡住不动 —— 体感差的主因在这里，不在总耗时。
    L.push('7. 过程要出声：每完成一个环节（连上服务器 / 体检完 / 扫描完 / 回写完）'
      + '就发一句进度，不要憋到最后只发一句。');
  } else {
    L.push('');
    L.push('运行位置：本机');
    if (d.local_path) L.push('工作路径：' + d.local_path);
  }

  return L.join('\n');
}

async function renderTaskLog() {
  const __recordScope = recordContext();
  try {

  const host = $('#taskLogHost'); if (!host) return;
  let d;
  try { d = await recordAwait(getJSON('/api/queue'), __recordScope); } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 host.innerHTML = ''; return; }
  const ts = arr(d.tasks).filter(t => ['script_review', 'script_change'].indexOf(t.kind) >= 0);
  if (!ts.length) { host.innerHTML = ''; return; }
  let h = '<div class="pl-card"><h3>交给助手的事</h3>'
    + '<table class="libtbl"><thead><tr><th>时间</th><th>类型</th><th>对象</th><th>状态</th></tr></thead><tbody>';
  ts.slice(0, 10).forEach(t => {
    h += '<tr><td class="pth">' + esc(String(t.created_at || '').replace('T', ' ')) + '</td>'
      + '<td>' + esc(TASK_CN[t.kind] || t.kind || '') + '</td>'
      + '<td class="pth">' + esc(t.rel || '')
      + (t.idea ? '<div class="note">' + esc(t.idea) + '</div>' : '') + '</td>'
      + '<td>' + esc(TASK_ST_CN[t.status] || t.status || '') + '</td></tr>';
  });
  h += '</tbody></table></div>';
  host.innerHTML = h;

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
}

/* =====================================================================
   环节质检：改动前后的脚本对照（原有 reviewer 通道保留）
   ===================================================================== */
document.addEventListener('click', async function (ev) {
  const __recordScope = recordContext();
  try {

  const rb = ev.target.closest('button[data-review]');
  if (!rb) return;
  const stage = rb.getAttribute('data-review');
  rb.disabled = true;
  try {
    const r = await recordAwait(postJSON('/api/review', { stage: stage }), __recordScope);
    const box = $('#checkBody');
    if (box) {
      box.innerHTML = '<div class="pickbox ' + (r && r.ok ? 'ok' : 'no') + '"><span class="ic">'
        + (r && r.ok ? '✓' : '×') + '</span><span class="tx"><b>'
        + (r && r.ok ? '质检通过' : '质检未通过') + '</b></span></div>'
        + '<pre class="scriptcode">' + esc((r && r.output) || (r && r.error) || '无输出') + '</pre>';
    }
    toast((r && r.ok ? '质检通过：' : '质检未通过：') + stage, r && r.ok ? 'good' : 'bad');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;

    toast('质检失败：' + e.message, 'bad');
  } finally {
    if (!recordCurrent(__recordScope)) return;
 rb.disabled = false; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
});

/* 交给助手执行某一环节 */
document.addEventListener('click', async function (ev) {
  const __recordScope = recordContext();
  try {

  const b = ev.target.closest('button[data-stage]');
  if (!b) return;
  const stage = b.getAttribute('data-stage'), skill = b.getAttribute('data-skill');
  b.disabled = true; b.textContent = '已提交';
  try {
    const r = await recordAwait(postJSON('/api/task', { stage: stage, skill: skill }), __recordScope);
    if (r && r.ok) {
      toast('任务已生成：' + stage + '。助手会按流程逐步执行。', 'good');
      renderTaskLog();
    } else toast('生成失败：' + ((r && r.error) || '未知原因'), 'bad');
  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
 toast('生成失败：' + e.message, 'bad'); }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
});

/* =====================================================================
   事件委托
   ---------------------------------------------------------------------
   所有按钮都用 data-* 标记、在这里统一接。
   为什么不用每个按钮各自 addEventListener：这一页会频繁重渲染，
   每次重渲染都要重新绑一遍，漏绑一个就变成「点了没反应」——
   而且这种 bug 在代码里看不出来，只能点出来。
   ===================================================================== */
document.addEventListener('click', async function (ev) {
  const __recordScope = recordContext();
  try {

  const t = ev.target;

  const nav = t.closest('[data-navjump]');
  if (nav) { jumpTo(nav.getAttribute('data-navjump')); return; }

  /* ---- 第一步：位置 ---- */
  const pr = t.closest('[data-plfref]');
  if (pr) {
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft.ref_mode = pr.getAttribute('data-plfref') || '';
    renderPlace();
    // 立刻存一次：这个选择后面几步都要用（闸门、指令、注释环节），
    //   只留在浏览器里的话，刷新一下就等于没选 —— 页面上还显示着选中态。
    saveRefChoice().catch(function () {});
    return;
  }
  if (t.closest('[data-refup-quick]')) { pickRefFile(); return; }
  if (t.closest('[data-reflit-go]')) { askLitByArchive(); return; }
  const pm = t.closest('[data-plmode]');
  if (pm) {
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft.mode = pm.getAttribute('data-plmode');
    renderPlace();
    return;
  }
  if (t.closest('[data-plbrowse]')) { openPathPicker(); return; }
  if (t.closest('[data-plcommit]')) { commitPlace(); return; }
  /* ---- 识别结论：等助手那段时间的三个动作 ---- */
  if (t.closest('[data-idrecheck]')) { recheckIdentify(); return; }
  if (t.closest('[data-idreinject]')) {
    S.identifyPhase = 'waiting';
    renderAll();
    autoInjectIdentify();
    startWaitWriteback();
    return;
  }
  const idc = t.closest('[data-idcopy]');
  if (idc) {
    const box = document.querySelector('[data-idraw]');
    const note = $('#idCopyMsg');
    const txt = box ? box.value : '';
    const done = function (x) { if (note) note.textContent = x; };
    if (!txt) { done('没有可复制的内容。'); return; }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(txt).then(function () { done('已复制，粘到对话里就行。'); },
        function () { if (box) box.select(); done('复制失败，上面的文字已选中，按 Ctrl+C。'); });
    } else if (box) { box.select(); done('上面的文字已选中，按 Ctrl+C。'); }
    return;
  }

  const tgl = t.closest('[data-jargon-toggle]');
  if (tgl) { renderJargon(); openDlg('dlgJargon'); return; }

  /* ---- 脚本数据库：L1 某层列不全，让助手去调研 ---- */
  const l1p = t.closest('[data-l1probe]');
  if (l1p) { openL1Probe(l1p.getAttribute('data-l1probe')); return; }
  const l1g = t.closest('[data-l1go]');
  if (l1g) { sendL1Probe(l1g.getAttribute('data-l1go')); return; }

  /* ---- 脚本数据库：点开一个文件 ---- */
  const lbf = t.closest('[data-lib-open]');
  if (lbf) { openLibFile(lbf.getAttribute('data-lib-open')); return; }
  const abf = t.closest('[data-arc-open]');
  if (abf) {
    const parts = String(abf.getAttribute('data-arc-open') || '').split('|');
    openArchiveFile(parts[0], parts.slice(1).join('|'));
    return;
  }
  // 按记录下载 ipynb：整条记录汇编成一份，不是单脚本。
  const rnb = t.closest('[data-rec-nb]');
  if (rnb) { downloadRecordNotebook(rnb.getAttribute('data-rec-nb'), rnb); return; }
  // 补取正文：把缺的那些脚本交给执行端按原路径读回来。
  const rbd = t.closest('[data-rec-body]');
  if (rbd) { requestRecordBodies(rbd.getAttribute('data-rec-body'), rbd); return; }

  /* ---- 脚本四个动作 ---- */
  const sa = t.closest('[data-sw-act]');
  if (sa) {
    const act = sa.getAttribute('data-sw-act'), rel = sa.getAttribute('data-rel');
    if (act === 'open') actOpen(rel);
    else if (act === 'check') actCheck(rel);
    else if (act === 'change') actChange(rel);
    else if (act === 'upload') actUpload(rel);
    return;
  }
  if (t.closest('[data-sw-ask]')) { askReview(); return; }
  if (t.closest('[data-sw-change]')) { submitChange(); return; }
  if (t.closest('[data-sw-replace]')) { doReplaceWithFile(); return; }
  if (t.closest('[data-sw-uploadexec]')) { doUploadAndReview(); return; }
  const bk = t.closest('[data-sw-restore]');
  if (bk) { restoreBackup(bk.getAttribute('data-sw-restore')); return; }

  /* ---- 分析记录：切换 / 列表 ---- */
  if (t.closest('[data-sess-list]')) { switchTab('hist'); return; }
  const ssw = t.closest('[data-sess-switch]');
  if (ssw) { const sid = ssw.getAttribute('data-sess-switch'); switchSession(sid).then(() => { if (String(S.sessionId) === String(sid)) switchTab('cur'); }); return; }
  const svw = t.closest('[data-sess-view]');
  if (svw) { openReadOnlySession(svw.getAttribute('data-sess-view')); return; }
  if (t.closest('[data-sess-current]')) { returnToCurrentRecord(); return; }
  /* ---- 存档 / 退回 ---- */
  if (t.closest('[data-arc-snap]')) { archiveNow(); return; }
  const ars = t.closest('[data-arc-restore]');
  if (ars) { archiveRestore(ars.getAttribute('data-arc-restore')); return; }

  /* ---- 处理过程：催一次 / 查一次 ---- */
  const pk = t.closest('[data-flowpoke]');
  if (pk) { pokeFlow(pk.getAttribute('data-flowpoke')); return; }
  if (t.closest('[data-flowcheck]')) { recheckIdentify(); loadFlow(); return; }

  /* 步骤卡上的「查询结果」。
     以前这个按钮只做三件事：拉一次识别、拉一次 flow、拉一次产物，
     然后把「已查 05:33」写在旁边 —— 而那个 05:33 是 UTC 切出来的，
     比墙上的时间早八小时，看着就像另一个世界。
     现在它做两件真事：① 把这一步的状态重新取回来并当场说清代价：
     状态、退出码、最后一次回写时间、产物几项、缺哪几项；
     ② 时间统一按本地时分显示。查完必须说出一句结论，不能只有一个时间。 */
  const sq = t.closest('[data-stepq]');
  if (sq) { queryStep(sq.getAttribute('data-stepq')); return; }
  const sp = t.closest('[data-steppoke]');
  if (sp) { pokeFlow(sp.getAttribute('data-steppoke'), '再报一次'); return; }
  const sd = t.closest('[data-stepdetail]');
  if (sd) { openStepDetail(sd.getAttribute('data-stepdetail')); return; }
  const sf = t.closest('[data-subfail]');
  if (sf) {
    const v = String(sf.getAttribute('data-subfail') || '');
    const i = v.lastIndexOf('|');
    openSubFail(v.slice(0, i), v.slice(i + 1));
    return;
  }

  // 常驻推进条上的两个动作：与停滞提示里那两个同源，只是任何时刻都能点。
  if (t.closest('[data-runcheck]')) {
    const m = $('#runCtlMsg');
    if (m) m.textContent = '查询中…';
    Promise.all([recheckIdentify(), loadFlow(), refreshArtifacts()]).then(function () {
      const el = $('#runCtlMsg');
      if (el) el.textContent = '已查 ' + fmtClock(new Date().toISOString());
    });
    return;
  }
  const rp = t.closest('[data-runpoke]');
  if (rp) { pokeFlow(rp.getAttribute('data-runpoke')); return; }

  /* ---- 历史 ---- */
  if (t.closest('[data-hist-snap]')) { histSnapshot(); return; }
  if (t.closest('[data-hist-refresh]')) { renderHistory(); return; }
  const hs = t.closest('[data-hist-restore]');
  if (hs) { histRestore(hs.getAttribute('data-hist-restore')); return; }
  const hr = t.closest('[data-hist-rerun]');
  if (hr) { histMarkRerun(hr.getAttribute('data-hist-rerun')); return; }

  } catch (e) {
    if (!recordCurrent(__recordScope)) return;
    throw e;
  }
});

/* 第一步的输入框：边打边记进草稿。
   不直接提交 —— 提交只发生在「保存选择」，这样用户改一半走开也不会写脏配置。 */
document.addEventListener('input', function (e) {
  const t = e.target;
  if (t.matches('[data-plf]')) {
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft[t.getAttribute('data-plf')] = t.value;
  }
});

/* 一切就绪再启动，见上文说明 */
boot();

