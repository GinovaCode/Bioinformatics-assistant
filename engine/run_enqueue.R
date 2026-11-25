run_enqueue <- function(con = NULL, req = NULL, q = list(), body = list(), owner_ctx,
                         current_sid = "", via = "/api/run",
                         req_id_override = "", step_override = NULL,
                         allow_local_launch = TRUE, agent_dispatch = FALSE,
                         chain_dispatch = NULL) {
  send_json <- function(.con, code, obj) list(pa_status = as.integer(code), pa_body = obj)
  # 链派工的来源标记：只认 req 上的内部字段，不认 body、不认环境变量。
  #   为什么改到 req 上（2026-11-16 修）：原来用 get0("RC_DISPATCH") 从环境里找，
  #   rc_dispatch 把标记写在它自建的临时环境里，而 run_enqueue 的闭包挂在
  #   serve.R 顶层环境，两层之间看不见 —— get0 恒定返回 FALSE。
  #   于是链自己派第一步就被自己的「链进行中」守卫 409 掉，链写成 blocked，
  #   而 blocked 又在拦截名单里，页面每次「再报一次」都撞同一句话，永不自愈。
  #   走 req 而不是 body：req 由 HTTP 层构造，客户端无法塞字段；
  #   body 是外部可控的，任何"放行"标记都不能从那里来。
  if (is.null(chain_dispatch)) chain_dispatch <- isTRUE(req$pa_chain_dispatch)
  st <- read_state()
  runDir <- NULL
  V <- list(run_dir = "")
  b <- if (is.list(body)) body else list()
  if (!is.null(step_override) && nzchar(trimws(as.character(step_override))))
    b$step <- trimws(as.character(step_override))
  if (nzchar(trimws(as.character(req_id_override %||% ""))))
    b$req_id <- trimws(as.character(req_id_override))
  if (isTRUE(agent_dispatch)) {
    b$force <- FALSE
    if (isTRUE(body$force)) return(send_json(con, 403, list(
      ok = FALSE, code = "force_requires_human", step = as.character(body$step %||% ""),
      error = "force 只接受人的身份。agent 不能给自己放行。")))
  }
  target <- record_session_target(q, b, path = via, current_sid = current_sid,
                                  allow_current = FALSE, require = TRUE)
  if (!isTRUE(target$ok)) {
    refused <- record_refusal(target)
    return(send_json(con, refused$status, refused$body))
  }
  access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "write",
                          require_ctx = TRUE, require_run_dir = TRUE)
  if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
  effective_sid <- target$sid
  b$session_id <- effective_sid
  runDir <- access$ctx$run_dir
  V$run_dir <- runDir
  st$runDir <- runDir
  st$runDirRemote <- access$ctx$run_dir_remote
  stepId <- as.character(b$step %||% "")
  if (isTRUE(agent_dispatch) && nzchar(stepId) &&
      !stepId %in% c("all", "config")) {
    valid_steps <- tryCatch(flow_run_order(effective_sid), error = function(e) character(0))
    if (!length(valid_steps) && isTRUE(HAS_FLOW))
      valid_steps <- tryCatch(FLOW_ORDER(flow_profile(effective_sid)),
                              error = function(e) character(0))
    if (length(valid_steps) && !stepId %in% valid_steps)
      return(send_json(con, 409, list(ok = FALSE, code = "invalid_step",
        step = stepId, error = "目标记录没有这个可执行步骤，未派活。")))
  }
  if (isTRUE(agent_dispatch) && !isTRUE(access$ctx$run_dir_remote)) {
    return(send_json(con, 409, list(ok = FALSE, code = "dispatch_requires_server_record",
      step = stepId, error = "agent 派活只进入服务器记录的执行队列；本地记录不能由执行端提交。")))
  }
  # A repeated agent request is answered from the durable queue key before any
  # mutable gate is re-read. The record ownership check above still runs first.
  rq_override <- trimws(as.character(req_id_override %||% b$.pa_req_id %||% ""))
  if (nzchar(rq_override) && nzchar(stepId)) {
    old <- exec_by_req(stepId, effective_sid, rq_override)
    if (!is.null(old)) {
      tryCatch(attempt_register(APP_DIR, effective_sid, rq_override, origin = "step_run"),
               error = function(e) NULL)
      tryCatch(sess_set_pending(APP_DIR, effective_sid, rq_override, slot = "step"),
               error = function(e) NULL)
      return(send_json(con, 200, list(
        ok = TRUE, delegated = TRUE, duplicate_dispatch = TRUE, step = stepId,
        step_id = stepId, title = (STEP_TASK[[stepId]] %||% list(title = stepId))$title,
        req_id = as.character(old$req_id %||% rq_override),
        request_id = as.character(old$req_id %||% rq_override),
        exec_id = as.character(old$id %||% ""),
        exec_status = as.character(old$status %||% ""),
        exec_job = as.character(old$job_id %||% ""),
        run_dir = as.character(old$run_dir %||% ""),
        reference_id = as.character(old$reference_id %||% ""),
        ref_mode = as.character(old$ref_mode %||% ""),
        ref_path = as.character(old$ref_path %||% ""),
        exec_queued = TRUE,
        msg = "同一 req_id 已经派过，返回原队列任务，没有重复入队。")))
    }
  }
  if (isTRUE(HAS_SESSIONS) && exists("intent_execution_gate", mode = "function")) {
    intent_gate <- tryCatch(intent_execution_gate(APP_DIR, effective_sid, requested_step = stepId),
      error = function(e) list(ok = FALSE, code = "intent_profile_unreadable",
        error = "分析限制无法完整读取，未启动执行。"))
    if (!isTRUE(intent_gate$ok)) return(send_json(con, 409, intent_gate))
  }

    # ---- 对账不一致：只拦被涉及的那几步 ------------------------------
    #   为什么在这里拦（2026-11-03，审核第 12 条）
    #     对账器一直只做一件事：在页面上画一个红框。
    #     没有任何闸门读它的 ok，于是「1 处不一致」可以挂一个多月，
    #     十二个阶段照跑 —— 被看见和被拦住，是两件事。
    #   为什么只拦涉及的那几步，不做全局闸门：
    #     现在就有两条不过（STEP_TASK 缺项、服务器快照未上报）。
    #     做成全局硬闸门，等于整个应用一步都点不动 —— 那不是修复，是停机。
    #   为什么带 force 放行：
    #     对账是静态事实核对，它可能滞后于实际能力（比如快照刚上报完、
    #     页面还没刷新）。留一条只给人用的放行路，理由必须写。
    if (HAS_CONTRACT && nzchar(stepId) && !identical(stepId, "all")
        && !identical(stepId, "config") && !isTRUE(b$force)) {
      gchk <- tryCatch(
        contract_check(ENGINE_DIR, flow_profile(effective_sid),
                       step_task_keys = names(STEP_TASK)),
        error = function(e) NULL)
      if (!is.null(gchk) && length(gchk$issues)) {
        hit <- Filter(function(x) {
          identical(as.character(x$level), "fail") &&
            as.character(stepId) %in% unlist(x$stages %||% list())
        }, gchk$issues)
        if (length(hit)) {
          return(send_json(con, 409, list(
            ok = FALSE, code = "contract_mismatch", step = stepId,
            error = paste0("这一环的定义对不上，先修好再跑：",
                           paste(vapply(hit, function(x) as.character(x$what %||% ""), ""),
                                 collapse = "；")),
            where = as.list(vapply(hit, function(x) as.character(x$where %||% ""), "")),
            fix = as.list(vapply(hit, function(x) as.character(x$fix %||% ""), "")),
            hint = "修完刷新页面再点。确实要现在跑，请在页面上以人的身份强制放行并写理由。")))
        }
      }
    }

    # ---- 闸门：上一步不是 verified，直接 409（L3 第 2 步）-----------
    #   这是「让 agent 没有绕过去的路」的那一步。
    #   以前闸门只在 qc 不是 pass 时拼一句 reviewWarn，然后照常派作业，
    #   force=true 还能直接越过 —— 那不是闸门，是提示。
    #   现在：未通过判定就返回 409 + 返工单号，force 只接受人身份并需理由。
    if (DB_READY && nzchar(stepId) && !identical(stepId, "all")) {
      sid_g <- effective_sid
      vinfo <- tryCatch(db_api_version_of(DB_CON, sid_g), error = function(e) NULL)
      if (!is.null(vinfo) && nzchar(vinfo$version_id %||% "") &&
          !is.na(vinfo$version_id %||% NA)) {
        if (isTRUE(b$force)) {
          who <- as.character(b$force_by %||% b$who %||% "")
          why <- as.character(b$force_reason %||% "")
          # agent 不能给自己放行：这条必须可被测试触发（I5 第 4 条）。
          if (!nzchar(who) || grepl("^(agent|assistant|bot|claude|gpt|model)", tolower(who)))
            return(send_json(con, 403, list(
              ok = FALSE, code = "force_requires_human", step = stepId,
              error = "force 只接受人的身份。agent 不能给自己放行。",
              hint = "放行请在页面上填写理由，或调用 /api/review/release 并给 who / why。")))
          if (!nzchar(why))
            return(send_json(con, 400, list(
              ok = FALSE, code = "force_reason_required", step = stepId,
              error = "强制放行必须填理由，理由是交付清单的一部分。")))
          tryCatch({
            prev <- stepId
            db_event(DB_CON, "review_override", owner_key = owner_ctx$key %||% NA,
                     version_id = vinfo$version_id, step_id = stepId,
                     payload = list(who = who, why = why, at = db_now(),
                                    path = paste0(via, " force")))
          }, error = function(e) NULL)
        } else {
          g <- tryCatch(db_api_gate(DB_CON, APP_DIR, owner_ctx, vinfo$version_id, stepId),
                        error = function(e) NULL)
          # 闸门返回非 NULL 不等于"必须拦"。
          #   踩过的坑（2026-11-06）：这里原来写的是
          #     if (!is.null(g)) return(send_json(con, g$code, g$body))
          #   也就是"闸门有话说就 409 返回"。可是 db_api_gate 的 warn 档
          #   （默认档）返回的正是 ok=TRUE + warn_only=TRUE —— 它的含义是
          #   "记一条未核验项，放行"。被这一行统一当成失败拦掉之后，
          #   warn 档实际等于 block 档：点击 → 409 → 从未入队 → 执行端永远
          #   取不到任务。页面看起来像"点了没反应"，而代码里三档齐全。
          #   正确判据是闸门自己的 ok 字段：ok=FALSE 才拦，ok=TRUE 带着警告继续。
          if (!is.null(g)) {
            if (isFALSE(g$body$ok)) return(send_json(con, g$code, g$body))
            # 放行但要留痕：这条警告要进交付清单，不能悄悄丢掉。
            tryCatch(db_event(DB_CON, "gate_warn_passed",
                              owner_key = owner_ctx$key %||% NA,
                              version_id = vinfo$version_id, step_id = stepId,
                              payload = list(code = g$body$code %||% "",
                                             upstream = g$body$upstream %||% "",
                                             upstream_status = g$body$upstream_status %||% "",
                                             at = db_now())),
                     error = function(e) NULL)
          }
        }
      }
    }

    # 服务器模式的运行目录在服务器上，本机起不了进程。
    #   但也不能只回一句"请到助手那边跑"就完了 —— 那等于把按钮做成死路，
    #   用户点下去看到的就是一条报错。现在改成：这一步照样能点，
    #   点了就把这步变成一条任务交给助手，并记成「已交给助手，等回写」。
    #   状态不再是空的：页面能显示"这一步交给谁了、在等什么"。
    if (isTRUE(access$ctx$run_dir_remote) || isTRUE(agent_dispatch)) {
      if (identical(stepId, "all")) {
        out <- rc_begin(effective_sid, runDir, as.character(b$req_id %||% ""), owner_ctx,
                        overwrite = isTRUE(b$overwrite))
        return(send_json(con, out$code, out$body))
      }
      active_chain <- rc_read(effective_sid)
      # 链自己派工（chain_dispatch=TRUE）时不再问链状态：这一步是协调器
      #   在回调之后决定的，它比这里的任何判断都更清楚该不该跑。
      #   blocked 不拦单步：它是上一次派工没派出去留下的痕迹，不是"在跑"；
      #   拿它拦下面的点击，等于把一次失败放大成永久阻塞（2026-11-16 实测）。
      refuse <- rc_step_guard(active_chain, chain_dispatch)
      if (!is.null(refuse)) return(send_json(con, refuse$code, refuse$body))
      # ---- 决策先于执行：这一步要人定的事，现在就生成并发出 ----
      #   位置很关键：必须在入队之前。
      #   以前的问题不是"入队太晚"，而是根本没有"这里该问人"这个动作 ——
      #   决策清单是初始化时抄进 run-dir 的九条，服务器模式下读不到，
      #   于是页面整块消失、也没人被告知，脚本就按默认值一路跑下去了。
      #   现在：走到这一步 → 用这一步当下的真实数据生成问题 → 发进会话
      #   → 没答完就不放行。答完再点，才入队。
      # 专业决定闸门：filter / final / all 不允许被 force 或
      # 「决策模块不可用」绕过。判断本身抽在 engine/decision_gate.R，
      # 这样能拿真实参数逐条验证，而不是靠读代码相信它。
      if (exists("dec_gate_block")) {
        gb <- dec_gate_block(stepId, HAS_DECISIONS, b$force)
        if (!is.null(gb))
          return(send_json(con, 409, c(list(ok = FALSE), gb)))
      }

      # 结构参数先置空，位置必须在所有分支之前。
      #   为什么：这段原来在决策块内部，force 或没开决策的时候根本不会执行，
      #   后面入队却仍会用到这两个变量 —— 要么报未定义，要么沿用到上一轮的值。
      #   专业参数校验不能因为"强制放行"就被绕过。
      execParams <- NULL
      execParamsRel <- ""

      if (isTRUE(HAS_DECISIONS) && !identical(as.character(stepId), "all")
          && !isTRUE(b$force)) {
        decSid <- as.character(b$session_id %||% "")
        if (!nzchar(decSid)) decSid <- effective_sid
        decision_error <- NULL
        tryCatch(dec_ensure(stepId, decSid), error = function(e) decision_error <<- conditionMessage(e))
        pend <- tryCatch(dec_pending(decSid), error = function(e) { decision_error <<- conditionMessage(e); list() })
        if (!is.null(decision_error)) return(send_json(con, 409,
          list(ok = FALSE, code = "decision_unreadable", error = decision_error)))
        # 只认"这一步自己的"待答问题：别的问题属于别的阶段，不该卡这一下。
        mine <- Filter(function(x) identical(as.character(x$step %||% ""), as.character(stepId)), pend)
        if (length(mine)) {
          txt <- dec_ask_text(mine, stepTitle = if (HAS_FLOW) {
            x <- flow_stage(stepId, flow_profile(effective_sid))
            if (is.null(x)) stepId else as.character(x$title)
          } else stepId)
          qd <- tryCatch(inject_enqueue(text = txt, session = decSid,
                                        reason = "decision", step = stepId,
                                        # 按钮名必须随入队一起存下去。
                                        #   为什么（2026-11-23，云准助手扫全量入队点指出）：
                                        #   九个入队点里这处最容易漏 —— 它只在派工被
                                        #   「先定一件事」拦住时才触发，平时走不到。
                                        #   漏了 origin，这条注入补发时就没有标题，
                                        #   而 decision 类正是人要读的那一类。
                                        origin = "需要你定一件事"),
                         error = function(e) NULL)
          return(send_json(con, 409, list(
            ok = FALSE, code = "need_decision", step = stepId,
            error = "这一步要你先定一件事，定完才能跑",
            pending = length(mine),
            queued = !is.null(qd),
            items = mine)))
        }
        # 答完了：把答案带进这次的指令里，别让助手自己去猜一个值。
        ans <- Filter(function(x) identical(as.character(x$step %||% ""), as.character(stepId)),
                      tryCatch(dec_read(decSid)$items, error = function(e) list()))
        ans <- Filter(function(x) identical(as.character(x$status %||% ""), "answered"), ans)
        if (length(ans)) {
          kv <- vapply(ans, function(x) {
            d <- x$answer_detail
            if (!is.null(d) && length(d) && nzchar(as.character(paste(unlist(d), collapse = ""))))
              paste0(as.character(x$title %||% x$key), " = ",
                     paste(as.character(unlist(d)), collapse = "、"))
            else paste0(as.character(x$title %||% x$key), " = ", as.character(x$answer %||% ""))
          }, "")
          b$gate_note <- paste(c(as.character(b$gate_note %||% ""),
                                 paste0("【人已定】", paste(kv, collapse = "；"))), collapse = " ")
        }

        # 筛群这一步的特殊处理：脚本已改成"只认参数文件"，不会再自己猜清单。
        #   所以必须把"写入哪个文件、写成什么"一起交给执行端，
        #   否则脚本会按设计停下报错（这是刻意的：宁可停下，也不照抄上一轮的号）。
        if (identical(as.character(stepId), "filter")) {
          cf <- Filter(function(x) identical(as.character(x$key %||% ""), "cluster_filter"), ans)
          if (length(cf) != 1L) {
            return(send_json(con, 409, list(ok = FALSE,
              code = "missing_cluster_decision", step = stepId,
              error = "本次需要一条已确认的筛群决定；尚无决定或存在冲突，未入队。")))
          }
          if (length(cf)) {
            rawAnswer <- cf[[1]]$answer
            if (!is.atomic(rawAnswer) || length(rawAnswer) != 1L ||
                is.na(rawAnswer) || !nzchar(trimws(as.character(rawAnswer)))) {
              return(send_json(con, 409, list(ok = FALSE,
                code = "missing_cluster_decision", step = stepId,
                error = "筛群答案为空，未入队；请明确选择全部保留或指定簇号。")))
            }
            ids <- trimws(as.character(rawAnswer))
            det <- trimws(paste(as.character(unlist(cf[[1]]$answer_detail)), collapse = ","))
            # 只有明确 keep_all 能产生空清单。说明文字不覆盖结构答案。
            cluNow <- tryCatch((dec_clusters(decSid) %||% list())$clusters,
                               error = function(e) NULL)
            known <- unique(as.character(unlist(lapply(cluNow %||% list(),
                            function(x) x$cluster %||% character(0)))))
            known <- known[!is.na(known) & nzchar(known)]
            badIds <- character(0)
            if (identical(ids, "keep_all")) {
              vec <- character(0)
            } else {
              selectedText <- if (identical(ids, "custom")) det else ids
              vec <- unique(trimws(unlist(strsplit(selectedText, "[,，[:space:]]+"))))
              vec <- vec[!is.na(vec) & nzchar(vec)]
              if (!length(vec)) {
                return(send_json(con, 409, list(ok = FALSE,
                  code = "empty_custom_clusters", step = stepId,
                  error = "自定义簇号为空，未入队；如需全部保留，请明确选择全部保留。")))
              }
              if (!length(known)) {
                return(send_json(con, 409, list(ok = FALSE,
                  code = "cluster_evidence_missing", step = stepId,
                  error = "缺少本次分析的真实簇统计，无法验证簇号，未入队。")))
              }
              badIds <- setdiff(vec, known)
              if (length(badIds)) {
                return(send_json(con, 409, list(ok = FALSE,
                  code = "invalid_cluster_ids", step = stepId,
                  error = paste0("这些簇号不属于本次合并对象：", paste(badIds, collapse = "、")),
                  known = as.list(known))))
              }
            }
            # ---- 把 dims / resolution 一起写进参数文件 ----------------
            #   为什么必须写进去（2026-11-03，审核第 9 条）
            #     前端有完整的参数面板，值存进了 variants_<sid>.json 的
            #     params 里；但筛群脚本 drop_and_recluster.R 里
            #     FindNeighbors(dims = 1:20) 与 FindClusters(resolution = 0.6)
            #     是写死的，从不读这两个值。
            #     于是「选了参数」和「参数生效」之间隔着一层看不见的假象：
            #     人以为换 0.9 就换成分群口径，实际还是 0.6。
            #   做法分两步，都不假装：
            #     ① 把所选值写进 drop_params.json（脚本一旦升级就能读到，
            #        也让这一轮到底想要什么有据可查）；
            #     ② 所选值与脚本写死的值不一致时，在指令里明说"这一版不生效"。
            #   只做①不做②，等于把人蒙在鼓里；只做②不做①，等于放弃这条线。
            dv <- ""
            rv <- ""
            tryCatch({
              # 用本路由已经验证过的 decSid 读参数候选。
              #   原来写的是 gateSid —— 它在这个作用域还没定义（后面才赋值），
              #   于是文件名变成 variants_.json、读不到，dv/rv 一路为空，
              #   最后派工里只剩 drop_clusters，dims/resolution 全丢。
              vf <- file.path(STATE_DIR, paste0("variants_", as.character(decSid %||% ""), ".json"))
              if (file.exists(vf)) {
                vd <- jsonlite::fromJSON(read_text_any(vf), simplifyVector = FALSE)
                fst <- (vd$stages %||% list())$filter %||% list()
                chosen <- as.character(fst$chosen %||% "")
                its <- fst$items %||% list()
                hit <- Filter(function(x) identical(as.character(x$id %||% ""), chosen), its)
                if (length(hit)) {
                  pv <- hit[[1]]$params %||% list()
                  dv <- as.character(pv$dims %||% "")
                  rv <- as.character(pv$resolution %||% "")
                }
              }
            }, error = function(e) NULL)

            # 缺参数就拒绝入队，不静默走默认。
            #   实测过一次：候选没读到 → 派工里只有 drop_clusters，
            #   脚本按默认跑，页面却以为按人选的在跑。宁可不派工。
            if (!nzchar(dv) || !nzchar(rv)) {
              return(send_json(con, 409, list(ok = FALSE,
                code = "missing_recluster_params", step = stepId,
                error = "这一步需要明确的 dims 与 resolution；当前没有读到已选中参数集，未入队。",
                hint = "请在参数面板里选定一套（例如 1:20 / 0.6）后重试。")))
            }

            jsl <- paste0(
              '{"drop_clusters":[',
              if (length(vec)) paste0('"', paste(vec, collapse = '","'), '"') else "",
              ']',
              if (nzchar(dv)) paste0(',"dims":"', dv, '"') else "",
              if (nzchar(rv)) paste0(',"resolution":"', rv, '"') else "",
              '}')

            # 脚本现在真的从参数文件读 dims / resolution（04_filter.R 的
            #   parse_dims / parse_res），所以这里不能再声称「写死、不生效」。
            #   改前那两段是旧版 drop_and_recluster.R 的遗留说明，会误报。
            #   现在只回显：选了什么、值来自哪里、最终由脚本写进
            #   recluster_params_used.xls 供复盘。没选就走脚本默认值，也要说清。
            DEF_DIMS_NOTE <- "1:20"
            DEF_RES_NOTE  <- "0.6"
            paramWarn <- ""
            if (!nzchar(dv) || !nzchar(rv)) {
              paramWarn <- paste0("【参数来源】未在页面选定 ",
                                  if (!nzchar(dv)) "dims" else "",
                                  if (!nzchar(dv) && !nzchar(rv)) " 与 " else "",
                                  if (!nzchar(rv)) "resolution" else "",
                                  "，脚本将使用默认值（dims ",
                                  DEF_DIMS_NOTE, "、resolution ", DEF_RES_NOTE,
                                  "）；实际生效值以 7.after_drop/recluster_params_used.xls 为准。")
            } else {
              paramWarn <- paste0("【参数来源】dims = ", dv, "、resolution = ", rv,
                                  " 已写入参数文件，由筛群脚本读取生效；",
                                  "实际生效值以 7.after_drop/recluster_params_used.xls 为准。")
            }
            # 结构参数直接挂在任务上，由执行端在 qsub 之前写成文件。
            #   改前只是把 JSON 塞进给人看的 gate_note，等于指望助手
            #   去替执行端写文件；实际做执行的是 worker，它拿不到这段文字。
            execParams <- list(drop_clusters = as.list(vec))
            if (nzchar(dv)) execParams$dims <- dv
            if (nzchar(rv)) execParams$resolution <- rv
            execParamsRel <- "7.after_drop/drop_params.json"
            # 非法簇号：不放行，直接报错让人重答。
            #   放行会把不认识的值写进参数文件，脚本按号删簇时删不到任何细胞，
            #   结果看起来"跑成功了"，其实筛群没生效。
            if (length(badIds)) {
              return(send_json(con, 409, list(
                ok = FALSE, code = "invalid_cluster_ids",
                step = stepId,
                error = paste0("这些簇号不属于本次合并对象：",
                               paste(badIds, collapse = "、")),
                known = as.list(known),
                hint = "请按页面列出的真实簇号重填，或改选「全部保留」。未入队、未下发。")))
            }
            b$gate_note <- paste(c(as.character(b$gate_note %||% ""),
                                   paste0("【参数文件由执行端生成】执行端会在提交作业前，",
                                          "把 7.after_drop/drop_params.json 写成 内容为 ",
                                          jsl,
                                          "。筛群脚本只从该文件读剔除清单，文件不存在会直接停下报错。"),
                                   "【不许写死清单】不要改脚本里的清单，也不要照抄上一轮的簇号。",
                                   if (nzchar(paramWarn)) paramWarn else NULL), collapse = " ")
          }
        }

        # ---- final：终版样本决定与差异守卫，必须在入队前问清 ----
        #   为什么要在这里拦：final_run.R 现在缺参数就停，
        #   但真正的问题是「页面上的选择根本没传下去」。
        #   所以先在入队前把两份决定要齐，缺一份就 409，
        #   不生成任务、不写 pending、不派工。
        if (identical(as.character(stepId), "final")) {
          decAll <- tryCatch(dec_read(decSid)$items, error = function(e) list())
          # 只认「这一次・这一步」的决定：key、step、session 三者都对上。
          #   只按 key 过滤的话，别的会话或别的步骤里同名 key 的答案
          #   也会被当成这一次的决定。
          pick <- function(key) {
            v <- Filter(function(x) {
              identical(as.character(x$key %||% ""), key) &&
                identical(as.character(x$step %||% ""), "final") &&
                identical(as.character(x$status %||% ""), "answered") &&
                (!nzchar(decSid) ||
                   is.null(x$session_id) ||
                   identical(as.character(x$session_id %||% ""), decSid))
            }, decAll)
            v
          }
          ds <- pick("dropsample")
          dg <- pick("diff_guard")
          if (length(ds) != 1L)
            return(send_json(con, 409, list(ok = FALSE,
              code = "missing_dropsample_decision", step = stepId,
              error = "终版样本决定尚未确认，或存在多条冲突记录，未入队。")))
          if (length(dg) != 1L)
            return(send_json(con, 409, list(ok = FALSE,
              code = "missing_diff_guard_decision", step = stepId,
              error = "差异守卫策略尚未确认，或存在多条冲突记录，未入队。")))

          # 标量校验与筛群一致：必须是长度 1、非 NA、非空白的原子值。
          #   不做这一步，向量答案或 NA 会被 as.character 变成 "NA"、
          #   多值会被拼成一串，最后当成一个不认识的样本名送进脚本。
          a0 <- ds[[1]]$answer
          if (!is.atomic(a0) || length(a0) != 1L || is.na(a0) ||
              !nzchar(trimws(as.character(a0))))
            return(send_json(con, 409, list(ok = FALSE,
              code = "empty_dropsample_answer", step = stepId,
              error = "终版样本答案为空或不是单个值，未入队。")))
          rawAns <- trimws(as.character(a0))
          keepAll <- rawAns %in% c("keep_all", "none", "keep-all")
          dropVec <- if (keepAll) character(0) else {
            v <- trimws(unlist(strsplit(rawAns, "[,，[:space:]]+")))
            unique(v[nzchar(v)])
          }
          if (!keepAll && !length(dropVec))
            return(send_json(con, 409, list(ok = FALSE,
              code = "empty_dropsample_answer", step = stepId,
              error = "既没说全部保留，也没有给出要剔的样本，未入队。")))

          # 要剔的样本必须真的出现在本次筛群后的对象里。
          #   不校验的后果：随便写个词也会被送进脚本，脚本按名字对不上就什么都不删，
          #   页面显示"已剔"，实际一个没动 —— 这正是最难发现的一类错。
          if (!keepAll) {
            knownSmp <- character(0)
            tryCatch({
              fst <- remote_step_get("filter", sid = decSid)
              ad <- (fst$evidence %||% list())$after_drop_stats
              if (is.list(ad) && length(ad$samples))
                knownSmp <- as.character(names(ad$samples))
            }, error = function(e) NULL)
            if (!length(knownSmp) && exists("dec_after_drop")) {
              knownSmp <- tryCatch({
                ad2 <- dec_after_drop(decSid)
                if (is.list(ad2) && length(ad2$samples)) as.character(names(ad2$samples))
                else character(0)
              }, error = function(e) character(0))
            }
            if (!length(knownSmp))
              return(send_json(con, 409, list(ok = FALSE,
                code = "missing_after_drop_samples", step = stepId,
                error = "拿不到本次筛群后的样本清单，无法确认要剔的样本，未入队。",
                hint = "先完成筛群并回写，再定终版样本。")))
            badSmp <- setdiff(dropVec, knownSmp)
            if (length(badSmp))
              return(send_json(con, 409, list(ok = FALSE,
                code = "invalid_drop_samples", step = stepId,
                error = paste0("这些样本不在本次筛群结果里：", paste(badSmp, collapse = "、")),
                known = as.list(knownSmp),
                hint = "请按真实样本名重填，或改选全部保留。未入队。")))
          }

          g0 <- dg[[1]]$answer
          if (!is.atomic(g0) || length(g0) != 1L || is.na(g0) ||
              !nzchar(trimws(as.character(g0))))
            return(send_json(con, 409, list(ok = FALSE,
              code = "invalid_diff_guard", step = stepId,
              error = "差异守卫答案为空或不是单个值，未入队。")))
          guard <- trimws(as.character(g0))
          if (!guard %in% c("skip", "stop", "merge"))
            return(send_json(con, 409, list(ok = FALSE,
              code = "invalid_diff_guard", step = stepId,
              error = paste0("差异守卫取值不合法：", guard, "（只接受 skip / stop / merge）"))))
          if (identical(guard, "merge"))
            return(send_json(con, 409, list(ok = FALSE,
              code = "merge_mapping_required", step = stepId,
              error = "merge 需要「把哪些簇并成一群」的映射，本步没有该输入，未入队。",
              hint = "请改选 skip 或 stop；要 merge 得先补映射参数。")))

          execParams <- list(keep_all_samples = keepAll,
                             drop_samples = as.list(dropVec),
                             diff_guard = guard)
          execParamsRel <- "8.final/final_params.json"
          b$gate_note <- paste(c(as.character(b$gate_note %||% ""),
            paste0("【参数文件由执行端生成】8.final/final_params.json 内容为 ",
                   "keep_all_samples=", keepAll,
                   "，drop_samples=", if (length(dropVec)) paste(dropVec, collapse = ",") else "（无）",
                   "，diff_guard=", guard,
                   "。脚本缺这个文件或字段会直接停下。"),
            "【只读已筛对象】终版消费 7.after_drop，不重新聚类、不套旧簇号。"),
            collapse = " ")
        }
      }

      # ---- 参考表闸门：这一步需要本次的注释依据，但还没选 ----
      #   为什么必须拦在这里：上一版没拦，人点一下就直接注入会话了，
      #   会话里拿到的指令虽然写着"先停一下去选"，但那是让助手去读一句话，
      #   而不是让应用在点的那一刻就把人拦住。结果就是 gina 看到的那个现象：
      #   "没有弹出任何选择啊？直接就注入会话了"。
      #   拦的方式沿用决策那套：返回 409 + code，前端据此弹出选择面板。
      if (isTRUE(HAS_REFERENCE) && !identical(as.character(stepId), "all")
          && !isTRUE(b$force) && supplement_needs_reference(stepId)) {
        refSid <- as.character(b$session_id %||% "")
        if (!nzchar(refSid)) refSid <- effective_sid

        # ---- 先在「服务器信息」里定的那一份，到这里自动生效 ----
        #   为什么要有这一步：用户可以在服务器信息那一节直接给路径 / 选上传 /
        #   选查文献 / 说这一轮不用。给了就意味着"这个决定已经做过"，
        #   到这里不该再拦一次、再弹一遍窗 —— 那是同一个问题问两遍。
        #   没给的（ref_mode 为空）才拦，让前端弹选择卡确认一次。
        plRef <- access$ctx$place
        srvRef <- (plRef %||% list())$server %||% list()
        refModeP <- as.character(srvRef$ref_mode %||% "")
        refPathP <- as.character(srvRef$ref_path %||% "")

        chosenRef <- ""
        if (nzchar(refSid) && HAS_SESSIONS) {
          s0 <- tryCatch(sess_find(APP_DIR, refSid), error = function(e) NULL)
          chosenRef <- as.character((s0 %||% list())$reference_id %||% "")
        }
        haveChosen <- nzchar(chosenRef) &&
          (nzchar(ref_path(chosenRef)) || nzchar(ref_server_path(chosenRef)))

        # ① 事先给了服务器路径：登记进候选库并直接选为本次使用。
        if (!haveChosen && identical(refModeP, "path") && nzchar(refPathP)) {
          tryCatch({
            ref_register_server(
              list(list(path = refPathP, name = basename(refPathP),
                        note = as.character(srvRef$ref_note %||% ""),
                        species = "", tissue = "")),
              host = as.character(srvRef$host %||% ""))
            ridP <- paste0("srv-", substr(paste0("h", digest_of_str(refPathP)), 1, 12))
            if (nzchar(ref_server_path(ridP)) && nzchar(refSid) && HAS_SESSIONS) {
              sess_update(APP_DIR, refSid, list(reference_id = ridP))
              chosenRef <- ridP
              haveChosen <- TRUE
            }
          }, error = function(e) NULL)
        }

        # ② 事先说了这一轮不用：放行，不拦。
        passNoRef <- identical(refModeP, "none")

        if (!haveChosen && !passNoRef) {
          return(send_json(con, 409, list(
            ok = FALSE, code = "need_reference", step = stepId,
            error = "这一步要先定注释依据，定完才能跑",
            # 把"事先选的模式"带回去：前端据此把卡片默认停在对的那一页，
            #   而不是每次都从"服务器已有"开始。
            pref_mode = refModeP,
            # 文献库模式：把存档号一起带回去。前端据此直接发起检索，
            #   不再弹"服务器/上传/查文献"那张卡 —— 这三个选项人早就在
            #   页面里选过了，再问一遍等于同一个决定做两次。
            ref_archive = as.character(srvRef$ref_archive %||% ""),
            items = ref_list())))
        }
      }

      # ---- 点击即入执行队列：先入队，再做任何判断 ----
      #   为什么放在最前：人点一下的意图是"跑这一步"。以前闸门先判，
      #   一旦上一步没做质检就直接 409 —— 按钮看起来就是死的。
      #   入队在这里，工作电脑上的执行端每分钟来取，取到就真跑。
      # 入队时把这一步的完整命令一起存进任务。
      #   为什么：执行端自己有一张"步骤->脚本"的表，那张表和应用的 flow_spec
      #   是两份，一改就错位。错位时的表现就是注释这一步报
      #   「模板脚本不存在：<结果目录>/02_annotate.R」——它以为要把
      #   02_annotate.R 渲染到结果目录，而真实脚本在 _engine/sc-pipeline/scripts 下。
      #   查表这件事本来就该应用做：它知道这一步用哪个脚本、什么参数。
      #   命令随任务下发，执行端照着跑就行，不需要再维护第二张表。
      plExec <- access$ctx$place
      execSid <- effective_sid
      execVars <- flow_vars((plExec %||% list())$server %||% list(), sid = execSid)
      execStage <- if (HAS_FLOW) flow_stage(stepId, flow_profile(execSid)) else NULL
      execContract <- if (!is.null(execStage)) flow_execution(execStage, execVars) else NULL
      if (!is.null(execContract) && !isTRUE(execContract$ok))
        return(send_json(con, 409, list(ok = FALSE, code = "missing_execution_input",
          error = paste(execContract$errors, collapse = "；"), missing = execContract$missing)))

      # ---- 环节前置审：开跑之前审一次（2026-11-05）---------------------
      #   为什么必须放在入队之前：入队之后再撤回，执行端可能已经取走了，
      #   撤回只是把记录改成 failed，服务器上照样跑了一遍。
      #   判法与后置审同一套（engine/stage_reviewer.R），判据仍来自 flow_spec：
      #     block  明确不合格：上游没放行、输入不全、脚本缺失、注释依据没选
      #     warn   有未核项或历史遗留（上游还没护照），放行但留痕
      #     pass   全部条件齐备
      #   为什么不像原来那样"只提示不拦"：
      #     原来上一步没过审也照派，理由是"拦下来只会让人点不动"。
      #     但那样一来，"处理没有偏移"就永远只能靠人看红条。
      #     现在把"明确不合格"和"暂时判不了"分开：前者拦，后者放行并留痕。
      #     force 放行沿用同一条规矩 —— 只接受人的身份并要写理由。
      srPre <- NULL
      if (isTRUE(HAS_STAGE_REVIEWER) && !identical(as.character(stepId), "all") &&
          !identical(as.character(stepId), "config")) {
        srPre <- tryCatch(
          sr_preflight(APP_DIR, stepId, sid = execSid, profile = flow_profile(execSid),
                       V = execVars, force = isTRUE(b$force)),
          error = function(e) list(verdict = "warn", checks = list(), blockers = list(),
                                   error = conditionMessage(e)))
        if (identical(as.character(srPre$verdict %||% ""), "block"))
          return(send_json(con, 409, list(
            ok = FALSE, code = "stage_review_blocked", step = stepId,
            error = "开跑前审没过：这一环还不具备开跑条件，未入队、未下发。",
            permit_id = srPre$permit_id %||% NULL,
            blockers = srPre$blockers %||% list(),
            checks = srPre$checks %||% list(),
            hint = paste0("按 blockers 里的 fix 逐条补齐后重试；",
                          "确实要现在跑，请以人的身份在页面上强制放行并写理由。"))))
      }
      # ---- 上游有"申报完成但判定没过"的步骤：不许往下走 ----------
      #   为什么必须在这里拦：attempt 里写着 done，前置审若只看产物与护照，
      #   这一步就会被放行，merge 于是踩着一个未验收的结果开跑。
      #   判据只认拒绝凭据（回写那一刻服务端写的），不看客户端自报。
      rejBlock <- tryCatch({
        ord0 <- flow_run_order(execSid)
        idx0 <- match(stepId, ord0)
        rq0 <- as.character(sess_get_pending(APP_DIR, execSid, slot = "step") %||% "")
        bad <- character(0)
        if (nzchar(rq0) && !is.na(idx0) && idx0 > 1L) {
          ad0 <- attempt_dir(APP_DIR, execSid, rq0)
          if (dir.exists(ad0)) {
            # 审计重验会留 .recheck-*.json 副本，它不是新的拒绝，不参与闸门。
            #   并且：该 step 已有 verified 行时，历史拒绝凭据不再挡下游 —— 否则
            #   判据修正后重验通过的那一步，永远无法放行 merge。
            f0s <- Filter(function(f) !grepl("\\.recheck-", basename(f)),
                          list.files(ad0, pattern = "^verdict_reject-.*[.]json$",
                                     full.names = TRUE))
            for (f0 in f0s) {
              j0 <- tryCatch(jsonlite::fromJSON(read_text_any(f0), simplifyVector = FALSE),
                             error = function(e) NULL)
              st0 <- as.character((j0 %||% list())$step %||% "")
              if (nzchar(st0)) {
                vq0 <- as.character((j0 %||% list())$req_id %||% rq0)
                vfn <- tryCatch(db_scalar(DB_CON,
                  paste0("SELECT COUNT(*) FROM step_attempts ",
                         "WHERE req_id = ? AND step_id = ? AND status = 'verified'"),
                  list(vq0, st0)), error = function(e) NA)
                if (length(vfn) == 1L && !is.na(vfn) && as.integer(vfn) > 0L) next
              }
              if (nzchar(st0) && !is.na(match(st0, ord0)) && match(st0, ord0) < idx0)
                bad <- c(bad, st0)
            }
          }
        }
        unique(bad)
      }, error = function(e) character(0))
      if (length(rejBlock))
        return(send_json(con, 409, list(ok = FALSE, code = "upstream_rejected",
          step = stepId,
          error = paste0("上游这步的完成申报没通过判定：", paste(rejBlock, collapse = "、"),
                         "。未入队、未下发。"),
          hint = "先按拒绝理由补齐产物或修正数据后重新回写，再点下一步；不要强制放行。")))
      # Only newly enqueued tasks receive the versioned audit contract.
      if (is.list(execContract$params) && length(execContract$params))
        execParams <- modifyList(execParams %||% list(), execContract$params)
      enqCmd <- ""
      enqArgs <- ""
      if (!is.null(execContract)) {
        enqCmd <- execContract$script
        enqArgs <- paste(vapply(execContract$argv, shQuote, "", type = "sh"), collapse = " ")
      }
      # ---- 位置参数兜底与拒绝 ----------------------------------------
      #   实测（2026-11-13 22:10 final）：合同里 argv 为空，作业实际收到
      #   0 个位置参数，脚本第一行就 stop「必须给第一个位置参数」，
      #   作业 1 秒内退出。这种失败最费人：看起来点出去了，其实什么也没算。
      #   这里按步骤补上必需的 argv；补不出来就拒绝入队，不派必失败的作业。
      # 命令必须由 flow_spec 这一处给全；这里只做拒绝，不自己拼。
      #   为什么不留兜底：兜底会把「合同写漏了」掩盖成「跑起来了」，
      #   下次换一步照样漏。实测 2026-11-13 22:10 final 就是合同 args 为空，
      #   作业收到 0 个位置参数、1 秒退出，看起来像点了却没算。
      #   现在缺参就拒绝入队，把问题留在派工这一刻暴露。
      if (stepId %in% c("filter", "final") && !nzchar(enqArgs))
        return(send_json(con, 409, list(ok = FALSE, code = "missing_script_args",
          step = stepId,
          error = paste0("这一步的命令缺少位置参数，派出去只会立刻失败，所以没有入队。",
                         "请先确认运行目录已选。"),
          hint = "选好运行目录后重试；不要用空参数硬派。")))
      # ---- 每次执行先定一个属于本步的请求号，入队成功后才登记 ----------
      #   为什么要先备号、后登记：执行端回写 /api/step/import 时只认
      #   "非空 req_id + 该 req 的 attempt 已存在"。没有这两样，
      #   作业在集群上真跑完了也写不回来（实测 nn9f 就是这样）。
      #   而如果先登记 attempt 再入队、入队却失败，就会留下一条
      #   "有 attempt、永远等回写"的假状态。所以顺序是：
      #     备号 → 入队 → 成功才 attempt_register + 设 pending。
      #   号必须在这里现生成：这段代码在 rq 赋值之前（rq 要到委派段末尾才算），
      #   早先写成"有 rq 就用 rq"会直接 object 'rq' not found —— 正常点一步就崩。
      #   也不从识别的 pending 继承：那是另一次请求的号，借用等于让两件事共用一个身份。
      # ---- 先分清：这一环是"本次新建"还是"已经有任务" ----------------
      #   为什么必须先分清：exec_enqueue 遇到同 session/step 的 queued/running
      #   会直接返回那条旧任务（防连点跑两遍）。如果调用方仍按"新任务"处理，
      #   就会把本步 pending 改成一个从未属于那条旧任务的号 ——
      #   完成回写时 attempt_selected 对不上，结果被判成别的题次而吞掉。
      #   force 也走这条路：放行不等于放开互斥，否则两份作业会写同一个输出目录。
      exReuse <- if (nzchar(rq_override)) NULL else tryCatch(exec_active_for(stepId, execSid), error = function(e) NULL)
      stepRq <- ""
      # 链式派工：整条链共用一个请求号。
      #   为什么必须在这里就定下来：原来先按新号登记 attempt 与 pending，
      #   随后 chain 分支又把 exec.req_id 覆盖成链请求号 ——
      #   三者不一致，回写时 attempt_selected 对不上，结果被判成别的题次。
      #   统一成链请求号之后，exec.req_id、attempt、pending 三者相同，
      #   既保住旧链一直用的回调语义（按链号销账），也不影响普通单步。
      chainRq <- if (isTRUE(chain_dispatch) && !is.null(active_chain))
        as.character(active_chain$req_id %||% "") else ""
      # Bind the queue item to the authorized record's reference decision. The
      # path may have just been registered from place$server above, so re-read
      # the target record after the gate instead of relying on global place.
      exec_ref <- list(reference_id = "", ref_mode = "", ref_path = "")
      exec_srv <- (access$ctx$place %||% list())$server %||% list()
      exec_ref$ref_mode <- as.character(exec_srv$ref_mode %||% "")
      if (HAS_SESSIONS && nzchar(execSid)) {
        s_ref <- tryCatch(sess_find(APP_DIR, execSid), error = function(e) NULL)
        exec_ref$reference_id <- as.character((s_ref %||% list())$reference_id %||% "")
        if (nzchar(exec_ref$reference_id) &&
            exists("ref_server_path", mode = "function")) {
          exec_ref$ref_path <- record_trim(
            tryCatch(ref_server_path(exec_ref$reference_id), error = function(e) ""))
        }
        if (!nzchar(exec_ref$ref_path) && nzchar(exec_ref$reference_id) &&
            exists("ref_path", mode = "function")) {
          exec_ref$ref_path <- record_trim(
            tryCatch(ref_path(exec_ref$reference_id), error = function(e) ""))
        }
      }
      if (!nzchar(exec_ref$ref_path) && identical(exec_ref$ref_mode, "path"))
        exec_ref$ref_path <- record_trim(exec_srv$ref_path)
      enq <- NULL
      # ---- 没有执行端在线，就不派活 ----------------------------------
      #   为什么（2026-11-22，gina 报「它的回复报错」）：
      #     原来照样入队，只在正文前加一句"执行端未就位"。这条任务在队列里
      #     停到超时，被回收成 failed，页面就显示"这一步失败过"；
      #     而会话里那条通知要求助手"只看不跑"，助手照做后只能回一段
      #     "什么都没跑、任务失败、同一环节已提交过三次、产物都是空的" ——
      #     人看到的是一次报错，真实原因只是没人接活。
      #   现在直接拒绝：不入队、不生成任务号、不改步骤状态。
      #   判据是心跳新鲜度，不是"历史上有没有过心跳"。
      if (is.null(exReuse)) {
        hb0 <- tryCatch(exec_worker_hb_all(), error = function(e) list())
        online0 <- Filter(function(h) isTRUE(exec_heartbeat_online(h)), hb0)
        if (length(online0) == 0L) {
          last_at <- ""
          if (length(hb0)) {
            hts <- vapply(hb0, function(h) as.character(h$at %||% ""), "")
            hts <- hts[nzchar(hts)]
            if (length(hts)) last_at <- max(hts)
          }
          # 检测不到，就主动发起启动：把"在哪台电脑装、装完怎么起"写出去。
          #   为什么不只是报错（gina 2026-11-22 要求：执行端要被应用检测并启动，
          #   检测不到就启动安装）：报错等于把这件事交回给人去猜。应用虽然起不了
          #   对方电脑上的进程，但能定位到该在哪台机器装、并留下一条可执行的待办。
          boot <- tryCatch(exec_bootstrap_request(reason = paste0("dispatch:", stepId)),
                           error = function(e) list(ok = FALSE, code = "bootstrap_error"))
          return(send_json(con, 409, list(
            ok = FALSE, code = "exec_worker_offline", step = stepId,
            exec_online_count = 0L,
            last_heartbeat_at = if (nzchar(last_at)) last_at else NULL,
            wrote_queue = FALSE, wrote_step_state = FALSE,
            bootstrap = boot,
            exec_pc = as.character((boot %||% list())$exec_pc %||% ""),
            error = "没有执行端在线，这条作业没有派出去，也没有入队。",
            hint = paste0(
              "已把「在哪台电脑装、装完怎么起」写成一条待办（见队列看板）。",
              if (nzchar(as.character((boot %||% list())$exec_pc %||% "")))
                paste0("要装的是 ", as.character(boot$exec_pc), "。"),
              if (nzchar(last_at)) paste0("它最后一次报信是 ", last_at, "。")
              else "它从来没有报过信。",
              "启动后 60 秒内心跳可见，队列会自己开始走；现在重复点只会堆空任务。"))))
        }
      }
      if (!is.null(exReuse)) {
        # 复用既有任务：只回报它的真实状态，不生成号、不登记 attempt、
        #   不改 pending、不写 remote_step、不碰 ledger。
        enq <- exReuse
      } else {
        stepRq <- if (nzchar(rq_override)) rq_override else if (nzchar(chainRq)) chainRq else
          paste0("req-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                 paste(sprintf("%02x", as.integer(openssl::rand_bytes(8))), collapse = ""))
        # 结构参数带上"这份参数属于谁"的绑定信息。
        #   为什么要写进文件本身：参数文件会被执行端写到集群上，
        #   一旦写错目录或串了会话，脚本照样能跑出一个"看起来成功"的结果。
        #   把 session / req / run_dir 一起写进去，执行端在提交前就能核对，
        #   事后复盘时也能确认这份清单属于哪一次执行。
        if (is.list(execParams) && length(execParams)) {
          execParams$binding <- list(
            session_id = as.character(execSid %||% ""),
            req_id = as.character(stepRq %||% ""),
            run_dir = as.character(execVars$work_root %||% ""),
            step = as.character(stepId %||% ""))
        }
        # con is the HTTP response connection. Internal dispatch has no HTTP
        # connection, but uses the same application database as manual dispatch.
        prepare_task <- function(task) {
          failure <- function(detail) list(ok = FALSE, detail = detail)
          if (!isTRUE(DB_READY) || is.null(DB_CON)) return(failure("数据库不可用"))
          owner <- owner_ctx_key(owner_ctx)
          if (length(owner) != 1L || is.na(owner) || !nzchar(owner))
            return(failure("无法确认分析记录的归属"))
          tryCatch({
            version <- db_ensure_session_version(DB_CON, execSid, owner)
            if (!isTRUE(version$ok)) return(failure(as.character(version$code)))
            mode <- if (isTRUE((execStage %||% list())$qsub)) "batch" else "login"
            binding <- db_register_dispatch(
              DB_CON, owner_key = owner, session_id = execSid,
              version_id = version$version_id, step_id = stepId,
              req_id = stepRq, exec_id = task$id,
              script_path = as.character(enqCmd %||% ""),
              script_digest = as.character(b$script_digest %||% b$script_md5 %||% ""),
              run_dir = task$run_dir, executor_mode = mode)
            if (!isTRUE(binding$ok) || !nzchar(record_trim(binding$attempt_id)))
              return(failure(as.character(binding$error %||% binding$code %||% "无法登记运行")))
            legacy <- attempt_register(APP_DIR, execSid, stepRq, origin = "step_run")
            if (!isTRUE(legacy$ok))
              return(failure(as.character(legacy$error %||% "无法登记本步请求")))
            pending <- sess_set_pending(APP_DIR, execSid, stepRq, slot = "step")
            if (!isTRUE(pending$ok))
              return(failure(as.character(pending$error %||% "无法登记待回写请求")))
            fields <- list(attempt_id = binding$attempt_id, run_id = binding$run_id,
                           version_id = binding$version_id, executor_mode = mode)
            if (nzchar(chainRq)) fields$chain_id <- chainRq
            list(ok = TRUE, fields = fields)
          }, error = function(e) failure(conditionMessage(e)))
        }
        enq <- tryCatch(exec_enqueue(stepId, session_id = execSid,
                            script = enqCmd, script_args = enqArgs, req_id = stepRq,
                            params = execParams, params_rel = execParamsRel,
                            run_dir = as.character(access$ctx$run_dir %||% ""),
                            mode = as.character(access$ctx$mode %||% ""),
                            reference_id = exec_ref$reference_id,
                            ref_mode = exec_ref$ref_mode,
                            ref_path = exec_ref$ref_path,
                            pool_id = exec_default_pool_id(), prepare = prepare_task),
                         error = function(e) NULL)
        if (!is.null(enq$dispatch_error)) {
          return(send_json(con, 409, list(ok = FALSE, code = "dispatch_binding_unavailable",
            step = stepId, detail = enq$dispatch_error$detail, wrote_queue = FALSE,
            error = "无法登记本次运行，作业未入队。")))
        }
      }
      if (nzchar(chainRq) && !is.null(enq) &&
          !identical(as.character(enq$chain_id %||% ""), chainRq)) {
        return(send_json(con, 409, list(ok = FALSE, code = "chain_task_conflict", step = stepId,
          error = "这个环节已有一条不属于本流程的任务，请等原任务结束后再启动。")))
      }

      # A concurrent click may have reused the existing queued or running task.
      # Leave its writeback binding and notification untouched.
      if (!is.null(enq) && !identical(as.character(enq$req_id %||% ""), stepRq)) {
        return(send_json(con, 200, list(ok = TRUE, delegated = TRUE,
          duplicate_dispatch = TRUE, exec_queued = TRUE, step = stepId,
          req_id = as.character(enq$req_id %||% ""),
          exec_id = as.character(enq$id %||% ""),
          exec_status = as.character(enq$status %||% ""))))
      }

      # ---- 质检闸门：上一步没过审 ----
      #   现在只作提示，不再拦本次点击：这次的作业由执行端按登记脚本跑，
      #   跑完照样带 mtime 回写，事后可核。拦下来只会让人点不动。
      #   用户要的三件事里，"处理没有偏移"只能靠这个卡住：
      #   上一步跑完了但没跑质检，或者质检没过，就说明没人核过
      #   "是不是在服务器上跑的、脚本对不对、有没有跳步"。
      #   这时候放它往下走，后面的产物就没有可信来源了。
      #   force=true 是留给人的手动放行，不是默认路径。
      # 注意：reviewWarn 必须先在块外声明。
      #   原来只在 if 里面赋值，外面根本读不到 —— 页面永远显示不出"上一步没过审"，
      #   等于这条提示从来没生效过。
      reviewWarn <- ""
      prevQual <- NULL
      if (HAS_FLOW && !identical(as.character(stepId), "all") &&
          !identical(as.character(stepId), "config") &&
          !isTRUE(b$force)) {
        flowOrder <- flow_run_order(effective_sid)
        idx <- match(as.character(stepId), flowOrder)
        # 只认当前这条分析的进度与质检结论。
        #   闸门原来读全局文件，等于拿别人的质检结论给这一步放行 ——
        #   比显示错结果更严重，所以这里必须按分析号过滤。
        gateSid <- effective_sid
        dNow <- remote_steps_read(gateSid)
        # 不再读服务器那份质检结论（state/flow_progress.json）。
        #   那份是服务器上 reviewer.R 判的，而服务器上的 reviewer.R 是另一份副本，
        #   按老流程的阶段名单写的，annotate / figs / enrich 都不认识 ——
        #   拿它判定，这三个环节永远"没有质检"，提示永远挂着。
        #   现在统一以应用侧质检（engine/quality.R）为准：判据是回写状态、
        #   产物清单、运行目录、日志有无失败字样，这些应用自己都能核。
        gNow <- list(stages = list())
        if (!is.na(idx) && idx > 1) {
          prevId <- flowOrder[idx - 1]
          prevRec <- Filter(function(x) identical(as.character(x$id %||% ""), prevId), (dNow$steps %||% list()))
          prevDone <- length(prevRec) > 0 &&
            identical(as.character(prevRec[[length(prevRec)]]$status %||% ""), "done")
          if (prevDone) {
            # 判据只有一份：应用侧质检（engine/quality.R）。
            #   不再是服务器那份 —— 服务器上的 reviewer.R 是另一份副本，
            #   阶段名单按老流程写，annotate / figs / enrich 都不认识。
            #   用两套账去判，结果就是这两套账互相打架：
            #   应用这边说没跑过，服务器那边说阶段名不对，人看到两行红字，
            #   却不知道该信哪一行、该改什么。
            if (isTRUE(HAS_QUALITY)) {
              qPrev <- tryCatch(qual_of(gateSid, prevId), error = function(e) NULL)
              qOk <- !is.null(qPrev) && identical(as.character(qPrev$verdict %||% ""), "pass")
              if (!qOk) {
                ptitle <- if (HAS_FLOW) {
                  x <- flow_stage(prevId, flow_profile(gateSid))
                  if (is.null(x)) prevId else x$title
                } else prevId
                why <- if (is.null(qPrev))
                  "应用侧还没有它的质检记录"
                else if (identical(as.character(qPrev$verdict %||% ""), "unknown"))
                  "应用侧质检判不了（信息不足）"
                else "应用侧质检没过"
                if (!is.null(qPrev) && nzchar(as.character(qPrev$note %||% "")))
                  why <- paste0(why, "：", as.character(qPrev$note))
                # 只作提示，作业照排：卡住按钮只会让人点不动，看不出问题在哪。
                reviewWarn <- paste0("「", ptitle, "」已经跑完，但", why,
                                     "；本次仍按登记脚本执行，结果会带产物时间回写，事后可核。")
              }
            }
            prevQual <- tryCatch(
              qual_prev_check(gateSid, stepId, order = flowOrder),
              error = function(e) NULL)
          }
        }
      }
      pl <- access$ctx$place
      sid <- execSid
      rq <- if (HAS_SESSIONS && nzchar(as.character(sid %||% "")))
        as.character(sess_get_pending(APP_DIR, sid, slot = "step") %||% "") else ""
      note <- as.character(b$gate_note %||% "")
      # 已保存的分析限制随这一步的指令一起下发（2026-11-18）。
      #   为什么要在这里并进去：改哪几个 cell、注释能不能合并相邻群、
      #   范围原话怎么理解，这些说清楚的对象是拿到 notebook 和相关表的执行端。
      #   应用这边把它判成「没转成计划」就挡掉，用户就得回头再解释一遍，
      #   而解释完还是回到同一步。写成一段话下发，活能往下走，
      #   判断留在能判断的地方，出错时也有原话可对。
      if (exists("intent_constraint_note", mode = "function")) {
        cnote <- tryCatch(intent_constraint_note(APP_DIR, as.character(sid %||% "")),
                          error = function(e) "")
        if (is.character(cnote) && length(cnote) == 1L && nzchar(cnote))
          note <- paste0(if (nzchar(note)) paste0(note, "；") else "",
                         "【这条记录已保存的要求】", cnote)
      }
      # 「再次交给助手」时用户填的具体要求：附在已经定下的事后面。
      #   为什么单独一段而不是混进 gate_note：两者来源不同 ——
      #   gate_note 是流程闸门的客观答案，这一条是这一次人工追加的要求。
      #   混在一起，事后分不清哪句是当初定的、哪句是这次补的。
      extra <- trimws(as.character(b$req_note %||% ""))
      if (nzchar(extra)) {
        note <- paste0(if (nzchar(note)) paste0(note, "；") else "",
                       "这次重新执行的额外要求：", extra)
      }
      ins <- remote_step_instruction(stepId, pl, session_id = as.character(sid %||% ""),
                                     req_id = rq, gate_note = note,
                                     binding = list(
                                       attempt_id = as.character((enq %||% list())$attempt_id %||% ""),
                                       run_id = as.character((enq %||% list())$run_id %||% ""),
                                       version_id = as.character((enq %||% list())$version_id %||% ""),
                                       executor_mode = as.character((enq %||% list())$executor_mode %||% "")))
      remote_step_put(stepId, list(
        title = (STEP_TASK[[stepId]] %||% list(title = stepId))$title,
        status = "delegated", code = NULL,
        note = "已交给助手，等它回写",
        delegated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        req_id = if (nzchar(rq)) rq else NULL,
        # ---- 把「人已经定下的事」一起存下来 ----------------------------
        #   为什么必须落盘（2026-11-03，审核第 8 条）
        #     短指令只写「办什么 + 去哪取全文」，真正的交代在 /api/brief 里。
        #     而 /api/brief 这条路由调 remote_step_instruction 时没传 gate_note，
        #     于是"人已经定下的事"这句在取全文时直接消失了。
        #     它的作用不是装饰：筛群剔群、口径选择这类决定就靠这句话往下传。
        #     现在交付时把它存进记录，/api/brief 再按 step+session 取回来。
        gate_note = if (nzchar(note)) note else NULL,
        session_id = if (nzchar(as.character(sid %||% ""))) as.character(sid) else NULL))
      # ---- 建一个查询作业：交出去之后由它负责「还没回写就再查一次」 ----
      #   以前三类作业只有 fetch 和 rework 有创建点，poll 一次都不会被建，
      #   所以「关掉标签页也接着推进」只写在注释里，实际从未发生。
      #   这个作业的存在意义很具体：交付后 10 分钟还没回写，
      #   由 poller 自动发一条「只查不重跑」的查询，而不是等人来问。
      if (DB_READY) {
        tryCatch({
          vinfo <- db_api_version_of(DB_CON, as.character(sid %||% ""))
          if (!is.null(vinfo) && nzchar(vinfo$version_id %||% "")) {
            rid0 <- db_ensure_run(DB_CON, vinfo$version_id,
                                  if (is.na(owner_ctx$key)) "unattributed" else owner_ctx$key,
                                  work_root = as.character(V$run_dir %||% ""))
            aid0 <- as.character((enq %||% list())$attempt_id %||% "")
            if (nzchar(as.character((enq %||% list())$run_id %||% "")))
              rid0 <- as.character(enq$run_id)
            if (!nzchar(aid0)) {
              aid0 <- db_new_id("att")
              db_tx(DB_CON, function() {
                no <- db_next_attempt_no(DB_CON, rid0, stepId)
                DBI::dbExecute(DB_CON,
                  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,step_id,
                     attempt_no,req_id,status,started_at,payload)
                   VALUES(?,?,?,?,?,?,?,?,?,?,?)",
                  list(aid0, rid0, vinfo$version_id,
                       if (is.na(vinfo$project_id %||% NA)) NA_character_ else vinfo$project_id,
                       if (is.na(owner_ctx$key)) "unattributed" else owner_ctx$key,
                       stepId, no, if (nzchar(stepRq)) stepRq else NA, "delegated",
                       db_now(), db_json(list(via = via, run_dir = V$run_dir %||% ""))))
              })
            }
            # Six fields must be present before a poll job is created. Reusing
            # the dispatch attempt keeps poll and write-back on one identity.
            DBI::dbExecute(DB_CON,
              paste0("INSERT OR IGNORE INTO poll_jobs(job_id,kind,owner_key,run_id,",
                     "version_id,step_id,attempt_id,query_request_id,payload,state,",
                     "next_check_at,created_at,updated_at) ",
                     "VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)"),
              list(paste0("job-poll-", aid0), "poll",
                   if (is.na(owner_ctx$key)) "unattributed" else owner_ctx$key,
                   rid0, vinfo$version_id, stepId, aid0,
                   if (nzchar(stepRq)) stepRq else paste0("q-", aid0),
                   db_json(list(project = vinfo$project_id, run_dir = V$run_dir %||% "")),
                   "pending", db_iso_add(600), db_now(), db_now()))
          }
        }, error = function(e) NULL)
      }

      # ---- 台账 start 行：交给执行端去追加 ----
      #   为什么在"交出去"这一刻就写，而不是等跑完：
      #   reviewer.R 要求每个阶段既有 start 又有 finish。
      #   只写 finish 的话它会判"台账里没有 start 记录 —— 这一步没经过统一入口"。
      #   这条正是上一版被判 REFUSED 的直接原因之一。
      if (isTRUE(HAS_LEDGER_OUT)) {
        tryCatch(led_out_push(
          sid = as.character(sid %||% ""), step = stepId, kind = "start",
          run_dir = as.character(V$run_dir %||% ""),
          note = "由应用在交付执行时登记"), error = function(e) NULL)
      }
      # ---- 进会话的那一段：已入队就只让助手观察，绝不再让它去执行 ------
      #   为什么必须分岔：执行端是唯一的提交者。作业一旦入队，
      #   会话里再说"你去服务器上执行"就等于给同一件事派第二个执行者 ——
      #   实测已经发生过：10:01 自动拿到的那条计划写着"后台启动逐样本去双"，
      #   而我照它真的要起一份 R 进程，正撞上执行端已提交的作业 60739。
      #   两套产物写同一个 1.doublet/，事后无法判定哪份有效。
      #   所以：入队成功 -> observe 文本；入队失败 -> 明确报错，不假装已入队。
      stage <- if (HAS_FLOW) flow_stage(stepId, flow_profile(sid)) else NULL
      briefPath <- paste0("/api/brief?step=", utils::URLencode(stepId, reserved = TRUE),
                          if (nzchar(as.character(sid %||% "")))
                            paste0("&session=", utils::URLencode(as.character(sid), reserved = TRUE)) else "",
                          if (nzchar(rq))
                            paste0("&req=", utils::URLencode(rq, reserved = TRUE)) else "")
      queuedOk <- !is.null(enq)
      if (!queuedOk) {
        # 没进队列就是没派出去。这里返回失败，不要走"已交给助手"那条路：
        #   以前 enq 为 NULL 也照样回 200 并生成执行正文，页面于是显示成功、
        #   助手于是真的去跑 —— 等于绕过了执行端。
        return(send_json(con, 502, list(
          ok = FALSE, code = "exec_enqueue_failed", step = stepId,
          error = "这一环没能排进执行队列，作业没有派出去；未生成执行指令。",
          hint = "先看执行队列状态与执行端心跳，再重试；不要改用聊天让助手直接跑。")))
      }
      short <- if (queuedOk) {
        remote_step_observe_text(stepId, sid = as.character(sid %||% ""), ex = enq)
      } else {
        paste(c(
          "【来自 个性化分析助手】",
          paste0("要办的事：在服务器上执行「",
                 (STEP_TASK[[stepId]] %||% list(title = stepId))$title, "」。",
                 if (!is.null(stage)) paste0("（阶段 id=", stage$id, "，skill=", stage$skill, "）") else ""),
          "第一步先取完整指令（顺序、命令、环节、回写格式都在里面）：",
          paste0("GET ", briefPath, "  取 text 字段，照它执行。"),
          "按里面的顺序跑，每个环节结束回写 /api/step/import（状态、运行目录、产物清单、日志都要写全）。",
          "不用另外跑质检脚本：质检由应用按你的回写内容自己判。"
        ), collapse = "\n")
      }
      # ---- 派工前确认执行端就位 --------------------------------------
      #   为什么放在这里而不是等人报错：
      #     任务能入队、页面也显示已交给助手，但如果没有任何执行端在线，
      #     它就一直停在 queued、作业号永远空。这个状态在系统里此前没有任何
      #     地方说过，于是每换一台电脑、每升一次服务端，都要人踩一次才被发现。
      #   现在把结论直接写进这一次派工的内容里：缺执行端时，第一条要求就是
      #     「先装好并启动执行端」，而不是让它安静地排在队列里。
      hb_now <- tryCatch(exec_worker_hb_all(), error = function(e) list())
      online_now <- Filter(function(h) isTRUE(exec_heartbeat_online(h)), hb_now)
      exec_offline <- length(online_now) == 0L
      offline_note <- if (exec_offline) paste0(
        "【执行端未就位】这台应用当前没有任何执行端在线，任务排队后不会有人领取。\n",
        "先在工作电脑上装好并启动执行端（同一台机器重复执行不会重复安装）：\n",
        "  python pa_exec_worker.py --install\n",
        "  <固定位置>/run-worker.cmd   （Windows）或 <固定位置>/run-worker.sh（Linux/macOS）\n",
        "启动后心跳 60 秒内可见，之后按下面正文执行；已经装好只是没启动的，直接运行启动脚本即可。\n\n") else ""
      final_ins <- paste0(offline_note,
        if (queuedOk) remote_step_observe_text(stepId, sid = as.character(sid %||% ""), ex = enq) else ins)
      final_short <- paste0(
        if (exec_offline) "【执行端未就位，先安装并启动执行端再跑】" else "", short)

      return(send_json(con, 200, list(
        ok = TRUE, delegated = TRUE, step = stepId,
        title = (STEP_TASK[[stepId]] %||% list(title = stepId))$title,
        # instruction 是长文，给页面留档与人工查看用；
        #   已入队时它同样不写"要你执行"，避免有人复制这段去手工跑。
        instruction = final_ins,
        brief_path = briefPath,
        session_text = final_short,
        # 执行端是否在线：前端据此提示，不必等人发现任务卡住。
        exec_offline = exec_offline,
        exec_online_count = length(online_now),
        # 观察模式下 brief 也要一致：带上 observe 标记，brief 端点照样给全文，
        #   但前端与助手都按"只观察"处理。两处说法必须同源，不能一个 observe 一个 execute。
        mode = if (queuedOk) "observe" else "execute",
        # 本次派工的请求号。前端拿它绑定「这一步」的通知，
        #   不能用识别请求号（commitReq）顶替 —— 那是另一件事的号。
        #   已入队时以任务自带的 req_id 为准；未入队时用本步的真实请求号。
        req_id = if (queuedOk) as.character(enq$req_id %||% "") else as.character(stepRq %||% ""),
        request_id = if (queuedOk) as.character(enq$req_id %||% "") else as.character(stepRq %||% ""),
        attempt_id = if (queuedOk) as.character(enq$attempt_id %||% "") else NULL,
        run_id = if (queuedOk) as.character(enq$run_id %||% "") else NULL,
        version_id = if (queuedOk) as.character(enq$version_id %||% "") else NULL,
        executor_mode = if (queuedOk) as.character(enq$executor_mode %||% "") else NULL,
        step_id = as.character(stepId),
        exec_id = if (queuedOk) as.character(enq$id %||% "") else NULL,
        exec_status = if (queuedOk) as.character(enq$status %||% "") else NULL,
        exec_job = if (queuedOk) as.character(enq$job_id %||% "") else NULL,
        run_dir = if (queuedOk) as.character(enq$run_dir %||% "") else NULL,
        reference_id = if (queuedOk) as.character(enq$reference_id %||% "") else NULL,
        ref_mode = if (queuedOk) as.character(enq$ref_mode %||% "") else NULL,
        ref_path = if (queuedOk) as.character(enq$ref_path %||% "") else NULL,
        # 上一步质检的检查结果：这一版只展示，不拦。
        #   要让它变成闸门，得单独一轮改动 + 明确说"现在开始拦"，
        #   不能一边说"不影响流程"一边偷偷卡住按钮。
        # 作业是否真的进了执行队列。
        #   页面拿它来判断"这件事到底派出去没有"：注入会话只是通知，
        #   入队才是执行的前提，两者不能混成一句成败。
        exec_queued = queuedOk,
        review_warn = if (nzchar(reviewWarn)) reviewWarn else NULL,
        prev_quality = prevQual,
        msg = if (queuedOk)
          "作业已排进执行队列，执行端会提交；会话那边只让它观察与解释，不会另派一次。"
          else "这一步没能排进执行队列，作业没派出去。")))
    }
    r <- tryCatch({
      if (HAS_NOTEBOOK) {
        session_nb <- sess_find(APP_DIR, effective_sid)
        if (is.null(session_nb) || !identical(session_nb$mode %||% "local", "local") ||
            !identical(normalizePath(session_nb$run_dir, mustWork = TRUE), normalizePath(runDir, mustWork = TRUE)))
          stop("Cannot establish local notebook session binding")
        nb_bind_local(runDir, APP_DIR, session_nb$id)
      }
      launch_step(stepId, runDir)
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
}
