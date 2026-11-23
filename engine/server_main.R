#!/usr/bin/env Rscript
# =====================================================================
#  server_main.R -- 个性化分析助手的本地服务实现
#
#  入口是仓库根的 serve.R：它保持极小，用 sys.source() 加载本文件，避免
#  Rscript 在每次冷启动时逐表达式解析一万行脚本。
#
#  为什么用 R 写服务：这套分析本来就只需要 R。
#  机器上有 R 就能开这个页面，不用再装 Node / Python / 任何东西。
#
#  启动：
#    Rscript serve.R [--port 18980] [--no-open]
#
#  它做的事：
#    1. 在 127.0.0.1 上开一个小网页（只本机可见，不对外）
#    2. 把 public/ 下的界面发给浏览器
#    3. 浏览器点按钮 -> 它去启动 engine/run_chain.R 跑分析脚本
#    4. 把产物列出来，供下载
#
#  分工（不要混）：
#    step_plan.R   「每一步跑哪条命令」的唯一出处
#    run_chain.R   「跑起来、登记状态、失败即停」的执行器
#    env_check.R   环境体检
#    server_main.R 只负责 网页 + 收发 JSON + 点按钮 -> 起执行器
#
#  本文件刻意只用 ASCII：Windows 中文控制台编码不一，
#  一旦出现乱码会让人以为程序坏了。中文都在网页与 JSON 里。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
# 会话正文首行的按钮名。任何注入会话的入口都走它，只允许短中文标签。
inject_origin_prefix <- function(origin) {
  lab <- gsub("[【】\r\n\t]", "", trimws(as.character(origin %||% "")))
  if (!nzchar(lab)) return("")
  if (nchar(lab, type = "chars") > 24L) lab <- substr(lab, 1L, 24L)
  paste0("【", lab, "】")
}

# 一句话把按钮名放到正文最前面，已经带过就不重复加。
#   所有进会话的路径先过这里，前端直接发、补发、后端自动派三种情形
#   才不会再各写一遍首行。
inject_with_origin <- function(text, origin) {
  t0 <- as.character(text %||% "")
  pre <- inject_origin_prefix(origin)
  if (!nzchar(pre)) return(t0)
  if (startsWith(t0, pre)) return(t0)
  # 换行而不是直接接上：按钮名独占第一行。
  #   接在一起时，第一行会被后半句的长路径占满，读起来看不出这次是什么操作。
  paste0(pre, "\n", t0)
}

# 去掉可选标题前缀，只留正文主体。
#   合并判据用它比较，而不是比全文。
#   为什么（2026-11-23，云准助手交接件举了反例、本机同型）：
#     先以没有按钮名的方式入队，带按钮名重试时正文多了 【按钮名】 前缀，
#     按全文比对就不相等，于是新建一条。旧那条 origin 仍是空的、首行仍没前缀，
#     照旧继续补发。面板上看起来像两件事，其实是同一件被记了两次。
#   判据改成"正文主体一致就是同一条"，标题差异就地升级，不另起一条。
inject_body_of <- function(text) {
  s <- as.character(text %||% "")
  sub("^【[^】]{1,24}】\r?\n?", "", s)
}

# 进会话那段短指针。抽成函数有两个原因：
#   一是 reuse 与新建两条分支原来各抄一份字符串，改一处会漏另一处；
#   二是它要能被直接跑正反例，埋在几千行的处理器里只能靠人读。
instruction_pointer_text <- function(path, context_path, session_id, req_id,
                                     origin = "", query_only = FALSE) {
  # ---- 第一行只放按钮名，后面才是指针 ------------------------------
  #   为什么改成独立一行（2026-11-22，gina 报「标题很奇怪」）：
  #     原来把【按钮名】和"请先完整读取交接说明：/srv/..."拼在同一行，
  #     会话里第一屏看到的就是一大串绝对路径，按钮名被淹没在路径里。
  #     她的要求是"第一行要标明这个按钮的信息"，那就让它独占一行。
  #   为什么路径要分行：三个路径挤在一行时，气泡里几乎全是 /srv/...，
  #     人分不清哪个是交接说明、哪个是正文。
  pre <- inject_origin_prefix(origin)
  body <- paste0(
    if (isTRUE(query_only)) "只读提问，不授权执行或回写。\n" else "",
    "请先完整读取交接说明：", context_path, "\n",
    "再完整读取正文TXT：", path, "\n",
    "session_id=", session_id, "；req_id=", req_id, "\n",
    "两文件属于应用运行主机；严格依交接说明定位主机及回写URL，",
    "不截断正文，不猜测localhost。不可读取时明确报错。")
  if (!nzchar(pre)) return(body)
  paste0(pre, "\n", body)
}

# 一条记录现在的识别状态。只看记录本身，不看任何页面的内存。
#   四种结论：none / waiting / ready / stale。
#   为什么要把"有结论但请求没回来"判成 stale 而不是 waiting：
#     请求挂着不回写时，页面继续写"识别中"等于替它保证还在跑。
#     实测有几条记录挂了好几天（952j：结论 10-26、pending 停在 11-12），
#     主人才会在另一个会话里看到"识别中"而实际早有结论。
identify_state <- function(app_dir, sid, now = Sys.time()) {
  s <- tryCatch(sess_find(app_dir, sid), error = function(e) NULL)
  blk <- (s %||% list())$identify %||% list()
  rr <- blk$result
  has_res <- is.list(rr) && length(rr) > 0L
  pend <- as.character(tryCatch(sess_get_pending(app_dir, sid, slot = "identify"),
                                error = function(e) NULL) %||% "")
  pend_at <- as.character(tryCatch(sess_get_pending_at(app_dir, sid, slot = "identify"),
                                   error = function(e) NULL) %||% "")
  res_req <- trimws(as.character(blk$req_id %||% ""))
  res_at <- as.character((rr %||% list())$imported_at %||% "")
  # 结论是否已经覆盖这条请求：号一致，或结论比请求更晚落盘。
  covers <- nzchar(res_at) && nzchar(pend_at) && res_at >= pend_at
  age_h <- if (nzchar(pend_at)) {
    t0 <- exec_parse_ts(pend_at)
    if (is.na(t0)) NA_real_ else as.numeric(difftime(now, t0, units = "hours"))
  } else NA_real_
  stale <- !nzchar(pend) || (is.finite(age_h) && age_h >= 24)
  status <- if (!has_res && !nzchar(pend)) "none"
    else if (has_res && (!nzchar(pend) || identical(pend, res_req) || covers)) "ready"
    else if (!has_res) { if (stale) "stale" else "waiting" }
    else { if (stale) "stale" else "waiting" }
  list(status = status, pending_req = if (nzchar(pend)) pend else NULL,
       pending_at = if (nzchar(pend_at)) pend_at else NULL,
       result_req = if (nzchar(res_req)) res_req else NULL,
       result_at = if (nzchar(res_at)) res_at else NULL,
       stale_hours = if (identical(status, "stale") && is.finite(age_h)) round(age_h, 1) else NULL)
}

# ---- 找自己的位置 ----
APP_DIR <- tryCatch({
  # Standalone maintenance entry points (engine/exec_reaper.R) source this file
  # for its definitions. They pass the app root explicitly so --file= points at
  # the maintenance script rather than serve.R.
  forced <- getOption("pa.app_dir", Sys.getenv("PA_APP_DIR", unset = ""))
  if (is.character(forced) && length(forced) == 1L && nzchar(forced)) {
    normalizePath(forced, winslash = "/", mustWork = TRUE)
  } else {
    a <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", a[grep("^--file=", a)])
    if (length(f)) normalizePath(dirname(f[1]), winslash = "/") else normalizePath(getwd(), winslash = "/")
  }
}, error = function(e) normalizePath(getwd(), winslash = "/"))

PUBLIC_DIR <- file.path(APP_DIR, "public")
ENGINE_DIR <- file.path(APP_DIR, "engine")
STATE_DIR  <- file.path(APP_DIR, "state")
dir.create(STATE_DIR, showWarnings = FALSE, recursive = TRUE)
STATE_FILE <- file.path(STATE_DIR, "state.json")

# 私有数据放在哪：一个开关，四个模块共用（B2）。
#   必须在 sessions/quality/decisions/mirror 之前加载 —— 它们在定义路径常量时
#   会问这个开关，晚了就还是老路径。
STATE_PATHS_FILE <- file.path(ENGINE_DIR, "state_paths.R")
if (file.exists(STATE_PATHS_FILE)) source(STATE_PATHS_FILE, local = TRUE)

argv <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, argv)
  if (is.na(i)) return(d)
  if (i == length(argv) || grepl("^--", argv[i + 1])) return(d)
  argv[i + 1]
}
has_flag <- function(k) k %in% argv

PORT     <- suppressWarnings(as.integer(get_opt("--port", "18980")))
if (is.na(PORT)) PORT <- 18980L
AUTOOPEN <- !has_flag("--no-open")
HOST     <- "127.0.0.1"
IS_WIN   <- .Platform$OS.type == "windows"

say <- function(...) { cat(enc2native(paste0(...)), "\n", sep = ""); flush(stdout()) }


if (!requireNamespace("jsonlite", quietly = TRUE)) {
  say("")
  say("[!] 缺少 jsonlite 包，这个助手依赖它读写配置。")
  say("    先装：  install.packages('jsonlite')")
  say("")
  quit(save = "no", status = 1)
}

# Port ownership is checked before schema/migration work. Production restarts
# were burning ~34s of CPU per attempt before discovering that a healthy
# instance already held the port. Exit immediately in that case.
STARTUP_GUARD_FILE <- file.path(ENGINE_DIR, "startup_guard.R")
if (file.exists(STARTUP_GUARD_FILE)) source(STARTUP_GUARD_FILE, local = TRUE)
if (!isTRUE(getOption("pa.serve.definitions_only", FALSE)) &&
    exists("startup_probe_existing", mode = "function") &&
    isTRUE(startup_probe_existing(PORT))) {
  say("[init] Healthy instance already listening on port ", PORT, "; exiting 0.")
  quit(save = "no", status = 0)
}

PLAN_FILE <- file.path(ENGINE_DIR, "step_plan.R")
CHAIN_FILE <- file.path(ENGINE_DIR, "run_chain.R")
local({
  old <- getOption("pa.chain.definitions_only")
  options(pa.chain.definitions_only = TRUE)
  on.exit(options(pa.chain.definitions_only = old))
  sys.source(CHAIN_FILE, envir = parent.env(environment()))
})
ENTRY_FILE <- file.path(ENGINE_DIR, "sc_workflow.R")
PICK_FILE <- file.path(ENGINE_DIR, "pick.R")
GUARD_DIR <- file.path(ENGINE_DIR, "workflow")
for (f in c(PLAN_FILE, CHAIN_FILE, ENTRY_FILE, PICK_FILE, file.path(GUARD_DIR, "guard.R"))) {
  if (!file.exists(f)) {
    say("")
    say("[!] 缺少运行文件：", f)
    say("    这个文件夹可能没拷完整，请重新解压一份。")
    say("")
    quit(save = "no", status = 1)
  }
}
# 载入「每一步跑什么」的唯一出处（同时拿到 read_cfg / cfg_val / pjoin 等）
source(PLAN_FILE, local = TRUE)
# 载入查表：网页上的「脚本库」页签与自动判定都用它，不另写一份
source(PICK_FILE, local = TRUE)
# 载入流程定义：「这套流程按什么顺序、用什么脚本、每个阶段内部有哪些环节」。
#   给助手的指令从它生成，质检也拿它对照 —— 两边同一个出处，才谈得上没有偏移。
FLOW_FILE <- file.path(ENGINE_DIR, "flow_spec.R")
HAS_FLOW <- file.exists(FLOW_FILE)
if (HAS_FLOW) source(FLOW_FILE, local = TRUE)
ENTRY_RUNNER <- ENTRY_FILE

# ---- 位置：本机 / 服务器 -------------------------------------------
#   place.R 是「在哪台机器上跑」的唯一出处：页面读它、页面写它、
#   点「保存选择」时按它决定去建哪个目录、连哪台机器。
#   分开写两份的后果是页面以为在跑 A、实际在跑 B，而这种错查起来很贵。
PLACE_FILE <- file.path(ENGINE_DIR, "place.R")
HAS_PLACE <- file.exists(PLACE_FILE)
if (HAS_PLACE) source(PLACE_FILE, local = TRUE)

# 历史 / 回退，脚本存取与审阅 -----------------------------------
#   这两个文件是可选的：老版本整包里没有它们。
#   缺了就降级（接口少一块），不能让整个服务起不来 ——
#   服务起不来，用户连"为什么"都看不到。
HISTORY_FILE <- file.path(ENGINE_DIR, "history.R")
SCRIPTS_FILE <- file.path(ENGINE_DIR, "script_ops.R")
# 多份分析记录（新建 / 历史 / 存档回滚）。
#   为什么单独一个文件：它是「这一次分析」这件事的唯一出处 ——
#   页面刷新后靠它知道自己停在哪、看的是哪一份、那一次的识别结果是什么。
#   混进 serve.R 会让"记录"和"页面收发"两件事互相牵制。
SESSIONS_FILE <- file.path(ENGINE_DIR, "sessions.R")
# 脚本存档（自动入库）：每次环节验收后把实际跑过的脚本登记进库。
#   为什么单独一个文件：它是「脚本库会自己长大」这条路的唯一写入方，
#   和 script_ops.R（换脚本/审脚本）职责不同，混在一起会让"登记"和"改文件"互相牵制。
ARCHIVE_FILE <- file.path(ENGINE_DIR, "script_archive.R")
HAS_HISTORY <- file.exists(HISTORY_FILE)
HAS_SCRIPT_OPS <- file.exists(SCRIPTS_FILE)
HAS_SESSIONS <- file.exists(SESSIONS_FILE)
HAS_SCRIPT_ARCHIVE <- file.exists(ARCHIVE_FILE)
# 决策：跑到那一步才生成问题、才发给人。
#   为什么不复用 gates.json：那份是初始化时把九条一次性抄进 run-dir 的，
#   本质上是"提前有清单"，正是要改掉的做法。详见 engine/decisions.R 抬头。
DECISIONS_FILE <- file.path(ENGINE_DIR, "decisions.R")
HAS_DECISIONS <- file.exists(DECISIONS_FILE)
if (HAS_HISTORY) source(HISTORY_FILE, local = TRUE)
if (HAS_SCRIPT_OPS) source(SCRIPTS_FILE, local = TRUE)
if (HAS_SCRIPT_ARCHIVE) source(ARCHIVE_FILE, local = TRUE)
if (HAS_SESSIONS) source(SESSIONS_FILE, local = TRUE)
if (HAS_DECISIONS) source(DECISIONS_FILE, local = TRUE)

# 会话意图：把「一段话」变成一条记录的校验与落地。
#   依赖 sessions.R（要建记录），所以必须在它之后加载。
ANNOT_REVIEW_FILE <- file.path(ENGINE_DIR, "annotation_review.R")
if (file.exists(ANNOT_REVIEW_FILE)) source(ANNOT_REVIEW_FILE, local = TRUE)
EVIDENCE_MERGE_FILE <- file.path(ENGINE_DIR, "evidence_merge.R")
if (file.exists(EVIDENCE_MERGE_FILE)) source(EVIDENCE_MERGE_FILE, local = TRUE)

DECISION_GATE_FILE <- file.path(ENGINE_DIR, "decision_gate.R")
if (file.exists(DECISION_GATE_FILE)) source(DECISION_GATE_FILE, local = TRUE)

EVIDENCE_CHECK_FILE <- file.path(ENGINE_DIR, "evidence_check.R")
if (file.exists(EVIDENCE_CHECK_FILE)) source(EVIDENCE_CHECK_FILE, local = TRUE)

# 覆盖判定单独成文件：要能用真实反例逐条测，不能内联在流程拼装里。
COVER_RULES_FILE <- file.path(ENGINE_DIR, "cover_rules.R")
if (file.exists(COVER_RULES_FILE)) source(COVER_RULES_FILE, local = TRUE)
# 产物清单补录准入：跨 req 不得改写、bytes 不可变、新条目须有验收绑定。
ART_SUPP_FILE <- file.path(ENGINE_DIR, "artifact_supplement.R")
if (file.exists(ART_SUPP_FILE)) source(ART_SUPP_FILE, local = TRUE)
# 活跃执行保护：终态回写必须绑定在管任务的 exec_id，
#   未绑定的 done/failed 一律拒收，也不许改 started_at。
# 图的小表守恒审计：单独一份，便于用真实表逐条测正反例。
FIGS_AUDIT_FILE <- file.path(ENGINE_DIR, "figs_audit.R")
HAS_FIGS_AUDIT <- file.exists(FIGS_AUDIT_FILE)
if (HAS_FIGS_AUDIT) source(FIGS_AUDIT_FILE, local = TRUE)
# 新鲜度豁免：只有一个窄口子（pack 复用 PPT 的来源清单）。
FRESHNESS_FILE <- file.path(ENGINE_DIR, "freshness.R")
HAS_FRESHNESS <- file.exists(FRESHNESS_FILE)
if (HAS_FRESHNESS) source(FRESHNESS_FILE, local = TRUE)

# pack 那个 ZIP 的归属判定：绑定本次数字作业，而不是跨机比较时间。
PACK_BIND_FILE <- file.path(ENGINE_DIR, "pack_binding.R")
HAS_PACK_BINDING <- file.exists(PACK_BIND_FILE)
if (HAS_PACK_BINDING) source(PACK_BIND_FILE, local = TRUE)
EXEC_GUARD_FILE <- file.path(ENGINE_DIR, "exec_guard.R")
HAS_EXEC_GUARD <- file.exists(EXEC_GUARD_FILE)
if (HAS_EXEC_GUARD) source(EXEC_GUARD_FILE, local = TRUE)
# 误报纠正（候选）：只处理"无任何执行证据的失败投影"，旧 attempt 文件不删不改。
EXEC_RECOVER_FILE <- file.path(ENGINE_DIR, "exec_recover.R")
HAS_EXEC_RECOVER <- file.exists(EXEC_RECOVER_FILE)
if (HAS_EXEC_RECOVER) source(EXEC_RECOVER_FILE, local = TRUE)

INTENT_FILE <- file.path(ENGINE_DIR, "intent_bridge.R")
HAS_INTENT <- file.exists(INTENT_FILE)
if (HAS_INTENT) source(INTENT_FILE, local = TRUE)
# 应用身份名（app id）从本机配置读，必须先于任何问网关的地方加载。
APP_IDENTITY_FILE <- file.path(ENGINE_DIR, "app_identity.R")
if (file.exists(APP_IDENTITY_FILE)) source(APP_IDENTITY_FILE, local = TRUE)
PLATFORM_IDENTITY_FILE <- file.path(ENGINE_DIR, "platform_identity.R")
if (file.exists(PLATFORM_IDENTITY_FILE)) source(PLATFORM_IDENTITY_FILE, local = TRUE)
APP_HANDOFF_FILE <- file.path(ENGINE_DIR, "app_handoff.R")
if (file.exists(APP_HANDOFF_FILE)) source(APP_HANDOFF_FILE, local = TRUE)

# 并行记录：多条分析同时在跑时，页面要知道各自跑到哪。
RUNS_FILE <- file.path(ENGINE_DIR, "runs.R")
HAS_RUNS <- file.exists(RUNS_FILE)
if (HAS_RUNS) source(RUNS_FILE, local = TRUE)

# 可复现 notebook：读 cell 流水、汇编、写文件。
NOTEBOOK_FILE <- file.path(ENGINE_DIR, "notebook_build.R")
HAS_NOTEBOOK <- file.exists(NOTEBOOK_FILE)
if (HAS_NOTEBOOK) source(NOTEBOOK_FILE, local = TRUE)
# 按记录汇编 ipynb：给「生成执行快照 / 生成 notebook / 脚本库下载 ipynb」用。
#   为什么单独一个文件：它读的是「这条记录实际登记过哪些脚本」，
#   不是执行端实时推来的流水。两条来源不同，混在一起会让"没有流水"被当成"没有东西可导出"。
NB_RECORD_FILE <- file.path(ENGINE_DIR, "notebook_from_record.R")
HAS_NB_RECORD <- file.exists(NB_RECORD_FILE)
if (HAS_NB_RECORD && HAS_NOTEBOOK) source(NB_RECORD_FILE, local = TRUE)
REPLAY_FILE <- file.path(ENGINE_DIR, "notebook_replay.R")
if (file.exists(REPLAY_FILE)) source(REPLAY_FILE, local = TRUE)
REPLAY_API_FILE <- file.path(ENGINE_DIR, "notebook_replay_api.R")
if (file.exists(REPLAY_API_FILE)) source(REPLAY_API_FILE, local = TRUE)

# 脚本溯源与一致性核对。
#   为什么必须有它：上一轮"重写脚本"时，注释声称参照了原始脚本，
#   实际参照的是上一轮自己写的那份，于是把原始版没有的写法带了进来，
#   连续两轮没人发现。没有溯源，跑通了就会被当成跑对了。
AUDIT_FILE <- file.path(ENGINE_DIR, "audit.R")
HAS_AUDIT <- file.exists(AUDIT_FILE)
if (HAS_AUDIT) source(AUDIT_FILE, local = TRUE)

# 每一步的质检：生成自己的质检文件、检查上一环的质检文件。
#   这一版只记账不拦路（blocking=FALSE），先让账存在，再谈要不要卡。
QUALITY_FILE <- file.path(ENGINE_DIR, "quality.R")
HAS_QUALITY <- file.exists(QUALITY_FILE)
if (HAS_QUALITY) source(QUALITY_FILE, local = TRUE)

# 环节级 reviewer：每环两审一护照（2026-11-05）。
#   为什么要有它：判定能力本来分散在 verdict / quality / reviewer / compliance
#   四处，各判各的、各存各的。于是"这一环该不该开"没人判（派了再说），
#   "产物能不能交给下一环"四处结论不一致，"放行了哪些文件"根本没人记。
#   这一个模块把"开跑前"与"跑完后"收进同一个地方，产出可举证的护照，
#   下一环只认上一环护照里的放行清单，不再看磁盘上恰好有什么。
STAGE_REVIEWER_FILE <- file.path(ENGINE_DIR, "stage_reviewer.R")
HAS_STAGE_REVIEWER <- file.exists(STAGE_REVIEWER_FILE)
if (HAS_STAGE_REVIEWER) source(STAGE_REVIEWER_FILE, local = TRUE)

# 注释依据（参考表 / marker 词典）的管理。
#   为什么单独一个模块：参考表不是"一份内置表就够"的东西 ——
#   换组织、换物种、换细胞类型范围，依据就不同。所以它必须是
#   可上传、可按分析记录选用、可下发的资产，而不是写死在代码里的路径。
REFERENCE_FILE <- file.path(ENGINE_DIR, "reference.R")
HAS_REFERENCE <- file.exists(REFERENCE_FILE)
if (HAS_REFERENCE) source(REFERENCE_FILE, local = TRUE)

# 对外台账：应用生成"合规的台账行"，执行端负责原样追加到服务器流水。
#   为什么分成这一层：reviewer.R 要检查 set_id / run_dir / code / 指纹 / 阶段名，
#   只写 run_id+step+event 的流水一定被判 REFUSED。
#   应用知道语义（阶段属于哪套 skill、该有哪些产物），
#   执行端知道事实（跑没跑成、退出码、文件在不在），各写各的那一半。
LEDGER_OUT_FILE <- file.path(ENGINE_DIR, "ledger_out.R")
HAS_LEDGER_OUT <- file.exists(LEDGER_OUT_FILE)
if (HAS_LEDGER_OUT) source(LEDGER_OUT_FILE, local = TRUE)

# 对账：同一份"阶段名单"在本项目里被写成好几份，这里是唯一能回答
#   "现在到底有几份说法、差在哪、该改哪一段"的地方。
# 产物镜像：服务器上的图与表格取回本地，人才能在应用里直接看。
#   不加载它，产物就永远只是"文件名 + 大小 + 需从服务器取"。
MIRROR_FILE <- file.path(ENGINE_DIR, "mirror.R")
HAS_MIRROR <- file.exists(MIRROR_FILE)
if (HAS_MIRROR) source(MIRROR_FILE, local = TRUE)
source(file.path(ENGINE_DIR, "fetch_client.R"), local = TRUE)

CONTRACT_FILE <- file.path(ENGINE_DIR, "contract.R")
HAS_CONTRACT <- file.exists(CONTRACT_FILE)
if (HAS_CONTRACT) source(CONTRACT_FILE, local = TRUE)

# PPT 生成：素材已取回本机之后，在本机直接出交付 PPT。
#   不加载它，「生成 PPT」就只能靠执行端跑，一旦那边没 python / 模板缺，
#   按钮点下去就停在那，人看到的是"点了没反应"。
PPT_BUILD_FILE <- file.path(ENGINE_DIR, "ppt_build.R")
HAS_PPT_BUILD <- file.exists(PPT_BUILD_FILE)
if (HAS_PPT_BUILD) source(PPT_BUILD_FILE, local = TRUE)

# 脚本来源判定：服务器 / 自带库 / 待编写，三层一次说清。
#   不加载它，"这一环该用哪个脚本"只能靠人看红条猜。
SCRIPT_RESOLVE_FILE <- file.path(ENGINE_DIR, "script_resolve.R")
HAS_SCRIPT_RESOLVE <- file.exists(SCRIPT_RESOLVE_FILE)
if (HAS_SCRIPT_RESOLVE) source(SCRIPT_RESOLVE_FILE, local = TRUE)

# ---- 数据层：SQLite 是唯一事实来源 --------------------------------
#   为什么在 serve.R 里加载，而不是在每个用到的地方各 source 一次：
#     同一个函数被两处加载会出现两份闭包，调试时改了 A 处的对象、
#     实际跑的是 B 处那份 —— 这类问题比语法错误难查得多。
#   加载顺序：db.R 先（其余三个都依赖它）。
DB_FILE <- file.path(ENGINE_DIR, "db.R")
VERDICT_FILE <- file.path(ENGINE_DIR, "verdict.R")
REWORK_FILE <- file.path(ENGINE_DIR, "rework.R")
POLLER_FILE <- file.path(ENGINE_DIR, "poller.R")
DB_API_FILE <- file.path(ENGINE_DIR, "db_api.R")
MIGRATE_HISTORY_FILE <- file.path(ENGINE_DIR, "migrate_history.R")
WRITEBACK_FILE <- file.path(ENGINE_DIR, "writeback_identity.R")
HTTP_STATIC_FILE <- file.path(ENGINE_DIR, "http_static.R")
STARTUP_GUARD_FILE <- file.path(ENGINE_DIR, "startup_guard.R")
HAS_DB <- file.exists(DB_FILE)
HAS_VERDICT <- file.exists(VERDICT_FILE)
# 助手代写的那道门：令牌签发与校验。
#   必须在 db_api.R 之前加载 —— db_api_owner 在解析身份时会先问它。
#   问不到就退回原来的 cookie 链，所以这个文件缺失只是降级，不是崩溃。
AGENT_GATE_FILE <- file.path(ENGINE_DIR, "agent_gate.R")
HAS_AGENT_GATE <- file.exists(AGENT_GATE_FILE)
if (HAS_AGENT_GATE) source(AGENT_GATE_FILE, local = TRUE)
# Worker identity is deliberately separate from agent tokens: a worker may take
# queue/mirror work, but must never inherit the agent's record-write authority.
WORKER_AUTH_FILE <- file.path(ENGINE_DIR, "worker_auth.R")
HAS_WORKER_AUTH <- file.exists(WORKER_AUTH_FILE)
if (HAS_WORKER_AUTH) source(WORKER_AUTH_FILE, local = TRUE)
if (HAS_DB) source(DB_FILE, local = TRUE)
if (file.exists(MIGRATE_HISTORY_FILE)) source(MIGRATE_HISTORY_FILE, local = TRUE)
if (file.exists(WRITEBACK_FILE)) source(WRITEBACK_FILE, local = TRUE)
if (file.exists(HTTP_STATIC_FILE)) source(HTTP_STATIC_FILE, local = TRUE)
if (file.exists(STARTUP_GUARD_FILE)) source(STARTUP_GUARD_FILE, local = TRUE)
SERVER_POOL_FILE <- file.path(ENGINE_DIR, "server_pool.R")
if (file.exists(SERVER_POOL_FILE)) source(SERVER_POOL_FILE, local = TRUE)
if (HAS_VERDICT) source(VERDICT_FILE, local = TRUE)
if (file.exists(REWORK_FILE)) source(REWORK_FILE, local = TRUE)
if (file.exists(POLLER_FILE)) source(POLLER_FILE, local = TRUE)
if (file.exists(DB_API_FILE)) source(DB_API_FILE, local = TRUE)
# 管理员与两个隔离开关。
#   必须排在 record_access.R 之前：记录权限判定要问「隔离开着吗」，
#   而那个问题的答案只在这个文件里读一次。
ADMIN_POLICY_FILE <- file.path(ENGINE_DIR, "admin_policy.R")
HAS_ADMIN_POLICY <- file.exists(ADMIN_POLICY_FILE)
if (HAS_ADMIN_POLICY) source(ADMIN_POLICY_FILE, local = TRUE)

RECORD_ACCESS_FILE <- file.path(ENGINE_DIR, "record_access.R")
if (file.exists(RECORD_ACCESS_FILE)) source(RECORD_ACCESS_FILE, local = TRUE)
# Shared execution admission path for /api/run, /api/agent/dispatch and
# post-commit intent auto_dispatch. Keep the full gate sequence in one file.
RUN_ENQUEUE_FILE <- file.path(ENGINE_DIR, "run_enqueue.R")
HAS_RUN_ENQUEUE <- file.exists(RUN_ENQUEUE_FILE)
if (HAS_RUN_ENQUEUE) source(RUN_ENQUEUE_FILE, local = TRUE)
# 合规检查器：reviewer.R 降级后的产物，只回答「用的 skill 与脚本对不对」。
COMPLIANCE_FILE <- file.path(ENGINE_DIR, "compliance.R")
if (file.exists(COMPLIANCE_FILE)) source(COMPLIANCE_FILE, local = TRUE)

# 平台能力探针：只回答「能不能无激活注入」，三档，未知不当已知（A5）。
ENV_PROBE_FILE <- file.path(ENGINE_DIR, "env_probe.R")
if (file.exists(ENV_PROBE_FILE)) source(ENV_PROBE_FILE, local = TRUE)

# 数据画像 / 环境判定 / 编排：F1、F7、F6、G1、G2、G4 落在三个文件里。
for (.f in c("dataset_profile.R", "runnable.R", "orchestrator.R")) {
  .p <- file.path(ENGINE_DIR, .f)
  if (file.exists(.p)) source(.p, local = TRUE)
}
rm(.f, .p)

# 步骤顺序（用于判断"从哪一步往后要重跑"）。
#   取现成的 CHAIN_ORDER，不在服务端另抄一份，避免两处不一致。
# 顺序只认 flow_spec（本应用真正在跑的那条链）。
#   原来取的是 CHAIN_ORDER —— step_plan.R 里老流程的八个名字
#   （env/decide/precheck/pipeline1/pipeline2/plots/ppt/pack）。
#   本流程是 doublet→…→enrich，两边名字一个都对不上，于是"从哪一步往后
#   要重跑"永远算错：拿一个不相交的名单去 match，结果恒为 NA。
#   这类"抄一份放在附近"的地方，就是对账器要盯的对象。
STEP_ORDER <- if (HAS_FLOW) {
  tryCatch(FLOW_ORDER("server"), error = function(e) character(0))
} else {
  tryCatch(CHAIN_ORDER, error = function(e) character(0))
}

# =====================================================================
# 状态（只记运行目录；其余都在 run-dir 下的文件里）
# =====================================================================
write_json_file <- function(path, obj) {
  # sessions.R is loaded before request handling; use its verified atomic writer
  # for view state and execution queues as well, never truncate a live JSON file.
  if (!exists("sess_write_json", mode = "function"))
    stop("atomic_json_writer_unavailable")
  sess_write_json(path, obj)
}

read_state <- function() {
  if (!file.exists(STATE_FILE)) return(list(runDir = NULL))
  st <- tryCatch(jsonlite::fromJSON(read_text_any(STATE_FILE), simplifyVector = FALSE),
                 error = function(e) list(runDir = NULL))
  rd <- st[["runDir", exact = TRUE]]
  if (is.null(rd) || !length(rd) || !nzchar(as.character(rd))) st["runDir"] <- list(NULL)
  st
}
save_state <- function(st) write_json_file(STATE_FILE, st)

# =====================================================================
# 步骤状态与日志（读 run_chain.R 写下的文件；不在内存里另建一套）
# =====================================================================
status_dir  <- function(runDir) file.path(runDir, "status")
step_state_file <- function(runDir) file.path(status_dir(runDir), "step_state.json")
step_log_file   <- function(runDir) file.path(status_dir(runDir), "run.log")

read_step_state <- function(runDir) {
  f <- step_state_file(runDir)
  if (!file.exists(f)) return(list(status = "idle", step = NULL, code = NULL))
  st <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                 error = function(e) list(status = "idle", step = NULL, code = NULL))
  if (is.null(st$status) || !nzchar(as.character(st$status))) st$status <- "idle"
  if (is.null(st$code) || !length(st$code)) st$code <- NULL
  st
}

# 判断「还在跑」不能只看状态文件：进程可能被强杀，文件来不及更新。
# 拿 pid 探一下，进程没了就把状态回落，避免界面一直卡在「运行中」。
step_is_running <- function(runDir) {
  st <- read_step_state(runDir)
  if (!identical(as.character(st$status), "running")) return(FALSE)
  pid <- suppressWarnings(as.integer(st$pid %||% NA))
  if (is.na(pid)) return(FALSE)
  alive <- if (IS_WIN) {
    out <- tryCatch(suppressWarnings(system2("tasklist", c("/FI", paste0("PID eq ", pid), "/NH"),
                                            stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    any(grepl(as.character(pid), out))
  } else {
    file.exists(file.path("/proc", as.character(pid)))
  }
  if (!alive) {
    write_json_file(step_state_file(runDir),
                    list(status = "stopped", step = st$step, code = NULL,
                         started = st$started, ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    return(FALSE)
  }
  TRUE
}

# =====================================================================
#  服务器模式：某一步交给助手执行
#
#  为什么要有这一段：
#    服务器模式的运行目录在服务器上，本机起不了进程。以前点「开始这一步」
#    只回一句"请到助手那边跑"，等于把这个按钮做成了死路 —— 用户点下去
#    得到的就是一条报错。
#    这一步本来就不该由应用执行，但也不能没有出口：
#    点了就把它变成一条任务交给助手，助手跑完把结果回写到这里，
#    页面上的状态随回写变化，而不是永远停在「空闲」。
# =====================================================================
REMOTE_STEP_FILE <- file.path(STATE_DIR, "remote_steps.json")

# =====================================================================
# 执行队列：人在应用里点一下，就有东西真的去跑
# ---------------------------------------------------------------------
#  为什么需要它：
#    应用装在云端这台机器上，它连不到 10.1.1.2（22 端口拒绝）。
#    真正能连服务器的是工作电脑。所以"点击即执行"只能是：
#      应用把这次点击记成一条待办 → 工作电脑上的执行端取走 → 在服务器上跑 → 写回应用。
#    以前点按钮只会往会话里发一句话，通道不通就只是入助手队列，
#    助手没接就永远不动 —— 人看到的就是"点了没反应"。
#  与助手队列的分工：
#    助手队列（injections.json）是"给会话发消息"；
#    这个队列是"给执行端派活"，两者互不干扰。
EXEC_FILE <- file.path(STATE_DIR, "exec_tasks.json")
# 执行端心跳：工作电脑上的 worker 每轮都盖一次时间戳。
#   为什么要它：执行端跑在别人的电脑上，应用这边看不见它。
#   没有心跳，"点了没反应"就分不清是"没人取活"还是"取了在跑"。
EXEC_HB_FILE <- file.path(STATE_DIR, "exec_heartbeat.json")
# 阶段三起心跳按 worker 分文件；旧的全局文件只在迁移期留作证据，不再作为
# 在线判据。任何伪造 by 字段的请求都无法改动这里的 worker 文件。
EXEC_HB_DIR <- file.path(STATE_DIR, "exec_heartbeats")
EXEC_HB_FRESH_SECS <- 180L
EXEC_WORKER_OFFLINE_SECS <- 30L * 60L

exec_hb_dir <- function() {
  d <- get0("EXEC_HB_DIR", ifnotfound = "")
  if (is.character(d) && length(d) == 1L && nzchar(d)) d else
    file.path(STATE_DIR, "exec_heartbeats")
}

exec_worker_file_id <- function(worker_id) {
  w <- trimws(as.character(worker_id %||% ""))
  if (!nzchar(w)) return("")
  gsub("[^A-Za-z0-9._:-]", "_", w)
}

exec_worker_hb_path <- function(worker_id) {
  w <- exec_worker_file_id(worker_id)
  if (!nzchar(w)) return("")
  file.path(exec_hb_dir(), paste0(w, ".json"))
}

exec_default_pool_id <- function() {
  if (exists("worker_default_pool", mode = "function"))
    return(tryCatch(worker_default_pool(APP_DIR), error = function(e) "default"))
  p <- trimws(Sys.getenv("PA_WORKER_POOL", unset = ""))
  if (nzchar(p)) p else "default"
}

exec_parse_ts <- function(x) {
  s <- trimws(as.character(x %||% ""))
  if (!nzchar(s)) return(as.POSIXct(NA))
  suppressWarnings(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S"))
}

exec_worker_hb_read <- function(worker_id) {
  p <- exec_worker_hb_path(worker_id)
  if (!nzchar(p) || !file.exists(p)) return(NULL)
  x <- tryCatch(jsonlite::fromJSON(read_text_any(p), simplifyVector = FALSE),
                error = function(e) NULL)
  if (!is.list(x)) return(NULL)
  x$worker_id <- as.character(x$worker_id %||% worker_id)
  x
}

exec_worker_hb_all <- function() {
  d <- exec_hb_dir()
  if (!dir.exists(d)) return(list())
  fs <- list.files(d, pattern = "[.]json$", full.names = TRUE)
  out <- list()
  for (f in fs) {
    x <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.list(x) && nzchar(as.character(x$worker_id %||% ""))) out[[length(out) + 1L]] <- x
  }
  out
}

exec_worker_hb_write <- function(worker_id, pool_id = NULL, version = "", note = "",
                                 now = Sys.time()) {
  w <- worker_clean_id(worker_id, "worker_id")
  p <- worker_clean_id(pool_id %||% worker_default_pool(APP_DIR), "pool_id")
  hb <- list(worker_id = w, pool_id = p, at = exec_ts(now),
             version = as.character(version %||% ""),
             note = as.character(note %||% ""))
  write_json_file(exec_worker_hb_path(w), hb)
  hb
}

exec_latest_heartbeat <- function(worker_id = "", pool_id = "", hbs = NULL) {
  if (is.null(hbs)) hbs <- exec_worker_hb_all()
  w <- trimws(as.character(worker_id %||% ""))
  p <- trimws(as.character(pool_id %||% ""))
  best <- NULL; bt <- as.POSIXct(NA)
  for (hb in hbs) {
    if (nzchar(w) && !identical(as.character(hb$worker_id %||% ""), w)) next
    if (nzchar(p) && !identical(as.character(hb$pool_id %||% ""), p)) next
    t0 <- exec_parse_ts(hb$at)
    if (is.na(t0)) next
    if (is.null(best) || is.na(bt) || t0 > bt) { best <- hb; bt <- t0 }
  }
  best
}

exec_heartbeat_age <- function(hb, now = Sys.time()) {
  if (is.null(hb)) return(Inf)
  t0 <- exec_parse_ts(hb$at)
  if (is.na(t0)) return(Inf)
  as.numeric(difftime(now, t0, units = "secs"))
}

exec_heartbeat_online <- function(hb, now = Sys.time(), fresh = EXEC_HB_FRESH_SECS) {
  age <- exec_heartbeat_age(hb, now)
  is.finite(age) && age < as.numeric(fresh)
}

# ---- 心跳在线 ≠ 真的会干活 -------------------------------------------
#   为什么必须分开判（2026-11-22 实测）：
#     pc-worker 的心跳每 60 秒准时到，页面就一直显示"执行端在线"，
#     但队列里最后一次真领活的 claimed_at 停在 11-20 09:37，
#     之后两天里它一次 claim 都没发过 —— 只是空转报活。
#   于是预检回答"一切正常"，任务永远 queued，人只能一个个报错才发现。
#   这里补一个独立事实：最后一次真的领活是什么时候、隔了多久。
#   判据只用队列里的 claimed_at，不猜进程、不看心跳，因为心跳已经证明会骗人。
exec_claim_last <- function(d = NULL) {
  if (is.null(d)) d <- tryCatch(exec_read(), error = function(e) list(items = list()))
  best_at <- ""; best_ts <- as.POSIXct(NA); best_id <- ""
  for (it in (d$items %||% list())) {
    v <- trimws(as.character(it$claimed_at %||% ""))
    if (!nzchar(v)) next
    ts <- exec_parse_ts(v)
    if (is.na(ts)) next
    if (is.na(best_ts) || ts > best_ts) { best_ts <- ts; best_at <- v; best_id <- as.character(it$id %||% "") }
  }
  list(at = best_at, id = best_id, ts = best_ts,
       age_secs = if (is.na(best_ts)) NA_real_ else
         as.numeric(difftime(Sys.time(), best_ts, units = "secs")))
}

exec_task_pool <- function(it) {
  p <- trimws(as.character(it$pool_id %||% ""))
  if (nzchar(p)) p else exec_default_pool_id()
}


# ---------------------------------------------------------------------
#  队列写锁：读-改-写必须是一个不可分割的动作
#
#  为什么必须有（2026-11-23 用两个独立进程实测复现）：
#    两个执行端同时领任务时，两边都打印了「已领到」，文件里最后只剩后写的那个。
#    过程是 A 读、B 读、A 整份写回、B 整份写回 —— A 的改动被 B 的旧快照覆盖。
#    能触发它的场景不止两台电脑：同一台机器装了两个执行端、或者 cron 里的
#    回收器与服务端同时写，都是跨进程写同一个文件。
#
#  为什么用文件锁而不是版本号重试：
#    这里所有写者都在同一台主机上（服务端、回收器 cron、执行端收尾），
#    文件锁能覆盖全部，而乐观锁要在每个调用点都写一遍重试逻辑。
#    锁用目录实现（mkdir 原子），带陈旧锁回收：持有者进程已不在或锁过期太久就清掉，
#    否则一次崩溃会让队列永久不可写。
#
#  为什么要有进程内重入计数：
#    回收器会调用 rc_callback，回调路径里可能再进一次队列写入。
#    同进程第二次拿锁若直接失败，就变成自己把自己锁死。计数只解决同进程重入，
#    跨进程仍然严格互斥。
.exec_lock_state <- new.env(parent = emptyenv())
.exec_lock_state$depth <- 0L

exec_lock_dir <- function() paste0(EXEC_FILE, ".lock")

exec_lock <- function(timeout_secs = 20, stale_secs = 120) {
  if (isTRUE(.exec_lock_state$depth > 0L)) {
    .exec_lock_state$depth <- .exec_lock_state$depth + 1L
    return(TRUE)
  }
  lk <- exec_lock_dir()
  dir.create(dirname(lk), showWarnings = FALSE, recursive = TRUE)
  deadline <- Sys.time() + timeout_secs
  repeat {
    if (dir.create(lk, mode = "0700", showWarnings = FALSE)) {
      tryCatch(cat(as.character(Sys.getpid()), file = file.path(lk, "pid")),
               error = function(e) NULL)
      .exec_lock_state$depth <- 1L
      return(TRUE)
    }
    mt <- tryCatch(file.info(lk)$mtime, error = function(e) NA)
    stale <- FALSE
    if (!is.na(mt)) {
      age <- as.numeric(difftime(Sys.time(), mt, units = "secs"))
      if (is.finite(age) && age > stale_secs) stale <- TRUE
    }
    if (!stale) {
      holder <- tryCatch(suppressWarnings(
        as.integer(readLines(file.path(lk, "pid"), warn = FALSE)[1])),
        error = function(e) NA_integer_)
      # 只有能确认持有者已死才抢锁；读不到 pid 时宁可等，避免误清掉正在用的锁。
      #   /proc 只在 Linux 有。没有它就只靠 stale_secs 兜底，
      #   否则在其它平台上每一把锁都会被判成"持有者已死"，等于没有锁。
      has_proc <- dir.exists("/proc")
      if (has_proc && !is.na(holder) && holder > 0L &&
          !dir.exists(file.path("/proc", as.character(holder))))
        stale <- TRUE
    }
    if (stale) { unlink(lk, recursive = TRUE, force = TRUE); next }
    if (Sys.time() > deadline) return(FALSE)
    Sys.sleep(0.02)
  }
}

exec_unlock <- function() {
  if (isTRUE(.exec_lock_state$depth > 1L)) {
    .exec_lock_state$depth <- .exec_lock_state$depth - 1L
    return(invisible(FALSE))
  }
  .exec_lock_state$depth <- 0L
  unlink(exec_lock_dir(), recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

exec_with_lock <- function(fn, timeout_secs = 20) {
  if (!isTRUE(exec_lock(timeout_secs))) stop("exec_queue_busy")
  on.exit(exec_unlock(), add = TRUE)
  fn()
}

# 在锁内读、改、写。fn 收 d 返回 list(data = <新队列>, value = <可选返回值>)。
exec_mutate <- function(fn, timeout_secs = 20) {
  exec_with_lock(function() {
    d <- exec_read()
    out <- fn(d)
    dd <- if (is.list(out) && !is.null(out$data)) out$data else out
    val <- if (is.list(out)) out$value else NULL
    exec_write(dd)
    val
  }, timeout_secs)
}

exec_read <- function() {
  if (!file.exists(EXEC_FILE)) return(list(items = list()))
  x <- tryCatch(jsonlite::fromJSON(read_text_any(EXEC_FILE), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || is.null(x$items)) return(list(items = list()))
  x
}
exec_write <- function(d) write_json_file(EXEC_FILE, d)
exec_ts <- function(t = Sys.time()) format(t, "%Y-%m-%dT%H:%M:%S")

# ---------------------------------------------------------------------
#  失联回收：一直在 running、但执行端早就不报信的任务，收尾成 failed
#
#  为什么必须有（2026-11-03，审核第 4 条后半）
#    state/exec_tasks.json 里有一条从 10-28 14:31 起就是 running 的任务，
#    last_seen 停在 11-02，exec_heartbeat.json 停在 10-27。
#    执行端早就不在了，任务还挂在那里 —— 页面上它与"正在跑"长得一样，
#    没有机制把它推出去。这与 fullmark 那个 133 小时是同一类病：
#    状态是个单向门，进去就出不来。
#
#  为什么收成 failed 而不是新加一个 stalled 状态
#    前端只认 queued/running/done/failed 四种。加第五种要同步改所有展示点，
#    漏一处就会显示成空白。用 failed 并写清原因，信息不丢：
#    error 里会说明"执行端失联、未回写、这不是任务本身失败"，
#    重跑一次即可，返工单也能据此生成。
#
#  阈值取 2 小时
#    慢的那几环（final 实测约 3 小时）全程都靠 keepalive 每 60 秒推 last_seen，
#    2 小时没有任何心跳是"人没了"，不是"活得慢"。
# ---------------------------------------------------------------------
EXEC_STALE_SECS <- 2L * 3600L
# 排队没人领的阈值。
#   为什么必须单独一条：exec_reap 原来只回收 running，而"排队中"没有任何过期，
#   于是一条 21:14 入队、从头到尾没人 claim 的任务会永远挂在 queued 上。
#   后果不是它自己失败，而是它把整个分析的重跑入口锁死 ——
#   run_chain.R 里那个"旧任务仍在执行或排队，等待它退出后才能安全重跑"的 409
#   就是它发出来的。用户看到的就是点一键重跑一直被打回，而屏幕上没有任何
#   在跑的东西。排队正常几秒到几分钟内就会被领走，半小时没人领只有一个解释：
#   执行端根本没在盯这个队列。
# queued 的唯一过期规则是「执行端离线」。在线时排多久都保留；从未上线
# 才退回任务创建时间。24 小时绝对期限已按 SPEC-V2 删除。
EXEC_QUEUE_STALE_SECS <- EXEC_WORKER_OFFLINE_SECS
# 排队多久没人认领就不再算"有人在管"—— 只用来标记 stuck，不自动回收。
#   为什么只标记、不自动杀（2026-11-22，gina 报队列死锁）：
#     现象是「一条 queued 任务没人认领，后续环节永远派不出去，界面也删不掉」。
#     但直接按"排队超时即回收"去做会把正常排队的任务一起杀掉：
#     队列是单执行端的，前一条 final 实测跑三小时，后面的任务本来就该一直 queued。
#   所以这里只回答一个问题（这条是不是看起来没人管），
#   处置交给两处：/api/exec 把它标成 stuck 显示出来，/api/exec/cancel 给人工作废。
EXEC_QUEUE_UNCLAIMED_SECS <- 30L * 60L

exec_reap_audit_path <- function() file.path(STATE_DIR, "exec_reaper_audit.jsonl")
exec_reap_audit_append <- function(rec) {
  f <- exec_reap_audit_path()
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  tryCatch(cat(paste0(jsonlite::toJSON(rec, auto_unbox = TRUE), "\n"),
               file = f, append = TRUE), error = function(e) NULL)
  invisible(TRUE)
}

# 回收器本身的状态。
#   为什么要单算一份：回收是个后台定时任务，它坏死时不会有任何报错冒到页面上 ——
#   表现只是"队列里那条一直不动"。那种故障没法从队列本身看出来，
#   因为队列看不见"谁在管它"。所以把回收器的最后一次扫描时间也摆出来，
#   人一眼就能分辨"没人回收"和"没人接活"。
exec_reaper_status <- function(now = Sys.time(), expect_secs = 300L) {
  f <- exec_reap_audit_path()
  lastScan <- NULL
  reapedRecent <- list()
  lines <- character(0)
  if (file.exists(f)) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    lines <- lines[nzchar(trimws(lines))]
  }
  if (length(lines)) {
    # 只回看末尾若干行：审计是追加写的，越靠后越新。
    for (x in rev(utils::tail(lines, 200L))) {
      j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.list(j)) next
      kd <- as.character(j$kind %||% "")
      if (identical(kd, "scan") && is.null(lastScan)) { lastScan <- j; next }
      if (kd %in% c("manual_cancel", "scan_error") ||
          (is.numeric(j$reaped) && j$reaped > 0) ||
          (is.list(j$ids) && length(j$ids) > 0)) {
        reapedRecent[[length(reapedRecent) + 1L]] <- j
        if (length(reapedRecent) >= 5L) break
      }
    }
  }
  at <- as.character((lastScan %||% list())$at %||% "")
  age <- if (nzchar(at)) {
    t0 <- exec_parse_ts(at)
    if (is.na(t0)) Inf else as.numeric(difftime(now, t0, units = "secs"))
  } else Inf
  list(
    last_scan_at = if (nzchar(at)) at else NULL,
    last_scan_age_secs = if (is.finite(age)) round(age) else NULL,
    # alive 的判据是"最近三倍期望间隔内扫过"。单次没扫到不等于坏了，
    #   但连着三倍间隔没动静，就说明它没在跑，这时候队列里的 waiting 不该被相信。
    alive = is.finite(age) && age < as.numeric(expect_secs) * 3,
    expect_secs = as.numeric(expect_secs),
    last_scanned = if (!is.null(lastScan)) as.integer(lastScan$scanned %||% 0L) else NULL,
    last_reaped = if (!is.null(lastScan)) as.integer(lastScan$reaped %||% 0L) else NULL,
    # 调度是否存在，跟"最近有没有扫过"是两件事。
    #   为什么必须分开（2026-11-23，云准助手那台实测）：
    #   那台没有任何 cron、没有 timer，回收器从来没被独立调度过。
    #   它偶尔有扫描记录，是因为有人打开页面、请求里顺手调了一次 exec_reap。
    #   于是"看起来在跑"，实际上没人打开页面就永远不跑。
    #   只报 alive 不足以说明这件事，必须同时报"有没有调度"。
    schedule = tryCatch(exec_reaper_schedule_check(), error = function(e) NULL),
    recent = reapedRecent)
}

# ---- 回收器的独立调度：检测 + 缺失时补上 ---------------------------
#   为什么要管调度（2026-11-23，gina 指出"回收器从未跑"不能就这么留着）：
#     回收逻辑写在 exec_reap 里，但它过去只在有请求进来时被顺手调用。
#     没有 cron 的部署上，这个功能等于不存在，而且界面上看不出来 ——
#     直到某条任务该被回收却一直挂着，才发现没人扫。
#   做法：检查 crontab 里有没有这条；没有就追加一条，已有的不动。
#     为什么追加而不是覆盖：这台机器上还有别人配的任务，覆盖等于替别人做主。
#     为什么失败也要记下来：补不上就得让人看见"补不上"，
#     否则又是一个静默失效。
exec_reaper_cron_line <- function(app_dir = APP_DIR) {
  paste0("*/5 * * * * cd ", shQuote(app_dir),
         " && /usr/bin/Rscript engine/exec_reaper.R >> state/reaper_cron.log 2>&1")
}

exec_reaper_schedule_check <- function(app_dir = APP_DIR, now = Sys.time(),
                                       expect_secs = 300L) {
  # 判据一（主）：审计里有没有 kind=scan。
  #   为什么不用 crontab 命令（2026-11-23 实测踩到）：
  #     crontab 二进制是 setgid 的，而本服务的 systemd unit 开了
  #     NoNewPrivileges=true —— 两者冲突，服务进程里调 crontab 必然失败。
  #     于是检测恒为"缺失"，还会误判成"需要人工配置"，把本来好的部署报成坏的。
  #   而 kind=scan 只有独立入口 exec_reaper.R 会写：
  #     内联的 exec_reap 只记回收动作，不记 scan。
  #   所以它是"有没有独立调度在跑"的直接证据，不依赖任何外部命令。
  f <- exec_reap_audit_path()
  scan_at <- ""
  if (file.exists(f)) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    lines <- lines[nzchar(trimws(lines))]
    for (x in rev(utils::tail(lines, 500L))) {
      j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.list(j)) next
      if (identical(as.character(j$kind %||% ""), "scan")) {
        scan_at <- as.character(j$at %||% "")
        break
      }
    }
  }
  if (nzchar(scan_at)) {
    t0 <- exec_parse_ts(scan_at)
    age <- if (is.na(t0)) Inf else as.numeric(difftime(now, t0, units = "secs"))
    # ok 的含义是"存在独立调度"，不是"刚才扫过"。
    #   两件事必须分开：调度配过但坏了（有记录、不新鲜），和从来没配过（无记录）。
    #   前者看 alive，后者看这里。混成一个布尔值会让"刚重启还没到点"被误判成没配。
    # fresh 用与 alive 同一个判据：expect_secs * 3。
    #   原来这里写 24 小时，而 cron 是 5 分钟一次、alive 用 15 分钟 ——
    #   同一份代码两套阈值，cron 停 23 小时照样报 fresh=true、"在跑"，
    #   等于只把"曾经"从三天缩成一天（2026-11-23，云准助手复验时指出）。
    #   现在两处共用一个阈值，看板与接口不会再各说各话。
    return(list(ok = TRUE, source = "audit_scan", last_scan_at = scan_at,
                age_secs = if (is.finite(age)) round(age) else NULL,
                fresh = is.finite(age) && age < as.numeric(expect_secs) * 3,
                fresh_window_secs = as.numeric(expect_secs) * 3,
                checked_at = exec_ts(now)))
  }
  # 判据二（兜底）：crontab 文件。服务里通常读不到，读不到就是未知，按缺失处理。
  u <- Sys.getenv("USER", unset = "")
  if (!nzchar(u)) u <- as.character(Sys.info()[["user"]] %||% "")
  txt <- character(0)
  if (nzchar(u)) {
    p <- file.path("/var/spool/cron/crontabs", u)
    txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) character(0))
  }
  has <- any(grepl("exec_reaper\\.R", txt))
  list(ok = has, source = if (has) "crontab_file" else "none",
       last_scan_at = NULL, checked_at = exec_ts(now))
}

exec_reaper_ensure_schedule <- function(app_dir = APP_DIR) {
  chk <- tryCatch(exec_reaper_schedule_check(app_dir), error = function(e) list(ok = FALSE))
  if (isTRUE(chk$ok))
    # fresh 一起透传：调用方要能区分"配过且在跑"和"配过但停了"。
    #   只回一个 ok，cron 停三天也会报"已配置"，等于把"曾经"当"现在"。
    return(list(ok = TRUE, added = FALSE,
                source = as.character(chk$source %||% ""),
                fresh = isTRUE(chk$fresh),
                age_secs = chk$age_secs,
                checked_at = exec_ts()))
  # 到这里说明真的一条独立扫描都没有。尝试补上，补不上就明确记下来。
  #   为什么补不上是正常的：NoNewPrivileges 下 crontab 调不动。
  #   所以这条路径的重点不是"一定补上"，而是"补不上要留下准确的说明和命令"，
  #   让人一次配好，而不是反复看到一句含糊的"调度缺失"。
  cur <- tryCatch(system2("crontab", "-l", stdout = TRUE, stderr = FALSE),
                  error = function(e) NULL)
  if (is.null(cur)) cur <- character(0)
  cur <- cur[nzchar(trimws(cur)) & !grepl("^#", cur)]
  cur <- c(cur, exec_reaper_cron_line(app_dir))
  tmp <- tempfile("pa-cron-")
  writeLines(cur, tmp)
  st <- tryCatch(suppressWarnings(system2("crontab", tmp)), error = function(e) 1L)
  unlink(tmp)
  ok <- identical(as.integer(st), 0L)
  rec <- list(checked_at = exec_ts(), ok = ok, added = TRUE,
              source = if (ok) "crontab_written" else "none",
              cron_line = exec_reaper_cron_line(app_dir),
              note = if (ok) "已追加回收器调度"
                     else "服务进程内写不了 crontab（NoNewPrivileges 与 setgid 冲突），需要部署方加一条")
  tryCatch(write_json_file(file.path(STATE_DIR, "reaper_schedule.json"), rec),
           error = function(e) NULL)
  list(ok = ok, added = TRUE, checked_at = rec$checked_at, note = rec$note,
       cron_line = rec$cron_line)
}

# ---- 检测不到执行端时，主动发起"启动执行端"请求 -------------------
#   为什么要有（gina 2026-11-22 明确要求：执行端要被应用检测并启动，
#   检测不到就启动安装）：
#     应用自己跑在 Linux 运行体里，看不到也起不了工作电脑上的进程 ——
#     这是硬边界，不假装能直接 spawn。但它能做三件比报错有用的事：
#       ① 检测：心跳新鲜度就是唯一判据；
#       ② 定位：从 server_profile 取出该在哪台电脑上装；
#       ③ 主动交代：把"装什么、在哪装、装完怎么起"写成一条待办交出去，
#          而不是让人对着"没有执行端在线"这六个字自己猜。
#   为什么必须节流：一次点击触发一次请求，连点十下就是十条重复指令。
#     十分钟内只发一条，重复请求只回上一次的时间。
exec_bootstrap_request <- function(reason = "dispatch", now = Sys.time(),
                                   throttle_secs = 600L) {
  f <- file.path(STATE_DIR, "exec_bootstrap.json")
  prev <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                   error = function(e) NULL)
  prev_count <- if (is.list(prev)) as.integer(prev$count %||% 0L) else 0L
  if (is.list(prev)) {
    t0 <- exec_parse_ts(prev$requested_at %||% "")
    if (!is.na(t0) && as.numeric(difftime(now, t0, units = "secs")) < throttle_secs)
      return(list(ok = FALSE, code = "throttled",
                  requested_at = as.character(prev$requested_at %||% ""),
                  exec_pc = as.character(prev$exec_pc %||% ""),
                  count = prev_count))
  }
  prof <- tryCatch(jsonlite::fromJSON(read_text_any(
    file.path(APP_DIR, "config", "server_profile.json")), simplifyVector = FALSE),
    error = function(e) NULL)
  pc <- as.character(((prof %||% list())$server %||% list())$exec_pc %||% "")
  txt <- paste0(
    "【启动执行端】应用检测到没有任何执行端在线，队列里的任务不会被领取。\n",
    if (nzchar(pc)) paste0("执行电脑：", pc, "\n") else "",
    "在那台电脑上执行一次（已装过会自动跳过，可以重复执行）：\n",
    "  python pa_exec_worker.py --install\n",
    "再运行固定位置里的启动脚本：\n",
    "  Windows：%LOCALAPPDATA%\\CatsCo\\pa-exec\\run-worker.cmd\n",
    "  Linux/macOS：~/.local/share/catsco/pa-exec/run-worker.sh\n",
    "启动后 60 秒内这里会变成在线，队列会自动开始走；不要重复点派工。")
  sid <- if (exists("sess_current_id", mode = "function"))
    tryCatch(sess_current_id(APP_DIR), error = function(e) "") else ""
  q <- tryCatch(inject_enqueue(text = txt, session = sid,
                               reason = "exec_bootstrap", origin = "启动执行端"),
                error = function(e) NULL)
  rec <- list(requested_at = exec_ts(now), reason = as.character(reason %||% ""),
              exec_pc = pc, count = prev_count + 1L,
              queue_id = if (is.null(q)) NULL else as.character(q$id %||% ""))
  tryCatch(write_json_file(f, rec), error = function(e) NULL)
  list(ok = TRUE, requested_at = rec$requested_at, exec_pc = pc,
       count = rec$count, queue_id = rec$queue_id)
}

exec_reap_ts <- function(it) {
  for (nm in c("last_seen", "claimed_at", "created_at")) {
    v <- as.character(it[[nm]] %||% "")
    if (nzchar(v)) return(list(at = v, source = nm, ts = exec_parse_ts(v)))
  }
  list(at = "", source = "", ts = as.POSIXct(NA))
}

exec_reap <- function(d = NULL, now = Sys.time(), audit = TRUE) {
  if (is.null(d)) d <- exec_read()
  items <- d$items %||% list()
  if (!length(items)) return(d)
  hbs <- tryCatch(exec_worker_hb_all(), error = function(e) list())
  reaped <- list()
  changed <- FALSE
  for (i in seq_along(items)) {
    it <- items[[i]]
    st <- as.character(it$status %||% "")
    if (!identical(st, "running") && !identical(st, "queued")) next
    ts0 <- exec_reap_ts(it)
    pool <- exec_task_pool(it)
    wid <- trimws(as.character(it$worker_id %||% ""))
    hb <- if (nzchar(wid)) exec_latest_heartbeat(worker_id = wid, hbs = hbs) else NULL
    if (is.null(hb)) hb <- exec_latest_heartbeat(pool_id = pool, hbs = hbs)
    hb_age <- exec_heartbeat_age(hb, now)
    reason <- ""; code <- ""
    if (identical(st, "running")) {
      if (is.na(ts0$ts)) next
      gap <- as.numeric(difftime(now, ts0$ts, units = "secs"))
      if (gap < EXEC_STALE_SECS) next
      code <- "executor_lost"
      reason <- paste0(
        "执行端失联：最后一次报信是 ", ts0$at, "，已 ", round(gap / 3600, 1),
        " 小时没有任何更新，任务未回写。这不是这一步本身跑失败 —— ",
        "它在服务器上的产物是否已生成，需要重新核一次。重跑这一步即可。")
    } else {
      # queued 的过期规则保持"只看执行端在不在线"，刻意不按排队时长回收。
      #   为什么不能按时长杀（2026-11-22 二次确认）：
      #     这个队列是单执行端的。一条 final 实测要跑三小时，它跑着的时候
      #     后面的任务本来就应该一直 queued。按"没人认领满 N 分钟就回收"
      #     去判，会把正常排队的任务误判成死任务，把好作业杀掉。
      #   "执行端在、心跳新鲜、但根本不轮询队列"这种情况不在这里处理：
      #     它靠 /api/exec 的 stuck 标记呈现，并给人工作废入口（/api/exec/cancel）。
      #     自动杀与"不许杀正常排队任务"这两件事无法同时成立，只能选后者。
      if (!is.null(hb)) {
        if (!is.finite(hb_age) || hb_age < as.numeric(EXEC_WORKER_OFFLINE_SECS)) next
        code <- "worker_offline"
        reason <- paste0(
          "执行端已离线：该 pool 最后一次心跳是 ",
          as.character(hb$at %||% ""), "，已 ", round(hb_age / 60, 0),
          " 分钟；queued 任务未开始，按离线规则回收。这不是任务本身跑失败。")
      } else {
        created <- exec_parse_ts(it$created_at)
        if (is.na(created)) next
        gap <- as.numeric(difftime(now, created, units = "secs"))
        if (gap < as.numeric(EXEC_WORKER_OFFLINE_SECS)) next
        code <- "worker_never_seen"
        reason <- paste0(
          "从未观测到可接该任务的执行端：任务已排队 ", round(gap / 60, 0),
          " 分钟，pool=", pool, " 没有任何心跳。这不是任务本身跑失败。")
      }
    }
    items[[i]]$status <- "failed"
    items[[i]]$finished_at <- exec_ts(now)
    items[[i]]$error <- reason
    items[[i]]$stale_reaped <- TRUE
    items[[i]]$reap_reason_code <- code
    items[[i]]$reap_previous_status <- st
    items[[i]]$reap_ts <- ts0$at
    items[[i]]$reap_ts_source <- ts0$source
    reap_wid <- if (nzchar(wid)) wid else as.character((hb %||% list())$worker_id %||% "")
    items[[i]]$reap_worker_id <- if (nzchar(reap_wid)) reap_wid else NULL
    items[[i]]$reap_pool_id <- pool
    items[[i]]$reap_heartbeat_at <- if (is.null(hb)) NULL else as.character(hb$at %||% "")
    reaped[[length(reaped) + 1L]] <- list(
      at = exec_ts(now), id = as.character(it$id %||% ""), previous_status = st,
      worker_id = if (nzchar(reap_wid)) reap_wid else NULL, pool_id = pool,
      heartbeat_at = if (is.null(hb)) NULL else as.character(hb$at %||% ""),
      reason_code = code, error = reason)
    changed <- TRUE
  }
  if (changed) {
    # 写回必须在锁内：回收器是独立 cron 进程，服务端同时在处理执行端请求，
    #   两个进程各持一份旧快照整份写回，谁后写谁把对方的改动抹掉。
    exec_mutate(function(dd) {
      # 锁内重读，用最新条目做替换，避免把锁外这段时间别人的改动覆盖掉。
      fresh <- dd$items %||% list()
      for (i in seq_along(fresh)) {
        id <- as.character(fresh[[i]]$id %||% "")
        hit <- Filter(function(x) identical(as.character(x$id %||% ""), id), items)
        if (length(hit)) fresh[[i]] <- hit[[1]]
      }
      list(data = list(items = fresh))
    })
    if (isTRUE(audit)) for (rec in reaped) exec_reap_audit_append(rec)
    for (it in items) if (isTRUE(it$stale_reaped) && nzchar(as.character(it$chain_id %||% "")))
      rc_callback(list(session_id = it$session_id, req_id = it$chain_id,
        step = it$step, status = "failed", code = 1L), FALSE)
  }
  attr(d, "reaped") <- reaped
  d
}

exec_enqueue <- function(step, session_id = "", note = "", script = "", script_args = "", req_id = "",
                         params = NULL, params_rel = "", run_dir = "", mode = "",
                         reference_id = "", ref_mode = "", ref_path = "", pool_id = NULL,
                         prepare = NULL) {
  # 整个"读-去重-追加-写"必须在锁内。
  #   为什么不能只锁写：去重是靠读到的现状判断的。两个执行端/两个点击同时进来，
  #   各自读到"还没有同 step+session 的任务"，就各自追加一条，
  #   于是同一步出现两个任务号、写同一个输出目录（2026-11-22 实测的那对
  #   ex-20261122151017-qugm 与 ex-20261122173829-nhlu 就是这么来的）。
  exec_with_lock(function() {
  d <- exec_read()
  # 持久幂等：同一个 session+step+req_id 不论当前是 queued/running/done/failed
  # 都只返回原任务。要重跑必须换新 req_id；这条比旧的「仅 queued/running
  # 去重」更严格，避免 auto_dispatch 重试时在终态后再生一条。
  rq <- trimws(as.character(req_id %||% ""))
  if (nzchar(rq)) {
    for (it in (d$items %||% list())) {
      if (!identical(as.character(it$step %||% ""), step)) next
      if (!identical(as.character(it$session_id %||% ""), session_id)) next
      if (!identical(trimws(as.character(it$req_id %||% "")), rq)) next
      # 作废过、被回收过的条目不算"原任务"，不能让同一请求号拿回它。
      #   为什么必须跳过（2026-11-22 实测）：作废后重派，如果这里返回了那条
      #   failed 的旧条目，调用方就以为活派出去了，实际队列里什么都没有。
      #   链式派工尤其致命 —— 链的 req_id 是全链共用的，一旦拿回作废那条，
      #   这条链就永远卡在同一个失败条目上，再点多少次都派不出去。
      #   跳过它 = 允许同一个请求号在作废后重新入队，这才是"重派即可"的真实语义。
      if (nzchar(as.character(it$cancelled_by %||% ""))) next
      if (isTRUE(it$stale_reaped)) next
      return(it)
    }
  }
  # 同一环节同一分析已有排队/在跑的就不再重复入队：连点两下不该跑两遍。
  #   这一段原来被 `if (!nzchar(rq))` 包着，而 run_enqueue 一定先生成 req_id
  #   再入队，于是这道去重对主路径永久失效 —— 同一步在排队期间再点一次，
  #   就生成第二个执行任务号，两个任务写同一个输出目录。
  #   2026-11-22 实测到 ex-20261122151017-qugm 与 ex-20261122173829-nhlu
  #   指向同一个 1.doublet 目录，就是这条的后果。
  #   现在不论有没有 req_id，只要同 step+session 还有 queued/running 就复用那一条，
  #   并把这次的请求号记进 attempted_req_ids 留痕（回写按实际执行的那条认账）。
  for (it in (d$items %||% list())) {
    if (!identical(as.character(it$step %||% ""), step)) next
    if (!identical(as.character(it$session_id %||% ""), session_id)) next
    if (!as.character(it$status %||% "") %in% c("queued", "running")) next
    keep <- it
    if (nzchar(rq)) {
      seen <- as.character(unlist(it$attempted_req_ids %||% character(0)))
      if (!rq %in% seen) {
        exec_set(as.character(it$id %||% ""),
                 list(attempted_req_ids = as.list(c(seen, rq))))
        keep <- exec_by_id(as.character(it$id %||% "")) %||% it
      }
    }
    return(keep)
  }
  pool <- trimws(as.character(pool_id %||% ""))
  if (!nzchar(pool)) pool <- exec_default_pool_id()
  it <- list(
    id = paste0("ex-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                paste(sample(c(letters, 0:9), 4, replace = TRUE), collapse = "")),
    step = step, session_id = session_id, note = note,
    # 这一步该跑哪个脚本、带什么参数：应用算好随任务下发。
    #   执行端不需要再自己查"步骤->脚本"表（那张表和应用里的定义早就会对不上）。
    script = as.character(script %||% ""),
    script_args = as.character(script_args %||% ""),
    # 回写绑定用的请求号。
    #   为什么必须记在任务上：执行端回写 /api/step/import 时，
    #   req_id 是从这条任务里取的（worker.py: item.req_id or item.chain_id）。
    #   不记的话它只能发空串，而回写接口要求非空且 attempt 已登记 ——
    #   于是作业在集群上真跑完了，应用这边永远收不到结果。
    #   实测就是这条：ex-20261113100134-nn9f 的 req_id 与 chain_id 都是 null。
    req_id = if (nzchar(as.character(req_id %||% ""))) as.character(req_id) else NULL,
    # 结构参数：由应用算好、随任务下发，执行端在 qsub 之前写成文件。
    #   为什么必须走这条路：筛群脚本只认参数文件，而应用主机看不到集群
    #   /share，写不了那个文件；执行端能写但原来没人把参数交给它。
    #   只在有参数时挂字段，避免给其它步骤凭空多一个空 params。
    params = if (is.list(params) && length(params)) params else NULL,
    params_rel = if (nzchar(as.character(params_rel %||% ""))) as.character(params_rel) else NULL,
    # The queue item must carry the record context it was authorized against;
    # the worker must never infer it from current/global state.
    run_dir = as.character(run_dir %||% ""),
    mode = as.character(mode %||% ""),
    reference_id = as.character(reference_id %||% ""),
    ref_mode = as.character(ref_mode %||% ""),
    ref_path = as.character(ref_path %||% ""),
    pool_id = pool,
    worker_id = NULL,
    status = "queued", created_at = exec_ts(),
    claimed_at = NULL, claimed_by = NULL, finished_at = NULL, error = NULL)
  # Publish the job only after its writeback identity has been prepared.
  if (!is.null(prepare)) {
    prepared <- prepare(it)
    if (!isTRUE(prepared$ok)) return(list(dispatch_error = prepared))
    it <- c(it, prepared$fields)
  }
  d$items <- c((d$items %||% list()), list(it))
  exec_write(d)
  it
  })
}

exec_by_req <- function(step, session_id, req_id) {
  st <- trimws(as.character(step %||% ""))
  sd <- trimws(as.character(session_id %||% ""))
  rq <- trimws(as.character(req_id %||% ""))
  if (!nzchar(st) || !nzchar(sd) || !nzchar(rq)) return(NULL)
  for (it in (tryCatch(exec_read(), error = function(e) list(items = list()))$items %||% list())) {
    if (identical(as.character(it$step %||% ""), st) &&
        identical(as.character(it$session_id %||% ""), sd) &&
        identical(trimws(as.character(it$req_id %||% "")), rq)) return(it)
  }
  NULL
}

exec_by_id <- function(id) {
  want <- trimws(as.character(id %||% ""))
  if (!nzchar(want)) return(NULL)
  for (it in (tryCatch(exec_read(), error = function(e) list(items = list()))$items %||% list()))
    if (identical(as.character(it$id %||% ""), want)) return(it)
  NULL
}

exec_by_job_number <- function(job_number, step = "", session_id = "") {
  want <- trimws(as.character(job_number %||% ""))
  if (!nzchar(want)) return(NULL)
  for (it in (tryCatch(exec_read(), error = function(e) list(items = list()))$items %||% list())) {
    if (!identical(trimws(as.character(it$job_number %||% "")), want)) next
    if (nzchar(step) && !identical(as.character(it$step %||% ""), step)) next
    if (nzchar(session_id) && !identical(as.character(it$session_id %||% ""), session_id)) next
    return(it)
  }
  NULL
}

exec_due <- function() {
  d <- exec_read()
  Filter(function(it) identical(as.character(it$status %||% ""), "queued"),
         (d$items %||% list()))
}

# 正在执行的任务。
#   为什么执行端也要拿到这一份：只报"待执行"等于只回答了一半问题。
#   没有待执行时，执行端必须还能看到"我手上有哪些正在跑" ——
#   否则作业跑完没人收尾（结果不写回），中途被杀也没人发现，
#   只能等下一次有人点按钮、恰好又把这条捡起来。这正是 14:03 那次
#   提交后彻底失联的原因。
exec_running <- function() {
  d <- exec_read()
  Filter(function(it) identical(as.character(it$status %||% ""), "running"),
         (d$items %||% list()))
}

# 这一环是否已经有执行端在管（排队中或正在跑）。
#   为什么需要它：执行端是唯一提交者。只要队列里有这一条，
#   会话里就绝不能再出现"你去服务器上执行"这种话 ——
#   那等于给同一件事派了第二个执行者，两套结果谁对谁错没人能判。
exec_active_for <- function(step, session_id = "") {
  st <- as.character(step %||% "")
  sd <- as.character(session_id %||% "")
  if (!nzchar(st)) return(NULL)
  d <- tryCatch(exec_read(), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  for (it in (d$items %||% list())) {
    if (!identical(as.character(it$step %||% ""), st)) next
    if (nzchar(sd) && !identical(as.character(it$session_id %||% ""), sd)) next
    if (!as.character(it$status %||% "") %in% c("queued", "running")) next
    return(it)
  }
  NULL
}

# 这一环最近一次的执行任务，不看状态。
#   为什么要一个不筛状态的版本：只按 queued/running 判断，
#   作业一变成 done/failed，观察通知就会退回"你去执行"那一版。
#   而 done/failed 恰恰是最需要说清"这件事已经派过、别再来一遍"的时刻 ——
#   失败后助手若被允许重跑，就会绕过执行端和它的输出目录隔离。
#   所以观察与否只看"有没有派过"，绑定的 exec_id 由这里给出。
exec_latest_for <- function(step, session_id = "") {
  st <- as.character(step %||% "")
  sd <- as.character(session_id %||% "")
  if (!nzchar(st)) return(NULL)
  d <- tryCatch(exec_read(), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  best <- NULL; bt <- ""
  for (it in (d$items %||% list())) {
    if (!identical(as.character(it$step %||% ""), st)) next
    if (nzchar(sd) && !identical(as.character(it$session_id %||% ""), sd)) next
    t0 <- as.character(it$created_at %||% "")
    if (is.null(best) || t0 >= bt) { best <- it; bt <- t0 }
  }
  best
}

# 这一环最近一次执行任务若是明确 failed，就按失败呈现。
#   为什么要单独一个函数：exec_active_for 只看 queued/running，
#   于是一次"领取后被拒、根本没提交"的失败在页面变成了
#   exec_id=null + 「交接未确认，尚无执行回写」—— 事实是作业被明确拒绝，
#   页面上反而像没派出去，人会再点一次。
#   约束一：只看该步最新一条（exec_latest_for 按 created_at 取最大），
#   旧 req 的失败不能盖掉后派出去的新任务。
#   约束二：调用方给了当前 req 时，exec 上记的 req 必须与它一致；
#   对不上说明这条失败属于别的题次，结论不适用。
exec_failed_for <- function(step, session_id = "", req_id = "") {
  it <- tryCatch(exec_latest_for(step, session_id), error = function(e) NULL)
  if (is.null(it)) return(NULL)
  if (!identical(as.character(it$status %||% ""), "failed")) return(NULL)
  rq <- as.character(it$req_id %||% "")
  if (nzchar(req_id) && nzchar(rq) && !identical(rq, req_id)) return(NULL)
  it
}

# 把「队列最新状态」与「回写记录」合成一个可测结论。
#   抽出来的理由：这段判定决定页面写什么，要能一条条跑正反例，
#   埋在几千行的处理器里就只能靠人读。
#   规则：有在跑/在排队的任务就以它为准；没有才是明确失败；
#   回写已 done 的步骤不被失败覆盖（那是已经跑成过的记录）。
flow_exec_state <- function(exNow, exFail, rec) {
  recSt <- if (is.null(rec)) "" else as.character(rec$status %||% "")
  settled <- recSt %in% c("done", "verified")
  pick <- NULL; status <- ""
  if (!is.null(exNow)) {
    pick <- exNow; status <- as.character(exNow$status %||% "queued")
  } else if (!is.null(exFail) && !settled) {
    pick <- exFail; status <- "failed"
  }
  if (is.null(pick)) {
    return(list(status = if (is.null(rec)) "todo" else "",
                exec_id = NULL, exec_status = NULL,
                exec_job = NULL, exec_last_seen = NULL, exec_error = NULL))
  }
  err <- as.character(pick$error %||% "")
  list(status = status,
       exec_id = as.character(pick$id %||% ""),
       exec_status = status,
       exec_job = as.character(pick$job_id %||% ""),
       exec_last_seen = as.character(pick$last_seen %||% ""),
       exec_error = if (nzchar(err)) err else NULL)
}

# 一条 exec 任务能否作为某次回写的绑定依据。
#   单独抽出来是为了能测：这段判定决定"谁能写进哪条记录"，
#   埋在几千行的处理器里就只能靠人读，没法一条条跑。
#   条件一条都不能少：
#     · 任务在案（ex0 非空）
#     · 记录号与步骤号都非空
#     · 任务的 session 与回写的一致 —— 挡住"拿别的任务的 id 写这条记录"
#     · 任务的 step 与回写的一致 —— 挡住"拿 doublet 的任务给 merge 背书"
#     · 任务状态是四种真实状态之一
#     · 任务真的带了脚本（空脚本说明它没被正确下发过）
#     · 任务有创建时间（产物 mtime 要跟它比）
exec_compat_ok <- function(ex0, sid_in, stp_in) {
  if (is.null(ex0)) return(FALSE)
  sid_in <- trimws(as.character(sid_in %||% ""))
  stp_in <- trimws(as.character(stp_in %||% ""))
  if (!nzchar(sid_in) || !nzchar(stp_in)) return(FALSE)
  identical(as.character(ex0$session_id %||% ""), sid_in) &&
    identical(as.character(ex0$step %||% ""), stp_in) &&
    as.character(ex0$status %||% "") %in% c("queued", "running", "done", "failed") &&
    nzchar(trimws(as.character(ex0$script %||% ""))) &&
    nzchar(trimws(as.character(ex0$created_at %||% "")))
}

# 已入队时进会话的那段：只观察、只解释、不另派。
#   和"要你去执行"那段必须由同一个函数产生，否则前端改了、brief 又重建执行正文，
#   两边说法不一致，人还是会被误导。
remote_step_observe_text <- function(step_id, sid = "", ex = NULL) {
  exid <- as.character((ex %||% list())$id %||% "")
  stt <- as.character((ex %||% list())$status %||% "")
  job <- as.character((ex %||% list())$job_id %||% "")
  att <- as.character((ex %||% list())$attempt_id %||% "")
  rid <- as.character((ex %||% list())$run_id %||% "")
  vid <- as.character((ex %||% list())$version_id %||% "")
  emode <- as.character((ex %||% list())$executor_mode %||% "")
  paste(c(
    "【来自 个性化分析助手】",
    paste0("要办的事：这一环已经由执行端接管，作业已提交，请你只做观察与解释。",
           "（阶段 id=", step_id, "）"),
    paste0("执行任务号：", if (nzchar(exid)) exid else "（未取到）",
           "；状态：", if (nzchar(stt)) stt else "未知",
           if (nzchar(job)) paste0("；集群作业名：", job) else ""),
    paste0("分析记录：", if (nzchar(as.character(sid %||% ""))) as.character(sid) else "（未指定）"),
    paste0("回写绑定（必须原样回传）：attempt_id=", if (nzchar(att)) att else "（未登记）",
           "；run_id=", if (nzchar(rid)) rid else "（未取到）",
           "；version_id=", if (nzchar(vid)) vid else "（未取到）",
           "；executor_mode=", if (nzchar(emode)) emode else "（未标注）"),
    "回写时 POST /api/step/import，body 必须带 attempt_id、session_id、req_id、step、version_id；",
    "不要把 qsub 作业号当 attempt_id，也不要让 login/observe 另生成一个编号。",
    "禁止另行提交作业：不要 qsub、不要手写脚本、不要在服务器上重跑同一步。",
    "已经在跑的时候再提交一次，会得到两套产物、覆盖同一个输出目录，事后无法判定哪份有效。",
    "你要做的三件事：一，按页面，或 GET /api/flow?session=",
    if (nzchar(as.character(sid %||% ""))) as.character(sid) else "<分析记录号>",
    " 说明真实进度（这条只读、会校验这份记录属于谁）；二，解释中间报错的原因；",
    "三，只在执行端明确失败、且人要求时，才提出补救方案（仍由执行端提交，不由你直接跑）。",
    "回写由执行端完成；你不必、也不要替它回写。"
  ), collapse = "\n")
}

# 执行端上报"我还活着、这个任务还在跑"。
#   不改变 status：它仍然是 running，只是把 last_seen 往前推。
#   判断"是否失联"只看这个时间戳，不看"等了多久" ——
#   等得久不等于失败，没人报信才是真的出问题了。
# 唯一作业号一旦登记，就不能被另一个号覆盖。
#   为什么必须这样：job_number 是"这份产物属于哪次作业"的唯一凭据。
#   如果后来的心跳可以随手把它改掉，那么一份旧产物只要再报一次心跳、
#   换上一个新号，就能冒充新作业的结果 —— 绑定判据等于被绕开。
#   所以同名号重复登记是幂等的，不同号一律冲突拒绝并保留原值。
exec_jobnum_merge <- function(prev, incoming) {
  p <- trimws(as.character(prev %||% ""))
  i <- trimws(as.character(incoming %||% ""))
  if (!nzchar(i)) {
    return(list(ok = TRUE, value = p, code = if (nzchar(p)) "kept" else "absent"))
  }
  if (!grepl("^[0-9]+$", i))
    return(list(ok = FALSE, code = "bad_jobnumber", value = p))
  if (!nzchar(p)) return(list(ok = TRUE, value = i, code = "set"))
  if (identical(p, i)) return(list(ok = TRUE, value = p, code = "same"))
  # 冲突：保留原值，不写新的
  list(ok = FALSE, code = "jobnumber_conflict", value = p)
}

exec_keepalive <- function(id, job = "", note = "", job_number = "",
                           worker_id = "", pool_id = "", now = Sys.time()) {
  # job_number 是 qsub 回执里的唯一数字号；job_id 是可重复的作业名。
  #   只存名字不够：名字按"步骤+时分秒"生成，同一分钟重试就会撞名。
  d <- exec_read()
  prevNum <- ""
  for (it in (d$items %||% list())) {
    if (identical(as.character(it$id %||% ""), id))
      prevNum <- as.character(it$job_number %||% "")
  }
  mg <- exec_jobnum_merge(prevNum, job_number)
  fields <- list(last_seen = exec_ts(now), job_id = as.character(job %||% ""),
                 note = as.character(note %||% ""))
  wid <- trimws(as.character(worker_id %||% ""))
  if (nzchar(wid)) fields$worker_id <- wid
  pid <- trimws(as.character(pool_id %||% ""))
  if (nzchar(pid)) fields$pool_id <- pid
  if (isTRUE(mg$ok) && nzchar(as.character(mg$value %||% "")))
    fields$job_number <- as.character(mg$value)
  if (!isTRUE(mg$ok) && identical(mg$code, "jobnumber_conflict")) {
    # 冲突保留原值，只留痕，不当成新作业。
    fields$job_number <- as.character(mg$value)
    fields$job_number_conflict <- trimws(as.character(job_number %||% ""))
    message("job_number_conflict ", id, " kept ", mg$value,
            " rejected ", trimws(as.character(job_number %||% "")))
  }
  exec_set(id, fields)
  invisible(mg)
}

exec_set <- function(id, fields) {
  # 读-改-写在锁内完成：两个执行端同时改不同条目时，谁后写谁整份覆盖，
  #   先写的那条改动会凭空消失（2026-11-23 实测）。
  exec_mutate(function(d) {
    for (i in seq_along(d$items)) {
      if (!identical(as.character(d$items[[i]]$id %||% ""), id)) next
      for (nm in names(fields)) d$items[[i]][[nm]] <- fields[[nm]]
    }
    list(data = d)
  })
  invisible(TRUE)
}


# =====================================================================
# C 类补充脚本：远端没有、必须由应用自带并按需下发的部分
# ---------------------------------------------------------------------
#  为什么要有这一类：
#     服务器上那套脚本是别人的，它里面缺的东西（少一个产物、少一张统计表、
#     少一次校验）不可能去改它的源码。补的那部分如果只在服务器上手写，
#     下次换个人跑就没有了 —— 没有出处、没有版本、事后无法复原。
#     所以补的部分必须留在应用里，用哪一次就下发哪一次。
#
#  为什么下发要带 md5：
#     既要防"服务器上残留的旧版本被当成这次的下发版"，也要防执行端
#     自己改一版顶替。校验不过就不许跑，不许"差不多就行"。
# =====================================================================
SUPPLEMENT_DIR <- file.path(ENGINE_DIR, "library", "C_supplement")

supplement_index <- function() {
  f <- file.path(SUPPLEMENT_DIR, "index.json")
  if (!file.exists(f)) return(list(entries = list()))
  x <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || is.null(x$entries)) return(list(entries = list()))
  x
}

# 某个阶段要用的补充脚本。status = draft 的不下发：口径没定，不能进正式流程。
supplement_for_stage <- function(stage_id) {
  idx <- supplement_index()
  Filter(function(e) {
    identical(as.character(e$for_stage %||% ""), as.character(stage_id)) &&
      !identical(as.character(e$status %||% "ready"), "draft")
  }, idx$entries)
}

# 这一步是否需要"本次才有的输入"（注释依据表）。
#   判据直接来自补充脚本的 argv 占位符，不另外维护一份清单 ——
#   另维护一份的后果是：流程定义改了、这里没改，于是"需要依据"这件事被漏掉，
#   人点一下就交出去，执行端拿着空占位符瞎跑。
supplement_needs_reference <- function(stage_id) {
  sup <- supplement_for_stage(stage_id)
  for (e in sup) {
    for (a in (e$argv %||% list())) {
      if (grepl("{reference_xlsx}", as.character(a), fixed = TRUE)) return(TRUE)
    }
  }
  FALSE
}

# 登记项里的 file 可能写成相对 library 的路径，也可能只写脚本名。
#   原来只按 ENGINE_DIR/library/<file> 拼一次，注释脚本登记成
#   sc-pipeline/scripts/02_annotate.R，而它实际住在
#   A_code/adapted/E1_windows_lowmem/D1_pipeline/ 下，
#   于是 404「条目登记了但文件不在」—— 不是没登记，是只用一条路径规则去找。
#   现在：先按登记路径找，找不到再按文件名在 library 下搜一次。
#   搜到多个同名文件不算数，宁可报歧义也不猜。
LIB_ROOT <- file.path(ENGINE_DIR, "library")

# 搜索根不止 library 一个。
#   引擎里本来就带着 sc-pipeline/scripts、sc-plot-kit/scripts 这些脚本，
#   它们不是"补充脚本"（不在 library/C_supplement 下），但登记表要能指到它们。
#   只搜 library 的后果：登记一条 sc-pipeline/scripts/02_annotate.R，
#   /api/supplement/get 直接 404，而文件就在引擎里 —— 看起来像"没登记"，
#   实际是"只用一条路径规则找"。
RESOLVE_ROOTS <- c(LIB_ROOT, ENGINE_DIR)

supplement_resolve <- function(rel) {
  rel <- as.character(rel %||% "")
  if (!nzchar(rel)) return(list(path = "", how = "empty", candidates = character(0)))

  # ① 按登记路径逐个根直接找
  for (root in RESOLVE_ROOTS) {
    direct <- file.path(root, rel)
    if (file.exists(direct)) return(list(path = direct, how = "direct", candidates = direct))
  }

  # ② 按文件名在各根下递归搜。搜到多个同名文件不算数，宁可报歧义也不猜。
  base <- basename(rel)
  pat <- paste0("^", gsub("([.\\\\^$|()\\[\\]{}*+?])", "\\\\\\1", base), "$")
  hit <- character(0)
  for (root in RESOLVE_ROOTS) {
    hit <- c(hit, tryCatch(list.files(root, pattern = pat, recursive = TRUE, full.names = TRUE),
                           error = function(e) character(0)))
  }
  hit <- unique(hit[!grepl("/\\.", hit)])
  if (length(hit) == 1L) return(list(path = hit, how = "byname", candidates = hit))
  list(path = "", how = if (length(hit) == 0L) "missing" else "ambiguous", candidates = hit)
}

supplement_path <- function(entry) {
  supplement_resolve(entry$file)$path
}

# 把 argv 模板里的 {占位符} 换成真值。占位符没有对应值就留着不换 ——
#   宁可让执行端看到未替换的占位符并停下来问，也不要静默替换成空串。
supplement_fill <- function(tmpl, V) {
  s <- as.character(tmpl %||% "")
  for (k in names(V)) {
    v <- as.character(V[[k]] %||% "")
    if (nzchar(v)) s <- gsub(paste0("\\{", k, "\\}"), v, s, fixed = FALSE)
  }
  s
}


# =====================================================================
# 待注入队列：指令没送进会话时，不能只丢一句"请自己复制"
# ---------------------------------------------------------------------
#  为什么要有这一段：
#     注入通道（页面 → 宿主会话）只在页面活着、且宿主把通道连上时才存在。
#     它断了的时候，以前的做法是把原文摆出来让人自己复制 —— 等于把
#     "交出去"这件事返工给用户。而指令里带着请求号、产物清单、命令，
#     手动复制粘贴一定出错。
#  所以改成：送不进去就入队，记下这次该投给哪个会话、属于哪个请求号，
#     并排下一次复查时间（默认 +1 小时）。之后每次页面活着、或者后端
#     被唤醒到点，都会再试一次；等这一步真的回写了，队列里的那条自动销项，
#     不再重发。
#  诚实边界：注入必须借着页面里那条宿主通道。页面全关的时候，
#     应用这边没有能把消息直接塞进会话的能力，只能把待投递记着，
#     一有机会（页面打开 / 到点复查）立刻补发。这一点写进状态里，不假装已经送达。
# =====================================================================
INJECT_FILE <- file.path(STATE_DIR, "injections.json")
INJECT_RETRY_SECS <- 3600          # 没送进去就隔这么久再试
# 重发不是无上限的：一直没人回写，说明问题不在"消息没送到"，
#   而在别处（助手没接、目标机器连不上、指令本身有错）。
#   到上限就停下并标成需要人工处理，免得一整天每小时往会话里塞同一段话。
MAX_INJECT_ATTEMPTS <- 6L

inject_read <- function() {
  if (!file.exists(INJECT_FILE)) return(list(items = list()))
  x <- tryCatch(jsonlite::fromJSON(read_text_any(INJECT_FILE), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || is.null(x$items)) return(list(items = list()))
  x
}

inject_write <- function(d) {
  keep <- lapply(d$items, function(it) {
    # 已销项且超过一天的条目不再留着：留着只会让状态越看越糊涂。
    it
  })
  d$items <- keep
  write_json_file(INJECT_FILE, d)
  invisible(TRUE)
}

inject_ts <- function(t) format(t, "%Y-%m-%dT%H:%M:%S")

# 会话号规范化：同一件事在系统里有两个写法（grp_5813 / cc_group_grp_5813），
#   从旧队列搬过来的那条就是缺前缀的写法，看起来像"另一个会话"。
#   不补前缀，面板上显示的会话号跟平台对不上，人也没法判断送去哪儿。
inject_norm_session <- function(s) {
  s <- trimws(as.character(s %||% ""))
  if (!nzchar(s)) return("")
  if (grepl("^cc_group_", s)) return(s)
  if (grepl("^grp_", s)) return(paste0("cc_group_", s))
  s
}

inject_enqueue <- function(text, session = "", req_id = "", reason = "", step = "", analysis_id = "", purpose = "execute", instruction_id = "", origin = "") {
  d <- inject_read()
  now <- Sys.time()
  session <- inject_norm_session(session)
  txt <- inject_with_origin(text, origin)

  # ---- 合并同一条，不重复入队 ----
  #   为什么必须合并：一次没送出去，重试几次就会攒出好几条一模一样的记录。
  #   面板上看起来像"有好几件事没送达"，实际是同一件事被记了多次；
  #   而且重试次数被摊到几条记录上，谁也到不了上限，反而一直重发。
  #   判据用「同阶段 + 同请求号 + 同正文」——正文一致就是同一条指令。
  for (i in seq_along(d$items)) {
    it <- d$items[[i]]
    if (!is.null(it$satisfied_at)) next
    # 作废过的记录不能再被新指令复用。
    #   否则新指令会静默继承那条的 dropped_at / 超限计数：面板上看不见它，
    #   系统也永远不会再发它 —— 指令凭空消失，比报错更难查。
    if (!is.null(it$dropped_at)) next
    if (!identical(sess_chr(it$instruction_id), sess_chr(instruction_id))) next
    if (!identical(sess_chr(it$analysis_id), sess_chr(analysis_id))) next
    if (!identical(sess_chr(it$purpose %||% "execute"), sess_chr(purpose))) next
    # 正文主体一致就是同一条：标题只是前缀，不该让同一条指令变成两条。
    if (!identical(inject_body_of(it$text), inject_body_of(txt))) next
    if (!identical(as.character(it$step %||% ""), as.character(step %||% ""))) next
    if (!identical(as.character(it$req_id %||% ""), as.character(req_id %||% ""))) next
    # 命中已有条目：只把会话号补正，不新增、不重置重试计数。
    d$items[[i]]$session <- if (nzchar(session)) session else as.character(it$session %||% "")
    if (nzchar(as.character(reason %||% ""))) d$items[[i]]$reason <- as.character(reason)
    if (nzchar(as.character(origin %||% ""))) {
      d$items[[i]]$origin <- as.character(origin)
      # 原来入库时没有按钮名、这次有：就地把正文升级成带首行的版本。
      #   不升级会留下两种首行：面板上显示的是旧的无标题正文，
      #   补发路径却按 origin 又重建一次首行，同一条记录两种样子。
      if (!identical(as.character(d$items[[i]]$text %||% ""), txt))
        d$items[[i]]$text <- txt
    }
    inject_write(d)
    return(d$items[[i]])
  }

  candidates <- if (HAS_SESSIONS && nzchar(req_id)) Filter(function(s) attempt_has(APP_DIR, s$id, req_id), sess_read(APP_DIR)$sessions) else list()
  if (!nzchar(sess_chr(analysis_id))) analysis_id <- if (length(candidates)==1L) candidates[[1]]$id else NULL
  it <- list(id = paste0("inj-", format(now, "%Y%m%d%H%M%S"), "-",
                         paste0(sample(c(letters, 0:9), 4, replace = TRUE), collapse = "")),
             text = txt,
             analysis_id = analysis_id, purpose = purpose, instruction_id = instruction_id,
             session = session,
             req_id = as.character(req_id %||% ""),
             step = as.character(step %||% ""),
             reason = as.character(reason %||% ""),
             # 按钮名。补发时靠它重建「【本次操作】」标题 ——
             #   否则队列里存的正文是内部指针，人看到就是一段不知道在说什么的话。
             origin = as.character(origin %||% ""),
             created_at = inject_ts(now),
             attempts = 0L,
             last_attempt_at = NULL,
             last_code = NULL,
             next_attempt_at = inject_ts(now + 0),      # 立刻可试一次
             delivered_at = NULL,
             satisfied_at = NULL)
  d$items <- c(d$items, list(it))
  inject_write(d)
  it
}

# 到点该重试的：没销项、且到了复查时间。
#   注意"送达过也要复查"：消息交给宿主只说明发出去了，不代表助手真去做了。
#   用户要的判定是"有没有回写"—— 回写没来，就一小时后再说一次。
inject_due <- function(now = Sys.time()) {
  d <- inject_read()
  since <- inject_ts(now)
  Filter(function(it) {
    if (!is.null(it$satisfied_at)) return(FALSE)
    # 已作废的绝不能再到点重发。
    #   踩过的坑：drop 只清了 next_attempt_at，没在 due 里排除；
    #   之后任意一次 attempt 又会把 next_attempt_at 写回未来时间，
    #   于是"作废"的条目一小时后照样被自动补发，作业等于没生效。
    if (!is.null(it$dropped_at)) return(FALSE)
    # 已被助手取走的，不能再按小时自动重发。
    if (!is.null(it$claimed_at)) return(FALSE)
    # 已经交给宿主的，不再自动重发。
    #   原来是"投递成功也排下一次复查"，理由是判据要看回写。这在有人盯着的时候
    #   说得通，但实测的形态是：页面刷新/切标签/窗口获得焦点都会触发一次补发，
    #   而每条补发又会生成新的指令包 -> 新队列项，8 分钟在会话里注入了 6 条同一件事。
    #   用户看到的就是"一刷新就往会话里灌"。
    #   现在：送达过的不再自动重发，只保留在队列里等回写；要再催一次由人点重试。
    if (!is.null(it$delivered_at)) return(FALSE)
    if (as.integer(it$attempts %||% 0L) >= MAX_INJECT_ATTEMPTS) return(FALSE)
    nxt <- as.character(it$next_attempt_at %||% "")
    nzchar(nxt) && nxt <= since
  }, d$items)
}

# 记一次投递结果。成功只代表"交给宿主了"，最终以回写为准。
inject_attempt <- function(id, ok, code = "", why = "") {
  d <- inject_read()
  hit <- FALSE
  for (i in seq_along(d$items)) {
    if (!identical(as.character(d$items[[i]]$id %||% ""), id)) next
    # 已回写、已作废的不再接受投递结果。
    #   踩过的坑：作废之后页面还挂着旧的补发请求，回来一记 attempt，
    #   就把 next_attempt_at 重新写成未来时间、把 attempts 又加一，
    #   作废记录于是复活并继续涨次数（面板上出现"试了 8 次"、上限却是 6）。
    if (!is.null(d$items[[i]]$satisfied_at)) next
    if (!is.null(d$items[[i]]$dropped_at)) next
    hit <- TRUE
    d$items[[i]]$attempts <- as.integer(d$items[[i]]$attempts %||% 0L) + 1L
    d$items[[i]]$last_attempt_at <- inject_ts(Sys.time())
    d$items[[i]]$last_code <- as.character(if (nzchar(code)) code else if (ok) "accepted" else "failed")
    d$items[[i]]$last_why <- as.character(why %||% "")
    if (isTRUE(ok)) {
      d$items[[i]]$delivered_at <- inject_ts(Sys.time())
      # 关键：投递成功也要排下一次复查。
      #   用户要的判据是"有没有回写"，不是"消息发没发出去"。
      #   发出去但助手没动、或动到一半断了，一小时后必须再提醒一次；
      #   只有回写才会走到 inject_satisfy 把 next_attempt_at 清掉。
      d$items[[i]]$next_attempt_at <- inject_ts(Sys.time() + INJECT_RETRY_SECS)
    } else {
      d$items[[i]]$next_attempt_at <- inject_ts(Sys.time() + INJECT_RETRY_SECS)
    }
  }
  if (hit) inject_write(d)
  hit
}

# 回写来了就销项：这一步已经有结果，再往会话里重发就是重复打扰。
#   按请求号销是最准的；没有请求号时按步骤 + 会话销。
# 回写没命中任何队列项时的留痕。
#   不改状态、不重发，只写一行诊断，避免「查了半天不知道为什么还在排队」。
inject_note_unmatched <- function(req_id, step) {
  f <- file.path(STATE_DIR, "inject_diag.jsonl")
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  line <- jsonlite::toJSON(list(
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), kind = "unmatched_satisfy",
    req_id = req_id, step = step,
    why = "回写没命中任何待发项；按精确匹配原则不销项"), auto_unbox = TRUE)
  cat(paste0(line, "\n"), file = f, append = TRUE)
  invisible(TRUE)
}

inject_satisfy <- function(req_id = "", step = "", session = "") {
  d <- inject_read()
  n <- 0L
  for (i in seq_along(d$items)) {
    it <- d$items[[i]]
    if (!is.null(it$satisfied_at)) next
    hit <- FALSE
    if (nzchar(req_id) && identical(as.character(it$req_id %||% ""), req_id)) hit <- TRUE
    # 只按 req_id 命中。原来这里还有一条「req_id 没命中就退到 step 名」的分支，
    #   已删除。删它的理由很具体：两版本同名环节并发时，旧版本晚到的回写会
    #   命中新版本的队列项，把「还没跑完」销成「已完成」—— 页面上两条都显示完成。
    #   这不是理论风险，是实测出现过的串版。
    #   现在对不上就是不动，由调用方决定要不要提示；宁可多留一条待发，
    #   也不要错销一条。
    if (!hit && nzchar(req_id) && nzchar(step)) {
      # 留痕：能回答「为什么这条指令还在排队」。
      tryCatch(inject_note_unmatched(req_id, step), error = function(e) NULL)
    }
    if (hit) {
      d$items[[i]]$satisfied_at <- inject_ts(Sys.time())
      d$items[[i]]$next_attempt_at <- NULL
      n <- n + 1L
    }
  }
  if (n > 0L) inject_write(d)
  n
}

# 后端侧的每小时体检：只做"把到点这件事算出来并留痕"，
#   真正的投递必须借页面里那条通道。所以这里不假装送达。
inject_sweep <- function() {
  due <- inject_due()
  if (length(due)) {
    say("[inject] ", format(Sys.time(), "%H:%M:%S"), " 到点待补发 ", length(due),
        " 条：", paste(vapply(due, function(it) as.character(it$id %||% ""), ""), collapse = ", "))
  }
  length(due)
}


# 每一步在服务器上要做什么。写给助手的是「做什么」，不是编出来的命令行 ——
#   服务器上的实际命令要按服务器那套脚本定，应用这边没有那个目录，
#   凭空写一条命令只会是假的。
STEP_TASK <- list(
  doublet = list(
    title = "逐样本去双与质控",
    ask = paste0("在服务器上跑 7 个样本的去双与质控：SoupX 关、DoubletFinder 开，",
                 "流水线细胞落到 1.doublet/。每个样本一个 rds 加一个 _stat.xls。"),
    writes = "1.doublet/<样本>.rds 与 _stat.xls（7 个样本）"),
  merge = list(
    title = "合并与批次校正",
    ask = paste0("读 1.doublet 的 7 个 rds，合并、标准化、周期回归、Harmony 批次校正、",
                 "一次聚类、RunTSNE，产出 3.merge/merge_7sample.rds。"),
    writes = "3.merge/merge_7sample.rds"),
  markers = list(
    title = "一次聚类 marker",
    ask = paste0("算一次聚类的簇大小、每个簇的 top15 marker，并出三张 UMAP，",
                 "落到 5.markers/。这些是后面筛群判断要用的依据。"),
    writes = "5.markers/cluster_stats.xls、top15_markers.xls"),
  filter = list(
    title = "筛群与剔样本后重聚类",
    ask = paste0("按人已经定下的筛群与剔样本结果重跑：剔除指定簇、剔除指定样本、",
                 "二次聚类，产出 7.after_drop/merged_after_drop.rds。"),
    writes = "7.after_drop/merged_after_drop.rds 与簇大小表"),
  final = list(
    title = "终版比例统计",
    ask = paste0("基于剔完的对象算每个样本的细胞数、每个簇的比例，落到 8.final/。"),
    writes = "8.final/cell_ratio_by_cluster.xls、cell_count_by_sample.xls"),
  fullmark = list(
    title = "全量 marker",
    ask = paste0("对终版对象算全量 marker 与 top15，写到 9.full/。"),
    writes = "9.full/all_markers_full.xls、top15_markers_full.xls"),
  diff = list(
    title = "组间差异",
    ask = paste0("算 Microplastics vs Mock 的全量差异表，写到 9.full/。"),
    writes = "9.full/diff_MP_vs_Mock_full.xls"),
  figs = list(
    title = "出图",
    ask = paste0("出全套图：UMAP 三张、dotplot、violin、feature、细胞比例、",
                 "火山图、质控三张、marker 热图，落到 10.figs/。"),
    writes = "10.figs/ 下约 22 张图与配套 xls"),
  # 注释这一步原来没有登记在这张表里。
  #   后果不是报错，而是"下发的指令里没有它"：助手拿到一个 stage id，
  #   在说明里查不到该干什么，只能自己猜 —— 猜出来的东西再被当成结论。
  #   对账器把它抓出来了，这里补上。
  annotate = list(
    title = "细胞类型注释",
    ask = paste0("用注释依据（参考表或 marker 词典）给每个簇判细胞类型，",
                 "写回对象的 annot 列，落到 9.annot/。"),
    writes = "9.annot/annotated.rds、annot_result.xls、celltype_count.xls"),
  enrich = list(
    title = "GO 富集",
    ask = paste0("对差异基因做 GO-BP 富集，出 dotplot 与富集汇总表，落到 10.figs/。"),
    writes = "10.figs/enrich_summary.xls、GO_BP_up.xls、30_GO_BP_dotplot.png"),
  # extract 是 2026-11-21 从 SERVER_STAGES 摘掉 fullmark/diff 时一并进来的新环，
  #   流程规格与 sc-report-extract 都登记了它，只有这张给助手的交代表漏了。
  #   后果与当初漏掉 ppt/pack 一样：flow_spec 里能算出命令，
  #   但 remote_step_instruction 取不到 STEP_TASK[[id]]，下发内容里没有可执行说明。
  #   对账器把它报成「少了这些阶段：extract」，这条是真缺。
  extract = list(
    title = "提取报告关键指标",
    ask = paste0("从注释后的对象与去双表抽出 PPT 第 3 页要用的六行指标，",
                 "机读版落 11.report/report_stats.json，明细表落同目录。"),
    writes = "11.report/report_stats.json、qc_stat.xls、celltype_composition.xls"),
  # ---- 第 11、12 环：ppt 与 pack（2026-11-03 补登记）----
  #   以前这两项没登记，后果不是"报错"，而是"下发的指令里没有可执行说明"：
  #   flow_spec 里已经有这两环、命令也能算出来，但 remote_step_instruction
  #   取不到 STEP_TASK[[id]]，就退化成一个占位标题 "按流程跑这一步"。
  #   实测 /api/brief?step=ppt 的正文就是这样。
  #   对账器把它抓出来了（「少了这些阶段：ppt、pack」），这条是真缺。
  ppt = list(
    title = "生成交付 PPT",
    ask = paste0("把 10.figs 的图按客户那份母版排成交付 PPT，封面写「单细胞转录组个性化分析」，",
                 "落 11.report/analysis_report.pptx。这一步在应用侧出，不在服务器上跑 R。"),
    writes = "11.report/analysis_report.pptx（约 22 页）"),
  pack = list(
    title = "打包交付物",
    ask = paste0("把 10.figs 整个目录与 11.report/analysis_report.pptx 打成一个 zip，",
                 "落 11.report/analysis_results.zip，打完检查 zip 能列目录、条目数不为 0。"),
    writes = "11.report/analysis_results.zip"),
  all = list(
    title = "全流程",
    ask = paste0("按顺序跑完整条链：去双质控 → 合并批次校正 → 一次聚类 marker → ",
                 "筛群重聚类 → 终版比例 → 全量 marker → 组间差异 → 出图 → GO 富集。",
                 "任何一步失败即停，把停在哪一步、为什么停下写回来。"),
    writes = "全部产物"),
  config = list(
    title = "生成配置文件",
    ask = paste0("在服务器的结果目录里写这一轮的参数文件（样本清单、分组表、",
                 "筛群与剔样本的决定），并登记哈希指纹。"),
    writes = "参数文件与指纹")
)

# 服务器档每步用哪个脚本（从 flow_spec 取，不在这里再抄一份）
# 当前分析号。进度、质检、闸门都必须挂在这一条记录上。
#   以前这三样存在全局单文件里、只按步骤 id 索引，于是新分析一打开
#   读到的就是上一轮那条 —— 这也是"新分析里带着跑到一半的结果"的根因。
cur_sid <- function() {
  if (!isTRUE(HAS_SESSIONS)) return("")
  v <- tryCatch(sess_current_id(APP_DIR), error = function(e) "")
  if (is.null(v)) "" else as.character(v)
}

# Switching records replaces even empty paths; global state is only the active view.
session_activate_state <- function(session) {
  st <- read_state()
  rd <- as.character(session$run_dir %||% "")
  st["runDir"] <- list(if (nzchar(rd)) rd else NULL)
  st$runDirRemote <- !identical(as.character(session$mode %||% "local"), "local")
  save_state(st)
}

# A callback may name its owner or resolve a unique pending request, never current.
callback_owner <- function(b) {
  sid <- trimws(as.character(b$session_id %||% ""))
  rq <- trimws(as.character(b$req_id %||% ""))
  if (!nzchar(rq)) rq <- trimws(as.character(b$req_id_carried %||% ""))
  bad <- function(code) list(ok = FALSE, code = code,
    error = "回写无法绑定到唯一分析记录；请携带原任务的 session_id 与 req_id，未写入。")
  if (!isTRUE(HAS_SESSIONS)) return(list(ok = TRUE, sid = sid, req_id = rq))
  if (nzchar(sid)) {
    if (is.null(sess_find(APP_DIR, sid))) return(bad("session_record_missing"))
  } else {
    if (!nzchar(rq)) return(bad("session_binding_required"))
    matches <- Filter(function(s) attempt_has(APP_DIR, s$id, rq),
                      sess_read(APP_DIR)$sessions %||% list())
    if (length(matches) != 1L) return(bad("session_binding_ambiguous"))
    sid <- as.character(matches[[1]]$id)
  }
  pending <- as.character(sess_get_pending(APP_DIR, sid) %||% "")
  if (!nzchar(rq) || !attempt_has(APP_DIR, sid, rq)) return(bad("req_id_mismatch"))
  list(ok = TRUE, sid = sid, req_id = rq)
}

# 一条记录是否属于这条分析。没有分析维度时不过滤（兼容单机旧数据）。
rec_belongs <- function(rec, sid) {
  if (!nzchar(sid)) return(TRUE)
  identical(as.character(rec$session_id %||% ""), sid)
}

# ---------------------------------------------------------------------
#  状态归一：把互相矛盾的步骤记录读成唯一一个结论
#
#  为什么需要它（2026-11-03，代码级审核第 3 条）
#    fullmark 这条记录长这样：
#      status = "delegated"   ← 页面据此认为"已交出、在等回写"
#      code   = 2             ← 退出码是失败
#      log    = "未执行…"      ← 日志写着没跑
#      subs   = [{ all: "failed" }]
#    同一件事被写成了两个结论。根因是整步状态有两条写入口：
#      · 交出去的那一刻直接写 delegated（serve.R 委派段）
#      · 子项回写时按子项推导 done/failed（serve.R 回写段）
#    后者只在"带 sub 的回写"上生效，走别的路就绕过去了，于是先写的那个留下。
#    结果就是页面上一条永远不动、也永远不失败的"在等回写"。
#
#  归一规则（只读时判，不改盘上的历史数据）
#    1. 整步是 running / delegated，但子项里有 failed  → failed
#    2. 整步是 running / delegated，子项非空且全 done    → done
#    3. 其余原样返回 —— 不乱改，看不清的保持"看不清"
#  为什么不直接改文件：历史记录是证据。把盘上的东西批量改写一遍，
#    等于抹掉"当时应用是这么记的"这个事实。读的时候给出正确结论就够了。
# ---------------------------------------------------------------------
rec_status_normalized <- function(rec) {
  stv <- as.character(rec$status %||% "")
  if (!stv %in% c("running", "delegated")) return(stv)
  subs <- rec$subs %||% list()
  if (!length(subs)) return(stv)
  sst <- vapply(subs, function(x) as.character(x$status %||% ""), "")
  sst <- sst[nzchar(sst)]
  if (!length(sst)) return(stv)

  # ---- 一次新的交付，不能被上一轮的子项结论盖掉 ----
  #   反例（会把功能改坏的那种）：fullmark 上一轮失败了，人重新点一次，
  #   委派写了 delegated、旧的 subs 还留着 failed。若只按子项判，
  #   刚交出去就立刻显示失败 —— 人以为点了没用，于是反复点。
  #   所以先比时间：这次交付的时间晚于所有子项的最后动静，
  #   说明子项是上一轮的，结论不适用，保留 delegated。
  t_dele <- as.character(rec$delegated_at %||% "")
  if (!nzchar(t_dele)) t_dele <- as.character(rec$started_at %||% "")
  t_sub <- ""
  for (x in subs) {
    for (k in c("updated_at", "ended_at", "started_at")) {
      v <- as.character(x[[k]] %||% "")
      if (nzchar(v) && v > t_sub) t_sub <- v
    }
  }
  if (nzchar(t_dele) && nzchar(t_sub) && t_dele > t_sub) return(stv)
  if (!nzchar(t_dele)) {
    # 没有任何时间可依：宁可说"看不清"，也不编一个结论。
    return(stv)
  }

  if (any(sst == "failed")) return("failed")
  if (all(sst == "done")) return("done")
  stv
}

# 这条产物的"最新已验收版本"指纹。
#   为什么要单独取：镜像目录里可能还躺着旧版文件，
#   只按"文件在不在"判已取回，就会把旧文件当成新结果给人下载。
#   回写里带的 md5 是执行端对该次产物的实测值，取最近一条即可。
#   md5 必须本身合法才采信；查不到就返回空，由调用方退回旧行为。
# 登记取回时该用哪个大小。
#   已知基准优先：库里能查到该产物最新验收的字节数时，以它为准，
#   浏览器带来的值只作兜底 —— 页面可能停在旧版，用户点取回时送来的还是上一版大小，
#   若优先采信它，新文件一回传就被判"大小与登记不一致"。
artifact_request_bytes <- function(sid, rel, requested = NULL) {
  exp <- tryCatch(artifact_expected_bytes(sid, rel), error = function(e) NA_real_)
  if (is.finite(exp) && exp > 0) return(exp)
  r <- suppressWarnings(as.numeric(requested %||% NA))
  if (is.finite(r) && r > 0) return(r)
  NA_real_
}

# 与基准 md5 同一条记录里的字节数。
#   为什么要它：执行端回传时会核对"磁盘大小 vs 登记大小"，
#   登记值若还是上一版的字节数，新文件一上来就被判"大小不一致"失败。
artifact_expected_bytes <- function(sid, rel) {
  want <- gsub("\\\\", "/", as.character(rel %||% ""))
  if (!nzchar(want)) return(NA_real_)
  sid <- as.character(sid %||% "")
  if (!nzchar(sid) || !isTRUE(DB_READY)) return(NA_real_)
  rows <- tryCatch(DBI::dbGetQuery(DB_CON, paste0(
    "SELECT a.payload AS payload, MAX(COALESCE(a.verified_at, a.reported_at)) AS t ",
    "FROM step_attempts a, json_each(a.payload, '$.artifacts') j ",
    "WHERE a.status = 'verified' ",
    "AND json_extract(a.payload, '$.session_id') = ? ",
    "AND TRIM(json_extract(j.value, '$.rel')) = ? ",
    "GROUP BY a.payload ORDER BY t DESC"), list(sid, want)),
    error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(NA_real_)
  p <- tryCatch(jsonlite::fromJSON(rows$payload[[1]], simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(p)) return(NA_real_)
  for (a in (p$artifacts %||% list())) {
    r0 <- gsub("\\\\", "/", as.character(if (is.list(a)) a$rel %||% "" else a))
    if (!identical(r0, want)) next
    b <- suppressWarnings(as.numeric(if (is.list(a)) a$bytes %||% NA else NA))
    return(if (is.finite(b) && b > 0) b else NA_real_)
  }
  NA_real_
}

artifact_expected_md5 <- function(sid, rel) {
  # 基准 = "包含这条产物的最新一条已验证记录"里的 md5。
  #   为什么不能拿整个 session 最新一条 verified 顶替：
  #     会话里每一步都会写自己的 verified 记录，最新那条通常只含它自己的产物。
  #     实测后果：PPT 一完成，最新记录只剩 pptx/manifest，
  #     于是 figs、enrich 的图全部查不到基准；pack 完成后再看 PPT 也一样查不到。
  #   为什么必须"先按精确 rel 筛、再取其中最新"：
  #     这才是"这份文件的最新验收版本"，而不是"这个 session 最近干了什么"。
  #   为什么最新相关记录里缺 md5 就返回空：
  #     它才是当前版本；它自己没给 hash，说明基准未核实，
  #     不能让更早的旧 hash 冒充它（那正是这次缓存事故的同类错）。
  want <- gsub("\\\\", "/", as.character(rel %||% ""))
  if (!nzchar(want)) return("")
  sid <- as.character(sid %||% "")
  if (!nzchar(sid) || !isTRUE(DB_READY)) return("")
  # 在 SQL 里就按 rel 筛：只要"这条记录含这个产物"的那批，再取其中最新一条。
  #   不用"取最近 N 条再在 R 里找"——N 是任意截断：
  #     早期步骤的产物会在若干次新验收之后被挤出窗口，表现成"老图又没法下载了"。
  #   用 json_each 展开 artifacts，避免对 JSON 结构写死下标。
  rows <- tryCatch(DBI::dbGetQuery(DB_CON, paste0(
    "SELECT a.payload AS payload, MAX(COALESCE(a.verified_at, a.reported_at)) AS t ",
    "FROM step_attempts a, json_each(a.payload, '$.artifacts') j ",
    "WHERE a.status = 'verified' ",
    "AND json_extract(a.payload, '$.session_id') = ? ",
    "AND TRIM(json_extract(j.value, '$.rel')) = ? ",
    "GROUP BY a.payload ORDER BY t DESC"), list(sid, want)),
    error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return("")
  # 第一条就是最新相关记录：它带合法 md5 就用它，没带就判未核实。
  p <- tryCatch(jsonlite::fromJSON(rows$payload[[1]], simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(p)) return("")
  for (a in (p$artifacts %||% list())) {
    r0 <- gsub("\\\\", "/", as.character(if (is.list(a)) a$rel %||% "" else a))
    if (!identical(r0, want)) next
    v <- tolower(as.character(if (is.list(a)) a$md5 %||% "" else ""))
    return(if (grepl("^[0-9a-f]{32}$", v)) v else "")
  }
  ""
}

# Schedule from verified facts, never from the caller's unverified artifact list.
artifact_enqueue_verified <- function(sid, req_id, step, status = "done", blocked = FALSE, historical = FALSE) {
  if (!isTRUE(HAS_MIRROR) || !isTRUE(DB_READY) || isTRUE(blocked) ||
      !identical(as.character(status), "done") ||
      step %in% c("selftest", "all", "config") ||
      (!isTRUE(historical) && !attempt_selected(APP_DIR, sid, req_id))) return(list(queued = 0L))
  rows <- DBI::dbGetQuery(DB_CON, paste0(
    "SELECT a.payload, a.attempt_id, a.owner_key, s.run_dir FROM step_attempts a JOIN sessions s ",
    "ON s.session_id = json_extract(a.payload, '$.session_id') ",
    "AND s.owner_key = a.owner_key WHERE s.session_id = ? AND a.req_id = ? ",
    "AND a.step_id = ? AND a.status = 'verified' ",
    "ORDER BY COALESCE(a.verified_at, a.reported_at) DESC LIMIT 1"),
    list(sid, req_id, step))
  if (!nrow(rows)) return(list(queued = 0L))
  payload <- jsonlite::fromJSON(rows$payload[[1]], simplifyVector = FALSE)
  if (!identical(as.character(payload$session_id %||% ""), as.character(sid)) ||
      !identical(as.character(payload$req_id %||% ""), as.character(req_id)) ||
      !identical(as.character(payload$step %||% ""), as.character(step)) ||
      !identical(as.character(payload$status %||% ""), "done"))
    return(list(queued = 0L, error = "verified_payload_binding_mismatch"))
  run_dir <- as.character(rows$run_dir[[1]] %||% "")
  if (!nzchar(run_dir) || !identical(as.character(payload$run_dir %||% ""), run_dir))
    return(list(queued = 0L, error = "verified_run_dir_mismatch"))
  if (isTRUE(historical)) {
    # A global head moves on to the next stage; the persisted per-stage selection does not.
    selected <- (sess_find(APP_DIR, sid)$steps %||% list())[[step]]
    terminal <- attempt_value(APP_DIR, sid, req_id, paste0("step--", step))
    pp <- if (exists("sr_passport", mode = "function")) sr_passport(APP_DIR, sid, step) else NULL
    bound <- identical(as.character(selected$req_id %||% ""), as.character(req_id)) &&
      identical(as.character(selected$status %||% ""), "done") &&
      identical(as.character(terminal$req_id %||% ""), as.character(req_id)) &&
      identical(as.character(terminal$status %||% ""), "done")
    if (!bound) return(list(queued = 0L, error = "historical_selection_unconfirmed"))
    if (is.null(pp) || !identical(pp$kind, "post") ||
        !identical(pp$session_id, sid) || !identical(pp$stage, step) ||
        !identical(pp$attempt_id, rows$attempt_id[[1]]) || !identical(pp$run_dir, run_dir) ||
        !(as.character(pp$verdict %||% "") %in% c("pass", "warn")) ||
        nzchar(as.character(pp$error %||% "")))
      return(list(queued = 0L, error = "historical_review_unconfirmed_or_blocked"))
  }
  results <- list()
  source_unconfirmed <- FALSE
  for (a in payload$artifacts %||% list()) {
    if (!is.list(a)) next
    rel <- as.character(a$rel %||% "")
    # Never borrow a newer hash from a foreign run, owner or unreviewed attempt.
    latest <- DBI::dbGetQuery(DB_CON, paste0(
      "SELECT a.attempt_id FROM step_attempts a, json_each(a.payload, '$.artifacts') j ",
      "WHERE a.status = 'verified' AND json_extract(a.payload, '$.session_id') = ? ",
      "AND TRIM(json_extract(j.value, '$.rel')) = ? ",
      "ORDER BY COALESCE(a.verified_at, a.reported_at) DESC, a.attempt_id DESC LIMIT 1"), list(sid, rel))
    if (!nrow(latest) || !identical(latest$attempt_id[[1]], rows$attempt_id[[1]])) {
      source_unconfirmed <- TRUE
      next
    }
    bytes <- suppressWarnings(as.numeric(a$bytes %||% NA_real_))
    md5 <- tolower(as.character(a$md5 %||% ""))
    if (!nzchar(rel) || !identical(mirror_safe(rel), rel) || grepl("\\.\\.", rel) ||
        startsWith(rel, "/") || !mirror_can(rel, bytes) ||
        tolower(tools::file_ext(rel)) %in% MIRROR_LARGE_EXTS ||
        any(vapply(MIRROR_SECRET_PATTERNS, function(p) grepl(p, rel, ignore.case = TRUE), TRUE)) ||
        !is.finite(bytes) || bytes <= 0 || bytes != floor(bytes) || !grepl("^[0-9a-f]{32}$", md5)) next
    results[[rel]] <- mirror_request(sid, rel, bytes = bytes, run_dir = run_dir,
                                    want_md5 = md5, preserve_existing = TRUE, recover = isTRUE(historical),
                                     owner_key = rows$owner_key[[1]])
  }
  list(queued = sum(vapply(results, function(x) isTRUE(x$ok), TRUE)), items = results,
       error = if (source_unconfirmed) "latest_verified_source_not_reviewed" else NULL)
}

# Bounded worker-poll backfill, not a browser/list-read side effect. The cursor is
# process-local: restart repeats safely, and old stages cannot starve behind new ones.
artifact_reconcile_pending <- function(limit = 64L, owner_key = NULL) {
  if (!isTRUE(DB_READY)) return(list(scanned = 0L, issues = list()))
  env <- environment(artifact_reconcile_pending)
  key <- paste0(".artifact_reconcile_cursor:", APP_DIR, ":", owner_key %||% "*")
  cursor <- if (exists(key, envir = env, inherits = FALSE)) get(key, envir = env) else 0L
  rows <- DBI::dbGetQuery(DB_CON, paste0(
    "SELECT a.req_id, a.step_id, json_extract(a.payload, '$.session_id') AS sid ",
    "FROM step_attempts a JOIN sessions s ON s.session_id = json_extract(a.payload, '$.session_id') ",
    "AND s.owner_key = a.owner_key WHERE a.status = 'verified' ",
    if (!is.null(owner_key)) "AND s.owner_key = ? " else "",
    "ORDER BY a.attempt_id LIMIT ? OFFSET ?"), c(if (!is.null(owner_key)) list(owner_key) else list(), list(as.integer(limit), cursor)))
  assign(key, if (nrow(rows) < limit) 0L else cursor + nrow(rows), envir = env)
  issues <- list()
  for (i in seq_len(nrow(rows))) {
    r <- tryCatch(artifact_enqueue_verified(rows$sid[[i]], rows$req_id[[i]], rows$step_id[[i]],
      historical = TRUE), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(r$error)) issues[[length(issues) + 1L]] <- list(session = rows$sid[[i]],
      req_id = rows$req_id[[i]], step = rows$step_id[[i]], error = r$error)
  }
  list(scanned = nrow(rows), issues = issues)
}

rec_display_status <- function(rec) {
  status <- rec_status_normalized(rec)
  if (identical(status, "delegated") &&
      !nzchar(as.character(rec$delivered_at %||% ""))) return("handoff_unconfirmed")
  status
}

rec_actual_artifacts <- function(rec) {
  rels <- character(0)
  for (a in rec$artifacts %||% list()) {
    value <- if (is.list(a)) as.character(a$rel %||% a$path %||% "") else as.character(a)
    if (nzchar(value)) rels <- c(rels, value)
  }
  as.list(rels)
}

# Persistence inputs must never pass through the bounded display/tail reader.
read_utf8_complete <- function(path) {
  if (!file.exists(path)) return("")
  con <- file(path, open = "rb"); on.exit(close(con), add = TRUE)
  chunks <- list()
  repeat {
    part <- readBin(con, "raw", n = 1048576L)
    if (!length(part)) break
    chunks[[length(chunks) + 1L]] <- part
  }
  txt <- rawToChar(do.call(c, chunks) %||% raw())
  if (!validUTF8(txt)) stop("invalid_utf8_persistence_input", call. = FALSE)
  Encoding(txt) <- "UTF-8"
  txt
}

remote_steps_snapshot <- function() {
  if (!file.exists(REMOTE_STEP_FILE)) return(list(steps = list()))
  x <- jsonlite::fromJSON(read_utf8_complete(REMOTE_STEP_FILE), simplifyVector = FALSE)
  # 迁移来的实例里这份文件只剩 {"items": []}：旧格式的执行端清单在建库时
  #   已经拆进 step_attempts，文件本身没有 steps 键。以前这里直接 stop()，
  #   于是每读一次状态就报一次错，页面上每一步都写「尚无运行记录」——
  #   而记录其实都在库里。缺 steps 当空清单，不是坏文件；
  #   steps 存在但结构不对的，仍旧报错。
  if (is.list(x) && is.null(x$steps)) return(list(steps = list()))
  if (!is.list(x) || is.null(x$steps) || !is.list(x$steps) ||
      !all(vapply(x$steps, function(s) is.list(s) &&
        length(s$id) == 1L && is.character(s$id) && nzchar(s$id), TRUE)))
    stop("invalid_remote_steps_snapshot", call. = FALSE)
  x
}

# 迁移来的记录：每一步到哪了，只能从已验证回写记录重建。
#   为什么重建成同一份形状：页面读的是「每一步一条记录」，而库里存的是
#   「每次回写一条 attempt」。同一步有多条历史时取时间最新那条，与
#   artifact_expected_md5 同一口径 —— 这里答的是这一步现在到哪了，不是流水账。
#   只读重建：不写回 remote_steps.json，不补造库里没有的步骤。
remote_steps_from_db <- function(sid = NULL) {
  sid <- as.character(sid %||% "")
  if (!nzchar(sid) || !isTRUE(DB_READY)) return(list())
  rows <- tryCatch(DBI::dbGetQuery(DB_CON, paste0(
    "SELECT step_id AS step_id, status AS status, ",
    "COALESCE(verified_at, reported_at) AS t, payload AS payload ",
    "FROM step_attempts ",
    "WHERE json_extract(payload, '$.session_id') = ? ",
    "ORDER BY t ASC"), list(sid)),
    error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(list())
  latest <- list()
  for (i in seq_len(nrow(rows))) {
    pl <- tryCatch(jsonlite::fromJSON(rows$payload[[i]], simplifyVector = FALSE),
                   error = function(e) NULL)
    if (is.null(pl)) pl <- list()
    step <- as.character(rows$step_id[[i]] %||% "")
    if (!nzchar(step)) step <- as.character(pl$step %||% "")
    if (!nzchar(step)) next
    stv <- as.character(rows$status[[i]] %||% "")
    latest[[step]] <- list(
      id = step,
      title = as.character(pl$title %||% ""),
      status = if (identical(stv, "verified")) "done"
               else if (identical(stv, "rejected")) "failed" else stv,
      code = if (is.null(pl$code)) NULL else pl$code,
      log = as.character(pl$log %||% ""),
      artifacts = pl$artifacts %||% list(),
      imported_at = as.character(rows$t[[i]] %||% ""),
      req_id = as.character(pl$req_id %||% ""),
      exec_id = as.character(pl$exec_id %||% ""),
      session_id = sid,
      run_dir = as.character(pl$run_dir %||% ""),
      subs = pl$subs %||% list(),
      source = "verified_writeback",
      migrated = TRUE)
  }
  unname(latest)
}

remote_status_projection <- function(steps) {
  states <- vapply(steps, function(s) rec_display_status(s), "")
  known <- sort(unique(states))
  counts <- as.list(table(states))
  # No complete-analysis certificate exists here; single-step done is not overall done.
  list(status = if (length(states) && all(states == "running")) "running" else "unknown",
       has_running = any(states == "running"),
       summary = list(scope = "observed_stage_writebacks", counts = counts,
                      conflicting = length(known) > 1L, completion_confirmed = FALSE))
}

# 已验收产物的基线（bytes / md5）。
#   回写里没带 md5 时返回空串，表示"未核验"，不能拿它冒充已验收证据。
# Receipt authentication deliberately does not use the legacy anonymous worker window.
mirror_receipt_auth <- function(req, owner_ctx, sid = "", b = list()) {
  deny <- function(code) list(ok = FALSE, status = 403L, code = code,
                              error = "mirror receipt authorization denied")
  h <- req$headers %||% list()
  worker_present <- !is.null(h[["x-worker-token"]]) ||
    startsWith(tolower(trimws(as.character(h[["authorization"]] %||% ""))), "worker ")
  if (worker_present) {
    if (!exists("worker_token_check", mode = "function")) return(deny("worker_auth_missing"))
    wa <- tryCatch(worker_token_check(APP_DIR, worker_token_from_req(req)),
      error = function(e) list(ok = FALSE, code = "worker_token_invalid"))
    if (!isTRUE(wa$ok)) return(deny(wa$code %||% "worker_token_invalid"))
    if (!"mirror" %in% wa$scopes) return(deny("worker_scope_denied"))
    # Match the existing execution-pool binding; a token from another pool cannot
    # retrieve this application's global mirror queue.
    if (!identical(wa$pool_id, worker_default_pool(APP_DIR))) return(deny("worker_pool_mismatch"))
    wa <- worker_route_auth(req, as.character(b$worker_id %||% ""),
                            as.character(b$pool_id %||% ""), "mirror")
    if (!isTRUE(wa$ok)) return(wa)
    return(list(ok = TRUE, worker = TRUE))
  }
  if (!nzchar(record_scalar(owner_ctx$key))) return(deny("mirror_identity_required"))
  if (!nzchar(sid)) return(list(ok = TRUE, worker = FALSE))
  access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "write")
  if (!isTRUE(access$ok)) return(list(ok = FALSE, status = access$status %||% 403L,
    code = access$code %||% "record_forbidden", error = access$error))
  list(ok = TRUE, worker = FALSE, cap = access$cap)
}

mirror_receipt_guard <- function(req, owner_ctx, b, failure = FALSE) {
  sid <- record_trim(b$session_id); rel <- record_trim(b$rel)
  auth <- mirror_receipt_auth(req, owner_ctx, sid, b)
  if (!isTRUE(auth$ok)) return(auth)
  deny <- function(code) list(ok = FALSE, status = 409L, code = code,
                              error = "mirror receipt binding is not current and verified")
  if (!nzchar(sid) || !nzchar(rel)) return(deny("binding_fields_required"))
  cap <- auth$cap %||% record_capability(DB_CON, APP_DIR, sid, list(key = NA_character_))
  if (!isTRUE(cap$ok) || !nzchar(cap$run_dir)) return(deny("record_run_dir_missing"))
  hits <- Filter(function(it) identical(as.character(it$session %||% ""), sid) &&
    identical(as.character(it$rel %||% ""), rel), mirror_req_read()$items %||% list())
  if (length(hits) != 1L) return(deny("not_registered"))
  it <- hits[[1]]
  if (!is.null(it$done) || nzchar(as.character(it$error %||% "")) ||
      as.character(it$status %||% "") %in% c("done", "failed", "stopped"))
    return(deny("receipt_not_pending"))
  if (!identical(as.character(it$run_dir %||% ""), cap$run_dir) ||
      (nzchar(as.character(it$owner_key %||% "")) && !identical(it$owner_key, cap$owner_key)))
    return(deny("registration_binding"))
  rows <- tryCatch(DBI::dbGetQuery(DB_CON, paste0(
    "SELECT a.payload, a.owner_key FROM step_attempts a ",
    "WHERE a.status = 'verified' AND json_extract(a.payload, '$.session_id') = ? ",
    "AND EXISTS (SELECT 1 FROM json_each(a.payload, '$.artifacts') j ",
    "WHERE TRIM(json_extract(j.value, '$.rel')) = ?) ",
    "ORDER BY COALESCE(a.verified_at, a.reported_at) DESC, a.rowid DESC LIMIT 1"),
    list(sid, rel)), error = function(e) NULL)
  if (is.null(rows) || !nrow(rows) || !identical(rows$owner_key[[1]], cap$owner_key))
    return(deny("verified_baseline_missing"))
  payload <- tryCatch(jsonlite::fromJSON(rows$payload[[1]], simplifyVector = FALSE), error = function(e) NULL)
  if (!identical(as.character(payload$run_dir %||% ""), cap$run_dir) ||
      !identical(as.character(payload$status %||% ""), "done")) return(deny("baseline_binding"))
  arts <- Filter(function(a) is.list(a) && identical(record_trim(a$rel), rel), payload$artifacts %||% list())
  if (length(arts) != 1L) return(deny("verified_baseline_missing"))
  md <- tolower(record_trim(arts[[1]]$md5))
  size <- suppressWarnings(as.numeric(arts[[1]]$bytes %||% NA))
  if (!grepl("^[0-9a-f]{32}$", md) || length(size) != 1L || !is.finite(size) || size <= 0 || size != round(size))
    return(deny("verified_baseline_missing"))
  if (!identical(tolower(record_trim(it$want_md5)), md) ||
      !isTRUE(suppressWarnings(as.numeric(it$bytes %||% NA)) == size)) return(deny("registration_stale"))
  if (!failure) {
    if (!identical(tolower(record_trim(b$md5)), md)) return(deny("baseline_md5"))
    # Legacy small-file workers omit bytes; actual decoded size is still checked
    # against the authoritative baseline by mirror_put.
    if ((!is.null(b$bytes) && !isTRUE(suppressWarnings(as.numeric(b$bytes)) == size)))
      return(deny("baseline_size"))
  }
  list(ok = TRUE, md5 = md, bytes = size)
}

artifact_baseline <- function(sid, rel) {
  d <- tryCatch(remote_steps_read(sid), error = function(e) list(steps = list()))
  want <- gsub("\\\\", "/", as.character(rel %||% ""))
  for (s in (d$steps %||% list())) {
    for (a in (s$artifacts %||% list())) {
      r0 <- gsub("\\\\", "/", as.character(a$rel %||% a$path %||% ""))
      if (!nzchar(r0) || !identical(r0, want)) next
      md <- tolower(as.character(a$md5 %||% ""))
      return(list(found = TRUE,
                  bytes = suppressWarnings(as.numeric(a$bytes %||% NA)),
                  md5 = if (grepl("^[0-9a-f]{32}$", md)) md else ""))
    }
  }
  list(found = FALSE, bytes = NA_real_, md5 = "")
}

remote_steps_read <- function(sid = NULL) {
  if (!file.exists(REMOTE_STEP_FILE)) return(list(steps = list()))
  x <- remote_steps_snapshot()
  if (is.null(sid)) sid <- cur_sid()
  if (nzchar(sid) && length(x$steps) > 0)
    x$steps <- Filter(function(s) rec_belongs(s, sid), x$steps)
  # 读出时归一，保证下游看到的整步状态与它自己的子项自洽。
  if (length(x$steps) > 0)
    x$steps <- lapply(x$steps, function(s) { s$status <- rec_status_normalized(s); s$display_status <- rec_display_status(s); s })
  x
}

remote_step_get <- function(id, sid = NULL) {
  d <- remote_steps_read(sid)
  hit <- Filter(function(s) identical(as.character(s$id %||% ""), id), d$steps)
  if (!length(hit)) NULL else hit[[length(hit)]]
}

# 同一步同一条分析只留最新一条：这里回答的是「这一步现在到哪了」，不是流水账。
#   sid 传 "" 表示看全表 —— 写的时候必须看全表，否则会把别的分析记录一起删掉。
remote_step_put <- function(id, rec, sid = NULL) {
  if (is.null(sid)) sid <- cur_sid()
  d <- remote_steps_snapshot()
  if (is.null(d$steps)) d$steps <- list()
  rec$id <- id
  if (nzchar(sid)) rec$session_id <- sid
  keep <- Filter(function(s)
    !(identical(as.character(s$id %||% ""), id) && rec_belongs(s, sid)), d$steps)
  d$steps <- c(keep, list(rec))
  write_json_file(REMOTE_STEP_FILE, d)
  invisible(TRUE)
}

# 把「这一步交给助手」写成一段能直接执行的指令。
#   为什么要服务端拼：服务器参数（地址、账号、四个目录、R、队列、节点约束）
#   只有这里拿得到，前端再拼一遍就会两边不一致。
#
#   为什么要改：以前这段只写「用服务器上已有的那套脚本跑这一步」，
#   具体哪个脚本、什么参数、跑完该有什么文件、怎么算跑完 —— 全靠执行端自己回忆。
#   每次都要重新对齐一次，而跑偏了没有任何东西能发现。
#   现在顺序与命令来自 engine/flow_spec.R（唯一出处），指令照它拼；
#   质检也照它核。两边同源，才谈得上「处理没有偏移」。
# 当前档位：服务器模式就是 server 档（真在跑的那套），否则 local 档。
#   这里自己读一次 state，不去引用请求处理函数里的局部变量 st ——
#   那个 st 是另一个函数作用域里的，跨作用域取会直接报 object 'st' not found。
# ---- PPT 自动补齐：缺图不是写进报告就完了，得回去把图真的画出来 ----
#   为什么要这一套：出 PPT 时发现缺图，正确的动作是回服务器重跑对应环节，
#   跑完再把 PPT 重出一遍。只在报告里写一句「缺 4 张图」，等于把活推回给人。
#   这里把它做成闭环：
#     PPT 缺图 → 记下"这批还欠哪几环" → 自动入队重跑那几环 + 发指令
#     → 那些环回写成功 → 自动重出 PPT → 产物登记好，人点下载即可。
PPT_PENDING_FILE <- file.path(STATE_DIR, "ppt_pending.json")

ppt_pending_read <- function() {
  if (!file.exists(PPT_PENDING_FILE)) return(list(items = list()))
  x <- tryCatch(read_json_file(PPT_PENDING_FILE), error = function(e) NULL)
  if (is.null(x) || is.null(x$items)) return(list(items = list())) else x
}

ppt_pending_write <- function(d) {
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  tryCatch(write_json_file(PPT_PENDING_FILE, d), error = function(e) NULL)
  invisible(TRUE)
}

# 给某条分析记下"等哪几环跑完就重出 PPT"
ppt_pending_arm <- function(sid, stages, title = "", missing = list()) {
  if (!nzchar(sid) || !length(stages)) return(FALSE)
  d <- ppt_pending_read()
  keep <- Filter(function(x) !identical(as.character(x$session %||% ""), sid), d$items)
  d$items <- c(keep, list(list(
    session = sid, stages = as.list(unique(as.character(stages))),
    title = title, missing = missing,
    armed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))))
  ppt_pending_write(d)
  TRUE
}

# 某一环回写成功了：把这一步从待补清单里划掉；划完就重出 PPT。
#   返回 list(done=是否已重出, ppt=..., remaining=...)
ppt_pending_advance <- function(sid, step_id) {
  d <- ppt_pending_read()
  if (!length(d$items)) return(list(touched = FALSE))
  hit <- NULL; idx <- 0
  for (i in seq_along(d$items)) {
    if (identical(as.character(d$items[[i]]$session %||% ""), sid)) { hit <- d$items[[i]]; idx <- i }
  }
  if (is.null(hit)) return(list(touched = FALSE))
  stages <- as.character(unlist(hit$stages %||% character(0)))
  left <- setdiff(stages, as.character(step_id))
  if (!length(left)) {
    d$items <- d$items[-idx]
    ppt_pending_write(d)
    # 该补的都补完了：直接重出，不等人来点。
    r <- tryCatch(ppt_build_from_mirror(ENGINE_DIR, mirror_dir(sid),
             out_rel = "11.report/analysis_report.pptx",
             title = as.character(hit$title %||% ""), on_missing = "error"),
           error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (isTRUE(r$ok)) {
      ppt_register_artifact(sid, r)
      return(list(touched = TRUE, built = TRUE, ppt = r, remaining = list()))
    }
    # 补完了还是出不来：说明缺的东西不是这几环能补的。
    #   不能就这么把单子划掉 —— 划掉等于"已处理"，而问题还在。
    #   重新算一次还缺什么，能派就再派一轮，派不了就留着并写清原因。
    plan2 <- tryCatch(ppt_autofix_plan((r %||% list())$missing), error = function(e) NULL)
    st2 <- as.character(unlist((plan2 %||% list())$stages %||% character(0)))
    if (length(st2)) {
      ppt_pending_arm(sid, st2, as.character(hit$title %||% ""), (r %||% list())$missing)
    } else {
      d2 <- ppt_pending_read()
      d2$items <- c(d2$items, list(list(
        session = sid, stages = list(), title = as.character(hit$title %||% ""),
        missing = (r %||% list())$missing,
        blocked = (r %||% list())$log_tail %||% (r %||% list())$error %||% "",
        armed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))))
      ppt_pending_write(d2)
    }
    return(list(touched = TRUE, built = FALSE, ppt = r, remaining = as.list(st2)))
  }
  d$items[[idx]]$stages <- as.list(left)
  ppt_pending_write(d)
  list(touched = TRUE, built = FALSE, remaining = as.list(left))
}

# 把生成的 pptx 登记进产物清单（页面「产物下载」直接能看到并下载）
ppt_register_artifact <- function(sid, r) {
  f0 <- file.path(STATE_DIR, "artifacts.json")
  aj <- tryCatch(read_json_file(f0), error = function(e) NULL) %||% list(items = list())
  its <- aj$items %||% list()
  has <- FALSE
  for (i in seq_along(its)) {
    if (identical(as.character(its[[i]]$rel %||% ""), as.character(r$out_rel))) {
      its[[i]]$size <- r$bytes
      its[[i]]$mtime <- format(Sys.time(), "%Y-%m-%dT%H:%M")
      has <- TRUE
    }
  }
  if (!has) its[[length(its) + 1L]] <- list(
    rel = r$out_rel, name = basename(r$out_rel), kind = "PPT",
    size = r$bytes, mtime = format(Sys.time(), "%Y-%m-%dT%H:%M"), full = NULL)
  aj$items <- its
  aj$listed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  tryCatch(write_json_file(f0, aj), error = function(e) NULL)
  invisible(TRUE)
}

flow_profile <- function(sid = NULL) {
  qs <- trimws(as.character(sid %||% ""))
  if (nzchar(qs) && isTRUE(HAS_SESSIONS)) {
    rec <- tryCatch(sess_find(APP_DIR, qs), error = function(e) NULL)
    md <- trimws(as.character((rec %||% list())$mode %||% ""))
    if (nzchar(md)) return(if (identical(md, "local")) "local" else "server")
  }
  s2 <- tryCatch(read_state(), error = function(e) list())
  if (isTRUE(s2$runDirRemote)) return("server")
  # 退路：state 里没标，就看位置设置里的 mode。
  pl <- if (HAS_PLACE) tryCatch(place_read(APP_DIR), error = function(e) NULL) else NULL
  md <- as.character(((pl %||% list())$mode) %||% "")
  if (identical(md, "server")) "server" else "server"
}
flow_run_order <- function(sid = NULL) {
  if (!HAS_FLOW) return(character(0))
  FLOW_ORDER(flow_profile(sid))
}

# ---------------------------------------------------------------------
#  物种值归一：白名单 + 规范化
#
#  为什么必须做（2026-11-03，审核第 7 条）
#    species 原来是"会话里是什么就原样发什么"，没有任何校验：
#      · 为空     → 命令里出现 --species ''，脚本读到空值就静默跳过富集，
#                   跑完零产物，而页面显示"跑完了"。10-30 那次就是这样。
#      · 写「小鼠」 → 脚本的库映射表只认 Mouse/Human/Rat，中文值一样跳过，
#                   同样是静默空转，比报错难查得多。
#    对比一下 reference_xlsx：它是有非空校验的（缺了就拒提交）。
#    同一类"缺了这个参数这一步就是空跑"的输入，不该一个拦一个不拦。
#
#  归一规则
#    空 → ""（由 flow_execution 统一拦下，不在这一层编默认值）
#    认得的中英文/大小写 → 规范化成脚本要的那个词
#    认不得的 → 原样返回，让上层拦下并把人填的东西原样显示出来
#  为什么不给默认值 Mouse：这批数据确实是小鼠，但下一批不是。
#    悄悄补默认值等于替人做了一个他不知道的决定，而且错了也看不出来。
# ---------------------------------------------------------------------
SPECIES_ALIASES <- list(
  mouse = c("mouse", "mice", "小鼠", "小家鼠", "mus", "mus musculus", "mm"),
  human = c("human", "homo sapiens", "人", "人类", "hs", "hsa"),
  rat   = c("rat", "rats", "大鼠", "rattus", "rn", "rno")
)

species_canon <- function(x) {
  s <- trimws(as.character(x %||% ""))
  if (!nzchar(s)) return("")
  low <- tolower(s)
  for (k in names(SPECIES_ALIASES)) {
    if (low %in% SPECIES_ALIASES[[k]])
      return(paste0(toupper(substr(k, 1, 1)), substr(k, 2, nchar(k))))
  }
  s
}

species_is_valid <- function(x) {
  s <- trimws(as.character(x %||% ""))
  nzchar(s) && grepl("^(Mouse|Human|Rat)$", species_canon(s))
}

flow_record_server <- function(srv = list(), sid = "") {
  if (!isTRUE(HAS_SESSIONS)) return(srv)
  qs <- trimws(as.character(sid %||% ""))
  if (!nzchar(qs)) qs <- cur_sid()
  rec <- if (nzchar(qs)) sess_find(APP_DIR, qs) else NULL
  (rec %||% list())$place$server %||% list()
}

flow_vars <- function(srv = list(), sid = "") {
  srv <- flow_record_server(srv, sid)
  rt <- srv$runtime %||% list()
  work <- as.character(srv$work_root %||% "")
  data <- as.character(srv$data_root %||% "")
  scr  <- as.character(srv$script_root %||% "")

  # ---- 本次选用的注释依据 ----
  #   参考表不是固定的：换组织、换物种、换细胞类型范围，依据就不同。
  #   所以这里取的是"这一条分析记录选用的那一份"，不是全局唯一一份。
  #   没选就留空 —— 让占位符原样留着、执行端停下来问，
  #   绝不能退回某个内置路径"先跑起来再说"：用错依据注释出来的结果，
  #   格式是对的、内容是错的，事后极难发现。
  refId <- ""
  qs <- as.character(sid %||% "")
  if (!nzchar(qs)) qs <- tryCatch(cur_sid(), error = function(e) "")
  if (nzchar(qs) && isTRUE(HAS_SESSIONS)) {
    s0 <- tryCatch(sess_find(APP_DIR, qs), error = function(e) NULL)
    refId <- as.character((s0 %||% list())$reference_id %||% "")
  }
  refFile <- ""
  refName <- ""
  refSource <- ""
  if (nzchar(refId) && isTRUE(HAS_REFERENCE)) {
    it <- tryCatch(ref_get(refId), error = function(e) NULL)
    if (!is.null(it)) {
      refName <- as.character(it$name %||% "")
      refSource <- as.character(it$source %||% "uploaded")
      if (identical(refSource, "server")) {
        # 服务器上本来就有这份表：直接用它的绝对路径，不需要下发、不需要落盘。
        #   这也是"直接点击已有文件库"这条路能成立的关键 ——
        #   否则会用下载副本，而副本与原件可能不是同一份。
        refFile <- as.character(it$server_path %||% "")
      } else {
        # 人传上来的：约定落盘位置，由执行端按这个路径写下去。
        ext <- tolower(tools::file_ext(refName))
        if (!nzchar(ext)) ext <- "xlsx"
        refFile <- pjoin(work, "0.reference", paste0("reference.", ext))
      }
    }
  }

  list(
    rscript = as.character(rt$rscript %||% "Rscript"),
    python  = as.character(rt$python %||% "python3"),
    # ---- server 档用 ----
    work_root = work,
    script_root = scr,
    # 原始数据在 liny 的 data3 下，不是 matrix 那一层 ——
    #   matrix 里是 7 个符号链接，脚本要的是 data3/<样本>/outs/... 的父目录。
    liny_data = as.character(srv$liny_data %||%
      "/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/data3"),
    samples = as.character(srv$samples %||%
      "18m-G489,18m-G490,18m-G491,18m-G756,18m-G757,18m-G760,18m-G761"),
    queue = as.character(rt$queue %||% "all.q"),
    qsub = as.character(rt$qsub %||% "/opt/gridengine/bin/lx-amd64/qsub"),
    hdf5 = as.character(rt$hdf5 %||% ""),
    # ---- 注释依据 ----
    #   reference_xlsx 就是 supplement argv 里的 {reference_xlsx}。
    #   以前这个占位符根本没进 flow_vars，于是从来不会被替换，
    #   指令里原样留着 "{reference_xlsx}" —— 执行端无从下手。
    # 这批数据的物种：富集段必须知道用哪个注释库。
    #   为什么不写在脚本里：换物种（小鼠→人）就换库，写死等于悄悄用错库。
    #   为什么不让人每次手填：它属于"这批数据是什么"，分析记录里已经有，
    #   由应用随命令传下去即可 —— 人点一次就该跑通。
    species = ({
      sp0 <- ""
      if (nzchar(qs) && isTRUE(HAS_SESSIONS)) {
        s9 <- tryCatch(sess_find(APP_DIR, qs), error = function(e) NULL)
        sp0 <- as.character((s9 %||% list())$species %||% "")
      }
      species_canon(sp0)
    }),
    reference_id = refId,
    reference_name = refName,
    reference_source = refSource,
    reference_xlsx = refFile,
    # ---- local 档用 ----
    engine  = if (nzchar(work)) pjoin(work, "_engine") else "<引擎目录>",
    scripts = if (nzchar(scr)) scr else "<脚本目录>",
    run_dir = if (nzchar(work)) work else "<结果目录>",
    indir   = if (nzchar(data)) data else "<数据目录>",
    outdir  = if (nzchar(work)) pjoin(work, "results") else "<结果目录>/results"
  )
}

# 服务器模式的步骤清单。
#   为什么必须从 flow_spec 取：以前这里一直返回 step_plan 的那 8 步
#   （env / decide / precheck / pipeline1 / pipeline2 / plots / ppt / pack），
#   那是应用自带 sc-pipeline 的汉程，服务器上根本没有对应脚本 ——
#   服务器上没有 00_run_all.R、没有 pipeline.json、没有 results/。
#   于是页面上一整排「交给助手去跑」，点了发出去的指令指向不存在的文件；
#   而真正在跑的那条链（1.doublet → 3.merge → 5.markers …）一个都没显示。
#   这不是"文案问题"，是清单本身错了。
flow_server_steps <- function() {
  if (!HAS_FLOW) return(list())
  lapply(FLOW_STAGES("server"), function(st) {
    list(id = st$id, title = st$title, note = st$note,
         needGate = FALSE, gatePhase = NULL,
         artifacts = as.list(st$artifacts %||% character(0)))
  })
}

# 某个阶段内部有哪些环节（给进度回写与页面可视化用同一份清单）
flow_subs_of <- function(stepId) {
  if (!HAS_FLOW) return(list())
  prof <- flow_profile()
  if (identical(as.character(stepId), "all")) {
    out <- list()
    for (id in FLOW_ORDER(prof)) {
      st <- flow_stage(id, prof)
      if (is.null(st)) next
      for (sb in (st$subs %||% list())) {
        out[[length(out) + 1]] <- list(stage = id, stage_title = st$title,
                                       sub = sb$id, title = sb$title)
      }
    }
    return(out)
  }
  st <- flow_stage(stepId, prof)
  if (is.null(st)) return(list())
  lapply((st$subs %||% list()), function(sb)
    list(stage = st$id, stage_title = st$title, sub = sb$id, title = sb$title))
}

remote_step_instruction <- function(stepId, pl = NULL, session_id = "", req_id = "",
                                    gate_note = "", binding = list()) {
  srv <- flow_record_server((pl %||% list())$server %||% list(), session_id)
  task <- STEP_TASK[[stepId]] %||% list(title = stepId, ask = "按流程跑这一步", writes = "")
  V <- flow_vars(srv, sid = session_id)

  credential_note <- paste0(
    "云端应用配置：",
    normalizePath(file.path(APP_DIR, "config", "server_profile.json"), mustWork = FALSE),
    "（仅供具备云端本地读取权限的执行者在授权范围内本地读取；不能把云端路径当作工作电脑路径）。",
    "工作电脑先复用已核验且适用于本次 host、port、user 和授权设备的合法连接配置或凭据提供者；不得仅因文件创建时间早就判失效，不扫描其他账号或任意脚本搜口令。",
    "密码认证使用私密进程环境 SC_REMOTE_PWD 交给 engine/workpc_check.py --transport ssh；不要把只试默认密钥且未提交密码的 BatchMode SSH 失败当作密码被拒绝。",
    "应用消息与直接聊天必须沿用同一授权连接通道；没有提交认证信息时记录 authentication_not_supplied，实际认证拒绝才记录 ssh_authentication_failed，并保留脱敏认证方式、异常类型和退出码。",
    "无可用凭据则回写真实失败，不向用户索要配置路径。",
    "口令不得进入对话、命令参数或日志，不得向浏览器输出口令。")
  if (identical(as.character(srv$exec_pc %||% ""), "PC-20250808YJEP") &&
      identical(as.character(srv$host %||% ""), "10.1.1.2") &&
      identical(as.integer(srv$port %||% 22), 22L) &&
      identical(as.character(srv$user %||% ""), "hhhong1")) {
    credential_note <- paste0(credential_note,
      " 已核验连接入口仅限本次获授权的 PC-20250808YJEP：",
      "C:/Users/Administrator/AppData/Local/Temp/pa_ssh.py。",
      "体检使用 engine/workpc_check.py --transport ssh --expected-device PC-20250808YJEP ",
      "--connector-sha256 5653441dc0130770190012b24cbda124ec2c6dc5307312cc88ca6c0df2f53760 --legacy-connector 指向该脚本；",
      "主机、端口、账号不符或文件指纹变化即拒绝。命令仅经标准输入传入，不提取口令，不逐机安装。",
      "其他阶段只复用同一已核验程序的既有命令入口，不把体检入口当成全分析接口；保持本次 session_id、req_id 的回写绑定。")
  }
  L <- c("【来自 个性化分析助手】",
         paste0("要办的事：在服务器上执行「", task$title, "」。", task$ask),
         "跨聊天执行：原聊天号仅作来源与回音参考，不是执行会话或设备的限制；当前获授权会话可接续同一分析，不扩大数据或设备权限。",
         "保留原 session_id、req_id；先查现有任务、队列和实际进度，避免重复启动；已有任务则接续跟踪与回写，不因换聊天重新提交。")
  if (length(binding)) L <- c(L, paste0(
    "本次执行绑定（两种执行方式共用，必须原样回传）：attempt_id=",
    as.character(binding$attempt_id %||% "（未登记）"),
    "；run_id=", as.character(binding$run_id %||% "（未取到）"),
    "；version_id=", as.character(binding$version_id %||% "（未取到）"),
    "；executor_mode=", as.character(binding$executor_mode %||% "（未标注）"),
    "。qsub 作业号只是执行端句柄，不得覆盖 attempt_id。"))
  if (nzchar(gate_note)) L <- c(L, paste0("人已经定下的事：", gate_note))

  L <- c(L, "",
         paste0("目标服务器：", as.character(srv$host %||% "（未填）"), ":",
                as.character(srv$port %||% 22)),
         paste0("登录账号：", as.character(srv$user %||% "（未填）")))
  epc <- as.character(srv$exec_pc %||% "")
  L <- c(L, if (nzchar(epc)) paste0("历史执行电脑：", epc, "（仅供参考，不锁定设备）")
            else "历史执行电脑：未记录。")
  L <- c(L, "执行设备按本次实际可用且获授权的设备选择；回写 exec_pc 为实际执行设备名、host 为实际服务器，不得照抄历史设备。")
  if (nzchar(V$run_dir %||% ""))  L <- c(L, paste0("结果目录：", V$run_dir))
  if (nzchar(V$indir  %||% ""))   L <- c(L, paste0("数据目录：", V$indir))
  if (nzchar(V$scripts %||% ""))  L <- c(L, paste0("主流程脚本目录：", V$scripts))
  if (nzchar(V$engine %||% ""))   L <- c(L, paste0("引擎目录：", V$engine,
        "（env_check.R / sc-plot-kit / sc-ppt-kit 在这一层；",
        "服务器上还没有就先从应用取一次：GET /api/engine-pack）"))
  if (nzchar(V$rscript %||% ""))  L <- c(L, paste0("R 程序：", V$rscript))
  rt <- srv$runtime %||% list()
  if (nzchar(as.character(rt$queue %||% ""))) L <- c(L, paste0("队列：", rt$queue))
  if (nzchar(as.character(rt$node_constraint %||% "")))
    L <- c(L, paste0("节点约束：", rt$node_constraint,
                     "（注意：只有带这个约束的节点才看得到原始数据）"))

  # ---- 这一件事到底跑哪条命令：直接给出来，不让执行端自己推 ----
  prof <- flow_profile()
  stepIds <- if (identical(as.character(stepId), "all")) FLOW_ORDER(prof) else as.character(stepId)
  if (identical(as.character(stepId), "config")) {
    L <- c(L, "",
           "怎么做：",
           paste0("1. ", credential_note),
           paste0("2. 在 ", V$run_dir, " 下按应用里现在这份设置写 pipeline.json 与 gates.json，"),
           "   并登记守卫指纹（引擎目录下的 workflow/guard.R）。",
           "3. 不要先做连通性试探、不要逐台机器试连；连不上就把这条当作结果写回。",
           "4. 跑完回写：POST /api/step/import，body 带 step=config、status、code、log、session_id。")
  } else {
    # ---- 这一轮到底有哪些阶段：由应用给出权威名单 ----
    #   为什么必须写在指令里：以前只给"这一步"，执行端于是拿服务器上那份
    #   老引擎去推断"有没有这条路"。老引擎按 env→pack 那套写的，
    #   自然不认识注释/出图/富集，于是得出"引擎里没有这条执行路径"的结论，
    #   再由此提出"改引擎"或"伪造入口变量"两条路 —— 两条都是错的方向。
    #   名单由应用给，执行端不再自己推。
    if (HAS_FLOW) {
      allIds <- tryCatch(FLOW_ORDER(prof), error = function(e) character(0))
      if (length(allIds))
        L <- c(L, "",
               "本流程的阶段（应用是唯一来源，服务器上那份老引擎不认识这些名字，不要拿它判断）：",
               paste0("  ", paste(allIds, collapse = " → ")))
      L <- c(L, "",
             "入口三个环境变量由执行端提交时从现有合法入口导出，沿用已登记的运行上下文。",
             "不要仅因应用未附远端核验凭证就停止或回写失败；以实际提交和脚本守卫返回为准。",
             "如果实际入口或守卫报错，保留原始报错回写；不要伪造变量、绕过守卫或修改台账。")
    }
    L <- c(L, "", "怎么做（顺序不能变、不能跳步、不许另写脚本顶替）：")
    L <- c(L, paste0("1. ", credential_note))
    k <- 2
    for (id in stepIds) {
      st <- if (HAS_FLOW) flow_stage(id, prof) else NULL
      cmd <- if (HAS_FLOW) flow_cmd(id, V, prof) else NULL
      title <- if (!is.null(st)) st$title else (STEP_TASK[[id]] %||% list(title = id))$title
      if (is.null(cmd)) {
        ex <- if (!is.null(st)) flow_execution(st, V) else list(errors = "未登记执行合同")
        L <- c(L, paste0(k, ". ", title, "：当前不可提交，", paste(ex$errors, collapse = "；")),
               "   不许猜值、替换输入或启动分析。先回写 failed、code=2 和实际缺项（未启动分析）。")
        k <- k + 1
        next
      }
      L <- c(L, paste0(k, ". ", title, "（阶段 id = ", id, "）照这条命令跑："))
      L <- c(L, paste0("   ", cmd))
      execution <- flow_execution(st, V)
      if (identical(execution$script %||% "", "-e"))
        L <- c(L, "   会话物种为空：识别程序已随上面的Rscript -e命令完整携带，无需另装脚本；由程序读取本阶段实际10x features或Seurat RNA特征识别，记录证据后传规范物种给原脚本。证据不足、混合物种或读取失败必须非零退出并回写，禁止让用户填值、按路径/参考表猜测或跳过。")
      if (!is.null(st) && length(st$artifacts))
        L <- c(L, paste0("   跑完必须有：", paste(st$artifacts, collapse = "、")))
      # ---- 这个阶段要用到的 C 类补充脚本 ----
      #   为什么要在这里写出来：这类脚本服务器上没有，只有应用库里有。
      #   不写这一段，执行端就只能"看情况自己补一个"——补出来的东西没有出处、
      #   没有版本，下一个人跑又是另一份。所以该调用的一次都不许漏。
      sup <- supplement_for_stage(id)
      if (length(sup)) {
        L <- c(L, "   脚本分发与依赖校验（同一脚本只执行上面的一条命令，不重复运行）：")
        for (e in sup) {
          fp <- supplement_path(e)
          md5 <- if (file.exists(fp)) unname(tools::md5sum(fp)) else "（库文件缺失，先停下）"
          # 参考表这类"本次才有的输入"没准备好时，占位符会原样留着。
          #   这时必须把"缺什么、去哪补"写清，而不是让执行端拿着
          #   "{reference_xlsx}" 这个字符串去猜 —— 猜出来的结果没人能复核。
          argvRaw <- vapply((e$argv %||% list()), function(x) as.character(x), "")
          if (any(grepl("{reference_xlsx}", argvRaw, fixed = TRUE)) &&
              !nzchar(V$reference_xlsx %||% "")) {
            L <- c(L, "   · 先停一下：这一步需要【注释依据（参考表）】，本次分析还没有选用。")
            L <- c(L, "     请在应用的「注释依据」一栏选一份表（上传、从服务器扫描、或用文献库检索生成都可以），")
            L <- c(L, "     选好之后重新取一次这条指令 —— 那时占位符会变成具体路径。")
            L <- c(L, "     不许跳过注释：出图的差异段与富集段都要 annot 列。")
          }
          argv <- paste(vapply((e$argv %||% list()),
                               function(x) paste0('"', supplement_fill(x, V), '"'), ""),
                        collapse = " ")
          L <- c(L, paste0("   · ", as.character(e$title %||% e$id %||% ""),
                           "（库条目 ", as.character(e$id %||% ""), "）："))
          # 指令里要把"这是哪个文件"说清，而不是只丢一串指纹。
          #   执行端看到"库条目 C_annotate"和 32 位十六进制，无从判断对不对。
          L <- c(L, paste0("     这是哪个文件：", as.character(e$file %||% ""),
                           "（", as.character(e$title %||% ""), "）"))
          L <- c(L, paste0("     取一次：GET /api/supplement/get?id=", as.character(e$id %||% "")))
          L <- c(L, paste0("     落地后核对文件指纹（md5）应为 ", md5,
                           " —— 这是内容指纹，用来确认你拿到的与应用里这份完全一致；",
                           "对不上就停下回报，不要凑合跑。"))
          ex <- flow_execution(st, V)
          target_script <- if (identical(ex$script %||% "", "-e")) ex$argv[3L] else ex$script
          same_script <- identical(basename(fp), basename(target_script %||% "")) &&
            endsWith(target_script %||% "", as.character(e$file %||% ""))
          if (same_script) {
            L <- c(L, paste0("     安装到主命令脚本路径：", target_script),
                   "     此条仅分发文件，不是第二条运行命令；安装并校验后，只执行上面的主命令一次。",
                   "     须保留引擎目录结构与 workflow/guard_load.R、workflow/guard.R；注释还需 scripts/lib/core_functions.R。")
          } else {
            L <- c(L, paste0("     补充脚本独立运行：", V$rscript, " <落地后的路径> ", argv))
          }
          if (length(e$writes))
            L <- c(L, paste0("     产出：", paste(vapply(e$writes, function(x)
              supplement_fill(x, V), ""), collapse = "、")))
          if (isTRUE(e$refuses_old_data))
            L <- c(L, "     只许读本题次刚产出的输入；任何输入文件的 mtime 早于本题次开始时间，就停下来报错，不许照旧跑完。")
        }
      }
      k <- k + 1
    }
    L <- c(L, paste0(k, ". 任何一步失败就停在那一步，把失败原因原样写回，"))
    L <- c(L, "   不许跳过去跑下一步，不许降级、不许抽样顶替、不许自己改参数。")
  }

  # ---- 进度怎么回写：环节级，带时间 ----
  subs <- flow_subs_of(stepId)
  L <- c(L, "",
         "回写（不回写等于没跑，页面只能靠这一段知道跑到哪了）：",
         "· 每个环节开始和结束各回写一次：POST /api/step/import",
         paste0("  body 基础字段：step=", stepId, "，session_id=", session_id,
                "，req_id=", req_id, "；sub 使用下列值，带实际 started_at、ended_at、log。"))
  if (length(subs)) {
    L <- c(L, paste0("· 这条任务包含的环节（sub 取值就用前面这一串）：",
                     paste(vapply(subs, function(x) x$sub, ""), collapse = " → ")))
  }
  L <- c(L, "· 整步跑完再回写一次总状态：status = done 或 failed，带 code、started_at、ended_at。",
         "· 执行时捕获真实 stdout 与 stderr（例如命令 > run.log 2>&1），将真实日志正文放入 log 回写；保留上面原 session_id、req_id，不新建或替换。",
         "· 每次回写 exec_pc、host 为实际执行设备与服务器；未取得日志正文就留空并说明未提供，不得编造日志或把状态描述当输出。这里是分次回写的日志快照，不是流式 stdout。")

  # ---- 产物清单也要回写 ----
  #   应用在云端，到不了内网，看不到服务器磁盘。不回写清单，
  #   页面「产物下载」里就永远是空的 —— 而文件明明都在。
  #   这不是"没产物"，是"没人告诉它有什么"。
  L <- c(L, "",
         "产物清单也要回写（应用看不到服务器磁盘，不回写这里就永远是空的）：",
         paste0("· 跑完在结果目录下执行一次 find，把清单回写 POST /api/artifacts/import："),
         paste0("  find ", if (nzchar(V$work_root %||% "")) V$work_root else "{结果目录}",
                " -type f \\( -name '*.png' -o -name '*.pdf' -o -name '*.xls' -o -name '*.xlsx'",
                " -o -name '*.csv' -o -name '*.rds' -o -name '*.pptx' -o -name '*.zip' \\)",
                " -printf '%p\\t%s\\t%TY-%Tm-%TdT%TH:%TM\\n'"),
         "  body 形如：{\"outdir\":\"<结果目录>\",\"host\":\"<主机>\",\"items\":[{\"rel\":\"10.figs/01_umap.png\",\"size\":12345,\"mtime\":\"2026-10-20T15:21\"}]}",
         "· rel 用相对结果目录的路径，别用绝对路径。")

  # ---- 质检：不再让执行端去跑服务器上的 reviewer.R ----
  #   为什么撤掉：服务器上那份 reviewer.R 是另一份副本，阶段名单按老流程写
  #   （env/decide/precheck/pipeline1/plots/ppt/pack），本流程的
  #   annotate / figs / enrich 它一个都不认识，跑起来只会判"阶段名不对"。
  #   现在质检由应用侧在回写时自判：回写状态、产物清单、运行目录、日志有无
  #   失败字样 —— 判据都是应用自己拿得到的事实，不依赖服务器上装了什么。
  #   执行端要做的只有一件事：照命令跑，跑完把真实状态和产物清单回写。
  if (HAS_FLOW && !identical(as.character(stepId), "config")) {
    L <- c(L, "",
           "· 跑完不要另外跑质检脚本：质检由应用在你回写时按状态、产物清单、",
           "  运行目录、日志失败字样自己判。你只要把真实情况写全，别把没跑的写成跑完。")
  }

  # ---- 顺手报一次服务器引擎快照 ---------------------------------------
  #   为什么要把这一条钉进指令（2026-11-04，审核第 10 条的收尾）
  #     应用侧有一个检查：「服务器那份 _engine 到底是什么版本？」
  #     它读 state/server_engine_snapshot.json，读不到就判 unknown。
  #     接口早就加了（POST /api/engine/snapshot），但只在有人手动调用时才有数据 ——
  #     而"没人会记得手动调"正是一切巡检类功能的死因。
  #     钉进每一次下发指令，执行端跑完顺手报一次，这条检查才会自己长出答案。
  #   为什么必须由执行端报，而不是应用去探：
  #     服务器在内网，应用在云端根本连不上。应用能做的只有收。
  #   为什么只报"它认得的阶段名"而不报整个文件清单：
  #     要判的是「服务器那份认不认识本流程」，阶段名就够。
  #     报文件清单会让每次下发的载荷变大，而结论并不会更准。
  if (HAS_FLOW && !identical(as.character(stepId), "config")) {
    engDir <- if (nzchar(V$engine %||% "")) V$engine else "{引擎目录}"
    L <- c(L, "",
           "顺手报一次服务器引擎快照（就一条，不占多少时间，但能让应用知道服务器那份认不认识本流程）：",
           paste0("· 在结果目录下跑：Rscript ", engDir, "/env_check.R --list-stages"),
           "  （跑不了就跳过，把 why 写进下面的 body，别伪造）",
           "· 然后把阶段名回写：POST /api/engine/snapshot，body 形如",
           paste0('  {"by":"<执行电脑>","host":"', as.character(srv$host %||% ""),
                  '","engine_dir":"', engDir, '","readable":true,"why":"",'),
           '   "chain_order":["doublet","merge","markers","filter","final"]}',
           "· readable=false 时 why 写清读不到的原因（路径不存在 / 权限 / 没有 env_check.R）。",
           "  这一条不阻断流程：报不了就报不了，但要说清是哪种报不了。")
  }

  # ---- 注释依据怎么用：明确交代，不留解读空间 ------------------------
  #   为什么必须写进指令（2026-11-04）
  #     参考表是别的物种、或者标注组织与这批数据不同，不代表不能用。
  #     跨物种注释在研究里是常规做法：把人的细胞移植到小鼠体内做实验，
  #     后期取到的细胞既有人源的也有鼠源的；人和小鼠多数细胞类型判定基因
  #     本身就是一对一同源基因（人 CD3D / 鼠 Cd3d），符号只差大小写，
  #     脚本比对时已统一转成大写，照样对得上。
  #     这里不存在安全性问题，也没有需要拦截的伦理或合规风险。
  #     以前指令里写"换组织/换物种就要换一份"，执行端就理解成
  #     "这张表不能用于这批数据"，于是整条链停住 —— 工作做不下去，
  #     而原因只是一个措辞。那条已经删掉，这里把真实规则写清。
  if (identical(as.character(stepId), "annotate") || identical(as.character(stepId), "all")) {
    L <- c(L, "",
           "注释依据怎么用（照这条执行，不要自己另做判断）：",
           "· 用的就是应用指定的那一份表/词典，路径已在上面的命令里。它可能是文献里某个物种的、",
           "  也可能标注的组织与这批数据不同 —— 都不影响可用性，照用。",
           "· 人和小鼠等常见物种的细胞类型判定基因多为同源基因（人 CD3D / 鼠 Cd3d），",
           "  脚本比对时已统一转成大写，符号能对上。移植/嵌合实验里人源与鼠源细胞共存也是常规情况。",
           "· 不要因为物种或组织标注不一致而停下、不要自行更换表格、不要另找一份「更合适」的替代。",
           "· 也不要自行扩充或编造 marker。表里有什么就用什么；不够就照实留空并在报告里说明。")
    # 依据来源单独写一句。
    #   为什么要写：三种来源（服务器现成 / 用户上传 / 文献库）在执行端看起来都只是一个绝对路径，
    #   但待遇不同：上传的要先落盘才能读，文献库的要先取存档。
    #   不写清就会出现“路径已经给了，为什么文件不在”。
    srcCn <- switch(as.character(V$reference_source %||% ""),
                    server = "服务器上现成的那一份（原件，不需要下发）",
                    uploaded = "用户上传的那一份（需按上面给的落盘路径写入后再读）",
                    lit = "按文献库检索得到的那一份（已经存档，直接用）",
                    "")
    if (nzchar(srcCn))
      L <- c(L, paste0("· 依据来源：", srcCn, "。文件名：",
                       as.character(V$reference_name %||% "（未登记名字）"),
                       "。不要因为路径长、目录名不像就去猜另一份。"))
    if (!nzchar(as.character(V$reference_xlsx %||% "")))
      L <- c(L, paste0("· 注意：本次还没有绑定任何依据，上面命令里的 {reference_xlsx} 会原样留着。",
                    "请先停下回报，不要自己找一份顶上。"))
  }

  if (nzchar(req_id)) L <- c(L, "", paste0("本次请求号：", req_id,
                                           "（回写时请在 body 里带上 req_id）"))
  if (nzchar(session_id)) L <- c(L, paste0("本次分析记录号：", session_id,
                                           "（回写时请带上 session_id，结果会挂在这一份分析下）"))

  # ---- 上一轮为什么没过：直接写进指令，不让执行端凭原样再跑一次 ----
  #   为什么必须放在这里：普通「重跑这一环」原来不带任何失败原因，
  #   agent 只能按原样再跑一遍，然后被同一条判据再打回一次 ——
  #   返工单里的 failed_checks / must_do / command_hint 明明已经有答案，
  #   却没走到这条路上来。这一节就是把已有答案搬进这次指令。
  if (nzchar(session_id) && exists("REWORK_FILE") && isTRUE(file.exists(REWORK_FILE))) {
    prev <- tryCatch({
      r <- db_rows(DB_CON,
        "SELECT * FROM rework_tickets WHERE version_id = ? AND step_id = ?
         ORDER BY issued_at DESC LIMIT 1",
        list(db_api_version_of(DB_CON, session_id)$version_id %||% NA_character_,
             if (identical(as.character(stepId), "all")) "__none__" else as.character(stepId)))
      if (!nrow(r)) NULL else as.list(r[1, ])
    }, error = function(e) NULL)
    if (!is.null(prev) && nzchar(as.character(prev$ticket_id %||% ""))) {
      txt <- tryCatch(rework_instruction_text(DB_CON, as.character(prev$ticket_id)),
                      error = function(e) NULL)
      if (!is.null(txt) && length(txt)) {
        L <- c(L, "",
               "【上一轮没过，为什么不过 —— 这次先按这里改，别原样再跑一遍】",
               paste(txt, collapse = "\n"))
      }
    }
  }

  # ---- 这台机器上踩过的坑：每次都给，不指望执行端记得 ----
  #   这几条都是真实发生过的误判来源，写在这里比写进文档有用 ——
  #   写进文档没人读，写进指令每次都会被执行端看到。
  L <- c(L, "",
         "【这台服务器上踩过的坑，动手前先对一遍】",
         "· R 不要用 PATH 上的那个：那是 R 3.6.0，包全装在 R 4.1.2 下，",
         "  用它跑会稳定报「there is no package called ...」，看起来像包全缺，其实是找错了 R。",
         "  必须用上面写的那条 R 程序路径。",
         "· 加载 R 包前必须设 LD_LIBRARY_PATH 到上面写的那个 hdf5 目录，",
         "  否则版本号读得出来、library() 一律失败在 dyn.load —— 这也像包缺，其实不是。",
         "· 只数目录里有几个文件，不足以判断数据可用：这批样本是符号链接，",
         "  要 readlink -f 逐个验到目标，并确认目标里有 barcodes / features / matrix 三件套。",
         "· 应用给的是阶段名单的唯一来源。服务器上那份老引擎不认识 annotate / figs / enrich，",
         "  不要拿它来判断「有没有这条执行路径」。")

  paste(L, collapse = "\n")
}
# =====================================================================
# 起一个执行器进程（后台，不等待）
# =====================================================================
launch_step <- function(stepId, runDir) {
  if (is.null(runDir) || !dir.exists(runDir))
    return(list(ok = FALSE, error = "还没选运行目录"))
  cfg <- read_cfg(runDir)
  if (is.null(cfg))
    return(list(ok = FALSE, error = "这个运行目录里还没有 pipeline.json。先点「生成一份默认设置」。"))

  # 本地 ENTRY_FILE（sc_workflow.R）source step_plan.R，支持的是 CHAIN_ORDER；
  # STEP_ORDER 属于远程 flow，不能据此向本地入口发送 flow id。
  if (!is.character(stepId) || length(stepId) != 1L || is.na(stepId) || !nzchar(stepId))
    return(list(ok = FALSE, error = "步骤必须是一个非空字符串"))
  valid <- CHAIN_ORDER
  if (!identical(stepId, "all") && !stepId %in% valid)
    return(list(ok = FALSE, error = paste0("未知步骤：", stepId)))

  if (step_is_running(runDir))
    return(list(ok = FALSE, error = "上一步还在跑。等它结束，或者点「停止」。"))

  # ---- 闸门检查：在这里同步做掉，别等启动器起来再说 ----
  # 为什么：如果只在 run_chain.R 里查，页面会先收到「已开始」，几秒后才变成
  #   「被拦住」。使用者看到的是「点了按钮、提示成功、然后失败」，很困惑。
  #   在这一步先查，页面当场就能给出「还差哪几件事」，不产生假成功。
  #
  # 按段位查，不笼统查：
  #   上段（pipeline1）要 A 段（数据在哪、注释依据、质控门槛、读入前剔样本）
  #   下段（pipeline2）要 B 段（筛群、剔样本、低置信门槛）
  #   B 段那几条的依据只有跑到一次聚类才产生。笼统查会让上段被还没到该问的
  #   问题挡住 —— 永远跑不到能产生依据的那一步，卡死。
  gatesWanted <- if (identical(stepId, "all")) {
    # 一键全流程：从头跑到尾，两段的要求都要满足
    c("A", "B")
  } else {
    hit0 <- Filter(function(s) identical(s$id, stepId), build_steps(cfg, runDir, ENGINE_DIR))
    if (length(hit0) > 0 && isTRUE(hit0[[1]]$needGate)) {
      ph <- step_gate_phase(stepId)
      if (is.null(ph)) character(0) else ph
    } else character(0)
  }
  for (ph in gatesWanted) {
    pend <- pending_gate_names(runDir, ph)
    if (!is.null(pend) && length(pend)) {
      where <- if (identical(ph, "A")) "跑主流程之前" else "一次聚类之后、继续往下之前"
      return(list(ok = FALSE, blocked = TRUE,
                  error = sprintf("%s要定 %d 件事：%s。请先在上面答完并保存。",
                                  where, length(pend), paste(pend, collapse = "；"))))
    }
  }

  dir.create(status_dir(runDir), showWarnings = FALSE, recursive = TRUE)
  rs <- rscript_bin(cfg)

  # ---- 一律走统一入口 ----
  #   网页也是入口之一，但它同样必须经过 sc_workflow.R。
  #   只有入口会写台账、设守卫环境变量。绕过它，
  #   用了哪些参数、什么时候跑的、产物指纹是什么，事后就查不到 ——
  #   而这三件事正是「能不能复现」的全部依据。
  #
  #   参数顺序必须是「命令在前」：sc_workflow.R <cmd> --dir <目录>。
  #   把 --dir 放前面，入口会把第一个参数当成子命令，行为变得莫名其妙。
  a <- c("--vanilla", ENTRY_FILE)
  if (identical(stepId, "all")) {
    a <- c(a, "run-all")
  } else {
    a <- c(a, "run", "--step", stepId, "--force")
  }
  a <- c(a, "--dir", runDir)

  logf <- step_log_file(runDir)

  # 先把状态置成 running。入口/执行器起来后会用真实 pid 覆盖它，
  # 避免界面在这几秒里显示成空闲。
  write_json_file(step_state_file(runDir),
                  list(status = "running", step = if (identical(stepId, "all")) "all" else stepId,
                       code = NULL, started = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), pid = NULL))

  # 后台启动，输出直接落到 run.log。
  #   这里传的是文件名而不是连接：R 在把「连接」交给子进程当 stdout 时，
  #   多数平台会静默丢掉子进程输出 —— 日志里只剩自己写的头尾，报错一个字看不到。
  ok <- tryCatch({
    system2(rs, a, stdout = logf, stderr = logf, wait = FALSE)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    write_json_file(step_state_file(runDir),
                    list(status = "failed", step = stepId, code = 127L,
                         started = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    return(list(ok = FALSE, error = "执行器起不来。检查一下 R 路径是否正确。"))
  }

  list(ok = TRUE, started = stepId)
}

# ---- 停止：连子进程一起收拾，不然分析脚本还在后台吃内存 ----
stop_step <- function(runDir) {
  st <- read_step_state(runDir)
  if (!identical(as.character(st$status), "running"))
    return(list(ok = TRUE, msg = "当前没有在跑的步骤。"))
  pid <- suppressWarnings(as.integer(st$pid %||% NA))
  if (is.na(pid)) {
    write_json_file(step_state_file(runDir),
                    list(status = "stopped", step = st$step, code = NULL,
                         started = st$started, ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    return(list(ok = TRUE, msg = "已标记为停止。"))
  }
  if (IS_WIN) {
    system2("taskkill", c("/PID", as.character(pid), "/T", "/F"),
            stdout = FALSE, stderr = FALSE, wait = TRUE)
  } else {
    # 先收子进程，再收自己
    kids <- tryCatch(suppressWarnings(system2("pgrep", c("-P", as.character(pid)),
                                              stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    for (k in kids) {
      kk <- suppressWarnings(as.integer(k))
      if (!is.na(kk)) try(system2("kill", c("-TERM", as.character(kk)), stdout = FALSE, stderr = FALSE), silent = TRUE)
    }
    try(system2("kill", c("-TERM", as.character(pid)), stdout = FALSE, stderr = FALSE), silent = TRUE)
  }
  write_json_file(step_state_file(runDir),
                  list(status = "stopped", step = st$step, code = NULL,
                       started = st$started, ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  list(ok = TRUE, msg = "已停止。已经算出来的中间结果不会删，下次可以从这一步继续。")
}

# =====================================================================
# 注意：init_config / autofill_environment / detect_memory_gb 都定义在
#   engine/step_plan.R 里，不在这里重复一份。
#
#   为什么：网页入口和命令行入口必须生成一模一样的配置文件。
#   两处各写一份，早晚会不一致 —— 而不一致的配置会让「同样的参数跑出
#   不同结果」这种最难查的问题出现。所以只留一个出处。
# =====================================================================
# 产物清单
# =====================================================================
ARTIFACT_KINDS <- list(".pptx" = "PPT", ".zip" = "打包", ".html" = "报告", ".md" = "报告",
                       ".png" = "图", ".pdf" = "图", ".xls" = "表", ".csv" = "表",
                       ".tsv" = "表", ".txt" = "报告")
SKIP_EXT <- c(".rds", ".rdata", ".rdata")

list_artifacts <- function(runDir) {
  cfg <- read_cfg(runDir)
  outdir <- cfg_val(cfg, "io", "outdir", NULL)
  if (is.null(outdir) || !length(outdir) || !nzchar(as.character(outdir))) outdir <- runDir
  outdir <- as.character(outdir)
  if (!dir.exists(outdir)) return(list(ok = TRUE, outdir = outdir, items = list()))
  files <- list.files(outdir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  items <- list()
  out_norm <- gsub("\\\\", "/", outdir)
  for (f in files) {
    if (dir.exists(f)) next
    fn <- gsub("\\\\", "/", f)
    # status 目录里放的是运行状态，不是给使用者下载的产物
    if (grepl("/status/", fn)) next
    ext <- paste0(".", tolower(tools::file_ext(f)))
    if (ext %in% SKIP_EXT) next
    kind <- ARTIFACT_KINDS[[ext]]
    if (is.null(kind)) next
    info <- file.info(f)
    rel <- sub(paste0("^", out_norm, "/?"), "", fn)
    items[[length(items) + 1L]] <- list(
      rel = rel, name = basename(f), kind = kind,
      size = as.numeric(info$size),
      mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S"), full = f
    )
  }
  if (length(items)) {
    ord <- order(vapply(items, function(x) x$mtime, character(1)), decreasing = TRUE)
    items <- items[ord]
  }
  list(ok = TRUE, outdir = outdir, items = items)
}

# =====================================================================
# 目录浏览
# =====================================================================
pct_decode <- function(s) {
  if (is.null(s) || !nzchar(s)) return(s)
  out <- raw(0); i <- 1L; n <- nchar(s, type = "chars")
  while (i <= n) {
    ch <- substr(s, i, i)
    if (ch == "%" && i + 2L <= n) {
      v <- suppressWarnings(strtoi(substr(s, i + 1L, i + 2L), 16L))
      if (!is.na(v)) { out <- c(out, as.raw(v)); i <- i + 3L; next }
    }
    out <- c(out, charToRaw(enc2utf8(ch))); i <- i + 1L
  }
  tryCatch(rawToChar(out), error = function(e) s)
}

list_dir <- function(dir) {
  entries <- list.files(dir, all.files = FALSE, full.names = TRUE, no.. = TRUE)
  dirs <- list(); files <- list()
  for (e in entries) {
    info <- tryCatch(file.info(e), error = function(x) NULL)
    if (is.null(info) || is.na(info$isdir)) next
    nm <- enc2utf8(basename(e))
    if (startsWith(nm, ".")) next
    if (isTRUE(info$isdir)) {
      dirs[[length(dirs) + 1L]] <- list(name = nm, mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S"))
    } else {
      files[[length(files) + 1L]] <- list(name = nm, size = as.numeric(info$size))
    }
  }
  if (length(dirs)) dirs <- dirs[order(vapply(dirs, function(x) x$name, character(1)))]
  if (length(files)) files <- files[order(vapply(files, function(x) x$name, character(1)))]
  list(dirs = dirs, files = files)
}

drives <- function() {
  if (!IS_WIN) return(list("/"))
  out <- list()
  for (L in LETTERS) { d <- paste0(L, ":/"); if (dir.exists(d)) out[[length(out) + 1L]] <- d }
  out
}
default_browse_dir <- function() {
  if (IS_WIN) { d <- drives(); if (length(d)) return(d[[1]]); return("C:/") }
  path.expand("~")
}

# =====================================================================
# HTTP
# =====================================================================
STATUS_TEXT <- c(`200` = "OK", `202` = "Accepted", `400` = "Bad Request",
                 `403` = "Forbidden", `404` = "Not Found",
                 `405` = "Method Not Allowed", `409` = "Conflict",
                 `422` = "Unprocessable Entity", `429` = "Too Many Requests",
                 `500` = "Internal Server Error", `503` = "Service Unavailable")

send_raw <- function(con, code, ctype, payload_raw) {
  # 状态码不在表里时要退回一个通用短语，不能用 [[ ]] 硬取 ——
  #   取不到会抛 subscript out of bounds，把"该返回 409 的业务拒绝"
  #   变成"500 服务器内部错误"，调用方就分不清是业务拦了还是服务坏了。
  stTxt <- unname(STATUS_TEXT[as.character(code)])
  if (!length(stTxt) || is.na(stTxt)) stTxt <- if (code >= 400 && code < 500) "Client Error" else "Server Error"
  hdr <- paste0("HTTP/1.1 ", code, " ", stTxt, "\r\n",
                "Content-Type: ", ctype, "\r\n",
                "Content-Length: ", length(payload_raw), "\r\n",
                "Cache-Control: no-store\r\n",
                "Connection: close\r\n\r\n")
  writeBin(c(charToRaw(hdr), payload_raw), con)
  flush(con)
}
send_text <- function(con, code, ctype, txt) send_raw(con, code, ctype, charToRaw(enc2utf8(txt)))
send_json <- function(con, code, obj) send_text(con, code, "application/json; charset=utf-8",
                                                jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA))

worker_route_auth <- function(req, claimed_worker_id = "", claimed_pool_id = "",
                              scope = "queue") {
  if (!isTRUE(HAS_WORKER_AUTH) || !exists("worker_request_auth", mode = "function"))
    return(list(ok = FALSE, status = 503L, code = "worker_auth_missing",
                error = "执行端身份模块未加载，拒绝匿名操作。"))
  tryCatch(worker_request_auth(APP_DIR, req, claimed_worker_id, claimed_pool_id, scope),
           error = function(e) list(ok = FALSE, status = 503L,
                                    code = "worker_auth_failed",
                                    error = conditionMessage(e)))
}

http_header_end <- function(buf) {
  if (length(buf) < 4L) return(0L)
  hit <- grepRaw(charToRaw("\r\n\r\n"), buf, fixed = TRUE)
  if (length(hit)) as.integer(hit[[1]]) else 0L
}

http_decode_text <- function(raw) {
  if (!length(raw)) return("")
  txt <- rawToChar(raw)
  if (!validUTF8(txt)) txt <- iconv(txt, from = "GBK", to = "UTF-8", sub = "?")
  txt
}

http_parse_head <- function(head_txt) {
  parts <- strsplit(head_txt, "\r\n", fixed = TRUE)[[1]]
  if (!length(parts) || !nzchar(parts[[1]])) return(NULL)
  first <- strsplit(parts[[1]], " ", fixed = TRUE)[[1]]
  headers <- list()
  if (length(parts) > 1L) {
    for (h in parts[-1]) {
      if (!nzchar(h)) next
      i <- regexpr(":", h, fixed = TRUE)
      if (i < 1) next
      headers[[tolower(trimws(substr(h, 1, i - 1)))]] <- trimws(substr(h, i + 1, nchar(h)))
    }
  }
  list(method = first[1], target = if (length(first) >= 2L) first[2] else "/",
       headers = headers)
}

http_content_length <- function(headers) {
  ch <- (headers %||% list())[["content-length"]]
  if (is.null(ch) || length(ch) != 1L || grepl(",", ch, fixed = TRUE)) return(0L)
  v <- suppressWarnings(as.integer(ch))
  if (length(v) != 1L || is.na(v) || v <= 0L) 0L else v
}

http_request_complete <- function(buf) {
  h <- http_header_end(buf)
  if (!h) return(FALSE)
  head <- http_parse_head(http_decode_text(buf[seq_len(h - 1L)]))
  if (is.null(head)) return(TRUE)
  length(buf) >= h + 3L + http_content_length(head$headers)
}

parse_http_request <- function(buf) {
  h <- http_header_end(buf)
  if (!h) return(NULL)
  head <- http_parse_head(http_decode_text(buf[seq_len(h - 1L)]))
  if (is.null(head)) return(NULL)
  body_start <- h + 4L
  body_raw <- if (length(buf) >= body_start) buf[body_start:length(buf)] else raw(0)
  clen <- http_content_length(head$headers)
  if (clen > 0L && length(body_raw) > clen) body_raw <- body_raw[seq_len(clen)]
  list(method = head$method, target = head$target, headers = head$headers,
       body = http_decode_text(body_raw))
}

read_request <- function(con) {
  buf <- raw(0)
  repeat {
    b <- tryCatch(readBin(con, "raw", 65536L), error = function(e) raw(0))
    if (!length(b)) break
    buf <- c(buf, b)
    if (http_request_complete(buf)) break
    # Keep the old header ceiling, but allow a complete header to keep reading
    # its declared body. This is the same contract callers already had.
    if (!http_header_end(buf) && length(buf) > 131072L) break
  }
  if (!length(buf)) return(NULL)
  parse_http_request(buf)
}

parse_query <- function(target) {
  i <- regexpr("?", target, fixed = TRUE)
  if (i < 0) return(list(path = target, q = list()))
  path <- substr(target, 1, i - 1)
  qs <- substr(target, i + 1, nchar(target))
  q <- list()
  for (kv in strsplit(qs, "&")[[1]]) {
    if (!nzchar(kv)) next
    j <- regexpr("=", kv, fixed = TRUE)
    k <- if (j > 0) substr(kv, 1, j - 1) else kv
    v <- if (j > 0) substr(kv, j + 1, nchar(kv)) else ""
    q[[pct_decode(k)]] <- pct_decode(v)
  }
  list(path = pct_decode(path), q = q)
}

# 取一个 query 参数，不依赖前面有没有 "?"。
#   为什么要单独一个：httpuv 给的 req$query 常常是 "a=1&b=2" 这种、
#   开头不带问号的形式。而 parse_query() 一见没有 "?" 就整段当成路径、
#   返回空 q —— 于是参数全部读不到、调用方还以为"没传值"。
#   这种"查了但什么都没查"的状态最危险：接口照样回 200，
#   看起来通过了，其实一条检查都没做。核对接口踩过一次，写在这里。
qget <- function(target, key) {
  # httpuv 给的 req$query 有三种可能形态：已解析的 named list、"a=1&b=2"、
  #   或带问号的 "?a=1&b=2"。三种都要能取到，否则接口会静默"什么都没查"。
  if (is.list(target)) {
    v <- target[[as.character(key)]]
    if (is.null(v) || !length(v)) return("")
    return(pct_decode(as.character(v)[1]))
  }
  t <- as.character(target %||% "")
  i <- regexpr("?", t, fixed = TRUE)
  if (i > 0) t <- substr(t, i + 1, nchar(t))
  if (!nzchar(t)) return("")
  for (kv in strsplit(t, "&", fixed = TRUE)[[1]]) {
    if (!nzchar(kv)) next
    j <- regexpr("=", kv, fixed = TRUE)
    k <- if (j > 0) substr(kv, 1, j - 1) else kv
    v <- if (j > 0) substr(kv, j + 1, nchar(kv)) else ""
    if (identical(pct_decode(k), as.character(key))) return(pct_decode(v))
  }
  ""
}

MIME <- list(".html" = "text/html; charset=utf-8", ".js" = "text/javascript; charset=utf-8",
             ".css" = "text/css; charset=utf-8", ".json" = "application/json; charset=utf-8",
             ".svg" = "image/svg+xml", ".png" = "image/png", ".jpg" = "image/jpeg",
             ".ico" = "image/x-icon", ".txt" = "text/plain; charset=utf-8")

# 前端构建指纹：取 public/app.js 与 index.html 的大小与修改时间。
#   为什么要它：页面靠 index.html 里的 ?v= 手动改版本号来破缓存，
#   改一次代码忘了改版本号、或者浏览器把旧 app.js 留在内存里，
#   人看到的就是上一版界面 —— 实际发生过：后端已经改成 11 环，
#   页面上仍然列着第 6、7 环和它们的脚本。
#   现在 index.html 里的版本号由这里现算现填，改完代码刷新就是新的。
app_build <- function() {
  fs <- c(file.path(PUBLIC_DIR, "app.js"), file.path(PUBLIC_DIR, "index.html"))
  info <- lapply(fs, function(f) {
    if (!file.exists(f)) return("0")
    paste0(as.integer(file.info(f)$size), ".", as.integer(as.numeric(file.info(f)$mtime)))
  })
  sub("[^0-9a-z]", "", paste0("b", paste(unlist(info), collapse = "")))
}

static_file <- function(rel) {
  rel <- gsub("^/+", "", rel)
  if (!nzchar(rel)) rel <- "index.html"
  if (grepl("\\.\\.", rel)) return(NULL)
  p <- file.path(PUBLIC_DIR, rel)
  if (file.exists(p) && !dir.exists(p)) p else NULL
}

SERVER_WORKER_POOL <- NULL

server_worker_status <- function() {
  p <- SERVER_WORKER_POOL
  if (is.null(p)) return(list(workers = 0L, workers_target = 0L))
  list(workers = length(p$conns), workers_target = as.integer(p$target))
}

serve_request <- function(con, req) {
  pr <- parse_query(req$target)
  p <- pr$path; q <- pr$q

  # Deterministic slow-handler seam for the concurrency contract test. It is
  # inactive unless the test process explicitly sets PA_SERVE_TEST_SLOW_MS.
  if (identical(p, "/__test_slow")) {
    ms <- suppressWarnings(as.integer(Sys.getenv("PA_SERVE_TEST_SLOW_MS", "0")))
    if (length(ms) == 1L && !is.na(ms) && ms > 0L) {
      Sys.sleep(ms / 1000)
      return(send_json(con, 200, list(ok = TRUE, slept_ms = ms)))
    }
  }

  # Public app-shell assets must not pay identity-gateway, database-owner or
  # record-permission costs. A page load asks for a dozen files; routing each
  # one through the authenticated API path serialised the whole first paint.
  if (identical(req$method, "GET") && !startsWith(p, "/api/")) {
    f <- static_file(p)
    if (!is.null(f)) {
      ext <- paste0(".", tolower(tools::file_ext(f)))
      ctype <- MIME[[ext]] %||% "application/octet-stream"
      # 下发 index.html 时把资源版本号换成实时构建指纹。
      #   原先是手写 ?v=，改代码忘了改它就等于没改：浏览器拿旧 app.js，
      #   界面上仍是被删掉的那几环。现算现填后，改完刷新就是新的。
      if (identical(ext, ".html")) {
        raw0 <- readBin(f, "raw", file.info(f)$size + 1024L)
        txt <- rawToChar(raw0)
        txt <- gsub("app\\.js\\?v=[0-9A-Za-z._-]*",
                    paste0("app.js?v=", app_build()), txt)
        return(send_text(con, 200, ctype, txt))
      }
      return(http_send_static(con, req, f, ctype))
    }
  }

  st <- read_state()
  runDir <- st$runDir
  body_cache <- NULL
  body_loaded <- FALSE
  body_json <- function(default = list()) {
    if (!body_loaded) {
      body_cache <<- if (!nzchar(req$body)) list() else
        tryCatch(jsonlite::fromJSON(req$body, simplifyVector = FALSE), error = function(e) list())
      body_loaded <<- TRUE
    }
    if (length(body_cache)) body_cache else default
  }

  # Canonical sid conflict is decided before auth or any side effect. No query/body
  # precedence exists, and record routes no longer compare target against current.
  early_target <- record_session_target(q, body_json(), path = p,
                                        current_sid = cur_sid(), allow_current = TRUE,
                                        require = FALSE)
  if (!isTRUE(early_target$ok) && identical(early_target$code, "session_conflict")) {
    refused <- record_refusal(early_target)
    return(send_json(con, refused$status, refused$body))
  }

  # ---- 每次请求算一次 owner（B1）------------------------------------
  #   算在这里而不是每条路由里各算一次：漏一条路由就是一个越权入口。
  #   代价是每次请求多一次网关问询；身份失败时 fail-closed，见 db_api.R。
  fetch_auth <- fetch_client_request_auth(APP_DIR, req, p, req$method, DB_CON, body_json())
  if (isTRUE(fetch_auth$present) && !isTRUE(fetch_auth$ok))
    return(send_json(con, fetch_auth$status %||% 403L, list(ok = FALSE, code = fetch_auth$code)))
  owner_ctx <- if (isTRUE(fetch_auth$present)) fetch_auth$owner_ctx else if (DB_READY) tryCatch(db_api_owner(APP_DIR, req), error = function(e)
    list(key = NA_character_, kind = "unidentified", external_id = NA_character_,
         error = conditionMessage(e))) else
    list(key = NA_character_, kind = "unidentified", external_id = NA_character_)

  if (identical(owner_ctx$kind, "agent_token_invalid"))
    return(send_json(con, 403, list(ok = FALSE, code = "agent_token_invalid",
      error = "助手令牌无效或已撤销")))

  if (p %in% c("/api/fetch-client/config", "/api/fetch-client/revoke", "/api/fetch-client/client", "/api/fetch-client/status")) {
    if (!fetch_client_web(owner_ctx)) return(send_json(con, 403, list(ok = FALSE, error = "请用已登录的应用页面接入设备")))
    expected_method <- if (p %in% c("/api/fetch-client/config", "/api/fetch-client/revoke")) "POST" else "GET"
    if (!identical(req$method, expected_method)) return(send_json(con, 405, list(ok = FALSE, error = "method denied")))
    if (identical(expected_method, "POST") && !as.character(req$headers[["origin"]] %||% "") %in%
        c("https://artifact.catsco.cc", "https://artifact.catsco.cn"))
      return(send_json(con, 403, list(ok = FALSE, error = "请从应用原页面操作")))
    out <- tryCatch({
      if (p == "/api/fetch-client/config") {
        # 子路径就是本机应用身份名，同样不写死：换机器换名字都不用改这里。
        .sub <- if (exists("pa_app_id", mode = "function")) pa_app_id() else NA_character_
        if (is.na(.sub) || !nzchar(.sub)) stop("本机没有配置应用身份名")
        base <- paste0(as.character(req$headers[["origin"]]), "/", .sub)
        fetch_client_issue(APP_DIR, owner_ctx, base, as.character(body_json()$device_id %||% "fetch-device"))
      } else if (p == "/api/fetch-client/revoke") fetch_client_revoke(APP_DIR, owner_ctx)
      else if (p == "/api/fetch-client/client") list(ok = TRUE, source = paste(readLines(file.path(APP_DIR, "tools", "pa_fetch_client.py"), warn = FALSE), collapse = "\n"))
      else {
        tokens <- Filter(function(x) identical(x$owner_key, owner_ctx$key) && is.null(x$revoked_at) &&
          is.numeric(x$expires_at) && x$expires_at > as.numeric(Sys.time()), fetch_client_read(APP_DIR)$tokens)
        list(ok = TRUE, devices = lapply(tokens, function(x) x[c("device_id", "expires_at")]))
      }
    }, error = function(e) list(ok = FALSE, status = 503L, error = "接入配置暂不可用，请稍后重试"))
    raw <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", digits = NA))))
    status <- if (isTRUE(out$ok)) 200L else out$status %||% 400L
    hdr <- paste0("HTTP/1.1 ", status, " Result\r\nContent-Type: application/json; charset=utf-8\r\nCache-Control: no-store\r\nPragma: no-cache\r\nReferrer-Policy: no-referrer\r\nContent-Length: ", length(raw), "\r\nConnection: close\r\n\r\n")
    writeBin(c(charToRaw(hdr), raw), con); flush(con); return(invisible(NULL))
  }

  # Every record-scoped route is classified here before its handler or any
  # side effect. This is the single gate that prevents a new route from silently
  # falling back to current/global state.
  record_preflight <- NULL
  send_json_base <- send_json
  send_json <- function(con, code, obj) {
    if (!is.null(record_preflight) && isTRUE(record_preflight$ok) &&
        is.list(obj) && is.list(record_preflight$access)) {
      cap <- record_preflight$access$cap
      if (is.null(obj$session_id) && !is.null(cap$session_id))
        obj$session_id <- cap$session_id
      if (is.null(obj$can_read)) obj$can_read <- isTRUE(cap$can_read)
      if (is.null(obj$can_write)) obj$can_write <- isTRUE(cap$can_write)
      if (is.null(obj$readonly)) obj$readonly <- isTRUE(cap$readonly)
    }
    send_json_base(con, code, obj)
  }
  record_preflight <- record_request_preflight(APP_DIR, DB_CON, p, req$method, q,
                                                body_json(), owner_ctx, cur_sid())
  if (!isTRUE(record_preflight$ok)) {
    return(send_json(con, record_preflight$status, record_preflight$body))
  }

  if (p %in% c("/api/run/logs", "/api/run/logs/download")) {
    if (!identical(req$method, "GET")) return(send_json(con, 405, list(ok = FALSE, error = "GET required")))
    if (!DB_READY) return(send_json(con, 503, list(ok = FALSE, error = "日志数据库不可用")))
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    rows <- db_run_log_read_record(DB_CON, access$cap$owner_key, sid)
    text <- db_run_log_text(rows)
    if (p == "/api/run/logs")
      return(send_json(con, 200, c(list(ok = TRUE, session_id = sid,
        read_at = db_now(), entries = rows, text = text),
        record_cap_fields(access$cap))))
    bytes <- charToRaw(enc2utf8(text))
    hdr <- paste0("HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\n",
      "Content-Disposition: attachment; filename=run-log.txt\r\nCache-Control: no-store\r\n",
      "Content-Length: ", length(bytes), "\r\nConnection: close\r\n\r\n")
    writeBin(c(charToRaw(hdr), bytes), con); flush(con)
    return(invisible(NULL))
  }

  if (p == "/api/app-handoff/config" && identical(req$method, "GET")) {
    cfg <- if (exists("pa_handoff_config", mode = "function")) pa_handoff_config() else NULL
    if (is.null(cfg)) return(send_json(con, 503, list(ok = FALSE, code = "handoff_not_configured")))
    return(send_json(con, 200, list(ok = TRUE, platform_origin = cfg$platform_origin, supports_run = FALSE)))
  }
  if (p == "/api/app-handoff/initialize" && identical(req$method, "POST")) {
    cfg <- if (exists("pa_handoff_config", mode = "function")) pa_handoff_config() else NULL
    if (is.null(cfg) || !exists("pah_validate", mode = "function"))
      return(send_json(con, 503, list(ok = FALSE, code = "handoff_not_configured")))
    if (!DB_READY || !agent_human_owner(owner_ctx))
      return(send_json(con, 403, list(ok = FALSE, code = "handoff_identity_required")))
    origin <- as.character(req$headers[["origin"]] %||% "")
    if (!identical(origin, cfg$app_origin))
      return(send_json(con, 403, list(ok = FALSE, code = "handoff_origin_invalid")))
    ident <- tryCatch(pa_platform_identity(req), error = function(e) NULL)
    out <- tryCatch(pah_validate(body_json(), ident, owner_ctx, cfg$agent_uid,
      floor(as.numeric(Sys.time()) * 1000)), error = function(e) list(ok = FALSE, code = "handoff_invalid"))
    if (!isTRUE(out$ok)) return(send_json(con, 403, list(ok = FALSE, code = out$code %||% "handoff_invalid")))
    if (!identical(out$action, "initialize"))
      return(send_json(con, 403, list(ok = FALSE, code = "handoff_action_unsupported")))
    if (!db_api_notebook_authorized(DB_CON, owner_ctx, out$session_id))
      return(send_json(con, 403, list(ok = FALSE, code = "handoff_record_forbidden")))
    if (!is.null(out$intent)) {
      source(file.path(ENGINE_DIR, "handoff_apply.R"), local = TRUE)
      applied <- pah_apply_initialize(APP_DIR, DB_CON, owner_ctx, body_json(), out)
      return(send_json(con, applied$status, applied$body))
    }
    return(send_json(con, 200, list(ok = TRUE, session_id = out$session_id,
      request_id = out$request_id, initialized = TRUE, supports_run = FALSE)))
  }

  # ---- 平台能力探针（A5）------------------------------------------
  #   为什么要有这个接口：页面以前写过「定时已生效」，
  #   依据只是「代码里有重发队列」—— 那是把「有逻辑」当成「平台允许」。
  #   判断错的代价很具体：人关掉页面，以为它还会推进，实际停住。
  #   现在只回三档之一，文案跟着档位走，未知不写成已知。
  if (p == "/api/envprobe" && identical(req$method, "GET")) {
    if (!exists("env_probe_read")) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/env_probe.R")))
    cur <- env_probe_read(APP_DIR)
    if (is.null(cur))
      return(send_json(con, 200, list(ok = TRUE, level = "unknown", probed = FALSE,
        message = env_probe_message("unknown"),
        why = "还没有探测过；不承诺自动推进")))
    return(send_json(con, 200, c(list(ok = TRUE, probed = TRUE), cur)))
  }

  if (p == "/api/envprobe" && identical(req$method, "POST")) {
    if (!exists("env_probe_write")) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/env_probe.R")))
    b <- body_json()
    # 只接受「可核事实」，不接受一个结论字符串。
    #   如果允许直接传 level=server，那这条探针就变成了「谁来都能自称可以」。
    obj <- tryCatch(env_probe_write(APP_DIR, list(
      has_sdk = b$has_sdk, handshake_ok = b$handshake_ok,
      can_schedule = b$can_schedule, evidence = b$evidence)),
      error = function(e) NULL)
    if (is.null(obj)) return(send_json(con, 400, list(ok = FALSE, error = "探针输入不合法")))
    return(send_json(con, 200, c(list(ok = TRUE), obj)))
  }

  # ---- 产物下载：真文件流 + Range（C1）----------------------------
  #   为什么单独在这里处理、不交给 db_api_handle：
  #     db_api_handle 只会回 JSON。而下载要的是二进制流，
  #     由它回一个 {"path": ...} 再让浏览器去读本地路径是做不到的
  #     （页面在浏览器里，读不到服务器文件系统）。
  #   三级取数：本地镜像命中 -> 直接流式返回；未命中 -> 入队并回 202；
  #     大文件必须带 confirm=1。
  if (p == "/api/artifact/download" && DB_READY) {
    rel <- as.character(qget(q, "rel") %||% "")
    if (!nzchar(rel)) return(send_json(con, 400, list(ok = FALSE, error = "缺 rel")))
    ok_owner <- !is.na(owner_ctx$key)
    if (!ok_owner)
      return(send_json(con, 403, list(ok = FALSE, code = "identity_unavailable",
        error = "身份未识别，不提供下载")))
    # 本地镜像可能落在两处：按使用者的（state/users/<owner>/mirror/<rel>），
    # 和按分析记录的（.../mirror/<session>/<rel>）。以前只查前者，
    # 而取回端写的是后者 —— 于是「明明取回了却还是 202」。
    # 这里两处都查，查到哪个用哪个。
    # 归属：显式给了 session 就只认这一份镜像，绝不拿 owner 目录里同名旧文件顶替。
    #   同名文件在不同记录里可能是两次不同的运行，混用等于把别人的结果当本次。
    access <- record_preflight$access
    sid_dl <- as.character(record_preflight$target$sid %||% "")
    fp <- ""
    if (isTRUE(HAS_MIRROR)) {
      p1 <- mirror_path(sid_dl, rel)
      if (nzchar(p1) && file.exists(p1)) fp <- p1
    }
    if (!nzchar(fp)) {
      if (!isTRUE(access$cap$can_write))
        return(send_json(con, 403, c(list(ok = FALSE, code = "fetch_requires_owner",
          session_id = sid_dl,
          error = "shared 记录只能读取已经取回的产物；创建取回任务需要记录所有者。"),
          record_cap_fields(access$cap))))
      # 没取回：登记取回任务，让页面显示进度，而不是直接报「找不到」。
      req_id <- db_new_id("fx")
      DBI::dbExecute(DB_CON,
        "INSERT INTO fetch_jobs(request_id,owner_key,version_id,rel,state,created_at,updated_at)
         VALUES(?,?,?,?,?,?,?)",
        list(req_id, access$cap$owner_key, as.character(access$cap$version_id %||% NA),
             rel, "pending", db_now(), db_now()))
      DBI::dbExecute(DB_CON,
        "INSERT INTO poll_jobs(job_id,kind,owner_key,version_id,ticket_id,state,
           next_check_at,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?)",
        list(paste0("job-", req_id), "fetch", access$cap$owner_key,
             as.character(access$cap$version_id %||% NA), req_id,
             "pending", db_now(), db_now(), db_now()))
      # 同时登记进「待取清单」：执行端（工作电脑上的取回器）每轮读的就是这份，
      #   只写 fetch_jobs 的话，取回器那一侧看不到这个请求，文件永远搬不回来。
      if (isTRUE(HAS_MIRROR) && nzchar(sid_dl))
        tryCatch(mirror_request(sid_dl, rel,
                                bytes = artifact_expected_bytes(sid_dl, rel),
                                run_dir = as.character(access$cap$run_dir %||% ""),
                                want_md5 = artifact_expected_md5(sid_dl, rel)),
                 error = function(e) NULL)
      return(send_json(con, 202, list(
        ok = TRUE, ready = FALSE, request_id = req_id, rel = rel,
        error = "还没取回到本地，已登记取回任务",
        hint = "轮询 /api/artifact/fetch/status?request_id=... 看进度；就绪后再点下载。")))
    }
    # 最终要流的那个文件必须自己过硬：前面选路径用的是 mirror_path / owner 回退，
    #   但真正发出去的是 fp。只核路径不核内容，等于核了 A 发 B。
    #   基准取该 session 最新一条 verified 记录的 md5；拿不到基准就不当已验收，
    #   尤其不能让镜像目录里的旧文件冒充。
    expA <- artifact_expected_md5(sid_dl, rel)
    actA <- if (isTRUE(HAS_MIRROR)) mirror_actual_md5(fp) else ""
    in_mirror <- isTRUE(HAS_MIRROR) && nzchar(fp) &&
      startsWith(normalizePath(fp, mustWork = FALSE),
                 normalizePath(mirror_dir(sid_dl), mustWork = FALSE))
    if (nzchar(expA)) {
      if (!identical(actA, expA)) {
        # 这份不是最新验收版本：镜像里的挪去 _stale 留审计，然后拒绝，不提供旧文件。
        if (isTRUE(in_mirror)) {
          mv <- tryCatch(mirror_quarantine(sid_dl, rel), error = function(e) "")
          if (nzchar(mv)) message("mirror_stale_quarantined ", rel, " -> ", mv)
        }
        return(send_json(con, 409, list(
          ok = FALSE, code = "mirror_stale",
          error = "本地这份不是最新已验收版本，请重新取回后再下载",
          expected_md5 = expA, cached_md5 = actA)))
      }
    } else {
      # 查不到最新基准就一律拒绝，不管这份来自镜像还是 owner 回退。
      #   只拦镜像时，owner 目录里那份同名旧文件仍会流出去 ——
      #   而这正是"看起来能下、下到的是旧结果"的那条路。
      #   要能下载，就必须先有一条该 session、该 rel 的 verified 记录给出 md5。
      return(send_json(con, 409, list(
        ok = FALSE, code = "mirror_unverified",
        error = "查不到这条产物最新已验收版本的 md5，无法确认是否最新，暂不下载",
        rel = rel, cached_md5 = actA)))
    }
    info <- file.info(fp)
    if (isTRUE(info$isdir)) return(send_json(con, 400, list(ok = FALSE, error = "这是一个目录")))
    fname <- enc2utf8(basename(fp))
    ext <- tools::file_ext(fname)
    ascii_fallback <- paste0("result", if (nzchar(ext)) paste0(".", ext) else ".bin")
    # Range：大文件断线要能续传，不能每次从 0 开始。
    rng <- as.character(req$headers[["range"]] %||% "")
    start <- 0L; end <- info$size - 1L; partial <- FALSE
    if (nzchar(rng) && grepl("^bytes=", rng)) {
      m <- regmatches(rng, regexec("bytes=([0-9]+)-([0-9]*)", rng))[[1]]
      if (length(m) >= 2L) {
        start <- as.integer(m[2])
        if (length(m) >= 3L && nzchar(m[3])) end <- as.integer(m[3])
        partial <- TRUE
      }
    }
    len <- end - start + 1L
    hdr <- paste0(if (partial) "HTTP/1.1 206 Partial Content\r\n" else "HTTP/1.1 200 OK\r\n",
                  "Content-Type: application/octet-stream\r\n",
                  "Content-Length: ", len, "\r\n",
                  if (partial) paste0("Content-Range: bytes ", start, "-", end, "/",
                                      info$size, "\r\n") else "",
                  "Accept-Ranges: bytes\r\n",
                  "Content-Disposition: attachment; filename=\"", ascii_fallback,
                  "\"; filename*=UTF-8''", utils::URLencode(fname, reserved = TRUE), "\r\n",
                  "Cache-Control: no-store\r\nConnection: close\r\n\r\n")
    writeBin(charToRaw(enc2utf8(hdr)), con)
    fc <- file(fp, open = "rb")
    if (start > 0L) seek(fc, where = start, origin = "start")
    remain <- len
    while (remain > 0L) {
      chunk <- readBin(fc, "raw", min(262144L, remain))
      if (!length(chunk)) break
      writeBin(chunk, con)
      remain <- remain - length(chunk)
    }
    close(fc); flush(con)
    return(invisible(NULL))
  }

  # 数据层路由统一在这里收口。返回 NULL 表示不归它管，继续往下走。
  api_res <- tryCatch(
    db_api_handle(DB_CON, APP_DIR, p, req, q, body_json, send_json, owner_ctx,
                  record_preflight),
    error = function(e) list(code = 500, body = list(ok = FALSE,
      code = "db_api_error", error = conditionMessage(e))))
  if (!is.null(api_res)) return(send_json(con, api_res$code, api_res$body))


  # ---- 物种/组织来源审计 --------------------------------------------
  #   为什么单独一条路由、不放进 db_api.R（2026-11-04）
  #     这条接口在 db_api.R 里加过一版，注册了却一直回 404：函数体里
  #     确实有那个分支，直接调用也不报错，但从 HTTP 进来就是到不了。
  #     与其继续猜它为什么不生效，不如把这条搬到 serve.R 的直连层 ——
  #     这一层每个 /api/xxx 都是直接的 if 判断，有没有生效一眼能看出来。
  #     这不是"绕过问题"，是把它放在更容易被看见、也更容易被验证的位置。
  #
  #   它回答什么：每条分析的物种/组织是什么、来源是哪种、有没有可疑。
  #   为什么要这个接口：一次误判改掉一个值只解决那一条。同类问题要能被
  #     发现，就得有一条"随时把所有分析的来源状态摊开"的通道。
  if (p == "/api/dataset/audit") {
    placeA <- tryCatch(place_read(APP_DIR), error = function(e) NULL)
    corpusA <- if (exists("dp_path_corpus", mode = "function"))
      dp_path_corpus(placeA) else ""
    ssA <- tryCatch({
      stA <- sess_read(APP_DIR)
      stA$sessions %||% list()
    }, error = function(e) list())
    rowsA <- Filter(Negate(is.null), lapply(ssA, function(s) {
      if (!is.list(s)) return(NULL)
      spA <- tryCatch(
        if (exists("dp_species", mode = "function"))
          dp_species(NULL, sess = s, corpus = corpusA) else list(),
        error = function(e) list())
      list(
        session_id = as.character(s$id %||% ""),
        name = as.character(s$name %||% ""),
        species = as.character(spA$value %||% NA_character_),
        tissue = as.character(spA$tissue %||% NA_character_),
        source = as.character(spA$source %||% "unknown"),
        # 两个字段分开给，别合并：
        #   suspect_from_path = "这个值可疑"（没确认过 + 路径里有）
        #   matches_path      = "路径里确实有这个词"（纯客观，确认过也可能为真）
        # 合成一个字段会误导：一条已确认的值被标成"可疑"，
        # 看的人以为它有问题，而它恰恰是唯一核对过的那条。
        suspect_from_path = isTRUE(spA$suspect_from_path),
        matches_path = isTRUE(spA$matches_path),
        why = as.character(spA$why %||% ""))
    }))
    srcA <- vapply(rowsA, function(r) r$source, "")
    suspA <- Filter(function(r) isTRUE(r$suspect_from_path) &&
                      !identical(r$source, "confirmed"), rowsA)
    return(send_json(con, 200, list(
      ok = TRUE,
      scanned = length(rowsA),
      by_source = list(
        confirmed = sum(srcA == "confirmed"),
        reference = sum(srcA == "reference"),
        unverified = sum(srcA == "unverified"),
        unknown = sum(srcA == "unknown")),
      need_review = suspA,
      items = rowsA,
      path_hints = list(
        tissue = if (exists("dp_tissue_candidates", mode = "function"))
          dp_tissue_candidates(corpusA, "tissue") else list(),
        species = if (exists("dp_tissue_candidates", mode = "function"))
          dp_tissue_candidates(corpusA, "species") else list()),
      rule = paste0("物种与组织只认人确认或表自身声明；路径名与文件名不是证据。",
                    "unverified 且值只出现在路径里的条目，会被列进 need_review。"))))
  }

  if (p == "/api/ping")
    return(send_json(con, 200, c(list(ok = TRUE, platform = .Platform$OS.type, port = PORT,
                                      build = app_build()),
                                 server_worker_status())))

  # ---- 管理员与两个隔离开关（app.admin.v1）--------------------------
  #   GET  看当前开关 + 我是不是管理员
  #   POST 改开关（只有管理员）
  # 为什么管理员是「机器人主人」而不是「第一个进来的人」：
  #   第一个进来的人可能只是打开了一次网址。这里只认部署时写在
  #   config/admin_policy.json（或 PA_ADMIN_VIEWER_UIDS / PA_ADMIN_USERNAMES）
  #   里的平台身份；配置为空时谁都不是管理员，开关维持默认（两个都开）。
  if (p == "/api/admin/policy") {
    if (!isTRUE(HAS_ADMIN_POLICY))
      return(send_json(con, 503, list(ok = FALSE, code = "admin_policy_missing",
        error = "缺少 engine/admin_policy.R")))
    a <- admin_identity(APP_DIR, owner_ctx)
    pol_now <- record_policy(APP_DIR)
    if (identical(req$method, "GET")) {
      return(send_json(con, 200, list(
        ok = TRUE, identified = nzchar(a$owner_key),
        is_admin = isTRUE(a$is_admin), admin_reason = a$reason,
        me = list(owner_key = if (nzchar(a$owner_key)) a$owner_key else NULL,
                  uid = if (nzchar(a$uid)) a$uid else NULL,
                  username = if (nzchar(a$username)) a$username else NULL),
        policy = record_policy_public(pol_now),
        hint = if (isTRUE(a$is_admin)) ""
               else "只有应用所属机器人的主人可以改这两个开关。")))
    }
    if (!identical(req$method, "POST"))
      return(send_json(con, 405, list(ok = FALSE, code = "method_not_allowed")))
    if (!isTRUE(a$is_admin)) {
      tryCatch(admin_audit(APP_DIR, list(action = "policy_denied",
        actor = a$owner_key %||% "", uid = a$uid %||% "", username = a$username %||% "",
        at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))), error = function(e) NULL)
      return(send_json(con, 403, list(ok = FALSE, code = "admin_required",
        is_admin = FALSE, policy = record_policy_public(pol_now),
        error = "只有应用所属机器人的主人可以改这两个开关。")))
    }
    b <- body_json()
    # 只认 TRUE / FALSE。写成 "false" 字符串或 0 一律拒绝，
    #   因为「以为关掉了其实没关」比报错危险得多。
    pick <- function(k, cur) {
      v <- b[[k]]
      if (is.null(v)) return(list(ok = TRUE, value = cur))
      if (is.logical(v) && length(v) == 1L && !is.na(v)) return(list(ok = TRUE, value = isTRUE(v)))
      list(ok = FALSE)
    }
    r1 <- pick("record_isolation", pol_now$record_isolation)
    r2 <- pick("write_isolation", pol_now$write_isolation)
    if (!isTRUE(r1$ok) || !isTRUE(r2$ok))
      return(send_json(con, 400, list(ok = FALSE, code = "policy_value_invalid",
        error = "两个开关只接受 true / false；未做任何修改。")))
    saved <- admin_policy_write(APP_DIR, r1$value, r2$value,
                               actor = a$owner_key, uid = a$uid, username = a$username)
    if (!isTRUE(saved$ok))
      return(send_json(con, 500, list(ok = FALSE, code = "policy_write_failed",
        error = saved$error %||% "策略写入失败")))
    return(send_json(con, 200, list(
      ok = TRUE, is_admin = TRUE,
      policy = record_policy_public(saved$policy),
      note = "已保存。刷新页面即生效；已在页面里打开的人下次请求就会按新开关判定。")))
  }

  # ---- 改应用代码的统一门 -------------------------------------------
  #   哪些算"改应用代码"：脚本库里的脚本被替换/新增/还原/采纳，
  #   以及把"帮我改脚本"这种请求交给助手。
  #   注意这条只管页面这条路。助手自己在本机改文件走的是文件系统，
  #   不经过这里 —— 那一侧靠的是 config/admin_policy.json 这个同一份事实，
  #   助手按它判断要不要动手。两处读同一份，才不会出现"页面说不行、助手照改"。
  code_write_denied <- function() {
    if (!isTRUE(HAS_ADMIN_POLICY)) return(NULL)
    g <- admin_code_write_gate(APP_DIR, owner_ctx)
    if (isTRUE(g$ok)) return(NULL)
    send_json(con, 403, list(ok = FALSE, code = g$code,
      is_admin = isTRUE(g$admin), error = g$error %||% "只有管理员可以修改应用代码。"))
  }

  # ---- 助手写入授权（app.agent.v1）----------------------------------
  #   为什么要有这块：
  #     助手在本机写记录时没有平台 cookie，只能被兜底成 local:default；
  #     人在页面上是 viewer:xxx。两套 identity 各看各的，于是"助手写进去了"
  #     和"我看得见"是两件事。授权一次，把助手绑到人的 owner_key 上。
  #   为什么发牌必须是 POST 且必须已确认身份：
  #     这条路一旦对未识别身份开放，任何本机进程都能给自己发一张牌，
  #     等于把全部记录开放出去。发牌是唯一需要人身份的动作。
  if (p == "/api/agent/token" && identical(req$method, "POST")) {
    if (!HAS_AGENT_GATE)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/agent_gate.R")))
    if (!agent_human_owner(owner_ctx))
      return(send_json(con, 403, list(ok = FALSE, code = "human_identity_required",
        error = "只有已认证的用户可以签发助手令牌")))
    b <- body_json()
    out <- agent_token_issue(APP_DIR, owner_ctx$key %||% NA_character_,
                             note = as.character(b$note %||% ""),
                             ttl_days = b$ttl_days %||% 30L)
    code <- if (isTRUE(out$ok)) 200 else 403
    return(send_json(con, code, out))
  }

  if (p == "/api/agent/token" && identical(req$method, "GET")) {
    if (!HAS_AGENT_GATE)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/agent_gate.R")))
    okv <- !is.na(owner_ctx$key)
    if (!okv) return(send_json(con, 403, list(ok = FALSE, code = "identity_unavailable",
      error = "身份未识别，无法列出令牌")))
    return(send_json(con, 200, list(ok = TRUE, tokens = agent_token_list(APP_DIR, owner_ctx$key))))
  }

  if (p == "/api/agent/token/revoke" && identical(req$method, "POST")) {
    if (!HAS_AGENT_GATE)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/agent_gate.R")))
    b <- body_json()
    # 撤销也要求人身份：拿令牌来撤销自己等于"被偷了还能自己续命"。
    if (!agent_human_owner(owner_ctx))
      return(send_json(con, 403, list(ok = FALSE, code = "identity_unavailable",
        error = "只有已确认身份的用户可以撤销授权")))
    out <- agent_token_revoke(APP_DIR, as.character(b$id %||% ""), owner_ctx$key)
    return(send_json(con, if (isTRUE(out$ok)) 200 else 400, out))
  }

  # ---- 我在哪个会话里 ------------------------------------------------
  #   为什么需要这条：
  #     应用是挂在公网子路径下的一个页面，它自己看不出"现在是谁、在哪个会话
  #     打开的我"。而注入指令、回写结果都要落到对的会话里 —— 不知道会话号，
  #     就只能由助手去猜，猜错了消息就落到别的会话，用户那边看着就是"点了没反应"。
  #   做法：把浏览器带来的凭据原样转给网关的身份端点，由网关回答。
  #     应用不解析票据、也不自己判身份，只做搬运和透传。
  #     `saw_cookie` / `saw_auth` 是给排查用的：能分清"没登录"和"凭据没传过来"。
  if (p == "/api/whoami") {
    hdrs <- list()
    ck <- req$headers[["cookie"]]
    au <- req$headers[["authorization"]]
    if (!is.null(ck) && nzchar(as.character(ck))) hdrs$cookie <- as.character(ck)
    if (!is.null(au) && nzchar(as.character(au))) hdrs$authorization <- as.character(au)
    ident <- tryCatch({
      if (!requireNamespace("curl", quietly = TRUE)) stop("没有 curl 包")
      h <- curl::new_handle(timeout = 6, connecttimeout = 4)
      if (length(hdrs)) curl::handle_setheaders(h, .list = hdrs)
      # 身份名从本机配置读，代码里不留名字。见 engine/app_identity.R。
      me_url <- if (exists("pa_gateway_me_url", mode = "function")) pa_gateway_me_url() else NA_character_
      if (is.na(me_url) || !nzchar(me_url)) stop("本机没有配置应用身份名")
      r <- curl::curl_fetch_memory(me_url, handle = h)
      if (r$status_code != 200L) stop(paste0("网关返回 ", r$status_code))
      jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
    }, error = function(e) list(error = conditionMessage(e)))
    return(send_json(con, 200, list(
      ok = is.null(ident$error),
      identity = ident,
      topic_id = ident$topic_id %||% NULL,
      viewer = ident$viewer %||% NULL,
      saw_cookie = !is.null(hdrs$cookie),
      saw_auth = !is.null(hdrs$authorization))))
  }

  # ---- 位置：本机 / 服务器 ------------------------------------------
  #   GET  /api/place    看当前选择（口令永不回传）
  #   POST /api/place    只保存，不做别的（改一半走开也不会写脏）
  #   POST /api/place/commit
  #        「保存选择」真正的动作：落盘 -> 建配置 -> 跑环境体检与脚本识别
  #   为什么保存和提交分成两个接口：
  #        保存是「记住我填的」，提交是「按这个开始干活」。
  #        合成一个的话，识别一旦失败，用户填的东西也一起回滚，
  #        得重填一遍 —— 那是纯粹白费力气。
  # ===================================================================
  #  多份分析记录：新建 / 历史 / 存档与步骤回滚
  #  -------------------------------------------------------------------
  #  为什么要这一组接口：
  #    在此之前「我看的是哪一次分析」只活在浏览器内存和 state.json 里，
  #    刷新一次就没了 —— 用户的原话是「刷新一下所有记录就都没有了」。
  #    记录必须落盘成一份一份的档案，页面打开的是其中一份，刷新不动它。
  #  口令不在这里：它仍然只在 config/server_profile.json，本机。
  #    记录里存的是「这次跑在哪台机器、哪个目录」，不是用来登录的凭据。
  # ===================================================================
  if (p == "/api/sessions" && identical(req$method, "GET")) {
    if (!HAS_SESSIONS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    if (!DB_READY) return(send_json(con, 503, list(ok = FALSE, code = "database_unavailable",
      error = "记录数据库不可用，不会返回未归属的记录列表")))
    lst <- sess_list(APP_DIR)
    # ---- 按使用者过滤（B1）----------------------------------------
    #   以前这里直接回全局 sessions.json：谁进来都看到同一批记录。
    #   身份已经能拿到了（/api/whoami），但识别结果没有任何地方使用。
    #   现在的规则：
    #     · 身份识别成功              -> 只回自己的 + 别人显式共享的
    #     · 身份识别失败（fail-closed） -> 一条都不回，并说明原因
    #     · 迁移来的历史记录           -> 归 legacy，标「历史（未归属）」，不混进任何人的列表
    if (DB_READY) {
      okv <- !is.na(owner_ctx$key)
      if (!okv) {
        return(send_json(con, 200, list(
          ok = TRUE, sessions = list(), current = NULL, identified = FALSE,
          code = "identity_unavailable",
          error = "身份未识别，不会显示任何已有记录",
          hint = "点「重试识别」，或从平台会话里重新打开这个应用")))
      }
      vis <- record_visibility_query(APP_DIR, owner_ctx)
      rows <- tryCatch(db_rows(DB_CON, vis$sql, vis$params),
        error = function(e) NULL)
      if (!is.null(rows)) {
        keep <- as.character(rows$session_id)
        lst$sessions <- Filter(function(s) as.character(s$id) %in% keep, lst$sessions %||% list())
        own <- stats::setNames(as.character(rows$owner_key), as.character(rows$session_id))
        pol_now <- record_policy(APP_DIR)
        write_open <- !isTRUE(pol_now$write_isolation)
        lst$sessions <- lapply(lst$sessions, function(s) {
          id <- as.character(s$id %||% "")
          is_owner <- identical(unname(own[[id]] %||% ""), as.character(owner_ctx$key))
          can_write <- is_owner || write_open
          c(s, list(can_read = TRUE, can_write = can_write, readonly = !can_write))
        })
        # 只读标记：页面据此把跑/改/删按钮置灰。
        #   隔离开着时 = 别人的记录；关着时谁都能改，所以列表为空。
        lst$readonly <- if (write_open) character(0)
          else as.character(rows$session_id[rows$owner_key != owner_ctx$key])
        lst$identified <- TRUE
        lst$owner_key <- owner_ctx$key
        lst$policy <- record_policy_public(pol_now)
      }
    }
    return(send_json(con, 200, lst))
  }

  if (p == "/api/sessions/state" && identical(req$method, "GET")) {
    if (!HAS_SESSIONS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    target <- record_session_target(q, list(id = q$id %||% ""), path = p,
                                    current_sid = cur_sid(), allow_current = TRUE,
                                    require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    sid <- target$sid
    access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "read")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    out <- sess_state(APP_DIR, sid)
    if (is.null(out$session))
      return(send_json(con, 404, c(list(ok = FALSE, code = "session_record_missing",
        error = "没有这条分析记录。"), record_cap_fields(access$cap))))
    # 页面还需要「当前这台机器/路径」的公开视图（不含口令）
    out$place <- record_public_place(access$cap$place %||% list())
    visible <- tryCatch({
      vis <- record_visibility_query(APP_DIR, owner_ctx)
      db_rows(DB_CON, sub("SELECT session_id, owner_key, visibility, name, updated_at",
                          "SELECT session_id, owner_key, visibility", vis$sql, fixed = TRUE),
              vis$params)
    }, error = function(e) NULL)
    if (!is.null(visible)) {
      keep <- as.character(visible$session_id)
      out$sessions <- Filter(function(s) as.character(s$id) %in% keep, out$sessions %||% list())
      own <- stats::setNames(as.character(visible$owner_key), as.character(visible$session_id))
      pol_now <- record_policy(APP_DIR)
      write_open <- !isTRUE(pol_now$write_isolation)
      out$sessions <- lapply(out$sessions, function(s) {
        id <- as.character(s$id %||% "")
        is_owner <- identical(unname(own[[id]] %||% ""), as.character(owner_ctx$key))
        can_write <- is_owner || write_open
        c(s, list(can_read = TRUE, can_write = can_write, readonly = !can_write))
      })
      out$policy <- record_policy_public(pol_now)
    }
    # 这次有没有在等助手写回：记录自带，不用全局标记。
    #   两个槽位分开报（2026-11-12）：识别在等哪个号、步骤在等哪个号互不覆盖。
    #   以前只有一个 pending_req，步骤下发会把它顶掉，识别结论被误判成旧数据。
    .sid0 <- sid
    out$pending_identify_req <- tryCatch(sess_get_pending(APP_DIR, .sid0, slot = "identify"),
                                         error = function(e) NULL)
    out$pending_step_req <- tryCatch(sess_get_pending(APP_DIR, .sid0, slot = "step"),
                                     error = function(e) NULL)
    # ---- 识别到底处在哪一步：由服务端一次说清 ------------------------
    #   为什么必须由服务端算（2026-11-22，gina 报「会话 1 已回写，会话 2 仍显示识别中」）：
    #     原来这个判断只活在前端内存里，判据是「记录里的识别号 == 页面手上的请求号」。
    #     而页面手上的号只属于点过按钮的那一个标签页 ——
    #     换一个会话打开同一条记录，手上没有号，同一个问题就答成两种结论。
    #     更糟的一种：识别请求发出去、回写却带着别的号回来（或干脆没回来），
    #     那条 pending 就永远留着，于是这条记录在任何页面都显示"识别中"，
    #     而记录里其实早就有一份成功的识别结论（实测 952j：结论 10-26，pending 停在 11-12）。
    #   判据集中在 identify_state：none / waiting / ready / stale 四种，只看记录本身。
    ident_st <- tryCatch(identify_state(APP_DIR, .sid0), error = function(e) NULL)
    if (is.null(ident_st))
      ident_st <- list(status = if (!is.null((out$session %||% list())$identify$result))
        "ready" else "none")
    out$identify_status <- as.character(ident_st$status %||% "none")
    out$identify_result_req <- ident_st$result_req
    out$identify_result_at <- ident_st$result_at
    out$pending_identify_at <- ident_st$pending_at
    out$identify_stale_hours <- ident_st$stale_hours
    # 旧字段保留指向识别槽：老页面读它判"还在等识别"，语义不变。
    out$pending_req <- out$pending_identify_req
    out$current <- sess_current_id(APP_DIR)
    out$viewing_session_id <- sid
    out$record_source <- target$record_source
    out <- c(out, record_cap_fields(access$cap))
    return(send_json(con, 200, out))
  }

  # ---- 并行记录摘要（app.runs.v1）----------------------------------
  #   为什么单开一条：页面现在只能看 current 那一条。多条同时在跑时，
  #   人要一眼看出「哪条在跑、哪条断了、哪条还没开始」。
  #   判据只用已经发生的事，见 engine/runs.R 抬头。
  # ---- 可复现 ipynb（app.notebook.v1）------------------------------
  #   三件事：助手推流水、应用汇编、人下载。
  #   为什么推流水而不是推成品 notebook：
  #     成品由应用统一汇编，格式只在一处定义；助手只负责说清"这一步跑了什么"。
  #     两边各写一份格式，迟早对不上。
  if (p == "/api/notebook/cells" && identical(req$method, "POST")) {
    if (!HAS_NOTEBOOK)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/notebook_build.R")))
    if (!identical(as.character(owner_ctx$kind %||% ""), "agent"))
      return(send_json(con, 403, list(ok = FALSE, code = "agent_token_required",
        error = "推流水只接受助手令牌；人不需要走这条路")))
    b <- body_json()
    sid_nb <- trimws(as.character(record_preflight$target$sid %||% ""))
    access <- record_preflight$access
    cell <- b$cell
    if (!is.list(cell))
      return(send_json(con, 400, list(ok = FALSE, error = "cell 必须是对象")))
    store <- nb_store_dir(APP_DIR, sid_nb)
    ok <- tryCatch(nb_append_cell(store, cell, event_id = b$event_id), error = function(e) FALSE)
    if (!isTRUE(ok)) return(send_json(con, 400, list(ok = FALSE, error = "流水写入失败")))
    if (isTRUE(b$final) || isTRUE(b$build)) {
      result <- tryCatch(nb_build_export(store), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
      result$session_id <- sid_nb
      return(send_json(con, if (isTRUE(result$ok)) 200 else 400,
                       c(result, record_cap_fields(access$cap))))
    }
    return(send_json(con, 200, c(list(ok = TRUE, session_id = sid_nb,
      cells_file = nb_cells_file(store)), record_cap_fields(access$cap))))
  }

  if (p %in% c("/api/notebook/replay/build", "/api/notebook/replay/download")) {
    building <- identical(p, "/api/notebook/replay/build")
    if (!identical(req$method, if (building) "POST" else "GET"))
      return(send_json(con, 405, list(ok = FALSE, code = "method_not_allowed")))
    b <- if (building) body_json() else list()
    sid_replay <- as.character(record_preflight$target$sid %||% "")
    access <- record_preflight$access
    session_replay <- tryCatch(sess_find(APP_DIR, sid_replay), error = function(e) NULL)
    if (is.null(session_replay)) return(send_json(con, 409, list(ok = FALSE, code = "session_record_missing")))
    if (!exists("nr_api_build", mode = "function") || !exists("nr_api_download", mode = "function"))
      return(send_json(con, 503, list(ok = FALSE, code = "replay_unavailable")))
    result <- tryCatch({
      if (building) nr_api_build(APP_DIR, session_replay, b$manifest)
      else nr_api_download(APP_DIR, session_replay)
    }, error = function(e) NULL)
    if (is.null(result) || !isTRUE(result$ok))
      return(send_json(con, 409, list(ok = FALSE, code = "replay_binding_or_manifest_invalid",
        error = "重放清单、记录绑定或本次产物校验未通过，未提供旧文件。")))
    if (building) return(send_json(con, 200, list(ok = TRUE, session_id = sid_replay,
      can_read = isTRUE(access$cap$can_read), can_write = isTRUE(access$cap$can_write),
      readonly = isTRUE(access$cap$readonly),
      full_analysis_replay_verified = FALSE, note = "已打包显式授权清单；尚非完整科学分析复现认证。")))
    bytes <- result$bytes
    if (!is.raw(bytes) || !length(bytes))
      return(send_json(con, 409, list(ok = FALSE, code = "replay_unavailable")))
    hdr <- paste0("HTTP/1.1 200 OK\r\nContent-Type: application/x-ipynb+json\r\n",
      "Content-Disposition: attachment; filename=declared-analysis-replay.ipynb\r\n",
      "Cache-Control: no-store\r\nContent-Length: ", length(bytes), "\r\nConnection: close\r\n\r\n")
    writeBin(c(charToRaw(hdr), bytes), con); flush(con)
    return(invisible(NULL))
  }

  if (p == "/api/notebook/build" && identical(req$method, "POST")) {
    if (!HAS_NOTEBOOK)
      return(send_json(con, 400, list(ok = FALSE, code = "notebook_module_missing",
        error = "缺少 engine/notebook_build.R")))
    # ---- 整段包一层，永远不带裸 500 ------------------------------------
    #   gina 2026-11-19 报「生成 notebook 请求未完成（HTTP 500）」。
    #   裸 500 的 body 里没有 code，页面只能显示一句 HTTP 状态码 ——
    #   那句话既不能定位，也看着像服务挂了，实际只是某一步 throw 了。
    #   现在无论卡在哪一步，都回 400 + 具体 code + 中文原因，页面能直接显示。
    nb_res <- tryCatch({
      b <- body_json()
      sid_nb <- trimws(as.character(record_preflight$target$sid %||% ""))
      access <- record_preflight$access
      if (!nzchar(sid_nb))
        return(list(ok = FALSE, code = "session_missing", status = 400L,
                    error = "没有指定分析记录，导不出 notebook。"))
      store <- nb_store_dir(APP_DIR, sid_nb)
      session_nb <- tryCatch(sess_find(APP_DIR, sid_nb), error = function(e) NULL)
      # 先把这条记录自己的流水同步过来（仅 local 模式会成功，server 模式返回 FALSE 不算错）。
      synced <- tryCatch({ if (!is.null(session_nb)) nb_sync_local(APP_DIR, session_nb); TRUE },
                         error = function(e) e)
      if (inherits(synced, "error"))
        return(list(ok = FALSE, code = "ledger_sync_failed", status = 409L,
                    error = paste0("同步执行流水失败：", conditionMessage(synced))))
      out <- tryCatch(nb_build_export(store, session_nb$name %||% NULL),
                      error = function(e) list(ok = FALSE, code = "build_error",
                                               error = conditionMessage(e)))
      # ---- 没有流水时不再直接失败 --------------------------------------
      #   这里只认 state/notebooks/<sid>/status/cells.jsonl，而那份流水只有
      #   助手推、且只对 local 模式同步（nb_sync_local 对 server 模式直接返回
      #   FALSE）。于是一条 server 模式的记录点「生成执行快照」或「生成
      #   notebook」永远是 no_cells_file —— 按钮看着能用，实际是死路。
      #   现在：流水在就按流水汇编（证据最硬）；流水不在，就按这条记录实际
      #   登记过的脚本汇编，并把这个来源如实写进 notebook 元数据，不冒充
      #   "有执行流水"。
      source_used <- "ledger"
      if (!isTRUE(out$ok) && identical(as.character(out$code %||% ""), "no_cells_file")) {
        fb <- if (isTRUE(HAS_NB_RECORD))
          tryCatch(nfr_cells_for_session(APP_DIR, ENGINE_DIR, DB_CON, sid_nb),
                   error = function(e) list(ok = FALSE, code = "fallback_error",
                                            error = conditionMessage(e)))
        else list(ok = FALSE, code = "notebook_module_missing",
                  error = "缺少 engine/notebook_from_record.R")
        if (!isTRUE(fb$ok)) {
          out <- list(ok = FALSE, code = as.character(fb$code %||% "no_cells_file"),
                      error = paste0("这条记录既没有执行流水，脚本库里也没有登记过它的脚本，",
                                     "因此导不出 notebook。",
                                     if (nzchar(as.character(fb$error %||% "")))
                                       paste0("（", as.character(fb$error), "）") else "",
                                     "先跑完一个环节（验收通过）之后再试。"))
        } else {
          w <- tryCatch({
            nb_with_lock(store, function() {
              dst <- nb_cells_file(store)
              dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
              lines <- vapply(fb$cells, function(x)
                as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)), "")
              tmp <- tempfile(".fromrec-", tmpdir = dirname(dst))
              writeLines(lines, tmp, useBytes = TRUE)
              if (!file.rename(tmp, dst)) stop("无法写入流水文件")
              TRUE
            })
          }, error = function(e) e)
          if (inherits(w, "error")) {
            out <- list(ok = FALSE, code = "ledger_write_failed",
                        error = paste0("写入流水失败：", conditionMessage(w)))
          } else {
            out <- tryCatch(nb_build_export(store, session_nb$name %||% NULL),
                            error = function(e) list(ok = FALSE, code = "build_error",
                                                     error = conditionMessage(e)))
            source_used <- "record_scripts"
          }
        }
      }
      out$session_id <- sid_nb
      if (isTRUE(out$ok)) out$source <- source_used
      out
    }, error = function(e) list(ok = FALSE, code = "notebook_build_exception",
                                 error = paste0("生成 notebook 时出错：", conditionMessage(e))))

    # 兜底：任何落下来的结果都必须带可读原因，不能只有状态码。
    if (!isTRUE(nb_res$ok)) {
      if (!nzchar(as.character(nb_res$code %||% "")))
        nb_res$code <- "notebook_build_failed"
      if (!nzchar(as.character(nb_res$error %||% "")))
        nb_res$error <- "没有生成 notebook，原因未记录。"
      nb_res$ok <- FALSE
    }
    capf <- tryCatch(record_cap_fields(record_preflight$access$cap), error = function(e) list())
    return(send_json(con, if (isTRUE(nb_res$ok)) 200L else 400L, c(nb_res, capf)))
  }

  if (p == "/api/notebook/download" && identical(req$method, "GET")) {
    sid_nb <- trimws(as.character(record_preflight$target$sid %||% ""))
    access <- record_preflight$access
    session_nb <- tryCatch(sess_find(APP_DIR, sid_nb), error = function(e) NULL)
    synced <- tryCatch({ if (!is.null(session_nb)) nb_sync_local(APP_DIR, session_nb); TRUE },
      error = function(e) e)
    if (inherits(synced, "error")) return(send_json(con, 409, list(ok = FALSE, error = conditionMessage(synced))))
    f <- file.path(nb_store_dir(APP_DIR, sid_nb), "reproducible.ipynb")
    if (!file.exists(f))
      return(send_json(con, 404, list(ok = FALSE, code = "not_built",
        error = "这条记录还没有生成 notebook")))
    raw <- readBin(f, "raw", file.info(f)$size + 1024L)
    fname <- paste0(sid_nb, "_reproducible.ipynb")
    hdr <- paste0("HTTP/1.1 200 OK\r\n",
                  "Content-Type: application/x-ipynb+json\r\n",
                  "Content-Length: ", length(raw), "\r\n",
                  "Content-Disposition: attachment; filename=\"", fname, "\"\r\n",
                  "Cache-Control: no-store\r\nConnection: close\r\n\r\n")
    writeBin(c(charToRaw(hdr), raw), con); flush(con)
    return(invisible(NULL))
  }

  if (p == "/api/runs" && identical(req$method, "GET")) {
    if (!HAS_RUNS || !HAS_SESSIONS)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/runs.R 或 sessions.R")))
    if (!DB_READY || is.na(owner_ctx$key))
      return(send_json(con, 403, list(ok = FALSE, code = "identity_unavailable",
        error = "身份未识别，不会显示任何记录")))
    lst <- sess_list(APP_DIR)
    rows <- tryCatch({
      vis <- record_visibility_query(APP_DIR, owner_ctx)
      db_rows(DB_CON, sub("SELECT session_id, owner_key, visibility, name, updated_at",
                          "SELECT session_id, owner_key, visibility", vis$sql, fixed = TRUE),
              vis$params)
    }, error = function(e) NULL)
    if (is.null(rows))
      return(send_json(con, 503, list(ok = FALSE, error = "记录表不可读")))
    keep <- as.character(rows$session_id)
    mine <- Filter(function(s) as.character(s$id) %in% keep, lst$sessions %||% list())
    shared <- as.character(rows$session_id[rows$owner_key != owner_ctx$key])
    order <- if (HAS_FLOW) tryCatch(FLOW_ORDER("server"), error = function(e) character(0))
             else character(0)
    items <- runs_list(APP_DIR, owner_ctx$key, mine, shared, order)
    for (i in seq_along(items)) {
      sid_i <- as.character(items[[i]]$session_id %||% "")
      cap_i <- record_capability(DB_CON, APP_DIR, sid_i, owner_ctx)
      if (isTRUE(cap_i$ok)) items[[i]] <- c(items[[i]], record_cap_fields(cap_i))
      items[[i]]$sentence <- runs_state_sentence(items[[i]])
    }
    return(send_json(con, 200, list(ok = TRUE, items = items,
                                    now = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))))
  }

  # ---- 会话意图写入（app.intent.v1）--------------------------------
  #   这条路只给带有效助手令牌的请求用。理由：
  #     它能建记录、能改归属。对匿名或本机默认身份开放，
  #     等于把「谁能造一条记录」这件事交出去。
  if (p == "/api/intent/import" && identical(req$method, "POST")) {
    if (!HAS_INTENT || !HAS_SESSIONS)
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/intent_bridge.R 或 sessions.R")))
    if (!DB_READY)
      return(send_json(con, 503, list(ok = FALSE, error = "数据库不可用，不能建记录")))
    if (!identical(as.character(owner_ctx$kind %||% ""), "agent"))
      return(send_json(con, 403, list(ok = FALSE, code = "agent_token_required",
        error = paste0("后台助手身份未配置或未通过验证。",
                       "请由受信任的后台身份通道提供凭据；当前请求未获授权。"))))
    b <- body_json()
    # from/to belonged to an older bounded-chain idea. rc_begin never accepted
    # them, so reject before the intent transaction can create a record.
    if (("from" %in% names(b)) || ("to" %in% names(b)) ||
        (is.list(b$auto_dispatch) &&
         (("from" %in% names(b$auto_dispatch)) || ("to" %in% names(b$auto_dispatch)))))
      return(send_json(con, 400, list(ok = FALSE, code = "from_to_removed",
        error = "from/to 已删除；请用单步 step，或使用应用已有的全链路语义。")))
    auto <- b$auto_dispatch
    if (!is.null(auto)) {
      if (!is.list(auto) || !all(names(auto) %in% c("step", "req_id")) ||
          is.null(auto$step) || is.null(auto$req_id) ||
          !is.character(auto$step) || length(auto$step) != 1L ||
          !is.character(auto$req_id) || length(auto$req_id) != 1L ||
          !nzchar(trimws(auto$step)) || !nzchar(trimws(auto$req_id)) ||
          !isTRUE(attempt_valid(trimws(auto$req_id))))
        return(send_json(con, 400, list(ok = FALSE, code = "auto_dispatch_invalid",
          error = "auto_dispatch 只接受非空 step 与合法 req_id；未创建记录。")))
      auto$step <- trimws(auto$step)
      auto$req_id <- trimws(auto$req_id)
      b$auto_dispatch <- NULL
    }
    norm <- intent_normalize(b)
    if (!isTRUE(norm$ok)) {
      return(send_json(con, 400, list(ok = FALSE, code = "intent_invalid",
        errors = norm$errors,
        error = paste(unlist(norm$errors), collapse = "；"))))
    }
    it <- norm$intent

    result <- intent_import(APP_DIR, DB_CON, owner_ctx, it)
    if (identical(result$status, 200L) && !isTRUE(result$body$duplicate) && HAS_AGENT_GATE)
      tryCatch(agent_audit(APP_DIR, "intent_import",
        list(token_id = owner_ctx$external_id, session_id = result$body$session_id,
             project = it$project_name, missing = it$missing)), error = function(e) NULL)
    if (is.null(auto) || !identical(result$status, 200L) || !isTRUE(result$body$ok))
      return(send_json(con, result$status, result$body))
    merge_payload <- function(base, extra) {
      for (nm in names(extra)) base[[nm]] <- extra[[nm]]
      base
    }
    if (!isTRUE(HAS_RUN_ENQUEUE))
      return(send_json(con, 503, c(result$body, list(ok = FALSE,
        code = "dispatch_retryable", retryable = TRUE, step = auto$step,
        req_id = auto$req_id, error = "入队实现不可用；记录已保留，可重试派活。"))))
    sid_dispatch <- as.character(result$body$session_id %||% "")
    out <- tryCatch(run_enqueue(
      con, req, q,
      body = list(session_id = sid_dispatch, step = auto$step,
                  req_id = auto$req_id, force = FALSE),
      owner_ctx = owner_ctx, current_sid = "", via = "/api/intent/import auto_dispatch",
      req_id_override = auto$req_id, step_override = auto$step,
      allow_local_launch = FALSE, agent_dispatch = TRUE),
      error = function(e) list(pa_status = 503L, pa_body = list(
        ok = FALSE, code = "dispatch_retryable", retryable = TRUE,
        error = paste0("记录已保留，但派活失败：", conditionMessage(e)))))
    created <- !isTRUE(result$body$duplicate)
    updated <- isTRUE(result$body$duplicate)
    audit_one <- function(code, exec_id = "") tryCatch(agent_audit(APP_DIR, "intent_auto_dispatch",
      list(token_id = owner_ctx$external_id, owner_key = owner_ctx$key,
           session_id = sid_dispatch, step = auto$step, req_id = auto$req_id,
           exec_id = exec_id, code = code, current_changed = FALSE)),
      error = function(e) NULL)
    if (!identical(as.integer(out$pa_status), 200L)) {
      db <- out$pa_body %||% list()
      gate <- identical(as.integer(out$pa_status), 409L)
      code <- if (gate) as.character(db$code %||% "dispatch_rejected")
              else "dispatch_retryable"
      audit_one(code)
      body <- merge_payload(result$body, db)
      body <- merge_payload(body, list(ok = FALSE,
        code = if (gate) code else "dispatch_retryable",
        record_created = created, record_updated = updated,
        session_id = sid_dispatch, retryable = !gate,
        step = auto$step, req_id = auto$req_id,
        dispatch_status = as.integer(out$pa_status)))
      if (!gate) body$error <- as.character(db$error %||% "记录已保留，但队列写入失败，可重试。")
      return(send_json(con, if (gate) 409L else 503L, body))
    }
    db <- out$pa_body %||% list()
    audit_one("queued", as.character(db$exec_id %||% ""))
    body <- merge_payload(result$body, db)
    body <- merge_payload(body, list(ok = TRUE,
      record_created = created, record_updated = updated,
      session_id = sid_dispatch, queued = TRUE, step = auto$step,
      req_id = auto$req_id, exec_id = as.character(db$exec_id %||% "")))
    return(send_json(con, 200, body))
  }

  # Agent-facing dispatch: the token owner selects the target record explicitly.
  # No current read/switch and no second page click are involved.
  if (p == "/api/agent/dispatch" && identical(req$method, "POST")) {
    if (!identical(as.character(owner_ctx$kind %||% ""), "agent"))
      return(send_json(con, 403, list(ok = FALSE, code = "agent_token_required",
        error = "后台助手身份未配置或未通过验证，未派活。")))
    if (!DB_READY)
      return(send_json(con, 503, list(ok = FALSE, code = "database_unavailable",
        error = "数据库不可用，不能校验记录归属。")))
    if (!isTRUE(HAS_RUN_ENQUEUE))
      return(send_json(con, 503, list(ok = FALSE, code = "dispatch_unavailable",
        error = "入队实现不可用，未派活。")))
    b <- body_json()
    if (isTRUE(b$force))
      return(send_json(con, 403, list(ok = FALSE, code = "force_requires_human",
        error = "force 只接受人的身份。agent 不能给自己放行。")))
    sid_in <- record_trim(b$session_id)
    step_in <- record_trim(b$step)
    req_in <- record_trim(b$req_id)
    if (!nzchar(sid_in) || !nzchar(step_in))
      return(send_json(con, 400, list(ok = FALSE, code = "dispatch_target_required",
        error = "必须显式提供 session_id 与 step。")))
    if (!nzchar(req_in) || !isTRUE(attempt_valid(req_in)))
      return(send_json(con, 400, list(ok = FALSE, code = "req_id_required",
        error = "必须提供合法的持久 req_id，才能保证重复派活不重复入队。")))
    target <- record_session_target(q, b, path = p, current_sid = "",
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    out <- tryCatch(run_enqueue(
      con, req, q,
      body = list(session_id = sid_in, step = step_in, req_id = req_in, force = FALSE),
      owner_ctx = owner_ctx, current_sid = "", via = p,
      req_id_override = req_in, step_override = step_in,
      allow_local_launch = FALSE, agent_dispatch = TRUE),
      error = function(e) list(pa_status = 503L, pa_body = list(
        ok = FALSE, code = "dispatch_retryable", retryable = TRUE,
        error = paste0("派活失败：", conditionMessage(e)))))
    db <- out$pa_body %||% list()
    if (identical(as.integer(out$pa_status), 502L) &&
        identical(as.character(db$code %||% ""), "exec_enqueue_failed")) {
      out$pa_status <- 503L
      db$dispatch_status <- 502L
      db$code <- "enqueue_failed"
      db$retryable <- TRUE
    }
    code <- if (identical(as.integer(out$pa_status), 200L)) "queued"
            else as.character(db$code %||% "dispatch_failed")
    tryCatch(agent_audit(APP_DIR, "agent_dispatch", list(
      token_id = owner_ctx$external_id, owner_key = owner_ctx$key,
      session_id = sid_in, step = step_in, req_id = req_in,
      exec_id = as.character(db$exec_id %||% ""), code = code,
      current_changed = FALSE)), error = function(e) NULL)
    if (is.null(db$session_id)) db$session_id <- sid_in
    return(send_json(con, out$pa_status, db))
  }

  if (p == "/api/sessions/new" && identical(req$method, "POST")) {
    if (!DB_READY || is.na(owner_ctx_key(owner_ctx)))
      return(send_json(con, 403, list(ok = FALSE, error = "身份或数据库不可用，不能创建有归属的分析记录")))
    if (!HAS_SESSIONS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    b <- body_json()
    me <- owner_ctx_key(owner_ctx)

    # ---- 来源记录：必须由当前身份可读，且只读 --------------------------
    #   为什么不能照抄全局 place：全局 place 只是「这台机器上一次保存的样子」，
    #   它可能来自另一份记录（包括别人名下的记录，只要页面切过去看过一眼）。
    #   拿它当模板，等于把那份记录里的路径和字段复制进新记录。
    #   所以来源只能显式给，并且必须当场用 owner 校验；校验不过就拒绝，
    #   而不是静默降级成全局 place。
    #   只读：这条分支从头到尾不写来源记录，也不改它的 place/run_dir。
    src_sid <- trimws(as.character(b$source_session_id %||% ""))
    src_note <- ""
    if (nzchar(src_sid)) {
      if (!isTRUE(db_run_log_authorized(DB_CON, me, src_sid)))
        return(send_json(con, 403, list(ok = FALSE, code = "source_not_owned",
          error = "这份来源记录不属于当前登录身份，不能拿来当新建模板。请先切换到自己的记录。")))
      src <- tryCatch(sess_find(APP_DIR, src_sid), error = function(e) NULL)
      if (is.null(src))
        return(send_json(con, 404, list(ok = FALSE, code = "source_missing",
          error = "来源记录不存在。")))
      pub <- src$place
      src_note <- src_sid
    } else {
      pub <- tryCatch(place_public(APP_DIR), error = function(e) NULL)
    }
    if (!is.list(pub)) pub <- list(mode = "server")

    # ---- 结果目录：本请求自己定，不回写任何记录 ------------------------
    #   以前新建只能继承，没有第二条路。于是「换一个独立结果目录」只能靠
    #   先保存设置 —— 而保存写的是当前记录，等于为了新建去改旧记录。
    want_rd <- trimws(as.character(b$run_dir %||% ""))
    if (nzchar(want_rd)) {
      rd_norm <- gsub("\\\\", "/", want_rd)
      rd_norm <- sub("/+$", "", rd_norm)
      if (!grepl("^/", rd_norm) || grepl("(^|/)\\.\\.(/|$)", rd_norm))
        return(send_json(con, 400, list(ok = FALSE, code = "run_dir_not_absolute",
          error = "结果目录要写成服务器上的绝对路径，且不能含 ..。")))
      if (is.list(pub$server)) pub$server$work_root <- rd_norm
      if (identical(tolower(as.character(pub$mode %||% "")), "local"))
        pub$local$path <- rd_norm
    }

    s <- tryCatch(sess_create(APP_DIR, name = as.character(b$name %||% ""),
                              place = pub, note = as.character(b$note %||% "")),
                  error = function(e) NULL)
    if (is.null(s)) return(send_json(con, 400, list(ok = FALSE, error = "新建失败")))
    # 新记录定下来之后，把这套设置落成页面当前设置。
    #   只写 config/server_profile.json，不碰任何记录的 place ——
    #   不这么做的话，页面显示的还是上一个目录，随后「保存并识别」会把
    #   旧目录写回新记录，白折腾一趟。
    tryCatch(place_write(APP_DIR, pub), error = function(e) NULL)
    # New records need ownership before their first status read or callback, not only
    # after a successful completed-step report. Never claim existing legacy records.
    DBI::dbExecute(DB_CON, "INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,created_at,updated_at) VALUES(?,?,?,?,?,?,?)",
      list(s$id, owner_ctx_key(owner_ctx), s$name, s$mode, s$run_dir, s$created_at, s$updated_at))
    session_activate_state(s)
    return(send_json(con, 200, list(ok = TRUE, session = s, sessions = sess_list(APP_DIR)$sessions)))
  }

  if (p == "/api/sessions/switch" && identical(req$method, "POST")) {
    if (!HAS_SESSIONS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    b <- body_json()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    sid <- target$sid
    access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    r <- tryCatch(sess_switch(APP_DIR, sid), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (!isTRUE(r$ok)) return(send_json(con, 404, c(r, record_cap_fields(access$cap))))
    s <- r$session
    # 记录里带着「这台机器/这些路径」。切过去就把它们恢复成当前设置 ——
    #   不恢复的话，页面显示的是这次分析的目录，实际动手用的是上一次的，
    #   两边不一致比报错更难查。口令留空 = 保留已存的那一份。
    if (!is.null(s$place)) {
      tryCatch(place_write(APP_DIR, s$place), error = function(e) NULL)
    }
    # 运行目录跟着记录走：服务器模式记的是服务器上的绝对路径，
    #   本机不可能存在，所以这里只落 state、不去判目录在不在。
    session_activate_state(s)
    return(send_json(con, 200, c(list(ok = TRUE, session = s), record_cap_fields(access$cap))))
  }

  if (p == "/api/sessions/update" && identical(req$method, "POST")) {
    if (!HAS_SESSIONS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    b <- body_json()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    sid <- target$sid
    access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    patch <- list()
    if (!is.null(b$name)) patch$name <- as.character(b$name)
    if (!is.null(b$note)) patch$note <- as.character(b$note)
    # 「哪些小节是收起的」也按记录记 —— 它属于阅读习惯，不属于配置
    if (!is.null(b$collapsed)) patch$collapsed <- b$collapsed
    # 物种与组织：判"这份依据适不适用"必须要有它。
    #   这两项属于这批数据的属性，不属于某一份表，所以记在分析记录上。
    # ---- 写入前：这个值是不是从路径里抄来的 --------------------------
    #   为什么要在这一层拦（2026-11-04，第二次加固）
    #     上一轮只是改了 261026测试1 那个值，那解决不了同类问题。
    #     成因是固定的：数据目录名里带着组织和物种字样
    #     （…/SY_wangwenbiao_spleen_mouse_20250521/…），
    #     看到路径写着 spleen，就把它当成"这批数据是脾脏"的证据写进记录。
    #     所以要防下一次，必须在写入这一刻做一次强制核对，
    #     而不是靠人记得别猜、也不是靠文档里写了一句。
    #
    #   拦的条件：要写的值恰好出现在这批数据的路径里。
    #     这正是"看起来合理所以被抄进来"的那种情况。
    #   不直接拒：值也可能是人根据实验信息写的，恰好和路径同名（比如路径叫
    #     spleen 但确实取的是脾脏 —— 那路径就是对的）。
    #     所以给一条明确的确认路：带 path_like_ack=true 再提交一次，
    #     并把这个确认记进记录。既不挡人，也不让"抄来的"悄悄过去。
    if (!is.null(b$species) || !is.null(b$tissue)) {
      spW <- if (is.null(b$species)) "" else species_canon(as.character(b$species))
      tiW <- if (is.null(b$tissue)) "" else trimws(as.character(b$tissue))
      plW <- access$cap$place %||% list()
      corpW <- if (exists("dp_path_corpus", mode = "function"))
        dp_path_corpus(plW) else ""
      hitW <- character(0)
      if (nzchar(spW) && exists("dp_value_in_path", mode = "function") &&
          dp_value_in_path(spW, corpW)) hitW <- c(hitW, paste0("物种「", spW, "」"))
      if (nzchar(tiW) && exists("dp_value_in_path", mode = "function") &&
          dp_value_in_path(tiW, corpW)) hitW <- c(hitW, paste0("组织「", tiW, "」"))
      if (length(hitW) && !isTRUE(b$path_like_ack)) {
        return(send_json(con, 409, list(
          ok = FALSE, code = "value_matches_path",
          error = paste0(paste(hitW, collapse = "、"),
                         " 这几个字也出现在这批数据的数据路径里。"),
          why = "路径名是项目的历史命名，不等于这批数据取的就是那个组织/物种。",
          detail = paste0("上一轮就是在这里出过事：有人看到目录名里有 spleen，",
                          "把组织记成了脾脏，实际是小鼠肾脏。",
                          "之后每一次提到这批数据都带着错的组织，而错的地方看起来很正常。"),
          hint = paste0("如果你是根据实验信息填的（不是从目录名抄的），",
                        "带上 path_like_ack=true 再提交一次即可；",
                        "这个确认会被记进分析记录。"),
          matched = as.list(hitW))))
      }
      if (length(hitW) && isTRUE(b$path_like_ack)) {
        patch$path_like_ack_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        patch$path_like_ack_note <- "填值人与数据路径里的字样重合，已明确确认是按实验信息填的"
      }
    }

    if (!is.null(b$species)) patch$species <- species_canon(as.character(b$species))
    if (!is.null(b$tissue)) patch$tissue <- as.character(b$tissue)
    # ---- 人在这里填的，就是"人确认过的" ------------------------------
    #   为什么要打这个标（2026-11-04）
    #     dataset_profile.R 取值时只认两个来源：人确认过的、或参考表自己声明的。
    #     页面上这个「记下」按钮就是人确认的动作 —— 不标记的话，
    #     人明明填过，画像仍然判"没有可核来源"，于是又问一遍。
    #   为什么重要：以前这个字段没有来源标记，于是"人填的"和"从路径猜的"
    #     在文件里长得一模一样。261026测试1 的 Spleen 就是这么混进来的，
    #     事后只能靠翻备份才看得出它没被确认过。
    if (!is.null(b$species) || !is.null(b$tissue)) {
      patch$species_tissue_source <- "user-confirmed"
      patch$species_tissue_updated_at <- format(Sys.time(), "%Y-%m-%d")
    }
    if (!length(patch)) return(send_json(con, 400, list(ok = FALSE, error = "没有要改的内容")))
    r <- tryCatch(sess_update(APP_DIR, sid, patch), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  if (p == "/api/sessions/archives" && identical(req$method, "GET")) {
    if (!HAS_SESSIONS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    target <- record_session_target(q, list(id = q$id %||% ""), path = p,
                                    current_sid = cur_sid(), allow_current = TRUE,
                                    require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "read")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    return(send_json(con, 200, c(list(ok = TRUE,
      archives = sess_archive_list(APP_DIR, target$sid)), record_cap_fields(access$cap))))
  }

  if (p == "/api/sessions/archive" && identical(req$method, "POST")) {
    if (!HAS_SESSIONS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    b <- body_json()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    sid <- target$sid
    access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    r <- tryCatch(sess_archive(APP_DIR, sid,
                               label = as.character(b$label %||% "manual"),
                               note = as.character(b$note %||% "")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  if (p == "/api/sessions/restore" && identical(req$method, "POST")) {
    if (!HAS_SESSIONS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/sessions.R")))
    b <- body_json()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    sid <- target$sid
    access <- record_access(DB_CON, APP_DIR, sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    arc <- as.character(b$archive %||% "")
    if (!nzchar(arc)) return(send_json(con, 400, list(ok = FALSE, error = "缺 archive")))
    r <- tryCatch(sess_archive_restore(APP_DIR, sid, arc),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  if (p == "/api/place") {
    if (!HAS_PLACE) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/place.R")))
    b <- if (identical(req$method, "POST")) body_json() else list()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = TRUE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx,
                            need = if (identical(req$method, "POST")) "write" else "read")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    if (identical(req$method, "POST")) {
      pub0 <- record_place_merge(access$cap$place, b)
      mode0 <- as.character(pub0$mode %||% "local")
      rd0 <- record_place_run_dir(pub0, mode0)
      saved <- tryCatch(sess_update(APP_DIR, target$sid, list(
        place = pub0, mode = mode0, run_dir = rd0)), error = function(e) NULL)
      if (is.null(saved) || !isTRUE(saved$ok))
        return(send_json(con, 400, list(ok = FALSE, error = "目标记录写入失败")))
      tryCatch(DBI::dbExecute(DB_CON,
        "UPDATE sessions SET mode=?, run_dir=?, updated_at=? WHERE session_id=? AND owner_key=?",
        list(mode0, rd0, db_now(), target$sid, access$cap$owner_key)), error = function(e) NULL)
      if (identical(target$sid, sess_current_id(APP_DIR)))
        tryCatch(session_activate_state(saved$session), error = function(e) NULL)
      return(send_json(con, 200, c(list(ok = TRUE,
        ready = isTRUE(record_place_ready(pub0, mode0, rd0)),
        local_path = if (nzchar(rd0) && identical(mode0, "local")) enc2utf8(rd0) else NULL,
        local_ok = if (nzchar(rd0) && identical(mode0, "local")) dir.exists(rd0) else NULL),
        record_public_place(pub0), record_cap_fields(access$cap))))
    }
    pub <- record_public_place(access$cap$place %||% list())
    mode0 <- record_trim(access$cap$mode)
    rd0 <- record_trim(access$cap$run_dir)
    lp <- if (identical(mode0, "local")) rd0 else as.character(pub$local$path %||% "")
    return(send_json(con, 200, c(list(ok = TRUE,
                                     ready = isTRUE(record_place_ready(pub, mode0, rd0)),
                                     local_path = if (nzchar(lp)) enc2utf8(lp) else NULL,
                                     local_ok = if (nzchar(lp)) dir.exists(lp) else NULL),
                                 pub, record_cap_fields(access$cap))))
  }

  if (p == "/api/place/commit" && identical(req$method, "POST")) {
    if (!HAS_PLACE) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/place.R")))
    b <- body_json()
    warns <- character(0)
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    target_sid <- target$sid

    # 1) 只合并并保存到目标记录；全局 place 不是这次写入的中转站。
    pub <- record_place_merge(access$cap$place, b)
    pub_srv <- pub$server %||% list()
    mode <- as.character(pub$mode %||% "local")

    # 本次「保存并识别」的请求标记。
    #   为什么要它：体检结果存在 state/remote_env.json 里，那是「上一次跑完的样子」。
    #   没有请求标记，就没法回答「手上这份结果是这一次跑的吗」，
    #   于是刚点完按钮、助手还没动，页面就已经拿着旧数据显示出结论。
    #   标记落盘后，/api/envcheck 用它判新鲜度：旧结果只能当参考，不能当结论。
    req_id <- paste0("req-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", paste(sprintf("%02x", as.integer(openssl::rand_bytes(16))), collapse = ""))
    req_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    try(write_json_file(file.path(STATE_DIR, "last_commit.json"),
                        list(id = req_id, at = req_at, mode = mode,
                             host = as.character(pub_srv$host %||% ""),
                             work_root = as.character(pub_srv$work_root %||% ""))),
        silent = TRUE)
    # 提交台账：谁、什么时候、从什么客户端发起了这次保存。
    #   为什么要单独一份：last_commit.json 是“当前标记”，每次写都覆盖；
    #   2026-11-18 那次争议里，只剩一个时间戳，既无法证实是人点的，
    #   也无法证实是哪个客户端发的。这里只追加、不做任何判定。
    tryCatch({
      ua <- as.character(req$headers[["user-agent"]] %||% "")
      ref <- as.character(req$headers[["referer"]] %||% "")
      org <- as.character(req$headers[["origin"]] %||% "")
      who <- tryCatch(as.character(owner_ctx_key(owner_ctx)), error = function(e) NA_character_)
      cat(paste0(jsonlite::toJSON(list(
        at = req_at, id = req_id, mode = mode,
        session_id = target_sid,
        # 身份取不到时必须落空串，不能让整条审计写不进去。
        #   为什么单独判长度：owner_ctx 为 NULL 时 owner_ctx_key 返回 character(0)，
        #   is.na(character(0)) 是 logical(0)，直接进 if 会抛错，
        #   而外层 tryCatch 会把这条审计整段吞掉 —— 恰恰在最该有记录的时候没记录。
        actor = if (length(who) != 1L || is.na(who)) "" else who,
        actor_kind = as.character((owner_ctx %||% list())$kind %||% ""),
        ua = substr(ua, 1, 300), origin = org, referer = substr(ref, 1, 500),
        host = as.character(pub_srv$host %||% ""),
        work_root = as.character(pub_srv$work_root %||% "")
      ), auto_unbox = TRUE), "\n"),
        file = file.path(STATE_DIR, "commit_audit.jsonl"), append = TRUE)
      try(Sys.chmod(file.path(STATE_DIR, "commit_audit.jsonl"), mode = "0600"), silent = TRUE)
    }, error = function(e) NULL)

    # 2) 本机模式：把选好的目录设成运行目录。
    #    服务器模式下运行目录是服务器上的绝对路径，本机拿不到，
    #    所以这里只在 local 时落 state。
    rd <- NULL
    if (identical(mode, "local")) {
      lp <- record_trim(pub$local$path)
      if (!nzchar(lp) || !dir.exists(lp)) {
        warns <- c(warns, "本机工作路径还没选，或者那个目录现在不存在。")
      } else {
        rd <- normalizePath(lp, winslash = "/", mustWork = FALSE)
      }
    } else {
      # 服务器模式下「运行目录」就是服务器上的结果目录。
      #   以前这里只取出来用、不落 state，于是页面永远写着「运行目录还没定」——
      #   而服务器路径在本机当然不存在，拿 dir.exists 判永远是假。
      #   记下来之后，页面才知道「跑在哪台机器、结果写哪里」，运行区才能进入就绪态。
      rd <- as.character(pub_srv$work_root %||% "")
      if (!nzchar(rd)) {
        warns <- c(warns, "服务器上的结果目录没填 —— 产物写到哪里还没有着落。")
      }
    }

    # 2b) 这次识别属于哪一份分析记录。
    #   没有记录就现开一份 —— 「保存并识别」本来就是在开始一次分析，
    #   不能出现"识别跑完了却没有归属"的状态：那种状态下刷新一定丢。
    #   识别还没回来（服务器模式），所以这里只记「在等什么」，不记结论。
    if (HAS_SESSIONS) {
      saved <- tryCatch(sess_update(APP_DIR, target_sid, list(
        place = pub, mode = mode, run_dir = if (is.null(rd)) "" else rd)),
        error = function(e) NULL)
      if (is.null(saved) || !isTRUE(saved$ok))
        return(send_json(con, 400, c(list(ok = FALSE, code = "record_write_failed",
          error = "目标分析记录没有保存成功。"), record_cap_fields(access$cap))))
      tryCatch(DBI::dbExecute(DB_CON,
        "UPDATE sessions SET mode=?, run_dir=?, updated_at=? WHERE session_id=? AND owner_key=?",
        list(mode, if (is.null(rd)) "" else rd, db_now(), target_sid, access$cap$owner_key)),
        error = function(e) NULL)
      # 只动目标记录的 identify 槽。
      if (identical(mode, "server")) {
        tryCatch(sess_set_pending(APP_DIR, target_sid, req_id, slot = "identify"),
                 error = function(e) NULL)
      } else {
        tryCatch(sess_clear_pending(APP_DIR, target_sid, slot = "identify"),
                 error = function(e) NULL)
      }
    }
    if (identical(target_sid, sess_current_id(APP_DIR))) {
      st$runDir <- rd
      st$runDirRemote <- !identical(mode, "local")
      try(save_state(st), silent = TRUE)
    }

    # Human save explicitly delegates this server check. Never let an agent mint
    # authorization; only reuse its own still-valid owner-specific handoff.
    agent_handoff_path <- NULL
    if (identical(mode, "server") && HAS_AGENT_GATE && nzchar(target_sid) &&
        db_api_notebook_authorized(if (DB_READY) DB_CON else NULL, owner_ctx, target_sid)) {
      ho <- tryCatch(agent_handoff_ensure(APP_DIR, owner_ctx),
                     error = function(e) list(ok = FALSE, code = "handoff_unavailable"))
      if (isTRUE(ho$ok)) agent_handoff_path <- ho$path
      else warns <- c(warns, "未取得体检回写凭据交接；请已认证用户重新保存，助手不能自行授权。")
    }

    # 3) 生成默认配置（pipeline.json / gates.json）+ 登记守卫指纹。
    #    顺序不能反：没有 pipeline.json，后面的体检和识别就没有依据可写。
    if (!is.null(rd) && nzchar(rd) && dir.exists(rd)) {
      tryCatch({
        if (!file.exists(file.path(rd, "pipeline.json")))
          init_config(rd, ENGINE_DIR, FALSE)
        gl <- file.path(GUARD_DIR, "guard.R")
        if (file.exists(gl)) {
          dir.create(file.path(rd, "status"), showWarnings = FALSE, recursive = TRUE)
          lk <- file.path(rd, "status", "guard.lock.json")
          if (!file.exists(lk)) {
            obj <- list(guard_version = "1.0", guard_md5 = unname(tools::md5sum(gl)),
                        locked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                        note = "entry-guard fingerprint")
            con2 <- file(lk, open = "wb")
            writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE))), con2)
            close(con2)
          }
        }
      }, error = function(e) warns <<- c(warns, paste0("生成默认配置失败：", conditionMessage(e))))
    }

    # 4) 「原始数据在哪」这件事，在第一步已经定过了。
    #    闸门里那条 data_indir 如果还挂着 pending，就把它写成已回答 ——
    #    否则用户会在两个地方被问同一件事。
    if (!is.null(rd) && nzchar(rd) && dir.exists(rd)) {
      tryCatch({
        gf <- file.path(rd, "gates.json")
        if (file.exists(gf)) {
          gj <- jsonlite::fromJSON(paste(readLines(gf, warn = FALSE, encoding = "UTF-8"),
                                         collapse = "\n"), simplifyVector = FALSE)
          data_path <- if (identical(mode, "local")) as.character(pub$local$path %||% "")
                       else as.character(pub_srv$data_root %||% "")
          changed <- FALSE
          for (i in seq_along(gj$gates)) {
            g <- gj$gates[[i]]
            if (identical(as.character(g$id %||% ""), "data_indir") &&
                identical(as.character(g$status %||% "pending"), "pending") && nzchar(data_path)) {
              gj$gates[[i]]$status <- "answered"
              gj$gates[[i]]$answer <- "auto"
              gj$gates[[i]]$answer_detail <- data_path
              gj$gates[[i]]$answered_by <- "place"
              gj$gates[[i]]$answered_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
              changed <- TRUE
            }
          }
          if (changed) {
            gj$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
            tmp <- paste0(gf, ".tmp")
            con2 <- file(tmp, open = "wb")
            writeBin(charToRaw(enc2utf8(jsonlite::toJSON(gj, auto_unbox = TRUE, pretty = TRUE,
                                                         null = "null", digits = NA))), con2)
            close(con2)
            file.rename(tmp, gf)
          }
        }
      }, error = function(e) warns <<- c(warns, paste0("更新闸门失败：", conditionMessage(e))))
    }

    # 5) 识别环境。
    #
    #    本机模式：本机当场跑 env_check.R 与判定，读到的数字只属于这台机器。
    #
    #    服务器模式：应用不自己去连服务器，一次都不试。
    #      它跑在容器里，本来就到不了客户内网。直连失败不是需要向用户解释的
    #      "正常现象"，而是一条根本不该存在的分支 —— 那条分支给页面添了一段
    #      "应用跑在云端、连不到你内网的服务器、这是正常的"自辩文案，
    #      把链路说成"应用连不上，所以只好让助手代跑"。
    #      真实的链路是：应用只负责把这次要做什么交给助手，
    #      执行方固定是能连上服务器、并且从会话里接到这条指令的那一端。
    #      所以这里只留下两件事：写一条待执行任务留痕、带回本次请求号，
    #      然后由页面把这次识别交给助手，等它把真实结果写回来。
    env_obj <- NULL; dec_obj <- NULL; scan_obj <- NULL
    stale_env <- NULL
    queued_check <- NULL
    needs_remote <- identical(mode, "server")
    # 本次请求标记：随任务一起下发，回写时带回来。
    #   它是"这份结果属于这一次"的唯一凭据。
    req_id <- if (exists("req_id")) req_id else NULL

    if (needs_remote) {
      # 待执行任务：谁从会话里接到这次指令，谁去执行，并把结果写回
      #   state/remote_env.json。
      #   写这条是为了留痕（事后能查到"这次请求排给谁了"），
      #   不是为了让应用自己再试一次 —— 应用侧根本没有"自己连"这条路。
      tq <- file.path(STATE_DIR, "tasks.jsonl")
      dir.create(dirname(tq), showWarnings = FALSE, recursive = TRUE)
      tid <- paste0("remote-check-", format(Sys.time(), "%Y%m%d%H%M%S"))
      tk <- list(id = tid, kind = "remote_check",
                 host = as.character(pub_srv$host %||% ""),
                 port = as.integer(pub_srv$port %||% 22L),
                 user = as.character(pub_srv$user %||% ""),
                 data_root = as.character(pub_srv$data_root %||% ""),
                 script_root = as.character(pub_srv$script_root %||% ""),
                 work_root = as.character(pub_srv$work_root %||% ""),
                 req_id = if (is.null(req_id) || !nzchar(req_id)) NULL else req_id,
                 reason = "delegated",
                 status = "pending",
                 created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      cc <- file(tq, open = "a", encoding = "UTF-8")
      writeLines(jsonlite::toJSON(tk, auto_unbox = TRUE, null = "null"), cc, useBytes = TRUE)
      close(cc)
      Sys.chmod(tq, mode = "0600")
      queued_check <- tid
      # 本机残留的体检结果不能拿去冒充服务器：清掉，免得下次又被读进来
      old_ec <- if (is.null(rd) || !nzchar(rd)) NULL else file.path(rd, "status", "env_check.json")
      old_dc <- if (is.null(rd) || !nzchar(rd)) NULL else file.path(rd, "status", "decision.json")
      if (!is.null(old_ec) && file.exists(old_ec)) try(unlink(old_ec), silent = TRUE)
      if (!is.null(old_dc) && file.exists(old_dc)) try(unlink(old_dc), silent = TRUE)
    }
    if (needs_remote) {
      srv_host <- as.character(pub_srv$host %||% "")
      warns <- c(warns, paste0(
        "环境体检还没做：需要在服务器 ", srv_host,
        " 上执行。这次识别已交给助手，结果写回来之前这一栏不下结论。"))
    } else if (!identical(mode, "local")) {
      # 服务器模式的运行目录在服务器上，本机不存在是正常的。
      #   这次的结果由执行端在服务器上跑完回写。
    } else if (!is.null(rd) && nzchar(rd) && dir.exists(rd)) {
      cfg <- tryCatch(read_cfg(rd), error = function(e) NULL)
      rs <- rscript_bin(cfg)
      sdir <- file.path(rd, "status")
      dir.create(sdir, showWarnings = FALSE, recursive = TRUE)

      ec <- file.path(ENGINE_DIR, "env_check.R")
      if (file.exists(ec)) {
        out_f <- file.path(sdir, "env_check.json")
        o <- tryCatch(suppressWarnings(system2(rs, c("--vanilla", ec, "--out", out_f),
                                              stdout = TRUE, stderr = TRUE)),
                      error = function(e) structure(conditionMessage(e), status = 1L))
        code <- attr(o, "status"); if (is.null(code)) code <- 0L
        if (file.exists(out_f)) env_obj <- read_json_file(out_f)
        if (!identical(as.integer(code), 0L) && is.null(env_obj))
          warns <- c(warns, paste0("环境体检没跑成：", substr(paste(utils::tail(o, 2L), collapse = " "), 1, 120)))
      }

      dc <- file.path(ENGINE_DIR, "decide_cli.R")
      if (file.exists(dc)) {
        o <- tryCatch(suppressWarnings(system2(rs, c("--vanilla", dc, "--dir", rd, "--engine", ENGINE_DIR),
                                              stdout = TRUE, stderr = TRUE)),
                      error = function(e) structure(conditionMessage(e), status = 1L))
        df <- file.path(sdir, "decision.json")
        if (file.exists(df)) dec_obj <- read_json_file(df)
      }
      sf <- file.path(sdir, "scripts_scan.json")
      if (file.exists(sf)) scan_obj <- read_json_file(sf)
    } else {
      # 已经有服务器执行回来的结果时，别再说"体检没跑" ——
      #   那会和页面上那 15 项数据打架。这里只说缺的是运行目录。
      if (is.null(env_obj)) {
        warns <- c(warns, "还没有可用的运行目录，所以环境体检与脚本识别先没跑。")
      } else {
        warns <- c(warns, paste0(
          "运行目录还没定。本机这次没做体检，上面显示的是服务器那台机器执行回来的结果。"))
      }
    }

    # 6) 最近用过的目录：本机模式下方便切回来
    if (!is.null(rd) && nzchar(rd) && HAS_HISTORY)
      try(hist_recent_add(APP_DIR, rd), silent = TRUE)

    return(send_json(con, 200, c(list(
      ok = TRUE,
      place = c(list(ready = isTRUE(record_place_ready(pub, mode,
        if (is.null(rd)) "" else rd))), pub),
      run_dir = if (is.null(rd) || !nzchar(rd)) NULL else enc2utf8(rd),
      run_dir_remote = !identical(mode, "local"),
      env = env_obj, decision = dec_obj, scans = scan_obj,
      # needs_remote=TRUE 表示"这一轮没有识别服务器"，
      # 前端据此显示"还没检查过"，而不是拿空结果当成"检查完了没问题"。
      needs_remote = needs_remote,
      queued_check = queued_check,
      agent_handoff_path = agent_handoff_path,
      # 本次请求标记：前端把它写进注入给助手的文本里，
      #   执行端回写时原样带回，页面才能确认"这份结果就是这一次跑出来的"。
      request_id = if (is.null(req_id) || !nzchar(req_id)) NULL else req_id,
      # 参考用的旧结果（只在直连不通、但以前有人跑过时才有）。
      #   单独一个字段，不混进 env —— 一旦混进 env，页面就会把它当成结论显示。
      stale_env = stale_env,
      stale_note = if (is.null(stale_env)) NULL else paste0(
        "以下是上一次执行回来的结果，仅供参考；这次的结果要等助手跑完写回来。"),
      warnings = as.list(warns)), record_cap_fields(access$cap))))
  }

  if (p == "/api/env") {
    cfg <- if (!is.null(runDir)) read_cfg(runDir) else NULL
    py <- find_python(cfg)
    return(send_json(con, 200, list(
      ok = TRUE, runDir = runDir %||% NULL,
      host = enc2utf8(Sys.info()[["nodename"]]),
      home = enc2utf8(path.expand("~")),
      platform = .Platform$OS.type, isWin = IS_WIN,
      tools = list(
        rscript = enc2utf8(rscript_bin(cfg)),
        rversion = paste(R.version$major, R.version$minor, sep = "."),
        python = if (is.null(py)) NULL else enc2utf8(py))
    )))
  }

  if (p == "/api/fs") {
    dir <- if (!is.null(q$dir) && nzchar(q$dir)) q$dir else default_browse_dir()
    dir <- gsub("\\\\", "/", dir)
    if (!dir.exists(dir)) return(send_json(con, 404, list(ok = FALSE, error = paste0("目录不存在：", dir))))
    listed <- list_dir(dir)
    parent <- dirname(dir)
    if (IS_WIN && grepl("^[A-Za-z]:/?$", dir)) parent <- NULL
    if (!IS_WIN && identical(parent, dir)) parent <- NULL
    return(send_json(con, 200, c(
      list(ok = TRUE, dir = enc2utf8(dir),
           parent = if (is.null(parent)) NULL else enc2utf8(gsub("\\\\", "/", parent)),
           drives = if (IS_WIN) enc2utf8(unlist(drives())) else NULL),
      listed)))
  }

  if (p == "/api/run-dir") {
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    if (identical(req$method, "GET")) {
      ctx <- record_context(access$cap, require_run_dir = TRUE)
      if (!isTRUE(ctx$ok)) {
        out <- record_refusal(ctx)
        out$body <- c(out$body, record_cap_fields(access$cap))
        return(send_json(con, out$status, out$body))
      }
      runDir <- ctx$run_dir
      cfg <- read_cfg(runDir)
      pipe <- read_json_file(file.path(runDir, "pipeline.json"))
      gates <- read_json_file(file.path(runDir, "gates.json"))
      # 步骤清单按档位给：服务器模式给 flow_spec 的 server 档，
      #   否则给 step_plan 那套。混着给就会出现"页面上一排按钮，
      #   每个都指向服务器上不存在的脚本"。
      stepsOut <- if (identical(flow_profile(sid), "server")) flow_server_steps()
                  else step_titles(cfg, runDir, ENGINE_DIR)
      return(send_json(con, 200, c(list(
        ok = TRUE, runDir = enc2utf8(gsub("\\\\", "/", runDir)),
        pipeline = pipe, gates = gates,
        profile = flow_profile(sid), session_id = sid,
        steps = stepsOut,
        stepState = read_step_state(runDir)), record_cap_fields(access$cap))))
    }
    if (identical(req$method, "POST")) {
      b <- body_json()
      dir <- gsub("\\\\", "/", as.character(b$dir %||% ""))
      if (!nzchar(dir) || !dir.exists(dir) || !isTRUE(file.info(dir)$isdir))
        return(send_json(con, 400, list(ok = FALSE, error = paste0("这个目录用不了：", dir))))
      dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
      pl <- record_place_merge(access$cap$place, list(local = list(path = dir), mode = "local"))
      saved <- tryCatch(sess_update(APP_DIR, sid, list(run_dir = dir, mode = "local", place = pl)),
                        error = function(e) NULL)
      if (is.null(saved) || !isTRUE(saved$ok))
        return(send_json(con, 400, list(ok = FALSE, error = "目标记录写入失败")))
      tryCatch(DBI::dbExecute(DB_CON,
        "UPDATE sessions SET mode='local', run_dir=?, updated_at=? WHERE session_id=? AND owner_key=?",
        list(dir, db_now(), sid, access$cap$owner_key)), error = function(e) NULL)
      if (identical(sid, sess_current_id(APP_DIR))) {
        st$runDir <- dir
        st$runDirRemote <- FALSE
        save_state(st)
        if (HAS_HISTORY) hist_recent_add(APP_DIR, dir)
      }
      return(send_json(con, 200, c(list(ok = TRUE, session_id = sid, runDir = dir),
        record_cap_fields(access$cap))))
    }
  }

  if (p == "/api/config" && identical(req$method, "POST")) {
    b <- body_json()
    target <- record_session_target(q, b, path = p, current_sid = cur_sid(),
                                    allow_current = FALSE, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "write",
                            require_ctx = TRUE, require_run_dir = TRUE)
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    target_sid <- target$sid
    runDir <- access$ctx$run_dir
    st$runDir <- runDir
    st$runDirRemote <- access$ctx$run_dir_remote
    # 服务器模式下运行目录在服务器上，本机写不了。
    #   以前这里直接 400，于是"把这次设置写到服务器上"没有出口。
    #   现在同样改成交给助手：设置内容一并带上，助手在服务器上落地。
    if (isTRUE(access$ctx$run_dir_remote)) {
      pl <- access$ctx$place
      sid <- target_sid
      rq <- if (HAS_SESSIONS && nzchar(as.character(sid %||% "")))
        as.character(sess_get_pending(APP_DIR, sid, slot = "step") %||% "") else ""
      ins <- remote_step_instruction("config", pl, session_id = as.character(sid %||% ""), req_id = rq)
      # 把这次要写的设置附在指令后面 —— 不给内容，助手只能猜。
      setbits <- character(0)
      if (!is.null(b[["gates.json"]])) {
        gg <- b[["gates.json"]][["gates"]]
        if (length(gg)) {
          ans <- Filter(function(g) identical(as.character(g$status %||% ""), "answered"), gg)
          if (length(ans)) {
            setbits <- vapply(ans, function(g) {
              v <- g$answer
              if (is.null(v)) v <- ""
              if (length(v) > 1) v <- paste(unlist(v), collapse = "/")
              paste0(as.character(g$title %||% g$id %||% ""), " = ", as.character(v))
            }, character(1))
          }
        }
      }
      if (length(setbits))
        ins <- paste0(ins, "\n\n人已经定下的事（按这个写进 gates.json）：\n",
                      paste(paste0("- ", setbits), collapse = "\n"))
      remote_step_put("config", list(
        title = "生成配置文件", status = "delegated", code = NULL,
        note = "已交给助手，等它回写",
        delegated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        req_id = if (nzchar(rq)) rq else NULL,
        session_id = target_sid), sid = target_sid)
      return(send_json(con, 200, c(list(
        ok = TRUE, delegated = TRUE, step = "config", instruction = ins,
        msg = "当前是服务器模式，设置要写在服务器的结果目录里。已把这次设置交给助手去落地。"),
        record_cap_fields(access$cap))))
    }
    if (isTRUE(b$init)) {
      # 注意参数顺序：init_config(runDir, engineDir, overwrite)
      #   漏了 engineDir 会让布尔值跑到 engineDir 位置上，表现为「模板缺失」。
      w <- init_config(runDir, ENGINE_DIR, isTRUE(b$overwrite))
      # 顺手登记守卫指纹。没有这一步，之后所有运行都会被守卫拒掉
      # （因为查不到 guard.lock.json）。网页入口和命令行入口必须做同样的事。
      tryCatch({
        gl <- file.path(GUARD_DIR, "guard.R")
        if (file.exists(gl)) {
          dir.create(file.path(runDir, "status"), showWarnings = FALSE, recursive = TRUE)
          md5 <- unname(tools::md5sum(gl))
          obj <- list(guard_version = "1.0", guard_md5 = md5,
                      locked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                      note = "entry-guard fingerprint")
          con2 <- file(file.path(runDir, "status", "guard.lock.json"), open = "wb")
          writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE))), con2)
          close(con2)
        }
      }, error = function(e) NULL)
      return(send_json(con, 200, c(list(ok = TRUE, written = w, runDir = runDir),
                                   record_cap_fields(access$cap))))
    }
    written <- character(0)
    for (nm in c("pipeline.json", "gates.json")) {
      if (is.null(b[[nm]])) next
      con2 <- file(file.path(runDir, nm), open = "wb")
      writeBin(charToRaw(enc2utf8(jsonlite::toJSON(b[[nm]], auto_unbox = TRUE, pretty = TRUE,
                                                   null = "null", digits = NA))), con2)
      close(con2)
      written <- c(written, nm)
    }
    if (!length(written)) return(send_json(con, 400, list(ok = FALSE, error = "没有可保存的内容")))
    return(send_json(con, 200, c(list(ok = TRUE, written = written),
                                 record_cap_fields(access$cap))))
  }

  if (p == "/api/run" && identical(req$method, "POST")) {
    out <- run_enqueue(con, req, q, body_json(), owner_ctx,
                       current_sid = cur_sid(), via = "/api/run")
    return(send_json(con, out$pa_status, out$pa_body))
  }

  # 助手回写某一步的执行结果。
  #   服务器模式下应用自己看不见服务器上的进程，只能靠这一条知道"跑到哪了"。
  #   没有它，页面上的步骤状态永远停在「空闲」，做了事也看不出变化。
  if (p == "/api/step/import" && identical(req$method, "POST")) {
    b <- body_json()
    sid_step <- as.character(b$step %||% "")
    if (!nzchar(sid_step)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 step")))
    stv <- as.character(b$status %||% "done")
    # ---- 不许自填完成（K1 第 1 项）----------------------------------
    #   verified 的定义权在判定器，不在回写方。
    #   这一条必须排在状态白名单之前：排在后面的话，请求会先被白名单
    #   以「不支持的状态」挡掉，调用方看到的是「你写错了字段名」，
    #   而不是「这件事不允许你决定」。前者让人去改字段，后者才是把规则说清。
    if (identical(stv, "verified"))
      return(send_json(con, 403, list(
        ok = FALSE, code = "verified_not_writable", step = as.character(b$step %||% ""),
        error = "verified 只能由判定器写入。回写一律按「申报」处理：先存 reported，再由 verdict.R 判定。",
        hint = "把 status 改成 done 并附上产物清单、退出码、脚本指纹与 set_id。")))
    if (!stv %in% c("done", "failed", "running", "delegated", "stopped"))
      return(send_json(con, 400, list(ok = FALSE, code = "invalid_step_status",
        error = paste0("不支持的回写状态：", stv, "；未更新步骤，不能把未知状态当完成。"))))
    code <- b$code
    # ---- 无请求号的历史在途任务：按 exec 任务派生一次性绑定 ----------
    #   为什么需要这条：/api/run 早期入队时没把请求号记到任务上，
    #   执行端回写只能发空 req_id，而 callback_owner 要求非空且 attempt 已登记。
    #   结果就是作业在集群上跑完了，应用这边永远收不到结果。
    #   这里不放松鉴权，反而多加几道核对 —— 客户端自报一个 exec_id 不足以写任何记录，
    #   必须同时满足：任务在案、session 一致、step 一致、状态合法、
    #   script 非空、记录属主可从库里确认、记录结果目录非空。
    # 本次已验收回写体：补录准入用它绑定的 rel+md5 判「新文件是否被验收过」。
    verified_payload <- function() {
      rq0 <- as.character(b$req_id %||% "")
      if (!nzchar(rq0) || !isTRUE(DB_READY)) return(NULL)
      row <- tryCatch(DBI::dbGetQuery(DB_CON,
        paste0("SELECT payload FROM step_attempts WHERE req_id = ? AND step_id = ? ",
               "AND status = 'verified' ORDER BY verified_at DESC LIMIT 1"),
        list(rq0, sid_step)), error = function(e) NULL)
      if (is.null(row) || !nrow(row)) return(NULL)
      tryCatch(jsonlite::fromJSON(row$payload[[1]], simplifyVector = FALSE),
               error = function(e) NULL)
    }
    rq_in <- trimws(as.character(b$req_id %||% ""))
    if (!nzchar(rq_in)) rq_in <- trimws(as.character(b$req_id_carried %||% ""))
    exid_in <- trimws(as.character(b$exec_id %||% ""))
    # Compatibility for executors that only echo exec_id/job_number. The queue
    # item already carries the dispatcher-owned attempt; copy it into the body
    # rather than letting the data layer infer a new attempt from req_id.
    if (!nzchar(as.character(b$attempt_id %||% ""))) {
      ex_bind <- if (nzchar(exid_in)) exec_by_id(exid_in) else NULL
      if (is.null(ex_bind) && nzchar(as.character(b$job_number %||% "")))
        ex_bind <- exec_by_job_number(as.character(b$job_number),
                                      step = as.character(b$step %||% ""),
                                      session_id = as.character(b$session_id %||% ""))
      if (!is.null(ex_bind)) {
        if (nzchar(as.character(ex_bind$attempt_id %||% "")))
          b$attempt_id <- as.character(ex_bind$attempt_id)
        if (!nzchar(as.character(b$run_id %||% "")) &&
            nzchar(as.character(ex_bind$run_id %||% "")))
          b$run_id <- as.character(ex_bind$run_id)
        if (!nzchar(as.character(b$version_id %||% "")) &&
            nzchar(as.character(ex_bind$version_id %||% "")))
          b$version_id <- as.character(ex_bind$version_id)
      }
    }
    # 这里不额外去 ex_bind 补 version_id（2026-11-23 核实后决定不补）。
    #   本来打算把版本补写从上面的 attempt_id 条件里拆出来，理由是"执行端自带
    #   attempt_id 时这段不进去"。核实后：拆不拆结果都一样。
    #   wbi_bind_attempt 一旦收到 attempt_id，就只按 attempt_id 命中记录
    #   （wbi_attempt_by_id），不做任何版本过滤；版本不一致时只记一条 overrides，
    #   随后仍用 att$version_id 覆盖 —— attempt 本身就是版本的真相。
    #   带 "AND version_id = ?" 过滤的只有 req_id 分支，而那条用的是
    #   记录当前版本的 target_vid，不是回写请求里带的那份版本。
    #   所以补不补都不影响成败，唯一差别是 overrides 里多一条观测记录。
    #   为一条观测记录去动回写主路径，不值得，保持最小。
    if (!nzchar(rq_in) && nzchar(exid_in) && HAS_SESSIONS) {
      sid_in <- trimws(as.character(b$session_id %||% ""))
      stp_in <- trimws(as.character(b$step %||% ""))
      hit <- Filter(function(it) identical(as.character(it$id %||% ""), exid_in),
                    tryCatch(exec_read()$items, error = function(e) list()) %||% list())
      ex0 <- if (length(hit)) hit[[1]] else NULL
      ok_ex <- exec_compat_ok(ex0, sid_in, stp_in)
      if (!ok_ex)
        return(send_json(con, 409, list(ok = FALSE, code = "exec_binding_invalid",
          error = "回写带了 exec_id，但它不对应本记录本步骤的在案任务；未写入。",
          exec_id = exid_in, session_id = sid_in, step = stp_in)))
      # 属主只从库里这条记录读，不看请求头，也不看客户端自报。
      own <- tryCatch(db_scalar(DB_CON, "SELECT owner_key FROM sessions WHERE session_id = ?",
                                list(sid_in)), error = function(e) NA)
      if (!DB_READY || length(own) != 1L || is.na(own) || !nzchar(as.character(own)))
        return(send_json(con, 403, list(ok = FALSE, code = "exec_binding_not_owner",
          error = "无法从库里确认这条记录的属主；未写入。")))
      # 结果目录必须已有值：没有它就无从判断产物落在哪里。
      rd0 <- if (!DB_READY) NA_character_ else tryCatch(
        db_scalar(DB_CON, "SELECT run_dir FROM sessions WHERE session_id = ?", list(sid_in)),
        error = function(e) NA_character_)
      rd0 <- trimws(as.character(rd0))
      if (length(rd0) != 1L || is.na(rd0) || !nzchar(rd0))
        return(send_json(con, 409, list(ok = FALSE, code = "exec_binding_no_rundir",
          error = "这条记录还没有结果目录，无法核对产物归属；未写入。")))
      # 派生一个稳定请求号，只用于这条任务，不借用识别请求号，也不改任务本身。
      derived <- paste0("exec-", exid_in)
      reg <- tryCatch(attempt_register(APP_DIR, sid_in, derived, origin = "exec_task_compat"),
                      error = function(e) list(ok = FALSE, error = conditionMessage(e)))
      if (!isTRUE(reg$ok))
        return(send_json(con, 409, list(ok = FALSE, code = "exec_binding_attempt_failed",
          error = paste0("这条任务的兼容 attempt 没能建立：", as.character(reg$error %||% "未知")),
          exec_id = exid_in)))
      # 审计旁证：attempt.json 是不可变事实，不往里补字段；
      #   来源与任务原始创建时间单独留一份，只写一次。
      tryCatch({
        side <- file.path(attempt_dir(APP_DIR, sid_in, derived), "binding_source.json")
        if (!file.exists(side)) {
          sess_write_json(side, list(
            binding_source = "exec_task_compat",
            exec_id = exid_in,
            exec_created_at = as.character(ex0$created_at %||% ""),
            exec_script = as.character(ex0$script %||% ""),
            session_id = sid_in, step = stp_in,
            owner_key = as.character(own),
            run_dir = rd0,
            derived_req_id = derived,
            noted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            rule = "服务端按已存在任务一次性派生；未借用识别请求号，未改写任务字段"))
          Sys.chmod(side, "0600")
        }
      }, error = function(e) NULL)
      b$req_id <- derived
      b$binding_source <- "exec_task_compat"
      b$exec_bound_id <- exid_in
      # 本题次的开始时间只认服务端任务创建时间，不采信客户端自报的 started_at。
      #   为什么必须无条件覆盖：产物 mtime 的基线就是这个值（下面 start0 取它）。
      #   客户端只要把 started_at 写早一点，上一轮的旧产物就"晚于本次开始"，
      #   于是旧结果被当成这一轮的新结果写进来 —— 数字齐全、时间齐全，看不出错。
      #   自报值留一份在旁边，只为核对，不参与判定。
      if (nzchar(trimws(as.character(b$started_at %||% ""))) &&
          !identical(trimws(as.character(b$started_at)), trimws(as.character(ex0$created_at %||% ""))))
        b$client_started_at_ignored <- as.character(b$started_at)
      # ---- 让这条派生 attempt 成为本步的选中项 ------------------------
      #   为什么必须做：回写存进 attempt 之后，还要过 attempt_selected 那一关
      #   才会投影到 remote_steps 与页面。它比的是"本步 pending 头"与这条 req，
      #   派生号不在里面，selected 就是 false —— 结果只落进 attempt 文件，
      #   页面永远看不到 running/done，等于"跑完了但界面不动"。
      #   前提两条，缺一不设：
      #     · 本步当前没有别的请求号在等回写（空，或就是这条派生号）
      #     · 这条 exec 仍是该分析该步骤最近的一条任务
      #   第二条专门防"旧任务顶掉后来者"：新任务一建就变成 latest，
      #   此后旧任务的回写只能走受限发布（只存 attempt），不改选中项。
      cur_pend <- tryCatch(as.character(sess_get_pending(APP_DIR, sid_in, slot = "step") %||% ""),
                           error = function(e) "")
      latest_ex <- tryCatch(exec_latest_for(stp_in, sid_in), error = function(e) NULL)
      is_latest <- !is.null(latest_ex) &&
        identical(as.character(latest_ex$id %||% ""), exid_in)
      can_select <- is_latest && (!nzchar(cur_pend) || identical(cur_pend, derived))
      sel_state <- if (!can_select) "skipped_other_pending_or_newer_task" else "set"
      if (can_select) {
        pset <- tryCatch(sess_set_pending(APP_DIR, sid_in, derived, slot = "step"),
                         error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        if (!isTRUE(pset$ok))
          sel_state <- paste0("set_failed:", as.character(pset$error %||% "unknown"))
      }
      tryCatch({
        side2 <- file.path(attempt_dir(APP_DIR, sid_in, derived), "binding_select.json")
        if (!file.exists(side2)) {
          sess_write_json(side2, list(
            exec_id = exid_in, step = stp_in, derived_req_id = derived,
            pending_before = cur_pend, exec_is_latest = is_latest,
            select_action = sel_state,
            noted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            rule = "仅当本步无其他待回写号、且该任务仍是最新任务时才设为选中项"))
          Sys.chmod(side2, "0600")
        }
      }, error = function(e) NULL)
      b$started_at <- as.character(ex0$created_at %||% "")
    }
    binding <- callback_owner(b)
    if (!isTRUE(binding$ok)) return(send_json(con, 409, binding))
    sid_chk <- binding$sid
    b$session_id <- sid_chk
    b$req_id <- binding$req_id
    # Task-bound callbacks get their execution context from the bound record,
    # never from the ephemeral current selection. A caller-supplied run_dir is
    # accepted only when it is the same directory.
    step_cap <- if (isTRUE(DB_READY)) tryCatch(
      record_capability(DB_CON, APP_DIR, sid_chk, list(key = NA_character_)),
      error = function(e) NULL) else NULL
    record_run_dir <- record_trim((step_cap %||% list())$run_dir)
    if (!nzchar(record_run_dir))
      return(send_json(con, 409, list(ok = FALSE, code = "record_run_dir_missing",
        session_id = sid_chk,
        error = "这条记录没有 run_dir，回写无法定位产物；未写入任何记录。")))
    callback_run_dir <- record_trim(b$run_dir)
    same_run_dir <- function(a, c) identical(
      sub("/+$", "", gsub("\\\\", "/", record_trim(a))),
      sub("/+$", "", gsub("\\\\", "/", record_trim(c))))
    if (nzchar(callback_run_dir) && !same_run_dir(callback_run_dir, record_run_dir))
      return(send_json(con, 409, list(ok = FALSE, code = "record_run_dir_mismatch",
        session_id = sid_chk, run_dir = record_run_dir,
        error = "回写的 run_dir 不属于这条分析记录；未写入任何记录。")))
    b$run_dir <- record_run_dir
    # ---- 活跃执行保护 ------------------------------------------------
    #   这一环还有执行端的任务在管时，只有那个任务本人能写终态。
    #   判据是 exec_id 绑定，不是"log 非空"——补一句 log 就能过，那种判据没用。
    #   执行端真实失败、准备阶段失败都带 exec_id，走同一条正常通路，不受影响。
    if (HAS_EXEC_GUARD) {
      # 守卫自己出错时不能当成放行。
      #   19:11 那次故障的本质就是"没人敢拦"，如果守卫抛异常就 null，
      #   等于给同类回写又开了一道门。这里一律按服务不可用拒收，零写入。
      gd <- tryCatch({
        b2 <- import_started_at_guard(b)
        list(ok = TRUE, b = b2, g = import_active_exec_guard(b2, sess_dir = {
          s_ctx0 <- tryCatch(sess_find(APP_DIR, binding$sid), error = function(e) NULL)
          as.character((s_ctx0 %||% list())$run_dir %||% "")
        }))
      }, error = function(e) list(ok = FALSE, err = conditionMessage(e)))
      if (!isTRUE(gd$ok)) {
        tryCatch(import_guard_audit(STATE_DIR, b, list(code = "exec_guard_error")),
                 error = function(e) NULL)
        return(send_json(con, 503, list(
          ok = FALSE, code = "exec_guard_unavailable", step = as.character(b$step %||% ""),
          wrote_state = FALSE, wrote_started_at = FALSE,
          error = paste0("执行保护模块出错，无法确认这一环是否已有执行端在管：",
                         as.character(gd$err %||% "未知"), "。按拒收处理，状态与开始时间都不改。"))))
      }
      b <- gd$b
      g0 <- gd$g
      if (!is.null(g0)) {
        tryCatch(import_guard_audit(STATE_DIR, b, g0), error = function(e) NULL)
        code0 <- if (identical(as.numeric(g0$http %||% 409), 503)) 503 else 409
        return(send_json(con, code0, g0))
      }
    }
    # 物种/组织：执行端没带就按本记录自己的会话上下文补齐。
    #   注释环用它们判断参考词典是否适用；值来自用户建记录时的选择，
    #   是本应用侧的上下文，不是回写体自报，也不允许跨记录借。
    if (!nzchar(as.character(b$species %||% "")) ||
        !nzchar(as.character(b$tissue %||% ""))) {
      s_ctx <- tryCatch(sess_find(APP_DIR, sid_chk), error = function(e) NULL)
      if (!is.null(s_ctx)) {
        if (!nzchar(as.character(b$species %||% "")))
          b$species <- as.character(s_ctx$species %||% "")
        if (!nzchar(as.character(b$tissue %||% "")))
          b$tissue <- as.character(s_ctx$tissue %||% "")
      }
    }
    chain_check <- rc_validate(b)
    if (!is.null(chain_check)) return(send_json(con, chain_check$code, chain_check$body))
    if (!DB_READY) return(send_json(con, 503, list(ok = FALSE, error = "日志数据库不可用，未接收回写")))
    if (!db_run_log_authorized(DB_CON, owner_ctx_key(owner_ctx), sid_chk)) {
      # 执行端回写：它不是人，拿不到平台身份。
      #   任务本来就是这个应用自己创建、自己下发的，执行端手里有那条任务的 id ——
      #   用它证明"我就是你派出去的那个执行端"，并只允许写回那条任务所属的记录。
      #   以前这里一律 403：作业在服务器上真跑完了，页面却永远停在"已交接"，
      #   日志里只有一行 run_log_not_owner，人看不出是身份问题。
      exid <- trimws(as.character(b$exec_id %||% ""))
      ok_exec <- FALSE
      if (nzchar(exid)) {
        hit <- Filter(function(it) identical(as.character(it$id %||% ""), exid),
                      exec_read()$items %||% list())
        if (length(hit)) {
          it0 <- hit[[1]]
          st0 <- as.character(it0$status %||% "")
          ok_base <- identical(as.character(it0$session_id %||% ""), sid_chk) &&
            identical(as.character(it0$step %||% ""), sid_step)
          ok_exec <- ok_base && st0 %in% c("running", "queued")
          if (!ok_exec && ok_base && st0 %in% c("done", "failed")) {
            # 终态任务的再次回写：只放行「这条任务仍是该步最新一条」的情形。
            #   为什么原来一律拒绝是错的：作业真跑完了、判定却因判据缺陷拒了，
            #   exec 随即被记成 failed，于是判据修好之后同一个 exec 再也交不进来 ——
            #   只能整步重跑，白烧几小时集群。实测就是这么卡住的。
            #   放行不等于放宽：仍要 session/step 一致、无后继任务、
            #   run_dir 与库里一致，三项缺一即拒。
            lat0 <- tryCatch(exec_latest_for(sid_step, sid_chk), error = function(e) NULL)
            vin0 <- tryCatch(db_api_version_of(DB_CON, sid_chk), error = function(e) NULL)
            rd_db <- trimws(as.character((vin0 %||% list())$run_dir %||% ""))
            rd_in <- trimws(as.character(b$run_dir %||% ""))
            ok_exec <- !is.null(lat0) &&
              identical(as.character(lat0$id %||% ""), exid) &&
              (!nzchar(rd_db) || !nzchar(rd_in) || identical(rd_db, rd_in))
          }
          if (ok_exec) {
            own <- tryCatch(db_scalar(DB_CON,
              "SELECT owner_key FROM sessions WHERE session_id=?", list(sid_chk)),
              error = function(e) NA)
            if (length(own) == 1L && !is.na(own) && nzchar(as.character(own))) {
              owner_ctx <- list(key = as.character(own), kind = "exec_task", external_id = exid)
            } else {
              ok_exec <- FALSE
            }
          }
        }
      }
      if (!ok_exec)
        return(send_json(con, 403, list(ok = FALSE, code = "run_log_not_owner", error = "回写身份不属于这条分析记录")))
    }
    prev <- if (attempt_selected(APP_DIR, sid_chk, b$req_id)) remote_step_get(sid_step, sid = sid_chk) else attempt_results(APP_DIR, sid_chk, b$req_id)$steps[[sid_step]]

    # ---- 本题次绑定：结论性回写必须对得上这一次请求 ------------------
    #  为什么必须拦（这条是硬规则，不做"大概对"的容忍）：
    #    以前回写既不校验请求号，也不看产物时间。结果是「上一轮已经跑完的产物」
    #    可以被当成这一轮的结果写进应用 —— 页面显示已完成，磁盘上却是旧文件。
    #    这类错误写入比"没跑"更危险：数字齐全、时间齐全，只是不是这一次的，
    #    看页面根本看不出来。
    #  规则：
    #    1. done / failed 这类结论性回写，已经是 done 的步骤不接受无请求号的覆盖；
    #    2. 带请求号就必须等于这条分析记录当前的待回写请求号，否则视为别的题次；
    #    3. 产物清单必须逐项带 mtime，且不早于本题次开始时间。
    norm_ts <- function(x) {
      s <- trimws(as.character(x %||% ""))
      if (!nzchar(s)) return("")
      # 纯数字一律按 epoch 秒解读（2026-11-18 修）。
      #   执行端上报产物时间给的是 mtime_epoch（Unix 秒），原来只做字符串截断，
      #   于是 "1795008558" 被整串拿去和 "2026-11-18T21:01:06" 比大小：
      #   首字符 '1' < '2'，刚跑完的产物被判成"早于本题次开始"，merge 因此被拒。
      #   先换算成同一格式的本地时间串再比；跨机时钟不必一致，但格式必须一致。
      if (grepl("^[0-9]+$", s)) {
        ep <- suppressWarnings(as.numeric(s))
        if (!is.na(ep) && ep > 0)
          return(format(as.POSIXct(ep, origin = "1970-01-01", tz = "Asia/Shanghai"),
                        "%Y-%m-%dT%H:%M:%S"))
      }
      s <- gsub(" ", "T", s, fixed = TRUE)
      if (nchar(s) > 19L) s <- substr(s, 1L, 19L)
      s
    }
    conclusive <- stv %in% c("done", "failed")

    # ---- 先确认这条分析记录真的在 ----
    #   为什么必须拦在写盘之前：remote_steps.json 是全局表，sessions.json 才
    #   是页面真正读的那一份。记录不存在时，以前只有全局表被写成功，
    #   记录写失败被下面那个 tryCatch 吞成 TRUE，接口照样回 ok:true ——
    #   调用方据此向人宣称"已回写"，而页面上什么也没有。
    #   现在：记录不存在就直接 409，一个字节都不写。
    sid_req <- trimws(as.character(b$session_id %||% ""))
    if (nzchar(sid_req) && HAS_SESSIONS &&
        is.null(tryCatch(sess_find(APP_DIR, sid_req), error = function(e) NULL))) {
      known <- tryCatch({
        ids <- vapply(sess_read(APP_DIR)$sessions,
                      function(s) as.character(s$id %||% ""), "")
        as.list(head(ids[nzchar(ids)], 12))
      }, error = function(e) list())
      return(send_json(con, 409, list(
        ok = FALSE, step = sid_step, code = "session_record_missing",
        error = paste0("回写没有落到任何分析记录上：", sid_req, " 不在这台机器的分析记录里。"),
        hint = paste0("用应用里当前那条分析记录的 id 重新回写（GET /api/sessions 可查 current）。",
                      "不要向人宣称已回写。"),
        got = sid_req, known_sessions = known)))
    }
    pending0 <- if (HAS_SESSIONS && nzchar(sid_chk))
      tryCatch(sess_get_pending(APP_DIR, sid_chk, slot = "step"), error = function(e) NULL) else NULL
    pending0 <- as.character(pending0 %||% "")
    rq0 <- trimws(as.character(b$req_id %||% ""))

    if (conclusive) {
      if (!nzchar(rq0) && !is.null(prev) &&
          identical(as.character(prev$status %||% ""), "done")) {
        return(send_json(con, 409, list(
          ok = FALSE, step = sid_step, code = "req_id_required",
          error = paste0("这一步已经是完成状态。没有请求号的回写一律不接收 —— ",
                         "无法证明这份结果属于这一次运行，正是这一点会让旧结果悄悄盖掉新的。"),
          hint = "带上本次下发指令里的 req_id 再回写。")))
      }
      if (nzchar(rq0)) {
        # 带了请求号，就必须能在"当前待回写"里对上。
        #   对不上有两种：不是这一条（换了号）、或者压根没人被派过活。
        #   两种都说明这份结果不属于正在进行的题次，一律不写。
        if (!attempt_has(APP_DIR, sid_chk, rq0))
          return(send_json(con, 409, list(ok = FALSE, code = "req_id_mismatch", got = rq0)))
      }
    }

    # 产物：必须是对象数组并带 mtime。只给路径的写入一律不接收 ——
    #   没有时间就无法回答"这是这次跑出来的，还是上次留下的"。
    if (!is.null(b$artifacts) && length(b$artifacts)) {
      arts <- b$artifacts
      plain <- is.character(arts) && is.null(names(arts))
      if (plain) {
        return(send_json(con, 409, list(
          ok = FALSE, step = sid_step, code = "artifact_mtime_required",
          error = "产物清单必须逐项带 mtime（形如 [{rel, mtime, bytes}]）。只给路径无法判断是不是这一次产生的。",
          example = '[{"rel":"1.doublet/18m-G489.rds","mtime":"2026-10-22T21:56:00","bytes":126233168}]')))
      }
      start0 <- norm_ts(b$started_at %||% (prev$started_at %||% ""))
      bad <- list()
      # pack 的 ZIP 不看跨机时间，改看"是不是这次那个作业产生的"。
      #   作业号从服务端自己的 exec 记录里取，不采信回写自报的那一份。
      execJob <- ""
      execJobNum <- ""
      if (identical(sid_step, "pack")) {
        exid0 <- trimws(as.character(b$exec_id %||% ""))
        if (nzchar(exid0)) {
          d0 <- tryCatch(exec_read(), error = function(e) list(items = list()))
          for (it0 in (d0$items %||% list())) {
            if (identical(as.character(it0$id %||% ""), exid0)) {
              execJob <- as.character(it0$job_id %||% "")
              execJobNum <- as.character(it0$job_number %||% "")
              break
            }
          }
        }
      }
      pbFail <- list()
      for (a in arts) {
        if (is.character(a) && length(a) == 1L) {
          bad[[length(bad) + 1L]] <- list(rel = as.character(a), mtime = "", reason = "缺少 mtime")
          next
        }
        m <- norm_ts(a$mtime %||% a$modified %||% "")
        rel <- as.character(a$rel %||% a$path %||% "")
        if (!nzchar(m)) {
          bad[[length(bad) + 1L]] <- list(rel = rel, mtime = "", reason = "缺少 mtime")
        } else if (isTRUE(HAS_PACK_BINDING) && identical(sid_step, "pack") &&
                   identical(rel, PACK_ZIP_REL)) {
          # pack 的打包产物：只认"本次作业 + 节点实算事实"这条链。
          #   走这里就不再比跨机时间 —— 两台的时钟差会让刚打好的包被当成旧的。
          pb <- tryCatch(
            pack_zip_binding(rel, a, b$sge_job, b$pack_zip, execJob, sid_step,
                             exec_jobnum = execJobNum, zip_structure = b$zip_structure),
            error = function(e) list(ok = FALSE, code = "binding_error",
                                     why = conditionMessage(e)))
          if (!isTRUE(pb$ok)) {
            pbFail[[length(pbFail) + 1L]] <- list(
              rel = rel, mtime = m, code = as.character(pb$code %||% ""),
              reason = as.character(pb$why %||% ""),
              exec_job = execJob)
          }
        } else if (nzchar(start0) && m < start0) {
          am <- tolower(as.character(a$md5 %||% a$checksum %||% ""))
          # 只有 pack 复用 PPT 来源清单这一种情形允许早于本题次开始；
          #   本步骤旧产物、无关旧文件、跨 session、旧 PPT 版本都仍然拒。
          ok_up <- isTRUE(HAS_FRESHNESS) && isTRUE(tryCatch(
            freshness_manifest_exempt(rel, am, sid_step, sid_chk, DB_CON),
            error = function(e) FALSE))
          if (!ok_up)
            bad[[length(bad) + 1L]] <- list(rel = rel, mtime = m,
                                            reason = paste0("早于本题次开始时间 ", start0))
        }
      }
      if (length(pbFail)) {
        return(send_json(con, 409, list(
          ok = FALSE, step = sid_step, code = "pack_binding_failed",
          error = "这份打包产物没能绑定到本次作业，不能当成这次的结果写入。",
          rejected = pbFail)))
      }
      if (length(bad)) {
        return(send_json(con, 409, list(
          ok = FALSE, step = sid_step, code = "stale_artifact",
          error = "有产物产生于本题次开始之前（或没给时间），不能当成这次的结果写入。",
          rejected = bad)))
      }
    }

    rec <- list(
      title = (STEP_TASK[[sid_step]] %||% list(title = sid_step))$title,
      status = stv,
      code = if (is.null(code)) NULL else code,
      log = as.character(b$log %||% ""),
      run_dir = as.character(b$run_dir %||% ""),
      source_host = as.character(b$host %||% ""),
      exec_pc = as.character(b$exec_pc %||% ""),
      imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      # 这里必须用本作用域里的 sid_chk（本次回写所属的分析记录）。
      #   原来写的是 rev_sid —— 那是 /api/review/import 里的变量，在这个作用域里不存在，
      #   结果每一次 /api/step/import 都在构造记录时抛 "object 'rev_sid' not found"，
      #   统一 500。页面因此永远收不到进度，看着像"助手没回写"。
      session_id = if (nzchar(sid_chk)) sid_chk else NULL)
    if (!is.null(b$artifacts)) rec$artifacts <- b$artifacts
    # 执行端带回来的依据（逐簇统计等）。
    #   为什么要在这里接：worker 已经在回写体里发了 evidence，
    #   但这一步的记录只挑了 artifacts 等字段，evidence 被静默丢掉 ——
    #   于是 markers 虽然 verified，下一步筛群却拿不到真实簇号可展示、可校验。
    #   只在带了 session 且与本次分析一致时收：依据必须属于这条分析记录，
    #   否则就成了拿别人的簇表来给这一次做决定。
    if (!is.null(b$evidence) && is.list(b$evidence) && nzchar(sid_chk)) {
      ev0 <- b$evidence
      if (is.list(ev0$cluster1_evidence)) {
        ce <- ev0$cluster1_evidence
        # 准入判断抽在 engine/evidence_check.R，便于用真实用例逐条验证。
        #   任何一项不符就不写进记录：宁可没有依据，也不能让人看着
        #   一份来路不明的簇表去做删除决定。
        vr <- tryCatch(
          # 这里的 step 要用本路由真实变量 sid_step。
          #   原来写的是 stepId —— 那是别的路由里的变量，在这个作用域不存在，
          #   于是每次都抛 object 'stepId' not found，被兜底 catch 成
          #   「校验异常」，逐簇依据一条都存不进去。实测就是这么丢的。
          evidence_check_cluster1(ce, step_id = as.character(sid_step),
                                  sid = as.character(sid_chk),
                                  artifacts = (b$artifacts %||% list())),
          error = function(e) list(ok = FALSE, why = paste0("校验异常：",
                                                            conditionMessage(e))))
        if (isTRUE(vr$ok)) {
          vce <- vr$evidence
          vce$session_id <- sid_chk
          ev0$cluster1_evidence <- vce
        } else {
          ev0$cluster1_evidence <- NULL
          # 留一条拒绝原因，事后能查为什么这一步没有依据可展示。
          ev0$cluster1_evidence_rejected <- list(
            at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            session_id = sid_chk,
            claimed_rel = as.character(ce$rel %||% ce$source_table %||% ""),
            claimed_md5 = as.character(ce$md5 %||% ce$source_md5 %||% ""),
            why = as.list(as.character(vr$why %||% "未说明")))
        }
      }
      if (length(ev0)) rec$evidence <- ev0
    }
    # 时间戳：页面上「这一环节花了多久」全靠这两个字段。
    #   执行端给了就用它给的（那是服务器上的真实时间）；
    #   没给就记下这次回写的时刻 —— 至少能算「交出去到现在过了多久」。
    if (!is.null(b$started_at)) rec$started_at <- as.character(b$started_at)
    if (!is.null(b$ended_at))   rec$ended_at   <- as.character(b$ended_at)
    if (identical(stv, "running") && is.null(rec$started_at))
      rec$started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    if (stv %in% c("done", "failed", "stopped") && is.null(rec$ended_at))
      rec$ended_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    # 环节级进度：sub 存在就是「某个环节」的进展，不是整步。
    #   不能拿它覆盖整步状态 —— 拿 merge 的 running 盖掉整步的 done，
    #   页面会把已经跑完的一步重新画成「正在跑」。
    rec$subs <- if (!is.null(prev) && !is.null(prev$subs)) prev$subs else list()
    sub_id <- as.character(b$sub %||% "")
    if (nzchar(sub_id)) {
      keep <- Filter(function(x) !identical(as.character(x$id), sub_id), rec$subs)
      srec <- list(id = sub_id,
                   title = as.character(b$sub_title %||% sub_id),
                   status = as.character(b$sub_status %||% stv),
                   note = as.character(b$log %||% ""),
                   updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      if (!is.null(b$started_at)) srec$started_at <- as.character(b$started_at)
      if (!is.null(b$ended_at))   srec$ended_at   <- as.character(b$ended_at)
      if (identical(srec$status, "running") && is.null(srec$started_at))
        srec$started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      if (srec$status %in% c("done", "failed") && is.null(srec$ended_at))
        srec$ended_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      rec$subs <- c(keep, list(srec))
      # 环节回写怎么影响整步状态：分三种情况，别一律不动。
      #   原来一律沿用 prev$status，于是出现过一个死结：
      #   17:23 整步被记成 failed（缺词典），之后程序真的跑完了、
      #   21:29 按环节报 done，但整步仍然是 failed —— 因为"环节不改整步"。
      #   人看到的是一步明明跑完了却标着失败，进度也算不进去（5/10 就是这么来的）。
      #   现在：
      #     ① 所有环节都 done → 整步 done（机器能自己判的事，不该让人再点一次）
      #     ② 有环节 failed    → 整步 failed
      #     ③ 其余（running / 部分完成）→ 沿用 prev，不乱改
      subAll <- vapply(rec$subs %||% list(),
                       function(x) as.character(x$status %||% ""), "")
      subAnyFail <- any(subAll == "failed")
      subAllDone <- length(subAll) > 0 && all(subAll == "done")
      if (subAnyFail) {
        rec$status <- "failed"
      } else if (subAllDone) {
        rec$status <- "done"
        if (is.null(rec$ended_at)) rec$ended_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      } else if (!is.null(prev) && nzchar(as.character(prev$status %||% ""))) {
        rec$status <- as.character(prev$status)
      }
      if (is.null(prev) && identical(stv, "done")) rec$status <- "running"
      if (!is.null(prev) && !is.null(prev$started_at) && is.null(rec$started_at))
        rec$started_at <- prev$started_at
    }
    rec$req_id <- b$req_id
    # 这一 req 之前到底验收过没有。
    #   判据只能来自数据库：step_attempts 里同一 session、同一 req、同一 step
    #   且 status=verified 的行。terminal 里写着 done 不算证据 ——
    #   「申报 done」和「验收通过」是两件事：判定器拒绝时 terminal 一样写成 done，
    #   选中分支还可能在判定之前就短路返回。只看 terminal 会把失败当成功。
    verified_before <- function() {
      rq0 <- as.character(b$req_id %||% "")
      if (!nzchar(rq0)) return(list(ok = FALSE, why = "no_req_id"))
      if (!isTRUE(DB_READY)) return(list(ok = FALSE, why = "db_not_ready"))
      n <- tryCatch(
        db_scalar(DB_CON,
          paste0("SELECT COUNT(*) FROM step_attempts ",
                 "WHERE req_id = ? AND step_id = ? AND status = 'verified' ",
                 "AND json_extract(payload, '$.session_id') = ?"),
          list(rq0, sid_step, sid_chk)),
        error = function(e) {
          # 查询失败一律按未验收处理。宁可让执行端多等一轮，
          # 也不能因为查不到就当成已通过。
          try(message("[step/import] verified_before: ", conditionMessage(e)), silent = TRUE)
          NULL
        })
      if (is.null(n) || !length(n) || is.na(n)) return(list(ok = FALSE, why = "db_error"))
      list(ok = as.integer(n) > 0L,
           why = if (as.integer(n) > 0L) "verified" else "not_verified",
           n = as.integer(n))
    }
    prior_attempt <- attempt_results(APP_DIR, sid_chk, b$req_id)$steps[[sid_step]]
    if (sess_chr(prior_attempt$status) %in% c("done", "failed", "stopped")) {
      vb0 <- verified_before()
      # 两个条件都要成立：terminal 是 done（不是 failed/stopped），
      # 且数据库里有这一 req 的 verified 行。
      #   只看前者会把被判定器拒绝的 done 当通过；
      #   只看后者会在 failed terminal 上误判，因为失败路径不写 step_attempts。
      accepted_before <- isTRUE(vb0$ok) && identical(sess_chr(prior_attempt$status), "done")
      # ---- 终态短路时也要留拒绝凭据 --------------------------------
      #   实测踩到的坑：一旦某次申报把 attempt 写成 done，后面再回写会走这条短路，
      #   既不重判也不留痕 —— 于是被拒过的 done 永远显示成完成。
      #   这里按"库里有没有 verified 行"补写一份凭据，页面与下游据此显示未通过。
      if (identical(sess_chr(prior_attempt$status), "done") && !isTRUE(accepted_before)) {
        tryCatch({
          side <- file.path(attempt_dir(APP_DIR, sid_chk, b$req_id),
                            paste0("verdict_reject-", sid_step, ".json"))
          if (!file.exists(side)) {
            sess_write_json(side, list(
              session_id = sid_chk, req_id = b$req_id, step = sid_step,
              proposed_status = "done",
              code = "not_verified",
              reason = paste0("申报 done，但库里没有 verified 行（", as.character(vb0$why %||% ""), "）"),
              at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              rule = "终态短路时补记；申报保留在 attempt，本凭据只表示未通过判定"))
            Sys.chmod(side, "0600")
          }
        }, error = function(e) NULL)
      }
      # ---- 审计重验：终态 done 但库里没有 verified 行 ------------------
      #   这是一次「被拒过、还没解决」的申报。旧行为在这里直接短路返回，
      #   结果是判据修好之后同一个 exec 也交不进判定，只能整步重跑。
      #   这里只放行「同一条 req 的再次申报」，继续走判定；
      #   首次拒绝凭据原样保留，另存一份 recheck 记录说明被哪一次取代。
      # 上一轮失败、这一轮重跑成功，也必须能重判（2026-11-19 实测）：
      #   04_filter 首次因入口守卫失败，重跑成功后回写被当成「补录」，
      #   补录要求已有已验收清单作基线 —— 失败那一轮从没验收过，于是 no_baseline，
      #   结果是「作业真的算完了却永远写不回去」，只能整条链重来。
      #   失败/停止不是已验收终态，重跑成功属于重新申报，走正常判定路径。
      rejudge <- sess_chr(prior_attempt$status) %in% c("done", "failed", "stopped") &&
        !isTRUE(accepted_before) && identical(as.character(stv), "done")
      if (rejudge) {
        tryCatch({
          side0 <- file.path(attempt_dir(APP_DIR, sid_chk, b$req_id),
                             paste0("verdict_reject-", sid_step, ".json"))
          if (file.exists(side0)) {
            rj0 <- sess_read_json(side0)
            rs0 <- file.path(dirname(side0),
                             paste0("verdict_reject-", sid_step, ".recheck-",
                                    format(Sys.time(), "%Y%m%d-%H%M%S"), ".json"))
            rj0$superseded_by <- "audit_recheck"
            rj0$recheck_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
            rj0$recheck_rule <- paste0("判据修正后的同任务审计重验；",
                                       "首次拒绝凭据仍保留在同一目录")
            sess_write_json(rs0, rj0)
            Sys.chmod(rs0, "0600")
          }
        }, error = function(e) NULL)
      }
      # ---- 已验收同任务的「只补决策依据」窄通道 ----------------------
      #   背景：accepted 之后再发一次会走下面的 terminal_preserved 直接返回，
      #   新采到的逐簇依据永远落不进 remote_steps，下游筛群拿不到 29 簇。
      #   这里只允许一件事：补证据。产物、状态、DB 一律不动；
      #   只有「本次产物清单与已验收的一致（或本次不带产物清单）」
      #   且新证据通过准入校验时，才合并 cluster1_evidence。
      # ---- 补「逐样本计数 + 分组」的独立支路 ----
      #   背景：分组（orig.ident→group）只有集群侧 RDS 里有；filter 当初回写时
      #   没带，已验收记录里就缺这一项，final 决策卡会显示「分组未提供」。
      #   这里允许补，但只走这一条：同 req / 同 session / 同 run_dir、已完成、
      #   计数自洽、分组键对得上样本、产物不变、不与已有数值冲突。
      # 状态既可能是 done，也可能已经是 verified —— 只认 done 的话，
      #   已验收记录永远进不来这条支路，补证据会被静默跳过。
      if (isTRUE(accepted_before) &&
          sess_chr(prior_attempt$status) %in% c("done", "verified") &&
          is.list(b$evidence) && is.list(b$evidence$after_drop_stats) &&
          !is.list(b$evidence$cluster1_evidence)) {
        tryCatch(cat(paste0(format(Sys.time(), "%H:%M:%S"), "\tgroup_supp\t",
          "accepted=", as.character(isTRUE(accepted_before)),
          "\tprior=", sess_chr(prior_attempt$status),
          "\tev=", as.character(is.list(b$evidence)),
          "\tads=", as.character(is.list(b$evidence$after_drop_stats)),
          "\tce=", as.character(is.list(b$evidence$cluster1_evidence)), "\n"),
          file = file.path(STATE_DIR, "group_supp_debug.log"), append = TRUE),
          error = function(e) NULL)
        rec0 <- tryCatch(remote_step_get(sid_step, sid = sid_chk), error = function(e) NULL)
        mg0 <- if (exists("evidence_after_drop_check"))
          tryCatch(evidence_after_drop_check(rec0, b$req_id, b$run_dir, sid_chk, sid_step,
                                             b$evidence$after_drop_stats,
                                             new_artifacts = b$artifacts %||% list()),
                   error = function(e) list(code = "check_error", error = conditionMessage(e)))
        else list(code = "no_guard", error = "缺少 evidence_merge.R")
        tryCatch(cat(paste0("  mg0=", if (is.null(mg0)) "NULL" else as.character(mg0$code %||% "?"),
          " ", if (is.null(mg0)) "" else as.character(mg0$error %||% ""), "\n"),
          file = file.path(STATE_DIR, "group_supp_debug.log"), append = TRUE),
          error = function(e) NULL)
        if (is.null(mg0)) {
          ev0 <- rec0$evidence %||% list()
          ev0$after_drop_stats <- b$evidence$after_drop_stats
          rec0$evidence <- ev0
          rec0$evidence_supplemented_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
          wok0 <- tryCatch({ remote_step_put(sid_step, rec0, sid = sid_chk); TRUE },
                           error = function(e) FALSE)
          rb0 <- if (isTRUE(wok0)) tryCatch(remote_step_get(sid_step, sid = sid_chk),
                                            error = function(e) NULL) else NULL
          # 读回必须逐字段相等才报成功；只看「samples 非空」等于自欺。
          vf0 <- if (exists("evidence_after_drop_verify"))
            tryCatch(evidence_after_drop_verify(rb0, b$req_id, b$run_dir,
                                                b$evidence$after_drop_stats),
                     error = function(e) list(ok = FALSE, why = conditionMessage(e)))
          else list(ok = FALSE, why = "缺少 evidence_merge.R")
          if (!isTRUE(wok0) || !isTRUE(vf0$ok)) {
            # 显式拒绝：不落进下面的 accepted 成功分支，也不谎报 merged。
            return(send_json(con, 409, list(ok = FALSE, code = "evidence_not_persisted",
              error = paste0("分组证据未写入或读回不一致：",
                             if (!isTRUE(wok0)) "写回失败" else as.character(vf0$why %||% "")),
              accepted = accepted_before)))
          }
          adb <- (rb0$evidence %||% list())$after_drop_stats
          if (is.list(adb) && length(adb$samples %||% list())) {
            tryCatch({
              af0 <- file.path(STATE_DIR, "evidence_supplements.jsonl")
              cat(paste0(jsonlite::toJSON(list(
                at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                session_id = sid_chk, step = sid_step, req_id = b$req_id %||% "",
                kind = "after_drop_stats_supplement",
                n_samples = length(adb$samples %||% list()),
                n_groups = length(unique(unlist(adb$sample_group %||% list()))),
                cells_total = as.character(adb$cells_total %||% ""),
                note = "已验收同任务补分组证据；产物与状态未变"), auto_unbox = TRUE), "\n"),
                file = af0, append = TRUE)
            }, error = function(e) NULL)
            return(send_json(con, 200, list(ok = TRUE, persisted = TRUE,
              terminal_preserved = TRUE, evidence_merged = TRUE,
              merged_kind = "after_drop_stats", accepted = accepted_before,
              selected = attempt_selected(APP_DIR, sid_chk, b$req_id),
              readback = rb0)))
          }
        } else {
          # 准入没过：必须显式拒绝。
          #   原来这里没有 else，mg0 非空时会一路落到下面的终态短路分支，
          #   返回 accepted=TRUE + evidence_merge_skipped，等于把「拒绝」
          #   说成「已接受但没合并」—— 调用方看不出失败，也不会来修。
          return(send_json(con, 409, list(ok = FALSE,
            code = "evidence_not_admitted",
            reason = as.character(mg0$code %||% "unknown"),
            error = as.character(mg0$error %||% "补证据准入未通过"),
            accepted = accepted_before)))
        }
      }
      evMerged <- FALSE
      if (isTRUE(accepted_before) && identical(sess_chr(prior_attempt$status), "done") &&
          is.list(b$evidence) && is.list(b$evidence$cluster1_evidence)) {
        sigOf <- function(xs) {
          v <- vapply(xs, function(a) paste0(as.character(a$rel %||% ""), ":",
                       as.character(a$md5 %||% "")), "")
          sort(v)
        }
        newA <- b$artifacts %||% list()
        oldA <- prior_attempt$artifacts %||% list()
        # 判据：共同项 md5 必须一致，且旧清单里的每一项都还在；
        #   允许本次多出条目（例如把日志登记成产物）。
        #   为什么不能要求两端完全相等：补证据这一轮往往会附带新登记的日志，
        #   要求相等会把合法补充挡死；而只要共同项变了，就是换了产物，必须拒绝。
        kv <- function(xs) {
          v <- vapply(xs, function(a) as.character(a$md5 %||% ""), "")
          names(v) <- vapply(xs, function(a) as.character(a$rel %||% ""), "")
          v
        }
        nk <- kv(newA); ok_ <- kv(oldA)
        sameArt <- TRUE
        if (length(ok_)) {
          miss <- setdiff(names(ok_), names(nk))
          if (length(miss)) sameArt <- FALSE
          shared <- intersect(names(ok_), names(nk))
          if (length(shared) &&
              !identical(unname(ok_[shared]), unname(nk[shared]))) sameArt <- FALSE
        }
        if (sameArt) {
          ce2 <- b$evidence$cluster1_evidence
          vr2 <- tryCatch(
            evidence_check_cluster1(ce2, step_id = as.character(sid_step),
                                    sid = as.character(sid_chk),
                                    artifacts = if (length(newA)) newA else oldA),
            error = function(e) list(ok = FALSE, why = paste0("校验异常：",
                                                              conditionMessage(e))))
          if (isTRUE(vr2$ok)) {
            rec2 <- tryCatch(remote_step_get(sid_step, sid = sid_chk),
                             error = function(e) NULL)
            # 身份必须与本次一致：同会话重跑过时，旧 req 的记录不能拿来合并。
            mg <- if (exists("evidence_merge_check"))
              evidence_merge_check(rec2, b$req_id, b$run_dir, sid_chk, sid_step,
                                   as.character((vr2$evidence %||% list())$md5 %||% ""))
            else list(code = "no_guard", error = "缺少 evidence_merge.R")
            if (!is.null(mg)) {
              evMerged <- FALSE
              evWhy2 <- as.character(mg$error %||% mg$code %||% "准入未通过")
            } else {
              vce2 <- vr2$evidence
              vce2$session_id <- sid_chk
              ev2 <- rec2$evidence %||% list()
              ev2$cluster1_evidence <- vce2
              rec2$evidence <- ev2
              rec2$evidence_supplemented_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
              # 写回结果必须捕获并读回核对：
              #   吞掉错误还报 merged，等于假报 persisted。
              wok <- tryCatch({ remote_step_put(sid_step, rec2, sid = sid_chk); TRUE },
                              error = function(e) FALSE)
              rb2 <- if (isTRUE(wok)) tryCatch(remote_step_get(sid_step, sid = sid_chk),
                                               error = function(e) NULL) else NULL
              vf <- if (exists("evidence_merge_verify"))
                evidence_merge_verify(rb2, b$req_id, sid_step, sid_chk,
                                      as.character(vce2$md5 %||% ""))
              else list(ok = FALSE, why = "缺少 evidence_merge.R")
              if (!isTRUE(vf$ok)) {
                evMerged <- FALSE
                evWhy2 <- as.character(vf$why %||% "写回读回不一致")
              }
              # 审计：只追加，不改历史；不碰拒绝凭据、不碰 DB。
              tryCatch({
                af <- file.path(STATE_DIR, "evidence_supplements.jsonl")
                cat(paste0(jsonlite::toJSON(list(
                  at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                  session_id = sid_chk, step = sid_step, req_id = b$req_id %||% "",
                  kind = "cluster1_evidence_supplement",
                  n_clusters = length(vce2$clusters %||% list()),
                  table_md5 = as.character(vce2$md5 %||% ""),
                  note = "已验收同任务补证据；产物与状态未变"),
                  auto_unbox = TRUE), "
"), file = af, append = TRUE)
              }, error = function(e) NULL)
              if (isTRUE(vf$ok)) evMerged <- TRUE
            }
          } else {
            evMerged <- FALSE
            evWhy2 <- as.character(vr2$why %||% "未说明")
          }
        }
      }
      if (isTRUE(evMerged))
        return(send_json(con, 200, list(ok=TRUE, persisted=TRUE,
          terminal_preserved=TRUE, evidence_merged=TRUE,
          accepted=accepted_before,
          selected=attempt_selected(APP_DIR,sid_chk,b$req_id),
          readback=remote_step_get(sid_step, sid = sid_chk))))

      if (!rejudge) {
        # 终态已验收时也要把本次新登记的产物补进记录，否则页面上永远取不到
        #   final_params.json / final_execution.log 这类后补的条目。
        #   只补清单：状态、判定结论、拒绝凭据一律不动；
        #   旧项必须全在新清单里且 md5 相同，共同项 md5 变了就是换了产物，拒绝。
        suppP <- character(0)
        supp_note <- ""
        vpP <- tryCatch(verified_payload(), error = function(e) NULL)
        curRecP <- tryCatch(remote_step_get(sid_step, sid = sid_chk),
                            error = function(e) NULL)
        apP <- tryCatch(artifact_supplement_apply(
                 as.character(b$req_id %||% ""), b$artifacts %||% list(),
                 curRecP, vpP,
                 function(r0) remote_step_put(sid_step, r0, sid = sid_chk),
                 function() remote_step_get(sid_step, sid = sid_chk)),
               error = function(e) list(ok = FALSE, code = "apply_error",
                                        error = conditionMessage(e)))
        if (!isTRUE(apP$ok) && !identical(as.character(apP$code), "req_mismatch"))
          return(send_json(con, 409, list(ok = FALSE,
            code = as.character(apP$code %||% "supplement_rejected"),
            accepted = TRUE, terminal_preserved = TRUE,
            error = as.character(apP$error %||% "补录准入未通过"))))
        if (identical(as.character(apP$code), "req_mismatch"))
          supp_note <- "req_mismatch_skipped"
        if (isTRUE(apP$wrote)) {
          suppP <- as.character(apP$added %||% character(0))
          prior_attempt <- apP$record %||% prior_attempt
          tryCatch({
            afP <- file.path(STATE_DIR, "evidence_supplements.jsonl")
            cat(paste0(jsonlite::toJSON(list(
              at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              session_id = sid_chk, step = sid_step, req_id = b$req_id %||% "",
              kind = "artifact_list_supplement", added = as.list(suppP),
              note = "终态已验收；仅补产物清单，状态与判定结论未变"),
              auto_unbox = TRUE), "\n"), file = afP, append = TRUE)
          }, error = function(e) NULL)
        }
        return(send_json(con, 200, list(ok=TRUE,persisted=TRUE,terminal_preserved=TRUE,
          accepted=accepted_before,
          evidence_merge_skipped=if (exists("evWhy2")) evWhy2 else "not_attempted",
          accepted_reason=as.character(vb0$why %||% ""),
          artifacts_supplemented = length(suppP) > 0,
          supplement_note = supp_note,
          artifacts_added = as.list(suppP),
          selected=attempt_selected(APP_DIR,sid_chk,b$req_id),readback=prior_attempt)))
      }
    }
    if (!attempt_selected(APP_DIR, sid_chk, b$req_id)) {
      # The same payload, artifact checks and run-chain admission above apply.
      # Judge a claimed done before persistence, without publishing session caches.
      judge <- NULL
      if (identical(stv, "done") && !identical(sid_step, "selftest")) {
        judge <- tryCatch(attempt_judge_history(DB_CON, APP_DIR, owner_ctx, b,
          runDir = as.character(b$run_dir %||% ""), sid = sid_chk),
          error = function(e) list(accepted = FALSE, code = "db_report_error", error = conditionMessage(e)))
      }
      rb_attempt <- attempt_save(APP_DIR, sid_chk, b$req_id, "step", rec, sid_step)
      accepted <- is.null(judge) || isTRUE(judge$accepted)
      # ---- 被拒的 done 必须留下"拒绝凭据" ----------------------------
      #   为什么不能只回 409 就完事：attempt 里已经写着 done，
      #   页面读 attempt 就显示成完成，还能放行下游的 merge。
      #   实测就是这样：模拟只给 2 个产物的 done 被判定器拒了，
      #   接口回 409 accepted=false，可 /api/flow 照样显示 doublet done。
      #   这里不动 attempt（那是申报的原始事实），另存一份拒绝凭据；
      #   页面与下游前置审读它，把这一步显示成「完成申报未通过」，不计入完成。
      if (!accepted) {
        tryCatch({
          side <- file.path(attempt_dir(APP_DIR, sid_chk, b$req_id),
                            paste0("verdict_reject-", sid_step, ".json"))
          sess_write_json(side, list(
            session_id = sid_chk, req_id = b$req_id, step = sid_step,
            proposed_status = as.character(stv),
            code = as.character((judge %||% list())$code %||% ""),
            reason = as.character((judge %||% list())$error %||% ""),
            at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            rule = "申报保留在 attempt；本凭据只表示这次 done 未通过判定"))
          Sys.chmod(side, "0600")
        }, error = function(e) NULL)
      }
      # 判定通过就必须落库为 verified。
      #   这一段以前没有：判定器说 accepted=TRUE，attempt 却仍停在 delegated，
      #   数据库里没有 verified 行。下游取上游证据时（出图取注释的逐类型计数、
      #   未定型数、已定型类型集合）查 verified 永远查不到，
      #   于是「产物完全正确」的一步被判成整串 unknown/fail。
      #   实测：注释判定通过后，出图的守恒全部失败。这里补齐落库。
      if (isTRUE(accepted) && !is.null(judge)) {
        tryCatch({
          aid2 <- as.character(judge$attempt_id %||% "")
          if (nzchar(aid2)) {
            DBI::dbExecute(DB_CON,
              paste0("UPDATE step_attempts SET status='verified', verified_at=?, payload=? ",
                     "WHERE attempt_id=?"),
              list(db_now(), db_json(b), aid2))
          }
        }, error = function(e) NULL)
        # 同时把这一步的正式记录写进 remote_steps。
        #   为什么必须写：页面（/api/flow）读的是 remote_steps.json，不是库。
        #   这条分支（尝试未被选为当前有效）原来只 attempt_save，不写 remote_steps，
        #   于是库里 verified、页面上这一步仍是 delegated、产物空、日志空 ——
        #   实测：注释判定通过并落库后，页面上仍显示没有回写。
        #   记录内容直接由回写体构造，字段与正常路径保持一致。
        tryCatch({
          rec_pub <- list(
            title = (STEP_TASK[[sid_step]] %||% list(title = sid_step))$title,
            status = as.character(stv),
            code = if (is.null(code)) NULL else code,
            log = as.character(b$log %||% ""),
            run_dir = as.character(b$run_dir %||% ""),
            source_host = as.character(b$host %||% ""),
            exec_pc = as.character(b$exec_pc %||% ""),
            imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            session_id = sid_chk,
            artifacts = b$artifacts %||% list(),
            started_at = as.character(b$started_at %||% ""),
            ended_at = as.character(b$ended_at %||% ""),
            subs = b$subs %||% list(),
            req_id = as.character(b$req_id %||% ""))
          remote_step_put(sid_step, rec_pub, sid = sid_chk)
        }, error = function(e) NULL)
      }
      # Preserve run-chain bookkeeping; rc_validate above remains the admission gate.
      chain_result <- rc_callback(b, !is.null(judge) && isTRUE(judge$accepted))
      if (stv %in% c("done", "failed", "stopped")) attempt_release_output(APP_DIR, sid_chk, b$req_id)
      return(send_json(con, if (accepted) 200 else 409, list(ok = accepted,
        persisted = TRUE, selected = FALSE, session = sid_chk, session_id = sid_chk,
        req_id = b$req_id, step = sid_step, readback = rb_attempt, judge = judge, chain = chain_result,
        readback_url = paste0("/api/attempts/result?session_id=", sid_chk, "&req_id=", b$req_id))))
    }
    proposed_status <- rec$status
    rec <- attempt_save(APP_DIR, sid_chk, b$req_id, "step", rec, sid_step)
    if (!identical(rec$status, proposed_status) && sess_chr(rec$status) %in% c("done","failed","stopped")) {
      # 与上面那条短路同一个口径：terminal 是 done 且数据库里有 verified 行才算验收过。
      #   这里额外要防的是「判定还没跑就短路」——rec 刚被 attempt_save 写成终态，
      #   判定代码在下面 4075 之后，所以此刻 terminal 的 done 只是申报。
      vb1 <- verified_before()
      accepted_before1 <- isTRUE(vb1$ok) && identical(sess_chr(rec$status), "done")
      return(send_json(con, 200, list(ok=TRUE,persisted=TRUE,terminal_preserved=TRUE,
        accepted=accepted_before1,
        accepted_reason=as.character(vb1$why %||% ""),
        readback=rec)))
    }
    # Capture only after all callback validation; retries dedupe on the received body,
    # not generated reception timestamps. Missing req_id remains explicitly unknown.
    db_run_log_append(DB_CON, owner_ctx_key(owner_ctx), sid_chk, list(
      step_id = sid_step, sub_id = b$sub, req_id = b$req_id, status = stv,
      provenance = "callback", source = rec$source_host,
      source_at = b$ended_at %||% b$started_at, received_at = rec$imported_at,
      content = rec$log), identity = b)
    remote_step_put(sid_step, rec, sid = sid_chk)

    # ---- 数据层：done 只是「申报」，不是「通过」（I1 / L3 第一步）----
    #   这里是「完成」定义权转移的那一处改动。以前收到 done 就记成完成，
    #   页面上显示已完成、下游按钮亮起，至于产物对不对没人问过。
    #   现在：写 reported 到 step_attempts，同步跑四层判定，
    #   不通就落 rejected + 返工单，并把 fails 直接回给调用方。
    #   为什么同步判而不是排队判：排队的话调用方会以为「已经过了」，
    #   在判定结果出来之前就把下游开起来 —— 那等于没拦。
    db_report <- NULL
    # 自检步骤不进判定器。
    #   为什么：verdict_step 的四层判定是围绕"某个真实分析环节的产物"设计的
    #   （stage 在 flow_spec 里、要有产物清单、要能对 mtime）。selftest 只是一次
    #   链路探针 —— 它连服务器跑一条 hostname 就返回，既没有 flow_spec 里的
    #   stage，也不该产出任何分析产物。硬塞进判定器只会得到
    #   "flow_spec 里没有这个环节"，于是 409，而这跟链路通不通毫无关系。
    #   判定器要拦的是"假完成"，探针不在此列，所以这里显式跳过，并留一条
    #   accepted=true 的回执，让调用方按正常成功处理。
    is_probe <- identical(sid_step, "selftest")
    # failed 不进判定器。
    #   踩过的坑（2026-11-06 16:07）：执行端明确报"脚本不存在"这类失败时，
    #   原来也走四层判定 —— 判定器按"这一环该有的产物一个都没有"给出 fail，
    #   于是 /api/step/import 返回 409。执行端看到 409 就当自己没写进去，
    #   而应用里那一步还挂着"已交给助手"。人回来看到的是没有下文的等待，
    #   真正的原因（脚本不存在）被判定结果盖掉了。
    #   判定器的职责是核"声称完成的东西是否真完成"，不是审判"失败"本身。
    #   失败是执行端的如实上报，应当落库、落日志、能被看见。
    if (is_probe && stv %in% c("done", "failed")) {
      db_report <- list(accepted = TRUE, verdict = "probe", attempt_id = NULL,
                        ticket_id = NULL, status = stv, failed_checks = list())
    } else if (identical(stv, "failed")) {
      db_report <- list(accepted = TRUE, verdict = "failed", attempt_id = NULL,
                        ticket_id = NULL, status = "failed",
                        failed_checks = list(list(
                          layer = "R0", name = "runner_reported_failure",
                          detail = substr(as.character(b$log %||% ""), 1, 300))))
    } else if (DB_READY && stv %in% c("done", "failed")) {
      db_report <- tryCatch(
        db_api_report_step(DB_CON, APP_DIR, owner_ctx, b,
                           runDir = as.character(b$run_dir %||% ""),
                           sid = sid_chk),
        error = function(e) list(accepted = FALSE, code = "db_report_error",
                                 error = conditionMessage(e)))
    }

    # ---- 脚本自动入库 --------------------------------------------------
    #   gina 2026-11-19：「我需要你把脚本自动存进脚本库，这个功能要做好」。
    #   为什么放在这个位置：/api/step/import 是每一次环节回写唯一必经的点，
    #   判定结果也刚好在这里出来 —— 有判定才知道这一次算成功还是退回。
    #   只登记实际跑过、且带 fingerprint 的脚本；探针步骤不登记。
    #   内容只在能证明字节一致时才收（script_archive.R 里的第一条规则），
    #   拿不到就如实标 content_missing，不伪造一份副本。
    if (HAS_SCRIPT_ARCHIVE && nzchar(sid_step) && !is_probe &&
        stv %in% c("done", "failed")) {
      tryCatch({
        s0a <- if (HAS_SESSIONS) tryCatch(sess_find(APP_DIR, sid_chk), error = function(e) NULL) else NULL
        sa_register(
          APP_DIR, ENGINE_DIR,
          sid = sid_chk,
          session_name = as.character((s0a %||% list())$name %||% ""),
          run_dir = as.character((s0a %||% list())$run_dir %||% b$run_dir %||% ""),
          step = sid_step,
          script_path = as.character(b$script_path %||% b$script %||% ""),
          script_md5 = as.character(b$script_md5 %||% b$script_digest %||% ""),
          attempt_id = as.character((db_report %||% list())$attempt_id %||% b$req_id %||% ""),
          # 归属：脚本库要按使用者隔离，登记时就要带上，否则后续无法过滤。
          owner_key = as.character(owner_ctx_key(owner_ctx) %||% ""),
          ok = isTRUE((db_report %||% list())$accepted) || identical(stv, "done"))
      }, error = function(e) {
        # 入库失败不能把一次已经合格的验收变成失败：记一条事件，人能从状态里看到。
        tryCatch(db_event(DB_CON, "script_archive_error",
                          owner_key = owner_ctx_key(owner_ctx),
                          step_id = sid_step,
                          payload = list(why = conditionMessage(e))),
                 error = function(e2) NULL)
      })
    }

    # ---- 应用侧质检 + 台账写入 ----
    #   为什么放在这里：/api/step/import 是这个 handler 里唯一必经的点。
    #   之前放在 envcheck 的回写块里，那段永远不会被这一步执行到 ——
    #   实测表现是"回写成功、但质检记录和台账都没有"，看起来像功能没做，
    #   其实是写在了到不了的代码里。
    qsid2 <- sid_chk
    if (!nzchar(qsid2)) qsid2 <- "_default"
    qDone2 <- identical(as.character(rec$status %||% stv), "done")
    qRels2 <- character(0)
    for (a in (rec$artifacts %||% list())) {
      r0 <- if (is.list(a)) as.character(a$rel %||% "") else as.character(a)
      if (nzchar(r0)) qRels2 <- c(qRels2, r0)
    }
    qLog2 <- as.character(b$log %||% "")

    if (isTRUE(HAS_QUALITY)) {
      tryCatch({
        checks2 <- list(
          list(name = "状态", ok = qDone2,
               detail = paste0("回写状态=", as.character(rec$status %||% stv))),
          list(name = "有产物", ok = if (!qDone2) NA else length(qRels2) > 0,
               detail = if (length(qRels2)) paste0("产物 ", length(qRels2), " 项") else "没有产物清单"),
          list(name = "运行目录", ok = if (!qDone2) NA else nzchar(as.character(b$run_dir %||% "")),
               detail = if (nzchar(as.character(b$run_dir %||% "")))
                 as.character(b$run_dir) else "未回写运行目录"),
          list(name = "日志无失败字样",
               ok = if (!nzchar(qLog2)) NA else
                 !grepl("Execution halted|Error:|CHECK_FAIL|No cells found", qLog2),
               detail = if (grepl("Execution halted|Error:|CHECK_FAIL|No cells found", qLog2))
                 "日志里出现失败字样" else "未见失败字样"))
        qual_make(qsid2, sid_step, checks = checks2, artifacts = qRels2,
                  cells = b$cells %||% NULL,
                  note = paste0("应用侧自动生成；回写于 ", format(Sys.time(), "%H:%M:%S")))
      }, error = function(e) NULL)
    }

    # ---- 环节后置审：回写之后立刻出护照（2026-11-05）------------------
    #   为什么放在这里：/api/step/import 是唯一必经的回写点，
    #   放在别处就会漏掉某条路（envcheck 那次就是这么漏的）。
    #   这一份护照是"这一环的产物能不能交给下一环用"的唯一凭据：
    #     下一环的前置审只认上游护照里的 released 清单，
    #     不再看磁盘上恰好有什么文件。
    #   为什么不再另写一套判据：L1-L4 直接取 verdict 的结论（同一次回写，
    #   同一批事实），这里只补"放行清单"与"回写契约"两件它不管的事。
    srPass <- NULL
    srWarn <- ""
    if (isTRUE(HAS_STAGE_REVIEWER) &&
        stv %in% c("done", "failed") &&
        !identical(as.character(sid_step), "all") &&
        !identical(as.character(sid_step), "config")) {
      srPass <- tryCatch(
        sr_admit(APP_DIR, sid_step, sid = qsid2, wb = b, verdict = db_report,
                 set_id = (db_report %||% list())$set_id,
                 set_id_provenance = (db_report %||% list())$set_id_provenance,
                 attempt_id = (db_report %||% list())$attempt_id %||% NULL,
                 # 把开跑前那份许可号带进护照：这样"审过才跑"这条能事后举证，
                 #   而不是只能相信"当时审了"。
                 permit_id = {
                   lp <- tryCatch(sr_preflight_last(APP_DIR, qsid2, sid_step),
                                  error = function(e) NULL)
                   as.character((lp %||% list())$permit_id %||% "") %||% NULL
                 },
                 profile = tryCatch(flow_profile(qsid2), error = function(e) "server")),
        error = function(e) list(verdict = "warn", error = conditionMessage(e)))
      # 护照结论回灌给旧的质检账，保证页面上的两处结论不会互相打架。
      #   为什么不删掉 quality：页面已有读它的地方，改了会一起坏。
      #   现在的分工：护照是判据，quality 是它的一个视图。
      if (isTRUE(HAS_QUALITY) && !is.null(srPass) && !is.null(srPass$verdict)) {
        pv <- as.character(srPass$verdict)
        tryCatch(qual_make(qsid2, sid_step,
          checks = lapply(srPass$checks %||% list(), function(c0) list(
            name = paste0(as.character(c0$layer %||% ""), "/", as.character(c0$name %||% "")),
            # 护照的 block 对应质检的失败；warn 保留为"判不了/需留意"，不折算成通过。
            ok = if (identical(as.character(c0$status), "pass") ||
                     identical(as.character(c0$status), "skipped")) TRUE
                 else if (identical(as.character(c0$status), "fail")) FALSE else NA,
            detail = as.character(c0$detail %||% ""))),
          artifacts = srPass$released %||% list(),
          note = paste0("环节审计：", pv, "（", srPass$passport_id %||% "", "）")),
          error = function(e) NULL)
      }
      if (identical(as.character(srPass$verdict %||% ""), "block")) {
        srWarn <- paste0("这一环的产物未放行（护照 ", srPass$passport_id %||% "",
                         "）：", paste(vapply(srPass$blockers %||% list(),
                                             function(x) as.character(x$detail %||% x$name %||% ""), ""),
                                      collapse = "；"))
      }
    }

    if (isTRUE(HAS_LEDGER_OUT)) {
      # 台账 finish 行：reviewer.R 要求每阶段既有 start 又有 finish，且带退出码。
      #   done 记 0；failed 记实际退出码，没给码记 1（没给码的失败不能当成功）。
      tryCatch(led_out_push(
        sid = qsid2, step = sid_step, kind = "finish",
        run_dir = as.character(b$run_dir %||% ""),
        code = if (qDone2) 0L else { cc <- b$code; if (is.null(cc)) 1L else as.integer(cc) },
        artifacts = qRels2,
        note = paste0("回写状态=", as.character(rec$status %||% stv))),
        error = function(e) NULL)
    }
    # 挂到这一次分析记录上：切到别的记录时不会看到这一份的进度。
    sess_saved <- NULL
    sess_why <- ""
    if (HAS_SESSIONS) {
      sid <- sid_chk
      if (nzchar(sid)) {
        # sess_set_step 失败时返回的是 list(ok = FALSE)，不是抛异常。
        #   原来这里写成 { sess_set_step(...); TRUE }，等于把返回值丢掉、
        #   无条件记成成功 —— 记录没写进去，接口却回 ok:true，
        #   调用方于是宣称"已回写"。这是可能导致误报的路径，不能据此断定具体事故原因。
        sr <- tryCatch(sess_set_step(APP_DIR, sid, sid_step, rec),
                       error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        if (isTRUE(sr$ok)) sess_saved <- sid
        else sess_why <- as.character(sr$error %||% "写入分析记录失败")
      } else sess_why <- "没有当前分析记录"
    }
    # 返回的是「这一步实际被记成的状态」，不是这次请求里那个字段。
    #   环节级回写（sub）不该把整步状态改成 done —— 那会让执行端以为整步跑完了。
    # 数据层的判定结果一并回给调用方：
    #   accepted=false 时调用方必须知道「没通过」，不能只看到 ok=true 就走下去。
    resp <- list(ok = TRUE, step = sid_step,
                 status = as.character(rec$status %||% stv),
                 sub = if (nzchar(sub_id)) sub_id else NULL,
                 session = sess_saved,
                 session_saved = !is.null(sess_saved))

    # ---- 落盘核对：读不回来就不许说"回写成功" ----
    #   为什么要有这一步：这一句返回值是调用方宣称"已回写"的唯一凭据。
    #   只要它还会在没写进去的时候回 ok:true，就还会出现
    #   「任务说完成了、回写说做了、页面什么都没有」。
    #   判据不用内存里的 rec（那是我们打算写的），而是把文件重新读一遍
    #   （那是真的写进去了的），两边对得上才算数。
    rb <- tryCatch({
      x <- remote_steps_snapshot()
      hit <- Filter(function(s)
        identical(as.character(s$id %||% ""), sid_step) &&
          identical(as.character(s$session_id %||% ""),
                    as.character(if (nzchar(sid_chk)) sid_chk else "")),
        x$steps %||% list())
      if (length(hit)) hit[[length(hit)]] else NULL
    }, error = function(e) NULL)
    expected_rb <- rec
    expected_rb$id <- sid_step
    if (nzchar(sid_chk)) expected_rb$session_id <- sid_chk
    if (!is.null(rb) && !isTRUE(all.equal(rb, expected_rb, check.attributes = FALSE))) rb <- NULL
    resp$persisted <- !is.null(rb)
    resp$persisted_status <- if (is.null(rb)) NULL else as.character(rb$status %||% "")
    resp$persisted_at <- if (is.null(rb)) NULL else (rb$imported_at %||% NULL)
    if (is.null(rb)) {
      resp$ok <- FALSE
      resp$code <- "writeback_not_persisted"
      resp$error <- "回写没有落盘：重读 remote_steps.json 找不到这条记录。不要宣称已回写。"
      return(send_json(con, 500, resp))
    }
    # 记录没写成功时也要说清：全局表写了、分析记录没写，页面照样看不到。
    if (HAS_SESSIONS && is.null(sess_saved) && nzchar(sid_chk)) {
      resp$ok <- FALSE
      resp$code <- "session_step_not_saved"
      resp$error <- paste0("回写没有挂到分析记录 ", sid_chk, " 上：",
                           if (nzchar(sess_why)) sess_why else "写入失败",
                           "。页面读的是记录，所以看不到这次回写。")
      return(send_json(con, 500, resp))
    }
    if (!is.null(db_report)) {
      resp$accepted <- isTRUE(db_report$accepted)
      resp$verdict <- db_report$verdict %||% NULL
      resp$attempt_id <- db_report$attempt_id %||% NULL
      resp$ticket_id <- db_report$ticket_id %||% NULL
      resp$duplicate <- db_report$duplicate %||% NULL
      resp$failed_checks <- db_report$failed_checks %||% NULL
      if (!is.null(db_report$error)) resp$judge_error <- db_report$error
      if (!is.null(db_report$code)) resp$judge_code <- db_report$code
      # 未通过时把 http 码也改成 409：让「脚本按退出码判断」的调用方
      #   也不会误以为这轮回写成功了。
      if (isFALSE(db_report$accepted)) {
        # ---- 首轮拒绝：先落凭据，再回 409 ---------------------------
        #   为什么必须在这里写：这是"首次、已选中"的正常路径。
        #   上面两处短路只覆盖重复回调与未选中情形；首轮拒绝不落凭据的话，
        #   attempt 里的 done 会让 flow 继续显示成完成，下游 merge 也就跟着放行。
        #   实测过：只给两份产物的首次 done，接口回了 409，
        #   但 flow 依然是 done —— 正是缺了这一处。
        tryCatch({
          side <- file.path(attempt_dir(APP_DIR, sid_chk, b$req_id),
                            paste0("verdict_reject-", sid_step, ".json"))
          sess_write_json(side, list(
            session_id = sid_chk, req_id = b$req_id, step = sid_step,
            proposed_status = as.character(stv),
            code = as.character(db_report$code %||% "rejected"),
            reason = as.character(db_report$error %||% ""),
            failed_checks = db_report$failed_checks %||% NULL,
            at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            rule = "首轮判定拒绝；申报保留在 attempt，本凭据只表示未通过判定"))
          Sys.chmod(side, "0600")
          resp$reject_recorded <- file.exists(side)
        }, error = function(e) { resp$reject_recorded <- FALSE })
        resp$ok <- FALSE
        resp$error <- if (!is.null(db_report$error)) db_report$error else
          "回写已收到，但未通过判定，不能进入下一步。详见 failed_checks 与返工单。"
        rc_callback(b, FALSE)
        return(send_json(con, 409, resp))
      }
    }
    if (!is.null(db_report) && isTRUE(db_report$accepted) &&
        !isTRUE(db_report$duplicate))
      resp$auto_fetch <- tryCatch(artifact_enqueue_verified(sid_chk, b$req_id, sid_step, stv,
        blocked = identical(as.character((srPass %||% list())$verdict %||% ""), "block")),
        error = function(e) list(error = conditionMessage(e)))
    if (stv %in% c("done", "failed", "stopped")) attempt_release_output(APP_DIR, sid_chk, b$req_id)
    resp$chain <- rc_callback(b, !is.null(db_report) && isTRUE(db_report$accepted) &&
      !identical(as.character((srPass %||% list())$verdict %||% ""), "block"))
    # 回写到了：队列里等重发的那条就该销项。
    #   不销的后果很具体 —— 一小时后外壳又会把同一条指令再发一遍，
    #   用户在会话里收到重复消息，还以为上次没成。
    tryCatch(inject_satisfy(req_id = trimws(as.character(b$req_id %||% "")),
                            step = sid_step,
                            session = as.character(b$session_id %||% "")),
             error = function(e) NULL)
    # ---- 这一步补完了：如果它正是 PPT 缺图等着的那一环，就自动重出 PPT ----
    #   以前缺图只写进报告，人要自己判断该重跑哪一环、自己点、再回来点一次 PPT。
    #   现在：回写成功即检查"这批还欠哪几环"，补齐最后一条就自动重出并登记产物。
    #   只在真的 armed 着才算，平时回写一律不动。
    if (qDone2 && nzchar(qsid2))
      tryCatch({
        adv <- ppt_pending_advance(qsid2, sid_step)
        if (isTRUE(adv$touched)) {
          resp$ppt_pending <- TRUE
          if (isTRUE(adv$built)) {
            resp$ppt_rebuilt <- TRUE
            resp$ppt_bytes <- (adv$ppt %||% list())$bytes
            resp$ppt_download_url <- paste0("/api/artifact/download?session=",
              utils::URLencode(qsid2, reserved = TRUE),
              "&rel=", utils::URLencode("11.report/analysis_report.pptx", reserved = TRUE))
          } else if (length(adv$remaining %||% list())) {
            resp$ppt_remaining <- adv$remaining
          } else {
            resp$ppt_rebuilt <- FALSE
            resp$ppt_error <- (adv$ppt %||% list())$log_tail %||%
              (adv$ppt %||% list())$error %||% ""
          }
        }
      }, error = function(e) NULL)

    return(send_json(con, 200, resp))
  }

  if (p == "/api/stop" && identical(req$method, "POST")) {
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    if (isTRUE(ctx$run_dir_remote))
      return(send_json(con, 409, c(list(ok = FALSE, code = "stop_not_supported_server",
        error = "目标记录在服务器模式；应用本机不能停止远端进程。"), record_cap_fields(access$cap))))
    r <- tryCatch(stop_step(ctx$run_dir),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  if (p == "/api/status") {
    if (!DB_READY) return(send_json(con, 503, list(ok = FALSE, error = "日志数据库不可用")))
    access <- record_preflight$access
    log_sid <- as.character(record_preflight$target$sid %||% "")
    log_key <- as.character(access$cap$owner_key %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    runDir <- ctx$run_dir
    st$runDir <- runDir
    st$runDirRemote <- ctx$run_dir_remote
    can_write <- isTRUE(access$cap$can_write)
    log_view <- function() {
      rows <- db_run_log_read_record(DB_CON, log_key, log_sid, 200L)
      # Historical snapshots retain original imported_at, never become live callbacks.
      received <- db_scalar(DB_CON, "SELECT MAX(received_at) FROM run_log_observations WHERE owner_key=? AND session_id=? AND provenance='callback'", list(log_key, log_sid))
      if (is.na(received %||% NA_character_)) received <- character()
      list(log_entries = rows, log_read_at = db_now(),
           latest_received_at = if (length(received)) max(received) else NULL)
    }
    cap_head <- c(list(session_id = log_sid, mode = ctx$mode,
                       run_dir = enc2utf8(gsub("\\\\", "/", runDir))),
                  record_cap_fields(access$cap))
    # 服务器模式：本机没有进程可看，状态只能来自助手回写。
    #   读的是 remote_steps.json —— 不然页面永远显示"空闲"。
    if (isTRUE(st$runDirRemote)) {
      # 只报当前这条分析的进度：全局表里混着别的分析时，
      #   页面上的状态灯会显示别人跑到哪了。
      d <- remote_steps_read(log_sid)
      # 迁移来的记录：回写清单只剩空壳，进度只能从已验证回写记录重建。
      #   不重建的话每一步都写「尚无运行记录」，而它其实早跑完、也验收过了。
      if (!length(d$steps)) {
        rebuilt2 <- tryCatch(remote_steps_from_db(log_sid), error = function(e) list())
        if (length(rebuilt2)) d$steps <- rebuilt2
      }
      d$steps <- lapply(d$steps, function(x) {
        if (!nzchar(record_trim(x$session_id))) x$session_id <- log_sid
        if (!nzchar(record_trim(x$run_dir))) x$run_dir <- runDir
        x
      })
      for (x in d$steps) {
        if (isTRUE(can_write) && nzchar(as.character(x$imported_at %||% ""))) {
          # Do not re-import snapshots already captured by this version.
          known <- db_scalar(DB_CON, "SELECT COUNT(*) FROM run_log_observations WHERE owner_key=? AND session_id=? AND step_id=? AND provenance='callback'",
                             list(log_key, log_sid, as.character(x$id)))
          if (as.integer(known) == 0L) db_run_log_append(DB_CON, log_key, log_sid, list(
            step_id = x$id, sub_id = "", req_id = x$req_id, status = x$status,
            provenance = "history_snapshot", source = x$source_host,
            source_at = x$imported_at, received_at = "", content = x$log), identity = x)
        }
      }
      projection <- remote_status_projection(d$steps)
      return(send_json(con, 200, c(cap_head, list(ok = TRUE, remote = TRUE,
        step = NULL, code = NULL, log = "", steps = d$steps), projection, log_view())))
    }
    # A shared read must not write step_state.json. Only the owner may perform
    # the stale-pid correction; shared readers just observe the stored state.
    if (isTRUE(can_write)) step_is_running(runDir)
    s <- read_step_state(runDir)
    logtxt <- read_utf8_complete(step_log_file(runDir))
    if (isTRUE(can_write) && nzchar(logtxt)) db_run_log_append(DB_CON, log_key, log_sid, list(
      step_id = s$step, req_id = "", status = s$status, provenance = "local_snapshot",
      source = "existing run.log (complete UTF-8 content snapshot; not deduplicated stdout)",
      source_at = format(file.info(step_log_file(runDir))$mtime, "%Y-%m-%dT%H:%M:%S"),
      content = logtxt), identity = list(step = s$step, status = s$status, content = logtxt))
    return(send_json(con, 200, c(cap_head, list(ok = TRUE, remote = FALSE,
      log = substr(logtxt, max(1L, nchar(logtxt) - 40000L + 1L), nchar(logtxt))), s, log_view())))
  }

  # 环境体检结果。
  #   本机模式：读运行目录里的 env_check.json（本机真跑出来的）。
  #   服务器模式：读 state/remote_env.json —— 那是从服务器上跑完导回来的，
  #     不能读本机运行目录里的文件（那个目录在服务器上，本机不存在；
  #     就算存在也是本机的旧数据，拿它当服务器结果是错的）。
  if (p == "/api/envcheck") {
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    if (isTRUE(ctx$run_dir_remote)) {
      ss <- access$cap$record %||% sess_find(APP_DIR, sid)
      rr <- ss$identify$result %||% NULL
      # 只比识别槽。步骤/链条的号在这里出现，等于把别人的题次当成识别题次。
      want <- as.character(sess_get_pending(APP_DIR, sid, slot = "identify") %||% "")
      have <- as.character(rr$req_id %||% "")
      bound <- isTRUE(rr$req_bound)
      req_at <- as.character(sess_get_pending_at(APP_DIR, sid, slot = "identify") %||% "")
      imp_at <- as.character(rr$imported_at %||% "")
      after <- !nzchar(req_at) || !nzchar(imp_at) || imp_at >= req_at
      return(send_json(con, 200, c(list(ok = TRUE, session_id = sid,
        result = rr, remote = !identical(as.character(ss$mode %||% "local"), "local"),
        stale = !is.null(rr) && !(nzchar(want) && identical(want, have) && bound && after),
        result_bound = bound, result_at = rr$imported_at %||% NULL,
        request_at = if (nzchar(req_at)) req_at else NULL,
        result_request = if (nzchar(have)) have else NULL,
        pending_request = if (nzchar(want)) want else NULL),
        record_cap_fields(access$cap))))
    }
    f <- file.path(status_dir(ctx$run_dir), "env_check.json")
    if (!file.exists(f))
      return(send_json(con, 200, c(list(ok = TRUE, session_id = sid,
        result = NULL, remote = FALSE), record_cap_fields(access$cap))))
    r <- read_json_file(f)
    return(send_json(con, 200, c(list(ok = TRUE, session_id = sid,
      result = r, remote = FALSE), record_cap_fields(access$cap))))
  }

  # 让应用自己去连服务器做体检。
  #   这条路已经废掉，不要再复活它。
  #   它原来是「应用自己拿配置里的口令去 SSH 连服务器」。
  #   那条路永远走不通（应用在容器里，到不了客户内网），而且它的存在本身
  #   就把链路讲错了：体检从来不是"应用连不上才轮到助手"，
  #   而是"这件事本来就由能连的那一端执行"。
  #   现在只回一句实话，不再 fork 任何子进程、不再碰 remote_check.R。
  if (p == "/api/remote-check" && identical(req$method, "POST")) {
    if (!HAS_PLACE) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/place.R")))
    pl <- tryCatch(place_read(APP_DIR), error = function(e) NULL)
    if (is.null(pl)) return(send_json(con, 400, list(ok = FALSE, error = "读不到位置配置")))
    if (!identical(as.character(pl$mode %||% ""), "server"))
      return(send_json(con, 400, list(ok = FALSE, error = "当前不是服务器模式")))
    return(send_json(con, 200, list(
      ok = FALSE, delegated = TRUE,
      error = paste0("应用不自己连服务器。服务器 ", as.character(pl$server$host %||% ""),
                     " 的体检由能连上它的那一端执行：点一次「保存并识别」，",
                     "这次识别会交给助手，结果写回后显示在「识别结论」里。"))))
  }

  # 导入远程（服务器）体检结果。
  #   应用跑在云端，写不了服务器上的文件，所以结果先存在应用本地，
  #   由 /api/envcheck 在服务器模式下读回。
  if (p == "/api/envcheck/import" && identical(req$method, "POST")) {
    b <- body_json()
    if (is.null(b$result) || !is.list(b$result))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 result")))
    binding <- callback_owner(b)
    if (!isTRUE(binding$ok)) return(send_json(con, 409, binding))
    sid_req <- binding$sid
    access <- record_access(DB_CON, APP_DIR, sid_req, owner_ctx, need = "write",
                            require_ctx = FALSE)
    if (!isTRUE(access$ok)) {
      out <- access
      out$body$code <- "envcheck_not_owner"
      out$body$error <- "无权写入这条分析记录的环境检查结果"
      return(send_json(con, out$status, out$body))
    }
    b$session_id <- sid_req
    b$req_id <- binding$req_id
    obj <- b$result
    obj$imported_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    obj$source_host <- as.character(b$host %||% "")
    obj$session_id <- sid_req
    # 请求标记：写回的人可以显式带上（表示"我跑的就是那一次请求"）。
    #   不带就取当前挂着的请求号 —— 执行端是从那条注入的指令接到任务的，
    #   它回写时天然就是在还这一次的账。
    #   带上之后，页面才能区分"这次的结果"和"上一轮的旧结果"。
    # 请求标记：执行端必须自己带上，不能再由服务端"替它认账"。
    #   原来不带号就回落到 last_commit.id —— 于是任何一次回写都会被贴成
    #   "本次请求的结果"，页面据此报"结果到了"，而实际上那份可能是几小时前的。
    #   这正是"点了查询显示已到、实际没有新写入"的来源之一。
    #   现在：不带号就照实记成未绑定，并单独留 req_bound 标记，
    #   新鲜度判据要求 req_bound 为真。
    rq <- as.character(b$req_id %||% "")
    obj$req_bound <- nzchar(rq)
    if (!nzchar(rq)) {
      cb0 <- as.character(b$req_id_carried %||% "")
      if (nzchar(cb0)) { rq <- cb0; obj$req_bound <- TRUE }
    }
    if (nzchar(rq)) {
      env_sid <- sid_req
      pending_env <- if (HAS_SESSIONS && nzchar(env_sid))
        as.character(sess_get_pending(APP_DIR, env_sid, slot = "identify") %||% "") else ""
      if (!attempt_has(APP_DIR, env_sid, rq))
        return(send_json(con, 409, list(ok = FALSE, code = "req_id_mismatch",
                                       error = "体检请求号不属于当前待回写请求，未写入结果。")))
      obj$req_id <- rq
    }
    rb_attempt <- attempt_save(APP_DIR, sid_req, rq, "envcheck", list(result = obj, req_id = rq))
    attempt_release_output(APP_DIR, sid_req, rq)
    # 体检结果按识别槽判选中：以前用步骤头比，识别结果一到就被判成"不是这一次"，
    #   虽然 persisted 了却 selected=false，记录里那句结论写不进去。
    if (!attempt_selected(APP_DIR, sid_req, rq, slot = "identify"))
      return(send_json(con, 200, list(ok = TRUE, persisted = TRUE, selected = FALSE,
        session = sid_req, session_id = sid_req, req_id = rq, readback = rb_attempt,
        readback_url = paste0("/api/attempts/result?session_id=", sid_req, "&req_id=", rq))))
    # 记下这一次是谁执行的。
    #   为什么要落盘：指令里原来只写「在能连上这台服务器的电脑上执行」，
    #   执行端看不到会话选中的是哪一台，只能逐台试 —— 试到连不上的那台
    #   就得把连接超时等完。一台账号通常只有一台能连的机器，
    #   第一次跑成功后把它记下来，下次指令里直接写名字，这一趟就省掉了。
    #   记的是结果里自称的执行来源（checked_from），不进对话、不涉及口令。
    exec_from <- gsub("[^A-Za-z0-9._-]", "",
                      as.character(obj$checked_from %||% ""))
    if (nzchar(exec_from) && !startsWith(as.character(obj$checked_via %||% ""), "local ")) {
      rec_p <- access$cap$place %||% list()
      cur_pc <- as.character((rec_p$server %||% list())$exec_pc %||% "")
      if (!identical(cur_pc, exec_from)) {
        rec_p$server <- rec_p$server %||% list()
        rec_p$server$exec_pc <- exec_from
        try(sess_update(APP_DIR, sid_req, list(place = rec_p)), silent = TRUE)
      }
      obj$exec_pc <- exec_from
    }
    rf <- ""
    if (!HAS_SESSIONS) {
      rf <- file.path(STATE_DIR, "remote_env.json")
      dir.create(dirname(rf), showWarnings = FALSE, recursive = TRUE)
      con2 <- file(rf, open = "wb")
      writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                                                   null = "null", digits = NA))), con2)
      close(con2)
    }

    # 同时存进「这一次分析」的记录里。
    #   为什么要存两份：remote_env.json 回答的是「最近一次识别是什么」——
    #   单线时够用，但一旦有多份记录就是错的：打开一份旧记录会看到新记录的数字。
    #   记录里的这一份回答的是「*这一次*识别出了什么」，切来切去也不会串。
    sess_saved <- NULL
    if (HAS_SESSIONS) {
      sid <- sid_req
      if (nzchar(sid)) {
        r <- tryCatch(sess_set_identify(APP_DIR, sid, obj, req_id = if (nzchar(rq)) rq else NULL,
                                        extra = list(host = as.character(b$host %||% ""),
                                                     mode = as.character(
                                                       (access$cap$mode %||% "local")))),
                      error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        if (isTRUE(r$ok)) {
          sess_saved <- sid
          # 识别回写后不清 identify 槽，是有意的：
          #   /api/envcheck 判新鲜度要拿「这一次识别的请求号」和结果里的号对账，
          #   一写回就清掉，那一栏立刻退回"没有结果"。
          #   （2026-11-12 拆槽后这条更稳：步骤/链条只写自己的槽，
          #   不会再像以前那样把识别号顶掉，识别结论也就不会莫名变成"识别中"。）
        }
      }
    }

    # ---- 每一步回写完成时，顺手出一份这一环的质检结论 ----
    #   gina 2026-10-29：把每一步的质检加上，生成质检文件、检查上一环。
    #   这一版只记账、不拦路（blocking=FALSE），所以放在回写收尾处，
    #   不影响任何一步的执行结果。
    #   检查项都挑"不用生信知识也能判"的：
    #     ① 状态是不是 done；② 有没有产物；③ 运行目录对不对；
    #     ④ 日志里有没有明确的失败字样；⑤ 产物清单是否为空。
    qualMade <- NULL
    # 质检与台账写入已移到 /api/step/import（原位置永远不会执行，见下）

    persisted_env <- if (HAS_SESSIONS) {
      tryCatch((sess_find(APP_DIR, sid_req) %||% list())$identify$result,
               error = function(e) NULL)
    } else tryCatch(jsonlite::fromJSON(read_text_any(rf), simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(persisted_env) ||
        !isTRUE(all.equal(persisted_env, obj, check.attributes = FALSE)))
      return(send_json(con, 500, list(ok = FALSE, code = "writeback_not_persisted",
                                     error = "体检结果未通过落盘读回核验。")))
    if (HAS_SESSIONS && is.null(sess_saved))
      return(send_json(con, 500, list(ok = FALSE, code = "session_identify_not_saved",
                                     error = "体检结果没有保存到分析记录，不能宣称回写成功。")))
    return(send_json(con, 200, c(list(ok = TRUE, saved = if (nzchar(rf)) enc2utf8(rf) else NULL,
                                    session = sess_saved,
                                    quality = qualMade,
                                    req_id = if (nzchar(rq)) rq else NULL),
                                    record_cap_fields(access$cap))))
  }

  # 自动判定的结论。
  #   页面要靠它来决定「能不能开跑」以及「能不能提示人去答 B 段那几件」。
  #   和 step_plan.R 里的 read_decision() 读的是同一个文件，
  #   不在这里另做一遍判定 —— 两处判定早晚不一致。
  if (p == "/api/decision") {
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    f <- file.path(status_dir(ctx$run_dir), "decision.json")
    if (!file.exists(f))
      return(send_json(con, 200, c(list(ok = TRUE, session_id = ctx$session_id,
        result = NULL), record_cap_fields(access$cap))))
    r <- read_json_file(f)
    return(send_json(con, 200, c(list(ok = TRUE, session_id = ctx$session_id,
      result = r), record_cap_fields(access$cap))))
  }

  # ---- 决策：到点生成的问题与人的答复 ------------------------------
  #   这里返回的只有"已经生成过的"条目。
  #   没有走到那一步，就没有条目 —— 这是刻意的：清单不是预置的，
  #   是跑到那一步、拿到当时的数据之后才生出来的。
  if (p == "/api/scripts/audit") {
    if (!isTRUE(HAS_AUDIT))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/audit.R")))
    tryCatch(prov_seed(), error = function(e) NULL)
    d <- prov_read()
    unv <- Filter(function(x) !isTRUE(x$verified), d$items)
    return(send_json(con, 200, list(
      ok = TRUE,
      items = d$items,
      total = length(d$items),
      unverified = length(unv),
      unverified_rel = as.list(vapply(unv, function(x) as.character(x$rel %||% ""), "")),
      updated_at = d$updated_at)))
  }

  # 一致性核对：拿"总账"对分项。
  #   不需要懂生信也能判的一条：所有分项加起来必须等于总数。
  #   上一轮的表就是在这里对不上却没人查，于是错表一路往后传。
  if (p == "/api/consistency") {
    if (!isTRUE(HAS_AUDIT))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/audit.R")))
    # 参数在 req$target 里（req 只有 method/target/headers/body，没有 query 字段）。
    #   这一点踩过两次坑：先写成 parse_query(req$query) —— req$query 是 NULL，
    #   于是"什么都没查"却回 200 ok。核对接口自己不长牙齿，比没有更坏。
    cells <- suppressWarnings(as.numeric(qget(req$target, "cells")))
    tsum <- suppressWarnings(as.numeric(qget(req$target, "table_sum")))
    # 两档容错：
    #   internal（默认）同一份对象内部对账，必须严格相等；
    #   cross 跨轮次/跨口径比对，用小容差并只告警，不阻断流程。
    md <- as.character(qget(req$target, "mode") %||% "")
    if (!nzchar(md)) md <- "internal"
    tol <- suppressWarnings(as.numeric(qget(req$target, "tol")))
    if (is.na(tol)) tol <- 0.001
    r <- consistency_check(cells = cells, table_sum = tsum, mode = md, tol = tol)
    return(send_json(con, 200, list(
      ok = r$ok, mode = r$mode, problems = r$problems, warnings = r$warnings,
      checked = list(cells = cells, table_sum = tsum))))
  }

  # 每一步的质检账（应用侧自己记的那份）。
  #   返回每一步的结论、检查项、以及"上一环查得怎么样"。
  #   这一版只读不拦：blocking=FALSE 写在每一条里，将来要改成闸门必须另开一次改动。
  if (p == "/api/quality") {
    if (!isTRUE(HAS_QUALITY))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/quality.R")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    d <- qual_read(qsid)
    ord <- if (isTRUE(HAS_FLOW)) tryCatch(FLOW_ORDER(flow_profile(qsid)), error = function(e) character(0)) else character(0)
    prevs <- lapply((d$steps %||% list()), function(x) {
      qual_prev_check(qsid, as.character(x$step %||% ""), order = ord)
    })
    return(send_json(con, 200, c(list(
      ok = TRUE, session_id = qsid,
      steps = d$steps, updated_at = d$updated_at,
      prev_checks = prevs,
      blocking = FALSE), record_cap_fields(access$cap))))
  }

  # ---- 环节审计看板：每环的前置审结论 + 最新护照 ----------------------
  #   为什么单独一个接口，不塞进 /api/quality：
  #     quality 回答的是"这一步回写完自己记了什么"，
  #     reviewer 回答的是"这一环现在能不能开、上一轮放行了什么"。
  #     两个问题的读者不同（一个是人看进度，一个是查为什么卡住），
  #     合成一个接口，页面就只能显示其中一半。
  if (p == "/api/reviewer") {
    if (!isTRUE(HAS_STAGE_REVIEWER))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/stage_reviewer.R")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    stage <- qget(req$target, "stage")
    if (nzchar(stage)) {
      pre <- tryCatch(sr_preflight(APP_DIR, stage, sid = qsid,
                                   profile = flow_profile(qsid), note = "看板按需预检"),
                      error = function(e) list(verdict = "unknown", error = conditionMessage(e)))
      pp <- tryCatch(sr_passport(APP_DIR, qsid, stage), error = function(e) NULL)
      return(send_json(con, 200, c(list(
        ok = TRUE, session_id = qsid, stage = stage,
        rule_version = SR_RULE_VERSION,
        pre = pre,
        passport = pp,
        history = tryCatch(sr_passport_history(APP_DIR, qsid, stage), error = function(e) list()),
        blocking = TRUE,
        meaning = "block=明确不合格必须修；warn=可以开但有未核项；pass=条件齐备"),
        record_cap_fields(access$cap))))
    }
    ch <- tryCatch(sr_chain_all(APP_DIR, qsid, flow_profile(qsid)),
                   error = function(e) list(stages = list(), error = conditionMessage(e)))
    return(send_json(con, 200, c(list(ok = TRUE, session_id = qsid),
                                 ch, record_cap_fields(access$cap),
                                 list(blocking = TRUE,
                                      meaning = "每环：最新护照结论 + 现在开跑的前置审结论"))))
  }

  # ---- 注释依据（参考表 / marker 词典）----
  #   为什么是一组接口而不是一个固定路径：
  #     换组织、换物种、换细胞类型范围，注释依据就不同。
  #     写死一份"官方表"能跑通这一轮，但下一批数据就没法用了 ——
  #     而用错依据注释出来的结果，格式是对的、内容是错的，事后极难发现。
  if (p == "/api/reference/list") {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/reference.R")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    chosen <- NULL
    if (nzchar(qsid) && HAS_SESSIONS) {
      s0 <- tryCatch(sess_find(APP_DIR, qsid), error = function(e) NULL)
      chosen <- (s0 %||% list())$reference_id %||% NULL
      if (identical(chosen, "")) chosen <- NULL
    }
    # 物种与组织按这条分析记录带上：候选表要据此判"适不适用"。
    #   不传的话每份都只能报"判不了"，等于没判。
    ssp <- ""; sts <- ""
    if (nzchar(qsid) && HAS_SESSIONS) {
      s0 <- tryCatch(sess_find(APP_DIR, qsid), error = function(e) NULL)
      ssp <- as.character((s0 %||% list())$species %||% "")
      sts <- as.character((s0 %||% list())$tissue %||% "")
    }
    return(send_json(con, 200, c(list(
      ok = TRUE, items = ref_list(species = ssp, tissue = sts),
      chosen = chosen, session_id = qsid,
      species = ssp, tissue = sts), record_cap_fields(access$cap))))
  }

  # 上传：前端把文件读成 base64 传上来，避开 multipart 解析。
  #   必填 note：说明这份表适用什么（组织/物种/细胞范围）。
  #   不填就不收 —— 没有说明的表，一个月后没人敢用。
  if (p == "/api/reference/upload" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/reference.R")))
    b <- body_json()
    r <- ref_save(name = as.character(b$name %||% ""),
                  b64 = as.character(b$b64 %||% ""),
                  note = as.character(b$note %||% ""),
                  species = as.character(b$species %||% ""),
                  tissue = as.character(b$tissue %||% ""))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 给某一份候选补说明：物种 / 组织 / 备注。
  #   为什么需要：很多补充材料只在正文说"这是人肾"，表里没有这一栏。
  #   空着就判不了适用性，人只能凭文件名猜 —— 猜错了就是整批结果错。
  if (p == "/api/reference/meta" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/reference.R")))
    b <- body_json()
    rid <- as.character(b$id %||% "")
    if (!nzchar(rid)) return(send_json(con, 400, list(ok = FALSE, error = "缺 id")))
    r <- tryCatch(ref_set_meta(
      rid,
      note = if (is.null(b$note)) NULL else as.character(b$note),
      species = if (is.null(b$species)) NULL else as.character(b$species),
      tissue = if (is.null(b$tissue)) NULL else as.character(b$tissue)),
      error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 选用：记在"这一条分析记录"上，不是全局。
  #   同一个人同时跑两批数据是完全正常的，全局一份会互相覆盖。
  if (p == "/api/reference/choose" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/reference.R")))
    b <- body_json()
    rid <- as.character(b$id %||% "")
    # server 来源的文件不在本地，ref_path 恒为空 —— 以前这条判断直接
    #   把服务器候选判成"没有这份参考表"，等于选不了。
    if (nzchar(rid)) {
      okRef <- nzchar(ref_path(rid)) || nzchar(ref_server_path(rid))
      if (!okRef) return(send_json(con, 404, list(ok = FALSE, error = "没有这份参考表")))
    }
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")

    # ---- 选之前先判适不适用 ----
    #   不适用不是"不让选"，是"选了要说明白后果"。
    #   以前没有任何提示：选完、跑完、每一群都得到一个名字，名字全错，
    #   而且从结果上看不出错。这一步就是为了在点下去之前把它摆出来。
    apInfo <- NULL
    if (nzchar(rid) && isTRUE(HAS_REFERENCE)) {
      it <- tryCatch(ref_get(rid), error = function(e) NULL)
      if (!is.null(it)) {
        s0 <- tryCatch(sess_find(APP_DIR, qsid), error = function(e) NULL)
        apInfo <- tryCatch(ref_applicability(
          it, species = as.character((s0 %||% list())$species %||% ""),
          tissue = as.character((s0 %||% list())$tissue %||% "")),
          error = function(e) NULL)
      }
    }
    # 清除选用时不能传 NULL：sess_update 对 NULL 是"跳过不写"，
    #   于是"取消选用"这个动作会静默失效 —— 界面上看着清了，
    #   实际 reference_id 还在，闸门因此永远放行。
    #   这里传空串，并在判定处把空串当"没选"。
    r <- if (HAS_SESSIONS) sess_update(APP_DIR, qsid,
             patch = list(reference_id = if (nzchar(rid)) rid else "")) else
           list(ok = FALSE, error = "缺少 sessions.R")
    if (!isTRUE(r$ok)) return(send_json(con, 400, r))
    return(send_json(con, 200, c(list(ok = TRUE, session_id = qsid,
                                    reference_id = if (nzchar(rid)) rid else "",
                                    applicable = (apInfo %||% list())$verdict,
                                    applicable_why = (apInfo %||% list())$why),
                                    record_cap_fields(access$cap))))
  }

  # 请助手去服务器上找一遍已有的参考表。
  #   为什么需要这一步而不是应用自己找：应用在公网侧，看不到对方内网磁盘。
  #   为什么只读：这是"列一下家底"，不该顺手改任何文件。
  if (p == "/api/reference/scan-request" && identical(req$method, "POST")) {
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    pl <- access$cap$place
    srv <- (pl %||% list())$server %||% list()
    host <- as.character(srv$host %||% "")
    port <- as.character(srv$port %||% 22)
    user <- as.character(srv$user %||% "")
    refMode <- as.character(srv$ref_mode %||% "")
    refPath <- as.character(srv$ref_path %||% "")
    refNote <- as.character(srv$ref_note %||% "")

    # ---- 2026-11-04 改：只核对用户指定的那一份，不再扫目录 ----
    #   为什么改：原来让执行端"到服务器上把可用的参考表列一遍"，扫出来的
    #   候选还得人再挑一次 —— 同一个决定被问了两遍，中间还夹一次全盘遍历。
    #   实测那次全盘深扫撞 300 秒超时、白等 5 分钟，而用户其实一句话就能说出路径。
    #   现在分三种情况，都不递归、不 find：
    #     ① 用户给了路径：只核对这一个文件；
    #     ② 用户说上传 / 查文献 / 不用：这一步根本不用发指令；
    #     ③ 用户什么都没给：不扫，直接请他给个路径。
    if (identical(refMode, "path") && nzchar(refPath)) {
      txt <- paste(c(
        "【来自 个性化分析助手】核对一份注释依据表（只读）。",
        "",
        paste0("目标服务器：", host, ":", port, "　登录账号：", user),
        paste0("要核对的表：", refPath),
        if (nzchar(refNote)) paste0("用户说明：", refNote),
        "",
        "只做这一件事：核对上面这个文件，然后登记。不要扫目录、不要 find、",
        "不要去找别的候选表 —— 用户已经指定了这一份，多找出来的只会让人再选一次。",
        "",
        "怎么做：",
        "1. 口令从本机应用的 config/server_profile.json 读，不要写进对话。",
        "2. 在能连上这台服务器的电脑上跑应用自带的 engine/workpc_scan_ref.py：",
        "   export SC_REMOTE_PWD='<从上面那个文件读>'",
        paste0("   python3 workpc_scan_ref.py --host ", host, " --user ", user,
               " --path '", refPath, "' --out <本机临时路径>"),
        "   它只核对给的这一条：在不在、多大、什么表头、表里声明的物种与组织。",
        "   给的是目录它会告诉你这是目录并列出这一层，不会递归进去。",
        "3. 把输出的 items 回写：POST /api/reference/scan",
        "   body 就是这份 JSON 原文（含 host 与 items），不要改结构。",
        "4. 一句话报告：在不在、多大、判出来的物种组织（判不出就说不确定）。",
        "",
        "红线：不要自己写脚本、不要绕过 workpc_scan_ref.py 去 find、",
        "      找不到就如实说找不到，不要拿名字相近的文件顶替。"
      ), collapse = "\n")
    } else if (refMode %in% c("upload", "lit", "none") && nzchar(refMode)) {
      # 这三种都不需要在服务器上找，也就不该发任何扫描指令。
      return(send_json(con, 200, list(
        ok = TRUE, session_id = qsid, queued = FALSE, skipped = TRUE,
        reason = switch(refMode,
                        upload = "用户会自己上传一份，不需要去服务器上找",
                        lit    = "用户选了查文献建一份，不需要去服务器上找",
                        none   = "这一轮不需要注释依据",
                        ""),
        text = "")))
    } else {
      # 什么都没给：不发扫描指令，直接把话问回去。
      return(send_json(con, 200, list(
        ok = TRUE, session_id = qsid, queued = FALSE, need_path = TRUE,
        reason = paste0("还没给注释依据的路径。请在「服务器信息 · 注释依据」里填上绝对路径，",
                        "或者改成上传 / 查文献 / 这一轮不需要；",
                        "填好后这里就只核对那一份。"),
        text = "")))
    }

    # 按钮名一起带上：这段是「核对注释依据路径」按钮点出来的自动派发。
    #   不写的话，会话里看到的就是一大段指令，看不出是哪一次操作发起的。
    enq <- tryCatch(inject_enqueue(text = txt, session = qsid,
                                   reason = "reference-scan", step = "annotate",
                                   origin = "核对注释依据"),
                    error = function(e) NULL)
    return(send_json(con, 200, list(ok = TRUE, session_id = qsid,
                                    queued = !is.null(enq), text = txt)))
  }

  # 执行端扫描完服务器、把候选表回写到这里。
  #   为什么由执行端扫：应用在公网侧，看不到对方内网磁盘。
  #   只有执行端能 ls 服务器，所以"罗列已有文件库"这件事必须它来做。
  if (p == "/api/reference/scan" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/reference.R")))
    b <- body_json()
    r <- ref_register_server(b$items %||% list(),
                             host = as.character(b$host %||% ""))
    return(send_json(con, 200, r))
  }

  # 下发：执行端按选用的那一份去服务器落盘。
  #   为什么要走接口而不是让它读应用磁盘：应用在公网这一侧，
  #   服务器在对方内网，只有执行端同时够得着两边。
  #   所以由应用把内容交出去，执行端落盘、校验指纹、再交给注释脚本。
  if (p == "/api/reference/get") {
    if (!isTRUE(HAS_REFERENCE))
      return(send_json(con, 404, list(ok = FALSE, error = "缺少 engine/reference.R")))
    rid <- qget(req$target, "id")
    if (!nzchar(rid)) {
      qsid <- qget(req$target, "session")
      if (nzchar(qsid) && HAS_SESSIONS) {
        s0 <- tryCatch(sess_find(APP_DIR, qsid), error = function(e) NULL)
        rid <- as.character((s0 %||% list())$reference_id %||% "")
      }
    }
    if (!nzchar(rid)) return(send_json(con, 404, list(ok = FALSE, error = "本次分析还没有选用参考表")))
    it <- ref_get(rid)
    # 服务器上本来就有的那一份：只回路径，不下发内容。
    #   为什么不下发：原件就在服务器上，传一遍再落一份只会多出一个
    #   "和原件不一定一致"的副本；而注释要读的是原件。
    if (identical(as.character((it %||% list())$source %||% ""), "server")) {
      sp <- as.character((it %||% list())$server_path %||% "")
      if (!nzchar(sp))
        return(send_json(con, 404, list(ok = FALSE, error = "这条是服务器来源，但没记下路径")))
      return(send_json(con, 200, list(
        ok = TRUE, id = rid, source = "server", server_path = sp,
        name = as.character((it %||% list())$name %||% basename(sp)),
        note = as.character((it %||% list())$note %||% ""))))
    }
    fp <- ref_path(rid)
    if (!nzchar(fp)) return(send_json(con, 404, list(ok = FALSE, error = "条目在但文件不在")))
    raw <- readBin(fp, "raw", n = file.size(fp))
    return(send_json(con, 200, list(
      ok = TRUE, id = rid, source = "uploaded",
      name = as.character((it %||% list())$name %||% basename(fp)),
      note = as.character((it %||% list())$note %||% ""),
      digest = unname(tools::md5sum(fp)),
      bytes = length(raw),
      b64 = jsonlite::base64_enc(raw))))
  }

  # 让助手去查文献、自己建一份注释依据。
  #   为什么要有这条路：手上没有现成参考表时就卡住了，而"卡住"不该是终点。
  #   但这条路必须留痕：查了哪些库、用了哪些文献、每条 marker 出自哪里，
  #   全部要写回来，不能只回一句"我查好了"。
  if (p == "/api/reference/litdict" && identical(req$method, "POST")) {
    b <- body_json()
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    sp <- trimws(as.character(b$species %||% ""))
    ts <- trimws(as.character(b$tissue %||% ""))
    # 存档号：文献库里那一份的编号。给了号就不必再靠物种组织去筛 ——
    #   号本身就是检索入口，这时物种组织缺了也不拦。
    ano <- trimws(as.character(b$ref_archive %||% b$archive_no %||% ""))
    if (!nzchar(ano) && (!nzchar(sp) || !nzchar(ts)))
      return(send_json(con, 400, list(ok = FALSE,
        # 不再用"物种不同就不能用"当理由挡人。
        #   填这两项是为了让检索更准，不是为了审核资格；
        #   缺了就换个说法提示，不判不可用。
        error = "填一下物种和组织，检索会准一些（不填也能建，只是范围会宽）")))
    dirQ <- pjoin(as.character(access$cap$run_dir %||% ""), "stages", "annotation")
    # ---- 2026-11-04 重写：从「建一份词典」改成「检索并引用已发表的」 ----
    #   上一版让助手"为这批数据建一份 marker 词典"，实际上默许了它会
    #   凭自己的理解去拼一份出来。产物就是那份 mouse_spleen_dict_draft.json：
    #   每类 4-7 个 marker、refs 全空、还写着"草案"。
    #   最要命的不是它不准，而是它看起来像一份正经依据 —— 之后所有人
    #   都拿它当结论用。用户的要求很明确：不许自己草拟，只用检索到的。
    #   所以现在只允许一条路：从已发表文献里摘，逐条带出处；
    #   摘不到就空着，不许补。补一个没出处的 marker，比少一类更糟。
    txt <- paste(c(
      "【来自 个性化分析助手】请从已发表文献里检索细胞类型 marker，汇总成一份注释依据。",
      "",
      if (nzchar(ano)) paste0("文献库存档号：", ano,
                             "　（按这个号去库里取那一份依据；取不到就如实说取不到）"),
      paste0("数据背景：物种 ", sp, "，组织 ", ts,
             if (nzchar(dirQ)) paste0("；对象在服务器结果目录的 8.final/final_object.rds"),
             if (nzchar(dirQ)) paste0("；上一轮 marker 表在 8.final/top15_markers.xls") else ""),
      "",
      "只做一件事：检索 + 摘录 + 标注出处。不做推理、不做推测、不做补齐。",
      "",
      "1. 用学术文献库检索（PubMed / Europe PMC / OpenAlex 等）。检索范围建议放宽：",
      "   这个组织 + 这个物种的经典 marker 文献与单细胞图谱；",
      "   如果该组织在该物种里文献很少，可以纳入人或其他常用模式动物的同源研究 ——",
      "   人和小鼠等常见物种的细胞类型判定基因多为同源基因（人 CD3D / 鼠 Cd3d），",
      "   这一步不需要为物种差异停下来。",
      "2. 每条细胞类型的每个 marker 都必须来自某篇具体文献，refs 里写 PMID 或 DOI。",
      "   refs 为空的条目一律不要写进去 —— 没有出处就等于自己编的。",
      "3. 汇总成 JSON，结构：",
      '   {"细胞类型": {"markers": ["GENEA","GENEB"], "refs": ["PMID:xxxx"], "full": "全称"}}',
      "4. 交回应用：POST /api/reference/upload，body 给 name、b64（文件 base64）、",
      "   note（一句话说明适用范围）、species、tissue（写清这张表出自哪个物种哪个组织）。",
      "5. 回一句：用了哪些库、查了哪些文献、收录了几类、哪几类没查到。",
      "",
      "红线（任一条违反，这份依据就不能用）：",
      "· 不许凭印象、凭常识、凭模型记忆写 marker。查不到就空着。",
      "· 不许为了凑类数而补基因。宁可只收录 5 类有出处的，也不要 20 类里一半没来源。",
      "· 不许把「我记得这些是经典 marker」当成检索结果。记忆不是出处。",
      "· 查不到就明说查不到，并列出缺口 —— 缺口本身就是有用信息。"
    ), collapse = "\n")
    if (nzchar(ano)) txt <- paste(c(
      "【来自 个性化分析助手】请按文献库存档号取已存档的注释依据。",
      paste0("文献库存档号：", ano),
      paste0("分析记录：", qsid),
      "只检索这个存档号对应的原资料；取不到就如实说明，不以其他文献或自行生成的词典替代。",
      "取到后保留原始出处，通过 POST /api/reference/upload 上传依据，并用 /api/reference/choose 绑定本条分析记录。",
      "只有上传、选择和读回核验全部成功后，才说明已准备好注释依据。"
    ), collapse = "\n")
    enq <- tryCatch(inject_enqueue(text = txt, session = qsid,
                                   reason = "reference-litdict", step = "annotate",
                                   origin = "查文献建注释依据"),
                    error = function(e) NULL)
    if (is.null(enq) || isFALSE(enq$ok))
      return(send_json(con, 500, list(ok = FALSE, code = "reference_lookup_not_queued",
                                     queued = FALSE, error = "检索任务没有成功入队，请勿宣称已交给助手。")))
    return(send_json(con, 200, list(ok = TRUE, session_id = qsid,
                                    queued = TRUE, ref_archive = ano, text = txt)))
  }

  # ---- 对外台账：给执行端取走"该追加到服务器流水的行" ----
  #   为什么台账由应用生成、执行端只负责追加：
  #     reviewer.R 检查的是 set_id / run_dir / code / 产物指纹 / 阶段名，
  #     只写 run_id+step+event 的流水必然被判 REFUSED（"没经过统一入口"）。
  #     应用知道语义（这一步属于哪套 skill、该有哪些产物），
  #     执行端知道事实（跑没跑成、退出码、文件在不在）。
  #     所以应用写"合规的行"，执行端原样追加。
  if (p == "/api/ledger/out") {
    if (!isTRUE(HAS_LEDGER_OUT))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/ledger_out.R")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    lines <- led_out_take(qsid)
    return(send_json(con, 200, c(list(
      ok = TRUE, session_id = qsid, count = length(lines),
      run_id = led_run_id(qsid), lines = as.list(lines)), record_cap_fields(access$cap))))
  }

  # 执行端明确回报"这些行已经追加到服务器流水"，才允许清空。
  #   不回报就不清：宁可重复追加一次（reviewer 取最后一条，不影响结论），
  #   也不能在没落盘的情况下把寄存丢掉。
  if (p == "/api/ledger/ack" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_LEDGER_OUT))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/ledger_out.R")))
    b <- body_json()
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    if (!isTRUE(b$ok)) {
      return(send_json(con, 200, list(ok = FALSE, cleared = FALSE,
                                      error = as.character(b$error %||% "执行端未确认"))))
    }
    led_out_clear(qsid)
    return(send_json(con, 200, c(list(ok = TRUE, cleared = TRUE, session_id = qsid),
      record_cap_fields(access$cap))))
  }

  if (p == "/api/decisions") {
    if (!isTRUE(HAS_DECISIONS))
      return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/decisions.R")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    d <- dec_read(qsid)
    pend <- Filter(function(x) identical(as.character(x$status %||% ""), "asked"), d$items)
    done <- Filter(function(x) identical(as.character(x$status %||% ""), "answered"), d$items)
    return(send_json(con, 200, c(list(
      ok = TRUE, session_id = qsid,
      items = d$items,
      pending = length(pend),
      answered = length(done),
      generated = length(d$items)), record_cap_fields(access$cap))))
  }

  # 到点生成并真的发给人。
  #   为什么"生成"必须伴随"发出去"：问题只写进文件、不进入会话，
  #   人在服务器模式下根本看不到，等于没问。
  if (p == "/api/decisions/ask" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_DECISIONS))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/decisions.R")))
    b <- body_json()
    stepId <- as.character(b$step %||% "")
    if (!nzchar(stepId)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 step")))
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    made <- dec_ensure(stepId, qsid)
    delivered <- NULL
    if (length(made)) {
      txt <- dec_ask_text(made, stepTitle = if (HAS_FLOW) {
        x <- flow_stage(stepId); if (is.null(x)) stepId else as.character(x$title)
      } else stepId)
      delivered <- tryCatch(inject_enqueue(text = txt, session = qsid,
                                           reason = "decision", step = stepId,
                                           origin = "需要你定一件事"),
                            error = function(e) NULL)
    }
    d <- dec_read(qsid)
    return(send_json(con, 200, c(list(
      ok = TRUE, generated = length(made), session_id = qsid,
      items = made, pending = length(dec_pending(qsid)),
      queued = !is.null(delivered),
      text = if (length(made)) dec_ask_text(made) else ""), record_cap_fields(access$cap))))
  }

  if (p == "/api/decisions/answer" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_DECISIONS))
      return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/decisions.R")))
    b <- body_json()
    access <- record_preflight$access
    qsid <- as.character(record_preflight$target$sid %||% "")
    it <- dec_answer(qsid, as.character(b$id %||% ""),
                     b$answer, as.character(b$detail %||% ""))
    if (is.null(it)) return(send_json(con, 404, list(ok = FALSE, error = "没有这一条")))
    return(send_json(con, 200, c(list(ok = TRUE, item = it,
      pending = length(dec_pending(qsid))), record_cap_fields(access$cap))))
  }

  # 脚本库：库表 + 按当前环境查表的结果
  #   页面上的「脚本库」页签用它。判定逻辑只在 pick.R 一份，
  #   这里只负责把结果发给界面，不重复实现。
  # ---- 运行位置：本机 / 服务器 -------------------------------------
  #   密码只存本机 config，不回传给浏览器；界面只看得到 host/user/path。
  if (p == "/api/where") {
    prof_f <- file.path(APP_DIR, "config", "server_profile.json")
    prof <- tryCatch(read_json_file(prof_f), error = function(e) NULL)
    if (is.null(prof)) prof <- list(mode = "local")
    if (identical(req$method, "POST")) {
      b <- body_json()
      if (!is.null(b$mode) && nzchar(as.character(b$mode))) {
        prof$mode <- as.character(b$mode)
        if (!is.null(b$run_dir) && nzchar(as.character(b$run_dir))) {
          prof[[prof$mode]] <- prof[[prof$mode]] %||% list()
          prof[[prof$mode]]$default_run_dir <- as.character(b$run_dir)
        }
        dir.create(dirname(prof_f), showWarnings = FALSE, recursive = TRUE)
        con2 <- file(prof_f, open = "wb")
        writeBin(charToRaw(enc2utf8(jsonlite::toJSON(prof, auto_unbox = TRUE,
                                                     pretty = TRUE, null = "null"))), con2)
        close(con2)
      }
    }
    srv <- prof$server %||% list()
    safe <- list(label = srv$label, host = srv$host, port = srv$port, user = srv$user,
                 exec_pc = srv$exec_pc,
                 data_root = srv$data_root, script_root = srv$script_root,
                 work_root = srv$work_root, note = srv$note,
                 runtime = srv$runtime,
                 has_password = nzchar(srv$password %||% ""))
    return(send_json(con, 200, list(ok = TRUE, mode = prof$mode %||% "local",
                                    local = prof$local, server = safe)))
  }

  # ---- engine 包外送：把出图引擎（含入口守卫与 skill 脚本）交给外部 ----------
  #   为什么用应用当中转：本机与用户电脑、服务器互相不可达，
  #   而这个应用有固定公网地址，是唯一一条三方都能走的通道。
  if (p == "/api/engine-pack") {
    fp <- file.path(APP_DIR, "state", "exports", "engine_pack.tar.xz")
    if (!file.exists(fp)) return(send_json(con, 404, list(ok = FALSE, error = "尚未准备 engine 包")))
    raw <- readBin(fp, "raw", n = file.size(fp))
    return(send_json(con, 200, list(ok = TRUE, name = "engine_pack.tar.xz",
                                    bytes = length(raw),
                                    md5 = unname(tools::md5sum(fp)),
                                    b64 = jsonlite::base64_enc(raw))))
  }

  # ---- C 类补充脚本：列清单 / 取文件 ---------------------------------
  #   为什么给两个端点而不是直接放进 engine_pack：
  #     整包几百兆，为了补一张统计表去拉整包不现实；而且每次都要能单独校验。
  #   md5 是给执行端落地后自校的 —— 对不上就停下，不许带着旧版本继续跑。
  if (p == "/api/supplement") {
    idx <- supplement_index()
    items <- lapply(idx$entries, function(e) {
      fp <- supplement_path(e)
      list(id = as.character(e$id %||% ""),
           for_stage = as.character(e$for_stage %||% ""),
           for_sub = as.character(e$for_sub %||% ""),
           title = as.character(e$title %||% ""),
           why = as.character(e$why %||% ""),
           runtime = as.character(e$runtime %||% "rscript"),
           status = as.character(e$status %||% "ready"),
           file = as.character(e$file %||% ""),
           exists = file.exists(fp),
           md5 = if (file.exists(fp)) unname(tools::md5sum(fp)) else NA_character_,
           bytes = if (file.exists(fp)) as.numeric(file.size(fp)) else NA_real_,
           # 人要看的是"这是哪个文件、干什么的、什么时候改的"，
           #   光给一串 md5 等于没说。三项一起给，md5 只作核对用。
           resolved = if (nzchar(fp)) enc2utf8(gsub("\\\\", "/", fp)) else "",
           # 相对 engine 的路径：页面点开脚本时要用它去 /api/script-read。
           #   file 字段是登记时写的，未必等于实际位置（C_supplement 那几条
           #   登记成 C_supplement/x.R，实际在 library/C_supplement/x.R），
           #   照 file 读就会"找不到这个脚本"。这里按真实路径算一份。
           rel = if (nzchar(fp)) {
             en <- enc2utf8(gsub("\\\\", "/", normalizePath(ENGINE_DIR, winslash = "/")))
             f2 <- enc2utf8(gsub("\\\\", "/", fp))
             sub(paste0("^", en, "/?"), "", f2)
           } else "",
           mtime = if (file.exists(fp))
             format(file.info(fp)$mtime, "%Y-%m-%d %H:%M") else "",
           stage_title = {
             st1 <- tryCatch(flow_stage(as.character(e$for_stage %||% "")), error = function(e2) NULL)
             if (is.null(st1)) as.character(e$for_stage %||% "") else as.character(st1$title)
           })
    })
    return(send_json(con, 200, list(ok = TRUE, count = length(items), items = items)))
  }

  if (p == "/api/supplement/get") {
    want <- as.character(q$id %||% "")
    idx <- supplement_index()
    hit <- Filter(function(e) identical(as.character(e$id %||% ""), want), idx$entries)
    if (!length(hit)) return(send_json(con, 404, list(ok = FALSE, error = "库里没有这个条目")))
    fp <- supplement_path(hit[[1]])
    if (!file.exists(fp)) return(send_json(con, 404, list(ok = FALSE, error = "条目登记了但文件不在")))
    raw <- readBin(fp, "raw", n = file.size(fp))
    return(send_json(con, 200, list(
      ok = TRUE, id = want, name = basename(fp),
      md5 = unname(tools::md5sum(fp)), bytes = length(raw),
      b64 = jsonlite::base64_enc(raw))))
  }

  if (p == "/api/attempts/result" && identical(req$method, "GET")) {
    target <- record_session_target(q, list(), path = p, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "read")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    sid <- target$sid; rq <- qget(req$target, "req_id")
    if (!attempt_has(APP_DIR, sid, rq))
      return(send_json(con, 404, list(ok = FALSE, code = "attempt_not_registered")))
    return(send_json(con, 200, c(list(ok = TRUE, session_id = sid, req_id = rq,
      selected = attempt_selected(APP_DIR, sid, rq), result = attempt_results(APP_DIR, sid, rq)),
      record_cap_fields(access$cap))))
  }
  # Private full instruction handoff. No client-controlled filesystem paths.
  if (p == "/api/instruction/prepare") {
    if (!identical(req$method, "POST"))
      return(send_json(con, 405, list(ok = FALSE, code = "post_required", stage = "method", http_status = 405)))
    if (!DB_READY || identical(owner_ctx$kind, "unidentified") ||
        is.na(owner_ctx_key(owner_ctx)))
      return(send_json(con, 403, list(ok = FALSE, code = "instruction_not_owner", stage = "authorization", http_status = 403)))
    b <- body_json()
    resolved <- instruction_source(APP_DIR, b, if (!is.null(b$item_id)) inject_read()$items %||% list() else list(),
      owner_ctx_key(owner_ctx), function(sid) db_api_notebook_authorized(DB_CON, owner_ctx, sid))
    if (!isTRUE(resolved$ok)) return(send_json(con, resolved$http_status, resolved))
    b <- resolved$body
    scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    # ---- 只读语义只有一种：query 与 observe 同待遇 ---------------------
    #   为什么要加 observe：前端在"作业已入队"时会把 purpose 标成 observe，
    #   意思是"这条只是通知，别当成新任务"。原来白名单只有 query/execute，
    #   observe 会被 400 挡掉 —— 而前端会把 400 当成投递失败，
    #   于是真实通道明明通着，页面却报没发出去。
    #   更糟的一种：如果这里放行但按 execute 走，它会重建执行正文、
    #   申请回写凭据、抢占输出目录 —— 等于为一条"只观察"的通知准备了全套执行条件，
    #   助手拿到手就可能真的再跑一遍。
    #   所以 observe 必须和 query 走同一条只读分支：不重建正文、不发凭据、不抢目录。
    query_only <- isTRUE(b$purpose %in% c("query", "observe"))
    # ---- 进会话那段正文的第一行必须是按钮名 ---------------------------
    #   为什么必须由服务端拼：会话里看到的那段不是前端发的，是这里生成的
    #   短指针（只有 path/context_path/session_id/req_id）。前端把
    #   「【保存并识别】」拼进正文，只落到 instruction.txt 第一行，
    #   会话首行仍然没有出处 —— gina 2026-11-22 报的就是这个。
    #   所以按钮名必须随请求传上来，并拼在这段指针的最前面。
    #   只允许短中文标签；换行/括号一律剔掉，避免有人靠它往会话里插第二行指令。
    origin_lab <- gsub("[【】\r\n\t]", "", trimws(as.character(b$origin %||% "")))
    if (nchar(origin_lab, type = "chars") > 24L)
      origin_lab <- substr(origin_lab, 1L, 24L)
    origin_prefix <- if (nzchar(origin_lab)) paste0("【", origin_lab, "】") else ""
    if ((!is.null(b$purpose) && !b$purpose %in% c("query", "execute", "observe")) ||
        !scalar(b$session_id) || (!query_only && !scalar(b$req_id)) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", b$session_id) ||
        (!query_only && !grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", b$req_id)))
      return(send_json(con, 400, list(ok = FALSE, code = "instruction_binding_required", stage = "binding", http_status = 400)))
    if (!db_api_notebook_authorized(DB_CON, owner_ctx, b$session_id))
      return(send_json(con, 403, list(ok = FALSE, code = "instruction_not_owner", stage = "authorization", http_status = 403)))
    # pending_req is the per-session last commit (never use the global marker,
    # which can belong to a different record after a tab switch).
    pending <- if (HAS_SESSIONS) sess_get_pending(APP_DIR, b$session_id, slot = "step") else NULL
    if ((!query_only && !attempt_has(APP_DIR, b$session_id, b$req_id)))
      return(send_json(con, 409, list(ok = FALSE, code = "req_id_mismatch", stage = "registration", http_status = 409)))
    if (!scalar(b$text) || nchar(b$text, type = "bytes") > 2000000L ||
        any(c("path", "output_path", "directory") %in% names(b)))
      return(send_json(con, 400, list(ok = FALSE, code = "instruction_payload_invalid", stage = "payload", http_status = 400)))
    # Restore the owner-bound handoff on the server, not from browser memory or Host.
    # Preparing an instruction must never grant or renew authorization.
    handoff <- if (!query_only && HAS_AGENT_GATE)
      tryCatch(agent_handoff_existing(APP_DIR, owner_ctx),
               error = function(e) list(ok = FALSE)) else list(ok = FALSE)
    if (!query_only && !isTRUE(handoff$ok))
      return(send_json(con, 409, list(ok = FALSE, code = "instruction_handoff_unavailable", stage = "authorization_handoff", http_status = 409,
        error = "缺少有效的已有回写授权；未准备执行指令，请由属主人类重新确认授权。")))
    output_claim <- if (query_only) list(ok = TRUE) else attempt_claim_output(APP_DIR, b$session_id, b$req_id)
    if (!isTRUE(output_claim$ok))
      return(send_json(con, 409, list(ok = FALSE, code = "attempt_output_conflict", stage = "output_coordination", http_status = 409,
        session_id = b$session_id, req_id = b$req_id, holder = output_claim$holder,
        error = "共享结果目录正由另一尝试持有；本尝试仍可回写状态与结果，禁止覆盖共享产物。")))
    prepare_stage <- "private_directory"
    out <- tryCatch({
      root <- instruction_root(APP_DIR, owner_ctx_key(owner_ctx))
      link <- Sys.readlink(root)
      if (!is.na(link) && nzchar(link)) stop("unsafe instruction root")
      dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")

      # 同一条指令只落一份包。
      #   以前这里每次调用都新生成一个 instruction_id，而 id 写在指针正文里，
      #   于是"同一条指令"每次 prepare 都变成一段不同的正文 → 新的队列项。
      #   补发（retryDue）走的也是 prepare，于是：投递失败 → 补发 → 新 id →
      #   新队列项；页面一加载又 prepare 一次 → 又一条。实测 8 分钟 6 条，
      #   req_id 全相同，会话里被反复注入同一件事。
      #   现在先按「属主 + 分析记录 + 请求号 + 用途 + 正文哈希」找已有包，
      #   找到就复用它的文件和正文，队列项因此命中合并，条数不再自增。
      reuse <- if (exists("instruction_existing"))
        tryCatch(instruction_existing(APP_DIR, owner_ctx_key(owner_ctx),
                                     as.character(b$session_id), as.character(b$req_id),
                                     as.character(b$purpose %||% "execute"),
                                     as.character(b$text)),
                 error = function(e) NULL) else NULL
      instruction_id <- if (!is.null(reuse)) as.character(reuse$instruction_id)
                        else paste0("ins-", paste(sprintf("%02x", as.integer(openssl::rand_bytes(16))),collapse=""))
      private <- file.path(root, instruction_id)

      if (!is.null(reuse)) {
        # 复用已有包：文件已经在位且内容哈希已核对过，这里不再重写，
        #   也不注册 on.exit 删除 —— 那个包可能正被助手读着。
        path <- as.character(reuse$path)
        context_path <- as.character(reuse$context_path)
        message <- instruction_pointer_text(path, context_path, b$session_id, b$req_id,
                                           origin_lab, query_only)
        if (nchar(message, type = "chars") >= 950L) stop("instruction pointer too long")
        prepared <- list(ok = TRUE, path = path, context_path = context_path, message = message,
              instruction_id = instruction_id, context_verified = TRUE, instruction_verified = TRUE,
              stage = "ready", http_status = 200, reused = TRUE,
              session_id = b$session_id, req_id = b$req_id)
        prepared$queue_item <- tryCatch(
          inject_enqueue(text = message,
                         session = as.character(b$session %||% ""),
                         req_id = as.character(b$req_id %||% ""),
                         reason = "prepared",
                         step = as.character(b$step %||% ""),
                         analysis_id = as.character(b$session_id %||% ""),
                         purpose = as.character(b$purpose %||% "execute"),
                         instruction_id = instruction_id,
                         # 按钮名必须随入队一起存下去。
                         #   为什么（2026-11-23，云准助手交接件指出、本机核实）：
                         #   这里已经把 origin_lab 算出来并写进了正文首行，
                         #   却没传给 inject_enqueue —— 于是队列条目 origin 字段为空，
                         #   补发时 inject_with_origin 拿不到按钮名，无法重建成 【按钮名】 首行；
                         #   而正文里那行前缀又因为文本比对不进来而不走合并分支。
                         #   结果是同一条指令在不同路径下首行不一致。
                         origin = origin_lab),
          error = function(e) NULL)
        return(send_json(con, 200, prepared))
      }

      if (!dir.create(private, mode = "0700")) stop("private directory unavailable")
      prepare_stage <- "instruction_write"
      fp <- file.path(private, "instruction.txt")
      complete <- FALSE
      on.exit(if (!complete) unlink(private, recursive = TRUE), add = TRUE)
      old_mask <- Sys.umask("0077")
      tryCatch({
        con2 <- file(fp, open = "wb")
        tryCatch(writeBin(charToRaw(enc2utf8(b$text)), con2), finally = close(con2))
      }, finally = Sys.umask(old_mask))
      if (!isTRUE(Sys.chmod(fp, "0600"))) stop("private file permissions unavailable")
      prepare_stage <- "instruction_readback"
      expected <- charToRaw(enc2utf8(b$text))
      if (!identical(readBin(fp, "raw", n = length(expected) + 1L), expected))
        stop("instruction persistence mismatch")
      path <- normalizePath(fp, winslash = "/", mustWork = TRUE)
      prepare_stage <- "context_write"
      context_path <- file.path(private, "context.json")
      local_base <- paste0("http://127.0.0.1:", PORT)
      context <- list(
        session_id = b$session_id, req_id = b$req_id, purpose = b$purpose %||% "execute",
        instruction_id = instruction_id,
        output_coordination = output_claim,
        output_directory = output_claim$output_directory,
        output_policy = "所有执行产物只能写 output_directory（按 session/req 隔离）；共享工作根、旧产物与输入目录一律只读。正文若要求写共享路径，必须先改写到隔离目录；无法改写就报告阻塞，禁止执行。相同 attempt 不得并行重复执行，重试先检查已有进程。已发出的旧远端任务不受此协议控制，不承诺其不覆盖旧共享目录；这不阻止新 attempt 在隔离目录重试。",
        application_host = unname(Sys.info()[["nodename"]]),
        app_dir = APP_DIR,
        import_url = paste0(local_base, "/api/envcheck/import"),
        readback_url = paste0(local_base, "/api/attempts/result?session_id=", b$session_id, "&req_id=", b$req_id),
        handoff_path = if (query_only) NULL else handoff$path,
        instructions = paste0(
          if (query_only) "只读提问，不授权执行或回写；不需要回写凭据。" else
            "仅从已有handoff_path读取凭据，以X-Agent-Token请求头回写；禁止输出凭据或另行签发授权。",
          "这些URL仅可在application_host指定的应用运行主机请求。工作PC和目标服务器不能把其localhost当作应用；",
          "在其他机器执行时，把完整结果带回应用主机再回写，并用readback_url核对结果。",
          "先读本交接说明，再完整读取旁边instruction.txt，保持正文原样；不可读取时明确报错，不猜测。"))
      old_mask <- Sys.umask("0077")
      tryCatch({
        con3 <- file(context_path, open = "wb")
        tryCatch(writeBin(charToRaw(enc2utf8(as.character(jsonlite::toJSON(
          context, auto_unbox = TRUE, null = "null")))), con3), finally = close(con3))
      }, finally = Sys.umask(old_mask))
      if (!isTRUE(Sys.chmod(context_path, "0600"))) stop("private context permissions unavailable")
      prepare_stage <- "context_readback"
      context_bytes <- charToRaw(enc2utf8(as.character(jsonlite::toJSON(context, auto_unbox = TRUE, null = "null"))))
      if (!identical(readBin(context_path, "raw", n = length(context_bytes) + 1L), context_bytes))
        stop("context persistence mismatch")
      context_path <- normalizePath(context_path, winslash = "/", mustWork = TRUE)
      message <- instruction_pointer_text(path, context_path, b$session_id, b$req_id,
                                         origin_lab, query_only)
      if (nchar(message, type = "chars") >= 950L) stop("instruction pointer too long")
      manifest <- list(instruction_id=instruction_id, owner_key=owner_ctx_key(owner_ctx),
        session_id=b$session_id, req_id=b$req_id %||% "", purpose=b$purpose %||% "execute", text=b$text)
      sf <- file.path(private,"manifest.json")
      sess_write_json(sf,manifest); Sys.chmod(sf,"0600")
      if (!isTRUE(all.equal(sess_read_json(sf),manifest,check.attributes=FALSE))) stop("manifest readback mismatch")
      complete <- TRUE
      prepared <- list(ok = TRUE, path = path, context_path = context_path, message = message,
            instruction_id = instruction_id, context_verified = TRUE, instruction_verified = TRUE, stage = "ready", http_status = 200,
           session_id = b$session_id, req_id = b$req_id)
      # 入队放在服务端：浏览器可能在这一步之后就被刷新或关闭。
      #   2026-11-22 实测：指令包 10:09:55 生成成功，9 秒后页面重载，
      #   浏览器那次 queue 请求随文档一起销毁，队列里一条也没有，
      #   retryDue 因此没有可补发的条目 —— 会话里永远看不到这条指令。
      #   包已生成、owner/session/req/purpose 都已核验，这里入队不新增任何权限，
      #   只是把"已经决定要发出去"这件事落到盘上。inject_enqueue 按
      #   instruction_id 合并，浏览器之后再入队也不会产生重复条目。
      prepared$queue_item <- tryCatch(
        inject_enqueue(text = message,
                       session = as.character(b$session %||% ""),
                       req_id = as.character(b$req_id %||% ""),
                       reason = "prepared",
                       step = as.character(b$step %||% ""),
                       analysis_id = as.character(b$session_id %||% ""),
                       purpose = as.character(b$purpose %||% "execute"),
                       instruction_id = instruction_id,
                       # 与上面 reuse 分支同样补上按钮名。
                       #   两处是同一件事的两条路：一条复用已生成的包，一条新建。
                       #   只补一处会让"同一条指令首次入队有标题、复用后没标题"，
                       #   人看到的差别取决于服务端走了哪条分支，比一直错更难查。
                       origin = origin_lab),
        error = function(e) NULL)
      prepared
    }, error = function(e) list(ok = FALSE, code = paste0("instruction_", prepare_stage, "_failed"),
      stage = prepare_stage, http_status = 500, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(out$ok)) 200 else 500, out))
  }

  # ---- 待注入队列 ----------------------------------------------------
  #   页面送不进会话时把指令交到这里；页面每次活着都来问"有没有到点该补发的"。
  #   回写一到，对应条目自动销项，不再重复打扰。
  if (p == "/api/inject") {
    if (identical(req$method, "POST")) {
      b <- body_json()
      action <- as.character(b$action %||% "queue")
      if (identical(action, "queue")) {
        pkg <- instruction_package(APP_DIR, owner_ctx_key(owner_ctx), sess_chr(b$instruction_id))
        if (is.null(pkg) || !db_api_notebook_authorized(DB_CON, owner_ctx, sess_chr(pkg$session_id)) ||
            !identical(sess_chr(pkg$session_id), sess_chr(b$analysis_id)) ||
            !identical(sess_chr(pkg$req_id), sess_chr(b$req_id)) ||
            !identical(sess_chr(pkg$purpose), sess_chr(b$purpose %||% "execute")) ||
            nchar(sess_chr(b$text)) >= 950L)
          return(send_json(con, 409, list(ok=FALSE,code="instruction_prepared_package_required",stage="queue_validation",http_status=409,
            error="队列只接受本属主已核验的完整指令包及短指针。")))
        it <- inject_enqueue(text = as.character(b$text %||% ""),
                             session = as.character(b$session %||% ""),
                             req_id = as.character(b$req_id %||% ""),
                             reason = as.character(b$reason %||% ""),
                             step = as.character(b$step %||% ""),
                             analysis_id = as.character(b$analysis_id %||% ""),
                             purpose = as.character(b$purpose %||% "execute"),
                             instruction_id = as.character(b$instruction_id %||% ""),
                             origin = as.character(b$origin %||% ""))
        return(send_json(con, 200, list(ok = TRUE, item = it,
                                        retry_secs = INJECT_RETRY_SECS)))
      }
      if (identical(action, "attempt")) {
        ok <- inject_attempt(as.character(b$id %||% ""),
                             isTRUE(b$ok),
                             as.character(b$code %||% ""),
                             as.character(b$why %||% ""))
        return(send_json(con, 200, list(ok = TRUE, updated = ok)))
      }
      if (identical(action, "satisfy")) {
        n <- inject_satisfy(req_id = as.character(b$req_id %||% ""),
                            step = as.character(b$step %||% ""),
                            session = as.character(b$session %||% ""))
        return(send_json(con, 200, list(ok = TRUE, closed = n)))
      }
      # Preserve historical input; dates never revoke attempts or imply replacement.
      if (identical(action, "import_outbox")) {
        return(send_json(con, 200, list(ok=TRUE, imported=0L, superseded=0L,
          code="legacy_source_review_required", stage="source",
          error="旧outbox原文保留不变；须核验完整来源及分析绑定后才能迁入，不能按日期作废或从残文推断请求。")))
      }
      # ---- 整理队列：合并重复、补正会话号 --------------------------------
      #   为什么需要手动清一次：去重逻辑是这次才写进 inject_enqueue 的，
      #   之前重试几次就攒出几条一模一样的记录，已经落在盘上了。
      #   光靠新逻辑不会回头收拾旧的，所以给一个明确的整理动作。
      if (identical(action, "dedupe")) {
        d <- inject_read()
        seen <- list()
        out <- list()
        merged <- 0L
        fixed_session <- 0L
        for (it in d$items) {
          # 会话号补前缀：grp_5813 → cc_group_grp_5813
          s0 <- as.character(it$session %||% "")
          s1 <- inject_norm_session(s0)
          if (!identical(s0, s1)) {
            it$session <- s1
            fixed_session <- fixed_session + 1L
          }
          # 只对"未销项"的条目合并；已回写/已作废的原样保留作为留痕。
          if (!is.null(it$satisfied_at)) { out[[length(out) + 1L]] <- it; next }
          key <- paste(as.character(it$step %||% ""),
                       as.character(it$req_id %||% ""),
                       substr(as.character(it$text %||% ""), 1, 120),
                       sep = "\u0001")
          if (!is.null(seen[[key]])) {
            # 保留先建的那条（它的历史最长），把它没记全的字段补上，
            # 重试次数取两者之和 —— 次数代表"已经试了多少次"，合并时不能丢。
            keep <- seen[[key]]
            idx <- NULL
            for (j in seq_along(out)) {
              if (identical(as.character(out[[j]]$id %||% ""), as.character(keep$id %||% ""))) {
                idx <- j; break
              }
            }
            if (!is.null(idx)) {
              out[[idx]]$attempts <- as.integer(out[[idx]]$attempts %||% 0L) +
                as.integer(it$attempts %||% 0L)
              if (is.null(out[[idx]]$delivered_at) && !is.null(it$delivered_at))
                out[[idx]]$delivered_at <- it$delivered_at
              lastA <- as.character(out[[idx]]$last_attempt_at %||% "")
              lastB <- as.character(it$last_attempt_at %||% "")
              if (nzchar(lastB) && (!nzchar(lastA) || lastB > lastA))
                out[[idx]]$last_attempt_at <- lastB
              nxt <- as.character(out[[idx]]$next_attempt_at %||% "")
              nxt2 <- as.character(it$next_attempt_at %||% "")
              if (nzchar(nxt2) && (!nzchar(nxt) || nxt2 < nxt))
                out[[idx]]$next_attempt_at <- nxt2
              seen[[key]] <- out[[idx]]
              merged <- merged + 1L
            }
            next
          }
          out[[length(out) + 1L]] <- it
          seen[[key]] <- it
        }
        d$items <- out
        inject_write(d)
        return(send_json(con, 200, list(ok = TRUE, merged = merged,
                                        fixed_session = fixed_session,
                                        remaining = length(out))))
      }
      # ---- 作废一条：人判断这条已经不该再发时用 --------------------------
      #   为什么需要它：有些条目确实过期了（阶段已被后续动作取代、会话已换），
      #   但系统自己判断不出来。不给入口的话，它只会一直躺在队列里，
      #   或者一路重试到上限，白白占着"待补发"这个位置。
      #   作废是软删：标 satisfied 之外单独一个 dropped_at，留痕说明是谁什么时候不要的。
      if (identical(action, "drop")) {
        want <- as.character(b$id %||% "")
        d <- inject_read()
        n <- 0L
        for (i in seq_along(d$items)) {
          if (!identical(as.character(d$items[[i]]$id %||% ""), want)) next
          d$items[[i]]$dropped_at <- inject_ts(Sys.time())
          d$items[[i]]$next_attempt_at <- NULL
          d$items[[i]]$drop_reason <- as.character(b$reason %||% "人工作废")
          n <- n + 1L
        }
        if (n > 0L) inject_write(d)
        return(send_json(con, 200, list(ok = n > 0L, dropped = n)))
      }
      # ---- 彻底删除一条（不是作废，是从队列里移除） ----------------------
      if (identical(action, "delete")) {
        want <- as.character(b$id %||% "")
        d <- inject_read()
        before <- length(d$items)
        d$items <- Filter(function(it) !identical(as.character(it$id %||% ""), want), d$items)
        n <- before - length(d$items)
        if (n > 0L) inject_write(d)
        return(send_json(con, 200, list(ok = n > 0L, deleted = n)))
      }
      # ---- 助手取走一条 --------------------------------------------------
      #   为什么要这个动作：平台面板不发握手时，compose 与任务通道都建不起来，
      #   但指令已经落在本机队列里。助手（bot）每轮读一次队列，就能把这条取走
      #   执行 —— 对用户来说，指令确实进了会话，只是走的是"队列"这条路。
      #   取走要留痕，否则页面只能显示"还没送出去"，看着像永远没进展。
      if (identical(action, "claim")) {
        want <- as.character(b$id %||% "")
        who <- as.character(b$by %||% "assistant")
        d <- inject_read()
        out <- NULL
        stale_why <- ""
        for (i in seq_along(d$items)) {
          it <- d$items[[i]]
          if (!identical(as.character(it$id %||% ""), want)) next
          if (!is.null(it$dropped_at)) next
          # 接管前重查：due 是一次读，这段时间里执行端可能已经把任务接走，
          #   或者这一步已经被验收。runner 拿到旧 due 直接启动会话，
          #   等于同一件事又有第二个执行者。
          #   生产 runner 只对非 2xx 停下，所以过时与读不到都必须返回非 2xx。
          if (HAS_EXEC_GUARD) {
            ck <- inject_claim_check(it)
            if (!isTRUE(ck$ok)) {
              code_n <- if (identical(as.integer(ck$http %||% 409L), 503L)) 503L else 409L
              if (code_n == 409L) {
                d$items[[i]]$dropped_at <- inject_ts(Sys.time())
                d$items[[i]]$drop_reason <- as.character(ck$reason %||% "")
                d$items[[i]]$next_attempt_at <- NULL
              }
              inject_write(d)
              tryCatch({
                f <- file.path(STATE_DIR, "inject_stale_close.jsonl")
                cat(paste0(jsonlite::toJSON(list(
                  at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                  kind = if (code_n == 409L) "stale_at_claim" else "guard_unavailable_at_claim",
                  id = want, code = as.character(ck$code %||% ""),
                  reason = as.character(ck$reason %||% ""), dispatched = FALSE,
                  wrote_step_state = FALSE), auto_unbox = TRUE), "\n"),
                  file = f, append = TRUE)
              }, error = function(e) NULL)
              return(send_json(con, code_n, list(
                ok = FALSE, code = as.character(ck$code %||% ""), id = want,
                reason = as.character(ck$reason %||% ""),
                dispatched = FALSE, wrote_step_state = FALSE,
                error = as.character(ck$reason %||% "这条通知已过时，不再派工。"))))
            }
          }
          d$items[[i]]$claimed_at <- inject_ts(Sys.time())
          d$items[[i]]$claimed_by <- who
          # 取走就撤掉"下次重试"：留着它，面板上会同时出现
          #   "助手已取走"和"下次 08:32"两句互相打架的话。
          d$items[[i]]$next_attempt_at <- NULL
          out <- d$items[[i]]
        }
        if (nzchar(stale_why)) {
          inject_write(d)
          tryCatch({
            f <- file.path(STATE_DIR, "inject_stale_close.jsonl")
            cat(paste0(jsonlite::toJSON(list(
              at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), kind = "stale_at_claim",
              id = want, reason = stale_why, dispatched = FALSE,
              wrote_step_state = FALSE), auto_unbox = TRUE), "\n"),
              file = f, append = TRUE)
          }, error = function(e) NULL)
          return(send_json(con, 409, list(
            ok = FALSE, code = "inject_stale_at_claim", id = want,
            reason = stale_why, dispatched = FALSE, wrote_step_state = FALSE,
            error = "这条通知已经过时，不再派工；已软关闭。")))
        }
        if (!is.null(out)) inject_write(d)
        return(send_json(con, 200, list(ok = !is.null(out), item = out)))
      }
      # ---- 人工重试一条：重新开始计数 ------------------------------------
      #   为什么要"重新计数"而不是再加一次：到上限的条目已经停自动重试了，
      #   人点"再试一次"的意思是"通道我修好了，请从现在起重来"。
      #   照旧只 +1 的话，点完立刻又回到超限状态，次数还会一路涨到 8、9 次，
      #   面板上就出现"试了 8 次"这种和上限 6 对不上的数字。
      if (identical(action, "retry")) {
        want <- as.character(b$id %||% "")
        d <- inject_read()
        n <- 0L
        for (i in seq_along(d$items)) {
          it <- d$items[[i]]
          if (!identical(as.character(it$id %||% ""), want)) next
          if (!is.null(it$satisfied_at)) next
          d$items[[i]]$attempts <- 0L
          d$items[[i]]$last_code <- NULL
          d$items[[i]]$last_why <- NULL
          d$items[[i]]$manual_retry_at <- inject_ts(Sys.time())
          d$items[[i]]$manual_retry_n <- as.integer(it$manual_retry_n %||% 0L) + 1L
          d$items[[i]]$dropped_at <- NULL
          d$items[[i]]$drop_reason <- NULL
          d$items[[i]]$claimed_at <- NULL
          d$items[[i]]$claimed_by <- NULL
          # 立刻可试：页面拿到这条就会马上交给通道，成败由随后的 attempt 记。
          d$items[[i]]$next_attempt_at <- inject_ts(Sys.time())
          n <- n + 1L
        }
        if (n > 0L) inject_write(d)
        return(send_json(con, 200, list(ok = n > 0L, reset = n,
                                        max_attempts = MAX_INJECT_ATTEMPTS)))
      }
      # ---- 通道诊断留痕：页面自己说清"哪一条不通、为什么" ------------------
      #   为什么要写进服务端：注入失败发生在浏览器里，服务端看不见。
      #   只看"没连上"三个字，分不清是没 SDK、独立标签页、还是宿主没握手；
      #   尤其父页地址（referrer）只有在页面里才拿得到，而它正是判断
      #   "宿主域是不是白名单里那几个"的唯一依据。
      if (identical(action, "diag")) {
        f <- file.path(STATE_DIR, "inject_diag.jsonl")
        rec <- list(at = inject_ts(Sys.time()),
                    referrer = as.character(b$referrer %||% ""),
                    parent_origin = as.character(b$parent_origin %||% ""),
                    in_frame = isTRUE(b$in_frame),
                    has_sdk = isTRUE(b$has_sdk),
                    has_instance = isTRUE(b$has_instance),
                    has_bridge = isTRUE(b$has_bridge),
                    task_handshake = isTRUE(b$task_handshake),
                    code = as.character(b$code %||% ""),
                    why = as.character(b$why %||% ""),
                    ua = as.character(b$ua %||% ""),
                    # 下面这几项是判定"平台面板到底有没有把这次打开绑到会话上"
                    # 的关键证据，必须落盘：
                    #   ancestors   祖先链 origin（Referer 被清空时唯一的来源）
                    #   parent_msgs 父页发来的消息（type+origin），一条都没有
                    #               就说明宿主从未开口，而不是"消息被白名单丢了"
                    #   url_query_keys / url_has_open_ref 平台有没有下发绑定票据
                    ancestors = as.character(b$ancestors %||% ""),
                    parent_msgs = as.character(b$parent_msgs %||% ""),
                    url_path = as.character(b$url_path %||% ""),
                    url_query_keys = as.character(b$url_query_keys %||% ""),
                    url_has_open_ref = isTRUE(b$url_has_open_ref),
                    doc_origin = as.character(b$doc_origin %||% ""),
                    is_secure = isTRUE(b$is_secure))
        con2 <- file(f, open = "a", encoding = "UTF-8")
        writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), con2, useBytes = TRUE)
        close(con2)
        try(Sys.chmod(f, mode = "0600"), silent = TRUE)
        return(send_json(con, 200, list(ok = TRUE)))
      }
      return(send_json(con, 400, list(ok = FALSE, error = "未知 action")))
    }
    d <- inject_read()
    due <- inject_due()
    now <- inject_ts(Sys.time())
    brief <- lapply(d$items, function(it) {
      att <- as.integer(it$attempts %||% 0L)
      sat <- !is.null(it$satisfied_at)
      drp <- !is.null(it$dropped_at)
      # 摘要：面板上不能只有"(未标阶段)"，三条长得一模一样，人没法判断该留哪条。
      #   取正文里"要办的事"那一行，没有就取正文开头。
      raw <- gsub("[\r\n]+", " ", as.character(it$text %||% ""))
      m <- regmatches(raw, regexpr("要办的事：[^ ]+( [^ ]+){0,20}", raw))
      head <- if (length(m)) m[1] else substr(raw, 1, 60)
      list(id = it$id, step = it$step, session = it$session, req_id = it$req_id,
           attempts = att, created_at = it$created_at,
           last_attempt_at = it$last_attempt_at, last_code = it$last_code,
           last_why = it$last_why %||% "",
           next_attempt_at = it$next_attempt_at,
           delivered = !is.null(it$delivered_at),
           delivered_at = it$delivered_at,
           satisfied = sat,
           dropped = drp,
           dropped_at = it$dropped_at,
           claimed = !is.null(it$claimed_at),
           claimed_at = it$claimed_at,
           head = head,
           # 已达上限：还在队列里、没回写、但不会再自动重试了。
           #   这个状态必须单独标出来 —— 否则面板上它和"等回写"长得一样，
           #   人以为系统还在努力，实际上早就停了。
           exhausted = (!sat && !drp && att >= MAX_INJECT_ATTEMPTS))
    })
    # pending 的判据：还没回写、没作废、且还没到上限 —— 也就是"系统还会自动再试"的那几条。
    #   不能只用"没投递过"来算：投递成功但没回写，仍然要重试，
    #   那种情况下待补发数会恒为 0，页面上的提示跟着消失，等于假装没事。
    pending_n <- sum(vapply(brief, function(x)
      !isTRUE(x$satisfied) && !isTRUE(x$exhausted) && !isTRUE(x$dropped), TRUE))
    exhausted_n <- sum(vapply(brief, function(x) isTRUE(x$exhausted), TRUE))
    dropped_n <- sum(vapply(brief, function(x) isTRUE(x$dropped), TRUE))
    return(send_json(con, 200, list(
      ok = TRUE, now = now, retry_secs = INJECT_RETRY_SECS,
      max_attempts = MAX_INJECT_ATTEMPTS,
      pending = pending_n,
      exhausted = exhausted_n,
      dropped = dropped_n,
      due = length(due), items = brief)))
  }

  # ---- 执行队列（给工作电脑上的执行端用） ----
  if (p == "/api/exec/due") {
    # 同时报两类：待执行的、正在执行的。
    #   原来只报待执行，于是"没有待执行"和"我手上还有活没干完"在执行端看起来一样，
    #   结果是作业跑完没人收尾、中途被杀也没人发现。现在两类都给，
    #   由执行端自己决定先做哪个（有待执行先做，没有就去盯正在执行的）。
    # 先回收失联任务，再报队列 —— 否则执行端会拿到一条"正在跑"的死任务。
    tryCatch(exec_reap(), error = function(e) NULL)
    dq <- exec_due()
    dr <- exec_running()
    # 每条任务带上它自己那条分析记录的 run_dir。
    #   为什么必须按 session 解析：执行端原来只能拿全局 place.work_root，
    #   于是 A 记录的任务去数 B 记录的产物目录，报出来的 files=14 是别人的文件。
    #   取不到就留空串，由执行端保守处理（推心跳并报错），绝不回退全局目录 ——
    #   回退看起来能跑，实际是把结果算到了另一条记录头上。
    due_run_dir <- function(sid) {
      s0 <- as.character(sid %||% "")
      if (!nzchar(s0) || !DB_READY) return("")
      rd <- tryCatch(db_scalar(DB_CON,
        "SELECT run_dir FROM sessions WHERE session_id = ?", list(s0)),
        error = function(e) NA_character_)
      if (length(rd) != 1L || is.na(rd)) return("")
      record_trim(rd)
    }
    mk <- function(it, kind) list(
      id = it$id, step = it$step, session_id = it$session_id,
      req_id = it$req_id, chain_id = it$chain_id,
      created_at = it$created_at, kind = kind,
      claimed_at = it$claimed_at, last_seen = it$last_seen,
      job_id = it$job_id,
      worker_id = as.character(it$worker_id %||% it$claimed_by %||% ""),
      pool_id = exec_task_pool(it),
      # 唯一数字作业号随派工下发：复用分支不再重跑作业，
      #   但它仍要把"这份产物属于哪次作业"带给判定层，
      #   而 worker 在复用路径上没有 submit 返回值，只能从这里取。
      job_number = as.character(it$job_number %||% ""),
      # 这一次尝试的身份，必须随任务下发。
      #   为什么（2026-11-23，子任务查证）：attempt_id 由服务端在派工时生成、
      #   也写进了队列条目，但 due/claim 两条下发路径都不带它 ——
      #   执行端回写时根本没有这个值，服务端只能靠 req_id 去猜，
      #   猜不中就判 attempt_id_unknown，19 条「回写未被接受」就是这么来的。
      #   作业在集群上真跑完了，结果却写不回来。
      attempt_id = as.character(it$attempt_id %||% ""),
      version_id = as.character(it$version_id %||% ""),
      run_id = as.character(it$run_id %||% ""),
      executor_mode = as.character(it$executor_mode %||% ""),
      run_dir = if (nzchar(as.character(it$run_dir %||% "")))
        as.character(it$run_dir) else due_run_dir(it$session_id),
      mode = as.character(it$mode %||% ""),
      reference_id = as.character(it$reference_id %||% ""),
      ref_mode = as.character(it$ref_mode %||% ""),
      ref_path = as.character(it$ref_path %||% ""),
      # 命令随任务给出，执行端照跑即可。
      script = as.character(it$script %||% ""),
      script_args = as.character(it$script_args %||% ""),
      # 结构参数必须随任务下发。
      #   实测后果：只在 exec 记录里存了 params，due 不返回，
      #   执行端 write_params 拿到 None，参数文件根本不会写，
      #   作业就在「缺参数」或守卫那一步失败。
      params = it$params %||% NULL,
      params_rel = as.character(it$params_rel %||% ""))
    out <- c(lapply(dq, mk, kind = "queued"), lapply(dr, mk, kind = "running"))
    return(send_json(con, 200, list(
      ok = TRUE, count = length(out),
      queued = length(dq), running = length(dr),
      items = out)))
  }
  # 心跳：执行端报"这个任务还在跑"，只推 last_seen，不改 status。
  #   判"失联"看的是 last_seen 是否过久，而不是"等了多久"。
  if (p == "/api/exec/keepalive" && identical(req$method, "POST")) {
    b <- body_json()
    id <- as.character(b$id %||% "")
    if (!nzchar(id)) return(send_json(con, 400, list(ok = FALSE, error = "缺 id")))
    claimed_w <- as.character(b$worker_id %||% b$by %||% "")
    wa <- worker_route_auth(req, claimed_w, as.character(b$pool_id %||% ""), "queue")
    if (!isTRUE(wa$ok)) return(send_json(con, wa$status, wa))
    task0 <- Filter(function(it) identical(as.character(it$id %||% ""), id),
                    exec_read()$items %||% list())
    if (!length(task0))
      return(send_json(con, 404, list(ok = FALSE, code = "exec_task_missing",
                                      error = "没有这条执行任务。")))
    owner_w <- trimws(as.character(task0[[1]]$worker_id %||% task0[[1]]$claimed_by %||% ""))
    if (nzchar(owner_w) && !identical(owner_w, wa$worker_id))
      return(send_json(con, 403, list(ok = FALSE, code = "exec_task_not_owned",
        error = "这条任务由另一个执行端领取，不能由当前 worker 续命。")))
    # 注意：这里必须用 %||% 而不是 ||。
    #   R 的 || 只吃逻辑值，喂它 NULL / 字符串会直接抛
    #   "invalid 'x' type in 'x || y'" —— 端点看着在，一调就 500。
    jobv <- as.character(b$job %||% "")
    if (!nzchar(jobv)) jobv <- as.character(b$job_id %||% "")
    mg0 <- exec_keepalive(id, job = jobv, note = as.character(b$note %||% ""),
                          job_number = as.character(b$job_number %||% ""),
                          worker_id = wa$worker_id, pool_id = wa$pool_id)
    if (!isTRUE(mg0$ok) && identical(as.character(mg0$code), "jobnumber_conflict"))
      return(send_json(con, 409, list(
        ok = FALSE, code = "job_number_conflict",
        kept = as.character(mg0$value %||% ""),
        rejected = as.character(b$job_number %||% ""),
        error = "该执行任务已登记另一个作业号，拒绝改写。")))
    if (isTRUE(DB_READY) && exists("db_attempt_note_job_for_exec", mode = "function")) {
      jr <- tryCatch(
        db_attempt_note_job_for_exec(
          DB_CON, task0[[1]], job_number = as.character(b$job_number %||% ""),
          exec_id = id, executor_mode = as.character(b$executor_mode %||% "")),
        error = function(e) list(ok = TRUE, code = "job_binding_skipped"))
      if (!isTRUE(jr$ok))
        return(send_json(con, 409, c(list(ok = FALSE, at = exec_ts()), jr)))
    }
    # Long submit/watch paths stay inside handle(); a fresh task timestamp alone
    # would still let the worker be judged offline. Keep both clocks moving.
    exec_worker_hb_write(wa$worker_id, wa$pool_id,
                         version = as.character(b$version %||% ""),
                         note = "keepalive")
    # 派工前脚本基线：执行端在提交 qsub 之前读一次脚本的路径/mtime/md5 存下来，
    #   收尾时再读一次比对。只有这样才能回答「运行期间脚本变没变」。
    #   为什么在这里收：keepalive 是执行端提交作业那一刻唯一会调的上报口，
    #   换一个新端点等于多一条要维护的链路，而这条数据本来就属于这次执行。
    #   只接受绝对路径 + 32 位 md5 + epoch 秒，三项齐全才落盘，缺一律不存。
    bs <- b$script_baseline %||% NULL
    if (is.list(bs)) {
      bsp <- as.character(bs$script_path %||% "")
      bsm <- tolower(as.character(bs$script_md5 %||% bs$md5 %||% ""))
      bse <- suppressWarnings(as.numeric(as.character(bs$script_mtime_epoch %||% "")))
      if (nzchar(bsp) && grepl("^/", bsp) && grepl("^[0-9a-f]{32}$", bsm) &&
          !is.na(bse) && bse > 0) {
        exec_set(id, list(script_baseline = list(
          script_path = bsp, script_md5 = bsm,
          script_mtime_epoch = as.numeric(bse),
          captured_at = exec_ts(), captured_by = as.character(b$by %||% "worker"))))
      }
    }
    return(send_json(con, 200, list(ok = TRUE, at = exec_ts(),
                                    worker_id = wa$worker_id, pool_id = wa$pool_id)))
  }
  if (p == "/api/exec/claim" && identical(req$method, "POST")) {
    b <- body_json()
    want <- as.character(b$id %||% "")
    claimed_w <- as.character(b$worker_id %||% b$by %||% "")
    wa <- worker_route_auth(req, claimed_w, as.character(b$pool_id %||% ""), "queue")
    if (!isTRUE(wa$ok)) return(send_json(con, wa$status, wa))
    who <- wa$worker_id
    # 领取必须"判断 + 占用"是一个原子动作。
    #   为什么（2026-11-23 实测）：原来读在锁外、写在锁内，两个执行端同时读到
    #   同一条 queued，两边都通过判断、都回报"已领到"，实际只有一个能占住，
    #   另一个拿着同一份任务去跑 —— 同一条任务跑两遍，产物写同一个目录。
    #   现在把判断挪进锁内：锁内重读，先确认它仍是 queued，再占用。
    claim <- exec_mutate(function(d) {
      it0 <- NULL
      for (it in (d$items %||% list()))
        if (identical(as.character(it$id %||% ""), want)) it0 <- it
      if (is.null(it0))
        return(list(data = d, value = list(ok = FALSE, code = "exec_task_missing",
          error = "队列里没有这条任务。")))
      st0 <- as.character(it0$status %||% "")
      if (!identical(st0, "queued")) {
        mine <- identical(trimws(as.character(it0$worker_id %||% it0$claimed_by %||% "")), who)
        if (identical(st0, "running") && mine)
          return(list(data = d, value = list(ok = TRUE, idempotent = TRUE, item = it0)))
        # 另一个执行端先一步占住了：明确告诉它，别让它以为领到了。
        return(list(data = d, value = list(ok = FALSE, code = "exec_task_taken",
          status = st0,
          by = as.character(it0$worker_id %||% it0$claimed_by %||% ""),
          error = paste0("这条任务已被 ", st0, " 状态占用，不是你可以接的。" ))))
      }
      task_pool0 <- exec_task_pool(it0)
      if (!identical(task_pool0, wa$pool_id))
        return(list(data = d, value = list(ok = FALSE, code = "worker_pool_mismatch_queue",
          error = paste0("任务属于 pool ", task_pool0, "，当前 worker 属于 ", wa$pool_id, "。"))))
      now0 <- exec_ts()
      for (i in seq_along(d$items)) {
        if (!identical(as.character(d$items[[i]]$id %||% ""), want)) next
        d$items[[i]]$status <- "running"
        d$items[[i]]$claimed_at <- now0
        d$items[[i]]$last_seen <- now0
        d$items[[i]]$claimed_by <- who
        d$items[[i]]$worker_id <- who
        d$items[[i]]$pool_id <- wa$pool_id
      }
      list(data = d, value = list(ok = TRUE, idempotent = FALSE, item = it0,
                                  task_pool = task_pool0))
    })
    if (!isTRUE(claim$ok)) {
      # 幂等命中与"被别人领走"都是 200，前者是正常重试，后者要让调用方看见原因。
      if (isTRUE(claim$idempotent))
        return(send_json(con, 200, list(ok = TRUE, idempotent = TRUE, item = claim$item)))
      if (identical(as.character(claim$code %||% ""), "worker_pool_mismatch_queue"))
        return(send_json(con, 409, claim))
      return(send_json(con, 200, claim))
    }
    # 同一个 worker 重复领同一条：这是重试，不是新领到。
    #   必须在这里返回，否则会落到下面的"领取成功"分支，
    #   把幂等命中报成一次新的成功领取（2026-11-23 路由实测踩到）。
    if (isTRUE(claim$idempotent))
      return(send_json(con, 200, list(ok = TRUE, idempotent = TRUE, item = claim$item)))
    hit <- claim$item
    task_pool <- claim$task_pool
    exec_worker_hb_write(wa$worker_id, wa$pool_id,
                         version = as.character(b$version %||% ""), note = "claim")
    return(send_json(con, 200, list(
      ok = TRUE,
      item = list(id = hit$id, step = hit$step, session_id = hit$session_id,
                  req_id = hit$req_id, chain_id = hit$chain_id,
                  job_id = as.character(hit$job_id %||% ""),
                  job_number = as.character(hit$job_number %||% ""),
                  # 与 due 同源：领取时也要把这一次尝试的身份交给执行端，
                  #   否则它回写时还是缺这个值（2026-11-23 修）。
                  attempt_id = as.character(hit$attempt_id %||% ""),
                  version_id = as.character(hit$version_id %||% ""),
                  run_id = as.character(hit$run_id %||% ""),
                  executor_mode = as.character(hit$executor_mode %||% ""),
                  run_dir = as.character(hit$run_dir %||% ""),
                  pool_id = task_pool, worker_id = who,
                  script = as.character(hit$script %||% ""),
                  script_args = as.character(hit$script_args %||% "")))))
  }
  # 退回队列：执行端发现作业还在跑（产出会继续增长），先放回来等下一轮，
  #   避免把半截结果当最终结果写回。
  if (p == "/api/exec/requeue" && identical(req$method, "POST")) {
    b <- body_json()
    want <- as.character(b$id %||% "")
    wa <- worker_route_auth(req, as.character(b$worker_id %||% b$by %||% ""),
                            as.character(b$pool_id %||% ""), "queue")
    if (!isTRUE(wa$ok)) return(send_json(con, wa$status, wa))
    chain_task <- Filter(function(it) identical(it$id, want), exec_read()$items %||% list())
    if (!length(chain_task))
      return(send_json(con, 404, list(ok = FALSE, code = "exec_task_missing")))
    task_w <- trimws(as.character(chain_task[[1]]$worker_id %||%
                                  chain_task[[1]]$claimed_by %||% ""))
    if (nzchar(task_w) && !identical(task_w, wa$worker_id))
      return(send_json(con, 403, list(ok = FALSE, code = "exec_task_not_owned",
        error = "这条任务由另一个执行端领取，不能由当前 worker 退回队列。")))
    if (length(chain_task) && nzchar(as.character(chain_task[[1]]$chain_id %||% ""))) {
      it <- chain_task[[1]]; c <- rc_read(it$session_id)
      if (is.null(c) || !identical(c$status, "running") ||
          !identical(it$step, c$todo[[c$index]]) || isTRUE(it$callback_received))
        return(send_json(con, 409, list(ok = FALSE, code = "chain_stage_not_active")))
    }
    exec_set(want, list(status = "queued", claimed_at = NULL, claimed_by = NULL,
                        worker_id = NULL, requeued_at = exec_ts()))
    exec_worker_hb_write(wa$worker_id, wa$pool_id, note = "requeue")
    return(send_json(con, 200, list(ok = TRUE, worker_id = wa$worker_id,
                                    pool_id = wa$pool_id)))
  }
  if (p == "/api/exec/done" && identical(req$method, "POST")) {
    b <- body_json()
    want <- as.character(b$id %||% "")
    okk <- isTRUE(b$ok)
    wa <- worker_route_auth(req, as.character(b$worker_id %||% b$by %||% ""),
                            as.character(b$pool_id %||% ""), "queue")
    if (!isTRUE(wa$ok)) return(send_json(con, wa$status, wa))
    task <- Filter(function(it) identical(it$id, want), exec_read()$items %||% list())
    if (!length(task))
      return(send_json(con, 404, list(ok = FALSE, code = "exec_task_missing")))
    task_w <- trimws(as.character(task[[1]]$worker_id %||% task[[1]]$claimed_by %||% ""))
    if (nzchar(task_w) && !identical(task_w, wa$worker_id))
      return(send_json(con, 403, list(ok = FALSE, code = "exec_task_not_owned",
        error = "这条任务由另一个执行端领取，不能由当前 worker 收尾。")))
    bind_w <- if (nzchar(task_w)) list() else list(worker_id = wa$worker_id)
    # 执行端这次上报的尝试身份，写回队列条目。
    #   为什么（2026-11-23，子任务查证）：attempt_id 是服务端派工时生成的，
    #   以前只在派工那一次写进队列，收尾时既不校验也不更新 ——
    #   执行端带回来的值如果和队列不一致，没有任何地方会说话。
    #   现在把它落回条目，队列与库能对上，也能在事后看出是哪一次尝试。
    att_in_ep <- trimws(as.character(b$attempt_id %||% ""))
    bind_att <- if (nzchar(att_in_ep)) list(attempt_id = att_in_ep) else list()
    if (length(task) && nzchar(as.character(task[[1]]$chain_id %||% ""))) {
      it <- task[[1]]
      if (okk && (!isTRUE(it$callback_received) || !isTRUE(it$callback_accepted)))
        return(send_json(con, 409, list(ok = FALSE, code = "stage_callback_required")))
      exec_set(want, c(bind_w, bind_att, list(status = if (okk) "done" else "failed",
        finished_at = exec_ts(), error = if (okk) NULL else as.character(b$error %||% ""))))
      exec_worker_hb_write(wa$worker_id, wa$pool_id, note = "done")
      if (!okk) rc_callback(list(session_id = it$session_id, req_id = it$chain_id,
        step = it$step, status = "failed", code = 1L), FALSE)
      return(send_json(con, 200, list(ok = TRUE)))
    }
    exec_set(want, c(bind_w, bind_att, list(status = if (okk) "done" else "failed",
                        finished_at = exec_ts(),
                        error = if (okk) NULL else as.character(b$error %||% ""))))
    exec_worker_hb_write(wa$worker_id, wa$pool_id, note = "done")
    return(send_json(con, 200, list(ok = TRUE, worker_id = wa$worker_id,
                                    pool_id = wa$pool_id)))
  }
  # 执行端上报一次心跳：它每 60 秒来一次，说明它活着。
  if (p == "/api/exec/heartbeat" && identical(req$method, "POST")) {
    b <- body_json()
    claimed_w <- as.character(b$worker_id %||% b$by %||% "")
    wa <- worker_route_auth(req, claimed_w, as.character(b$pool_id %||% ""), "heartbeat")
    if (!isTRUE(wa$ok)) return(send_json(con, wa$status, wa))
    hb <- exec_worker_hb_write(wa$worker_id, wa$pool_id,
                               version = as.character(b$version %||% ""),
                               note = as.character(b$note %||% ""))
    worker_audit(APP_DIR, "heartbeat", list(worker_id = wa$worker_id,
                                             pool_id = wa$pool_id, at = hb$at))
    return(send_json(con, 200, list(ok = TRUE, worker_id = wa$worker_id,
                                    pool_id = wa$pool_id, at = hb$at)))
  }

  # -------------------------------------------------------------------
  #  服务器引擎快照上报
  #
  #  为什么必须补这一段（2026-11-03，审核第 10 条）
  #    contract.R 里有一条检查：服务器上那份 _engine 到底是什么版本？
  #    它读 state/server_engine_snapshot.json，读不到就判 unknown，
  #    并写一句「执行端跑完会带一次快照回来」。
  #    问题在于：全仓只有"读"，没有任何地方"写"，也没有这个端点 ——
  #    那个"会带回来"的承诺从来没有实现过。
  #    于是这条永远 unknown、contract 永远 ok=false，而它想回答的问题
  #    （服务器那份和应用这份是不是同一版）永远没有答案。
  #
  #  上报什么
  #    chain_order：服务器 _engine 认得的阶段名，用来判"它认不认识本流程"
  #    file_md5  ：关键文件的指纹，用来判"是不是同一份"
  #    readable  ：能不能读到，读不到要说为什么 —— 读不到 != 一致
  #  这些都只增不改：执行端报什么就存什么，应用侧不去猜、不去补。
  # -------------------------------------------------------------------
  if (p == "/api/engine/snapshot" && identical(req$method, "POST")) {
    b <- body_json()
    if (is.null(b))
      return(send_json(con, 400, list(ok = FALSE, error = "body 不是合法 JSON")))
    snap <- list(
      at = exec_ts(),
      # 这份上报属于哪条分析记录。
      #   为什么必须记：这个文件是全局单文件，写入是整份覆盖。
      #   不记归属的话，一条记录失败的结果会一直挂在别的记录页面上 ——
      #   实测就是这样：bta0 那次 Connection refused 的快照，
      #   让新记录 s-…-g5nq 在体检 ready 的情况下还显示"无法执行"。
      #   老文件没有这个字段，读取端一律当"不属于本记录"，不拿它下结论。
      session_id = as.character(b$session_id %||% ""),
      req_id = as.character(b$req_id %||% ""),
      by = as.character(b$by %||% "worker"),
      host = as.character(b$host %||% ""),
      engine_dir = as.character(b$engine_dir %||% ""),
      readable = isTRUE(b$readable),
      why = as.character(b$why %||% ""),
      chain_order = as.list(as.character(unlist(b$chain_order %||% character(0)))),
      files = (b$files %||% list()))
    write_json_file(file.path(STATE_DIR, "server_engine_snapshot.json"), snap)
    return(send_json(con, 200, list(ok = TRUE, at = snap$at,
                                    stages = length(snap$chain_order))))
  }
  # ---- 后台在不在动：一条接口回答清楚 ------------------------------
  #   为什么需要（2026-11-06）：主循环里那段注释一直写着"会回 last_sweep_at 与
  #   sweeps"，但这个端点从来不存在（请求返回 Not found）。于是"后台有没有在
  #   推进"只能靠翻日志，人关掉页面回来时无法判断这段时间它到底动没动。
  #
  #   这里一次给三件事：
  #     exec     —— 执行端（工作电脑上的 worker）在不在线、队列几条在等、几条在跑
  #     poller   —— 应用侧调度器最后一次推进的时间
  #     agent    —— 会话指令队列里还有多少条没投递
  #   都取自磁盘上的真实状态，不做乐观估计。
  if (p == "/api/poller/status") {
    hbs <- tryCatch(exec_worker_hb_all(), error = function(e) list())
    hb <- exec_latest_heartbeat(hbs = hbs)
    hbAt <- as.character((hb %||% list())$at %||% "")
    online <- exec_heartbeat_online(hb)
    ej <- tryCatch(exec_read(), error = function(e) list(items = list()))
    eitems <- ej$items %||% list()
    cnt <- function(st) sum(vapply(eitems, function(it)
      identical(as.character(it$status %||% ""), st), TRUE))
    inj <- tryCatch(inject_read(), error = function(e) list(items = list()))
    iitems <- inj$items %||% list()
    ipending <- sum(vapply(iitems, function(it)
      is.null(it$satisfied_at) && is.null(it$dropped_at), TRUE))
    last <- ""
    lastRun <- ""
    lastCount <- ""
    if (DB_READY) {
      last <- tryCatch(as.character(db_meta_get(DB_CON, "last_sweep_at") %||% ""),
                       error = function(e) "")
      # poller_last_run / poller_last_count 由 engine/poller.R 每轮写入，
      #   不管这一轮有没有领到作业。所以"调度活着"与"调度有活干"是两件事，
      #   页面上要能分别看到 —— 只看 last_sweep_at 的话，队列一闲就永远是 null。
      lastRun <- tryCatch(as.character(db_meta_get(DB_CON, "poller_last_run") %||% ""),
                          error = function(e) "")
      lastCount <- tryCatch(as.character(db_meta_get(DB_CON, "poller_last_count") %||% ""),
                            error = function(e) "")
    }
    alive <- FALSE
    if (nzchar(lastRun)) {
      t1 <- suppressWarnings(as.POSIXct(lastRun, format = "%Y-%m-%dT%H:%M:%S"))
      if (!is.na(t1)) alive <- as.numeric(difftime(Sys.time(), t1, units = "secs")) < 600
    }
    return(send_json(con, 200, list(
      ok = TRUE,
      exec = list(online = online, at = if (nzchar(hbAt)) hbAt else NULL,
                  version = as.character((hb %||% list())$version %||% ""),
                  queued = cnt("queued"), running = cnt("running"),
                  done = cnt("done"), failed = cnt("failed")),
      poller = list(last_sweep_at = if (nzchar(last)) last else NULL,
                    last_run_at = if (nzchar(lastRun)) lastRun else NULL,
                    last_run_count = if (nzchar(lastCount)) lastCount else NULL,
                    alive = alive),
      agent = list(pending = ipending))))
  }

  if (p == "/api/engine/snapshot") {
    sn <- tryCatch(read_json_file(file.path(STATE_DIR, "server_engine_snapshot.json")),
                   error = function(e) NULL)
    if (is.null(sn))
      return(send_json(con, 200, list(ok = FALSE, present = FALSE,
        error = "还没有上报过：服务器那份引擎与应用这份是否同一版，目前属未知，不能当一致。",
        how = "POST /api/engine/snapshot，body 带 readable / chain_order / files")))
    return(send_json(con, 200, list(ok = TRUE, present = TRUE, snapshot = sn)))
  }

  # ---- 作废一条排队中的任务：人判断它不会有人接了 ---------------------
  #   为什么必须有这个入口（2026-11-22，gina 报队列死锁）：
  #     一条没人认领的 queued 任务会让 run_chain 的
  #     「旧任务仍在执行或排队」409 一直成立，新的环节派不出去；
  #     而页面上此前没有任何地方能把这条清掉 —— 只能等人去改文件。
  #     于是整条分析锁死，且从界面上看不出是"卡在一条死任务上"。
  #   边界：
  #     · 只允许作废本记录、且仍是 queued 的任务。
  #     · running 一律不许（那是在跑，作废等于骗自己它没跑）。
  #     · requeue 过、worker 可能正在跑的那条（claimed_at 非空且最近有报信）也不许。
  #     · 只改状态并写清原因，不删记录，回写时仍能对上号。
  if (p == "/api/exec/cancel" && identical(req$method, "POST")) {
    access <- record_preflight$access
    sid_c <- as.character(record_preflight$target$sid %||% "")
    b <- body_json()
    want <- trimws(as.character(b$id %||% ""))
    if (!nzchar(want))
      return(send_json(con, 400, list(ok = FALSE, code = "exec_id_required",
                                      error = "要作废哪一条？没有给任务号。")))
    d <- exec_read()
    hit <- NA_integer_
    for (i in seq_along(d$items %||% list())) {
      if (identical(as.character(d$items[[i]]$id %||% ""), want)) { hit <- i; break }
    }
    if (is.na(hit))
      return(send_json(con, 404, c(list(ok = FALSE, code = "exec_task_missing",
        error = "队列里没有这条任务。"), record_cap_fields(access$cap))))
    it <- d$items[[hit]]
    if (!identical(as.character(it$session_id %||% ""), sid_c))
      return(send_json(con, 403, c(list(ok = FALSE, code = "exec_task_not_owned",
        error = "这条任务不属于当前分析记录，不能在这里作废。"),
        record_cap_fields(access$cap))))
    st <- as.character(it$status %||% "")
    if (!st %in% c("queued", "running"))
      return(send_json(con, 409, c(list(ok = FALSE, code = "exec_task_terminal",
        status = st,
        error = paste0("这条已经是 ", st, "，不需要作废。")),
        record_cap_fields(access$cap))))
    # 强制作废：人已经确认"这个不该再等了"。
    #   为什么必须有这条路（2026-11-22，gina 报"只能改代码才能继续"）：
    #     原来只允许作废 queued，running 一律拒绝；而 queued 若 recently 报过信
    #     也拒绝。于是一条执行端早已死掉、却停在 running 的任务，任何人、
    #     任何界面都动不了它 —— 唯一出路是去改 state/exec_tasks.json。
    #     这不是保护，是把人挡在系统外面。
    #   强制作废仍然是"改状态、不删记录"，回写时照样能对上号；
    #   唯一的代价是：如果它其实还在跑，那个作业的产物可能仍会落地。
    #   这句话必须原样告诉调用方，不能悄悄放行。
    force0 <- isTRUE(b$force)
    claimed0 <- trimws(as.character(it$claimed_at %||% ""))
    seen0 <- trimws(as.character(it$last_seen %||% ""))
    # 判活：心跳新鲜 = 真的可能还在跑。
    hbs_c <- tryCatch(exec_worker_hb_all(), error = function(e) list())
    pool_c <- exec_task_pool(it)
    wid_c <- trimws(as.character(it$worker_id %||% it$claimed_by %||% ""))
    hb_c <- if (nzchar(wid_c)) exec_latest_heartbeat(worker_id = wid_c, hbs = hbs_c) else NULL
    if (is.null(hb_c)) hb_c <- exec_latest_heartbeat(pool_id = pool_c, hbs = hbs_c)
    hb_age_c <- exec_heartbeat_age(hb_c)
    worker_dead <- !is.finite(hb_age_c) || hb_age_c >= as.numeric(EXEC_WORKER_OFFLINE_SECS)
    if (!force0) {
      # 执行端确实还活着、这条任务也确实还在报信 —— 只有这种才拦。
      if (!worker_dead) {
        seen_ts <- exec_parse_ts(seen0)
        seen_age <- if (is.na(seen_ts)) Inf else
          as.numeric(difftime(Sys.time(), seen_ts, units = "secs"))
        if (is.finite(seen_age) && seen_age < as.numeric(EXEC_QUEUE_UNCLAIMED_SECS))
          return(send_json(con, 409, c(list(ok = FALSE, code = "exec_task_still_alive",
            status = st, claimed_at = claimed0, last_seen = seen0,
            worker_heartbeat_at = as.character((hb_c %||% list())$at %||% ""),
            can_force = TRUE,
            error = paste0("这条在 ", round(seen_age / 60, 0),
                           " 分钟前还有执行端报信，执行端也还在线，可能真的在跑。",
                           "如果你确定它不该再等了，再点一次「强制作废」。")),
            record_cap_fields(access$cap))))
      }
    }
    why <- trimws(as.character(b$reason %||% ""))
    if (!nzchar(why)) why <- "人工作废：排队期间无人认领，为解除后续环节的阻塞"
    # 作废也走锁内读改写：上面的判断基于锁外读到的快照，
    #   写完这段时间执行端可能刚好领取或回写了同一份队列，
    #   整份覆盖会把它的改动抹掉（2026-11-23 实测的丢更新同型）。
    #   这里按 id 重新定位后再改，不用锁外的索引。
    exec_mutate(function(dd) {
      for (i in seq_along(dd$items %||% list())) {
        if (!identical(as.character(dd$items[[i]]$id %||% ""), want)) next
        dd$items[[i]]$status <- "failed"
        dd$items[[i]]$finished_at <- exec_ts()
        dd$items[[i]]$error <- paste0("已由人工作废（", why,
                                      "）。这不是这一步本身跑失败：",
                                      "它在服务器上没有产出，重派即可。")
        dd$items[[i]]$cancelled_by <- "user"
        dd$items[[i]]$cancelled_at <- exec_ts()
        dd$items[[i]]$cancel_previous_status <- st
      }
      list(data = dd)
    })
    # 作废链上的任务，必须同时告诉链"这一步失败了"。
    #   为什么（2026-11-22 实测）：链的 req_id 全链共用，作废后如果只改任务状态、
    #   不通知链，链会永远停在 running；此后同一条链再点，rc_begin 走的是
    #   "same && status in (running, starting)" 那条分支，直接回 duplicate=TRUE ——
    #   页面显示"已交接"，队列里一条新任务都没有。实测确认：作废后链仍是 running，
    #   再派返回 200 且不带任何新任务。
    #   回收器早就是这么做的（stale_reaped 会调 rc_callback），作废这条路漏了。
    cancel_chain <- as.character(d$items[[hit]]$chain_id %||% "")
    if (nzchar(cancel_chain) && exists("rc_callback", mode = "function")) {
      tryCatch(rc_callback(list(session_id = as.character(it$session_id %||% ""),
                                req_id = cancel_chain,
                                step = as.character(it$step %||% ""),
                                status = "failed", code = 1L), FALSE),
               error = function(e) NULL)
    }
    exec_reap_audit_append(list(at = exec_ts(), kind = "manual_cancel", id = want,
                                step = as.character(it$step %||% ""),
                                previous_status = st, reason = why))
    return(send_json(con, 200, c(list(ok = TRUE, id = want, status = "failed",
                                      reason = why), record_cap_fields(access$cap))))
  }

  if (p == "/api/exec/view") {
    access <- record_preflight$access
    sid_view <- as.character(record_preflight$target$sid %||% "")
    d <- tryCatch(exec_read(), error = function(e) list(items = list()))
    its <- Filter(function(it) identical(as.character(it$session_id %||% ""), sid_view),
                  d$items %||% list())
    its <- lapply(its, function(it) list(
      id = it$id, step = it$step, session_id = it$session_id,
      status = it$status, created_at = it$created_at, claimed_at = it$claimed_at,
      finished_at = it$finished_at, error = it$error,
      run_dir = if (nzchar(as.character(it$run_dir %||% "")))
        as.character(it$run_dir) else "",
      mode = as.character(it$mode %||% ""),
      reference_id = as.character(it$reference_id %||% ""),
      ref_mode = as.character(it$ref_mode %||% ""),
      ref_path = as.character(it$ref_path %||% "")))
    return(send_json(con, 200, c(list(ok = TRUE, session_id = sid_view,
                                     items = its), record_cap_fields(access$cap))))
  }

  if (p == "/api/exec") {
    # 页面看队列时同样先回收：一条 10-28 起就挂着、执行端早就没了的任务，
    #   不该继续显示成"正在跑"。
    d <- tryCatch(exec_reap(), error = function(e) exec_read())
    now_ev <- Sys.time()
    # 排队久、没人认领、执行端却在线的，单独标出来。
    #   为什么需要这个标记（2026-11-22，gina 报队列死锁）：
    #     这种任务自动回收不了 —— 队列是单执行端的，前一条 final 跑三小时，
    #     后面的本来就该排队。可它又确实会让后续环节永远派不出去。
    #     系统能做的是把它指出来，让人工作废（/api/exec/cancel），
    #     而不是自己判断谁该死。
    #   online 只是"有进程在报心跳"，不等于"这个队列有人轮询"，
    #     所以这里把两件事分开报，不合并成一个结论。
    hbs_ev <- tryCatch(exec_worker_hb_all(), error = function(e) list())
    online_ev <- exec_heartbeat_online(exec_latest_heartbeat(hbs = hbs_ev), now_ev)
    its <- lapply((d$items %||% list()), function(it) {
      created0 <- exec_parse_ts(it$created_at)
      q_age <- if (is.na(created0)) NA_real_ else
        as.numeric(difftime(now_ev, created0, units = "secs"))
      claimed0 <- trimws(as.character(it$claimed_at %||% ""))
      stuck <- identical(as.character(it$status %||% ""), "queued") &&
        !nzchar(claimed0) && isTRUE(online_ev) &&
        is.finite(q_age) && q_age >= as.numeric(EXEC_QUEUE_UNCLAIMED_SECS)
      list(id = it$id, step = it$step, session_id = it$session_id, status = it$status,
           created_at = it$created_at, claimed_at = it$claimed_at,
           finished_at = it$finished_at, error = it$error,
           queued_minutes = if (identical(as.character(it$status %||% ""), "queued") &&
                                is.finite(q_age)) round(q_age / 60) else NULL,
           stuck = stuck)
    })
    hb <- exec_latest_heartbeat(hbs = hbs_ev)
    hbAt <- as.character((hb %||% list())$at %||% "")
    online <- online_ev
    # 心跳在动不等于会干活：把"最后一次真领活"一起报出去，
    #   否则看板只会说"执行端在线"，而队列其实两天没人碰（2026-11-22 实测）。
    clq <- tryCatch(exec_claim_last(d), error = function(e)
      list(at = "", id = "", age_secs = NA_real_))
    # 空转只有在"真有活没人领"时才成立。
    #   队列本来就空、它两天没领活是正常的（没东西可领），
    #   这种时候报"空转"就是诬告，人会被叫去重装一台没毛病的机器。
    #   判据：有 queued 任务没人认领，且最久那条已经超过判定线。
    q_unclaimed <- Filter(function(it)
      identical(as.character(it$status %||% ""), "queued") &&
        !nzchar(trimws(as.character(it$claimed_at %||% ""))),
      (d$items %||% list()))
    q_oldest_secs <- NA_real_
    for (it in q_unclaimed) {
      t0 <- exec_parse_ts(it$created_at)
      if (is.na(t0)) next
      a0 <- as.numeric(difftime(now_ev, t0, units = "secs"))
      if (!is.finite(q_oldest_secs) || a0 > q_oldest_secs) q_oldest_secs <- a0
    }
    spin <- isTRUE(online) && length(q_unclaimed) > 0L &&
      is.finite(q_oldest_secs) &&
      q_oldest_secs >= as.numeric(EXEC_QUEUE_UNCLAIMED_SECS)
    # 队列计数按状态一次给全：页面要回答"现在到底排着几条、跑着几条"，
    #   不能让人从明细里自己数。
    cnt <- function(st) sum(vapply((d$items %||% list()), function(it)
      identical(as.character(it$status %||% ""), st), TRUE))
    # 回收器状态：它坏死时不会有报错冒到页面上，只表现成"那条一直不动"。
    #   所以把它的最后一次扫描一起报出来，让"没人回收"和"没人接活"能分开看。
    return(send_json(con, 200, list(
      ok = TRUE, items = its,
      stuck_count = sum(vapply(its, function(x) isTRUE(x$stuck), TRUE)),
      counts = list(queued = cnt("queued"), running = cnt("running"),
                    done = cnt("done"), failed = cnt("failed"),
                    total = length(d$items %||% list())),
      reclaim = tryCatch(exec_reaper_status(now = now_ev), error = function(e) NULL),
      worker = list(online = online,
                    at = if (nzchar(hbAt)) hbAt else NULL,
                    by = as.character((hb %||% list())$worker_id %||% ""),
                    pool_id = as.character((hb %||% list())$pool_id %||% ""),
                    workers = length(hbs_ev),
                    # 在报活 ≠ 会干活。这两项让看板能说清"在线但空转"。
                    serving = isTRUE(online) && !isTRUE(spin),
                    spin_idle = isTRUE(spin),
                    last_claim_at = if (nzchar(as.character(clq$at %||% "")))
                      as.character(clq$at) else NULL,
                    last_claim_id = as.character(clq$id %||% ""),
                    last_claim_age_secs = if (is.finite(as.numeric(clq$age_secs %||% NA_real_)))
                      round(as.numeric(clq$age_secs)) else NULL,
                    # 已经发过几次启动/安装请求、什么时候发的、发给哪台电脑。
                    bootstrap = tryCatch({
                      b0 <- jsonlite::fromJSON(read_text_any(
                        file.path(STATE_DIR, "exec_bootstrap.json")),
                        simplifyVector = FALSE)
                      list(requested_at = as.character(b0$requested_at %||% ""),
                           exec_pc = as.character(b0$exec_pc %||% ""),
                           count = as.integer(b0$count %||% 0L))
                    }, error = function(e) NULL)))))
  }

  # ---- 执行端预检：派工前先回答「有没有人接活」-----------------------
  #   为什么必须有这一步（2026-11-22）：
  #     任务能入队、页面显示已交给助手、作业号空、状态一直 queued。
  #     真正的原因是谁也没来接，但这句话系统里没有任何地方说过 ——
  #     于是每换一台设备、每升一次服务端，都要人报错一次才被发现。
  #     这里把「有没有在线执行端、版本对不对、令牌通不通、目标机器在不在线」
  #     一次答完，缺什么就直接说缺什么、下一步做什么。
  if (p == "/api/exec/preflight") {
    hbs <- tryCatch(exec_worker_hb_all(), error = function(e) list())
    workers <- lapply(hbs, function(hb) list(
      worker_id = as.character(hb$worker_id %||% ""),
      pool_id = as.character(hb$pool_id %||% ""),
      at = as.character(hb$at %||% ""),
      version = as.character(hb$version %||% ""),
      online = isTRUE(exec_heartbeat_online(hb))))
    online <- Filter(function(w) isTRUE(w$online), workers)
    pools <- unique(vapply(online, function(w) as.character(w$pool_id %||% ""), ""))
    # 队列积压：可领取的任务数。有人在线却一直不动，和没人来领是两回事。
    dq <- tryCatch(exec_due(), error = function(e) list())
    backlog <- length(dq %||% list())
    # 心跳会骗人：它准时到，任务却两天没人领。所以再答一句"最后一次真的领活"。
    #   serving = 心跳新鲜 且 真的领过活（或有活可领且在正常时限内领过）。
    #   只要最后一次领活早于"没人领活"的判定线，就判成空转，按没有执行端处理。
    cl <- tryCatch(exec_claim_last(), error = function(e)
      list(at = "", id = "", age_secs = NA_real_))
    age_cl <- as.numeric(cl$age_secs %||% NA_real_)
    # 空转必须是"真有活没人领"。队列空着的时候它不领活是正常的，
    #   那种情况报空转就是把没毛病的机器叫去重装（这条在 2026-11-22 自己踩过）。
    #   所以先看 backlog：没有可领取的任务，就不算空转。
    spin_idle <- length(online) > 0L && backlog > 0L &&
      (!is.finite(age_cl) || age_cl > as.numeric(EXEC_QUEUE_UNCLAIMED_SECS))
    serving <- length(online) > 0L && !isTRUE(spin_idle)
    actions <- character(0)
    if (!length(online)) {
      actions <- c(actions,
        "没有任何执行端在线：在要用的电脑上执行 pa_exec_worker.py --install，再运行 run-worker 启动脚本。",
        "若已安装但没启动，直接运行固定位置里的 run-worker；心跳 60 秒一次，启动后这里就会变成在线。")
    } else if (isTRUE(spin_idle)) {
      # 这一条是本次核心：不把"在报活"当成"能用"。
      actions <- c(actions, paste0(
        "执行端在报心跳但已经 ",
        if (is.finite(age_cl)) paste0(round(age_cl / 3600), " 小时") else "从未",
        "没有领过任务：它是空转的，队列不会动。",
        if (nzchar(cl$at %||% "")) paste0("最后一次领活：", cl$at) else "",
        "请在那台电脑上重装并重启执行端（pa_exec_worker.py --install 后跑 run-worker）。"))
    } else if (backlog > 0L) {
      actions <- c(actions, paste0("执行端在线但队列还有 ", backlog,
        " 条未领：检查它是否卡在上一条任务上，或 pool_id 与任务不一致。"))
    }
    # 检测不到能用的执行端，就主动发起一次启动/安装请求（十分钟内只发一条）。
    boot <- NULL
    if (!isTRUE(serving)) {
      boot <- tryCatch(exec_bootstrap_request(
        reason = if (length(online)) "spin_idle" else "offline"),
        error = function(e) list(ok = FALSE, code = "bootstrap_failed",
                                 error = conditionMessage(e)))
    }
    return(send_json(con, 200, list(ok = TRUE,
      online_count = length(online), total_workers = length(workers),
      workers = workers, pools = as.list(pools), backlog = backlog,
      serving = isTRUE(serving), spin_idle = isTRUE(spin_idle),
      last_claim_at = as.character(cl$at %||% ""),
      last_claim_id = as.character(cl$id %||% ""),
      last_claim_age_secs = if (is.finite(age_cl)) round(age_cl) else NULL,
      bootstrap = boot,
      heartbeat_fresh_secs = EXEC_HB_FRESH_SECS,
      actions = as.list(Filter(function(s) nzchar(s), actions)))))
  }

  # ---- 误报纠正（窄入口，候选）----------------------------------------
  #   与 step/import 分开：import 必须拒绝"没有执行证据的终态"，
  #   纠正做的是相反的事 —— 承认先前那份终态没有证据并撤下来。
  #   鉴权沿用现有受控通路：先按 callback_owner 绑会话与请求，
  #   再要求"本分析属主"或"该 exec 任务确实是本会话本环节在管的那条"。
  #   不新增匿名通道；exec_id 本身只是绑定线索，不是凭据。
  if (p == "/api/step/recover" && identical(req$method, "POST")) {
    if (!HAS_EXEC_RECOVER)
      return(send_json(con, 503, list(ok = FALSE, code = "recover_module_missing",
        error = "纠正模块未加载")))
    b <- body_json()
    stp <- trimws(as.character(b$step %||% ""))
    rq  <- trimws(as.character(b$req_id %||% ""))
    sid_in <- trimws(as.character(b$session_id %||% ""))
    exid <- trimws(as.character(b$exec_id %||% ""))
    if (!nzchar(stp) || !nzchar(rq) || !nzchar(sid_in) || !nzchar(exid))
      return(send_json(con, 400, list(ok = FALSE, code = "recover_fields_required",
        error = "需要 step / req_id / session_id / exec_id；四者缺一不改。")))
    binding <- callback_owner(b)
    if (!isTRUE(binding$ok)) return(send_json(con, 409, binding))
    sid <- binding$sid
    rq  <- binding$req_id
    items <- tryCatch(import_exec_items(), error = function(e) NULL)
    if (is.null(items))
      return(send_json(con, 503, list(ok = FALSE, code = "recover_exec_unavailable",
        error = "读不到执行任务表，无法核对在管任务；不改。")))
    ex <- NULL
    for (it in items) if (identical(as.character(it$id %||% ""), exid)) ex <- it
    if (is.null(ex))
      return(send_json(con, 409, list(ok = FALSE, code = "recover_exec_not_found",
        error = "找不到这条执行任务；不改。")))
    # 授权：只认本分析的属主或已授权 agent 凭据。
    #   为什么删掉"带对 exec_id 就放行"的兜底：exec_id 在 /api/flow 里就能看到，
    #   任何能读页面的人都能抄一个。抄一个就能撤下终态，等于把纠错入口变成
    #   匿名改状态的通道 —— 这正是这次故障要防的那类问题。
    own_ok <- FALSE
    tryCatch({
      own_ok <- isTRUE(db_run_log_authorized(DB_CON, owner_ctx_key(owner_ctx), sid))
    }, error = function(e) own_ok <<- FALSE)
    if (!isTRUE(own_ok))
      return(send_json(con, 403, list(ok = FALSE, code = "recover_not_authorized",
        error = "调用方不是这个分析的属主，也没有该分析的授权凭据；不改。")))
    latest_ex <- NULL; latest_err <- FALSE
    tryCatch({ latest_ex <- exec_latest_for(stp, sid) },
             error = function(e) latest_err <<- TRUE)
    verified_n <- NULL; verified_err <- FALSE
    tryCatch({
      if (!DB_READY) verified_err <- TRUE else {
        n <- db_scalar(DB_CON,
          "SELECT COUNT(*) FROM step_attempts WHERE req_id = ? AND step_id = ? AND status = 'verified'",
          list(rq, stp))
        if (length(n) == 1L && !is.na(n)) verified_n <- as.integer(n) else verified_err <- TRUE
      }
    }, error = function(e) verified_err <<- TRUE)
    # 待回写号读不出来时不能当成"没有后继请求"，那正是放过旧记录的路。
    pend <- tryCatch(as.character(sess_get_pending(APP_DIR, sid, slot = "step") %||% ""),
                     error = function(e) NA_character_)
    if (length(pend) != 1L || is.na(pend))
      return(send_json(con, 503, list(ok = FALSE, code = "recover_pending_unavailable",
        error = "读不到这一环当前的待回写号，无法判断是否已有后继请求；不改。")))
    pend <- as.character(pend)
    # 当前有效值：必须就是那条失败投影，绝不回退去读旧 failed。
    cur <- tryCatch(attempt_results(APP_DIR, sid, rq)$steps[[stp]], error = function(e) NULL)
    ctx <- list(sid = sid, step = stp, req = rq, rec = cur, ex = ex,
                latest_ex = latest_ex, latest_err = latest_err,
                verified_n = verified_n, verified_err = verified_err,
                pending_req = pend, facts = b$facts %||% NULL)
    why <- recover_eligibility(ctx)
    if (nzchar(why))
      return(send_json(con, 409, list(ok = FALSE, code = "recover_not_eligible",
        step = stp, reason = why, wrote_db_done_or_verified = FALSE,
        old_files_kept = TRUE,
        error = paste0("不符合纠正条件：", why, "。未改动任何状态。"))))
    res <- recover_apply(APP_DIR, sid, stp, rq, cur, ex, b$facts %||% list(), STATE_DIR)
    if (!isTRUE(res$ok))
      return(send_json(con, 409, list(ok = FALSE,
        code = as.character(res$code %||% "recover_failed"),
        wrote_state = FALSE, wrote_db_done_or_verified = FALSE, old_files_kept = TRUE,
        error = as.character(res$error %||% "纠正未写入"))))
    return(send_json(con, 200, list(ok = TRUE, recovered = TRUE,
      step = stp, session_id = sid, req_id = rq,
      exec_id = as.character(ex$id %||% ""), job_id = as.character(ex$job_id %||% ""),
      started_at = as.character(ex$created_at %||% ex$claimed_at %||% ""),
      superseded = res$superseded, new_terminal_slot = res$new_terminal_slot,
      old_files_kept = TRUE, wrote_db_done_or_verified = FALSE,
      recovered_file = basename(as.character(res$recovered_file %||% "")))))
  }

  if (p == "/api/inject/due") {
    due <- inject_due()
    # 派工前先摘掉过时通知：同环节已有执行端在管、或该请求已验收的，
    #   不再交给 runner 另起会话去"启动计算"，只软关闭并留原因。
    closed_n <- 0L
    if (HAS_EXEC_GUARD && length(due)) {
      sc <- tryCatch(inject_stale_close(due), error = function(e) NULL)
      if (!is.null(sc)) { due <- sc$kept; closed_n <- as.integer(sc$closed_n %||% 0L) }
    }
    return(send_json(con, 200, list(ok = TRUE, count = length(due), items = due,
                                    stale_closed = closed_n)))
  }

  # 单条取全文：页面上"现在补发"按钮要用它。
  #   状态列表里不带正文（队列会长），点哪条取哪条。
  if (p == "/api/inject/item") {
    want <- as.character(q$id %||% "")
    d <- inject_read()
    hit <- Filter(function(it) identical(as.character(it$id %||% ""), want), d$items)
    if (!length(hit)) return(send_json(con, 404, list(ok = FALSE, error = "队列里没有这一条")))
    return(send_json(con, 200, list(ok = TRUE, item = hit[[1]])))
  }

  # ---- 环节质检：对指定环节跑 reviewer（skill + 脚本双检）-----------
  if (p == "/api/review") {
    b <- body_json()
    access <- record_preflight$access
    sid2 <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    stage <- as.character(b$stage %||% "")
    rd <- as.character(b$run_dir %||% ctx$run_dir)
    if (!nzchar(rd)) return(send_json(con, 400, list(ok = FALSE, error = "缺运行目录")))
    if (!identical(gsub("\\\\", "/", rd), gsub("\\\\", "/", ctx$run_dir)))
      return(send_json(con, 409, c(list(ok = FALSE, code = "review_run_dir_mismatch",
        error = "run_dir 不属于这条分析记录，未写质检结论。"),
        record_cap_fields(access$cap))))
    # 不再跑服务器上的 reviewer.R，也不再往服务器派质检任务。
    #   为什么撤：服务器那份 reviewer.R 是另一份副本，阶段名单按老流程写
    #   （env/decide/precheck/pipeline1/plots/ppt/pack），本流程的
    #   annotate / figs / enrich 它一个都不认识 —— 派过去也只会回一句
    #   "阶段名不对"，然后这条红字就一直挂在界面上，人以为流程坏了。
    #   质检改由应用自己判，判据是回写时留下的四件事：
    #   状态是否 done、有没有产物清单、有没有运行目录、日志里有没有失败字样。
    if (!isTRUE(HAS_QUALITY))
      return(send_json(con, 200, list(ok = FALSE, error = "质检模块未加载（engine/quality.R）")))
    q <- tryCatch(qual_of(sid2, stage), error = function(e) NULL)
    if (is.null(q)) {
      # 没有记录就说没有，不能当成通过。
      return(send_json(con, 200, list(
        ok = FALSE, session_id = sid2, stage = stage, verdict = "missing", checked = FALSE,
        output = paste0("「", stage, "」还没有应用侧质检记录 —— 不是通过了，是还没跑过。",
                        "质检在步骤回写时自动生成：回写里要带状态、运行目录、产物清单和日志。"))))
    }
    okQ <- identical(as.character(q$verdict %||% ""), "pass")
    lines <- c(paste0("环节：", stage),
               paste0("结论：", as.character(q$verdict %||% "unknown")),
               paste0("时间：", as.character(q$at %||% "")))
    for (c0 in (q$checks %||% list())) {
      mk <- if (isTRUE(c0$ok)) "通过" else if (identical(c0$ok, FALSE)) "不过" else "判不了"
      lines <- c(lines, paste0("· ", as.character(c0$name %||% ""), "：", mk,
                               if (nzchar(as.character(c0$detail %||% "")))
                                 paste0("（", as.character(c0$detail %||% ""), "）") else ""))
    }
    return(send_json(con, 200, list(ok = okQ, session_id = sid2, stage = stage,
                                    verdict = as.character(q$verdict %||% "unknown"),
                                    checks = q$checks %||% list(),
                                    output = paste(lines, collapse = "\n"))))
  }

  # ---- 任务通道：把「跑某个环节」变成一条任务，交给助手执行 ----------
  #   为什么走这条路：应用跑在本机（云上），数据在远端服务器，两边网络不通；
  #   助手能操作的是用户的工作电脑，而那台电脑能连服务器。
  #   所以应用不直接连服务器，只把「跑哪一步、用哪套脚本、在哪个目录」写成任务，
  #   由助手在工作电脑上按 step_plan 执行，再把结果写回 state/tasks.jsonl。
  if (p == "/api/task") {
    tf <- file.path(APP_DIR, "state", "tasks.jsonl")
    dir.create(dirname(tf), showWarnings = FALSE, recursive = TRUE)
    access <- record_preflight$access
    task_sid <- as.character(record_preflight$target$sid %||% "")
    if (identical(req$method, "POST")) {
      b <- body_json()
      stage <- as.character(b$stage %||% "")
      if (!nzchar(stage)) return(send_json(con, 400, list(ok = FALSE, error = "缺 stage")))
      ctx <- record_context(access$cap, require_run_dir = FALSE)
      if (!isTRUE(ctx$ok)) {
        out <- record_refusal(ctx)
        out$body <- c(out$body, record_cap_fields(access$cap))
        return(send_json(con, out$status, out$body))
      }
      srv <- ctx$server_place %||% list()
      task <- list(
        id = paste0("task-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", stage),
        session_id = task_sid,
        stage = stage,
        skill = as.character(b$skill %||% ""),
        run_dir = ctx$run_dir,
        mode = ctx$mode,
        note = as.character(b$note %||% ""),
        server = list(host = srv$host, port = srv$port, user = srv$user,
                      # 只放引用键，不放口令本体
                      password_ref = "config/server_profile.json",
                      data_root = srv$data_root, work_root = srv$work_root,
                      runtime = srv$runtime),
        status = "pending",
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        result = NULL
      )
      con2 <- file(tf, open = "a", encoding = "UTF-8")
      writeLines(jsonlite::toJSON(task, auto_unbox = TRUE, null = "null"), con2, useBytes = TRUE)
      close(con2)
      Sys.chmod(tf, mode = "0600")  # 不吞失败：设不上要能被看见
      return(send_json(con, 200, c(list(ok = TRUE, session_id = task_sid, task = task),
        record_cap_fields(access$cap))))
    }
    items <- list()
    if (file.exists(tf)) {
      ln <- readLines(tf, warn = FALSE, encoding = "UTF-8")
      ln <- ln[nzchar(ln)]
      ln <- utils::tail(ln, 20)
      items <- lapply(ln, function(x)
        tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL))
      items <- Filter(Negate(is.null), items)
      items <- Filter(function(x) identical(as.character(x$session_id %||% ""), task_sid), items)
      # 口令只落盘给助手读，不回传给浏览器：这个应用是公网可访问的。
      items <- lapply(items, function(x) {
        if (!is.null(x$server)) x$server$password <- NULL
        x
      })
    }
    return(send_json(con, 200, c(list(ok = TRUE, session_id = task_sid, tasks = items),
      record_cap_fields(access$cap))))
  }

  # ===================================================================
  #  流程：环节清单 · 进度 · 耗时
  #
  #  为什么要有这一块：
  #    用户要的不是「这一步跑没跑成」这一格状态，而是「现在跑到哪个环节、
  #    每个环节花了多久」。整步只有一个 running/done，看不出中途卡在哪。
  #    所以：阶段与环节清单来自 flow_spec.R（唯一出处），
  #    进度来自执行端的环节级回写，耗时由时间戳算。
  #    两边用的是同一份环节清单 —— 页面不会出现「显示了一个流程里没有的环节」。
  # ===================================================================
  FLOW_PROG_FILE <- file.path(STATE_DIR, "flow_progress.json")

  # 质检结论也按分析隔离：同一环节在不同分析里是两件事，
  #   混在一张表里就会出现"新分析显示上一步审过了"。
  flow_prog_read <- function(sid = NULL) {
    if (!file.exists(FLOW_PROG_FILE)) return(list(stages = list()))
    x <- tryCatch(jsonlite::fromJSON(read_text_any(FLOW_PROG_FILE), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.null(x) || is.null(x$stages)) return(list(stages = list()))
    if (is.null(sid)) sid <- cur_sid()
    if (nzchar(sid) && length(x$stages) > 0)
      x$stages <- Filter(function(g) rec_belongs(g, sid), x$stages)
    x
  }

  # 按环节把状态拼成一张表。判据只用已经发生的事：
  #   · 执行端对某个环节回写过 running/done/failed  → 用它
  #   · 整步已经是 done 但环节没回写              → 标「未回写」，不替它填成完成
  #   · 没有环节回写时一律 unknown，不按排列推测正在运行的环节。
  flow_progress <- function(stepIds, sid = NULL) {
    if (is.null(sid)) sid <- cur_sid()
    d <- remote_steps_read(sid)
    byId <- list()
    for (s in (d$steps %||% list())) byId[[as.character(s$id %||% "")]] <- s
    prog <- flow_prog_read(sid)
    gmap <- list()
    for (g in (prog$stages %||% list())) gmap[[as.character(g$stage %||% "")]] <- g

    out <- list()
    fprof <- flow_profile(sid)
    for (id in stepIds) {
      st <- if (HAS_FLOW) flow_stage(id, fprof) else NULL
      if (is.null(st)) next
      rec <- byId[[id]]
      # 这一环在执行队列里的真实状态。
      #   为什么状态要以它为准：作业一旦入队，跑不跑就不由会话决定了。
      #   原来只按回写记录算，于是"已入队、执行端正在跑"被显示成
      #   「交接未确认，尚无执行回写」—— 人看到这句会以为没派出去，再点一次。
      exNow <- tryCatch(exec_active_for(id, if (is.null(sid)) cur_sid() else sid),
                        error = function(e) NULL)
      # 本 req 的这一环若被明确判失败（比如引擎版本不一致、根本没提交），
      #   也要把它当成"这一步最近一次执行的真实结论"给出。
      exFail <- tryCatch(
        exec_failed_for(id, if (is.null(sid)) cur_sid() else sid,
                        as.character((rec %||% list())$req_id %||% "")),
        error = function(e) NULL)
      exPick <- tryCatch(flow_exec_state(exNow, exFail, rec),
                         error = function(e) NULL)
      subsIn <- list()
      if (!is.null(rec) && !is.null(rec$subs)) for (x in rec$subs) subsIn[[as.character(x$id %||% "")]] <- x
      gate <- gmap[[id]]
      # 应用侧质检结论：有就用它，没有就老实说"没做过"，不拿服务器的来顶。
      qreview <- NULL
      if (isTRUE(HAS_QUALITY)) {
        qq <- tryCatch(qual_of(if (is.null(sid)) cur_sid() else sid, id),
                       error = function(e) NULL)
        if (!is.null(qq)) {
          qreview <- list(
            ok = identical(as.character(qq$verdict %||% ""), "pass"),
            verdict = as.character(qq$verdict %||% "unknown"),
            imported_at = as.character(qq$at %||% ""),
            checks = qq$checks %||% list(),
            note = as.character(qq$note %||% ""))
        }
      }
      subs <- list()
      for (sb in (st$subs %||% list())) {
        r <- subsIn[[as.character(sb$id)]]
        stt <- if (!is.null(r)) as.character(r$status %||% "") else ""
        note <- if (!is.null(r)) as.character(r$note %||% "") else ""
        sa <- if (!is.null(r)) as.character(r$started_at %||% "") else ""
        ea <- if (!is.null(r)) as.character(r$ended_at %||% "") else ""
        if (!nzchar(stt)) stt <- "unknown"
        subs[[length(subs) + 1]] <- list(id = sb$id, title = sb$title, status = stt,
                                         note = note, started_at = sa, ended_at = ea)
      }
      # ---- 自相矛盾的记录不要照抄成"已完成" ----
      #   gina 2026-10-28 指着这一条问：详情里状态写「已完成」，
      #   可同一页的执行记录原文是「……filter 这一步继续按未复核处理，
      #   阶段状态保持原样，不写成完成。」
      #   回写自己都说没跑、没复核、没有产物、没有运行目录，
      #   状态字段却写着 done —— 应用照这个字段把卡片画成绿的，
      #   人看到的就是"明明没跑，界面说跑完了"。
      #   这里不猜、也不替它改成 failed（那同样是编），而是标出来：
      #   状态仍按原样保留，另给一个 conflict 说明，页面据此显示「未复核」。
      recLog <- if (is.null(rec)) "" else as.character(rec$log %||% "")
      recArts <- if (is.null(rec)) list() else (rec$artifacts %||% list())
      recRunDir <- if (is.null(rec)) "" else as.character(rec$run_dir %||% "")
      conflict <- ""
      if (!is.null(rec) && identical(as.character(rec$status %||% ""), "done")) {
        said <- character(0)
        if (grepl("不写成完成", recLog, fixed = TRUE)) said <- c(said, "回写原文写着「不写成完成」")
        if (grepl("未复核", recLog, fixed = TRUE)) said <- c(said, "回写原文写着「按未复核处理」")
        if (grepl("没有任何可执行的阶段动作", recLog, fixed = TRUE))
          said <- c(said, "回写原文写着「没有任何可执行的阶段动作」")
        if (!length(recArts) && !nzchar(recRunDir))
          said <- c(said, "没有产物清单，也没有运行目录")
        if (length(said)) conflict <- paste(said, collapse = "；")
      }
      # ---- 已被上游覆盖的步骤：显示"已完成"，但不重算 ----
      #   fullmark / diff 这两步要的东西，终版已经算出来了
      #   （8.final/all_markers.xls、top15_markers.xls、diff_MP_vs_Mock_by_cluster.xls）。
      #   原来的表现是：这一步没有脚本、显示"还没跑"，人点了就报错，
      #   于是看起来像流程断了一截。
      #   现在按 flow_spec 里的 cover_by 判定：上游那步已完成、且覆盖产物清单里
      #   有东西，就把这一步标成 covered，页面显示"已由终版覆盖"，按钮不再要求重跑。
      coverBy <- as.character(st$cover_by %||% "")
      covered <- FALSE
      coverNote <- ""
      upVerified <- FALSE
      hitN <- 0L
      want <- character(0)
      if (nzchar(coverBy)) {
        upRec <- byId[[coverBy]]
        upDone <- !is.null(upRec) && identical(as.character(upRec$status %||% ""), "done")
        want <- as.character(st$covered_artifacts %||% character(0))
        # 上游回写的产物清单里，能对上几个算几个
        upRels <- character(0)
        if (!is.null(upRec)) {
          for (a in (upRec$artifacts %||% list())) {
            r0 <- if (is.list(a)) as.character(a$rel %||% "") else as.character(a)
            if (nzchar(r0)) upRels <- c(upRels, r0)
          }
        }
        # 严格验收轴：同一 req 的这一环必须已 verified。
        #   原来只认 status == done，而上游刚完成、还没判定（甚至已被拒）时
        #   status 也是 done，于是 fullmark/diff 提前显示"已由终版覆盖"，
        #   页面把两项算进已通过 —— 这就是 6/12 里多出来的那两项。
        rqUp <- if (is.null(upRec)) "" else as.character(upRec$req_id %||% "")
        if (nzchar(rqUp) && isTRUE(DB_READY)) {
          vf <- tryCatch(DBI::dbGetQuery(DB_CON,
                  "SELECT COUNT(*) AS n FROM step_attempts
                    WHERE req_id = ? AND step_id = ? AND status = 'verified'",
                  list(rqUp, as.character(coverBy))), error = function(e) NULL)
          upVerified <- !is.null(vf) && nrow(vf) == 1L &&
            isTRUE(as.integer(vf$n[[1]]) > 0L)
        }
        cd0 <- cover_decide(up_status = as.character(upRec$status %||% ""),
                            up_verified = upVerified, want = want,
                            up_rels = upRels)
        covered <- isTRUE(cd0$covered)
        hitN <- as.integer(cd0$hit %||% 0L)
        upTitle <- as.character(
          (flow_stage(coverBy, fprof) %||% list(title = coverBy))$title)
        if (covered) {
          coverNote <- paste0("这一步的产出已由「", upTitle, "」一并算出：",
                              paste(want, collapse = "、"),
                              "（已确认 ", hitN, " 个，上游已通过验收）。",
                              "不再重算，免得产出两份对不上的表。")
        } else {
          coverNote <- paste0("这一步的产出由「", upTitle, "」负责，",
                              as.character(cd0$why %||% "条件未满足"),
                              "；本环节没有独立脚本，不要单独点运行。")
        }
      }
      out[[length(out) + 1]] <- list(
        id = id, title = st$title, note = st$note, skill = st$skill,
        # 这一步「实际会跑哪个脚本」、脚本在哪一侧。
        #   页面要按环节列脚本并标来源（服务器自带 / 脚本库提取 / AI 补充），
        #   判据只能来自这里登记的 script：它是 flow_spec 里唯一权威的那份，
        #   前端自己拼路径就又变成第二份判据了。
        #   {script_root}、{work_root} 开头 = 服务器上的脚本；
        #   {engine} 开头 = 应用自带的脚本，随命令下发。
        script = as.character(st$script %||% ""),
        script_source = if (grepl("^\\{engine\\}", as.character(st$script %||% ""))) "app" else "server",
        # 这一步实际怎么调它。服务器脚本在这台机器上读不到内容，
        #   能说清的"细节"就是：在哪个环节、用什么参数、按什么顺序跑。
        args = as.list(as.character(st$args %||% character(0))),
        # 这一环节是否真有对应的 skill 可调用。
        #   flow_spec 里的 skill 名有一部分是甲方脚本的代号（liny-doublet 这类），
        #   本地根本没有这个 skill —— 那种只写名字，会让人以为有个能调的技能。
        #   只有 skills/ 下确实存在的，页面才显示。
        skill_exists = {
          sk <- as.character(st$skill %||% "")
          nzchar(sk) && dir.exists(file.path(path.expand("~"), "skills", sk))
        },
        phase = if (is.null(st$phase) || is.na(st$phase)) "" else as.character(st$phase),
        # 状态优先级：执行队列的真实状态 > 回写记录。
        #   队列里有 queued/running，说明作业已经派出去并由执行端接管，
        #   这时候再说「交接未确认」是错的 —— 那是"会话没送到"的说法，
        #   而这里的执行本来就不靠会话。实测就是这个误报：
        #   作业 60739 已经在 master01 上跑，页面还写着交接未确认。
        #   只在没有回写、或回写还没开始时才用队列状态，避免盖掉真实结论。
        status = {
          # 已验收优先于一切：同一环后来通过判定、库里已有 verified 行时，
          #   页面必须显示已完成。否则一次历史失败任务会永久盖住真结论 ——
          #   实测：注释已验收并落库，流程表仍显示失败，下游按钮点不动。
          #   判据按「本记录 + 本步骤 + verified」查库，不猜、不看队列残留。
          vf_flow <- local({
            ok0 <- FALSE
            sid0 <- if (is.null(sid)) cur_sid() else sid
            if (isTRUE(DB_READY) && nzchar(sid0)) {
              # 必须同时限定版本：step_attempts 里同一 session 会跨多个
              #   version_id 累积行，只按 session 查会让旧版本的一次
              #   verified 把新版本的同一步也照成「已完成」。
              #   版本取自 sessions.version_id；该列为空时（老记录）才退回
              #   只按 session 判，保持历史记录显示不变。
              q0 <- tryCatch(DBI::dbGetQuery(DB_CON,
                paste0("SELECT COUNT(*) AS n FROM step_attempts ",
                       "WHERE step_id = ? AND status = 'verified' ",
                       "AND json_extract(payload, '$.session_id') = ? ",
                       "AND (version_id IS NULL OR version_id = ",
                       "     (SELECT version_id FROM sessions WHERE session_id = ?))"),
                list(id, sid0, sid0)), error = function(e) NULL)
              ok0 <- !is.null(q0) && nrow(q0) == 1L && isTRUE(as.integer(q0$n[[1]]) > 0L)
            }
            ok0
          })
          # 先按「执行队列优先、其次回写」得一个状态，再校正一种特例：
          #   申报 done、但判定器拒过。
          st0 <- if (isTRUE(vf_flow)) "done" else {
            # 队列结论优先（含"本 req 明确失败"），其次回写。
            #   判据集中在 flow_exec_state 里，可单测；这里只做回退。
            es <- exPick
            esSt <- if (is.null(es)) "" else as.character(es$status %||% "")
            if (nzchar(esSt)) esSt
            else if (is.null(rec)) "todo" else rec_display_status(rec)
          }
          # 为什么必须校正：attempt 里写的是申报值 done，页面照着读就会把
          #   一次被拒的完成画成"已完成"，还能让下游 merge 放行。
          #   实测就是这样：只给两个产物的 done 被 409 拒了，
          #   /api/flow 仍显示 doublet done。
          #   判据用回写那一刻留下的拒绝凭据，不动 attempt 里的原申报。
          if (identical(as.character(st0), "done")) {
            sid0 <- if (is.null(sid)) cur_sid() else sid
            rq0 <- as.character((rec %||% list())$req_id %||% "")
            if (!nzchar(rq0) && HAS_SESSIONS && nzchar(sid0))
              rq0 <- as.character(sess_get_pending(APP_DIR, sid0, slot = "step") %||% "")
            if (nzchar(rq0)) {
              # 拒绝凭据的效力边界：只在该 req 的该步「库里没有 verified 行」时才生效。
              #   为什么要这条判据：判定判据修正后，同一个 req 重验通过、库里已有
              #   verified 行，但第一次的拒绝凭据还留在目录里。旧逻辑只看文件在不在，
              #   于是页面永远停在「完成申报未通过」，下游也永远被挡。
              #   这不是放宽：真被拒的 req 不会有 verified 行，显示照旧是 rejected。
              vf0 <- tryCatch(db_scalar(DB_CON,
                paste0("SELECT COUNT(*) FROM step_attempts ",
                       "WHERE req_id = ? AND step_id = ? AND status = 'verified'"),
                list(rq0, id)), error = function(e) NA)
              reject_eff_verified <- !(length(vf0) == 1L && !is.na(vf0) &&
                                         as.integer(vf0) > 0L)
              rj <- file.path(attempt_dir(APP_DIR, sid0, rq0),
                              paste0("verdict_reject-", id, ".json"))
              if (reject_eff_verified && file.exists(rj)) st0 <- "rejected"
            }
          }
          st0
        },
        # 队列侧的信息单独给出一份，前端据此显示"执行端正在跑（作业名）"，
        #   而不是笼统地写"已交给助手"。
        exec_id = if (is.null(exPick)) NULL else as.character(exPick$exec_id %||% ""),
        exec_status = if (is.null(exPick)) NULL else as.character(exPick$exec_status %||% ""),
        exec_job = if (is.null(exPick)) NULL else as.character(exPick$exec_job %||% ""),
        # 执行端原文错误：页面照原样显示，别让"交接未确认"掩盖一次明确拒绝。
        exec_error = if (is.null(exPick)) NULL else (exPick$exec_error %||% NULL),
        # 执行端最后一次报到的时间。
        #   为什么要单独给出来：中途没有 step/import 回写是很正常的 ——
        #   一个样本跑十几分钟，执行端只按分钟报 keepalive。
        #   页面原来只拿回写时间算"多久没更新"，于是把"正常在跑但没回写"
        #   算成了失联，实测报了 1 小时 31 分钟。
        exec_last_seen = if (is.null(exPick)) NULL else as.character(exPick$exec_last_seen %||% ""),
        conflict = conflict,
        covered = covered, cover_by = coverBy, cover_note = coverNote,
        # 这一步要不要人先定注释依据。前端据此拦一下，别让人点完才发现没依据。
        needs_reference = supplement_needs_reference(id),
        # 交出去那一刻也算一次动静：否则"已交出、还没动手"会被算成"没有时间戳"，
        #   页面就永远显示不出「已经多久没消息」。
        started_at = if (is.null(rec)) "" else as.character(
          if (nzchar(as.character(rec$started_at %||% ""))) rec$started_at
          else (rec$delegated_at %||% "")),
        imported_at = if (is.null(rec)) "" else as.character(rec$imported_at %||% ""),
        ended_at = if (is.null(rec)) "" else as.character(rec$ended_at %||% ""),
        source_host = if (is.null(rec)) "" else as.character(rec$source_host %||% ""),
        log = if (is.null(rec)) "" else as.character(rec$log %||% ""),
        deps = as.list(st$deps %||% character(0)),
        artifacts = rec_actual_artifacts(rec),
        expected_artifacts = as.list(st$artifacts %||% character(0)),
        # 质检结论只认应用侧（engine/quality.R 记的那份），不再用服务器那份。
        #   原因：服务器上的 reviewer.R 是另一份副本，阶段名单按老流程写的，
        #   annotate / figs / enrich 它一个都不认识，永远判"没有质检"或"阶段名不对"。
        #   拿它做依据，表现就是这三步永远卡着动不了。
        #   应用侧判据是自己能核的：回写状态、产物清单、运行目录、日志有无失败字样。
        review = qreview,
        review_src = if (is.null(qreview)) NULL else "app",
        subs = subs)
    }
    out
  }

  # 完整流程指令（长版）。
  #   为什么要有这一条：会话通道对载荷有长度上限（manifest 里 text 上限 1000 字），
  #   而一份能照着执行的指令必然超过它。所以进会话的是一段短的「要办什么 + 去哪取全文」，
  #   全文留在这里由助手现取。这样既不超限，也不丢细节 ——
  #   以前是把长指令截断，助手拿到半截，只能自己补，那正是跑偏的来源。
  if (p == "/api/brief") {
    # 参数在 req$target 里，不在 req$query —— req 只有 method/target/headers/body。
    #   原来这里用 parse_query(req$query)，req$query 恒为 NULL，
    #   于是 stepId 永远为空、永远退回流程第一步。
    #   表现就是：取哪一步的全文，拿到的都是 doublet 那一条 ——
    #   助手照着它去跑，等于点 figs 却去跑了去双。这个坑踩过两次
    #   （先是在 /api/consistency，这里是同一处），所以统一走 qget。
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    stepId <- qget(req$target, "step")
    if (!nzchar(stepId)) stepId <- flow_run_order(sid)[1] %||% "doublet"
    ctx <- record_context(access$cap, require_run_dir = FALSE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    pl <- ctx$place
    rq <- qget(req$target, "req")
    if (!nzchar(rq)) {
      rq <- if (HAS_SESSIONS && nzchar(sid))
        as.character(sess_get_pending(APP_DIR, sid, slot = "step") %||% "") else ""
    }
    # ---- 取回「人已经定下的事」----------------------------------------
    #   为什么不从 URL 拿：它是交付那一刻应用的判断结果，不是执行端能提供的输入。
    #   存盘在交付时（见委派段 remote_step_put 的 gate_note），这里按 step 取回。
    #   取不到就老实没有 —— 不编一句"人已经定下的事：无"，那会让人以为真没有。
    gateNote <- ""
    tryCatch({
      recG <- remote_step_get(stepId, as.character(sid %||% ""))
      gateNote <- as.character((recG %||% list())$gate_note %||% "")
    }, error = function(e) NULL)
    # ---- 已入队时，brief 也必须是"只观察"那一版 --------------------------
    #   为什么不能只改 run 端点的 session_text：
    #     助手进会话看到的是短句，接着它按短句里的地址来取全文。
    #     如果 brief 照样回"照这条命令跑"，那前面那句"只观察"就白写了 ——
    #     两处说法打架，助手会按更具体的那条去做，于是又出现第二个执行者。
    #   所以这里先问执行队列：这一环已有 queued/running 的任务，
    #   就把 mode 标成 observe，并把全文换成观察版本。
    exActive <- tryCatch(exec_active_for(stepId, as.character(sid %||% "")),
                         error = function(e) NULL)
    txt <- if (!is.null(exActive)) {
      remote_step_observe_text(stepId, sid = as.character(sid %||% ""), ex = exActive)
    } else {
      remote_step_instruction(stepId, pl, session_id = as.character(sid %||% ""),
                              req_id = rq, gate_note = gateNote)
    }
    stage <- if (HAS_FLOW) flow_stage(stepId, flow_profile(sid)) else NULL
    return(send_json(con, 200, list(
      ok = TRUE, session_id = sid, step = stepId,
      title = if (is.null(stage)) stepId else stage$title,
      skill = if (is.null(stage)) NULL else stage$skill,
      subs = flow_subs_of(stepId),
      mode = if (is.null(exActive)) "execute" else "observe",
      exec_id = if (is.null(exActive)) NULL else as.character(exActive$id %||% ""),
      exec_status = if (is.null(exActive)) NULL else as.character(exActive$status %||% ""),
      text = txt,
      chars = nchar(txt))))
  }

  if (p == "/api/flow") {
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    runDir <- ctx$run_dir
    st$runDir <- runDir
    st$runDirRemote <- ctx$run_dir_remote
    if (!HAS_FLOW) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/flow_spec.R")))
    sc <- b_placeholder <- NULL
    # 只报「这一段要用的阶段」：跑分析里的阶段，加上 env/decide/precheck 这三步识别腿。
    prof <- flow_profile(sid)
    want <- FLOW_ORDER(prof)
    chk <- tryCatch(flow_spec_selfcheck(ENGINE_DIR), error = function(e) list(ok = FALSE, problems = conditionMessage(e)))
    # 服务器档没有 PPT 与打包这两步 —— 如实报出来，让人知道是"没有"，
    #   而不是"该有却一直没跑"。写一个空按钮在那儿，人只会反复点、反复等。
    missing <- if (identical(prof, "server") && !is.null(SERVER_MISSING))
      lapply(SERVER_MISSING, function(x) if (identical(x, "ppt")) "出 PPT"
                                          else if (identical(x, "pack")) "打包结果"
                                          else x) else list()
    # 对账：同一件事在五个文件里各写一份，这里一次说清差在哪、该改哪一段。
    #   为什么放进 /api/flow 而不是另开一个页面：人是在看这张流程表的时候
    #   发现"这一步怎么不动"的，答案必须出现在同一处，否则又要跑别处找。
    cchk <- if (isTRUE(HAS_CONTRACT)) tryCatch(
      contract_check(ENGINE_DIR, prof,
                     step_task_keys = names(STEP_TASK),
                     server_snapshot = (function() {
                       sn <- tryCatch(read_json_file(file.path(STATE_DIR, "server_engine_snapshot.json")),
                                      error = function(e) NULL)
                       if (is.null(sn)) return(NULL)
                       # 只认属于当前这条记录的那一份。
                       #   这个文件是全局单文件、整份覆盖，谁最后写就是谁。
                       #   以前不判归属，于是一条旧记录（bta0）的失败快照
                       #   会一直挂在新记录的页面上，和这次真跑出来的结论打架。
                       #   老文件没有 session_id：一样当"不属本记录"，
                       #   宁可不给判断，也不拿别人的结论下注。
                       cur_snap_sid <- sid
                       sn_sid <- as.character(sn$session_id %||% "")
                       if (!nzchar(sn_sid) || !identical(sn_sid, as.character(cur_snap_sid)))
                         return(NULL)
                       list(readable = isTRUE(sn$readable),
                            chain_order = as.character(sn$chain_order %||% character(0)),
                            why = as.character(sn$why %||% ""))
                     })()),
      error = function(e) list(ok = FALSE, issues = list(list(
        level = "unknown", where = "engine/contract.R",
        what = paste0("对账没跑起来：", conditionMessage(e)), fix = "先修对账模块本身。"))))
      else list(ok = FALSE, issues = list(list(
        level = "unknown", where = "engine/contract.R", what = "模块没加载", fix = "检查文件是否存在。")))
    return(send_json(con, 200, list(
      ok = TRUE, session_id = sid, mode = ctx$mode, order = as.list(want),
      profile = prof,
      missing = missing,
      stages = flow_progress(want, sid),
      # 进度的三种数法分开给，不再让人自己数卡片：
      #   done      真正跑完的
      #   covered   产出已由上游一并算出、不需要再跑的
      #   passed    done + covered —— 这才是"这条链走到哪了"
      #   以前只有一个"完成"数，于是 fullmark / diff 这种被终版覆盖的步骤
      #   永远算不进去，任何 X/10 都必然偏低。人看到 5/10 会以为流程没跑，
      #   其实是数法漏了这两类。
      progress = (function(ss) {
        d <- Filter(function(x) identical(as.character(x$status %||% ""), "done"), ss)
        cv <- Filter(function(x) isTRUE(x$covered) &&
                       !identical(as.character(x$status %||% ""), "done"), ss)
        list(done = length(d), covered = length(cv),
             passed = length(d) + length(cv), total = length(ss),
             done_ids = as.list(vapply(d, function(x) as.character(x$id %||% ""), "")),
             covered_ids = as.list(vapply(cv, function(x) as.character(x$id %||% ""), "")))
      })(flow_progress(want, sid)),
      gates = FLOW_GATES,
      selfcheck = chk,
      contract = cchk,
      run_dir = enc2utf8(gsub("\\\\", "/", runDir)))))
  }

  # 质检回写：执行端在服务器上跑完 reviewer.R 之后，把结论送回来。
  #   为什么要单独存：这是「能不能往下走」的唯一依据。
  #   不落盘的话，页面一刷新就不知道这一步到底审过没有，
  #   而"审过了"和"忘了审"在界面上看起来一样危险。
  if (p == "/api/review/import" && identical(req$method, "POST")) {
    b <- body_json()
    stage_id <- as.character(b$stage %||% "")
    if (!nzchar(stage_id)) return(send_json(con, 400, list(ok = FALSE, error = "缺 stage")))
    target <- record_session_target(q, list(session_id = b$session_id %||% ""),
                                    path = p, require = TRUE)
    if (!isTRUE(target$ok)) {
      refused <- record_refusal(target)
      return(send_json(con, refused$status, refused$body))
    }
    access <- record_access(DB_CON, APP_DIR, target$sid, owner_ctx, need = "write")
    if (!isTRUE(access$ok)) return(send_json(con, access$status, access$body))
    rev_sid <- target$sid
    d <- flow_prog_read(sid = "")
    keep <- Filter(function(x)
      !(identical(as.character(x$stage %||% ""), stage_id) && rec_belongs(x, rev_sid)),
      (d$stages %||% list()))
    item <- list(
      stage = stage_id,
      title = if (HAS_FLOW) { x <- flow_stage(stage_id); if (is.null(x)) stage_id else x$title } else stage_id,
      expect_skill = if (HAS_FLOW) { x <- flow_stage(stage_id); if (is.null(x)) "" else x$skill } else "",
      ok = isTRUE(b$ok),
      code = b$code,
      output = substr(as.character(b$output %||% ""), 1, 20000),
      exec_pc = as.character(b$exec_pc %||% ""),
      host = as.character(b$host %||% ""),
      imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
    # 「确实在服务器上跑」这一条用回写里的机器信息核，能核就核，不能核就说不能核。
    pl <- access$cap$place
    srvHost <- as.character(((pl %||% list())$server %||% list())$host %||% "")
    item$on_server <- if (!nzchar(item$host)) "unknown"
                      else if (!nzchar(srvHost)) "unknown"
                      else if (identical(item$host, srvHost)) "yes" else "no"
    d$stages <- c(keep, list(item))
    write_json_file(FLOW_PROG_FILE, d)
    return(send_json(con, 200, c(list(ok = TRUE, session_id = rev_sid,
      stage = stage_id, verdict = item), record_cap_fields(access$cap))))
  }

  # ---- 按环节分组的脚本清单 ---------------------------------------
  # ---- 脚本三层判定：服务器 / 自带库 / 待编写 -------------------------
  #   以前这条链只做到「服务器有就用」，服务器没有时靠人看红条判断，
  #   看不出是"服务器没有"还是"库里有但没挂上"还是"真缺、缺什么"。
  #   这里一次给出每一环的结论与依据，第三层额外落一份骨架草稿供人改。
  if (p == "/api/script-resolve") {
    if (!isTRUE(HAS_SCRIPT_RESOLVE) || !isTRUE(HAS_FLOW))
      return(send_json(con, 200, list(ok = FALSE,
        error = "缺少 engine/script_resolve.R 或 engine/flow_spec.R")))
    access <- record_preflight$access
    sid <- as.character(record_preflight$target$sid %||% "")
    want_drafts <- identical(qget(q, "drafts"), "1")
    if (isTRUE(want_drafts) && !isTRUE(access$cap$can_write))
      return(send_json(con, 403, c(list(ok = FALSE, code = "not_owner",
        error = "只有记录所有者可以生成脚本草稿。"),
        record_cap_fields(access$cap))))
    prof <- flow_profile(sid)
    ids <- tryCatch(FLOW_ORDER(prof), error = function(e) character(0))
    res <- tryCatch(script_resolve_all(ENGINE_DIR, APP_DIR, ids,
             stage_getter = function(id) flow_stage(id, prof),
             sup_getter = function() supplement_index(), sid = sid),
           error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (isTRUE(want_drafts) && isTRUE(res$ok)) {
      made <- list()
      for (one in res$stages) {
        if (identical(one$plan, "draft"))
          made[[length(made) + 1L]] <- tryCatch(
            script_draft_skeleton(APP_DIR, ENGINE_DIR, one), error = function(e) NULL)
      }
      res$drafts <- Filter(Negate(is.null), made)
      res$draft_note <- "草稿只写到 state/drafts/，不会自动生效；改完要人工合入。"
    }
    return(send_json(con, 200, res))
  }

  if (p == "/api/stage-scripts") {
    sf <- file.path(ENGINE_DIR, "stage_scripts.R")
    if (!file.exists(sf)) return(send_json(con, 200, list(ok = FALSE, error = "找不到 engine/stage_scripts.R")))
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    source(sf, local = TRUE)
    return(send_json(con, 200, c(stage_scripts(ENGINE_DIR, ctx$run_dir),
                                 record_cap_fields(access$cap))))
  }

  # ===================================================================
  #  历史记录 · 快照 · 回退
  #
  #  为什么要有这三样：
  #    原来的台账只回答"这一步跑没跑成"，回答不了
  #    "我上次是怎么跑的" 和 "我想改一个决定、从第 3 步重来"。
  #    没有它们，页面只能往前走，唯一的回头路是删掉整个运行目录。
  # ===================================================================

  # 历次运行：按 run_id 归并，给出时间、涉及步骤、用的是哪套脚本
  if (p == "/api/history") {
    if (!HAS_HISTORY) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/history.R")))
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    runDirTarget <- ctx$run_dir
    return(send_json(con, 200, c(list(
      ok = TRUE,
      session_id = ctx$session_id,
      run_dir = enc2utf8(gsub("\\\\", "/", runDirTarget)),
      runs = hist_runs(runDirTarget),
      steps = hist_step_states(runDirTarget, STEP_ORDER),
      order = as.list(STEP_ORDER),
      snapshots = hist_snapshots(runDirTarget),
      mark = hist_read_json(hist_roll_file(runDirTarget))
    ), record_cap_fields(access$cap))))
  }

  # 手动存一个快照（也可以在保存设置时自动存）
  if (p == "/api/snapshot" && identical(req$method, "POST")) {
    if (!HAS_HISTORY) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/history.R")))
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    b <- body_json()
    r <- tryCatch(hist_snapshot(ctx$run_dir, label = as.character(b$label %||% "manual"),
                                kind = "manual", note = as.character(b$note %||% "")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  # 回退：恢复某个快照；或只做"从某一步往后重跑"的标记
  #   mode=restore  恢复快照里的 pipeline.json / gates.json
  #   mode=rerun    不还原配置，只标记从某一步起为"待重跑"
  #   两种都不删产物：删是不可逆的，而重跑本来会覆盖。
  if (p == "/api/rollback" && identical(req$method, "POST")) {
    if (!HAS_HISTORY) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/history.R")))
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    b <- body_json()
    mode <- as.character(b$mode %||% "rerun")
    r <- tryCatch({
      if (identical(mode, "restore")) {
        hist_rollback(ctx$run_dir, as.character(b$snapshot %||% ""), STEP_ORDER,
                      from_step = as.character(b$from_step %||% "") )
      } else {
        hist_mark_rerun(ctx$run_dir, as.character(b$from_step %||% ""), STEP_ORDER)
      }
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  # 最近用过的运行目录：换回来 / 退回到欢迎页
  if (p == "/api/recent-dirs") {
    if (identical(req$method, "POST")) {
      b <- body_json()
      if (isTRUE(b$clear)) {
        # 退回欢迎页：只清"当前"，不动磁盘上任何东西
        st$runDir <- NULL
        save_state(st)
        return(send_json(con, 200, list(ok = TRUE, runDir = NULL)))
      }
      d <- as.character(b$dir %||% "")
      if (nzchar(d) && dir.exists(d)) {
        st$runDir <- normalizePath(d, winslash = "/", mustWork = FALSE)
        save_state(st)
        if (HAS_HISTORY) hist_recent_add(APP_DIR, st$runDir)
        return(send_json(con, 200, list(ok = TRUE, runDir = st$runDir)))
      }
      return(send_json(con, 400, list(ok = FALSE, error = "这个目录用不了")))
    }
    dirs <- if (HAS_HISTORY) hist_recent_dirs(APP_DIR) else list()
    return(send_json(con, 200, list(ok = TRUE, current = runDir %||% NULL, dirs = dirs)))
  }

  # ===================================================================
  #  脚本工作台：看、换、传、审、改
  #
  #  为什么要做成一套接口而不是"直接改文件"：
  #    脚本一换，整条链路的其余部分就可能对不上（没有守卫、写死阈值、
  #    非 ASCII 源码在中文 Windows 上直接不解析）。
  #    所以换之前先给机械检查的结论，换之后重建白名单 ——
  #    否则下一次 reviewer 会一直报"脚本被改动"，人就不再看它了。
  # ===================================================================

  if (p == "/api/scripts") {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    return(send_json(con, 200, sop_inventory(ENGINE_DIR)))
  }

  # ---- 脚本调研回写：助手把现场找到的脚本清单送回来 ----
  #   页面 L1 的前两层列不全，点「让助手去调研」会发一条指令进会话；
  #   助手查完把清单 POST 到这里，页面据此补上。
  #   为什么单独一个接口、不改 archive 里的文件：
  #     调研结果和"某一次处理记录"不是一回事 —— 它只是"这一层还有这些"。
  #     混进记录里会把它伪装成一次真实处理。
  if (p == "/api/script-scan" && identical(req$method, "POST")) {
    b <- body_json()
    src <- as.character(b$source %||% "")
    items <- b$items %||% list()
    if (!src %in% c("seurat", "server"))
      return(send_json(con, 400, list(ok = FALSE, error = "source 只能是 seurat 或 server")))
    clean <- list()
    for (x in items) {
      pth <- enc2utf8(gsub("\\\\", "/", as.character(x$path %||% "")))
      if (!nzchar(pth)) next
      clean[[length(clean) + 1L]] <- list(
        path = pth,
        name = if (nzchar(as.character(x$name %||% ""))) as.character(x$name) else basename(pth),
        bytes = if (is.null(x$bytes)) NULL else as.numeric(x$bytes),
        mtime = as.character(x$mtime %||% ""),
        stage = as.character(x$stage %||% ""),
        note = as.character(x$note %||% ""))
    }
    out <- list(source = src, at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                host = as.character(b$host %||% ""), items = clean)
    write_json_file(file.path(STATE_DIR, paste0("script_scan_", src, ".json")), out)
    return(send_json(con, 200, list(ok = TRUE, count = length(clean))))
  }

  # ---- 脚本存档：按记录 × 环节的脚本库 ----
  #   为什么要新开一组接口：原来那个库表（library/index.json）是"按环境取哪套"的
  #   取用规则，回答不了"这个环节历史上用过哪些脚本、成功几次"。
  #   存档是另一件事，独立一份 archive/index.json，不跟取用规则混。
  if (p == "/api/script-archive") {
    # 合并读取：官方底稿（随代码分发）+ 运行时记录（在 state 下）。
    #   为什么不再直接读 engine 里那份 index.json：运行时记录写在那里，
    #   一推送就会把某个人某次处理的服务器路径和归属带上公开仓库。
    d <- if (exists("sa_index_read", mode = "function"))
      tryCatch(sa_index_read(ENGINE_DIR, APP_DIR), error = function(e) NULL)
    else tryCatch(read_json_file(file.path(ENGINE_DIR, "library", "archive", "index.json")),
                  error = function(e) NULL)
    if (is.null(d)) return(send_json(con, 200, list(ok = FALSE, error = "存档索引读不出来")))
    # ---- 逐文件核对实体在不在（gina 2026-11-19）------------------------
    #   她的问题：「这两个记录都读不到脚本，前端为什么还会有这个内容」。
    #   原因是列表只按索引画，从没问过存档目录里到底有没有那个文件：
    #   索引是一条一条登进去的，文件却是另一件事 —— 迁移只搬了索引、
    #   某次复制失败、或者根本没取回正文，都会变成"列得出来、点开是空的"。
    #   现在每条文件带上 exists / bytes_now，记录上带 readable 与 missing，
    #   页面对着这两项决定是给按钮还是标「缺」。
    #   只读：不补文件、不改索引，缺了就如实说缺。
    base_arc <- file.path(ENGINE_DIR, "library", "archive")
    # 取回补齐（gina 2026-11-20）：执行端已经把脚本正文取回来时，先收进存档再列。
    #   只在 md5 与登记值一致时才收；对不上就不动，列表照旧标「缺」。
    #   放在这里的原因：人打开脚本库就是想知道"能不能看到正文"，
    #   这一步不占额外操作，取回后下一次打开就自动有了。
    if (exists("sa_adopt_mirror", mode = "function")) {
      for (r0 in (d$records %||% list()))
        tryCatch(sa_adopt_mirror(APP_DIR, ENGINE_DIR, as.character(r0$id %||% "")),
                 error = function(e) NULL)
      d <- if (exists("sa_index_read", mode = "function"))
        tryCatch(sa_index_read(ENGINE_DIR, APP_DIR), error = function(e) d) else d
    }
    recs <- d$records %||% list()

    # ---- 按使用者隔离（gina 2026-11-19 要求）----------------------------
    #   以前 archive 是全局一份：谁打开都看到同一批记录，包括别人的分析
    #   用过的脚本、服务器路径、成功与退回次数。要求是「脚本数据库也要做好
    #   用户隔离」。
    #   规则跟记录列表同一套口径，不另立一套：
    #     · 官方底稿（随应用分发，owner_key 为空）所有人可见 —— 那是产品
    #       自带的东西，不是某个人的处理历史；
    #     · 自己的记录自己可见；
    #     · 管理员可见全部（他要能核对整库）；
    #     · 记录隔离被显式关掉时，退回"全员可见"，并在返回里标明原因，
    #       免得有人以为隔离还开着。
    pol_arc <- if (isTRUE(HAS_ADMIN_POLICY)) record_policy(APP_DIR) else
      list(record_isolation = TRUE, write_isolation = TRUE)
    me_key <- as.character(owner_ctx_key(owner_ctx) %||% "")
    am_admin <- FALSE
    if (isTRUE(HAS_ADMIN_POLICY)) {
      a_arc <- tryCatch(admin_identity(APP_DIR, owner_ctx), error = function(e) NULL)
      am_admin <- isTRUE(a_arc$is_admin)
    }
    isolation_on <- isTRUE(pol_arc$record_isolation) && isTRUE(pol_arc$write_isolation)
    keep_arc <- vapply(recs, function(rec) {
      own <- trimws(as.character(rec$owner_key %||% ""))
      if (!nzchar(own)) return(TRUE)          # 官方底稿
      if (!isolation_on) return(TRUE)         # 隔离被显式关掉
      if (am_admin) return(TRUE)
      nzchar(me_key) && identical(own, me_key)
    }, logical(1))
    recs <- recs[keep_arc]

    recs <- lapply(recs, function(rec) {
      rid <- as.character(rec$id %||% "")
      root <- normalizePath(file.path(base_arc, rid), winslash = "/", mustWork = FALSE)
      files <- rec$files %||% list()
      n_ok <- 0L
      files <- lapply(files, function(x) {
        rel <- as.character(x$rel %||% "")
        fp <- if (nzchar(rel)) normalizePath(file.path(root, rel), winslash = "/", mustWork = FALSE) else ""
        inside <- nzchar(fp) && startsWith(fp, paste0(root, "/"))
        exists_now <- inside && file.exists(fp)
        if (isTRUE(exists_now)) n_ok <<- n_ok + 1L
        c(x, list(
          exists = isTRUE(exists_now),
          bytes_now = if (isTRUE(exists_now)) as.numeric(file.size(fp)) else NA_real_,
          # 索引登记了但正文本来就没收进来（自动入库时本机没有同内容副本）。
          #   这和"文件丢了"是两种不同的缺，页面分开说，人才知道该不该找回来。
          never_stored = isTRUE(x$content_missing)))
      })
      # 直接改原列表字段，不要再用 c(rec, list(files=...))：
      #   rec 里已经有 files，再追加一个同名元素，jsonlite 会写成
      #   files 与 files.1 两个键 —— 页面读的是旧的那个，核对结果白算一遍。
      rec$files <- files
      rec$dir_exists <- dir.exists(root)
      rec$readable <- n_ok
      rec$missing <- length(files) - n_ok
      rec
    })
    return(send_json(con, 200, list(ok = TRUE, built_at = as.character(d$built_at %||% ""),
                                    records = recs)))
  }

  # 存档里的脚本正文。只放行存档目录下的文件，不做任意路径读取。
  # ---- 按一条脚本库记录下载 ipynb（gina 2026-11-19 要求）------------
  #   她要在「按记录」每个条目右侧有一个下载 ipynb 的按钮，格式跟
  #   zjz 机器上那份 scRNA_analysis_GPU.ipynb 一样（含标题写法）。
  #   导出的是这条记录实际登记过、且存档里有正文的脚本，按流程顺序排列。
  #   取不到正文的段落只出标题并注明缺失，绝不补一段假代码。
  if (p == "/api/script-archive/notebook" && identical(req$method, "GET")) {
    if (!isTRUE(HAS_NB_RECORD) || !exists("nfr_from_archive", mode = "function"))
      return(send_json(con, 503, list(ok = FALSE, code = "notebook_module_missing",
        error = "缺少 engine/notebook_from_record.R")))
    rid <- as.character(qget(q, "record") %||% "")
    if (!nzchar(rid)) return(send_json(con, 400, list(ok = FALSE, error = "缺 record")))
    if (grepl("\\.\\.", rid) || grepl("[/\\\\]", rid))
      return(send_json(con, 400, list(ok = FALSE, error = "record 不合法")))
    # 可见性：与列表同一口径（官方底稿所有人可见，其余按归属）。
    if (exists("sa_visible", mode = "function")) {
      vis2 <- sa_visible(ENGINE_DIR,
                         owner_key = as.character(owner_ctx_key(owner_ctx) %||% ""),
                         is_admin = local({
                           a0 <- tryCatch(admin_identity(APP_DIR, owner_ctx),
                                          error = function(e) NULL)
                           isTRUE(a0$is_admin) }),
                         isolation_on = local({
                           p0 <- if (isTRUE(HAS_ADMIN_POLICY)) record_policy(APP_DIR) else
                             list(record_isolation = TRUE, write_isolation = TRUE)
                           isTRUE(p0$record_isolation) && isTRUE(p0$write_isolation) }))
      if (!(rid %in% vis2$ids))
        return(send_json(con, 403, list(ok = FALSE, code = "not_visible",
          error = "这条脚本记录不属于当前身份")))
    }
    d_arc <- if (exists("sa_index_read", mode = "function"))
      tryCatch(sa_index_read(ENGINE_DIR, APP_DIR), error = function(e) NULL)
    else tryCatch(read_json_file(file.path(ENGINE_DIR, "library", "archive", "index.json")),
                  error = function(e) NULL)
    rec_arc <- NULL
    for (x in ((d_arc %||% list())$records %||% list()))
      if (identical(as.character(x$id %||% ""), rid)) { rec_arc <- x; break }
    if (is.null(rec_arc))
      return(send_json(con, 404, list(ok = FALSE, code = "record_missing",
        error = "没有这条脚本记录")))
    built <- tryCatch(nfr_from_archive(ENGINE_DIR, rec_arc),
                      error = function(e) list(ok = FALSE, code = "build_failed",
                                               error = conditionMessage(e)))
    if (!isTRUE(built$ok))
      return(send_json(con, 400, list(ok = FALSE,
        code = as.character(built$code %||% "build_failed"),
        error = as.character(built$error %||% "生成失败"))))
    txt <- as.character(jsonlite::toJSON(built$notebook, auto_unbox = TRUE,
                                         null = "null", pretty = TRUE, digits = NA))
    bytes <- charToRaw(enc2utf8(txt))
    fname <- paste0(gsub("[^A-Za-z0-9._-]", "_", rid), ".ipynb")
    hdr <- paste0("HTTP/1.1 200 OK\r\n",
                  "Content-Type: application/x-ipynb+json\r\n",
                  "Content-Length: ", length(bytes), "\r\n",
                  "Content-Disposition: attachment; filename=\"", fname, "\"\r\n",
                  "Cache-Control: no-store\r\nConnection: close\r\n\r\n")
    writeBin(c(charToRaw(hdr), bytes), con); flush(con)
    return(invisible(NULL))
  }

  # ---- 补取正文：把缺正文的脚本交给执行端读回来（gina 2026-11-20）----
  #   为什么需要这一步：分析跑在服务器上，脚本常只在那个工作目录里。
  #   执行端回写时只带路径与指纹，没带正文，本机又没有同版本副本 —— 于是
  #   notebook 里只能标「取不到正文」。这里把路径登记给执行端，取回后
  #   md5 与登记值一致才收进存档，绝不拿一份"差不多"的文件顶替。
  if (p == "/api/script-archive/fetch-body" && identical(req$method, "POST")) {
    if (!exists("sa_request_bodies", mode = "function"))
      return(send_json(con, 503, list(ok = FALSE, code = "archive_module_missing",
        error = "缺少 engine/script_archive.R")))
    b <- body_json()
    rid <- as.character(b$record %||% "")
    if (!nzchar(rid)) return(send_json(con, 400, list(ok = FALSE, error = "缺 record")))
    if (grepl("\\.\\.", rid) || grepl("[/\\\\]", rid))
      return(send_json(con, 400, list(ok = FALSE, error = "record 不合法")))
    if (exists("sa_visible", mode = "function")) {
      vis3 <- sa_visible(ENGINE_DIR,
                         owner_key = as.character(owner_ctx_key(owner_ctx) %||% ""),
                         is_admin = local({
                           a0 <- tryCatch(admin_identity(APP_DIR, owner_ctx), error = function(e) NULL)
                           isTRUE(a0$is_admin) }),
                         isolation_on = local({
                           p0 <- if (isTRUE(HAS_ADMIN_POLICY)) record_policy(APP_DIR) else
                             list(record_isolation = TRUE, write_isolation = TRUE)
                           isTRUE(p0$record_isolation) && isTRUE(p0$write_isolation) }),
                         app_dir = APP_DIR)
      if (!(rid %in% vis3$ids))
        return(send_json(con, 403, list(ok = FALSE, code = "not_visible",
          error = "这条脚本记录不属于当前身份")))
    }
    r <- tryCatch(sa_request_bodies(APP_DIR, ENGINE_DIR, rid,
                                    owner_key = as.character(owner_ctx_key(owner_ctx) %||% "")),
                  error = function(e) list(ok = FALSE, code = "request_failed",
                                           error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  if (p == "/api/archive-read") {
    rid <- as.character(q$record %||% "")
    rel <- as.character(q$rel %||% "")
    if (!nzchar(rid) || !nzchar(rel))
      return(send_json(con, 400, list(ok = FALSE, error = "缺 record 或 rel")))
    if (grepl("\\.\\.", rid) || grepl("\\.\\.", rel))
      return(send_json(con, 400, list(ok = FALSE, error = "路径不合法")))
    # 读正文也要过可见性：列表挡住了、这条路不挡，等于给了个绕过去的门。
    if (exists("sa_visible", mode = "function")) {
      visr <- sa_visible(ENGINE_DIR,
                         owner_key = as.character(owner_ctx_key(owner_ctx) %||% ""),
                         is_admin = local({
                           a0 <- tryCatch(admin_identity(APP_DIR, owner_ctx),
                                          error = function(e) NULL)
                           isTRUE(a0$is_admin) }),
                         isolation_on = local({
                           p0 <- if (isTRUE(HAS_ADMIN_POLICY)) record_policy(APP_DIR) else
                             list(record_isolation = TRUE, write_isolation = TRUE)
                           isTRUE(p0$record_isolation) && isTRUE(p0$write_isolation) }))
      if (!(rid %in% visr$ids))
        return(send_json(con, 403, list(ok = FALSE, code = "not_visible",
          error = "这条脚本记录不属于当前身份")))
    }
    base <- normalizePath(file.path(ENGINE_DIR, "library", "archive", rid), winslash = "/", mustWork = FALSE)
    fp <- normalizePath(file.path(base, rel), winslash = "/", mustWork = FALSE)
    if (!startsWith(fp, paste0(base, "/")) || !file.exists(fp))
      return(send_json(con, 404, list(ok = FALSE, error = "存档里没有这个文件")))
    raw <- readBin(fp, "raw", n = min(file.size(fp), 400000))
    txt <- tryCatch(rawToChar(raw), error = function(e) "")
    return(send_json(con, 200, list(
      ok = TRUE, record = rid, rel = rel, name = basename(fp),
      size = as.numeric(file.size(fp)),
      mtime = format(file.info(fp)$mtime, "%Y-%m-%dT%H:%M:%S"),
      content = enc2utf8(txt))))
  }

  # ---- 脚本数据库里点开一个文件 ----
  #   库里的文件不在 engine 根下（skill 在 skills/，B_analysis 在 library/），
  #   走 /api/script-read 会被安全校验挡掉。这里只放行"库表登记过的那些路径"，
  #   不做任意路径读取。
  if (p == "/api/lib-read") {
    want <- enc2utf8(gsub("\\\\", "/", as.character(q$path %||% "")))
    if (!nzchar(want)) return(send_json(con, 400, list(ok = FALSE, error = "缺 path")))
    idx2 <- tryCatch(read_library_index(ENGINE_DIR), error = function(e) NULL)
    if (is.null(idx2)) return(send_json(con, 400, list(ok = FALSE, error = "读不到库表")))
    allowed <- character(0)
    for (e in (idx2$entries %||% list())) {
      fl <- e$files
      if (is.list(fl) && length(fl)) {
        for (f in fl) {
          fp0 <- as.character(if (is.list(f)) f$path %||% "" else f)
          allowed <- c(allowed, fp0)
          if (!grepl("^(/|[A-Za-z]:/)", fp0))
            allowed <- c(allowed, file.path(ENGINE_DIR, "library", fp0))
        }
      }
      pr <- as.character(e$path %||% "")
      if (nzchar(pr)) {
        ab <- file.path(ENGINE_DIR, "library", pr)
        if (dir.exists(ab)) allowed <- c(allowed, list.files(ab, recursive = TRUE, full.names = TRUE))
      }
    }
    allowed <- unique(enc2utf8(gsub("\\\\", "/", allowed)))
    allowed <- allowed[nzchar(allowed)]
    hit <- allowed[vapply(allowed, function(x) {
      identical(normalizePath(x, winslash = "/", mustWork = FALSE),
                normalizePath(want, winslash = "/", mustWork = FALSE))
    }, TRUE)]
    if (!length(hit) || !file.exists(hit[1]))
      return(send_json(con, 404, list(ok = FALSE, error = "这个文件不在库表清单里，或本机没有它")))
    fp <- hit[1]
    raw <- readBin(fp, "raw", n = min(file.size(fp), 400000))
    txt <- tryCatch(rawToChar(raw), error = function(e) "")
    return(send_json(con, 200, list(
      ok = TRUE, path = want, name = basename(fp),
      size = as.numeric(file.size(fp)),
      mtime = format(file.info(fp)$mtime, "%Y-%m-%dT%H:%M:%S"),
      content = enc2utf8(txt))))
  }

  if (p == "/api/script-read") {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    rel <- as.character(q$rel %||% "")
    full <- sop_safe_path(ENGINE_DIR, rel)
    if (is.null(full) || !file.exists(full))
      return(send_json(con, 404, list(ok = FALSE, error = "找不到这个脚本")))
    txt <- sop_read_text(full)
    info <- file.info(full)
    return(send_json(con, 200, list(
      ok = TRUE, rel = rel, md5 = sop_md5(full),
      size = as.numeric(info$size),
      mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S"),
      truncated = nchar(txt, type = "bytes") > 400000L,
      content = substr(txt, 1, 400000L),
      backups = sop_backups(ENGINE_DIR, rel)
    )))
  }

  # 机械检查：问"这个脚本能不能接进现有工作流"，立刻给答案
  #   两种用法：
  #     rel       查磁盘上现在这一份
  #     rel+content  查一份草稿 —— 改完还没保存时，想知道"改完还接得上吗"
  #   漏掉 content 转发的话，前端点了「检查这份改动」也只会回磁盘上旧版的结论，
  #   用户以为改动没问题，其实检查的是旧内容。
  if (p == "/api/script-review") {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    b <- if (identical(req$method, "POST")) body_json() else list()
    rel <- as.character(b$rel %||% q$rel %||% "")
    draft <- if (!is.null(b$content) && length(b$content) && nzchar(as.character(b$content)))
      as.character(b$content) else NULL
    r <- tryCatch(sop_review(ENGINE_DIR, rel, content = draft),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 写入：置换已有脚本（必须存在）/ 新增（必须不存在）
  if (p == "/api/script-put" && identical(req$method, "POST")) {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    denied <- code_write_denied(); if (!is.null(denied)) return(denied)
    b <- body_json()
    r <- tryCatch(sop_put(ENGINE_DIR, as.character(b$rel %||% ""),
                          as.character(b$content %||% ""),
                          mode = as.character(b$mode %||% "replace")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (isTRUE(r$ok)) {
      # 每次改动都留痕，和台账分开：台账记"跑"，这里记"改"
      if (HAS_HISTORY && !is.null(runDir)) {
        try(hist_snapshot(runDir, label = paste0("script-edit-", basename(as.character(b$rel %||% ""))),
                          kind = "auto", note = "script replaced"), silent = TRUE)
      }
    }
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 从备份还原
  #   backup 收的是备份 ID（<时间戳>/<相对路径>），不是绝对路径。
  #   前端列表里回传的也是这个 ID，两边保持一致。
  if (p == "/api/script-restore" && identical(req$method, "POST")) {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    denied <- code_write_denied(); if (!is.null(denied)) return(denied)
    b <- body_json()
    r <- tryCatch(sop_restore_backup(ENGINE_DIR, as.character(b$backup %||% b$backup_id %||% "")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 上传：先落到 incoming，不直接覆盖任何在用脚本
  #   直接覆盖等于让未审阅的代码变成在用代码，而且没有回头路。
  if (p == "/api/script-upload" && identical(req$method, "POST")) {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    denied <- code_write_denied(); if (!is.null(denied)) return(denied)
    b <- body_json()
    r <- tryCatch(sop_incoming_add(ENGINE_DIR, as.character(b$name %||% "upload.R"),
                                   as.character(b$content %||% ""),
                                   note = as.character(b$note %||% "")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  if (p == "/api/incoming") {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    if (identical(req$method, "POST")) {
      # 采纳会把上传件写成在用脚本，等同于改应用代码，先过同一道门。
      #   drop 只是丢弃一个没采纳的上传件，不碰在用脚本，所以不拦。
      b <- body_json()
      act <- as.character(b$action %||% "drop")
      id <- as.character(b$id %||% "")
      if (!identical(act, "drop")) { denied <- code_write_denied(); if (!is.null(denied)) return(denied) }
      if (identical(act, "drop")) {
        return(send_json(con, 200, sop_incoming_drop(ENGINE_DIR, id)))
      }
      if (identical(act, "adopt")) {
        # 采纳：把它写成目标位置，原文件留着（等于一次"带来源的替换"）
        txt <- sop_incoming_text(ENGINE_DIR, id)
        if (is.null(txt)) return(send_json(con, 400, list(ok = FALSE, error = "找不到这个上传件")))
        r <- tryCatch(sop_put(ENGINE_DIR, as.character(b$rel %||% ""), txt,
                              mode = as.character(b$mode %||% "replace")),
                      error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
      }
      return(send_json(con, 400, list(ok = FALSE, error = "未知操作")))
    }
    return(send_json(con, 200, list(ok = TRUE, items = sop_incoming_list(ENGINE_DIR))))
  }

  # 有改动想法但没有现成脚本：把请求连同当前文件一起交给助手
  if (p == "/api/script-change" && identical(req$method, "POST")) {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    # 这条路是把「改这个脚本」交给助手，等于让助手改应用代码，同样要过门。
    denied <- code_write_denied(); if (!is.null(denied)) return(denied)
    b <- body_json()
    rel <- as.character(b$rel %||% "")
    idea <- as.character(b$idea %||% "")
    if (!nzchar(idea)) return(send_json(con, 400, list(ok = FALSE, error = "请先写下想改什么")))
    r <- tryCatch(sop_change_request(APP_DIR, ENGINE_DIR, rel, idea, run_dir = runDir),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 请助手做一次"读得懂代码"的审阅（机械检查之外的判断）
  if (p == "/api/script-ask" && identical(req$method, "POST")) {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    b <- body_json()
    r <- tryCatch(sop_review_request(APP_DIR, ENGINE_DIR, as.character(b$rel %||% ""),
                                     run_dir = runDir,
                                     question = as.character(b$question %||% "")),
                  error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400, r))
  }

  # 统一的任务队列：脚本审阅 / 脚本改动 / 环节质检 / 跑某一步
# ---- 会话指令：页面上写下要做什么，由助手去执行 ------------------
  #
  #   为什么要有这条通道：
  #     应用跑在这台机器上，连不到用户内网的服务器；
  #     但用户的工作电脑能连上。所以"去服务器上做某件事"这件事，
  #     只能由能连的那一端执行。
  #   这里负责三件：收下指令、记下状态、把结果收回来显示。
  #   它不假装自己能执行 —— 状态里会明说"等助手执行"。
  if (p == "/api/instruct") {
    f <- file.path(APP_DIR, "state", "instruct.jsonl")
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)

    read_all <- function() {
      if (!file.exists(f)) return(list())
      ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character(0))
      ln <- ln[nzchar(trimws(ln))]
      out <- list()
      for (x in ln) {
        j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(j)) out[[length(out) + 1L]] <- j
      }
      out
    }
    write_all <- function(items) {
      con <- file(f, open = "w", encoding = "UTF-8")
      for (it in items) {
        writeLines(jsonlite::toJSON(it, auto_unbox = TRUE, null = "null"), con, useBytes = TRUE)
      }
      close(con)
      Sys.chmod(f, mode = "0600")
    }

    if (identical(req$method, "POST")) {
      b <- body_json()

      # 回写：助手执行完把结果写回来
      if (!is.null(b$result_for)) {
        rid <- as.character(b$result_for)
        items <- read_all()
        hit <- FALSE
        for (i in seq_along(items)) {
          if (identical(as.character(items[[i]]$id %||% ""), rid)) {
            items[[i]]$status <- as.character(b$status %||% "done")
            items[[i]]$summary <- as.character(b$summary %||% "")
            items[[i]]$detail <- as.character(b$detail %||% "")
            items[[i]]$finished_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
            hit <- TRUE
          }
        }
        if (!hit) {
          # 助手回写时可能不带原指令号（任务由平台承载，页面并不知道它）。
          #   这时不该拒收 —— 结果本身是有价值的，拒收等于白跑一趟。
          #   直接追加一条已完成记录，页面照样看得到。
          items[[length(items) + 1L]] <- list(
            id = if (nzchar(rid)) rid else paste0("instr-", format(Sys.time(), "%Y%m%d%H%M%S")),
            text = as.character(b$text %||% "（由助手执行）"),
            status = as.character(b$status %||% "done"),
            created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            target_host = as.character(b$host %||% ""),
            target_user = "",
            by = "assistant",
            summary = as.character(b$summary %||% ""),
            detail = as.character(b$detail %||% ""))
        }
        write_all(items)
        return(send_json(con, 200, list(ok = TRUE, id = rid)))
      }

      # 新指令
      txt <- trimws(as.character(b$text %||% ""))
      if (!nzchar(txt)) return(send_json(con, 400, list(ok = FALSE, error = "指令是空的")))
      prof <- tryCatch(read_json_file(file.path(APP_DIR, "config", "server_profile.json")),
                       error = function(e) NULL) %||% list()
      srv <- prof$server %||% list()

      # 注入结果：这一段只有浏览器知道，服务端补不上。
      #   没有它，记录里只能证明"文本生成了"，证明不了"文本交出去了"，
      #   而这两件事恰恰是排查注入问题时要分开的两件事。
      #   昨天那几条记录就卡在这个盲区里：文本在，但没有任何一处写着
      #   平台到底收没收、通道到底连没连上。
      inj_note <- ""
      inj <- b$inject
      if (is.list(inj)) {
        code <- as.character(inj$code %||% "")
        why <- as.character(inj$why %||% "")
        inj_note <- paste0("注入：", if (isTRUE(inj$ok)) "已交给平台" else "未交出去",
                           if (nzchar(code)) paste0("（", code, "）") else "",
                           if (nzchar(why)) paste0("；", why) else "")
      }

      it <- list(
        id = paste0("instr-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                    paste(sample(c(letters, 0:9), 4L), collapse = "")),
        text = txt,
        status = "pending",
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        target_host = as.character(srv$host %||% ""),
        target_user = as.character(srv$user %||% ""),
        by = "user",
        # 注入结果：这一段只有浏览器知道，服务端补不上。
        #   没有它，记录里只能证明"文本生成了"，证明不了"文本交出去了"，
        #   而这两件事恰恰是排查注入问题时要分开的两件事。
        summary = "",
        detail = inj_note)
      con2 <- file(f, open = "a", encoding = "UTF-8")
      writeLines(jsonlite::toJSON(it, auto_unbox = TRUE, null = "null"), con2, useBytes = TRUE)
      close(con2)
      Sys.chmod(f, mode = "0600")
      return(send_json(con, 200, list(ok = TRUE, instruction = it)))
    }

    items <- read_all()
    items <- utils::tail(items, 30)
    return(send_json(con, 200, list(ok = TRUE, instructions = rev(items))))
  }

  # ---- 待发送队列：通道不通时指令不丢 -------------------------------
  #
  #   为什么要有：页面点一下，指令本该变成一条消息进会话。可这条路要平台
  #   先跟页面握手；握手不来，指令就只停在浏览器里，刷新一下连这点都没了。
  #   所以落到本机一份：页面显示"已存进队列"，助手也能从这里取走执行。
  #   只放指令原文，不含口令（口令在 config，从不进这段文本）。
  if (p == "/api/outbox") {
    f <- file.path(APP_DIR, "state", "outbox.jsonl")
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)

    ob_read <- function() {
      if (!file.exists(f)) return(list())
      ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                     error = function(e) character(0))
      ln <- ln[nzchar(trimws(ln))]
      out <- list()
      for (x in ln) {
        j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(j)) out[[length(out) + 1L]] <- j
      }
      out
    }

    if (identical(req$method, "POST")) {
      b <- body_json()
      rid <- as.character(b$done %||% "")
      if (nzchar(rid)) {
        # 助手做完标记一下，免得同一条被重复执行。
        items <- ob_read()
        for (i in seq_along(items)) {
          if (identical(as.character(items[[i]]$id %||% ""), rid)) {
            items[[i]]$status <- "done"
            items[[i]]$finished_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
          }
        }
        con2 <- file(f, open = "w", encoding = "UTF-8")
        for (it in items) writeLines(jsonlite::toJSON(it, auto_unbox = TRUE, null = "null"), con2, useBytes = TRUE)
        close(con2)
        return(send_json(con, 200, list(ok = TRUE, id = rid)))
      }
      txt <- trimws(as.character(b$text %||% ""))
      if (!nzchar(txt)) return(send_json(con, 400, list(ok = FALSE, error = "指令是空的")))
      sid <- as.character(b$session %||% "")
      it <- list(
        id = paste0("ob-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                    paste(sample(c(letters, 0:9), 4L), collapse = "")),
        text = txt,
        status = "pending",
        session = sid,
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      con2 <- file(f, open = "a", encoding = "UTF-8")
      writeLines(jsonlite::toJSON(it, auto_unbox = TRUE, null = "null"), con2, useBytes = TRUE)
      close(con2)
      Sys.chmod(f, mode = "0600")
      return(send_json(con, 200, list(ok = TRUE, id = it$id, instruction = it)))
    }

    items <- ob_read()
    items <- utils::tail(items, 20)
    return(send_json(con, 200, list(ok = TRUE, items = rev(items))))
  }

  if (p == "/api/queue") {
    if (!HAS_SCRIPT_OPS) return(send_json(con, 200, list(ok = FALSE, error = "缺少 engine/script_ops.R")))
    # 服务可能挂在公网地址上，而队列里的任务带着服务器连接信息。
    #   口令只落盘给助手读，绝不回传给浏览器 —— 和 /api/task 同一条规矩。
    #   漏掉这一步就等于把口令印在网页上。
    tasks <- sop_queue_list(APP_DIR)
    tasks <- lapply(tasks, function(x) {
      if (!is.null(x$server)) x$server$password <- NULL
      if (!is.null(x$static)) x$static <- NULL
      x
    })
    return(send_json(con, 200, list(ok = TRUE, tasks = tasks)))
  }

  if (p == "/api/library") {
    pk <- file.path(ENGINE_DIR, "pick.R")
    if (!file.exists(pk)) {
      return(send_json(con, 200, list(ok = FALSE, error = "找不到 engine/pick.R")))
    }
    src <- tryCatch({ source(pk, local = TRUE); TRUE }, error = function(e) FALSE)
    if (!src) return(send_json(con, 200, list(ok = FALSE, error = "pick.R 载入失败")))

    idx <- read_library_index(ENGINE_DIR)
    if (is.null(idx)) {
      return(send_json(con, 200, list(ok = FALSE, error = "读不到库表 library/index.json")))
    }

    # 本机是否已扫到甲方脚本（跟数据放一起的那套）
    local_found <- FALSE
    if (!is.null(runDir)) {
      sf <- file.path(status_dir(runDir), "scripts_scan.json")
      if (file.exists(sf)) {
        sj <- read_json_file(sf)
        if (!is.null(sj) && !is.null(sj$n_found)) {
          local_found <- suppressWarnings(as.integer(sj$n_found)) > 0L
        }
      }
    }

    env <- detect_env(mem_gb = NULL)
    a_pick <- tryCatch(pick_set(ENGINE_DIR, category = "A", demand = "D1_pipeline",
                                local_scripts_found = local_found),
                       error = function(e) list(error = conditionMessage(e)))
    b_pick <- tryCatch(pick_set(ENGINE_DIR, category = "B"),
                       error = function(e) list(error = conditionMessage(e)))

    # 每一条目的落地状态（文件在不在、状态是否 ready）
    entries <- lapply(idx$entries, function(e) {
      r <- resolve_entry(ENGINE_DIR, e)
      rq <- check_requires(e$requires, env)
      r$requires_ok <- rq$ok
      r$requires_why <- rq$reasons
      r$usable <- isTRUE(r$ok) && isTRUE(rq$ok)
      r$residency <- e$residency %||% ""
      r$own_locations <- e$own_locations %||% list()
      r$notes <- e$notes %||% ""
      r$env <- e$env %||% "*"
      r$demand <- e$demand %||% ""
      # 条目里到底有哪几个文件：脚本数据库要能展开看到具体脚本，
      #   只给"4 个文件、4 个都在"看不出是哪四个。
      #   skill 条目在库表里登记了 files（带绝对路径）；set 条目直接列目录。
      r$files <- tryCatch({
        fl <- e$files
        if (is.list(fl) && length(fl)) {
          # 库里记的文件名有三种写法，都得认，否则整层全显示"缺"——
          #   那看起来像数据不全，其实是路径没解析对。
          #     绝对路径            B_analysis 那几条、skill 条目都是这种
          #     相对 library/       形如 B_analysis/plots/x.R
          #     光文件名            形如 00_run_all.R，实际在条目 path 那个目录里
          ep <- as.character(r$path_rel %||% "")
          lapply(fl, function(f) {
            fp <- as.character(if (is.list(f)) f$path %||% "" else f)
            cand <- fp
            if (!grepl("^(/|[A-Za-z]:/)", fp)) {
              if (grepl("/", fp)) {
                cand <- file.path(ENGINE_DIR, "library", fp)
              } else if (nzchar(ep)) {
                cand <- file.path(ENGINE_DIR, "library", ep, fp)
              } else {
                cand <- file.path(ENGINE_DIR, "library", fp)
              }
            }
            # 仍找不到时，在条目目录下递归找同名文件：
            #   适配那套把 lib/ 子模块登记成光文件名，实际在 <path>/lib/ 下。
            #   不递归就永远报"缺"，那会让人以为库残缺。
            #   匹配用 basename 相等，不拼正则 —— 文件名里有 . 和 _，
            #   手拼转义容易写错，一错就是整个服务起不来。
            if (!file.exists(cand) && nzchar(ep)) {
              base <- file.path(ENGINE_DIR, "library", ep)
              if (dir.exists(base)) {
                hitf <- list.files(base, recursive = TRUE, full.names = TRUE)
                same <- hitf[basename(hitf) == basename(fp)]
                if (length(same)) cand <- same[1]
              }
            }
            list(name = basename(fp), path = if (file.exists(cand)) cand else fp,
                 bytes = if (file.exists(cand)) as.numeric(file.size(cand)) else NA_real_,
                 exists = file.exists(cand))
          })
        } else if (nzchar(as.character(r$path_abs %||% "")) && dir.exists(r$path_abs)) {
          fs <- list.files(r$path_abs, recursive = TRUE, full.names = TRUE)
          lapply(utils::head(fs, 60), function(fp) list(
            name = sub(paste0("^", r$path_abs, "/?"), "", fp),
            path = fp,
            bytes = as.numeric(file.size(fp)),
            exists = TRUE))
        } else list()
      }, error = function(e2) list())
      r
    })

    return(send_json(con, 200, list(
      ok = TRUE,
      env = list(id = env$id, os = env$os, memory_gb = env$memory_gb, why = env$why),
      local_scripts_found = local_found,
      categories = idx$categories,
      # 三层结构与每层说明：页面「脚本数据库」要把结构本身讲清楚，
      #   不能只铺一堆条目 —— 看不出条目属于哪一层，就等于没结构。
      structure = as.character(idx$structure %||% ""),
      layers = idx$layers,
      entries = entries,
      pick_A = if (is.null(a_pick$error)) a_pick else list(ok = FALSE, error = a_pick$error),
      pick_B = if (is.null(b_pick$error)) b_pick else list(ok = FALSE, error = b_pick$error)
    )))
  }

  if (p == "/api/artifacts") {
    access <- record_preflight$access
    sid_art <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    runDir <- ctx$run_dir
    st$runDir <- runDir
    st$runDirRemote <- ctx$run_dir_remote
    # 服务器模式：运行目录在服务器上，本机 dir.exists 永远是假。
    #   以前这里照本机那套 list.files 去列，结果永远 items=[]，
    #   页面就写「还没有产物」—— 而服务器上那些图、表、rds 都在。
    #   这不是"没产物"，是"列产物的地方错了"。改成读执行端回写的那份清单。
    if (isTRUE(st$runDirRemote)) {
      # 服务器模式下列产物：以各步骤回写里带的 artifacts 为准。
      #   为什么不再只读 artifacts.json：那份文件要靠单独一次「列清单」动作写，
      #   实际使用中没人会去点它，于是页面永远显示"还没让执行端列过"——
      #   而每个步骤回写时早就把产物路径与大小带回来了。手上有的东西不摆出来，
      #   等于让人以为没产物。
      dArt <- remote_steps_read(sid_art)
      items <- list()
      seen <- character(0)
      for (r in (dArt$steps %||% list())) {
        for (a in (r$artifacts %||% list())) {
          rel <- if (is.list(a)) as.character(a$rel %||% a$path %||% "") else as.character(a)
          if (!nzchar(rel)) next
          rel <- gsub("\\\\", "/", rel)
          if (rel %in% seen) next
          seen <- c(seen, rel)
          ext <- paste0(".", tolower(tools::file_ext(rel)))
          kind <- ARTIFACT_KINDS[[ext]]
          if (is.null(kind)) kind <- "其他"
          sz <- if (is.list(a) && !is.null(a$bytes)) as.numeric(a$bytes) else NA_real_
          mt <- if (is.list(a)) as.character(a$mtime %||% "") else ""
          items[[length(items) + 1L]] <- list(
            rel = rel, name = basename(rel), kind = kind,
            size = sz, mtime = mt, step = as.character(r$id %||% ""),
            step_title = as.character(r$title %||% r$id %||% ""),
            full = NULL)
        }
      }
      # The legacy global artifacts.json is intentionally not consulted:
      # it mixes records. Only this session's index is allowed to supplement
      # the verified writeback list.
      a0 <- record_artifact_index_read(APP_DIR, sid_art)
      for (x in (a0$items %||% list())) {
        rel <- as.character(x$rel %||% "")
        if (!nzchar(rel) || rel %in% seen) next
        seen <- c(seen, rel)
        items[[length(items) + 1L]] <- x
      }
      # 迁移来的记录必须能从已验证记录补出产物清单。
      #   原实例的执行端清单在建库时已拆进 step_attempts，remote_steps.json
      #   只剩空壳，record_artifact_index_read 那份也从未写过 —— 于是上面两处
      #   都列不出东西，页面写「还没有产物」，而镜像里躺着 195 个真实文件。
      #   只读回退：按本会话已验证记录重拼，同一 rel 取最新一条，与
      #   artifact_expected_md5 同一口径。不推测、不补造不存在的文件。
      if (length(items) == 0L && isTRUE(DB_READY)) {
        rowsA <- tryCatch(DBI::dbGetQuery(DB_CON, paste0(
          "SELECT a.payload AS payload, COALESCE(a.verified_at, a.reported_at) AS t ",
          "FROM step_attempts a ",
          "WHERE a.status = 'verified' ",
          "AND json_extract(a.payload, '$.session_id') = ? ",
          "ORDER BY t ASC"), list(sid_art)),
          error = function(e) NULL)
        byRel <- list()
        if (!is.null(rowsA) && nrow(rowsA)) {
          for (iA in seq_len(nrow(rowsA))) {
            pA <- tryCatch(jsonlite::fromJSON(rowsA$payload[[iA]], simplifyVector = FALSE),
                           error = function(e) NULL)
            if (is.null(pA)) next
            for (aA in (pA$artifacts %||% list())) {
              relA <- gsub("\\\\", "/", as.character(if (is.list(aA)) aA$rel %||% "" else aA))
              if (!nzchar(relA)) next
              byRel[[relA]] <- aA   # 后写覆盖先写 = 取时间最新的一条
            }
          }
        }
        for (relA in names(byRel)) {
          if (relA %in% seen) next
          aA <- byRel[[relA]]
          extA <- paste0(".", tolower(tools::file_ext(relA)))
          kindA <- ARTIFACT_KINDS[[extA]]
          if (is.null(kindA)) kindA <- "其他"
          szA <- suppressWarnings(as.numeric(if (is.list(aA)) aA$bytes %||% NA else NA))
          items[[length(items) + 1L]] <- list(
            rel = relA, name = basename(relA), kind = kindA,
            size = if (is.finite(szA)) szA else NULL,
            mtime = as.character(if (is.list(aA)) aA$mtime %||% "" else ""),
            step = "", step_title = "", full = NULL)
          seen <- c(seen, relA)
        }
      }
      listed <- length(items) > 0
      # 给每个产物标三件事：能不能在应用里看、多大、取回了没有。
      #   不标这三件，页面只能一律写「需从服务器取」——
      #   人没法判断"哪些我可以马上看、哪些太大、哪些还得取"。
      sidA <- sid_art
      items <- lapply(items, function(x) {
        rel <- as.character(x$rel %||% "")
        k <- if (isTRUE(HAS_MIRROR)) mirror_kind(rel) else ""
        can <- nzchar(k) && (is.na(suppressWarnings(as.numeric(x$size %||% NA))) ||
                             as.numeric(x$size %||% 0) <= MIRROR_MAX_BYTES)
        x$view_kind <- k
        x$viewable <- isTRUE(can)
        # 已取回的判据从"文件在不在"改成"这份缓存是否就是最新那一份"。
        #   事故原样：重新生成后旧镜像还在，页面写已取回，下载拿到旧版。
        wantMd5 <- if (isTRUE(HAS_MIRROR)) artifact_expected_md5(sidA, rel) else ""
        vv <- if (isTRUE(HAS_MIRROR)) tryCatch(mirror_validity(sidA, rel, wantMd5),
                                               error = function(e) NULL) else NULL
        cacheSt <- if (is.null(vv)) "missing" else as.character(vv$state %||% "missing")
        if (identical(cacheSt, "stale")) {
          # 旧缓存不删，挪到 _stale；随后按未取回呈现，人正常点取回即可。
          mv <- tryCatch(mirror_quarantine(sidA, rel), error = function(e) "")
          if (nzchar(mv)) message("mirror_stale_quarantined ", rel, " -> ", mv)
        }
        x$mirrored <- identical(cacheSt, "ok")
        x$cache_state <- cacheSt
        x$mirror_md5 <- if (is.null(vv)) "" else as.character(vv$actual %||% "")
        x$expected_md5 <- if (is.null(vv)) "" else as.character(vv$want %||% "")
        x$too_big <- nzchar(k) && !isTRUE(can)
        # 单个文件能不能取回、取到哪一步，随每条一起给。
        #   原来只按 8MB 判"太大"，PPT 与打包 ZIP 全被藏掉，页面只剩
        #   "需从服务器取"；实际上分片通道能搬，缺的是这一层的状态。
        st1 <- if (isTRUE(HAS_MIRROR)) mirror_item_state(sidA, rel, wantMd5)
               else list(state = "", error = "")
        has_pipe <- isTRUE(HAS_MIRROR) && (nzchar(k) || grepl("\\.(pptx?|zip)$", rel))
        x$fetchable <- isTRUE(has_pipe)
        # 只有"内容等于最新基准"才写 done；否则交回真实排队状态，
        #   这样取回按钮不会被一条历史 done 永久遮住。
        x$fetch_state <- if (isTRUE(x$mirrored)) "done" else as.character(st1$state %||% "")
        # 排队位次：把「正在取回…」变成可判断的等待。
        #   原来只有一个 pending，人看不出是排第 1 还是第 59，只能反复点、反复切页面。
        #   0 表示不在队列里（已取回、已失败，或根本没登记）。
        x$fetch_rank <- if (isTRUE(HAS_MIRROR) && !isTRUE(x$mirrored) &&
                            identical(as.character(x$fetch_state %||% ""), "pending"))
          tryCatch(mirror_queue_rank(sidA, rel), error = function(e) 0L) else 0L
        if (identical(cacheSt, "unverified"))
          x$fetch_error <- "本地有文件但查不到最新基准 md5，无法确认它是不是最新版本；不能据此下载"
        else x$fetch_error <- as.character(st1$error %||% "")
        x
      })
      return(send_json(con, 200, list(
        ok = TRUE, session_id = sid_art, mode = ctx$mode, remote = TRUE, listed = listed,
        outdir = enc2utf8(runDir),
        listed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        total = length(items),
        items = items)))
    }
    r <- list_artifacts(runDir)
    sidA2 <- sid_art
    its2 <- lapply(r$items, function(x) {
      y <- x[names(x) != "full"]
      rel <- as.character(y$rel %||% "")
      k <- if (isTRUE(HAS_MIRROR)) mirror_kind(rel) else ""
      y$view_kind <- k
      y$viewable <- nzchar(k)
      y$mirrored <- FALSE
      y$too_big <- FALSE
      y
    })
    return(send_json(con, 200, list(
      ok = TRUE, session_id = sid_art, mode = ctx$mode, remote = FALSE,
      outdir = enc2utf8(gsub("\\\\", "/", r$outdir)),
      items = its2)))
  }

  # 执行端把服务器上的产物清单回写过来。
  #   为什么必须由执行端列：应用在云端，到不了内网，list.files 看不到服务器磁盘。
  #   回写进来的只有元数据（相对路径、大小、时间），不搬文件内容 ——
  #   下载仍然走执行端或由人从服务器取，这一点在页面上写清楚。
  if (p == "/api/artifacts/import" && identical(req$method, "POST")) {
    b <- body_json()
    access <- record_preflight$access
    sid_art <- as.character(record_preflight$target$sid %||% "")
    its <- b$items %||% list()
    if (!is.list(its)) return(send_json(con, 400, list(ok = FALSE, error = "items 要是数组")))
    clean <- list()
    for (x in its) {
      nm <- as.character(x$rel %||% x$name %||% "")
      if (!nzchar(nm)) next
      nm <- gsub("\\\\", "/", nm)
      if (grepl("(^|/)\\.\\.(/|$)", nm)) next   # 不许带 ..
      ext <- paste0(".", tolower(tools::file_ext(nm)))
      kind <- ARTIFACT_KINDS[[ext]]
      if (is.null(kind)) kind <- "其他"
      clean[[length(clean) + 1L]] <- list(
        rel = nm, name = basename(nm), kind = kind,
        size = if (is.null(x$size)) NULL else as.numeric(x$size),
        mtime = as.character(x$mtime %||% ""),
        full = NULL)
    }
    obj <- list(session_id = sid_art,
                outdir = enc2utf8(as.character(b$outdir %||% access$cap$run_dir %||% "")),
                host = as.character(b$host %||% ""),
                exec_pc = as.character(b$exec_pc %||% ""),
                listed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                items = clean)
    record_artifact_index_write(APP_DIR, sid_art, obj)
    return(send_json(con, 200, c(list(ok = TRUE, total = length(clean),
                                    outdir = obj$outdir, listed_at = obj$listed_at),
                                 record_cap_fields(access$cap))))
  }

  # ---- 产物取回：让人在应用里直接点开看 ----
  #   为什么需要这一步：产物清单只有文件名和大小，人没法据此判断这一步对不对。
  #   应用到不了内网，所以只能"应用登记 -> 执行端取回 -> 应用落盘"，
  #   之后同一个文件就从本地读，不再需要服务器。
  if (p == "/api/artifact/request" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    b <- body_json()
    access <- record_preflight$access
    sid2 <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    rel <- as.character(b$rel %||% "")
    if (!nzchar(rel)) return(send_json(con, 400, list(ok = FALSE, error = "缺 rel")))
    # 以已知的最新基准为准，请求里带来的大小只作兜底。
    #   反例：页面停在旧版，用户点取回时 b$bytes 还是上一版的字节数；
    #   若优先采信它，新文件一回传就被判"大小与登记不一致"。
    #   所以只要库里能查到该产物最新验收的字节数，就必须覆盖浏览器送来的值。
    bBytes <- artifact_request_bytes(sid2, rel, b$bytes)
    r <- mirror_request(sid2, rel, bytes = bBytes,
                        run_dir = as.character(ctx$run_dir %||% ""),
                        want_md5 = artifact_expected_md5(sid2, rel))
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     c(r, record_cap_fields(access$cap))))
  }

  # 执行端来取：还没取回的清单。
  if (p == "/api/mirror/pending") {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 200, list(ok = TRUE, items = list())))
    auth <- mirror_receipt_auth(req, owner_ctx, b = c(q, body_json()))
    if (!isTRUE(auth$ok)) return(send_json(con, auth$status, auth))
    reconciled <- tryCatch(artifact_reconcile_pending(owner_key =
      if (isTRUE(auth$worker)) NULL else owner_ctx$key),
      error = function(e) list(issues = list(list(error = conditionMessage(e)))))
    # 允许执行端指定本批条数：默认 24，最多 500。
    #   为什么需要：队列里混着各种类型，只按固定条数取时，后面类型的活会长期轮不到。
    lim <- suppressWarnings(as.integer(qget(q, "limit") %||% 24L))
    if (is.na(lim) || lim < 1L) lim <- 24L
    if (lim > 500L) lim <- 500L
    items <- mirror_pending(lim)
    if (!isTRUE(auth$worker)) items <- Filter(function(it) {
      cap <- record_capability(DB_CON, APP_DIR, it$session, owner_ctx)
      isTRUE(cap$ok) && isTRUE(cap$is_owner) && isTRUE(cap$can_write)
    }, items)
    return(send_json(con, 200, list(ok = TRUE, items = items, reconcile = reconciled)))
  }

  # 执行端把文件内容传回来。md5 对不上就拒收并删掉。
  if (p == "/api/mirror/put" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    b <- body_json()
    sid2 <- as.character(b$session_id %||% "")
    rel2 <- as.character(b$rel %||% "")
    if (!nzchar(sid2) || !nzchar(rel2))
      return(send_json(con, 400, list(ok = FALSE, code = "binding_fields_required",
        error = "mirror 回传必须带 session_id 与 rel")))
    guard <- mirror_receipt_guard(req, owner_ctx, b)
    if (!isTRUE(guard$ok)) return(send_json(con, guard$status, guard))
    r <- mirror_put(sid2, rel2, b$b64, want_md5 = guard$md5, bytes = guard$bytes)
    if (isTRUE(r$ok)) mirror_done(sid2, rel2)
    return(send_json(con, if (isTRUE(r$ok)) 200 else 400,
                     r[setdiff(names(r), "path")]))
  }

  # 分片取回：网关对单次请求体有上限（实测约 1 MB），大一点的表和图
  #   走 /api/mirror/put 会直接 413。这里按片上传，最后一片拼装并校验 md5。
  #   为什么不用 /api/artifact/chunk：那条要求先有 fetch_jobs 的 expected_bytes，
  #   而取回器是按清单批量搬的，拿不到那个号。
  # 执行端取不回来（文件不在、大小不符、分片被拒）时登记原因。
  #   登记后该条不再进 pending，避免每分钟重试同一件注定失败的事。
  if (p == "/api/mirror/fail" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    b <- body_json()
    sid2 <- as.character(b$session_id %||% "")
    if (!nzchar(sid2))
      return(send_json(con, 400, list(ok = FALSE, code = "session_required",
        error = "mirror fail 必须带 session_id")))
    guard <- mirror_receipt_guard(req, owner_ctx, b, failure = TRUE)
    if (!isTRUE(guard$ok)) return(send_json(con, guard$status, guard))
    r <- mirror_fail(sid2, as.character(b$rel %||% ""), as.character(b$error %||% ""))
    if (!isTRUE(r$ok))
      return(send_json(con, 409, list(ok = FALSE, code = "not_registered",
        error = "这条取回没有登记，拒绝登记失败状态")))
    return(send_json(con, 200, r))
  }

  if (p == "/api/mirror/put-chunk" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    b <- body_json()
    sid2 <- as.character(b$session_id %||% "")
    if (!nzchar(sid2))
      return(send_json(con, 400, list(ok = FALSE, code = "session_required",
        error = "mirror chunk 必须带 session_id")))
    guard <- mirror_receipt_guard(req, owner_ctx, b)
    if (!isTRUE(guard$ok)) return(send_json(con, guard$status, guard))
    rel2 <- as.character(b$rel %||% "")
    idx <- suppressWarnings(as.integer(b$chunk_index %||% 0L))
    tot <- suppressWarnings(as.integer(b$chunks %||% 1L))
    if (!nzchar(rel2) || is.na(idx) || is.na(tot) || idx < 0L || tot < 1L || idx >= tot)
      return(send_json(con, 400, list(ok = FALSE, error = "缺 rel / chunk_index / chunks，或越界")))
    # 分片必须自带合法 md5 与登记总量：没有这两项就没法判断拼装结果对不对，
    #   放行等于把未经校验的字节落成"已取回"。
    want_md5 <- tolower(as.character(b$md5 %||% ""))
    total_bytes <- suppressWarnings(as.numeric(b$bytes %||% NA))
    if (length(want_md5) != 1L || is.na(want_md5) || !grepl("^[0-9a-f]{32}$", want_md5))
      return(send_json(con, 400, list(ok = FALSE, code = "bad_md5",
                                      error = "分片请求必须带 32 位 md5")))
    if (is.na(total_bytes) || total_bytes <= 0 || total_bytes != round(total_bytes))
      return(send_json(con, 400, list(ok = FALSE, code = "bad_size",
                                      error = "分片请求必须带登记字节数")))
    if (total_bytes > MIRROR_LARGE_BYTES)
      return(send_json(con, 413, list(ok = FALSE, code = "too_large",
        error = sprintf("超过分片通道上限 %s 字节", format(MIRROR_LARGE_BYTES, scientific = FALSE)))))
    baseline_verified <- TRUE
    fin <- mirror_path(sid2, rel2)
    if (!nzchar(fin)) return(send_json(con, 400, list(ok = FALSE, error = "路径不合法")))
    dir.create(dirname(fin), showWarnings = FALSE, recursive = TRUE)
    part <- tryCatch(jsonlite::base64_dec(as.character(b$b64 %||% "")), error = function(e) NULL)
    if (is.null(part)) return(send_json(con, 400, list(ok = FALSE, error = "分片不是合法 base64")))
    pf <- paste0(fin, ".part", sprintf("%04d", idx))
    # 同一片重复上传且内容不同，说明两次传输对不上，直接拒，别拼出坏文件。
    if (file.exists(pf)) {
      keep <- readBin(pf, "raw", file.info(pf)$size)
      if (!identical(keep, part))
        return(send_json(con, 409, list(ok = FALSE, code = "chunk_conflict",
          error = sprintf("第 %d 片与已收到的内容不一致", idx))))
    }
    con2 <- file(pf, open = "wb"); writeBin(part, con2); close(con2)
    if (idx + 1L < tot) return(send_json(con, 200, list(ok = TRUE, chunk_index = idx, complete = FALSE)))
    # 最后一片到了：按序拼装，校验，然后落到正式位置。
    parts <- paste0(fin, ".part", sprintf("%04d", seq_len(tot) - 1L))
    if (!all(file.exists(parts))) {
      unlink(parts)
      return(send_json(con, 409, list(ok = FALSE, code = "missing_chunks",
        error = "有分片没到，已清掉半成品，请重传")))
    }
    fz <- mirror_chunk_finalize(fin, tot, want_md5, total_bytes)
    if (!isTRUE(fz$ok)) {
      if (identical(fz$code, "missing_chunks"))
        return(send_json(con, 409, list(ok = FALSE, code = "missing_chunks",
          error = "有分片没到，已清掉半成品，请重传")))
      return(send_json(con, 409, c(fz, list(rel = rel2))))
    }
    tryCatch(mirror_done(sid2, rel2), error = function(e) NULL)
    return(send_json(con, 200, list(ok = TRUE, rel = rel2, bytes = fz$bytes,
                                    md5 = fz$md5, complete = TRUE,
                                    baseline_verified = baseline_verified)))
  }

  # ---- 生成交付 PPT（在应用本机做，不等服务器）----------------------
  #   以前的 PPT 环节要下发到服务器跑，缺 python、缺模板、素材不齐，
  #   任何一条不满足就停住，人只看到"点了没反应"。
  #   素材取回本机之后，本机就能生成：镜像里有 10.figs 的图与表，
  #   模板与脚本就在引擎里，python3 是系统自带的。
  #   所以这条端点直接在本机出 pptx，落到镜像目录，再登记成产物。
  if (p == "/api/ppt/build" && identical(req$method, "POST")) {
    if (!isTRUE(HAS_PPT_BUILD))
      return(send_json(con, 400, list(ok = FALSE, code = "no_module",
        error = "缺少 engine/ppt_build.R")))
    b <- body_json()
    access <- record_preflight$access
    sid_b <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    mroot <- mirror_dir(sid_b)
    if (!dir.exists(mroot))
      return(send_json(con, 409, list(ok = FALSE, code = "no_mirror",
        error = "这批结果的素材还没取回到应用本机。先点「全部取回」，取回完再生成 PPT。",
        hint = "取回是可重复的：已经取回的文件会跳过。")))
    title_b <- as.character(b$title %||% "")
    if (!nzchar(title_b)) {
      sess <- tryCatch(db_rows(DB_CON, "SELECT name FROM sessions WHERE session_id = ?",
                               list(sid_b)), error = function(e) NULL)
      title_b <- if (!is.null(sess) && nrow(sess)) as.character(sess$name[1]) else ""
    }
    r <- tryCatch(ppt_build_from_mirror(ENGINE_DIR, mroot,
             out_rel = as.character(b$out_rel %||% "11.report/analysis_report.pptx"),
             # 默认 error：缺图就停下并把缺什么报出来，不拿别的图顶上。
             #   以前默认 skip，缺图时会拿目录里第一张 png 充数，
             #   生成一份看着完整、其实有 7 页放错图的 PPT。
             #   宁可不出，也不能出错版 —— 出错版人会被误导着拿去交。
             title = title_b,
             on_missing = as.character(b$on_missing %||% "error")),
           error = function(e) list(ok = FALSE, code = "exception",
                                    error = conditionMessage(e)))
    if (isTRUE(r$ok)) {
      # 登记成产物，页面「产物下载」里直接能看到并下载。
      aj <- record_artifact_index_read(APP_DIR, sid_b)
      its <- aj$items %||% list()
      has <- FALSE
      for (i in seq_along(its)) {
        if (identical(as.character(its[[i]]$rel %||% ""), as.character(r$out_rel))) {
          its[[i]]$size <- r$bytes; its[[i]]$mtime <- format(Sys.time(), "%Y-%m-%dT%H:%M")
          has <- TRUE
        }
      }
      if (!has) its[[length(its) + 1L]] <- list(
        rel = r$out_rel, name = basename(r$out_rel), kind = "PPT",
        size = r$bytes, mtime = format(Sys.time(), "%Y-%m-%dT%H:%M"), full = NULL)
      aj$items <- its
      aj$listed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      tryCatch(record_artifact_index_write(APP_DIR, sid_b, aj), error = function(e) NULL)
      tryCatch(db_event(DB_CON, "ppt_built", owner_key = access$cap$owner_key,
                        payload = list(session = sid_b, rel = r$out_rel,
                                       bytes = r$bytes, missing = r$missing_dirs)),
               error = function(e) NULL)
      r$download_url <- paste0("/api/artifact/download?session=",
                               utils::URLencode(sid_b, reserved = TRUE),
                               "&rel=", utils::URLencode(as.character(r$out_rel), reserved = TRUE))
      r$ready <- TRUE
    } else {
      # ---- 缺图：不是写进报告就完了，回去把图真的画出来 ----
      #   以前这里回一句"缺 4 张图"结束，等于把活推回给人：
      #   人要自己判断该重跑哪一环、自己点、自己再回来点一次 PPT。
      #   现在的动作是：算出该补哪几环 → 自动入队 + 发指令
      #   → 那几环回写成功后自动重出 PPT 并登记成产物。
      #   人只需要在最后点一下下载。
      plan <- tryCatch(ppt_autofix_plan(r$missing), error = function(e) NULL)
      r$autofix <- plan
      stages_need <- as.character(unlist((plan %||% list())$stages %||% character(0)))
      if (length(stages_need)) {
        queued <- list()
        for (sd in stages_need) {
          ctr <- tryCatch({
            stg <- flow_stage(sd, flow_profile(sid_b))
            if (is.null(stg)) NULL else flow_execution(stg, flow_vars(
              ctx$server_place %||% list(), sid = sid_b))
          }, error = function(e) NULL)
          if (is.null(ctr) || !isTRUE(ctr$ok)) next
          autofix_sess <- tryCatch(sess_find(APP_DIR, sid_b), error = function(e) NULL)
          autofix_ref <- as.character((autofix_sess %||% list())$reference_id %||% "")
          tryCatch(exec_enqueue(sd, session_id = sid_b,
                                script = ctr$script,
                                script_args = paste(vapply(ctr$argv, shQuote, "", type = "sh"),
                                                    collapse = " "),
                                run_dir = ctx$run_dir, mode = ctx$mode,
                                reference_id = autofix_ref,
                                ref_mode = ctx$ref_mode, ref_path = ctx$ref_path),
                   error = function(e) NULL)
          instr <- tryCatch(remote_step_instruction(sd,
                     ctx$server_place %||% list(),
                     session_id = sid_b, req_id = ""),
                   error = function(e) "")
          if (nzchar(instr))
            tryCatch(inject_enqueue(text = instr, session = sid_b,
                                    reason = "ppt_autofix", step = sd,
                                    origin = "补图后重出 PPT"),
                     error = function(e) NULL)
          queued[[length(queued) + 1L]] <- sd
        }
        if (length(queued)) {
          ppt_pending_arm(sid_b, queued, title_b, r$missing)
          r$autofix_queued <- queued
          r$message <- paste0(
            "缺 ", length(as.character(unlist(r$missing %||% character(0)))),
            " 样素材，已自动把「", paste(unlist(queued), collapse = "、"),
            "」重新派下去补图；补完会自动重出 PPT，到时点下载即可。")
        }
      }
      unprod <- as.character(unlist((plan %||% list())$unproducible %||% character(0)))
      if (length(unprod)) {
        r$message <- paste0(r$message %||% "",
          if (nzchar(r$message %||% "")) "　" else "",
          "另有 ", length(unprod), " 样（", paste(unprod, collapse = "、"),
          "）不是出图脚本产出的，重跑补不出来，需要单独确认数据来源。")
      }
    }
    return(send_json(con, if (isTRUE(r$ok)) 200 else 409,
                     c(r, record_cap_fields(access$cap))))
  }

  # 打包交付物：把镜像里的图与 PPT 压成一个 zip。
  #   为什么要有这个接口：pack 这一步在流程定义里是「服务器上跑」的环节，
  #   而服务器模式下一环一环点下去，最后一步要等服务器回写。
  #   实际使用中图已经取回本机、PPT 也已经在本机生成，
  #   打包这件纯本地的事没必要再绕一趟服务器 —— 绕过去只会多一次等待和一个失败点。
  #   所以这里单开一条本机通道：素材在本机就能打，打完直接登记成产物可下载。
  if (p == "/api/pack/build" && identical(req$method, "POST")) {
    b <- body_json()
    access <- record_preflight$access
    sid_p <- as.character(record_preflight$target$sid %||% "")
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    mroot <- mirror_dir(sid_p)
    if (!dir.exists(mroot))
      return(send_json(con, 409, list(ok = FALSE, code = "no_mirror",
        error = "这批结果的素材还没取回到应用本机。先点「全部取回」，取回完再打包。")))

    zb <- tryCatch(zip_bin(), error = function(e) NULL)
    if (is.null(zb) || !nzchar(zb))
      return(send_json(con, 400, list(ok = FALSE, code = "no_zip",
        error = "这台机器上找不到 zip 或 tar，打不了包。")))

    # 要打进去的东西：图（必需）、PPT（可选但建议）、报告类文本（有就带）。
    #   缺哪样不硬失败 —— 但必须把「带了什么、没带什么」如实写回。
    #   静默打一个只有图的 zip，人会以为交付物齐了。
    want <- c("10.figs", "11.report/analysis_report.pptx",
              "9.annot/annot_report.md", "8.final")
    have <- character(0); lack <- character(0)
    for (w in want) {
      full <- file.path(mroot, w)
      if (file.exists(full)) have <- c(have, w) else lack <- c(lack, w)
    }
    if (!length(have))
      return(send_json(con, 409, list(ok = FALSE, code = "no_assets",
        error = "镜像里没有可打包的素材。先取回产物，或先生成图与 PPT。")))

    out_rel <- as.character(b$out_rel %||% "11.report/analysis_results.zip")
    out_abs <- file.path(mroot, out_rel)
    dir.create(dirname(out_abs), showWarnings = FALSE, recursive = TRUE)
    # 先删旧包：留着会让「这次没打成」被判成成功，人下载到的还是上一轮那个。
    if (file.exists(out_abs)) unlink(out_abs)

    old_wd <- getwd()
    ok_zip <- FALSE; blog <- character(0)
    tryCatch({
      setwd(mroot)
      # 目标写相对路径，不是 basename。
      #   2026-11-03 踩过一次：用 basename 时 zip 把包写到了当前目录（镜像根），
      #   而后面检查的是 11.report/ 下那个路径，于是「明明打成了」被判成
      #   zip_failed，日志还是空的 —— 因为 zip 一切正常，只是东西在别处。
      #   zip 的这个行为是对的（相对 CWD 解析目标），错的是传参。
      if (grepl("zip$", zb)) {
        # -r 递归；-q 安静；-X 去掉额外属性，跨机器解压更干净。
        o <- suppressWarnings(system2(zb, c("-r", "-q", "-X",
                                            shQuote(out_rel), have),
                                     stdout = TRUE, stderr = TRUE))
      } else {
        o <- suppressWarnings(system2(zb, c("-a", "-c", "-f",
                                            shQuote(out_rel), have),
                                     stdout = TRUE, stderr = TRUE))
      }
      blog <- utils::tail(as.character(o), 8L)
      ok_zip <- file.exists(out_abs) && file.info(out_abs)$size > 0
    }, error = function(e) blog <<- c(blog, conditionMessage(e)),
    finally = setwd(old_wd))

    if (!ok_zip) {
      return(send_json(con, 500, list(ok = FALSE, code = "zip_failed",
        error = "打包命令没有产出文件。", log = as.list(blog))))
    }

    bytes <- file.info(out_abs)$size
    md5v <- tryCatch(as.character(tools::md5sum(out_abs)), error = function(e) NA_character_)

    # 登记成产物：页面「产物下载」里直接能看到并下载，不必再走取回。
    aj <- record_artifact_index_read(APP_DIR, sid_p)
    its <- aj$items %||% list()
    has <- FALSE
    for (i in seq_along(its)) {
      if (identical(as.character(its[[i]]$rel %||% ""), out_rel)) {
        its[[i]]$size <- bytes
        its[[i]]$mtime <- format(Sys.time(), "%Y-%m-%dT%H:%M")
        has <- TRUE
      }
    }
    if (!has) its[[length(its) + 1L]] <- list(
      rel = out_rel, name = basename(out_rel), kind = "打包",
      size = bytes, mtime = format(Sys.time(), "%Y-%m-%dT%H:%M"), full = NULL)
    aj$items <- its
    aj$listed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    tryCatch(record_artifact_index_write(APP_DIR, sid_p, aj), error = function(e) NULL)
    tryCatch(db_event(DB_CON, "pack_built", owner_key = access$cap$owner_key,
                      payload = list(session = sid_p, rel = out_rel,
                                     bytes = bytes, included = as.list(have),
                                     lacking = as.list(lack))),
             error = function(e) NULL)

    return(send_json(con, 200, c(list(
      ok = TRUE, code = "ok", out_rel = out_rel, bytes = bytes,
      md5 = md5v, included = as.list(have), lacking = as.list(lack),
      log = as.list(blog),
      download_url = paste0("/api/artifact/download?session=",
                            utils::URLencode(sid_p, reserved = TRUE),
                            "&rel=", utils::URLencode(out_rel, reserved = TRUE)),
      ready = TRUE), record_cap_fields(access$cap))))
  }

  # 看图 / 看 PDF：直接从镜像读，浏览器内联显示。
  # ---- 同一环节的多套候选结果（筛群参数组合 / 注释版本）----
  #   为什么单独存：这些不是"某一步的产物清单"，而是"同一环节跑出来的几套结果，
  #   等人挑一套给下游用"。挑中的那套要能被下游读到，所以必须落盘，不能只放在浏览器里。
  if (p == "/api/variants") {
    access <- record_preflight$access
    vsid <- as.character(record_preflight$target$sid %||% "")
    vfile <- file.path(STATE_DIR, paste0("variants_",
      gsub("[^A-Za-z0-9_-]", "_", if (nzchar(vsid)) vsid else "_default"), ".json"))
    vread <- function() {
      d0 <- tryCatch(read_json_file(vfile), error = function(e) NULL)
      if (is.null(d0) || is.null(d0$stages)) list(stages = list()) else d0
    }

    if (identical(req$method, "GET")) {
      return(send_json(con, 200, c(list(ok = TRUE, session_id = vsid, stages = vread()$stages),
                                    record_cap_fields(access$cap))))
    }

    b <- body_json()
    act <- as.character(b$action %||% "save")
    stage <- as.character(b$stage %||% "")
    if (!nzchar(stage)) return(send_json(con, 400, list(ok = FALSE, error = "缺 stage")))

    d <- vread()
    st0 <- d$stages[[stage]]
    if (is.null(st0)) st0 <- list(items = list(), chosen = "")

    if (identical(act, "save")) {
      it <- b$item %||% list()
      vid <- as.character(it$id %||% "")
      if (!nzchar(vid)) vid <- paste0("v", as.integer(Sys.time()), sample(100:999, 1))
      row <- list(
        id = vid,
        params = it$params %||% list(),
        label = as.character(it$label %||% ""),
        outdir = as.character(it$outdir %||% ""),
        status = as.character(it$status %||% "pending"),
        note = as.character(it$note %||% ""),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      # 同 id 覆盖，否则追加：重试同一条不该在列表里出现两次
      keep <- Filter(function(x) !identical(as.character(x$id %||% ""), vid), st0$items %||% list())
      st0$items <- c(keep, list(row))
    } else if (identical(act, "choose")) {
      vid <- as.character(b$id %||% "")
      hit <- Filter(function(x) identical(as.character(x$id %||% ""), vid), st0$items %||% list())
      if (!length(hit)) return(send_json(con, 404, list(ok = FALSE, error = "没有这个候选")))
      st0$chosen <- vid
    } else if (identical(act, "clear")) {
      vid <- as.character(b$id %||% "")
      st0$items <- Filter(function(x) !identical(as.character(x$id %||% ""), vid), st0$items %||% list())
      if (identical(st0$chosen, vid)) st0$chosen <- ""
    } else {
      return(send_json(con, 400, list(ok = FALSE, error = "action 只能是 save / choose / clear")))
    }
    d$stages[[stage]] <- st0
    write_json_file(vfile, d)
    return(send_json(con, 200, c(list(ok = TRUE, session_id = vsid, stage = stage,
                                    items = st0$items, chosen = st0$chosen),
                                 record_cap_fields(access$cap))))
  }

  if (p == "/api/artifact/file") {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 404, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    sid2 <- as.character(record_preflight$target$sid %||% "")
    rel <- as.character(q$rel %||% "")
    fp <- mirror_path(sid2, rel)
    if (!nzchar(fp) || !file.exists(fp))
      return(send_json(con, 404, list(ok = FALSE, error = "还没取回这个文件")))
    # 缓存与最新验收版本不一致时不给旧文件。
    #   宁可 409 让人重新取回，也不能让人以为手里的是新结果。
    wmv <- artifact_expected_md5(sid2, rel)
    vvv <- mirror_validity(sid2, rel, wmv)
    if (!isTRUE(vvv$ok)) {
      # 三态分开说，别用一句"请重新取回"盖掉"根本没法核实"。
      code <- as.character(vvv$state %||% "stale")
      msg <- switch(code,
        unverified = "这份缓存查不到最新基准 md5，无法确认是否最新，暂不提供下载",
        stale = "本地缓存不是最新已验收版本，请重新取回",
        "本地没有这份文件的缓存，请先取回")
      return(send_json(con, 409, list(
        ok = FALSE, code = paste0("mirror_", code), error = msg,
        expected_md5 = as.character(vvv$want %||% ""),
        cached_md5 = as.character(vvv$actual %||% ""))))
    }
    expP <- artifact_expected_md5(sid2, rel)
    if (nzchar(expP)) {
      actP <- mirror_actual_md5(fp)
      if (!identical(actP, expP)) {
        return(send_json(con, 409, list(
          ok = FALSE, code = "mirror_stale",
          error = "本地预览文件不是最新已验收版本，请重新取回",
          expected_md5 = expP, cached_md5 = actP)))
      }
    }
    ext <- tolower(tools::file_ext(fp))
    ctype <- switch(ext,
      png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
      pdf = "application/pdf", html = "text/html; charset=utf-8",
      md = "text/plain; charset=utf-8", txt = "text/plain; charset=utf-8",
      "application/octet-stream")
    info <- file.info(fp)
    # inline：图要在浏览器里直接显示，不能弹出下载框。
    hdr <- paste0("HTTP/1.1 200 OK\r\n",
                  "Content-Type: ", ctype, "\r\n",
                  "Content-Length: ", info$size, "\r\n",
                  "Content-Disposition: inline\r\n",
                  # 同名产物重新生成后，同一个 URL 不能让浏览器继续喂旧内容。
                  #   max-age=300 会让旧预览存活五分钟，正是这次"下载到旧版"的同类坑。
                  "Cache-Control: no-store, must-revalidate\r\nConnection: close\r\n\r\n")
    writeBin(charToRaw(enc2utf8(hdr)), con)
    fc <- file(fp, open = "rb")
    repeat {
      chunk <- readBin(fc, "raw", 262144L)
      if (!length(chunk)) break
      writeBin(chunk, con)
    }
    close(fc); flush(con)
    return(invisible(NULL))
  }

  # 看表格：读前若干行，转成 JSON 让页面渲染成表。
  #   .xls 在这里多是制表符分隔的文本，不是真 Excel；两种都试。
  if (p == "/api/artifact/preview") {
    if (!isTRUE(HAS_MIRROR)) return(send_json(con, 400, list(ok = FALSE, error = "缺少 engine/mirror.R")))
    sid2 <- as.character(record_preflight$target$sid %||% "")
    rel <- as.character(q$rel %||% "")
    fp <- mirror_path(sid2, rel)
    if (!nzchar(fp) || !file.exists(fp))
      return(send_json(con, 404, list(ok = FALSE, error = "还没取回这个文件")))
    pv <- mirror_table_preview(fp)
    pv$rel <- rel
    return(send_json(con, if (isTRUE(pv$ok)) 200 else 400, pv))
  }

  if (p == "/api/download") {
    access <- record_preflight$access
    ctx <- record_context(access$cap, require_run_dir = TRUE)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(access$cap))
      return(send_json(con, out$status, out$body))
    }
    runDir <- ctx$run_dir
    rel <- as.character(q$rel %||% "")
    r <- list_artifacts(runDir)
    hit <- Filter(function(x) identical(x$rel, rel), r$items)
    if (!length(hit)) return(send_json(con, 404, list(ok = FALSE, error = paste0("找不到这个文件：", rel))))
    f <- hit[[1]]$full
    info <- file.info(f)
    fname <- enc2utf8(basename(f))
    # 中文文件名用 filename*=UTF-8'' 才不会乱码；
    # 同时给一个纯 ASCII 的 filename= 兜底，给不吃 filename* 的老浏览器用。
    ext <- tools::file_ext(fname)
    ascii_fallback <- paste0("result", if (nzchar(ext)) paste0(".", ext) else ".bin")
    hdr <- paste0("HTTP/1.1 200 OK\r\n",
                  "Content-Type: application/octet-stream\r\n",
                  "Content-Length: ", info$size, "\r\n",
                  "Content-Disposition: attachment; filename=\"", ascii_fallback,
                  "\"; filename*=UTF-8''", utils::URLencode(fname, reserved = TRUE), "\r\n",
                  "Cache-Control: no-store\r\nConnection: close\r\n\r\n")
    writeBin(charToRaw(enc2utf8(hdr)), con)
    fc <- file(f, open = "rb")
    repeat {
      chunk <- readBin(fc, "raw", 262144L)
      if (!length(chunk)) break
      writeBin(chunk, con)
    }
    close(fc); flush(con)
    return(invisible(NULL))
  }

  f <- static_file(p)
  if (!is.null(f)) {
    ext <- paste0(".", tolower(tools::file_ext(f)))
    ctype <- MIME[[ext]] %||% "application/octet-stream"
    if (exists("http_send_static", mode = "function"))
      return(http_send_static(con, req, f, ctype))
    raw <- readBin(f, "raw", file.info(f)$size + 1024L)
    return(send_raw(con, 200, ctype, raw))
  }

  send_text(con, 404, "text/plain; charset=utf-8", "Not found")
}

# =====================================================================
# 数据层启动：连库、建表、必要时迁移历史
# =====================================================================
DB_CON <- NULL
if (!isTRUE(getOption("pa.serve.definitions_only", FALSE)) && HAS_DB) {
  DB_CON <- tryCatch(db_api_boot(APP_DIR), error = function(e) { say("[warn] 数据库起不来：", conditionMessage(e)); NULL })
  if (!is.null(DB_CON)) {
    DB_READY <- TRUE
    # 第一次启动且还没迁移过：把 state/*.json 里的历史处理信息规整进库。
    #   为什么自动跑而不是等人点：不迁移的话，页面切到新数据层就"一条记录都没有"，
    #   看起来像数据丢了。迁移只做一次（migrated_at 有值就跳过），
    #   且不删任何旧文件，随时可以对照。
    if (is.null(db_meta_get(DB_CON, "migrated_at"))) {
      say("[init] 第一次启动数据层：迁移已有处理信息到 app.db（旧 JSON 保留不动）")
      ok <- tryCatch({ migrate_history(APP_DIR, dry_run = FALSE, verbose = FALSE); TRUE },
                     error = function(e) { say("[warn] 迁移出错：", conditionMessage(e)); FALSE })
      if (!ok) say("[warn] 迁移未完成；旧数据仍在，可稍后重跑 engine/migrate_history.R")
    }
    say("[init] 数据层就绪：", db_path(APP_DIR))
  }
}

# =====================================================================
# 主循环
# =====================================================================
main <- function() {
  srv <- tryCatch(serverSocket(PORT), error = function(e) {
    say("")
    say("[!] 端口 ", PORT, " 用不了：", conditionMessage(e))
    say("    可能已经开着一个了。换个端口再试：")
    say("      Rscript serve.R --port ", PORT + 1L)
    say("")
    if (exists("startup_probe_existing", mode = "function") &&
        isTRUE(startup_probe_existing(PORT))) {
      say("[init] A healthy instance is already serving /api/ping on port ", PORT, ".")
      say("[init] This process exits 0 so systemd does not enter a restart loop.")
      say("")
      quit(save = "no", status = 0)
    }
    say("[!] Port ", PORT, " is held by another process, or the existing instance is unhealthy.")
    say("    Try another port: Rscript serve.R --port ", PORT + 1L)
    say("")
    quit(save = "no", status = 1)
  })
  st <- read_state()
  pid_file <- Sys.getenv('PA_SERVE_PID_FILE', unset = '')
  if (nzchar(pid_file)) {
    try(dir.create(dirname(pid_file), showWarnings = FALSE, recursive = TRUE), silent = TRUE)
    try(writeLines(as.character(Sys.getpid()), pid_file), silent = TRUE)
  }

  say("")
  say("===========================================================")
  say("  Personal Analysis Assistant  --  local server")
  say("")
  say("  在浏览器打开：  http://127.0.0.1:", PORT, "/")
  say("")
  say("  关掉这个窗口就是停止服务。所有计算都在本机，不联网。")
  say("  运行目录：", if (is.null(st$runDir)) "（还没选）" else st$runDir)
  # 身份自检：名字对不对，开机就写一行。
  #   2026-11-20 那次故障里，页面能开、静态资源全 200、日志无报错，
  #   只有每个记录接口 403 —— 因为应用在问另一台应用的身份。
  #   这行输出的目的就是让那种情况一眼可见，不用等用户报障。
  .idcheck <- tryCatch(
    if (exists("pa_identity_selfcheck", mode = "function")) pa_identity_selfcheck(),
    error = function(e) list(ok = FALSE, status = "error", app_id = NA_character_,
                             detail = conditionMessage(e)))
  .idapp <- if (is.na(.idcheck$app_id %||% NA_character_)) "未配置" else .idcheck$app_id
  say("  应用身份：  ", .idapp, "  (自检 ", .idcheck$status, ": ", .idcheck$detail, ")")
  if (!isTRUE(.idcheck$ok)) {
    say("")
    say("  [!] 身份自检没通过。这个状态下记录接口会一律 403，不是页面坏了。")
    say("      正名顺序：设环境变量 PA_APP_ID，或让 connector.json 的 localPort 等于 ", PORT, "。")
  }
  # 回收器调度自检：没有调度就补上，补不上要让人一眼看见。
  #   为什么放在启动段（2026-11-23，gina 指出"回收器从未跑"不能就这么留着）：
  #     回收逻辑过去只在有请求进来时被顺手调用。没有 cron 的部署上这个功能等于
  #     不存在，而界面上完全看不出来 —— 只能等某条任务该回收却一直挂着才发现。
  #     所以判定从"最近有没有扫过"扩成"有没有调度"，并在启动时补一次。
  .reapchk <- tryCatch(
    if (exists("exec_reaper_ensure_schedule", mode = "function"))
      exec_reaper_ensure_schedule(APP_DIR)
    else list(ok = FALSE, added = FALSE, note = "调度自检函数未加载"),
    error = function(e) list(ok = FALSE, added = FALSE, note = conditionMessage(e)))
  if (isTRUE(.reapchk$ok)) {
    if (isTRUE(.reapchk$fresh)) {
      say("  回收器调度： 在跑",
          if (isTRUE(.reapchk$added)) "（本次启动补上的）" else "")
    } else {
      # 有过扫描记录但已经不新鲜：说明调度配过、现在停了。
      #   这种必须单独说，而不是并进"已配置"里 —— 后者会让人以为一切正常。
      say("  回收器调度： 停了")
      say("  [!] 上次独立扫描已经是 ",
          if (is.finite(as.numeric(.reapchk$age_secs %||% NA)))
            paste0(round(as.numeric(.reapchk$age_secs) / 3600, 1), " 小时前") else "很久以前",
          "：调度配过但现在没在跑，失联任务不会被自动收尾。")
      say("      检查 crontab 里这一条还在不在：")
      say("      */5 * * * * cd ", APP_DIR,
          " && /usr/bin/Rscript engine/exec_reaper.R >> state/reaper_cron.log 2>&1")
    }
  } else {
    say("  回收器调度： 缺失")
    say("  [!] 没有定时回收：失联任务不会被自动收尾，队列状态会停在旧样子。")
    say("      补一条 crontab 即可：")
    say("      */5 * * * * cd ", APP_DIR,
        " && /usr/bin/Rscript engine/exec_reaper.R >> state/reaper_cron.log 2>&1")
  }
  say("===========================================================")
  say("")

  if (AUTOOPEN) try(browseURL(paste0("http://127.0.0.1:", PORT, "/")), silent = TRUE)

  if (exists("server_serve_loop", mode = "function")) {
    server_serve_loop(srv, APP_DIR, PORT)
  } else {
    stop("engine/server_pool.R missing: request loop unavailable")
  }
}

if (!isTRUE(getOption("pa.serve.definitions_only", FALSE))) main()
