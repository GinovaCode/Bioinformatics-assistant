#!/usr/bin/env Rscript
# Exercise the WebApp full-flow action against temporary SQLite and queue files.
run_contract <- function() {
  root <- tempfile("webapp-chain-dispatch-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  stopifnot(file.symlink(normalizePath("engine"), file.path(root, "engine")))
  stopifnot(file.symlink(normalizePath("public"), file.path(root, "public")))
  stopifnot(file.symlink(normalizePath("serve.R"), file.path(root, "serve.R")))
  dir.create(file.path(root, "state"), recursive = TRUE)
  dir.create(file.path(root, "config"), recursive = TRUE)

  old_opts <- options(pa.serve.definitions_only = TRUE, pa.app_dir = root)
  e <- new.env(parent = globalenv())
  sys.source(file.path(root, "serve.R"), envir = e)
  options(old_opts)
  # sessions.R loads attempts.R with local=FALSE; in this isolated environment
  # re-bind it beside sess_dir so verdict fixtures resolve the same temp tree.
  sys.source("engine/attempts.R", envir = e)
  e$send_json <- function(con, code, body) list(code = as.integer(code), body = body)
  e$DB_READY <- TRUE
  e$DB_CON <- e$db_connect(root)
  e$db_init(e$DB_CON)
  on.exit({
    if (!is.null(e$DB_CON) && DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON)
  }, add = TRUE)

  fail <- character()
  check <- function(label, ok) {
    cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
    if (!isTRUE(ok)) fail <<- c(fail, label)
  }
  owner <- "viewer:chain-owner"
  `%||%` <- e$`%||%`
  chain_rec <- e$sess_create(root, "fresh-chain", list(mode = "server", server = list(
    host = "fixture-host", user = "fixture-user", data_root = "/fixture/data",
    work_root = "/fixture/chain", ref_mode = "", ref_path = "")), activate = FALSE)
  DBI::dbExecute(e$DB_CON,
    paste0("INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,",
           "project_id,version_id,created_at,updated_at) VALUES(?,?,?,?,?,'private','','',?,?)"),
    list(chain_rec$id, owner, chain_rec$name, chain_rec$mode, chain_rec$run_dir,
         chain_rec$created_at, chain_rec$updated_at))
  e$HAS_CONTRACT <- FALSE
  e$HAS_DECISIONS <- FALSE
  e$HAS_REFERENCE <- FALSE
  e$HAS_STAGE_REVIEWER <- FALSE
  e$HAS_QUALITY <- FALSE
  e$HAS_LEDGER_OUT <- FALSE
  e$HAS_NOTEBOOK <- FALSE
  e$intent_execution_gate <- function(...) list(ok = TRUE)
  e$flow_stage <- function(id, profile = NULL) list(id = id, title = id, skill = "fixture")
  e$flow_profile <- function(...) "server"
  e$flow_execution <- function(st, V) list(ok = TRUE, script = "/fixture/step.R", argv = list("fixture"))
  e$flow_vars <- function(...) list(work_root = "/fixture/chain", run_dir = "/fixture/chain")
  e$remote_step_instruction <- function(...) "INSTRUCTION"
  e$remote_step_put <- function(...) TRUE
  e$remote_step_observe_text <- function(...) "OBSERVE"
  e$db_api_gate <- function(...) NULL
  e$FLOW_ORDER <- function(...) c("first", "second")
  e$flow_run_order <- function(...) c("first", "second")
  e$STEP_TASK <- list(first = list(title = "First"), second = list(title = "Second"))
  e$exec_worker_hb_all <- function(...) list(list(worker_id = "fixture-worker", at = e$exec_ts()))
  e$db_api_owner <- function(...) list(key = owner, kind = "viewer", external_id = "chain-owner")
  chain_call <- function(rec, req_id) e$serve_request(NULL, list(
    target = "/api/run", method = "POST", headers = list(),
    body = as.character(jsonlite::toJSON(list(session_id = rec$id, step = "all",
      overwrite = TRUE, req_id = req_id), auto_unbox = TRUE))))
  published <- list()
  original_write <- e$exec_write
  e$exec_write <- function(d) {
    for (it in d$items) if (identical(it$session_id, chain_rec$id) &&
                            is.null(published[[it$id]])) published[[it$id]] <<- it
    original_write(d)
  }
  broken <- e$db_register_dispatch
  e$db_register_dispatch <- function(...) list(ok = FALSE, code = "fixture_db_write_failed")
  denied <- chain_call(chain_rec, "chain-refused")
  if (denied$code != 409L || !identical(denied$body$code, "dispatch_binding_unavailable")) print(denied)
  check("full flow refuses an unregistered job before the first queue write",
        denied$code == 409L && identical(denied$body$code, "dispatch_binding_unavailable") &&
          !length(published))
  e$db_register_dispatch <- broken
  started <- chain_call(chain_rec, "chain-ok")
  first <- published[[started$body$exec_id %||% "missing"]]
  if (started$code != 200L) print(started)
  check("full flow publishes its first job with the database and chain identifiers",
        started$code == 200L && length(published) == 1L && !is.null(first) &&
          identical(first$step, "first") && identical(first$chain_id, "chain-ok") &&
          nzchar(first$attempt_id %||% "") && nzchar(first$version_id %||% "") &&
          identical(e$db_api_version_of(e$DB_CON, chain_rec$id)$version_id, first$version_id) &&
          identical(e$db_scalar(e$DB_CON, "SELECT req_id FROM step_attempts WHERE attempt_id=?",
            list(first$attempt_id %||% "")), "chain-ok"))
  if (!is.null(first)) {
    due <- e$serve_request(NULL, list(target = "/api/exec/due", method = "GET", headers = list(), body = ""))
    tok <- e$worker_token_issue(root, "fixture-worker", "default", note = "chain fixture")$token
    claim <- e$serve_request(NULL, list(target = "/api/exec/claim", method = "POST",
      headers = list("x-worker-token" = tok),
      body = as.character(jsonlite::toJSON(list(id = first$id, worker_id = "fixture-worker",
        pool_id = "default"), auto_unbox = TRUE))))
    check("the worker receives the full dispatch identity and execution mode from due and claim",
          due$code == 200L && claim$code == 200L && isTRUE(claim$body$ok) &&
          length(due$body$items) == 1L &&
          all(vapply(list(due$body$items[[1]], claim$body$item), function(it)
            identical(it$id, first$id) && identical(it$attempt_id, first$attempt_id) &&
            identical(it$version_id, first$version_id) && identical(it$run_id, first$run_id) &&
            identical(it$executor_mode, first$executor_mode) &&
            identical(it$run_dir, first$run_dir), TRUE)))
    # This is the coordinator's input after step/import accepts a result.
    next_stage <- e$rc_callback(list(session_id = chain_rec$id, req_id = "chain-ok",
      step = "first", status = "done", code = 0L), TRUE)
    second <- published[[next_stage$body$exec_id %||% "missing"]]
    check("accepted first result automatically dispatches a bound second job in the same version",
          next_stage$code == 200L && length(published) == 2L && !is.null(second) &&
            identical(second$step, "second") && identical(second$chain_id, "chain-ok") &&
            identical(second$version_id, first$version_id) &&
            nzchar(second$attempt_id %||% "") && !identical(second$attempt_id, first$attempt_id))
  }
  e$exec_write <- original_write

  if (length(fail)) stop(paste(length(fail), "full-flow dispatch failures"), call. = FALSE)
}
run_contract()
