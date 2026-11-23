#!/usr/bin/env python3
"""执行端 worker：轮询应用的执行队列，领取任务并在目标服务器上运行。

为什么需要这个文件
    serve.R 里有完整的执行队列服务端（/api/exec/due、claim、keepalive、
    done、heartbeat，以及 worker 令牌鉴权），但仓库里从来没有随附客户端：
    任务能入队，却没有任何进程去领。表现就是「已交给助手、作业号空、
    一直 queued」。这个脚本补的就是缺的那一半。

协议（与 engine/server_main.R 的 /api/exec/* 对齐）
    POST /api/exec/heartbeat        每 60 秒一次，证明执行端活着
    GET  /api/exec/due              取待执行 + 正在执行的清单
    POST /api/exec/claim            领取一条 queued 任务，服务端置 running
    POST /api/exec/keepalive        运行期间续命，只推 last_seen
    POST /api/step/import           链式任务必须先回写环节，否则 done 会被 409 拦
    POST /api/exec/done             收尾（ok / error）
    鉴权头：X-Worker-Token（或 Authorization: Worker <token>）

用法
    # 令牌从环境变量读，不进命令行、不进日志
    set PA_WORKER_TOKEN=...            (Windows)
    export PA_WORKER_TOKEN=...         (Linux/macOS)
    python pa_exec_worker.py --app http://127.0.0.1:18980 \
        --ssh-host 10.1.1.2 --ssh-user hhhong1 \
        --ssh-key ~/.ssh/id_rsa --rscript /share/apps/R/R-4.1.2-4/bin/Rscript

    # 只看会发生什么，不领、不跑
    python pa_exec_worker.py --dry-run --once

边界
    · 只执行队列里给的任务，不自己推算阶段、不改参数。
    · 执行在目标服务器上进行，本机只作跳板；没有 ssh 时用 --local 在这台机器跑。
    · 失败原样回写，不重试压制、不伪造成功。
"""

import argparse
import json
import os
import shlex
import subprocess
import threading
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime

TOKEN_ENV = "PA_WORKER_TOKEN"


def now():
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def log(msg):
    print("[%s] %s" % (now(), msg), flush=True)


class Client:
    def __init__(self, base, token, worker_id, pool_id, timeout=30):
        self.base = base.rstrip("/")
        self.token = token
        self.worker_id = worker_id
        self.pool_id = pool_id
        self.timeout = timeout

    def _req(self, method, path, body=None):
        url = self.base + path
        data = None
        headers = {"X-Worker-Token": self.token, "Accept": "application/json"}
        if body is not None:
            data = json.dumps(body).encode("utf-8")
            headers["Content-Type"] = "application/json"
        req = urllib.request.Request(url, data=data, headers=headers, method=method)
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as r:
                return json.loads(r.read().decode("utf-8") or "{}")
        except urllib.error.HTTPError as e:
            raw = e.read().decode("utf-8", "replace")
            try:
                parsed = json.loads(raw)
            except Exception:
                parsed = {"ok": False, "error": raw[:400]}
            parsed["_http_status"] = e.code
            return parsed
        except Exception as e:
            return {"ok": False, "error": "%s: %s" % (type(e).__name__, e)}

    def heartbeat(self, note=""):
        return self._req("POST", "/api/exec/heartbeat", {
            "worker_id": self.worker_id, "pool_id": self.pool_id,
            "version": "pa_exec_worker/1", "note": note})

    def due(self):
        return self._req("GET", "/api/exec/due")

    def claim(self, task_id):
        return self._req("POST", "/api/exec/claim", {
            "id": task_id, "worker_id": self.worker_id, "pool_id": self.pool_id})

    def keepalive(self, task_id):
        return self._req("POST", "/api/exec/keepalive", {
            "id": task_id, "worker_id": self.worker_id, "pool_id": self.pool_id})

    def step_import(self, payload):
        return self._req("POST", "/api/step/import", payload)

    def done(self, task_id, ok, error="", attempt_id=""):
        # attempt_id 一起上报：done 也会走到判定与落库，
        #   少了它，服务端只能回落到"按 req_id 猜是哪一次尝试"。
        return self._req("POST", "/api/exec/done", {
            "id": task_id, "ok": bool(ok), "error": error,
            "worker_id": self.worker_id, "pool_id": self.pool_id,
            "attempt_id": str(attempt_id or "")})


def build_remote_command(task, rscript):
    """把任务里的 script / script_args 还原成一条可执行命令。

    队列里 script 可能就是 "-e"（Rscript -e "<代码>"），也可能是脚本路径。
    两种情况都按「rscript <script> <args>」拼，交给远端 shell 解释。
    """
    script = str(task.get("script") or "").strip()
    args = str(task.get("script_args") or "")
    if not script:
        return None
    parts = [rscript, script]
    if args.strip():
        parts.append(args)
    return " ".join(parts)


def run_task(task, args, keepalive_every=45, timeout_s=None):
    """在目标上执行任务，返回 (ok, exit_code, log_text, cmd_display)。"""
    cmd = build_remote_command(task, args.rscript)
    if cmd is None:
        return False, -1, "任务没有给出 script，无法执行。", ""

    host = args.ssh_host
    if args.local or not host:
        argv = ["bash", "-lc", cmd]
        display = cmd
    else:
        ssh = ["ssh", "-o", "BatchMode=yes", "-o", "StrictHostKeyChecking=accept-new",
               "-o", "ServerAliveInterval=15", "-p", str(args.ssh_port)]
        if args.ssh_key:
            ssh += ["-i", os.path.expanduser(args.ssh_key)]
        ssh += ["%s@%s" % (args.ssh_user, host), cmd]
        argv = ssh
        display = "ssh %s@%s %s" % (args.ssh_user, host, shlex.quote(cmd)[:200])

    log("执行：%s" % display)
    limit = timeout_s or args.task_timeout
    started = time.time()
    proc = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            universal_newlines=True, bufsize=1)
    chunks = []
    last_alive = time.time()
    try:
        while True:
            line = proc.stdout.readline()
            if line:
                chunks.append(line)
                sys.stdout.write(line)
                sys.stdout.flush()
            if proc.poll() is not None and not line:
                break
            if time.time() - last_alive >= keepalive_every:
                client_keepalive(task.get("id"))
                last_alive = time.time()
            if limit and time.time() - started > limit:
                proc.kill()
                chunks.append("\n[worker] 超时 %ss，已终止。\n" % limit)
                return False, -9, "".join(chunks), display
    finally:
        if proc.poll() is None:
            proc.kill()
    code = proc.returncode if proc.returncode is not None else -1
    return code == 0, code, "".join(chunks), display


CLIENT = None


def client_keepalive(task_id):
    if CLIENT is not None and task_id:
        CLIENT.keepalive(task_id)


def handle(task, args):
    tid = str(task.get("id") or "")
    step = str(task.get("step") or "")
    sid = str(task.get("session_id") or "")
    rid = str(task.get("req_id") or "")
    chain = str(task.get("chain_id") or "")
    # 这一次尝试的身份，来自 due/claim 下发。
    #   服务端它自己生成的，回写时必须原样带回，否则只能按 req_id 猜（2026-11-23）。
    att = str(task.get("attempt_id") or "")

    got = CLIENT.claim(tid)
    if not got.get("ok"):
        log("领取失败（%s）：%s" % (tid, got.get("error") or got.get("code") or ""))
        return False
    log("已领取 %s  step=%s  session=%s" % (tid, step, sid))

    ok, code, out, cmd = run_task(task, args)
    tail = out[-4000:] if out else ""
    log("任务结束 %s  ok=%s exit=%s" % (tid, ok, code))

    # 链式任务：done 之前必须先回写环节，否则服务端回 409 stage_callback_required。
    if chain and sid:
        imp = {
            "session_id": sid,
            "req_id": rid,
            "step": step,
            "chain_id": chain,
            "status": "done" if ok else "failed",
            "code": code,
            "exit_code": code,
            "log": tail,
            "worker_id": args.worker_id,
            "pool_id": args.pool_id,
            # 这一次尝试的身份 + 这条任务的号，必须原样带回去。
            #   为什么（2026-11-23，子任务查证）：服务端在派工时生成了 attempt_id、
            #   也写进了队列条目，但回写体里没有它，服务端只能按 req_id 去猜，
            #   猜不中就判 attempt_id_unknown，19 条「回写未被接受」就这么丢的。
            #   exec_id 一起带：服务端也要靠它定位是哪条任务在回写。
            "attempt_id": att,
            "exec_id": tid,
            "version_id": str(task.get("version_id") or ""),
            "run_id": str(task.get("run_id") or ""),
            "run_dir": str(task.get("run_dir") or ""),
        }
        res = CLIENT.step_import(imp)
        if not res.get("ok"):
            log("环节回写未通过：%s" % (res.get("error") or res.get("code") or ""))
            CLIENT.done(tid, False, "环节回写失败：%s" % (res.get("error") or res.get("code")), attempt_id=att)
            return False

    fin = CLIENT.done(tid, ok, "" if ok else ("exit=%s\n%s" % (code, tail[-1500:])), attempt_id=att)
    if not fin.get("ok"):
        log("收尾回写失败：%s" % (fin.get("error") or fin.get("code") or ""))
    return ok


def select_queued(items, pool_id):
    """只挑属于本 pool 的排队任务。

    为什么单独成函数（2026-11-23）：
      下发是全量的，执行端又固定取第一条。队头只要压着一条别的 pool 的任务，
      它每轮去领、每轮被拒，后面真正属于它的永远轮不到 —— 一条任务堵死一个 pool。
      这段逻辑抽出来才能直接被测，否则只能靠人读主循环。

    pool_id 为空的条目视为未指定，按兼容放行（历史任务不带这个字段）。
    """
    want = str(pool_id or "")
    out = []
    for x in (items or []):
        if x.get("kind") != "queued":
            continue
        p = str(x.get("pool_id") or "")
        if p and want and p != want:
            continue
        out.append(x)
    return out


def slot_worker(args, slot, stop_evt):
    """一个执行槽：自己轮询、自己领取、自己盯到结束。

    为什么可以并行（2026-11-23，gina 问"一台电脑上有多个任务需要运行"）：
      主循环原来是同步的，领一条就盯到它结束，第二个任务只能干等。
      分析作业真正跑在集群上，本地只做提交与盯梢，开几个槽不占多少资源。
      同一条任务不会被两个槽同时领到 —— 领取在服务端是锁内完成的原子动作。
    """
    rounds = 0
    while not stop_evt.is_set():
        rounds += 1
        d = CLIENT.due()
        if not d.get("ok"):
            log("[slot%s] 取队列失败：%s" % (slot, d.get("error") or d.get("code") or ""))
        else:
            items = d.get("items") or []
            # 只挑属于自己的 pool。
            #   为什么（2026-11-23）：下发是全量的，执行端又固定取第一条。
            #   队头只要压着一条别的 pool 的任务，它每轮去领、每轮被拒，
            #   后面真正属于它的任务永远轮不到 —— 一条任务就能把这个 pool 堵死。
            #   pool_id 为空视为未指定，按兼容放行（历史任务不带这个字段）。
            queued = select_queued(items, args.pool_id)
            if queued:
                t = queued[0]
                if args.dry_run:
                    log("[slot%s] dry-run：将领取 %s step=%s" % (slot, t.get("id"), t.get("step")))
                else:
                    got = handle(t, args)
                    # got 为 None 表示压根没领到（被别处抢走/条目不存在）。
                    #   这时不能 continue：continue 会跳过本轮末尾的 sleep，
                    #   拿同一条空转，把 CPU 打满还一直刷日志。
                    if got is not None:
                        continue
            elif rounds % 6 == 1:
                log("[slot%s] 队列空闲（queued=0 running=%s）" % (slot, d.get("running")))
        if args.once:
            break
        time.sleep(args.interval)


def loop(args):
    global CLIENT
    CLIENT = Client(args.app, args.token, args.worker_id, args.pool_id)
    hb = CLIENT.heartbeat("start")
    if not hb.get("ok"):
        log("心跳失败：%s" % (hb.get("error") or hb.get("code") or ""))
        log("令牌必须是本应用签发的 worker 令牌（tools/issue_worker_token.R）。")
        if hb.get("_http_status") in (401, 403):
            return 2
    slots = max(1, int(getattr(args, "slots", 1) or 1))
    log("已在队列上线：worker=%s pool=%s slots=%d" % (args.worker_id, args.pool_id, slots))

    # 心跳由这里统一发，不放进每个槽：
    #   心跳表达的是"这台执行端活着"，跟开了几个槽无关。
    #   每个槽各发一次只会让服务端收到重复报信，看不出任何额外信息。
    stop_evt = threading.Event()
    threads = [threading.Thread(target=slot_worker, args=(args, i + 1, stop_evt),
                                daemon=True)
               for i in range(slots)]
    for t in threads:
        t.start()
    last_hb = time.time()
    try:
        while any(t.is_alive() for t in threads):
            if time.time() - last_hb >= args.heartbeat_every:
                CLIENT.heartbeat("alive")
                last_hb = time.time()
            time.sleep(1)
    except KeyboardInterrupt:
        stop_evt.set()
    return 0


def default_install_dir():
    """执行端的固定安装位置。

    为什么必须固定：每台电脑都靠"报错再人工上传一次"装机，成本是设备数量乘以
    一次排查。定死一个位置之后，任何设备上先查这里，有就直接用、没有就装一份，
    设备再多也只是一条命令。
    """
    if sys.platform.startswith("win"):
        base = os.environ.get("LOCALAPPDATA") or os.path.expanduser("~")
        return os.path.join(base, "CatsCo", "pa-exec")
    return os.path.join(os.path.expanduser("~"), ".local", "share", "catsco", "pa-exec")


def do_install(target_dir, args, token=""):
    """把执行端放到固定位置。幂等：内容一致就不重写。"""
    import hashlib
    import shutil

    target_dir = os.path.abspath(target_dir)
    os.makedirs(target_dir, exist_ok=True)
    src = os.path.abspath(__file__)
    dst = os.path.join(target_dir, "pa_exec_worker.py")

    def digest(path):
        try:
            with open(path, "rb") as fh:
                return hashlib.sha256(fh.read()).hexdigest()
        except Exception:
            return ""

    if os.path.exists(dst) and digest(dst) == digest(src):
        log("执行端已是最新，跳过写入：%s" % dst)
    else:
        shutil.copyfile(src, dst)
        log("已安装执行端：%s" % dst)

    env_path = os.path.join(target_dir, "worker.env")
    lines = [
        "# 执行端启动参数。令牌是本机凭据，不要提交到仓库、不要贴进聊天。",
        "PA_WORKER_TOKEN=%s" % (token or "<在这里填入应用签发的 worker 令牌>"),
        "PA_EXEC_APP=%s" % args.app,
        "PA_EXEC_WORKER_ID=%s" % args.worker_id,
        "PA_EXEC_POOL_ID=%s" % args.pool_id,
        "PA_EXEC_SSH_HOST=%s" % args.ssh_host,
        "PA_EXEC_SSH_USER=%s" % args.ssh_user,
        "PA_EXEC_RSCRIPT=%s" % args.rscript,
    ]
    with open(env_path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")
    try:
        os.chmod(env_path, 0o600)
    except Exception:
        pass
    log("已写入参数：%s（权限 600）" % env_path)

    if sys.platform.startswith("win"):
        runner = os.path.join(target_dir, "run-worker.cmd")
        body = (
            "@echo off\r\n"
            "cd /d \"%~dp0\"\r\n"
            "for /f \"usebackq tokens=1,* delims==\" %%a in (\"worker.env\") do (\r\n"
            "  if not \"%%a\"==\"\" if not \"%%a:~0,1\"==\"#\" set \"%%a=%%b\"\r\n"
            ")\r\n"
            "python pa_exec_worker.py --app \"%PA_EXEC_APP%\" --worker-id \"%PA_EXEC_WORKER_ID%\" "
            "--pool-id \"%PA_EXEC_POOL_ID%\" --ssh-host \"%PA_EXEC_SSH_HOST%\" "
            "--ssh-user \"%PA_EXEC_SSH_USER%\" --rscript \"%PA_EXEC_RSCRIPT%\"\r\n"
        )
    else:
        runner = os.path.join(target_dir, "run-worker.sh")
        body = (
            "#!/bin/sh\n"
            "set -a\n"
            ". \"$(dirname \"$0\")/worker.env\"\n"
            "set +a\n"
            "exec python3 \"$(dirname \"$0\")/pa_exec_worker.py\" \\\n"
            "  --app \"$PA_EXEC_APP\" --worker-id \"$PA_EXEC_WORKER_ID\" \\\n"
            "  --pool-id \"$PA_EXEC_POOL_ID\" --ssh-host \"$PA_EXEC_SSH_HOST\" \\\n"
            "  --ssh-user \"$PA_EXEC_SSH_USER\" --rscript \"$PA_EXEC_RSCRIPT\"\n"
        )
    with open(runner, "w", encoding="utf-8", newline="") as fh:
        fh.write(body)
    try:
        os.chmod(runner, 0o755)
    except Exception:
        pass
    log("启动脚本：%s" % runner)
    log("固定位置：%s" % target_dir)
    return 0


def _digest(path):
    import hashlib
    try:
        with open(path, "rb") as fh:
            return hashlib.sha256(fh.read()).hexdigest()
    except Exception:
        return ""


def do_check(args):
    """自检：固定位置有没有执行端、版本对不对、能不能连上队列。

    为什么要有这一步：设备一多，"先报错再人工上传"就会变成每台一次排查。
    自检把"有没有、新不新、通不通"三件事一次答完，缺什么说什么。
    """
    d = args.install_dir or default_install_dir()
    dst = os.path.join(d, "pa_exec_worker.py")
    log("固定位置：%s" % d)
    if not os.path.exists(dst):
        log("结果：未安装。运行 --install 即可就位。")
        return 3
    same = _digest(dst) == _digest(os.path.abspath(__file__))
    log("结果：已安装%s" % ("，版本一致" if same else "，但版本与当前脚本不同，建议重装"))
    env_file = os.path.join(d, "worker.env")
    log("参数文件：%s" % ("存在" if os.path.exists(env_file) else "缺失"))

    token = args.token or os.environ.get(TOKEN_ENV, "")
    if not token:
        log("未带令牌，跳过队列连通性检查。")
        return 0 if same else 4
    c = Client(args.app, token, args.worker_id, args.pool_id)
    hb = c.heartbeat("check")
    if not hb.get("ok"):
        log("连不上队列：%s" % (hb.get("error") or hb.get("code") or ""))
        log("多半是令牌不是本应用签发的，或不带令牌已被服务端拒绝。")
        return 2
    due = c.due()
    log("队列连通：queued=%s running=%s" % (due.get("queued"), due.get("running")))
    return 0 if same else 4


def main(argv=None):
    p = argparse.ArgumentParser(description="个性化分析助手 · 执行端 worker")
    p.add_argument("--app", default=os.environ.get("PA_EXEC_APP", "http://127.0.0.1:18980"),
                   help="应用服务地址")
    p.add_argument("--worker-id", default=os.environ.get("PA_EXEC_WORKER_ID", "pc-worker"))
    p.add_argument("--pool-id", default=os.environ.get("PA_EXEC_POOL_ID", "default"))
    p.add_argument("--token", default=os.environ.get(TOKEN_ENV, ""),
                   help="worker 令牌；默认取环境变量 %s" % TOKEN_ENV)
    p.add_argument("--token-file", default="",
                   help="一行明文令牌文件；应用登记表只存哈希，不能用它")
    p.add_argument("--interval", type=float, default=10.0, help="轮询间隔秒")
    p.add_argument("--heartbeat-every", type=float, default=60.0)
    p.add_argument("--task-timeout", type=int, default=0, help="单任务超时秒，0 表示不限")
    p.add_argument("--once", action="store_true", help="只跑一轮")
    # 并行槽数：一台电脑要同时跑多个任务时调大。
    #   为什么需要（2026-11-23，gina 问"一台电脑上有多个任务需要运行"）：
    #   主循环原来是同步的，领一条就盯到它结束，第二个任务只能干等。
    #   分析作业真正跑在集群上，本地只是提交与盯梢，开几个槽不占多少资源。
    #   默认 1，保持既有部署的行为不变。
    p.add_argument("--slots", type=int, default=1,
                   help="并行执行槽数（默认 1）")
    p.add_argument("--dry-run", action="store_true", help="只观察，不领取不执行")
    p.add_argument("--local", action="store_true", help="在运行本脚本的机器上执行，不 SSH")
    p.add_argument("--ssh-host", default=os.environ.get("PA_EXEC_SSH_HOST", ""))
    p.add_argument("--ssh-port", type=int, default=22)
    p.add_argument("--ssh-user", default=os.environ.get("PA_EXEC_SSH_USER", ""))
    p.add_argument("--ssh-key", default="")
    p.add_argument("--rscript", default=os.environ.get("PA_EXEC_RSCRIPT", "Rscript"))
    p.add_argument("--install", action="store_true",
                   help="安装到固定位置（幂等），不启动轮询")
    p.add_argument("--install-dir", default="", help="覆盖固定位置，仅用于测试")
    p.add_argument("--print-install-dir", action="store_true", help="打印固定位置后退出")
    p.add_argument("--check", action="store_true",
                   help="自检：固定位置是否有执行端、是否可连队列")
    args = p.parse_args(argv)

    if args.print_install_dir:
        print(default_install_dir())
        return 0

    # 安装与自检必须在令牌校验之前：这两件事本身就是为了把令牌配上，
    #   先要求令牌等于「没配令牌时连装都装不上」，绕成一个死结。
    if args.install:
        return do_install(args.install_dir or default_install_dir(), args,
                          args.token or os.environ.get(TOKEN_ENV, ""))
    if args.check:
        return do_check(args)

    token = args.token
    if not token and args.token_file:
        # 两种文件要分开处理，这里踩过坑：
        #   config/worker_tokens.json 只存 hash，永远取不出明文令牌；
        #   拿它当令牌来源只会得到「缺少令牌」，让部署的人以为脚本坏了。
        #   明文只存在于签发那一刻的 stdout，所以要么用独立的一行令牌文件，
        #   要么重新签发一枚。
        try:
            with open(args.token_file, encoding="utf-8") as fh:
                text = fh.read().strip()
        except Exception as e:
            log("读令牌文件失败：%s" % e)
            return 1
        if text.startswith("{"):
            try:
                doc = json.loads(text)
            except Exception:
                doc = {}
            if "tokens" in doc:
                log("这是应用的令牌登记表，只存哈希，取不出明文令牌。")
                log("请用 tools/issue_worker_token.R 重新签发一枚，把输出存进环境变量 %s。" % TOKEN_ENV)
                log("  Rscript tools/issue_worker_token.R <app_dir> pc-worker default")
                return 1
            token = doc.get("token") or ""
        else:
            token = text.splitlines()[0].strip() if text else ""
    if not token:
        log("缺少 worker 令牌：设环境变量 %s，或用 --token-file 指向 config/worker_tokens.json。" % TOKEN_ENV)
        return 1
    args.token = token

    if not args.dry_run and not args.local and not args.ssh_host:
        log("未指定 --ssh-host，将在这台机器上执行（等同 --local）。")
        args.local = True
    return loop(args)


if __name__ == "__main__":
    sys.exit(main())