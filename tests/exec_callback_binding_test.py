"""A worker must report a real execution failure against the job it claimed."""
import importlib.util
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch


worker_path = Path(__file__).resolve().parents[1] / "tools" / "pa_exec_worker.py"
spec = importlib.util.spec_from_file_location("pa_exec_worker", worker_path)
worker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(worker)


class WorkerCallbackBindingTest(unittest.TestCase):
    def test_ssh_failure_preserves_execution_identity_and_exit_code(self):
        task = dict(
            id="job-fixture", step="doublet", session_id="session-fixture",
            req_id="chain-fixture", chain_id="chain-fixture",
            attempt_id="attempt-fixture", version_id="version-fixture",
            run_id="run-fixture", run_dir="/fixture/output",
        )
        args = SimpleNamespace(worker_id="worker-fixture", pool_id="default")
        client = Mock()
        client.claim.return_value = {"ok": True}
        client.step_import.return_value = {"ok": True}
        client.done.return_value = {"ok": True}
        failure = (False, 255, "Permission denied (publickey).", "")
        with patch.object(worker, "CLIENT", client), patch.object(worker, "run_task", return_value=failure):
            self.assertFalse(worker.handle(task, args))

        report, = client.step_import.call_args[0]
        self.assertEqual(report["exec_id"], task["id"])
        for key in ("session_id", "req_id", "step", "chain_id", "attempt_id", "version_id", "run_id", "run_dir"):
            self.assertEqual(report[key], task[key], key)
        self.assertEqual(report["code"], 255)
        self.assertEqual(report["exit_code"], 255)
        self.assertEqual(report["status"], "failed")
        self.assertIn("Permission denied", report["log"])


if __name__ == "__main__":
    unittest.main()
