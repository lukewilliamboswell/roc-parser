from __future__ import annotations

import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

from scripts import check_roc_syntax as check


class ValidationTests(unittest.TestCase):
    def test_source_digest_tracks_contents_and_names(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name in ["package/A.roc", "bench/roc/roc-syntax.roc", "bench/roc/roc-syntax-alloc.roc", ".roc-version"]:
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("initial")
            before = check.source_identity(root)
            (root / "package/A.roc").write_text("changed")
            changed = check.source_identity(root)
            self.assertNotEqual(before, changed)
            (root / "package/A.roc").rename(root / "package/B.roc")
            self.assertNotEqual(changed, check.source_identity(root))

    def test_wrong_compiler_stops_and_preserves_evidence(self):
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp)
            response = subprocess.CompletedProcess(["roc", "version"], 0, "Roc compiler version wrong\n", "")
            with patch.object(check.subprocess, "run", return_value=response) as run:
                self.assertEqual(check.main(["--output", tmp]), 1)
            self.assertEqual(run.call_count, 1)
            record = json.loads((output / "validation.json").read_text())
            self.assertEqual(record["status"], "failed")
            self.assertIn("pinned executable", record["error"])
            self.assertEqual(record["steps"][0]["name"], "compiler-version")

    def test_command_failure_keeps_stdout_stderr_and_exit_status(self):
        with tempfile.TemporaryDirectory() as tmp:
            version = "Roc compiler version " + (check.ROOT / ".roc-version").read_text().strip()
            replies = [subprocess.CompletedProcess([], 0, version, ""),
                       subprocess.CompletedProcess([], 2, "check output", "check error")]
            with patch.object(check.subprocess, "run", side_effect=replies):
                self.assertEqual(check.main(["--output", tmp]), 1)
            output = Path(tmp)
            record = json.loads((output / "validation.json").read_text())
            self.assertEqual(record["steps"][-1]["returncode"], 2)
            self.assertEqual((output / "package-check.stdout").read_text(), "check output")
            self.assertEqual((output / "package-check.stderr").read_text(), "check error")
            self.assertEqual(record["status"], "failed")


if __name__ == "__main__":
    unittest.main()
