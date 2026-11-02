"""Exercise the Roc driver's command boundaries with failing subprocesses."""
from __future__ import annotations

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("roc-stable"), "run through Blueprint for the fixed tooling compiler")
class CheckDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "check"
        subprocess.run(["roc-stable", "build", str(ROOT / "scripts/check_all.roc"),
                        f"--output={cls.binary}"], cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        self.log = self.directory / "commands"
        stub = self.directory / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import os, sys
with open(os.environ["CHECK_TEST_LOG"], "a") as log:
    log.write(" ".join(sys.argv[1:]) + "\\n")
sys.exit(7 if sys.argv[0].endswith("check_doc_examples.roc") else 0)
''')
        stub.chmod(0o755)
        for program in ("roc", "python3", "scripts/check_doc_examples.roc"):
            (self.directory / program).parent.mkdir(exist_ok=True)
            (self.directory / program).symlink_to(stub)
        self.env = {**os.environ, "PATH": str(self.directory), "CHECK_TEST_LOG": str(self.log)}

    def run_driver(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), *args], cwd=self.directory, env=self.env,
                              capture_output=True, text=True)

    def test_invalid_selection_runs_no_commands(self) -> None:
        result = self.run_driver("docs", "unknown")
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertFalse(self.log.exists())

    def test_failure_stops_the_section_and_following_sections(self) -> None:
        result = self.run_driver("docs", "conformance")
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(self.log.read_text().splitlines(), ["version", ""])

    def test_keep_going_reports_failure_and_runs_next_section_once(self) -> None:
        result = self.run_driver("--keep-going", "docs", "conformance", "conformance")
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(self.log.read_text().splitlines(), [
            "version", "", "scripts/check_conformance.py"])

    def test_list_requires_no_package_compiler(self) -> None:
        result = self.run_driver("--list")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("roc-syntax:", result.stdout)
        self.assertFalse(self.log.exists())
