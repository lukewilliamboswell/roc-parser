"""Exercise review selection, compiler forwarding and subprocess failures."""
from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("roc-stable"), "run through Blueprint for the fixed tooling compiler")
class HarnessDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binaries = {}
        for name in ("check_conformance", "check_asciidoc"):
            binary = Path(cls.build.name) / name
            subprocess.run(["roc-stable", "build", str(ROOT / "scripts" / f"{name}.roc"),
                            f"--output={binary}"], cwd=ROOT, check=True, capture_output=True)
            cls.binaries[name] = binary

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.log = self.root / "commands"
        programs = self.root / "tools with spaces"
        programs.mkdir()
        stub = programs / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import json, os, sys
with open(os.environ["HARNESS_LOG"], "a") as log:
    log.write(json.dumps({"program": os.path.basename(sys.argv[0]), "args": sys.argv[1:], "path": os.environ["PATH"]}) + "\\n")
if sys.argv[1:2] == ["scripts/setup_oracles.py"]:
    print(os.environ["HARNESS_PYTHON"])
sys.exit(7 if os.environ.get("HARNESS_FAIL", "unmatched") in sys.argv else 0)
''')
        stub.chmod(0o755)
        for name in ("python3", "oracle-python", "package-roc"):
            (programs / name).symlink_to(stub)
        self.env = {**os.environ, "PATH": str(programs), "ROC": "package-roc",
                    "HARNESS_LOG": str(self.log), "HARNESS_PYTHON": str(programs / "oracle-python")}

    def invoke(self, name: str, *args: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binaries[f"check_{name}"]), *args], cwd=self.root,
                              env={**self.env, **env}, capture_output=True, text=True)

    def calls(self) -> list[dict]:
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def test_invalid_review_selection_runs_no_commands(self) -> None:
        result = self.invoke("conformance", "json", "typo")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(self.log.exists())

    def test_list_needs_no_interpreter_or_package_compiler(self) -> None:
        result = self.invoke("conformance", "--list", PATH="")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("json-robustness: scheduled", result.stdout)
        self.assertFalse(self.log.exists())

    def test_review_failures_continue_and_selection_is_deduplicated(self) -> None:
        result = self.invoke("conformance", "json", "http", "json", HARNESS_FAIL="scripts/review_json.py")
        self.assertNotEqual(result.returncode, 0)
        calls = self.calls()
        self.assertEqual(calls[0]["args"], ["scripts/setup_oracles.py", "scripts/http/requirements.txt",
                                           "scripts/markdown/requirements.txt", "scripts/yaml/requirements.txt"])
        self.assertEqual([call["args"][0] for call in calls[1:]], ["scripts/review_json.py", "scripts/review_http.py"])
        self.assertTrue(all(call["program"] == "oracle-python" for call in calls[1:]))

    def test_setup_failure_prevents_review_execution(self) -> None:
        result = self.invoke("conformance", "json", HARNESS_FAIL="scripts/setup_oracles.py")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.calls()), 1)

    def test_asciidoc_builds_before_review_and_forwards_options(self) -> None:
        result = self.invoke("asciidoc", "--generated", "12", "--mutations", "34", "--seed", "56")
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls()
        self.assertEqual(calls[1]["program"], "package-roc")
        self.assertEqual(calls[1]["args"][:3], ["build", "--no-cache", "--opt=speed"])
        review = calls[2]["args"]
        for flag, value in (("--generated", "12"), ("--mutations", "34"), ("--seed", "56")):
            self.assertEqual(review[review.index(flag) + 1], value)
        self.assertIn("--require-complete", review)
        self.assertIn("--compare-html", review)
        self.assertIn("scripts/check_asciidoc_performance.py", calls[3]["args"])

    def test_asciidoc_skip_performance_and_fail_fast(self) -> None:
        result = self.invoke("asciidoc", "--skip-performance")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(self.calls()), 3)
        self.log.unlink()
        result = self.invoke("asciidoc", HARNESS_FAIL="build")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.calls()), 2)

    def test_asciidoc_rejects_invalid_counts_before_setup(self) -> None:
        result = self.invoke("asciidoc", "--generated", "invalid")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse(self.log.exists())
