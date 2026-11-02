"""Check that the migrated driver rejects documentation output drift."""
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
class DocExamplesDriverTests(unittest.TestCase):
    def test_expected_stdout_is_checked_and_expects_run(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve()
            binary = root / "doc-examples"
            subprocess.run(["roc-stable", "build", str(ROOT / "scripts/check_doc_examples.roc"),
                            f"--output={binary}"], cwd=ROOT, check=True, capture_output=True)
            examples = root / "docs/examples"
            examples.mkdir(parents=True)
            (examples / "demo.roc").write_text("expect True\n")
            expected = examples / "demo.expected"
            expected.write_text("actual\n")
            programs = root / "bin"
            programs.mkdir()
            compiler = programs / "roc"
            compiler.write_text(f"#!{sys.executable}\n" + '''import os, sys
with open(os.environ["DOC_TEST_LOG"], "a") as log:
    log.write(" ".join(sys.argv[1:]) + "\\n")
if sys.argv[1:] == ["demo.roc"]:
    print("actual")
''')
            compiler.chmod(0o755)
            log = root / "commands"
            env = {**os.environ, "PATH": str(programs), "DOC_TEST_LOG": str(log)}
            passed = subprocess.run([str(binary)], cwd=root, env=env, capture_output=True, text=True)
            self.assertEqual(passed.returncode, 0, passed.stderr)
            self.assertIn("test --no-cache demo.roc", log.read_text())
            expected.write_text("different\n")
            failed = subprocess.run([str(binary)], cwd=root, env=env, capture_output=True, text=True)
            self.assertEqual(failed.returncode, 1, failed.stderr)
            self.assertIn("stdout differs", failed.stderr)
