"""Verify the Roc package driver's compiler/archive handoff without a parser build."""
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
class PackageDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "test-package"
        subprocess.run(["roc-stable", "build", str(ROOT / "scripts/test_package.roc"),
                        f"--output={cls.binary}"], cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def test_checks_package_with_nightly_then_builds_and_tests_shared_assets(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary).resolve()
            binaries = root / "bin"
            binaries.mkdir()
            stub = binaries / "stub"
            stub.write_text(f"#!{sys.executable}\n" + 'import json, os, sys\nwith open(os.environ["PACKAGE_TEST_LOG"], "a") as log:\n    log.write(json.dumps({"program": os.path.basename(sys.argv[0]), "args": sys.argv[1:], "compiler": os.environ.get("RP_COMPILER"), "output": os.environ.get("RP_OUTPUT")}) + "\\n")\nsys.exit(7 if os.environ.get("PACKAGE_TEST_FAIL", "unmatched") in sys.argv else 0)\n')
            stub.chmod(0o755)
            for program in ("roc", "python3", "release-package"):
                (binaries / program).symlink_to(stub)
            log = root / "commands"
            env = {**os.environ, "PATH": str(binaries) + os.pathsep + os.environ["PATH"], "PACKAGE_TEST_LOG": str(log)}
            result = subprocess.run([str(self.binary)], cwd=root, env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            calls = [json.loads(line) for line in log.read_text().splitlines()]
            self.assertIn(["test", "package/main.roc"], [call["args"] for call in calls])
            self.assertEqual([call["program"] for call in calls[-2:]], ["release-package", "release-package"])
            self.assertEqual([call["args"] for call in calls[-2:]], [["build", "0.0.1-dryrun"], ["test-archive"]])
            self.assertTrue(all(call["compiler"] == "roc" and call["output"] == str(root / ".roc-parser-tmp/roc-parser-ci/dist") for call in calls[-2:]))
            log.unlink()
            result = subprocess.run([str(self.binary)], cwd=root, env={**env, "PACKAGE_TEST_FAIL": "build"}, capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn('"test-archive"', log.read_text())
