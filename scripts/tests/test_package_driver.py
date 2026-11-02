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

    def test_absolute_and_relative_bundle_paths_reach_archive_test(self) -> None:
        for printed in ("/tmp/bundle with spaces.tar.zst", "../dist/hash.tar.zst"):
            with self.subTest(printed=printed), tempfile.TemporaryDirectory() as temporary:
                root = Path(temporary).resolve()
                (root / "package").mkdir()
                for name in ("main.roc", "Yaml.roc", "CSV.roc"):
                    (root / "package" / name).touch()
                binaries = root / "bin"
                binaries.mkdir()
                stub = binaries / "stub"
                stub.write_text(f"#!{sys.executable}\n" + '''import json, os, sys
with open(os.environ["PACKAGE_TEST_LOG"], "a") as log:
    log.write(json.dumps(sys.argv[1:]) + "\\n")
if sys.argv[1:2] == ["bundle"]:
    print("Created: " + os.environ["PACKAGE_TEST_BUNDLE"])
''')
                stub.chmod(0o755)
                for program in ("roc", "python3"):
                    (binaries / program).symlink_to(stub)
                log = root / "commands"
                env = {**os.environ, "PATH": str(binaries) + os.pathsep + os.environ["PATH"],
                       "PACKAGE_TEST_LOG": str(log), "PACKAGE_TEST_BUNDLE": printed}
                result = subprocess.run([str(self.binary)], cwd=root, env=env, capture_output=True, text=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                commands = [json.loads(line) for line in log.read_text().splitlines()]
                self.assertIn(["test", "package/main.roc"], commands)
                bundle = next(command for command in commands if command[0] == "bundle")
                self.assertEqual(bundle[:4], ["bundle", "main.roc", "CSV.roc", "Yaml.roc"])
                expected = Path(printed) if printed.startswith("/") else root / "package" / printed
                self.assertEqual(commands[-1], ["scripts/test_bundle_examples.py", "--bundle-path", str(expected)])
