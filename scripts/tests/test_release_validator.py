"""Run the Roc release gates with controlled GitHub and utility responses."""
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
class ReleaseValidatorTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "validate-release"
        subprocess.run(["roc-stable", "build", str(ROOT / "scripts/validate_release.roc"),
                        f"--output={cls.binary}"], cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.log = self.root / "commands"
        stub = self.root / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import os, sys
with open(os.environ["VALIDATE_TEST_LOG"], "a") as log:
    log.write(os.path.basename(sys.argv[0]) + " " + " ".join(sys.argv[1:]) + "\\n")
if os.path.basename(sys.argv[0]) == "release-package":
    print(os.environ.get("VALIDATE_TEST_DIAGNOSTIC", "comparison passed"))
    sys.exit(int(os.environ.get("VALIDATE_TEST_UTILITY_STATUS", "0")))
if "/releases/latest" in sys.argv[2]:
    print("2.0.0")
    sys.exit(0)
print("HTTP 404" if os.environ.get("VALIDATE_TEST_GITHUB", "missing") == "missing" else "network failure", file=sys.stderr)
sys.exit(0 if os.environ.get("VALIDATE_TEST_GITHUB") == "exists" else 1)
''')
        stub.chmod(0o755)
        for program in ("gh", "release-package"):
            (self.root / program).symlink_to(stub)
        self.env = {**os.environ, "PATH": str(self.root), "VALIDATE_TEST_LOG": str(self.log)}

    def invoke(self, *args: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), "--event", "workflow_dispatch", "--ref", "refs/heads/main",
                               "--release-version", "3.0.0", *args], cwd=self.root,
                              env={**self.env, **env}, capture_output=True, text=True)

    def test_pull_request_uses_shared_dry_run_and_appends_outputs(self) -> None:
        output = self.root / "github-output"
        output.write_text("earlier=value\n")
        result = self.invoke("--event", "pull_request", "--github-output", str(output))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(output.read_text(), "earlier=value\nversion=0.0.1-dryrun\nprerelease=true\n")
        self.assertEqual(self.log.read_text(), "release-package check-version 0.0.1-dryrun\n")

    def test_nondefault_branch_and_output_injection_run_nothing(self) -> None:
        for args in (("--ref", "refs/heads/other"), ("--release-version", "3.0.0\nversion=evil")):
            with self.subTest(args=args):
                self.assertNotEqual(self.invoke(*args).returncode, 0)
                self.assertFalse(self.log.exists())

    def test_existing_tags_and_unavailable_github_stop_before_packaging(self) -> None:
        for response in ("exists", "network"):
            with self.subTest(response=response):
                self.log.unlink(missing_ok=True)
                result = self.invoke(VALIDATE_TEST_GITHUB=response)
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("release-package", self.log.read_text())

    def test_understated_or_unknown_comparison_failure_stops_a_release(self) -> None:
        result = self.invoke(VALIDATE_TEST_UTILITY_STATUS="1")
        self.assertNotEqual(result.returncode, 0)
        smaller = self.invoke("--release-version", "2.1.0", VALIDATE_TEST_UTILITY_STATUS="1",
                              VALIDATE_TEST_DIAGNOSTIC="Package does not compile")
        self.assertNotEqual(smaller.returncode, 0)

    def test_only_a_major_release_can_survive_an_unreadable_baseline(self) -> None:
        result = self.invoke(VALIDATE_TEST_UTILITY_STATUS="1", VALIDATE_TEST_DIAGNOSTIC="Cannot extract public API")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("::warning::", result.stdout)
