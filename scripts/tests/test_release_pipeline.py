"""Exercise the migrated pipeline's transferred-file gates without rebuilding."""
from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
import zipfile

ROOT = Path(__file__).resolve().parents[2]
VERSION = "0.0.1-dryrun"


@unittest.skipUnless(shutil.which("roc-stable") and shutil.which("unzip"), "run through Blueprint for release tools")
class ReleasePipelineTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "release-assets"
        subprocess.run(["roc-stable", "build", str(ROOT / "scripts/release_assets.roc"),
                        f"--output={cls.binary}"], cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.release = self.root / "release"
        self.dist = self.release / "dist"
        self.dist.mkdir(parents=True)
        self.bundle = "hash.tar.zst"
        for name in [self.bundle, self.bundle + ".spdx.json", f"roc-parser-examples-{VERSION}.zip", f"roc-parser-{VERSION}.pdf"]:
            (self.dist / name).write_bytes(b"fixture")
        self.notes = self.release / "release-notes.md"
        self.notes.write_text(f"https://github.com/owner/repo/releases/download/{VERSION}/{self.bundle}\n")
        self.docs = self.dist / f"roc-parser-docs-{VERSION}.zip"
        with zipfile.ZipFile(self.docs, "w") as archive:
            for name in ("index.html", "api/index.html", "404.html"):
                archive.writestr(f"roc-parser-docs-{VERSION}/{name}", name)
        programs = self.root / "bin"
        programs.mkdir()
        stub = programs / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import json, os, sys
with open(os.environ["PIPELINE_TEST_LOG"], "a") as log:
    log.write(json.dumps({"program": os.path.basename(sys.argv[0]), "args": sys.argv[1:], "output": os.environ.get("RP_OUTPUT"), "compiler": os.environ.get("RP_COMPILER")}) + "\\n")
sys.exit(int(os.environ.get("PIPELINE_TEST_VERIFY_STATUS", "0")) if os.path.basename(sys.argv[0]) == "release_sbom.roc" else 0)
''')
        stub.chmod(0o755)
        for name in ("python3", "release-package"):
            (programs / name).symlink_to(stub)
        (self.root / "scripts").mkdir()
        (self.root / "scripts/release_sbom.roc").symlink_to(stub)
        self.log = self.root / "commands"
        self.env = {**os.environ, "PATH": str(programs) + os.pathsep + os.environ["PATH"],
                    "PIPELINE_TEST_LOG": str(self.log)}

    def invoke(self, *args: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), "test", "--release-version", VERSION,
                               "--directory", str(self.release), "--repo", "owner/repo", *args],
                              cwd=self.root, env={**self.env, **env}, capture_output=True, text=True)

    def test_tests_the_transferred_archive_in_place_without_building(self) -> None:
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = [json.loads(line) for line in self.log.read_text().splitlines()]
        self.assertEqual(calls[0]["args"][1], "verify")
        self.assertEqual(calls[0]["args"][-2:], ["--repo", "owner/repo"])
        self.assertEqual(calls[-1], {"program": "release-package", "args": ["test-archive"],
                                     "output": str(self.dist), "compiler": "roc"})

    def test_missing_empty_or_extra_assets_are_rejected_before_commands(self) -> None:
        extra = self.dist / "unexpected.txt"
        extra.write_text("extra")
        self.assertNotEqual(self.invoke().returncode, 0)
        extra.unlink()
        (self.dist / self.bundle).write_bytes(b"")
        self.assertNotEqual(self.invoke().returncode, 0)
        (self.dist / self.bundle).unlink()
        self.assertNotEqual(self.invoke().returncode, 0)
        self.assertFalse(self.log.exists())

    def test_sbom_rejection_stops_archive_execution(self) -> None:
        result = self.invoke(PIPELINE_TEST_VERIFY_STATUS="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("release-package", self.log.read_text())

    def test_unresolved_or_wrong_bundle_notes_are_rejected(self) -> None:
        self.notes.write_text("install {package-url}\n")
        result = self.invoke()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("release notes", result.stderr)
        self.assertNotIn("release-package", self.log.read_text())

    def test_incomplete_documentation_archive_is_rejected(self) -> None:
        with zipfile.ZipFile(self.docs, "w") as archive:
            archive.writestr(f"roc-parser-docs-{VERSION}/index.html", "manual")
        result = self.invoke()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing api/index.html", result.stderr)
        self.assertNotIn("release-package", self.log.read_text())

    def test_build_refuses_workspace_root_before_cleanup(self) -> None:
        result = subprocess.run([str(self.binary), "build", "--release-version", VERSION, "--output-dir", "."],
                                cwd=self.root, env=self.env, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertTrue(self.notes.is_file())
        self.assertFalse(self.log.exists())

    def test_build_refuses_source_directories_before_cleanup(self) -> None:
        package = self.root / "package"
        package.mkdir()
        source = package / "main.roc"
        source.write_text("package []\n")
        for directory in ("package", ".git", ".roc-parser-tmp", ".roc-parser-tmp/../package"):
            with self.subTest(directory=directory):
                result = subprocess.run([str(self.binary), "build", "--release-version", VERSION,
                                         "--output-dir", directory], cwd=self.root, env=self.env,
                                        capture_output=True, text=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(source.read_text(), "package []\n")
                self.assertFalse(self.log.exists())
