"""Test the Roc site layout and ZIP writer with small renderer/API fixtures."""
from __future__ import annotations
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
import zipfile

ROOT = Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("roc-stable") and shutil.which("zip"), "run through Blueprint for documentation tools")
class DocsPackagingDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "package-docs"
        subprocess.run(["roc-stable", "build", "scripts/package_docs.roc", f"--output={cls.binary}"],
                       cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.output = self.root / "dist"
        self.site = self.root / ".roc-parser-tmp/published/site"
        self.tools = self.root / "bin"
        self.tools.mkdir()
        stub = self.tools / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import os, sys
from pathlib import Path
name = Path(sys.argv[0]).name
def option(flag):
    return sys.argv[sys.argv.index(flag) + 1]
if name == "build_api_docs.roc":
    root = Path(option("--docs-root")) / option("--version")
    root.mkdir(parents=True)
    (root / "index.html").write_text("API reference")
elif name == "roc-docs":
    if os.environ.get("DOCS_RENDER_FAIL"):
        sys.exit(7)
    root = Path(option("--output-directory"))
    (root / "site").mkdir(parents=True)
    (root / "site/index.html").write_text("Manual")
    (root / "site/roc-parser.pdf").write_bytes(b"%PDF fixture")
    (root / "roc-parser.pdf").write_bytes(b"%PDF fixture")
else:
    sys.exit(int(os.environ.get("DOCS_AUDIT_STATUS", "0")))
''')
        stub.chmod(0o755)
        for name in ("roc-docs", "python3"):
            (self.tools / name).symlink_to(stub)
        (self.root / "scripts").mkdir()
        (self.root / "scripts/build_api_docs.roc").symlink_to(stub)
        self.env = {**os.environ, "PATH": str(self.tools) + os.pathsep + os.environ["PATH"]}

    def invoke(self, *args: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), "--release-version", "1.2.3", "--repo", "owner/repository",
                               "--output-dir", str(self.output), "--site", str(self.site), *args],
                              cwd=self.root, env={**self.env, **env}, capture_output=True, text=True)

    def test_combined_archive_keeps_manual_api_pdf_and_missing_page(self) -> None:
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        archive_path = self.output / "roc-parser-docs-1.2.3.zip"
        with zipfile.ZipFile(archive_path) as archive:
            prefix = "roc-parser-docs-1.2.3/"
            names = archive.namelist()
            self.assertEqual(names, [prefix + name for name in ["404.html", "api/index.html", "index.html", "roc-parser.pdf"]])
            self.assertEqual(archive.read(prefix + "index.html"), b"Manual")
            self.assertEqual(archive.read(prefix + "api/index.html"), b"API reference")
            self.assertIn(b"https://owner.github.io/repository/api/", archive.read(prefix + "404.html"))
            self.assertTrue(all(info.date_time == (1980, 1, 1, 0, 0, 0) for info in archive.infolist()))
        self.assertEqual((self.output / "roc-parser-1.2.3.pdf").read_bytes(), b"%PDF fixture")
        self.assertEqual((self.site / "api/index.html").read_text(), "API reference")
        original = archive_path.read_bytes()
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(archive_path.read_bytes(), original)

    def test_render_and_audit_failure_preserve_previous_site(self) -> None:
        self.site.mkdir(parents=True)
        (self.site / "index.html").write_text("previous publication")
        for env in ({"DOCS_RENDER_FAIL": "1"}, {"DOCS_AUDIT_STATUS": "1"}):
            result = self.invoke(**env)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual((self.site / "index.html").read_text(), "previous publication")
            self.assertFalse(self.output.exists())

    def test_unsafe_output_and_versions_fail_before_commands(self) -> None:
        for args in (("--site", str(self.root)), ("--site", str(self.root / "package")),
                     ("--site", str(self.root / ".docs-out/../package")), ("--release-version", "../package"), ("--output-dir", str(self.site))):
            with self.subTest(args=args):
                result = self.invoke(*args)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse((self.root / ".roc-parser-tmp/docs").exists())
