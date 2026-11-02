"""Check generation failures and normalization at the Roc subprocess boundary."""
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
class ApiDocsDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "build-api-docs"
        subprocess.run(["roc-stable", "build", "scripts/build_api_docs.roc", f"--output={cls.binary}"],
                       cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        (self.root / "package").mkdir()
        self.entry = self.root / "package/main.roc"
        self.entry.write_text("## Parse `Str` & bytes.\npackage [CSV]\n")
        programs = self.root / "bin"
        programs.mkdir()
        stub = programs / "stub"
        stub.write_text(f"#!{sys.executable}\n" + '''import os, sys
from pathlib import Path
if os.path.basename(sys.argv[0]) == "package-roc":
    if os.environ.get("API_COMPILER_FAIL"):
        sys.exit(7)
    root = Path(next(arg.split("=", 1)[1] for arg in sys.argv if arg.startswith("--output=")))
    (root / "CSV").mkdir(parents=True)
    common = '<title>package Docs</title><h1 class="pkg-full-name"><a href="../">package</a></h1>'
    (root / "index.html").write_text(common + '        <div class="index-decoration">logo</div>')
    (root / "CSV/index.html").write_text(common + '<h1 class="module-name">CSV</h1>\\n<pre class="entry-type-def"><code class="entry-type-def-code">CSV :: # (opaque)</code></pre>body')
else:
    sys.exit(int(os.environ.get("API_AUDIT_STATUS", "0")))
''')
        stub.chmod(0o755)
        for name in ("package-roc", "python3"):
            (programs / name).symlink_to(stub)
        self.env = {**os.environ, "PATH": str(programs) + os.pathsep + os.environ["PATH"], "ROC": "package-roc"}

    def invoke(self, *args: str, **env: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), *args], cwd=self.root, env={**self.env, **env},
                              capture_output=True, text=True)

    def test_writes_normalized_version_after_audit(self) -> None:
        result = self.invoke("--version", "1.2.3")
        self.assertEqual(result.returncode, 0, result.stderr)
        index = (self.root / ".docs-api/1.2.3/index.html").read_text()
        module = (self.root / ".docs-api/1.2.3/CSV/index.html").read_text()
        self.assertIn("<p>Parse <code>Str</code> &amp; bytes.</p>", index)
        for page in (index, module):
            self.assertIn("<title>roc-parser Docs</title>", page)
            self.assertIn(">roc-parser</a></h1>", page)
        self.assertNotIn("CSV :: # (opaque)", module)

    def test_check_leaves_versioned_output_alone(self) -> None:
        result = self.invoke("--check")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse((self.root / ".docs-api").exists())
        self.assertEqual(list((self.root / ".roc-parser-tmp/api-docs").iterdir()), [])

    def test_compiler_and_audit_failures_preserve_existing_version(self) -> None:
        previous = self.root / ".docs-api/1.2.3/index.html"
        previous.parent.mkdir(parents=True)
        previous.write_text("published bytes")
        for env in ({"API_COMPILER_FAIL": "1"}, {"API_AUDIT_STATUS": "1"}):
            result = self.invoke("--version", "1.2.3", **env)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(previous.read_text(), "published bytes")

    def test_missing_header_and_invalid_version_are_rejected(self) -> None:
        self.assertNotEqual(self.invoke().returncode, 0)
        self.assertNotEqual(self.invoke("--version", "../package").returncode, 0)
        self.entry.write_text("package [CSV]\n")
        result = self.invoke("--version", "1.2.3")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("header documentation", result.stderr)
        self.assertFalse((self.root / ".docs-api").exists())
