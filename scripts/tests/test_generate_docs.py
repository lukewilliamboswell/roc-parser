from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from scripts import generate_docs


class GenerateDocsScriptTests(unittest.TestCase):
    def test_normalize_version_accepts_optional_v_prefix(self) -> None:
        self.assertEqual(generate_docs.normalize_version("v1.2.3"), "1.2.3")
        self.assertEqual(generate_docs.normalize_version("1.2.3"), "1.2.3")

    def test_normalize_version_rejects_non_semver(self) -> None:
        with self.assertRaisesRegex(ValueError, "format x.y.z"):
            generate_docs.normalize_version("1.2")

    def test_main_replaces_version_directory_and_writes_redirect(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            docs_root = root / "site"
            old_docs = docs_root / "1.2.3"
            old_docs.mkdir(parents=True)
            (old_docs / "obsolete.html").touch()
            completed = subprocess.CompletedProcess([], 0)

            with (
                mock.patch.object(generate_docs, "ROOT", root),
                mock.patch.object(generate_docs, "run_command", return_value=completed) as run,
            ):
                status = generate_docs.main(["v1.2.3", "--docs-root", str(docs_root)])

            self.assertEqual(status, 0)
            self.assertFalse((old_docs / "obsolete.html").exists())
            self.assertIn("/roc-parser/1.2.3/", (docs_root / "index.html").read_text())
            run.assert_called_once_with(
                ["roc", "docs", "package/main.roc", f"--output={old_docs}"]
            )

    def test_skip_index_leaves_existing_index_untouched(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            docs_root = root / "site"
            docs_root.mkdir()
            index = docs_root / "index.html"
            index.write_text("keep", encoding="utf-8")

            with (
                mock.patch.object(generate_docs, "ROOT", root),
                mock.patch.object(
                    generate_docs,
                    "run_command",
                    return_value=subprocess.CompletedProcess([], 0),
                ),
            ):
                status = generate_docs.main(
                    ["1.2.3", "--docs-root", str(docs_root), "--skip-index"]
                )

            self.assertEqual(status, 0)
            self.assertEqual(index.read_text(encoding="utf-8"), "keep")


if __name__ == "__main__":
    unittest.main()
