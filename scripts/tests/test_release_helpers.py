from __future__ import annotations

import argparse
import tempfile
import unittest
import unittest.mock
import zipfile
from pathlib import Path

from scripts import release_helpers as helpers


class AssemblePagesTests(unittest.TestCase):
    def test_manual_at_root_api_beneath_and_old_links_answered(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "manual").mkdir()
            (root / "manual" / "index.html").write_text("manual", encoding="utf-8")
            (root / "api" / "Parser").mkdir(parents=True)
            (root / "api" / "index.html").write_text("api", encoding="utf-8")
            (root / "api" / "Parser" / "index.html").write_text("parser", encoding="utf-8")
            (root / "site" / "1.2.0").mkdir(parents=True)
            args = argparse.Namespace(
                manual=str(root / "manual"), api=str(root / "api"), repo="owner/repo", output=str(root / "site")
            )
            helpers.cmd_assemble_pages(args)
            site = root / "site"
            self.assertEqual((site / "index.html").read_text(encoding="utf-8"), "manual")
            self.assertEqual((site / "api" / "Parser" / "index.html").read_text(encoding="utf-8"), "parser")
            not_found = (site / "404.html").read_text(encoding="utf-8")
            self.assertIn("https://github.com/owner/repo/releases", not_found)
            self.assertIn('href="https://owner.github.io/repo/api/"', not_found)
            self.assertFalse((site / "1.2.0").exists())

    def test_missing_input_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaises(RuntimeError):
                helpers.assemble_pages(root / "none", root / "none", root / "site", "o/r")

    def test_manual_with_its_own_api_directory_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "manual" / "api").mkdir(parents=True)
            (root / "manual" / "index.html").write_text("manual", encoding="utf-8")
            (root / "api").mkdir()
            (root / "api" / "index.html").write_text("api", encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "api/"):
                helpers.assemble_pages(root / "manual", root / "api", root / "site", "o/r")


class ZipTreeTests(unittest.TestCase):
    def test_files_sit_under_one_prefix_in_a_stable_order(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "site" / "b").mkdir(parents=True)
            (root / "site" / "index.html").write_text("<p>hi</p>", encoding="utf-8")
            (root / "site" / "b" / "a.css").write_text("p{}", encoding="utf-8")
            output = root / "out.zip"
            helpers.zip_tree(root / "site", output, "roc-parser-docs-2.0.0")
            with zipfile.ZipFile(output) as archive:
                self.assertEqual(
                    archive.namelist(),
                    ["roc-parser-docs-2.0.0/b/a.css", "roc-parser-docs-2.0.0/index.html"],
                )

    def test_an_empty_tree_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "site").mkdir()
            with self.assertRaises(RuntimeError):
                helpers.zip_tree(root / "site", root / "out.zip", "manual")


class PackageDocsTests(unittest.TestCase):
    def test_asset_names(self) -> None:
        self.assertEqual(helpers.asset_name("docs-zip", "2.0.0"), "roc-parser-docs-2.0.0.zip")
        self.assertEqual(helpers.asset_name("pdf", "2.0.0"), "roc-parser-2.0.0.pdf")
        with self.assertRaises(KeyError):
            helpers.asset_name("examples-zip", "2.0.0")

    def test_invalid_versions_are_rejected_before_building(self) -> None:
        for version in ("", " ", "a/b", "a\\b"):
            with unittest.mock.patch.object(helpers.subprocess, "run") as run:
                with self.assertRaises(RuntimeError):
                    helpers.package_docs(version, Path("unused"), "o/r")
                run.assert_not_called()

    def test_builds_the_site_and_pdf_assets(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            output_dir = Path(temporary) / "out"

            def fake_run(command: list[str], **_: object) -> None:
                script = Path(command[1]).name
                if script == "build_docs.py":
                    version_root = Path(command[command.index("--docs-root") + 1]) / command[command.index("--version") + 1]
                    version_root.mkdir(parents=True)
                    (version_root / "index.html").write_text("api", encoding="utf-8")
                elif script == "build_manual.py":
                    root = Path(command[command.index("--output") + 1])
                    (root / "site").mkdir(parents=True)
                    (root / "site" / "index.html").write_text("manual", encoding="utf-8")
                    (root / "site" / "roc-parser.pdf").write_bytes(b"%PDF")
                    (root / "roc-parser.pdf").write_bytes(b"%PDF")
                else:
                    self.fail(f"unexpected command {command}")

            with unittest.mock.patch.object(helpers.subprocess, "run", side_effect=fake_run), \
                    unittest.mock.patch.object(helpers, "ROOT", Path(temporary)):
                helpers.package_docs("2.0.0", output_dir, "owner/repo", site=Path(temporary) / "site")
            self.assertEqual(
                sorted(path.name for path in output_dir.iterdir()),
                ["roc-parser-2.0.0.pdf", "roc-parser-docs-2.0.0.zip"],
            )
            with zipfile.ZipFile(output_dir / "roc-parser-docs-2.0.0.zip") as archive:
                self.assertEqual(sorted(archive.namelist()), [
                    "roc-parser-docs-2.0.0/404.html",
                    "roc-parser-docs-2.0.0/api/index.html",
                    "roc-parser-docs-2.0.0/index.html",
                    "roc-parser-docs-2.0.0/roc-parser.pdf",
                ])
            self.assertTrue((Path(temporary) / "site" / "api" / "index.html").is_file())
            self.assertFalse((Path(temporary) / ".docs-out" / "release-2.0.0").exists())


if __name__ == "__main__":
    unittest.main()
