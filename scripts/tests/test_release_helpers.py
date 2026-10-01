from __future__ import annotations

import argparse
import json
import os
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
            args = argparse.Namespace(
                manual=str(root / "none"), api=str(root / "none"), repo="o/r", output=str(root / "site")
            )
            with self.assertRaises(RuntimeError):
                helpers.cmd_assemble_pages(args)

    def test_manual_with_its_own_api_directory_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "manual" / "api").mkdir(parents=True)
            (root / "manual" / "index.html").write_text("manual", encoding="utf-8")
            (root / "api").mkdir()
            (root / "api" / "index.html").write_text("api", encoding="utf-8")
            args = argparse.Namespace(
                manual=str(root / "manual"), api=str(root / "api"), repo="o/r", output=str(root / "site")
            )
            with self.assertRaisesRegex(RuntimeError, "api/"):
                helpers.cmd_assemble_pages(args)


class ZipTreeTests(unittest.TestCase):
    def test_files_sit_under_one_prefix_in_a_stable_order(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "site" / "b").mkdir(parents=True)
            (root / "site" / "index.html").write_text("<p>hi</p>", encoding="utf-8")
            (root / "site" / "b" / "a.css").write_text("p{}", encoding="utf-8")
            output = root / "out.zip"
            helpers.zip_tree(root / "site", output, "roc-parser-manual-2.0.0")
            with zipfile.ZipFile(output) as archive:
                self.assertEqual(
                    archive.namelist(),
                    ["roc-parser-manual-2.0.0/b/a.css", "roc-parser-manual-2.0.0/index.html"],
                )

    def test_an_empty_tree_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "site").mkdir()
            with self.assertRaises(RuntimeError):
                helpers.zip_tree(root / "site", root / "out.zip", "manual")


class PackageDocsTests(unittest.TestCase):
    def test_asset_names(self) -> None:
        self.assertEqual(helpers.asset_name("manual-zip", "2.0.0"), "roc-parser-manual-2.0.0.zip")
        self.assertEqual(helpers.asset_name("manual-pdf", "2.0.0"), "roc-parser-manual-2.0.0.pdf")
        self.assertEqual(helpers.asset_name("api-zip", "2.0.0"), "roc-parser-api-docs-2.0.0.zip")

    def test_invalid_versions_are_rejected_before_building(self) -> None:
        for version in ("", " ", "a/b", "a\\b"):
            args = argparse.Namespace(release_version=version, docs_version="", roc="roc", output_dir="unused")
            with unittest.mock.patch.dict(os.environ, {"RELEASE_VERSION": ""}), \
                    unittest.mock.patch.object(helpers.subprocess, "run") as run:
                with self.assertRaises(RuntimeError):
                    helpers.cmd_package_docs(args)
                run.assert_not_called()

    def test_builds_api_and_manual_and_packages_three_assets(self) -> None:
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
                    (root / "roc-parser.pdf").write_bytes(b"%PDF")
                else:
                    self.fail(f"unexpected command {command}")

            args = argparse.Namespace(release_version="2.0.0", docs_version="", roc="roc", output_dir=str(output_dir))
            with unittest.mock.patch.object(helpers.subprocess, "run", side_effect=fake_run), \
                    unittest.mock.patch.object(helpers, "ROOT", Path(temporary)):
                helpers.cmd_package_docs(args)
            self.assertEqual(
                sorted(path.name for path in output_dir.iterdir()),
                ["roc-parser-api-docs-2.0.0.zip", "roc-parser-manual-2.0.0.pdf", "roc-parser-manual-2.0.0.zip"],
            )
            with zipfile.ZipFile(output_dir / "roc-parser-manual-2.0.0.zip") as archive:
                self.assertEqual(archive.namelist(), ["roc-parser-manual-2.0.0/index.html"])
            with zipfile.ZipFile(output_dir / "roc-parser-api-docs-2.0.0.zip") as archive:
                self.assertEqual(archive.namelist(), ["roc-parser-api-docs-2.0.0/index.html"])
            self.assertFalse((Path(temporary) / ".docs-out" / "release-2.0.0").exists())


class ReleaseNotesTests(unittest.TestCase):
    def make_notes(self, root: Path, version: str, bump: str | None = None) -> str:
        bundles = root / "bundles.json"
        bundles.write_text(json.dumps([{"artifact_file": f"{version}-hash.tar.zst"}]), encoding="utf-8")
        bump_file = root / "bump.txt"
        if bump is not None:
            bump_file.write_text(bump, encoding="utf-8")
        output = root / "release.md"
        args = argparse.Namespace(
            release_version=version, release_bundles=str(bundles), output_file=str(output),
            docs_url="https://example.com/docs/", notes_dir=str(root / "notes"), bump_output=str(bump_file),
        )
        with unittest.mock.patch.dict(os.environ, {"GITHUB_REPOSITORY": "owner/repo"}):
            helpers.cmd_make_release_notes(args)
        return output.read_text(encoding="utf-8")

    def test_uses_versioned_notes_and_keeps_generated_links(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "notes").mkdir()
            (root / "notes" / "2.0.0.md").write_text("# roc-parser 2.0.0\n\nNew API.\n", encoding="utf-8")
            body = self.make_notes(root, "2.0.0")
            self.assertTrue(body.startswith("# roc-parser 2.0.0\n\nNew API.\n"))
            self.assertIn('parser: "https://github.com/owner/repo/releases/download/2.0.0/2.0.0-hash.tar.zst"', body)
            self.assertIn("https://example.com/docs/", body)
            for asset in ("roc-parser-manual-2.0.0.pdf", "roc-parser-manual-2.0.0.zip", "roc-parser-api-docs-2.0.0.zip"):
                self.assertIn(f"https://github.com/owner/repo/releases/download/2.0.0/{asset}", body)
            self.assertNotIn("Roc API changes", body)

    def test_missing_versioned_notes_uses_generated_intro(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            body = self.make_notes(Path(temporary), "2.1.0")
            self.assertTrue(body.startswith("Release 2.1.0.\n"))

    def test_empty_versioned_notes_are_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "notes").mkdir()
            (root / "notes" / "2.0.0.md").write_text("  \n", encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "release notes are empty"):
                self.make_notes(root, "2.0.0")

    def test_bump_output_is_included(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            body = self.make_notes(Path(temporary), "2.0.0", bump="Removed Parser.foo\n")
            self.assertIn("## Roc API changes\n\n```\nRemoved Parser.foo\n```", body)

    def test_published_notes_exist_for_the_next_release(self) -> None:
        notes = Path(__file__).resolve().parents[2] / "docs" / "releases" / "2.0.0.md"
        self.assertTrue(helpers.read_editorial_notes(notes.parent, "2.0.0"))


class PackageExamplesTests(unittest.TestCase):
    URL = "https://github.com/owner/repo/releases/download/2.0.0/abc.tar.zst"

    def make_examples(self, root: Path) -> Path:
        examples = root / "examples"
        examples.mkdir()
        for name in ("a.roc", "b.roc"):
            (examples / name).write_text(
                'app [main!] {\n\tcli: platform "https://x.test/cli.tar.zst",\n\tparser: "../package/main.roc",\n}\n',
                encoding="utf-8",
            )
        return examples

    def test_archive_pins_every_example_to_the_release(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples = self.make_examples(root)
            args = argparse.Namespace(
                release_version="2.0.0", bundle_url=self.URL, release_bundles="", repo="owner/repo",
                examples_dir=str(examples), output="", output_dir=str(root / "out"),
            )
            self.assertEqual(helpers.cmd_package_examples(args), 0)
            archive = root / "out" / "roc-parser-examples-2.0.0.zip"
            with zipfile.ZipFile(archive) as bundle:
                self.assertEqual(sorted(bundle.namelist()), [
                    "roc-parser-examples-2.0.0/README.md",
                    "roc-parser-examples-2.0.0/examples/a.roc",
                    "roc-parser-examples-2.0.0/examples/b.roc",
                ])
                app = bundle.read("roc-parser-examples-2.0.0/examples/a.roc").decode()
                readme = bundle.read("roc-parser-examples-2.0.0/README.md").decode()
            self.assertIn(f'parser: "{self.URL}"', app)
            self.assertIn('platform "https://x.test/cli.tar.zst"', app)
            self.assertNotIn("../package", app)
            self.assertIn("release 2.0.0", readme)
            self.assertIn(self.URL, readme)
            # The repository's examples are left alone.
            self.assertIn("../package/main.roc", (examples / "a.roc").read_text(encoding="utf-8"))

    def test_url_resolves_from_release_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples = self.make_examples(root)
            metadata = root / "release-bundles.json"
            metadata.write_text(json.dumps([{"artifact_file": "abc.tar.zst"}]), encoding="utf-8")
            output = root / "examples.zip"
            args = argparse.Namespace(
                release_version="2.0.0", bundle_url="", release_bundles=str(metadata), repo="owner/repo",
                examples_dir=str(examples), output=str(output), output_dir="",
            )
            helpers.cmd_package_examples(args)
            with zipfile.ZipFile(output) as bundle:
                self.assertIn(self.URL, bundle.read("roc-parser-examples-2.0.0/examples/b.roc").decode())

    def test_an_example_without_one_parser_dependency_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples = self.make_examples(root)
            (examples / "c.roc").write_text("app [main!] {}\n", encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "c.roc must declare exactly one"):
                helpers.package_examples(examples, self.URL, "2.0.0", root / "x.zip")

    def test_a_version_is_required(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaisesRegex(RuntimeError, "release version"):
                helpers.package_examples(self.make_examples(root), self.URL, "", root / "x.zip")


if __name__ == "__main__":
    unittest.main()
