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

URL = "https://github.com/owner/repo/releases/download/2.0.0/abc.tar.zst"


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
        self.assertEqual(helpers.asset_name("examples-zip", "2.0.0"), "roc-parser-examples-2.0.0.zip")

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


class VersionTests(unittest.TestCase):
    def test_semver_precedence(self) -> None:
        versions = ["2.1.0", "2.0.0", "2.1.0-rc.10", "2.1.0-rc.2", "10.0.0", "2.1.0-rc.1"]
        self.assertEqual(sorted(versions, key=helpers.semver_key),
                         ["2.0.0", "2.1.0-rc.1", "2.1.0-rc.2", "2.1.0-rc.10", "2.1.0", "10.0.0"])
        for bad in ("v2.0.0", "2.0", "02.0.0", "unreleased"):
            with self.subTest(version=bad), self.assertRaises(ValueError):
                helpers.semver_key(bad)

    def test_the_dry_run_renders_the_highest_version(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            notes = Path(temporary)
            for name in ("2.0.0", "2.1.0-rc.3", "2.1.0", "unreleased", "index"):
                (notes / f"{name}.adoc").write_text("= x\n", encoding="utf-8")
            self.assertEqual(helpers.latest_notes(notes).name, "2.1.0.adoc")

    def test_prerelease(self) -> None:
        self.assertTrue(helpers.is_prerelease("2.1.0-rc.1"))
        self.assertTrue(helpers.is_prerelease(helpers.DRY_RUN_VERSION))
        self.assertFalse(helpers.is_prerelease("2.1.0"))


class ReleaseNotesTests(unittest.TestCase):
    NOTES = (
        "= roc-parser 2.0.0\n\nThis release adds *JSON* and https://example.com/docs[the docs].\n\n"
        "== Use this release\n\n[source,roc]\n----\napp [main!] {\n    parser: \"{package-url}\",\n}\n----\n\n"
        "=== Changes\n\n* `JsonValue.parse_str` with `+{ x }+`\n** nested item\n. first\n\n"
        "[cols=\"1,1\",options=\"header\"]\n|===\n| Old | New\n| `a` | `b`\n|===\n\nNOTE: Read this.\n"
    )

    def test_renders_markdown_with_the_url_and_compilers(self) -> None:
        body = helpers.render_notes(self.NOTES, URL, "Roc compiler version nightly-2026-10-21-5349569")
        self.assertEqual(body, (
            "# roc-parser 2.0.0\n\nThis release adds **JSON** and [the docs](https://example.com/docs).\n\n"
            f"## Use this release\n\n```roc\napp [main!] {{\n    parser: \"{URL}\",\n}}\n```\n\n"
            "### Changes\n\n- `JsonValue.parse_str` with `{ x }`\n  - nested item\n1. first\n\n"
            "| Old | New |\n|---|---|\n| `a` | `b` |\n\n> **Note:** Read this.\n\n"
            "## Compilers\n\n```\nRoc compiler version nightly-2026-10-21-5349569\n```\n"
        ))

    def test_notes_must_use_the_package_url(self) -> None:
        with self.assertRaisesRegex(RuntimeError, "never use"):
            helpers.render_notes("= x\n\nNo URL.\n", URL, "roc")
        with self.assertRaisesRegex(RuntimeError, "tzdb-url"):
            helpers.render_notes("= x\n\n{package-url} {tzdb-url}\n", URL, "roc")

    def test_unsupported_markup_is_refused(self) -> None:
        for source in ("====\nexample\n====\n", ":toc:\n", ".Title\n", "a\n+\nb\n", "[source,roc]\ntext\n", "----\nopen\n"):
            with self.subTest(source=source), self.assertRaises(ValueError):
                helpers.notes_markdown(source)

    def test_make_release_notes_writes_the_body(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            notes = root / "2.0.0.adoc"
            notes.write_text(self.NOTES, encoding="utf-8")
            args = argparse.Namespace(release_version="2.0.0", bundle="abc.tar.zst", notes=notes,
                                      output=root / "notes.md", repo="owner/repo")
            with unittest.mock.patch.object(helpers, "compiler_versions", return_value="Roc compiler version x"):
                helpers.cmd_make_release_notes(args)
            body = (root / "notes.md").read_text(encoding="utf-8")
            self.assertIn(f'parser: "{URL}"', body)
            self.assertTrue(body.endswith("## Compilers\n\n```\nRoc compiler version x\n```\n"))

    def test_missing_or_empty_notes_are_refused(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            with self.assertRaisesRegex(RuntimeError, "write the release notes first"):
                helpers.read_notes(root / "2.1.0.adoc")
            (root / "2.1.0.adoc").write_text("  \n", encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "empty"):
                helpers.read_notes(root / "2.1.0.adoc")

    def test_every_notes_file_converts(self) -> None:
        for path in sorted(helpers.NOTES_DIR.glob("*.adoc")):
            if path.name == "index.adoc":
                continue
            with self.subTest(notes=path.name):
                helpers.notes_markdown(path.read_text(encoding="utf-8"))

    def test_the_next_release_notes_use_the_package_url(self) -> None:
        self.assertIn(helpers.PLACEHOLDER, helpers.latest_notes().read_text(encoding="utf-8"))


class PackageExamplesTests(unittest.TestCase):
    def make_examples(self, root: Path) -> tuple[Path, Path]:
        examples = root / "examples"
        examples.mkdir()
        for name in ("a.roc", "b.roc"):
            (examples / name).write_text(
                'app [main!] {\n\tcli: platform "https://x.test/cli.tar.zst",\n\tparser: "../package/main.roc",\n}\n',
                encoding="utf-8",
            )
        spec = root / "examples.json"
        cases = [{"name": "default", "args": [], "stdin": "", "stdout": "", "stderr": "", "exit_code": 0}]
        spec.write_text(json.dumps({"apps": [{"path": f"examples/{n}", "cases": cases} for n in ("a.roc", "b.roc")]}),
                        encoding="utf-8")
        return examples, spec

    def test_archive_pins_every_example_to_the_release(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples, spec = self.make_examples(root)
            archive = helpers.package_examples(examples, URL, "2.0.0", root / "out" / "x.zip", "owner/repo", spec)
            with zipfile.ZipFile(archive) as bundle:
                self.assertEqual(sorted(bundle.namelist()), [
                    "roc-parser-examples-2.0.0/README.md",
                    "roc-parser-examples-2.0.0/examples/a.roc",
                    "roc-parser-examples-2.0.0/examples/b.roc",
                    "roc-parser-examples-2.0.0/spec.json",
                ])
                app = bundle.read("roc-parser-examples-2.0.0/examples/a.roc").decode()
                readme = bundle.read("roc-parser-examples-2.0.0/README.md").decode()
                archived_spec = json.loads(bundle.read("roc-parser-examples-2.0.0/spec.json"))
            self.assertIn(f'parser: "{URL}"', app)
            self.assertIn('platform "https://x.test/cli.tar.zst"', app)
            self.assertNotIn("../package", app)
            self.assertIn("release 2.0.0", readme)
            self.assertIn(URL, readme)
            self.assertEqual(archived_spec["schema_version"], 1)
            self.assertEqual(archived_spec["release"], "2.0.0")
            self.assertEqual(archived_spec["dependencies"], [URL])
            self.assertEqual([app["path"] for app in archived_spec["apps"]], ["examples/a.roc", "examples/b.roc"])
            # The repository's examples are left alone.
            self.assertIn("../package/main.roc", (examples / "a.roc").read_text(encoding="utf-8"))

    def test_url_is_made_from_the_bundle_name(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples, _ = self.make_examples(root)
            args = argparse.Namespace(
                release_version="2.0.0", bundle_url="", bundle="dist/abc.tar.zst", repo="owner/repo",
                examples_dir=examples, output=root / "examples.zip", output_dir=None,
            )
            with unittest.mock.patch.object(helpers, "examples_spec", return_value={"apps": []}):
                helpers.cmd_package_examples(args)
            with zipfile.ZipFile(root / "examples.zip") as bundle:
                self.assertIn(URL, bundle.read("roc-parser-examples-2.0.0/examples/b.roc").decode())

    def test_adjacent_imported_assets_are_preserved_without_rewriting(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples, spec = self.make_examples(root)
            assets = examples / "assets"
            assets.mkdir()
            contents = {
                "settings.yaml": b"port: 8080\r\n",
                "data.csv": b"name,age\nAda,36\n",
                "source.roc": b"answer = 42\n",
                "nested/examples/source.roc": b"answer = 43\n",
            }
            for name, content in contents.items():
                (assets / name).parent.mkdir(parents=True, exist_ok=True)
                (assets / name).write_bytes(content)
            archive = helpers.package_examples(examples, URL, "2.0.0", root / "examples.zip", spec_path=spec)
            with zipfile.ZipFile(archive) as bundle:
                for name, content in contents.items():
                    self.assertEqual(bundle.read(f"roc-parser-examples-2.0.0/examples/assets/{name}"), content)

    def test_an_example_without_one_parser_dependency_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples, spec = self.make_examples(root)
            (examples / "c.roc").write_text("app [main!] {}\n", encoding="utf-8")
            with self.assertRaisesRegex(RuntimeError, "c.roc must declare exactly one"):
                helpers.package_examples(examples, URL, "2.0.0", root / "x.zip", spec_path=spec)

    def test_a_version_is_required(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            examples, spec = self.make_examples(root)
            with self.assertRaisesRegex(RuntimeError, "release version"):
                helpers.package_examples(examples, URL, "", root / "x.zip", spec_path=spec)


class ValidateTests(unittest.TestCase):
    def validate(self, event: str, version: str = "", ref: str = "refs/heads/main", existing: tuple = ()) -> dict:
        def gh_json(path: str) -> dict | None:
            if path.endswith("releases/latest"):
                return {"tag_name": "2.0.0", "assets": [{"name": "abc.tar.zst", "browser_download_url": URL}]}
            return {} if any(path.endswith(f"/{name}") for name in existing) else None

        with tempfile.TemporaryDirectory() as temporary:
            output = Path(temporary) / "output"
            args = argparse.Namespace(event=event, release_version=version, default_branch="main", ref=ref,
                                      repo="owner/repo", github_output=output)
            with unittest.mock.patch.object(helpers, "gh_json", side_effect=gh_json), \
                    unittest.mock.patch.object(helpers, "check_bump", return_value=0) as bump:
                helpers.cmd_validate(args)
            self.bump = bump
            return dict(line.split("=", 1) for line in output.read_text(encoding="utf-8").splitlines())

    def test_a_pull_request_is_a_dry_run_of_the_next_notes(self) -> None:
        outputs = self.validate("pull_request")
        self.assertEqual(outputs["version"], helpers.DRY_RUN_VERSION)
        self.assertEqual(outputs["prerelease"], "true")
        self.assertEqual(outputs["notes"], str(helpers.latest_notes().relative_to(helpers.ROOT)))
        self.bump.assert_called_once_with(URL, None)

    def test_a_release_needs_the_default_branch_notes_and_a_new_tag(self) -> None:
        version = helpers.latest_notes().stem
        outputs = self.validate("workflow_dispatch", version)
        self.assertEqual(outputs["version"], version)
        self.bump.assert_called_once_with(URL, version)
        with self.assertRaisesRegex(RuntimeError, "refs/heads/main only"):
            self.validate("workflow_dispatch", version, ref="refs/heads/feature")
        with self.assertRaisesRegex(RuntimeError, "already exists"):
            self.validate("workflow_dispatch", version, existing=(version,))
        with self.assertRaisesRegex(RuntimeError, "write the release notes first"):
            self.validate("workflow_dispatch", "99.0.0")
        with self.assertRaises(ValueError):
            self.validate("workflow_dispatch", "v99.0.0")

    def test_an_understated_version_stops_the_release(self) -> None:
        args = argparse.Namespace(event="workflow_dispatch", release_version=helpers.latest_notes().stem,
                                  default_branch="main", ref="refs/heads/main", repo="owner/repo", github_output=None)
        latest = {"tag_name": "2.0.0", "assets": [{"name": "abc.tar.zst", "browser_download_url": URL}]}
        with unittest.mock.patch.object(helpers, "gh_json", side_effect=lambda p: latest if p.endswith("latest") else None), \
                unittest.mock.patch.object(helpers, "check_bump", return_value=1):
            with self.assertRaisesRegex(RuntimeError, "roc bump refuses"):
                helpers.cmd_validate(args)


if __name__ == "__main__":
    unittest.main()
