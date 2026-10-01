from __future__ import annotations

import json
import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts import workflow_helpers


class WorkflowHelpersTests(unittest.TestCase):
    def test_read_roc_version_and_append_github_output(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            version_file = root / ".roc-version"
            output = root / "output"
            version_file.write_text("nightly-2026-09-01-db83307\n", encoding="utf-8")

            version = workflow_helpers.read_roc_version(version_file)
            workflow_helpers.append_github_output(output, "nightly-tag", version)

            self.assertEqual(
                output.read_text(encoding="utf-8"),
                "nightly-tag=nightly-2026-09-01-db83307\n",
            )

    def test_validate_release_ref_requires_default_branch(self) -> None:
        workflow_helpers.validate_release_ref("branch", "main", "main")
        with self.assertRaisesRegex(ValueError, "default branch"):
            workflow_helpers.validate_release_ref("branch", "feature", "main")
        with self.assertRaisesRegex(ValueError, "from a branch"):
            workflow_helpers.validate_release_ref("tag", "1.2.3", "main")

    def test_resolve_bundle_url(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            metadata = Path(tmp) / "bundles.json"
            metadata.write_text(
                json.dumps([{"artifact_file": "abc123.tar.zst"}]),
                encoding="utf-8",
            )
            url = workflow_helpers.resolve_bundle_url(
                metadata,
                "owner/roc-parser",
                "1.2.3",
            )
            self.assertEqual(
                url,
                "https://github.com/owner/roc-parser/releases/download/1.2.3/abc123.tar.zst",
            )

    def test_resolve_bundle_url_requires_one_safe_artifact(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            metadata = Path(tmp) / "bundles.json"
            for bundles in ([], [{"artifact_file": "../bad.tar.zst"}]):
                metadata.write_text(json.dumps(bundles), encoding="utf-8")
                with self.assertRaises(ValueError):
                    workflow_helpers.resolve_bundle_url(metadata, "owner/repo", "1.2.3")

    def test_require_success(self) -> None:
        workflow_helpers.require_success(["success", "success"])
        with self.assertRaisesRegex(ValueError, "failure"):
            workflow_helpers.require_success(["success", "failure", "skipped"])

    def test_github_outputs_must_be_single_line(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaisesRegex(ValueError, "single-line"):
                workflow_helpers.append_github_output(Path(tmp) / "output", "name", "bad\nvalue")


class ValidateExamplesTests(unittest.TestCase):
    def test_repository_examples_must_use_the_package_source(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            examples = Path(tmp)
            (examples / "a.roc").write_text('app [main!] {\n\tparser: "../package/main.roc",\n}\n', encoding="utf-8")
            workflow_helpers.validate_examples(examples)
            (examples / "b.roc").write_text('app [main!] {\n\tparser: "https://x.test/a.tar.zst",\n}\n', encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "b.roc must depend on"):
                workflow_helpers.validate_examples(examples)

    def test_archive_examples_must_share_one_bundle_url(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            archive = Path(tmp) / "examples.zip"
            with zipfile.ZipFile(archive, "w") as bundle:
                bundle.writestr("x/examples/a.roc", 'parser: "https://x.test/a.tar.zst"\n')
                bundle.writestr("x/examples/b.roc", 'parser: "https://x.test/a.tar.zst"\n')
            workflow_helpers.validate_examples(archive=archive)
            with zipfile.ZipFile(archive, "a") as bundle:
                bundle.writestr("x/examples/c.roc", 'parser: "../package/main.roc"\n')
            with self.assertRaisesRegex(ValueError, "c.roc must depend on parser through a bundle URL"):
                workflow_helpers.validate_examples(archive=archive)


if __name__ == "__main__":
    unittest.main()
