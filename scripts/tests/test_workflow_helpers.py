from __future__ import annotations

import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts import workflow_helpers


class WorkflowHelpersTests(unittest.TestCase):
    def test_append_github_output(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            output = Path(tmp) / "output"
            workflow_helpers.append_github_output(output, "version", "2.1.0")
            workflow_helpers.append_github_output(output, "prerelease", "false")
            self.assertEqual(output.read_text(encoding="utf-8"), "version=2.1.0\nprerelease=false\n")

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
