from __future__ import annotations

import json
import tempfile
import unittest
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


if __name__ == "__main__":
    unittest.main()
