from __future__ import annotations

import tempfile
import unittest
import subprocess
import base64
from pathlib import Path
from unittest.mock import patch

from scripts import github_signed_commit


class GitHubSignedCommitTests(unittest.TestCase):
    def test_validate_path_accepts_repository_paths(self) -> None:
        self.assertEqual(github_signed_commit.validate_path("www/1.2.0/index.html"), "www/1.2.0/index.html")

    def test_validate_path_rejects_paths_outside_repository(self) -> None:
        for path in ("", "/tmp/file", "../secret", "www/../../secret"):
            with self.subTest(path=path), self.assertRaises(ValueError):
                github_signed_commit.validate_path(path)

    def test_outputs_must_be_single_line(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaisesRegex(ValueError, "single-line"):
                github_signed_commit.append_output(Path(tmp) / "output", "name", "bad\nvalue")

    def test_staged_file_changes_collects_additions_and_deletions(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            subprocess.run(["git", "init", "-q"], cwd=root, check=True)
            (root / "old.txt").write_text("old\n", encoding="utf-8")
            subprocess.run(["git", "add", "old.txt"], cwd=root, check=True)
            subprocess.run(
                [
                    "git",
                    "-c",
                    "user.name=Test",
                    "-c",
                    "user.email=test@example.com",
                    "commit",
                    "-q",
                    "-m",
                    "initial",
                ],
                cwd=root,
                check=True,
            )
            (root / "old.txt").unlink()
            (root / "new.txt").write_bytes(b"new\x00contents")
            with patch.object(github_signed_commit, "ROOT", root):
                changes = github_signed_commit.staged_file_changes(["old.txt", "new.txt"])
            self.assertEqual(changes["deletions"], [{"path": "old.txt"}])
            self.assertEqual(
                changes["additions"],
                [{"path": "new.txt", "contents": base64.b64encode(b"new\x00contents").decode("ascii")}],
            )


if __name__ == "__main__":
    unittest.main()
