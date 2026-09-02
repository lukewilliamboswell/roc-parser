from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts import bundle


class BundleScriptTests(unittest.TestCase):
    def test_parse_args_preserves_roc_arguments(self) -> None:
        output_dir, roc_args = bundle.parse_args(
            ["--output-dir=build/release", "--some-roc-option", "value"]
        )
        self.assertEqual(output_dir, Path("build/release"))
        self.assertEqual(roc_args, ["--some-roc-option", "value"])

    def test_bundle_command_lists_main_first_then_sorted_modules(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            package = root / "package"
            package.mkdir()
            for name in ("Yaml.roc", "main.roc", "CSV.roc"):
                (package / name).touch()

            command = bundle.bundle_command(
                root=root,
                output_dir=root / "dist",
                roc="roc",
                roc_args=["--flag"],
            )

        self.assertEqual(
            command,
            [
                "roc",
                "bundle",
                "main.roc",
                "CSV.roc",
                "Yaml.roc",
                "--output-dir",
                str(root / "dist"),
                "--flag",
            ],
        )


if __name__ == "__main__":
    unittest.main()
