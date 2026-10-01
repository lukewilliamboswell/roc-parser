from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts import update_roc_nightly


class UpdateRocNightlyTests(unittest.TestCase):
    def test_update_version_file(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            version_file = Path(tmp) / ".roc-version"
            version_file.write_text("nightly-2026-09-01-db83307\n", encoding="utf-8")
            self.assertTrue(
                update_roc_nightly.update_version_file(
                    version_file, "nightly-2026-09-02-abcdef0"
                )
            )
            self.assertEqual(version_file.read_text(encoding="utf-8"), "nightly-2026-09-02-abcdef0\n")
            self.assertFalse(
                update_roc_nightly.update_version_file(
                    version_file, "nightly-2026-09-02-abcdef0"
                )
            )

    def test_update_blueprint_file(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            blueprint = Path(tmp) / "Blueprint.roc"
            blueprint.write_text('tools = [\n\t"rocpkgs.nightly-2026-09-01-db83307",\n\t"go",\n]\n', encoding="utf-8")
            self.assertTrue(update_roc_nightly.update_blueprint_file(blueprint, "nightly-2026-09-02-abcdef0"))
            self.assertEqual(
                blueprint.read_text(encoding="utf-8"),
                'tools = [\n\t"rocpkgs.nightly-2026-09-02-abcdef0",\n\t"go",\n]\n',
            )
            self.assertFalse(update_roc_nightly.update_blueprint_file(blueprint, "nightly-2026-09-02-abcdef0"))
            blueprint.write_text('"go"\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                update_roc_nightly.update_blueprint_file(blueprint, "nightly-2026-09-02-abcdef0")

    def test_repository_blueprint_matches_roc_version(self) -> None:
        root = Path(update_roc_nightly.__file__).resolve().parents[1]
        self.assertEqual(
            update_roc_nightly.blueprint_nightly((root / "Blueprint.roc").read_text(encoding="utf-8")),
            (root / ".roc-version").read_text(encoding="utf-8").strip(),
        )

    def test_invalid_tag_is_rejected(self) -> None:
        with self.assertRaises(ValueError):
            update_roc_nightly.validate_nightly_tag("nightly")


if __name__ == "__main__":
    unittest.main()
