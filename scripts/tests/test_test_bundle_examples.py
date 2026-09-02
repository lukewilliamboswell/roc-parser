from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts import test_bundle_examples


class TestBundleExamplesScriptTests(unittest.TestCase):
    def test_copy_examples_rewrites_url_and_skips_known_example(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "source"
            target = root / "target"
            source.mkdir()
            target.mkdir()
            (source / "example.roc").write_text('    parser: "old"\n', encoding="utf-8")
            (source / "xml-svg.roc").write_text('    parser: "old"\n', encoding="utf-8")

            examples = test_bundle_examples.copy_examples_with_bundle_url(
                target,
                "http://127.0.0.1:1234/bundle.tar.zst",
                source_dir=source,
            )

            self.assertEqual([path.name for path in examples], ["example.roc"])
            rewritten = (target / "examples" / "example.roc").read_text(encoding="utf-8")
            self.assertIn('parser: "http://127.0.0.1:1234/bundle.tar.zst"', rewritten)

    def test_copy_examples_rejects_missing_package_dependency(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "source"
            target = root / "target"
            source.mkdir()
            target.mkdir()
            (source / "example.roc").write_text("app [main] {}\n", encoding="utf-8")

            with self.assertRaisesRegex(SystemExit, "does not declare"):
                test_bundle_examples.copy_examples_with_bundle_url(
                    target,
                    "http://example.test/bundle.tar.zst",
                    source_dir=source,
                )


if __name__ == "__main__":
    unittest.main()
