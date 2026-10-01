from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from scripts import test_bundle_examples

HEADER = 'app [main] {{\n    parser: "{}",\n}}\n'


class TestBundleExamplesScriptTests(unittest.TestCase):
    def test_checkout_examples_use_the_checkout_package(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp)
            example = source / "example.roc"
            example.write_text(HEADER.format("../package/main.roc"), encoding="utf-8")

            self.assertEqual(test_bundle_examples.checkout_examples(source), [example])

    def test_checkout_examples_reject_a_pinned_url(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp)
            (source / "example.roc").write_text(
                HEADER.format("https://example.test/1.2.3/bundle.tar.zst"), encoding="utf-8"
            )

            with self.assertRaisesRegex(SystemExit, "must depend on"):
                test_bundle_examples.checkout_examples(source)

    def test_package_and_extract_tests_the_archive(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "source"
            target = root / "target"
            source.mkdir()
            target.mkdir()
            for name in ("example.roc", "xml-svg.roc"):
                (source / name).write_text(HEADER.format("../package/main.roc"), encoding="utf-8")

            url = "http://127.0.0.1:1234/bundle.tar.zst"
            examples = test_bundle_examples.package_and_extract(target, url, source_dir=source)

            self.assertEqual([path.name for path in examples], ["example.roc", "xml-svg.roc"])
            self.assertEqual(examples[0].parent, target / "roc-parser-examples-local" / "examples")
            self.assertIn(f'parser: "{url}"', examples[0].read_text(encoding="utf-8"))
            self.assertTrue((target / "roc-parser-examples-local" / "README.md").is_file())

    def test_package_and_extract_rejects_missing_package_dependency(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            source = root / "source"
            source.mkdir()
            (source / "example.roc").write_text("app [main] {}\n", encoding="utf-8")

            with self.assertRaisesRegex(SystemExit, "exactly one parser package dependency"):
                test_bundle_examples.package_and_extract(
                    root, "http://example.test/bundle.tar.zst", source_dir=source
                )


if __name__ == "__main__":
    unittest.main()
