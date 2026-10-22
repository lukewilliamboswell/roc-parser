from __future__ import annotations

import json
import tempfile
import unittest
import zipfile
from pathlib import Path

from scripts import release_helpers, test_bundle_examples

HEADER = 'app [main] {{\n    parser: "{}",\n}}\n'
URL = "https://github.com/owner/repo/releases/download/1.2.3/abc.tar.zst"


def case(**fields: object) -> dict:
    return {"name": "default", "args": [], "stdin": "", "stdout": "hi\n", "stderr": "", "exit_code": 0, **fields}


class CheckoutTests(unittest.TestCase):
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

    def test_the_repository_spec_lists_every_example(self) -> None:
        apps = test_bundle_examples.load_spec()
        self.assertEqual(
            [app["path"] for app in apps],
            sorted(f"examples/{path.name}" for path in (test_bundle_examples.ROOT / "examples").glob("*.roc")),
        )


class SpecTests(unittest.TestCase):
    def test_a_case_names_every_field(self) -> None:
        test_bundle_examples.validate_apps([{"path": "examples/a.roc", "cases": [case()]}], "spec")
        for bad in (case(exit_code="0"), case(args=[1]), {k: v for k, v in case().items() if k != "stdin"},
                    case(extra=True), case(exit_code=True)):
            with self.subTest(case=bad), self.assertRaises(ValueError):
                test_bundle_examples.validate_apps([{"path": "examples/a.roc", "cases": [bad]}], "spec")

    def test_apps_are_unique_and_under_examples(self) -> None:
        for apps in ([], [{"path": "a.roc", "cases": [case()]}], [{"path": "examples/../a.roc", "cases": [case()]}],
                     [{"path": "examples/a.roc", "cases": [case()]}] * 2,
                     [{"path": "examples/a.roc", "cases": [case(), case()]}],
                     [{"path": "examples/a.roc", "cases": []}]):
            with self.subTest(apps=apps), self.assertRaises(ValueError):
                test_bundle_examples.validate_apps(apps, "spec")

    def test_load_spec_requires_every_app(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "examples").mkdir()
            (root / "examples" / "a.roc").write_text("", encoding="utf-8")
            (root / "examples" / "b.roc").write_text("", encoding="utf-8")
            spec = root / "spec.json"
            spec.write_text(json.dumps({"apps": [{"path": "examples/a.roc", "cases": [case()]}]}), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "missing \\['examples/b.roc'\\]"):
                test_bundle_examples.load_spec(spec, root / "examples")

    def test_differences_name_the_stream_and_exit_code(self) -> None:
        problems = test_bundle_examples.differences("examples/a.roc", case(), case(stdout="bye\n", exit_code=1))
        self.assertEqual(len(problems), 2)
        self.assertIn("stdout differs", problems[0])
        self.assertIn("-hi", problems[0])
        self.assertIn("exited 1, expected 0", problems[1])


class ArchiveTests(unittest.TestCase):
    def make_archive(self, root: Path) -> tuple[Path, Path]:
        examples = root / "examples"
        (examples / "assets").mkdir(parents=True)
        (examples / "a.roc").write_text(HEADER.format("../package/main.roc"), encoding="utf-8")
        (examples / "assets" / "data.csv").write_text("x,y\n", encoding="utf-8")
        spec = root / "examples.json"
        spec.write_text(json.dumps({"apps": [{"path": "examples/a.roc", "cases": [case()]}]}), encoding="utf-8")
        archive = release_helpers.package_examples(
            examples, URL, "1.2.3", root / "out" / "roc-parser-examples-1.2.3.zip", "owner/repo", spec
        )
        return archive, examples

    def check(self, archive: Path, examples: Path) -> list[dict]:
        spec = examples.parent / "examples.json"
        original = test_bundle_examples.SPEC
        test_bundle_examples.SPEC = spec
        try:
            return test_bundle_examples.check_archive(archive, "1.2.3", URL, examples)
        finally:
            test_bundle_examples.SPEC = original

    def test_the_canonical_archive_is_accepted(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            archive, examples = self.make_archive(Path(tmp))
            with zipfile.ZipFile(archive) as bundle:
                self.assertEqual(sorted(bundle.namelist()), [
                    "roc-parser-examples-1.2.3/README.md",
                    "roc-parser-examples-1.2.3/examples/a.roc",
                    "roc-parser-examples-1.2.3/examples/assets/data.csv",
                    "roc-parser-examples-1.2.3/spec.json",
                ])
                spec = json.loads(bundle.read("roc-parser-examples-1.2.3/spec.json"))
            self.assertEqual(spec["schema_version"], 1)
            self.assertEqual(spec["release"], "1.2.3")
            self.assertEqual(spec["dependencies"], [URL])
            self.assertEqual(self.check(archive, examples)[0]["path"], "examples/a.roc")

    def test_an_archive_that_differs_beyond_the_dependency_is_refused(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            archive, examples = self.make_archive(Path(tmp))
            (examples / "assets" / "data.csv").write_text("x,y,z\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "data.csv differs"):
                self.check(archive, examples)

    def test_an_archive_for_another_release_is_refused(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            archive, examples = self.make_archive(Path(tmp))
            with self.assertRaisesRegex(ValueError, "release '1.2.4'"):
                test_bundle_examples.check_archive(archive, "1.2.4", URL, examples)


if __name__ == "__main__":
    unittest.main()
