from __future__ import annotations

import unittest

from scripts import check, check_asciidoc, check_conformance

WORKFLOWS = check.ROOT / ".github" / "workflows"


def matrix(workflow: str, key: str) -> list[str]:
    """The one-per-line `key:` matrix list of a workflow."""
    text = (WORKFLOWS / workflow).read_text(encoding="utf-8")
    block = text.split(f"        {key}:\n", 1)[1].split("    steps:", 1)[0]
    return [line.strip()[2:] for line in block.splitlines() if line.strip().startswith("- ")]


class CheckTests(unittest.TestCase):
    def test_sections_run_in_their_listed_order(self) -> None:
        self.assertEqual(check.select([]), list(check.SECTIONS))
        self.assertEqual(check.select(["fuzz", "scripts", "fuzz"]), ["fuzz", "scripts"])
        with self.assertRaisesRegex(ValueError, "unknown section"):
            check.select(["nonsense"])

    def test_check_covers_what_pull_requests_ran_before_ci_yml(self) -> None:
        commands = {" ".join(command[1:]) for section in check.SECTIONS.values() for command in section.commands}
        for expected in (
            "-m unittest discover -s scripts/tests -p test_*.py",
            "scripts/all_tests.py",
            "scripts/check_doc_examples.py",
            "scripts/build_docs.py --check",
            "scripts/run_fuzz.py smoke all",
            "scripts/check_conformance.py",
            "scripts/check_asciidoc.py",
            "scripts/check_roc_syntax.py --skip-fuzz --reference-performance smoke",
        ):
            self.assertIn(expected, commands)

    def test_ci_runs_check_through_blueprint(self) -> None:
        text = (WORKFLOWS / "ci.yml").read_text(encoding="utf-8")
        self.assertIn("  check:\n    name: check\n", text)
        self.assertIn("run: blueprint run check\n", text)


class ConformanceTests(unittest.TestCase):
    def test_the_fast_checks_are_the_default(self) -> None:
        self.assertEqual(
            check_conformance.select([]),
            ["yaml", "markdown-inline", "markdown-blocks", "xml", "json", "csv", "http"],
        )
        self.assertEqual(check_conformance.select(["all"]), list(check_conformance.REVIEWS))
        with self.assertRaises(ValueError):
            check_conformance.select(["all", "yaml"])

    def test_the_scheduled_matrix_lists_every_review(self) -> None:
        self.assertEqual(matrix("fuzz.yml", "review"), list(check_conformance.REVIEWS))

    def test_every_requirements_file_exists(self) -> None:
        for path in check_conformance.requirements_for(list(check_conformance.REVIEWS)):
            self.assertTrue((check_conformance.ROOT / path).is_file(), path)


class AsciiDocTests(unittest.TestCase):
    def test_the_probe_is_built_before_it_is_reviewed(self) -> None:
        steps = check_asciidoc.commands("roc", 200, 200, 1, True)
        self.assertEqual(steps[1][:2], ["roc", "build"])
        self.assertIn("--probe", steps[2])
        self.assertEqual(steps[2][steps[2].index("--generated") + 1], "200")
        self.assertEqual(len(check_asciidoc.commands("roc", 1, 1, 1, False)), 3)


if __name__ == "__main__":
    unittest.main()
