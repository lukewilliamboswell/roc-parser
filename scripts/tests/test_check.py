from __future__ import annotations

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]

WORKFLOWS = ROOT / ".github" / "workflows"


def matrix(workflow: str, key: str) -> list[str]:
    """The one-per-line `key:` matrix list of a workflow."""
    text = (WORKFLOWS / workflow).read_text(encoding="utf-8")
    block = text.split(f"        {key}:\n", 1)[1].split("    steps:", 1)[0]
    return [line.strip()[2:] for line in block.splitlines() if line.strip().startswith("- ")]


class CheckTests(unittest.TestCase):
    def test_check_covers_what_pull_requests_ran_before_ci_yml(self) -> None:
        # Independent coverage guard: every pre-migration check must remain in
        # the Roc plan. Behavior and selection tests live beside that plan.
        source = (ROOT / "scripts/src/CheckPlan.roc").read_text()
        commands = {" ".join(re.findall(r'"([^"\n]+)"', command)[1:])
                    for command in re.findall(r'\["(?:python3|roc-stable)"[^\[\]]*\]', source)}
        self.assertIn('["scripts/test_package.roc"]', source)
        self.assertIn('["release-package", "test-examples"]', source)
        self.assertIn('["scripts/check_doc_examples.roc"]', source)
        self.assertIn('["scripts/check_conformance.roc", "--"]', source)
        self.assertIn('["scripts/check_asciidoc.roc", "--"]', source)
        for expected in (
            "-m unittest discover -s scripts/tests -p test_*.py",
            "scripts/run_fuzz.py smoke all",
            "scripts/check_roc_syntax.py --skip-fuzz --reference-performance smoke",
        ):
            self.assertIn(expected, commands)

    def test_ci_runs_check_through_blueprint(self) -> None:
        text = (WORKFLOWS / "ci.yml").read_text(encoding="utf-8")
        self.assertIn("  check:\n    name: check\n", text)
        self.assertIn("run: blueprint run check\n", text)


class ConformanceTests(unittest.TestCase):
    def test_the_scheduled_matrix_lists_every_review(self) -> None:
        source = (ROOT / "scripts/src/HarnessPlan.roc").read_text()
        self.assertEqual(matrix("fuzz.yml", "review"), re.findall(r'name: "([^"\n]+)"', source))

    def test_every_requirements_file_exists(self) -> None:
        source = (ROOT / "scripts/src/HarnessPlan.roc").read_text()
        paths = re.findall(r'"(scripts/[^"\n]+/requirements.txt)"', source)
        self.assertEqual(len(paths), 3)
        for path in paths:
            self.assertTrue((ROOT / path).is_file(), path)
