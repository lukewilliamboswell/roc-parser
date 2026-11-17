from __future__ import annotations

import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]

WORKFLOWS = ROOT / ".github" / "workflows"


class CheckTests(unittest.TestCase):
    def test_check_covers_native_validation_and_mocked_harness_tests(self) -> None:
        source = (ROOT / "scripts/src/CheckPlan.roc").read_text()
        commands = {" ".join(re.findall(r'"([^"\n]+)"', command)[1:])
                    for command in re.findall(r'\["(?:python3|roc-stable)"[^\[\]]*\]', source)}
        self.assertIn('["scripts/test_package.roc"]', source)
        self.assertIn('["release-package", "test-examples"]', source)
        self.assertIn('["scripts/check_doc_examples.roc"]', source)
        for expected in (
            "-m unittest discover -s scripts/tests -p test_*.py",
            "scripts/run_fuzz.py smoke --ci all",
            "scripts/bench.py --quick --langs roc --timeout 60 --fail-on-error",
        ):
            self.assertIn(expected, commands)

    def test_ci_runs_check_through_blueprint(self) -> None:
        text = (WORKFLOWS / "ci.yml").read_text(encoding="utf-8")
        self.assertIn("  check:\n    name: check\n", text)
        self.assertIn("run: blueprint run check\n", text)


class ConformanceTests(unittest.TestCase):
    def test_local_conformance_task_keeps_the_review_registry(self) -> None:
        source = (ROOT / "scripts/src/HarnessPlan.roc").read_text()
        self.assertTrue(re.findall(r'name: "([^"\n]+)"', source))
        blueprint = (ROOT / "Blueprint.roc").read_text()
        self.assertIn('Task("conformance", [Use("package"), Run(["scripts/check_conformance.roc", "--"])])', blueprint)

    def test_every_requirements_file_exists(self) -> None:
        source = (ROOT / "scripts/src/HarnessPlan.roc").read_text()
        paths = re.findall(r'"(scripts/[^"\n]+/requirements.txt)"', source)
        self.assertEqual(len(paths), 5)
        for path in paths:
            self.assertTrue((ROOT / path).is_file(), path)
