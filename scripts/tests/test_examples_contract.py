"""Repository-owned inputs to the released example builder and test runner."""
from __future__ import annotations
import json
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2]


class ExamplesContractTests(unittest.TestCase):
    def test_spec_covers_every_application_once(self) -> None:
        suite = json.loads((ROOT / "tests/spec.json").read_text())
        self.assertEqual(suite["schema_version"], 1)
        self.assertEqual(suite["release"], "working-tree")
        self.assertEqual(suite["dependencies"], [])
        self.assertEqual([app["path"] for app in suite["apps"]],
                         sorted(str(path.relative_to(ROOT)) for path in (ROOT / "examples").glob("*.roc")))
        for app in suite["apps"]:
            with self.subTest(app=app["path"]):
                self.assertTrue(app["cases"])
                self.assertEqual(len({case["name"] for case in app["cases"]}), len(app["cases"]))
                for case in app["cases"]:
                    self.assertEqual(set(case), {"name", "args", "stdin", "stdout", "stderr", "exit_code"})
                    self.assertIs(type(case["exit_code"]), int)
                    self.assertTrue(all(isinstance(arg, str) for arg in case["args"]))
                    for field in ("name", "stdin", "stdout", "stderr"):
                        self.assertIsInstance(case[field], str)

    def test_examples_use_the_working_tree_package(self) -> None:
        for path in (ROOT / "examples").glob("*.roc"):
            with self.subTest(path=path.name):
                dependencies = re.findall(r'(?m)^\s*parser:\s*"([^"\n]+)"', path.read_text())
                self.assertEqual(dependencies, ["../package/main.roc"])

