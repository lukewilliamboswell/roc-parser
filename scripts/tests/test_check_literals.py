from __future__ import annotations

import json
import subprocess
import unittest

from scripts.check_literals import FIXTURES, assert_compiler_result


class LiteralContractTests(unittest.TestCase):
    def test_rejection_needs_failure_and_intended_message(self) -> None:
        intended = ["settings.yaml:2:3", "YAML", "legacy boolean"]
        assert_compiler_result(subprocess.CompletedProcess([], 1, "settings.yaml:2:3: YAML: legacy boolean", ""), intended)
        for result in (
            subprocess.CompletedProcess([], 0, "settings.yaml:2:3: YAML: legacy boolean", ""),
            subprocess.CompletedProcess([], 1, "missing import", ""),
            subprocess.CompletedProcess([], -11, "settings.yaml:2:3: YAML: legacy boolean", ""),
            subprocess.CompletedProcess([], 1, "panic: settings.yaml:2:3: YAML: legacy boolean", ""),
        ):
            with self.assertRaises(AssertionError):
                assert_compiler_result(result, intended)

    def test_positive_case_requires_success(self) -> None:
        assert_compiler_result(subprocess.CompletedProcess([], 0, "", ""), [])
        with self.assertRaises(AssertionError):
            assert_compiler_result(subprocess.CompletedProcess([], 1, "type mismatch", ""), [])

    def test_import_rejection_cannot_be_satisfied_by_a_source_excerpt(self) -> None:
        mismatch = subprocess.CompletedProcess(
            [], 1,
            'type mismatch\nvalue = Literal.require(Yaml.validate({source:text,name:"bad.yaml"}))', "",
        )
        with self.assertRaises(AssertionError):
            assert_compiler_result(mismatch, ["compile time crash", "bad.yaml", "YAML"])

    def test_all_negative_fixtures_require_the_compiler_rejection_marker(self) -> None:
        for case in json.loads(FIXTURES.read_text(encoding="utf-8")):
            errors = case.get("errors", [])
            if errors:
                self.assertTrue(
                    any(marker in errors for marker in ("invalid string", "compile time crash")),
                    case["name"],
                )


if __name__ == "__main__":
    unittest.main()
