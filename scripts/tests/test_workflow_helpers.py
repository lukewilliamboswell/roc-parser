from __future__ import annotations

import unittest

from scripts import workflow_helpers


class WorkflowHelpersTests(unittest.TestCase):
    def test_require_success(self) -> None:
        workflow_helpers.require_success(["success", "success"])
        with self.assertRaisesRegex(ValueError, "failure"):
            workflow_helpers.require_success(["success", "failure", "skipped"])

    def test_main_reports_a_failure(self) -> None:
        self.assertEqual(workflow_helpers.main(["require-success", "success"]), 0)
        self.assertEqual(workflow_helpers.main(["require-success", "cancelled"]), 1)


if __name__ == "__main__":
    unittest.main()
