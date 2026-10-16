from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import unittest

from scripts import import_roc_syntax_snapshots, review_roc_syntax

ROOT = Path(__file__).resolve().parents[2]


class SyntaxPinTests(unittest.TestCase):
    def test_fixture_importer_supervisor_and_oracle_share_reference_pin(self):
        path = ROOT / "scripts/roc_syntax/build_oracle.py"
        spec = importlib.util.spec_from_file_location("pin_checked_build_oracle", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        fixture = json.loads((ROOT / "scripts/roc_syntax/fixtures.json").read_text())
        self.assertEqual(module.ROC_REVISION, fixture["reference_commit"])
        self.assertEqual(review_roc_syntax.REFERENCE_COMMIT, module.ROC_REVISION)
        self.assertEqual(import_roc_syntax_snapshots.REFERENCE_COMMIT, module.ROC_REVISION)
        self.assertEqual(len(module.ROC_REVISION), 40)
        self.assertEqual(fixture["version"], review_roc_syntax.PROTOCOL_VERSION)

    def test_ci_uses_the_oracle_toolchain_version(self):
        path = ROOT / "scripts/roc_syntax/build_oracle.py"
        spec = importlib.util.spec_from_file_location("ci_checked_build_oracle", path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        workflow = (ROOT / ".github/workflows/fuzz.yml").read_text()
        job = workflow.split("  roc-syntax:\n", 1)[1].split("  property-quality:\n", 1)[0]
        self.assertIn("version: " + module.ZIG_VERSION, job)
        self.assertIn("python3 scripts/check_roc_syntax.py", job)


if __name__ == "__main__":
    unittest.main()
