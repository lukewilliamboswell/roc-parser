import json
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2]


class CoverageEvidenceTests(unittest.TestCase):
    def test_checklist_references_live_fixture_contracts(self):
        document = (ROOT / 'docs/roc-syntax-coverage.adoc').read_text()
        fixtures = json.loads((ROOT / 'scripts/roc_syntax/fixtures.json').read_text())
        cases = {case['id']: case for case in fixtures['cases']}
        self.assertIn(fixtures['reference_commit'], document)
        evidence = re.findall(r'fixture `([^`]+)` \(`([^`]+)`, `([^`]+)`\)', document)
        self.assertTrue(evidence, 'checklist must expose concrete regression evidence')
        for id, rule, coverage in evidence:
            with self.subTest(fixture=id):
                self.assertIn(id, cases)
                self.assertEqual((rule, coverage), (cases[id]['rule'], cases[id]['coverage']))
