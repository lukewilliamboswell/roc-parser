import hashlib
import importlib.util
import json
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
REPRO = ROOT / 'scripts/roc_syntax/repros'
spec = importlib.util.spec_from_file_location('ownership_repro', REPRO / 'run_ownership_nesting.py')
repro = importlib.util.module_from_spec(spec)
spec.loader.exec_module(repro)


class OwnershipReproductionTests(unittest.TestCase):
    def test_independent_postorder_model_against_handwritten_trees(self):
        for depth, nodes, edges, checksum in [(0, 1, 0, 8), (1, 3, 2, 30), (2, 7, 6, 108)]:
            with self.subTest(depth=depth):
                self.assertEqual(repro.expected(depth), {'nodes': nodes, 'edges': edges, 'checksum': checksum})

    def test_retained_native_observations_match_exact_repro_sources(self):
        observed = json.loads((REPRO / 'ownership-observed.json').read_text())
        for label, report in observed.items():
            with self.subTest(report=label):
                self.assertIn(repro.PIN, report['compiler'])
                self.assertEqual(report['source_sha256'], hashlib.sha256((REPRO / report['source']).read_bytes()).hexdigest())
                self.assertEqual(report['runner_sha256'], hashlib.sha256((REPRO / 'run_ownership_nesting.py').read_bytes()).hexdigest())
                self.assertTrue(report['reproduced'])
                modes = set(range(8 if label == 'matrix' else 2))
                self.assertEqual({r['mode'] for r in report['records']}, modes)
                self.assertEqual({r['depth'] for r in report['records']}, {8, 10, 12})
                for record in report['records']:
                    expected = repro.expected(record['depth'])
                    self.assertEqual({key: record[key] for key in expected}, expected)
                for depth in (8, 10, 12):
                    records = {r['mode']: r for r in report['records'] if r['depth'] == depth}
                    bad, good = (3, 4) if label == 'matrix' else (0, 1)
                    self.assertGreater(records[bad]['allocations'], 4 * records[good]['allocations'])
