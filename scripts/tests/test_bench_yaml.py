import importlib.util
import unittest
from unittest.mock import patch
from types import SimpleNamespace
from pathlib import Path

spec = importlib.util.spec_from_file_location('bench_yaml', Path(__file__).resolve().parents[1] / 'bench_yaml.py')
bench = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bench)


class BenchmarkFixtures(unittest.TestCase):
    def test_repeated_values_equivalent(self):
        cases = bench.fixtures(3)
        self.assertEqual(cases['repeated_blocks'].count('abcdefgh'), 3)
        self.assertEqual(cases['repeated_quoted'].count('abcdefgh'), 3)
        self.assertIn('|-\n', cases['repeated_blocks'])

    def test_controlled_scaling_and_depth_cap(self):
        for name in ['mapping', 'sequence', 'quoted', 'literal', 'repeated_blocks']:
            self.assertGreater(len(bench.fixtures(128)[name]), len(bench.fixtures(16)[name]))
        self.assertEqual(bench.fixtures(512)['nested_flow'].count('['), 260)

    def test_malformed_case_fails_at_end(self):
        text = bench.fixtures(8)['invalid_late']
        self.assertTrue(text.endswith('broken: [1, 2\n'))
        self.assertEqual(text.count('key'), 8)


class TimingRecords(unittest.TestCase):
    def test_rejects_wrong_iteration_count(self):
        with patch.object(bench.subprocess, 'run', return_value=SimpleNamespace(stdout=b'{"iterations": 2, "elapsed_ns": 100}')):
            with self.assertRaises(ValueError):
                bench.run_case(Path('/fake'), b'a', 3, 2)

    def test_uses_stdin_and_preserves_raw_samples(self):
        response = b'{"iterations":3,"elapsed_ns":900,"successes":3,"checksum":10}'
        with patch.object(bench.subprocess, 'run', return_value=SimpleNamespace(stdout=response)) as run:
            result = bench.run_case(Path('/fake'), b'a\n', 3, 2)
            self.assertEqual(result['elapsed_ns'], 900)
            self.assertEqual(run.call_args.kwargs['input'], b'a\n')
            self.assertEqual(run.call_args.kwargs['timeout'], 2)
