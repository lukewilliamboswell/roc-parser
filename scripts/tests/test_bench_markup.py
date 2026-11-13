from __future__ import annotations
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
from scripts import bench_markup as bench


class WorkloadTests(unittest.TestCase):
    def test_workloads_cover_the_distinct_parser_costs(self):
        records = list(bench.workloads([32, 64]))
        self.assertEqual({r[0] for r in records}, {'svg-siblings', 'svg-long-text', 'svg-path-commands', 'mathml-fractions', 'html-siblings', 'html-attributes', 'html-adoption', 'lossless-source', 'xml-nesting', 'hole-planning', 'typed-binding'})
        plans = [row for row in records if row[0] == 'hole-planning']
        self.assertEqual([len(row[3].split('\0')) - 1 for row in plans], [64, 128])
        for _, _, size, source in records:
            self.assertGreater(len(source.encode()), size)

    def test_identity_changes_when_a_parser_or_measurement_boundary_changes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for path in ['package/X.roc', 'bench/roc/markup.roc', 'fuzz/markup-alloc.roc', 'scripts/bench_markup.py']:
                file = root / path
                file.parent.mkdir(parents=True, exist_ok=True)
                file.write_text('initial')
            with patch.object(bench, 'ROOT', root):
                before = bench.source_identity()
                (root / 'package/X.roc').write_text('changed')
                self.assertNotEqual(before, bench.source_identity())


class ScalingTests(unittest.TestCase):
    def rows(self, factors):
        return [{'family': 'work', 'size': size, 'bytes': size, 'samples_ns': [time], 'median_ns': time} for size, time in factors]

    def test_linear_growth_is_accepted(self):
        self.assertEqual(bench.scaling(self.rows([(32, 100), (64, 200), (128, 400)])), [])

    def test_quadratic_growth_is_not_accepted_as_linear(self):
        failures = bench.scaling(self.rows([(32, 100), (64, 400), (128, 1600)]))
        self.assertEqual([f['size'] for f in failures], [64, 128])

    def test_noise_is_retained_without_claiming_a_regression(self):
        records = self.rows([(32, 100), (64, 400)])
        records[0]['samples_ns'] = [100, 200]
        records[1]['samples_ns'] = [200, 400]
        self.assertEqual(bench.scaling(records), [])

    def test_failed_parse_is_not_a_successful_timing(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'driver'
            path.write_text('#!/usr/bin/python3\nimport json,sys\nn=int(sys.argv[2])\nprint(json.dumps({"iterations":n,"elapsed_ns":10,"successes":0,"checksum":0}))\n')
            path.chmod(0o755)
            with self.assertRaisesRegex(RuntimeError, 'rejected'):
                bench.sample(path, 'svg', b'<g/>', 1, 1)
