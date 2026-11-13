from __future__ import annotations
import json
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch
import xml.etree.ElementTree as ET
from scripts import compare_markup_performance as compare
from scripts.markup.compare_python import element_weight


class MarkupComparisonTests(unittest.TestCase):
    def test_cold_first_parse_does_not_set_the_measured_batch_length(self):
        calls = []
        def synthetic_batch(implementation, data, count):
            # A cold first invocation is 100 times slower than sustained work.
            elapsed = 100_000 if not calls else count * 1_000
            calls.append(count)
            return {'iterations': count, 'successes': count, 'elapsed_ns': elapsed,
                    'checksum': 7 * count}
        with patch.object(compare, 'batch', side_effect=synthetic_batch):
            count, calibration = compare.calibrate({'name': 'synthetic'}, b'x', 7, 50)
        self.assertEqual(count, 50_000)
        self.assertEqual(calibration[-1]['elapsed_ns'], 50_000_000)
        self.assertEqual(calibration[-1]['iterations'], count)

    def test_calibration_requires_complete_consumption_on_every_warm_batch(self):
        def corrupt_warmup(implementation, data, count):
            return {'iterations': count, 'successes': count, 'elapsed_ns': 1_000,
                    'checksum': 7 if count == 1 else 0}
        with patch.object(compare, 'batch', side_effect=corrupt_warmup):
            with self.assertRaisesRegex(RuntimeError, 'warmup checksum changed'):
                compare.calibrate({'name': 'synthetic'}, b'x', 7, 50)

    def test_gate_uses_fastest_dom_across_languages_and_full_roc_pipeline(self):
        rows = [
            {'name': 'roc-full', 'eligible': False, 'median_ns': 200},
            {'name': 'roc-raw', 'eligible': False, 'median_ns': 20},
            {'name': 'rust-dom', 'eligible': True, 'median_ns': 100},
            {'name': 'python-c-dom', 'eligible': True, 'median_ns': 80},
            {'name': 'go-dom', 'eligible': True, 'median_ns': 300},
        ]
        result = compare.ratios(rows, 1.5)
        self.assertEqual(result['best'], 'python-c-dom')
        self.assertEqual(result['roc-full'], 2.5)
        self.assertEqual(result['roc-raw'], .25)
        self.assertGreater(result['roc-full'], result['threshold'])

    def test_fast_rejection_is_not_a_timing_success(self):
        output = {'iterations': 10, 'successes': 0, 'elapsed_ns': 1, 'checksum': 0}
        process = unittest.mock.Mock(returncode=0, stdout=json.dumps(output), stderr=b'')
        with patch.object(compare.subprocess, 'run', return_value=process):
            with self.assertRaisesRegex(RuntimeError, 'rejected workload'):
                compare.batch({'command': ['adapter'], 'name': 'bad'}, b'<svg/>', 10)

    def test_xml_common_cases_are_documents_with_nontrivial_consumed_trees(self):
        cases = compare.fixtures()
        self.assertEqual(len(cases), len({case['id'] for case in cases}))
        self.assertEqual({case['format'] for case in cases}, {'svg', 'mathml', 'html'})
        for case in cases:
            if case['mode'] == 'xml':
                root = ET.fromstring(case['input'])
                self.assertTrue(root.tag.startswith('{http://www.w3.org/'))
                self.assertGreater(element_weight(root), 100)

    def test_prose_extends_existing_common_cases_with_separate_text_paths(self):
        cases = {case['id']: case for case in compare.fixtures()}
        existing = {f'{kind}-{count}' for kind in ['svg-paths', 'mathml-fractions', 'html-cards']
                    for count in [8, 32, 128]}
        existing.update(['svg-drawing', 'mathml-formula', 'html-snippet', 'html-document'])
        self.assertTrue(existing.issubset(cases))
        for name in ['html-prose', 'html-prose-unicode', 'html-prose-entities']:
            self.assertGreater(len(cases[name]['input'].encode()), 20000)
            self.assertEqual(cases[name]['mode'], 'fragment')
        self.assertTrue(cases['html-prose']['input'].isascii())
        self.assertFalse(cases['html-prose-unicode']['input'].isascii())
        self.assertIn('&amp;', cases['html-prose-entities']['input'])

    def test_canonical_parity_retains_namespace_text_and_shape(self):
        tree = [['element', 'urn:a', 'g', [['', 'z', 'v'], ['', 'a', 'x']], [['text', '<path/>']]]]
        normalized = compare.normalize_tree(tree)
        self.assertEqual(normalized[0][3], [['', 'a', 'x'], ['', 'z', 'v']])
        for changed in [
            [['element', 'urn:b', 'g', tree[0][3], tree[0][4]]],
            [['element', 'urn:a', 'g', tree[0][3], [['element', 'urn:a', 'path', [], []]]]],
            [['element', 'urn:a', 'g', tree[0][3], [['text', 'different']]]],
        ]:
            self.assertNotEqual(normalized, compare.normalize_tree(changed))

    def test_canonical_mismatch_rejects_before_timing_even_with_same_checksum(self):
        expected = [['element', 'urn:a', 'svg', [], []]]
        changed = [['element', 'urn:b', 'svg', [], []]]
        self.assertEqual(compare.model_checksum(expected), compare.model_checksum(changed))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            (work / 'build.json').write_text(json.dumps({'source_sha256': 'source', 'binaries': {}}))
            args = SimpleNamespace(work=work, cases=[])
            adapters = [{'name': 'roc-full'}, {'name': 'python-etree'}]
            case = {'id': 'tiny', 'format': 'svg', 'mode': 'xml', 'input': '<svg/>'}
            with patch.object(compare, 'source_identity', return_value='source'), \
                 patch.object(compare, 'fixtures', return_value=[case]), \
                 patch.object(compare, 'implementations', return_value=adapters), \
                 patch.object(compare, 'canonical', side_effect=[expected, changed]), \
                 patch.object(compare, 'batch') as timing:
                with self.assertRaisesRegex(RuntimeError, 'canonical tree mismatch'):
                    compare.run(args)
                timing.assert_not_called()

    def test_ordinary_namespaced_xmlns_attribute_is_not_a_declaration(self):
        tree = [['element', 'urn:svg', 'svg', [
            ['http://www.w3.org/2000/xmlns/', 'x', 'urn:extension'],
            ['', 'xmlns', 'urn:svg'],
            ['urn:extension', 'xmlns', 'metadata'],
        ], []]]
        self.assertEqual(compare.normalize_tree(tree)[0][3], [['urn:extension', 'xmlns', 'metadata']])

    def test_changed_prepared_sources_cannot_run(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            (work / 'build.json').write_text(json.dumps({'source_sha256': 'old'}))
            with patch.object(compare, 'source_identity', return_value='new'), \
                 patch.object(compare, 'batch') as timing:
                with self.assertRaisesRegex(RuntimeError, 'stale source identity'):
                    compare.run(SimpleNamespace(work=work))
                timing.assert_not_called()


if __name__ == '__main__':
    unittest.main()
