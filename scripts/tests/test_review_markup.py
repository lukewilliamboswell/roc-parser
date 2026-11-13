from __future__ import annotations
import hashlib
import json
from pathlib import Path
import random
import tempfile
import unittest
from unittest.mock import patch
from scripts import review_markup as review


class XmlOracleTests(unittest.TestCase):
    def case(self, source, **kwargs):
        return {'id': 'x', 'format': 'svg', 'input': source, **kwargs}

    def test_expanded_names_exclude_namespace_bindings(self):
        result = review.xml_oracle(self.case('<s:svg xmlns:s="urn:a" xmlns:p="urn:b" p:x="v" plain="w"/>'))
        self.assertEqual(result['roots'], [['element', 'urn:a', 'svg', [['urn:b', 'x', 'v'], ['', 'plain', 'w']], []]])

    def test_fragment_context_and_explicit_undeclaration(self):
        result = review.xml_oracle(self.case('<g/><x xmlns=""/>', fragment=True))
        self.assertEqual([n[1] for n in result['roots']], [review.NS['svg'], ''])

    def test_comments_processing_instructions_and_merged_cdata(self):
        result = review.xml_oracle(self.case('<!--before--><?t d?><svg>a<![CDATA[b]]>c<!--x-->d</svg>'))
        self.assertEqual(result['roots'][:2], [['comment', 'before'], ['pi', 't', 'd']])
        self.assertEqual(result['roots'][2][4], [['text', 'abc'], ['comment', 'x'], ['text', 'd']])

    def test_xml_normalization_preserves_reference_whitespace(self):
        result = review.xml_oracle(self.case('<svg a="x\t&#9;\r\ny">x\r\ny</svg>'))
        self.assertEqual(result['roots'][0][3], [['', 'a', 'x \t y']])
        self.assertEqual(result['roots'][0][4], [['text', 'x\ny']])

    def test_rejects_doctype_duplicate_expanded_names_and_unknown_entity(self):
        for source in ['<!DOCTYPE svg><svg/>', '<svg xmlns:a="u" xmlns:b="u" a:x="1" b:x="2"/>', '<svg>&unknown;</svg>']:
            self.assertEqual(review.xml_oracle(self.case(source))['status'], 'error')


class ComparisonTests(unittest.TestCase):
    case = {'id': 'fixture', 'format': 'html', 'input': '<p>x</p>'}

    def test_wrong_tree_is_failure(self):
        actual = {'status': 'ok', 'roots': [['text', 'x']]}
        expected = {'status': 'ok', 'roots': [['text', 'y']]}
        self.assertEqual(review.compare(self.case, actual, expected)['kind'], 'wrong_tree')

    def test_crash_is_not_a_valid_rejection(self):
        row = review.compare(self.case, {'status': 'crash'}, {'status': 'error'})
        self.assertEqual(row['kind'], 'crash')

    def test_expected_problem_is_checked(self):
        row = review.compare({**self.case, 'problem': 'UnknownElement'}, {'status': 'error', 'error': {'code': 'WrongRoot'}}, {'status': 'error'})
        self.assertEqual(row['kind'], 'wrong_diagnostic')

    def test_strict_html_rejects_parse_error_but_recovery_keeps_tree(self):
        oracle = unittest.mock.Mock()
        oracle.parse.return_value = {'status': 'ok', 'roots': [], 'diagnostics': [{'code': 'duplicate-attribute'}]}
        self.assertEqual(review.expected(self.case, oracle)['status'], 'error')
        self.assertEqual(review.expected({**self.case, 'recovery': 'browser'}, oracle)['status'], 'ok')

    def test_semantic_verdict_does_not_follow_browser_acceptance(self):
        oracle = unittest.mock.Mock()
        oracle.parse.return_value = {'status': 'ok', 'roots': [], 'diagnostics': []}
        self.assertEqual(review.expected({**self.case, 'valid': False}, oracle)['status'], 'error')

    def test_protocol_rejects_unknown_nodes_and_malformed_attributes(self):
        for tree in [[['unexpected', 'x']], [['element', '', 'x', [['x']], []]], [['text', 1]]]:
            with self.assertRaises(ValueError):
                review.validate_tree(tree)

    def test_generated_and_mutated_cases_are_reproducible(self):
        a = list(review.generated(42, 100))
        self.assertEqual(a, list(review.generated(42, 100)))
        self.assertEqual(set(c['format'] for c in a), {'html', 'svg', 'mathml'})
        self.assertEqual(review.mutate(a[0], random.Random(7), 0), review.mutate(a[0], random.Random(7), 0))

    def test_reproducer_keeps_exact_source_and_options(self):
        case = {**self.case, 'input': '\x00é\r\n<broken', 'fragment': True, 'recovery': 'browser'}
        row = review.compare(case, {'status': 'crash'}, {'status': 'ok', 'roots': []})
        with tempfile.TemporaryDirectory() as directory:
            review.retain(row, Path(directory))
            saved = json.loads(next((Path(directory) / 'reproducers').glob('*.json')).read_text())
        self.assertEqual(saved['case'], case)
        self.assertEqual(saved['input_sha256'], hashlib.sha256(case['input'].encode()).hexdigest())


class SupplierTests(unittest.TestCase):
    def test_digest_mismatch_never_replaces_cached_artifact(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'artifact'
            path.write_bytes(b'original')
            import io
            with patch.object(review.setup.urllib.request, 'urlopen', return_value=io.BytesIO(b'wrong')):
                with self.assertRaisesRegex(ValueError, 'digest mismatch'):
                    review.setup.fetch_verified('https://example.invalid/supplier', '0' * 64, path)
            self.assertEqual(path.read_bytes(), b'original')

    def test_verified_cache_needs_no_network(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'artifact'
            path.write_bytes(b'bytes')
            with patch.object(review.setup.urllib.request, 'urlopen') as network:
                self.assertEqual(review.setup.fetch_verified('url', hashlib.sha256(b'bytes').hexdigest(), path), path)
                network.assert_not_called()

    def test_all_fixtures_have_distinct_ids_and_explicit_semantic_claims(self):
        cases = review.load_cases(list(review.NS))
        self.assertEqual(len(cases), len({c['id'] for c in cases}))
        self.assertTrue(any(c.get('problem') for c in cases))
        for case in cases:
            if case.get('valid') is False:
                self.assertTrue(case.get('problem'), case['id'])
