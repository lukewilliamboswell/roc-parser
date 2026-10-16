from copy import deepcopy
import unittest

from scripts import check_roc_syntax_recovery as recovery


class RecoveryContracts(unittest.TestCase):
    def setUp(self):
        self.source = 'bad = )\nafter = 42\n'
        self.case = {'source': self.source, 'damaged': [0, 7], 'survivors': [
            {'start': 8, 'end': 18, 'shape': ['decl', ['p_identifier', 'after'], ['number', '42']]}]}
        self.response = {'ok': False, 'root': 3,
                         'tokens': [{'kind': 'Invalid', 'start': 0, 'end': 19},
                                    {'kind': 'EndOfFile', 'start': 19, 'end': 19}],
                         'diagnostics': [{'kind': 'UnexpectedToken', 'start': 6, 'end': 7}],
                         'children': [0, 1, 2], 'nodes': [
                             {'kind': 'IdentifierPattern', 'start': 8, 'end': 13, 'children_start': 0, 'children_len': 0},
                             {'kind': 'NumberLiteral', 'start': 16, 'end': 18, 'children_start': 0, 'children_len': 0},
                             {'kind': 'Declaration', 'start': 8, 'end': 18, 'children_start': 0, 'children_len': 2},
                             {'kind': 'File', 'start': 0, 'end': 19, 'children_start': 2, 'children_len': 1}]}

    def test_healthy_declaration_and_local_diagnostic_pass(self):
        self.assertEqual(recovery.check(self.case, self.response), [])

    def test_absorbed_declaration_fails_even_with_valid_spans(self):
        response = deepcopy(self.response)
        response['nodes'][-1]['children_len'] = 0
        self.assertEqual(recovery.check(self.case, response)[0]['field'], 'surviving_declaration')

    def test_changed_tree_fails_even_with_same_span_and_rejection(self):
        response = deepcopy(self.response)
        response['nodes'][2]['kind'] = 'Call'
        self.assertEqual(recovery.check(self.case, response)[0]['field'], 'surviving_shape')

    def test_diagnostic_on_healthy_declaration_fails(self):
        response = deepcopy(self.response)
        response['diagnostics'].append({'kind': 'UnexpectedToken', 'start': 8, 'end': 13})
        self.assertEqual(recovery.check(self.case, response)[0]['field'], 'diagnostic_locality')

    def test_false_acceptance_fails(self):
        response = deepcopy(self.response)
        response['ok'] = True
        self.assertEqual(recovery.check(self.case, response)[0]['field'], 'malformed_rejection')

    def test_generated_contracts_cover_unicode_offsets_and_damage(self):
        cases = recovery.cases()
        self.assertEqual(len(cases), 18)
        self.assertEqual(len({c['id'] for c in cases}), len(cases))
        for case in cases:
            raw = case['source'].encode()
            start, end = case['damaged']
            self.assertTrue(raw[start:end].startswith(b'bad = '))
            for survivor in case['survivors']:
                self.assertTrue(raw[survivor['start']:survivor['end']].startswith(
                    survivor['shape'][1][1].encode() + b' = '))


if __name__ == '__main__':
    unittest.main()
