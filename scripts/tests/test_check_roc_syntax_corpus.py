import importlib.util
from pathlib import Path
import sys
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(SCRIPTS))
spec = importlib.util.spec_from_file_location('corpus_gate', SCRIPTS / 'check_roc_syntax_corpus.py')
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


class CorpusGateTests(unittest.TestCase):
    def response(self, ok=True, **changes):
        return {'ok': ok, 'consumed': True, 'status': 'Complete' if ok else 'Recovered',
                'tokens': [{'kind': 'EndOfFile', 'start': 0, 'end': 0}], **changes}

    def test_missing_grammar_is_failure_even_when_explicitly_unsupported(self):
        for status in ['Recovered', 'Unsupported', 'Limited']:
            actual = self.response(False, status=status)
            self.assertEqual(gate.outcome({'source': ''}, actual, self.response())[0], 'missing_grammar')

    def test_invalid_or_obsolete_input_requires_rejection(self):
        self.assertEqual(gate.outcome({'source': ''}, self.response(), self.response(False))[0], 'false_acceptance')
        self.assertEqual(gate.outcome({'source': ''}, self.response(False), self.response(False))[0], 'rejected')

    def test_unknowns_are_distinct_from_rejection(self):
        for actual in [{'worker_error': 'crash'}, self.response(ok=1), self.response(tokens=[])]:
            self.assertEqual(gate.outcome({'source': ''}, actual, self.response(False))[0], 'unknown')

    def test_acceptance_requires_complete_consumption(self):
        self.assertEqual(gate.outcome({'source': ''}, self.response(consumed=False), self.response())[0], 'missing_grammar')
        self.assertEqual(gate.outcome({'source': ''}, self.response(), self.response(consumed=False))[0], 'unknown')

    def test_acceptance_agreement_cannot_hide_a_wrong_tree(self):
        actual = self.response(shape=[['binary', '+', ['number', '1'], ['number', '2']]])
        expected = self.response(shape=[['binary', '*', ['number', '1'], ['number', '2']]])
        classification, differences = gate.outcome({'source': ''}, actual, expected)
        self.assertEqual(classification, 'structure_mismatch')
        self.assertEqual([d['field'] for d in differences], ['shape'])

    def test_matching_unknown_projections_are_unknown_not_passing(self):
        response = self.response(shape=[['unsupported', 'future']])
        classification, differences = gate.outcome({'source': ''}, response, response)
        self.assertEqual(classification, 'unknown')
        self.assertEqual(differences[0]['field'], 'projection_gap')

    def test_unhandled_imports_cannot_pass_empty_inventory(self):
        doc = {'cases': [], 'provenance': {'skipped': [{'path': 'x', 'reason': 'unsupported_snapshot_mode:repl'}]}}
        report = gate.run_corpus(doc, [], [])
        self.assertFalse(report['passed'])
        self.assertEqual(report['counts']['unknown_imports'], 1)
