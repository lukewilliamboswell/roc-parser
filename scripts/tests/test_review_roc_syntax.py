import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('review_roc_syntax', SCRIPTS / 'review_roc_syntax.py')
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)

class WorkerTests(unittest.TestCase):
    def worker(self, folder, body):
        path = Path(folder) / 'worker.py'
        path.write_text('import json,sys,time\n' + body)
        return [sys.executable, '-u', str(path)]

    def test_multiple_requests_use_one_process(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'for line in sys.stdin:\n r=json.loads(line); print(json.dumps({"version":1,"id":r["id"],"pid":__import__("os").getpid()}),flush=True)\n')
            with review.Worker(command) as worker:
                first = worker.request({'id': 'one', 'source':'multi\nline'})
                second = worker.request({'id':'two','source':'λ'})
                self.assertEqual(first['pid'], second['pid'])
                self.assertEqual(worker.requests, 2)

    def test_crash_is_reported(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'sys.exit(9)\n')
            with review.Worker(command) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'exited'):
                    worker.request({'id':'crash'})

    def test_timeout_terminates_worker(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'time.sleep(5)\n')
            with review.Worker(command, timeout=.05) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'timeout'):
                    worker.request({'id':'slow'})
            self.assertIsNotNone(worker.proc.poll())

    def test_wrong_id_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'print(json.dumps({"version":1,"id":"wrong"}),flush=True)\n')
            with review.Worker(command) as worker:
                with self.assertRaisesRegex(review.WorkerError, 'ID'):
                    worker.request({'id':'correct'})

    def test_crash_case_saved_and_later_cases_continue(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'for line in sys.stdin:\n r=json.loads(line)\n if r["source"]=="crash": sys.exit(7)\n print(json.dumps({"version":1,"id":r["id"],"ok":True,"consumed":True,"tokens":[{"kind":"EndOfFile","start":0,"end":0}],"shape":[]}),flush=True)\n')
            cases = [{'id':'bad','mode':'expr','source':'crash'}, {'id':'good','mode':'expr','source':''}]
            records = review.run_cases(cases, command, command, Path(tmp)/'failures')
            self.assertFalse(records[0]['pass'])
            self.assertTrue(records[1]['pass'])
            self.assertEqual(len(list((Path(tmp)/'failures').glob('*.json'))), 1)

    def test_mismatch_retained_and_request_replayable(self):
        with tempfile.TemporaryDirectory() as tmp:
            command = self.worker(tmp, 'for line in sys.stdin:\n r=json.loads(line); print(json.dumps({"version":1,"id":r["id"],"ok":True,"consumed":True,"tokens":[{"kind":"EndOfFile","start":0,"end":0}],"shape":["wrong"]}),flush=True)\n')
            case = {'id':'deliberate/mismatch','source':'','mode':'expr','model_shape':[['number','1']]}
            result = review.run_cases([case], command, command, Path(tmp)/'failures')
            self.assertFalse(result[0]['pass'])
            saved = json.loads(next((Path(tmp)/'failures').glob('*.json')).read_text())
            self.assertEqual(saved['request']['source'], '')
            self.assertEqual(saved['model_shape'], case['model_shape'])
            self.assertTrue(any(d['field']=='actual_model_shape' for d in saved['differences']))

class ProjectionTests(unittest.TestCase):
    def test_fixture_loader_accepts_direct_header_statement_and_repl_modes(self):
        document = {'version': review.PROTOCOL_VERSION, 'reference_commit': review.REFERENCE_COMMIT,
                    'coverage': ['modes'], 'cases': [
                        {'id': mode, 'mode': mode, 'rule': 'modes', 'coverage': 'supported', 'source': ''}
                        for mode in ('header', 'statement', 'repl')
                    ]}
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'fixtures.json'
            path.write_text(json.dumps(document))
            self.assertEqual(len(review.load_cases(path)[1]), 3)

    def test_file_retains_import_structure(self):
        response = {'nodes': [
            {'kind': 'ImportPath', 'start': 7, 'end': 10, 'children_start': 0, 'children_len': 0},
            {'kind': 'Import', 'start': 0, 'end': 10, 'children_start': 0, 'children_len': 1},
            {'kind': 'File', 'start': 0, 'end': 10, 'children_start': 1, 'children_len': 1},
        ], 'children': [0, 1], 'root': 2}
        self.assertEqual(review.syntax_shape(response, 'import Foo'), [['import', 'Foo', None, []]])

    def test_declaration_projection_preserves_binding(self):
        response = {'nodes': [
            {'kind': 'IdentifierPattern', 'start': 0, 'end': 1, 'children_start': 0, 'children_len': 0},
            {'kind': 'NumberLiteral', 'start': 2, 'end': 3, 'children_start': 0, 'children_len': 0},
            {'kind': 'Declaration', 'start': 0, 'end': 3, 'children_start': 0, 'children_len': 2},
        ], 'children': [0, 1], 'root': 2}
        self.assertEqual(review.syntax_shape(response, 'x=1'), ['decl', ['p_identifier', 'x'], ['number', '1']])

    def test_interpolation_and_typed_literal_project_children(self):
        response = {'nodes': [
            {'kind': 'Identifier', 'start': 3, 'end': 4, 'children_start': 0, 'children_len': 0},
            {'kind': 'Interpolation', 'start': 1, 'end': 5, 'children_start': 0, 'children_len': 1},
            {'kind': 'StringLiteral', 'start': 0, 'end': 6, 'children_start': 1, 'children_len': 1},
        ], 'children': [0, 1], 'root': 2}
        self.assertEqual(review.syntax_shape(response, '"${x}"'), ['interpolated_string', [['interpolation', ['identifier', 'x']]]])
        response = {'nodes': [
            {'kind': 'NumberLiteral', 'start': 0, 'end': 3, 'children_start': 0, 'children_len': 0},
            {'kind': 'Identifier', 'start': 4, 'end': 7, 'children_start': 0, 'children_len': 0},
            {'kind': 'TypedLiteral', 'start': 0, 'end': 7, 'children_start': 0, 'children_len': 2},
        ], 'children': [0, 1], 'root': 2}
        self.assertEqual(review.syntax_shape(response, '123.U64'), ['typed', ['number', '123'], ['U64']])

    def test_constructor_pattern_preserves_explicit_empty_arguments(self):
        for source, called in [('Some', False), ('Some()', True)]:
            response = {'nodes': [
                {'kind': 'Identifier', 'start': 0, 'end': 4, 'children_start': 0, 'children_len': 0},
                {'kind': 'TagPattern', 'start': 0, 'end': len(source), 'children_start': 0, 'children_len': 1},
            ], 'children': [0], 'root': 1}
            self.assertEqual(review.syntax_shape(response, source), ['p_tag', ['identifier', 'Some'], called, []])

    def test_character_and_suffix_nodes_project_structure(self):
        character = {'nodes': [{'kind': 'CharacterLiteral', 'start': 0, 'end': 4,
                                'children_start': 0, 'children_len': 0}],
                     'children': [], 'root': 0}
        self.assertEqual(review.syntax_shape(character, "'é'"), ['character', "'é'"])
        suffix = {'nodes': [
            {'kind': 'Identifier', 'start': 0, 'end': 1, 'children_start': 0, 'children_len': 0},
            {'kind': 'Suffix', 'start': 0, 'end': 2, 'children_start': 0, 'children_len': 1},
        ], 'children': [0], 'root': 1}
        self.assertEqual(review.syntax_shape(suffix, 'x?'), ['suffix', '?', ['identifier', 'x']])

    def test_control_and_optional_field_nodes_project_structure(self):
        for kind, keyword in [('Debug', 'dbg'), ('Crash', 'crash'), ('Return', 'return')]:
            source = keyword + ' 1'
            response = {'nodes': [
                {'kind': 'NumberLiteral', 'start': len(keyword) + 1, 'end': len(source), 'children_start': 0, 'children_len': 0},
                {'kind': kind, 'start': 0, 'end': len(source), 'children_start': 0, 'children_len': 1},
            ], 'children': [0], 'root': 1}
            self.assertEqual(review.syntax_shape(response, source), [keyword, ['number', '1']])
        for kind, source, expected in [('Break', 'break', ['break']), ('Ellipsis', '...', ['ellipsis'])]:
            response = {'nodes': [{'kind': kind, 'start': 0, 'end': len(source), 'children_start': 0, 'children_len': 0}], 'children': [], 'root': 0}
            self.assertEqual(review.syntax_shape(response, source), expected)
        response = {'nodes': [
            {'kind': 'Identifier', 'start': 0, 'end': 1, 'children_start': 0, 'children_len': 0},
            {'kind': 'Identifier', 'start': 3, 'end': 8, 'children_start': 0, 'children_len': 0},
            {'kind': 'OptionalFieldAccess', 'start': 0, 'end': 8, 'children_start': 0, 'children_len': 2},
        ], 'children': [0, 1], 'root': 2}
        self.assertEqual(review.syntax_shape(response, 'x.?field'), ['optional_field', ['identifier', 'x'], 'field'])

    def test_optional_field_suffix_and_discard_token_classes(self):
        response = {'tokens': [
            {'kind': 'NoSpaceDotQuestionLowerIdent', 'start': 1, 'end': 8},
            {'kind': 'NoSpaceOpQuestion', 'start': 8, 'end': 9},
        ]}
        self.assertEqual(review.normalize_tokens(response, 'x.?field?'), [
            ['Punctuation', 1, 3], ['LowerIdent', 3, 8], ['Operator', 8, 9],
        ])
        for kind in ('Underscore', 'NamedUnderscore'):
            self.assertEqual(review.normalize_tokens({'tokens': [{'kind': kind, 'start': 0, 'end': 1}]}, '_'), [['LowerIdent', 0, 1]])

    def test_tuple_index_tokens_preserve_dot_and_original_index_span(self):
        for kind in ('DotInt', 'NoSpaceDotInt'):
            with self.subTest(kind=kind):
                reference = {'tokens': [{'kind': kind, 'start': 2, 'end': 5}]}
                candidate = {'tokens': [
                    {'kind': 'Punctuation', 'start': 2, 'end': 3},
                    {'kind': 'Number', 'start': 3, 'end': 5},
                ]}
                self.assertEqual(review.normalize_tokens(reference, 'x .01'),
                                 [['Punctuation', 2, 3], ['Number', 3, 5]])
                self.assertEqual(review.normalize_tokens(reference, 'x .01'),
                                 review.normalize_tokens(candidate, 'x .01'))

    def test_precedence_disagreement_detected(self):
        first = {'ok':True,'consumed':True,'tokens':[], 'shape':['binary','+',1,['binary','*',2,3]]}
        second = {**first,'shape':['binary','*',['binary','+',1,2],3]}
        self.assertTrue(any(d['field']=='shape' for d in review.compare({'source':'1+2*3'},first,second)))

    def test_empty_string_part_is_not_a_lexical_difference(self):
        empty = {'tokens': [{'kind': 'StringPart', 'start': 1, 'end': 1}]}
        self.assertEqual(review.normalize_tokens(empty, '""'), [])
        content = {'tokens': [{'kind': 'StringPart', 'start': 1, 'end': 2}]}
        self.assertEqual(review.normalize_tokens(content, '"x"'), [['StringPart', 1, 2]])

    def test_oracle_empty_string_part_is_valid_but_candidate_must_progress(self):
        reference = {'tokens': [
            {'kind': 'StringStart', 'start': 0, 'end': 1},
            {'kind': 'StringPart', 'start': 1, 'end': 1},
            {'kind': 'StringEnd', 'start': 1, 'end': 2},
            {'kind': 'EndOfFile', 'start': 2, 'end': 2},
        ]}
        review.validate_spans(reference, '""')
        with self.assertRaisesRegex(review.WorkerError, 'no progress'):
            review.validate_spans(reference, '""', lossless=True)

    def test_call_adjacency_preserved(self):
        first={'tokens':[{'kind':'NoSpaceOpenRound','start':1,'end':2}]}
        second={'tokens':[{'kind':'OpenRound','start':1,'end':2}]}
        self.assertNotEqual(review.normalize_tokens(first,'f('),review.normalize_tokens(second,'f('))

    def test_recovery_tree_placement_is_not_language_mismatch(self):
        actual = {'ok':False,'consumed':True,'tokens':[{'kind':'Number','start':0,'end':1},{'kind':'EndOfFile','start':1,'end':1}], 'shape':['partial']}
        expected = {'ok':False,'consumed':True,'tokens':[{'kind':'EndOfFile','start':0,'end':0}], 'shape':[]}
        self.assertEqual(review.compare({'source':'1','coverage':'recovery'},actual,expected),[])
        expected['consumed'] = False
        self.assertEqual(review.compare({'source':'1','coverage':'recovery'},actual,expected),[])
        self.assertTrue(review.compare({'source':'1','coverage':'recovery','expect_consumed':False},actual,expected))
        expected['ok'] = True
        self.assertTrue(any(d['field']=='recovery_acceptance' for d in review.compare({'source':'1','coverage':'recovery'},actual,expected)))

    def test_unsupported_gap_requires_status_and_detects_stale_inventory(self):
        actual = {'status':'Unsupported','ok':False,'tokens':[{'kind':'EndOfFile','start':0,'end':0}], 'shape':[]}
        expected = {'ok':True,'tokens':[{'kind':'EndOfFile','start':0,'end':0}], 'shape':[]}
        case = {'source':'','coverage':'unsupported'}
        self.assertEqual(review.compare(case,actual,expected),[])
        actual['status'] = 'Complete'
        self.assertEqual(review.compare(case,actual,expected)[0]['field'],'unsupported_status')

    def test_matching_unknown_shapes_are_projection_gap_not_agreement(self):
        response = {'ok':True,'consumed':True,'tokens':[{'kind':'EndOfFile','start':0,'end':0}], 'shape':['unsupported','future_syntax']}
        differences = review.compare({'source':'','coverage':'supported'},response,response)
        self.assertEqual([d['field'] for d in differences],['projection_gap'])

    def test_exploratory_unsupported_is_coverage_gap_not_false_rejection(self):
        actual = {'status':'Unsupported','ok':False,'tokens':[{'kind':'EndOfFile','start':0,'end':0}], 'shape':[]}
        expected = {'ok':True,'tokens':[], 'shape':[]}
        differences = review.compare({'source':'','coverage':'exploratory'},actual,expected)
        self.assertEqual([d['field'] for d in differences],['coverage_gap'])

    def test_generated_models_reproducible_and_varied(self):
        first=review.generated_cases(100,7)
        self.assertEqual(first,review.generated_cases(100,7))
        self.assertGreater(len({case['source'] for case in first}),80)
        self.assertTrue(any('binary' in str(c['model_shape']) for c in first))

    def test_generated_models_cross_nest_all_expression_families(self):
        cases = review.generated_cases(1000, 91)
        families = {'number', 'identifier', 'string', 'binary', 'call', 'list',
                    'tuple', 'lambda', 'if', 'record', 'field'}
        observed, nested = set(), set()
        def visit(value, depth=0):
            if not isinstance(value, list):
                return
            if value and isinstance(value[0], str) and value[0] in families:
                observed.add(value[0])
                if depth > 1:
                    nested.add(value[0])
            for child in value:
                visit(child, depth + 1)
        for case in cases:
            visit(case['model_shape'])
            self.assertLess(len(case['source'].encode()), 4096)
        self.assertEqual(observed, families)
        self.assertEqual(nested, families)

    def test_fixture_rule_inventory_checked(self):
        metadata,cases=review.load_cases(review.DATA/'fixtures.json')
        self.assertEqual(set(metadata['coverage']),{c['rule'] for c in cases})
