import importlib.util
import os
from pathlib import Path
import sys
import unittest

SCRIPTS = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('models', SCRIPTS / 'roc_syntax/model_cases.py')
models = importlib.util.module_from_spec(spec)
spec.loader.exec_module(models)
N = models.node


class IndependentModelTests(unittest.TestCase):
    def test_precedence_and_right_child_grouping(self):
        one, two, three = (N('number', str(x)) for x in (1, 2, 3))
        cases = [
            (N('binary', '+', one, N('binary', '*', two, three)), '1 + 2 * 3',
             ['binary', '+', ['number', '1'], ['binary', '*', ['number', '2'], ['number', '3']]]),
            (N('binary', '*', N('binary', '+', one, two), three), '(1 + 2) * 3',
             ['binary', '*', ['binary', '+', ['number', '1'], ['number', '2']], ['number', '3']]),
            (N('binary', '-', one, N('binary', '-', two, three)), '1 - (2 - 3)',
             ['binary', '-', ['number', '1'], ['binary', '-', ['number', '2'], ['number', '3']]]),
            (N('binary', '%', N('binary', '//', one, two), three), '1 // 2 % 3',
             ['binary', '%', ['binary', '//', ['number', '1'], ['number', '2']], ['number', '3']]),
        ]
        for model, source, expected in cases:
            with self.subTest(source=source):
                self.assertEqual(models.render(model), source)
                self.assertEqual(models.project(model), expected)

    def test_complete_file_independent_identity(self):
        file = N('file', (N('header', 'module', ('main',), ()),
                          N('import', 'Utility', 'Tools', ('value',)),
                          N('annotation', 'main', N('t_named', 'U8')),
                          N('decl', N('p_identifier', 'main'), N('number', '1'))))
        self.assertEqual(models.render(file), 'module [main]\nimport Utility as Tools exposing [value]\nmain : U8\nmain = 1\n')
        self.assertEqual(models.project(file), [
            ['header', 'module', [['exposed', ['identifier', 'main'], None]]],
            ['import', 'Utility', 'Tools', [['exposed', ['identifier', 'value'], None]]],
            ['annotation', 'main', ['t_named', ['identifier', 'U8']], None, False],
            ['decl', ['p_identifier', 'main'], ['number', '1']]])

    def test_determinism_and_domains(self):
        cases = models.generated_cases(1000, 91)
        self.assertEqual(cases, models.generated_cases(1000, 91))
        self.assertNotEqual(cases, models.generated_cases(1000, 92))
        self.assertEqual({c['mode'] for c in cases}, {'file', 'expr', 'statement'})
        self.assertEqual({c['rule'] for c in cases}, {'generated-files', 'generated-blocks', 'generated-types', 'generated-expressions'})
        shapes = str([c['model_shape'] for c in cases])
        for tag in ['opaque', 'nominal', 'alias', 't_fn', 't_apply', 't_union', 't_record',
                    'p_list', 'p_tuple', 'associated', 'where_method', 'expect', 'while', 'for', 'var', 'package', 'hosted', 'app', 'import']:
            self.assertIn("'" + tag + "'", shapes)

    def test_budgets_control_structure_and_reject_excessive_work(self):
        shallow = models.generated_cases(100, 91, expression_depth=0, pattern_depth=0, type_depth=0, statements=1)
        deep = models.generated_cases(100, 91, expression_depth=6, pattern_depth=4, type_depth=5, statements=6)
        self.assertGreater(sum(len(c['source']) for c in deep), 2 * sum(len(c['source']) for c in shallow))
        shapes = str([c['model_shape'] for c in shallow])
        for excluded in ('binary', 'lambda', 't_tuple', 'p_tuple', 'p_list'):
            self.assertNotIn("'" + excluded + "'", shapes)
        for kwargs in ({'expression_depth': 9}, {'pattern_depth': -1}, {'type_depth': 7}, {'statements': 0}):
            with self.assertRaises(ValueError):
                models.generated_cases(1, 1, **kwargs)

    @unittest.skipUnless(os.environ.get('ROC_SYNTAX_ORACLE'), 'set ROC_SYNTAX_ORACLE for native structural contract checks')
    def test_live_pinned_oracle_independent_models(self):
        spec = importlib.util.spec_from_file_location('review_models_test', SCRIPTS / 'review_roc_syntax.py')
        review = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(review)
        count = int(os.environ.get('ROC_SYNTAX_MODEL_CASES', '1000'))
        with review.Worker([os.environ['ROC_SYNTAX_ORACLE']]) as worker:
            for seed in (1, 91, 475):
                for case in models.generated_cases(count, seed):
                    actual = worker.request(case)
                    with self.subTest(id=case['id']):
                        self.assertTrue(actual['ok'], case['source'])
                        self.assertEqual(review.syntax_shape(actual, case['source']), case['model_shape'], case['source'])
