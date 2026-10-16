import importlib.util
from pathlib import Path
import unittest

spec=importlib.util.spec_from_file_location('bench_roc_syntax',Path(__file__).resolve().parents[1]/'bench_roc_syntax.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

class ScalingTests(unittest.TestCase):
    def records(self,times):
        return [{'family':'operators','size':2**i,'bytes':2**i,'median_ns':n,'sha256':str(i)} for i,n in enumerate(times)]

    def test_linear_growth_passes(self):
        self.assertEqual(bench.scaling_failures(self.records([10,20,40]),1.75),[])

    def test_quadratic_growth_fails(self):
        self.assertEqual(len(bench.scaling_failures(self.records([10,40,160]),1.75)),2)

    def test_baseline_regression_detected(self):
        baseline={'records':self.records([10,20])}
        self.assertEqual(len(bench.baseline_failures(self.records([20,40]),baseline,1.5)),2)

    def test_corpus_deterministic_and_contains_incomplete_source(self):
        first=list(bench.corpus((8,16)))
        self.assertEqual(first,list(bench.corpus((8,16))))
        self.assertTrue(any(f=='unclosed' for f,_,_ in first))
