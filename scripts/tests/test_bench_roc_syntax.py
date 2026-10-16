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

    def test_allocation_baseline_regression_fails_with_stable_timing(self):
        previous=self.records([10])[0]; previous['allocation_calls']=100
        current={**previous,'allocation_calls':126}
        failures=bench.baseline_failures([current],{'records':[previous]},1.5)
        self.assertEqual([f['metric'] for f in failures],['allocation_calls'])
        self.assertEqual(bench.baseline_failures([current],{'records':[previous]},1.5,1.3),[])

    def test_zero_allocation_baseline_detects_first_allocation(self):
        previous=self.records([10])[0]; previous['allocation_calls']=0
        current={**previous,'allocation_calls':1}
        failures=bench.baseline_failures([current],{'records':[previous]},1.5)
        self.assertEqual(failures[0]['metric'],'allocation_calls')
        self.assertIsNone(failures[0]['ratio'])
