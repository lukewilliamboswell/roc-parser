import importlib.util
from pathlib import Path
import unittest
import tempfile

spec=importlib.util.spec_from_file_location('bench_roc_syntax',Path(__file__).resolve().parents[1]/'bench_roc_syntax.py')
bench=importlib.util.module_from_spec(spec);spec.loader.exec_module(bench)

class ScalingTests(unittest.TestCase):
    def records(self,times):
        return [{'family':'operators','size':2**i,'bytes':2**i,'median_ns':n,'sha256':str(i),'accepted':True,'status':'Complete'} for i,n in enumerate(times)]

    def test_linear_growth_passes(self):
        self.assertEqual(bench.scaling_failures(self.records([10,20,40]),1.75),[])

    def test_quadratic_growth_fails(self):
        self.assertEqual(len(bench.scaling_failures(self.records([10,40,160]),1.75)),2)

    def test_load_noise_needs_consistent_slowdown_to_fail(self):
        records = self.records([10, 45])
        records[0]['samples_ns'] = [9, 10, 11, 10, 10]
        records[1]['samples_ns'] = [20, 45, 48, 50, 44]
        self.assertEqual(bench.scaling_failures(records, 1.75), [])
        records[1]['samples_ns'] = [40, 45, 48, 50, 44]
        self.assertEqual(len(bench.scaling_failures(records, 1.75)), 1)

    def test_valid_sources_cannot_pass_by_rejecting_or_limiting(self):
        for family in ['definitions', 'comment', 'operators', 'sibling-document']:
            for status, accepted in [('Limited', False), ('Recovered', False), ('Complete', False)]:
                with self.subTest(family=family, status=status, accepted=accepted):
                    record = {**self.records([1])[0], 'family': family, 'status': status, 'accepted': accepted}
                    failures = bench.scaling_failures([record], 1.75)
                    self.assertEqual([f['metric'] for f in failures], ['parse_outcome'])

    def test_only_incomplete_source_allows_recovery_or_limits(self):
        for status in ['Recovered', 'Limited']:
            record = {**self.records([1])[0], 'family': 'unclosed', 'status': status, 'accepted': False}
            self.assertEqual(bench.scaling_failures([record], 1.75), [])
        record = {**self.records([1])[0], 'family': 'unclosed'}
        self.assertEqual(bench.scaling_failures([record], 1.75)[0]['metric'], 'parse_outcome')

    def test_baseline_without_identical_source_overlap_fails(self):
        for change in [{'family': 'other'}, {'size': 100}, {'sha256': 'changed'}]:
            current = self.records([10])
            previous = [{**current[0], **change}]
            counts = {}
            failures = bench.baseline_failures(current, {'records': previous}, 1.5, comparison=counts)
            self.assertEqual(failures[0]['metric'], 'baseline_corpus_overlap')
            self.assertEqual(counts, {'matched': 0, 'skipped': 1, 'baseline_records': 1})

    def test_baseline_reports_partial_overlap(self):
        current = self.records([10, 20])
        counts = {}
        self.assertEqual(bench.baseline_failures(current, {'records': current[:1]}, 1.5, comparison=counts), [])
        self.assertEqual(counts, {'matched': 1, 'skipped': 1, 'baseline_records': 1})

    def test_source_identity_covers_private_buffers_and_driver(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / 'package').mkdir()
            (root / 'bench' / 'roc').mkdir(parents=True)
            parser = root / 'package' / 'RocSyntax.roc'
            buffer = root / 'package' / 'RocSyntaxBuffer.roc'
            driver = root / 'bench' / 'roc' / 'roc-syntax.roc'
            for path in [parser, buffer, driver]:
                path.write_text('original')
            initial = bench.source_identity(root)
            self.assertEqual(initial, bench.source_identity(root))
            for path in [buffer, driver]:
                path.write_text('changed')
                self.assertNotEqual(initial, bench.source_identity(root))
                path.write_text('original')
                self.assertEqual(initial, bench.source_identity(root))

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

    def test_baseline_cpu_model_core_count_and_build_configuration_checked(self):
        metadata={'compiler':'pinned','hardware':{'cpu_model':'Apple M1','logical_cpus':8,'strength':'model_and_core_count'},
                  'reused_binary':False,'build_flags':['--no-cache','--opt=speed']}
        bench.validate_baseline(metadata,metadata)
        different={**metadata,'hardware':{**metadata['hardware'],'cpu_model':'Apple M4'}}
        with self.assertRaisesRegex(RuntimeError,'CPU'):
            bench.validate_baseline(metadata,different)
        reused={**metadata,'reused_binary':True,'build_flags':None}
        with self.assertRaisesRegex(RuntimeError,'configuration'):
            bench.validate_baseline(reused,metadata)
        weak={**metadata,'hardware':{'strength':'weak','cpu_model':None}}
        with self.assertRaisesRegex(RuntimeError,'weak'):
            bench.validate_baseline(weak,weak)
