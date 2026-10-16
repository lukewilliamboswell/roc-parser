"""Tests for the local performance gate, independent of host timing."""
import importlib.util
import pathlib
import sys
import argparse
import json
import shlex
import tempfile
import unittest
from unittest import mock
from types import SimpleNamespace

ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'scripts'))
spec = importlib.util.spec_from_file_location('adoc_performance', ROOT / 'scripts/check_asciidoc_performance.py')
perf = importlib.util.module_from_spec(spec)
spec.loader.exec_module(perf)


class Performance(unittest.TestCase):
    def test_ceiling_cannot_be_relaxed(self):
        with self.assertRaisesRegex(SystemExit, "1.0x ceiling"):
            perf.main(["--max-ratio", "1.01"])

    def test_default_ratio_is_one(self):
        self.assertEqual(perf.parser().parse_args([]).max_ratio, 1.0)

    def test_worker_request_uses_remaining_deadline(self):
        clock = [9.75]
        worker = mock.Mock()
        with mock.patch.object(perf, 'Worker', return_value=worker), mock.patch.object(perf.time, 'monotonic', side_effect=lambda: clock[0]):
            bounded = perf.DeadlineWorker(['worker'], timeout=30, deadline=10)
            bounded.request({})
            self.assertEqual(worker.timeout, 0.25)
            worker.request.side_effect = lambda _: clock.__setitem__(0, 10.1)
            with self.assertRaisesRegex(TimeoutError, 'full-run deadline'):
                bounded.request({})

    def test_final_row_overrun_fails_and_retains_replay_provenance(self):
        clock = [0.0]
        environments = []
        reply = {'oracle_version': '2.0.23', 'oracle_code_sha256': 'a'*64,
                 'include_io': 'caller-map-memory', 'include_path_metadata': {}, 'projection': {}, 'html': ''}
        class FakeWorker:
            def __init__(self, command, timeout, env=None):
                environments.append(env)
            def request(self, value):
                return reply
            def close(self):
                pass
        measured = perf.distribution([100,100,100],1,1)
        def finish_after_deadline(*args, **kwargs):
            clock[0] = 10.1
            return [measured,measured], [{'qualification': {}}]
        with tempfile.TemporaryDirectory() as tmp, \
             mock.patch.dict(perf.os.environ, {'RUBYOPT':'ambient options', 'RUBYLIB':'ambient library', 'GEM_HOME':'ambient gems'}), \
             mock.patch.object(perf.time, 'monotonic', side_effect=lambda: clock[0]), \
             mock.patch.object(perf.platform, 'platform', return_value='test OS'), \
             mock.patch.object(perf.platform, 'processor', return_value='test CPU'), \
             mock.patch.multiple(perf,
                 Worker=FakeWorker,
                 build=mock.Mock(return_value=(pathlib.Path('/fake-worker'), {'source_identity':'same','compiler':{'sha256':'compiler-hash'}})),
                 digest=mock.Mock(return_value='a'*64), source_identity=mock.Mock(return_value='same'),
                 corpus=mock.Mock(return_value=[{'id':'last','family':'test','source':'hello'}]),
                 validate_reply=mock.Mock(side_effect=lambda value,*_: value), equivalent=mock.Mock(),
                 calibrate=mock.Mock(return_value=1), sample_pair=finish_after_deadline), \
             mock.patch.object(perf.subprocess, 'run', return_value=SimpleNamespace(stdout='Ruby test')):
            output=pathlib.Path(tmp)
            self.assertEqual(perf.main(['--smoke','--modes','render','--deadline-seconds','10','--output',tmp]),1)
            report=json.loads((output/'results.json').read_text())
            self.assertEqual(len(report['records']),1)
            self.assertEqual(report['records'][0]['status'],'error')
            self.assertIn('full-run deadline',report['records'][0]['error'])
            retained=json.loads(next((output/'retained').glob('*.json')).read_text())
            self.assertIn('--replay',retained['measurement']['replay'])
            self.assertEqual(retained['run']['candidate_build']['compiler']['sha256'],'compiler-hash')
            self.assertEqual(retained['run']['reference']['version'],'2.0.23')
            self.assertIsNone(environments[0])
            self.assertNotIn('RUBYOPT',environments[1])
            self.assertNotIn('RUBYLIB',environments[1])
            self.assertEqual(environments[1]['GEM_HOME'],str(perf.parser().parse_args([]).gem_home.resolve()))
            self.assertEqual(environments[1]['TZ'],'UTC')
            self.assertEqual(perf.os.environ['GEM_HOME'],'ambient gems')

    def test_default_oracle_home_matches_the_local_installer(self):
        from asciidoc.setup_oracle import DEFAULT_HOME as installed_home
        self.assertEqual(perf.parser().parse_args([]).gem_home, installed_home)
        self.assertEqual(perf.parser().parse_args(['--gem-home', '/tmp/custom-oracle']).gem_home,
                         pathlib.Path('/tmp/custom-oracle'))

    def test_ratchet_has_no_floor_or_aggregate_exception(self):
        faster = perf.distribution([900,900,900], 10, 1)
        reference = perf.distribution([1000,1000,1000], 10, 1)
        slower = perf.distribution([1001,1001,1001], 10, 1)
        self.assertFalse(perf.ratchet(faster, reference)['regression'])
        self.assertFalse(perf.ratchet(reference, reference)['regression'])
        self.assertTrue(perf.ratchet(slower, reference)['regression'])

    def test_distribution_per_operation_and_noise(self):
        measured = perf.distribution([1000,2000,3000], 10, 100)
        self.assertEqual(measured['median_ns'],200)
        self.assertEqual(measured['samples_ns'],[100,200,300])
        self.assertTrue(perf.ratchet(measured,measured)['noisy'])

    def test_zero_clock_resolution_rejected(self):
        with self.assertRaises(ValueError): perf.distribution([0],1,1)

    def test_generated_models_and_mutations_reproduce(self):
        a=list(perf.generated(719,100))
        self.assertEqual(a,list(perf.generated(719,100)))
        self.assertNotEqual(a,list(perf.generated(720,100)))
        self.assertEqual(len(a),125)
        self.assertTrue(any(case['family']=='raw-mutation' for case in a))
        self.assertEqual(len({case['id'] for case in a}),len(a))

    def test_retained_inputs_distinguish_virtual_source_environments(self):
        first = {'id': 'one', 'source': 'include::part.adoc[]',
                 'includes': {'part.adoc': 'first', 'other.adoc': 'same'}}
        reordered = {**first, 'id': 'two', 'includes': dict(reversed(list(first['includes'].items())))}
        changed = {**first, 'includes': {**first['includes'], 'part.adoc': 'second'}}
        self.assertEqual(perf.case_identity(first), perf.case_identity(reordered))
        self.assertNotEqual(perf.case_identity(first), perf.case_identity(changed))
        self.assertNotEqual(perf.case_identity({'id': 'x', 'source': ''}),
                            perf.case_identity({'id': 'x', 'source': '', 'includes': {}}))

    def test_geometric_scaling_uses_actual_bytes(self):
        def row(size,time):
            return {'case':{'id':str(size),'family':'many','source':'a'*size,'provenance':{'geometric':True}},'mode':'parse','candidate':{'median_ns':time}}
        self.assertFalse(perf.scaling([row(100,100),row(200,200)])[0]['regression'])
        self.assertTrue(perf.scaling([row(100,100),row(200,400)])[0]['regression'])
        self.assertAlmostEqual(perf.scaling([row(100,100),row(200,400)])[0]['exponent'],2)

    def test_depth_scaling_times_legal_inputs(self):
        deep=[case for case in perf.geometric([32,64,128,256,512]) if case['family'] in ('nested-lists','nested-containers')]
        self.assertEqual(len(deep),10)
        self.assertEqual(max(case['size'] for case in deep),32)
        self.assertEqual({case['size'] for case in deep},{2,4,8,16,32})

    def test_default_scaling_sources_fit_the_parser_input_limit(self):
        for case in perf.geometric(perf.parser().parse_args([]).sizes):
            with self.subTest(case=case['id']):
                self.assertLessEqual(len(case['source'].encode()), 262144)

    def test_geometric_modes_are_independent(self):
        rows=[]
        for mode in ['parse','render']:
            for size in [32,64]:
                rows.append({'case':{'id':str(size),'family':'f','source':'a'*size,'provenance':{'geometric':True}},'mode':mode,'candidate':{'median_ns':size}})
        self.assertEqual(len(perf.scaling(rows)),2)

    def test_projection_mismatch_rejected_before_timing(self):
        with self.assertRaises(ValueError):
            perf.equivalent({'projection':{'title':'','blocks':[]},'html':''},{'projection':{'title':'other','blocks':[]},'html':''})

    def test_dom_mismatch_is_not_hidden_by_projection(self):
        with self.assertRaises(ValueError):
            perf.equivalent({'projection':{'title':'','blocks':[]},'html':'<p>a</p>'},{'projection':{'title':'','blocks':[]},'html':'<p>b</p>'})

    def test_dom_ignores_only_interblock_whitespace(self):
        self.assertEqual(perf.canonical_html('<div>\n<p>a</p>\n</div>'),perf.canonical_html('<div><p>a</p></div>'))
        self.assertNotEqual(perf.canonical_html('<pre>a\nb</pre>'),perf.canonical_html('<pre>a b</pre>'))

    def test_protocol_wrong_count_rejected(self):
        case={'id':'x','source':'text'}
        with self.assertRaises(ValueError): perf.validate_reply({'version':1,'id':'x','iterations':3,'successes':2},case,3)

    def test_calibration_too_short_is_explicit(self):
        class Worker:
            def request(self,request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':1,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        with self.assertRaisesRegex(ValueError,'minimum measurement window'):
            perf.calibrate(Worker(),{'id':'x','source':'x'},'parse',10000,8,expected)

    def test_common_batches_enforce_actual_sample_duration(self):
        class Worker:
            def request(self,request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*100,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        values,attempts=perf.sample_pair([Worker(),Worker()],{'id':'x','source':'x'},'parse',1,1000,3,100,expected)
        self.assertEqual(len(attempts),2)
        self.assertEqual(values[0]['iterations'],values[1]['iterations'])
        self.assertGreaterEqual(values[0]['observed_minimum_window_ns'],1000)

    def test_clear_headroom_screen_does_not_force_huge_reference_batch(self):
        class Worker:
            def __init__(self,period):self.period=period
            def request(self,request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        measured,attempts=perf.sample_pair([Worker(10),Worker(1000)],{'id':'x','source':'x'},'parse',10,10000,5,10000,expected,screen=True,clock_floor_ns=100)
        self.assertEqual(measured[0]['iterations'],10)
        self.assertEqual(attempts[-1]['qualification'],'stable-headroom-screen')
        self.assertLess(measured[0]['observed_minimum_window_ns'],10000)
        self.assertLessEqual(attempts[-1]['ratio_interval'][1],1.0)

    def test_screen_cannot_hide_slower_candidate(self):
        class Worker:
            def __init__(self,period):self.period=period
            def request(self,request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        measured,attempts=perf.sample_pair([Worker(110),Worker(100)],{'id':'x','source':'x'},'parse',10,10000,5,10000,expected,screen=True,clock_floor_ns=100)
        self.assertEqual(attempts[-1]['qualification'],'both-duration-floor')
        self.assertGreaterEqual(min(item['observed_minimum_window_ns']for item in measured),10000)
        self.assertTrue(perf.ratchet(*measured)['regression'])

    def test_noisy_duration_qualified_batch_refines_until_stable(self):
        class Worker:
            def __init__(self,stable_period):self.calls=0;self.stable_period=stable_period
            def request(self,request):
                self.calls+=1
                period=(1000 if self.calls%2 else 3000) if request['iterations']<20 else self.stable_period
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*period,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        values,attempts=perf.sample_pair([Worker(1000),Worker(2000)],{'id':'x','source':'x'},'parse',10,10000,3,1000,expected)
        self.assertEqual([item['qualification']for item in attempts],['refine-noise','both-duration-floor'])
        self.assertEqual([item['iterations']for item in attempts],[10,20])
        self.assertFalse(perf.ratchet(*values)['noisy'])
        self.assertEqual(perf.ratchet(*values)['ratio'],0.5)

    def test_persistent_noise_remains_failure_after_bounded_refinement(self):
        class Worker:
            def __init__(self):self.calls=0
            def request(self,request):
                self.calls+=1
                period=1000 if self.calls%2 else 3000
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*period,'checksum':request['iterations'],'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        values,attempts=perf.sample_pair([Worker(),Worker()],{'id':'x','source':'x'},'parse',10,10000,3,1000,expected)
        self.assertEqual(len(attempts),4)
        self.assertEqual(attempts[-1]['qualification'],'both-duration-floor-noisy')
        self.assertTrue(perf.ratchet(*values)['noisy'])
        self.assertEqual([item['iterations']for item in attempts],[10,20,40,80])
        self.assertLessEqual(values[0]['iterations'],1000)

    def test_prefix_families_retain_trailing_syntax(self):
        cases={case['family']:case for case in perf.geometric([8])}
        self.assertTrue(cases['long-prefix-strong']['source'].endswith('*strong*'))
        self.assertTrue(cases['long-prefix-link']['source'].endswith('link:https://example.test[label]'))
        self.assertTrue(cases['long-prefix-url']['source'].endswith('https://example.test'))

    def test_stateful_and_plain_punctuation_scaling_families(self):
        cases={case['family']:case for case in perf.geometric([4,8]) if case['size']==8}
        self.assertEqual(cases['many-sections']['source'].count('== Section'),8)
        self.assertEqual(cases['many-footnotes']['source'].count('footnote:['),8)
        self.assertEqual(cases['many-crossrefs']['source'].count('See <<target'),8)
        self.assertIn('[[target7]]',cases['many-crossrefs']['source'])
        self.assertEqual(cases['attribute-redefinitions']['source'].count(':name:'),8)
        self.assertEqual(cases['attribute-redefinitions']['source'].count('{name}'),8)
        self.assertEqual(cases['plain-punctuation-adversary']['source'],'( . - = '*8)

    def test_scaling_replay_retains_both_inputs(self):
        cases=[{'id':'small','source':'x'},{'id':'large','source':'xx'}]
        with tempfile.TemporaryDirectory() as tmp:
            path=pathlib.Path(tmp)/'pair.json'
            path.write_text(json.dumps({'cases':cases}))
            self.assertEqual(list(perf.corpus(argparse.Namespace(replay=[path]))),cases)

    def test_replay_preserves_measurement_confidence_and_limits(self):
        args=perf.parser().parse_args(['--window-ms','50','--samples','7','--max-ratio','0.9',
              '--max-scaling-exponent','1.2','--max-iterations','12345','--timeout','41',
              '--ruby','/opt/ruby path/bin/ruby','--gem-home','/tmp/gem home'])
        tokens=shlex.split(perf.replay_command(args,pathlib.Path('/tmp/slow case.json'),['parse']))
        replay=perf.parser().parse_args(tokens[2:])
        for key in ('window_ms','samples','max_ratio','max_scaling_exponent','max_iterations',
                    'timeout','build_timeout','deadline_seconds','ruby','roc','gem_home'):
            self.assertEqual(getattr(args,key),getattr(replay,key),key)
        self.assertEqual(replay.replay,[pathlib.Path('/tmp/slow case.json')])
        self.assertEqual(replay.modes,['parse'])

    def test_full_campaign_prioritizes_stateful_geometry_without_dropping_inputs(self):
        args=perf.parser().parse_args(['--generated','8','--sizes','32','64'])
        args.fixtures=[]
        cases=list(perf.corpus(args))
        geometry=list(perf.geometric(args.sizes))
        generated=list(perf.generated(args.seed,args.generated))
        self.assertEqual(cases,geometry+generated)
        self.assertEqual([case['family']for case in geometry[:10:2]],
                         ['many-sections','many-footnotes','many-crossrefs','attribute-redefinitions','plain-punctuation-adversary'])
        self.assertEqual(len({case['id']for case in cases}),len(cases))

    def test_smoke_exercises_both_native_include_selectors(self):
        cases=list(perf.corpus(perf.parser().parse_args(['--smoke'])))
        self.assertEqual(len(cases),2)
        included=next(case for case in cases if 'includes' in case)
        self.assertIn('[lines=2..4]',included['source'])
        self.assertIn('[tag=chosen]',included['source'])
        self.assertIn('// tag::chosen[]',included['includes']['part.adoc'])

    def test_request_explicit_empty_map_differs_secure_default(self):
        self.assertFalse(perf.request({'id':'x','source':'x'},'parse',1)['expand'])
        self.assertTrue(perf.request({'id':'x','source':'x','includes':{}},'parse',1)['expand'])

if __name__ == '__main__': unittest.main()
