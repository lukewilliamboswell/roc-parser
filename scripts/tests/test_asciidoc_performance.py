"""Tests for the local performance gate, independent of host timing."""
import importlib.util
import pathlib
import sys
import argparse
import json
import hashlib
import io
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
    def test_measured_regressions_reproduce_independent_source_fingerprints(self):
        expected = {
            'scaling-header-first-section-quote-counters-body-128': '8944138aa4830d6b33eda97c68d59978ae6572e55fa9571b571762c389e45b76',
            'scaling-header-first-section-quote-counters-body-256': '8567780e7690c6ef6ec1c0950cd3f74f9ef39a5b1ed7978e6667ca57a0ccc82d',
            'scaling-header-first-section-quote-counters-body-512': '5ec9d7e463607569e353177ac65d4d8001ff672d2d226fc7e08c9c9536addc81',
            'scaling-header-first-section-mutating-previews-body-128': 'cbef0b021e476b780a0d6d38675e7fee8994477f231c75a40cae4130665a7f0c',
            'scaling-header-first-section-mutating-previews-body-256': 'd51b06bdf3c2be13e8feec2bf10f44506626f8fe19dc77e1a403ac5a5d8be2dc',
            'scaling-header-first-section-mutating-previews-body-512': '32d0966410cdfbc694ea070db81dd5aa2c9e7f94dbd0b37e82b0af223954187f',
            'scaling-source-store-colliding-attribute-names-128': '72303b287f9094ad8dee9c5e1a2a31ab585f593f635ed91ba018a23ee3f58a13',
            'scaling-source-store-colliding-attribute-names-256': '0a89107a2e0e7aa1d395f105b68c62803995ed5cd9de281b074a05027a78cccf',
            'scaling-source-store-colliding-attribute-names-512': '258855eb62c1cb3a938b4e6decaaabb0a1e0d44f5ed5bdc922d8fea40b4d9d46',
        }
        cases = list(perf.measured_regressions([32,64,128,256,512,1024]))
        self.assertEqual({c['id']:hashlib.sha256(c['source'].encode()).hexdigest() for c in cases},expected)
        self.assertEqual([c['size'] for c in cases],[128,256,512]*3)
        self.assertTrue(all(c['provenance']['geometric'] for c in cases))
        self.assertTrue(all(len(c['source'].encode()) <= 262144 for c in cases))
        self.assertEqual(list(perf.measured_regressions([32,64,1024])),[])
        self.assertEqual([c['size'] for c in perf.measured_regressions([256])],[256]*3)

    def test_collision_regressions_use_distinct_valid_names_with_exact_hash_collisions(self):
        for case in perf.measured_regressions([128,256,512]):
            if case['family'] != 'source-store-colliding-attribute-names':
                self.assertTrue(case['source'].endswith('\n\nBody.'))
                self.assertEqual(case['expect'],{'html_contains':['<p>Body.</p>']})
                continue
            names=[line.split(':')[1] for line in case['source'].split('\n\n')[0].splitlines()]
            self.assertEqual(len(set(names)),case['size'])
            hashes=[]
            for name in names:
                self.assertRegex(name,r'^[a-z][a-z0-9]+$')
                value=5381
                for byte in name.encode():
                    value=(value*33+byte)%4294967296
                hashes.append(value)
            self.assertEqual(set(hashes),{case['provenance']['name_hash']})
            self.assertTrue(all(len(name)==case['provenance']['name_length'] for name in names))

    def test_measured_regressions_append_after_historical_inputs_and_keep_scaling_pairs(self):
        args=perf.parser().parse_args(['--generated','2','--sizes','128','256','512'])
        args.fixtures=[]
        old=list(perf.geometric(args.sizes))+list(perf.generated(args.seed,args.generated))
        added=list(perf.measured_regressions(args.sizes))
        self.assertEqual(list(perf.corpus(args)),old+added+list(perf.fixed_performance_guards(args)))
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render') for case in added]
        pairs=perf.scaling(rows,1.3)
        self.assertEqual(len(pairs),12)
        self.assertTrue(all(not pair['regression'] for pair in pairs))

    def test_request_scaling_measures_canonical_semantic_payload(self):
        case={'id':'options','source':'Body é.', 'includes':{'z.adoc':'z','a.adoc':'a'},
              'attributes':[{'name':'x','value':'first'},{'name':'x','value':'last'}],
              'scaling_input':'request', 'scaling_input_bytes':1, 'size':999999}
        payload={'source':'Body é.','includes':[{'path':'a.adoc','source':'a'},{'path':'z.adoc','source':'z'}],
                 'expand':True,'attributes':case['attributes']}
        expected=len(json.dumps(payload,sort_keys=True,ensure_ascii=False,separators=(',',':')).encode())
        self.assertEqual(perf.scaling_measure(case),{'scaling_input':'request','scaling_input_bytes':expected})
        self.assertEqual(perf.scaling_measure({**case,'id':'renamed'}),perf.scaling_measure(case))
        plain={key:value for key,value in case.items() if key not in ('scaling_input','scaling_input_bytes','size')}
        self.assertEqual(perf.case_identity(case),perf.case_identity(plain))
        for mode in ('parse','render'):
            self.assertEqual(perf.request(case,mode,7,2),perf.request(plain,mode,7,2))
        empty={'id':'empty','source':'','scaling_input':'request'}
        self.assertEqual(perf.scaling_measure(empty),perf.scaling_measure({**empty,'attributes':[]}))

    def test_constant_source_growing_ordered_options_gets_request_scaling_gate(self):
        rows=[]
        for size in (8,16,32):
            case={'id':str(size),'family':'options-only','source':'', 'scaling_input':'request',
                  'attributes':[{'name':f'key{i}','value':f'value{i}'} for i in range(size)],
                  'provenance':{'geometric':True}}
            length=perf.scaling_measure(case)['scaling_input_bytes']
            rows.append({'case':case,'mode':'render','candidate':{'median_ns':length*length}})
        checks=perf.scaling(rows)
        self.assertEqual(len(checks),2)
        self.assertTrue(all(c['regression'] and abs(c['exponent']-2)<1e-10 for c in checks))
        self.assertTrue(all(c['scaling_input']=='request' and c['from_input_bytes']<c['to_input_bytes'] for c in checks))
        retained={'cases':[rows[0]['case'],rows[1]['case']],'scaling':checks[0]}
        with tempfile.TemporaryDirectory() as tmp:
            path=pathlib.Path(tmp)/'options-pair.json'
            path.write_text(json.dumps(retained))
            self.assertEqual(list(perf.corpus(argparse.Namespace(replay=[path]))),retained['cases'])
        source_rows=[{**r,'case':{**r['case'],'source':'Body.','scaling_input':'source'}} for r in rows]
        self.assertEqual(perf.scaling(source_rows),[])

    def test_scaling_metric_rejects_invalid_values_and_mixed_families(self):
        case={'id':'x','family':'f','source':'Body.','provenance':{'geometric':True}}
        for invalid in (None,True,False,0,1,[],{},'', 'bytes','REQUEST'):
            with self.subTest(invalid=invalid),self.assertRaisesRegex(ValueError,'scaling_input'):
                perf.scaling_measure({**case,'scaling_input':invalid})
        rows=[{'case':case,'mode':'parse','candidate':{'median_ns':1}},
              {'case':{**case,'id':'y','scaling_input':'request'},'mode':'parse','candidate':{'median_ns':2}}]
        with self.assertRaisesRegex(ValueError,'mixed scaling_input'):
            perf.scaling(rows)
        rows[1]['mode']='render'
        with self.assertRaisesRegex(ValueError,'mixed scaling_input'):
            perf.scaling(rows)

    def test_default_source_scaling_retains_utf8_growth_and_threshold(self):
        cases=[{'id':str(i),'family':'legacy','source':'é'*i,'provenance':{'geometric':True}} for i in (2,4)]
        rows=[{'case':case,'mode':'parse','candidate':{'median_ns':time}} for case,time in zip(cases,(10,30))]
        check=perf.scaling(rows)[0]
        self.assertEqual((check['scaling_input'],check['from_input_bytes'],check['to_input_bytes']),('source',4,8))
        self.assertEqual(check['byte_growth'],2)
        self.assertEqual(check['time_growth'],3)
        self.assertEqual(check['max_exponent'],1.3)
        self.assertTrue(check['regression'])
        self.assertEqual(perf.scaling_measure(cases[0]),perf.scaling_measure({**cases[0],'scaling_input':'source'}))
        self.assertEqual(perf.parser().parse_args([]).max_ratio,1.0)
        self.assertEqual(perf.parser().parse_args([]).max_scaling_exponent,1.3)

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
                 'include_io': 'caller-map-memory', 'include_path_metadata': {}, 'projection': {'title':'','blocks':[]}, 'html': '', 'version':1}
        class FakeWorker:
            def __init__(self, command, timeout, env=None):
                environments.append(env)
            def request(self, value):
                return {**reply, 'id':value['id'], 'iterations':value['iterations'], 'successes':value['iterations'], 'checksum':value['iterations'], 'elapsed_ns':1}
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
                 calibrate=mock.Mock(return_value=1), sample_pair=finish_after_deadline), \
             mock.patch.object(perf.subprocess, 'run', return_value=SimpleNamespace(stdout='Ruby test')):
            output=pathlib.Path(tmp)
            self.assertEqual(perf.main(['--smoke','--batch-policy','common','--modes','render','--deadline-seconds','10','--output',tmp]),1)
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

    def test_optional_attributes_preserve_original_request_and_identity(self):
        case = {'id': 'old', 'source': 'Text'}
        original = {'id': 'old', 'source': 'Text', 'includes': [],
                    'expand': False, 'mode': 'parse', 'iterations': 3, 'warmup': 2}
        self.assertEqual(perf.request(case, 'parse', 3, 2), original)
        self.assertEqual(perf.request({**case, 'attributes': []}, 'parse', 3, 2), original)
        self.assertEqual(perf.case_identity(case), perf.case_identity({**case, 'attributes': []}))
        import hashlib
        payload = {'source': 'Text', 'includes': [], 'expand': False}
        original_hash = hashlib.sha256(json.dumps(payload, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()
        self.assertEqual(perf.case_identity(case), original_hash)

    def test_ordered_attribute_options_are_part_of_request_identity(self):
        attributes = [{'name': 'project', 'value': 'first'}, {'name': 'project', 'value': 'last'}]
        case = {'id': 'options', 'source': '{project}', 'attributes': attributes}
        self.assertEqual(perf.request(case, 'render', 0)['attributes'], attributes)
        self.assertNotEqual(perf.case_identity(case), perf.case_identity({**case, 'attributes': list(reversed(attributes))}))
        self.assertNotEqual(perf.case_identity(case), perf.case_identity({**case, 'attributes': attributes[-1:]}))
        self.assertNotEqual(perf.case_identity(case), perf.case_identity({**case, 'attributes': []}))

    def test_attribute_options_replay_preserves_order_and_provenance(self):
        case = {'id': 'options', 'source': '{project}', 'attributes': [{'name': 'project', 'value': 'Roc'}],
                'provenance': {'generator': 'options-test', 'parameter': 2}}
        with tempfile.TemporaryDirectory() as tmp:
            replay = pathlib.Path(tmp) / 'replay.json'
            replay.write_text(json.dumps({'cases': [case]}))
            replayed = list(perf.corpus(perf.parser().parse_args(['--replay', str(replay)])))
        self.assertEqual(replayed, [case])
        self.assertEqual(perf.case_identity(replayed[0]), perf.case_identity(case))

    def test_invalid_attribute_options_rejected_before_worker_request(self):
        for invalid in (None, {}, ['x'], [{'name': 'x'}], [{'name': 'x', 'value': 4}], [{'name': 'x', 'value': '', 'extra': ''}]):
            with self.subTest(invalid=invalid), self.assertRaisesRegex(ValueError, 'ordered list'):
                perf.request({'id': 'bad', 'source': '', 'attributes': invalid}, 'parse', 0)

    @unittest.skipUnless(__import__('os').environ.get('ROC_ASCIIDOC_PERFORMANCE_BINARY'), 'set ROC_ASCIIDOC_PERFORMANCE_BINARY for live protocol checks')
    def test_native_optional_attribute_protocol(self):
        import os
        import subprocess
        binary = os.environ['ROC_ASCIIDOC_PERFORMANCE_BINARY']
        original = perf.request({'id': 'legacy', 'source': 'Text "attributes".'}, 'render', 0)
        def run(value, escaped=False):
            raw = json.dumps(value)
            if escaped:
                raw = raw.replace('"attributes":', '"attr\\u0069butes":')
            result = subprocess.run([binary], input=raw+'\n', text=True, capture_output=True, timeout=30, check=True)
            return json.loads(result.stdout)
        self.assertNotIn('error', run(original))
        self.assertNotIn('error', run({**original, 'attributes': []}))
        changed = {**original, 'source': 'Value {project}.', 'attributes': [{'name': 'project', 'value': 'first'}, {'name': 'project', 'value': 'last'}]}
        self.assertIn('Value last.', run(changed)['html'])
        self.assertIn('Value last.', run(changed, escaped=True)['html'])
        for malformed in ('invalid', [{'name': 'n', 'value': 4}]):
            self.assertIn('error', run({**original, 'attributes': malformed}))
            self.assertIn('error', run({**original, 'attributes': malformed}, escaped=True))

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

    def test_default_table_footnotes_retain_large_footer_regression(self):
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family']=='table-footnotes']
        large=next(case for case in cases if case['size']==8192)
        self.assertEqual(large['source'].count('footnote:['),8192)
        self.assertTrue({2048,4096,8192}.issubset({case['size']for case in cases}))
        small=[case for case in perf.geometric([4,8]) if case['family']=='table-footnotes']
        self.assertEqual([case['size']for case in small],[4,8])

    def test_geometric_modes_are_independent(self):
        rows=[]
        for mode in ['parse','render']:
            for size in [32,64]:
                rows.append({'case':{'id':str(size),'family':'f','source':'a'*size,'provenance':{'geometric':True}},'mode':mode,'candidate':{'median_ns':size}})
        self.assertEqual(len(perf.scaling(rows)),2)

    def test_default_scaling_retains_explicit_and_passthrough_state_growth(self):
        families={'explicit-attribute-counter-blocks','passthrough-counter-blocks',
                  'explicit-macro-footnote-blocks'}
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family'] in families]
        for family in families:
            with self.subTest(family=family):
                sizes={case['size'] for case in cases if case['family']==family}
                self.assertTrue({1024,2048,4096}.issubset(sizes))
        large={case['family']:case['source'] for case in cases if case['size']==4096}
        self.assertEqual(large['explicit-attribute-counter-blocks'].count('[subs="attributes"]'),4096)
        self.assertEqual(large['passthrough-counter-blocks'].count('pass:a['),4096)
        for family in ('explicit-attribute-counter-blocks','passthrough-counter-blocks'):
            self.assertTrue(large[family].endswith('{key0} {key4095}'))
        notes=large['explicit-macro-footnote-blocks']
        self.assertEqual(notes.count('[subs="macros"]'),4097)
        self.assertEqual(notes.count('footnote:note4095[Note 4095.]'),1)
        self.assertTrue(notes.endswith('footnote:note0[] footnote:note4095[]'))
        small=[case for case in perf.geometric([4,8]) if case['family'] in families]
        self.assertEqual(len(small),6)
        self.assertEqual({case['size'] for case in small},{4,8})

    def test_default_scaling_retains_attribute_growth_with_immediate_macros(self):
        families={'immediate-quote-pass-counter-blocks':'pass:q[*word*]',
                  'immediate-math-counter-blocks':'stem:[x]',
                  'set-passthrough-blocks':'pass:a[{key4095}]',
                  'url-counter-blocks':'https://example.test'}
        cases=list(perf.geometric(perf.parser().parse_args([]).sizes))
        for family, marker in families.items():
            with self.subTest(family=family):
                selected=[case for case in cases if case['family']==family]
                self.assertEqual({case['size'] for case in selected},{32,64,128,256,512,1024,2048,4096})
                source=next(case['source'] for case in selected if case['size']==4096)
                self.assertIn(marker,source)
                self.assertTrue(source.endswith('{key0} {key4095}'))
                if family=='set-passthrough-blocks':
                    self.assertEqual(source.count('{set:key'),4096)
                else:
                    self.assertEqual(source.count('{counter:key'),4096)

    def test_default_scaling_retains_static_snapshots_and_mutating_previews(self):
        families={'static-attributes-url-blocks','mutating-preview-counter-blocks',
                  'mailto-positional-query-counter-blocks'}
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family'] in families]
        for family in families:
            self.assertTrue({1024,2048,4096}.issubset({case['size'] for case in cases if case['family']==family}))
        sources={case['family']:case['source'] for case in cases if case['size']==4096}
        static=sources['static-attributes-url-blocks']
        self.assertEqual(static.count(':key'),4096)
        self.assertEqual(static.count('https://example.test'),4096)
        self.assertTrue(static.endswith('{key0} {key4095}'))
        preview=sources['mutating-preview-counter-blocks']
        self.assertEqual(preview.count('pass:a[{counter:key'),4096)
        self.assertEqual(preview.count('{set:last:value}'),4096)
        self.assertTrue(preview.endswith('{key0} {key4095} {last}'))
        query=sources['mailto-positional-query-counter-blocks']
        self.assertEqual(query.count('mailto:x@y.z[N,S,B]'),4096)
        self.assertEqual(query.count('{counter:key'),4096)
        self.assertNotIn('subject=',query)
        self.assertTrue(query.endswith('{key0} {key4095}'))

    def test_static_nonparagraph_defaults_retain_nine_measured_sources(self):
        # Exact source identities from the retained static nonparagraph audit.
        expected = {
            'scaling-static-attributes-formatted-sections-1024': '70a11d6ede67017d202364c5652216bdfcecdc10487e3148f91da2dbc191c8c2',
            'scaling-static-attributes-formatted-sections-2048': '41d92cb3c10054bee4bdabfa333e8286697fca498c706d59ed30767e561df924',
            'scaling-static-attributes-formatted-sections-4096': '72e79b871fcd603136a24d4b6d752ae46399127d393e81337ed19dde12caabd2',
            'scaling-static-attributes-listing-blocks-1024': '23ae2607cfe7da431a4499671bf0330a8b230ac42cc0b5cd8abe5e9a17f8389b',
            'scaling-static-attributes-listing-blocks-2048': '38bf3656e1b8a98e99bf85602dd91c434ba922c7643c70994ce4821fab76ea09',
            'scaling-static-attributes-listing-blocks-4096': 'ae3ad8bd8bd81cdab1dbd25c95e34643c029a921ba8ad48c6e5fc854fbe4fdba',
            'scaling-static-attributes-url-list-items-1024': '843a59f9eccd0a5310140848af1140fc96fa2777f9eddde89d8dccbe712d1972',
            'scaling-static-attributes-url-list-items-2048': '736be78aed1e591f0166b5d30454db3516d2e62e79b014ec7f8162b795cc3151',
            'scaling-static-attributes-url-list-items-4096': 'b67fad64816bca78697f1a5d492176658025a5c7f180de1fe6397f1f5c00858c',
        }
        families={'static-attributes-url-list-items','static-attributes-listing-blocks',
                  'static-attributes-formatted-sections'}
        selected=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
                  if case['family'] in families]
        for family in families:
            self.assertEqual({case['size'] for case in selected if case['family']==family},
                             {32,64,128,256,512,1024,2048,4096})
        retained=[case for case in selected if case['id'] in expected]
        self.assertEqual(len(retained),9)
        for case in retained:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
                self.assertEqual(case['provenance'],{'generator':case['family'],
                                                    'parameter':case['size'],'geometric':True})
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render') for case in retained]
        checks=perf.scaling(rows)
        self.assertEqual(len(checks),12)
        for check in checks:
            self.assertFalse(check['regression'])
            self.assertAlmostEqual(check['exponent'],1)

    def test_static_nonparagraph_formulas_preserve_context_and_whitespace(self):
        prefix=':k0: v\n:k1: v\n\n'
        self.assertEqual(perf.static_attribute_nonparagraph(2,'list'),
                         prefix+'* https://a.b\n* https://a.b\n')
        self.assertEqual(perf.static_attribute_nonparagraph(2,'listing'),
                         prefix+'----\nx < y & z\n----\n\n----\nx < y & z\n----\n\n')
        self.assertEqual(perf.static_attribute_nonparagraph(2,'section'),
                         prefix+'== *S0*\n\n== *S1*\n\n')

    def test_static_asciidoc_cells_retain_corrected_noheader_sources(self):
        expected = {
            'scaling-static-attributes-asciidoc-table-cells-1024': 'c70ace5be049b2667ec683644516da81dffff6a65d739ff093bca747de660e53',
            'scaling-static-attributes-asciidoc-table-cells-2048': '02131fbb953fa9dbd1854d5e81c73071bd8c1e3f81cf41779982c46b6db97465',
            'scaling-static-attributes-asciidoc-table-cells-4096': '86792223f8f4ca13fa55ddc7170308776df0fa905e9e59c210c200d018ec564d',
        }
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family']=='static-attributes-asciidoc-table-cells']
        self.assertEqual({case['size']for case in cases},{32,64,128,256,512,1024,2048,4096})
        retained=[case for case in cases if case['id']in expected]
        self.assertEqual(len(retained),3)
        for case in retained:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
                self.assertEqual(case['provenance'],{'generator':case['family'],
                                                    'parameter':case['size'],'geometric':True})
        self.assertEqual(perf.static_attribute_nonparagraph(2,'asciidoc-table'),
                         ':k0: v\n:k1: v\n\n[cols="a",options=noheader]\n|===\n'
                         '|https://a.b\n\n|https://a.b\n\n|===\n')
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in retained]
        checks=perf.scaling(rows)
        self.assertEqual(len(checks),4)
        for check in checks:
            self.assertFalse(check['regression'])
            self.assertAlmostEqual(check['exponent'],1)

    def test_oracle_response_budget_default_survives_replay(self):
        args=perf.parser().parse_args([])
        self.assertEqual(args.timeout,180)
        self.assertEqual(args.batch_policy,'per-engine')
        self.assertEqual(args.deadline_seconds,7200)
        tokens=shlex.split(perf.replay_command(args,pathlib.Path('/tmp/slow oracle.json'),['parse']))
        replay=perf.parser().parse_args(tokens[2:])
        self.assertEqual(replay.timeout,180)
        self.assertEqual(replay.batch_policy,'per-engine')
        self.assertEqual(replay.deadline_seconds,7200)
        self.assertEqual(replay.max_ratio,1)
        self.assertEqual(replay.max_scaling_exponent,1.3)

    def test_static_image_defaults_retain_exact_shared_and_varied_media_sources(self):
        expected = {
            'scaling-static-attributes-image-blocks-1024': 'e9fc2db292ee8602483779f03302e25c855f3914e7537be6807c1363840ed58b',
            'scaling-static-attributes-image-blocks-2048': 'c2c6ddfcd4ca4a1d72533f3b9bb68d729e7fcad884c44bfbf85093b466aeb12b',
            'scaling-static-attributes-image-blocks-4096': '5e7c36725b853060cd16fd5db452e67cf21b738a3dd7ecf000835b90bedbe1fc',
            'scaling-static-attributes-varied-image-blocks-1024': 'c2a6353ca88ba8708f964f1a0309504477d49a0a4c01838f08fa18f947490ee9',
            'scaling-static-attributes-varied-image-blocks-2048': 'bb3e5cc45d8180c6cca675e37edd78df3b5c9055a0d43939adf73738b2f053c1',
            'scaling-static-attributes-varied-image-blocks-4096': '1f14139deaf43224b6371b13c1c0aacc8f3bea43921a2d7d684fc2c0bcd4aebf',
        }
        families={'static-attributes-image-blocks','static-attributes-varied-image-blocks'}
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes) if case['family']in families]
        for family in families:
            self.assertEqual({case['size']for case in cases if case['family']==family},
                             {32,64,128,256,512,1024,2048,4096})
        retained=[case for case in cases if case['id']in expected]
        self.assertEqual(len(retained),6)
        for case in retained:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
                self.assertEqual(case['source'].count(':k'),case['size'])
                self.assertEqual(case['source'].count('image::'),case['size'])
                self.assertLessEqual(len(case['source'].encode()),262144)
                self.assertEqual(case['provenance'],{'generator':case['family'],
                                                    'parameter':case['size'],'geometric':True})
        prefix=':k0: v\n:k1: v\n\n'
        self.assertEqual(perf.static_attribute_nonparagraph(2,'image'),
                         prefix+'image::cat.png[Cat]\n\nimage::cat.png[Cat]\n\n')
        self.assertEqual(perf.static_attribute_nonparagraph(2,'varied-image'),
                         prefix+'image::cat0.png[Cat 0]\n\nimage::cat1.png[Cat 1]\n\n')
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in retained]
        checks=perf.scaling(rows)
        self.assertEqual(len(checks),8)
        for check in checks:
            self.assertFalse(check['regression'])
            self.assertAlmostEqual(check['exponent'],1)

    def test_bounded_snapshot_regressions_retain_exact_audited_sources(self):
        expected = {
            'scaling-static-attributes-many-tables-128': '1e6401ac3982855fe9f4525f1e933a8621e46082b82f67ed6a1c727a787dbfa1',
            'scaling-static-attributes-many-tables-256': 'b820103707743e00383a58d0b52c4db0c0dd98a8f4fc42f91a61e20ded441065',
            'scaling-static-attributes-many-tables-512': 'c342cc9996a0836efc38f4a1a0a7d5bade4afb7f42aee69381c73f217c39f844',
            'scaling-static-base-attribute-revisions-text-128': 'c2121c63b73e8c810cd190042d71f1ababe209a4e576f020b812535c49295663',
            'scaling-static-base-attribute-revisions-text-256': 'e9ed7c214955e2e7563a23c38dc93b73e3a2ef30afbea23da1f230516a06fe86',
            'scaling-static-base-attribute-revisions-text-512': 'a240215bbcc5eeca549926f9839f4df2ab506ad6b460190c1cc26a34b2487a86',
            'scaling-static-base-attribute-revisions-url-128': '97cd2e143783a1e6acb87f155a9a210b98b85bfd94e6931773dd12a19339984b',
            'scaling-static-base-attribute-revisions-url-256': '0125fadb63caaab878981b06b38ed055ab05713f99c151555180f3d43b27c831',
            'scaling-static-base-attribute-revisions-url-512': '60fb6f51a28c480933b7b53b9d937cef5fbc128f8e0dd3301f6952bc4d41d4f8',
        }
        families={'static-base-attribute-revisions-url','static-base-attribute-revisions-text',
                  'static-base-attribute-revisions-varied-unset-readd','static-attributes-many-tables'}
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes) if case['family']in families]
        self.assertEqual(len(cases),12)
        for family in families:
            self.assertEqual({case['size']for case in cases if case['family']==family},{128,256,512})
        retained=[case for case in cases if case['id']in expected]
        self.assertEqual(len(retained),9)
        for case in retained:
            self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
            self.assertEqual(case['provenance'],{'generator':case['family'],
                                                'parameter':case['size'],'geometric':True})
        for case in cases:
            self.assertLessEqual(len(case['source'].encode()),262144)
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in cases]
        self.assertEqual(len(perf.scaling(rows)),16)

    def test_varied_source_revisions_unset_and_readd_each_name(self):
        self.assertEqual(perf.static_base_revisions(2,varied=True),
                         ':k0: initial\n:k1: initial\n\n:k0!:\n:k0: v0\n\n{k0}\n\n'
                         ':k1!:\n:k1: v1\n\n{k1}\n\n')
        source=perf.static_base_revisions(512,varied=True)
        self.assertEqual(source.count('!:'),512)
        self.assertIn(':k511!:\n:k511: v511\n\n{k511}',source)

    def test_varied_source_revision_model_matches_pinned_oracle(self):
        if not (perf.DEFAULT_HOME/'gems/asciidoctor-2.0.23').is_dir():
            self.skipTest('run setup_oracle.py for pinned integration tests')
        from review_asciidoc import Oracle
        with Oracle() as oracle:
            result=oracle.request(perf.static_base_revisions(3,varied=True))
        self.assertEqual(result['status'],'ok')
        self.assertEqual([block['context']for block in result['projection']['blocks']],['paragraph']*3)
        for value in ('v0','v1','v2'):
            self.assertIn('<p>'+value+'</p>',result['html'])
        self.assertEqual(result['warnings'],[])

    def test_live_dynamic_redefinition_guards_retain_exact_bounded_sources(self):
        expected = {
            'scaling-live-dynamic-source-redefinitions-counter-reset-128': 'b2eee16dc34a82ec50bca7b2bab965eebf4acce1e23aec4830ec96fd80d80b81',
            'scaling-live-dynamic-source-redefinitions-counter-reset-256': '2fb98a7906b953241a4e2244d81f3a78c1965321396a32ea361f3146035b602f',
            'scaling-live-dynamic-source-redefinitions-counter-reset-512': '5be3a43bb47190c1bdbedc71fab5bae64d894ab26e9c1f1038bf559707d190a7',
            'scaling-live-dynamic-source-redefinitions-counters-128': 'b0433bb9bc1014c7090d038e7abd3f4201c3eb09afa0ffeabe363b8604a7b513',
            'scaling-live-dynamic-source-redefinitions-counters-256': 'fe3804cd725f43e2423ef436f294aedce88c457912aed15d03e5ebe8cd43ef74',
            'scaling-live-dynamic-source-redefinitions-counters-512': '399027b34a837d268fe9c95bd46d4ce2fd3b62e5766d9c0f564a490f7033a848',
            'scaling-live-dynamic-source-redefinitions-sets-128': '1b5956a74f5a6922d6d04ffa4247fcf065b639c6240ae5428900af10379bfbc2',
            'scaling-live-dynamic-source-redefinitions-sets-256': '3863f35f6a30e9815ca094424402519497f20326008ee01faa57f5ab50574495',
            'scaling-live-dynamic-source-redefinitions-sets-512': '70cce70bc97f3bd0ac89c6c9975f0cc2cb3f0212c4d0508cfd0e58e11330dba4',
        }
        families={'live-dynamic-source-redefinitions-counters','live-dynamic-source-redefinitions-sets',
                  'live-dynamic-source-redefinitions-counter-reset'}
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes) if case['family']in families]
        self.assertEqual(len(cases),9)
        for family in families:
            self.assertEqual({case['size']for case in cases if case['family']==family},{128,256,512})
        for case in cases:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
                self.assertLessEqual(len(case['source'].encode()),262144)
                self.assertEqual(case['source'].count(':unrelated:'),case['size'])
                self.assertEqual(case['provenance'],{'generator':case['family'],
                                                    'parameter':case['size'],'geometric':True})
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in cases]
        self.assertEqual(len(perf.scaling(rows)),12)

    def test_live_dynamic_redefinition_shapes_cover_sets_counters_and_reset(self):
        source=perf.live_dynamic_source_redefinitions(4,'counters')
        self.assertTrue(source.startswith('Begin.\n\nSeed {counter:key0} {counter:key1}'))
        self.assertEqual(source.count('{counter:key'),6)
        self.assertTrue(source.endswith('Final {counter:key0} {counter:key3} {unrelated}.\n'))
        source=perf.live_dynamic_source_redefinitions(4,'sets')
        self.assertEqual(source.count('{set:key'),4)
        self.assertNotIn('{counter:',source)
        self.assertTrue(source.endswith('Final {key0} {key3} {unrelated}.\n'))
        source=perf.live_dynamic_source_redefinitions(4,'counter-reset')
        self.assertEqual(source.count(':key0: 40'),1)
        self.assertIn(':unrelated: 2\n:key0: 40\n\nProbe {counter:key0} {key3} {unrelated}.',source)
        self.assertEqual(source.count('{counter:key'),7)

    def test_each_live_counter_reset_retains_exact_bounded_sources(self):
        expected = {
            'scaling-live-dynamic-source-redefinitions-each-counter-reset-128': '67f570e0d8134c9c363edd90017f166d26a02cfbbfb68824c1dc6d5efe1daec6',
            'scaling-live-dynamic-source-redefinitions-each-counter-reset-256': '01f3e6953820a0aabd897daf973470f0b118a8b32432cb0d9c84e7a4852605f5',
            'scaling-live-dynamic-source-redefinitions-each-counter-reset-512': '6d57278720a8f1a5db5511dc7fe6cff8ad8c5ef9e8139821eb5380e1601e28e6',
        }
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family']=='live-dynamic-source-redefinitions-each-counter-reset']
        self.assertEqual({case['size']for case in cases},{128,256,512})
        for case in cases:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']])
                self.assertLessEqual(len(case['source'].encode()),262144)
                self.assertEqual(case['source'].count(': 40'),case['size'])
                self.assertEqual(case['source'].count('{counter:key'),case['size']*2+2)
                self.assertEqual(case['provenance'],{'generator':case['family'],
                                                    'parameter':case['size'],'geometric':True})
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in cases]
        checks=perf.scaling(rows)
        self.assertEqual(len(checks),4)
        self.assertTrue(all(not check['regression']for check in checks))
        self.assertEqual(perf.each_live_counter_reset(2),
                         'Begin.\n\nSeed {counter:key0} {counter:key1}.\n\n'
                         ':key0: 40\n\nProbe {counter:key0}.\n\n'
                         ':key1: 40\n\nProbe {counter:key1}.\n\n'
                         'Final {counter:key0} {counter:key1}.\n')

    def test_alternating_rich_simple_guards_retain_exact_requests_and_predicates(self):
        expected = {
            'scaling-alternating-rich-simple-counters-128': ('7d2da7fdc41325a90f90abd49d55ece197812cd6e7353f99e24ec251645fecd7', 'b7f824ac32760bd3d52814f8733795a076e97cd87d7c9e681d2865a8e038d5b7'),
            'scaling-alternating-rich-simple-counters-256': ('b5582d3da108fdedcf5f57581616144924307c2be8a83ac62ec006bd78078440', 'af37186c98018bab9cb13b9b63c8a8faad4d7e58e7133ac27fffd3138710d9ee'),
            'scaling-alternating-rich-simple-counters-512': ('19b3a1b6946b6ede549f5ebf4894b197ad92a2239a30e76d9c87e1c1d188edfc', 'b9f1c3f4831260965a9a78fe5f9501863fc3f6fad258031831769a3932ddd33c'),
        }
        cases=[case for case in perf.geometric(perf.parser().parse_args([]).sizes)
               if case['family']=='alternating-rich-simple-counters']
        self.assertEqual({case['size']for case in cases},{128,256,512})
        for case in cases:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']][0])
                self.assertEqual(perf.case_identity(case),expected[case['id']][1])
                self.assertEqual(case['source'].count('\n\nBody.\n\n'),case['size'])
                self.assertEqual(case['source'].count('{counter:key'),case['size']+2)
                self.assertLessEqual(len(case['source'].encode()),262144)
                self.assertEqual(case['expect'],{'html_contains':['<p>Counter 1.</p>','<p>Body.</p>','<p>Final 2 2.</p>']})
                self.assertEqual(case['provenance'],{'generator':case['family'],'parameter':case['size'],'geometric':True})
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in cases]
        self.assertEqual(len(perf.scaling(rows)),4)

    def test_alternating_rich_simple_model_matches_authored_oracle_predicates(self):
        if not (perf.DEFAULT_HOME/'gems/asciidoctor-2.0.23').is_dir():
            self.skipTest('run setup_oracle.py for pinned integration tests')
        from review_asciidoc import Oracle
        with Oracle() as oracle:
            result=oracle.request(perf.alternating_rich_simple_counters(2))
        self.assertEqual(result['status'],'ok')
        for expected in ('<p>Counter 1.</p>','<p>Body.</p>','<p>Final 2 2.</p>'):
            self.assertIn(expected,result['html'])

    def test_alternating_options_guards_retain_ordered_requests_and_measurement_status(self):
        expected = {
            'scaling-alternating-rich-simple-counters-128-same-standard-option': ('7d2da7fdc41325a90f90abd49d55ece197812cd6e7353f99e24ec251645fecd7', 'bb16bb844fde07eed5cfc320f839f295bde1a1c6c9cf12760d7268dae83a9d1c'),
            'scaling-alternating-rich-simple-counters-256-same-standard-option': ('b5582d3da108fdedcf5f57581616144924307c2be8a83ac62ec006bd78078440', '0b5a5b60210cc813db5fafcff801209e817326559e29387a438eb6879eb2b627'),
            'scaling-alternating-rich-simple-counters-512-same-standard-option': ('19b3a1b6946b6ede549f5ebf4894b197ad92a2239a30e76d9c87e1c1d188edfc', '8c67cc6a2366543f634f3a5c556493c89a9351057453cf1f91d57dc604d3c403'),
            'scaling-alternating-rich-simple-counters-128-restored-standard-option': ('7d2da7fdc41325a90f90abd49d55ece197812cd6e7353f99e24ec251645fecd7', 'efefa9d17a2c80edf3af08d051b49c9a06ddc06a57e98cd04de1b346a263dc1a'),
            'scaling-alternating-rich-simple-counters-256-restored-standard-option': ('b5582d3da108fdedcf5f57581616144924307c2be8a83ac62ec006bd78078440', '9670c7c17623e0234bd929ac9e18b9c4dadc7351b4c42d85250f3781bf282188'),
            'scaling-alternating-rich-simple-counters-512-restored-standard-option': ('19b3a1b6946b6ede549f5ebf4894b197ad92a2239a30e76d9c87e1c1d188edfc', '0de828457d1b4fc34075a6b58938b89e80775583c67898099d8642d20f4d2ed4'),
        }
        all_cases=list(perf.geometric(perf.parser().parse_args([]).sizes))
        families={'alternating-rich-simple-counters-option','alternating-rich-simple-counters-restored-standard-option'}
        cases=[case for case in all_cases if case['family']in families]
        self.assertEqual(len(cases),6)
        for family in families:
            self.assertEqual({case['size']for case in cases if case['family']==family},{128,256,512})
        for case in cases:
            with self.subTest(case=case['id']):
                self.assertEqual(hashlib.sha256(case['source'].encode()).hexdigest(),expected[case['id']][0])
                self.assertEqual(perf.case_identity(case),expected[case['id']][1])
                self.assertEqual(perf.request(case,'render',0)['attributes'],case['attributes'])
                self.assertLessEqual(len(case['source'].encode()),262144)
                self.assertEqual(case['expect'],{'html_contains':['<p>Counter 1.</p>','<p>Body.</p>','<p>Final 2 2.</p>']})
                self.assertTrue(case['provenance']['geometric'])
                self.assertEqual(case['provenance']['parameter'],case['size'])
                if case['family'].endswith('restored-standard-option'):
                    self.assertEqual(case['attributes'],[{'name':'outfilesuffix','value':'.x'},{'name':'outfilesuffix','value':'.html'}])
                    self.assertEqual(case['provenance']['measurement_status'],'unmeasured')
                else:
                    self.assertEqual(case['attributes'],[{'name':'outfilesuffix','value':'.html'}])
                    self.assertEqual(case['provenance']['measurement_status'],'measured-regression')
                base=next(c for c in all_cases if c['id']==f'scaling-alternating-rich-simple-counters-{case["size"]}')
                self.assertNotIn('attributes',perf.request(base,'render',0))
                self.assertNotEqual(perf.case_identity(base),perf.case_identity(case))
        rows=[{'case':case,'mode':mode,'candidate':{'median_ns':len(case['source'].encode())}}
              for mode in ('parse','render')for case in cases]
        self.assertEqual(len(perf.scaling(rows)),8)

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

    def test_duration_exhaustion_preserves_all_raw_attempts(self):
        class Worker:
            def request(self, request):
                return {'version':1, 'id':'x', 'iterations':request['iterations'],
                        'successes':request['iterations'], 'elapsed_ns':999,
                        'checksum':request['iterations'],
                        'projection':{'title':'', 'blocks':[]}, 'html':''}
        expected={'projection':{'title':'', 'blocks':[]}, 'html':''}
        with self.assertRaisesRegex(ValueError, 'repeated samples') as caught:
            perf.sample_pair([Worker(),Worker()], {'id':'x','source':'x'},
                             'parse', 1, 1000, 5, 100, expected)
        attempts=caught.exception.measurement_attempts
        self.assertEqual(len(attempts),4)
        for attempt in attempts:
            self.assertEqual(attempt['candidate_elapsed_ns'],[999]*5)
            self.assertEqual(attempt['reference_elapsed_ns'],[999]*5)
            self.assertEqual(attempt['qualification'],'refine-duration')
        self.assertEqual([attempt['iterations']for attempt in attempts],[1,2,3,4])

    def test_iteration_cap_preserves_the_unqualified_samples(self):
        class Worker:
            def request(self, request):
                return {'version':1, 'id':'x', 'iterations':request['iterations'],
                        'successes':request['iterations'], 'elapsed_ns':1,
                        'checksum':request['iterations'],
                        'projection':{'title':'', 'blocks':[]}, 'html':''}
        expected={'projection':{'title':'', 'blocks':[]}, 'html':''}
        with self.assertRaisesRegex(ValueError, 'iteration cap') as caught:
            perf.sample_pair([Worker(),Worker()], {'id':'x','source':'x'},
                             'render', 8, 1000, 5, 8, expected)
        attempts=caught.exception.measurement_attempts
        self.assertEqual(len(attempts),1)
        self.assertEqual(attempts[0]['iterations'],8)
        self.assertEqual(attempts[0]['candidate_elapsed_ns'],[1]*5)
        self.assertEqual(attempts[0]['reference_elapsed_ns'],[1]*5)

    def test_qualification_failure_retains_attempts_in_report_journal_and_replay(self):
        class Worker:
            def __init__(self, command, timeout, env=None):
                pass
            def request(self, request):
                return {'version':1, 'id':request['id'], 'iterations':request['iterations'],
                        'successes':request['iterations'], 'elapsed_ns':999999,
                        'checksum':request['iterations'],
                        'projection':{'title':'', 'blocks':[]}, 'html':'',
                        'oracle_version':'2.0.23', 'oracle_code_sha256':'a'*64,
                        'include_io':'caller-map-memory', 'include_path_metadata':{}}
            def close(self):
                pass
        with tempfile.TemporaryDirectory() as tmp, \
             mock.patch.object(perf.time, 'monotonic', return_value=0), \
             mock.patch.object(perf.platform, 'platform', return_value='test OS'), \
             mock.patch.object(perf.platform, 'processor', return_value='test CPU'), \
             mock.patch.multiple(perf,
                 Worker=Worker,
                 build=mock.Mock(return_value=(pathlib.Path('/fake-worker'), {'source_identity':'same','compiler':{'sha256':'compiler-hash'}})),
                 digest=mock.Mock(return_value='a'*64), source_identity=mock.Mock(return_value='same'),
                 corpus=mock.Mock(return_value=[{'id':'x','family':'test','source':'hello'}]),
                 calibrate=mock.Mock(return_value=1)), \
             mock.patch.object(perf.subprocess, 'run', return_value=SimpleNamespace(stdout='Ruby test')), \
             mock.patch.object(perf.sys, 'stderr', new_callable=io.StringIO) as progress_output:
            self.assertEqual(perf.main(['--smoke','--progress','--modes','render','--output',tmp]),1)
            output=pathlib.Path(tmp)
            report=json.loads((output/'results.json').read_text())
            row=report['records'][0]
            self.assertEqual(report['schema_version'],2)
            self.assertEqual(report['batch_policy'],'per-engine')
            self.assertEqual(row['status'],'error')
            self.assertIn('repeated samples',row['error'])
            self.assertTrue(row['correctness']['projection'])
            self.assertTrue(row['correctness']['rendered_dom'])
            self.assertEqual(len(row['correctness']['before']),2)
            self.assertEqual(row['correctness']['after'],row['correctness']['before'])
            self.assertEqual(len(row['measurement_attempts']),4)
            retained=json.loads(next((output/'retained').glob('*.json')).read_text())
            journal=json.loads((output/'measurements.jsonl').read_text())
            self.assertEqual(retained['measurement']['measurement_attempts'],row['measurement_attempts'])
            self.assertEqual(journal['measurement_attempts'],row['measurement_attempts'])
            self.assertEqual(retained['run']['batch_policy'],'per-engine')
            self.assertTrue(retained['run']['progress'])
            self.assertIn('--progress',retained['measurement']['replay'])
            self.assertIn('[x render]: begin',progress_output.getvalue())
            self.assertIn('sample=1/1 engine=candidate iterations=',progress_output.getvalue())
            self.assertIn('qualification=refine-duration',progress_output.getvalue())
            self.assertIn('complete status=error',progress_output.getvalue())
            self.assertTrue(all('iteration_counts'in attempt for attempt in row['measurement_attempts']))
            self.assertIn('--replay',retained['measurement']['replay'])

    def test_per_engine_batches_normalize_counts_and_alternate_all_seven_samples(self):
        calls=[]
        class Worker:
            def __init__(self, name, period):self.name=name;self.period=period
            def request(self, request):
                calls.append((self.name,request['iterations']))
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],
                        'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        workers=[Worker('candidate',30000000),Worker('reference',32000000000)]
        measured,attempts=perf.sample_pair_per_engine(workers,{'id':'x','source':'x'},'parse',
                                                     [2,1],50000000,7,100000,expected)
        self.assertEqual([item['iterations']for item in measured],[2,1])
        self.assertEqual([item['median_ns']for item in measured],[30000000,32000000000])
        self.assertAlmostEqual(perf.ratchet(*measured)['ratio'],30000000/32000000000)
        self.assertTrue(all(item['observed_minimum_window_ns']>=50000000 for item in measured))
        self.assertEqual(attempts[0]['iteration_counts'],{'candidate':2,'reference':1})
        self.assertNotIn('iterations',attempts[0])
        self.assertEqual(calls[2:],[('candidate',2),('reference',1),('reference',1),('candidate',2),
                                   ('candidate',2),('reference',1),('reference',1),('candidate',2),
                                   ('candidate',2),('reference',1),('reference',1),('candidate',2),
                                   ('candidate',2),('reference',1)])

    def test_per_engine_duration_refinement_does_not_repeat_qualified_oracle_work(self):
        class Worker:
            def __init__(self, period):self.period=period
            def request(self, request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],
                        'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        measured,attempts=perf.sample_pair_per_engine([Worker(10),Worker(1000)],{'id':'x','source':'x'},
                                                     'parse',[10,1],1000,5,10000,expected)
        self.assertEqual(len(attempts),2)
        self.assertGreater(measured[0]['iterations'],10)
        self.assertEqual(measured[1]['iterations'],1)
        self.assertTrue(all(item['observed_minimum_window_ns']>=1000 for item in measured))

    def test_per_engine_noise_refines_only_noisy_batches_and_stays_bounded(self):
        class Worker:
            def __init__(self, noisy, settles):self.noisy=noisy;self.settles=settles;self.calls=0
            def request(self, request):
                self.calls+=1
                period=(1000 if self.calls%2 else 3000) if self.noisy and (not self.settles or request['iterations']<20) else 4000
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*period,'checksum':request['iterations'],
                        'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        for settles in (True,False):
            with self.subTest(settles=settles):
                measured,attempts=perf.sample_pair_per_engine([Worker(True,settles),Worker(False,True)],
                    {'id':'x','source':'x'},'parse',[10,10],10000,5,1000,expected)
                self.assertEqual([attempt['iteration_counts']['reference']for attempt in attempts],[10]*len(attempts))
                self.assertEqual(len(attempts),2 if settles else 4)
                self.assertEqual(perf.ratchet(*measured)['noisy'],not settles)
                self.assertEqual(attempts[-1]['qualification'],'both-duration-floor' if settles else 'both-duration-floor-noisy')

    def test_per_engine_iteration_cap_retains_truthful_raw_counts(self):
        class Worker:
            def __init__(self, period):self.period=period
            def request(self, request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],
                        'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        with self.assertRaisesRegex(ValueError,'iteration cap') as caught:
            perf.sample_pair_per_engine([Worker(1),Worker(1000)],{'id':'x','source':'x'},
                                        'render',[8,1],1000,5,8,expected)
        attempt=caught.exception.measurement_attempts[0]
        self.assertEqual(attempt['iteration_counts'],{'candidate':8,'reference':1})
        self.assertEqual(attempt['candidate_elapsed_ns'],[8]*5)
        self.assertEqual(attempt['reference_elapsed_ns'],[1000]*5)

    def test_per_engine_sampling_checks_fresh_semantics_and_counts(self):
        class Worker:
            def __init__(self, failure):self.failure=failure;self.calls=0
            def request(self, request):
                self.calls+=1
                fail=self.calls==3
                return {'version':1,'id':'x','iterations':request['iterations'],
                        'successes':0 if fail and self.failure=='count' else request['iterations'],
                        'elapsed_ns':request['iterations']*1000,
                        'checksum':0 if fail and self.failure=='checksum' else request['iterations'],
                        'projection':{'title':'wrong' if fail and self.failure=='projection' else '', 'blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        for failure in ('count','projection','checksum'):
            with self.subTest(failure=failure),self.assertRaises(ValueError):
                perf.sample_pair_per_engine([Worker(failure),Worker('none')],{'id':'x','source':'x'},
                                            'parse',[1,1],1000,7,100,expected)

    def test_per_engine_screen_does_not_hide_near_threshold_regression(self):
        class Worker:
            def __init__(self, period):self.period=period
            def request(self, request):
                return {'version':1,'id':'x','iterations':request['iterations'],'successes':request['iterations'],
                        'elapsed_ns':request['iterations']*self.period,'checksum':request['iterations'],
                        'projection':{'title':'','blocks':[]},'html':''}
        expected={'projection':{'title':'','blocks':[]},'html':''}
        measured,attempts=perf.sample_pair_per_engine([Worker(110),Worker(100)],{'id':'x','source':'x'},
            'parse',[1,1],1000,5,1000,expected,screen=True,clock_floor_ns=1)
        self.assertTrue(perf.ratchet(*measured)['regression'])
        self.assertTrue(all(item['observed_minimum_window_ns']>=1000 for item in measured))
        self.assertEqual(attempts[-1]['qualification'],'both-duration-floor')

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
              '--max-scaling-exponent','1.2','--max-iterations','12345','--timeout','41','--batch-policy','common','--progress',
              '--ruby','/opt/ruby path/bin/ruby','--gem-home','/tmp/gem home'])
        tokens=shlex.split(perf.replay_command(args,pathlib.Path('/tmp/slow case.json'),['parse']))
        replay=perf.parser().parse_args(tokens[2:])
        for key in ('window_ms','samples','max_ratio','max_scaling_exponent','max_iterations',
                    'timeout','build_timeout','deadline_seconds','ruby','roc','gem_home','batch_policy','progress'):
            self.assertEqual(getattr(args,key),getattr(replay,key),key)
        self.assertEqual(replay.replay,[pathlib.Path('/tmp/slow case.json')])
        self.assertEqual(replay.modes,['parse'])

    def test_full_campaign_prioritizes_stateful_geometry_without_dropping_inputs(self):
        args=perf.parser().parse_args(['--generated','8','--sizes','32','64'])
        args.fixtures=[]
        cases=list(perf.corpus(args))
        geometry=list(perf.geometric(args.sizes))
        generated=list(perf.generated(args.seed,args.generated))
        self.assertEqual(cases,geometry+generated+list(perf.fixed_performance_guards(args)))
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

class FixedPerformanceGuards(unittest.TestCase):
    def write_guards(self, root, cases):
        path = root / 'scripts/asciidoc/performance-guards.json'
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps({'schema_version': 1, 'cases': cases}))
        return path

    def args(self, root, sizes=('128', '256', '512')):
        args = perf.parser().parse_args(['--root', str(root), '--generated', '0', '--sizes', *sizes])
        args.fixtures = []
        return args

    def test_append_after_all_historical_inputs_preserves_exact_request_prefix(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            fixture = root / 'fixtures.json'
            fixture.write_text(json.dumps({'cases': [{'id': 'fixture', 'source': 'Fixture.'}]}))
            docs = root / 'docs'
            docs.mkdir()
            (docs / 'guide.adoc').write_text('Document.')
            guard = {'id': 'guard', 'source': 'Guard.', 'family': 'ordered-options',
                     'attributes': [{'name': 'n', 'value': 'first'}, {'name': 'n', 'value': 'last'}],
                     'size': 32, 'scaling_input': 'request', 'provenance': {'authored': 'kept'}}
            path = self.write_guards(root, [guard])
            args = self.args(root)
            args.fixtures = [fixture]
            args.corpus = [docs]
            geometry = [{'id': 'geometry', 'source': 'Geometry.', 'family': 'geometry', 'includes': {}}]
            generated = [{'id': 'generated', 'source': 'Generated.', 'family': 'generated-model'}]
            measured = [{'id': 'measured', 'source': 'Measured.', 'family': 'measured'}]
            with mock.patch.object(perf, 'geometric', return_value=geometry), \
                 mock.patch.object(perf, 'generated', return_value=generated), \
                 mock.patch.object(perf, 'measured_regressions', return_value=measured):
                old = list(perf.historical_corpus(args))
                old_fingerprints = [perf.case_identity(case) for case in old]
                actual = list(perf.corpus(args))
            self.assertEqual([case['id'] for case in old[:3]], ['geometry', 'generated', 'fixture'])
            self.assertTrue(old[3]['id'].startswith('corpus-'))
            self.assertEqual(old[-1]['id'], 'measured')
            self.assertEqual(actual[:-1], old)
            self.assertEqual([perf.case_identity(case) for case in actual[:-1]], old_fingerprints)
            self.assertEqual(actual[-1]['id'], 'guard')
            self.assertEqual(actual[-1]['family'], guard['family'])
            self.assertEqual(actual[-1]['scaling_input'], 'request')
            self.assertEqual(actual[-1]['attributes'], guard['attributes'])
            self.assertEqual(perf.case_identity(actual[-1]), perf.case_identity(guard))
            self.assertEqual(actual[-1]['provenance']['authored'], 'kept')
            self.assertEqual(actual[-1]['provenance']['performance_guard_sha256'], perf.digest(path))

    def test_source_extensions_require_expanded_sizes_but_small_and_request_guards_always_load(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            cases = [{'id': 'small', 'source': 'S', 'size': 512, 'family': 'standard'},
                     {'id': 'request', 'source': 'R', 'size': 2048, 'family': 'options', 'scaling_input': 'request'},
                     *[{'id': f'large-{size}', 'source': 'X', 'size': size, 'family': 'source-growth'}
                       for size in (1024, 2048, 4096)]]
            self.write_guards(root, cases)
            for sizes in [('32',), ('128', '256', '512')]:
                self.assertEqual([c['id'] for c in perf.fixed_performance_guards(self.args(root, sizes))], ['small', 'request'])
            expanded = list(perf.fixed_performance_guards(self.args(root, ('1024',))))
            self.assertEqual([c['id'] for c in expanded], [c['id'] for c in cases])
            self.assertEqual([c['family'] for c in expanded], [c['family'] for c in cases])
            self.assertEqual([c.get('scaling_input', 'source') for c in expanded], ['source', 'request', 'source', 'source', 'source'])

    def test_identical_request_duplicate_keeps_original_case_and_measurement_fields(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            old = {'id': 'same', 'source': 'Body.', 'family': 'historical', 'provenance': {'original': True}}
            self.write_guards(root, [{**old, 'family': 'guard', 'scaling_input': 'request'}])
            with mock.patch.object(perf, 'historical_corpus', return_value=iter([old])):
                actual = list(perf.corpus(self.args(root)))
            self.assertEqual(actual, [old])
            self.assertIs(actual[0], old)

    def test_duplicate_id_with_changed_source_include_policy_or_option_order_is_an_error(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            old = {'id': 'same', 'source': 'Body.', 'attributes': [{'name': 'n', 'value': 'first'}, {'name': 'n', 'value': 'last'}]}
            changed = [{**old, 'source': 'Changed.'}, {**old, 'includes': {}},
                       {**old, 'attributes': list(reversed(old['attributes']))}]
            for case in changed:
                with self.subTest(case=case):
                    self.write_guards(root, [case])
                    with mock.patch.object(perf, 'historical_corpus', return_value=iter([old])):
                        with self.assertRaisesRegex(ValueError, 'duplicate ID.*different request fingerprint: same'):
                            list(perf.corpus(self.args(root)))

    def test_normal_campaign_missing_guard_file_fails_instead_of_silently_omitting_cases(self):
        with tempfile.TemporaryDirectory() as directory:
            args = self.args(pathlib.Path(directory))
            with mock.patch.object(perf, 'historical_corpus', return_value=iter([])):
                with self.assertRaisesRegex(ValueError, 'required performance guard file missing'):
                    list(perf.corpus(args))

    def test_smoke_and_replay_never_load_guards(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            replay = root / 'replay.json'
            cases = [{'id': 'retained', 'source': 'Body.', 'family': 'retained', 'scaling_input': 'request'}]
            replay.write_text(json.dumps({'cases': cases}))
            with mock.patch.object(perf, 'fixed_performance_guards', side_effect=AssertionError('must not load')):
                smoke = list(perf.corpus(perf.parser().parse_args(['--root', str(root), '--smoke'])))
                self.assertEqual(len(smoke), 2)
                self.assertEqual(list(perf.corpus(argparse.Namespace(replay=[replay]))), cases)

    def test_guard_preflight_finishes_before_first_historical_yield_but_output_stays_appended(self):
        old = {'id': 'old', 'source': 'Body.'}
        guard = {'id': 'guard', 'source': 'Guard.'}
        args = self.args(pathlib.Path('/unused'))
        events = []
        def historical(_):
            events.append('historical')
            yield old
        def guards(_):
            events.append('guards')
            yield guard
            events.append('guards-complete')
        with mock.patch.object(perf, 'historical_corpus', side_effect=historical), \
             mock.patch.object(perf, 'fixed_performance_guards', side_effect=guards) as load:
            cases = perf.corpus(args)
            load.assert_not_called()
            self.assertIs(next(cases), old)
            self.assertEqual(events, ['guards', 'guards-complete', 'historical'])
            load.assert_called_once_with(args)
            self.assertEqual(list(cases), [guard])

    def test_missing_or_invalid_guard_fails_next_before_any_historical_yield(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            path = root / 'scripts/asciidoc/performance-guards.json'
            path.parent.mkdir(parents=True)
            for content in [None, 'not-json', json.dumps({'schema_version': 99, 'cases': []}),
                            json.dumps({'schema_version': 1, 'cases': {}})]:
                with self.subTest(content=content):
                    if content is None:
                        path.unlink(missing_ok=True)
                    else:
                        path.write_text(content)
                    with mock.patch.object(perf, 'historical_corpus', return_value=iter([{'id': 'old', 'source': 'Body.'}])) as historical:
                        cases = perf.corpus(self.args(root))
                        with self.assertRaises(ValueError):
                            next(cases)
                        historical.assert_not_called()

    def test_retained_file_has_exact_cohorts_and_preserves_request_scaling(self):
        path = ROOT / 'scripts/asciidoc/performance-guards.json'
        value = json.loads(path.read_text())
        cases = value['cases']
        self.assertEqual(value['schema_version'], 1)
        self.assertFalse(value['provenance']['passing_claim'])
        self.assertEqual(len(cases), 248)
        self.assertEqual(len({case['id'] for case in cases}), 248)
        options = [c for c in cases if c.get('scaling_input') == 'request']
        self.assertEqual(len(options), 18)
        self.assertTrue(all(c.get('attributes') for c in options))
        large = [c for c in cases[:83] if c.get('size', 0) > 512]
        self.assertEqual(len(large), 24)
        self.assertEqual({c['size'] for c in large}, {1024, 2048, 4096})
        self.assertEqual({c['family'] for c in large}, {'live-dynamic-source-redefinitions-each-counter-reset', 'header-first-section-quote-counters', 'header-first-section-quote-counters-escaped', 'header-first-section-quote-counters-role-plus', 'header-first-section-link-counters', 'header-first-section-angle-https-counters', 'header-first-section-angle-http-counters', 'header-first-section-angle-literal-counters'})
        self.assertTrue(all(c.get('scaling_input', 'source') == 'source' for c in large))
        ids = [case_id for origin in value['provenance']['origins'] for case_id in origin['case_ids']]
        self.assertEqual(ids, [c['id'] for c in cases])
        self.assertTrue(all(len(origin['source_sha256']) == 64 for origin in value['provenance']['origins']))


    def test_qualified_extensions_preserve_exact_requests_and_bounded_inventory(self):
        value = json.loads((ROOT / 'scripts/asciidoc/performance-guards.json').read_text())
        cases = value['cases']
        compact = lambda rows: json.dumps(rows, sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()
        self.assertEqual(hashlib.sha256(compact(cases[:83])).hexdigest(), 'da84890e20192e3d71ae9ea6783bcd2568dff5d8be5dff26fdd985a14583a77e')
        self.assertEqual(hashlib.sha256(compact(cases[83:])).hexdigest(), 'c9875b5004a6d81e89f021ed14aa718e3268e2e81d35fcb41e0d967fc7e8b1e3')
        self.assertEqual(len(cases[83:]), 165)
        self.assertEqual(len({perf.case_identity(c) for c in cases[83:]}), 165)
        self.assertTrue({perf.case_identity(c) for c in cases[:83]}.isdisjoint(perf.case_identity(c) for c in cases[83:]))
        self.assertTrue(all(c['provenance']['geometric'] is True for c in cases[83:]))
        self.assertTrue(all(len(c['source'].encode()) <= 262144 for c in cases[83:]))
        bounded = list(perf.fixed_performance_guards(self.args(ROOT)))
        expanded = list(perf.fixed_performance_guards(self.args(ROOT, ('1024',))))
        self.assertEqual(len(bounded), 134)
        self.assertEqual([c['id'] for c in expanded], [c['id'] for c in cases])
        self.assertEqual(len([c for c in cases if c.get('size', 0) > 512]), 114)
        origins = [o for o in value['provenance']['origins'] if o['cohort'].startswith('qualified-')]
        self.assertEqual([len(o['case_ids']) for o in origins], [24, 15, 24, 48, 24, 24, 6])
        self.assertEqual([case_id for o in origins for case_id in o['case_ids']], [c['id'] for c in cases[83:]])
        self.assertEqual(len(value['provenance']['qualified_extension_reports']), 1)
        resolutions = value['provenance']['qualified_family_resolutions']
        self.assertEqual([r['family'] for r in resolutions], ['literal-search-missing-xref-entity'])
        self.assertEqual(resolutions[0]['status'], 'resolved by stronger replay')
        self.assertTrue(resolutions[0]['primary_flags_unchanged'])
        self.assertTrue(all(not c['regression'] for c in resolutions[0]['resolved_scaling']))

    def test_supported_title_extensions_preserve_legacy47_and_exact_geometric_inputs(self):
        value = json.loads((ROOT / 'scripts/asciidoc/performance-guards.json').read_text())
        cases = value['cases']
        # These twelve requests were already geometric in the broad corpus.
        # Restore only the formerly absent marker for historical byte hashes;
        # require the real retained guards to keep their scaling checks enabled.
        promoted = {
            f'scaling-header-first-section-{kind}-body-{size}'
            for kind in ('quote-counters-escaped', 'quote-counters-role-plus')
            for size in (128, 256, 512, 1024, 2048, 4096)
        }
        legacy = json.loads(json.dumps(cases[:83]))
        self.assertEqual({c['id'] for c in legacy if c['id'] in promoted}, promoted)
        for case in legacy:
            if case['id'] in promoted:
                self.assertIs(case['provenance'].pop('geometric'), True)
        prefix = hashlib.sha256(json.dumps(legacy[:47], sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()
        self.assertEqual(prefix, '8a394040639a4eb3ba1e183707cb3b1e3d330e7eaabd4e90e4024ec0a5c72c0e')
        first65 = hashlib.sha256(json.dumps(legacy[:65], sort_keys=True, ensure_ascii=False, separators=(',', ':')).encode()).hexdigest()
        self.assertEqual(first65, 'c1a5b0479a6ddb0ccb66ce8e683a8c42aa043cd17c27bc76cabcf6f6be5f5d01')
        families = {
            'header-first-section-quote-counters-escaped': 'escaped-header-quotes',
            'header-first-section-quote-counters-role-plus': 'role-plus-header-quotes',
            'header-first-section-link-counters': 'explicit-header-links',
            'header-first-section-angle-https-counters': 'angle-https-header-counters',
            'header-first-section-angle-http-counters': 'angle-http-header-counters',
            'header-first-section-angle-literal-counters': 'angle-literal-header-counters',
        }
        for family, cohort in families.items():
            selected = [case for case in cases[47:83] if case['family'] == family]
            self.assertEqual([case['size'] for case in selected], [128, 256, 512, 1024, 2048, 4096])
            origin = next(item for item in value['provenance']['origins'] if item['cohort'] == cohort)
            self.assertTrue(origin['geometric'])
            self.assertEqual(origin['sizes'], [128, 256, 512, 1024, 2048, 4096])
            self.assertEqual(origin['case_ids'], [case['id'] for case in selected])
            self.assertEqual(origin['max_source_utf8_bytes'], max(len(case['source'].encode()) for case in selected))
            for case in selected:
                self.assertLessEqual(len(case['source'].encode()), 262144)
                self.assertEqual(case['provenance']['source_sha256'], hashlib.sha256(case['source'].encode()).hexdigest())
                self.assertEqual(case.get('scaling_input', 'source'), 'source')
                self.assertEqual(case['expect'], {'html_contains': ['<p>Body.</p>']})
        self.assertFalse(value['provenance']['passing_claim'])
        self.assertEqual(len(cases[47:83]), 36)




class DefaultCalloutGeometry(unittest.TestCase):
    families = {'default-callout-comment','default-callout-escaped'}

    def test_default_and_bounded_sizes_are_natural_legal_growth(self):
        cases=[c for c in perf.geometric(perf.parser().parse_args([]).sizes) if c['family'] in self.families]
        self.assertEqual(len(cases),16)
        for family in self.families:
            selected=[c for c in cases if c['family']==family]
            self.assertEqual([c['size']for c in selected],[32,64,128,256,512,1024,2048,4096])
            previous=0
            for c in selected:
                lines=c['source'].split('\n')
                self.assertEqual(lines[0], '----');self.assertEqual(lines[-1], '----')
                self.assertEqual(len(lines)-2,c['size'])
                token='code <!--1-->' if family=='default-callout-comment' else 'code \\<1>'
                self.assertEqual(set(lines[1:-1]),{token})
                self.assertNotIn('\\<!--',c['source'])
                self.assertTrue(c['provenance']['geometric']);self.assertNotIn('scaling_input',c)
                size=len(c['source'].encode());self.assertGreater(size,previous);self.assertLessEqual(size,262144);previous=size
        bounded=[c for c in perf.geometric([128,256,512]) if c['family'] in self.families]
        self.assertEqual([c['size']for c in bounded],[128,256,512]*2)

    def test_sources_match_six_fresh_reference_cached_requests(self):
        expected={
            ('default-callout-comment', 512): '9e9f52efc31b345a5295e9fb42e7c355e54addc572c728f7924c6f99947cbac3',
            ('default-callout-comment', 1024): '96d582327018fb905dfa353c03f77d4d962fe7e1647416c25adb40e374efcf4e',
            ('default-callout-comment', 4096): '6766d08631664f20bd8119b49ca8db852fd0c5d5031dbe1bd576761192633aef',
            ('default-callout-escaped', 512): '618e895b95334d58a4b4f449fe302b692862a4a579d886f4bce9b5a96c9a6c0a',
            ('default-callout-escaped', 1024): '3da50e55f79b52416d1bc2f6bb84e1110d9cfbec481f239e126137c85ca1ac16',
            ('default-callout-escaped', 4096): '92de77099b9b5864cafade9707ae67bd06376e63933d29686d7715836a831460',
        }
        cases=[c for c in perf.geometric([512,1024]) if (c['family'],c['size']) in expected]
        self.assertEqual({(c['family'],c['size']):perf.case_identity(c) for c in cases},expected)

    def test_supported_authored_predicates_distinguish_numbered_and_escaped(self):
        from review_asciidoc import expectation_errors
        cases={c['family']:c for c in perf.geometric([2]) if c['family'] in self.families}
        projection={'title':'','blocks':[{'context':'listing','blocks':[]}]}
        numbered={'projection':projection,'html':'code &lt;!--<b class="conum">(1)</b>--&gt;'}
        escaped={'projection':projection,'html':'code &lt;1&gt;'}
        self.assertEqual(expectation_errors(cases['default-callout-comment'],numbered),[])
        self.assertEqual(expectation_errors(cases['default-callout-escaped'],escaped),[])
        self.assertTrue(expectation_errors(cases['default-callout-comment'],escaped))
        self.assertTrue(expectation_errors(cases['default-callout-escaped'],numbered))

    def test_smoke_early_return_keeps_exact_two_requests(self):
        args=perf.parser().parse_args(['--smoke'])
        with mock.patch.object(perf,'geometric',side_effect=AssertionError('no geometry in smoke')),mock.patch.object(perf,'fixed_performance_guards',side_effect=AssertionError('no guards in smoke')):
            cases=list(perf.corpus(args))
        self.assertEqual(cases,[{'id':'smoke-paragraph','family':'smoke','source':'Hello *Roc*.'},
            {'id':'smoke-include-selectors','family':'smoke','source':'----\ninclude::part.adoc[lines=2..4]\ninclude::part.adoc[tag=chosen]\n----',
             'includes':{'part.adoc':'omitted\n// tag::chosen[]\n<code>\n// end::chosen[]\nomitted'}}])

if __name__ == '__main__': unittest.main()
