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
            self.assertEqual(row['correctness'],{'projection':True,'rendered_dom':True})
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
