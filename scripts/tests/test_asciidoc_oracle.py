"""Protocol/resource regression tests; integration tests use the real pinned gem."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / 'review_asciidoc.py'
spec = importlib.util.spec_from_file_location('review_asciidoc', SCRIPT)
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)


class ProtocolTests(unittest.TestCase):
    def fake(self, program, timeout=.4):
        return review.Oracle(timeout=timeout, command=[sys.executable, '-c', program])

    def test_bad_json_stops_worker(self):
        with self.fake('import sys;sys.stdin.readline();print("oops",flush=True)') as oracle:
            self.assertEqual(oracle.request('abc')['status'], 'protocol_error')
            self.assertIsNotNone(oracle.process.poll())

    def test_wrong_id(self):
        with self.fake('import sys;sys.stdin.readline();print(\'{"id":99,"status":"ok"}\',flush=True)') as oracle:
            self.assertEqual(oracle.request('abc')['status'], 'protocol_error')

    def test_deadline_stops_worker(self):
        with self.fake('import time;time.sleep(30)', timeout=.05) as oracle:
            self.assertEqual(oracle.request('abc')['status'], 'timeout')
            self.assertIsNotNone(oracle.process.poll())

    def test_output_bound_stops_worker(self):
        with self.fake('import sys;sys.stdin.readline();sys.stdout.write("x"*9000000);sys.stdout.flush()') as oracle:
            self.assertEqual(oracle.request('abc')['status'], 'protocol_error')

    def test_context_manager_reaps_process(self):
        with self.fake('import time;time.sleep(30)') as oracle:
            process = oracle.process
        self.assertIsNotNone(process.poll())
        self.assertTrue(process.stdout.closed)

    def test_source_bound(self):
        with self.fake('import time;time.sleep(30)') as oracle:
            self.assertEqual(oracle.request('x' * (review.MAX_SOURCE + 1))['status'], 'protocol_error')

    def test_projection_types_strict(self):
        for value in ({'title': '', 'blocks': [], 'extra': 0}, {'title': False, 'blocks': []}, {'title': '', 'blocks': [{'context': 'paragraph', 'level': True, 'title': '', 'source': '', 'blocks': []}]}):
            with self.assertRaises(ValueError):
                review.canonical_projection(value)

    def test_future_is_not_conformance_pass(self):
        projection = {'title': '', 'blocks': []}
        oracle = {'status': 'ok', 'projection': projection, 'html': ''}
        actual = {'status': 'ok', 'projection': projection}
        self.assertEqual(review.classify({'support': 'future'}, oracle, actual)[0], 'structural_only')
        self.assertEqual(review.classify({'support': 'future', 'reject': True}, oracle, actual)[0], 'unexpected_acceptance')
        self.assertEqual(review.classify({'support': 'future'}, oracle, {'status': 'unsupported'})[0], 'unsupported')

    def test_authored_expectation_is_checked_without_probe(self):
        oracle = {'status': 'ok', 'projection': {'title': '', 'blocks': []}, 'html': ''}
        self.assertEqual(review.classify({'expect': {'html_contains': ['<em>']}}, oracle)[0], 'oracle_disagreement')

    def test_warning_expectations(self):
        oracle = {'status': 'ok', 'projection': {'title': '', 'blocks': []}, 'html': '', 'warnings': [{'message': 'unterminated listing block', 'severity': 'WARN'}]}
        case = {'expect': {'warnings_contains': ['unterminated'], 'warning_severities': ['WARN']}}
        self.assertEqual(review.classify(case, oracle)[0], 'oracle_verified')
        case['expect']['warnings_contains'] = ['missing']
        self.assertEqual(review.classify(case, oracle)[0], 'oracle_disagreement')

    def test_manifest_missing_required_case_fails(self):
        fixture = {'schema_version': 1, 'oracle_version': review.VERSION, 'coverage': [{'category': 'core', 'required_cases': ['missing']}], 'cases': [{'id': 'one', 'category': 'core', 'source': '', 'support': 'minimal', 'expect': {'projection': {'title': '', 'blocks': []}}}]}
        with self.assertRaises(ValueError):
            review.validate_fixture(fixture)
        fixture['coverage'][0]['required_cases'] = ['one']
        self.assertEqual(len(review.validate_fixture(fixture)), 1)

    def test_missing_probe_fingerprint(self):
        self.assertIsNone(review.file_sha256(Path('/nonexistent/asciidoc-probe')))

    def test_probe_missing_and_not_executable(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / 'probe'
            self.assertEqual(review.run_probe(executable, '')['status'], 'protocol_error')
            executable.write_text('not executable')
            self.assertEqual(review.run_probe(executable, '')['status'], 'protocol_error')

    def test_only_unasserted_exploration_can_exclude_include_policy_rejection(self):
        rejection = {'status': 'oracle_policy_rejection', 'error': 'VirtualIncludePolicyError'}
        self.assertEqual(review.classify({'support': 'explore'}, rejection)[0], 'oracle_policy_rejection')
        self.assertEqual(review.classify({'support': 'future', 'expect': {'contexts': ['paragraph']}}, rejection)[0], 'oracle_policy_disagreement')
        self.assertEqual(review.classify({'support': 'minimal'}, rejection)[0], 'oracle_policy_disagreement')
        self.assertEqual(review.classify({'support': 'explore', 'expect': {'html_contains': ['yes']}}, rejection)[0], 'oracle_policy_disagreement')
        self.assertEqual(review.classify({'support': 'explore'}, rejection, {'status': 'crash'})[0], 'crash')
        self.assertEqual(review.classify({'support': 'explore'}, {'status': 'oracle_error'})[0], 'oracle_error')
        oracle = {'status': 'ok', 'projection': {'title': '', 'blocks': []}, 'html': '', 'warnings': []}
        self.assertEqual(review.classify({'support': 'explore', 'expect': {'html_contains': ['wrong']}}, oracle)[0], 'oracle_disagreement')

    def test_probe_timeout_crash_protocol(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / 'probe'
            for code, status in [('import time;time.sleep(30)', 'timeout'), ('raise SystemExit(3)', 'crash'), ('print("not json")', 'protocol_error')]:
                executable.write_text('#!' + sys.executable + '\n' + code + '\n')
                executable.chmod(0o755)
                self.assertEqual(review.run_probe(executable, 'x', timeout=.05 if status == 'timeout' else 2)['status'], status)


@unittest.skipUnless((review.DEFAULT_HOME / 'gems/asciidoctor-2.0.23').is_dir(), 'run setup_oracle.py for pinned integration tests')
class RealOracleTests(unittest.TestCase):
    def test_preamble_and_titleless_sections(self):
        with review.Oracle() as oracle:
            result = oracle.request('= Title\n\nBefore\n\n== Section\n\nAfter')
            self.assertEqual(result['status'], 'ok')
            self.assertEqual(review.contexts(result['projection']), ['paragraph', 'section', 'paragraph'])
            result = oracle.request('== Section\n\nWords')
            self.assertEqual(result['projection']['title'], '')

    def test_request_isolation_and_dates(self):
        with review.Oracle() as oracle:
            oracle.request(':flavour: vanilla\n\n{flavour}')
            result = oracle.request('{flavour} {docdate} {localdate}')
            self.assertIn('{flavour} 2000-01-01 2000-01-01', result['html'])
            self.assertNotIn('vanilla', result['html'])
            self.assertEqual(result, {**oracle.request('{flavour} {docdate} {localdate}'), 'id': result['id']})

    def test_secure_includes_do_not_read_paths(self):
        with tempfile.TemporaryDirectory() as directory:
            secret = Path(directory) / 'secret.adoc'
            secret.write_text('SECRET-FILE-CONTENT')
            with review.Oracle() as oracle:
                result = oracle.request(f'include::{secret}[]\n\ninclude::https://example.invalid/test[]')
                self.assertEqual(result['status'], 'ok')
                self.assertNotIn('SECRET-FILE-CONTENT', result['html'])
                self.assertIn('role=include', result['projection']['blocks'][0]['source'])
                self.assertTrue(result['unsupported'])

    def test_rich_lists_tables_and_warnings(self):
        with review.Oracle() as oracle:
            result = oracle.request('Term:: Description\n\n[cols="1,1"]\n|===\n|A |B\n|===')
            self.assertEqual(result['status'], 'ok', result)
            self.assertEqual(review.contexts(result['projection']), ['dlist', 'list_item', 'list_item', 'table'])
            self.assertEqual(result['ast']['blocks'][1]['rows']['body'][0][0]['text'], 'A')
            self.assertIn('<table', result['html'])
            first = oracle.request('== One\n\n==== Skipped')['warnings']
            self.assertEqual(first, oracle.request('== One\n\n==== Skipped')['warnings'])
            self.assertIn('out of sequence', first[0]['message'])
            self.assertEqual(first[0]['source_location']['line'], 3)

    def test_description_term_without_definition_regression(self):
        with review.Oracle() as oracle:
            result = oracle.request(':show:\n\nifdef::')
            self.assertEqual(result['status'], 'ok', result)
            self.assertEqual(result['ast']['blocks'][0]['items'][0][1], None)
            self.assertEqual(review.contexts(result['projection']), ['dlist', 'list_item'])

    def test_virtual_includes_basic_nested_conditionals_and_isolation(self):
        sources = {'parts/start.adoc': ':feature:\n\ninclude::next.adoc[]', 'parts/next.adoc': 'ifdef::feature[]\nIncluded *text*\nendif::[]'}
        with review.Oracle() as oracle:
            result = oracle.request('include::parts/start.adoc[]', sources)
            self.assertEqual(result['status'], 'ok', result)
            self.assertEqual(result['include_policy'], 'virtual-map-secure')
            self.assertIn('Included <strong>text</strong>', result['html'])
            self.assertEqual(result['ast']['attributes']['safe-mode-level'], 20)
            result = oracle.request('include::parts/start.adoc[]')
            self.assertEqual(result['include_policy'], 'secure-no-expansion')
            self.assertNotIn('Included <strong>', result['html'])

    def test_virtual_includes_fail_closed_for_paths_and_missing(self):
        with review.Oracle() as oracle:
            for target, includes, message in [
                ('missing.adoc[]', {}, 'not supplied'),
                ('https://example.org/input[]', {}, 'prohibit'),
                ('/etc/passwd[]', {}, 'prohibit'),
                ('../outside.adoc[]', {}, 'escapes root'),
            ]:
                result = oracle.request('include::' + target, includes)
                self.assertEqual(result['status'], 'oracle_policy_rejection', result)
                self.assertIn(message, result['message'])
            self.assertEqual(oracle.request('Fine')['status'], 'ok')

    def test_virtual_includes_use_native_line_and_tag_selectors(self):
        with review.Oracle() as oracle:
            result = oracle.request('include::one.adoc[lines=2..3]', {'one.adoc': 'A\nB\nC\nD'})
            self.assertEqual(result['status'], 'ok', result)
            self.assertEqual(result['projection']['blocks'][0]['source'], 'B\nC')
            result = oracle.request('include::one.adoc[tag=body]', {'one.adoc': 'Before\n// tag::body[]\nSelected\n// end::body[]\nAfter'})
            self.assertEqual(result['status'], 'ok', result)
            self.assertEqual(result['projection']['blocks'][0]['source'], 'Selected')
            self.assertEqual(result['ast']['attributes']['safe-mode-level'], 20)

    def test_virtual_includes_cycles_depth_and_expansion_bound(self):
        with review.Oracle() as oracle:
            result = oracle.request('include::loop.adoc[]', {'loop.adoc': 'include::loop.adoc[]'})
            self.assertEqual(result['status'], 'oracle_policy_rejection', result)
            self.assertIn('cycle', result['message'])
            sources = {str(index) + '.adoc': 'include::' + str(index + 1) + '.adoc[]' for index in range(18)}
            result = oracle.request('include::0.adoc[]', sources)
            self.assertEqual(result['status'], 'oracle_policy_rejection', result)
            self.assertIn('maximum virtual include depth', result['message'])
            result = oracle.request('include::large.adoc[]\n\n' * 7, {'large.adoc': 'x' * 200000})
            self.assertEqual(result['status'], 'oracle_policy_rejection', result)
            self.assertIn('expansion byte limit', result['message'])

    def test_virtual_include_request_total_bound(self):
        with review.Oracle() as oracle:
            result = oracle.request('include::large.adoc[]', {'large.adoc': 'x' * review.MAX_SOURCE})
            self.assertEqual(result['status'], 'protocol_error')

    def test_malformed_request_then_valid(self):
        with review.Oracle() as oracle:
            response = json.loads(review.bounded_exchange(oracle.process, b'{oops}\n', 2, line=True))
            self.assertEqual(response['status'], 'error')
            self.assertEqual(oracle.request('Fine')['status'], 'ok')


if __name__ == '__main__':
    unittest.main()
