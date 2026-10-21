from __future__ import annotations

import hashlib
import unittest
from pathlib import Path

from scripts.import_roc_syntax_snapshots import snapshot_case, snapshot_cases


def snapshot(kind: str, source: str, meta: str = "") -> bytes:
    return (f"# META\n~~~ini\ntype={kind}\n{meta}~~~\n"
            f"# SOURCE\n~~~roc\n{source}~~~\n# PARSE\nNIL\n").encode()


class SnapshotImportTests(unittest.TestCase):
    def test_preserves_source_bytes_and_provenance(self):
        source = '# café\nvalue = "\\n"\r\n'
        case, reason = snapshot_case("test/snapshots/example.md", "a" * 40,
                                     snapshot("snippet", source, "skip=true\n"))
        self.assertIsNone(reason)
        self.assertEqual(case["source"], source)
        self.assertEqual(case["mode"], "file")
        self.assertEqual(case["coverage"], "exploratory")
        self.assertEqual(case["source_sha256"], hashlib.sha256(source.encode()).hexdigest())
        self.assertTrue(case["upstream"]["snapshot_skip"])

    def test_only_decodes_carriage_return_when_requested(self):
        source = '"\\r\\n\\t"\n'
        plain, _ = snapshot_case("example.md", "blob", snapshot("expr", source))
        escaped, _ = snapshot_case("example.md", "blob",
                                   snapshot("expr", source, "source_escapes=true\n"))
        self.assertEqual(plain["source"], source)
        self.assertEqual(escaped["source"], '"\r\\n\\t"\n')
        self.assertEqual(escaped["mode"], "expr")

    def test_named_file_snapshot_uses_file_parser_without_source_rewriting(self):
        source = 'Thing := [Ready].{\n value = 1\n}\n'
        case, reason = snapshot_case('named.md', 'blob', snapshot('file:Thing.roc', source))
        self.assertIsNone(reason)
        self.assertEqual(case['mode'], 'file')
        self.assertEqual(case['source'], source)
        self.assertEqual(case['upstream']['type'], 'file:Thing.roc')

    def test_reports_unhandled_snapshot_modes(self):
        case, reason = snapshot_case("repl.md", "blob", snapshot("repl", "1 + 2\n"))
        self.assertIsNone(case)
        self.assertEqual(reason, "unsupported_snapshot_mode:repl")

    def test_missing_source_section_fails(self):
        with self.assertRaisesRegex(ValueError, "SOURCE"):
            snapshot_case("bad.md", "blob", b"# META\n~~~ini\ntype=expr\n~~~\n")


class PinnedExtractionTests(unittest.TestCase):
    # Full immutable blobs from reference 0d4b0ab0..., not invented transcripts.
    FIXTURES = {
        'repl/two_defs.md': '4a12ea85e693a48f6a257b8e3bd90fb403780882',
        'dev_object_hello_world.md': '6e14655dd70810f065ebfa755b24dd0d8dfc2ab2',
        'docs_complex_inferred_types.md': '9541d05df5bf9fb114e1953e023c2845b5ef9b5a',
    }

    def extract(self, name):
        data = (Path(__file__).parent / 'data' / 'roc-snapshots' / name.replace('/', '_')).read_bytes()
        blob = self.FIXTURES[name]
        self.assertEqual(hashlib.sha1(b'blob ' + str(len(data)).encode() + b'\0' + data).hexdigest(), blob)
        cases, reason = snapshot_cases('test/snapshots/' + name, blob, data)
        self.assertIsNone(reason)
        for case in cases:
            audit = case['upstream']['extraction']
            extracted = data[audit['snapshot_byte_start']:audit['snapshot_byte_end']]
            self.assertEqual(extracted, case['source'].encode())
            self.assertEqual(hashlib.sha256(extracted).hexdigest(), case['source_sha256'])
            self.assertEqual(case['upstream']['blob'], blob)
        return cases

    def test_pinned_repl_inputs_follow_ascii_trim_and_preserve_each_input(self):
        cases = self.extract('repl/two_defs.md')
        self.assertEqual([(c['mode'], c['source']) for c in cases],
                         [('repl', 'x = 1'), ('repl', 'y = 2'), ('repl', 'x + y')])
        self.assertEqual(len({c['id'] for c in cases}), 3)

    def test_pinned_dev_object_extracts_each_file_without_codegen(self):
        cases = self.extract('dev_object_hello_world.md')
        self.assertEqual([c['upstream']['filename'] for c in cases], ['app.roc', 'platform.roc'])
        self.assertTrue(all(c['mode'] == 'file' for c in cases))
        self.assertIn('main = "Hello, World!"\n', cases[0]['source'])

    def test_pinned_docs_do_not_mistake_doc_comments_for_file_headings(self):
        cases = self.extract('docs_complex_inferred_types.md')
        self.assertEqual(len(cases), 2)
        self.assertIn('## Makes a record from components.\n', cases[0]['source'])

    def test_exact_header_and_statement_modes_preserve_source(self):
        for mode, source in [('header', 'module []\n'), ('statement', 'return 1\n')]:
            cases, reason = snapshot_cases('test.md', 'blob', snapshot(mode, source))
            self.assertIsNone(reason)
            self.assertEqual([(c['mode'], c['source']) for c in cases], [(mode, source)])

    def test_repl_split_is_literal_delimiter_and_ascii_trim_only(self):
        cases, reason = snapshot_cases('test.md', 'blob', snapshot('repl', 'preamble» a »\t»\u00a0b\u00a0\n'))
        self.assertIsNone(reason)
        self.assertEqual([c['source'] for c in cases], ['a', '\u00a0b\u00a0'])

    def test_repl_carriage_return_decode_precedes_trimming(self):
        data = snapshot('repl', '» first\\r» a\\rb\\r\n', 'source_escapes=true\n')
        cases, reason = snapshot_cases('escaped.md', 'blob', data)
        self.assertIsNone(reason)
        self.assertEqual([c['source'] for c in cases], ['first', 'a\rb'])
        audit = cases[1]['upstream']['extraction']
        self.assertEqual(data[audit['snapshot_byte_start']:audit['snapshot_byte_end']], b'a\\rb')
        self.assertTrue(audit['source_escapes'])

    def test_broken_multifile_or_empty_repl_is_not_silent_success(self):
        data = b'# META\n~~~ini\ntype=docs\n~~~\n# SOURCE\n## broken.roc\n'
        with self.assertRaisesRegex(ValueError, 'unhandled multi-file'):
            snapshot_cases('broken.md', 'blob', data)
        cases, reason = snapshot_cases('empty.md', 'blob', snapshot('repl', '» \n'))
        self.assertEqual(cases, [])
        self.assertEqual(reason, 'empty_parse_input_inventory:repl')


if __name__ == "__main__":
    unittest.main()
