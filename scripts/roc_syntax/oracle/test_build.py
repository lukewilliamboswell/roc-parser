"""Archive trust boundary and optional live worker contract checks."""
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile
import unittest
from unittest.mock import patch
from types import SimpleNamespace

spec = importlib.util.spec_from_file_location("build_oracle", Path(__file__).resolve().parents[1] / "build_oracle.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


def archive(name, kind=tarfile.REGTYPE):
    out = io.BytesIO()
    with tarfile.open(fileobj=out, mode="w") as bundle:
        item = tarfile.TarInfo(name)
        item.type = kind
        item.size = 1 if kind == tarfile.REGTYPE else 0
        item.linkname = "/tmp/untrusted"
        bundle.addfile(item, io.BytesIO(b"x") if item.size else None)
    return out.getvalue()


class ArchiveTests(unittest.TestCase):
    def test_extracts_source(self):
        with tempfile.TemporaryDirectory() as tmp:
            module.extract_source(archive("src/parse/mod.zig"), Path(tmp))
            self.assertEqual((Path(tmp) / "src/parse/mod.zig").read_bytes(), b"x")

    def test_cached_source_is_replaced_from_git_objects(self):
        with tempfile.TemporaryDirectory() as tmp:
            cache = Path(tmp)
            replies = [SimpleNamespace(stdout=module.ROC_REVISION + "\n"), SimpleNamespace(stdout=archive("src/original.zig"))]
            with patch.object(module, "run", side_effect=replies):
                source = module.source_tree(cache, cache / "objects")
            (source / "src/original.zig").write_text("mutated")
            (source / "src/injected.zig").write_text("injected")
            with patch.object(module, "run", side_effect=replies):
                rebuilt = module.source_tree(cache, cache / "objects")
            self.assertEqual((rebuilt / "src/original.zig").read_bytes(), b"x")
            self.assertFalse((rebuilt / "src/injected.zig").exists())

    def test_rejects_escape_and_links(self):
        for name, kind in [("../escape", tarfile.REGTYPE), ("/src/escape", tarfile.REGTYPE), ("src/../../escape", tarfile.REGTYPE), ("vendor/file", tarfile.REGTYPE), ("src/link", tarfile.SYMTYPE), ("src/link", tarfile.LNKTYPE)]:
            with self.subTest(name=name, kind=kind), tempfile.TemporaryDirectory() as tmp:
                with self.assertRaises(ValueError):
                    module.extract_source(archive(name, kind), Path(tmp))


@unittest.skipUnless(os.environ.get("ORACLE_BINARY"), "set ORACLE_BINARY for live worker tests")
class WorkerTests(unittest.TestCase):
    def test_platform_requires_symbols_and_target_configuration(self):
        base = 'platform "p" requires {} exposes [] packages {} provides {}'
        prefix = [['string', '"p"'], ['platform_requires', []], ['dependencies', []], ['platform_provides', []]]
        cases = [
            (base, ['header', 'platform', prefix]),
            (base + ' hosted {}', ['header', 'platform', prefix + [['platform_hosted', []]]]),
            (base + ' targets: {}', ['header', 'platform', prefix + [['platform_targets', []]]]),
            (base + ' targets: {linux:{inputs:["crt.o",app],output:Exe,custom}}', ['header', 'platform', prefix + [['platform_targets', [
                ['target_entry', 'linux', ['target_config', [
                    ['target_field', 'inputs', ['target_list', [['target_file', ['string', '"crt.o"']], ['target_file', ['identifier', 'app']]]]],
                    ['target_field', 'output', ['identifier', 'Exe']], ['target_field', 'custom', None],
                ]]],
            ]]]]),
            (base + ' targets: {linux:{},inputs_dir:"a",inputs_dir:"b"}', ['header', 'platform', prefix + [['platform_targets', [
                ['target_inputs_directory', ['string', '"b"']], ['target_entry', 'linux', ['target_config', []]],
            ]]]]),
            (base + ' targets: {linux:{value:"${x}"}}', ['header', 'platform', prefix + [['platform_targets', [
                ['target_entry', 'linux', ['target_config', [['target_field', 'value', ['string', '"${x}"']]]]],
            ]]]]),
            ('platform "" requires {[Model:model] for main : () -> Model} exposes [run] packages {} provides {"":Module.run}', ['header', 'platform', [
                ['string', '""'], ['platform_requires', [['required_value', [
                    ['required_type', ['t_named', ['identifier', 'Model']], ['t_var', 'model']],
                ], 'main', ['t_fn', False, [], ['t_named', ['identifier', 'Model']]]]]],
                ['exposed', ['identifier', 'run'], None], ['dependencies', []], ['platform_provides', [['symbol_mapping', '""', 'Module.run']]],
            ]]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': 'header', 'source': source}) + '\n' for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], [expected])

    def test_record_updates_unset_fields_and_nominal_forms(self):
        number = ['number', '1']
        fields = [['x', number]]
        foo = ['identifier', 'Foo']
        cases = [
            ('{x: _}', ['record', [['x', ['unset']]]]),
            ('{..base,x:1}', ['record_update', ['identifier', 'base'], fields]),
            ('{x:1}.Foo', ['record_builder', foo, fields]),
            ('Foo.{x:1}', ['nominal_record', foo, ['record', fields]]),
            ('Foo.({x:1})', ['nominal_apply', foo, [['record', fields]]]),
            ('Foo.(1)', ['nominal_apply', foo, [number]]),
            ('Foo.()', ['nominal_apply', foo, []]),
            ('Foo.(1,2)', ['nominal_apply', foo, [number, ['number', '2']]]),
            ('Module.Foo.(1)', ['nominal_apply', ['field', ['identifier', 'Module'], 'Foo'], [number]]),
            ('Module.Foo.{..base,x:_}', ['nominal_record', ['field', ['identifier', 'Module'], 'Foo'], ['record_update', ['identifier', 'base'], [['x', ['unset']]]]]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': 'expr', 'source': source}) + '\n' for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], [expected])

    def test_current_reference_source_policy_and_dependency_rules(self):
        cases = [
            ('expr', '{1\n...}', True),
            ('expr', '-...', True),
            ('expr', '\ufeff1', False),
            ('expr', '1 #\u202e', False),
            ('expr', '"\u202e"', False),
            ('header', 'package [] {dep}', False),
            ('header', 'package [] {dep: 1}', False),
            ('header', 'app [] {dep: "a", dep: "b"}', False),
            ('header', 'package [] {dep: "a\\nb"}', False),
            ('header', 'package [] {dep: "${name}"}', False),
            ('header', 'package [] {dep: "../dep/main.roc"}', True),
            ('header', 'app [] {pf: platform "../platform/main.roc"}', True),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': mode, 'source': source}) + '\n' for i, (mode, source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (mode, source, expected), row in zip(cases, rows):
            with self.subTest(mode=mode, source=source):
                self.assertEqual(row['ok'], expected, row['diagnostics'])
                if not expected:
                    self.assertTrue(row['diagnostics'])
        self.assertEqual(rows[0]['facts'][0]['shape'], ['block', [['number', '1'], ['ellipsis']]])
        self.assertEqual(rows[1]['facts'][0]['shape'], ['unary', '-', ['ellipsis']])

    def test_types_extensions_constraints_and_associated_definitions(self):
        named = lambda name: ['t_named', ['identifier', name]]
        var = lambda name: ['t_var', name]
        cases = [
            ('f : a', ['annotation', 'f', var('a'), None, False]),
            ('f : _a', ['annotation', 'f', var('_a'), None, False]),
            ('f : _', ['annotation', 'f', ['t_wildcard'], None, False]),
            ('f : List(U8)', ['annotation', 'f', ['t_apply', [named('List'), named('U8')]], None, False]),
            ('f : a -> b', ['annotation', 'f', ['t_fn', False, [var('a')], var('b')], None, False]),
            ('f : () => U8', ['annotation', 'f', ['t_fn', True, [], named('U8')], None, False]),
            ('f : (U8, Str)', ['annotation', 'f', ['t_tuple', [named('U8'), named('Str')]], None, False]),
            ('f : (U8)', ['annotation', 'f', named('U8'), None, False]),
            ('f : Foo.Bar', ['annotation', 'f', ['t_named', ['field', ['identifier', 'Foo'], 'Bar']], None, False]),
            ('f : {a?: U8, b: Str ?? "x", ..ext}', ['annotation', 'f', ['t_record', [
                ['t_field', 'a', True, named('U8'), None], ['t_field', 'b', False, named('Str'), ['string', '"x"']]], ['t_extension', var('ext')]], None, False]),
            ('f : [A, B(U8), ..]', ['annotation', 'f', ['t_union', [named('A'), ['t_apply', [named('B'), named('U8')]]], ['t_open']], None, False]),
            ('Foo : U8', ['type_decl', 'alias', ['type_header', 'Foo', []], named('U8'), None, None]),
            ('Foo(a) := {value: a}', ['type_decl', 'nominal', ['type_header', 'Foo', [var('a')]], ['t_record', [['t_field', 'value', False, var('a'), None]], None], None, None]),
            ('Foo :: [A]', ['type_decl', 'opaque', ['type_header', 'Foo', []], ['t_union', [named('A')], None], None, None]),
            ('f : a -> a where [a.copy : a -> a]', ['annotation', 'f', ['t_fn', False, [var('a')], var('a')], ['where', [
                ['where_method', var('a'), 'copy', ['t_fn', False, [var('a')], var('a')]]]], False]),
            ('f : a where [a.Sort]', ['annotation', 'f', var('a'), ['where', [['where_alias', var('a'), named('Sort')]]], False]),
            ('a.Sort : where [a.copy : a -> a]', ['type_decl', 'where_alias', ['type_header', 'Sort', []], var('a'), ['where', [
                ['where_method', var('a'), 'copy', ['t_fn', False, [var('a')], var('a')]]]], None]),
            ('Foo := [A].{ make = A }', ['type_decl', 'nominal', ['type_header', 'Foo', []], ['t_union', [named('A')], None], None, ['associated', [['decl', ['p_identifier', 'make'], ['identifier', 'A']]]]]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': 'file', 'source': source}) + '\n' for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], [expected])

    def test_app_package_headers_and_string_pattern_captures(self):
        cases = [
            ('header', '', ['header', 'type_module', []]),
            ('header', 'app [] {}', ['header', 'app', [['dependencies', []]]]),
            ('header', 'package [Foo]', ['header', 'package', [['exposed', ['identifier', 'Foo'], None], ['dependencies', []]]]),
            ('header', 'app [main!] {pf: platform "x"}', ['header', 'app', [['exposed', ['identifier', 'main!'], None], ['dependencies', [['dependency', 'pf', ['platform', ['string', '"x"']]]]]]]),
            ('header', 'package [] {util: "../util/main.roc"}', ['header', 'package', [['dependencies', [['dependency', 'util', ['string', '"../util/main.roc"']]]]]]),
            ('expr', '|"hello ${name}"| name', ['lambda', [['p_string_parts', [['text', 'hello '], ['capture', 'name']]]], ['identifier', 'name']]),
            ('expr', '|"${_}"| 1', ['lambda', [['p_string_parts', [['capture', None]]]], ['number', '1']]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': mode, 'source': source}) + '\n' for i, (mode, source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (_, source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], [expected])

    def test_header_statement_and_repl_modes_are_direct_and_strict(self):
        cases = [
            ('header', 'module [run]', True, True),
            ('header', 'hosted [main!]', True, True),
            ('header', 'module []\nx = 1', False, False),
            ('statement', 'var x = 1', True, True),
            ('file', 'var x = 1', False, True),
            ('statement', '1 + 2', True, True),
            ('statement', 'x = 1\ny = 2', False, False),
            ('repl', 'x = 1', True, True),
            ('repl', 'var x = 1', True, True),
            ('repl', '1 + 2', True, True),
            ('repl', 'dbg 1', True, True),
            ('repl', 'x = 1\ny = 2', False, False),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': mode, 'source': source}) + '\n' for i, (mode, source, _, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (mode, source, ok, consumed), row in zip(cases, rows):
            with self.subTest(mode=mode, source=source):
                self.assertEqual(row['ok'], ok, row['diagnostics'])
                self.assertEqual(row['consumed'], consumed)
        self.assertEqual(rows[0]['facts'][0]['shape'], ['header', 'module', [['exposed', ['identifier', 'run'], None]]])
        self.assertEqual(rows[3]['facts'][0]['shape'], ['var', 'x', ['number', '1']])

    def test_import_target_alias_exposes_and_file_content_type(self):
        cases = [
            ('import Foo', ['import', 'Foo', None, []]),
            ('import json.Json', ['import', 'json.Json', None, []]),
            ('import json.Json as MyJson exposing [parse, Value]', ['import', 'json.Json', 'MyJson', [
                ['exposed', ['identifier', 'parse'], None], ['exposed', ['identifier', 'Value'], None]]]),
            ('import http.Client.Auth as Auth exposing [get as fetch, Request.*]', ['import', 'http.Client.Auth', 'Auth', [
                ['exposed', ['identifier', 'get'], 'fetch'], ['exposed_wildcard', ['identifier', 'Request']]]]),
            ('import ../../lib/Parser.Config', ['import', '../../lib/Parser.Config', None, []]),
            ('import /root/Module', ['import', '/root/Module', None, []]),
            ('import ./relative/Module', ['import', './relative/Module', None, []]),
            ('import "data.txt" as text : Str', ['file_import', 'data.txt', 'text', 'Str']),
            ('import "bytes.bin" as data : List ( U8 )', ['file_import', 'bytes.bin', 'data', 'List(U8)']),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': 'file', 'source': source}) + '\n' for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], [expected])

    def test_statement_binding_and_control_structure(self):
        cases = [
            ('file', 'module []\nx = 1', [['header', 'module', []], ['decl', ['p_identifier', 'x'], ['number', '1']]]),
            ('file', 'expect True', [['expect', ['identifier', 'True']]]),
            ('file', 'dbg 1', [['dbg', ['number', '1']]]),
            ('expr', '{var x = 1\nx}', [['block', [['var', 'x', ['number', '1']], ['identifier', 'x']]]]),
            ('expr', '{var x\nx}', [['block', [['var', 'x', None], ['identifier', 'x']]]]),
            ('expr', '{expect True\n1}', [['block', [['expect', ['identifier', 'True']], ['number', '1']]]]),
            ('expr', '{while True { break }\n1}', [['block', [['while', ['identifier', 'True'], ['block', [['break']]]], ['number', '1']]]]),
            ('expr', '{for x in items { dbg x }\n1}', [['block', [['for', ['p_identifier', 'x'], ['identifier', 'items'], ['block', [['dbg', ['identifier', 'x']]]]], ['number', '1']]]]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': mode, 'source': source}) + '\n' for i, (mode, source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (_, source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual([fact['shape'] for fact in row['facts']], expected)

    def test_structural_pattern_projections(self):
        identifier = ['p_identifier', 'x']
        cases = [
            ('|_| 1', ['p_wildcard']),
            ('|var x| 1', ['p_mutable', 'x']),
            ('|var $x| 1', ['p_mutable', '$x']),
            ('|Some(x)| 1', ['p_tag', ['identifier', 'Some'], True, [identifier]]),
            ('|Some()| 1', ['p_tag', ['identifier', 'Some'], True, []]),
            ('|Some| 1', ['p_tag', ['identifier', 'Some'], False, []]),
            ('|Foo.Bar(x)| 1', ['p_tag', ['field', ['identifier', 'Foo'], 'Bar'], True, [identifier]]),
            ('|Type.(x)| 1', ['p_nominal', ['identifier', 'Type'], True, [identifier], False]),
            ('|Type.{x}| 1', ['p_nominal', ['identifier', 'Type'], True, [['p_record', [['p_field', 'x', None]]]], True]),
            ('|(x,_)| 1', ['p_tuple', [identifier, ['p_wildcard']]]),
            ('|[x,.. as rest]| 1', ['p_list', [identifier, ['p_rest', 'rest']]]),
            ('|{x,y: z,..}| 1', ['p_record', [['p_field', 'x', None], ['p_field', 'y', ['p_identifier', 'z']], ['p_rest', None]]]),
            ('|Some(x) as whole| 1', ['p_alias', ['p_tag', ['identifier', 'Some'], True, [identifier]], 'whole']),
            ('|42| 1', ['number', '42']),
            ('|"text"| 1', ['string', '"text"']),
            ("|'é'| 1", ['character', "'é'"]),
        ]
        requests = ''.join(json.dumps({'id': str(i), 'mode': 'expr', 'source': source}) + '\n' for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ['ORACLE_BINARY']], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row['ok'], row['diagnostics'])
                self.assertEqual(row['facts'][0]['shape'], ['lambda', [expected], ['number', '1']])

    def test_persistent_shapes_errors_and_consumption(self):
        requests = [
            {"id": "integer-snapshot", "mode": "expr", "source": "42\n"},
            {"id": "addition-snapshot", "mode": "expr", "source": "1 + 2\n"},
            {"id": "precedence", "mode": "expr", "source": "1 + 2 * 3"},
            {"id": "left-assoc", "mode": "expr", "source": "8 - 3 - 1"},
            {"id": "trailing", "mode": "expr", "source": "1 2"},
            {"id": "file", "mode": "file", "source": "module []\nanswer = 42\n"},
            {"id": "bad", "mode": "expr", "source": "[1,"},
            {"id": "group", "mode": "expr", "source": "(1 + 2) * 3"},
            {"id": "list", "mode": "expr", "source": "[1, 2]"},
            {"id": "string", "mode": "expr", "source": '\"hello\"'},
            {"id": "unary", "mode": "expr", "source": "-answer"},
        ]
        result = subprocess.run([os.environ["ORACLE_BINARY"]], input="".join(json.dumps(x) + "\n" for x in requests), text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual([r["id"] for r in rows], [r["id"] for r in requests])
        self.assertEqual(rows[0]["facts"][0]["shape"], ["number", "42"])
        self.assertEqual(rows[1]["facts"][0]["shape"], ["binary", "+", ["number", "1"], ["number", "2"]])
        self.assertEqual(rows[2]["facts"][0]["shape"], ["binary", "+", ["number", "1"], ["binary", "*", ["number", "2"], ["number", "3"]]])
        self.assertEqual(rows[3]["facts"][0]["shape"], ["binary", "-", ["binary", "-", ["number", "8"], ["number", "3"]], ["number", "1"]])
        self.assertFalse(rows[4]["consumed"])
        self.assertFalse(rows[4]["ok"])
        self.assertTrue(rows[5]["ok"])
        self.assertEqual(rows[5]["facts"][0]["kind"], "header")
        self.assertEqual(rows[5]["facts"][0]["end"], len('module []'))
        self.assertEqual(rows[5]["facts"][1]["kind"], "definition")
        self.assertFalse(rows[6]["ok"])
        self.assertEqual(rows[7]["facts"][0]["shape"], ["binary", "*", ["binary", "+", ["number", "1"], ["number", "2"]], ["number", "3"]])
        self.assertEqual(rows[8]["facts"][0]["shape"], ["list", [["number", "1"], ["number", "2"]]])
        self.assertEqual(rows[9]["facts"][0]["shape"], ["string", '\"hello\"'])
        self.assertEqual(rows[10]["facts"][0]["shape"], ["unary", "-", ["identifier", "answer"]])
        for row in rows:
            self.assertEqual(row["version"], 1)
            for token in row["tokens"]:
                self.assertLessEqual(token["start"], token["end"])

    def test_common_expression_projections(self):
        cases = [
            ("True", ["identifier", "True"]),
            ('"a ${1 + 2} b"', ['interpolated_string', [['text', 'a '], ['interpolation', ['binary', '+', ['number', '1'], ['number', '2']]], ['text', ' b']]]),
            ('"${name}"', ['interpolated_string', [['interpolation', ['identifier', 'name']]]]),
            ('"""hello', ['multiline_string', [['text', 'hello']]]),
            ('"""hello ${name}', ['multiline_string', [['text', 'hello '], ['interpolation', ['identifier', 'name']]]]),
            ('\\\\hello\n\\\\world', ['multiline_string', [['text', 'hello'], ['text', 'world']]]),
            ('123.U64', ['typed', ['number', '123'], ['U64']]),
            ('3.14.Dec', ['typed', ['number', '3.14'], ['Dec']]),
            ("'é'.U8", ['typed', ['character', "'é'"], ['U8']]),
            ('"hello".Str', ['typed', ['string', '"hello"'], ['Str']]),
            ('123.Module.Integer', ['typed', ['number', '123'], ['Module', 'Integer']]),
            ('"${name}".Str', ['typed', ['interpolated_string', [['interpolation', ['identifier', 'name']]]], ['Str']]),
            ('x -> f(1)', ['arrow', '->', ['identifier', 'x'], ['call', ['identifier', 'f'], [['number', '1']]], 'ordinary']),
            ('x |> f(1)', ['arrow', '|>', ['identifier', 'x'], ['call', ['identifier', 'f'], [['number', '1']]], 'ordinary']),
            ('for x in items { x }', ['for', ['p_identifier', 'x'], ['identifier', 'items'], ['block', [['identifier', 'x']]]]),
            ('match x { A if True => 1 }', ['match', ['identifier', 'x'], [['branch', ['p_tag', ['identifier', 'A'], False, []], ['identifier', 'True'], ['number', '1']]]]),
            ("'é'", ["character", "'é'"]),
            ("'\\n'", ["character", "'\\n'"]),
            ("answer?", ["suffix", "?", ["identifier", "answer"]]),
            ("f(1)?", ["suffix", "?", ["call", ["identifier", "f"], [["number", "1"]]]]),
            ("dbg 1", ["dbg", ["number", "1"]]),
            ('crash "oops"', ["crash", ["string", '"oops"']]),
            ("return 1", ["return", ["number", "1"]]),
            ("break", ["break"]),
            ("...", ["ellipsis"]),
            ("x.?field", ["optional_field", ["identifier", "x"], "field"]),
            ("x.?field.next", ["field", ["optional_field", ["identifier", "x"], "field"], "next"]),
            ("|x| x + 1", ["lambda", [["p_identifier", "x"]], ["binary", "+", ["identifier", "x"], ["number", "1"]]]),
            ("if True { 1 } else { 2 }", ["if", ["identifier", "True"], ["block", [["number", "1"]]], ["block", [["number", "2"]]]]),
            ("{name: 1}", ["record", [["name", ["number", "1"]]]]),
            ("{name,}", ["record", [["name", ["identifier", "name"]]]]),
            ("{x = 1\nx + 1}", ["block", [["decl", ["p_identifier", "x"], ["number", "1"]], ["binary", "+", ["identifier", "x"], ["number", "1"]]]]),
            ("point.x", ["field", ["identifier", "point"], "x"]),
            ("point.x.y", ["field", ["field", ["identifier", "point"], "x"], "y"]),
            ("Foo.Bar", ["field", ["identifier", "Foo"], "Bar"]),
            ("Foo.Bar.Baz", ["field", ["field", ["identifier", "Foo"], "Bar"], "Baz"]),
            ("Foo .Bar", ["field", ["identifier", "Foo"], "Bar"]),
            ("Foo.Bar.baz", ["field", ["field", ["identifier", "Foo"], "Bar"], "baz"]),
            ("x.01", ["field", ["identifier", "x"], "01"]),
            ("x .01", ["field", ["identifier", "x"], "01"]),
            ("point.move(1)", ["call", ["field", ["identifier", "point"], "move"], [["number", "1"]]]),
            ("match x { A => 1, B => 2 }", ["match", ["identifier", "x"], [["branch", ["p_tag", ["identifier", "A"], False, []], None, ["number", "1"]], ["branch", ["p_tag", ["identifier", "B"], False, []], None, ["number", "2"]]]]),
        ]
        requests = "".join(json.dumps({"id": str(i), "mode": "expr", "source": source}) + "\n" for i, (source, _) in enumerate(cases))
        result = subprocess.run([os.environ["ORACLE_BINARY"]], input=requests, text=True, capture_output=True, check=True, timeout=30)
        rows = [json.loads(line) for line in result.stdout.splitlines()]
        self.assertEqual(len(rows), len(cases))
        for (source, expected), row in zip(cases, rows):
            with self.subTest(source=source):
                self.assertTrue(row["ok"], row["diagnostics"])
                self.assertEqual(row["facts"][0]["shape"], expected)


if __name__ == "__main__":
    unittest.main()
