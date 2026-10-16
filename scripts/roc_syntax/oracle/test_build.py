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
            ('file', 'module []\nx = 1', [['decl', ['p_identifier', 'x'], ['number', '1']]]),
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
        self.assertEqual(rows[5]["facts"][0]["kind"], "definition")
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
