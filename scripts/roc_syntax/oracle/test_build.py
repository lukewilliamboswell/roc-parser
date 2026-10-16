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


if __name__ == "__main__":
    unittest.main()
