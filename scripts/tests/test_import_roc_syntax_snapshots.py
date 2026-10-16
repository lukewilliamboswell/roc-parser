from __future__ import annotations

import hashlib
import unittest

from scripts.import_roc_syntax_snapshots import snapshot_case


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

    def test_reports_unhandled_snapshot_modes(self):
        case, reason = snapshot_case("repl.md", "blob", snapshot("repl", "1 + 2\n"))
        self.assertIsNone(case)
        self.assertEqual(reason, "unsupported_snapshot_mode:repl")

    def test_missing_source_section_fails(self):
        with self.assertRaisesRegex(ValueError, "SOURCE"):
            snapshot_case("bad.md", "blob", b"# META\n~~~ini\ntype=expr\n~~~\n")


if __name__ == "__main__":
    unittest.main()
