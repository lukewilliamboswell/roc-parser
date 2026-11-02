"""Exercise the Roc combiner with real JSON validation and exact token checks."""
from __future__ import annotations

import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("roc-stable") and shutil.which("jq"), "run through Blueprint for JSON tools")
class AttestationDriverTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.build = tempfile.TemporaryDirectory()
        cls.binary = Path(cls.build.name) / "combine-attestations"
        subprocess.run(["roc-stable", "build", "scripts/combine_attestations.roc", f"--output={cls.binary}"],
                       cwd=ROOT, check=True, capture_output=True)

    @classmethod
    def tearDownClass(cls) -> None:
        cls.build.cleanup()

    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.provenance = self.root / "provenance.json"
        self.sbom = self.root / "sbom.json"
        self.output = self.root / "combined/bundle.intoto.jsonl"
        self.provenance.write_text('{\n "payload": "YQ==", "logIndex": 9007199254740993123456789\n}\n')
        self.sbom.write_text('{"kind":"sbom","escaped":"quote: \\\"; newline: \\n","items":[1,2]}\n')

    def invoke(self) -> subprocess.CompletedProcess[str]:
        return subprocess.run([str(self.binary), "--provenance-bundle", str(self.provenance),
                               "--sbom-bundle", str(self.sbom), "--output", str(self.output)],
                              cwd=ROOT, capture_output=True, text=True)

    def test_writes_two_compact_records_without_changing_values(self) -> None:
        result = self.invoke()
        self.assertEqual(result.returncode, 0, result.stderr)
        lines = self.output.read_text().splitlines()
        self.assertEqual(len(lines), 2)
        self.assertEqual(json.loads(lines[0]), json.loads(self.provenance.read_text()))
        self.assertEqual(json.loads(lines[1]), json.loads(self.sbom.read_text()))
        self.assertIn('"logIndex":9007199254740993123456789', lines[0])
        self.assertIn('"payload":"YQ=="', lines[0])

    def test_non_object_empty_or_multiple_documents_are_rejected_before_writing(self) -> None:
        for content in ("[]\n", "{}\n", "null\n", '{"one":1}\n{"two":2}\n', "invalid\n", "\ufeff{\"one\":1}\n"):
            with self.subTest(content=content):
                self.provenance.write_text(content)
                result = self.invoke()
                self.assertNotEqual(result.returncode, 0, result.stderr)
                self.assertFalse(self.output.exists())

    def test_second_input_failure_preserves_existing_output(self) -> None:
        self.output.parent.mkdir()
        self.output.write_text("previous output\n")
        self.sbom.write_text("")
        result = self.invoke()
        self.assertNotEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.output.read_text(), "previous output\n")

    def test_documents_cannot_move_between_input_files(self) -> None:
        self.provenance.write_text('{"one":1}\n{"two":2}\n')
        self.sbom.write_text("")
        self.assertNotEqual(self.invoke().returncode, 0)
        self.assertFalse(self.output.exists())
