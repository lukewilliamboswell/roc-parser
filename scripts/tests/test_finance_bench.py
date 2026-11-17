"""Financial benchmark inputs must be comparable across independent models."""
import importlib.util
import os
import random
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import bench
import bench_python


class FinancialCorpora(unittest.TestCase):
    def test_ofx_identifiers_are_unique_and_transactions_are_framed(self):
        import xml.etree.ElementTree as ET
        for size in (4096, 65536):
            tree = ET.fromstring(bench.ofx_doc(random.Random(1), size))
            transactions = tree.findall(".//STMTTRN")
            self.assertGreater(len(transactions), 0)
            identifiers = [tx.findtext("FITID") for tx in transactions]
            self.assertEqual(len(identifiers), len(set(identifiers)))
            self.assertTrue(all(tx.findtext("DTPOSTED") for tx in transactions))

    @unittest.skipUnless(os.environ.get("ROC_PARSER_RUN_ORACLE_TESTS") == "1" and not os.environ.get("CI") and importlib.util.find_spec("quiffen"), "local only: set ROC_PARSER_RUN_ORACLE_TESTS=1 with the pinned QIF oracle")
    def test_quiffen_checksum_matches_independent_generated_records(self):
        text = bench.qif_doc(random.Random(1), 4096)
        records = text.split("\n^\n")[:-1]
        expected = 0
        for record in records:
            fields = record.splitlines()
            payee = next(line[1:] for line in fields if line.startswith("P"))
            memo = next(line[1:] for line in fields if line.startswith("M"))
            expected += 1 + bench_python.field_checksum(payee) + bench_python.field_checksum(memo)
        self.assertEqual(bench_python.finance_parser("quiffen")(text), expected)

    @unittest.skipUnless(os.environ.get("ROC_PARSER_RUN_ORACLE_TESTS") == "1" and not os.environ.get("CI") and importlib.util.find_spec("ofxtools") and importlib.util.find_spec("ofxparse"), "local only: set ROC_PARSER_RUN_ORACLE_TESTS=1 with the pinned OFX oracles")
    def test_ofx_comparators_match_independent_xml_projection(self):
        import xml.etree.ElementTree as ET
        text = bench.ofx_doc(random.Random(1), 4096)
        expected = sum(1 + bench_python.field_checksum(tx.findtext("NAME", ""))
                       + bench_python.field_checksum(tx.findtext("MEMO", ""))
                       for tx in ET.fromstring(text).findall(".//STMTTRN"))
        for name in ("ofxtools", "ofxparse"):
            self.assertEqual(bench_python.finance_parser(name)(text), expected, name)
