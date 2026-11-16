from __future__ import annotations
import importlib.util
from pathlib import Path
import unittest
import xml.etree.ElementTree as ET

SPEC = importlib.util.spec_from_file_location('review_ofx', Path(__file__).resolve().parents[1] / 'review_ofx.py')
review = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(review)


class OracleNormalizationTests(unittest.TestCase):
    def test_xml_codec_normalization_changes_only_declared_encoding(self):
        data = '<?xml encoding="windows-1252"?><OFX><MEMO>Café windows-1252</MEMO></OFX>'.encode('cp1252')
        expected = '<?xml encoding="UTF-8"?><OFX><MEMO>Café windows-1252</MEMO></OFX>'.encode()
        self.assertEqual(review.oracle_bytes(data), expected)

    def test_legacy_codec_normalization_preserves_body_header_words(self):
        data = b'ENCODING:USASCII\nCHARSET:1252\n\n<OFX><MEMO>CHARSET:1252 ENCODING:USASCII</MEMO></OFX>'
        expected = b'ENCODING:UTF-8\nCHARSET:NONE\n\n<OFX><MEMO>CHARSET:1252 ENCODING:USASCII</MEMO></OFX>'
        self.assertEqual(review.oracle_bytes(data), expected)

    def test_undefined_windows1252_is_rejected(self):
        with self.assertRaises(UnicodeDecodeError):
            review.oracle_bytes(b'ENCODING:USASCII\nCHARSET:1252\n\n<OFX>\x81</OFX>')

    def test_raw_tree_entities_are_decoded_exactly_once(self):
        root = ET.fromstring('<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS><BANKACCTFROM/><BANKTRANLIST><STMTTRN><NAME/></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>')
        root.find('.//NAME').text = 'A &amp;amp; B'
        normalized = review.normalize_tree(root, decode_entities=True)
        self.assertEqual(normalized['statements'][0]['transactions'][0]['name'], 'A &amp; B')


if __name__ == '__main__':
    unittest.main()


class XmlProvenanceTests(unittest.TestCase):
    def test_xml_cdata_is_not_decoded_as_a_character_reference(self):
        root = ET.fromstring('<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS><BANKACCTFROM/><BANKTRANLIST><STMTTRN><NAME><![CDATA[&amp;lt;]]></NAME></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>')
        normalized = review.normalize_tree(root)
        self.assertEqual(normalized['statements'][0]['transactions'][0]['name'], '&amp;lt;')

    def test_numeric_entity_is_decoded_once_by_the_xml_parser(self):
        root = ET.fromstring('<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS><BANKACCTFROM/><BANKTRANLIST><STMTTRN><NAME>&#38;lt;</NAME></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>')
        normalized = review.normalize_tree(root)
        self.assertEqual(normalized['statements'][0]['transactions'][0]['name'], '&lt;')
