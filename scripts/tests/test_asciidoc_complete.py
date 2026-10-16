"""Completion checks must not confuse structural source with semantic coverage."""
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('asciidoc_complete_review', Path(__file__).resolve().parents[1] / 'review_asciidoc.py')
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)


class CompleteCoverageTests(unittest.TestCase):
    def test_future_labels_cannot_exempt_completion(self):
        projection = {'title': '', 'blocks': []}
        expected = {'status': 'ok', 'projection': projection, 'html': ''}
        case = {'support': 'future', 'reject': True}
        self.assertEqual(review.classify(case, expected, {'status': 'unsupported'}, require_complete=True)[0], 'coverage_gap')
        actual = {'status': 'ok', 'projection': projection, 'html': ''}
        self.assertEqual(review.classify(case, expected, actual, require_complete=True, compare_html=True)[0], 'pass')
        del actual['html']
        self.assertEqual(review.classify(case, expected, actual, require_complete=True, compare_html=True)[0], 'missing_html')

    def test_dom_comparison_keeps_semantics(self):
        canonical = review.canonical_html
        self.assertEqual(canonical('<div>\n<p>A &amp; B</p>\n<hr>\n</div>'), canonical('<div><p>A &#38; B</p><hr /></div>'))
        self.assertNotEqual(canonical('<p>A B</p>'), canonical('<p>AB</p>'))
        self.assertNotEqual(canonical('<pre>A\n B</pre>'), canonical('<pre>A B</pre>'))
        self.assertNotEqual(canonical('<a href="a">X</a>'), canonical('<a href="b">X</a>'))
        self.assertNotEqual(canonical('<p><em>X</em></p>'), canonical('<p><strong>X</strong></p>'))
        self.assertNotEqual(canonical('<div id="a"></div>'), canonical('<div id="b"></div>'))

    def test_virtual_frame_lengths_are_bytes_and_environment_is_explicit(self):
        args, data = review.probe_request('日', {'é.adoc': '😀\n'})
        self.assertEqual(args, ['--request'])
        self.assertEqual(data, b'ADOC1\n1\n7\n' + 'é.adoc'.encode() + b'5\n' + '😀\n日'.encode())
        self.assertEqual(review.probe_request('A'), ([], b'A'))
        self.assertEqual(review.probe_request('A', {}), (['--request'], b'ADOC1\n0\nA'))


if __name__ == '__main__':
    unittest.main()
