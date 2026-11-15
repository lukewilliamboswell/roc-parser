import subprocess
import unittest
from scripts.check_nightly_compat import assert_clean, nightly_revision


class NightlyCompatibilityTests(unittest.TestCase):
    def test_identifies_nightly_source_and_rejects_unexpected_refs(self):
        self.assertEqual(nightly_revision('nightly-2026-11-14-a87808d'), 'a87808d')
        for tag in ['main', 'nightly-2026-11-14-../main', 'nightly-2026-11-14-A87808D']:
            with self.assertRaises(ValueError):
                nightly_revision(tag)

    def test_clean_success(self):
        assert_clean(subprocess.CompletedProcess([], 0, '0 errors and 0 warnings\nAll (2814) tests passed', ''))

    def test_panics_and_warnings_cannot_pass_even_with_zero_status(self):
        for output in ['thread 1 panic: checked artifact invariant violated',
                       '0 errors and 4 warnings', 'segmentation fault', 'internal compiler error']:
            with self.subTest(output=output), self.assertRaises(AssertionError):
                assert_clean(subprocess.CompletedProcess([], 0, output, ''))

    def test_nonzero_status_cannot_pass_even_without_diagnostic(self):
        for status in [1, 2, -6, -11]:
            with self.subTest(status=status), self.assertRaises(AssertionError):
                assert_clean(subprocess.CompletedProcess([], status, '', ''))
