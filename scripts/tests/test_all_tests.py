from __future__ import annotations

import unittest
from pathlib import Path

from scripts import all_tests


class AllTestsScriptTests(unittest.TestCase):
    def test_extract_bundle_path(self) -> None:
        output = "Created: /tmp/example.tar.zst\nCompressed size: 42 bytes\n"
        self.assertEqual(all_tests.extract_bundle_path(output), Path("/tmp/example.tar.zst"))

    def test_extract_bundle_path_requires_created_line(self) -> None:
        with self.assertRaisesRegex(ValueError, "could not extract"):
            all_tests.extract_bundle_path("bundle complete\n")

    def test_http_remains_excluded_from_test_modules(self) -> None:
        self.assertNotIn("HTTP", all_tests.TEST_MODULES)
        self.assertEqual(all_tests.TEST_MODULES[0:2], ("Parser", "CSV"))


if __name__ == "__main__":
    unittest.main()
