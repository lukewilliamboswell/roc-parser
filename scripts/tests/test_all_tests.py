from __future__ import annotations

import unittest
from pathlib import Path

from scripts import all_tests


class AllTestsScriptTests(unittest.TestCase):
    def test_package_tests_run_through_main(self) -> None:
        # Modules import package dependencies, which resolve only through main.roc.
        source = Path(all_tests.__file__).read_text(encoding="utf-8")
        self.assertIn('[roc, "test", "package/main.roc"]', source)
        self.assertNotIn("TEST_MODULES", source)

    def test_the_release_rehearsal_is_not_repeated_here(self) -> None:
        # The shared release workflow bundles the package and tests the archive.
        source = Path(all_tests.__file__).read_text(encoding="utf-8")
        self.assertNotIn("bundle", source)


if __name__ == "__main__":
    unittest.main()
