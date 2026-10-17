from __future__ import annotations

import hashlib
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from asciidoc.mutations import mutate_cases


class AsciiDocMutationsTests(unittest.TestCase):
    def test_reproducible_edits_preserve_transport_and_provenance(self):
        parents = [{"id": "unicode", "source": "= Title\n\n日 é 😀\n"}]
        edits = mutate_cases(parents, 300, 42)
        self.assertEqual(edits, mutate_cases(parents, 300, 42))
        self.assertNotEqual(edits, mutate_cases(parents, 300, 43))
        self.assertEqual(len(edits), 300)
        self.assertEqual(len({case["id"] for case in edits}), 300)
        self.assertEqual({case["operation"] for case in edits},
                         {"insert", "delete", "replace", "duplicate", "truncate"})
        for case in edits:
            raw = case["source"].encode("utf-8")
            self.assertEqual(raw.decode("utf-8"), case["source"])
            self.assertEqual(case["sha256"], hashlib.sha256(raw).hexdigest())
            self.assertEqual(case["parent_id"], "unicode")
            self.assertEqual(case["support"], "explore")
        self.assertEqual(parents[0]["source"], "= Title\n\n日 é 😀\n")

    def test_inventory_growth_only_changes_slots_won_by_new_parent(self):
        parents = [{"id": f"parent-{i}", "source": f"= Title {i}\n\nBody {i}"}
                   for i in range(12)]
        added = {"id": "new-regression", "source": "Unicode 日 😀\n\n[source,roc]\n----"}
        before = mutate_cases(parents, 1024, 719)
        after = mutate_cases(parents + [added], 1024, 719)
        changed = [new for old, new in zip(before, after) if old != new]
        self.assertTrue(changed, "a new parent must remain eligible")
        self.assertLess(len(changed), len(after) // 4)
        self.assertEqual({case["parent_id"] for case in changed}, {added["id"]})
        for old, new in zip(before, after):
            if new["parent_id"] != added["id"]:
                self.assertEqual(old, new)

    def test_selection_is_order_independent_prefix_stable_and_all_parents_eligible(self):
        parents = [{"id": f"seed-{i}", "source": "same source"} for i in range(24)]
        edits = mutate_cases(parents, 2048, 113)
        self.assertEqual(edits, mutate_cases(list(reversed(parents)), 2048, 113))
        self.assertEqual(edits[:100], mutate_cases(parents, 100, 113))
        self.assertEqual({case["parent_id"] for case in edits}, {case["id"] for case in parents})

    def test_virtual_environments_participate_in_stable_parent_identity(self):
        parents = [{"id": "same", "source": "include::part.adoc[]",
                    "includes": {"part.adoc": text}} for text in ("first", "second")]
        edits = mutate_cases(parents, 100, 42)
        self.assertEqual(edits, mutate_cases(list(reversed(parents)), 100, 42))
        self.assertEqual({case["includes"]["part.adoc"] for case in edits}, {"first", "second"})

    def test_empty_and_invalid_requests(self):
        self.assertEqual(mutate_cases([], 0, 0), [])
        with self.assertRaises(ValueError):
            mutate_cases([], 1, 0)
        with self.assertRaises(ValueError):
            mutate_cases([], -1, 0)
        edits = mutate_cases([{"id": "empty", "source": ""}], 10, 0)
        self.assertEqual(len(edits), 10)

    def test_virtual_source_environment_is_preserved(self):
        parent = {"id": "include", "source": "include::part.adoc[]", "includes": {"part.adoc": "Words"}}
        edit = mutate_cases([parent], 1, 0)[0]
        self.assertEqual(edit["includes"], parent["includes"])
        self.assertIsNot(edit["includes"], parent["includes"])


if __name__ == "__main__":
    unittest.main()
