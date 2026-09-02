#!/usr/bin/env python3

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest


SCRIPT_PATH = Path(__file__).resolve().parents[1] / "run_fuzz.py"
SPEC = importlib.util.spec_from_file_location("run_fuzz", SCRIPT_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"could not load {SCRIPT_PATH}")
run_fuzz = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = run_fuzz
SPEC.loader.exec_module(run_fuzz)


class FuzzStrEncodingTests(unittest.TestCase):
    def test_encodes_empty_seed(self) -> None:
        self.assertEqual(run_fuzz.encode_fuzz_str(""), b"\x00")

    def test_encodes_ascii_seed_with_length_selector(self) -> None:
        self.assertEqual(run_fuzz.encode_fuzz_str("Roc"), b"Roc\x03")

    def test_uses_utf8_byte_length(self) -> None:
        self.assertEqual(run_fuzz.encode_fuzz_str("λ"), "λ".encode() + b"\x02")

    def test_rejects_seed_larger_than_one_byte_length_encoding(self) -> None:
        with self.assertRaisesRegex(ValueError, "255-byte"):
            run_fuzz.encode_fuzz_str("x" * 256)


class SeedCorpusTests(unittest.TestCase):
    def test_all_reviewable_seed_files_are_valid(self) -> None:
        for target_name in run_fuzz.TARGET_ORDER:
            config = run_fuzz.TARGETS[target_name]
            values = run_fuzz.load_seed_values(config.seeds)
            self.assertGreater(len(values), 0, target_name)
            for value in values:
                run_fuzz.encode_fuzz_str(value)

    def test_all_target_inputs_exist(self) -> None:
        for config in run_fuzz.TARGETS.values():
            self.assertTrue(config.source.is_file(), config.source)
            self.assertTrue(config.seeds.is_file(), config.seeds)
            self.assertTrue(config.dictionary.is_file(), config.dictionary)

    def test_load_seed_values_rejects_non_string_values(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            seed_file = Path(directory) / "seeds.json"
            seed_file.write_text(json.dumps(["valid", 1]), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "array of strings"):
                run_fuzz.load_seed_values(seed_file)

    def test_prepare_corpus_is_content_addressed_and_idempotent(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            corpus = Path(directory) / "corpus"
            first = run_fuzz.prepare_corpus("csv", corpus)
            first_files = sorted(first.iterdir())
            second = run_fuzz.prepare_corpus("csv", corpus)
            second_files = sorted(second.iterdir())

            self.assertEqual(first_files, second_files)
            self.assertEqual(
                len(first_files),
                len(set(run_fuzz.load_seed_values(run_fuzz.TARGETS["csv"].seeds))),
            )
            for seed_file in first_files:
                self.assertTrue(seed_file.name.startswith("seed-"))


class TargetSelectionTests(unittest.TestCase):
    def test_empty_selection_and_all_select_every_target(self) -> None:
        expected = list(run_fuzz.TARGET_ORDER)
        self.assertEqual(run_fuzz.resolve_targets([]), expected)
        self.assertEqual(run_fuzz.resolve_targets(["all"]), expected)

    def test_selection_preserves_order_and_removes_duplicates(self) -> None:
        self.assertEqual(
            run_fuzz.resolve_targets(["xml", "csv", "xml"]),
            ["xml", "csv"],
        )

    def test_unknown_target_is_rejected(self) -> None:
        with self.assertRaisesRegex(ValueError, "unknown fuzz target"):
            run_fuzz.resolve_targets(["unknown"])

    def test_all_cannot_be_combined_with_a_target(self) -> None:
        with self.assertRaisesRegex(ValueError, "cannot be combined"):
            run_fuzz.resolve_targets(["all", "csv"])


if __name__ == "__main__":
    unittest.main()
