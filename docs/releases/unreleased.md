# Unreleased

## Upgrade impact

Existing runtime parsing functions keep their signatures and acceptance rules.
The new checked conversions also reject enabled lint checks by default; each
check can be disabled through defaulted props. The compiler pin moves to
`nightly-2026-10-21-5349569`.

The new `JsonValue` module adds API only. It is not called `Json` because Roc
auto-imports a builtin `Json`, and its error tag is `InvalidJsonValue` so that
it does not collide with the builtin's `InvalidJson(Str)`.

## New features

### `JsonValue`

`JsonValue` reads JSON whose shape a record type cannot describe, such as
software bills of materials, JSON Schema files and configuration with
comments, and writes it back out. Use the builtin `Json` to decode JSON into
records.

- **Lossless values.** A `JsonValue` is `Null`, `Bool`, `Number`, `String`,
  `Array` or `Object`. Numbers keep their exact text and objects keep their
  member order. `get`, `at`, `get_path`, `keys` and `as_str`, `as_bool`,
  `as_list` and `as_object` walk a value; `as_i64`, `as_u64` and `as_dec`
  convert a number.
- **Strict by default.** `parse_str` accepts only RFC 8259 JSON, and also
  rejects repeated keys and nesting deeper than 128 levels. Errors are
  `InvalidJsonValue({ line, column, message })`.
- **Options.** `parse_with` takes a `JsonValue.Options` record with defaulted
  fields:
  - limits: `max_depth`, `max_items`, `max_string_bytes` and
    `max_number_digits`;
  - relaxations, each off by default: `comments` and `trailing_commas`
    (JSONC), `bom`, `non_finite` (`NaN` and `Infinity`),
    `control_characters`, and `lone_surrogates: Replace`;
  - checks: `duplicates` (`Reject`, `CollapseIdentical`, `KeepFirst` or
    `KeepLast`), `top_level`, and `interoperable_numbers` (I-JSON).
- **Writer.** `to_str` writes compact JSON and `to_str_with` takes `indent`
  and `sort_keys`. The layout matches Python's
  `json.dumps(ensure_ascii=False)` byte for byte, with number text kept as
  written. Values JSON cannot express are refused with `InvalidNumber`,
  `DuplicateKey` or `TooDeep`.
- **Compile time.** A string literal annotated `JsonValue` is checked during
  compilation, and `JsonValue.validate` checks an imported file with the same
  options as `parse_with`.
- **Composition.** `parser` and `parser_with` are `Parser(Utf8.Bytes, JsonValue)`
  values for use with the combinators.
- **Evidence.** `scripts/review_json.py` compares the parser and writer with
  Python's `json` module on 397 hand-written cases, the 318 JSONTestSuite
  cases and five pinned SPDX, CycloneDX, Syft and Trivy documents: all 720
  pass, with no known failures. Three fuzz targets (`json-raw`,
  `json-options` and `json-roundtrip`) run in CI with the other targets, and
  `review_json.py campaign` adds a differential robustness campaign.
  Benchmarks compare it with serde_json, Go's `encoding/json` and Python's
  `json`.

### Compile-time parsing

- Compile-time quotations for YAML, XML, CSV, JSON, Markdown, AsciiDoc, HTTP,
  and Roc source, plus typed YAML and CSV decoding.
- Direct nominal document types for `CSV`, `AsciiDoc`, and `RocSyntax`, with
  `from_quote` methods; `Markdown.Document` and HTTP request/response quotation
  targets cover their distinct document shapes.
- Configurable `validate` and `validate_as` conversions, simple located errors,
  and `Literal.require` for demanded top-level constants from imported text.
- `Utf8.parse_quote` for custom combinator-based quotation types.
- An enduring compile-time parsing guide, runnable examples with adjacent
  assets, and compiler rejection and cache-invalidation tests in CI.

See [Parse and validate during compilation](../compile-time-parsing.adoc) and
[Reading JSON](../json.adoc).
