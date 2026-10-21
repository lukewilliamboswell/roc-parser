# Unreleased

## Upgrade impact

Existing runtime parsing functions keep their signatures and acceptance rules.
The new checked conversions also reject enabled lint checks by default; each
check can be disabled through defaulted props. The compiler pin remains
`nightly-2026-10-20-7a8a800`.

## New features

- Compile-time quotations for YAML, XML, CSV, Markdown, AsciiDoc, HTTP, and Roc
  source, plus typed YAML and CSV decoding.
- Direct nominal document types for `CSV`, `AsciiDoc`, and `RocSyntax`, with
  `from_quote` methods; `Markdown.Document` and HTTP request/response quotation
  targets cover their distinct document shapes.
- Configurable `validate` and `validate_as` conversions, simple located errors,
  and `Literal.require` for demanded top-level constants from imported text.
- `Utf8.parse_quote` for custom combinator-based quotation types.
- An enduring compile-time parsing guide, runnable examples with adjacent
  assets, and compiler rejection and cache-invalidation tests in CI.

See [Parse and validate during compilation](../compile-time-parsing.adoc).
