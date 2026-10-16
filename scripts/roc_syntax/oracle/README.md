# Compiler reference worker

This worker imports only Roc's tokenization/parser modules, pinned to compiler
commit `7f11a825fb2df7fd48edb11e8c593ffdad4fcdb5`, using Zig `0.16.0`.
It does not invoke the compiler CLI or link compiler backends/LLVM libraries.

Build from the repository root:

```sh
python3 scripts/roc_syntax/build_oracle.py --zig /path/to/zig --roc-repo /path/to/roc
```

The helper prints the executable path. `--roc-repo` supplies Git objects, never
working-tree files. Without it, an existing sibling `roc` Git repository is used;
otherwise the helper fetches the exact commit into its own bare Git cache. Git
verifies the requested immutable commit object. It extracts only regular files
and directories within `src`, rejecting traversal and links. The source and Zig
build caches live under ignored `.roc-parser-tmp/syntax-oracle`; remove that
folder to rebuild everything. No global Zig cache or installed compiler source
is modified. Each build re-extracts the source from immutable Git objects, replacing
any cached edits. A JSON sidecar beside the binary records compiler SHA, Zig
version, optimization mode, worker-source hash and executable hash.

The build adapter mirrors the pinned compiler module dependency graph directly.
It avoids invoking the compiler's full `build.zig`, including its unrelated
backend/package dependencies. Source upgrades must update the revision pin,
review this graph, and rerun the live contract and differential tests.

## JSONL protocol version 1

One request per line, one response per line, in request order. The worker stays
alive across requests and releases each request's arena before the next request.
Input/output use libc stdio/read and write on macOS/Linux. Responses flush without
waiting for the next request. Protocol stdout contains only JSONL; process errors
use stderr and a nonzero exit status, which the supervisor must handle.

```json
{"version":1,"id":"case-1","mode":"expr","source":"1 + 2 * 3"}
```

`version` defaults to 1. `id` is a string; `mode` is `file` or `expr`; `source` is
a JSON string decoded as UTF-8. Unknown fields are ignored. Arbitrary invalid UTF-8
source transport is not implemented in this version. There is a 16 MiB request-line
limit. Invalid JSON, unsupported modes/versions, and compiler crashes terminate
the worker, so the supervisor must retain the current case and restart it.

Response fields:

- `version`, `id`: protocol version and request identity.
- `ok`: no tokenizer/parser diagnostics and the entire token stream consumed.
- `consumed`: parser position reached `EndOfFile`; this is checked explicitly,
  because the compiler's expression convenience API may accept a prefix.
- `tokens`: compiler token `kind` plus half-open UTF-8 byte `start`/`end` offsets.
  Includes `EndOfFile`; trivia is omitted by the compiler tokenizer. Token kinds
  are compiler-specific and include compound forms such as `NoSpaceDotLowerIdent`.
- `diagnostics`: `stage`, compiler `kind`, and half-open byte `start`/`end` offsets.
  Includes explicit `UnconsumedInput` and `TooManyTokenizationErrors` markers.
- `facts`: valid-file top-level definitions or a valid expression root, with
  `kind`, byte `start`/`end`, `node_kind`, and a structural `shape` projection.

Shape projection is intentionally limited, with unsupported syntax explicit:

```json
["number", "42"]
["identifier", "answer"]
["binary", "+", ["number", "1"], ["number", "2"]]
["call", ["identifier", "f"], [["number", "1"]]]
["unsupported", "lambda"]
```

Lists and tuples use `["list", [...]]` and `["tuple", [...]]`; plain strings
use `["string", rawSourceIncludingQuotes]`; unary expressions use
`["unary", operator, child]`. Interpolated strings remain explicitly unsupported.

Definition facts project the RHS. Parentheses follow the compiler AST's grouping;
they do not introduce a separate shape. Projection depth above 256 produces an
explicit unsupported marker. A consumer must not count unsupported projections
as structural agreement. Raw number/identifier spellings are retained.

The compiler's tokenization diagnostic storage is bounded at 128, and parser
storage is bounded upstream. Compare acceptance and supported structure rather
than exact diagnostic counts or recovery ASTs. This worker is a reference for the
pinned syntax, and its throughput does not measure the Roc parser under test.

## Validation

```sh
ORACLE_BINARY=/path/to/roc-syntax-oracle python3 -m unittest discover \
  -s scripts/roc_syntax/oracle -p 'test_*.py'
```

The live contract test sends multiple requests to one process, validates numeric
and addition structures from the pinned compiler's `expr_int_simple.md` and
`expr/binop_simple_add.md` snapshots, checks precedence and left associativity,
then checks rejected trailing input, a module definition, malformed input,
grouping, lists, strings and unary operators.
The archive tests run without a worker binary.
