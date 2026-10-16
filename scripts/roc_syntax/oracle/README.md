# Compiler reference worker

This worker imports only Roc's tokenization/parser modules, pinned to compiler
commit `0d4b0ab02f9239448d034db17b4e63051a7c57ae`, using Zig `0.17.0`.
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

`version` defaults to 1. `id` is a string; `mode` is `file`, `expr`, `header`, `statement`, or `repl`; `source` is
a JSON string decoded as UTF-8. Unknown fields are ignored. Arbitrary invalid UTF-8
source transport is not implemented in this version. There is a 16 MiB request-line
limit. Invalid JSON, unsupported modes/versions, and compiler crashes terminate
the worker, so the supervisor must retain the current case and restart it.

Response fields:

- `version`, `id`: protocol version and request identity.
- `ok`: no tokenizer/parser diagnostics and the entire token stream consumed.
- `consumed`: parser position reached `EndOfFile`; this is checked explicitly,
  because the compiler convenience APIs may accept a prefix. This strict
  full-input contract also applies to headers, statements and REPL fragments.
- `tokens`: compiler token `kind` plus half-open UTF-8 byte `start`/`end` offsets.
  Includes `EndOfFile`; trivia is omitted by the compiler tokenizer. Token kinds
  are compiler-specific and include compound forms such as `NoSpaceDotLowerIdent`.
- `diagnostics`: `stage`, compiler `kind`, and half-open byte `start`/`end` offsets.
  Includes explicit `UnconsumedInput` and `TooManyTokenizationErrors` markers.
- `facts`: valid-file statements, a valid expression, header, or statement root, with
  `kind`, byte `start`/`end`, `node_kind`, and a structural `shape` projection.

Shape projection is intentionally limited, with unsupported syntax explicit:

```json
["number", "42"]
["identifier", "answer"]
["binary", "+", ["number", "1"], ["number", "2"]]
["call", ["identifier", "f"], [["number", "1"]]]
["lambda", [["p_identifier", "x"]], ["identifier", "x"]]
```

Lists and tuples use `["list", [...]]` and `["tuple", [...]]`; plain strings
use `["string", rawSourceIncludingQuotes]`; unary expressions use
`["unary", operator, child]`. Interpolated strings use `["interpolated_string", parts]`; multiline strings
use `["multiline_string", parts]`. A part is `["text", rawSource]` or
`["interpolation", expression]`; empty text parts are omitted. Typed literals
use `["typed", literal, [typePathSegments...]]`.

Definition facts retain bindings as `["decl", pattern, expression]`. Parentheses follow the compiler AST's grouping;
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

Common expression projections include lambdas, conditionals, guarded matches,
records, blocks, required/optional field paths, qualified names, tuple access,
characters, suffix propagation, arrows, loops and control expressions. Patterns
retain their structure, argument-list presence and nominal record shorthand.
Variable declarations preserve their name and optional initializer. Imports
preserve source targets, aliases, exposed qualified names and file content type.
Explicit file headers and all statements retain source order. Module, hosted,
app and package headers preserve exposure lists and dependencies. Type annotations,
declarations, constraints, associated definitions and open type extensions retain
their structure. Platform headers preserve required values and rigid type aliases, symbol maps,
dependencies and target configurations. Record extensions, unset fields, record
builders and nominal constructors have distinct shapes. The expression, pattern
and type switches enumerate every pinned AST variant; future variants fail the
worker build until their projection is implemented.

`statement` invokes the pinned parser's in-body statement entry point; it can
accept a variable declaration that file mode rejects. `header` invokes its header
entry point directly, without wrapping the source. `repl` takes one extracted
fragment, not a transcript. It follows `resolveSnapshotReplInputKind`: retain a
successful file parse containing exactly one definition statement; otherwise try
an in-body statement, falling back to the valid single-file statement if that
parse fails. This dispatch inspects parser errors, then the selected parse must
consume the entire fragment to satisfy `ok`. The pinned snapshot tool can accept
a parsed prefix, so the strict worker does not claim identical snapshot acceptance
for trailing input. REPL evaluation and cross-fragment bindings are outside this
parse-only protocol.

The live contract test sends multiple requests to one process, validates numeric
and addition structures from the pinned compiler's `expr_int_simple.md` and
`expr/binop_simple_add.md` snapshots, checks precedence and left associativity,
then checks rejected trailing input, a module definition, malformed input,
grouping, lists, strings and unary operators.
Additional live cases cover lambda/if/match, records and shorthand fields, block
definitions, chained field access, method calls and uppercase tags.
The archive tests run without a worker binary.

Target configurations retain file-list wrappers and raw string spelling. The
compiler stores only the last `inputs_dir` and separates it from target entries;
the normalized candidate view follows that ordering and last-value behavior.
The candidate syntax tree and lossless tokens still retain every occurrence.
