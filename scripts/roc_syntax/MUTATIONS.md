The differential mutation lane checks correctness against the pinned current Zig
parser, using persistent JSONL workers with a deadline for every request:

```sh
python3 scripts/mutate_roc_syntax.py \
  --probe-command 'path/to/probe' \
  --oracle-command 'path/to/roc-syntax-oracle' \
  --fixtures scripts/roc_syntax/fixtures.json \
  --fixtures scripts/roc_syntax/type-fixtures.json \
  --fixtures scripts/roc_syntax/platform-fixtures.json \
  --runs 2000 --seed 20261016 \
  --output .roc-parser-tmp/mutations
```

Repeat `--fixtures` for additional pinned fixture or imported upstream corpus JSON
files. Only oracle-accepted seeds enter the mutation pool. The deterministic
campaign inserts, deletes, and replaces short fragments, including delimiters,
keywords, comments, Unicode, and control characters. Edits respect Unicode scalar
boundaries; the transport therefore tests valid UTF-8. The separate raw byte fuzzer
continues to cover malformed UTF-8.

Add `--corpus path/to/raw-fuzz-corpus` to compare unchanged harvested raw inputs in
file mode, **including oracle-rejected inputs**. These comparisons precede the
fresh mutations. Invalid UTF-8, oversized sources, and entries above
`--max-corpus-files` are counted as exclusions, never as oracle comparisons.
Sources are limited by `--max-source-bytes` (default 16384); a mutation uses at most
`--max-edits` (default 3). Each worker request has `--timeout` seconds (default 5).
A worker crash or timeout fails that case and the next case starts a new worker.

Every case compares acceptance. Both workers must satisfy bounded token,
node, diagnostic, and child-span invariants; candidate tokens must reconstruct the
complete source. Accepted cases additionally require complete consumption and
identical normalized tokens and syntax trees. Unknown accepted projections fail.
Rejected cases permit different recovery trees while retaining all invariants.
There is no allowlist of known mismatches.

`seeds.json` saves seed sources and acceptance decisions. `cases.jsonl` saves every
request with its original source, campaign seed, case index, hashes, and ordered
UTF-8 byte-offset edits. `report.json` records bounds, worker identities, exclusion
counts, mode/edit coverage, and failure paths from this run. Each failure JSON
also retains both responses. Reproduce a failure without the original fixtures:

```sh
python3 scripts/mutate_roc_syntax.py \
  --probe-command 'path/to/probe' --oracle-command 'path/to/roc-syntax-oracle' \
  --replay .roc-parser-tmp/mutations/failures/FAILURE.json \
  --output .roc-parser-tmp/mutation-replay
```

The command exits nonzero on any disagreement, invariant violation, worker failure,
or projection gap. Distinct output directories preserve independent campaigns.
