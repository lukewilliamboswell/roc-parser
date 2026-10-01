# YAML parser benchmarks and profiling

`scripts/bench_yaml.py` builds an optimized binary with debug information using basic-cli 0.23.0. It measures parsing plus recursive result consumption inside the Roc process. Reading stdin and writing JSON are outside the timing region. This measures repeated parsing of a retained input, which can affect reference ownership relative to a one-shot parse. The UTC clock is not monotonic; use sufficiently long batches, retain raw samples, and compare on an idle machine.

## Run benchmarks

```sh
python3 scripts/bench_yaml.py --label current
python3 scripts/bench_yaml.py --label head \
  --parser-root .roc-parser-tmp/yaml-review/sources/head/package
python3 scripts/bench_yaml.py --label base \
  --parser-root .roc-parser-tmp/yaml-review/sources/base/package
python3 scripts/bench_yaml.py --label head-large \
  --parser-root .roc-parser-tmp/yaml-review/sources/head/package \
  --sizes 1024,2048 --only mapping,repeated_blocks,repeated_quoted
```

Add `--allocations` to either command to collect allocation diagnostic counters alongside timings.

The default compiler is the September 29 2026 nightly specified for the review; override with `--roc`. Reports under `.roc-parser-tmp/yaml-bench/<label>/` retain compiler version, build arguments, parser and input hashes, inputs, per-batch timings, success counts, and medians. `--repetitions`, `--iterations`, and `--timeout` bound measurements. Compare successful parses with successful parses: base and current reject block scalars, so those timings measure rejection rather than equivalent work.

Fixtures cover wide block mappings and sequences, flow sequences, long quoted strings, literal and folded blocks, repeated small blocks and equivalent quoted values, late syntax failures, mixed nested configuration records, nested block mappings at the parser depth limit, and nested flow sequences. Block mapping nesting is capped at 101; use `--sizes 99,100,101 --only nested_mapping` to exercise its limit. Flow nesting is capped at 260; sizes above 260 repeat the same nesting case. These are diagnostic microbenchmarks, not a claim about all application workloads.

## Sampling profile

```sh
samply record --save-only --duration 8 \
  -o .roc-parser-tmp/yaml-bench/head/repeated-blocks-profile.json \
  .roc-parser-tmp/yaml-bench/head/benchmark 1000 \
  < .roc-parser-tmp/yaml-bench/head/repeated_blocks-512.yaml
samply load .roc-parser-tmp/yaml-bench/head/repeated-blocks-profile.json
```

On macOS, samply needs access to profiling APIs; the sandboxed attempt failed with `Unknown(1100)`, while the authorized unsandboxed attempt worked. Keep the matching binary beside the profile. Some Roc functions have hashed linker names even with `--debug`; `nm -n` can map recorded binary offsets to these symbols. Do not identify a source function from a hashed symbol without debug symbol evidence.

## Allocation diagnostic

`fuzz/yaml-alloc.roc` is a diagnostic target, deliberately excluded from fuzz campaigns. It uses roc-fuzz 0.4.2's `measure_allocs!` around parsing and intentionally crashes with a counter report. Exit code 77 is expected and does not represent a parser defect. Input decoding and report formatting are outside the counter region. Counts include `roc_alloc` and `roc_realloc` calls, not bytes allocated or bytes copied.

```sh
ROC=~/roc_nightly-macos_apple_silicon-2026-09-29-7f11a82/roc
"$ROC" build --fuzz fuzz/yaml-alloc.roc \
  --output=.roc-parser-tmp/yaml-bench/head/alloc \
  --replace-dep "$PWD/package/main.roc" \
  "$PWD/.roc-parser-tmp/yaml-review/sources/head/package/main.roc"
.roc-parser-tmp/yaml-bench/head/alloc replay \
  .roc-parser-tmp/yaml-bench/head/repeated_blocks-512.yaml
```

## Initial measurements

Fixed PR head `08bb780397bb5035a8c8630693f400b8cfa0661b`, optimized speed build on Apple silicon, three batches per fixture: five parses per batch through 512 entries, three parses per batch for larger inputs.

| Entries | Repeated blocks median ms | Equivalent quotes median ms | Block allocation calls | Quote allocation calls |
| --- | --- | --- | --- | --- |
| 16 | 0.057 | 0.050 | 328 | 182 |
| 128 | 0.616 | 0.419 | 2470 | 1314 |
| 512 | 5.266 | 2.423 | 9782 | 5169 |
| 1024 | 18.372 | 6.950 | 19515 | 10297 |
| 2048 | 65.979 | 21.213 | 38978 | 20541 |

From 512 to 2048 entries, repeated blocks take 12.5 times longer for four times the entries. Quotes also scale poorly, taking 8.8 times longer. Allocation call counts remain approximately linear; counting allocations alone therefore misses the time growth. The new block implementation repeatedly scans `raw_lines` from the beginning via `drop_lines_before_number`, which is a concrete additional quadratic traversal candidate.

The bounded profile captured 5407 samples over about 5.4 seconds. Matching binary symbols identify 2245 exclusive leaf samples (41.5%) in reference-count decrement routines. This supports investigating list ownership and repeated traversal/cleanup, but does not prove which source expressions clone data or establish whether a different ownership arrangement would be ARC optimized. Preserve and compare profiles after a cursor-based implementation before claiming improvement.

Recorded results, allocation logs, and the sampling profile are in the ignored benchmark artifact directory; deterministic fixture and timing-protocol tests live in `scripts/tests/test_bench_yaml.py`.
