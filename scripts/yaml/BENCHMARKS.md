# YAML benchmarks

Benchmarking, profiling and allocation counting are documented in the
manual's Benchmarks chapter, `docs/benchmarks.adoc`; measured results and
their analysis are in `docs/performance.adoc`.

`scripts/bench.py` is the harness for every format, with Go, Rust and Python
comparators. `scripts/bench_yaml.py` remains for comparing two revisions of
the YAML parser on its microbenchmark fixtures (`--parser-root`, `--sizes`,
`--only`, `--allocations`); it is described at the end of that chapter.
