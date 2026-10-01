# Reproducible benchmark environment for roc-parser (docs/benchmarks.adoc).
#
# Needs the roc-blueprint 0.3.0 CLI, which runs on x86_64 Linux with Nix
# (flakes enabled). From the repository root:
#
#     blueprint update            # create or refresh Blueprint.lock
#     blueprint run bench-quick   # CI-sized smoke run
#     blueprint run bench         # full comparison with Go, Rust and Python
#     blueprint shell             # a shell with every toolchain on PATH
#
# Go, Rust and Python come from the nixpkgs revision Blueprint.lock records.
# Library versions are pinned separately, by bench/compare/rust/Cargo.lock,
# bench/compare/go/go.sum and bench/compare/python/requirements.txt (which
# scripts/bench.py installs into a virtual environment), so they match runs
# made without blueprint. Roc comes from roc-overlay's moving nightly, which
# may not yet name the nightly in .roc-version (the compiler the benchmark
# workflow uses); the report records `roc version`, so check it, or set ROC.
app [config] { pf: platform "https://github.com/lukewilliamboswell/roc-blueprint/releases/download/0.3.0/DdfMePZbeL5hodg7j4B6Jzpm9t555B9SPPAJNQ5WzCR9.tar.zst" }

bench_tools = [
	"rocpkgs.nightly",
	"go",
	"cargo",
	"rustc",
	"python3",
	"git",
]

config = [
	Name("roc-parser-bench"),
	Systems(["x86_64-linux"]),
	Overlay("github:roc-lang/roc-overlay"),
	Shell("default", [Tools(bench_tools)]),
	Task("bench", [Run(["python3", "scripts/bench.py", "--allocations"])]),
	Task("bench-quick", [Run(["python3", "scripts/bench.py", "--quick"])]),
]
