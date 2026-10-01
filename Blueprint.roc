# Reproducible benchmark environment for roc-parser (docs/benchmarks.adoc).
#
# Needs the roc-blueprint 0.3.0 CLI and Nix with flakes. Works on x86_64 Linux
# (CI) and aarch64 macOS. From the repository root:
#
#     nix run github:lukewilliamboswell/roc-blueprint/0.3.0 -- run bench-quick
#
# or enter `nix shell github:lukewilliamboswell/roc-blueprint/0.3.0` and run
#
#     blueprint run bench-quick   # CI-sized smoke run
#     blueprint run bench         # full comparison with Go, Rust and Python
#     blueprint run test          # scripts/all_tests.py
#     blueprint run fuzz-smoke    # scripts/run_fuzz.py smoke
#     blueprint shell             # a shell with every toolchain on PATH
#     blueprint update            # refresh Blueprint.lock (nixpkgs, roc-overlay)
#
# Go, Rust and Python come from the nixpkgs revision Blueprint.lock records.
# Library versions are pinned separately, by bench/compare/rust/Cargo.lock,
# bench/compare/go/go.sum and bench/compare/python/requirements.txt, so they
# match runs made without blueprint.
#
# Roc is pinned to exactly the nightly in .roc-version: roc-overlay publishes
# every nightly as `rocpkgs."<tag>"`. scripts/update_roc_nightly.py rewrites
# the tag below together with .roc-version (a test checks they agree); then run
# `blueprint update` so Blueprint.lock points at an overlay revision that has it.
app [config] { pf: platform "https://github.com/lukewilliamboswell/roc-blueprint/releases/download/0.3.0/DdfMePZbeL5hodg7j4B6Jzpm9t555B9SPPAJNQ5WzCR9.tar.zst" }

bench_tools = [
	"rocpkgs.nightly-2026-09-29-7f11a82",
	"go",
	"cargo",
	"rustc",
	"python3",
	"git",
]

config = [
	Name("roc-parser-bench"),
	Systems(["x86_64-linux", "aarch64-darwin"]),
	Overlay("github:roc-lang/roc-overlay/76befb4facc5c12dbd88a86e89bb82881c03887c"),
	Shell("default", [Tools(bench_tools)]),
	Raw("nix", "shell:default", Attrs([
		# scripts/bench.py records that it ran here, and which Blueprint.lock.
		("ROC_PARSER_BLUEPRINT", Str("1")),
		# The blueprint CLI exports ROC for its own (older) compiler, and the
		# scripts honour ROC; reset it to the pinned roc on PATH.
		("ROC", Str("roc")),
	])),
	Task("bench", [Run(["python3", "scripts/bench.py", "--allocations"])]),
	Task("bench-quick", [Run(["python3", "scripts/bench.py", "--quick"])]),
	Task("test", [Run(["python3", "scripts/all_tests.py"])]),
	Task("fuzz-smoke", [Run(["python3", "scripts/run_fuzz.py", "smoke"])]),
]
