# roc-parser's development environments and the tasks CI and contributors run.
#
# Needs the roc-blueprint 0.6.0 `blueprint` binary, the release this file's
# platform URL names (docs/contributing.adoc says how to install it), and the
# latest Roc nightly on PATH as `roc`. From the repository root:
#
#     blueprint run check           # everything a pull request must pass
#     blueprint run check -- fuzz   # one section of it; `-- --list` names them
#     blueprint run fuzz-smoke      # build every fuzz target and replay its seeds
#     blueprint run docs            # the manual (HTML and PDF) and the API reference
#     blueprint run bench-quick     # a short comparison with Go, Rust and Python (Nix)
#     blueprint run bench           # the full comparison (Nix)
#     blueprint tasks               # every task
#
# The package compiler is never pinned here. Every task that builds or tests
# the package expects `roc` of the machine, which in CI is the nightly that
# roc-lang/setup-roc installs. The automation under scripts/ is Python, so no
# environment has a `roc-stable` (docs/maintaining.adoc). Building, testing
# and publishing a release is roc-automation's shared workflow, called by
# .github/workflows/release.yml; it runs no task here.
#
# Zig 0.17.0 builds the compiler's own parser as the RocSyntax oracle. It is
# downloaded and checked against Blueprint.lock, so `check` needs no Nix. Only
# the benchmark environment takes tools from Nix: Go, Rust and Python for the
# comparison drivers, from the nixpkgs revision Blueprint.lock records. Their
# libraries are pinned separately, by bench/compare/rust/Cargo.lock,
# bench/compare/go/go.sum and bench/compare/python/requirements.txt.
app [config] { pf: platform "https://github.com/lukewilliamboswell/roc-blueprint/releases/download/0.6.0/BenWAmrQp3qFLJPgGerHFPzzAn7ouXDew69CCg2LTtQX.tar.zst" }

import pf.Config
import pf.System

zig : System -> Config.Download
zig = |system| {
	base = "https://ziglang.org/download/0.17.0"
	match system {
		"x86_64-linux" => Archive("${base}/zig-x86_64-linux-0.17.0.tar.xz", TarXz, "zig-x86_64-linux-0.17.0/zig")
		"aarch64-darwin" => Archive("${base}/zig-aarch64-macos-0.17.0.tar.xz", TarXz, "zig-aarch64-macos-0.17.0/zig")
		_ => Unavailable
	}
}

config = [
	Name("roc-parser"),
	Systems(["x86_64-linux", "aarch64-darwin"]),
	Packages("nix", From(NixPackages("github:NixOS/nixpkgs/nixos-unstable"))),
	# The package and its release artifacts, built with the machine's nightly.
	Environment("package", [Expect(["roc", "python3", "git"])]),
	# The reference implementations the checks compare with. The Python
	# oracles and the Asciidoctor gem are pinned by scripts/ and installed
	# under .roc-parser-tmp/.
	Environment("oracles", [Extend("package"), Programs([Fetch("zig", zig)]), Expect(["ruby", "gem"])]),
	# The manual builds in Docker (.github/docs.Dockerfile).
	Environment("docs", [Extend("package"), Expect(["docker"])]),
	Environment("bench", [Tools(["nix#go", "nix#cargo", "nix#rustc", "nix#python3", "nix#git"]), Expect(["roc"])]),
	Shell("default", [Use("oracles")]),
	Shell("bench", [Use("bench")]),
	# Everything a pull request must pass; `blueprint run check -- --list`.
	Task("check", [Use("oracles"), Run(["python3", "scripts/check.py"])]),
	# Parts of check, and the longer runs of the scheduled workflows.
	Task("test", [Use("package"), Run(["python3", "scripts/all_tests.py"])]),
	Task("fuzz-smoke", [Use("package"), Run(["python3", "scripts/run_fuzz.py", "smoke"])]),
	Task("fuzz", [Use("package"), Run(["python3", "scripts/run_fuzz.py"])]),
	Task("conformance", [Use("package"), Run(["python3", "scripts/check_conformance.py"])]),
	Task("asciidoc", [Use("oracles"), Run(["python3", "scripts/check_asciidoc.py"])]),
	Task("roc-syntax", [Use("oracles"), Run(["python3", "scripts/check_roc_syntax.py"])]),
	Task("bench", [Use("bench"), Run(["python3", "scripts/bench.py", "--allocations", "--blueprint"])]),
	Task("bench-quick", [Use("bench"), Run(["python3", "scripts/bench.py", "--quick", "--blueprint"])]),
	# The documentation site and its release assets: the manual (HTML and PDF)
	# and the API reference, laid out as GitHub Pages serves them.
	Task("docs", [Use("docs"), Run(["python3", "scripts/release_helpers.py", "package-docs"])]),
]
