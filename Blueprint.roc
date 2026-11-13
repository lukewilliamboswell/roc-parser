# roc-parser's development environments and the tasks CI and contributors run.
#
# Needs the roc-blueprint 0.13.0 `blueprint` binary, the release this file's
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
# roc-lang/setup-roc installs. Roc orchestration uses the separate fixed
# `roc-stable` compiler; Python remains for independent parser harnesses.
#
# Zig 0.17.0 builds the compiler's own parser as the RocSyntax oracle. It is
# downloaded and checked against Blueprint.lock, so `check` needs no Nix. Only
# the benchmark environment takes tools from Nix: Go, Rust and Python for the
# comparison drivers, from the nixpkgs revision Blueprint.lock records. Their
# libraries are pinned separately, by bench/compare/rust/Cargo.lock,
# bench/compare/go/go.sum and bench/compare/python/requirements.txt.
app [config] { pf: platform "https://github.com/lukewilliamboswell/roc-blueprint/releases/download/0.13.0/DBtzmMfrSgt6nCGVAcZYCVPxWbhpqw3MXpm6sdQzMn4v.tar.zst" }

import pf.Config
import pf.System
import pf.Recipes

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
	Environment("tooling", [
		Programs([Fetch("roc-stable", Recipes.roc_nightly("nightly-2026-10-09-258ab27"))]),
		RocPackages(["https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst"]),
	]),
	Environment("package", [Extend("tooling"), Programs([Fetch("release-package", Recipes.file("https://github.com/lukewilliamboswell/roc-automation/releases/download/tools-0.10.1/release-package-{system}"))]), Expect(["roc", "python3", "git", "rm", "mkdir", "cp", "tar", "unzip", "zip", "chmod", "touch", "env"])]),
	Environment("inventory", [Extend("package"), Programs([Fetch("blueprint-sbom", Recipes.file("https://github.com/lukewilliamboswell/roc-blueprint/releases/download/0.13.0/blueprint-sbom-{system}"))]), Expect(["blueprint", "openssl"])]),
	# The reference implementations the checks compare with. The Python
	# oracles and the Asciidoctor gem are pinned by scripts/ and installed
	# under .roc-parser-tmp/.
	Environment("oracles", [Extend("inventory"), Programs([Fetch("zig", zig)]), Expect(["ruby", "gem"])]),
	# Shared release utilities run without a local build compiler.
	Environment("release", [Extend("inventory"), Expect(["gh"])]),
	Environment("docs", [Extend("release"), Programs([Fetch("roc-docs", Recipes.file("https://github.com/lukewilliamboswell/roc-automation/releases/download/tools-0.10.1/roc-docs-{system}"))])]),
	Environment("bench", [Tools(["nix#go", "nix#cargo", "nix#rustc", "nix#python3", "nix#git"]), Expect(["roc"])]),
	Shell("default", [Use("oracles")]),
	Shell("bench", [Use("bench")]),
	# Everything a pull request must pass; `blueprint run check -- --list`.
	Task("check", [Use("oracles"), Run(["scripts/check_all.roc", "--"])]),
	# Parts of check, and the longer runs of the scheduled workflows.
	Task("test", [Use("package"), Run(["scripts/test_package.roc"])]),
	Task("fuzz-smoke", [Use("package"), Run(["python3", "scripts/run_fuzz.py", "smoke"])]),
	Task("fuzz", [Use("package"), Run(["python3", "scripts/run_fuzz.py"])]),
	Task("doc-examples", [Use("package"), Run(["scripts/check_doc_examples.roc", "--"])]),
	Task("conformance", [Use("package"), Run(["scripts/check_conformance.roc", "--"])]),
	Task("asciidoc", [Use("oracles"), Run(["scripts/check_asciidoc.roc", "--"])]),
	Task("roc-syntax", [Use("oracles"), Run(["python3", "scripts/check_roc_syntax.py"])]),
	Task("bench", [Use("bench"), Run(["python3", "scripts/bench.py", "--allocations", "--blueprint"])]),
	Task("bench-quick", [Use("bench"), Run(["python3", "scripts/bench.py", "--quick", "--blueprint"])]),
	# Shared packaging and documentation; the release workflow owns publishing.
	Task("sbom-composition", [Use("release"), Run(["scripts/sbom_composition.roc"])]),
	Task("docs-path", [Use("docs"), Run(["scripts/docs_path.roc"])]),
	Task("docs", [Use("docs"), Run(["roc-docs", "build", "--workspace", ".", "--api-entrypoint", "package/main.roc", "--roc-command", "roc", "--pdf-filename", "roc-parser.pdf", "--site-url", "https://lukewilliamboswell.github.io/roc-parser/"])]),
	Task("api-docs", [Use("package"), Run(["roc", "docs", "package/main.roc", "--output=.docs-out/api"])]),
	Task("release-package", [Use("release"), Run(["release-package"])]),
	Task("bundle", [Use("package"), Run(["scripts/bundle.roc", "--"])]),
	Task("examples", [Use("package"), Run(["release-package", "test-examples"])]),
	Task("test-examples-archive", [Use("package"), Run(["release-package", "test-archive"])]),
]
