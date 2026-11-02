## The checks a pull request runs, in dependency order.
CheckPlan := [].{
	Section : { name : Str, summary : Str, commands : List(List(Str)) }

	sections : List(Section)
	sections = [
		{ name: "scripts", summary: "Roc orchestration and independent harness tests, including repository policy", commands: [["roc-stable", "version"], ["roc-stable", "test", "scripts/src/CheckPlan.roc"], ["roc-stable", "test", "scripts/src/BundlePlan.roc"], ["roc-stable", "test", "scripts/src/DocExamples.roc"], ["roc-stable", "check", "scripts/check_all.roc"], ["roc-stable", "check", "scripts/test_package.roc"], ["roc-stable", "check", "scripts/bundle.roc"], ["roc-stable", "check", "scripts/check_doc_examples.roc"], ["roc-stable", "fmt", "--check", "scripts/check_all.roc", "scripts/test_package.roc", "scripts/bundle.roc", "scripts/check_doc_examples.roc", "scripts/src"], ["python3", "-m", "unittest", "discover", "-s", "scripts/tests", "-p", "test_*.py"]] },
		{ name: "package", summary: "check, format and test the package, compile-time literals, roc docs, and a release dry run: bundle, examples archive and its spec run against a loopback bundle", commands: [["scripts/test_package.roc"]] },
		{ name: "examples", summary: "the repository's examples against the working tree, compared with tests/spec.json", commands: [["python3", "scripts/test_bundle_examples.py", "--checkout"]] },
		{ name: "docs", summary: "the manual's Roc examples and the API reference audit", commands: [["scripts/check_doc_examples.roc"], ["python3", "scripts/build_docs.py", "--check"]] },
		{ name: "fuzz", summary: "build every registered fuzz target and run a short smoke campaign from its seeds", commands: [["python3", "scripts/run_fuzz.py", "smoke", "all"]] },
		{ name: "conformance", summary: "the fast conformance checks against each format's pinned reference implementation", commands: [["python3", "scripts/check_conformance.py"]] },
		{ name: "asciidoc", summary: "AsciiDoc against the pinned Asciidoctor, and the performance harness smoke run", commands: [["python3", "scripts/check_asciidoc.py"]] },
		{ name: "roc-syntax", summary: "RocSyntax against the compiler's own parser, recovery, mutations and the performance protocol", commands: [["python3", "scripts/check_roc_syntax.py", "--skip-fuzz", "--reference-performance", "smoke"]] },
		{ name: "bench", summary: "benchmark smoke run of the roc-parser drivers; timings here are not measurements", commands: [["python3", "scripts/bench.py", "--quick", "--langs", "roc", "--timeout", "60", "--fail-on-error"]] },
	]

	## Reject the whole request before running anything, and preserve first use.
	select : List(Str) -> Try(List(Section), Str)
	select = |requested| {
		if requested.is_empty() {
			return Ok(sections)
		}
		var $chosen = []
		for name in requested {
			match sections.find_first(|section| section.name == name) {
				Err(_) => return Err("unknown section: ${name}; choose from ${Str.join_with(sections.map(|section| section.name), ", ")}")
				Ok(section) => if !$chosen.any(|item| item.name == name) {
					$chosen = $chosen.append(section)
				}
			}
		}
		Ok($chosen)
	}
}

expect CheckPlan.select([]) == Ok(CheckPlan.sections)
expect CheckPlan.select(["fuzz", "scripts", "fuzz"]).map_ok(|chosen| chosen.map(|section| section.name)) == Ok(["fuzz", "scripts"])
expect CheckPlan.select(["scripts", "typo"]).is_err()
