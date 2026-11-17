import weaver.Cli
import weaver.Opt
import weaver.Param

## Contributor command schemas. Weaver owns tokenization, conversion and help.
CommandLine := [].{
	fuzz = Cli.assert_valid(
		Cli.finish(
			{
				ci: Opt.flag({ short: "", long: "ci", help: "Select only CI-enabled targets." }),
				no_build: Opt.flag({ short: "", long: "no-build", help: "Verify and reuse existing binaries." }),
				roc: Opt.str({ short: "", long: "roc", help: "Package compiler (default: ROC or roc).", default: Value("") }),
				runs: Opt.u64({ short: "", long: "runs", help: "Smoke exploration count (default: 2000).", default: Value(2000) }),
				seconds: Opt.u64({ short: "", long: "seconds", help: "Campaign duration (default: 600 seconds).", default: Value(600) }),
				timeout: Opt.maybe_u64({ short: "", long: "timeout", help: "Override per-input timeout in seconds." }),
				max_input_size: Opt.maybe_u64({ short: "", long: "max-input-size", help: "Override target input size bound." }),
				memory_limit: Opt.u64({ short: "", long: "memory-limit", help: "Memory limit in MiB (default: 2048).", default: Value(2048) }),
				corpus_dir: Opt.str({ short: "", long: "corpus-dir", help: "Override the corpus directory.", default: Value("") }),
				jobs: Opt.u64({ short: "j", long: "jobs", help: "Maximum concurrent builds (default: 1).", default: Value(1) }),
				mode: Param.str({ name: "mode", help: "list, describe, prepare, build, smoke, campaign, show, replay or minimize.", default: Value("list") }),
				arguments: Param.str_list({ name: "targets-or-paths", help: "Targets, or TARGET INPUT [OUTPUT] for artifact commands." }),
			}.Cli,
			metadata("fuzz", "Build, explore and replay native Roc properties."),
		),
	)
	literals = Cli.assert_valid(
		Cli.finish(
			{
				filter: Opt.str({ short: "", long: "filter", help: "Select contracts by name substring.", default: Value("") }),
				roc: Opt.str({ short: "", long: "roc", help: "Package compiler (default: ROC or roc).", default: Value("") }),
			}.Cli,
			metadata("literals", "Check compiler quotations, rejection diagnostics and imported assets."),
		),
	)
	generate = Cli.assert_valid(
		Cli.finish(
			{
				check: Opt.flag({ short: "", long: "check", help: "Check generated source freshness without writing." }),
				generator: Param.str({ name: "generator", help: "Generator to run: vocabulary.", default: NoDefault }),
			}.Cli,
			metadata("generate", "Regenerate repository-owned source tables."),
		),
	)
	check = Cli.assert_valid(
		Cli.finish(
			{
				list: Opt.flag({ short: "", long: "list", help: "List available sections." }),
				keep_going: Opt.flag({ short: "", long: "keep-going", help: "Continue with subsequent sections after a failure." }),
				sections: Param.str_list({ name: "sections", help: "Sections to run; empty selects all." }),
			}.Cli,
			metadata("check", "Run the native checks required for a pull request."),
		),
	)
	conformance = Cli.assert_valid(
		Cli.finish(
			{
				list: Opt.flag({ short: "", long: "list", help: "List independent reviews." }),
				reviews: Param.str_list({ name: "reviews", help: "Review names, all, or empty for fast reviews." }),
			}.Cli,
			metadata("conformance", "Run local independent parser reviews."),
		),
	)
	asciidoc = Cli.assert_valid(
		Cli.finish(
			{
				generated: Opt.u64({ short: "", long: "generated", help: "Generated models (default: 200).", default: Value(200) }),
				mutations: Opt.u64({ short: "", long: "mutations", help: "Mutated inputs (default: 200).", default: Value(200) }),
				seed: Opt.u64({ short: "", long: "seed", help: "Deterministic seed (default: 1).", default: Value(1) }),
				skip_performance: Opt.flag({ short: "", long: "skip-performance", help: "Skip the performance smoke run." }),
			}.Cli,
			metadata("asciidoc", "Compare generated AsciiDoc with the pinned independent reference."),
		),
	)
	doc_examples = Cli.assert_valid(Cli.finish(Param.str({ name: "filename-substring", help: "Select example filenames.", default: Value("") }), metadata("doc-examples", "Check and run the manual's executable examples.")))
	bundle = Cli.assert_valid(
		Cli.finish(
			{
				output: Opt.str({ short: "", long: "output-dir", help: "Bundle output directory (default: dist).", default: Value("dist") }),
				forwarded: Param.str_list({ name: "compiler-args", help: "Compiler arguments after --." }),
			}.Cli,
			metadata("bundle", "Bundle the package; use -- before additional compiler flags."),
		),
	)
	positive : Str, U64 -> Try(U64, [InvalidPositiveInteger({ option : Str, value : U64 })])
	positive = |option, value| if value == 0 Err(InvalidPositiveInteger({ option, value })) else Ok(value)
}

metadata = |name, description| { name, version: "dev", authors: ["roc-parser contributors"], description, text_style: Plain }

expect CommandLine.positive("runs", 0).is_err()
expect {
	var $valid = True
	for number in List.repeat({}, 2000).map_with_index(|_, index| index + 1) {
		parsed = Cli.parse_or_display_message(CommandLine.fuzz, ["smoke", "csv", "--runs=${number.to_str()}", "-j", "2"], |text| Utf8(text))
		$valid = $valid and parsed.map_ok(|args| (args.mode, args.arguments, args.runs, args.jobs)) == Ok(("smoke", ["csv"], number, 2))
	}
	$valid
}
expect Cli.parse_or_display_message(CommandLine.literals, ["--filter"], |text| Utf8(text)).is_err()
expect Cli.parse_or_display_message(CommandLine.fuzz, ["--runs", "bad"], |text| Utf8(text)).is_err()
expect Cli.parse_or_display_message(CommandLine.fuzz, ["--runs", "1", "--runs", "2"], |text| Utf8(text)).is_err()
expect Cli.parse_or_display_message(CommandLine.generate, ["vocabulary", "--unknown"], |text| Utf8(text)).is_err()
expect Cli.parse_or_display_message(CommandLine.bundle, ["--output-dir=dist", "--", "--opt=speed"], |text| Utf8(text)).map_ok(|args| args.forwarded) == Ok(["--opt=speed"])

expect {
	match Cli.parse_or_display_message(CommandLine.fuzz, ["--help"], |text| Utf8(text)) {
		Err(Help(message)) => message.contains("--runs") and message.contains("--jobs")
		_ => False
	}
}
expect {
	match Cli.parse_or_display_message(CommandLine.check, ["--version"], |text| Utf8(text)) {
		Err(Version("dev")) => True
		_ => False
	}
}
expect Cli.parse_or_display_message(CommandLine.asciidoc, ["--generated=0", "--seed=18446744073709551615"], |text| Utf8(text)).map_ok(|args| args.generated) == Ok(0)
