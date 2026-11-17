#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/FuzzPlan
import src/Process

main! = |_args| {
	match check!() {
		Ok({}) => Stdout.line!("Native automation contracts passed.")
		Err(error) => {
			Stderr.line!(Str.inspect(error))?
			Err(Exit(1))
		}
	}
}

check! = || {
	for name in ["Arguments", "FuzzPlan", "LiteralContract", "Vocabulary"] {
		_ = Process.run!(["roc-stable", "test", "scripts/src/${name}.roc"], ".", [], 60_000)?
	}
	for name in ["fuzz", "check_literals", "generate", "test_automation"] {
		_ = Process.run!(["roc-stable", "check", "scripts/${name}.roc"], ".", [], 60_000)?
	}
	_ = Process.run!(["roc-stable", "fmt", "--check", "scripts/fuzz.roc", "scripts/check_literals.roc", "scripts/generate.roc", "scripts/test_automation.roc", "scripts/src"], ".", [], 60_000)?
	declared = Path.list!("fuzz")?.keep_if(|path| Path.display(path).ends_with(".roc"))
	for path in declared {
		source = Path.read_utf8!(path)?
		if source.starts_with("app [target]") {
			filename = Path.filename(path).map_ok(Path.display) ?? ""
			name = filename.drop_suffix(".roc")
			if !FuzzPlan.entries.any(|entry| entry.name == name) and !FuzzPlan.measurement_only.contains(name) {
				return Err(UnregisteredTarget(name))
			}
		}
	}
	workflow = Path.read_utf8!(".github/workflows/fuzz.yml")?
	matrix = workflow.split_on("\n").keep_if(|line| line.starts_with("          - ")).map(|line| line.trim().drop_prefix("- "))
	if matrix != FuzzPlan.entries.keep_if(|entry| entry.scheduled).map(|entry| entry.name) {
		return Err(ScheduledTargetMismatch)
	}
	for entry in FuzzPlan.entries {
		path = Path.utf8("fuzz/${entry.name}.roc")
		source = Path.read_utf8!(path)?
		if !source.starts_with("app [target]") {
			return Err(NotATarget(entry.name))
		}
		if !source.contains("https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst") {
			return Err(UnreviewedFuzzPlatform(entry.name))
		}
		if entry.seeds != "" {
			parsed : Try(List(Str), _)
			parsed = Json.parse(Path.read_utf8!(Path.utf8(entry.seeds))?)
			for seed in parsed.map_err(|_| InvalidSeeds(entry.seeds))? {
				_ = FuzzPlan.encode(seed, entry.encoding)?
			}
		}
		if entry.dictionary != "" {
			dictionary = Path.read_utf8!(Path.utf8(entry.dictionary))?
			if dictionary.trim() == "" {
				return Err(EmptyDictionary(entry.dictionary))
			}
		}
	}
	_ = Process.run!(["scripts/generate.roc", "--", "vocabulary", "--check"], ".", [], 60_000)?
	Ok({})
}
