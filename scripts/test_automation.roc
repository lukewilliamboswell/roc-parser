#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-23-b5c77fc",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/CommandLine
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
	_ = Process.run!(["roc-stable", "test", "scripts/test_automation.roc"], ".", [], 60_000)?
	_ = Process.run!(["roc-stable", "test", "scripts/fuzz.roc"], ".", [], 60_000)?
	for name in ["FuzzPlan", "LiteralContract", "Vocabulary"] {
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
			if !FuzzPlan.entries.any(|entry| "${entry.family}Properties" == name) and !FuzzPlan.measurement_only.contains(name) {
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
		path = Path.utf8("fuzz/${entry.family}Properties.roc")
		source = Path.read_utf8!(path)?
		if !source.starts_with("app [target]") {
			return Err(NotATarget(entry.name))
		}
		if !source.contains("https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst") {
			return Err(UnreviewedFuzzPlatform(entry.name))
		}
		stem = Str.join_with(entry.name.split_on("-").map(|part| Str.from_utf8_lossy(part.to_utf8().map_with_index(|byte, index| if (index == 0) byte - 32 else byte))), "")
		module_name = "${stem}Property"
		property = Path.read_utf8!(Path.utf8("fuzz/${module_name}.roc"))?
		if !source.contains("${module_name}.target") or !property.contains("name: \"${entry.name}\"") {
			return Err(UnregisteredTarget(entry.name))
		}
		if entry.seeds != "" {
			for seed in FuzzPlan.seed_values(entry.name) {
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
	_ = Process.run!(["scripts/generate.roc", "vocabulary", "--check"], ".", [], 60_000)?
	Ok({})
}
