#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-15-f25da45",
}

import cli.Cmd
import cli.Duration
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/BundlePlan
import src/DocExamples

## Format-check, check, test and run the manual's examples with the nightly.
## An optional substring selects example filenames. Run from the repo root.
main! = |args| {
	filter = match args.map(OsStr.display) {
		[] => ""
		[value] => value
		_ => {
			Stderr.line!("usage: check_doc_examples.roc -- [filename-substring]")?
			return Err(Exit(2))
		}
	}
	names = Path.list!("docs/examples")?.map(|path| Path.filename(path).map_ok(Path.display) ?? "")
	apps = BundlePlan.module_names(names).keep_if(|name| names.contains(name) and name.contains(filter))
	if apps.is_empty() {
		Stderr.line!("no documentation examples match ${Str.inspect(filter)}")?
		return Err(Exit(1))
	}
	var $failures = []
	for example in apps {
		Stdout.line!("docs/examples/${example} ...")?
		match check!(example) {
			Ok({}) => Stdout.line!("ok")?
			Err(reason) => {
				Stderr.line!("FAILED ${example}: ${reason}")?
				$failures = $failures.append(example)
			}
		}
	}
	if !$failures.is_empty() {
		Stderr.line!("documentation examples failed: ${Str.join_with($failures, ", ")}")?
		Err(Exit(1))
	} else {
		Stdout.line!("All ${apps.len().to_str()} documentation examples format-check, check, test and run.")?
		Ok({})
	}
}

check! = |name| {
	for args in [["fmt", "--check", name], ["check", name]] {
		_ = run!(args)?
	}
	source = Path.read_utf8!(Path.utf8("docs/examples/${name}")).map_err(|_| "cannot read example")?
	if DocExamples.needs_tests(source) {
		_ = run!(["test", name])?
	}
	output = run!([name])?
	expected = Path.utf8("docs/examples/${name.drop_suffix(".roc")}.expected")
	if Path.is_file!(expected).map_err(|_| "cannot inspect expected stdout")? {
		DocExamples.check_stdout(output, Path.read_utf8!(expected).map_err(|_| "cannot read expected stdout")?)?
	}
	Ok({})
}

run! = |args| {
	ran = Cmd.new("roc").args(args.map(OsStr.from_str)).cwd("docs/examples").stdout(Capture).stderr(Capture).timeout(Duration.millis(600_000)).output_limit(MaxBytes(50_000_000)).run!().map_err(|_| "roc did not complete: ${Str.join_with(args, " ")}")?
	if ran.status != Exited(0) {
		return Err("roc ${Str.join_with(args, " ")} failed:\n${Str.from_utf8_lossy(ran.stdout_bytes)}${Str.from_utf8_lossy(ran.stderr_bytes)}")
	}
	Str.from_utf8(ran.stdout_bytes).map_err(|_| "roc stdout was not UTF-8")
}
