#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/BundlePlan

## Package validation and release dry run with the floating nightly.
main! = |_args| {
	result = test!()
	match result {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!(reason)?
			Err(Exit(1))
		}
	}
}

test! = || {
	root = Path.absolute!(".").map_err(|_| "cannot resolve repository root")?
	work = Path.join(root, ".roc-parser-tmp/roc-parser-ci")
	# Only remove the dedicated output directory, never a caller-supplied path.
	run!(["rm", "-rf", Path.display(work)], root)?
	run!(["mkdir", "-p", Path.display(Path.join(work, "docs")), Path.display(Path.join(work, "bundle"))], root)?
	for command in [
		["roc", "version"],
		["roc", "check", "package/main.roc"],
		["roc", "fmt", "--check", "package"],
		["roc", "test", "package/main.roc"],
		["python3", "scripts/check_literals.py"],
		["roc", "docs", "package/main.roc", "--output=${Path.display(Path.join(work, "docs"))}"],
	] {
		run!(command, root)?
	}
	names = Path.list!("package").map_err(|_| "cannot list package modules")?.map(|path| Path.filename(path).map_ok(Path.display) ?? "")
	args = ["bundle"].concat(BundlePlan.module_names(names)).concat(["--output-dir", Path.display(Path.join(work, "bundle"))])
	ran = Cmd.new("roc").args(args.map(OsStr.from_str)).cwd(Path.join(root, "package")).stdout(Capture).stderr(Inherit).timeout_ms(600_000).run!().map_err(|_| "roc bundle did not complete")?
	Stdout.write_bytes!(ran.stdout_bytes).map_err(|_| "cannot print bundle output")?
	if ran.status != Exited(0) {
		return Err("roc bundle failed")
	}
	output = Str.from_utf8(ran.stdout_bytes).map_err(|_| "bundle output is not UTF-8")?
	printed_path = BundlePlan.bundle_path(output)?
	bundle = if printed_path.starts_with("/") Path.utf8(printed_path) else Path.join(Path.join(root, "package"), printed_path)
	run!(["python3", "scripts/test_bundle_examples.py", "--bundle-path", Path.display(bundle)], root)
}

run! = |command, cwd| {
	Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot write command")?
	match command {
		[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).cwd(cwd).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
		[] => Err("empty command")
	}
}
