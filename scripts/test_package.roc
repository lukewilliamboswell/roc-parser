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
	] {
		run!(command, root)?
	}
	for args in [["build", "0.0.1-dryrun"], ["test-archive"]] {
		Cmd.new("release-package").args(args.map(OsStr.from_str)).env("RP_COMPILER", "roc").env("RP_OUTPUT", OsStr.from_str(Path.display(Path.join(work, "dist")))).env("RP_DIRECTORY", OsStr.from_str(Path.display(Path.join(work, "logs")))).env("GITHUB_OUTPUT", "").exec_cmd!().map_err(|_| "shared release dry run failed: ${Str.join_with(args, " ")}")?
	}
	Ok({})
}

run! = |command, cwd| {
	Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot write command")?
	match command {
		[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).cwd(cwd).exec_cmd!().map_err(|_| "command failed: ${Str.join_with(command, " ")}")
		[] => Err("empty command")
	}
}
