#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.Env
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/HarnessPlan

## Compose oracle installation, the package compiler probe, review and smoke run.
main! = |args| match check!(args.map(OsStr.display)) {
	Ok({}) => Ok({})
	Err(reason) => {
		Stderr.line!("AsciiDoc: ${reason}; evidence: .roc-parser-tmp/asciidoc")?
		Err(Exit(1))
	}
}

check! = |args| {
	options = options_for(args.keep_if(|arg| arg != "--skip-performance"), ["--generated", "--mutations", "--seed"])?
	generated = option(options, "--generated", "200")
	mutations = option(options, "--mutations", "200")
	seed = option(options, "--seed", "1")
	for value in [generated, mutations, seed] {
		if value == "" or !value.to_utf8().all(|byte| byte >= '0' and byte <= '9') {
			return Err("counts and seed must be non-negative integers")
		}
	}
	roc = Env.var_str!("ROC") ?? "roc"
	Path.create_all!(".roc-parser-tmp/asciidoc").map_err(|_| "cannot create evidence directory")?
	for command in HarnessPlan.asciidoc(roc, generated, mutations, seed, !args.contains("--skip-performance")) {
		Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot print command")?
		match command {
			[program, .. as rest] => Cmd.new(OsStr.from_str(program)).args(rest.map(OsStr.from_str)).exec_cmd!().map_err(|_| "command failed: ${program}")?
			[] => return Err("empty command")
		}
	}
	Ok({})
}

# Options for this harness only; reject unknown or incomplete requests.
options_for : List(Str), List(Str) -> Try(List({ name : Str, value : Str }), Str)
options_for = |args, names| match args {
	[] => Ok([])
	[name, .. as rest] => match name.split_first("=") {
		Ok({ before, after }) if names.contains(before) => options_for(rest, names).map_ok(|more| [{ name: before, value: after }].concat(more))
		_ => match rest {
			[value, .. as more] if names.contains(name) => options_for(more, names).map_ok(|entries| [{ name, value }].concat(entries))
			_ => Err("unknown option or missing value: ${name}")
		}
	}
}

option = |entries, name, fallback| entries.keep_if(|entry| entry.name == name).last().map_ok(|entry| entry.value) ?? fallback
