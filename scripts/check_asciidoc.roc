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
import src/ReleasePlan

## Compose oracle installation, the package compiler probe, review and smoke run.
main! = |args| match check!(args.map(OsStr.display)) {
	Ok({}) => Ok({})
	Err(reason) => {
		Stderr.line!("AsciiDoc: ${reason}; evidence: .roc-parser-tmp/asciidoc")?
		Err(Exit(1))
	}
}

check! = |args| {
	options = ReleasePlan.options_for(args.keep_if(|arg| arg != "--skip-performance"), ["--generated", "--mutations", "--seed"])?
	generated = ReleasePlan.option(options, "--generated", "200")
	mutations = ReleasePlan.option(options, "--mutations", "200")
	seed = ReleasePlan.option(options, "--seed", "1")
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
