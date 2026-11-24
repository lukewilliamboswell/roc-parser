#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-24-ff5e544",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Cmd
import cli.Env
import src/CommandLine
import weaver.Cli
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/HarnessPlan

## Compose oracle installation, the package compiler probe, review and smoke run.
main! = |raw| {
	options = match Cli.parse_or_display_message(CommandLine.asciidoc, raw, OsStr.to_raw) {
		Ok(config) => config
		Err(Help(message)) | Err(Version(message)) => {
			Stdout.line!(message)?
			return Ok({})
		}
		Err(InvalidUsage(message)) => {
			Stderr.line!(message)?
			return Err(Exit(2))
		}
	}
	match check!(options) {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("AsciiDoc: ${reason}; evidence: .roc-parser-tmp/asciidoc")?
			Err(Exit(1))
		}
	}
}

check! = |args| {
	generated = args.generated.to_str()
	mutations = args.mutations.to_str()
	seed = args.seed.to_str()
	roc = Env.var_str!("ROC") ?? "roc"
	Path.create_all!(".roc-parser-tmp/asciidoc").map_err(|_| "cannot create evidence directory")?
	for command in HarnessPlan.asciidoc(roc, generated, mutations, seed, !args.skip_performance) {
		Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| "cannot print command")?
		match command {
			[program, .. as rest] => Cmd.new(OsStr.from_str(program)).args(rest.map(OsStr.from_str)).call!().map_err(|_| "command failed: ${program}")?
			[] => return Err("empty command")
		}
	}
	Ok({})
}
