#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-23-b5c77fc",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Cmd
import cli.Duration
import cli.Env
import src/CommandLine
import weaver.Cli
import cli.OsStr
import cli.Stderr
import cli.Stdout
import src/HarnessPlan

## Run selected independent reviews, retaining evidence from every failure.
main! = |raw| {
	options = match Cli.parse_or_display_message(CommandLine.conformance, raw, OsStr.to_raw) {
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
	requested = options.reviews
	if options.list {
		for review in HarnessPlan.reviews {
			kind = if (review.fast) "fast" else "scheduled"
			Stdout.line!("${review.name}: ${kind}; ${Str.join_with(review.args, " ")}")?
		}
		return Ok({})
	}
	match reviews!(requested) {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("conformance: ${reason}")?
			Err(Exit(1))
		}
	}
}

reviews! = |requested| {
	selected = HarnessPlan.select(requested)?
	# One fingerprinted Python environment serves every selection.
	setup = Cmd.new("python3").args(["scripts/setup_oracles.py"].concat(HarnessPlan.requirements).map(OsStr.from_str)).stdout(Capture).stderr(Inherit).timeout(Duration.millis(600_000)).run!().map_err(|_| "Python oracle setup did not complete")?
	if setup.status != Exited(0) {
		return Err("Python oracle setup failed")
	}
	printed = Str.from_utf8(setup.stdout_bytes).map_err(|_| "oracle setup output must be UTF-8")?
	Stdout.write!(printed).map_err(|_| "cannot print setup output")?
	python = printed.trim().split_on("\n").last().map_err(|_| "oracle setup returned no interpreter")?
	parent = python.split_last("/").map_ok(|parts| parts.before).map_err(|_| "oracle interpreter must have a directory")?
	path = Env.var_str!("PATH").map_err(|_| "PATH is required")?
	var $failed = []
	for review in selected {
		Stdout.line!("== ${review.name}: ${Str.join_with(review.args, " ")}").map_err(|_| "cannot print review")?
		ran = Cmd.new(OsStr.from_str(python)).args(review.args.map(OsStr.from_str)).env("PATH", OsStr.from_str("${parent}:${path}")).run!().map_err(|_| "review did not complete: ${review.name}")?
		if ran.status != Exited(0) {
			$failed = $failed.append(review.name)
		}
	}
	if $failed.is_empty() {
		Ok({})
	} else {
		Err("reviews failed: ${Str.join_with($failed, ", ")}")
	}
}
