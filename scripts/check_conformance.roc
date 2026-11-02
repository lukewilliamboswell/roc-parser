#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.Env
import cli.OsStr
import cli.Stderr
import cli.Stdout
import src/HarnessPlan

## Run selected independent reviews, retaining evidence from every failure.
main! = |args| {
	requested = args.map(OsStr.display)
	if requested == ["--list"] {
		for review in HarnessPlan.reviews {
			kind = if review.fast "fast" else "scheduled"
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
	setup = Cmd.new("python3").args(["scripts/setup_oracles.py"].concat(HarnessPlan.requirements).map(OsStr.from_str)).stdout(Capture).stderr(Inherit).timeout_ms(600_000).run!().map_err(|_| "Python oracle setup did not complete")?
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
