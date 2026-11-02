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
import src/ReleasePlan

## Validate a dispatch, or choose the shared utility's unpublished dry-run version.
## Common package comparison is owned by the locked release-package executable.
main! = |args| {
	match validate!(args.map(OsStr.display)) {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!("release: ${reason}")?
			Err(Exit(1))
		}
	}
}

validate! = |args| {
	options = ReleasePlan.options(args)?
	version = ReleasePlan.option(options, "--release-version", "")
	default_branch = ReleasePlan.option(options, "--default-branch", "main")
	event = ReleasePlan.option(options, "--event", optional!("GITHUB_EVENT_NAME", "")?)
	ref = ReleasePlan.option(options, "--ref", optional!("GITHUB_REF", "")?)
	repository = ReleasePlan.option(options, "--repo", optional!("GITHUB_REPOSITORY", "lukewilliamboswell/roc-parser")?)
	if !ReleasePlan.repository(repository) {
		return Err("repository must be owner/name")
	}
	request = ReleasePlan.request(event, version.trim(), ref, default_branch)?
	if !request.dry_run {
		for resource in ["git/ref/tags", "releases/tags"] {
			found = capture!(["gh", "api", "repos/${repository}/${resource}/${request.version}"])?
			if found.status == Exited(0) {
				return Err("the tag or release ${request.version} already exists; a release is never replaced")
			}
			if !Str.from_utf8_lossy(found.stderr_bytes).contains("HTTP 404") {
				return Err("cannot determine whether ${request.version} already exists: ${Str.from_utf8_lossy(found.stderr_bytes)}")
			}
		}
	}
	# The utility validates the version, authored notes and each package's bump.
	compared = Cmd.new("release-package").args(["check-version", request.version].map(OsStr.from_str)).env("RP_REPOSITORY", OsStr.from_str(repository)).stdout(Capture).stderr(Capture).timeout_ms(600_000).output_limit(50_000_000).run!().map_err(|_| "release-package did not complete")?
	Stdout.write_bytes!(compared.stdout_bytes).map_err(|_| "cannot print comparison output")?
	Stderr.write_bytes!(compared.stderr_bytes).map_err(|_| "cannot print comparison errors")?
	if compared.status != Exited(0) {
		previous = capture!(["gh", "api", "repos/${repository}/releases/latest", "--jq", ".tag_name"])?
		output = Str.from_utf8_lossy(compared.stdout_bytes).concat(Str.from_utf8_lossy(compared.stderr_bytes))
		if previous.status != Exited(0) or !ReleasePlan.unreadable_major(request.version, Str.from_utf8_lossy(previous.stdout_bytes).trim(), output) {
			return Err("release-package refused the version")
		}
		Stdout.line!("::warning::the previous package API is unreadable; this major release cannot understate its changes").map_err(|_| "cannot print warning")?
	}
	outputs = ReleasePlan.outputs(request)
	Stdout.write!(outputs).map_err(|_| "cannot print release request")?
	output = ReleasePlan.option(options, "--github-output", "")
	if output != "" {
		path = Path.utf8(output)
		exists = Path.exists!(path).map_err(|_| "cannot inspect GitHub output")?
		before = if exists Path.read_utf8!(path).map_err(|_| "cannot read GitHub output")? else ""
		Path.write_utf8!(path, before.concat(outputs)).map_err(|_| "cannot write GitHub output")?
	}
	Ok({})
}

optional! = |name, fallback| match Env.var_str!(OsStr.from_str(name)) {
	Ok(value) if value != "" => Ok(value)
	Ok(_) | Err(VarNotFound(_)) => Ok(fallback)
	Err(_) => Err("${name} is not UTF-8")
}

capture! = |command| match command {
	[program, .. as args] => Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).stdout(Capture).stderr(Capture).timeout_ms(60_000).output_limit(2_000_000).run!().map_err(|_| "${program} did not complete")
	[] => Err("empty command")
}
