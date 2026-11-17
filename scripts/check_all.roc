#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Cmd
import src/CommandLine
import weaver.Cli
import cli.OsStr
import cli.Stderr
import cli.Stdout
import src/CheckPlan

## Run from the repository root, through `blueprint run check`.
## Arguments: [--list] [--keep-going] [SECTION ...].
main! = |raw| {
	options = match Cli.parse_or_display_message(CommandLine.check, raw, OsStr.to_raw) {
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

	if options.list {
		for section in CheckPlan.sections {
			Stdout.line!("${section.name}: ${section.summary}")?
		}
		return Ok({})
	}
	keep_going = options.keep_going
	sections = match CheckPlan.select(options.sections) {
		Ok(found) => found
		Err(reason) => {
			Stderr.line!(reason)?
			return Err(Exit(2))
		}
	}
	Cmd.new("roc").args(["version"].map(OsStr.from_str)).exec_cmd!()?
	var $failures = []
	for section in sections {
		Stdout.line!("\n== ${section.name}: ${section.summary}")?
		var $passed = Bool.True
		for command in section.commands {
			Stdout.line!("+ ${Str.join_with(command, " ")}")?
			match command {
				[program, .. as rest] => {
					match Cmd.new(OsStr.from_str(program)).args(rest.map(OsStr.from_str)).exec_cmd!() {
						Ok({}) => {}
						Err(_) => {
							$passed = Bool.False
							break
						}
					}
				}
				[] => {
					$passed = Bool.False
					break
				}
			}
		}
		if $passed {
			Stdout.line!("PASS ${section.name}")?
		} else {
			$failures = $failures.append(section.name)
			Stderr.line!("FAIL ${section.name}")?
			if !keep_going {
				break
			}
		}
	}
	if $failures.is_empty() {
		Ok({})
	} else {
		Stderr.line!("check failed: ${Str.join_with($failures, ", ")}")?
		Err(Exit(1))
	}
}
