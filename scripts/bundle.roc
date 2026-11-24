#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-24-ff5e544",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Cmd
import src/CommandLine
import weaver.Cli
import cli.OsStr
import cli.Path
import cli.Stdout
import cli.Stderr
import src/BundlePlan

## Bundle every package module, with main.roc first to establish dependencies.
main! = |raw| {
	options = match Cli.parse_or_display_message(CommandLine.bundle, raw, OsStr.to_raw) {
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
	result = bundle!(options)
	match result {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!(Str.inspect(reason))?
			Err(Exit(1))
		}
	}
}

bundle! = |options| {
	if options.output == "" {
		return Err(EmptyOutputDirectory)
	}
	output = Path.absolute!(Path.utf8(options.output)).map_err(|error| OutputPathUnavailable(error))?
	Path.create_all!(output).map_err(|error| OutputDirectoryUnavailable(error))?
	names = Path.list!("package").map_err(|error| ModuleDirectoryUnavailable(error))?.map(|path| Path.filename(path).map_ok(Path.display) ?? "")
	command = ["bundle"].concat(BundlePlan.module_names(names)).concat(["--output-dir", Path.display(output)]).concat(options.forwarded)
	Cmd.new("roc").args(command.map(OsStr.from_str)).cwd("package").call!().map_err(|error| BundleCompilerFailed(error))
}
