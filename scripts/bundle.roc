#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Cmd
import cli.OsStr
import cli.Path
import cli.Stderr
import src/BundlePlan

## Bundle every package module, with main.roc first to establish dependencies.
main! = |args| {
	result = bundle!(args.map(OsStr.display))
	match result {
		Ok({}) => Ok({})
		Err(reason) => {
			Stderr.line!(reason)?
			Err(Exit(1))
		}
	}
}

bundle! = |args| {
	options = BundlePlan.options(args)?
	output = Path.absolute!(Path.utf8(options.output)).map_err(|_| "cannot resolve output directory")?
	Path.create_all!(output).map_err(|_| "cannot create output directory")?
	names = Path.list!("package").map_err(|_| "cannot list package")?.map(|path| Path.filename(path).map_ok(Path.display) ?? "")
	command = ["bundle"].concat(BundlePlan.module_names(names)).concat(["--output-dir", Path.display(output)]).concat(options.forwarded)
	Cmd.new("roc").args(command.map(OsStr.from_str)).cwd("package").exec_cmd!().map_err(|_| "roc bundle failed")
}
