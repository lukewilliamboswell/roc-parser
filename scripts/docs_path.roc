#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}

import cli.Env
import cli.Path
import cli.Stdout

## The shared workflow needs the persistent tool behind Blueprint's PATH links.
main! = |_args| {
	for directory in Env.var_str!("PATH")?.split_on(":") {
		candidate = Path.join(Path.utf8(directory), "roc-docs")
		if Path.exists!(candidate)? {
			resolved = Path.canonicalize!(candidate)?
			if Path.is_file!(resolved)? {
				return Stdout.line!(Path.display(resolved))
			}
		}
	}
	Err(Exit(1))
}
