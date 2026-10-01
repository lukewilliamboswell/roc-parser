app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Markdown
import parser.Utf8
import Bench

## Benchmark driver: the whole document through Markdown.all, which builds the
## block tree and parses every inline.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match Utf8.parse_str(Markdown.all, input) {
		Ok(blocks) => Ok(blocks.len() + 1)
		Err(_) => Err(1)
	}
}
