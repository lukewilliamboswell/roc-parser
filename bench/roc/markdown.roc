app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.27.0/9yjxqdGTP4xSpSEmDazwTmRsehfRZzA19pwEKkR6fj1g.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.Markdown
import Bench

## Benchmark driver: the whole document through Markdown.parse_str, which builds the
## block tree and parses every inline.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	Ok(Markdown.parse_str(input).len() + 1)
}
