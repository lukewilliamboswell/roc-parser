app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.AsciiDoc
import Bench

## Parse and construct a complete document; rendering is a separate workload.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |source| AsciiDoc.parse_str(source, {}).map_ok(|document| document.blocks.len() + document.title.count_utf8_bytes() + 1).map_err(|_| 1)
