app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.CSV
import parser.Parser
import Bench

## Benchmark driver: CSV.parse_with decoding every field to a Str.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match CSV.parse_with(Parser.many(CSV.field(CSV.string)), input) {
		Ok(rows) => Ok(rows.fold(0, |total, row| row.fold(total + 1, |sum, field| sum + field.count_utf8_bytes().to_u64())))
		Err(_) => Err(1)
	}
}
