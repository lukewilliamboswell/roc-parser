app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.CSV
import Bench

## Benchmark driver: collect every CSV field as a Str.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	match CSV.parse_string_records(input) {
		Ok(rows) => Ok(rows.fold(0, |total, row| row.fold(total + 1, |sum, field| sum + Bench.weighted_utf8(field))))
		Err(_) => Err(1)
	}
}

expect {
	["", "ASCII", "café 日本 🐦", "1234567890123456789012", "12345678901234567890123", "123456789012345678901234", "\u(0000)\r\n"].all(
		|text|
			Bench.weighted_utf8(text) == text.to_utf8().iter().with_index().fold(0.U64, |sum, pair| sum + (pair.0 + 1) * pair.1.to_u64()),
	)
}
