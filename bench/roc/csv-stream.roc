app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.CSV
import parser.Parser
import Bench

## Parse-only comparison: Bench preloads input; this driver discards rows.
## Use csv-file-stream for the bounded-source memory measurement.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	bytes = input.to_utf8()
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|pos| {
			if pos >= bytes.len() {
				Err(NoMore)
			} else {
				len = U64.min(65_536, bytes.len() - pos)
				Ok((bytes.sublist({ start: pos, len }), pos + len))
			}
		},
	)
	var $sum = 0.U64
	for event in (CSV.positional_decoder({ row: Parser.many(CSV.field(CSV.string({}))) })).read_iter(chunks) {
		match event {
			Ok(Value(row)) => {
				$sum = row.fold($sum + 1, |total, field| total + Bench.weighted_utf8(field))
			}
			Ok(Finished(_)) => return Ok($sum)
			_ => return Err(1)
		}
	}
	Err(1)
}
