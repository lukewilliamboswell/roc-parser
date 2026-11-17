app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.QIF
import parser.Streaming
import Bench

## Parse-only consumption: Bench preloads input; 64 KiB chunks are decoded
## through the public streaming API and each typed transaction is discarded.
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
	for event in Streaming.read_iter(chunks, QIF.transaction_decoder({ encoding: Utf8, date_order: DayFirst })) {
		match event {
			Ok(Value(Transaction(row))) => {
				$sum = $sum + 1 + Bench.weighted_utf8(row.payee) + Bench.weighted_utf8(row.memo)
			}
			Ok(Value(_)) => {}
			Ok(Finished(_)) => return Ok($sum)
			_ => return Err(1)
		}
	}
	Err(1)
}

# Account/header events do not count as transactions. Unicode weights are
# calculated from UTF-8 bytes, with their position reset for each field.
expect parse("!Account\nNCurrent\n^\n!Type:Bank\nD27/08/2024\nT1\nPé\nM😀\n^\n") == Ok(2060)
expect parse("!Type:Bank\nD27/08/2024\nTbad\n^\n") == Err(1)
expect parse("!Type:Bank\nD27/08/2024\nT1\n^\nPunfinished") == Err(1)
