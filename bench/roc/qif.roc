app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import parser.QIF
import Bench

## Collect banking transactions and consume matching payee/memo UTF-8 checksums.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

weighted : Str -> U64
weighted = |text| text.to_utf8().iter().with_index().fold(0.U64, |sum, pair| sum + (pair.0 + 1) * pair.1.to_u64())

parse : Str -> Try(U64, U64)
parse = |input| {
	doc = QIF.parse_str(input).map_err(|_| 1)?
	rows = doc.transactions({ date_order: DayFirst }).map_err(|_| 1)?
	Ok(rows.fold(0.U64, |sum, row| sum + 1 + weighted(row.payee) + weighted(row.memo)))
}
