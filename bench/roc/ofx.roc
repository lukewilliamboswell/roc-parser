app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import parser.OFX
import Bench

## Collect complete statements and consume every transaction's name and memo.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |source| match OFX.parse_str(source, {}) {
	Ok(document) => Ok(document.statements.fold(0, |sum, statement| statement.transactions.fold(sum, |total, transaction| total + 1 + Bench.weighted_utf8(transaction.name) + Bench.weighted_utf8(transaction.memo))))
	Err(_) => Err(1)
}
