app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import parser.Streaming
import cli.OsStr
import parser.OFX
import Bench

## Parse-only consumption: Bench preloads input; 64 KiB chunks are decoded
## through the public streaming API and each transaction is discarded.
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
	for event in Streaming.read_iter(chunks, OFX.decoder({ encoding: Utf8 })) {
		match event {
			Ok(Value(Transaction(row))) => {
				$sum = $sum + 1 + Bench.weighted_utf8(row.name) + Bench.weighted_utf8(row.memo)
			}
			Ok(Value(_)) => {}
			Ok(Finished(_)) => return Ok($sum)
			_ => return Err(1)
		}
	}
	Err(1)
}

sample : Str
sample = "<OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>012</BANKID><ACCTID>000123</ACCTID><ACCTTYPE>CHECKING</ACCTTYPE></BANKACCTFROM><BANKTRANLIST><STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260103</DTPOSTED><TRNAMT>-12.30</TRNAMT><FITID>00001</FITID><NAME>é</NAME><MEMO>😀</MEMO></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>"

expect parse(sample) == Ok(2060)
expect parse(sample.replace_each("-12.30", "bad")) == Err(1)
expect parse(sample.replace_each("</OFX>", "")) == Err(1)
expect parse("<?xml version=\"1.0\" encoding=\"windows-1252\"?>${sample}") == Err(1)
