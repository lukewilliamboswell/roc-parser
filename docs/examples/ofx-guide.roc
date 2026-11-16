app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import cli.Stdout
import parser.OFX
import parser.Streaming

source = "<OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>001</BANKID><ACCTID>000123</ACCTID><ACCTTYPE>CHECKING</ACCTTYPE></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND><STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260103</DTPOSTED><TRNAMT>-12.30</TRNAMT><FITID>00001</FITID><NAME>Café</NAME></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>"

main! : List(OsStr) => Try({}, _)
main! = |_| {
	chunks = source.to_utf8().map(|byte| [byte]).iter()
	for item in OFX.parse_iter(chunks, {}) {
		match item {
			Ok(Value(Transaction(transaction))) => Stdout.line!("${transaction.id}: ${transaction.amount} ${transaction.name}")?
			Ok(Finished(report)) => Stdout.line!("finished: ${report.emitted_items.to_str()} items, ${report.rejected_records.to_str()} rejected")?
			Err(error) => Stdout.line!(Str.inspect(error))?
			_ => {}
		}
	}
	document = OFX.parse_str(source)?
	for syntax in [Xml203, Sgml102] {
		bytes = OFX.to_bytes(document, { syntax, encoding: Windows1252 })?
		Stdout.line!("${Str.inspect(syntax)} roundtrip: ${if (OFX.parse_bytes(bytes, {}) == Ok(document)) "true" else "false"}")?
	}
	Ok({})
}
