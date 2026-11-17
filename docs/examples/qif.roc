app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.Stdout
import parser.QIF
import parser.Streaming

source = "!Type:Bank\nD27/08/2024\nT-12.3400\nPCafé\n^\n"

main! = |_| {
	# Each byte is a separate chunk, including the UTF-8 characters in Café.
	chunks = source.to_utf8().map(|byte| [byte])
	for result in Streaming.read_iter(chunks.iter(), QIF.transaction_decoder({})) {
		match result {
			Ok(Value(Transaction(row))) => Stdout.line!("${row.payee}: ${row.amount}")?
			Ok(Finished(report)) => Stdout.line!("rejected: ${report.rejected_records.to_str()}")?
			Ok(_) => {}
			Err(_) => return Err(InvalidQif)
		}
	}
	Ok({})
}

expect Streaming.collect(Streaming.read_iter([source.to_utf8()].iter(), QIF.decoder({}))).is_ok()
