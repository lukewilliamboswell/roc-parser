app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}

import cli.Stdin
import cli.Stdout
import parser.JsonValue
import parser.Parser

## Read NDJSON from stdin and write one compact JSON array to stdout.
## Retain only one record; success requires the parser's Finished event.
main! = |_| {
	for! chunk in (JsonValue.record_encoder({ framing: Array, chunk_bytes: 65_536 })).write_stream((JsonValue.record_decoder({})).values_stream(Stdin.chunks())) {
		Stdout.write_bytes!(chunk?)?
	}
	Ok({})
}
