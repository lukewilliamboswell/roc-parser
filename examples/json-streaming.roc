app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}

import cli.Stdin
import cli.Stdout
import parser.JsonStream
import parser.Streaming

## Read NDJSON from stdin and write one compact JSON array to stdout.
## Retain only one record; success requires the parser's Finished event.
main! = |_| {
	parsed = Streaming.read_stream(Stdin.chunks(), JsonStream.decoder({}))
	for! chunk in Streaming.write_stream(Streaming.values_stream(parsed), JsonStream.encoder({ format: Array }).with_chunk_bytes(65_536)) {
		Stdout.write_bytes!(chunk?)?
	}
	Ok({})
}
