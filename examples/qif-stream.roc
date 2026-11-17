app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import cli.Stdin
import cli.Stdout
import parser.QIF
import parser.Streaming

## Canonicalize QIF from stdin to stdout without retaining the document.
## Full success requires consuming the parser's Finished event.

main! = |_| {
	parsed = Streaming.read_stream(Stdin.chunks(), QIF.decoder({}))
	for! chunk in Streaming.write_stream(Streaming.values_stream(parsed), QIF.encoder({})) {
		Stdout.write_bytes!(chunk?)?
	}
	Ok({})
}
