app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import cli.Stdin
import cli.Stdout
import cli.OsStr
import parser.OFX
import parser.Streaming

## Canonicalize stdin incrementally; pass sgml to write OFX 1.02 instead of XML.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	syntax = if (args.first().map_ok(OsStr.display) == Ok("sgml")) Sgml102 else Xml203
	parsed = OFX.parse_stream(Stdin.chunks(), {})
	for! bytes in OFX.write_stream(
		Streaming.values_stream(parsed),
		{ syntax: syntax },
		65_536,
	) {
		Stdout.write_bytes!(bytes?)?
	}
	Ok({})
}
