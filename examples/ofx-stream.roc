app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import cli.Stdin
import cli.Stdout
import cli.OsStr
import parser.OFX
import parser.Parser

## Canonicalize stdin incrementally; pass sgml to write OFX 1.02 instead of XML.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	syntax = if (args.first().map_ok(OsStr.display) == Ok("sgml")) Sgml102 else Xml203

	for! bytes in (OFX.encoder({ syntax: syntax, chunk_bytes: 65_536 })).write_stream((OFX.decoder({})).values_stream(Stdin.chunks())) {
		Stdout.write_bytes!(bytes?)?
	}
	Ok({})
}
