app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.RocSyntax

# tag::parse[]
main! = |_args| {
	parsed = RocSyntax.parse_str("answer = 1 + 2 * 3\n")
	Stdout.line!("status: ${Str.inspect(parsed.status)}")?
	Stdout.line!("diagnostics: ${parsed.diagnostics.len().to_str()}")?
	for token in parsed.tokens {
		if token.kind == Number {
			text = Str.from_utf8(parsed.source.sublist({ start: token.start, len: token.end - token.start })) ?? ""
			Stdout.line!("number ${text} at bytes ${token.start.to_str()}..${token.end.to_str()}")?
		}
	}
	Ok({})
}
# end::parse[]
