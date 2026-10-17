app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../package/main.roc",
}

import cli.Stdout
import parser.RocSyntax

main! = |_args| {
	source = "# Example Roc source\nanswer = 1 + 2 * 3\n"
	parsed = RocSyntax.parse_str(source)
	Stdout.line!("parse: ${Str.inspect(parsed.status)}, ${parsed.nodes.len().to_str()} syntax nodes")?
	lexed = RocSyntax.tokenize_str(source)
	for token in lexed.tokens {
		category = RocSyntax.highlight_category(token.kind)
		if category != Trivia {
			text = Str.from_utf8(lexed.source.sublist({ start: token.start, len: token.end - token.start })) ?? ""
			Stdout.line!("${Str.inspect(category)} ${token.start.to_str()}..${token.end.to_str()}: ${text}")?
		}
	}
	Ok({})
}
