app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Yaml

# tag::source[]
workflow = "# CI\r\non: [push]\r\nname: 'Check café'\r\nurl: https://example.com/manual#setup\r\ncontinue-on-error: false\r\nrun: |-\r\n  # Shell text, not a YAML comment\r\n  echo ok\r\n"

# Each piece retains bytes from the original source, including whitespace.
# Both an HTML writer and a PDF writer can consume these same pieces.
source_pieces = |tokenized| tokenized.tokens.map(
	|token| {
		category: Yaml.highlight_category(token),
		bytes: tokenized.source.sublist({ start: token.start, len: token.end - token.start }),
	},
)

copy_source = |tokenized| source_pieces(tokenized).fold([], |bytes, piece| bytes.concat(piece.bytes))

# end::source[]

# tag::recovery[]
# Tokenization succeeds as a source operation even when decoding cannot.
broken = Yaml.tokenize_str("name: \"unfinished")

invalid_utf8 = Yaml.tokenize_bytes([0xFF, 0x0D, 0x0A])

expect copy_source(broken) == broken.source
expect !broken.diagnostics.is_empty()
expect copy_source(invalid_utf8) == [0xFF, 0x0D, 0x0A]
expect !invalid_utf8.diagnostics.is_empty()
# end::recovery[]

expect copy_source(Yaml.tokenize_str(workflow)) == workflow.to_utf8()

main! = |_args| {
	# tag::source-run[]
	tokenized = Yaml.tokenize_str(workflow)
	Stdout.line!("preserved: ${Str.inspect(copy_source(tokenized) == workflow.to_utf8())}")?
	for token in tokenized.tokens {
		bytes = tokenized.source.sublist({ start: token.start, len: token.end - token.start })
		if bytes == "on".to_utf8() or bytes == "false".to_utf8() or bytes == "https://example.com/manual#setup".to_utf8() {
			Stdout.line!("${Str.from_utf8(bytes) ?? ""}: ${Str.inspect(Yaml.highlight_category(token))}")?
		}
	}
	# end::source-run[]
	Ok({})
}
