app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markup
import MarkupCheck

## Includes arbitrary malformed UTF-8; the original source is never normalized.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	source = Markup.Source.from_bytes(bytes)
	if source.bytes != bytes {
		crash "lossless source changed arbitrary bytes"
	}
	MarkupCheck.check_source(source)
	reconstructed = source.tokens.fold([], |out, token| out.concat(source.slice(token.span)))
	if reconstructed != bytes {
		crash "lossless regions reconstruct different bytes"
	}
	match Str.from_utf8(bytes) {
		Ok(input) => {
			if Markup.parse_xml(input, {}) != Markup.parse_xml(input, { max_work_per_byte: 15 }) {
				crash "XML document parser routes disagree on the tree, source, or error"
			}
		}
		Err(_) => {}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "markup-source", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: [] })
