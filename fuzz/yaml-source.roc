app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Yaml

## Every byte, including invalid UTF-8 and partial edits, remains displayable.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	lexed = Yaml.tokenize_bytes(bytes)
	if lexed.source != bytes {
		crash "YAML source bytes changed"
	}
	var $cursor = 0
	for token in lexed.tokens {
		if token.start != $cursor or token.end <= token.start or token.end > bytes.len() {
			crash "YAML token partition is invalid"
		}
		$cursor = token.end
		# Exercise presentation independently of successful semantic parsing.
		category = Yaml.highlight_category(token)
		if token.role == Key and category != PropertyName {
			crash "YAML key category changed"
		}
	}
	if $cursor != bytes.len() {
		crash "YAML tokens omitted source bytes"
	}
	for diagnostic in lexed.diagnostics {
		if diagnostic.start > diagnostic.end or diagnostic.end > bytes.len() {
			crash "YAML diagnostic span is invalid"
		}
	}
	if (lexed.status == Complete) != lexed.diagnostics.is_empty() {
		crash "YAML status does not match diagnostics"
	}
	if Yaml.tokenize_bytes(bytes) != lexed {
		crash "YAML tokenization is not deterministic"
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "yaml-source", generator: Fuzz.raw_bytes, test, show: Str.inspect })
