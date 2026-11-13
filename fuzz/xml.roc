app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Utf8
import parser.Xml

test : Str -> Fuzz.Outcome
test = |input| {
	match Utf8.parse_str(Xml.parser, input) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "xml",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
