app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.MathML
import MarkupCheck

## Arbitrary UTF-8 markup must terminate, retain its exact source, and return
## valid tree edges and bounded source locations. Named allocation observation
## supplies local performance evidence without accepting a baseline in CI.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	input = match Str.from_utf8(bytes) {
		Ok(text) => text
		Err(_) => return Fuzz.reject
	}
	measured = Fuzz.measure!("parse", || MathML.parse_fragment(input, { allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }))
	legacy = MathML.parse_fragment(input, { allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False, max_work_per_byte: 15 })
	if measured.value.map_ok(|value| value.parsed) != legacy.map_ok(|value| value.parsed) {
		crash "MathML parser routes disagree on the tree, source, or error"
	}
	match measured.value {
		Ok(value) => MarkupCheck.check(value.parsed, input)
		Err(InvalidMarkup(error)) => {
			if error.line == 0 or error.column == 0 {
				crash "error location is not one-based"
			}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "mathml-raw", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(Str.from_utf8(bytes)), examples: [] })
