app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Html
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
	measured = Fuzz.measure!("parse", || Html.parse_fragment(input, { recovery: Browser, allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }))
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

target = Fuzz.target_with!({ name: "html-raw", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(Str.from_utf8(bytes)), examples: [] })
