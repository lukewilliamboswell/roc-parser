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
	# Authored spelling is independent of recovery and leaves canonical DOMs
	# untouched. Keep these checks outside the default allocation observation.
	options : Html.Props
	options = { recovery: Browser, allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }
	ordinary = Html.parse_source(input, options, True)
	strict = Html.parse_source(input, { ..options, strict_foreign_spelling: True }, True)
	match strict {
		Ok(parsed) => if ordinary != Ok(parsed) {
			crash "canonical foreign spelling changed the parsed source/tree"
		}
		Err(InvalidMarkup(error)) => match error.problem {
			NonCanonicalName(detail) => {
				if ordinary.is_err() or detail.span.start >= detail.span.end or detail.span.end > bytes.len() {
					crash "foreign spelling displaced syntax/limits or has an invalid name span"
				}
				authored = bytes.sublist({ start: detail.span.start, len: detail.span.end - detail.span.start })
				if authored != detail.authored.to_utf8() or detail.authored == detail.expected {
					crash "foreign spelling does not witness the exact authored name bytes"
				}
			}
			_ => if strict != ordinary {
				crash "foreign spelling changed syntax/resource rejection precedence"
			}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "html-raw", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(Str.from_utf8(bytes)), examples: [] })
