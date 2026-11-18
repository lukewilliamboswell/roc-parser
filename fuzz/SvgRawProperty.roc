import parser.Parser
import fuzz.Fuzz
import parser.Svg
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
	measured = Fuzz.measure!("parse", || Svg.parse_fragment(input, { allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }))
	# Fifteen work units per byte cannot exhaust XML parsing, but exercise
	# the compatibility route retained for custom work budgets below sixteen.
	legacy = Svg.parse_fragment(input, { allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False, max_work_per_byte: 15 })
	if measured.value.map_ok(|value| value.parsed) != legacy.map_ok(|value| value.parsed) {
		crash "SVG parser routes disagree on the tree, source, or error"
	}
	match measured.value {
		Ok(value) => MarkupCheck.check(value.parsed, input)
		Err(InvalidMarkup(error)) => {
			if source_position(error.location).line == 0 or source_position(error.location).column == 0 {
				crash "error location is not one-based"
			}
		}
	}
	Fuzz.keep
}

property_target = Fuzz.target_with!({ name: "svg-raw", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(Str.from_utf8(bytes)), examples: [] })

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source coordinates"
}

SvgRawProperty :: [].{
	target = property_target
}
