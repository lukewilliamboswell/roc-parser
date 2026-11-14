app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Markup
import parser.MarkupTemplate
import parser.Html
import parser.Svg
import parser.MathML

## Local diagnostic only, never a scheduled/CI campaign. A one-byte operation
## prefix frames the input; 'p'/'b' use NUL-separated static segments as transport.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	operation = bytes.first() ?? 's'
	input = Str.from_utf8(bytes.drop_first(1)) ?? ""
	if operation == 'o' {
		_measured = Fuzz.measure!("parse", || Markup.Source.from_str(input))
	} else if operation == 'p' {
		segments = input.split_on("\u(0)")
		_measured = Fuzz.measure!("parse", || Svg.parse_segments(segments, {}))
	} else if operation == 'b' {
		segments = input.split_on("\u(0)")
		match Svg.parse_segments(segments, {}) {
			Ok(plan) => {
				values = List.repeat("<path/>", plan.slots.len())
				_measured = Fuzz.measure!("parse", || MarkupTemplate.bind(plan, values.iter()))
			}
			Err(_) => return Fuzz.reject
		}
	} else if operation == 'h' {
		_measured = Fuzz.measure!("parse", || Html.parse_fragment(input, {}))
	} else if operation == 'r' {
		_measured = Fuzz.measure!("parse", || Html.parse_fragment(input, { recovery: Browser, allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }))
	} else if operation == 'm' {
		_measured = Fuzz.measure!("parse", || MathML.parse_fragment(input, {}))
	} else {
		_measured = Fuzz.measure!("parse", || Svg.parse_fragment(input, {}))
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "markup-alloc-diagnostic", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(bytes), examples: [] })
