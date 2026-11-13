app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Svg
import parser.MathML
import parser.Html
import parser.Markup
import MarkupCheck

## Byte choices build both a serialization and its expected tree directly.
## The expected structure never calls a production parser or renderer.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	format = (bytes.first() ?? 0) % 3
	count = U8.to_u64(bytes.get(1) ?? 0) % 32
	text = if ((bytes.get(2) ?? 0) % 2 == 0) "<&é" else "😀"
	escaped = if (text == "<&é") "&lt;&amp;é" else "😀"
	namespace = if (format == 0) Markup.svg_namespace else if (format == 1) Markup.mathml_namespace else Markup.html_namespace
	root_name = if (format == 0) "g" else if (format == 1) "mrow" else "div"
	child_name = if (format == 0) "text" else if (format == 1) "mi" else "span"
	extended = format < 2 and (bytes.get(3) ?? 0) % 2 == 1
	binding = if (extended) " xmlns:x='urn:extension'" else ""
	attribute = if (extended) " x:xmlns='v'" else ""
	var $source = "<${root_name}${binding}>"
	var $expected = []
	var $index = 0
	while $index < count {
		value = $index.to_str()
		$source = "${$source}<${child_name} id='x${value}'${attribute}>${escaped}</${child_name}>"
		attributes = if (extended) [("", "id", "x${value}"), ("urn:extension", "xmlns", "v")] else [("", "id", "x${value}")]
		$expected = $expected.append(Element({ namespace, local: child_name, attributes, children: [Text(text)] }))
		$index = $index + 1
	}
	$source = "${$source}</${root_name}>"
	parsed = if format == 0 {
		Svg.parse_fragment($source, { allow_unknown_attributes: extended }).map_ok(|value| value.parsed)
	} else if format == 1 {
		MathML.parse_fragment($source, { allow_unknown_attributes: extended }).map_ok(|value| value.parsed)
	} else {
		Html.parse_fragment($source, {}).map_ok(|value| value.parsed)
	}
	match parsed {
		Err(error) => crash "generated valid markup was rejected: ${Str.inspect(error)}"
		Ok(value) => {
			MarkupCheck.check(value, $source)
			models = value.tree.roots.map(|index| MarkupCheck.model(value.tree, index))
			expected : List(MarkupCheck.Model)
			expected = [Element({ namespace, local: root_name, attributes: [], children: $expected })]
			if models != expected {
				crash "generated markup differs from its independent model"
			}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "markup-structured", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: [] })
