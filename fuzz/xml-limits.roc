app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml

## Xml.parse with lowered limits: the first three bytes choose `max_depth`
## (0-15), `max_elements` (0-63) and `max_attributes` (0-7), and the rest is
## the document.
##
## - A document within every limit parses to parse_str's tree.
## - A document past a limit fails with LimitExceeded for a limit the tree
##   really exceeds, with that limit as `max`, located at a start tag (or, for
##   attributes, inside one) before any well-formedness error parse_str finds.
## - A malformed document fails with parse_str's error unless a limit is
##   reached first.
## - The parse keeps fewer live bytes than a ceiling linear in the input and
##   in `max_elements`, so a hostile document cannot make the tree large.

Shape : { depth : U64, elements : U64, attributes : U64 }

shape_of : Xml.Node -> Shape
shape_of = |root| {
	var $shape = { depth: 0, elements: 0, attributes: 0 }
	var $pending = [{ node: root, depth: 1 }]
	while !$pending.is_empty() {
		item = $pending.last() ?? { node: Text(""), depth: 0 }
		$pending = $pending.drop_last(1)
		match item.node {
			Element(element) => {
				$shape = {
					depth: if (item.depth > $shape.depth) item.depth else $shape.depth,
					elements: $shape.elements + 1,
					attributes: if (element.attributes.len() > $shape.attributes) element.attributes.len() else $shape.attributes,
				}
				for child in element.children {
					$pending = $pending.append({ node: child, depth: item.depth + 1 })
				}
			}

			Text(_) => {}
		}
	}
	$shape
}

## Line-and-column order.
before_or_at : { line : U64, column : U64 }, { line : U64, column : U64 } -> Bool
before_or_at = |a, b| a.line < b.line or (a.line == b.line and a.column <= b.column)

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	props =
		match bytes {
			[d, e, a, ..] => { max_depth: U8.to_u64(d % 16), max_elements: U8.to_u64(e % 64), max_attributes: U8.to_u64(a % 8) }
			_ => return Fuzz.reject
		}
	input =
		match Str.from_utf8(bytes.drop_first(3)) {
			Ok(text) => text
			Err(_) => return Fuzz.reject
		}
	measured = Fuzz.measure!("parse", || Xml.parse(input, { max_depth: props.max_depth, max_elements: props.max_elements, max_attributes: props.max_attributes }))
	ceiling = 4096 + 16 * input.count_utf8_bytes() + 512 * (props.max_elements + 1)
	if measured.stats.peak_live_bytes > ceiling {
		crash "parsing kept ${measured.stats.peak_live_bytes.to_str()} live bytes, over the ceiling ${ceiling.to_str()}"
	}
	bounded = measured.value
	match Xml.parse_str(input) {
		Ok(xml) => {
			shape = shape_of(xml.root)
			within = shape.depth <= props.max_depth and shape.elements <= props.max_elements and shape.attributes <= props.max_attributes
			match bounded {
				Ok(parsed) =>
					if !within or parsed != xml {
						crash "a document past its limits, or a different tree, was returned"
					}

				Err(LimitExceeded(exceeded)) => {
					if within {
						crash "a document within its limits failed: ${Str.inspect(exceeded)}"
					}
					real =
						match exceeded.limit {
							Depth => exceeded.max == props.max_depth and shape.depth > props.max_depth
							Elements => exceeded.max == props.max_elements and shape.elements > props.max_elements
							Attributes => exceeded.max == props.max_attributes and shape.attributes > props.max_attributes
						}
					if !real {
						crash "LimitExceeded names a limit the document does not exceed: ${Str.inspect(exceeded)}"
					}
				}

				Err(InvalidXml(error)) => crash "a well-formed document failed with ${Str.inspect(error)}"
			}
		}

		Err(InvalidXml(error)) =>
			match bounded {
				Ok(_) => crash "a malformed document was accepted with limits"
				Err(InvalidXml(other)) =>
					if other != error {
						crash "the bounded parser reports ${Str.inspect(other)}, parse_str ${Str.inspect(error)}"
					}

				Err(LimitExceeded(exceeded)) =>
					if !before_or_at({ line: exceeded.line, column: exceeded.column }, { line: error.line, column: error.column }) {
						crash "a limit was reported at ${Str.inspect(exceeded)}, after the error ${Str.inspect(error)}"
					}
			}
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	examples: [],
	name: "xml-limits",
	generator: Fuzz.raw_bytes,
	test!,
	show: |bytes| Str.inspect({ limits: bytes.take_first(3), document: Str.from_utf8(bytes.drop_first(3)) }),
})
