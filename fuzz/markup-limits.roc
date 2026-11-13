app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Markup
import MarkupCheck

## Shape and limits come from independent byte selectors. A generated document
## succeeds exactly when source, nesting, element and attribute limits permit it.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	depth = U8.to_u64(bytes.first() ?? 0) % 16 + 1
	max_depth = U8.to_u64(bytes.get(1) ?? 0) % 20
	max_elements = U8.to_u64(bytes.get(2) ?? 0) % 20
	max_attributes = U8.to_u64(bytes.get(3) ?? 0) % 4
	input = Str.concat(Str.repeat("<g id='x'>", depth), Str.repeat("</g>", depth))
	max_source_bytes = if ((bytes.get(4) ?? 0) % 2 == 0) input.count_utf8_bytes() else input.count_utf8_bytes() - 1
	within = depth <= max_depth and depth <= max_elements and max_attributes >= 1 and input.count_utf8_bytes() <= max_source_bytes
	measured = Fuzz.measure!("parse", || Markup.parse_xml(input, { max_depth, max_elements, max_attributes, max_source_bytes }))
	legacy = Markup.parse_xml(input, { max_depth, max_elements, max_attributes, max_source_bytes, max_work_per_byte: 15 })
	if measured.value != legacy {
		crash "XML parser routes disagree on the configured resource limits"
	}
	match measured.value {
		Ok(value) => {
			if !within {
				crash "accepted markup beyond its configured limit"
			}
			MarkupCheck.check(value, input)
		}
		Err(InvalidMarkup(error)) => {
			if within {
				crash "a generated document inside all limits was rejected: ${Str.inspect(error)}"
			}
			match error.problem {
				LimitExceeded(_) | SourceTooLarge(_) => {}
				_ => crash "a resource rejection was classified as syntax"
			}
		}
	}
	ceiling = 8192 + 64 * input.count_utf8_bytes() + 2048 * depth
	if measured.stats.peak_live_bytes > ceiling {
		crash "markup allocation ceiling exceeded"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "markup-limits", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(bytes), examples: [] })
