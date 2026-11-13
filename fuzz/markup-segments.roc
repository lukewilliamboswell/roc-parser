app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Markup
import parser.MarkupTemplate
import parser.Html
import MarkupCheck

## Explicit holes cannot alter names/delimiters or split a reference. Bound byte
## values remain bytes; inspecting bindings must never interpret their contents.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	kind = (bytes.first() ?? 0) % 10
	segments = match kind {
		0 => ["<g>", "</g>"]
		1 => ["<path d='", "'/>"]
		2 => ["<g id='", "'>", "</g>"]
		3 => ["<pa", "th/>"]
		4 => ["<g xmlns='", "'/>"]
		5 => ["<g>&am", "p;</g>"]
		6 => ["<g><!--", "--></g>"]
		7 => ["<script>", "</script>"]
		8 => ["<select><button><selectedcontent></selectedcontent></button><option selected><span title='", "'>", "</span></option></select>"]
		_ => ["<script>if(a<b)x()</script><p>", "</p>"]
	}
	valid = kind < 3 or kind >= 8
	result = if kind >= 8 {
		Html.parse_segments(segments, {})
	} else {
		MarkupTemplate.parse_segments(segments, |text| Markup.parse_xml_fragment(text, Markup.svg_namespace, {}), Markup.decode_xml_text)
	}
	match result {
		Ok(plan) => {
			expected = segments.len() - 1
			physical = if (kind == 8) 4 else expected
			if !valid or plan.slots.len() != physical or Set.from_list(plan.slots.map(|slot| slot.index)).len() != expected or plan.slots.any(|slot| slot.index >= expected) {
				crash "illegal interpolation structure was accepted"
			}
			MarkupCheck.check(plan.parsed, Str.join_with(segments, ""))
			values = bytes.drop_first(1)
			match MarkupTemplate.bind(plan, values.iter()) {
				Ok(bound) => if values.len() != expected or bound.values != values {
					crash "binding changed typed hole values or arity"
				}
				Err(InvalidMarkup(error)) => {
					if values.len() == expected {
						crash "valid binding arity was rejected"
					}
					match error.problem {
						BindingCount(count) => if count.expected != expected or count.actual != (if (values.len() > expected) expected + 1 else values.len()) {
							crash "incorrect arity diagnostic"
						}
						_ => crash "binding arity has wrong diagnostic type"
					}
				}
			}
		}
		Err(_) => if valid {
			crash "valid interpolation structure was rejected"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "markup-segments", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: [] })
