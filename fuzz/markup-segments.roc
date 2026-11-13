app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Markup
import parser.MarkupTemplate
import MarkupCheck

## Explicit holes cannot alter names/delimiters or split a reference. Bound byte
## values remain bytes; inspecting bindings must never interpret their contents.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	kind = (bytes.first() ?? 0) % 8
	segments = match kind {
		0 => ["<g>", "</g>"]
		1 => ["<path d='", "'/>"]
		2 => ["<g id='", "'>", "</g>"]
		3 => ["<pa", "th/>"]
		4 => ["<g xmlns='", "'/>"]
		5 => ["<g>&am", "p;</g>"]
		6 => ["<g><!--", "--></g>"]
		_ => ["<script>", "</script>"]
	}
	valid = kind < 3
	result = MarkupTemplate.parse_segments(segments, |text| Markup.parse_xml_fragment(text, Markup.svg_namespace, {}), Markup.decode_xml_text)
	match result {
		Ok(plan) => {
			if !valid or plan.slots.len() + 1 != segments.len() {
				crash "illegal interpolation structure was accepted"
			}
			MarkupCheck.check(plan.parsed, Str.join_with(segments, ""))
			values = bytes.drop_first(1)
			match MarkupTemplate.bind(plan, values.iter()) {
				Ok(bound) => if values.len() != plan.slots.len() or bound.values != values {
					crash "binding changed typed hole values or arity"
				}
				Err(InvalidMarkup(error)) => {
					if values.len() == plan.slots.len() {
						crash "valid binding arity was rejected"
					}
					match error.problem {
						BindingCount(count) => if count.expected != plan.slots.len() or count.actual != values.len() {
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
