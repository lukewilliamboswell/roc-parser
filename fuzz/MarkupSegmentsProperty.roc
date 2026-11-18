import parser.Svg
import fuzz.Fuzz
import parser.Markup
import parser.Html
import MarkupCheck

## The generator records the authored tree and byte ranges while emitting
## segments. Expected parents never come from the parser's tree or slot plan.
Authored := [
	Element({ local : Str, attributes : List((Str, Str)), start : U64, end : U64, children : List(Authored) }),
	Text({ value : Str, start : U64, end : U64 }),
	Hole({ index : U64, offset : U64 }),
].{
	is_eq : _
}

Segments : { parts : List(Str), current : Str, offset : U64, holes : List(U64) }

emit : Segments, Str -> Segments
emit = |built, text| { ..built, current: "${built.current}${text}", offset: built.offset + text.count_utf8_bytes() }

emit_hole : Segments -> { built : Segments, node : Authored }
emit_hole = |built| {
	node = Hole({ index: built.holes.len(), offset: built.offset })
	{ built: { ..built, parts: built.parts.append(built.current), current: "", holes: built.holes.append(built.offset) }, node }
}

authored_element : Segments, Str, U64, U8, U64, Bool -> { built : Segments, node : Authored }
authored_element = |initial, tag, depth, shape, count, outer_hole| {
	start = initial.offset
	attributes = if (tag == "a") [("href", "/destination")] else []
	attribute_text = if (tag == "a") " href='/destination'" else ""
	var $built = emit(initial, "<${tag}${attribute_text}>")
	opening_end = $built.offset
	var $children = []
	if shape % 2 == 1 {
		at = $built.offset
		$built = emit($built, "π&amp;")
		$children = $children.append(Text({ value: "π&", start: at, end: $built.offset }))
	}
	if depth > 0 {
		child_tag = ["em", "strong", "b", "i"].get((depth - 1) % 4) ?? "em"
		child = authored_element($built, child_tag, depth - 1, shape, count, outer_hole)
		$built = child.built
		$children = $children.append(child.node)
		if outer_hole {
			hole = emit_hole($built)
			$built = hole.built
			$children = $children.append(hole.node)
		}
	} else {
		for _ in (0.U64).until(count) {
			hole = emit_hole($built)
			$built = hole.built
			$children = $children.append(hole.node)
		}
	}
	if shape >= 2 {
		at = $built.offset
		$built = emit($built, "é")
		$children = $children.append(Text({ value: "é", start: at, end: $built.offset }))
	}
	$built = emit($built, "</${tag}>")
	# Formatting/adoption and form-pointer nodes retain their opening-token
	# origins. Placement must respect the authored tree independently of them.
	end = if (["strong", "b", "em", "i", "a", "code", "s", "small", "u", "form"].contains(tag)) opening_end else $built.offset
	{ built: $built, node: Element({ local: tag, attributes, start, end, children: $children }) }
}

observed : Markup.Tree, U64, Dict(U64, { index : U64, offset : U64 }) -> Authored
observed = |tree, index, holes| {
	node = tree.nodes.get(index) ?? crash "template edge lies outside tree"
	span = match node.origin {
		Authored(value) => value
		_ => crash "generated authored node lost its origin"
	}
	match holes.get(index) {
		Ok(hole) => {
			if node.kind != Text("") or span != { start: hole.offset, end: hole.offset } {
				crash "a text hole changed its kind or zero-width source origin"
			}
			Hole(hole)
		}
		Err(_) => match node.kind {
			Element(name) => {
				if name.namespace != Uri(Markup.html_namespace) {
					crash "generated HTML element changed namespace"
				}
				Element({ local: name.local, attributes: node.attributes.map(|attribute| (attribute.name.local, attribute.value)), start: span.start, end: span.end, children: node.children.map(|child| observed(tree, child, holes)) })
			}
			Text(value) => Text({ value, start: span.start, end: span.end })
			_ => crash "generated template acquired an unexpected node kind"
		}
	}
}

check_authored_holes : List(U8) -> {}
check_authored_holes = |bytes| {
	tag = ["strong", "b", "em", "i", "a", "code", "s", "small", "u", "form", "span", "p"].get((bytes.first() ?? 0).to_u64() % 12) ?? "strong"
	shape = (bytes.get(1) ?? 0) % 4
	depth = (bytes.get(2) ?? 0).to_u64() % 4
	count = (bytes.get(3) ?? 0).to_u64() % 3 + 1
	wrapped = (bytes.get(4) ?? 0) % 2 == 0
	context = if ((bytes.get(5) ?? 0) % 2 == 0) "body" else "section"
	outer_hole = (bytes.get(6) ?? 0) % 2 == 1
	initial : Segments
	initial = { parts: [], current: "", offset: 0, holes: [] }
	before = if (wrapped) emit(initial, "<div>") else initial
	child = authored_element(before, tag, depth, shape, count, outer_hole)
	final = if (wrapped) emit(child.built, "</div>") else child.built
	want = if wrapped {
		Element({ local: "div", attributes: [], start: 0, end: final.offset, children: [child.node] })
	} else child.node
	segments = final.parts.append(final.current)
	source = Str.join_with(segments, "")
	plan = Html.parse_segments(segments, { context_element: context, exact_structure: True, strict_foreign_spelling: True }) ?? crash "independently authored valid HTML template was rejected"
	metadata = plan.metadata()
	MarkupCheck.check(metadata.parsed, source)
	if metadata.slots.len() != final.holes.len() {
		crash "template changed the number of authored text holes"
	}
	var $holes = Dict.empty()
	for slot in metadata.slots {
		offset = final.holes.get(slot.index) ?? crash "template invented a logical slot"
		if slot.location != { start: offset, end: offset } {
			crash "template changed an authored hole's original byte offset"
		}
		match slot.target {
			TextSlot(target) => {
				if $holes.contains(target.node) {
					crash "adjacent holes share one physical node"
				}
				$holes = $holes.insert(target.node, { index: slot.index, offset })
			}
			_ => crash "an authored text hole became an attribute hole"
		}
	}
	actual = metadata.parsed.tree.roots.map(|root| observed(metadata.parsed.tree, root, $holes))
	if actual != [want] {
		crash "HTML holes or origins differ from the independently authored tree: ${Str.inspect({ bytes, actual, want })}"
	}
}

## Explicit holes cannot alter names/delimiters or split a reference. Bound byte
## values remain bytes; inspecting bindings must never interpret their contents.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	check_authored_holes(bytes)
	kind = (bytes.first() ?? 0) % 10
	segments = match kind {
		0 => ["<text>", "</text>"]
		1 => ["<path d='", "'/>"]
		2 => ["<text id='", "'>", "</text>"]
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
		Svg.parse_segments(segments, { allow_unknown_attributes: True })
	}
	match result {
		Ok(plan) => {
			expected = segments.len() - 1
			physical = if (kind == 8) 4 else expected
			if !valid or plan.metadata().slots.len() != physical or Set.from_list(plan.metadata().slots.map(|slot| slot.index)).len() != expected or plan.metadata().slots.any(|slot| slot.index >= expected) {
				crash "illegal interpolation structure was accepted"
			}
			MarkupCheck.check(plan.metadata().parsed, Str.join_with(segments, ""))
			values = bytes.drop_first(1)
			match plan.bind(values.iter()) {
				Ok(bound) => if values.len() != expected or bound.metadata().values != values {
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

property_target = Fuzz.target_with({ name: "markup-segments", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: [Fuzz.example({ name: "authored-formatting-hole", revision: "1", make: || [] })] })

MarkupSegmentsProperty :: [].{
	target = property_target
}
