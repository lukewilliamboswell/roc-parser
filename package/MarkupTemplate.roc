import Markup

## Parsed literal plans with explicit, indexed holes. Static segments are kept
## separately from bound values
## No placeholder strings are inserted into input.
## The parser callback establishes syntax, then the format wrapper validates the
## fixed vocabulary/values while exempting exactly the attribute slots below.
MarkupTemplate :: [].{

	## Position and consumer constraint for one hole in the fixed tree.
	Slot : { index : U64, location : Markup.Span, target : [TextSlot({ node : U64 }), AttributeSlot({ node : U64, attribute : U64 })], expected : Markup.ValueGrammar }

	## A template's immutable structure and its original static segments.
	Plan : { segments : List(Str), parsed : Markup.Parsed, slots : List(Slot) }

	## Values retain their original type, including closures and reactive values.
	Bound(a) := { plan : Plan, values : List(a) }.{

		## Look up one bound value without interpreting it as markup.
		binding : Bound(a), U64 -> Try(a, [Missing])
		binding = |bound, index| bound.values.get(index).map_err(|_| Missing)
	}

	## Parse fixed syntax and legal text/whole-attribute holes. Parser and decoder
	## callbacks let HTML and XML use their own normalization rules. Structural
	## recovery is rejected because repaired positions cannot promise fixed slots.
	parse_segments : List(Str), (Str -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])), (Str -> Try(Str, [InvalidMarkup(Markup.Error)])) -> Try(Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, parse, decode| {
		if segments.is_empty() {
			return Err(InvalidMarkup({ problem: Interpolation({ index: 0, reason: "an interpolation requires at least one static segment" }), line: 1, column: 1 }))
		}
		source_text = Str.join_with(segments, "")
		source = Markup.Source.from_str(source_text)
		var $holes = []
		var $offset = 0
		var $index = 0
		for segment in segments.drop_last(1) {
			$offset = $offset + segment.count_utf8_bytes()
			$holes = $holes.append({ index: $index, offset: $offset })
			$index = $index + 1
		}
		# Classify holes before calling the document parser, so a hole in a
		# delimiter/name is rejected even when removing it makes valid syntax.
		positions = classify_holes(source, $holes)?
		parsed = parse(source_text)?
		if !parsed.diagnostics.is_empty() {
			return Err(template_error(source, 0, 0, "template structure requires recovery"))
		}
		build_plan(segments, parsed, positions, decode)
	}

	## Fallibly bind a manually supplied iterator, checking its exact arity.
	bind : Plan, Iter(a) -> Try(Bound(a), [InvalidMarkup(Markup.Error)])
	bind = |plan, values| {
		bound = capture(plan, values)
		if bound.values.len() == plan.slots.len() {
			Ok(bound)
		} else {
			Err(InvalidMarkup({ problem: BindingCount({ expected: plan.slots.len(), actual: bound.values.len() }), line: 1, column: 1 }))
		}
	}

	## Capture values for a compiler-provided interpolation assembler. The
	## compiler supplies one item per hole
	## Manual callers should use bind.
	capture : Plan, Iter(a) -> Bound(a)
	capture = |plan, values| {
		var $values = []
		for value in values {
			$values = $values.append(value)
		}
		Bound.{ plan, values: $values }
	}

	## Format a structural hole error for Roc's interpolation conversion hook.
	interpolation_error : Markup.Error -> [InvalidInterpolation(Str)]
	interpolation_error = |error| InvalidInterpolation("${error.line.to_str()}:${error.column.to_str()}: ${Str.inspect(error.problem)}")
}

Hole : { index : U64, offset : U64 }

Position : { hole : Hole, kind : [Data, Attribute] }

classify_holes : Markup.Source, List(Hole) -> Try(List(Position), [InvalidMarkup(Markup.Error)])
classify_holes = |source, holes| {
	var $positions = []
	var $token_index = 0
	var $raw = ""
	for hole in holes {
		while $token_index < source.tokens.len() {
			token = source.tokens.get($token_index) ?? { kind: Text, span: { start: 0, end: 0 } }
			if token.span.end > hole.offset {
				break
			}
			if token.kind == Tag {
				bytes = source.slice(token.span)
				name = tag_name(bytes)
				if bytes.get(1) == Ok('/') {
					if name == $raw {
						$raw = ""
					}
				} else if ["script", "style", "xmp", "iframe", "noembed", "noframes", "plaintext"].contains(name) {
					$raw = name
				}
			}
			$token_index = $token_index + 1
		}
		if $raw != "" {
			return Err(template_error(source, hole.offset, hole.index, "holes are not allowed in raw script/style content"))
		}
		match source.tokens.get($token_index) {
			Ok(token) if token.span.start < hole.offset => {
				match token.kind {
					Text => {
						if unfinished_reference(source.bytes, token.span.start, hole.offset) {
							return Err(template_error(source, hole.offset, hole.index, "a hole cannot split a character reference"))
						}
						$positions = $positions.append({ hole, kind: Data })
					}
					Tag => {
						if !whole_attribute(source, token.span, hole.offset) {
							return Err(template_error(source, hole.offset, hole.index, "expected character data or an entire quoted attribute value"))
						}
						if $positions.any(|position| position.kind == Attribute and position.hole.offset == hole.offset) {
							return Err(template_error(source, hole.offset, hole.index, "one attribute value accepts exactly one hole"))
						}
						$positions = $positions.append({ hole, kind: Attribute })
					}
					_ => return Err(template_error(source, hole.offset, hole.index, "holes are not allowed in comments, declarations or markup delimiters"))
				}
			}
			Ok({ kind: Invalid, .. }) => return Err(template_error(source, hole.offset, hole.index, "incomplete markup around a hole"))
			_ => {
				$positions = $positions.append({ hole, kind: Data })
			}
		}
	}
	Ok($positions)
}

whole_attribute : Markup.Source, Markup.Span, U64 -> Bool
whole_attribute = |source, span, hole| {
	bytes = source.bytes
	var $pos = span.start + 1
	while $pos < span.end and !space(bytes.get($pos) ?? 0) and !['/', '>'].contains(bytes.get($pos) ?? 0) {
		$pos = $pos + 1
	}
	name = tag_name(source.slice(span))
	while $pos < span.end {
		while $pos < span.end and space(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if ['/', '>'].contains(bytes.get($pos) ?? 0) {
			break
		}
		start = $pos
		while $pos < span.end and !space(bytes.get($pos) ?? 0) and !['=', '/', '>'].contains(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		attr = ascii_lower(Str.from_utf8_lossy(bytes.drop_first(start).take_first($pos - start)))
		while $pos < span.end and space(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if bytes.get($pos) != Ok('=') {
			if $pos == start {
				$pos = $pos + 1
			}
		} else {
			$pos = $pos + 1
			while $pos < span.end and space(bytes.get($pos) ?? 0) {
				$pos = $pos + 1
			}
			quote = bytes.get($pos) ?? 0
			if quote == '"' or quote == '\'' {
				$pos = $pos + 1
				content_start = $pos
				while $pos < span.end and bytes.get($pos) != Ok(quote) {
					$pos = $pos + 1
				}
				if content_start == hole and $pos == hole {
					return attr != "xmlns" and !attr.starts_with("xmlns:") and !(attr == "encoding" and (name == "annotation-xml" or name.ends_with(":annotation-xml")))
				}
				$pos = $pos + 1
			} else {
				while $pos < span.end and !space(bytes.get($pos) ?? 0) and bytes.get($pos) != Ok('>') {
					$pos = $pos + 1
				}
			}
		}
	}
	False
}

build_plan : List(Str), Markup.Parsed, List(Position), (Str -> Try(Str, [InvalidMarkup(Markup.Error)])) -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
build_plan = |segments, parsed, positions, decode| {
	var $nodes = parsed.tree.nodes
	var $slots = []
	var $text_groups = Dict.empty()
	var $insertions = []
	# Attribute targets and source text intervals are explicit, not recognized
	# by marker spelling, so arbitrary authored text can never impersonate slots.
	for position in positions {
		hole = position.hole
		if position.kind == Attribute {
			var $found = False
			var $node_index = 0
			for node in $nodes {
				var $attribute_index = 0
				for attribute in node.attributes {
					if attribute.span.start < hole.offset and attribute.span.end > hole.offset {
						$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: AttributeSlot({ node: $node_index, attribute: $attribute_index }), expected: Language("attribute") })
						$found = True
					}
					$attribute_index = $attribute_index + 1
				}
				$node_index = $node_index + 1
			}
			if !$found {
				return Err(template_error(parsed.source, hole.offset, hole.index, "attribute hole lost during tree construction"))
			}
		} else {
			var $text_node = Err(Missing)
			var $parent = Err(Missing)
			var $parent_span = parsed.source.bytes.len() + 1
			var $node_index = 0
			for node in $nodes {
				match node.origin {
					Authored(span) => {
						match node.kind {
							Text(_) if span.start <= hole.offset and span.end >= hole.offset => {
								$text_node = Ok($node_index)
							}
							Element(_) if span.start < hole.offset and span.end > hole.offset and span.end - span.start < $parent_span => {
								$parent = Ok($node_index)
								$parent_span = span.end - span.start
							}
							_ => {}
						}
					}
					Implied(_) => {}
				}
				$node_index = $node_index + 1
			}
			match $text_node {
				Ok(index) => {
					$text_groups = $text_groups.insert(index, ($text_groups.get(index) ?? []).append(hole))
				}
				Err(_) => {
					$insertions = $insertions.append({ hole, parent: $parent })
				}
			}
		}
	}
	var $replacement = Dict.empty()
	for (index, holes) in $text_groups.to_list() {
		node = $nodes.get(index) ?? empty_node
		span = match node.origin {
			Authored(value) => value
			Implied(at) => { start: at, end: at }
		}
		var $pieces = []
		var $start = span.start
		var $first = True
		for hole in holes {
			if hole.offset > $start {
				value = decode(Str.from_utf8_lossy(parsed.source.bytes.drop_first($start).take_first(hole.offset - $start)))?
				part = { kind: Text(value), attributes: [], children: [], origin: Authored({ start: $start, end: hole.offset }) }
				if $first {
					$nodes = $nodes.set(index, part) ?? []
					$pieces = $pieces.append(index)
					$first = False
				} else {
					$pieces = $pieces.append($nodes.len())
					$nodes = $nodes.append(part)
				}
			}
			slot_node = { kind: Text(""), attributes: [], children: [], origin: Authored({ start: hole.offset, end: hole.offset }) }
			slot_index = if ($first) index else $nodes.len()
			if $first {
				$nodes = $nodes.set(index, slot_node) ?? []
				$first = False
			} else {
				$nodes = $nodes.append(slot_node)
			}
			$pieces = $pieces.append(slot_index)
			$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: TextSlot({ node: slot_index }), expected: Language("text") })
			$start = hole.offset
		}
		if $start < span.end {
			value = decode(Str.from_utf8_lossy(parsed.source.bytes.drop_first($start).take_first(span.end - $start)))?
			$pieces = $pieces.append($nodes.len())
			$nodes = $nodes.append({ kind: Text(value), attributes: [], children: [], origin: Authored({ start: $start, end: span.end }) })
		}
		$replacement = $replacement.insert(index, $pieces)
	}
	var $index = 0
	for node in $nodes {
		var $children = []
		for child in node.children {
			$children = $children.concat($replacement.get(child) ?? [child])
		}
		$nodes = $nodes.set($index, { ..node, children: $children }) ?? []
		$index = $index + 1
	}
	var $roots = []
	for root in parsed.tree.roots {
		$roots = $roots.concat($replacement.get(root) ?? [root])
	}
	for insertion in $insertions {
		hole = insertion.hole
		index = $nodes.len()
		$nodes = $nodes.append({ kind: Text(""), attributes: [], children: [], origin: Authored({ start: hole.offset, end: hole.offset }) })
		$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: TextSlot({ node: index }), expected: Language("text") })
		match insertion.parent {
			Ok(parent) => {
				node = $nodes.get(parent) ?? empty_node
				$nodes = $nodes.set(parent, { ..node, children: insert_in_source_order($nodes, node.children, index, hole.offset) }) ?? []
			}
			Err(_) => {
				$roots = insert_in_source_order($nodes, $roots, index, hole.offset)
			}
		}
	}
	Ok({ segments, parsed: { ..parsed, tree: { nodes: $nodes, roots: $roots } }, slots: $slots.sort_with(|a, b| if (a.index < b.index) Before else if (a.index > b.index) After else Same) })
}

insert_in_source_order : List(Markup.Node), List(U64), U64, U64 -> List(U64)
insert_in_source_order = |nodes, children, index, offset| {
	var $out = []
	var $inserted = False
	for child in children {
		at = match nodes.get(child) {
			Ok({ origin: Authored(span), .. }) => span.start
			Ok({ origin: Implied(pos), .. }) => pos
			Err(_) => 0
		}
		zero_width = match nodes.get(child) {
			Ok({ origin: Authored(span), .. }) => span.start == span.end and span.start == offset
			_ => False
		}
		if !$inserted and at >= offset and !zero_width {
			$out = $out.append(index)
			$inserted = True
		}
		$out = $out.append(child)
	}
	if !$inserted {
		$out = $out.append(index)
	}
	$out
}

unfinished_reference : List(U8), U64, U64 -> Bool
unfinished_reference = |bytes, start, end| {
	var $entity = False
	var $i = start
	while $i < end {
		byte = bytes.get($i) ?? 0
		if byte == '&' {
			$entity = True
		} else if !((byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '#') {
			$entity = False
		}
		$i = $i + 1
	}
	$entity
}

space : U8 -> Bool
space = |byte| [' ', '\t', '\r', '\n', 12].contains(byte)

ascii_lower : Str -> Str
ascii_lower = |text| Str.from_utf8_lossy(text.to_utf8().map(|byte| if (byte >= 'A' and byte <= 'Z') byte + 32 else byte))

tag_name : List(U8) -> Str
tag_name = |bytes| {
	start = if (bytes.get(1) == Ok('/')) 2 else 1
	var $end = start
	while $end < bytes.len() and !space(bytes.get($end) ?? 0) and !['/', '>'].contains(bytes.get($end) ?? 0) {
		$end = $end + 1
	}
	ascii_lower(Str.from_utf8_lossy(bytes.drop_first(start).take_first($end - start)))
}

template_error : Markup.Source, U64, U64, Str -> [InvalidMarkup(Markup.Error)]
template_error = |source, offset, index, reason| {
	at = Markup.position(source, offset)
	InvalidMarkup({ problem: Interpolation({ index, reason }), line: at.line, column: at.column })
}

empty_node : Markup.Node
empty_node = { kind: Text(""), attributes: [], children: [], origin: Implied(0) }

xml_plan : List(Str) -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
xml_plan = |segments| MarkupTemplate.parse_segments(segments, |text| Markup.parse_xml_fragment(text, Markup.svg_namespace, {}), Markup.decode_xml_text)

expect {
	plan = xml_plan(["<g a='", "'>a", "b", "c</g>"])?
	plan.slots.len() == 3 and plan.slots.map(|slot| slot.index) == [0, 1, 2] and plan.slots.first()?.target == AttributeSlot({ node: 0, attribute: 0 }) and MarkupTemplate.bind(plan, [42, 43, 44].iter())?.values == [42, 43, 44]
}
expect {
	plan = xml_plan(["<g>a", "b", "c</g>"])?
	root = plan.parsed.tree.nodes.get(Markup.single_root(plan.parsed.tree)?)?
	root.children.map(|index| plan.parsed.tree.nodes.get(index).map_ok(|node| node.kind)) == [Ok(Text("a")), Ok(Text("")), Ok(Text("b")), Ok(Text("")), Ok(Text("c"))]
}
expect {
	plan = xml_plan(["<g>", "", "</g>"])?
	root = plan.parsed.tree.nodes.get(Markup.single_root(plan.parsed.tree)?)?
	root.children == plan.slots.map(
		|slot| match slot.target {
			TextSlot(target) => target.node
			_ => 999
		},
	)
}
expect xml_plan(["<g a='prefix", "'/>"]).is_err()
expect xml_plan(["<g a=", "/>"]).is_err()
expect xml_plan(["<", "/>"]).is_err()
expect xml_plan(["<g", "/>"]).is_err()
expect xml_plan(["<g ", "='x'/>"]).is_err()
expect xml_plan(["<g xmlns='", "'/>"]).is_err()
expect xml_plan(["<annotation-xml encoding='", "'/>"]).is_err()
expect xml_plan(["<g><!--", "--></g>"]).is_err()
expect xml_plan(["<g>&am", "p;</g>"]).is_err()
expect xml_plan(["<script>", "</script>"]).is_err()
expect xml_plan(["<style>", "</style>"]).is_err()
expect xml_plan(["<g a='", "", "'/>"]).is_err()
expect {
	plan = xml_plan(["<g>__hole_0__&amp;", "&lt;__hole_0__</g>"])?
	Markup.text(plan.parsed.tree, Markup.single_root(plan.parsed.tree)?) == "__hole_0__&<__hole_0__" and plan.slots.len() == 1
}
expect {
	plan = xml_plan(["<g>", "</g>"])?
	MarkupTemplate.bind(plan, [1, 2].iter()).map_ok(|bound| bound.values) == Err(InvalidMarkup({ problem: BindingCount({ expected: 1, actual: 2 }), line: 1, column: 1 }))
}
expect {
	plan = xml_plan(["<g>", "</g>"])?
	MarkupTemplate.bind(plan, [].iter()).map_ok(|bound| bound.values) == Err(InvalidMarkup({ problem: BindingCount({ expected: 1, actual: 0 }), line: 1, column: 1 }))
}
