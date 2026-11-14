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
	parse_segments = |segments, parse, decode| parse_segments_with(segments, {}, parse, decode)

	## Parse a plan with the same source, node, and slot ceilings as runtime parsing.
	parse_segments_with : List(Str), Markup.Props, (Str -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])), (Str -> Try(Str, [InvalidMarkup(Markup.Error)])) -> Try(Plan, [InvalidMarkup(Markup.Error)])
	parse_segments_with = |segments, props, parse, decode| {
		if segments.is_empty() {
			return Err(InvalidMarkup({ problem: Interpolation({ index: 0, reason: "an interpolation requires at least one static segment" }), line: 1, column: 1 }))
		}
		var $source_size = 0
		for segment in segments {
			if segment.count_utf8_bytes() > props.max_source_bytes - $source_size {
				return Err(InvalidMarkup({ problem: SourceTooLarge(props.max_source_bytes), line: 1, column: 1 }))
			}
			$source_size = $source_size + segment.count_utf8_bytes()
		}
		if segments.len() - 1 > props.max_slots {
			return Err(InvalidMarkup({ problem: ResourceLimit("template slots"), line: 1, column: 1 }))
		}
		if $source_size > Markup.work_budget(props, $source_size) or segments.len() > Markup.work_budget(props, $source_size) {
			return Err(InvalidMarkup({ problem: ResourceLimit("template work"), line: 1, column: 1 }))
		}
		source_text = Str.join_with(segments, "")
		var $holes = []
		var $offset = 0
		var $index = 0
		for segment in segments.drop_last(1) {
			$offset = $offset + segment.count_utf8_bytes()
			$holes = $holes.append({ index: $index, offset: $offset })
			$index = $index + 1
		}
		# The actual parser knows raw-text and foreign-content contexts. Its
		# retained regions distinguish JavaScript '<' characters from tag opens.
		# On syntax failure, a generic pass still diagnoses structural holes,
		# including holes whose removal merely happened to produce valid names.
		parsed = match parse(source_text) {
			Ok(value) => value
			Err(error) => {
				_ = classify_holes(Markup.Source.from_str(source_text), $holes, props.syntax)?
				return Err(error)
			}
		}
		positions = classify_holes(parsed.source, $holes, props.syntax)?
		if parsed.tree.nodes.len() > props.max_nodes {
			return Err(InvalidMarkup({ problem: ResourceLimit("nodes"), line: 1, column: 1 }))
		}
		if !parsed.diagnostics.is_empty() {
			return Err(template_error(parsed.source, 0, 0, "template structure requires recovery"))
		}
		build_plan(segments, parsed, positions, props, decode)
	}

	## Fallibly bind a manually supplied iterator, checking its arity.
	## Stop at the first excess value; BindingCount.actual is the observed count.
	bind : Plan, Iter(a) -> Try(Bound(a), [InvalidMarkup(Markup.Error)])
	bind = |plan, values| {
		if plan.segments.is_empty() {
			return Err(InvalidMarkup({ problem: Interpolation({ index: 0, reason: "a template requires at least one static segment" }), line: 1, column: 1 }))
		}
		expected = plan.segments.len() - 1
		var $items = []
		for value in values {
			if $items.len() == expected {
				return Err(InvalidMarkup({ problem: BindingCount({ expected: expected, actual: $items.len() + 1 }), line: 1, column: 1 }))
			}
			$items = $items.append(value)
		}
		if $items.len() != expected {
			return Err(InvalidMarkup({ problem: BindingCount({ expected: expected, actual: $items.len() }), line: 1, column: 1 }))
		}
		Ok(Bound.{ plan, values: $items })
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

	## Apply exact clone provenance after a format-specific tree projection.
	## Projection edges identify newly appended copies in creation order. One
	## logical binding can drive several rendered slots without being re-read.
	fanout : Plan, Markup.Parsed, List({ original : U64, clone : U64 }), Markup.Props -> Try(Plan, [InvalidMarkup(Markup.Error)])
	fanout = |plan, projected, clones, props| {
		if plan.segments.is_empty() {
			return Err(InvalidMarkup({ problem: Interpolation({ index: 0, reason: "a template requires at least one static segment" }), line: 1, column: 1 }))
		}
		if projected.tree.nodes.len() > props.max_nodes {
			return Err(projection_error(ResourceLimit("nodes")))
		}
		if projected.source.bytes != plan.parsed.source.bytes {
			return Err(projection_error(Unsupported("projection changed template source")))
		}
		reachable = reachable_nodes(projected.tree)?
		var $by_node = Dict.empty()
		var $slots = []
		for slot in plan.slots {
			index = slot_node_index(slot)
			if slot.index >= plan.segments.len() - 1 or index >= plan.parsed.tree.nodes.len() {
				return Err(projection_error(Unsupported("invalid original template slot")))
			}
			$by_node = $by_node.insert(index, ($by_node.get(index) ?? []).append(slot))
			if reachable.contains(index) {
				if $slots.len() >= props.max_slots {
					return Err(projection_error(ResourceLimit("template slots")))
				}
				$slots = $slots.append(slot)
			}
		}
		var $seen = Set.empty()
		for edge in clones {
			if edge.original >= edge.clone or edge.clone < plan.parsed.tree.nodes.len() or edge.clone >= projected.tree.nodes.len() or $seen.contains(edge.clone) {
				return Err(projection_error(Unsupported("invalid template clone mapping")))
			}
			$seen = $seen.insert(edge.clone)
			match $by_node.get(edge.original) {
				Ok(base_slots) => {
					# Share descriptors through clone chains. Only reachable rendered
					# references allocate slots, avoiding attribute_count * orphan_copies.
					$by_node = $by_node.insert(edge.clone, base_slots)
					if reachable.contains(edge.clone) {
						for base in base_slots {
							if $slots.len() >= props.max_slots {
								return Err(projection_error(ResourceLimit("template slots")))
							}
							target = match base.target {
								TextSlot(_) => TextSlot({ node: edge.clone })
								AttributeSlot(attribute) => AttributeSlot({ node: edge.clone, attribute: attribute.attribute })
							}
							$slots = $slots.append({ ..base, target })
						}
					}
				}
				Err(_) => {}
			}
		}
		Ok({ ..plan, parsed: projected, slots: $slots.sort_with(|a, b| if (a.index == b.index) order(slot_node_index(a), slot_node_index(b)) else order(a.index, b.index)) })
	}

	## Format a structural hole error for Roc's interpolation conversion hook.
	interpolation_error : Markup.Error -> [InvalidInterpolation(Str)]
	interpolation_error = |error| InvalidInterpolation("${error.line.to_str()}:${error.column.to_str()}: ${Str.inspect(error.problem)}")
}

Hole : { index : U64, offset : U64 }

Position : { hole : Hole, kind : [Data, CData, Attribute] }

classify_holes : Markup.Source, List(Hole), Markup.Syntax -> Try(List(Position), [InvalidMarkup(Markup.Error)])
classify_holes = |source, holes, syntax| {
	var $positions = []
	var $token_index = 0
	var $raw = ""
	var $reference_cursor = 0
	var $reference_open = False
	var $last_attribute = Err(Missing)
	for hole in holes {
		while $token_index < source.tokens.len() {
			token = source.tokens.get($token_index) ?? { kind: Text, span: { start: 0, end: 0 } }
			if token.span.end > hole.offset {
				break
			}
			if token.kind == Tag {
				bytes = source.slice(token.span)
				qualified = tag_name(bytes)
				name = qualified.split_on(":").last() ?? qualified
				if bytes.get(1) == Ok('/') {
					if name == $raw {
						$raw = ""
					}
				} else if bytes.get(bytes.len() - 2) != Ok('/') and ["script", "style", "noscript", "xmp", "iframe", "noembed", "noframes", "plaintext"].contains(name) {
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
						if $reference_cursor < token.span.start {
							$reference_cursor = token.span.start
							$reference_open = False
						}
						while $reference_cursor < hole.offset {
							byte = source.bytes.get($reference_cursor) ?? 0
							if byte == '&' {
								$reference_open = True
							} else if !reference_char(byte) {
								$reference_open = False
							}
							$reference_cursor = $reference_cursor + 1
						}
						if $reference_open {
							return Err(template_error(source, hole.offset, hole.index, "a hole cannot split a character reference"))
						}
						$positions = $positions.append({ hole, kind: Data })
					}
					CData if hole.offset >= token.span.start + 9 and hole.offset <= token.span.end - 3 => {
						$positions = $positions.append({ hole, kind: CData })
					}
					Tag => {
						if !whole_attribute(source, token.span, hole.offset, syntax) {
							return Err(template_error(source, hole.offset, hole.index, "expected character data or an entire quoted attribute value"))
						}
						if $last_attribute == Ok(hole.offset) {
							return Err(template_error(source, hole.offset, hole.index, "one attribute value accepts exactly one hole"))
						}
						$last_attribute = Ok(hole.offset)
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

whole_attribute : Markup.Source, Markup.Span, U64, Markup.Syntax -> Bool
whole_attribute = |source, span, hole, syntax| {
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
		raw_attr = Str.from_utf8_lossy(bytes.drop_first(start).take_first($pos - start))
		attr = if (syntax == HtmlSyntax) ascii_lower(raw_attr) else raw_attr
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

build_plan : List(Str), Markup.Parsed, List(Position), Markup.Props, (Str -> Try(Str, [InvalidMarkup(Markup.Error)])) -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
build_plan = |segments, parsed, positions, props, decode| {
	# Sorting intervals and locating slots is bounded by an explicit structural
	# progress estimate, in addition to the engine's source-scanning budget.
	var $levels = 0
	var $size = parsed.tree.nodes.len() + positions.len() + 1
	while $size > 1 {
		$levels = $levels + 1
		$size = $size // 2
	}
	structural_work = (parsed.tree.nodes.len() + positions.len() + 1) * (4 + $levels * 3)
	if structural_work > Markup.work_budget(props, parsed.source.bytes.len()) {
		return Err(InvalidMarkup({ problem: ResourceLimit("template work"), line: 1, column: 1 }))
	}
	var $nodes = parsed.tree.nodes
	var $slots = []
	var $text_groups = Dict.empty()
	var $insertions = []
	var $texts = []
	var $attributes = []
	var $elements = []
	var $text_namespaces = Dict.empty()
	var $index = 0
	# Source intervals are sorted once. Holes already arrive in authored order,
	# so locating every target is a bounded sweep instead of holes * nodes work.
	for node in $nodes {
		match node.origin {
			Authored(span) => match node.kind {
				Text(_) => {
					$texts = $texts.append({ span, node: $index })
				}
				Element(_) => {
					$elements = $elements.append({ span, node: $index })
				}
				_ => {}
			}
			Implied(_) => {}
			Cloned(_) => {
				if !positions.is_empty() {
					return Err(projection_error(Unsupported("template syntax callback must defer projections")))
				}
			}
		}
		match node.kind {
			Element(name) => {
				for child in node.children {
					$text_namespaces = $text_namespaces.insert(child, name.namespace)
				}
			}
			_ => {}
		}
		var $attr_index = 0
		for attribute in node.attributes {
			$attributes = $attributes.append({ span: attribute.span, node: $index, attribute: $attr_index })
			$attr_index = $attr_index + 1
		}
		$index = $index + 1
	}
	text_intervals = $texts.sort_with(|a, b| order(a.span.start, b.span.start))
	attribute_intervals = $attributes.sort_with(|a, b| order(a.span.start, b.span.start))
	element_intervals = $elements.sort_with(|a, b| if (a.span.start == b.span.start) order(b.span.end, a.span.end) else order(a.span.start, b.span.start))
	var $text_cursor = 0
	var $attribute_cursor = 0
	var $element_cursor = 0
	var $active = []
	for position in positions {
		hole = position.hole
		if position.kind == Attribute {
			while $attribute_cursor < attribute_intervals.len() {
				entry = attribute_intervals.get($attribute_cursor) ?? empty_attribute_interval
				if entry.span.end > hole.offset {
					break
				}
				$attribute_cursor = $attribute_cursor + 1
			}
			match attribute_intervals.get($attribute_cursor) {
				Ok(entry) if entry.span.start < hole.offset and entry.span.end > hole.offset => {
					$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: AttributeSlot({ node: entry.node, attribute: entry.attribute }), expected: Language("attribute") })
				}
				_ => return Err(template_error(parsed.source, hole.offset, hole.index, "attribute hole lost during tree construction"))
			}
		} else {
			while $text_cursor < text_intervals.len() {
				entry = text_intervals.get($text_cursor) ?? empty_interval
				if entry.span.end >= hole.offset {
					break
				}
				$text_cursor = $text_cursor + 1
			}
			match text_intervals.get($text_cursor) {
				Ok(entry) if entry.span.start <= hole.offset and entry.span.end >= hole.offset => {
					$text_groups = $text_groups.insert(entry.node, ($text_groups.get(entry.node) ?? []).append(hole))
				}
				_ => {
					if position.kind == CData {
						return Err(template_error(parsed.source, hole.offset, hole.index, "CDATA hole is not character data in this namespace"))
					}
					while $element_cursor < element_intervals.len() {
						entry = element_intervals.get($element_cursor) ?? empty_interval
						if entry.span.start >= hole.offset {
							break
						}
						while !$active.is_empty() and ($active.last() ?? empty_interval).span.end <= entry.span.start {
							$active = $active.drop_last(1)
						}
						$active = $active.append(entry)
						$element_cursor = $element_cursor + 1
					}
					while !$active.is_empty() and ($active.last() ?? empty_interval).span.end <= hole.offset {
						$active = $active.drop_last(1)
					}
					$insertions = $insertions.append({ hole, parent: $active.last().map_ok(|entry| entry.node).map_err(|_| Missing) })
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
			Cloned(copy) => copy.source
		}
		cdata_markup = props.syntax == XmlSyntax or ($text_namespaces.get(index) ?? NoNamespace) != Uri(Markup.html_namespace)
		content_end = span.end
		var $pieces = []
		var $start = span.start
		var $first = True
		for hole in holes {
			if hole.offset > $start {
				value = decode_source_region(parsed.source, $start, hole.offset, cdata_markup, decode)?
				part = { kind: Text(value), attributes: [], children: [], origin: Authored({ start: $start, end: hole.offset }) }
				if $first {
					$nodes = $nodes.set(index, part) ?? []
					$pieces = $pieces.append(index)
					$first = False
				} else {
					$pieces = $pieces.append($nodes.len())
					$nodes = append_bounded($nodes, part, props.max_nodes)?
				}
			}
			slot_node = { kind: Text(""), attributes: [], children: [], origin: Authored({ start: hole.offset, end: hole.offset }) }
			slot_index = if ($first) index else $nodes.len()
			if $first {
				$nodes = $nodes.set(index, slot_node) ?? []
				$first = False
			} else {
				$nodes = append_bounded($nodes, slot_node, props.max_nodes)?
			}
			$pieces = $pieces.append(slot_index)
			$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: TextSlot({ node: slot_index }), expected: Language("text") })
			$start = hole.offset
		}
		if $start < content_end {
			value = decode_source_region(parsed.source, $start, content_end, cdata_markup, decode)?
			$pieces = $pieces.append($nodes.len())
			$nodes = append_bounded($nodes, { kind: Text(value), attributes: [], children: [], origin: Authored({ start: $start, end: content_end }) }, props.max_nodes)?
		}
		$replacement = $replacement.insert(index, $pieces)
	}
	var $by_parent = Dict.empty()
	var $at_roots = []
	for insertion in $insertions {
		hole = insertion.hole
		index = $nodes.len()
		$nodes = append_bounded($nodes, { kind: Text(""), attributes: [], children: [], origin: Authored({ start: hole.offset, end: hole.offset }) }, props.max_nodes)?
		$slots = $slots.append({ index: hole.index, location: { start: hole.offset, end: hole.offset }, target: TextSlot({ node: index }), expected: Language("text") })
		entry = { offset: hole.offset, node: index }
		match insertion.parent {
			Ok(parent) => {
				$by_parent = $by_parent.insert(parent, ($by_parent.get(parent) ?? []).append(entry))
			}
			Err(_) => {
				$at_roots = $at_roots.append(entry)
			}
		}
	}
	# A while cursor preserves unique ownership while updating the arena;
	# iterating over the same list while modifying it would copy each update.
	$index = 0
	while $index < $nodes.len() {
		node = $nodes.get($index) ?? empty_node
		var $children = []
		for child in node.children {
			$children = $children.concat($replacement.get(child) ?? [child])
		}
		children = merge_insertions($nodes, $children, $by_parent.get($index) ?? [])
		$nodes = $nodes.set($index, { ..node, children }) ?? []
		$index = $index + 1
	}
	var $roots = []
	for root in parsed.tree.roots {
		$roots = $roots.concat($replacement.get(root) ?? [root])
	}
	roots = merge_insertions($nodes, $roots, $at_roots)

	if $nodes.len() > props.max_nodes {
		return Err(InvalidMarkup({ problem: ResourceLimit("nodes"), line: 1, column: 1 }))
	}
	Ok({ segments, parsed: { ..parsed, tree: { nodes: $nodes, roots } }, slots: $slots.sort_with(|a, b| if (a.index < b.index) Before else if (a.index > b.index) After else Same) })
}

Interval : { span : Markup.Span, node : U64 }

empty_interval : Interval
empty_interval = { span: { start: 0, end: 0 }, node: 0 }

empty_attribute_interval : { span : Markup.Span, node : U64, attribute : U64 }
empty_attribute_interval = { span: { start: 0, end: 0 }, node: 0, attribute: 0 }

order : U64, U64 -> [Before, Same, After]
order = |a, b| if (a < b) Before else if (a > b) After else Same

merge_insertions : List(Markup.Node), List(U64), List({ offset : U64, node : U64 }) -> List(U64)
merge_insertions = |nodes, children, insertions| {
	var $out = []
	var $cursor = 0
	for child in children {
		at = match nodes.get(child) {
			Ok({ origin: Authored(span), .. }) => span.start
			Ok({ origin: Implied(pos), .. }) => pos
			Ok({ origin: Cloned(copy), .. }) => copy.source.start
			Err(_) => 0
		}
		while $cursor < insertions.len() {
			entry = insertions.get($cursor) ?? { offset: 0, node: 0 }
			if entry.offset > at {
				break
			}
			$out = $out.append(entry.node)
			$cursor = $cursor + 1
		}
		$out = $out.append(child)
	}
	while $cursor < insertions.len() {
		$out = $out.append((insertions.get($cursor) ?? { offset: 0, node: 0 }).node)
		$cursor = $cursor + 1
	}
	$out
}

reference_char : U8 -> Bool
reference_char = |byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '#'

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

expect {
	segments = ["<g>"].concat(List.repeat("</g><g>", 1023)).concat(["</g>"])
	plan = xml_plan(segments)?
	plan.slots.len() == 1024 and plan.parsed.tree.roots.len() == 1024 and plan.slots.all(
		|slot| match slot.target {
			TextSlot(target) => plan.parsed.tree.nodes.get(target.node).map_ok(|node| node.kind) == Ok(Text(""))
			_ => False
		},
	)
}
expect xml_plan(["<script/>", "<g/>"]).is_ok()

expect MarkupTemplate.parse_segments_with(["<g>", "</g>"], { max_slots: 0 }, |text| Markup.parse_xml_fragment(text, "", {}), Markup.decode_xml_text).is_err()
expect MarkupTemplate.parse_segments_with(["<g>", "</g>"], { max_source_bytes: 6 }, |text| Markup.parse_xml_fragment(text, "", {}), Markup.decode_xml_text).is_err()
expect MarkupTemplate.parse_segments_with(["<g>", "</g>"], { max_nodes: 1 }, |text| Markup.parse_xml_fragment(text, "", {}), Markup.decode_xml_text).is_err()
expect MarkupTemplate.parse_segments_with(["<g>", "</g>"], { max_nodes: 2 }, |text| Markup.parse_xml_fragment(text, "", {}), Markup.decode_xml_text).is_ok()

normalize_lines : Str -> Str
normalize_lines = |text| Str.join_with(Str.join_with(text.split_on("\r\n"), "\n").split_on("\r"), "\n")

expect {
	plan = xml_plan(["<g><![CDATA[a&", "b<]]></g>"])?
	Markup.text(plan.parsed.tree, Markup.single_root(plan.parsed.tree)?) == "a&b<" and plan.slots.len() == 1
}
expect xml_plan(["<s:script xmlns:s='http://www.w3.org/2000/svg'>", "</s:script>"]).is_err()
expect xml_plan(["<g XMLNS='", "'/>"]).is_ok()

# A coalesced text leaf can span ordinary character data and CDATA. Decode
# each lexical region independently so holes never need synthetic marker text
# or partial CDATA delimiters. Binary-search the first overlapping region.
decode_source_region : Markup.Source, U64, U64, Bool, (Str -> Try(Str, [InvalidMarkup(Markup.Error)])) -> Try(Str, [InvalidMarkup(Markup.Error)])
decode_source_region = |source, start, end, cdata_markup, decode| {
	var $low = 0
	var $high = source.tokens.len()
	while $low < $high {
		middle = $low + ($high - $low) // 2
		token = source.tokens.get(middle) ?? { kind: Text, span: { start: 0, end: 0 } }
		if token.span.end <= start {
			$low = middle + 1
		} else {
			$high = middle
		}
	}
	var $out = []
	var $cursor = $low
	while $cursor < source.tokens.len() {
		token = source.tokens.get($cursor) ?? { kind: Text, span: { start: 0, end: 0 } }
		if token.span.start >= end {
			break
		}
		region_start = if (cdata_markup and token.kind == CData) token.span.start + 9 else token.span.start
		region_end = if (cdata_markup and token.kind == CData) token.span.end - 3 else token.span.end
		left = if (start > region_start) start else region_start
		right = if (end < region_end) end else region_end
		if left < right {
			text = Str.from_utf8_lossy(source.bytes.drop_first(left).take_first(right - left))
			decoded = if (cdata_markup and token.kind == CData) normalize_lines(text) else decode(text)?
			$out = $out.concat(decoded.to_utf8())
		}
		$cursor = $cursor + 1
	}
	Ok(Str.from_utf8_lossy($out))
}

expect {
	plan = xml_plan(["<g>a<![CDATA[b&", "<c]]>d</g>"])?
	Markup.text(plan.parsed.tree, Markup.single_root(plan.parsed.tree)?) == "ab&<cd" and plan.slots.len() == 1
}

CheckedTemplate(a) := { bound : MarkupTemplate.Bound(a) }.{
	from_interpolation : List(Str) -> Try((Iter(a) -> CheckedTemplate(a)), [InvalidInterpolation(Str)])
	from_interpolation = |segments| {
		plan = xml_plan(segments).map_err(|InvalidMarkup(error)| MarkupTemplate.interpolation_error(error))?
		Ok(|values| CheckedTemplate.{ bound: MarkupTemplate.capture(plan, values) })
	}
}

expect {
	first = { number: 42.U64 }
	second = { number: 43.U64 }
	literal : CheckedTemplate({ number : U64 })
	literal =
		\\<g x="${first}">${second}</g>
	literal.bound.values == [first, second] and literal.bound.plan.slots.len() == 2
}

expect MarkupTemplate.parse_segments_with(["<g>", "</g>"], { max_work_per_byte: 0 }, |text| Markup.parse_xml_fragment(text, "", {}), Markup.decode_xml_text).is_err()

append_bounded : List(Markup.Node), Markup.Node, U64 -> Try(List(Markup.Node), [InvalidMarkup(Markup.Error)])
append_bounded = |nodes, node, limit| {
	if nodes.len() >= limit {
		Err(InvalidMarkup({ problem: ResourceLimit("nodes"), line: 1, column: 1 }))
	} else {
		Ok(nodes.append(node))
	}
}

# A syntax parser can retain '<' inside raw text as a Text region. Generic
# XML-shaped source splitting cannot provide this context on its own.
raw_text_parser : Str -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
raw_text_parser = |source| {
	# Test callback models HTML's token regions for one fixed script and p.
	# The production callback is HtmlTree's own tokenizer-backed source.
	bytes = source.to_utf8()
	closing = source.split_first("</script>").map_err(|_| InvalidMarkup({ problem: SyntaxError("missing script end"), line: 1, column: 1 }))?
	end_script_text = closing.before.count_utf8_bytes()
	p_start = end_script_text + 9
	p_end = bytes.len()
	Ok({ source: Markup.Source.from_tokens(bytes, [{ kind: Tag, span: { start: 0, end: 8 } }, { kind: Text, span: { start: 8, end: end_script_text } }, { kind: Tag, span: { start: end_script_text, end: p_start } }, { kind: Tag, span: { start: p_start, end: p_start + 3 } }, { kind: Tag, span: { start: p_start + 3, end: p_end } }]), tree: { nodes: [{ kind: Element({ namespace: Uri(Markup.html_namespace), local: "script", prefix: Unprefixed }), attributes: [], children: [1], origin: Authored({ start: 0, end: p_start }) }, { kind: Text("if(a<b)x()"), attributes: [], children: [], origin: Authored({ start: 8, end: end_script_text }) }, { kind: Element({ namespace: Uri(Markup.html_namespace), local: "p", prefix: Unprefixed }), attributes: [], children: [], origin: Authored({ start: p_start, end: p_end }) }], roots: [0, 2] }, diagnostics: [] })
}

expect MarkupTemplate.parse_segments_with(["<script>if(a<b)x()</script><p>", "</p>"], { syntax: HtmlSyntax }, raw_text_parser, |text| Ok(text)).is_ok()
expect MarkupTemplate.parse_segments_with(["<script>if(a<", "b)x()</script><p></p>"], { syntax: HtmlSyntax }, raw_text_parser, |text| Ok(text)).is_err()

slot_node_index : MarkupTemplate.Slot -> U64
slot_node_index = |slot| match slot.target {
	TextSlot(target) => target.node
	AttributeSlot(target) => target.node
}

projection_error : Markup.Problem -> [InvalidMarkup(Markup.Error)]
projection_error = |problem| InvalidMarkup({ problem, line: 1, column: 1 })

reachable_nodes : Markup.Tree -> Try(Set(U64), [InvalidMarkup(Markup.Error)])
reachable_nodes = |tree| {
	var $seen = Set.empty()
	var $pending = tree.roots
	while !$pending.is_empty() {
		index = $pending.last() ?? 0
		$pending = $pending.drop_last(1)
		if $seen.contains(index) {
			return Err(projection_error(Unsupported("projected structure is not a tree")))
		}
		$seen = $seen.insert(index)
		match tree.nodes.get(index) {
			Ok(node) => {
				$pending = $pending.concat(node.children)
			}
			Err(_) => return Err(projection_error(Unsupported("invalid projected node index")))
		}
	}
	Ok($seen)
}

# Independent test projection copies the already-parsed arena with exact IDs.
# Coincident zero-width source ranges deliberately cannot identify the holes.
clone_plan_roots : MarkupTemplate.Plan -> { projected : Markup.Parsed, clones : List({ original : U64, clone : U64 }) }
clone_plan_roots = |plan| {
	count = plan.parsed.tree.nodes.len()
	copies = plan.parsed.tree.nodes.map(
		|node| {
			span = match node.origin {
				Authored(value) => value
				Implied(at) => { start: at, end: at }
				Cloned(copy) => copy.source
			}
			{ ..node, children: node.children.map(|child| count + child), origin: Cloned({ source: span, at: 0 }) }
		},
	)
	var $clones = []
	var $index = 0
	while $index < count {
		$clones = $clones.append({ original: $index, clone: count + $index })
		$index = $index + 1
	}
	{ projected: { ..plan.parsed, tree: { nodes: plan.parsed.tree.nodes.concat(copies), roots: plan.parsed.tree.roots.concat(plan.parsed.tree.roots.map(|root| count + root)) } }, clones: $clones }
}

expect {
	plan = xml_plan(["<g>", "", "</g>"])?
	projection = clone_plan_roots(plan)
	fanned = MarkupTemplate.fanout(plan, projection.projected, projection.clones, {})?
	fanned.slots.map(|slot| (slot.index, slot_node_index(slot))) == [(0, 1), (0, 4), (1, 2), (1, 5)] and MarkupTemplate.bind(fanned, [42, 43].iter())?.values == [42, 43]
}
expect {
	plan = xml_plan(["<g title='", "'>", "</g>"])?
	projection = clone_plan_roots(plan)
	fanned = MarkupTemplate.fanout(plan, projection.projected, projection.clones, {})?
	fanned.slots.map(|slot| (slot.index, slot.target)) == [(0, AttributeSlot({ node: 0, attribute: 0 })), (0, AttributeSlot({ node: 2, attribute: 0 })), (1, TextSlot({ node: 1 })), (1, TextSlot({ node: 3 }))]
}
expect {
	plan = xml_plan(["<g>", "</g>"])?
	projection = clone_plan_roots(plan)
	fanned = MarkupTemplate.fanout(plan, projection.projected, projection.clones, {})?
	MarkupTemplate.bind(fanned, [42, 43].iter()).map_ok(|bound| bound.values) == Err(InvalidMarkup({ problem: BindingCount({ expected: 1, actual: 2 }), line: 1, column: 1 }))
}
expect {
	plan = xml_plan(["<g title='", "'>", "</g>"])?
	projection = clone_plan_roots(plan)
	MarkupTemplate.fanout(plan, projection.projected, projection.clones, { max_slots: 3 }).is_err() and MarkupTemplate.fanout(plan, projection.projected, projection.clones, { max_slots: 4 }).is_ok()
}
expect {
	plan = xml_plan(["<g/>"])?
	projection = clone_plan_roots(plan)
	fanned = MarkupTemplate.fanout(plan, projection.projected, projection.clones, { max_slots: 0 })?
	fanned.slots.is_empty() and MarkupTemplate.bind(fanned, [].iter()).is_ok()
}
expect {
	plan = xml_plan(["<g>", "</g>"])?
	projection = clone_plan_roots(plan)
	projected = { ..projection.projected, tree: { ..projection.projected.tree, roots: [2] } }
	fanned = MarkupTemplate.fanout(plan, projected, projection.clones, { max_slots: 1 })?
	fanned.slots.map(slot_node_index) == [3]
}
expect {
	plan : MarkupTemplate.Plan
	plan = { segments: [], parsed: { source: Markup.Source.from_str(""), tree: { nodes: [], roots: [] }, diagnostics: [] }, slots: [] }
	MarkupTemplate.bind(plan, [].iter()).is_err() and MarkupTemplate.fanout(plan, plan.parsed, [], { max_slots: 0 }).is_err()
}
