import ForeignNameRules
import Diagnostic
import MarkupCore
import MarkupTemplate
import MarkupValues
import SvgPath
import MarkupVocabulary

## SVG and Presentation MathML authoring checks shared by the three frontends.
## These checks do not execute or validate embedded CSS and JavaScript.
MarkupValidate :: [].{

	## Whole attribute-value holes whose future values are not yet available.
	Hole : { node : U64, attribute : U64 }

	## Validate foreign vocabularies throughout a mixed-namespace tree.
	foreign : MarkupCore.Parsed, MarkupCore.Props, List(Hole) -> Try(MarkupCore.Parsed, [InvalidMarkup(MarkupCore.Error)])
	foreign = |parsed, props, holes| foreign_with_html(parsed, props, holes).map_ok(|checked| checked.parsed)

	## Validate foreign vocabularies and report whether HTML checks are needed.
	foreign_with_html : MarkupCore.Parsed, MarkupCore.Props, List(Hole) -> Try({ parsed : MarkupCore.Parsed, has_html : Bool }, [InvalidMarkup(MarkupCore.Error)])
	foreign_with_html = |parsed, props, holes| {
		hole_positions = holes.fold(Dict.empty(), |dict, hole| dict.insert((hole.node, hole.attribute), True))
		var $diagnostics = parsed.diagnostics
		var $has_html = False
		var $pending = parsed.tree.roots.map(|index| { index, extension: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, extension: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(InvalidTreeIndex), location: LineColumn({ line: 1, column: 1 }), path: [] }))
			match node.kind {
				Element(name) => {
					$has_html = $has_html or name.namespace == Uri(MarkupCore.html_namespace)
					svg = name.namespace == Uri(MarkupCore.svg_namespace)
					mathml = name.namespace == Uri(MarkupCore.mathml_namespace)
					if !item.extension {
						known = if (svg) svg_elements(name.local) else if (mathml) math_elements(name.local) else True
						if !known or (!svg and !mathml and name.namespace != Uri(MarkupCore.html_namespace)) {
							if !props.allow_unknown_elements {
								return fail(parsed, node, UnknownElement(name.local.release_excess_capacity()))
							}
							if $diagnostics.len() >= props.max_diagnostics {
								return fail(parsed, node, LimitExceeded({ resource: Diagnostics, max: props.max_diagnostics, observed: AtLeast(props.max_diagnostics + 1) }))
							}
							offset = match node.origin {
								Authored(span) => span.start
								Cloned(copy) => copy.source.start
								Implied(at) => at
							}
							at = MarkupCore.position(parsed.source, offset)
							$diagnostics = $diagnostics.append(Diagnostic.{ problem: UnknownElement(name.local.release_excess_capacity()), location: LineColumn({ line: at.line, column: at.column }), path: [] })
						}
						var $attr_index = 0
						for attr in node.attributes {
							if svg or mathml {
								allowed = allowed_attribute(svg, name.local, attr.name)
								if !allowed {
									problem = UnknownAttribute({ element: name.local.release_excess_capacity(), attribute: attr.name.local.release_excess_capacity() })
									if !props.allow_unknown_attributes {
										return attr_fail(parsed, attr, problem)
									}
									if $diagnostics.len() >= props.max_diagnostics {
										return fail(parsed, node, LimitExceeded({ resource: Diagnostics, max: props.max_diagnostics, observed: AtLeast(props.max_diagnostics + 1) }))
									}
									at = MarkupCore.position(parsed.source, attr.span.start)
									$diagnostics = $diagnostics.append(Diagnostic.{ problem: problem, location: LineColumn({ line: at.line, column: at.column }), path: [] })
								}
								if props.check_values and allowed and attr.name.namespace == NoNamespace and !hole_positions.contains((item.index, $attr_index)) {
									expected_grammar = value_grammar(svg, name.local, attr.name.local)
									if !valid_value(attr.value, expected_grammar) and !(svg and (svg_presentation(attr.name.local) or member(["x", "y", "cx", "cy", "r", "rx", "ry", "width", "height"], attr.name.local)) and ["inherit", "initial", "unset", "revert", "revert-layer"].contains(ascii_lower(attr.value))) {
										return attr_fail(parsed, attr, InvalidValue({ attribute: attr.name.local, value: Diagnostic.excerpt(attr.value.to_utf8()), expected: expected_grammar }))
									}
								}
							}
							$attr_index = $attr_index + 1
						}
						if svg and known {
							check_svg_children(parsed, node, name.local)?
						}
						if mathml and known {
							check_math_children(parsed, node, name.local)?
						}
					}
					extension = item.extension or (svg and name.local == "metadata") or (mathml and name.local == "annotation-xml")
					for index in node.children {
						$pending = $pending.append({ index, extension })
					}
				}
				_ => {}
			}
		}
		Ok({ parsed: { ..parsed, diagnostics: $diagnostics }, has_html: $has_html })
	}

	## Reject text holes where a format permits only element/whitespace content.
	text_slots : MarkupTemplate.Plan -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
	text_slots = |plan| {
		var $parents = Dict.empty()
		var $index = 0
		for node in plan.metadata().parsed.tree.nodes {
			for child in node.children {
				$parents = $parents.insert(child, $index)
			}
			$index = $index + 1
		}
		for slot in plan.metadata().slots {
			match slot.target {
				TextSlot(target) => {
					match $parents.get(target.node) {
						Ok(parent) => {
							node = plan.metadata().parsed.tree.nodes.get(parent) ?? return Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(InvalidSlotParent), location: LineColumn({ line: 1, column: 1 }), path: [] }))
							match node.kind {
								Element(name) => {
									copied = match (plan.metadata().parsed.tree.nodes.get(target.node) ?? node).origin {
										Cloned(_) => True
										_ => False
									}
									html_nothing = member(["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr", "selectedcontent"], name.local) or (name.local == "option" and node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == "label") and node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == "value"))
									allowed = copied or if (name.namespace == Uri(MarkupCore.svg_namespace)) !svg_elements(name.local) or member(["text", "tspan", "textPath", "title", "desc", "metadata", "foreignObject", "script", "style"], name.local) else if (name.namespace == Uri(MarkupCore.mathml_namespace)) !math_elements(name.local) or member(["mi", "mn", "mo", "ms", "mtext", "annotation", "annotation-xml"], name.local) else if (name.namespace == Uri(MarkupCore.html_namespace)) !html_nothing and !member(["selectedcontent", "html", "head", "table", "thead", "tbody", "tfoot", "tr", "colgroup", "ul", "ol", "menu", "dl", "select", "optgroup", "picture"], name.local) else True
									if !allowed {
										at = MarkupCore.position(plan.metadata().parsed.source, slot.location.start)
										return Err(InvalidMarkup(Diagnostic.{ problem: Interpolation({ index: slot.index, reason: TextHoleForbidden(name.local) }), location: LineColumn({ line: at.line, column: at.column }), path: [] }))
									}
								}
								_ => {}
							}
						}
						Err(_) => {}
					}
				}
				_ => {}
			}
		}
		Ok(plan)
	}

	# Internal single-child predicates; complete-parent cardinality stays below.
	child_placement : Bool, Str, Str -> Bool
	child_placement = |svg, parent, child| if (svg) svg_child_allowed((svg_child_model(parent)).0, child) else if (parent == "mtable") member(["mtr", "mlabeledtr"], child) else if (member(["mtr", "mlabeledtr"], parent)) child == "mtd" else !math_leaf(parent)
	text_placement : Bool, Str, Str -> Bool
	text_placement = |svg, parent, text| MarkupCore.whitespace(text) or (if (svg) !(svg_child_model(parent)).1 else math_text_allowed(parent))
	child_expectation : Bool, Str -> MarkupCore.ChildGrammar
	child_expectation = |svg, parent| if (svg) SvgContentModel else if (parent == "mtable") NamedElements(["mtr", "mlabeledtr"]) else if (member(["mtr", "mlabeledtr"], parent)) NamedElements(["mtd"]) else TextOnly

	element_known : Bool, Str -> Bool
	element_known = |svg, local| if (svg) svg_elements(local) else math_elements(local)
	attribute_known : Bool, Str, MarkupCore.Name -> Bool
	attribute_known = allowed_attribute

	## The typed value grammar applied to a known attribute.
	grammar : Bool, Str, Str -> MarkupCore.ValueGrammar
	grammar = |svg, element, attr| if allowed_attribute(svg, element, { namespace: NoNamespace, local: attr, prefix: Unprefixed }) {
		value_grammar(svg, element, attr)
	} else {
		Language(ExtensionAttribute)
	}
}

svg_elements : _
svg_elements = ForeignNameRules.svg_elements

math_elements : _
math_elements = ForeignNameRules.math_elements

global_attributes : _
global_attributes = ForeignNameRules.global_attributes

svg_presentation : _
svg_presentation = ForeignNameRules.svg_presentation

svg_events : _
svg_events = ForeignNameRules.svg_events

math_global : _
math_global = ForeignNameRules.math_global

allowed_attribute : Bool, Str, MarkupCore.Name -> Bool
allowed_attribute = |svg, element, name| ForeignNameRules.allowed_attribute(svg, element, { namespace: name.namespace, local: name.local, prefix: name.prefix })

svg_attribute : Str, Str -> Bool
svg_attribute = ForeignNameRules.svg_attribute

math_attribute : Str, Str -> Bool
math_attribute = ForeignNameRules.math_attribute

value_grammar : Bool, Str, Str -> MarkupCore.ValueGrammar
value_grammar = ForeignNameRules.value_grammar

valid_value : Str, MarkupCore.ValueGrammar -> Bool
valid_value = |text, grammar| {
	match grammar {
		Number => MarkupValues.scalar(text).is_ok()
		NonnegativeNumber => MarkupValues.scalar(text).map_ok(|v| v >= 0) == Ok(True)
		Length => MarkupValues.length(text).is_ok()
		NonnegativeLength => MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		NumberList(count) => MarkupValues.numbers(text).map_ok(|values| values.len() == count and (count != 4 or ((values.get(2) ?? -1) >= 0 and (values.get(3) ?? -1) >= 0))) == Ok(True)
		LengthList => !MarkupCore.whitespace(text) and words_of(text).all(|v| MarkupValues.length(v).is_ok())
		PathData => text == "none" or SvgPath.validate(text, {}).is_ok()
		Paint => valid_paint(text)
		Color => valid_color(text)
		TransformList => text == "none" or MarkupValues.transforms(text).is_ok()
		Keyword(values) => member(values, text)
		AspectRatio => {
			parts = words_of(text)
			align = parts.first() ?? ""
			parts.len() >= 1 and parts.len() <= 2 and member(["none", "xMinYMin", "xMidYMin", "xMaxYMin", "xMinYMid", "xMidYMid", "xMaxYMid", "xMinYMax", "xMidYMax", "xMaxYMax"], align) and (parts.len() == 1 or [Ok("meet"), Ok("slice")].contains(parts.get(1)))
		}
		Language(AutoOrNonnegativeLength) => text == "auto" or MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		Language(Integer) => I64.from_str(text).is_ok()
		Language(NonnegativeInteger) => U64.from_str(text).is_ok()
		Language(NonnegativeNumberPair) => MarkupValues.numbers(text).map_ok(|v| v.len() >= 1 and v.len() <= 2 and v.all(|n| n >= 0)) == Ok(True)
		Language(Miterlimit) => MarkupValues.scalar(text).map_ok(|v| v >= 1) == Ok(True)
		Language(Dasharray) => text == "none" or (!words_of(text).is_empty() and words_of(text).all(|v| MarkupValues.length(v).map_ok(|n| n.value >= 0) == Ok(True)))
		Language(NonnegativeMathLength) => member(["thin", "medium", "thick"], text) or MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		Language(PositiveInteger) => U64.from_str(text).map_ok(|v| v > 0) == Ok(True)
		Language(CoordinatePairs) => MarkupValues.numbers(text).map_ok(|v| v.len() % 2 == 0) == Ok(True)
		Language(NumberList) => MarkupValues.numbers(text).is_ok()
		Language(NumberOrPercentage) => MarkupValues.scalar(text.drop_suffix("%")).is_ok()
		Language(MathLength) => member(["thin", "medium", "thick", "normal", "small", "big", "veryverythinmathspace", "verythinmathspace", "thinmathspace", "mediummathspace", "thickmathspace", "verythickmathspace", "veryverythickmathspace", "infinity"], text) or MarkupValues.length(text).is_ok()
		_ => True
	}
}

check_math_children : MarkupCore.Parsed, MarkupCore.Node, Str -> Try({}, [InvalidMarkup(MarkupCore.Error)])
check_math_children = |parsed, node, name| {
	var $elements = []
	var $count = 0
	needs_names = name == "mmultiscripts" or name == "semantics" or name == "mtable" or name == "mtr" or name == "mlabeledtr"
	text_allowed = math_text_allowed(name)
	var $child_index = 0
	while $child_index < node.children.len() {
		index = node.children.get($child_index) ?? 0
		$child_index = $child_index + 1
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported(InvalidChildIndex))
		match child.kind {
			Element(n) => {
				$count = $count + 1
				if needs_names {
					$elements = $elements.append(n.local)
				}
			}
			Text(text) if !text_allowed and !MarkupCore.whitespace(text) => return fail(parsed, node, InvalidChildren({ element: name, expected: MathElements }))
			_ => {}
		}
	}
	count = $count
	expected = match name {
		"mfrac" | "mroot" | "msub" | "msup" | "munder" | "mover" => 2
		"msubsup" | "munderover" => 3
		"mspace" | "mprescripts" | "none" => 0
		_ => count
	}
	if count != expected {
		return fail(parsed, node, InvalidChildren({ element: name, expected: ChildCount(expected) }))
	}
	if name == "mmultiscripts" {
		if count == 0 or $elements.first() == Ok("mprescripts") {
			return fail(parsed, node, InvalidChildren({ element: name, expected: BaseAndScriptPairs }))
		}
		var $post = 0
		var $pre = 0
		var $prescripts = False
		for child in $elements.drop_first(1) {
			if child == "mprescripts" {
				if $prescripts or $post % 2 != 0 {
					return fail(parsed, node, InvalidChildren({ element: name, expected: ScriptsAndPrescripts }))
				}
				$prescripts = True
			} else if $prescripts {
				$pre = $pre + 1
			} else {
				$post = $post + 1
			}
		}
		if $post % 2 != 0 or $pre % 2 != 0 {
			return fail(parsed, node, InvalidChildren({ element: name, expected: ScriptPairs }))
		}
	}
	if name == "semantics" and (count == 0 or [Ok("annotation"), Ok("annotation-xml")].contains($elements.first()) or !$elements.drop_first(1).all(|e| member(["annotation", "annotation-xml"], e))) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: ExpressionAndAnnotations }))
	}
	if name == "mtable" and !$elements.all(|e| MarkupValidate.child_placement(False, name, e)) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: NamedElements(["mtr", "mlabeledtr"]) }))
	}
	if member(["mtr", "mlabeledtr"], name) and !$elements.all(|e| MarkupValidate.child_placement(False, name, e)) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: NamedElements(["mtd"]) }))
	}
	if math_leaf(name) and count != 0 {
		return fail(parsed, node, InvalidChildren({ element: name, expected: TextOnly }))
	}
	Ok({})
}

fail : MarkupCore.Parsed, MarkupCore.Node, MarkupCore.Problem -> Try(a, [InvalidMarkup(MarkupCore.Error)])
fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(span) => span.start
		Cloned(copy) => copy.source.start
		Implied(at) => at
	}
	at = MarkupCore.position(parsed.source, offset)
	Err(InvalidMarkup(Diagnostic.{ problem: problem, location: LineColumn({ line: at.line, column: at.column }), path: [] }))
}

attr_fail : MarkupCore.Parsed, MarkupCore.Attribute, MarkupCore.Problem -> Try(a, [InvalidMarkup(MarkupCore.Error)])
attr_fail = |parsed, attr, problem| {
	at = MarkupCore.position(parsed.source, attr.span.start)
	Err(InvalidMarkup(Diagnostic.{ problem: problem, location: LineColumn({ line: at.line, column: at.column }), path: [] }))
}

words_of : Str -> List(Str)
words_of = |text| (Str.from_utf8(text.to_utf8().map(|b| if ([44, 9, 10, 13].contains(b)) 32 else b)) ?? "").split_on(" ").keep_if(|v| !v.is_empty())

check_svg_children : MarkupCore.Parsed, MarkupCore.Node, Str -> Try({}, [InvalidMarkup(MarkupCore.Error)])
check_svg_children = |parsed, node, name| {
	if node.children.is_empty() {
		return Ok({})
	}
	(model, reject_text) = svg_child_model(name)
	var $child_index = 0
	while $child_index < node.children.len() {
		index = node.children.get($child_index) ?? 0
		$child_index = $child_index + 1
		if index >= parsed.tree.nodes.len() {
			return fail(parsed, node, Unsupported(InvalidChildIndex))
		}
		# Unrestricted parents need only the arena-index check. Their children
		# are validated independently by the outer vocabulary traversal.
		if model != Any {
			child = parsed.tree.nodes.get(index) ?? node
			match child.kind {
				Element(n) if !svg_child_allowed(model, n.local) => return fail(parsed, child, InvalidChildren({ element: name, expected: SvgContentModel }))
				Text(text) if reject_text and !MarkupCore.whitespace(text) => return fail(parsed, child, InvalidChildren({ element: name, expected: SvgElements }))
				_ => {}
			}
		}
	}
	Ok({})
}

svg_child_model = |name| match name {
	"path" | "rect" | "circle" | "ellipse" | "line" | "polygon" | "polyline" | "stop" => (Shape, True)
	"image" | "use" => (Shape, False)
	"linearGradient" | "radialGradient" => (Gradient, True)
	"filter" => (Filter, True)
	"feComponentTransfer" => (ComponentTransfer, False)
	"feMerge" => (Merge, False)
	"feDiffuseLighting" | "feSpecularLighting" => (Lighting, False)
	"text" | "tspan" | "textPath" => (Textual, False)
	_ => (Any, False)
}

svg_child_allowed = |model, child| {
	if match child {
		"title" | "desc" | "metadata" => True
		_ => False
	} {
		return True
	}
	match model {
		Shape => svg_animation_child(child)
		Gradient => child == "stop" or svg_animation_child(child)
		Filter => child.starts_with("fe") or svg_animation_child(child)
		ComponentTransfer => match child {
			"feFuncA" | "feFuncB" | "feFuncG" | "feFuncR" => True
			_ => False
		}
		Merge => child == "feMergeNode"
		Lighting => match child {
			"feDistantLight" | "fePointLight" | "feSpotLight" => True
			_ => False
		}
		Textual => (
			match child {
				"a" | "tspan" | "textPath" => True
				_ => False
			}
		) or svg_animation_child(child)
		Any => True
	}
}

svg_animation_child = |child| match child {
	"animate" | "animateMotion" | "animateTransform" | "set" | "discard" => True
	_ => False
}

valid_paint : Str -> Bool
valid_paint = |text| {
	value = MarkupValues.trim_space(text)
	if member(["none", "context-fill", "context-stroke"], value) {
		return True
	}
	if value.starts_with("url(") {
		match value.split_first(")") {
			Ok({ before, after }) => before.count_utf8_bytes() > 4 and (MarkupValues.trim_space(after).is_empty() or valid_color(MarkupValues.trim_space(after)) or MarkupValues.trim_space(after) == "none")
			Err(_) => False
		}
	} else {
		valid_color(value)
	}
}

valid_color : Str -> Bool
valid_color = |text| {
	value = MarkupValues.trim_space(text).with_ascii_lowercased()
	if color_names(value) or member(["currentcolor", "transparent", "inherit", "initial", "unset", "revert", "revert-layer"], value) {
		return True
	}
	if value.starts_with("#") {
		hex = value.to_utf8().drop_first(1)
		return [3, 4, 6, 8].contains(hex.len()) and hex.all(|b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f'))
	}
	# Embedded CSS functions remain CSS; check their lexical envelope here,
	# leaving CSS evaluation and complete CSS syntax validation to consumers.
	match value.split_first("(") {
		Ok({ before, after }) => {
			if !member(["rgb", "rgba", "hsl", "hsla", "hwb", "lab", "lch", "oklab", "oklch", "color", "color-mix", "light-dark", "var"], before) or !after.ends_with(")") {
				return False
			}
			var $depth = 1.I64
			for b in after.to_utf8() {
				if b == '(' {
					$depth = $depth + 1
				}
				if b == ')' {
					$depth = $depth - 1
				}
				if $depth < 0 {
					return False
				}
			}
			$depth == 0 and after.count_utf8_bytes() > 1
		}
		Err(_) => False
	}
}

color_names = |key| match key {
	"aliceblue" | "antiquewhite" | "aqua" | "aquamarine" | "azure" | "beige" | "bisque" | "black" | "blanchedalmond" | "blue" | "blueviolet" | "brown" | "burlywood" | "cadetblue" | "chartreuse" | "chocolate" | "coral" | "cornflowerblue" | "cornsilk" | "crimson" | "cyan" | "darkblue" | "darkcyan" | "darkgoldenrod" | "darkgray" | "darkgreen" | "darkgrey" | "darkkhaki" | "darkmagenta" | "darkolivegreen" | "darkorange" | "darkorchid" | "darkred" | "darksalmon" | "darkseagreen" | "darkslateblue" | "darkslategray" | "darkslategrey" | "darkturquoise" | "darkviolet" | "deeppink" | "deepskyblue" | "dimgray" | "dimgrey" | "dodgerblue" | "firebrick" | "floralwhite" | "forestgreen" | "fuchsia" | "gainsboro" | "ghostwhite" | "gold" | "goldenrod" | "gray" | "green" | "greenyellow" | "grey" | "honeydew" | "hotpink" | "indianred" | "indigo" | "ivory" | "khaki" | "lavender" | "lavenderblush" | "lawngreen" | "lemonchiffon" | "lightblue" | "lightcoral" | "lightcyan" | "lightgoldenrodyellow" | "lightgray" | "lightgreen" | "lightgrey" | "lightpink" | "lightsalmon" | "lightseagreen" | "lightskyblue" | "lightslategray" | "lightslategrey" | "lightsteelblue" | "lightyellow" | "lime" | "limegreen" | "linen" | "magenta" | "maroon" | "mediumaquamarine" | "mediumblue" | "mediumorchid" | "mediumpurple" | "mediumseagreen" | "mediumslateblue" | "mediumspringgreen" | "mediumturquoise" | "mediumvioletred" | "midnightblue" | "mintcream" | "mistyrose" | "moccasin" | "navajowhite" | "navy" | "oldlace" | "olive" | "olivedrab" | "orange" | "orangered" | "orchid" | "palegoldenrod" | "palegreen" | "paleturquoise" | "palevioletred" | "papayawhip" | "peachpuff" | "peru" | "pink" | "plum" | "powderblue" | "purple" | "rebeccapurple" | "red" | "rosybrown" | "royalblue" | "saddlebrown" | "salmon" | "sandybrown" | "seagreen" | "seashell" | "sienna" | "silver" | "skyblue" | "slateblue" | "slategray" | "slategrey" | "snow" | "springgreen" | "steelblue" | "tan" | "teal" | "thistle" | "tomato" | "turquoise" | "violet" | "wheat" | "white" | "whitesmoke" | "yellow" | "yellowgreen" => True
	_ => False
}

aria_attributes : _
aria_attributes = ForeignNameRules.aria_attributes

ascii_lower : Str -> Str
ascii_lower = |text| text.with_ascii_lowercased()

# Fixed vocabulary scans use an index so a membership check creates no iterator
# state. Dynamic per-element vocabularies above use direct predicates.
member : List(Str), Str -> Bool
member = ForeignNameRules.member

expect {
	var $passed = True
	for input in ["<g><path/></g>", "<path><title>x</title><animate/></path>", "<linearGradient><stop/></linearGradient>", "<filter><feGaussianBlur/></filter>", "<feComponentTransfer><feFuncA/></feComponentTransfer>", "<feMerge><feMergeNode/></feMerge>", "<feDiffuseLighting><fePointLight/></feDiffuseLighting>", "<text><tspan>x</tspan></text>", "<image>text</image>"] {
		parsed = MarkupCore.parse_xml_fragment(input, MarkupCore.svg_namespace, {})?
		$passed = $passed and MarkupValidate.foreign(parsed, {}, []).is_ok()
	}
	for input in ["<path><rect/></path>", "<linearGradient><rect/></linearGradient>", "<filter><rect/></filter>", "<feComponentTransfer><animate/></feComponentTransfer>", "<feMerge><animate/></feMerge>", "<feDiffuseLighting><feFuncA/></feDiffuseLighting>", "<text><rect/></text>", "<path>text</path>", "<linearGradient>text</linearGradient>", "<filter>text</filter>"] {
		parsed = MarkupCore.parse_xml_fragment(input, MarkupCore.svg_namespace, {})?
		$passed = $passed and MarkupValidate.foreign(parsed, {}, []).is_err()
	}
	$passed
}

expect {
	parsed = MarkupCore.parse_xml_fragment("<g><path/></g>", MarkupCore.svg_namespace, {})?
	group = parsed.tree.nodes.first()?
	nodes = parsed.tree.nodes.set(0, { ..group, children: [999] })?
	invalid = { ..parsed, tree: { ..parsed.tree, nodes } }
	MarkupValidate.foreign(invalid, {}, []).map_ok(|_| {}) == Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(InvalidChildIndex), location: LineColumn({ line: 1, column: 1 }), path: [] }))
}

math_text_allowed = |name| name == "mi" or name == "mn" or name == "mo" or name == "ms" or name == "mtext" or name == "annotation"

math_leaf = |name| member(["mi", "mn", "mo", "ms", "mspace", "none", "mprescripts"], name)
