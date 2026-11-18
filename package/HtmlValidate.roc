import HtmlNameRules
import Parser
import Diagnostic
import MarkupCore
import MarkupValidate
import HtmlTokenizer
import DomName
import MarkupVocabulary

## HTML authoring checks, separate from browser tree recovery. Embedded CSS and
## JavaScript remain source text rather than claims of validated programs.
HtmlValidate :: [].{
	validate : MarkupCore.Parsed, MarkupCore.Props, List(MarkupValidate.Hole) -> Try(MarkupCore.Parsed, [InvalidMarkup(MarkupCore.Error)])
	validate = |parsed, props, holes| {
		var $diagnostics = parsed.diagnostics
		hole_positions = holes.fold(Dict.empty(), |dict, hole| dict.insert((hole.node, hole.attribute), True))
		var $pending = parsed.tree.roots.map(|index| { index, parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(InvalidTreeIndex), location: LineColumn({ line: 1, column: 1 }), path: [] }))
			match node.kind {
				Element(name) if name.namespace == Uri(MarkupCore.html_namespace) => {
					known_element = elements(name.local)
					custom = !known_element and custom_element(name.local)
					if !known_element and !custom {
						if !props.allow_unknown_elements {
							return fail(parsed, node, UnknownElement(name.local.release_excess_capacity()))
						}
						offset = match node.origin {
							Authored(span) => span.start
							Implied(at) => at
							Cloned(info) => info.source.start
						}
						at = Parser.Quotation.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
						if $diagnostics.len() >= props.max_diagnostics {
							return fail(parsed, node, LimitExceeded({ resource: Diagnostics, max: props.max_diagnostics, observed: AtLeast(props.max_diagnostics + 1) }))
						}
						$diagnostics = $diagnostics.append(Diagnostic.{ problem: UnknownElement(name.local.release_excess_capacity()), location: LineColumn({ line: at.line, column: at.column }), path: [] })
					}
					var $at = 0
					for attr in node.attributes {
						known = match attr.name.namespace {
							NoNamespace => allowed(name.local, attr.name.local) or custom
							Uri("http://www.w3.org/2000/xmlns/") => True
							Uri("http://www.w3.org/XML/1998/namespace") => (
								match attr.name.local {
									"lang" | "space" | "base" | "id" => True
									_ => False
								}
							)
							_ => False
						}
						if !known {
							if !props.allow_unknown_attributes {
								return attr_fail(parsed, attr, UnknownAttribute({ element: name.local.release_excess_capacity(), attribute: attr.name.local.release_excess_capacity() }))
							}
							at = Parser.Quotation.position(Str.from_utf8_lossy(parsed.source.bytes), attr.span.start)
							if $diagnostics.len() >= props.max_diagnostics {
								return attr_fail(parsed, attr, LimitExceeded({ resource: Diagnostics, max: props.max_diagnostics, observed: AtLeast(props.max_diagnostics + 1) }))
							}
							$diagnostics = $diagnostics.append(Diagnostic.{ problem: UnknownAttribute({ element: name.local.release_excess_capacity(), attribute: attr.name.local.release_excess_capacity() }), location: LineColumn({ line: at.line, column: at.column }), path: [] })
						}
						if props.check_values and known and attr.name.namespace == NoNamespace and !hole_positions.contains((item.index, $at)) {
							expected = grammar(name.local, attr.name.local)
							if !valid(attr.value, expected) {
								return attr_fail(parsed, attr, InvalidValue({ attribute: attr.name.local, value: Diagnostic.excerpt(attr.value.to_utf8()), expected }))
							}
						}
						$at = $at + 1
					}
					ancestry = context_of(item)
					attributes = if (ancestry.interactive or ancestry.in_option or name.local == "option") summarize_attributes(node.attributes) else empty_attributes
					match context_problem(ancestry, name.local, attributes) {
						Ok(_) => {}
						Err(problem) => return fail(parsed, node, problem)
					}
					check_children_with_attributes(parsed, node, name.local, item.in_datalist, attributes)?
					# Standalone fragment roots have no authored parent.
					if item.parent != "" and !parent_allowed(ancestry, name.local) {
						return fail(parsed, node, InvalidChildren({ element: item.parent, expected: AllowedParent(name.local) }))
					}
					if name.local == "img" and !has_attribute(node, "alt") and !(
						match value(node, "role") {
							"presentation" | "none" => True
							_ => False
						}
					) {
						return fail(parsed, node, InvalidChildren({ element: "img", expected: ImageAlternative }))
					}
					if name.local == "input" and HtmlTokenizer.lower(value(node, "type")) == "image" and (!has_attribute(node, "src") or !has_attribute(node, "alt")) {
						return fail(parsed, node, InvalidChildren({ element: "input", expected: ImageInputAttributes }))
					}
					next_context = descend(ancestry, name)
					for child in node.children {
						$pending = $pending.append(context_frame(child, next_context))
					}
				}
				Element(name) => {
					next_context = descend(context_of(item), name)
					for child in node.children {
						$pending = $pending.append(context_frame(child, next_context))
					}
				}
				_ => {}
			}
		}
		Ok({ ..parsed, diagnostics: $diagnostics })
	}

	# Internal finite authoring state shared with constructor-context queries.
	ContextState : { parent : Str, interactive : Bool, anchor : Bool, option_context : Bool, in_option : Bool, in_datalist : Bool, selected_context : Bool }
	empty_context : ContextState
	empty_context = { parent: "", interactive: False, anchor: False, option_context: False, in_option: False, in_datalist: False, selected_context: False }
	AttributeSummary : { href : Bool, tabindex : Bool, hidden_input : Bool, controls : Bool, usemap : Bool, label : Bool, value : Bool, html_encoding : Bool, font_breakout : Bool }
	empty_attributes : AttributeSummary
	empty_attributes = { href: False, tabindex: False, hidden_input: False, controls: False, usemap: False, label: False, value: False, html_encoding: False, font_breakout: False }
	summarize_attributes : List({ name : MarkupCore.Name, value : Str, ..extra }) -> AttributeSummary
	summarize_attributes = |attributes| {
		var $seen_type = False
		var $seen_encoding = False
		var $summary = empty_attributes
		for attribute in attributes {
			if attribute.name.namespace == NoNamespace {
				match attribute.name.local {
					"href" => {
						$summary = { ..$summary, href: True }
					}
					"tabindex" => {
						$summary = { ..$summary, tabindex: True }
					}
					"type" if !$seen_type => {
						$seen_type = True
						$summary = { ..$summary, hidden_input: HtmlTokenizer.lower(attribute.value) == "hidden" }
					}
					"controls" => {
						$summary = { ..$summary, controls: True }
					}
					"usemap" => {
						$summary = { ..$summary, usemap: True }
					}
					"label" => {
						$summary = { ..$summary, label: True }
					}
					"value" => {
						$summary = { ..$summary, value: True }
					}
					"encoding" if !$seen_encoding => {
						$seen_encoding = True
						$summary = { ..$summary, html_encoding: ["text/html", "application/xhtml+xml"].contains(HtmlTokenizer.lower(attribute.value)) }
					}
					"color" | "face" | "size" => {
						$summary = { ..$summary, font_breakout: True }
					}
					_ => {}
				}
			}
		}
		$summary
	}
	context_problem : ContextState, Str, AttributeSummary -> Try({}, [InvalidChildren({ element : Str, expected : MarkupCore.ChildGrammar })])
	context_problem = |context, element, attributes| {
		if element == "a" and context.anchor {
			return Err(InvalidChildren({ element: "a", expected: NoNestedAnchor }))
		}
		if context.interactive and (interactive_summary(attributes, element) or attributes.tabindex) {
			return Err(InvalidChildren({ element: "button", expected: NoInteractiveContent }))
		}
		if context.in_option and (interactive_summary(attributes, element) or ["datalist", "object"].contains(element) or attributes.tabindex) {
			return Err(InvalidChildren({ element: "option", expected: OptionContent }))
		}
		if element == "selectedcontent" and !context.selected_context {
			return Err(InvalidChildren({ element: "selectedcontent", expected: SelectButtonDescendant }))
		}
		Ok({})
	}
	descend : ContextState, MarkupCore.Name -> ContextState
	descend = |context, element| if element.namespace == Uri(MarkupCore.html_namespace) {
		{ parent: element.local, interactive: context.interactive or element.local == "button", anchor: context.anchor or element.local == "a", option_context: context.option_context or ["select", "optgroup", "datalist"].contains(element.local), in_option: context.in_option or element.local == "option", in_datalist: context.in_datalist or element.local == "datalist", selected_context: context.selected_context or (element.local == "button" and context.parent == "select") }
	} else { ..context, parent: "" }
	parent_allowed : ContextState, Str -> Bool
	parent_allowed = |context, element| match element {
		"li" => ["ul", "ol", "menu"].contains(context.parent)
		"dt" | "dd" => context.parent == "dl" or context.parent == "div"
		"tr" => ["table", "thead", "tbody", "tfoot"].contains(context.parent)
		"td" | "th" => context.parent == "tr"
		"thead" | "tbody" | "tfoot" | "caption" | "colgroup" => context.parent == "table"
		"col" => context.parent == "colgroup"
		"option" | "optgroup" => context.option_context
		"summary" => context.parent == "details"
		"legend" => context.parent == "fieldset" or context.parent == "optgroup"
		"source" => ["audio", "video", "picture"].contains(context.parent)
		"track" => context.parent == "audio" or context.parent == "video"
		_ => True
	}
	ChildRule : [AnyChildren, NoChildren, TextOnly, ElementNames(List(Str))]
	child_rule : Str, AttributeSummary, Bool -> ChildRule
	child_rule = |element, attributes, in_datalist| {
		if void_elements(element) or element == "selectedcontent" or (element == "option" and attributes.label and attributes.value) {
			return NoChildren
		}
		if element == "option" and (attributes.label or in_datalist) {
			return TextOnly
		}
		match element {
			"ul" | "ol" | "menu" => ElementNames(["li", "script", "template"])
			"dl" => ElementNames(["dt", "dd", "div", "script", "template"])
			"table" => ElementNames(["caption", "colgroup", "thead", "tbody", "tfoot", "tr", "script", "template"])
			"thead" | "tbody" | "tfoot" => ElementNames(["tr", "script", "template"])
			"tr" => ElementNames(["td", "th", "script", "template"])
			"colgroup" => ElementNames(["col", "script", "template"])
			"select" => ElementNames(["button", "option", "optgroup", "hr", "div", "script", "template", "noscript"])
			"optgroup" => ElementNames(["legend", "option", "div", "script", "template", "noscript"])
			_ => AnyChildren
		}
	}
	child_expected : ChildRule, MarkupCore.Name -> Try({}, [Rejected(MarkupCore.ChildGrammar)])
	child_expected = |rule, element| match rule {
		AnyChildren => Ok({})
		NoChildren => Err(Rejected(NoChildren))
		TextOnly => Err(Rejected(TextOnly))
		ElementNames(names) => if (element.namespace == Uri(MarkupCore.html_namespace) and names.contains(element.local)) Ok({}) else Err(Rejected(NamedElements(names)))
	}
	text_expected : ChildRule, Str -> Try({}, [Rejected(MarkupCore.ChildGrammar)])
	text_expected = |rule, text| if (MarkupCore.whitespace(text)) Ok({}) else match rule {
		NoChildren | ElementNames(_) => Err(Rejected(ElementsOnly))
		_ => Ok({})
	}

	element_known : Str -> Bool
	element_known = HtmlNameRules.element_known

	custom_element_name : Str -> Bool
	custom_element_name = HtmlNameRules.custom_element_name

	boolean_attribute : Str, Str -> Bool
	boolean_attribute = HtmlNameRules.boolean_attribute

	attribute_known : Str, MarkupCore.Name -> Bool
	attribute_known = |element, name| match name.namespace {
		NoNamespace => allowed(element, name.local)
		Uri("http://www.w3.org/2000/xmlns/") => True
		Uri("http://www.w3.org/XML/1998/namespace") => ["lang", "space", "base", "id"].contains(name.local)
		_ => False
	}

	## Known finite keyword values for an applicable HTML attribute. Metadata
	## does not change the ordinary author's value validation or supply defaults.
	enumerated_values : Str, Str -> Try(List(Str), [NotEnumerated])
	enumerated_values = HtmlNameRules.enumerated_values

	grammar : Str, Str -> MarkupCore.ValueGrammar
	grammar = HtmlNameRules.grammar

}

valid = |text, expected| match expected {
	Keyword(values) => values.any(|v| HtmlTokenizer.lower(text) == HtmlTokenizer.lower(v))
	Number => I64.from_str(text).is_ok()
	NonnegativeNumber => U64.from_str(text).is_ok()
	_ => True
}

custom_element : _
custom_element = HtmlNameRules.custom_element

allowed : _
allowed = HtmlNameRules.allowed

has_attribute = |node, key| node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == key)

value = |node, key| node.attributes.find_first(|a| a.name.namespace == NoNamespace and a.name.local == key).map_ok(|a| a.value) ?? ""

fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(s) => s.start
		Implied(o) => o
		Cloned(info) => info.source.start
	}
	p = Parser.Quotation.position(Str.from_utf8_lossy(parsed.source.bytes), offset)
	Err(InvalidMarkup(Diagnostic.{ problem: problem, location: LineColumn({ line: p.line, column: p.column }), path: [] }))
}

attr_fail = |parsed, attr, problem| {
	p = Parser.Quotation.position(Str.from_utf8_lossy(parsed.source.bytes), attr.span.start)
	Err(InvalidMarkup(Diagnostic.{ problem: problem, location: LineColumn({ line: p.line, column: p.column }), path: [] }))
}

elements : _
elements = HtmlNameRules.elements

global : _
global = HtmlNameRules.global

booleans : _
booleans = HtmlNameRules.booleans

aria : _
aria = HtmlNameRules.aria

events : _
events = HtmlNameRules.events

attributes : _
attributes = HtmlNameRules.attributes

interactive_summary = |attribute_facts, name| ["button", "details", "embed", "iframe", "label", "select", "textarea"].contains(name) or (name == "input" and !attribute_facts.hidden_input) or (name == "a" and attribute_facts.href) or (["audio", "video"].contains(name) and attribute_facts.controls) or (name == "img" and attribute_facts.usemap)

context_of = |item| { parent: item.parent, interactive: item.interactive, anchor: item.anchor, option_context: item.option_context, in_option: item.in_option, in_datalist: item.in_datalist, selected_context: item.selected_context }

check_children : MarkupCore.Parsed, MarkupCore.Node, Str, Bool -> Try({}, [InvalidMarkup(MarkupCore.Error)])
check_children = |parsed, node, name, in_datalist| check_children_with_attributes(parsed, node, name, in_datalist, if (name == "option") HtmlValidate.summarize_attributes(node.attributes) else HtmlValidate.empty_attributes)

check_children_with_attributes = |parsed, node, name, in_datalist, attribute_facts| {
	rule = HtmlValidate.child_rule(name, attribute_facts, in_datalist)
	if rule == AnyChildren {
		node_count = parsed.tree.nodes.len()
		if node.children.any(|index| index >= node_count) {
			return fail(parsed, node, Unsupported(InvalidChildIndex))
		}
		return Ok({})
	}
	for index in node.children {
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported(InvalidChildIndex))
		derived = match child.origin {
			Cloned(_) => True
			_ => False
		}
		if !(name == "selectedcontent" and derived) {
			match child.kind {
				Element(n) => match HtmlValidate.child_expected(rule, n) {
					Ok(_) => {}
					Err(Rejected(expected)) => return fail(parsed, child, InvalidChildren({ element: name, expected }))
				}
				Text(text) => match HtmlValidate.text_expected(rule, text) {
					Ok(_) => {}
					Err(Rejected(expected)) => return fail(parsed, child, InvalidChildren({ element: name, expected }))
				}
				_ => {}
			}
		}
	}
	Ok({})
}

void_elements = |name| match name {
	"area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "param" | "source" | "track" | "wbr" => True
	_ => False
}

# Metadata covers current names without tightening the ordinary value grammar.
expect HtmlValidate.boolean_attribute("iframe", "credentialless")
expect !HtmlValidate.boolean_attribute("div", "credentialless")
expect HtmlValidate.boolean_attribute("video", "playsinline") and HtmlValidate.boolean_attribute("video", "disablepictureinpicture")
expect !HtmlValidate.boolean_attribute("audio", "playsinline") and !HtmlValidate.boolean_attribute("audio", "disablepictureinpicture")
expect HtmlValidate.enumerated_values("audio", "playsinline") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("div", "popover") == Ok(["", "auto", "manual", "hint"])
expect HtmlValidate.enumerated_values("div", "inputmode").map_ok(|values| values.contains("decimal")) == Ok(True)
expect HtmlValidate.enumerated_values("template", "shadowrootmode") == Ok(["open", "closed"])
expect HtmlValidate.enumerated_values("div", "shadowrootmode") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("form", "autocomplete") == Ok(["on", "off"])
expect HtmlValidate.enumerated_values("input", "autocomplete") == Err(NotEnumerated)
expect HtmlValidate.enumerated_values("input", "capture") == Ok(["user", "environment"])
expect HtmlValidate.enumerated_values("dialog", "closedby") == Ok(["any", "closerequest", "none"])
expect HtmlValidate.enumerated_values("div", "aria-hidden") == Ok(["true", "false", "undefined"])
expect HtmlValidate.grammar("div", "inputmode") == Language(HTMLAttribute)
expect HtmlValidate.custom_element_name("x-a=b") and HtmlValidate.custom_element_name("x:a-b") and HtmlValidate.custom_element_name("x-<")
expect HtmlValidate.custom_element_name("x-\u(80)") and HtmlValidate.custom_element_name("x-\u(F0000)")
expect !HtmlValidate.custom_element_name("x-a>b") and !HtmlValidate.custom_element_name("x-a b") and !HtmlValidate.custom_element_name("x-\u(0)")
expect !HtmlValidate.custom_element_name("x-Ab") and !HtmlValidate.custom_element_name("font-face") and !HtmlValidate.custom_element_name("nohyphen")

expect HtmlValidate.enumerated_values("td", "scope") == Err(NotEnumerated) and HtmlValidate.enumerated_values("th", "scope") == Ok(["row", "col", "rowgroup", "colgroup"])
expect HtmlValidate.grammar("iframe", "credentialless") == Language(HTMLAttribute)

expect {
	parent : MarkupCore.Node
	parent = { kind: Element({ local: "p", namespace: Uri(MarkupCore.html_namespace), prefix: Unprefixed }), attributes: [], children: [1], origin: Authored({ start: 0, end: 8 }) }
	child : MarkupCore.Node
	child = { kind: Text("x"), attributes: [], children: [], origin: Authored({ start: 3, end: 4 }) }
	parsed : MarkupCore.Parsed
	parsed = { source: MarkupCore.Source.from_bytes("<p>x</p>".to_utf8()), tree: { nodes: [parent, child], roots: [0] }, diagnostics: [] }
	invalid = { ..parent, children: [1, 2] }
	check_children(parsed, parent, "p", False) == Ok({}) and check_children(parsed, invalid, "p", False) == Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(InvalidChildIndex), location: LineColumn({ line: 1, column: 1 }), path: [] }))
}

context_frame : U64, HtmlValidate.ContextState -> _
context_frame = |index, context| { index, parent: context.parent, interactive: context.interactive, anchor: context.anchor, option_context: context.option_context, in_option: context.in_option, in_datalist: context.in_datalist, selected_context: context.selected_context }
