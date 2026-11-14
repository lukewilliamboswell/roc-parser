import Markup
import DomName
import HtmlValidate
import HtmlContextRules as Rules
import HtmlTree
import MarkupValidate
import MarkupVocabulary

## Finite placement checks for balanced DOM construction. Retain the returned
## child context for descendant updates without walking ancestors or parsing
## source again.
## Vocabulary, attribute values, escaping and whole-parent arity remain separate
## checks. UnsupportedContext identifies cases needing the complete parser.
MarkupContext :: [].{

	## Opaque bounded state for one enclosing element, not an ancestor stack.
	Context : ConstructorContext

	## Concrete DOM attributes have no authored source span.
	Attribute : { name : Markup.Name, value : Str }

	## HTML checks use standards-mode tree construction and the scripting flag.
	Options := { syntax : Markup.Syntax ?? HtmlSyntax, scripting : Bool ?? True }

	## Placement failures contain explicit names and no fabricated source location.
	Problem : [InvalidChild({ parent : Markup.Name, child : Markup.Name, expected : Str }), InvalidText({ parent : Markup.Name, expected : Str }), WouldRepair(Markup.StructureReason), UnexpectedNamespace({ element : Markup.Name, expected : DomName.Namespace }), UnsupportedContext({ element : Markup.Name, reason : Str })]

	## Start at an explicit enclosing boundary. No outer ancestors are assumed.
	## Propagate child contexts from a common boundary when ancestry matters.
	seed : Markup.Name, List(Attribute), Options -> Try(Context, Problem)
	seed = |element, attributes, options| {
		check_supported(element, options)?
		check_identity(element, attributes, options)?
		Ok(ConstructorContext.{ parent: element, attributes: HtmlValidate.summarize_attributes(attributes), authoring: HtmlValidate.descend(HtmlValidate.empty_context, element), scopes: Rules.descend(Rules.empty_scopes, classification(element), element.local), extension: False, options, mode: child_mode(Body, element) })
	}

	## Check one element and return its independent descendant context. The
	## original parent context remains reusable for other children and updates.
	child : Context, Markup.Name, List(Attribute) -> Try(Context, Problem)
	child = |context, element, attributes| {
		check_supported(element, context.options)?
		check_identity(element, attributes, context.options)?
		summary = HtmlValidate.summarize_attributes(attributes)
		if context.options.syntax == HtmlSyntax {
			check_html_child(context, element, summary)?
		}
		check_child_model(context, element)?
		if element.namespace == Uri(Markup.html_namespace) {
			match HtmlValidate.context_problem(context.authoring, element.local, summary) {
				Ok(_) => {}
				Err(InvalidChildren(detail)) => return Err(InvalidChild({ parent: context.parent, child: element, expected: detail.expected }))
			}
			if context.authoring.parent != "" and !HtmlValidate.parent_allowed(context.authoring, element.local) {
				return Err(InvalidChild({ parent: context.parent, child: element, expected: "allowed parent for ${element.local}" }))
			}
		}
		Ok(ConstructorContext.{ parent: element, attributes: summary, authoring: HtmlValidate.descend(context.authoring, element), scopes: Rules.descend(context.scopes, classification(element), element.local), extension: context.extension or extension_boundary(context.parent), options: context.options, mode: child_mode(context.mode, element) })
	}

	## Check a decoded text value's placement. Empty values are no-ops. This
	## does not encode source or certify raw-text escaping or initial-LF output.
	text : Context, Str -> Try({}, Problem)
	text = |context, value| {
		if value.is_empty() {
			return Ok({})
		}
		if context.options.syntax == HtmlSyntax and context.parent.namespace == Uri(Markup.html_namespace) {
			if MarkupVocabulary.html_void(context.parent.local) {
				return Err(WouldRepair(ReparentedContent))
			}
			if context.mode != Body and (["table", "tbody", "thead", "tfoot", "tr", "colgroup"].contains(context.parent.local)) and !Rules.html_whitespace(value) {
				return Err(WouldRepair(ReparentedContent))
			}
		}
		if context.parent.namespace == Uri(Markup.html_namespace) {
			rule = HtmlValidate.child_rule(context.parent.local, context.attributes, context.authoring.in_datalist)
			match HtmlValidate.text_expected(rule, value) {
				Ok(_) => {}
				Err(Rejected(expected)) => return Err(InvalidText({ parent: context.parent, expected }))
			}
		} else if !context.extension {
			foreign = classification(context.parent)
			if foreign == SvgNamespace or foreign == MathNamespace {
				if !MarkupValidate.text_placement(foreign == SvgNamespace, context.parent.local, value) {
					return Err(InvalidText({ parent: context.parent, expected: if (foreign == SvgNamespace) "SVG elements" else "MathML elements" }))
				}
			}
		}
		Ok({})
	}
}

# Opaqueness prevents callers from forging the inherited scope flags. Every
# field has fixed shape. Parent identity and finite attribute facts replace
# ancestor nodes, source strings and recursive context links.
ConstructorContext :: { parent : Markup.Name, attributes : HtmlValidate.AttributeSummary, authoring : HtmlValidate.ContextState, scopes : Rules.Scopes, extension : Bool, options : MarkupContext.Options, mode : TableMode }

TableMode : [Body, Table, Section, Row, Colgroup]

classification = |element| match element.namespace {
	Uri(uri) => Rules.namespace_class(uri)
	NoNamespace => OtherNamespace
}

uri = |element| match element.namespace {
	Uri(value) => value
	NoNamespace => ""
}

check_supported : Markup.Name, MarkupContext.Options -> Try({}, MarkupContext.Problem)
check_supported = |element, options| {
	if options.syntax == HtmlSyntax {
		if element.namespace == NoNamespace {
			return Err(UnsupportedContext({ element, reason: "HTML construction requires an explicit non-null enclosing namespace" }))
		}
		if element.namespace == Uri(Markup.html_namespace) and ["html", "head", "template", "frameset", "frame", "plaintext", "selectedcontent", "ruby", "rb", "rp", "rt", "rtc"].contains(element.local) {
			return Err(UnsupportedContext({ element, reason: "document, template, projection or history-sensitive parsing requires the complete parser" }))
		}
	}
	Ok({})
}

check_identity : Markup.Name, List(MarkupContext.Attribute), MarkupContext.Options -> Try({}, MarkupContext.Problem)
check_identity = |element, attributes, options| {
	if options.syntax != HtmlSyntax {
		return Ok({})
	}
	if HtmlTree.canonical_element_name(DomName.qualified(element), uri(element)) != element {
		return Err(UnsupportedContext({ element, reason: "HTML spelling changes the element's explicit identity" }))
	}
	var $seen = Dict.empty()
	for attribute in attributes {
		key = (attribute.name.namespace, attribute.name.local)
		if $seen.contains(key) {
			return Err(UnsupportedContext({ element, reason: "duplicate expanded attribute identity" }))
		}
		$seen = $seen.insert(key, True)
		if HtmlTree.canonical_attribute_name(DomName.qualified(attribute.name), uri(element)) != attribute.name {
			return Err(UnsupportedContext({ element, reason: "HTML spelling changes an attribute's explicit identity" }))
		}
	}
	Ok({})
}

child_mode = |previous, element| {
	if element.namespace != Uri(Markup.html_namespace) {
		return previous
	}
	match element.local {
		"table" => Table
		"tbody" | "thead" | "tfoot" => Section
		"tr" => Row
		"colgroup" => Colgroup
		"td" | "th" | "caption" => Body
		_ => previous
	}
}

extension_boundary = |element| (element.namespace == Uri(Markup.svg_namespace) and element.local == "metadata") or (element.namespace == Uri(Markup.mathml_namespace) and element.local == "annotation-xml")

check_child_model : MarkupContext.Context, Markup.Name -> Try({}, MarkupContext.Problem)
check_child_model = |context, element| {
	if context.parent.namespace == Uri(Markup.html_namespace) {
		rule = HtmlValidate.child_rule(context.parent.local, context.attributes, context.authoring.in_datalist)
		match HtmlValidate.child_expected(rule, element) {
			Ok(_) => {}
			Err(Rejected(expected)) => return Err(InvalidChild({ parent: context.parent, child: element, expected: if (expected.is_empty()) "text only" else expected }))
		}
	} else if !context.extension {
		foreign = classification(context.parent)
		if (foreign == SvgNamespace or foreign == MathNamespace) and !MarkupValidate.child_placement(foreign == SvgNamespace, context.parent.local, element.local) {
			return Err(InvalidChild({ parent: context.parent, child: element, expected: MarkupValidate.child_expectation(foreign == SvgNamespace, context.parent.local) }))
		}
	}
	Ok({})
}

check_html_child : MarkupContext.Context, Markup.Name, HtmlValidate.AttributeSummary -> Try({}, MarkupContext.Problem)
check_html_child = |context, element, attributes| {
	parent_class = classification(context.parent)
	namespace = element.namespace
	name = element.local
	text_behavior = if (parent_class == HtmlNamespace) MarkupVocabulary.text_behavior(context.parent.local, context.options.scripting) else Data
	if text_behavior != Data {
		return Err(InvalidChild({ parent: context.parent, child: element, expected: "text content" }))
	}
	integration = (parent_class == SvgNamespace and Rules.svg_integration(context.parent.local)) or (parent_class == MathNamespace and context.parent.local == "annotation-xml" and context.attributes.html_encoding)
	math_text = parent_class == MathNamespace and Rules.math_text_integration(context.parent.local) and !["mglyph", "malignmark"].contains(name)
	math_svg = parent_class == MathNamespace and context.parent.local == "annotation-xml" and name == "svg"
	in_html = parent_class == HtmlNamespace or integration or math_text or math_svg
	if !in_html and Rules.foreign_breakout(name, attributes.font_breakout) {
		return Err(WouldRepair(ReparentedContent))
	}
	expected = if in_html {
		if (name == "svg") Uri(Markup.svg_namespace) else if (name == "math") Uri(Markup.mathml_namespace) else Uri(Markup.html_namespace)
	} else context.parent.namespace
	if namespace != expected {
		return Err(UnexpectedNamespace({ element, expected }))
	}
	if !in_html {
		return Ok({})
	}
	if namespace != Uri(Markup.html_namespace) {
		return Ok({})
	}
	if context.scopes.p and Rules.closes_p(name) {
		return Err(WouldRepair(MissingEndTag("p")))
	}
	if Rules.heading(name) and context.parent.namespace == Uri(Markup.html_namespace) and Rules.heading(context.parent.local) {
		return Err(WouldRepair(MissingEndTag(context.parent.local)))
	}
	if name == "form" and context.scopes.form {
		return Err(WouldRepair(IgnoredMarkup))
	}
	if name == "button" and context.scopes.button {
		return Err(WouldRepair(MissingEndTag("button")))
	}
	if name == "nobr" and context.scopes.nobr {
		return Err(WouldRepair(ReconstructedContent))
	}
	if name == "li" and context.scopes.li {
		return Err(WouldRepair(MissingEndTag("li")))
	}
	if ["dd", "dt"].contains(name) and context.scopes.definition_item {
		return Err(WouldRepair(ReparentedContent))
	}
	if name == "select" and context.scopes.select {
		return Err(WouldRepair(IgnoredMarkup))
	}
	if name == "input" and context.scopes.select {
		return Err(WouldRepair(ReparentedContent))
	}
	if ["option", "optgroup", "hr"].contains(name) and context.scopes.select and context.parent.namespace == Uri(Markup.html_namespace) and Rules.implied_end_element(context.parent.local) and !(name == "option" and context.parent.local == "optgroup") {
		return Err(WouldRepair(MissingEndTag(context.parent.local)))
	}
	if ["option", "optgroup"].contains(name) and context.parent.namespace == Uri(Markup.html_namespace) and context.parent.local == "option" {
		return Err(WouldRepair(MissingEndTag("option")))
	}
	if ["option", "optgroup", "hr"].contains(name) and context.scopes.select and (context.scopes.option or (name != "option" and context.scopes.optgroup)) {
		return Err(UnsupportedContext({ element, reason: "select implied-end handling needs the full parser in this descendant context" }))
	}
	if ["html", "head", "body", "image"].contains(name) {
		return Err(WouldRepair(IgnoredMarkup))
	}
	if context.mode == Body and ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr", "frame"].contains(name) {
		return Err(WouldRepair(IgnoredMarkup))
	}
	if context.mode == Table and name == "tr" {
		return Err(WouldRepair(ImpliedElement("tbody")))
	}
	if context.mode == Table and name == "col" {
		return Err(WouldRepair(ImpliedElement("colgroup")))
	}
	if (context.mode == Table or context.mode == Section) and ["td", "th"].contains(name) {
		return Err(WouldRepair(ImpliedElement("tr")))
	}
	if context.mode == Table and !["caption", "colgroup", "thead", "tbody", "tfoot", "script", "style"].contains(name) {
		return Err(WouldRepair(ReparentedContent))
	}
	if context.mode == Section and !["tr", "script", "style"].contains(name) {
		return Err(WouldRepair(ReparentedContent))
	}
	if context.mode == Row and !["td", "th", "script", "style"].contains(name) {
		return Err(WouldRepair(ReparentedContent))
	}
	if context.mode == Colgroup and name != "col" {
		return Err(WouldRepair(ReparentedContent))
	}
	Ok({})
}

html_name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

svg_name = |local| { namespace: Uri(Markup.svg_namespace), prefix: Unprefixed, local }

math_name = |local| { namespace: Uri(Markup.mathml_namespace), prefix: Unprefixed, local }

plain_attribute = |local, value| { name: { namespace: NoNamespace, prefix: Unprefixed, local }, value }

expect {
	body = MarkupContext.seed(html_name("body"), [], {})?
	paragraph = MarkupContext.child(body, html_name("p"), [])?
	span = MarkupContext.child(paragraph, html_name("span"), [])?
	MarkupContext.child(span, html_name("div"), []).map_ok(|_| {}) == Err(WouldRepair(MissingEndTag("p")))
}
expect {
	paragraph = MarkupContext.seed(html_name("p"), [], { syntax: XmlSyntax })?
	span = MarkupContext.child(paragraph, html_name("span"), [])?
	MarkupContext.child(span, html_name("div"), []).is_ok()
}
expect {
	button = MarkupContext.seed(html_name("button"), [], {})?
	span = MarkupContext.child(button, html_name("span"), [])?
	MarkupContext.child(span, html_name("a"), [plain_attribute("href", "/")]).is_err() and MarkupContext.child(span, html_name("input"), [plain_attribute("type", "hidden")]).is_ok() and MarkupContext.child(span, html_name("input"), [plain_attribute("type", "text")]).is_err()
}
expect {
	anchor = MarkupContext.seed(html_name("a"), [], {})?
	span = MarkupContext.child(anchor, html_name("span"), [])?
	MarkupContext.child(span, html_name("a"), []).is_err()
}
expect {
	form = MarkupContext.seed(html_name("form"), [], {})?
	div = MarkupContext.child(form, html_name("div"), [])?
	MarkupContext.child(div, html_name("form"), []).map_ok(|_| {}) == Err(WouldRepair(IgnoredMarkup))
}
expect {
	list = MarkupContext.seed(html_name("ul"), [], {})?
	item = MarkupContext.child(list, html_name("li"), [])?
	span = MarkupContext.child(item, html_name("span"), [])?
	nested = MarkupContext.child(item, html_name("ul"), [])?
	MarkupContext.child(span, html_name("li"), []).is_err() and MarkupContext.child(nested, html_name("li"), []).is_ok()
}
expect {
	select = MarkupContext.seed(html_name("select"), [], {})?
	group = MarkupContext.child(select, html_name("optgroup"), [])?
	option = MarkupContext.child(group, html_name("option"), [])?
	span = MarkupContext.child(option, html_name("span"), [])?
	MarkupContext.text(span, "option text").is_ok() and MarkupContext.child(span, html_name("input"), [plain_attribute("type", "hidden")]).is_err()
}
expect {
	list = MarkupContext.seed(html_name("datalist"), [], {})?
	option = MarkupContext.child(list, html_name("option"), [])?
	MarkupContext.child(option, html_name("span"), []).is_err() and MarkupContext.text(option, "choice").is_ok()
}
expect {
	table = MarkupContext.seed(html_name("table"), [], {})?
	MarkupContext.child(table, html_name("tr"), []).map_ok(|_| {}) == Err(WouldRepair(ImpliedElement("tbody"))) and MarkupContext.text(table, "outside").map_ok(|_| {}) == Err(WouldRepair(ReparentedContent))
}
expect {
	table = MarkupContext.seed(html_name("table"), [], {})?
	section = MarkupContext.child(table, html_name("tbody"), [])?
	row = MarkupContext.child(section, html_name("tr"), [])?
	cell = MarkupContext.child(row, html_name("td"), [])?
	MarkupContext.text(cell, "cell text").is_ok()
}
expect {
	table = MarkupContext.seed(html_name("table"), [], {})?
	script = MarkupContext.child(table, html_name("script"), [])?
	MarkupContext.text(script, "program text").is_ok() and MarkupContext.child(script, html_name("span"), []).is_err()
}
expect {
	void = MarkupContext.seed(html_name("br"), [], {})?
	MarkupContext.text(void, "").is_ok() and MarkupContext.text(void, " ").is_err() and MarkupContext.child(void, html_name("span"), []).is_err()
}
expect {
	drawing = MarkupContext.seed(svg_name("svg"), [], {})?
	gradient = MarkupContext.child(drawing, svg_name("linearGradient"), [])?
	MarkupContext.child(gradient, svg_name("stop"), []).is_ok() and MarkupContext.child(gradient, svg_name("rect"), []).is_err() and MarkupContext.text(gradient, "text").is_err()
}
expect {
	shape = MarkupContext.seed(svg_name("path"), [], {})?
	MarkupContext.child(shape, svg_name("title"), []).is_ok() and MarkupContext.child(shape, svg_name("rect"), []).is_err()
}
expect {
	foreign = MarkupContext.seed(svg_name("foreignObject"), [], {})?
	MarkupContext.child(foreign, html_name("div"), []).is_ok() and MarkupContext.child(foreign, html_name("td"), []).is_err()
}
expect {
	drawing = MarkupContext.seed(svg_name("g"), [], {})?
	MarkupContext.child(drawing, svg_name("div"), []).map_ok(|_| {}) == Err(WouldRepair(ReparentedContent))
}
expect {
	text = MarkupContext.seed(math_name("mtext"), [], {})?
	MarkupContext.child(text, html_name("span"), []).is_ok() and MarkupContext.child(text, math_name("mrow"), []).is_err()
}
expect {
	annotation = MarkupContext.seed(math_name("annotation-xml"), [plain_attribute("encoding", "TEXT/HTML")], {})?
	MarkupContext.child(annotation, html_name("div"), []).is_ok()
}
expect {
	table = MarkupContext.seed(math_name("mtable"), [], { syntax: XmlSyntax })?
	row = MarkupContext.child(table, math_name("mtr"), [])?
	MarkupContext.child(row, math_name("mtd"), []).is_ok() and MarkupContext.child(table, math_name("mi"), []).is_err()
}
expect {
	row = MarkupContext.seed(math_name("mrow"), [], { syntax: XmlSyntax })?
	identifier = MarkupContext.child(row, math_name("mi"), [])?
	MarkupContext.text(identifier, "x").is_ok() and MarkupContext.text(row, "x").is_err() and MarkupContext.child(identifier, math_name("mi"), []).is_err()
}
expect {
	metadata = MarkupContext.seed(svg_name("metadata"), [], { syntax: XmlSyntax })?
	path = MarkupContext.child(metadata, svg_name("path"), [])?
	MarkupContext.child(path, svg_name("rect"), []).is_ok() and MarkupContext.text(path, "extension text").is_ok()
}
expect {
	body = MarkupContext.seed(html_name("body"), [], {})?
	MarkupContext.child(body, { namespace: NoNamespace, prefix: Unprefixed, local: "div" }, []).is_err() and MarkupContext.seed(svg_name("lineargradient"), [], {}).is_err() and MarkupContext.seed({ namespace: Uri(Markup.html_namespace), prefix: Prefix("h"), local: "div" }, [], {}).is_err()
}
expect ["template", "frameset", "plaintext", "selectedcontent", "html"].all(|local| MarkupContext.seed(html_name(local), [], {}).is_err())

expect {
	select = MarkupContext.seed(html_name("select"), [], {})?
	div = MarkupContext.child(select, html_name("div"), [])?
	term = MarkupContext.child(div, html_name("dt"), [])?
	MarkupContext.child(term, html_name("optgroup"), []).map_ok(|_| {}) == Err(WouldRepair(MissingEndTag("dt")))
}
expect {
	select = MarkupContext.seed(html_name("select"), [], {})?
	div = MarkupContext.child(select, html_name("div"), [])?
	paragraph = MarkupContext.child(div, html_name("p"), [])?
	MarkupContext.child(paragraph, html_name("option"), []).map_ok(|_| {}) == Err(WouldRepair(MissingEndTag("p")))
}
expect {
	select = MarkupContext.seed(html_name("select"), [], {})?
	div = MarkupContext.child(select, html_name("div"), [])?
	var $ok = True
	for parent in ["dd", "dt", "p"] {
		current = MarkupContext.child(div, html_name(parent), [])?
		$ok = $ok and ["option", "optgroup", "hr"].all(|incoming| MarkupContext.child(current, html_name(incoming), []).is_err())
	}
	group = MarkupContext.child(select, html_name("optgroup"), [])?
	$ok and MarkupContext.child(group, html_name("option"), []).is_ok() and MarkupContext.child(group, html_name("optgroup"), []).is_err() and MarkupContext.child(group, html_name("hr"), []).is_err()
}
