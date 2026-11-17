import MarkupCore
import HtmlContextRules as Rules
import MarkupVocabulary

## Check a bounded set of HTML tree-construction repairs for explicitly named
## DOM nodes. Names are already resolved: this module never folds spelling,
## infers a namespace, validates a vocabulary or reads an ancestor list.
## A successful check does not certify a complete serialization round trip.
## No nodes are created or mutated. Use [MarkupContext] for canonical
## source-authoring placement and HTML, SVG or MathML content models.
DomConstruction :: [].{

	## Opaque immediate-parent identity and finite inherited scope facts.
	Context : ConstructionContext
	Props : ConstructionContext.Props

	## Seed a fresh enclosing boundary; no outer ancestors are assumed.
	RootProps := { parent : MarkupCore.Name, scripting : Bool ?? True }

	RepairRule : [CloseParagraph, InsertTableSection, InvalidTableChild, TablePartOutsideTable, NestedAnchor, NestedForm, NestedButton, CloseListItem, CloseDescriptionItem, CloseOption, CloseHeading, ChildOfVoid, ElementInTextContent, FosterTableText]
	RepairDetail : { rule : RepairRule, parent : MarkupCore.Name, child : [Element(MarkupCore.Name), Text] }
	Problem : [Repair(RepairDetail), Unsupported({ parent : MarkupCore.Name, child : MarkupCore.Name, reason : [AttributeDependentTableInput] })]

	## Total for every explicit name, including unknown and non-HTML names.
	root : RootProps -> Context
	root = |props| descend({ parent: props.parent, scripting: props.scripting, scopes: Rules.empty_scopes, anchor: False, form_pointer: False, in_template: False, table_form: False, mode: Body }, props.parent)

	## Check one child and return its independent descendant context. Reuse the
	## original context for siblings; each call does constant work in tree depth.
	enter : Context, MarkupCore.Name -> Try(Context, Problem)
	enter = |context, child| {
		parent_html = html(context.parent)
		if parent_html and MarkupVocabulary.html_void(context.parent.local) {
			return repair(context, Element(child), ChildOfVoid)
		}
		if parent_html and MarkupVocabulary.text_behavior(context.parent.local, context.scripting) != Data {
			return repair(context, Element(child), ElementInTextContent)
		}
		if context.table_form {
			return repair(context, Element(child), InvalidTableChild)
		}
		child_html = html(child)
		name = child.local
		if child_html {
			if name == "form" and context.form_pointer and (!context.in_template or context.mode == Table or context.mode == Section or context.mode == Row) {
				return repair(context, Element(child), NestedForm)
			}
			if name == "a" and context.anchor {
				return repair(context, Element(child), NestedAnchor)
			}
			if name == "button" and context.scopes.button {
				return repair(context, Element(child), NestedButton)
			}
			if name == "li" and context.scopes.li {
				return repair(context, Element(child), CloseListItem)
			}
			if ["dd", "dt"].contains(name) and context.scopes.definition_item {
				return repair(context, Element(child), CloseDescriptionItem)
			}
			if context.scopes.p and Rules.closes_p(name) {
				return repair(context, Element(child), CloseParagraph)
			}
			if parent_html and Rules.heading(context.parent.local) and Rules.heading(name) {
				return repair(context, Element(child), CloseHeading)
			}
			if parent_html and ((context.parent.local == "option" and ["option", "optgroup"].contains(name)) or (context.scopes.select and ["option", "optgroup", "hr"].contains(name) and Rules.implied_end_element(context.parent.local) and !(name == "option" and context.parent.local == "optgroup"))) {
				return repair(context, Element(child), CloseOption)
			}
		}
		match context.mode {
			Body => {
				if child_html and Rules.table_part(name) and !(parent_html and context.parent.local == "template") {
					return repair(context, Element(child), TablePartOutsideTable)
				}
			}
			Colgroup => {
				if !child_html or (name != "col" and name != "template") {
					return repair(context, Element(child), InvalidTableChild)
				}
			}
			Table | Section | Row => {
				if child_html and name == "input" {
					return Err(Unsupported({ parent: context.parent, child, reason: AttributeDependentTableInput }))
				}
				if context.mode == Table and child_html and name == "tr" {
					return repair(context, Element(child), InsertTableSection)
				}
				allowed = child_html and (
					Rules.table_delegated_child(name) or (
						match context.mode {
							Table => Rules.table_regular_child(name)
							Section => name == "tr"
							Row => Rules.table_cell(name)
							_ => False
						}
					)
				)
				if !allowed {
					return repair(context, Element(child), InvalidTableChild)
				}
			}
		}
		Ok(descend(context, child))
	}

	## Check only the new decoded text. Unrestricted contexts do not scan it.
	## HTML void parents reject even an empty text node. Restricted table
	## contexts scan HTML whitespace in O(length of this value), not tree depth.
	text : Context, Str -> Try({}, Problem)
	text = |context, value| {
		if html(context.parent) and MarkupVocabulary.html_void(context.parent.local) {
			return repair(context, Text, ChildOfVoid)
		}
		if context.table_form and !value.is_empty() {
			return repair(context, Text, FosterTableText)
		}
		if html(context.parent) and Rules.restricted_table_parent(context.parent.local) and !Rules.html_whitespace(value) {
			return repair(context, Text, FosterTableText)
		}
		Ok({})
	}
}

# No ancestor, recursive context, node arena, source or attribute list is kept.
ConstructionContext :: { parent : MarkupCore.Name, scripting : Bool, scopes : Rules.Scopes, anchor : Bool, form_pointer : Bool, in_template : Bool, table_form : Bool, mode : [Body, Table, Section, Row, Colgroup] }.{

	## Seed a bounded HTML repair context at an explicit parent.
	Props := { parent : MarkupCore.Name, scripting : [Enabled, Disabled] ?? Enabled }
	from_parent : Props -> ConstructionContext
	from_parent = |props| DomConstruction.root({ parent: props.parent, scripting: props.scripting == Enabled })

	## Check a new child and return its descendant context.
	child : ConstructionContext, MarkupCore.Name -> Try(ConstructionContext, DomConstruction.Problem)
	child = |context, name| DomConstruction.enter(context, name)

	## Check decoded text placement.
	text : ConstructionContext, Str -> Try({}, DomConstruction.Problem)
	text = |context, value| DomConstruction.text(context, value)
}

html = |name| name.namespace == Uri(MarkupCore.html_namespace)

repair = |context, child, rule| Err(Repair({ rule, parent: context.parent, child }))

descend = |context, child| {
	is_html = html(child)
	namespace = match child.namespace {
		Uri(uri) => Rules.namespace_class(uri)
		NoNamespace => OtherNamespace
	}
	mode = if is_html and child.local == "template" {
		Body
	} else if is_html {
		match Rules.table_mode(child.local) {
			Inherit => context.mode
			Body => Body
			Table => Table
			Section => Section
			Row => Row
			Colgroup => Colgroup
		}
	} else context.mode
	ConstructionContext.{ parent: child, scripting: context.scripting, scopes: Rules.descend(context.scopes, namespace, child.local), anchor: if (is_html and Rules.formatting_marker(child.local)) False else context.anchor or (is_html and child.local == "a"), form_pointer: context.form_pointer or (is_html and child.local == "form" and (!context.in_template or context.mode != Body)), in_template: context.in_template or (is_html and child.local == "template"), table_form: is_html and child.local == "form" and context.mode != Body, mode }
}

h = |local| { namespace: Uri(MarkupCore.html_namespace), prefix: Unprefixed, local }

seed = |local| DomConstruction.root({ parent: h(local) })

rule = |context, local| DomConstruction.enter(context, h(local)).map_ok(|_| {}).map_err(
	|problem| match problem {
		Repair(detail) => detail.rule
		Unsupported(_) => crash "unexpected attribute-dependent input"
	},
)

expect rule(seed("p"), "div") == Err(CloseParagraph) and rule(seed("div"), "div").is_ok()
expect rule(seed("table"), "tr") == Err(InsertTableSection) and rule(seed("tbody"), "tr").is_ok()
expect rule(seed("table"), "div") == Err(InvalidTableChild) and rule(seed("table"), "tbody").is_ok()
expect rule(seed("div"), "tr") == Err(TablePartOutsideTable) and rule(seed("tbody"), "tr").is_ok()
expect rule(seed("a"), "a") == Err(NestedAnchor) and rule(seed("span"), "a").is_ok()
expect rule(seed("form"), "form") == Err(NestedForm) and rule(seed("div"), "form").is_ok()
expect rule(seed("button"), "button") == Err(NestedButton) and rule(seed("div"), "button").is_ok()
expect rule(seed("li"), "li") == Err(CloseListItem) and rule(seed("ul"), "li").is_ok()
expect rule(seed("dd"), "dt") == Err(CloseDescriptionItem) and rule(seed("dl"), "dt").is_ok()
expect rule(seed("option"), "option") == Err(CloseOption) and rule(seed("select"), "option").is_ok()
expect rule(seed("h1"), "h2") == Err(CloseHeading) and rule(seed("div"), "h2").is_ok()
expect rule(seed("br"), "span") == Err(ChildOfVoid) and rule(seed("div"), "span").is_ok()
expect rule(seed("textarea"), "span") == Err(ElementInTextContent) and rule(seed("div"), "span").is_ok()
expect DomConstruction.text(seed("table"), "x") == Err(Repair({ rule: FosterTableText, parent: h("table"), child: Text })) and DomConstruction.text(seed("table"), " \t\r\n\u(0C)").is_ok()

expect {
	p = seed("p")
	span = DomConstruction.enter(p, h("span"))?
	button = DomConstruction.enter(p, h("button"))?
	rule(span, "div") == Err(CloseParagraph) and rule(button, "div").is_ok() and rule(p, "div") == Err(CloseParagraph)
}
expect {
	a = seed("a")
	marked = DomConstruction.enter(a, h("object"))?
	button = DomConstruction.enter(a, h("button"))?
	rule(marked, "a").is_ok() and rule(button, "a") == Err(NestedAnchor)
}
expect {
	form = seed("form")
	template = DomConstruction.enter(form, h("template"))?
	inner = DomConstruction.enter(template, h("form"))?
	rule(inner, "form").is_ok() and rule(form, "form") == Err(NestedForm)
}
expect {
	button = seed("button")
	template = DomConstruction.enter(button, h("template"))?
	rule(template, "button").is_ok() and rule(template, "tr").is_ok()
}
expect {
	li = DomConstruction.enter(seed("li"), h("ul"))?
	dd = DomConstruction.enter(seed("dd"), h("dl"))?
	heading = DomConstruction.enter(seed("h1"), h("span"))?
	rule(li, "li").is_ok() and rule(dd, "dt").is_ok() and rule(heading, "h2").is_ok()
}
expect {
	select = seed("select")
	dt = DomConstruction.enter(select, h("dt"))?
	group = DomConstruction.enter(select, h("optgroup"))?
	rule(dt, "option") == Err(CloseOption) and rule(group, "option").is_ok() and rule(group, "optgroup") == Err(CloseOption)
}
expect DomConstruction.enter(seed("table"), h("input")).map_ok(|_| {}) == Err(Unsupported({ parent: h("table"), child: h("input"), reason: AttributeDependentTableInput }))
expect {
	form = DomConstruction.enter(seed("table"), h("form"))?
	rule(form, "span") == Err(InvalidTableChild) and DomConstruction.text(form, " ").is_err()
}
expect DomConstruction.text(seed("br"), "").is_err() and DomConstruction.text(seed("div"), "\u(0)<&").is_ok()
expect rule(DomConstruction.root({ parent: h("noscript"), scripting: False }), "div").is_ok() and rule(seed("noscript"), "div") == Err(ElementInTextContent)
expect {
	parent = { namespace: NoNamespace, prefix: Prefix("raw"), local: "p" }
	custom = { namespace: Uri("urn:extension"), prefix: Prefix("x"), local: "IMG" }
	child = DomConstruction.enter(DomConstruction.root({ parent: parent }), custom)?
	DomConstruction.text(child, "anything").is_ok() and rule(seed("math"), "div").is_ok()
}
expect {
	parent = { namespace: Uri(MarkupCore.svg_namespace), prefix: Prefix("svg"), local: "foreignObject" }
	child = { namespace: Uri(MarkupCore.html_namespace), prefix: Prefix("html"), local: "p" }
	context = DomConstruction.enter(DomConstruction.root({ parent: parent }), child)?
	DomConstruction.enter(context, h("div")).map_ok(|_| {}) == Err(Repair({ rule: CloseParagraph, parent: child, child: Element(h("div")) }))
}

expect {
	outer = seed("a")
	select = DomConstruction.enter(outer, h("select"))?
	rule(select, "a") == Err(NestedAnchor)
}
expect {
	parent = { namespace: NoNamespace, prefix: Prefix("x"), local: "p" }
	child = { namespace: Uri("urn:widgets"), prefix: Prefix("w"), local: "PANEL" }
	context = DomConstruction.enter(DomConstruction.root({ parent: parent }), child)?
	# Return the exact supplied identity in subsequent diagnostics.
	html_child = { namespace: Uri(MarkupCore.html_namespace), prefix: Prefix("h"), local: "br" }
	void = DomConstruction.enter(context, html_child)?
	DomConstruction.enter(void, parent).map_ok(|_| {}) == Err(Repair({ rule: ChildOfVoid, parent: html_child, child: Element(parent) }))
}
expect rule(seed("P"), "div").is_ok() and rule(seed("p"), "DIV").is_ok()
expect {
	context = seed("table")
	DomConstruction.enter(context, h("script")).is_ok() and DomConstruction.enter(context, h("style")).is_ok() and DomConstruction.enter(context, h("template")).is_ok()
}
expect {
	caption = DomConstruction.enter(seed("table"), h("caption"))?
	anchor = DomConstruction.enter(caption, h("a"))?
	inner_table = DomConstruction.enter(anchor, h("table"))?
	section = DomConstruction.enter(inner_table, h("tbody"))?
	row = DomConstruction.enter(section, h("tr"))?
	cell = DomConstruction.enter(row, h("td"))?
	rule(cell, "a").is_ok()
}

expect {
	outer = seed("form")
	template = DomConstruction.enter(outer, h("template"))?
	table = DomConstruction.enter(template, h("table"))?
	rule(table, "form") == Err(NestedForm)
}
expect {
	template = seed("template")
	form = DomConstruction.enter(template, h("form"))?
	table = DomConstruction.enter(form, h("table"))?
	rule(table, "form").is_ok()
}
