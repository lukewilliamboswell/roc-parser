import Markup
import MarkupTemplate
import MarkupValidate
import HtmlValidate
import HtmlTree
import HtmlTokenizer
import Literal

## HTML documents, fragments and typed literal templates. Strict HTML syntax and
## authoring checks are the default. Browser recovery is opt-in and retains typed
## diagnostics beside the repaired tree and exact original source bytes.
Html := { parsed : Markup.Parsed }.{

	## HTML defaults and explicit XML/recovery/validation/fragment-context options.
	Props := { syntax : Markup.Syntax ?? HtmlSyntax, recovery : [Strict, Browser] ?? Strict, allow_unknown_elements : Bool ?? False, allow_unknown_attributes : Bool ?? False, check_values : Bool ?? True, max_source_bytes : U64 ?? 16_777_216, max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_nodes : U64 ?? 4_194_304, max_slots : U64 ?? 65_536, max_attributes : U64 ?? 1024, max_diagnostics : U64 ?? 1024, max_work_per_byte : U64 ?? 256, context_namespace : Str ?? "http://www.w3.org/1999/xhtml", context_element : Str ?? "body", scripting : Bool ?? True }

	## Source and diagnostics label for checked imported constants.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Parse a complete HTML document with strict defaults.
	parse_str : Str -> Try(Html, [InvalidMarkup(Markup.Error)])
	parse_str = |text| parse_with(text, {})

	## Parse and validate a document, retaining recovery diagnostics when enabled.
	parse_with : Str, Props -> Try(Html, [InvalidMarkup(Markup.Error)])
	parse_with = |text, props| {
		parsed = syntax(text, props, False)?
		root = Markup.single_root(parsed.tree).map_err(|_| InvalidMarkup({ problem: WrongRoot({ expected: "html", actual: "fragment" }), line: 1, column: 1 }))?
		node = parsed.tree.nodes.get(root) ?? return Err(InvalidMarkup({ problem: Unsupported("missing document root"), line: 1, column: 1 }))
		match node.kind {
			Element(n) if n.namespace == Uri(Markup.html_namespace) and n.local == "html" => {}
			_ => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "html in the HTML namespace", actual: "other" }), line: 1, column: 1 }))
		}
		checked = validate_tree(parsed, shared(props), [])?
		Ok(Html.{ parsed: checked })
	}

	## Parse one or more roots in the configured fragment context.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(Markup.Error)])
	parse_fragment = |text, props| {
		parsed = syntax(text, props, True)?
		checked = validate_tree(parsed, shared(props), [])?
		Ok(Fragment.{ parsed: checked })
	}

	## Syntax-only parsing with all HTML options, including the scripting flag
	## and fragment context. No scripts are executed by this pure parser.
	parse_source : Str, Props, Bool -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	parse_source = syntax

	## Syntax-only entrypoint used by mixed-format frontends and independent
	## tree-construction oracles. This does not perform vocabulary/value checks.
	parse_syntax : Str, Markup.Props, Bool, Markup.Name -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	parse_syntax = |text, props, fragment, context| {
		if props.syntax == XmlSyntax {
			if fragment {
				Markup.parse_xml_fragment(text, namespace_uri(context), props)
			} else {
				Markup.parse_xml(text, props)
			}
		} else {
			HtmlTree.parse(text, fragment, { name: context.local, namespace: namespace_uri(context) }, props)
		}
	}

	## Validate mixed markup, skipping the future values of typed attribute holes.
	validate_tree : Markup.Parsed, Markup.Props, List(MarkupValidate.Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	validate_tree = |parsed, props, holes| {
		foreign = MarkupValidate.foreign(parsed, props, holes)?
		HtmlValidate.validate(foreign, props, holes)
	}

	## Validate a complete document for a checked constant or imported file.
	validate : ValidationProps -> Try(Html, [BadQuotedBytes(Str)])
	validate = |props| parse_with(props.source, props.options).map_err(|InvalidMarkup(e)| Literal.error({ name: props.name, format: "HTML", message: Str.inspect(e.problem), line: e.line, column: e.column }))

	## Check a complete HTML literal during compilation.
	from_quote : Str -> Try(Html, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## A fragment preserves all authored roots and source bytes.
	Fragment := { parsed : Markup.Parsed }.{

		## Check a fragment literal in the default body context.
		from_quote : Str -> Try(Fragment, [BadQuotedBytes(Str)])
		from_quote = |text| Html.parse_fragment(text, {}).map_err(|InvalidMarkup(e)| Markup.quoted_error("HTML", e))

		## Require exactly one element root, ignoring comments/whitespace.
		single_element : Fragment -> Try(Markup.Node, [RootCount(U64)])
		single_element = |fragment| {
			root = Markup.single_root(fragment.parsed.tree)?
			Ok(fragment.parsed.tree.nodes.get(root) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
		}
	}

	## Parse fixed segments and explicit text/whole-attribute holes. No textual
	## sentinels are inserted, and structural positions cannot contain holes.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, props| {
		plan = MarkupTemplate.parse_segments_with(segments, shared(props), |text| syntax(text, props, True), |text| Ok(HtmlTokenizer.normalized(text.to_utf8(), True, False).text))?
		_ = MarkupValidate.text_slots(plan)?
		holes = plan.slots.keep_oks(
			|slot| match slot.target {
				AttributeSlot(target) => Ok(target)
				_ => Err(Skip)
			},
		)
		checked = validate_tree(plan.parsed, shared(props), holes)?
		slots = plan.slots.map(
			|slot| match slot.target {
				AttributeSlot(target) => {
					node = checked.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
					attr = node.attributes.get(target.attribute)
					element = match node.kind {
						Element(n) => n
						_ => { local: "", namespace: NoNamespace, prefix: Unprefixed }
					}
					match attr {
						Ok(a) => { ..slot, expected: if (element.namespace == Uri(Markup.svg_namespace)) MarkupValidate.grammar(True, element.local, a.name.local) else if (element.namespace == Uri(Markup.mathml_namespace)) MarkupValidate.grammar(False, element.local, a.name.local) else HtmlValidate.grammar(element.local, a.name.local) }
						_ => slot
					}
				}
				_ => slot
			},
		)
		Ok({ ..plan, parsed: checked, slots })
	}

	## A parsed template and its typed bindings
	## Binding performs no source parse.
	Template(a) := { bound : MarkupTemplate.Bound(a) }.{

		## Bind an explicit iterator to an existing plan without reparsing.
		bind : MarkupTemplate.Plan, Iter(a) -> Try(Template(a), [InvalidMarkup(Markup.Error)])
		bind = |plan, values| MarkupTemplate.bind(plan, values).map_ok(|bound| Template.{ bound })

		## Parse a static fragment as a template with no holes.
		from_quote : Str -> Try(Template(a), [BadQuotedBytes(Str)])
		from_quote = |source| Html.parse_segments([source], {}).map_ok(|plan| Template.{ bound: MarkupTemplate.capture(plan, [].iter()) }).map_err(|InvalidMarkup(e)| Markup.quoted_error("HTML", e))

		## Validate fixed segments at compile time and capture arbitrary typed
		## bindings through the compiler's Iter-based interpolation assembler.
		from_interpolation : List(Str) -> Try((Iter(a) -> Template(a)), [InvalidInterpolation(Str)])
		from_interpolation = |segments| Html.parse_segments(segments, {}).map_ok(|plan| |values| Template.{ bound: MarkupTemplate.capture(plan, values) }).map_err(|InvalidMarkup(e)| MarkupTemplate.interpolation_error(e))
	}
}

namespace_uri = |name| match name.namespace {
	Uri(uri) => uri
	NoNamespace => ""
}

syntax = |text, props, fragment| {
	if props.syntax == HtmlSyntax {
		HtmlTree.parse(text, fragment, { name: props.context_element, namespace: props.context_namespace, scripting: props.scripting }, shared(props))
	}
	else {
		Html.parse_syntax(text, shared(props), fragment, { local: props.context_element, namespace: Uri(props.context_namespace), prefix: Unprefixed })
	}
}

shared : Html.Props -> Markup.Props
shared = |p| { syntax: p.syntax, recovery: p.recovery, allow_unknown_elements: p.allow_unknown_elements, allow_unknown_attributes: p.allow_unknown_attributes, check_values: p.check_values, max_source_bytes: p.max_source_bytes, max_depth: p.max_depth, max_elements: p.max_elements, max_nodes: p.max_nodes, max_slots: p.max_slots, max_attributes: p.max_attributes, max_diagnostics: p.max_diagnostics, max_work_per_byte: p.max_work_per_byte }

expect Html.parse_str("<!doctype html><html lang='en'><head><title>Example</title></head><body><p>Hello &amp; welcome</p></body></html>").is_ok()
expect Html.parse_fragment("<input type='CHECKBOX' checked><span aria-pressed='false'>x</span>", {}).is_ok()
expect Html.parse_fragment("<div hidden='false'></div>", {}).is_err()
expect Html.parse_fragment("<div hidden='until-found'></div>", {}).is_ok()
expect Html.parse_fragment("<button disabled='false'>x</button>", {}).is_err()
expect Html.parse_fragment("<p><img src='x' alt=''></p>", {}).is_ok()
expect Html.parse_fragment("<p><img src='x'></p>", {}).is_err()
expect Html.parse_fragment("<madeup/>", {}).is_err()
expect Html.parse_fragment("<x-widget custom='yes'></x-widget>", {}).is_ok()
expect Html.parse_fragment("<span custom='yes'></span>", { allow_unknown_attributes: True }).is_ok()
expect Html.parse_fragment("<span aria-presed='false'></span>", {}).is_err()
expect Html.parse_fragment("<table><tr><td>x</table>", {}).is_ok()
expect Html.parse_fragment("<circle r='2'/>", { context_namespace: Markup.svg_namespace, context_element: "svg" }).is_ok()
expect Html.parse_fragment("<div xmlns='http://www.w3.org/1999/xhtml'/>", { syntax: XmlSyntax }).is_ok()
expect Html.parse_fragment("<input checked>", { syntax: XmlSyntax }).is_err()
expect Html.parse_segments(["<button disabled='", "'>", "</button>"], {}).map_ok(|p| p.slots.len()) == Ok(2)
expect Html.parse_segments(["<", "></div>"], {}).is_err()
expect {
	fragment : Html.Fragment
	fragment = "<p>checked</p>"
	fragment.single_element().is_ok()
}
expect {
	template : Html.Template(U64)
	template = "<p>${42.U64}</p>"
	template.bound.values == [42]
}

expect Html.parse_fragment("<input disabled checked=\"\"><br><hr>", {}).is_ok()
expect {
	p = Html.parse_segments(["<svg><circle r='", "'/></svg>"], {})?
	p.slots.first()?.expected == NonnegativeLength
}
expect Html.parse_segments(["<span>", "</span>"], { max_slots: 0 }).is_err()
expect Html.parse_with("<html/>", { max_diagnostics: 0 }).is_err()

expect {
	f = Html.parse_fragment("<unknown thing='x'></unknown>", { allow_unknown_elements: True, allow_unknown_attributes: True })?
	f.parsed.diagnostics.len() == 2
}
expect Html.parse_fragment("<button><input></button>", {}).is_err()
expect Html.parse_fragment("<button><input type='hidden'></button>", {}).is_ok()
