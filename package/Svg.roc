import DiagnosticOwnership
import MarkupDiagnosticOwnership
import MarkupValidation
import Parser
import Diagnostic
import SvgPath
import MarkupCore
import MarkupValidate
import MarkupTemplate
import HtmlTemplate
import HtmlTree
import HtmlValidate

## Strict namespace-aware Svg documents and fragments.
## XML syntax is the default; fragments start in the Svg namespace.
## Original bytes and source spans remain available beside the decoded tree.
Svg := { parsed : MarkupCore.Parsed }.{

	Path : SvgPath

	## Common parsing, validation and resource-limit options.
	Props : MarkupCore.Props

	## Source, diagnostic name, and explicit parsing/validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : MarkupCore.Props ?? {} }

	## Parse a complete namespaced document with strict defaults.

	## Parse a complete document with the requested syntax and checks.
	parse_str : Str, Props -> Try(Svg, [InvalidMarkup(MarkupCore.Error)])
	parse_str = |source, props| parse_document(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## Parse any number of fragment roots in the format's default namespace.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(MarkupCore.Error)])
	parse_fragment = |source, props| parse_fragment_source(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## Check a document for quotation or an explicitly asserted imported file.
	validate : ValidationProps -> Try(Svg, [InvalidMarkup(MarkupCore.Error)])
	validate = |props| parse_str(props.source, props.options).map_err(|InvalidMarkup(error)| InvalidMarkup({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) }))

	## Convert a checked complete document literal.
	from_quote : Str -> Try(Svg, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source }).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("Svg", error))

	## Parse fixed literal segments into an indexed plan without binding values.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
	parse_segments = |source, props| parse_template_segments(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## A parsed fragment bound to homogeneous downstream values, including signals.
	Template(a) : SvgTemplateValue(a)

	## A fragment retains every authored root and its exact source.
	Fragment : SvgFragment
}

SvgTemplateValue(a) := { bound : MarkupTemplate.Bound(a) }.{

	## Parse the fixed segments during compilation; bind without reparsing.
	from_interpolation : List(Str) -> Try((Iter(a) -> Svg.Template(a)), [InvalidInterpolation(Str)])
	from_interpolation = |segments| {
		plan = Svg.parse_segments(segments, {}).map_err(|InvalidMarkup(error)| MarkupTemplate.interpolation_error(error))?
		Ok(|values| Svg.Template.{ bound: MarkupTemplate.capture(plan, values) })
	}

	## A static fragment can use the same template type.
	from_quote : Str -> Try(Svg.Template(a), [BadQuotedBytes(Str)])
	from_quote = |source| {
		plan = Svg.parse_segments([source], {}).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("Svg", error))?
		Ok(Svg.Template.{ bound: MarkupTemplate.capture(plan, List.iter([])) })
	}

}

SvgFragment := { parsed : MarkupCore.Parsed }.{

	## Check imported fragment text with explicit options and a diagnostic name.
	validate : Svg.ValidationProps -> Try(Svg.Fragment, [InvalidMarkup(MarkupCore.Error)])
	validate = |props| Svg.parse_fragment(props.source, props.options).map_err(|InvalidMarkup(error)| InvalidMarkup({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) }))

	## Check a fragment literal with strict defaults.
	from_quote : Str -> Try(Svg.Fragment, [BadQuotedBytes(Str)])
	from_quote = |source| Svg.parse_fragment(source, {}).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("Svg", error))

	## Require a single element root for downstream row/component templates.
	single_element : Svg.Fragment -> Try(MarkupCore.Node, [RootCount(U64)])
	single_element = |fragment| {
		index = (fragment.parsed.tree).single_root()?
		Ok(fragment.parsed.tree.nodes.get(index) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
	}
}

expect Svg.parse_str("<svg xmlns='http://www.w3.org/2000/svg'><path d='M0 0L10 10z' pathLength='10'/></svg>", {}).is_ok()
expect Svg.parse_str("<svg/>", {}).is_err()
expect Svg.parse_str("<s:svg xmlns:s='http://www.w3.org/2000/svg'/>", {}).is_ok()
expect Svg.parse_fragment("<path d='M0 0' pathLength='1'/>", {}).is_ok()
expect Svg.parse_fragment("<path pathlength='1'/>", {}).is_err()
expect Svg.parse_fragment("<lineargradient/>", {}).is_err()
expect Svg.parse_fragment("<linearGradient id='g'><stop offset='50%' stop-color='red'/></linearGradient>", {}).is_ok()
expect Svg.parse_fragment("<svg viewBox='0 0 100'/>", {}).is_err()
expect Svg.parse_fragment("<svg viewBox='0 0 -100 100'/>", {}).is_err()
expect Svg.parse_fragment("<svg viewBox='0,0,100,100'/>", {}).is_ok()
expect Svg.parse_fragment("<path d='L0 0'/>", {}).is_err()
expect Svg.parse_fragment("<path d='M0 0L10'/>", {}).is_err()
expect Svg.parse_fragment("<circle r='-1'/>", {}).is_err()
expect Svg.parse_fragment("<circle r='2em'/>", {}).is_ok()
expect Svg.parse_fragment("<circle r='someday'/>", { check_values: False }).is_ok()
expect Svg.parse_fragment("<newShape/>", { allow_unknown_elements: True }).is_ok()
expect Svg.parse_fragment("<path newAttribute='x'/>", { allow_unknown_attributes: True }).is_ok()
expect Svg.parse_fragment("<g><path/></g>", { max_depth: 1 }).is_err()
expect Svg.parse_fragment("<path/>", { max_source_bytes: 2 }).is_err()
expect Svg.parse_fragment("<text x='1em 2px' y='0'>A &amp; B</text>", {}).is_ok()
expect Svg.parse_fragment("<g transform='rotate(20 1)'/>", {}).is_err()
expect Svg.parse_fragment("<g transform='translate(2 3) scale(2)'/>", {}).is_ok()
expect {
	fragment = Svg.parse_fragment("<path/>\n<circle/>", {})?
	fragment.single_element() == Err(RootCount(2))
}
expect {
	fragment = Svg.parse_fragment("<path pathLength='2'/>\r\n", {})?
	fragment.parsed.source.bytes == "<path pathLength='2'/>\r\n".to_utf8()
}

expect {
	label = "<path/>"
	value : Svg.Template(Str)
	value = "<text>${label}</text>"
	value.bound.metadata().values == ["<path/>"] and value.bound.metadata().plan.metadata().slots.len() == 1
}
expect {
	x = 42.U64
	value : Svg.Template(U64)
	value = "<circle cx='${x}' r='2'/>"
	value.bound.metadata().values == [42] and value.bound.metadata().plan.metadata().slots.map(|s| s.expected) == [Length]
}
expect Svg.parse_segments(["<circle r='", "'/>"], {}).is_ok()
expect Svg.parse_segments(["<path ", "='1'/>"], {}).is_err()
expect Svg.parse_segments(["<path d='M0 0 ", "'/>"], {}).is_err()
expect Svg.parse_segments(["<svg xmlns='", "'/>"], {}).is_err()
expect Svg.parse_segments(["<text>&am", "p;</text>"], {}).is_err()

fragment_syntax : Str, MarkupCore.Props -> Try(MarkupCore.Parsed, [InvalidMarkup(MarkupCore.Error)])
fragment_syntax = |source, props| if props.syntax == HtmlSyntax {
	HtmlTree.parse(source, True, { namespace: MarkupCore.svg_namespace, name: "svg", defer_projection: True }, props)
} else {
	MarkupCore.parse_xml_fragment(source, MarkupCore.svg_namespace, props)
}
expect Svg.parse_fragment("<lineargradient viewbox='0 0 10 10'/>", { syntax: HtmlSyntax }).is_err()
expect Svg.parse_fragment("<lineargradient id='g'/>", { syntax: HtmlSyntax }).is_ok()
expect Svg.parse_str("<svg viewbox='0 0 10 10'><path pathlength='1' d='M0 0'/></svg>", { syntax: HtmlSyntax }).is_ok()
expect Svg.parse_fragment("<circle><path/></circle>", {}).is_err()
expect Svg.parse_fragment("<path fill='gibberish'/>", {}).is_err()
expect Svg.parse_fragment("<path fill='url(#gradient) red'/>", {}).is_ok()
expect Svg.parse_fragment("<path fill='#abc8'/>", {}).is_ok()
expect Svg.parse_fragment("<path fill='#xxaa99'/>", {}).is_err()

expect Svg.parse_fragment("<path stroke-width='-1'/>", {}).is_err()
expect Svg.parse_fragment("<path stroke-dasharray='1 2 -3'/>", {}).is_err()
expect Svg.parse_fragment("<path stroke-dasharray='1em,2px'/>", {}).is_ok()
expect Svg.parse_fragment("<filter><feGaussianBlur stdDeviation='-2'/></filter>", {}).is_err()
expect Svg.parse_fragment("<filter><feGaussianBlur stdDeviation='2 3'/></filter>", {}).is_ok()
expect Svg.parse_fragment("<filter><feComposite operator='bogus'/></filter>", {}).is_err()
expect Svg.parse_segments(["<circle>", "</circle>"], {}).is_err()

project_checked = |parsed, props| if props.syntax == HtmlSyntax {
	HtmlTree.project_parsed(parsed, props).map_ok(|projection| projection.parsed)
} else {
	Ok(parsed)
}

expect Svg.parse_fragment("<svg xmlns:x='urn:x' xmlns:r='urn:r'><path d='M0 0'/></svg>", {}).is_ok()

expect {
	p = Svg.parse_segments(["<circle xmlns:x='urn:extension' x:r='", "'></circle>"], { allow_unknown_attributes: True })?
	p.metadata().slots.first()?.expected == Language(NamespacedAttribute)
}

expect {
	p = Svg.parse_segments(["<g r='", "'/>"], { allow_unknown_attributes: True })?
	p.metadata().slots.first()?.expected == Language(ExtensionAttribute)
}

expect match Svg.parse_str("<svg viewbox='0 0 1 1'></svg>", { syntax: HtmlSyntax, strict_foreign_spelling: True }) {
	Err(InvalidMarkup(error)) => match error.problem {
		NonCanonicalName(detail) => detail.authored == "viewbox" and detail.expected == "viewBox"
		_ => False
	}
	_ => False
}
expect Svg.parse_fragment("<linearGradient id='g'/>", { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_ok()
expect Svg.parse_fragment("<lineargradient id='g'/>", { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_err()
expect Svg.parse_segments(["<lineargradient id='", "'/>"], { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_err()
expect Svg.parse_segments(["<linearGradient id='", "'/>"], { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_ok()
expect Svg.parse_str("<svg xmlns='http://www.w3.org/2000/svg' viewbox='0 0 1 1'/>", { strict_foreign_spelling: True, allow_unknown_attributes: True }).is_ok()

parse_document : Str, Svg.Props -> Try(Svg, [InvalidMarkup(MarkupCore.Error)])
parse_document = |source, props| {
	parsed = if props.syntax == HtmlSyntax {
		HtmlTree.parse(source, True, { namespace: MarkupCore.html_namespace, name: "body", defer_projection: True }, props)?
	} else {
		MarkupCore.parse_xml(source, props)?
	}
	index = (parsed.tree).single_root().map_err(|_| InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.svg_namespace), local: "svg" }, actual: Fragment }), location: LineColumn({ line: 1, column: 1 }), path: [] }))?
	node = parsed.tree.nodes.get(index) ?? return Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(MissingRoot), location: LineColumn({ line: 1, column: 1 }), path: [] }))
	match node.kind {
		Element(name) if name.namespace == Uri(MarkupCore.svg_namespace) and name.local == "svg" => {}
		Element(name) => return Err(InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.svg_namespace), local: "svg" }, actual: Element(name) }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
		_ => return Err(InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.svg_namespace), local: "svg" }, actual: Text }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
	}
	validated = MarkupValidation.check(parsed, props, [])?
	Ok(Svg.{ parsed: project_checked(validated, props)? })
}

parse_fragment_source : Str, Svg.Props -> Try(Svg.Fragment, [InvalidMarkup(MarkupCore.Error)])
parse_fragment_source = |source, props| {
	parsed = fragment_syntax(source, props)?
	validated = MarkupValidation.check(parsed, props, [])?
	Ok(SvgFragment.{ parsed: project_checked(validated, props)? })
}

parse_template_segments : List(Str), Svg.Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
parse_template_segments = |segments, props| {
	if props.syntax == HtmlSyntax {
		return HtmlTemplate.parse_segments(segments, { name: "svg", namespace: MarkupCore.svg_namespace }, props)
	}
	plan = MarkupTemplate.parse_segments_with(segments, props, |source| fragment_syntax(source, props), MarkupCore.decode_xml_text)?
	_ = MarkupValidate.text_slots(plan)?
	var $holes = []
	for slot in plan.metadata().slots {
		match slot.target {
			AttributeSlot(target) => {
				$holes = $holes.append(target)
			}
			_ => {}
		}
	}
	parsed = MarkupValidation.check(plan.metadata().parsed, props, $holes)?
	slots = plan.metadata().slots.map(
		|slot| {
			match slot.target {
				AttributeSlot(target) => {
					node = parsed.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
					match (node.kind, node.attributes.get(target.attribute)) {
						(Element(name), Ok(attr)) => { ..slot, expected: if (attr.name.namespace != NoNamespace) Language(NamespacedAttribute) else if (name.namespace == Uri(MarkupCore.html_namespace)) HtmlValidate.grammar(name.local, attr.name.local) else MarkupValidate.grammar(name.namespace == Uri(MarkupCore.svg_namespace), name.local, attr.name.local) }
						_ => slot
					}
				}
				_ => slot
			}
		},
	)
	Ok(MarkupTemplate.rebuild({ ..plan.metadata(), parsed, slots }))
}
