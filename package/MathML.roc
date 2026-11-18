import DiagnosticOwnership
import MarkupDiagnosticOwnership
import MarkupValidation
import Parser
import Diagnostic
import MarkupCore
import MarkupValidate
import MarkupTemplate
import HtmlTemplate
import HtmlTree
import HtmlValidate

## Strict namespace-aware MathML documents and fragments.
## XML syntax is the default; fragments start in the MathML namespace.
## Original bytes and source spans remain available beside the decoded tree.
MathML := { parsed : MarkupCore.Parsed }.{

	## Common parsing, validation and resource-limit options.
	Props : MarkupCore.Props

	## Source, diagnostic name, and explicit parsing/validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Parse a complete namespaced document with strict defaults.

	## Parse a complete document with the requested syntax and checks.
	parse_str : Str, Props -> Try(MathML, [InvalidMarkup(MarkupCore.Error)])
	parse_str = |source, props| parse_document(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## Parse any number of fragment roots in the format's default namespace.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(MarkupCore.Error)])
	parse_fragment = |source, props| parse_fragment_source(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## Check a document for quotation or an explicitly asserted imported file.
	validate : ValidationProps -> Try(MathML, [InvalidMarkup(MarkupCore.Error)])
	validate = |props| parse_str(props.source, props.options).map_err(|InvalidMarkup(error)| InvalidMarkup({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) }))

	## Convert a checked complete document literal.
	from_quote : Str -> Try(MathML, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source }).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("MathML", error))

	## Parse fixed literal segments into an indexed plan without binding values.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
	parse_segments = |source, props| parse_template_segments(source, props).map_err(MarkupDiagnosticOwnership.failure)

	## A parsed fragment bound to homogeneous downstream values, including signals.
	Template(a) : MathmlTemplateValue(a)

	## A fragment retains every authored root and its exact source.
	Fragment : MathmlFragment
}

MathmlTemplateValue(a) := { bound : MarkupTemplate.Bound(a) }.{

	## Parse the fixed segments during compilation; bind without reparsing.
	from_interpolation : List(Str) -> Try((Iter(a) -> MathML.Template(a)), [InvalidInterpolation(Str)])
	from_interpolation = |segments| {
		plan = MathML.parse_segments(segments, {}).map_err(|InvalidMarkup(error)| MarkupTemplate.interpolation_error(error))?
		Ok(|values| MathML.Template.{ bound: MarkupTemplate.capture(plan, values) })
	}

	## A static fragment can use the same template type.
	from_quote : Str -> Try(MathML.Template(a), [BadQuotedBytes(Str)])
	from_quote = |source| {
		plan = MathML.parse_segments([source], {}).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("MathML", error))?
		Ok(MathML.Template.{ bound: MarkupTemplate.capture(plan, List.iter([])) })
	}

}

MathmlFragment := { parsed : MarkupCore.Parsed }.{

	## Check imported fragment text with explicit options and a diagnostic name.
	validate : MathML.ValidationProps -> Try(MathML.Fragment, [InvalidMarkup(MarkupCore.Error)])
	validate = |props| MathML.parse_fragment(props.source, props.options).map_err(|InvalidMarkup(error)| InvalidMarkup({ ..error, path: error.path.append(Source(DiagnosticOwnership.own_text(props.name))) }))

	## Check a fragment literal with strict defaults.
	from_quote : Str -> Try(MathML.Fragment, [BadQuotedBytes(Str)])
	from_quote = |source| MathML.parse_fragment(source, {}).map_err(|InvalidMarkup(error)| MarkupCore.quoted_error("MathML", error))

	## Require a single element root for downstream row/component templates.
	single_element : MathML.Fragment -> Try(MarkupCore.Node, [RootCount(U64)])
	single_element = |fragment| {
		index = (fragment.parsed.tree).single_root()?
		Ok(fragment.parsed.tree.nodes.get(index) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
	}
}

expect MathML.parse_str("<math xmlns='http://www.w3.org/1998/Math/MathML'><mi>x</mi></math>", {}).is_ok()
expect MathML.parse_str("<math/>", {}).is_err()
expect MathML.parse_fragment("<mfrac><mn>1</mn><mn>2</mn></mfrac>", {}).is_ok()
expect MathML.parse_fragment("<mfrac><mn>1</mn></mfrac>", {}).is_err()
expect MathML.parse_fragment("<mfrac>x<mn>1</mn><mn>2</mn></mfrac>", {}).is_err()
expect MathML.parse_fragment("<mroot><mi>x</mi><mn>3</mn></mroot>", {}).is_ok()
expect MathML.parse_fragment("<msubsup><mi>x</mi><mi>i</mi></msubsup>", {}).is_err()
expect MathML.parse_fragment("<msup><mi>x</mi><mn>2</mn></msup>", {}).is_ok()
expect MathML.parse_fragment("<mi><mn>2</mn></mi>", {}).is_err()
expect MathML.parse_fragment("<mtable><mtr><mtd><mi>x</mi></mtd></mtr></mtable>", {}).is_ok()
expect MathML.parse_fragment("<mtable><mi>x</mi></mtable>", {}).is_err()
expect MathML.parse_fragment("<mo stretchy='false'>+</mo>", {}).is_ok()
expect MathML.parse_fragment("<mo stretchy='yes'>+</mo>", {}).is_err()
expect MathML.parse_fragment("<math display='fullscreen'/>", {}).is_err()
expect MathML.parse_fragment("<apply><plus/><ci>x</ci></apply>", { allow_unknown_elements: True }).is_ok()
expect MathML.parse_fragment("<apply/>", {}).is_err()
expect MathML.parse_fragment("<mspace width='2em' height='3px'/>", {}).is_ok()
expect MathML.parse_fragment("<mtd rowspan='0'/>", {}).is_err()
expect MathML.parse_fragment("<mi>α</mi><mo>+</mo><mi>β</mi>", {}).is_ok()
expect MathML.parse_fragment("<semantics><mi>x</mi><annotation encoding='application/x-tex'>x</annotation></semantics>", {}).is_ok()

expect {
	x = "<mi>markup stays text</mi>"
	value : MathML.Template(Str)
	value = "<mrow><mi>${x}</mi><mo>+</mo><mn>1</mn></mrow>"
	value.bound.metadata().values == [x] and value.bound.metadata().plan.metadata().slots.len() == 1
}
expect MathML.parse_segments(["<mo stretchy='", "'>+</mo>"], {}).is_ok()

fragment_syntax : Str, MarkupCore.Props -> Try(MarkupCore.Parsed, [InvalidMarkup(MarkupCore.Error)])
fragment_syntax = |source, props| if props.syntax == HtmlSyntax {
	HtmlTree.parse(source, True, { namespace: MarkupCore.mathml_namespace, name: "math", defer_projection: True }, props)
} else {
	MarkupCore.parse_xml_fragment(source, MarkupCore.mathml_namespace, props)
}
expect MathML.parse_str("<math><mfrac><mn>1</mn><mn>2</mn></mfrac></math>", { syntax: HtmlSyntax }).is_ok()
expect MathML.parse_fragment("<mtext><span>HTML</span></mtext>", { syntax: HtmlSyntax }).is_ok()

expect MathML.parse_fragment("<mmultiscripts/>", {}).is_err()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mi>i</mi></mmultiscripts>", {}).is_err()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mi>i</mi><mn>2</mn><mprescripts/><mi>j</mi><mn>3</mn></mmultiscripts>", {}).is_ok()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mprescripts/><mi>i</mi></mmultiscripts>", {}).is_err()
expect MathML.parse_fragment("<mfrac linethickness='-2px'><mn>1</mn><mn>2</mn></mfrac>", {}).is_err()
expect MathML.parse_segments(["<mfrac>", "</mfrac>"], {}).is_err()

expect MathML.parse_fragment("<semantics><annotation encoding='text/plain'>x</annotation></semantics>", {}).is_err()

project_checked = |parsed, props| if props.syntax == HtmlSyntax {
	HtmlTree.project_parsed(parsed, props).map_ok(|projection| projection.parsed)
} else {
	Ok(parsed)
}

expect MathML.parse_fragment("<math xmlns:display='urn:metadata'><mi>x</mi></math>", {}).is_ok()

expect {
	p = MathML.parse_segments(["<mi xmlns:x='urn:extension' x:r='", "'>x</mi>"], { allow_unknown_attributes: True })?
	p.metadata().slots.first()?.expected == Language(NamespacedAttribute)
}

expect match MathML.parse_fragment("<annotation definitionurl='x'>x</annotation>", { syntax: HtmlSyntax, strict_foreign_spelling: True }) {
	Err(InvalidMarkup(error)) => match error.problem {
		NonCanonicalName(detail) => detail.authored == "definitionurl" and detail.expected == "definitionURL"
		_ => False
	}
	_ => False
}
expect MathML.parse_fragment("<annotation definitionURL='x'>x</annotation>", { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_ok()
expect MathML.parse_segments(["<annotation definitionurl='", "'>x</annotation>"], { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_err()
expect MathML.parse_segments(["<annotation definitionURL='", "'>x</annotation>"], { syntax: HtmlSyntax, strict_foreign_spelling: True }).is_ok()

parse_document : Str, MathML.Props -> Try(MathML, [InvalidMarkup(MarkupCore.Error)])
parse_document = |source, props| {
	parsed = if props.syntax == HtmlSyntax {
		HtmlTree.parse(source, True, { namespace: MarkupCore.html_namespace, name: "body", defer_projection: True }, props)?
	} else {
		MarkupCore.parse_xml(source, props)?
	}
	index = (parsed.tree).single_root().map_err(|_| InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.mathml_namespace), local: "math" }, actual: Fragment }), location: LineColumn({ line: 1, column: 1 }), path: [] }))?
	node = parsed.tree.nodes.get(index) ?? return Err(InvalidMarkup(Diagnostic.{ problem: Unsupported(MissingRoot), location: LineColumn({ line: 1, column: 1 }), path: [] }))
	match node.kind {
		Element(name) if name.namespace == Uri(MarkupCore.mathml_namespace) and name.local == "math" => {}
		Element(name) => return Err(InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.mathml_namespace), local: "math" }, actual: Element(name) }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
		_ => return Err(InvalidMarkup(Diagnostic.{ problem: WrongRoot({ expected: { namespace: Uri(MarkupCore.mathml_namespace), local: "math" }, actual: Text }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
	}
	validated = MarkupValidation.check(parsed, props, [])?
	Ok(MathML.{ parsed: project_checked(validated, props)? })
}

parse_fragment_source : Str, MathML.Props -> Try(MathML.Fragment, [InvalidMarkup(MarkupCore.Error)])
parse_fragment_source = |source, props| {
	parsed = fragment_syntax(source, props)?
	validated = MarkupValidation.check(parsed, props, [])?
	Ok(MathmlFragment.{ parsed: project_checked(validated, props)? })
}

parse_template_segments : List(Str), MathML.Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
parse_template_segments = |segments, props| {
	if props.syntax == HtmlSyntax {
		return HtmlTemplate.parse_segments(segments, { name: "math", namespace: MarkupCore.mathml_namespace }, props)
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
