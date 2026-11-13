import Markup
import MarkupValidate
import MarkupTemplate
import Literal
import Html
import HtmlTokenizer

## Strict namespace-aware MathML documents and fragments.
## XML syntax is the default; fragments start in the MathML namespace.
## Original bytes and source spans remain available beside the decoded tree.
MathML := { parsed : Markup.Parsed }.{

	## Common parsing, validation and resource-limit options.
	Props : Markup.Props

	## Source, diagnostic name, and explicit parsing/validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Parse a complete namespaced document with strict defaults.
	parse_str : Str -> Try(MathML, [InvalidMarkup(Markup.Error)])
	parse_str = |source| parse_with(source, {})

	## Parse a complete document with the requested syntax and checks.
	parse_with : Str, Props -> Try(MathML, [InvalidMarkup(Markup.Error)])
	parse_with = |source, props| {
		parsed = if props.syntax == HtmlSyntax {
			Html.parse_syntax(source, props, True, { namespace: Uri(Markup.html_namespace), local: "body", prefix: Unprefixed })?
		} else {
			Markup.parse_xml(source, props)?
		}
		index = Markup.single_root(parsed.tree).map_err(|_| InvalidMarkup({ problem: WrongRoot({ expected: "math", actual: "fragment" }), line: 1, column: 1 }))?
		node = parsed.tree.nodes.get(index) ?? return Err(InvalidMarkup({ problem: Unsupported("missing root"), line: 1, column: 1 }))
		match node.kind {
			Element(name) if name.namespace == Uri(Markup.mathml_namespace) and name.local == "math" => {}
			Element(name) => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "math in the MathML namespace", actual: name.local }), line: 1, column: 1 }))
			_ => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "math", actual: "text" }), line: 1, column: 1 }))
		}
		validated = validate_tree(parsed, props, [])?
		Ok(MathML.{ parsed: validated })
	}

	## Parse any number of fragment roots in the format's default namespace.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(Markup.Error)])
	parse_fragment = |source, props| {
		parsed = fragment_syntax(source, props)?
		validated = validate_tree(parsed, props, [])?
		Ok(Fragment.{ parsed: validated })
	}

	## Validate fixed markup, skipping unavailable whole attribute holes.
	validate_tree : Markup.Parsed, Props, List(MarkupValidate.Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	validate_tree = Html.validate_tree

	## Check a document for quotation or an explicitly asserted imported file.
	validate : ValidationProps -> Try(MathML, [BadQuotedBytes(Str)])
	validate = |props| parse_with(props.source, props.options).map_err(|InvalidMarkup(error)| Literal.error({ name: props.name, format: "MathML", message: Str.inspect(error.problem), line: error.line, column: error.column }))

	## Convert a checked complete document literal.
	from_quote : Str -> Try(MathML, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## Parse fixed literal segments into an indexed plan without binding values.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, props| {
		plan = MarkupTemplate.parse_segments_with(segments, props, |source| fragment_syntax(source, props), |text| if (props.syntax == HtmlSyntax) Ok(HtmlTokenizer.normalized(text.to_utf8(), False, False).text) else Markup.decode_xml_text(text))?
		_ = MarkupValidate.text_slots(plan)?
		var $holes = []
		for slot in plan.slots {
			match slot.target {
				AttributeSlot(target) => {
					$holes = $holes.append(target)
				}
				_ => {}
			}
		}
		parsed = validate_tree(plan.parsed, props, $holes)?
		slots = plan.slots.map(
			|slot| {
				match slot.target {
					AttributeSlot(target) => {
						node = parsed.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
						match (node.kind, node.attributes.get(target.attribute)) {
							(Element(name), Ok(attr)) => { ..slot, expected: MarkupValidate.grammar(name.namespace == Uri(Markup.svg_namespace), name.local, attr.name.local) }
							_ => slot
						}
					}
					_ => slot
				}
			},
		)
		Ok({ ..plan, parsed, slots })
	}

	## A parsed fragment bound to homogeneous downstream values, including signals.
	Template(a) := { bound : MarkupTemplate.Bound(a) }.{

		## Parse the fixed segments during compilation; bind without reparsing.
		from_interpolation : List(Str) -> Try((Iter(a) -> Template(a)), [InvalidInterpolation(Str)])
		from_interpolation = |segments| {
			plan = MathML.parse_segments(segments, {}).map_err(|InvalidMarkup(error)| MarkupTemplate.interpolation_error(error))?
			Ok(|values| Template.{ bound: MarkupTemplate.capture(plan, values) })
		}

		## A static fragment can use the same template type.
		from_quote : Str -> Try(Template(a), [BadQuotedBytes(Str)])
		from_quote = |source| {
			plan = MathML.parse_segments([source], {}).map_err(|InvalidMarkup(error)| Markup.quoted_error("MathML", error))?
			Ok(Template.{ bound: MarkupTemplate.capture(plan, List.iter([])) })
		}

		## Fallibly bind an explicitly parsed plan, checking the binding count.
		bind : MarkupTemplate.Plan, Iter(a) -> Try(Template(a), [InvalidMarkup(Markup.Error)])
		bind = |plan, values| MarkupTemplate.bind(plan, values).map_ok(|bound| Template.{ bound })
	}

	## A fragment retains every authored root and its exact source.
	Fragment := { parsed : Markup.Parsed }.{

		## Check imported fragment text with explicit options and a diagnostic name.
		validate : MathML.ValidationProps -> Try(Fragment, [BadQuotedBytes(Str)])
		validate = |props| MathML.parse_fragment(props.source, props.options).map_err(|InvalidMarkup(error)| Literal.error({ name: props.name, format: "MathML", message: Str.inspect(error.problem), line: error.line, column: error.column }))

		## Check a fragment literal with strict defaults.
		from_quote : Str -> Try(Fragment, [BadQuotedBytes(Str)])
		from_quote = |source| MathML.parse_fragment(source, {}).map_err(|InvalidMarkup(error)| Markup.quoted_error("MathML", error))

		## Require a single element root for downstream row/component templates.
		single_element : Fragment -> Try(Markup.Node, [RootCount(U64)])
		single_element = |fragment| {
			index = Markup.single_root(fragment.parsed.tree)?
			Ok(fragment.parsed.tree.nodes.get(index) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
		}
	}
}

expect MathML.parse_str("<math xmlns='http://www.w3.org/1998/Math/MathML'><mi>x</mi></math>").is_ok()
expect MathML.parse_str("<math/>").is_err()
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
	value.bound.values == [x] and value.bound.plan.slots.len() == 1
}
expect MathML.parse_segments(["<mo stretchy='", "'>+</mo>"], {}).is_ok()

fragment_syntax : Str, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
fragment_syntax = |source, props| if props.syntax == HtmlSyntax {
	Html.parse_syntax(source, props, True, { namespace: Uri(Markup.mathml_namespace), local: "math", prefix: Unprefixed })
} else {
	Markup.parse_xml_fragment(source, Markup.mathml_namespace, props)
}
expect MathML.parse_with("<math><mfrac><mn>1</mn><mn>2</mn></mfrac></math>", { syntax: HtmlSyntax }).is_ok()
expect MathML.parse_fragment("<mtext><span>HTML</span></mtext>", { syntax: HtmlSyntax }).is_ok()

expect MathML.parse_fragment("<mmultiscripts/>", {}).is_err()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mi>i</mi></mmultiscripts>", {}).is_err()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mi>i</mi><mn>2</mn><mprescripts/><mi>j</mi><mn>3</mn></mmultiscripts>", {}).is_ok()
expect MathML.parse_fragment("<mmultiscripts><mi>x</mi><mprescripts/><mi>i</mi></mmultiscripts>", {}).is_err()
expect MathML.parse_fragment("<mfrac linethickness='-2px'><mn>1</mn><mn>2</mn></mfrac>", {}).is_err()
expect MathML.parse_segments(["<mfrac>", "</mfrac>"], {}).is_err()
