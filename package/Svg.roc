import Markup
import MarkupValidate
import MarkupTemplate

## Strict namespace-aware Svg documents and fragments.
## XML syntax is the default; fragments start in the Svg namespace.
## Original bytes and source spans remain available beside the decoded tree.
Svg := { parsed : Markup.Parsed }.{

	## Common parsing, validation and resource-limit options.
	Props : Markup.Props

	## Source, diagnostic name, and explicit parsing/validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Parse a complete namespaced document with strict defaults.
	parse_str : Str -> Try(Svg, [InvalidMarkup(Markup.Error)])
	parse_str = |source| parse_with(source, {})

	## Parse a complete document with the requested syntax and checks.
	parse_with : Str, Props -> Try(Svg, [InvalidMarkup(Markup.Error)])
	parse_with = |source, props| {
		parsed = Markup.parse_xml(source, props)?
		index = Markup.single_root(parsed.tree).map_err(|_| InvalidMarkup({ problem: WrongRoot({ expected: "svg", actual: "fragment" }), line: 1, column: 1 }))?
		node = parsed.tree.nodes.get(index) ?? return Err(InvalidMarkup({ problem: Unsupported("missing root"), line: 1, column: 1 }))
		match node.kind {
			Element(name) if name.namespace == Uri(Markup.svg_namespace) and name.local == "svg" => {}
			Element(name) => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "svg in the Svg namespace", actual: name.local }), line: 1, column: 1 }))
			_ => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "svg", actual: "text" }), line: 1, column: 1 }))
		}
		validated = validate_tree(parsed, props, [])?
		Ok(Svg.{ parsed: validated })
	}

	## Parse any number of fragment roots in the format's default namespace.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(Markup.Error)])
	parse_fragment = |source, props| {
		parsed = Markup.parse_xml_fragment(source, Markup.svg_namespace, props)?
		validated = validate_tree(parsed, props, [])?
		Ok(Fragment.{ parsed: validated })
	}

	## Validate fixed markup, skipping unavailable whole attribute holes.
	validate_tree : Markup.Parsed, Props, List(MarkupValidate.Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	validate_tree = MarkupValidate.foreign

	## Check a document for quotation or an explicitly asserted imported file.
	validate : ValidationProps -> Try(Svg, [BadQuotedBytes(Str)])
	validate = |props| parse_with(props.source, props.options).map_err(|InvalidMarkup(error)| Markup.quoted_error("Svg", error))

	## Convert a checked complete document literal.
	from_quote : Str -> Try(Svg, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## Parse fixed literal segments into an indexed plan without binding values.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, props| {
		plan = MarkupTemplate.parse_segments(segments, |source| Markup.parse_xml_fragment(source, Markup.svg_namespace, props), Markup.decode_xml_text)?
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
			plan = Svg.parse_segments(segments, {}).map_err(|InvalidMarkup(error)| MarkupTemplate.interpolation_error(error))?
			Ok(|values| Template.{ bound: MarkupTemplate.capture(plan, values) })
		}

		## A static fragment can use the same template type.
		from_quote : Str -> Try(Template(a), [BadQuotedBytes(Str)])
		from_quote = |source| {
			plan = Svg.parse_segments([source], {}).map_err(|InvalidMarkup(error)| Markup.quoted_error("Svg", error))?
			Ok(Template.{ bound: MarkupTemplate.capture(plan, List.iter([])) })
		}

		## Fallibly bind an explicitly parsed plan, checking the binding count.
		bind : MarkupTemplate.Plan, Iter(a) -> Try(Template(a), [InvalidMarkup(Markup.Error)])
		bind = |plan, values| MarkupTemplate.bind(plan, values).map_ok(|bound| Template.{ bound })
	}

	## A fragment retains every authored root and its exact source.
	Fragment := { parsed : Markup.Parsed }.{

		## Check a fragment literal with strict defaults.
		from_quote : Str -> Try(Fragment, [BadQuotedBytes(Str)])
		from_quote = |source| Svg.parse_fragment(source, {}).map_err(|InvalidMarkup(error)| Markup.quoted_error("Svg", error))

		## Require a single element root for downstream row/component templates.
		single_element : Fragment -> Try(Markup.Node, [RootCount(U64)])
		single_element = |fragment| {
			index = Markup.single_root(fragment.parsed.tree)?
			Ok(fragment.parsed.tree.nodes.get(index) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
		}
	}
}

expect Svg.parse_str("<svg xmlns='http://www.w3.org/2000/svg'><path d='M0 0L10 10z' pathLength='10'/></svg>").is_ok()
expect Svg.parse_str("<svg/>").is_err()
expect Svg.parse_str("<s:svg xmlns:s='http://www.w3.org/2000/svg'/>").is_ok()
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
	value.bound.values == ["<path/>"] and value.bound.plan.slots.len() == 1
}
expect {
	x = 42.U64
	value : Svg.Template(U64)
	value = "<circle cx='${x}' r='2'/>"
	value.bound.values == [42] and value.bound.plan.slots.map(|s| s.expected) == [Length]
}
expect Svg.parse_segments(["<circle r='", "'/>"], {}).is_ok()
expect Svg.parse_segments(["<path ", "='1'/>"], {}).is_err()
expect Svg.parse_segments(["<path d='M0 0 ", "'/>"], {}).is_err()
expect Svg.parse_segments(["<svg xmlns='", "'/>"], {}).is_err()
expect Svg.parse_segments(["<text>&am", "p;</text>"], {}).is_err()
