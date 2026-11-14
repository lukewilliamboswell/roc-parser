import Markup
import MarkupTemplate
import MarkupValidate
import HtmlValidate
import HtmlTree
import HtmlTemplate
import Literal

## HTML documents, fragments and typed literal templates. Strict HTML syntax and
## authoring checks are the default. Browser recovery is opt-in and retains typed
## diagnostics beside the repaired tree and exact original source bytes.
Html := { parsed : Markup.Parsed }.{

	## HTML defaults and explicit XML/recovery/validation/fragment-context options.
	## exact_structure requires authored nesting without browser repairs.
	## strict_foreign_spelling requires authored SVG/MathML names to match
	## their canonical DOM spelling in HTML syntax, including closing tags.
	Props := { syntax : Markup.Syntax ?? HtmlSyntax, exact_structure : Bool ?? False, strict_foreign_spelling : Bool ?? False, recovery : [Strict, Browser] ?? Strict, allow_unknown_elements : Bool ?? False, allow_unknown_attributes : Bool ?? False, check_values : Bool ?? True, max_source_bytes : U64 ?? 16_777_216, max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_nodes : U64 ?? 4_194_304, max_slots : U64 ?? 65_536, max_attributes : U64 ?? 1024, max_diagnostics : U64 ?? 1024, max_work_per_byte : U64 ?? 256, context_namespace : Str ?? "http://www.w3.org/1999/xhtml", context_element : Str ?? "body", scripting : Bool ?? True }

	## Source and diagnostics label for checked imported constants.
	ValidationProps := { source : Str, name : Str ?? "<literal>", options : Props ?? {} }

	## Parse a complete HTML document with strict defaults.
	parse_str : Str -> Try(Html, [InvalidMarkup(Markup.Error)])
	parse_str = |text| parse_with(text, {})

	## Parse and validate a document, retaining recovery diagnostics when enabled.
	parse_with : Str, Props -> Try(Html, [InvalidMarkup(Markup.Error)])
	parse_with = |text, props| {
		{ parsed, domain } = authored_syntax(text, props, False)?
		root = Markup.single_root(parsed.tree).map_err(|_| InvalidMarkup({ problem: WrongRoot({ expected: "html", actual: "fragment" }), line: 1, column: 1 }))?
		node = parsed.tree.nodes.get(root) ?? return Err(InvalidMarkup({ problem: Unsupported("missing document root"), line: 1, column: 1 }))
		match node.kind {
			Element(n) if n.namespace == Uri(Markup.html_namespace) and n.local == "html" => {}
			_ => return Err(InvalidMarkup({ problem: WrongRoot({ expected: "html in the HTML namespace", actual: "other" }), line: 1, column: 1 }))
		}
		checked = validate_authored(parsed, shared(props), domain)?
		Ok(Html.{ parsed: project_authored(checked, props, False, domain)? })
	}

	## Parse one or more roots in the configured fragment context.
	parse_fragment : Str, Props -> Try(Fragment, [InvalidMarkup(Markup.Error)])
	parse_fragment = |text, props| {
		{ parsed, domain } = authored_syntax(text, props, True)?
		checked = validate_authored(parsed, shared(props), domain)?
		Ok(Fragment.{ parsed: project_authored(checked, props, True, domain)? })
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
		foreign = MarkupValidate.foreign_with_html(parsed, props, holes)?
		if foreign.has_html {
			HtmlValidate.validate(foreign.parsed, props, holes)
		} else {
			Ok(foreign.parsed)
		}
	}

	## Validate a complete document for a checked constant or imported file.
	validate : ValidationProps -> Try(Html, [BadQuotedBytes(Str)])
	validate = |props| parse_with(props.source, props.options).map_err(|InvalidMarkup(e)| Literal.error({ name: props.name, format: "HTML", message: Str.inspect(e.problem), line: e.line, column: e.column }))

	## Check a complete HTML literal during compilation.
	from_quote : Str -> Try(Html, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })

	## A fragment preserves all authored roots and source bytes.
	Fragment : HtmlFragment

	## Parse fixed segments and explicit text/whole-attribute holes. No textual
	## sentinels are inserted, and structural positions cannot contain holes.
	parse_segments : List(Str), Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	parse_segments = |segments, props| HtmlTemplate.parse_segments(segments, { name: props.context_element, namespace: props.context_namespace, scripting: props.scripting }, shared(props))

	## A parsed template and its typed bindings
	## Binding performs no source parse.
	Template(a) : HtmlTemplateValue(a)
}

HtmlTemplateValue(a) := { bound : MarkupTemplate.Bound(a) }.{

	## Bind an explicit iterator to an existing plan without reparsing.
	bind : MarkupTemplate.Plan, Iter(a) -> Try(Html.Template(a), [InvalidMarkup(Markup.Error)])
	bind = |plan, values| MarkupTemplate.bind(plan, values).map_ok(|bound| Html.Template.{ bound })

	## Parse a static fragment as a template with no holes.
	from_quote : Str -> Try(Html.Template(a), [BadQuotedBytes(Str)])
	from_quote = |source| Html.parse_segments([source], {}).map_ok(|plan| Html.Template.{ bound: MarkupTemplate.capture(plan, [].iter()) }).map_err(|InvalidMarkup(e)| Markup.quoted_error("HTML", e))

	## Validate fixed segments at compile time and capture arbitrary typed
	## bindings through the compiler's Iter-based interpolation assembler.
	from_interpolation : List(Str) -> Try((Iter(a) -> Html.Template(a)), [InvalidInterpolation(Str)])
	from_interpolation = |segments| Html.parse_segments(segments, {}).map_ok(|plan| |values| Html.Template.{ bound: MarkupTemplate.capture(plan, values) }).map_err(|InvalidMarkup(e)| MarkupTemplate.interpolation_error(e))
}

HtmlFragment := { parsed : Markup.Parsed }.{

	## Validate imported fragment text with named diagnostics and options.
	validate : Html.ValidationProps -> Try(Html.Fragment, [BadQuotedBytes(Str)])
	validate = |props| Html.parse_fragment(props.source, props.options).map_err(|InvalidMarkup(e)| Literal.error({ name: props.name, format: "HTML", message: Str.inspect(e.problem), line: e.line, column: e.column }))

	## Check a fragment literal in the default body context.
	from_quote : Str -> Try(Html.Fragment, [BadQuotedBytes(Str)])
	from_quote = |text| Html.parse_fragment(text, {}).map_err(|InvalidMarkup(e)| Markup.quoted_error("HTML", e))

	## Require exactly one element root, ignoring comments/whitespace.
	single_element : Html.Fragment -> Try(Markup.Node, [RootCount(U64)])
	single_element = |fragment| {
		root = Markup.single_root(fragment.parsed.tree)?
		Ok(fragment.parsed.tree.nodes.get(root) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) })
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
shared = |p| { syntax: p.syntax, exact_structure: p.exact_structure, strict_foreign_spelling: p.strict_foreign_spelling, recovery: p.recovery, allow_unknown_elements: p.allow_unknown_elements, allow_unknown_attributes: p.allow_unknown_attributes, check_values: p.check_values, max_source_bytes: p.max_source_bytes, max_depth: p.max_depth, max_elements: p.max_elements, max_nodes: p.max_nodes, max_slots: p.max_slots, max_attributes: p.max_attributes, max_diagnostics: p.max_diagnostics, max_work_per_byte: p.max_work_per_byte }

# Browser recovery retains reference diagnostics while Exact independently checks
# authored structure. Decoded CR references are not source newline preprocessing.
expect {
	source = "<p title='a&#13;b'>a&#xD;b</p>"
	match Html.parse_fragment(source, { recovery: Browser, exact_structure: True }) {
		Ok(fragment) => {
			parsed = fragment.parsed
			root = parsed.tree.roots.first()?
			node = parsed.tree.nodes.get(root)?
			attribute = node.attributes.first()?
			Markup.text(parsed.tree, root) == "a\rb" and attribute.value == "a\rb" and parsed.source.bytes == source.to_utf8() and parsed.source.partition_valid() and parsed.diagnostics == [
				{ problem: HtmlSyntax(InvalidCharacterReference), line: 1, column: 12 },
				{ problem: HtmlSyntax(InvalidCharacterReference), line: 1, column: 21 },
			]
		}
		Err(_) => False
	}
}
expect {
	match Html.parse_fragment("<p title='a&#13;b'>a&#xD;b</p>", { exact_structure: True }) {
		Err(InvalidMarkup({ problem: HtmlSyntax(InvalidCharacterReference), line: 1, column: 12 })) => True
		_ => False
	}
}
expect {
	source = "<p title='a\r\nb\rc'>a\r\nb\rc</p>"
	match Html.parse_fragment(source, { exact_structure: True }) {
		Ok(fragment) => {
			parsed = fragment.parsed
			root = parsed.tree.roots.first()?
			node = parsed.tree.nodes.get(root)?
			attribute = node.attributes.first()?
			Markup.text(parsed.tree, root) == "a\nb\nc" and attribute.value == "a\nb\nc" and parsed.source.bytes == source.to_utf8() and parsed.diagnostics.is_empty()
		}
		Err(_) => False
	}
}
expect {
	source = "<p>\u(000B)</p>"
	match (Html.parse_fragment(source, {}), Html.parse_fragment(source, { recovery: Browser, exact_structure: True })) {
		(Err(InvalidMarkup({ problem: HtmlSyntax(ControlCharacterInInputStream), line: 1, column: 4 })), Ok(fragment)) => {
			root = fragment.parsed.tree.roots.first()?
			Markup.text(fragment.parsed.tree, root) == "\u(000B)" and fragment.parsed.diagnostics == [{ problem: HtmlSyntax(ControlCharacterInInputStream), line: 1, column: 4 }]
		}
		_ => False
	}
}
expect {
	match Html.parse_fragment("<p>&#13;</p><table><tr><td>x</td></tr></table>", { recovery: Browser, exact_structure: True }) {
		Err(InvalidMarkup({ problem: HtmlStructure({ reason: ImpliedElement("tbody"), .. }), .. })) => True
		_ => False
	}
}
expect {
	match Html.parse_fragment("<p a='1' a='2'></p>", { recovery: Browser, exact_structure: True, allow_unknown_attributes: True }) {
		Err(InvalidMarkup({ problem: HtmlStructure({ reason: ChangedAttributes, span: { start: 0, end: 15 } }), .. })) => True
		_ => False
	}
}

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
expect Html.parse_segments(["<table>", "</table>"], {}).is_err()
expect {
	plan = Html.parse_segments(["<textarea>\nA", "B</textarea>"], {})?
	Markup.text(plan.parsed.tree, Markup.single_root(plan.parsed.tree)?) == "AB"
}

expect Html.parse_segments(["<script>if(a<b)x()</script><p>", "</p>"], {}).is_ok()
expect Html.parse_segments(["<style>p::before{content:'<';}</style><p>", "</p>"], {}).is_ok()
expect Html.parse_segments(["<script>if(a<b)", "</script>"], {}).is_err()

expect Html.parse_segments(["<style>a>b { color:red }</style><p>", "</p>"], {}).is_ok()
expect Html.parse_segments(["", ""], { context_element: "script" }).is_err()
expect {
	p = Html.parse_segments(["<select><button><selectedcontent></selectedcontent></button><option>", "</option></select>"], {})?
	p.slots.len() == 2 and p.slots.all(|slot| slot.index == 0) and MarkupTemplate.bind(p, [42.U64].iter()).is_ok()
}
expect {
	p = Html.parse_segments(["<select><button><selectedcontent></selectedcontent></button><option>", "", "</option></select>"], {})?
	p.slots.len() == 4 and p.slots.keep_if(|slot| slot.index == 0).len() == 2 and p.slots.keep_if(|slot| slot.index == 1).len() == 2
}
expect {
	p = Html.parse_segments(["<select><button><selectedcontent></selectedcontent></button><option><span title='", "'>x</span></option></select>"], {})?
	p.slots.len() == 2 and p.slots.all(|slot| slot.index == 0)
}

authored_syntax = |text, props, fragment| if props.syntax == HtmlSyntax {
	HtmlTree.parse_classified(text, fragment, { name: props.context_element, namespace: props.context_namespace, scripting: props.scripting, defer_projection: True }, shared(props))
} else {
	parsed = syntax(text, props, fragment)?
	Ok({ parsed, domain: General })
}

# Only the parser's internal proof can bypass identity foreign/projection
# walks. Public validate_tree continues to check arbitrary supplied trees.
validate_authored = |parsed, props, domain| if domain == PureHtmlNoProjection {
	HtmlValidate.validate(parsed, props, [])
} else {
	Html.validate_tree(parsed, props, [])
}

project_authored = |parsed, props, fragment, domain| if domain == PureHtmlNoProjection {
	Ok(parsed)
} else {
	project_checked(parsed, props, fragment)
}

project_checked = |parsed, props, fragment| if props.syntax == HtmlSyntax {
	HtmlTree.project_parsed_with(parsed, shared(props), fragment).map_ok(|projection| projection.parsed)
} else {
	Ok(parsed)
}

expect Html.parse_fragment("<select><button><selectedcontent><mystery></mystery></selectedcontent></button><option>text</option></select>", {}).is_err()
expect Html.parse_segments(["<select><button><selectedcontent>", "</selectedcontent></button><option>text</option></select>"], {}).is_err()
expect Html.parse_fragment("<select><div><option>text</option></div></select>", {}).is_ok()
expect Html.parse_fragment("<select><option><button>bad</button></option></select>", {}).is_err()
expect Html.parse_fragment("<img role='button'>", {}).is_err()
expect Html.parse_fragment("<input type='IMAGE'>", {}).is_err()
expect Html.parse_fragment("<ul xmlns='http://www.w3.org/1999/xhtml'><div/></ul>", { syntax: XmlSyntax }).is_err()
expect Html.parse_fragment("<div xmlns='http://www.w3.org/1999/xhtml'><br>x</br></div>", { syntax: XmlSyntax }).is_err()
expect Html.parse_fragment("<div xmlns='http://www.w3.org/1999/xhtml'><ul>\u(00A0)</ul></div>", { syntax: XmlSyntax }).is_err()

expect Html.parse_fragment("<div xmlns='http://www.w3.org/1999/xhtml' xmlns:hidden='urn:example'/>", { syntax: XmlSyntax }).is_ok()
expect Html.parse_fragment("<img xmlns='http://www.w3.org/1999/xhtml' src='x' xml:alt='fake'/>", { syntax: XmlSyntax }).is_err()

expect Html.parse_fragment("<datalist><option><span>x</span></option></datalist>", {}).is_err()
expect Html.parse_fragment("<select><option label='Label'><span>x</span></option></select>", {}).is_err()
expect Html.parse_fragment("<select><option><object></object></option></select>", {}).is_err()
expect Html.parse_fragment("<button><span tabindex='0'>x</span></button>", {}).is_err()
expect Html.parse_fragment("<table xmlns='http://www.w3.org/1999/xhtml'><tr><td/></tr></table>", { syntax: XmlSyntax }).is_ok()

expect Html.parse_fragment("<datalist><div><option><span>x</span></option></div></datalist>", {}).is_err()
expect Html.parse_segments(["<select><option label='x' value='v'>", "</option></select>"], {}).is_err()
expect Html.parse_segments(["<br xmlns='http://www.w3.org/1999/xhtml'>", "</br>"], { syntax: XmlSyntax }).is_err()

expect Html.parse_fragment("<my-widget width='wide' loading='custom' disabled='false'></my-widget>", {}).is_ok()
expect Html.parse_fragment("<my-widget hidden='false'></my-widget>", {}).is_err()

expect {
	sources = [
		"<path d='M0 0'/>",
		"<g><text>x</text><path d='L0 0'/></g>",
		"<g><mystery bad='value'/></g>",
		"<metadata><h:button xmlns:h='http://www.w3.org/1999/xhtml'><h:input/></h:button></metadata>",
		"<foreignObject><h:img xmlns:h='http://www.w3.org/1999/xhtml'/></foreignObject>",
		"<g><h:button xmlns:h='http://www.w3.org/1999/xhtml'><h:input/></h:button><mystery/></g>",
		"<math xmlns='http://www.w3.org/1998/Math/MathML'><mfrac><mi>x</mi><mn>2</mn></mfrac></math>",
		"<math xmlns='http://www.w3.org/1998/Math/MathML'><annotation-xml><h:img xmlns:h='http://www.w3.org/1999/xhtml'/></annotation-xml></math>",
	]
	options : List(Markup.Props)
	options = [
		{},
		{ allow_unknown_elements: True, allow_unknown_attributes: True },
		{ allow_unknown_elements: True, allow_unknown_attributes: True, max_diagnostics: 0 },
		{ allow_unknown_elements: True, allow_unknown_attributes: True, max_diagnostics: 1 },
	]
	var $same = True
	for source in sources {
		parsed = Markup.parse_xml_fragment(source, Markup.svg_namespace, {})?
		for props in options {
			legacy = match MarkupValidate.foreign(parsed, props, []) {
				Ok(foreign) => HtmlValidate.validate(foreign, props, [])
				Err(error) => Err(error)
			}
			$same = $same and Html.validate_tree(parsed, props, []) == legacy
		}
	}
	$same
}

expect Html.parse_fragment("<p><b>one</b> &amp; two<br></p>", { exact_structure: True }).is_ok()
expect Html.parse_fragment("<p><div></div></p>", { exact_structure: True, recovery: Browser }).is_err()
expect Html.parse_fragment("<table><tbody><tr><td>x</td></tr></tbody></table>", { exact_structure: True }).is_ok()
expect Html.parse_segments(["<p>", "</p>"], { exact_structure: True }).is_ok()
expect Html.parse_segments(["<p>", ""], { exact_structure: True }).is_err()
expect Html.parse_segments(["<select><button><selectedcontent></selectedcontent></button><option>", "</option></select>"], { exact_structure: True }).is_ok()
expect Html.parse_fragment("<div/>", { syntax: XmlSyntax, exact_structure: True }).is_ok()
expect {
	p = Html.parse_fragment("<select><button><selectedcontent></selectedcontent></button><option>A</option></select>", { exact_structure: True })?
	p.parsed.tree.nodes.any(
		|node| match node.origin {
			Cloned(_) => True
			_ => False
		},
	)
}

expect {
	p = HtmlTree.parse_classified("<section><p>x</p></section>", True, {}, {})?
	p.domain == PureHtmlNoProjection
}
expect {
	p = HtmlTree.parse_classified("<select><option>x</option></select>", True, {}, {})?
	p.domain == General
}
expect {
	p = Html.parse_source("<p>x</p>", {}, True)?
	id = p.tree.roots.first()?
	node = p.tree.nodes.get(id)?
	match node.kind {
		Element(name) => {
			forged = { ..p, tree: { ..p.tree, nodes: p.tree.nodes.set(id, { ..node, kind: Element({ ..name, namespace: Uri("urn:untrusted") }) })? } }
			Html.validate_tree(forged, {}, []).is_err()
		}
		_ => False
	}
}
expect Html.parse_fragment("<p strange='value'>x</p>", { allow_unknown_attributes: True, max_diagnostics: 0 }).is_err()
expect Html.parse_fragment("<img>", {}).is_err()
expect Html.parse_fragment("<p hidden='false'>x</p>", {}).is_err()

# Foreign spelling is an authored-name policy, independent of browser repair.
expect {
	cases = [{ source: "<SVG></SVG>", authored: "SVG", expected: "svg", start: 1.U64, end: 4.U64, line: 1.U64, column: 2.U64, browser: False }, { source: "<svg viewbox='0 0 1 1'></svg>", authored: "viewbox", expected: "viewBox", start: 5.U64, end: 12.U64, line: 1.U64, column: 6.U64, browser: False }, { source: "<svg VIEWBOX='0 0 1 1'></svg>", authored: "VIEWBOX", expected: "viewBox", start: 5.U64, end: 12.U64, line: 1.U64, column: 6.U64, browser: False }, { source: "<svg><lineargradient></lineargradient></svg>", authored: "lineargradient", expected: "linearGradient", start: 6.U64, end: 20.U64, line: 1.U64, column: 7.U64, browser: False }, { source: "<svg><linearGradient></lineargradient></svg>", authored: "lineargradient", expected: "linearGradient", start: 23.U64, end: 37.U64, line: 1.U64, column: 24.U64, browser: False }, { source: "<svg><linearGradient></LINEARGRADIENT></svg>", authored: "LINEARGRADIENT", expected: "linearGradient", start: 23.U64, end: 37.U64, line: 1.U64, column: 24.U64, browser: False }, { source: "<svg><path XML:LANG='en'/></svg>", authored: "XML:LANG", expected: "xml:lang", start: 11.U64, end: 19.U64, line: 1.U64, column: 12.U64, browser: False }, { source: "<svg><path XLINK:HREF='#id'/></svg>", authored: "XLINK:HREF", expected: "xlink:href", start: 11.U64, end: 21.U64, line: 1.U64, column: 12.U64, browser: False }, { source: "<svg xmlns:XLINK='http://www.w3.org/1999/xlink'></svg>", authored: "xmlns:XLINK", expected: "xmlns:xlink", start: 5.U64, end: 16.U64, line: 1.U64, column: 6.U64, browser: False }, { source: "<math definitionurl='x'></math>", authored: "definitionurl", expected: "definitionURL", start: 6.U64, end: 19.U64, line: 1.U64, column: 7.U64, browser: False }, { source: "<math definitionURL='x'></MATH>", authored: "MATH", expected: "math", start: 26.U64, end: 30.U64, line: 1.U64, column: 27.U64, browser: False }, { source: "Ω<svg viewbox='x'></svg>", authored: "viewbox", expected: "viewBox", start: 7.U64, end: 14.U64, line: 1.U64, column: 8.U64, browser: False }, { source: "\r\n<svg\r\n viewbox = 'value &amp; >'></svg>", authored: "viewbox", expected: "viewBox", start: 9.U64, end: 16.U64, line: 3.U64, column: 2.U64, browser: False }, { source: "<svg><foreignObject><math></FOREIGNOBJECT></svg>", authored: "FOREIGNOBJECT", expected: "foreignObject", start: 28.U64, end: 41.U64, line: 1.U64, column: 29.U64, browser: True }, { source: "<svg><path data-X='x'/></svg>", authored: "data-X", expected: "data-x", start: 11.U64, end: 17.U64, line: 1.U64, column: 12.U64, browser: False }]
	cases.all(
		|case| match Html.parse_fragment(case.source, { strict_foreign_spelling: True, recovery: if (case.browser) Browser else Strict, allow_unknown_attributes: True, check_values: False }) {
			Err(InvalidMarkup(error)) => match error.problem {
				NonCanonicalName(detail) => detail == { authored: case.authored, expected: case.expected, span: { start: case.start, end: case.end } } and error.line == case.line and error.column == case.column
				_ => False
			}
			_ => False
		},
	)
}

expect Html.parse_fragment("<svg viewBox='0 0 1 1'><linearGradient id='g'></linearGradient><path xml:lang='en' xlink:href='#g'/></svg>", { strict_foreign_spelling: True }).is_ok()
expect Html.parse_fragment("<math><semantics><mi>x</mi><annotation definitionURL='x'>x</annotation></semantics></math>", { strict_foreign_spelling: True }).is_ok()
expect Html.parse_fragment("<svg><foreignObject><DIV DATA-X='x'>Text</DIV></foreignObject></svg>", { strict_foreign_spelling: True, exact_structure: True }).is_ok()
expect Html.parse_fragment("<math><mtext><SPAN TITLE='text'>x</SPAN></mtext></math>", { strict_foreign_spelling: True, exact_structure: True }).is_ok()
expect Html.parse_fragment("<svg viewbox='0 0 1 1'></svg>", { exact_structure: True }).is_ok()
expect Html.parse_fragment("<svg xmlns='http://www.w3.org/2000/svg' viewbox='0 0 1 1'/>", { syntax: XmlSyntax, strict_foreign_spelling: True, allow_unknown_attributes: True }).is_ok()
expect Html.parse_segments(["<svg viewbox='", "'></svg>"], { strict_foreign_spelling: True }).is_err()
expect {
	plan = Html.parse_segments(["<svg viewBox='", "'></svg>"], { strict_foreign_spelling: True })?
	_ = Html.Template.bind(plan, ["0 0 1 1"].iter())?
	True
}
expect match Html.parse_fragment("<svg viewbox='x' viewbox='y'></svg>", { strict_foreign_spelling: True, max_diagnostics: 0 }) {
	Err(InvalidMarkup(error)) => error.problem == ResourceLimit("max_diagnostics")
	_ => False
}
expect match Html.parse_fragment("<svg viewbox='x'></svg>", { strict_foreign_spelling: True, max_attributes: 0 }) {
	Err(InvalidMarkup(error)) => error.problem == LimitExceeded(Attributes)
	_ => False
}
expect match Html.parse_fragment("<svg viewbox='x' viewbox='y'></svg>", { strict_foreign_spelling: True }) {
	Err(InvalidMarkup(error)) => match error.problem {
		HtmlSyntax(DuplicateAttribute("viewbox")) => True
		_ => False
	}
	_ => False
}
expect match Html.parse_fragment("<svg viewbox='x'></svg><p>x", { strict_foreign_spelling: True, exact_structure: True }) {
	Err(InvalidMarkup(error)) => match error.problem {
		HtmlStructure(_) => True
		_ => False
	}
	_ => False
}

expect match Html.parse_fragment("<svg viewBox='x' VIEWBOX='y'></svg>", { strict_foreign_spelling: True, recovery: Browser, check_values: False }) {
	Err(InvalidMarkup(error)) => match error.problem {
		NonCanonicalName(detail) => detail.authored == "VIEWBOX" and detail.expected == "viewBox" and detail.span == { start: 17, end: 24 }
		_ => False
	}
	_ => False
}
expect match Html.parse_fragment("<svg a\u(0)b='x'></svg>", { strict_foreign_spelling: True, recovery: Browser, allow_unknown_attributes: True }) {
	Err(InvalidMarkup(error)) => match error.problem {
		NonCanonicalName(detail) => detail.authored == "a\u(0)b" and detail.expected == "a\u(FFFD)b" and detail.span == { start: 5, end: 8 }
		_ => False
	}
	_ => False
}
