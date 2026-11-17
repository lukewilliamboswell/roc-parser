app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Markup
import parser.Html
import parser.Svg
import parser.MathML
import parser.Xml

## Parameterized native failures retain complete identifiers after input is dropped.
long_local = "a_root_name_long_enough_to_require_shared_storage"

long_prefix = "a_prefix_long_enough_to_require_shared_storage"

long_namespace = "urn:a_namespace_long_enough_to_require_shared_storage"

custom_name = "x-a-custom-element-name-long-enough-to-require-shared-storage"

name_payloads : Markup.Name -> List(Str)
name_payloads = |name| [
	name.local,
	match name.namespace {
		Uri(value) => value
		NoNamespace => ""
	},
	match name.prefix {
		Prefix(value) => value
		Unprefixed => ""
	},
]

source_name : U64 -> Markup.Name
source_name = |count| {
	doc = Xml.Namespaced.parse_str("<${long_prefix}:${long_local} xmlns:${long_prefix}='${long_namespace}'>${Str.repeat("x", count)}</${long_prefix}:${long_local}>", {}) ?? crash "valid generated name source was rejected"
	doc.root.name() ?? crash "generated name source omitted root"
}

retained_semantic_payloads : U64, U64 -> List(Str)
retained_semantic_payloads = |count, mode| {
	if mode == 0 {
		source = "<${long_prefix}:${long_local} xmlns:${long_prefix}='${long_namespace}'>${Str.repeat("x", count)}</${long_prefix}:${long_local}>"
		return match Svg.parse_str(source, {}) {
			Err(InvalidMarkup({ problem: WrongRoot({ expected, actual: Element(name) }), .. })) if expected.namespace == Uri("http://www.w3.org/2000/svg") and expected.local == "svg" => name_payloads(name)
			_ => crash "SVG wrong-root diagnostic changed meaning"
		}
	}
	if mode == 1 {
		source = "<lineargradient>${Str.repeat("x", count)}</lineargradient>"
		return match Svg.parse_fragment(source, { syntax: HtmlSyntax, strict_foreign_spelling: True }) {
			Err(InvalidMarkup({ problem: NonCanonicalName(detail), .. })) if detail.authored == "lineargradient" and detail.expected == "linearGradient" => [detail.authored, detail.expected]
			_ => crash "foreign spelling diagnostic changed meaning"
		}
	}
	if mode == 2 {
		source = "<${custom_name}>${Str.repeat("x", count)}"
		return match Html.parse_source(source, { mode: Fragment, options: { recovery: Browser, exact_structure: True } }) {
			Err(InvalidMarkup({ problem: HtmlStructure({ reason: MissingEndTag(name), .. }), .. })) if name == custom_name => [name]
			_ => crash "exact-structure diagnostic changed meaning"
		}
	}
	if mode == 3 or mode == 4 {
		spelling = if (mode == 3) "a_computed_name_long_enough_to_use_shared_storage invalid" else "xmlns:a_computed_name_long_enough_to_use_shared_storage"
		carrier = "${spelling}${Str.repeat("x", count)}"
		borrowed = carrier.drop_last_bytes(count) ?? crash "ASCII name boundary failed"
		namespace_carrier = "${long_namespace}${Str.repeat("x", count)}"
		namespace = namespace_carrier.drop_last_bytes(count) ?? crash "ASCII namespace boundary failed"
		match Markup.Name.from_qualified({ spelling: borrowed, namespace: Uri(namespace), context: Attribute }) {
			Err(InvalidDomName(error)) if error.location == Unavailable(GeneratedValue) => match error.problem {
				InvalidCharacter(detail) if mode == 3 and detail.spelling == spelling and detail.context == Attribute => return [detail.spelling]
				InvalidNamespace(detail) if mode == 4 and detail.spelling == spelling and detail.namespace == Uri(long_namespace) => return [
					detail.spelling,
					match detail.namespace {
						Uri(value) => value
						_ => crash "missing namespace identity"
					},
				]
				_ => crash "computed-name diagnostic changed meaning"
			}
			_ => crash "invalid computed name was accepted"
		}
	}
	if mode == 5 {
		name = source_name(count)
		context = Markup.Context.from_parent({ parent: { namespace: Uri(Markup.html_namespace), local: "div", prefix: Unprefixed } }) ?? crash "valid parent context was rejected"
		return match context.child({ name: name }) {
			Err(InvalidPlacement(error)) if error.location == Unavailable(GeneratedValue) => match error.problem {
				UnsupportedContext(detail) => name_payloads(detail.element)
				UnexpectedNamespace(detail) => name_payloads(detail.element)
				_ => crash "authoring context refusal changed meaning"
			}
			_ => crash "unsupported authored namespace was accepted"
		}
	}
	doc = Xml.Namespaced.parse_str("<p xmlns='http://www.w3.org/1999/xhtml'>${Str.repeat("x", count)}<div/></p>", {}) ?? crash "valid repair name source was rejected"
	parent = match doc.root {
		Element(node) => node
		_ => crash "missing parent name"
	}
	child = parent.children.find_first(
		|node| match node {
			Element(_) => True
			_ => False
		},
	) ?? crash "missing child element"
	context = Html.RepairContext.from_parent({ parent: parent.name })
	match context.child(child.name() ?? crash "missing child name") {
		Err(InvalidConstruction(error)) if error.location == Unavailable(GeneratedValue) => match error.problem {
			Repair({ rule: CloseParagraph, parent: parent_name, child: Element(child_name) }) => name_payloads(parent_name).concat(name_payloads(child_name))
			_ => crash "repair diagnostic changed meaning"
		}
		_ => crash "HTML paragraph repair was accepted"
	}
}

expected_semantic_payloads : U64 -> List(Str)
expected_semantic_payloads = |mode| match mode {
	0 | 5 => [long_local, long_namespace, long_prefix]
	1 => ["lineargradient", "linearGradient"]
	2 => [custom_name]
	3 => ["a_computed_name_long_enough_to_use_shared_storage invalid"]
	4 => ["xmlns:a_computed_name_long_enough_to_use_shared_storage", long_namespace]
	_ => ["p", "http://www.w3.org/1999/xhtml", "", "div", "http://www.w3.org/1999/xhtml", ""]
}

## Local diagnostic only, never a scheduled/CI campaign. A one-byte operation
## prefix frames the input; 'p'/'b' use NUL-separated static segments as transport.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	operation = bytes.first() ?? 's'
	if operation == 'e' {
		count = 8192 + (bytes.get(1) ?? 0).to_u64() * 1024
		for mode in (0.U64).to(6) {
			retained = Fuzz.measure!("markup-error-lifetime", || retained_semantic_payloads(count, mode))
			if retained.stats.exit_live_bytes > retained.stats.entry_live_bytes + 1024 {
				crash "markup diagnostic retained its source: mode ${mode.to_str()}, held ${retained.stats.exit_live_bytes.to_str()} bytes"
			}
			if retained.value != expected_semantic_payloads(mode) {
				crash "markup diagnostic lost complete identifier meaning"
			}
		}
		return Fuzz.keep
	}

	input = Str.from_utf8(bytes.drop_first(1)) ?? ""
	if operation == 'o' {
		_measured = Fuzz.measure!("parse", || Markup.Source.from_str(input))
	} else if operation == 'p' {
		segments = input.split_on("\u(0)")
		_measured = Fuzz.measure!("parse", || Svg.parse_segments(segments, {}))
	} else if operation == 'b' {
		segments = input.split_on("\u(0)")
		match Svg.parse_segments(segments, {}) {
			Ok(plan) => {
				values = List.repeat("<path/>", plan.metadata().slots.len())
				_measured = Fuzz.measure!("parse", || plan.bind(values.iter()))
			}
			Err(_) => return Fuzz.reject
		}
	} else if operation == 'h' {
		_measured = Fuzz.measure!("parse", || Html.parse_fragment(input, {}))
	} else if operation == 'r' {
		_measured = Fuzz.measure!("parse", || Html.parse_fragment(input, { recovery: Browser, allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }))
	} else if operation == 'm' {
		_measured = Fuzz.measure!("parse", || MathML.parse_fragment(input, {}))
	} else {
		_measured = Fuzz.measure!("parse", || Svg.parse_fragment(input, {}))
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "markup-alloc-diagnostic", generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(bytes), examples: [Fuzz.example({ name: "error-lifetime", revision: "1", make: || ['e'] })] })
