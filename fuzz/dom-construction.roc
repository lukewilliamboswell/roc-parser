app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Html
import parser.Markup
import MarkupCheck

# These names exercise the declared repairs without document-mode changes,
# foreign namespace inference, nested selects, ruby or adoption end tags.
names = ["div", "span", "p", "button", "a", "form", "ul", "li", "dl", "dd", "dt", "h1", "h2", "table", "tbody", "thead", "tfoot", "tr", "td", "th", "caption", "colgroup", "col", "script", "style", "template", "option", "optgroup", "select", "hr", "br", "textarea", "object", "applet", "marquee", "b", "address", "x-widget"]

name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

opening = |local, index| "<${local} data-ctx-id='${index.to_str()}'>"

# The expected model comes from the intended append operations, independently
# of the constructor checks and the parser. Unique attributes detect cloning.
expected = |chain, children| {
	var $children = children
	var $at = chain.len()
	while $at > 0 {
		$at = $at - 1
		local = chain.get($at) ?? crash "invalid generated chain"
		$children = [MarkupCheck.Model.Element({ namespace: Markup.html_namespace, local, attributes: [("", "data-ctx-id", $at.to_str())], children: $children })]
	}
	MarkupCheck.Model.Element({ namespace: Markup.html_namespace, local: "body", attributes: [], children: $children })
}

preserved = |source, model| {
	parsed = Html.parse_source("<!doctype html><html><head></head><body>${source}", { recovery: Browser }, False) ?? crash "bounded generated input exceeds parser limits"
	body = parsed.tree.nodes.find_first(|node| node.kind == Element(name("body"))) ?? crash "missing body"
	actual = MarkupCheck.Model.Element({ namespace: Markup.html_namespace, local: "body", attributes: [], children: body.children.map(|id| MarkupCheck.model(parsed.tree, id)) })
	# A nested anchor can repair the active-formatting/open-element state
	# across a scope boundary while leaving the immediate DOM shape equal.
	# Observe the parser's typed repair diagnostic as well as the full tree.
	actual == model and !parsed.diagnostics.any(|diagnostic| diagnostic.problem == HtmlSyntax(NestedAnchor))
}

# The same explicit input name is preserved only with type=hidden. A name-only
# API must return Unsupported for both attribute choices, never a false Repair.
check_table_inputs = || {
	var $context = Html.RepairContext.from_parent({ parent: name("body") })
	var $source = ""
	var $chain = []
	for local in ["table", "tbody", "tr"] {
		$context = $context.child(name(local)) ?? crash "legal table chain rejected"
		$source = "${$source}${opening(local, $chain.len())}"
		$chain = $chain.append(local)
		match $context.child(name("input")) {
			Err(InvalidConstruction({ problem: Unsupported({ reason: AttributeDependentTableInput, .. }), .. })) => {}
			_ => crash "table input requires attributes"
		}
		for kind in ["hidden", "text"] {
			id = $chain.len().to_str()
			source = "${$source}<input data-ctx-id='${id}' type='${kind}'>"
			input = MarkupCheck.Model.Element({ namespace: Markup.html_namespace, local: "input", attributes: [("", "data-ctx-id", id), ("", "type", kind)], children: [] })
			if preserved(source, expected($chain, [input])) != (kind == "hidden") {
				crash "independent table-input attribute control disagrees with tree construction"
			}
		}
	}
}

# EOF terminates the probe, so closing-tag repairs do not confound a start-tag
# check. Only accepted start tags extend the prefix; rejected probes are siblings.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	if (bytes.first() ?? 0) % 31 == 0 {
		check_table_inputs()
	}
	var $context = Html.RepairContext.from_parent({ parent: name("body") })
	var $source = ""
	var $chain = []
	for byte in bytes.take_first(12) {
		local = names.get(U8.to_u64(byte) % names.len()) ?? "div"
		# Nested select is outside RepairRule's declared subset.
		if !(local == "select" and $chain.contains("select")) {
			candidate = "${$source}${opening(local, $chain.len())}"
			matches = preserved(candidate, expected($chain.append(local), []))
			result = $context.child(name(local))
			if matches != result.is_ok() {
				crash "element check disagrees with tree construction: ${Str.inspect($chain)} -> ${local}, result ${Str.inspect(result.map_ok(|_| {}))}"
			}
			match result {
				Ok(child) => {
					$context = child
					$source = candidate
					$chain = $chain.append(local)
				}
				Err(_) => {}
			}
		}
		# Keep decoded values and spelling equal even in raw-text/RCDATA contexts;
		# escaping, initial-newline stripping and null preprocessing are separate.
		for value in ["word", " \t", "é"] {
			text_matches = preserved("${$source}${value}", expected($chain, [Text(value)]))
			if text_matches != $context.text(value).is_ok() {
				crash "text check disagrees with tree construction: ${Str.inspect($chain)} -> ${Str.inspect(value)}"
			}
		}
	}
	Fuzz.keep
}

examples = [[5, 25, 13, 5], [25, 5, 13, 5], [4, 28, 4], [2, 1, 0], [13, 17], [13, 14, 17, 18], [4, 1, 4], [4, 32, 4], [4, 3, 4], [5, 25, 5, 5], [3, 25, 3], [7, 6, 7], [9, 8, 10], [11, 1, 12], [28, 27, 26], [28, 9, 26], [13, 5, 0], [13, 21, 23], [13, 20, 4, 4], [30, 1], [31, 1]]

target = Fuzz.target_with({ name: "dom-construction", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: examples.map(|bytes| Fuzz.example({ name: Str.inspect(bytes), revision: "1", make: || bytes })) })
