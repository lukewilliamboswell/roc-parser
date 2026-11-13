app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Markup
import parser.MarkupTemplate
import parser.Html
import parser.Svg
import parser.MathML
import parser.XmlNs
import parser.JsonValue

## A subprocess boundary using only public parser APIs. Source comes from stdin.
main! : List(OsStr) => Try({}, _)
main! = |arguments| {
	args = arguments.map(OsStr.display)
	format = args.get(0) ?? "html"
	syntax = if (args.get(1) == Ok("xml")) XmlSyntax else HtmlSyntax
	fragment = args.get(2) == Ok("fragment")
	recovery = if (args.get(3) == Ok("browser")) Browser else Strict
	validate = args.get(4) != Ok("structural")
	namespace = args.get(5) ?? Markup.html_namespace
	context = args.get(6) ?? "body"
	bytes = Stdin.read_to_end!()?
	if format == "template" or format == "template-html" {
		caption = Str.from_utf8(bytes) ?? ""
		plan = (
			if format == "template-html" {
				Html.parse_segments(["<script>if (a < b) x = 1;</script><svg><circle cx='", "' cy='0' r='1'/><text>", "</text></svg>"], {})
			} else {
				Svg.parse_segments(["<svg><circle cx='", "' cy='0' r='1'/><text>", "</text></svg>"], {})
			}
		).map_err(|InvalidMarkup(error)| crash Str.inspect(error))?
		bound = MarkupTemplate.bind(plan, ["12", caption].iter()).map_err(|InvalidMarkup(error)| crash Str.inspect(error))?
		roots = Str.join_with(bound.plan.parsed.tree.roots.map(|index| encode_node(bound.plan.parsed.tree, index, False)), ",")
		bindings = Str.join_with(
			bound.plan.slots.map(
				|slot| {
					(node, attribute) = match slot.target {
						TextSlot(target) => (target.node, "null")
						AttributeSlot(target) => {
							owner = bound.plan.parsed.tree.nodes.get(target.node) ?? crash "invalid slot node"
							attr = owner.attributes.get(target.attribute) ?? crash "invalid slot attribute"
							(target.node, "[${quote(namespace_uri(attr.name.namespace))},${quote(attr.name.local)}]")
						}
					}
					path = Str.join_with(node_path(bound.plan.parsed.tree, node).map(U64.to_str), ",")
					value = bound.values.get(slot.index) ?? crash "missing bound value"
					"{\"path\":[${path}],\"attribute\":${attribute},\"value\":${quote(value)}}"
				},
			),
			",",
		)
		Stdout.line!("{\"status\":\"ok\",\"roots\":[${roots}],\"bindings\":[${bindings}],\"diagnostics\":[]}")?
		return Ok({})
	}
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"error\",\"error\":{\"code\":\"InvalidUtf8\",\"line\":1,\"column\":1}}")?
		Ok(source) => {
			props : Markup.Props
			props = { syntax, recovery, allow_unknown_elements: !validate, allow_unknown_attributes: !validate, check_values: validate }
			result = if format == "html" {
				html_props : Html.Props
				html_props = { syntax, recovery, allow_unknown_elements: !validate, allow_unknown_attributes: !validate, check_values: validate, context_namespace: namespace, context_element: context, scripting: args.get(7) == Ok("on") }
				if !validate {
					Html.parse_source(source, html_props, fragment)
				} else if fragment {
					Html.parse_fragment(source, html_props).map_ok(|value| value.parsed)
				} else {
					Html.parse_with(source, html_props).map_ok(|value| value.parsed)
				}
			} else if !validate and syntax == XmlSyntax {
				if fragment {
					Markup.parse_xml_fragment(source, namespace, props)
				} else {
					Markup.parse_xml(source, props)
				}
			} else if format == "svg" {
				if fragment {
					Svg.parse_fragment(source, props).map_ok(|value| value.parsed)
				} else {
					Svg.parse_with(source, props).map_ok(|value| value.parsed)
				}
			} else {
				if fragment {
					MathML.parse_fragment(source, props).map_ok(|value| value.parsed)
				} else {
					MathML.parse_with(source, props).map_ok(|value| value.parsed)
				}
			}
			match result {
				Ok(parsed) => {
					roots = Str.join_with(parsed.tree.roots.map(|index| encode_node(parsed.tree, index, args.get(8) == Ok("bindings"))), ",")
					issues = Str.join_with(parsed.diagnostics.map(encode_error), ",")
					Stdout.line!("{\"status\":\"ok\",\"roots\":[${roots}],\"diagnostics\":[${issues}]}")?
				}
				Err(InvalidMarkup(error)) => Stdout.line!("{\"status\":\"error\",\"error\":${encode_error(error)}}")?
			}
		}
	}
	Ok({})
}

quote = |value| JsonValue.to_str(JsonValue.String(value)) ?? "\"\""

namespace_uri = |namespace| match namespace {
	Uri(uri) => uri
	NoNamespace => ""
}

encode_error = |error| "{\"code\":${quote(Str.inspect(error.problem))},\"line\":${error.line.to_str()},\"column\":${error.column.to_str()}}"

encode_node : Markup.Tree, U64, Bool -> Str
encode_node = |tree, index, bindings| {
	match tree.nodes.get(index) {
		Err(_) => "null"
		Ok(node) => match node.kind {
			Text(text) => "[\"text\",${quote(text)}]"
			Comment(text) => "[\"comment\",${quote(text)}]"
			Doctype(doctype) => "[\"doctype\",${quote(doctype.name)},${quote(doctype.public_id)},${quote(doctype.system_id)}]"
			ProcessingInstruction(pi) => "[\"pi\",${quote(pi.target)},${quote(pi.data)}]"
			Element(name) => {
				attributes = node.attributes.keep_if(|attribute| bindings or (attribute.name.namespace != Uri("http://www.w3.org/2000/xmlns/") and attribute.name.local != "xmlns"))
				attrs = Str.join_with(attributes.map(|attribute| "[${quote(namespace_uri(attribute.name.namespace))},${quote(attribute.name.local)},${quote(attribute.value)}]"), ",")
				children = Str.join_with(node.children.map(|child| encode_node(tree, child, bindings)), ",")
				"[\"element\",${quote(namespace_uri(name.namespace))},${quote(name.local)},[${attrs}],[${children}]]"
			}
		}
	}
}

node_path : Markup.Tree, U64 -> List(U64)
node_path = |tree, target| {
	var $pending = []
	var $root = 0.U64
	for index in tree.roots {
		$pending = $pending.append({ node: index, path: [$root] })
		$root = $root + 1
	}
	while !$pending.is_empty() {
		item = $pending.last() ?? crash "missing path"
		$pending = $pending.drop_last(1)
		if item.node == target {
			return item.path
		}
		node = tree.nodes.get(item.node) ?? crash "invalid tree edge"
		var $child = 0.U64
		for index in node.children {
			$pending = $pending.append({ node: index, path: item.path.append($child) })
			$child = $child + 1
		}
	}
	crash "slot not reachable from roots"
}
