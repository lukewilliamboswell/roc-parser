app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Svg
import parser.MathML
import parser.Html
import parser.Markup
import parser.MarkupTemplate

# tag::literals[]
fraction : MathML.Fragment
fraction = "<mfrac><mn>1</mn><mn>2</mn></mfrac>"

panel : Html.Fragment
panel = "<section hidden='until-found'><p>Find this panel</p></section>"

cx = "42"

caption = "<path/> stays text"

drawing : Svg.Template(Str)
drawing =
	\\<svg viewBox="0 0 100 100">
	\\  <circle cx="${cx}" cy="50" r="4" />
	\\  <text>${caption}</text>
	\\</svg>

# end::literals[]

# This independent consumer could instead contain DOM commands or signal reads.
ConsumerNode := [Element({ namespace : Str, local : Str, attributes : List({ local : Str, value : Str }), children : List(ConsumerNode) }), Text(Str)]

# tag::lower[]
lower : Markup.Tree, U64, MarkupTemplate.Bound(Str) -> ConsumerNode
lower = |tree, index, bound| {
	node = tree.nodes.get(index) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
	match node.kind {
		Text(text) => {
			slot = bound.plan.slots.find_first(|s| s.target == TextSlot({ node: index }))
			Text(
				match slot {
					Ok(s) => bound.values.get(s.index) ?? ""
					Err(_) => text
				},
			)
		}
		Element(name) => {
			attrs = node.attributes.map_with_index(
				|attr, attribute| {
					slot = bound.plan.slots.find_first(|s| s.target == AttributeSlot({ node: index, attribute }))
					{
						local: attr.name.local,
						value: match slot {
							Ok(s) => bound.values.get(s.index) ?? ""
							Err(_) => attr.value
						},
					}
				},
			)
			namespace = match name.namespace {
				Uri(uri) => uri
				NoNamespace => ""
			}
			Element({ namespace, local: name.local, attributes: attrs, children: node.children.map(|child| lower(tree, child, bound)) })
		}
		_ => Text("")
	}
}

# end::lower[]

text_content : ConsumerNode -> Str
text_content = |node| match node {
	Text(value) => value
	Element(e) => Str.join_with(e.children.map(text_content), "")
}

element_info = |node| match node {
	Element(element) => element
	Text(_) => { namespace: "", local: "", attributes: [], children: [] }
}

main! = |_args| {
	root = drawing.bound.plan.parsed.tree.roots.first() ?? 0
	consumer = lower(drawing.bound.plan.parsed.tree, root, drawing.bound)
	svg = element_info(consumer)
	Stdout.line!("SVG: ${svg.local} in ${svg.namespace}")?
	circle = svg.children.find_first(|n| element_info(n).local == "circle") ?? Text("")
	circle_info = element_info(circle)
	cx_attr = circle_info.attributes.find_first(|a| a.local == "cx") ?? { local: "", value: "" }
	Stdout.line!("Bound cx: ${cx_attr.value}")?
	Stdout.line!("Caption: ${text_content(consumer).trim()}")?
	Stdout.line!("MathML fraction: ${Str.inspect(fraction.single_element().is_ok())}")?
	Stdout.line!("HTML panel: ${Str.inspect(panel.single_element().is_ok())}")?
	Ok({})
}

expect drawing.bound.values == ["42", "<path/> stays text"]
expect drawing.bound.plan.slots.len() == 2
expect {
	original = "<path pathLength='2'/>\r\n"
	Svg.parse_fragment(original, {})?.parsed.source.bytes == original.to_utf8()
}
