app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import cli.Stdout
import parser.Markup
import parser.MarkupContext

html_name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

svg_name = |local| { namespace: Uri(Markup.svg_namespace), prefix: Unprefixed, local }

math_name = |local| { namespace: Uri(Markup.mathml_namespace), prefix: Unprefixed, local }

plain_name = |local| { namespace: NoNamespace, prefix: Unprefixed, local }

# tag::context[]
# Seed a known enclosing boundary once. Keep each returned context beside its
# constructed node so a later reactive update checks only the changed value.
checks : {} -> Try({}, MarkupContext.Problem)
checks = |_| {
	body = MarkupContext.seed(html_name("body"), [], {})?
	button = MarkupContext.child(body, html_name("button"), [])?
	label = MarkupContext.child(button, html_name("span"), [])?
	MarkupContext.text(label, "Save")?

	# The button restriction survives the intervening span.
	nested_link = MarkupContext.child(label, html_name("a"), [{ name: plain_name("href"), value: "/save" }])
	if nested_link.is_ok() {
		crash "interactive descendant accepted"
	}

	# An explicit section avoids the implied tbody required by HTML parsing.
	table = MarkupContext.child(body, html_name("table"), [])?
	if MarkupContext.child(table, html_name("tr"), []).is_ok() {
		crash "implicit tbody accepted"
	}
	section = MarkupContext.child(table, html_name("tbody"), [])?
	row = MarkupContext.child(section, html_name("tr"), [])?
	cell = MarkupContext.child(row, html_name("td"), [])?
	MarkupContext.text(cell, "A reactive value")?

	# SVG and MathML use the same explicit namespace identities as parsed trees.
	drawing = MarkupContext.child(body, svg_name("svg"), [])?
	group = MarkupContext.child(drawing, svg_name("g"), [])?
	_circle = MarkupContext.child(group, svg_name("circle"), [])?
	formula = MarkupContext.child(body, math_name("math"), [])?
	expression = MarkupContext.child(formula, math_name("mrow"), [])?
	identifier = MarkupContext.child(expression, math_name("mi"), [])?
	MarkupContext.text(identifier, "x")?
	if MarkupContext.text(expression, "unwrapped text").is_ok() {
		crash "MathML row text accepted"
	}
	Ok({})
}
# end::context[]

expect checks({}).is_ok()
main! = |_| {
	checks({}) ?? crash "constructor context check failed"
	Stdout.line!("HTML, SVG and MathML constructor contexts checked")
}
