app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import parser.Parser
import cli.Stdout
import parser.Markup
html_name : Str -> Markup.Name
html_name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

svg_name : Str -> Markup.Name
svg_name = |local| { namespace: Uri(Markup.svg_namespace), prefix: Unprefixed, local }

math_name : Str -> Markup.Name
math_name = |local| { namespace: Uri(Markup.mathml_namespace), prefix: Unprefixed, local }

plain_name : Str -> Markup.Name
plain_name = |local| { namespace: NoNamespace, prefix: Unprefixed, local }

# tag::context[]
# Seed a known enclosing boundary once. Keep each returned context beside its
# constructed node so a later reactive update checks only the changed value.
checks : {} -> Try({}, [InvalidPlacement(Parser.Diagnostic(Markup.ContextProblem))])
checks = |_| {
	body = Markup.Context.from_parent({ parent: html_name("body"), attributes: [] })?
	button = body.child({ name: html_name("button"), attributes: [] })?
	label = button.child({ name: html_name("span"), attributes: [] })?
	label.text("Save")?

	# The button restriction survives the intervening span.
	nested_link = label.child({ name: html_name("a"), attributes: [{ name: plain_name("href"), value: "/save" }] })
	if nested_link.is_ok() {
		crash "interactive descendant accepted"
	}

	# An explicit section avoids the implied tbody required by HTML parsing.
	table = body.child({ name: html_name("table"), attributes: [] })?
	if table.child({ name: html_name("tr"), attributes: [] }).is_ok() {
		crash "implicit tbody accepted"
	}
	section = table.child({ name: html_name("tbody"), attributes: [] })?
	row = section.child({ name: html_name("tr"), attributes: [] })?
	cell = row.child({ name: html_name("td"), attributes: [] })?
	cell.text("A reactive value")?

	# SVG and MathML use the same explicit namespace identities as parsed trees.
	drawing = body.child({ name: svg_name("svg"), attributes: [] })?
	group = drawing.child({ name: svg_name("g"), attributes: [] })?
	_circle = group.child({ name: svg_name("circle"), attributes: [] })?
	formula = body.child({ name: math_name("math"), attributes: [] })?
	expression = formula.child({ name: math_name("mrow"), attributes: [] })?
	identifier = expression.child({ name: math_name("mi"), attributes: [] })?
	identifier.text("x")?
	if expression.text("unwrapped text").is_ok() {
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
