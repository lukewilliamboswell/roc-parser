app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import parser.Parser
import parser.Html
import cli.Stdout
import parser.Markup

html : Str -> Markup.Name
html = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

# tag::construction[]
checks : {} -> Try({}, [InvalidConstruction(Parser.Diagnostic(Html.RepairProblem))])
checks = |_| {
	body = Html.RepairContext.from_parent({ parent: html("body") })
	paragraph = body.child(html("p"))?
	span = paragraph.child(html("span"))?
	match span.child(html("div")) {
		Err(InvalidConstruction({ problem: Repair({ rule: CloseParagraph, .. }), .. })) => {}
		_ => crash "expected paragraph closure"
	}

	# Reuse the parent context for siblings and subsequent replacements.
	other_paragraph = body.child(html("p"))?
	other_paragraph.text("A reactive value")?
	table = body.child(html("table"))?
	match table.child(html("tr")) {
		Err(InvalidConstruction({ problem: Repair({ rule: InsertTableSection, .. }), .. })) => {}
		_ => crash "expected implied tbody"
	}
	section = table.child(html("tbody"))?
	_row = section.child(html("tr"))?

	# A name-only check cannot distinguish hidden and ordinary table inputs.
	match table.child(html("input")) {
		Err(InvalidConstruction({ problem: Unsupported({ reason: AttributeDependentTableInput, .. }), .. })) => {}
		_ => crash "expected attribute-dependent result"
	}

	# DOM names are explicit; no spelling or namespace is inferred.
	custom = { namespace: Uri("urn:widgets"), prefix: Prefix("w"), local: "Panel" }
	widget = body.child(custom)?
	widget.text("Custom text")?
	raw_math = body.child(html("math"))?
	_div = raw_math.child(html("div"))?
	Ok({})
}
# end::construction[]

expect checks({}).is_ok()
main! = |_| {
	checks({}) ?? crash "DOM construction check failed"
	Stdout.line!("Explicit DOM names and bounded HTML repairs checked")
}
