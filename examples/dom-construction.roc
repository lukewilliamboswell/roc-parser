app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import cli.Stdout
import parser.DomConstruction
import parser.Markup

html = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

# tag::construction[]
checks : {} -> Try({}, DomConstruction.Problem)
checks = |_| {
	body = DomConstruction.root({ parent: html("body") })
	paragraph = DomConstruction.enter(body, html("p"))?
	span = DomConstruction.enter(paragraph, html("span"))?
	match DomConstruction.enter(span, html("div")) {
		Err(Repair({ rule: CloseParagraph, .. })) => {}
		_ => crash "expected paragraph closure"
	}

	# Reuse the parent context for siblings and subsequent replacements.
	other_paragraph = DomConstruction.enter(body, html("p"))?
	DomConstruction.text(other_paragraph, "A reactive value")?
	table = DomConstruction.enter(body, html("table"))?
	match DomConstruction.enter(table, html("tr")) {
		Err(Repair({ rule: InsertTableSection, .. })) => {}
		_ => crash "expected implied tbody"
	}
	section = DomConstruction.enter(table, html("tbody"))?
	_row = DomConstruction.enter(section, html("tr"))?

	# A name-only check cannot distinguish hidden and ordinary table inputs.
	match DomConstruction.enter(table, html("input")) {
		Err(Unsupported({ reason: AttributeDependentTableInput, .. })) => {}
		_ => crash "expected attribute-dependent result"
	}

	# DOM names are explicit; no spelling or namespace is inferred.
	custom = { namespace: Uri("urn:widgets"), prefix: Prefix("w"), local: "Panel" }
	widget = DomConstruction.enter(body, custom)?
	DomConstruction.text(widget, "Custom text")?
	raw_math = DomConstruction.enter(body, html("math"))?
	_div = DomConstruction.enter(raw_math, html("div"))?
	Ok({})
}
# end::construction[]

expect checks({}).is_ok()
main! = |_| {
	checks({}) ?? crash "DOM construction check failed"
	Stdout.line!("Explicit DOM names and bounded HTML repairs checked")
}
