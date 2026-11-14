app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.DomName

# tag::names[]
html = Uri("http://www.w3.org/1999/xhtml")

svg = Uri("http://www.w3.org/2000/svg")

computed_svg = DomName.validate_and_extract(svg, "s:a=b", Element)

plain_html = DomName.html_element("H:DIV")

qualified_html = DomName.validate_and_extract(html, "h:div", Element)

plain_href = DomName.unqualified_attribute("xlink:href")

qualified_href = DomName.validate_and_extract(Uri("http://www.w3.org/1999/xlink"), "xlink:href", Attribute)
# end::names[]

expect computed_svg == Ok({ namespace: svg, prefix: Prefix("s"), local: "a=b" })
expect plain_html == Ok({ namespace: html, prefix: Unprefixed, local: "h:div" })
expect qualified_html == Ok({ namespace: html, prefix: Prefix("h"), local: "div" })
expect plain_href != qualified_href
expect plain_html.map_ok(DomName.qualified) == qualified_html.map_ok(DomName.qualified)

main! = |_args| {
	Stdout.line!("DOM names retain explicit identity")?
	Ok({})
}
