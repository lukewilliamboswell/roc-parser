app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import parser.Markup
import cli.Stdout
# tag::names[]
html = Uri("http://www.w3.org/1999/xhtml")

svg = Uri("http://www.w3.org/2000/svg")

computed_svg = Markup.Name.from_qualified({ namespace: svg, spelling: "s:a=b", context: Element })

plain_html : Markup.Name
plain_html = { namespace: html, prefix: Unprefixed, local: "h:div" }

qualified_html = Markup.Name.from_qualified({ namespace: html, spelling: "h:div", context: Element })

plain_href : Markup.Name
plain_href = { namespace: NoNamespace, prefix: Unprefixed, local: "xlink:href" }

qualified_href = Markup.Name.from_qualified({ namespace: Uri("http://www.w3.org/1999/xlink"), spelling: "xlink:href", context: Attribute })
# end::names[]

expect computed_svg == Ok({ namespace: svg, prefix: Prefix("s"), local: "a=b" })
expect plain_html == { namespace: html, prefix: Unprefixed, local: "h:div" }
expect qualified_html == Ok({ namespace: html, prefix: Prefix("h"), local: "div" })
expect Ok(plain_href) != qualified_href
expect Ok(plain_html.qualified()) == qualified_html.map_ok(|name| name.qualified())

main! = |_args| {
	Stdout.line!("DOM names retain explicit identity")?
	Ok({})
}
