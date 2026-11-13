app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Svg
import parser.MathML
import parser.Markup
import parser.MarkupFacts
import parser.Literal

# tag::policy[]
# This consumer permits extension attributes, but forbids known inline handler attributes.
# It is a consumer policy; ordinary SVG and MathML parsing preserves handlers.
NoHandlers := { parsed : Markup.Parsed }.{
	from_quote : Str -> Try(NoHandlers, [BadQuotedBytes(Str)])
	from_quote = |source| {
		fragment = Svg.Fragment.validate({ source, options: { allow_unknown_attributes: True } })?
		check(fragment.parsed)
	}

	check : Markup.Parsed -> Try(NoHandlers, [BadQuotedBytes(Str)])
	check = |parsed| {
		for node in parsed.tree.nodes {
			match node.kind {
				Element(element) => {
					for attribute in node.attributes {
						if MarkupFacts.is_event_handler(element, attribute.name) {
							position = Markup.position(parsed.source, attribute.span.start)
							return Err(Literal.error({ format: "NoHandlers", message: "inline handler ${attribute.name.local} forbidden on ${element.local}", line: position.line, column: position.column }))
						}
					}
				}
				_ => {}
			}
		}
		Ok(NoHandlers.{ parsed })
	}
}

# end::policy[]

# tag::ordinary[]
ordinary_svg : Svg.Fragment
ordinary_svg = "<metadata><math xmlns='http://www.w3.org/1998/Math/MathML'><mrow onclick='go()'><mi>x</mi></mrow></math></metadata><g oninput='go()'/>"

ordinary_math : MathML.Fragment
ordinary_math = "<mrow onclick='go()'><mi>x</mi></mrow>"

# end::ordinary[]

# tag::literal[]
consumer : NoHandlers
consumer = "<metadata><extension online='custom' customonline='also custom'/></metadata>"

# A namespaced onclick is a different expanded attribute identity.
namespaced : NoHandlers
namespaced = "<g xmlns:e='urn:extension' e:onclick='data'/>"

# end::literal[]

main! = |_args| {
	Stdout.line!("Ordinary SVG accepts handlers: ${Str.inspect(ordinary_svg.parsed.tree.roots.len() == 2)}")?
	Stdout.line!("Ordinary MathML accepts handlers: ${Str.inspect(ordinary_math.parsed.tree.roots.len() == 1)}")?
	Stdout.line!("Consumer rejects mixed MathML onclick: ${Str.inspect(NoHandlers.check(ordinary_svg.parsed).is_err())}")?
	Stdout.line!("Consumer rejects MathML onclick: ${Str.inspect(NoHandlers.check(ordinary_math.parsed).is_err())}")?
	Stdout.line!("Consumer accepts custom attributes: ${Str.inspect(consumer.parsed.tree.roots.len() == 1)}")?
	Stdout.line!("Consumer accepts namespaced onclick: ${Str.inspect(namespaced.parsed.tree.roots.len() == 1)}")?
	Ok({})
}

expect NoHandlers.check(ordinary_svg.parsed).is_err()
expect NoHandlers.check(ordinary_math.parsed).is_err()
