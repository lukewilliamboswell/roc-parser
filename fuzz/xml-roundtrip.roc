app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Utf8
import parser.Xml
import XmlGen

## Round-trip property: fuzzer bytes choose an XML document and how to write
## it (see XmlGen); Xml.parse_str must return exactly the tree XML 1.0 says a
## processor reports, and Xml.parser must agree with it.

Input : { xml : Str, expected : Xml }

generate : List(U8) -> Input
generate = |bytes| {
	doc = XmlGen.generate(bytes)
	{ xml: XmlGen.render(doc.pieces), expected: doc.expected }
}

test : Input -> Fuzz.Outcome
test = |input| {
	actual = Xml.parse_str(input.xml)
	match actual {
		Ok(xml) if xml == input.expected => {}
		_ => crash "round trip mismatch\n--- xml ---\n${Str.inspect(input.xml)}\n--- expected ---\n${Str.inspect(input.expected)}\n--- actual ---\n${show_result(actual)}"
	}
	combinator = Utf8.parse_str(Xml.parser, input.xml)
	if combinator != Ok(input.expected) {
		crash "Xml.parser disagrees with Xml.parse_str on ${Str.inspect(input.xml)}"
	}
	check_helpers(input.expected.root)
	Fuzz.keep
}

## The Node helpers agree with a direct walk of the tree.
check_helpers : Xml.Node -> {}
check_helpers = |node| {
	match node {
		Text(text) =>
			if node.text() != text or node.name() != Err(NotAnElement) or node.attribute("id") != Err(Missing) or !node.children_named("a").is_empty() {
				crash "Node helpers are wrong for ${Str.inspect(node)}"
			}

		Element({ name, attributes, children }) => {
			if node.name() != Ok(name) {
				crash "name() is wrong for ${Str.inspect(node)}"
			}
			for attribute in attributes {
				if node.attribute(attribute.name) != Ok(attribute.value) {
					crash "attribute(${attribute.name}) is wrong for ${Str.inspect(node)}"
				}
			}
			if node.attribute("no-such-attribute") != Err(Missing) {
				crash "attribute() found a missing attribute in ${Str.inspect(node)}"
			}
			expected_text = Str.join_with(children.map(|child| child.text()), "")
			if node.text() != expected_text {
				crash "text() is wrong for ${Str.inspect(node)}"
			}
			for child in children {
				match child.name() {
					Ok(child_name) => {
						named = children.keep_if(|other| other.name() == Ok(child_name))
						if node.children_named(child_name) != named {
							crash "children_named(${child_name}) is wrong for ${Str.inspect(node)}"
						}
					}

					Err(NotAnElement) => {}
				}
				check_helpers(child)
			}
		}
	}
}

show_result : Try(Xml, [InvalidXml(Xml.Error)]) -> Str
show_result = |result| {
	match result {
		Ok(xml) => Str.inspect(xml)
		Err(InvalidXml(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

target = Fuzz.target_with({
	name: "xml-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| XmlGen.show(input.xml),
})
