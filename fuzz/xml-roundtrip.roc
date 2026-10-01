app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.String
import parser.Xml
import XmlGen

## Round-trip property: fuzzer bytes choose an XML document and how to write
## it (see XmlGen); Xml.parse_str must return exactly the tree XML 1.0 says a
## processor reports, and Xml.xml_parser must agree with it.

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
	combinator = String.parse_str(Xml.xml_parser, input.xml)
	if combinator != Ok(input.expected) {
		crash "Xml.xml_parser disagrees with Xml.parse_str on ${Str.inspect(input.xml)}"
	}
	Fuzz.keep
}

show_result : Try(Xml, [XmlError(Xml.Error)]) -> Str
show_result = |result| {
	match result {
		Ok(xml) => Str.inspect(xml)
		Err(XmlError(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

target = Fuzz.target_with({
	name: "xml-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| XmlGen.show(input.xml),
})
