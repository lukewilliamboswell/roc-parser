# XML parser
# Original author: [Johannes Maas](https://github.com/j-maas)
import Parser
import String

## XML document tree and parser based on the XML 1.0 specification.
##
## The parser supports optional XML declarations, attributes, nested elements,
## character data, CDATA sections, and self-closing elements.
Xml := {
	xml_declaration : [Given(Xml.Declaration), Missing],
	root : Xml.Node,
}.{

	## Compare two XML documents structurally.
	is_eq : _

	## An XML attribute name and decoded value.
	Attribute : { name : Str, value : Str }

	## Text encoding declared by an XML declaration.
	TextEncoding : [
		Utf8Encoding,
		OtherEncoding(Str),
	]

	## Version and optional encoding from an XML declaration.
	Declaration : {
		version : Version,
		encoding : [Given(TextEncoding), Missing],
	}

	## An XML 1.x version, storing the digit after `1.`.
	Version :: {
		after_dot : U8,
	}.{

		## Compare two XML versions.
		is_eq : _

		## Construct an XML 1.x version from the digit after `1.`.
		new : U8 -> Version
		new = |after_dot| {
			{ after_dot }
		}
	}

	## An XML element or text node.
	Node := [
		Element(Str, List({ name : Str, value : Str }), List(Node)),
		Text(Str),
	].{

		## Compare two XML nodes structurally.
		is_eq : _
	}

	## Parse one XML document, including an optional declaration and trailing whitespace.
	xml_parser : Parser(String.Utf8, Xml)
	xml_parser =
		Parser.const(
			|xml_declaration| |root| {
				Xml.{ xml_declaration, root }
			},
		)
			.keep(p_prolog)
			.keep(p_element)
			.skip(p_whitespace.many())
}

v1_dot0 : Xml.Version
v1_dot0 = {
	Xml.Version.new(0)
}

## Full XML parsing captures the declaration and root element.
expect {
	result = String.parse_str(Xml.xml_parser, test_xml)?

	result
		== {
			xml_declaration: Given({
				version: v1_dot0,
				encoding: Given(Utf8Encoding),
			}),
			root: Element(
				"root",
				[],
				[
					Text("\n    "),
					Element(
						"element",
						[{ name: "arg", value: "value" }],
						[],
					),
					Text("\n"),
				],
			),
		}
}

## XML parsing accepts documents without a prolog.
expect {
	result = String.parse_str(Xml.xml_parser, "<element />")?

	result
		== {
			xml_declaration: Missing,
			root: Element("element", [], []),
		}
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-prolog
p_prolog : Parser(String.Utf8, [Given(Xml.Declaration), Missing])
p_prolog =
	Parser.const(
		|xml_declaration| {
			|_misc| {
				xml_declaration
			}
		},
	)
		.keep(p_xml_declaration.map(|a| Given(a)) |> maybe_with_default(Missing))
		.keep(p_many_misc)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-XMLDecl
p_xml_declaration : Parser(String.Utf8, Xml.Declaration)
p_xml_declaration =
	Parser.const(
		|version| {
			|encoding| {
				{
					version,
					encoding,
				}
			}
		},
	)
		.skip(String.string("<?xml"))
		.skip(p_whitespace.one_or_more())
		.keep(p_version)
		.keep(
			Parser.const(|encoding| encoding)
				.skip(p_whitespace.one_or_more())
				.keep(p_encoding_declaration)
				.map(|a| Given(a))
				|> maybe_with_default(Missing),
		)
		.skip(p_whitespace.many())
		.skip(String.string("?>"))

## XML declaration parsing captures version and encoding.
expect {
	result =
		String.parse_str(
			p_xml_declaration,
			\\<?xml version="1.0" encoding="utf-8"?>
			,
		)?

	result
		== {
			version: v1_dot0,
			encoding: Given(Utf8Encoding),
		}
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionInfo
p_version : Parser(String.Utf8, Xml.Version)
p_version =
	p_version_number
		|> between_quotes
		|> p_attribute("version")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionNum
p_version_number : Parser(String.Utf8, Xml.Version)
p_version_number =
	Parser.const(
		|after_dot| {
			Xml.Version.new(U64.to_u8_wrap(after_dot)) # TODO: change to to_u8_try
		},
	)
		.skip(String.string("1."))
		.keep(String.digits)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncodingDecl
p_encoding_declaration : Parser(String.Utf8, Xml.TextEncoding)
p_encoding_declaration =
	p_encoding_name
		|> between_quotes
		|> p_attribute("encoding")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncName
p_encoding_name : Parser(String.Utf8, Xml.TextEncoding)
p_encoding_name =
	Parser.const(
		|first_char| {
			|rest| {
				combine_to_str(first_char, rest)
					.map_ok(
						|encoding_name| {
							match encoding_name {
								"utf-8" => Utf8Encoding
								other => OtherEncoding(other)
							}
						},
					)
			}
		},
	)
		.keep(String.codeunit_satisfies(is_alphabetical))
		.keep(
			Parser.chomp_while(
				|c| {
					is_alphabetical(c)
						or is_digit(c)
							or (c == '-')
								or (c == '.')
									or (c == '_')
				},
			),
		)
		.flatten()

## UTF-8 encoding names parse as the Utf8Encoding tag.
expect {
	result = String.parse_str(p_encoding_name, "utf-8")?

	result == Utf8Encoding
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-element
p_element : Parser(String.Utf8, Xml.Node)
p_element = Parser.build_primitive_parser(parse_element_partial)

# Keep recursive parsing behind a function. Mutually recursive top-level parser
# values currently trigger roc-lang/roc#10098.
parse_element_partial : String.Utf8 -> Parser.ParseResult(String.Utf8, Xml.Node)
parse_element_partial = |input| {
	parser = Parser.const(
		|name| {
			|arguments| {
				|contents| {
					Element(name, arguments, contents)
				}
			}
		},
	)
		.skip(String.string("<"))
		.keep(p_name)
		.keep(
			Parser.const(
				|attribute| {
					attribute
				},
			)
				.skip(p_whitespace.many())
				.keep(p_element_attribute)
				.many(),
		)
		.skip(p_whitespace.many())
		.keep(
			Parser.alt(
				Parser.const(
					|contents| {
						contents
					},
				)
					.skip(String.string(">"))
					.keep(
						Parser.one_of([
							p_character_data,
							Parser.build_primitive_parser(parse_element_partial),
							p_cdata_section,
						])
							.many(),
					)
					.skip(p_end_tag),
				String.string("/>").map(
					|_| {
						[]
					},
				),
			),
		)

	Parser.parse_partial(parser, input)
}

## Empty elements can include whitespace before the self-closing marker.
expect {
	result = String.parse_str(p_element, "<element />")?

	result == Element("element", [], [])
}

## Empty elements can omit whitespace before the self-closing marker.
expect {
	result = String.parse_str(p_element, "<element/>")?

	result == Element("element", [], [])
}

## Empty elements can carry attributes.
expect {
	result = String.parse_str(
		p_element,
		\\<element arg="value"/>
		,
	)?

	result == Element("element", [{ name: "arg", value: "value" }], [])
}

## Explicit start and end tags can represent an empty element.
expect {
	result = String.parse_str(p_element, "<element></element>")?

	result == Element("element", [], [])
}

## Elements can parse multiple attributes and text content.
expect {
	result = String.parse_str(
		p_element,
		\\<element firstArg="one" secondArg="two">text content</element>
		,
	)?

	result
		== Element(
			"element",
			[
				{ name: "firstArg", value: "one" },
				{ name: "secondArg", value: "two" },
			],
			[Text("text content")],
		)
}

## CDATA sections parse into text nodes.
expect {
	result = String.parse_str(
		p_element,
		"<element><![CDATA[<literal />]]></element>",
	)?

	result
		== Element(
			"element",
			[],
			[Text("<literal />")],
		)
}

## Partial CDATA closing text is preserved until the real close marker.
expect {
	result = String.parse_str(
		p_element,
		"<element><![CDATA[this is ]] not ]> the end]]></element>",
	)?

	result
		== Element(
			"element",
			[],
			[Text("this is ]] not ]> the end")],
		)
}

## Nested elements parse into child nodes.
expect {
	result = String.parse_str(
		p_element,
		"<parent><child /></parent>",
	)?

	result == Element("parent", [], [Element("child", [], [])])
}

## Nested elements preserve attributes on parent and child nodes.
expect {
	result = String.parse_str(
		p_element,
		\\<parent argParent="outer"><child argChild="inner" /></parent>
		,
	)?

	result
		== Element(
			"parent",
			[
				{ name: "argParent", value: "outer" },
			],
			[
				Element(
					"child",
					[
						{ name: "argChild", value: "inner" },
					],
					[],
				),
			],
		)
}

## Nested element parsing preserves whitespace text nodes.
expect {
	result = String.parse_str(
		p_element,
		\\<parent>
		\\    <child />
		\\</parent>
		,
	)?

	result
		== Element(
			"parent",
			[],
			[
				Text("\n    "),
				Element("child", [], []),
				Text("\n"),
			],
		)
}

## Elements can parse a diverse set of child nodes.
expect {
	result = String.parse_str(
		p_element,
		\\<feed xmlns="http://www.w3.org/2005/Atom">
		\\    <title>Atom Feed</title>
		\\    <link rel="self" type="application/atom+xml" href="http://example.org" />
		\\    <updated>2024-02-23T20:38:24Z</updated>
		\\</feed>
		,
	)?

	result
		== Element(
			"feed",
			[{ name: "xmlns", value: "http://www.w3.org/2005/Atom" }],
			[
				Text("\n    "),
				Element("title", [], [Text("Atom Feed")]),
				Text("\n    "),
				Element(
					"link",
					[
						{ name: "rel", value: "self" },
						{ name: "type", value: "application/atom+xml" },
						{ name: "href", value: "http://example.org" },
					],
					[],
				),
				Text("\n    "),
				Element(
					"updated",
					[],
					[
						Text("2024-02-23T20:38:24Z"),
					],
				),
				Text("\n"),
			],
		)
}

p_element_attribute : Parser(String.Utf8, Xml.Attribute)
p_element_attribute =
	Parser.const(
		|name| {
			|value| {
				{
					name,
					value,
				}
			}
		},
	)
		.keep(p_name)
		.skip(p_equal)
		.keep(
			Parser.one_of([
				p_attribute_value('"').between(String.string("\""), String.string("\"")),
				p_attribute_value('\'').between(String.string("'"), String.string("'")),
			]),
		)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-AttValue
p_attribute_value : U8 -> Parser(String.Utf8, Str)
p_attribute_value = |quote| {
	Parser.chomp_while(
		|c| {
			c != quote
		},
	)
		.map(
			|chomped| {
				str_from_utf8(chomped)
			},
		)
		.flatten()
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-ETag
p_end_tag : Parser(String.Utf8, Str)
p_end_tag =
	Parser.const(
		|name| {
			name
		},
	)
		.skip(String.string("</"))
		.keep(p_name)
		.skip(p_whitespace.many())
		.skip(String.string(">"))

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CharData
p_character_data : Parser(String.Utf8, Xml.Node)
p_character_data =
	Parser.const(
		|first| {
			|chars| {
				combine_to_str(first, chars)
			}
		},
	)
		.keep(String.codeunit_satisfies(is_character_data))
		.keep(Parser.chomp_while(is_character_data))
		.flatten()
		.map(|s| Text(s))

is_character_data : U8 -> Bool
is_character_data = |c| {
	(c != '<')
		and (c != '&')
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CDSect
p_cdata_section : Parser(String.Utf8, Xml.Node)
p_cdata_section =
	Parser.const(
		|text| {
			text
		},
	)
		.skip(String.string("<![CDATA["))
		.keep(p_cdata_section_content)
		.map(|s| Text(s))

p_cdata_section_content : Parser(String.Utf8, Str)
p_cdata_section_content = Parser.build_primitive_parser(parse_cdata_section_content_partial)

parse_cdata_section_content_partial : String.Utf8 -> Parser.ParseResult(String.Utf8, Str)
parse_cdata_section_content_partial = |input| {
	parser = Parser.const(
		|first| {
			|rest| {
				Str.concat(first, rest)
			}
		},
	)
		.keep(Parser.chomp_until(']').map(str_from_utf8).flatten())
		.skip(String.string("]"))
		.keep(
			Parser.one_of([
				String.string("]>").map(
					|_| {
						""
					},
				),
				Parser.build_primitive_parser(parse_cdata_section_content_partial).map(
					|rest| {
						Str.concat("]", rest)
					},
				),
			]),
		)

	Parser.parse_partial(parser, input)
}

p_name : Parser(String.Utf8, Str)
p_name =
	Parser.const(|first_char| |rest| combine_to_str(first_char, rest))
		.keep(String.codeunit_satisfies(is_name_start_char))
		.keep(Parser.chomp_while(is_name_char))
		.flatten()

is_name_start_char : U8 -> Bool
is_name_start_char = |c| {
	is_alphabetical(c)
		or (c == ':')
			or (c == '_')
}

is_name_char : U8 -> Bool
is_name_char = |c| {
	is_name_start_char(c)
		or (c == '-')
			or (c == '.')
}

combine_to_str : U8, List(U8) -> Try(Str, Str)
combine_to_str = |first, rest| {
	rest
		.prepend(first)
		|> str_from_utf8
}

str_from_utf8 : List(U8) -> Try(Str, Str)
str_from_utf8 = |chars| {
	Str.from_utf8(chars)
		.map_err(
			|_| {
				"Error decoding UTF8"
			},
		)
}

XmlMisc : List([Comment, ProcessingInstruction])

p_many_misc : Parser(String.Utf8, XmlMisc)
p_many_misc =
	p_whitespace.many()
		.map(|_| [])

p_attribute : Parser(String.Utf8, output), Str -> Parser(String.Utf8, output)
p_attribute = |parser, attribute_name| {
	Parser.const(
		|result| {
			result
		},
	)
		.skip(String.string(attribute_name))
		.skip(p_equal)
		.keep(parser)
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-Eq
p_equal : Parser(String.Utf8, Str)
p_equal =
	p_whitespace.many()
		.skip(String.string("="))
		.skip(p_whitespace.many())
		.map(
			|strings| {
				strings |> Str.join_with("")
			},
		)

between_quotes : Parser(String.Utf8, a) -> Parser(String.Utf8, a)
between_quotes = |parser| {
	Parser.one_of([
		parser.between(String.string("\""), String.string("\"")),
		parser.between(String.string("'"), String.string("'")),
	])
}

maybe_with_default : Parser(input, output), output -> Parser(input, output)
maybe_with_default = |parser, default| {
	Parser.alt(parser, Parser.const(default))
}

p_whitespace : Parser(String.Utf8, Str)
p_whitespace =
	Parser.one_of([
		String.string("\u(20)"),
		String.string("\u(9)"),
		String.string("\u(D)"),
		String.string("\u(A)"),
	])

is_alphabetical : U8 -> Bool
is_alphabetical = |c| {
	(c >= 'A' and c <= 'Z')
		or (c >= 'a' and c <= 'z')
}

is_digit : U8 -> Bool
is_digit = |c| {
	c >= '0' and c <= '9'
}

test_xml =
	\\<?xml version=\"1.0\" encoding=\"utf-8\"?>
	\\<root>
	\\    <element arg=\"value\" />
	\\</root>

trailing_whitespace_xml =
	\\<?xml version="1.0" encoding="UTF-8"?>
	\\<root><Example></Example></root>
	\\

## Full XML parsing ignores trailing whitespace after the root.
expect {
	result = String.parse_str(Xml.xml_parser, trailing_whitespace_xml)?

	expected : Xml
	expected = {
		xml_declaration: Given({
			version: v1_dot0,
			encoding: Given(OtherEncoding("UTF-8")),
		}),
		root: Element(
			"root",
			[],
			[
				Element("Example", [], []),
			],
		),
	}

	result == expected
}

## Malformed input ending in a multibyte scalar returns an error instead of crashing
## while rendering a parser failure from a mid-scalar byte position.
expect String.parse_str(Xml.xml_parser, "<ӿ").is_err()
