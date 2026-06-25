# # # XML Parser
# # Original author: [Johannes Maas](https://github.com/j-maas)
# #
# # Following the specification from https://www.w3.org/TR/2008/REC-xml-20081126/
import Parser
import String

# TODO: bring back into Xml once https://github.com/roc-lang/roc/issues/9796 is resolved
import XmlNode
import XmlVersion

Xml :: {
	xml_declaration : [
		Given(
			{
				version : XmlVersion,
				encoding : [Given([Utf8Encoding, OtherEncoding(Str)]), Missing],
			},
		),
		Missing,
	],
	root : XmlNode,
}.{
	Attribute : { name : Str, value : Str }

	Declaration : {
		version : XmlVersion,
		encoding : [Given(Encoding), Missing],
	}

	Encoding : [
		Utf8Encoding,
		OtherEncoding(Str),
	]

	xml_parser : Parser(String.Utf8, Xml)
	xml_parser = 
		Parser.const(
			|xml_declaration| |root| {
				{ xml_declaration, root }
			},
		)
			.keep(p_prolog)
			.keep(p_element)
			.skip(p_whitespace.many())
}

v1_dot0 : XmlVersion
v1_dot0 = {
	XmlVersion.new(0)
}

expect {
	# xml to be parsed
	result = String.parse_str(Xml.xml_parser, test_xml)

	result
		== Ok(
			{
				xml_declaration: Given(
					{
						version: v1_dot0,
						encoding: Given(Utf8Encoding),
					},
				),
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
			},
		)
}

expect {
	# XML with empty prolog to be parsed
	result = String.parse_str(Xml.xml_parser, "<element />")

	result
		== Ok(
			{
				xml_declaration: Missing,
				root: Element("element", [], []),
			},
		)
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
		.keep(p_xml_declaration.map(|a| Given(a))->maybe_with_default(Missing))
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
				->maybe_with_default(Missing),
		)
		.skip(p_whitespace.many())
		.skip(String.string("?>"))

expect {
	# XML declaration to be parsed
	result = 
		String.parse_str(
			p_xml_declaration,
			\\<?xml version="1.0" encoding="utf-8"?>
			,
		)

	result
		== Ok(
			{
				version: v1_dot0,
				encoding: Given(Utf8Encoding),
			},
		)
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionInfo
p_version : Parser(String.Utf8, XmlVersion)
p_version = 
	p_version_number
		->between_quotes()
		->p_attribute("version")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionNum
p_version_number : Parser(String.Utf8, XmlVersion)
p_version_number = 
	Parser.const(
		|after_dot| {
			XmlVersion.new(U64.to_u8_wrap(after_dot)) # TODO: change to to_u8_try
		},
	)
		.skip(String.string("1."))
		.keep(String.digits)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncodingDecl
p_encoding_declaration : Parser(String.Utf8, Xml.Encoding)
p_encoding_declaration = 
	p_encoding_name
		->between_quotes()
		->p_attribute("encoding")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncName
p_encoding_name : Parser(String.Utf8, Xml.Encoding)
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

expect {
	# encoding name to be parsed
	result = String.parse_str(p_encoding_name, "utf-8")

	result == Ok(Utf8Encoding)
}

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-element
p_element : Parser(String.Utf8, XmlNode)
p_element = 
	Parser.const(
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
						Parser.lazy(
							|_| {
								p_element_contents
							},
						),
					)
					.skip(p_end_tag),
				String.string("/>").map(
					|_| {
						[]
					},
				),
			),
		)

expect {
	# empty element tag without arguments to be parsed
	result = String.parse_str(p_element, "<element />")

	result == Ok(Element("element", [], []))
}

expect {
	# empty element tag without arguments and without whitespace to be parsed
	result = String.parse_str(p_element, "<element/>")

	result == Ok(Element("element", [], []))
}

expect {
	# empty element tag with argument to be parsed
	result = String.parse_str(
		p_element,
		\\<element arg="value"/>
		,
	)

	result == Ok(Element("element", [{ name: "arg", value: "value" }], []))
}

expect {
	# empty element without arguments to be parsed
	result = String.parse_str(p_element, "<element></element>")

	result == Ok(Element("element", [], []))
}

expect {
	# element with multiple arguments and text content to be parsed
	result = String.parse_str(
		p_element,
		\\<element firstArg="one" secondArg="two">text content</element>
		,
	)

	result
		== Ok(
			Element(
				"element",
				[
					{ name: "firstArg", value: "one" },
					{ name: "secondArg", value: "two" },
				],
				[Text("text content")],
			),
		)
}

expect {
	# content with CDATA sections to be parsed
	result = String.parse_str(
		p_element,
		"<element><![CDATA[<literal />]]></element>",
	)

	result
		== Ok(
			Element(
				"element",
				[],
				[Text("<literal />")],
			),
		)
}

expect {
	# CDATA section with partial CDATA section end tag to be parsed
	result = String.parse_str(
		p_element,
		"<element><![CDATA[this is ]] not ]> the end]]></element>",
	)

	result
		== Ok(
			Element(
				"element",
				[],
				[Text("this is ]] not ]> the end")],
			),
		)
}

expect {
	# nested elements to be parsed
	result = String.parse_str(
		p_element,
		"<parent><child /></parent>",
	)

	result == Ok(Element("parent", [], [Element("child", [], [])]))
}

expect {
	# nested element with arguments to be parsed
	result = String.parse_str(
		p_element,
		\\<parent argParent="outer"><child argChild="inner" /></parent>
		,
	)

	result
		== Ok(
			Element(
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
			),
		)
}

expect {
	# nested elements with whitespace to be parsed
	result = String.parse_str(
		p_element,
		\\<parent>
		\\    <child />
		\\</parent>
		,
	)

	result
		== Ok(
			Element(
				"parent",
				[],
				[
					Text("\n    "),
					Element("child", [], []),
					Text("\n"),
				],
			),
		)
}

expect {
	# element with diverse children to be parsed
	result = String.parse_str(
		p_element,
		\\<feed xmlns="http://www.w3.org/2005/Atom">
		\\    <title>Atom Feed</title>
		\\    <link rel="self" type="application/atom+xml" href="http://example.org" />
		\\    <updated>2024-02-23T20:38:24Z</updated>
		\\</feed>
		,
	)

	result
		== Ok(
			Element(
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
			),
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
			Parser.one_of(
				[
					p_attribute_value('"').between(String.string("\""), String.string("\"")),
					p_attribute_value('\'').between(String.string("'"), String.string("'")),
				],
			),
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

p_element_contents : Parser(String.Utf8, List(XmlNode))
p_element_contents = 
	Parser.one_of(
		[
			p_character_data,
			p_element,
			p_cdata_section,
		],
	)
		.many()

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
p_character_data : Parser(String.Utf8, XmlNode)
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
p_cdata_section : Parser(String.Utf8, XmlNode)
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
p_cdata_section_content = 
	Parser.const(
		|first| {
			|rest| {
				Str.concat(first, rest)
			}
		},
	)
		.keep(Parser.chomp_until(']').map(str_from_utf8).flatten())
		.skip(String.string("]"))
		.keep(
			Parser.one_of(
				[
					String.string("]>").map(
						|_| {
							""
						},
					),
					Parser.lazy(
						|_| {
							p_cdata_section_content.map(
								|rest| {
									Str.concat("]", rest)
								},
							)
						},
					),
				],
			),
		)

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
		->str_from_utf8()
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
				strings->Str.join_with("")
			},
		)

between_quotes : Parser(String.Utf8, a) -> Parser(String.Utf8, a)
between_quotes = |parser| {
	Parser.one_of(
		[
			parser.between(String.string("\""), String.string("\"")),
			parser.between(String.string("'"), String.string("'")),
		],
	)
}

maybe_with_default : Parser(input, output), output -> Parser(input, output)
maybe_with_default = |parser, default| {
	Parser.alt(parser, Parser.const(default))
}

p_whitespace : Parser(String.Utf8, Str)
p_whitespace = 
	Parser.one_of(
		[
			String.string("\u(20)"),
			String.string("\u(9)"),
			String.string("\u(D)"),
			String.string("\u(A)"),
		],
	)

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

expect {
	# ignore trailing newline
	result : Try(Xml, _)
	result = String.parse_str(Xml.xml_parser, trailing_whitespace_xml)

	expected : Xml
	expected = {
		xml_declaration: Given(
			{
				version: v1_dot0,
				encoding: Given(OtherEncoding("UTF-8")),
			},
		),
		root: Element(
			"root",
			[],
			[
				Element("Example", [], []),
			],
		),
	}

	result == Ok(expected)
}
