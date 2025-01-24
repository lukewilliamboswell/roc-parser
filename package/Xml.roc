## # XML Parser
## Original author: [Johannes Maas](https://github.com/j-maas)
##
## Following the specification from https://www.w3.org/TR/2008/REC-xml-20081126/
module [
    Xml,
    XmlDeclaration,
    XmlVersion,
    Node,
    Attribute,
    xml_parser,
]

import Parser exposing [Parser, const, map, skip, keep, one_or_more, one_of, many, between, alt, chomp_while, flatten, lazy, chomp_until]
import String exposing [parse_str, string, Utf8, digits, codeunit_satisfies]

Xml : {
    xml_declaration : [Given XmlDeclaration, Missing],
    root : Node,
}

XmlDeclaration : {
    version : XmlVersion,
    encoding : [Given XmlEncoding, Missing],
}

XmlVersion := {
    after_dot : U8,
}
    implements [Eq]

v1_dot0 : XmlVersion
v1_dot0 = @XmlVersion(
    {
        after_dot: 0,
    },
)

XmlEncoding : [
    Utf8Encoding,
    OtherEncoding Str,
]

Node : [
    Element Str (List Attribute) (List Node),
    Text Str,
]

Attribute : { name : Str, value : Str }

expect
    # xml to be parsed
    result = parse_str(xml_parser, test_xml)

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

expect
    # XML with empty prolog to be parsed
    result = parse_str(xml_parser, "<element />")

    result
    == Ok(
        {
            xml_declaration: Missing,
            root: Element("element", [], []),
        },
    )

xml_parser : Parser Utf8 Xml
xml_parser =
    const(
        |xml_declaration|
            |root| {
                xml_declaration,
                root,
            },
    )
    |> keep(p_prolog)
    |> keep(p_element)
    |> skip(many(p_whitespace))

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-prolog
p_prolog : Parser Utf8 [Given XmlDeclaration, Missing]
p_prolog =
    const(|xml_declaration| |_misc| xml_declaration)
    |> keep((p_xml_declaration |> map(Given) |> maybe_with_default(Missing)))
    |> keep(p_many_misc)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-XMLDecl
p_xml_declaration : Parser Utf8 XmlDeclaration
p_xml_declaration =
    (
        const(
            |version|
                |encoding| {
                    version,
                    encoding,
                },
        )
    )
    |> skip(string("<?xml"))
    |> skip(one_or_more(p_whitespace))
    |> keep(p_version)
    |> keep(
        (
            (
                const(|encoding| encoding)
                |> skip(one_or_more(p_whitespace))
                |> keep(p_encoding_declaration)
                |> map(Given)
            )
            |> maybe_with_default(Missing)
        ),
    )
    |> skip(many(p_whitespace))
    |> skip(string("?>"))

expect
    # XML declaration to be parsed
    result =
        parse_str(
            p_xml_declaration,
            """
            <?xml version="1.0" encoding="utf-8"?>
            """,
        )

    result
    == Ok(
        {
            version: v1_dot0,
            encoding: Given(Utf8Encoding),
        },
    )

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionInfo
p_version : Parser Utf8 XmlVersion
p_version =
    between_quotes(p_version_number)
    |> p_attribute("version")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionNum
p_version_number : Parser Utf8 XmlVersion
p_version_number =
    const(
        |after_dot|
            @XmlVersion(
                {
                    after_dot: after_dot |> Num.to_u8,
                },
            ),
    )
    |> skip(string("1."))
    |> keep(digits)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncodingDecl
p_encoding_declaration : Parser Utf8 XmlEncoding
p_encoding_declaration =
    between_quotes(p_encoding_name)
    |> p_attribute("encoding")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncName
p_encoding_name : Parser Utf8 XmlEncoding
p_encoding_name =
    const(
        |first_char|
            |rest|
                combine_to_str(first_char, rest)
                |> Result.map_ok(
                    |encoding_name|

                        when encoding_name is
                            "utf-8" -> Utf8Encoding
                            other -> OtherEncoding(other),
                ),
    )
    |> keep(codeunit_satisfies(is_alphabetical))
    |> keep(
        chomp_while(
            |c|
                is_alphabetical(c)
                or is_digit(c)
                or (c == '-')
                or (c == '.')
                or (c == '_'),
        ),
    )
    |> flatten

expect
    # encoding name to be parsed
    result = parse_str(p_encoding_name, "utf-8")

    result == Ok(Utf8Encoding)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-element
p_element : Parser Utf8 Node
p_element =
    const(|name| |arguments| |contents| Element(name, arguments, contents))
    |> skip(string("<"))
    |> keep(p_name)
    |> keep(
        many(
            (
                const(|attribute| attribute)
                |> skip(many(p_whitespace))
                |> keep(p_element_attribute)
            ),
        ),
    )
    |> skip(many(p_whitespace))
    |> keep(
        (
            empty_tag =
                string("/>") |> map(|_| [])
            tag_with_content =
                const(|contents| contents)
                |> skip(string(">"))
                |> keep(lazy(|_| p_element_contents))
                |> skip(p_end_tag)
            # Due to https://github.com/lukewilliamboswell/roc-parser/issues/13 we cannot use `oneOf`, since we are using oneOf in `pElementContents`.
            alt(
                tag_with_content,
                empty_tag,
            )
        ),
    )

expect
    # empty element tag without arguments to be parsed
    result = parse_str(p_element, "<element />")

    result == Ok(Element("element", [], []))

expect
    # empty element tag without arguments and without whitespace to be parsed
    result = parse_str(p_element, "<element/>")

    result == Ok(Element("element", [], []))

expect
    # empty element tag with argument to be parsed
    result = parse_str(
        p_element,
        """
        <element arg="value"/>
        """,
    )

    result == Ok(Element("element", [{ name: "arg", value: "value" }], []))

expect
    # empty element without arguments to be parsed
    result = parse_str(p_element, "<element></element>")

    result == Ok(Element("element", [], []))

# TODO: reject mismatched tags for better debugging
# expect
#     # mismatched end tag is rejected
#     result = parseStr pElement "<open></close>"

#     when result is
#         Err (ParsingFailure _) -> Bool.true
#         _ -> Bool.false

expect
    # element with multiple arguments and text content to be parsed
    result = parse_str(
        p_element,
        """
        <element firstArg="one" secondArg="two">text content</element>
        """,
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

expect
    # content with CDATA sections to be parsed
    result = parse_str(
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

expect
    # CDATA section with partial CDATA section end tag to be parsed
    result = parse_str(
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

expect
    # nested elements to be parsed
    result = parse_str(
        p_element,
        "<parent><child /></parent>",
    )

    result == Ok(Element("parent", [], [Element("child", [], [])]))

expect
    # nested element with arguments to be parsed
    result = parse_str(
        p_element,
        """
        <parent argParent="outer"><child argChild="inner" /></parent>
        """,
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

expect
    # nested elements with whitespace to be parsed
    result = parse_str(
        p_element,
        """
        <parent>
            <child />
        </parent>
        """,
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

expect
    # element with diverse children to be parsed
    result = parse_str(
        p_element,
        """
        <feed xmlns="http://www.w3.org/2005/Atom">
            <title>Atom Feed</title>
            <link rel="self" type="application/atom+xml" href="http://example.org" />
            <updated>2024-02-23T20:38:24Z</updated>
        </feed>
        """,
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

p_element_attribute : Parser Utf8 Attribute
p_element_attribute =
    const(
        |name|
            |value| {
                name,
                value,
            },
    )
    |> keep(p_name)
    |> skip(p_equal)
    |> keep(
        one_of(
            [
                p_attribute_value('"') |> between(string("\""), string("\"")),
                p_attribute_value('\'') |> between(string("'"), string("'")),
            ],
        ),
    )

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-AttValue
p_attribute_value : U8 -> Parser Utf8 Str
p_attribute_value = |quote|
    chomp_while(|c| c != quote)
    |> map(|chomped| str_from_utf8(chomped))
    |> flatten
# TODO: Implement reference values

p_element_contents : Parser Utf8 (List Node)
p_element_contents =
    many(
        one_of(
            [
                p_character_data,
                p_element,
                p_cdata_section,
            ],
        ),
    )

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-ETag
p_end_tag : Parser Utf8 Str
p_end_tag =
    const(|name| name)
    |> skip(string("</"))
    |> keep(p_name)
    |> skip(many(p_whitespace))
    |> skip(string(">"))

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CharData
p_character_data : Parser Utf8 Node
p_character_data =
    const(|first| |chars| combine_to_str(first, chars))
    |> keep(codeunit_satisfies(is_character_data))
    |> keep(chomp_while(is_character_data))
    |> flatten
    |> map(Text)
# TODO: Reject CDATA section close delimiter

is_character_data : U8 -> Bool
is_character_data = |c|
    (c != '<')
    and (c != '&')

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CDSect
p_cdata_section : Parser Utf8 Node
p_cdata_section =
    (
        const(|text| text)
        |> skip(string("<![CDATA["))
        |> keep(p_cdata_section_content)
    )
    |> map(Text)

p_cdata_section_content : Parser Utf8 Str
p_cdata_section_content =
    const(|first| |rest| Str.concat(first, rest))
    |> keep((chomp_until(']') |> map(str_from_utf8) |> flatten))
    |> skip(string("]"))
    |> keep(
        one_of(
            [
                string("]>") |> map(|_| ""),
                lazy(|_| p_cdata_section_content |> map(|rest| Str.concat("]", rest))),
            ],
        ),
    )

p_name : Parser Utf8 Str
p_name =
    const(
        |first_char|
            |rest|
                combine_to_str(first_char, rest),
    )
    |> keep(codeunit_satisfies(is_name_start_char))
    |> keep(chomp_while(is_name_char))
    |> flatten

is_name_start_char : U8 -> Bool
is_name_start_char = |c|
    is_alphabetical(c)
    or (c == ':')
    or (c == '_')
# TODO: Implement missing character groups

is_name_char : U8 -> Bool
is_name_char = |c|
    is_name_start_char(c)
    or (c == '-')
    or (c == '.')

combine_to_str : U8, List U8 -> Result Str Str
combine_to_str = |first, rest|
    rest
    |> List.prepend(first)
    |> str_from_utf8

str_from_utf8 : List U8 -> Result Str Str
str_from_utf8 = |chars|
    Str.from_utf8(chars)
    |> Result.map_err(|_| "Error decoding UTF8")

XmlMisc : List [Comment, ProcessingInstruction]

p_many_misc : Parser Utf8 XmlMisc
p_many_misc =
    # TODO: Implement comment and processing instructions
    many(p_whitespace)
    |> map(|_| [])

p_attribute : Parser Utf8 output, Str -> Parser Utf8 output
p_attribute = |parser, attribute_name|
    const(|result| result)
    |> skip(string(attribute_name))
    |> skip(p_equal)
    |> keep(parser)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-Eq
p_equal : Parser Utf8 Str
p_equal =
    many(p_whitespace)
    |> skip(string("="))
    |> skip(many(p_whitespace))
    |> map(|strings| strings |> Str.join_with(""))

between_quotes : Parser Utf8 a -> Parser Utf8 a
between_quotes = |parser|
    one_of(
        [
            parser |> between(string("\""), string("\"")),
            parser |> between(string("'"), string("'")),
        ],
    )

maybe_with_default : Parser input output, output -> Parser input output
maybe_with_default = |parser, default|
    alt(parser, const(default))

p_whitespace : Parser Utf8 Str
p_whitespace =
    one_of(
        [
            string("\u(20)"),
            string("\u(9)"),
            string("\u(D)"),
            string("\u(A)"),
        ],
    )

is_alphabetical : U8 -> Bool
is_alphabetical = |c|
    (c >= 'A' and c <= 'Z')
    or (c >= 'a' and c <= 'z')

is_digit : U8 -> Bool
is_digit = |c|
    c >= '0' and c <= '9'

test_xml =
    """
    <?xml version=\"1.0\" encoding=\"utf-8\"?>
    <root>
        <element arg=\"value\" />
    </root>
    """

trailing_whitespace_xml =
    """
    <?xml version="1.0" encoding="UTF-8"?>
    <root><Example></Example></root>

    """

expect
    # ignore trailing newline
    result : Result Xml _
    result = parse_str(xml_parser, trailing_whitespace_xml)

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
