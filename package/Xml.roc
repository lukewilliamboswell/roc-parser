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
    xmlParser,
]

import Parser exposing [Parser, const, map, skip, keep, oneOrMore, oneOf, many, between, alt, chompWhile, flatten, lazy, chompUntil]
import String exposing [parseStr, string, Utf8, digits, codeunitSatisfies]

Xml : {
    xmlDeclaration : [Given XmlDeclaration, Missing],
    root : Node,
}

XmlDeclaration : {
    version : XmlVersion,
    encoding : [Given XmlEncoding, Missing],
}

XmlVersion := {
    afterDot : U8,
}
    implements [Eq]

v1Dot0 : XmlVersion
v1Dot0 = @XmlVersion {
    afterDot: 0,
}

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
    result = parseStr xmlParser testXml

    result
    == Ok {
        xmlDeclaration: Given {
            version: v1Dot0,
            encoding: Given Utf8Encoding,
        },
        root: Element "root" [] [
            Text "\n    ",
            Element
                "element"
                [{ name: "arg", value: "value" }]
                [],
            Text "\n",
        ],
    }

expect
    # XML with empty prolog to be parsed
    result = parseStr xmlParser "<element />"

    result
    == Ok {
        xmlDeclaration: Missing,
        root: Element "element" [] [],
    }

xmlParser : Parser Utf8 Xml
xmlParser =
    const
        (\xmlDeclaration -> \root -> {
                xmlDeclaration,
                root,
            }
        )
    |> keep pProlog
    |> keep pElement
    |> skip (many pWhitespace)

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-prolog
pProlog : Parser Utf8 [Given XmlDeclaration, Missing]
pProlog =
    const (\xmlDeclaration -> \_misc -> xmlDeclaration)
    |> keep (pXmlDeclaration |> map Given |> maybeWithDefault Missing)
    |> keep pManyMisc

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-XMLDecl
pXmlDeclaration : Parser Utf8 XmlDeclaration
pXmlDeclaration =
    (
        const
            (\version -> \encoding -> {
                    version,
                    encoding,
                })
    )
    |> skip (string "<?xml")
    |> skip (oneOrMore pWhitespace)
    |> keep pVersion
    |> keep
        (
            (
                const (\encoding -> encoding)
                |> skip (oneOrMore pWhitespace)
                |> keep pEncodingDeclaration
                |> map Given
            )
            |> maybeWithDefault Missing
        )
    |> skip (many pWhitespace)
    |> skip (string "?>")

expect
    # XML declaration to be parsed
    result =
        parseStr
            pXmlDeclaration
            """
            <?xml version="1.0" encoding="utf-8"?>
            """

    result
    == Ok {
        version: v1Dot0,
        encoding: Given Utf8Encoding,
    }

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionInfo
pVersion : Parser Utf8 XmlVersion
pVersion =
    betweenQuotes pVersionNumber
    |> pAttribute "version"

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-VersionNum
pVersionNumber : Parser Utf8 XmlVersion
pVersionNumber =
    const
        (\afterDot ->
            @XmlVersion {
                afterDot: afterDot |> Num.toU8,
            }
        )
    |> skip (string "1.")
    |> keep digits

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncodingDecl
pEncodingDeclaration : Parser Utf8 XmlEncoding
pEncodingDeclaration =
    betweenQuotes pEncodingName
    |> pAttribute "encoding"

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-EncName
pEncodingName : Parser Utf8 XmlEncoding
pEncodingName =
    const
        (\firstChar -> \rest ->
                combineToStr firstChar rest
                |> Result.map
                    (\encodingName ->

                        when encodingName is
                            "utf-8" -> Utf8Encoding
                            other -> OtherEncoding other
                    )
        )
    |> keep (codeunitSatisfies isAlphabetical)
    |> keep
        (
            chompWhile \c -> isAlphabetical c
                || isDigit c
                || (c == '-')
                || (c == '.')
                || (c == '_')
        )
    |> flatten

expect
    # encoding name to be parsed
    result = parseStr pEncodingName "utf-8"

    result == Ok Utf8Encoding

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-element
pElement : Parser Utf8 Node
pElement =
    const (\name -> \arguments -> \contents -> Element name arguments contents)
    |> skip (string "<")
    |> keep pName
    |> keep
        (
            many
                (
                    const (\attribute -> attribute)
                    |> skip (many pWhitespace)
                    |> keep pElementAttribute
                )
        )
    |> skip (many pWhitespace)
    |> keep
        (
            emptyTag =
                string "/>" |> map (\_ -> [])
            tagWithContent =
                const (\contents -> contents)
                |> skip (string ">")
                |> keep (lazy \_ -> pElementContents)
                |> skip pEndTag
            # Due to https://github.com/lukewilliamboswell/roc-parser/issues/13 we cannot use `oneOf`, since we are using oneOf in `pElementContents`.
            alt
                tagWithContent
                emptyTag
        )

expect
    # empty element tag without arguments to be parsed
    result = parseStr pElement "<element />"

    result == Ok (Element "element" [] [])

expect
    # empty element tag without arguments and without whitespace to be parsed
    result = parseStr pElement "<element/>"

    result == Ok (Element "element" [] [])

expect
    # empty element tag with argument to be parsed
    result = parseStr
        pElement
        """
        <element arg="value"/>
        """

    result == Ok (Element "element" [{ name: "arg", value: "value" }] [])

expect
    # empty element without arguments to be parsed
    result = parseStr pElement "<element></element>"

    result == Ok (Element "element" [] [])

# TODO: reject mismatched tags for better debugging
# expect
#     # mismatched end tag is rejected
#     result = parseStr pElement "<open></close>"

#     when result is
#         Err (ParsingFailure _) -> Bool.true
#         _ -> Bool.false

expect
    # element with multiple arguments and text content to be parsed
    result = parseStr
        pElement
        """
        <element firstArg="one" secondArg="two">text content</element>
        """

    result
    == Ok
        (
            Element
                "element"
                [
                    { name: "firstArg", value: "one" },
                    { name: "secondArg", value: "two" },
                ]
                [Text "text content"]
        )

expect
    # content with CDATA sections to be parsed
    result = parseStr
        pElement
        "<element><![CDATA[<literal />]]></element>"

    result
    == Ok
        (
            Element
                "element"
                []
                [Text "<literal />"]
        )

expect
    # CDATA section with partial CDATA section end tag to be parsed
    result = parseStr
        pElement
        "<element><![CDATA[this is ]] not ]> the end]]></element>"

    result
    == Ok
        (
            Element
                "element"
                []
                [Text "this is ]] not ]> the end"]
        )

expect
    # nested elements to be parsed
    result = parseStr
        pElement
        "<parent><child /></parent>"

    result == Ok (Element "parent" [] [Element "child" [] []])

expect
    # nested element with arguments to be parsed
    result = parseStr
        pElement
        """
        <parent argParent="outer"><child argChild="inner" /></parent>
        """

    result
    == Ok
        (
            Element
                "parent"
                [
                    { name: "argParent", value: "outer" },
                ]
                [
                    Element
                        "child"
                        [
                            { name: "argChild", value: "inner" },
                        ]
                        [],
                ]
        )

expect
    # nested elements with whitespace to be parsed
    result = parseStr
        pElement
        """
        <parent>
            <child />
        </parent>
        """

    result
    == Ok
        (
            Element
                "parent"
                []
                [
                    Text "\n    ",
                    Element "child" [] [],
                    Text "\n",
                ]
        )

expect
    # element with diverse children to be parsed
    result = parseStr
        pElement
        """
        <feed xmlns="http://www.w3.org/2005/Atom">
            <title>Atom Feed</title>
            <link rel="self" type="application/atom+xml" href="http://example.org" />
            <updated>2024-02-23T20:38:24Z</updated>
        </feed>
        """

    result
    == Ok
        (
            Element
                "feed"
                [{ name: "xmlns", value: "http://www.w3.org/2005/Atom" }]
                [
                    Text "\n    ",
                    Element "title" [] [Text "Atom Feed"],
                    Text "\n    ",
                    Element
                        "link"
                        [
                            { name: "rel", value: "self" },
                            { name: "type", value: "application/atom+xml" },
                            { name: "href", value: "http://example.org" },
                        ]
                        [],
                    Text "\n    ",
                    Element
                        "updated"
                        []
                        [
                            Text "2024-02-23T20:38:24Z",
                        ],
                    Text "\n",
                ]
        )

pElementAttribute : Parser Utf8 Attribute
pElementAttribute =
    const
        (\name -> \value -> {
                name,
                value,
            }
        )
    |> keep pName
    |> skip pEqual
    |> keep
        (
            oneOf [
                pAttributeValue '"' |> between (string "\"") (string "\""),
                pAttributeValue '\'' |> between (string "'") (string "'"),
            ]
        )

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-AttValue
pAttributeValue : U8 -> Parser Utf8 Str
pAttributeValue = \quote ->
    chompWhile \c -> c != quote
    |> map (\chomped -> strFromUtf8 chomped)
    |> flatten
# TODO: Implement reference values

pElementContents : Parser Utf8 (List Node)
pElementContents =
    many
        (
            oneOf [
                pCharacterData,
                pElement,
                pCdataSection,
            ]
        )

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-ETag
pEndTag : Parser Utf8 Str
pEndTag =
    const (\name -> name)
    |> skip (string "</")
    |> keep pName
    |> skip (many pWhitespace)
    |> skip (string ">")

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CharData
pCharacterData : Parser Utf8 Node
pCharacterData =
    const (\first -> \chars -> combineToStr first chars)
    |> keep (codeunitSatisfies isCharacterData)
    |> keep (chompWhile isCharacterData)
    |> flatten
    |> map (Text)
# TODO: Reject CDATA section close delimiter

isCharacterData : U8 -> Bool
isCharacterData = \c ->
    (c != '<')
    && (c != '&')

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-CDSect
pCdataSection : Parser Utf8 Node
pCdataSection =
    (
        const (\text -> text)
        |> skip (string "<![CDATA[")
        |> keep pCdataSectionContent
    )
    |> map Text

pCdataSectionContent : Parser Utf8 Str
pCdataSectionContent =
    const (\first -> \rest -> Str.concat first rest)
    |> keep (chompUntil ']' |> map strFromUtf8 |> flatten)
    |> skip (string "]")
    |> keep
        (
            oneOf [
                string "]>" |> map \_ -> "",
                lazy \_ -> pCdataSectionContent |> map \rest -> Str.concat "]" rest,
            ]
        )

pName : Parser Utf8 Str
pName =
    const
        (\firstChar -> \rest ->
                combineToStr firstChar rest
        )
    |> keep (codeunitSatisfies isNameStartChar)
    |> keep (chompWhile isNameChar)
    |> flatten

isNameStartChar : U8 -> Bool
isNameStartChar = \c ->
    isAlphabetical c
    || (c == ':')
    || (c == '_')
# TODO: Implement missing character groups

isNameChar : U8 -> Bool
isNameChar = \c ->
    isNameStartChar c
    || (c == '-')
    || (c == '.')

combineToStr : U8, List U8 -> Result Str Str
combineToStr = \first, rest ->
    rest
    |> List.prepend first
    |> strFromUtf8

strFromUtf8 : List U8 -> Result Str Str
strFromUtf8 = \chars ->
    Str.fromUtf8 chars
    |> Result.mapErr (\_ -> "Error decoding UTF8")

XmlMisc : List [Comment, ProcessingInstruction]

pManyMisc : Parser Utf8 XmlMisc
pManyMisc =
    # TODO: Implement comment and processing instructions
    many pWhitespace
    |> map (\_ -> [])

pAttribute : Parser Utf8 output, Str -> Parser Utf8 output
pAttribute = \parser, attributeName ->
    const (\result -> result)
    |> skip (string attributeName)
    |> skip pEqual
    |> keep parser

# See https://www.w3.org/TR/2008/REC-xml-20081126/#NT-Eq
pEqual : Parser Utf8 Str
pEqual =
    many pWhitespace
    |> skip (string "=")
    |> skip (many pWhitespace)
    |> map (\strings -> strings |> Str.joinWith "")

betweenQuotes : Parser Utf8 a -> Parser Utf8 a
betweenQuotes = \parser ->
    oneOf [
        parser |> between (string "\"") (string "\""),
        parser |> between (string "'") (string "'"),
    ]

maybeWithDefault : Parser input output, output -> Parser input output
maybeWithDefault = \parser, default ->
    alt parser (const default)

pWhitespace : Parser Utf8 Str
pWhitespace =
    oneOf [
        string "\u(20)",
        string "\u(9)",
        string "\u(D)",
        string "\u(A)",
    ]

isAlphabetical : U8 -> Bool
isAlphabetical = \c ->
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')

isDigit : U8 -> Bool
isDigit = \c ->
    c >= '0' && c <= '9'

testXml =
    """
    <?xml version=\"1.0\" encoding=\"utf-8\"?>
    <root>
        <element arg=\"value\" />
    </root>
    """

trailingWhitespaceXml =
    """
    <?xml version="1.0" encoding="UTF-8"?>
    <root><Example></Example></root>

    """

expect
    # ignore trailing newline
    result : Result Xml _
    result = parseStr xmlParser trailingWhitespaceXml

    expected : Xml
    expected = {
        xmlDeclaration: Given {
            version: v1Dot0,
            encoding: Given (OtherEncoding "UTF-8"),
        },
        root: Element "root" [] [
            Element "Example" [] [],
        ],
    }

    result == Ok expected
