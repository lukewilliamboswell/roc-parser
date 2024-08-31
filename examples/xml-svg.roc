app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    html: "https://github.com/Hasnep/roc-html/releases/download/v0.6.0/IOyNfA4U_bCVBihrs95US9Tf5PGAWh3qvrBN4DRbK5c.tar.br",
    parser: "../package/main.roc",
}

import cli.Stdout
import parser.String
import parser.Xml
import html.Html
import html.Attribute

svgInput =
    """
    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-sort-up" viewBox="0 0 16 16"><path d="M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.5.5 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1z"/></svg>
    """

expectedHtml =
    """
    svg [
        xmlns "http://www.w3.org/2000/svg",
        width "16",
        height "16",
        fill "currentColor",
        class "bi bi-sort-up",
        viewBox "0 0 16 16"
    ] [
        path [
            d "M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.5.5 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1z"
        ] []
    ]
    """

main =

    svgConvertedToHtml =
        String.parseStr Xml.xmlParser svgInput
            |> Result.map \xml -> htmlToRocDSL (svgToHtml xml.root) "" 0
            |> Task.fromResult!

    if svgConvertedToHtml == expectedHtml then
        Stdout.line! "Successfully converted SVG into HTML DSL"
    else
        Stdout.line! "Did not match expected HTML DSL"

svgToHtml : Xml.Node -> Html.Node
svgToHtml = \xml ->
    when xml is
        Element name attrs children ->
            (Html.element name)
                (List.map attrs xmlToHtmlAttribute)
                (List.map children svgToHtml)

        Text text -> Html.text text

xmlToHtmlAttribute : { name : Str, value : Str } -> Attribute.Attribute
xmlToHtmlAttribute = \{ name, value } -> (Attribute.attribute name) value

htmlToRocDSL : Html.Node, Str, U8 -> Str
htmlToRocDSL = \html, buf, depth ->

    mapChild = \child -> htmlToRocDSL child "    $(depthToIdent depth)" (depth + 1)
    mapAttr = \Attribute name value -> "    $(depthToIdent depth)$(name) \"$(value)\""

    when html is
        Element name _ attrs children ->
            formattedAttrs =
                if List.isEmpty attrs then "[]" else "[\n$(List.map attrs mapAttr |> Str.joinWith ",\n")\n$(depthToIdent depth)]"

            formattedChildren =
                if List.isEmpty children then "[]" else "[\n$(List.map children mapChild |> Str.joinWith ",\n")\n$(depthToIdent depth)]"

            "$(buf)$(name) $(formattedAttrs) $(formattedChildren)"

        Text text -> "$(buf)text \"$(text)\""
        UnescapedHtml _raw -> crash "UnescapedHtml not supported"

expect
    a = htmlToRocDSL (Html.text "foo") "" 0
    a == "text \"foo\""

expect
    a = htmlToRocDSL (Html.h1 [Attribute.class "green", Attribute.width "1rem"] [Html.text "foo"]) "" 0
    a
    ==
    """
    h1 [
        class \"green\",
        width \"1rem\"
    ] [
        text \"foo\"
    ]
    """

expect
    a = htmlToRocDSL (Html.h1 [] [Html.text "foo", Html.text "bar", Html.text "baz"]) "" 0
    a
    ==
    """
    h1 [] [
        text \"foo\",
        text \"bar\",
        text \"baz\"
    ]
    """

expect
    a = htmlToRocDSL (Html.h1 [] [Html.h2 [Attribute.class "green"] [Html.text "foo"], Html.text "bar"]) "" 0
    a
    ==
    """
    h1 [] [
        h2 [
            class \"green\"
        ] [
            text \"foo\"
        ],
        text \"bar\"
    ]
    """

depthToIdent = \depth ->
    List.range { start: At 0, end: Before depth }
    |> List.map \_ -> "    "
    |> Str.joinWith ""

expect depthToIdent 0 == ""
expect depthToIdent 1 == "    "
expect depthToIdent 2 == "        "
