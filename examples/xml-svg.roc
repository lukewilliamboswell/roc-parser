app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    # TODO replace with release URL after https://github.com/Hasnep/roc-html/pull/20 is merged
    # and a new release is published
    html: "https://github.com/lukewilliamboswell/roc-html/releases/download/testing0/cFRUcxD_hiFxkjG21muA4gIrPd9wePNkHY2FQoElXW4.tar.br",
    parser: "../package/main.roc",
}

import cli.Stdout
import parser.String
import parser.Xml
import html.Html
import html.Attribute

svg_input =
    """
    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-sort-up" viewBox="0 0 16 16"><path d="M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.5.5 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1z"/></svg>
    """

expected_html =
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

main! = |_args|

    svg_converted_to_html =
        Result.map_ok(
            String.parse_str(Xml.xml_parser, svg_input),
            |xml| html_to_roc_dsl(svg_to_html(xml.root), "", 0),
        )?

    if svg_converted_to_html == expected_html then
        Stdout.line!("Successfully converted SVG into HTML DSL")
    else
        Stdout.line!("Did not match expected HTML DSL")

svg_to_html : Xml.Node -> Html.Node
svg_to_html = |xml|
    when xml is
        Element(name, attrs, children) ->
            (Html.element(name))(
                List.map(attrs, xml_to_html_attribute),
                List.map(children, svg_to_html),
            )

        Text(text) -> Html.text(text)

xml_to_html_attribute : { name : Str, value : Str } -> Attribute.Attribute
xml_to_html_attribute = |{ name, value }| (Attribute.attribute(name))(value)

html_to_roc_dsl : Html.Node, Str, U8 -> Str
html_to_roc_dsl = |html, buf, depth|

    map_child = |child| html_to_roc_dsl(child, "    ${depth_to_ident(depth)}", (depth + 1))
    map_attr = |Attribute(name, value)| "    ${depth_to_ident(depth)}${name} \"${value}\""

    when html is
        Element(name, _, attrs, children) ->
            formatted_attrs =
                if List.is_empty(attrs) then "[]" else "[\n${List.map(attrs, map_attr) |> Str.join_with(",\n")}\n${depth_to_ident(depth)}]"

            formatted_children =
                if List.is_empty(children) then "[]" else "[\n${List.map(children, map_child) |> Str.join_with(",\n")}\n${depth_to_ident(depth)}]"

            "${buf}${name} ${formatted_attrs} ${formatted_children}"

        Text(text) -> "${buf}text \"${text}\""
        UnescapedHtml(_raw) -> crash("UnescapedHtml not supported")

expect
    a = html_to_roc_dsl(Html.text("foo"), "", 0)
    a == "text \"foo\""

expect
    a = html_to_roc_dsl(Html.h1([Attribute.class("green"), Attribute.width("1rem")], [Html.text("foo")]), "", 0)
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
    a = html_to_roc_dsl(Html.h1([], [Html.text("foo"), Html.text("bar"), Html.text("baz")]), "", 0)
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
    a = html_to_roc_dsl(Html.h1([], [Html.h2([Attribute.class("green")], [Html.text("foo")]), Html.text("bar")]), "", 0)
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

depth_to_ident = |depth|
    List.range({ start: At(0), end: Before(depth) })
    |> List.map(|_| "    ")
    |> Str.join_with("")

expect depth_to_ident(0) == ""
expect depth_to_ident(1) == "    "
expect depth_to_ident(2) == "        "
