app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.0.0/AnZoxzoGPtSGQ15EQh6pBeeaHJ7aizP9MQhK81dES3Uq.tar.zst",
	# TODO: point to the migrated html library
	html: "https://github.com/lukewilliamboswell/roc-html/...",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import cli.Stdout
import parser.String
import parser.Xml
import html.Html
import html.Attribute

svg_input = 
	\\<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-sort-up" viewBox="0 0 16 16"><path d="M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.5.5 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1z"/></svg>

expected_html = 
	\\svg [
	\\    xmlns "http://www.w3.org/2000/svg",
	\\    width "16",
	\\    height "16",
	\\    fill "currentColor",
	\\    class "bi bi-sort-up",
	\\    viewBox "0 0 16 16"
	\\] [
	\\    path [
	\\        d "M3.5 12.5a.5.5 0 0 1-1 0V3.707L1.354 4.854a.5.5 0 1 1-.708-.708l2-1.999.007-.007a.5.5 0 0 1 .7.006l2 2a.5.5 0 1 1-.707.708L3.5 3.707zm3.5-9a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M7.5 6a.5.5 0 0 0 0 1h5a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h3a.5.5 0 0 0 0-1zm0 3a.5.5 0 0 0 0 1h1a.5.5 0 0 0 0-1z"
	\\    ] []
	\\]

main! : List(Str) => Try({}, [Exit(I32), StdoutErr(Str), ..])
main! = |_args| {
	result = 
		String.parse_str(Xml.xml_parser, svg_input)
			.map_ok(
				|xml| {
					html_to_roc_dsl(svg_to_html(xml.root), "", 0)
				},
			)

	match result {
		Ok(svg_converted_to_html) if svg_converted_to_html == expected_html =>
			Stdout.line!("Successfully converted SVG into HTML DSL")?

		Ok(_) =>
			Stdout.line!("Did not match expected HTML DSL")?

		Err(_) =>
			Stdout.line!("Failed while parsing SVG")?
		}

	Ok({})
}

svg_to_html : Xml.Node -> Html.Node
svg_to_html = |xml| {
	match xml {
		Element(name, attrs, children) => {
			(Html.element(name))(
				attrs.map(xml_to_html_attribute),
				children.map(svg_to_html),
			)
		}

		Text(text) => Html.text(text)
	}
}

xml_to_html_attribute : { name : Str, value : Str } -> Attribute.Attribute
xml_to_html_attribute = |{ name, value }| {
	(Attribute.attribute(name))(value)
}

html_to_roc_dsl : Html.Node, Str, U8 -> Str
html_to_roc_dsl = |html, buf, depth| {
	map_child = |child| {
		html_to_roc_dsl(child, "    ${depth_to_ident(depth)}", (depth + 1))
	}
	map_attr = |Attribute(name, value)| {
		"    ${depth_to_ident(depth)}${name} \"${value}\""
	}

	match html {
		Element(name, _, attrs, children) => {
			formatted_attrs = 
				if attrs.is_empty() {
					"[]"
				} else {
					"[\n${attrs.map(map_attr).join_with(",\n")}\n${depth_to_ident(depth)}]"
				}

			formatted_children = 
				if children.is_empty() {
					"[]"
				} else {
					"[\n${children.map(map_child).join_with(",\n")}\n${depth_to_ident(depth)}]"
				}

			"${buf}${name} ${formatted_attrs} ${formatted_children}"
		}

		Text(text) => "${buf}text \"${text}\""
		UnescapedHtml(_raw) => {
			crash "UnescapedHtml not supported"
		}
	}
}

## Text nodes render as text calls.
expect {
	a = html_to_roc_dsl(Html.text("foo"), "", 0)
	a == "text \"foo\""
}

## Element attributes render in a bracketed attribute list.
expect {
	a = html_to_roc_dsl(Html.h1([Attribute.class("green"), Attribute.width("1rem")], [Html.text("foo")]), "", 0)
	a
		==
		\\h1 [
		\\    class \"green\",
		\\    width \"1rem\"
		\\] [
		\\    text \"foo\"
		\\]
}

## Multiple text children render on separate lines.
expect {
	a = html_to_roc_dsl(Html.h1([], [Html.text("foo"), Html.text("bar"), Html.text("baz")]), "", 0)
	a
		==
		\\h1 [] [
		\\    text \"foo\",
		\\    text \"bar\",
		\\    text \"baz\"
		\\]
}

## Nested elements increase the rendered indentation.
expect {
	a = html_to_roc_dsl(Html.h1([], [Html.h2([Attribute.class("green")], [Html.text("foo")]), Html.text("bar")]), "", 0)
	a
		==
		\\h1 [] [
		\\    h2 [
		\\        class \"green\"
		\\    ] [
		\\        text \"foo\"
		\\    ],
		\\    text \"bar\"
		\\]
}

depth_to_ident : U8 -> Str
depth_to_ident = |depth| {
	(0..<depth)
		.map(
			|_| {
				"    "
			},
		)
		.join_with("")
}

## Zero nesting renders no indentation.
expect depth_to_ident(0) == ""

## One nesting level renders four spaces.
expect depth_to_ident(1) == "    "

## Two nesting levels render eight spaces.
expect depth_to_ident(2) == "        "
