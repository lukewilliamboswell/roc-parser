app [main!] {
	cli: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/1.0.0/AnZoxzoGPtSGQ15EQh6pBeeaHJ7aizP9MQhK81dES3Uq.tar.zst",
	parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.11.0/HS5cXN8JrJKdxM2Y8azXzbHCxCx2qxocySTGr6sLGQTZ.tar.zst",
}

import cli.Stdout
import parser.String
import parser.Markdown

content : Str
content =
	\\# Title with **style**
	\\
	\\Intro with **bold**, `code`, and [a link](https://example.com).
	\\
	\\[roc website](https://roc-lang.org)
	\\
	\\![alt text](/images/logo.png)
	\\
	\\---
	\\
	\\## Sub-title
	\\
	\\```roc
	\\# some code
	\\foo = bar
	\\```
	\\
	\\- One
	\\  continued
	\\  - Nested
	\\
	\\1. First
	\\2. Second
	\\
	\\> Quote with **strong** text

main! : List(Str) => Try({}, [Exit(I32), StdoutErr(Str), ..])
main! = |_args| {
	parsed = 
		String.parse_str(Markdown.all, content)
			.map_ok(
				|nodes| {
					render_content(nodes, "")
				},
			)
			?? "PARSING ERROR"

	Stdout.line!(parsed)?
	Ok({})
}

render_content : List(Markdown.Markdown), Str -> Str
render_content = |nodes, buf| {
	match nodes {
		[] =>
			buf # base case

		[Heading(level, inlines), .. as rest] =>
			render_content(rest, buf.concat("HEADING: ${Str.inspect(level)} ${render_inlines(inlines, "")}\n"))

		[Paragraph(inlines), .. as rest] =>
			render_content(rest, buf.concat("PARAGRAPH: ${render_inlines(inlines, "")}\n"))

		[Blockquote(children), .. as rest] =>
			render_content(rest, buf.concat("BLOCKQUOTE:\n").concat(render_content(children, "")))

		[UnorderedList(items), .. as rest] =>
			render_content(rest, buf.concat("LIST:\n").concat(render_list_items(items, "")))

		[OrderedList(items), .. as rest] =>
			render_content(rest, buf.concat("ORDERED LIST:\n").concat(render_ordered_list_items(items, 1, "")))

		[ListItem(inlines, children), .. as rest] =>
			render_content(rest, buf.concat("ITEM: ${render_inlines(inlines, "")}\n").concat(render_content(children, "")))

		[HorizontalRule, .. as rest] =>
			render_content(rest, buf.concat("HORIZONTAL RULE\n"))

		[Link({ alt, href }), .. as rest] =>
			render_content(rest, buf.concat("LINK: alt: ${Str.inspect(alt)}, ref: ${Str.inspect(href)}\n"))

		[Image({ alt, href }), .. as rest] =>
			render_content(rest, buf.concat("IMAGE: alt: ${Str.inspect(alt)}, ref: ${Str.inspect(href)}\n"))

		[Code({ ext, pre }), .. as rest] =>
			render_content(rest, buf.concat("CODE: ext: ${Str.inspect(ext)}, pre: ${Str.inspect(pre)}\n"))

		[TODO(line), .. as rest] =>
			render_content(rest, buf.concat("TODO: ${line}\n"))
	}
}

render_list_items : List(Markdown.Markdown), Str -> Str
render_list_items = |items, buf| {
	match items {
		[] =>
			buf

		[ListItem(inlines, children), .. as rest] =>
			render_list_items(rest, buf.concat("- ${render_inlines(inlines, "")}\n").concat(render_content(children, "")))

		[other, .. as rest] =>
			render_list_items(rest, buf.concat(render_content([other], "")))
	}
}

render_ordered_list_items : List(Markdown.Markdown), U64, Str -> Str
render_ordered_list_items = |items, index, buf| {
	match items {
		[] =>
			buf

		[ListItem(inlines, children), .. as rest] =>
			render_ordered_list_items(rest, index + 1, buf.concat("${index.to_str()}. ${render_inlines(inlines, "")}\n").concat(render_content(children, "")))

		[other, .. as rest] =>
			render_ordered_list_items(rest, index, buf.concat(render_content([other], "")))
	}
}

render_inlines : List(Markdown.Inline), Str -> Str
render_inlines = |inlines, buf| {
	match inlines {
		[] =>
			buf

		[Text(text), .. as rest] =>
			render_inlines(rest, buf.concat(text))

		[Strong(children), .. as rest] =>
			render_inlines(rest, buf.concat("**").concat(render_inlines(children, "")).concat("**"))

		[Emphasis(children), .. as rest] =>
			render_inlines(rest, buf.concat("*").concat(render_inlines(children, "")).concat("*"))

		[InlineCode(code), .. as rest] =>
			render_inlines(rest, buf.concat("`").concat(code).concat("`"))

		[InlineLink({ alt, href }), .. as rest] =>
			render_inlines(rest, buf.concat("[").concat(render_inlines(alt, "")).concat("](").concat(href).concat(")"))
	}
}
