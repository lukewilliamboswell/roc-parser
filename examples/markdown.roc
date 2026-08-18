app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.22.0/F1JVZPYfWP71s8vk6tHcV1Qx1Ef6CZkwswGoCn8VHZmL.tar.zst",
	parser: "../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.String
import parser.Markdown

content =
	\\---
	\\title: Demo
	\\---
	\\# Title with **style**
	\\
	\\Intro with **bold**, ~~old text~~, `code`, ![alt](/image.png), and [a link](https://example.com).
	\\
	\\[roc]: https://roc-lang.org "Roc"
	\\Read [Roc][roc] and <https://example.com>.
	\\
	\\| Name | Count |
	\\| :--- | ---: |
	\\| **Roc** | `1|2` |
	\\
	\\```roc
	\\# some code
	\\foo = bar
	\\```
	\\
	\\- [x] One
	\\  - Nested
	\\
	\\1. First
	\\2. Second
	\\
	\\> Quote with **strong** text
	\\
	\\<section>
	\\raw
	\\</section>

main! : List(OsStr) => Try({}, _)
main! = |args| {
	markdown_input = args.get(1).map_ok(OsStr.display) ?? content
	parsed =
		String.parse_str(Markdown.all, markdown_input)
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
			buf

		[Heading({ level, content: inlines }), .. as rest] =>
			render_content(rest, buf.concat("HEADING: ${Str.inspect(level)} ${render_inlines(inlines, "")}\n"))

		[Paragraph(inlines), .. as rest] =>
			render_content(rest, buf.concat("PARAGRAPH: ${render_inlines(inlines, "")}\n"))

		[Blockquote(children), .. as rest] =>
			render_content(rest, buf.concat("BLOCKQUOTE:\n").concat(render_content(children, "")))

		[ListBlock({ kind, loose, items }), .. as rest] =>
			render_content(rest, buf.concat("LIST ${kind.to_str()} loose=${render_bool(loose)}:\n").concat(render_list_items(items, "")))

		[Code({ info, pre }), .. as rest] =>
			render_content(rest, buf.concat("CODE: info: ${Str.inspect(info)}, pre: ${Str.inspect(pre)}\n"))

		[ThematicBreak, .. as rest] =>
			render_content(rest, buf.concat("THEMATIC BREAK\n"))

		[Table({ header, align, rows }), .. as rest] =>
			render_content(rest, buf.concat("TABLE: ${render_cells(header)} | ${render_alignments(align)} | ${render_rows(rows)}\n"))

		[HtmlBlock(raw), .. as rest] =>
			render_content(rest, buf.concat("HTML: ${Str.inspect(raw)}\n"))

		[Frontmatter({ raw }), .. as rest] =>
			render_content(rest, buf.concat("FRONTMATTER: ${Str.inspect(raw)}\n"))

		[TODO(line), .. as rest] =>
			render_content(rest, buf.concat("TODO: ${line}\n"))
		}
}

render_list_items : List({ task : Markdown.TaskState, blocks : List(Markdown.Markdown) }), Str -> Str
render_list_items = |items, buf| {
	match items {
		[] =>
			buf

		[item, .. as rest] =>
			render_list_items(rest, buf.concat("- ${item.task.to_str()}\n").concat(render_content(item.blocks, "")))
		}
}

render_alignments : List(Markdown.Alignment) -> Str
render_alignments = |alignments| {
	join_strs(alignments.map(|alignment| alignment.to_str()), ",")
}

render_rows : List(List(List(Markdown.Inline))) -> Str
render_rows = |rows| {
	join_strs(rows.map(render_cells), ";")
}

render_cells : List(List(Markdown.Inline)) -> Str
render_cells = |cells| {
	join_strs(
		cells.map(
			|cell| {
				render_inlines(cell, "")
			},
		),
		"|",
	)
}

render_bool : Bool -> Str
render_bool = |value| {
	if value {
		"true"
	} else {
		"false"
	}
}

join_strs : List(Str), Str -> Str
join_strs = |items, separator| {
	join_strs_help(items, separator, "")
}

join_strs_help : List(Str), Str, Str -> Str
join_strs_help = |items, separator, acc| {
	match items {
		[] =>
			acc

		[item, .. as rest] if acc.is_empty() =>
			join_strs_help(rest, separator, item)

		[item, .. as rest] =>
			join_strs_help(rest, separator, acc.concat(separator).concat(item))
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

		[Strikethrough(children), .. as rest] =>
			render_inlines(rest, buf.concat("~~").concat(render_inlines(children, "")).concat("~~"))

		[InlineCode(code), .. as rest] =>
			render_inlines(rest, buf.concat("`").concat(code).concat("`"))

		[Link({ label, target }), .. as rest] =>
			render_inlines(rest, buf.concat("[").concat(render_inlines(label, "")).concat("](").concat(target.href).concat(")"))

		[Image({ alt, target }), .. as rest] =>
			render_inlines(rest, buf.concat("![").concat(render_inlines(alt, "")).concat("](").concat(target.href).concat(")"))

		[HardBreak, .. as rest] =>
			render_inlines(rest, buf.concat("\\n"))

		[HtmlInline(raw), .. as rest] =>
			render_inlines(rest, buf.concat(raw))
		}
}
