app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Markdown

source =
	\\# Release notes
	\\
	\\Version **2** is out. Read the [changelog](/changes "All changes").
	\\
	\\## Fixed
	\\
	\\- Faster `parse`
	\\- Safer <b>HTML</b> & entities
	\\
	\\## Known issues
	\\
	\\None.

# tag::escape[]
escape : Str -> Str
escape = |text| {
	text
		.replace_each("&", "&amp;")
		.replace_each("<", "&lt;")
		.replace_each(">", "&gt;")
		.replace_each("\"", "&quot;")
}

# end::escape[]

# tag::inline[]
render_inlines : List(Markdown.Inline) -> Str
render_inlines = |inlines| {
	var $out = ""
	for inline in inlines {
		$out = $out.concat(render_inline(inline))
	}
	$out
}

render_inline : Markdown.Inline -> Str
render_inline = |inline| {
	match inline {
		Text(text) => escape(text)
		Strong(children) => "<strong>${render_inlines(children)}</strong>"
		Emphasis(children) => "<em>${render_inlines(children)}</em>"
		Strikethrough(children) => "<del>${render_inlines(children)}</del>"
		InlineCode(code) => "<code>${escape(code)}</code>"
		Link({ label, target }) => "<a href=\"${escape(target.href)}\"${title_attr(target.title)}>${render_inlines(label)}</a>"
		Image({ alt, target }) => "<img src=\"${escape(target.href)}\" alt=\"${escape(plain_text(alt))}\"${title_attr(target.title)} />"
		HardBreak => "<br />\n"
		# Raw HTML passes through unchanged; see the warning above.
		HtmlInline(raw) => raw
	}
}

title_attr : Try(Str, [Missing]) -> Str
title_attr = |title| {
	match title {
		Ok(text) => " title=\"${escape(text)}\""
		Err(Missing) => ""
	}
}

# end::inline[]

# tag::blocks[]
render_blocks : List(Markdown) -> Str
render_blocks = |blocks| {
	var $out = ""
	for block in blocks {
		$out = $out.concat(render_block(block))
	}
	$out
}

render_block : Markdown -> Str
render_block = |block| {
	match block {
		Heading({ level, content }) => {
			n = level.to_str()
			"<h${n}>${render_inlines(content)}</h${n}>\n"
		}

		Paragraph(inlines) => "<p>${render_inlines(inlines)}</p>\n"
		Blockquote(children) => "<blockquote>\n${render_blocks(children)}</blockquote>\n"
		ListBlock({ kind, loose, items }) => {
			tag =
				match kind {
					Unordered => "ul"
					Ordered(_) => "ol"
				}
			var $body = ""
			for item in items {
				# A tight list shows its paragraphs without <p> tags.
				inner =
					match item.blocks {
						[Paragraph(inlines)] if !loose => render_inlines(inlines)
						_ => render_blocks(item.blocks).trim()
					}
				$body = $body.concat("<li>${inner}</li>\n")
			}
			"<${tag}>\n${$body}</${tag}>\n"
		}

		Code({ pre, .. }) => "<pre><code>${escape(pre)}</code></pre>\n"
		ThematicBreak => "<hr />\n"
		HtmlBlock(raw) => raw
		# Tables, frontmatter and the remaining variants are left out of this sketch.
		_ => ""
	}
}

# end::blocks[]

# tag::toc[]
plain_text : List(Markdown.Inline) -> Str
plain_text = |inlines| {
	var $out = ""
	for inline in inlines {
		piece =
			match inline {
				Text(text) => text
				InlineCode(code) => code
				Strong(children) | Emphasis(children) | Strikethrough(children) => plain_text(children)
				Link({ label, .. }) => plain_text(label)
				Image({ alt, .. }) => plain_text(alt)
				HardBreak => " "
				HtmlInline(_) => ""
			}
		$out = $out.concat(piece)
	}
	$out
}

table_of_contents : List(Markdown) -> List(Str)
table_of_contents = |blocks| {
	var $lines = []
	for block in blocks {
		match block {
			Heading({ level, content }) => {
				indent =
					match level {
						One => ""
						Two => "  "
						_ => "    "
					}
				$lines = $lines.append("${indent}- ${plain_text(content)}")
			}

			_ => {}
		}
	}
	$lines
}

# end::toc[]

main! = |_args| {
	# tag::main[]
	blocks = Markdown.parse_str(source)
	Stdout.write!(render_blocks(blocks))?
	Stdout.line!("--- contents ---")?
	for line in table_of_contents(blocks) {
		Stdout.line!(line)?
	}
	# end::main[]
	Ok({})
}
