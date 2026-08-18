import Parser
import String

## Markdown syntax tree and parsers for documents and inline content.
##
## The block parser preserves frontmatter, headings, paragraphs, blockquotes,
## lists, code blocks, thematic breaks, tables, and raw HTML. Inline parsing
## supports emphasis, links, images, code, hard breaks, and raw HTML.
Markdown := [
	Heading({ level : Markdown.Level, content : List(Markdown.Inline) }),
	Paragraph(List(Markdown.Inline)),
	Blockquote(List(Markdown)),
	ListBlock({ kind : Markdown.ListKind, loose : Bool, items : List({ task : Markdown.TaskState, blocks : List(Markdown) }) }),
	Code({ info : Str, pre : Str }),
	ThematicBreak,
	Table({ header : List(List(Markdown.Inline)), align : List(Markdown.Alignment), rows : List(List(List(Markdown.Inline))) }),
	HtmlBlock(Str),
	Frontmatter({ raw : Str }),
	TODO(Str),
].{

	## Render a Markdown block in Roc source-like notation for inspection.
	to_inspect : Markdown -> Str
	to_inspect = |node| {
		inspect_markdown(node)
	}

	## Compare two Markdown syntax trees structurally.
	is_eq : _

	## Render a Markdown block as a stable debug string.
	to_debug_str : Markdown -> Str
	to_debug_str = |node| {
		inspect_markdown(node)
	}

	## Render an inline node as a stable debug string.
	inline_to_debug_str : Markdown.Inline -> Str
	inline_to_debug_str = |inline| {
		inspect_inline(inline)
	}

	## Heading levels from one through six.
	Level := [One, Two, Three, Four, Five, Six].{

		## Render a heading level for inspection.
		to_inspect : Level -> Str
		to_inspect = |level| {
			inspect_level(level)
		}

		## Convert a heading level to its decimal representation.
		to_str : Level -> Str
		to_str = |level| {
			level_to_str(level)
		}

		## Compare two heading levels.
		is_eq : _
	}

	## The marker and starting number of a Markdown list.
	ListKind := [
		Unordered,
		Ordered({ start : U64 }),
	].{

		## Render a list kind for inspection.
		to_inspect : ListKind -> Str
		to_inspect = |kind| {
			inspect_list_kind(kind)
		}

		## Convert a list kind to a compact string.
		to_str : ListKind -> Str
		to_str = |kind| {
			list_kind_to_str(kind)
		}

		## Compare two list kinds structurally.
		is_eq : _
	}

	## Whether a list item is a task and, if so, whether it is checked.
	TaskState := [
		NoTask,
		Unchecked,
		Checked,
	].{

		## Render a task state for inspection.
		to_inspect : TaskState -> Str
		to_inspect = |task| {
			inspect_task_state(task)
		}

		## Convert a task state to a compact string.
		to_str : TaskState -> Str
		to_str = |task| {
			task_state_to_str(task)
		}

		## Compare two task states.
		is_eq : _
	}

	## Column alignment declared by a Markdown table delimiter row.
	Alignment := [
		Default,
		Left,
		Center,
		Right,
	].{

		## Render a table alignment for inspection.
		to_inspect : Alignment -> Str
		to_inspect = |alignment| {
			inspect_alignment(alignment)
		}

		## Convert a table alignment to a compact string.
		to_str : Alignment -> Str
		to_str = |alignment| {
			alignment_to_str(alignment)
		}

		## Compare two table alignments.
		is_eq : _
	}

	## Destination and optional title of a link or image.
	LinkTarget : {
		href : Str,
		title : [Some(Str), None],
	}

	## Inline Markdown syntax nodes.
	Inline := [
		Text(Str),
		Strong(List(Inline)),
		Emphasis(List(Inline)),
		Strikethrough(List(Inline)),
		InlineCode(Str),
		Link({ label : List(Inline), target : LinkTarget }),
		Image({ alt : List(Inline), target : LinkTarget }),
		HardBreak,
		HtmlInline(Str),
	].{

		## Render an inline node in Roc source-like notation for inspection.
		to_inspect : Inline -> Str
		to_inspect = |inline| {
			inspect_inline(inline)
		}

		## Compare two inline syntax trees structurally.
		is_eq : _
	}

	## Parse a complete Markdown document into block nodes.
	all : Parser(String.Utf8, List(Markdown))
	all = parse_all

	## Parse inline Markdown content.
	inlines : Parser(String.Utf8, List(Inline))
	inlines = parse_inlines_parser

	## Parse an ATX or Setext heading.
	heading : Parser(String.Utf8, Markdown)
	heading =
		Parser.one_of([
			inline_heading,
			two_line_heading_level_one,
			two_line_heading_level_two,
		])

	## Parse an inline link with a destination in parentheses.
	link : Parser(String.Utf8, Inline)
	link =
		Parser.const(
			|label| {
				|target| {
					Link({ label: parse_inlines(label.to_utf8()), target: parse_link_target(target.to_utf8()) })
				}
			},
		)
			.skip(String.string("["))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ']'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.string("]("))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ')'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.codeunit(')'))

	## Parse an inline image with a destination in parentheses.
	image : Parser(String.Utf8, Inline)
	image =
		Parser.const(
			|alt| {
				|target| {
					Image({ alt: parse_inlines(alt.to_utf8()), target: parse_link_target(target.to_utf8()) })
				}
			},
		)
			.skip(String.string("!["))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ']'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.string("]("))
			.keep(
				Parser.chomp_while(
					|b| {
						b != ')'
					},
				).map(String.str_from_utf8),
			)
			.skip(String.codeunit(')'))

	## Parse a fenced code block delimited by triple backticks.
	code : Parser(String.Utf8, Markdown)
	code =
		Parser.const(|info| |pre| Code({ info: info, pre: pre }))
			.keep(
				Parser.one_of([
					Parser.const(|i| i)
						.skip(String.string("```"))
						.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
						.skip(end_of_line),
					Parser.const("")
						.skip(String.string("```")),
				]),
			)
			.keep(chomp_until_code_block_end)
}

inspect_markdown : Markdown -> Str
inspect_markdown = |node| {
	match node {
		Heading({ level, content }) =>
			"Heading({ level: ${Str.inspect(level)}, content: ${Str.inspect(content)} })"

		Paragraph(content) =>
			"Paragraph(${Str.inspect(content)})"

		Blockquote(children) =>
			"Blockquote(${Str.inspect(children)})"

		ListBlock({ kind, loose, items }) =>
			"ListBlock({ kind: ${Str.inspect(kind)}, loose: ${Str.inspect(loose)}, items: ${Str.inspect(items)} })"

		Code({ info, pre }) =>
			"Code({ info: ${Str.inspect(info)}, pre: ${Str.inspect(pre)} })"

		ThematicBreak =>
			"ThematicBreak"

		Table({ header, align, rows }) =>
			"Table({ header: ${Str.inspect(header)}, align: ${Str.inspect(align)}, rows: ${Str.inspect(rows)} })"

		HtmlBlock(raw) =>
			"HtmlBlock(${Str.inspect(raw)})"

		Frontmatter({ raw }) =>
			"Frontmatter({ raw: ${Str.inspect(raw)} })"

		TODO(line) =>
			"TODO(${Str.inspect(line)})"
		}
}

inspect_level : Markdown.Level -> Str
inspect_level = |level| {
	match level {
		One => "One"
		Two => "Two"
		Three => "Three"
		Four => "Four"
		Five => "Five"
		Six => "Six"
	}
}

level_to_str : Markdown.Level -> Str
level_to_str = |level| {
	match level {
		One => "1"
		Two => "2"
		Three => "3"
		Four => "4"
		Five => "5"
		Six => "6"
	}
}

inspect_list_kind : Markdown.ListKind -> Str
inspect_list_kind = |kind| {
	match kind {
		Unordered =>
			"Unordered"

		Ordered({ start }) =>
			"Ordered({ start: ${start.to_str()} })"
		}
}

list_kind_to_str : Markdown.ListKind -> Str
list_kind_to_str = |kind| {
	match kind {
		Unordered =>
			"unordered"

		Ordered({ start }) =>
			"ordered:${start.to_str()}"
		}
}

inspect_task_state : Markdown.TaskState -> Str
inspect_task_state = |task| {
	match task {
		NoTask => "NoTask"
		Unchecked => "Unchecked"
		Checked => "Checked"
	}
}

task_state_to_str : Markdown.TaskState -> Str
task_state_to_str = |task| {
	match task {
		NoTask => "none"
		Unchecked => "unchecked"
		Checked => "checked"
	}
}

inspect_alignment : Markdown.Alignment -> Str
inspect_alignment = |alignment| {
	match alignment {
		Default => "Default"
		Left => "Left"
		Center => "Center"
		Right => "Right"
	}
}

alignment_to_str : Markdown.Alignment -> Str
alignment_to_str = |alignment| {
	match alignment {
		Default => "default"
		Left => "left"
		Center => "center"
		Right => "right"
	}
}

inspect_inline : Markdown.Inline -> Str
inspect_inline = |inline| {
	match inline {
		Text(text) =>
			"Text(${Str.inspect(text)})"

		Strong(children) =>
			"Strong(${Str.inspect(children)})"

		Emphasis(children) =>
			"Emphasis(${Str.inspect(children)})"

		Strikethrough(children) =>
			"Strikethrough(${Str.inspect(children)})"

		InlineCode(code) =>
			"InlineCode(${Str.inspect(code)})"

		Link({ label, target }) =>
			"Link({ label: ${Str.inspect(label)}, target: ${inspect_link_target(target)} })"

		Image({ alt, target }) =>
			"Image({ alt: ${Str.inspect(alt)}, target: ${inspect_link_target(target)} })"

		HardBreak =>
			"HardBreak"

		HtmlInline(raw) =>
			"HtmlInline(${Str.inspect(raw)})"
		}
}

inspect_link_target : Markdown.LinkTarget -> Str
inspect_link_target = |target| {
	"{ href: ${Str.inspect(target.href)}, title: ${inspect_link_title(target.title)} }"
}

inspect_link_title : [Some(Str), None] -> Str
inspect_link_title = |title| {
	match title {
		Some(value) =>
			"Some(${Str.inspect(value)})"

		None =>
			"None"
		}
}

Line : { raw : String.Utf8 }

ReferenceDefinition : {
	label : Str,
	target : Markdown.LinkTarget,
}

DocumentParts : {
	frontmatter : [Some(Markdown), None],
	refs : List(ReferenceDefinition),
	lines : List(Line),
}

ListMarker : {
	kind : Markdown.ListKind,
	width : U64,
	content : String.Utf8,
}

Fence : {
	marker : U8,
	len : U64,
	info : Str,
}

parse_all : Parser(String.Utf8, List(Markdown))
parse_all =
	Parser.build_primitive_parser(
		|input| {
			prepared = prepare_document(split_lines(input))
			parsed = parse_blocks_from_lines(prepared.lines, 0, prepared.refs)?

			blocks =
				match prepared.frontmatter {
					Some(block) => List.prepend(parsed.val, block)
					None => parsed.val
				}

			match parsed.input {
				[] =>
					Ok({ val: blocks, input: [] })

				_ =>
					Err(ParsingFailure("unexpected unparsed markdown lines"))
				}
		},
	)

prepare_document : List(Line) -> DocumentParts
prepare_document = |lines| {
	front = take_frontmatter(lines)
	without_front =
		match front.frontmatter {
			Some(_) => front.input
			None => lines
		}

	refs = collect_reference_definitions(without_front, [], [])

	{ frontmatter: front.frontmatter, refs: refs.refs, lines: refs.lines }
}

take_frontmatter : List(Line) -> { frontmatter : [Some(Markdown), None], input : List(Line) }
take_frontmatter = |lines| {
	match lines {
		[first, .. as rest] if is_exact_bytes(first.raw, "---".to_utf8()) => {
			found = take_until_frontmatter_close(rest, [])

			match found {
				Ok(done) => {
					raw = String.str_from_utf8(join_lines_with_newlines(done.raw))
					{ frontmatter: Some(Frontmatter({ raw: raw })), input: done.input }
				}

				Err(_) =>
					{ frontmatter: None, input: lines }
				}
		}

		_ =>
			{ frontmatter: None, input: lines }
		}
}

take_until_frontmatter_close : List(Line), List(String.Utf8) -> Try({ raw : List(String.Utf8), input : List(Line) }, [NotFound])
take_until_frontmatter_close = |lines, acc| {
	match lines {
		[] =>
			Err(NotFound)

		[line, .. as rest] if is_exact_bytes(line.raw, "---".to_utf8()) =>
			Ok({ raw: acc, input: rest })

		[line, .. as rest] =>
			take_until_frontmatter_close(rest, acc.append(line.raw))
		}
}

collect_reference_definitions : List(Line), List(ReferenceDefinition), List(Line) -> { refs : List(ReferenceDefinition), lines : List(Line) }
collect_reference_definitions = |lines, refs, kept| {
	match lines {
		[] =>
			{ refs: refs, lines: kept }

		[line, .. as rest] => {
			match parse_reference_definition(line.raw) {
				Ok(ref) =>
					collect_reference_definitions(rest, refs.append(ref), kept)

				Err(_) =>
					collect_reference_definitions(rest, refs, kept.append(line))
				}
		}
	}
}

parse_reference_definition : String.Utf8 -> Try(ReferenceDefinition, [NotFound])
parse_reference_definition = |line| {
	match trim_spaces(line) {
		['[', .. as rest] => {
			label = find_sequence(rest, "]:".to_utf8())?
			target_text = trim_spaces(label.after)

			if target_text.is_empty() {
				Err(NotFound)
			} else {
				Ok({ label: normalize_reference_label(label.before), target: parse_link_target(target_text) })
			}
		}

		_ =>
			Err(NotFound)
		}
}

parse_blocks_from_lines : List(Line), U64, List(ReferenceDefinition) -> Try({ val : List(Markdown), input : List(Line) }, [ParsingFailure(Str)])
parse_blocks_from_lines = |lines, min_indent, refs| {
	parse_blocks_help(lines, min_indent, refs, [])
}

parse_blocks_help : List(Line), U64, List(ReferenceDefinition), List(Markdown) -> Try({ val : List(Markdown), input : List(Line) }, [ParsingFailure(Str)])
parse_blocks_help = |lines, min_indent, refs, blocks| {
	match lines {
		[] =>
			Ok({ val: blocks, input: [] })

		[line, .. as rest] if line_is_blank(line) =>
			parse_blocks_help(rest, min_indent, refs, blocks)

		[line, ..] if line_indent(line) < min_indent =>
			Ok({ val: blocks, input: lines })

		_ => {
			parsed = parse_one_block(lines, min_indent, refs)?
			parse_blocks_help(parsed.input, min_indent, refs, blocks.append(parsed.val))
		}
	}
}

parse_one_block : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_one_block = |lines, min_indent, refs| {
	match lines {
		[line, underline, .. as rest_after_heading] if can_be_setext_heading_text(line, min_indent) => {
			match underline_level(underline, min_indent) {
				Ok(level) =>
					Ok({ val: Heading({ level: level, content: parse_inlines_with_refs(refs, strip_indent(line, min_indent)) }), input: rest_after_heading })

				Err(_) =>
					parse_one_block_without_setext(lines, min_indent, refs)
				}
		}

		_ =>
			parse_one_block_without_setext(lines, min_indent, refs)
		}
}

parse_one_block_without_setext : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_one_block_without_setext = |lines, min_indent, refs| {
	match lines {
		[] =>
			Err(ParsingFailure("expected a markdown block"))

		[line, ..] if line_indent(line) >= min_indent + 4 =>
			parse_indented_code_block(lines, min_indent)

		[line, .. as rest] => {
			content = strip_indent(line, min_indent)

			match parse_hash_heading_line(content, refs) {
				Ok(block) =>
					Ok({ val: block, input: rest })

				Err(_) => {
					match parse_fence_start(line, min_indent) {
						Ok(fence) =>
							parse_fenced_code_block(lines, min_indent, fence)

						Err(_) if is_thematic_break_line(line, min_indent) =>
							Ok({ val: ThematicBreak, input: rest })

						Err(_) => {
							match parse_table_block(lines, min_indent, refs) {
								Ok(table_block) =>
									Ok(table_block)

								Err(_) if is_html_block_line(line, min_indent) =>
									parse_html_block(lines, min_indent)

								Err(_) if is_blockquote_line(line, min_indent) =>
									parse_blockquote(lines, min_indent, refs)

								Err(_) => {
									match parse_list_marker(line, min_indent) {
										Ok(_) => parse_list_block(lines, min_indent, refs)
										Err(_) => parse_paragraph(lines, min_indent, refs)
									}
								}
							}
						}
					}
				}
			}
		}
	}
}

parse_paragraph : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_paragraph = |lines, min_indent, refs| {
	collected = collect_paragraph_lines(lines, min_indent, [])

	if collected.val.is_empty() {
		Err(ParsingFailure("expected a paragraph line"))
	} else {
		text = join_inline_lines(collected.val)
		Ok({ val: Paragraph(parse_inlines_with_refs(refs, text)), input: collected.input })
	}
}

collect_paragraph_lines : List(Line), U64, List(String.Utf8) -> { val : List(String.Utf8), input : List(Line) }
collect_paragraph_lines = |lines, min_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, ..] if line_is_blank(line) =>
			{ val: acc, input: lines }

		[line, ..] if line_indent(line) < min_indent =>
			{ val: acc, input: lines }

		[line, ..] if is_block_start(line, min_indent) =>
			{ val: acc, input: lines }

		[line, .. as rest] =>
			collect_paragraph_lines(rest, min_indent, acc.append(strip_indent(line, min_indent)))
		}
}

parse_fenced_code_block : List(Line), U64, Fence -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_fenced_code_block = |lines, min_indent, fence| {
	match lines {
		[_line, .. as rest] => {
			code = collect_fenced_code_lines(rest, min_indent, fence, [])?

			Ok({ val: Code({ info: fence.info, pre: String.str_from_utf8(join_lines_with_newlines(code.val)) }), input: code.input })
		}

		[] =>
			Err(ParsingFailure("expected a code fence"))
		}
}

collect_fenced_code_lines : List(Line), U64, Fence, List(String.Utf8) -> Try({ val : List(String.Utf8), input : List(Line) }, [ParsingFailure(Str)])
collect_fenced_code_lines = |lines, min_indent, fence, acc| {
	match lines {
		[] =>
			Err(ParsingFailure("expected closing code fence"))

		[line, .. as rest] if is_matching_fence_close(line, min_indent, fence) =>
			Ok({ val: acc, input: rest })

		[line, .. as rest] => {
			body_line =
				if line_indent(line) >= min_indent {
					strip_indent(line, min_indent)
				} else {
					line.raw
				}

			collect_fenced_code_lines(rest, min_indent, fence, acc.append(body_line))
		}
	}
}

parse_indented_code_block : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_indented_code_block = |lines, min_indent| {
	collected = collect_indented_code_lines(lines, min_indent, [])

	if collected.val.is_empty() {
		Err(ParsingFailure("expected indented code"))
	} else {
		Ok({ val: Code({ info: "", pre: String.str_from_utf8(join_lines_with_newlines(collected.val)) }), input: collected.input })
	}
}

collect_indented_code_lines : List(Line), U64, List(String.Utf8) -> { val : List(String.Utf8), input : List(Line) }
collect_indented_code_lines = |lines, min_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, .. as rest] if line_is_blank(line) =>
			collect_indented_code_lines(rest, min_indent, acc.append([]))

		[line, .. as rest] if line_indent(line) >= min_indent + 4 =>
			collect_indented_code_lines(rest, min_indent, acc.append(strip_indent(line, min_indent + 4)))

		_ =>
			{ val: acc, input: lines }
		}
}

parse_blockquote : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_blockquote = |lines, min_indent, refs| {
	quoted = collect_blockquote_lines(lines, min_indent, [])
	inner = parse_blocks_from_lines(quoted.val, 0, refs)?

	Ok({ val: Blockquote(inner.val), input: quoted.input })
}

collect_blockquote_lines : List(Line), U64, List(Line) -> { val : List(Line), input : List(Line) }
collect_blockquote_lines = |lines, min_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, .. as rest] if is_blockquote_line(line, min_indent) =>
			collect_blockquote_lines(rest, min_indent, acc.append({ raw: strip_blockquote_marker(line, min_indent) }))

		[line, .. as rest] if line_is_blank(line) =>
			collect_blockquote_lines(rest, min_indent, acc.append({ raw: [] }))

		_ =>
			{ val: acc, input: lines }
		}
}

parse_list_block : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_list_block = |lines, min_indent, refs| {
	match lines {
		[first, ..] => {
			match parse_list_marker(first, min_indent) {
				Ok(marker) => {
					parsed = parse_list_items(lines, min_indent, refs, marker.kind, [], Bool.False)?
					Ok({ val: ListBlock({ kind: marker.kind, loose: parsed.loose, items: parsed.items }), input: parsed.input })
				}

				Err(_) =>
					Err(ParsingFailure("expected list item"))
				}
		}

		[] =>
			Err(ParsingFailure("expected list item"))
		}
}

parse_list_items : List(Line), U64, List(ReferenceDefinition), Markdown.ListKind, List({ task : Markdown.TaskState, blocks : List(Markdown) }), Bool -> Try({ items : List({ task : Markdown.TaskState, blocks : List(Markdown) }), loose : Bool, input : List(Line) }, [ParsingFailure(Str)])
parse_list_items = |lines, min_indent, refs, kind, items, loose| {
	match lines {
		[line, .. as rest] => {
			match parse_list_marker(line, min_indent) {
				Ok(marker) if list_kind_matches(kind, marker.kind) => {
					item = parse_list_item_after_marker(marker, rest, min_indent, refs)?
					parse_list_items(item.input, min_indent, refs, kind, items.append(item.item), loose or item.loose)
				}

				_ =>
					Ok({ items, loose, input: lines })
				}
		}

		[] =>
			Ok({ items, loose, input: [] })
		}
}

parse_list_item_after_marker : ListMarker, List(Line), U64, List(ReferenceDefinition) -> Try({ item : { task : Markdown.TaskState, blocks : List(Markdown) }, loose : Bool, input : List(Line) }, [ParsingFailure(Str)])
parse_list_item_after_marker = |marker, rest, min_indent, refs| {
	task = parse_task_marker(marker.content)
	content_indent = min_indent + marker.width
	continuation = collect_list_item_continuation(rest, content_indent, [task.content])
	saw_blank_before_children = blank_line_keeps_list_loose(continuation.input, min_indent, content_indent)
	children = parse_blocks_from_lines(continuation.input, content_indent, refs)?

	content_text = join_inline_lines(continuation.val)
	content_blocks =
		if trim_spaces(content_text).is_empty() {
			children.val
		} else {
			List.prepend(children.val, Paragraph(parse_inlines_with_refs(refs, content_text)))
		}

	blank = consume_blank_lines(children.input, Bool.False)
	saw_blank_after_children = blank_line_keeps_list_loose(children.input, min_indent, content_indent)

	Ok({ item: { task: task.task, blocks: content_blocks }, loose: saw_blank_before_children or saw_blank_after_children, input: blank.input })
}

blank_line_keeps_list_loose : List(Line), U64, U64 -> Bool
blank_line_keeps_list_loose = |lines, min_indent, content_indent| {
	match lines {
		[line, ..] if line_is_blank(line) => {
			after_blank = consume_blank_lines(lines, Bool.False)

			match after_blank.input {
				[next, ..] if line_indent(next) >= content_indent =>
					Bool.True

				[next, ..] if parse_list_marker(next, min_indent).is_ok() =>
					Bool.True

				_ =>
					Bool.False
				}
		}

		_ =>
			Bool.False
		}
}

collect_list_item_continuation : List(Line), U64, List(String.Utf8) -> { val : List(String.Utf8), input : List(Line) }
collect_list_item_continuation = |lines, content_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, ..] if line_is_blank(line) =>
			{ val: acc, input: lines }

		[line, ..] if line_indent(line) < content_indent =>
			{ val: acc, input: lines }

		[line, ..] if is_block_start(line, content_indent) =>
			{ val: acc, input: lines }

		[line, .. as rest] =>
			collect_list_item_continuation(rest, content_indent, acc.append(strip_indent(line, content_indent)))
		}
}

parse_table_block : List(Line), U64, List(ReferenceDefinition) -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_table_block = |lines, min_indent, refs| {
	match lines {
		[header, delimiter, .. as rest] if line_indent(header) >= min_indent and line_indent(delimiter) >= min_indent => {
			match split_table_cells(strip_indent(header, min_indent)) {
				Ok(header_cells) => {
					match split_table_cells(strip_indent(delimiter, min_indent)) {
						Ok(delimiter_cells) => {
							align = parse_table_delimiter(delimiter_cells)?

							if header_cells.len() != align.len() {
								Err(ParsingFailure("table header and delimiter column counts differ"))
							} else {
								rows = collect_table_rows(rest, min_indent, refs, [])
								parsed_header = List.from_iter(header_cells.iter().map(|cell| parse_inlines_with_refs(refs, trim_spaces(cell))))

								Ok({ val: Table({ header: parsed_header, align, rows: rows.rows }), input: rows.input })
							}
						}

						Err(_) =>
							Err(ParsingFailure("expected table delimiter"))
						}
				}

				Err(_) =>
					Err(ParsingFailure("expected table header"))
				}
		}

		_ =>
			Err(ParsingFailure("expected table"))
		}
}

collect_table_rows : List(Line), U64, List(ReferenceDefinition), List(List(List(Markdown.Inline))) -> { rows : List(List(List(Markdown.Inline))), input : List(Line) }
collect_table_rows = |lines, min_indent, refs, rows| {
	match lines {
		[] =>
			{ rows, input: [] }

		[line, ..] if line_is_blank(line) =>
			{ rows, input: lines }

		[line, ..] if line_indent(line) < min_indent =>
			{ rows, input: lines }

		[line, ..] if is_block_start(line, min_indent) =>
			{ rows, input: lines }

		[line, .. as rest] => {
			match split_table_cells(strip_indent(line, min_indent)) {
				Ok(cells) => {
					parsed = List.from_iter(cells.iter().map(|cell| parse_inlines_with_refs(refs, trim_spaces(cell))))
					collect_table_rows(rest, min_indent, refs, rows.append(parsed))
				}

				Err(_) =>
					{ rows, input: lines }
				}
		}
	}
}

parse_html_block : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_html_block = |lines, min_indent| {
	collected = collect_html_block(lines, min_indent, [])

	if collected.val.is_empty() {
		Err(ParsingFailure("expected HTML block"))
	} else {
		Ok({ val: HtmlBlock(String.str_from_utf8(join_lines_with_newlines(collected.val))), input: collected.input })
	}
}

collect_html_block : List(Line), U64, List(String.Utf8) -> { val : List(String.Utf8), input : List(Line) }
collect_html_block = |lines, min_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, ..] if line_is_blank(line) =>
			{ val: acc, input: lines }

		[line, ..] if line_indent(line) < min_indent =>
			{ val: acc, input: lines }

		[line, .. as rest] =>
			collect_html_block(rest, min_indent, acc.append(strip_indent(line, min_indent)))
		}
}

is_block_start : Line, U64 -> Bool
is_block_start = |line, min_indent| {
	if line_indent(line) < min_indent {
		Bool.False
	} else {
		content = strip_indent(line, min_indent)

		is_hash_heading_line(content)
			or is_thematic_break_line(line, min_indent)
				or parse_fence_start(line, min_indent).is_ok()
					or is_html_block_line(line, min_indent)
						or is_blockquote_line(line, min_indent)
							or parse_list_marker(line, min_indent).is_ok()
	}
}

can_be_setext_heading_text : Line, U64 -> Bool
can_be_setext_heading_text = |line, min_indent| {
	(!line_is_blank(line)) and line_indent(line) >= min_indent and !is_block_start(line, min_indent)
}

inline_heading : Parser(String.Utf8, Markdown)
inline_heading =
	Parser.const(
		|level| {
			|str| {
				Heading({ level: level, content: parse_inlines(trim_closing_heading_marker(str.to_utf8())) })
			}
		},
	)
		.keep(
			Parser.one_of([
				Parser.const(One).skip(String.string("# ")),
				Parser.const(Two).skip(String.string("## ")),
				Parser.const(Three).skip(String.string("### ")),
				Parser.const(Four).skip(String.string("#### ")),
				Parser.const(Five).skip(String.string("##### ")),
				Parser.const(Six).skip(String.string("###### ")),
			]),
		)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

two_line_heading_level_one : Parser(String.Utf8, Markdown)
two_line_heading_level_one =
	Parser.const(
		|str| {
			Heading({ level: One, content: parse_inlines(str.to_utf8()) })
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("=="))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '='
				},
			),
		)

two_line_heading_level_two : Parser(String.Utf8, Markdown)
two_line_heading_level_two =
	Parser.const(
		|str| {
			Heading({ level: Two, content: parse_inlines(str.to_utf8()) })
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("--"))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '-'
				},
			),
		)

parse_hash_heading_line : String.Utf8, List(ReferenceDefinition) -> Try(Markdown, [NotFound])
parse_hash_heading_line = |content, refs| {
	hashes = count_leading_byte(content, '#', 0)

	if hashes < 1 or hashes > 6 {
		Err(NotFound)
	} else {
		after_hashes = content.drop_first(hashes)

		match after_hashes {
			[' ', .. as raw_text] => {
				level = heading_level_from_count(hashes)?
				Ok(Heading({ level: level, content: parse_inlines_with_refs(refs, trim_closing_heading_marker(raw_text)) }))
			}

			_ =>
				Err(NotFound)
			}
	}
}

is_hash_heading_line : String.Utf8 -> Bool
is_hash_heading_line = |content| {
	parse_hash_heading_line(content, []).is_ok()
}

heading_level_from_count : U64 -> Try(Markdown.Level, [NotFound])
heading_level_from_count = |count| {
	match count {
		1 => Ok(One)
		2 => Ok(Two)
		3 => Ok(Three)
		4 => Ok(Four)
		5 => Ok(Five)
		6 => Ok(Six)
		_ => Err(NotFound)
	}
}

underline_level : Line, U64 -> Try(Markdown.Level, [NotFound])
underline_level = |line, min_indent| {
	if line_indent(line) != min_indent {
		Err(NotFound)
	} else {
		content = trim_spaces(strip_indent(line, min_indent))

		match content {
			['=', '=', ..] if underline_rest_matches(content, '=') =>
				Ok(One)

			['-', '-', ..] if underline_rest_matches(content, '-') =>
				Ok(Two)

			_ =>
				Err(NotFound)
			}
	}
}

underline_rest_matches : String.Utf8, U8 -> Bool
underline_rest_matches = |content, marker| {
	all_bytes_are(content.drop_first(2), marker)
}

parse_fence_start : Line, U64 -> Try(Fence, [NotFound])
parse_fence_start = |line, min_indent| {
	if line_indent(line) < min_indent {
		Err(NotFound)
	} else {
		content = strip_indent(line, min_indent)

		match content {
			['`', '`', '`', ..] =>
				parse_fence_start_help(content, '`')

			['~', '~', '~', ..] =>
				parse_fence_start_help(content, '~')

			_ =>
				Err(NotFound)
			}
	}
}

parse_fence_start_help : String.Utf8, U8 -> Try(Fence, [NotFound])
parse_fence_start_help = |content, marker| {
	len = count_leading_byte(content, marker, 0)

	if len < 3 {
		Err(NotFound)
	} else {
		info = trim_spaces(content.drop_first(len))
		Ok({ marker, len, info: String.str_from_utf8(info) })
	}
}

is_matching_fence_close : Line, U64, Fence -> Bool
is_matching_fence_close = |line, min_indent, fence| {
	if line_indent(line) < min_indent {
		Bool.False
	} else {
		content = trim_spaces(strip_indent(line, min_indent))
		count = count_leading_byte(content, fence.marker, 0)
		rest = content.drop_first(count)

		count >= fence.len and rest.is_empty()
	}
}

is_thematic_break_line : Line, U64 -> Bool
is_thematic_break_line = |line, min_indent| {
	if line_indent(line) != min_indent {
		Bool.False
	} else {
		content = strip_indent(line, min_indent)

		match first_non_space(content) {
			Ok(marker) if marker == '-' or marker == '*' or marker == '_' =>
				thematic_marker_count(content, marker, 0) >= 3

			_ =>
				Bool.False
			}
	}
}

thematic_marker_count : String.Utf8, U8, U64 -> U64
thematic_marker_count = |content, marker, count| {
	match content {
		[] =>
			count

		[' ', .. as rest] =>
			thematic_marker_count(rest, marker, count)

		[first, .. as rest] if first == marker =>
			thematic_marker_count(rest, marker, count + 1)

		_ =>
			0
		}
}

is_blockquote_line : Line, U64 -> Bool
is_blockquote_line = |line, min_indent| {
	line_indent(line) >= min_indent and starts_with_bytes(strip_indent(line, min_indent), ">".to_utf8())
}

strip_blockquote_marker : Line, U64 -> String.Utf8
strip_blockquote_marker = |line, min_indent| {
	content = strip_indent(line, min_indent).drop_first(1)

	match content {
		[' ', .. as rest] =>
			rest

		_ =>
			content
		}
}

parse_list_marker : Line, U64 -> Try(ListMarker, [NotFound])
parse_list_marker = |line, min_indent| {
	if line_indent(line) != min_indent {
		Err(NotFound)
	} else {
		content = strip_indent(line, min_indent)

		match content {
			['-', ' ', .. as rest] =>
				Ok({ kind: Unordered, width: 2, content: rest })

			['*', ' ', .. as rest] =>
				Ok({ kind: Unordered, width: 2, content: rest })

			['+', ' ', .. as rest] =>
				Ok({ kind: Unordered, width: 2, content: rest })

			[first, .. as rest] if is_digit_byte(first) =>
				parse_ordered_marker(rest, [first])

			_ =>
				Err(NotFound)
			}
	}
}

parse_ordered_marker : String.Utf8, String.Utf8 -> Try(ListMarker, [NotFound])
parse_ordered_marker = |content, digits| {
	match content {
		['.', ' ', .. as rest] =>
			Ok({ kind: Ordered({ start: digits_to_u64(digits) }), width: digits.len() + 2, content: rest })

		[')', ' ', .. as rest] =>
			Ok({ kind: Ordered({ start: digits_to_u64(digits) }), width: digits.len() + 2, content: rest })

		[first, .. as rest] if is_digit_byte(first) =>
			parse_ordered_marker(rest, digits.append(first))

		_ =>
			Err(NotFound)
		}
}

list_kind_matches : Markdown.ListKind, Markdown.ListKind -> Bool
list_kind_matches = |expected, actual| {
	match { expected, actual } {
		{ expected: Unordered, actual: Unordered } =>
			Bool.True

		{ expected: Ordered(_), actual: Ordered(_) } =>
			Bool.True

		_ =>
			Bool.False
		}
}

parse_task_marker : String.Utf8 -> { task : Markdown.TaskState, content : String.Utf8 }
parse_task_marker = |content| {
	match content {
		['[', ' ', ']', ' ', .. as rest] =>
			{ task: Unchecked, content: rest }

		['[', 'x', ']', ' ', .. as rest] =>
			{ task: Checked, content: rest }

		['[', 'X', ']', ' ', .. as rest] =>
			{ task: Checked, content: rest }

		_ =>
			{ task: NoTask, content }
		}
}

is_html_block_line : Line, U64 -> Bool
is_html_block_line = |line, min_indent| {
	if line_indent(line) < min_indent {
		Bool.False
	} else {
		content = trim_spaces(strip_indent(line, min_indent))

		match content {
			['<', first, ..] if is_alphabetic_byte(first) or first == '/' or first == '!' =>
				Bool.True

			_ =>
				Bool.False
			}
	}
}

split_table_cells : String.Utf8 -> Try(List(String.Utf8), [NotFound])
split_table_cells = |line| {
	if !contains_unescaped_pipe(line) {
		Err(NotFound)
	} else {
		cells = split_table_cells_help(strip_outer_table_pipes(line), [], [], Bool.False, Bool.False)

		if cells.is_empty() {
			Err(NotFound)
		} else {
			Ok(cells)
		}
	}
}

split_table_cells_help : String.Utf8, String.Utf8, List(String.Utf8), Bool, Bool -> List(String.Utf8)
split_table_cells_help = |input, current, cells, in_code, escaped| {
	match input {
		[] =>
			cells.append(current)

		[first, .. as rest] if escaped =>
			split_table_cells_help(rest, current.append(first), cells, in_code, Bool.False)

		['\\', .. as rest] =>
			split_table_cells_help(rest, current.append('\\'), cells, in_code, Bool.True)

		['`', .. as rest] =>
			split_table_cells_help(rest, current.append('`'), cells, !in_code, Bool.False)

		['|', .. as rest] if !in_code =>
			split_table_cells_help(rest, [], cells.append(current), in_code, Bool.False)

		[first, .. as rest] =>
			split_table_cells_help(rest, current.append(first), cells, in_code, Bool.False)
		}
}

strip_outer_table_pipes : String.Utf8 -> String.Utf8
strip_outer_table_pipes = |line| {
	trimmed = trim_spaces(line)
	left =
		match trimmed {
			['|', .. as rest] => rest
			_ => trimmed
		}

	drop_trailing_pipe(trim_spaces(left))
}

drop_trailing_pipe : String.Utf8 -> String.Utf8
drop_trailing_pipe = |bytes| {
	drop_trailing_pipe_help(bytes, [], [])
}

drop_trailing_pipe_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
drop_trailing_pipe_help = |bytes, out, pending_spaces| {
	match bytes {
		[] =>
			out

		[' ', .. as rest] =>
			drop_trailing_pipe_help(rest, out, pending_spaces.append(' '))

		['|'] =>
			out

		[first, .. as rest] =>
			drop_trailing_pipe_help(rest, append_bytes(out, pending_spaces).append(first), [])
		}
}

parse_table_delimiter : List(String.Utf8) -> Try(List(Markdown.Alignment), [ParsingFailure(Str)])
parse_table_delimiter = |cells| {
	align = List.from_iter(cells.iter().map(parse_alignment_cell))

	if align_has_failure(align) {
		Err(ParsingFailure("expected table delimiter"))
	} else {
		Ok(List.from_iter(align.iter().map(unwrap_alignment)))
	}
}

parse_alignment_cell : String.Utf8 -> Try(Markdown.Alignment, [NotFound])
parse_alignment_cell = |cell| {
	trimmed = trim_spaces(cell)
	left_colon = starts_with_bytes(trimmed, ":".to_utf8())
	right_colon = ends_with_byte(trimmed, ':')
	dashes = count_byte(trimmed, '-')

	if dashes < 3 or !only_alignment_chars(trimmed) {
		Err(NotFound)
	} else if left_colon and right_colon {
		Ok(Center)
	} else if left_colon {
		Ok(Left)
	} else if right_colon {
		Ok(Right)
	} else {
		Ok(Default)
	}
}

align_has_failure : List(Try(Markdown.Alignment, [NotFound])) -> Bool
align_has_failure = |items| {
	match items {
		[] =>
			Bool.False

		[Err(_), ..] =>
			Bool.True

		[Ok(_), .. as rest] =>
			align_has_failure(rest)
		}
}

unwrap_alignment : Try(Markdown.Alignment, [NotFound]) -> Markdown.Alignment
unwrap_alignment = |item| {
	match item {
		Ok(align) => align
		Err(_) => Default
	}
}

only_alignment_chars : String.Utf8 -> Bool
only_alignment_chars = |bytes| {
	match bytes {
		[] =>
			Bool.True

		['-', .. as rest] =>
			only_alignment_chars(rest)

		[':', .. as rest] =>
			only_alignment_chars(rest)

		[' ', .. as rest] =>
			only_alignment_chars(rest)

		_ =>
			Bool.False
		}
}

contains_unescaped_pipe : String.Utf8 -> Bool
contains_unescaped_pipe = |bytes| {
	contains_unescaped_pipe_help(bytes, Bool.False, Bool.False)
}

contains_unescaped_pipe_help : String.Utf8, Bool, Bool -> Bool
contains_unescaped_pipe_help = |bytes, in_code, escaped| {
	match bytes {
		[] =>
			Bool.False

		[_, .. as rest] if escaped =>
			contains_unescaped_pipe_help(rest, in_code, Bool.False)

		['\\', .. as rest] =>
			contains_unescaped_pipe_help(rest, in_code, Bool.True)

		['`', .. as rest] =>
			contains_unescaped_pipe_help(rest, !in_code, Bool.False)

		['|', ..] if !in_code =>
			Bool.True

		[_, .. as rest] =>
			contains_unescaped_pipe_help(rest, in_code, Bool.False)
		}
}

parse_inlines_parser : Parser(String.Utf8, List(Markdown.Inline))
parse_inlines_parser =
	Parser.build_primitive_parser(
		|input| {
			Ok({ val: parse_inlines(input), input: [] })
		},
	)

parse_inlines : String.Utf8 -> List(Markdown.Inline)
parse_inlines = |input| {
	parse_inlines_with_refs([], input)
}

parse_inlines_with_refs : List(ReferenceDefinition), String.Utf8 -> List(Markdown.Inline)
parse_inlines_with_refs = |refs, input| {
	parse_inlines_help(refs, input, [], [])
}

parse_inlines_help : List(ReferenceDefinition), String.Utf8, String.Utf8, List(Markdown.Inline) -> List(Markdown.Inline)
parse_inlines_help = |refs, input, text, nodes| {
	match input {
		[] =>
			flush_text(text, nodes)

		[' ', ' ', '\n', .. as rest] => {
			next_nodes = flush_text(text, nodes).append(HardBreak)
			parse_inlines_help(refs, rest, [], next_nodes)
		}

		['\\', '\n', .. as rest] => {
			next_nodes = flush_text(text, nodes).append(HardBreak)
			parse_inlines_help(refs, rest, [], next_nodes)
		}

		['\\', escaped, .. as rest] if is_escapable_inline_byte(escaped) =>
			parse_inlines_help(refs, rest, text.append(escaped), nodes)

		['\\', .. as rest] =>
			parse_inlines_help(refs, rest, text.append('\\'), nodes)

		['!', '[', ..] => {
			match parse_image_inline(input, refs) {
				Ok(parsed) =>
					parse_inlines_help(refs, parsed.input, [], flush_text(text, nodes).append(parsed.val))

				Err(_) =>
					parse_inlines_help(refs, input.drop_first(1), text.append('!'), nodes)
				}
		}

		['[', ..] => {
			match parse_link_inline(input, refs) {
				Ok(parsed) =>
					parse_inlines_help(refs, parsed.input, [], flush_text(text, nodes).append(parsed.val))

				Err(_) =>
					parse_inlines_help(refs, input.drop_first(1), text.append('['), nodes)
				}
		}

		['<', ..] => {
			match parse_angle_inline(input) {
				Ok(parsed) =>
					parse_inlines_help(refs, parsed.input, [], flush_text(text, nodes).append(parsed.val))

				Err(_) =>
					parse_inlines_help(refs, input.drop_first(1), text.append('<'), nodes)
				}
		}

		['~', '~', .. as rest] => {
			match find_unescaped_sequence(rest, "~~".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Strikethrough(parse_inlines_with_refs(refs, found.before)))
					parse_inlines_help(refs, found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(refs, rest, text.append('~').append('~'), nodes)
				}
		}

		['*', '*', .. as rest] => {
			match find_unescaped_sequence(rest, "**".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Strong(parse_inlines_with_refs(refs, found.before)))
					parse_inlines_help(refs, found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(refs, rest, text.append('*').append('*'), nodes)
				}
		}

		['*', .. as rest] => {
			match find_unescaped_sequence(rest, "*".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Emphasis(parse_inlines_with_refs(refs, found.before)))
					parse_inlines_help(refs, found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(refs, rest, text.append('*'), nodes)
				}
		}

		['_', .. as rest] => {
			match find_unescaped_sequence(rest, "_".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Emphasis(parse_inlines_with_refs(refs, found.before)))
					parse_inlines_help(refs, found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(refs, rest, text.append('_'), nodes)
				}
		}

		['`', .. as rest] => {
			match find_unescaped_sequence(rest, "`".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(InlineCode(String.str_from_utf8(found.before)))
					parse_inlines_help(refs, found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(refs, rest, text.append('`'), nodes)
				}
		}

		_ if starts_with_bytes(input, "https://".to_utf8()) or starts_with_bytes(input, "http://".to_utf8()) or starts_with_bytes(input, "www.".to_utf8()) => {
			parsed = parse_bare_url(input)
			target = { href: String.str_from_utf8(parsed.url), title: None }
			node = Link({ label: [Text(String.str_from_utf8(parsed.url))], target })

			parse_inlines_help(refs, parsed.input, [], flush_text(text, nodes).append(node))
		}

		[first, .. as rest] =>
			parse_inlines_help(refs, rest, text.append(first), nodes)
		}
}

parse_image_inline : String.Utf8, List(ReferenceDefinition) -> Try({ val : Markdown.Inline, input : String.Utf8 }, [NotFound])
parse_image_inline = |input, refs| {
	match input {
		['!', '[', .. as rest] => {
			label = find_unescaped_sequence(rest, "]".to_utf8())?
			target = parse_link_or_reference_target(label.after, label.before, refs)?

			Ok({ val: Image({ alt: parse_inlines_with_refs(refs, label.before), target: target.target }), input: target.input })
		}

		_ =>
			Err(NotFound)
		}
}

parse_link_inline : String.Utf8, List(ReferenceDefinition) -> Try({ val : Markdown.Inline, input : String.Utf8 }, [NotFound])
parse_link_inline = |input, refs| {
	match input {
		['[', .. as rest] => {
			label = find_unescaped_sequence(rest, "]".to_utf8())?
			target = parse_link_or_reference_target(label.after, label.before, refs)?

			Ok({ val: Link({ label: parse_inlines_with_refs(refs, label.before), target: target.target }), input: target.input })
		}

		_ =>
			Err(NotFound)
		}
}

parse_link_or_reference_target : String.Utf8, String.Utf8, List(ReferenceDefinition) -> Try({ target : Markdown.LinkTarget, input : String.Utf8 }, [NotFound])
parse_link_or_reference_target = |input, label, refs| {
	match input {
		['(', .. as rest] => {
			target_text = find_unescaped_sequence(rest, ")".to_utf8())?
			Ok({ target: parse_link_target(target_text.before), input: target_text.after })
		}

		['[', ']', .. as rest] => {
			target = lookup_reference(refs, normalize_reference_label(label))?
			Ok({ target, input: rest })
		}

		['[', .. as rest] => {
			ref_label = find_unescaped_sequence(rest, "]".to_utf8())?
			target = lookup_reference(refs, normalize_reference_label(ref_label.before))?
			Ok({ target, input: ref_label.after })
		}

		_ => {
			target = lookup_reference(refs, normalize_reference_label(label))?
			Ok({ target, input })
		}
	}
}

parse_angle_inline : String.Utf8 -> Try({ val : Markdown.Inline, input : String.Utf8 }, [NotFound])
parse_angle_inline = |input| {
	match input {
		['<', .. as rest] => {
			inside = find_sequence(rest, ">".to_utf8())?
			inside_str = String.str_from_utf8(inside.before)

			if starts_with_bytes(inside.before, "http://".to_utf8()) or starts_with_bytes(inside.before, "https://".to_utf8()) {
				Ok({ val: Link({ label: [Text(inside_str)], target: { href: inside_str, title: None } }), input: inside.after })
			} else if is_email_bytes(inside.before) {
				Ok({ val: Link({ label: [Text(inside_str)], target: { href: "mailto:${inside_str}", title: None } }), input: inside.after })
			} else {
				raw = append_bytes("<".to_utf8(), append_bytes(inside.before, ">".to_utf8()))
				Ok({ val: HtmlInline(String.str_from_utf8(raw)), input: inside.after })
			}
		}

		_ =>
			Err(NotFound)
		}
}

parse_bare_url : String.Utf8 -> { url : String.Utf8, input : String.Utf8 }
parse_bare_url = |input| {
	parse_bare_url_help(input, [])
}

parse_bare_url_help : String.Utf8, String.Utf8 -> { url : String.Utf8, input : String.Utf8 }
parse_bare_url_help = |input, url| {
	match input {
		[] =>
			{ url, input: [] }

		[first, ..] if first == ' ' or first == '\n' or first == '\t' =>
			{ url, input }

		[first, ..] if first == ')' or first == ']' =>
			{ url, input }

		[first, .. as rest] =>
			parse_bare_url_help(rest, url.append(first))
		}
}

parse_link_target : String.Utf8 -> Markdown.LinkTarget
parse_link_target = |raw| {
	clean = trim_spaces(raw)
	parts = split_first_space(clean)

	title =
		if parts.rest.is_empty() {
			None
		} else {
			Some(String.str_from_utf8(strip_wrapping_quotes(trim_spaces(parts.rest))))
		}

	{ href: String.str_from_utf8(parts.first), title }
}

lookup_reference : List(ReferenceDefinition), Str -> Try(Markdown.LinkTarget, [NotFound])
lookup_reference = |refs, label| {
	match refs {
		[] =>
			Err(NotFound)

		[ref, ..] if ref.label == label =>
			Ok(ref.target)

		[_, .. as rest] =>
			lookup_reference(rest, label)
		}
}

flush_text : String.Utf8, List(Markdown.Inline) -> List(Markdown.Inline)
flush_text = |text, nodes| {
	if text.is_empty() {
		nodes
	} else {
		nodes.append(Text(String.str_from_utf8(text)))
	}
}

find_sequence : String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_sequence = |input, needle| {
	find_sequence_help(input, needle, [])
}

find_unescaped_sequence : String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_unescaped_sequence = |input, needle| {
	find_unescaped_sequence_help(input, needle, [])
}

find_sequence_help : String.Utf8, String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_sequence_help = |input, needle, acc| {
	if starts_with_bytes(input, needle) {
		Ok({ before: acc, after: input.drop_first(needle.len()) })
	} else {
		match input {
			[] =>
				Err(NotFound)

			[first, .. as rest] =>
				find_sequence_help(rest, needle, acc.append(first))
			}
	}
}

find_unescaped_sequence_help : String.Utf8, String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_unescaped_sequence_help = |input, needle, acc| {
	if starts_with_bytes(input, needle) {
		Ok({ before: acc, after: input.drop_first(needle.len()) })
	} else {
		match input {
			[] =>
				Err(NotFound)

			['\\', escaped, .. as rest] if is_escapable_inline_byte(escaped) =>
				find_unescaped_sequence_help(rest, needle, acc.append('\\').append(escaped))

			[first, .. as rest] =>
				find_unescaped_sequence_help(rest, needle, acc.append(first))
			}
	}
}

split_lines : String.Utf8 -> List(Line)
split_lines = |input| {
	split_lines_help(input, [], [])
}

split_lines_help : String.Utf8, String.Utf8, List(Line) -> List(Line)
split_lines_help = |input, current, lines| {
	match input {
		[] =>
			lines.append({ raw: current })

		['\r', '\n', .. as rest] =>
			split_lines_help(rest, [], lines.append({ raw: current }))

		['\n', .. as rest] =>
			split_lines_help(rest, [], lines.append({ raw: current }))

		[first, .. as rest] =>
			split_lines_help(rest, current.append(first), lines)
		}
}

consume_blank_lines : List(Line), Bool -> { input : List(Line), saw_blank : Bool }
consume_blank_lines = |lines, saw_blank| {
	match lines {
		[line, .. as rest] if line_is_blank(line) =>
			consume_blank_lines(rest, Bool.True)

		_ =>
			{ input: lines, saw_blank }
		}
}

line_indent : Line -> U64
line_indent = |line| {
	count_leading_byte(line.raw, ' ', 0)
}

strip_indent : Line, U64 -> String.Utf8
strip_indent = |line, indent| {
	line.raw.drop_first(indent)
}

line_is_blank : Line -> Bool
line_is_blank = |line| {
	bytes_are_blank(line.raw)
}

bytes_are_blank : String.Utf8 -> Bool
bytes_are_blank = |bytes| {
	match bytes {
		[] =>
			Bool.True

		[' ', .. as rest] =>
			bytes_are_blank(rest)

		['\t', .. as rest] =>
			bytes_are_blank(rest)

		_ =>
			Bool.False
		}
}

starts_with_bytes : String.Utf8, String.Utf8 -> Bool
starts_with_bytes = |input, prefix| {
	{ before: start, others: _ } = input.split_at(prefix.len())
	start == prefix
}

ends_with_byte : String.Utf8, U8 -> Bool
ends_with_byte = |bytes, expected| {
	ends_with_byte_help(bytes, expected, Err(NotFound))
}

ends_with_byte_help : String.Utf8, U8, Try(U8, [NotFound]) -> Bool
ends_with_byte_help = |bytes, expected, last| {
	match bytes {
		[] =>
			last == Ok(expected)

		[first, .. as rest] =>
			ends_with_byte_help(rest, expected, Ok(first))
		}
}

is_exact_bytes : String.Utf8, String.Utf8 -> Bool
is_exact_bytes = |left, right| {
	left == right
}

all_bytes_are : String.Utf8, U8 -> Bool
all_bytes_are = |bytes, expected| {
	match bytes {
		[] =>
			Bool.True

		[first, .. as rest] =>
			first == expected and all_bytes_are(rest, expected)
		}
}

append_bytes : String.Utf8, String.Utf8 -> String.Utf8
append_bytes = |left, right| {
	match right {
		[] =>
			left

		[first, .. as rest] =>
			append_bytes(left.append(first), rest)
		}
}

join_lines_with_newlines : List(String.Utf8) -> String.Utf8
join_lines_with_newlines = |lines| {
	join_lines_with_newlines_help(lines, [])
}

join_lines_with_newlines_help : List(String.Utf8), String.Utf8 -> String.Utf8
join_lines_with_newlines_help = |lines, acc| {
	match lines {
		[] =>
			acc

		[line, .. as rest] =>
			join_lines_with_newlines_help(rest, append_bytes(acc, line).append('\n'))
		}
}

join_inline_lines : List(String.Utf8) -> String.Utf8
join_inline_lines = |lines| {
	join_inline_lines_help(lines, [])
}

join_inline_lines_help : List(String.Utf8), String.Utf8 -> String.Utf8
join_inline_lines_help = |lines, acc| {
	match lines {
		[] =>
			acc

		[line, .. as rest] if acc.is_empty() =>
			join_inline_lines_help(rest, append_bytes(acc, line))

		[line, .. as rest] if ends_with_hard_break_marker(acc) =>
			join_inline_lines_help(rest, append_bytes(acc.append('\n'), line))

		[line, .. as rest] =>
			join_inline_lines_help(rest, append_bytes(acc.append(' '), line))
		}
}

ends_with_hard_break_marker : String.Utf8 -> Bool
ends_with_hard_break_marker = |bytes| {
	ends_with_byte(bytes, '\\') or ends_with_two_spaces(bytes)
}

ends_with_two_spaces : String.Utf8 -> Bool
ends_with_two_spaces = |bytes| {
	ends_with_two_spaces_help(bytes, 0)
}

ends_with_two_spaces_help : String.Utf8, U64 -> Bool
ends_with_two_spaces_help = |bytes, spaces| {
	match bytes {
		[] =>
			spaces >= 2

		[' ', .. as rest] =>
			ends_with_two_spaces_help(rest, spaces + 1)

		[_, .. as rest] =>
			ends_with_two_spaces_help(rest, 0)
		}
}

trim_spaces : String.Utf8 -> String.Utf8
trim_spaces = |bytes| {
	trim_end_spaces(trim_start_spaces(bytes))
}

trim_start_spaces : String.Utf8 -> String.Utf8
trim_start_spaces = |bytes| {
	match bytes {
		[' ', .. as rest] =>
			trim_start_spaces(rest)

		['\t', .. as rest] =>
			trim_start_spaces(rest)

		_ =>
			bytes
		}
}

trim_end_spaces : String.Utf8 -> String.Utf8
trim_end_spaces = |bytes| {
	trim_end_spaces_help(bytes, [], [])
}

trim_end_spaces_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_end_spaces_help = |bytes, out, pending_spaces| {
	match bytes {
		[] =>
			out

		[' ', .. as rest] =>
			trim_end_spaces_help(rest, out, pending_spaces.append(' '))

		['\t', .. as rest] =>
			trim_end_spaces_help(rest, out, pending_spaces.append('\t'))

		[first, .. as rest] =>
			trim_end_spaces_help(rest, append_bytes(out, pending_spaces).append(first), [])
		}
}

trim_closing_heading_marker : String.Utf8 -> String.Utf8
trim_closing_heading_marker = |bytes| {
	trim_closing_heading_marker_help(trim_spaces(bytes), [], [])
}

trim_closing_heading_marker_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_closing_heading_marker_help = |bytes, out, pending_hashes| {
	match bytes {
		[] if pending_hashes.is_empty() =>
			out

		[] =>
			trim_spaces(out)

		['#', .. as rest] =>
			trim_closing_heading_marker_help(rest, out, pending_hashes.append('#'))

		[first, .. as rest] =>
			trim_closing_heading_marker_help(rest, append_bytes(out, pending_hashes).append(first), [])
		}
}

split_first_space : String.Utf8 -> { first : String.Utf8, rest : String.Utf8 }
split_first_space = |bytes| {
	split_first_space_help(bytes, [])
}

split_first_space_help : String.Utf8, String.Utf8 -> { first : String.Utf8, rest : String.Utf8 }
split_first_space_help = |bytes, first_part| {
	match bytes {
		[] =>
			{ first: first_part, rest: [] }

		[' ', .. as rest] =>
			{ first: first_part, rest }

		['\t', .. as rest] =>
			{ first: first_part, rest }

		[first, .. as rest] =>
			split_first_space_help(rest, first_part.append(first))
		}
}

strip_wrapping_quotes : String.Utf8 -> String.Utf8
strip_wrapping_quotes = |bytes| {
	match bytes {
		['"', .. as rest] => {
			found = find_sequence(rest, "\"".to_utf8()) ?? { before: rest, after: [] }
			found.before
		}

		['\'', .. as rest] => {
			found = find_sequence(rest, "'".to_utf8()) ?? { before: rest, after: [] }
			found.before
		}

		_ =>
			bytes
		}
}

normalize_reference_label : String.Utf8 -> Str
normalize_reference_label = |label| {
	String.str_from_utf8(collapse_reference_whitespace(lower_ascii_bytes(trim_spaces(label)), [], Bool.False))
}

lower_ascii_bytes : String.Utf8 -> String.Utf8
lower_ascii_bytes = |bytes| {
	List.from_iter(bytes.iter().map(lower_ascii_byte))
}

lower_ascii_byte : U8 -> U8
lower_ascii_byte = |byte| {
	if byte >= 'A' and byte <= 'Z' {
		byte + 32
	} else {
		byte
	}
}

collapse_reference_whitespace : String.Utf8, String.Utf8, Bool -> String.Utf8
collapse_reference_whitespace = |bytes, out, pending_space| {
	match bytes {
		[] =>
			out

		[first, .. as rest] if first == ' ' or first == '\t' or first == '\n' =>
			collapse_reference_whitespace(rest, out, Bool.True)

		[first, .. as rest] if pending_space and !out.is_empty() =>
			collapse_reference_whitespace(rest, out.append(' ').append(first), Bool.False)

		[first, .. as rest] =>
			collapse_reference_whitespace(rest, out.append(first), Bool.False)
		}
}

count_leading_byte : String.Utf8, U8, U64 -> U64
count_leading_byte = |bytes, expected, count| {
	match bytes {
		[first, .. as rest] if first == expected =>
			count_leading_byte(rest, expected, count + 1)

		_ =>
			count
		}
}

count_byte : String.Utf8, U8 -> U64
count_byte = |bytes, expected| {
	count_byte_help(bytes, expected, 0)
}

count_byte_help : String.Utf8, U8, U64 -> U64
count_byte_help = |bytes, expected, count| {
	match bytes {
		[] =>
			count

		[first, .. as rest] if first == expected =>
			count_byte_help(rest, expected, count + 1)

		[_, .. as rest] =>
			count_byte_help(rest, expected, count)
		}
}

first_non_space : String.Utf8 -> Try(U8, [NotFound])
first_non_space = |bytes| {
	match bytes {
		[] =>
			Err(NotFound)

		[' ', .. as rest] =>
			first_non_space(rest)

		[first, ..] =>
			Ok(first)
		}
}

digits_to_u64 : String.Utf8 -> U64
digits_to_u64 = |digits| {
	digits.fold(
		0,
		|sum, digit| {
			sum * 10 + (digit - '0').to_u64()
		},
	)
}

is_digit_byte : U8 -> Bool
is_digit_byte = |byte| {
	byte >= '0' and byte <= '9'
}

is_alphabetic_byte : U8 -> Bool
is_alphabetic_byte = |byte| {
	(byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z')
}

is_email_bytes : String.Utf8 -> Bool
is_email_bytes = |bytes| {
	contains_byte(bytes, '@') and contains_byte(bytes, '.')
}

contains_byte : String.Utf8, U8 -> Bool
contains_byte = |bytes, expected| {
	match bytes {
		[] =>
			Bool.False

		[first, ..] if first == expected =>
			Bool.True

		[_, .. as rest] =>
			contains_byte(rest, expected)
		}
}

is_escapable_inline_byte : U8 -> Bool
is_escapable_inline_byte = |byte| {
	byte == '\\'
		or byte == '*'
			or byte == '_'
				or byte == '`'
					or byte == '['
						or byte == ']'
							or byte == '('
								or byte == ')'
									or byte == '!'
										or byte == '~'
											or byte == '|'
}

end_of_line : Parser(String.Utf8, Str)
end_of_line = Parser.one_of([String.string("\n"), String.string("\r\n")])

not_end_of_line : U8 -> Bool
not_end_of_line = |b| {
	b != '\n' and b != '\r'
}

todo : Parser(String.Utf8, Markdown)
todo =
	Parser.const(|s| TODO(s))
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

## Unsupported markdown lines can still be preserved as TODO nodes directly.
expect {
	a = String.parse_str(todo, "Foo Bar")?
	a == TODO("Foo Bar")
}

## Small nominal support types expose equality, debug, and string helpers.
expect {
	level : Markdown.Level
	level = Two

	kind : Markdown.ListKind
	kind = Ordered({ start: 3 })

	task : Markdown.TaskState
	task = Checked

	alignment : Markdown.Alignment
	alignment = Right

	actual =
		\\level inspect: ${Str.inspect(level)}
		\\level str: ${level.to_str()}
		\\level eq: ${Str.inspect(level == Two)}
		\\kind inspect: ${Str.inspect(kind)}
		\\kind str: ${kind.to_str()}
		\\kind eq same: ${Str.inspect(kind == Ordered({ start: 3 }))}
		\\kind eq different: ${Str.inspect(kind == Ordered({ start: 4 }))}
		\\task inspect: ${Str.inspect(task)}
		\\task str: ${task.to_str()}
		\\task eq: ${Str.inspect(task == Checked)}
		\\alignment inspect: ${Str.inspect(alignment)}
		\\alignment str: ${alignment.to_str()}
		\\alignment eq: ${Str.inspect(alignment == Right)}

	expected =
		\\level inspect: Two
		\\level str: 2
		\\level eq: True
		\\kind inspect: Ordered({ start: 3 })
		\\kind str: ordered:3
		\\kind eq same: True
		\\kind eq different: False
		\\task inspect: Checked
		\\task str: checked
		\\task eq: True
		\\alignment inspect: Right
		\\alignment str: right
		\\alignment eq: True

	actual == expected
}

## Inline and block AST nodes expose useful inspect strings and structural equality.
expect {
	inline : Markdown.Inline
	inline = Strong([Text("Roc")])

	block : Markdown
	block = Heading({ level: Two, content: [Text("Roc")] })

	actual =
		\\inline inspect: ${Str.inspect(inline)}
		\\inline debug: ${Markdown.inline_to_debug_str(inline)}
		\\inline eq same: ${Str.inspect(inline == Strong([Text("Roc")]))}
		\\inline eq different: ${Str.inspect(inline == Emphasis([Text("Roc")]))}
		\\block inspect: ${Str.inspect(block)}
		\\block debug: ${Markdown.to_debug_str(block)}
		\\block eq same: ${Str.inspect(block == Heading({ level: Two, content: [Text("Roc")] }))}
		\\block eq different: ${Str.inspect(block == Paragraph([Text("Roc")]))}

	expected =
		\\inline inspect: Strong([Text("Roc")])
		\\inline debug: Strong([Text("Roc")])
		\\inline eq same: True
		\\inline eq different: False
		\\block inspect: Heading({ level: Two, content: [Text("Roc")] })
		\\block debug: Heading({ level: Two, content: [Text("Roc")] })
		\\block eq same: True
		\\block eq different: False

	actual == expected
}

## Hash-prefixed headings parse with inline content.
expect {
	a = String.parse_str(Markdown.heading, "# Foo **Bar** #")?
	a == Heading({ level: One, content: [Text("Foo "), Strong([Text("Bar")])] })
}

## Underlined headings parse as level two headings.
expect {
	a = String.parse_str(Markdown.heading, "Foo Bar\n---")?
	a == Heading({ level: Two, content: [Text("Foo Bar")] })
}

## Markdown links parse as inline link nodes.
expect {
	a = String.parse_str(Markdown.link, "[roc](https://roc-lang.org \"Roc\")")?
	a == Link({ label: [Text("roc")], target: { href: "https://roc-lang.org", title: Some("Roc") } })
}

## Markdown images parse as inline image nodes.
expect {
	a = String.parse_str(Markdown.image, "![alt text](/images/logo.png)")?
	a == Image({ alt: [Text("alt text")], target: { href: "/images/logo.png", title: None } })
}

## Code blocks capture their info string and body text.
expect {
	text =
		\\```roc
		\\# some code
		\\foo = bar
		\\```

	a = String.parse_str(Markdown.code, text)?
	a == Code({ info: "roc", pre: "# some code\nfoo = bar\n" })
}

## Public inline parser parses emphasis, strong, strikethrough, code, and links.
expect {
	actual = String.parse_str(Markdown.inlines, "Intro with **bold**, *em*, ~~gone~~, `code`, and [a link](https://example.com).")?

	actual
		== [
			Text("Intro with "),
			Strong([Text("bold")]),
			Text(", "),
			Emphasis([Text("em")]),
			Text(", "),
			Strikethrough([Text("gone")]),
			Text(", "),
			InlineCode("code"),
			Text(", and "),
			Link({ label: [Text("a link")], target: { href: "https://example.com", title: None } }),
			Text("."),
		]
}

## Escaped inline delimiters parse as literal text.
expect {
	text = "\\*literal\\*, \\_em\\_, \\`code\\`, and \\[a link](target)"

	actual = String.parse_str(Markdown.inlines, text)?

	actual == [Text("*literal*, _em_, `code`, and [a link](target)")]
}

## Inline images parse inside prose.
expect {
	actual = String.parse_str(Markdown.inlines, "Logo ![Roc](/roc.png) here")?

	actual
		== [
			Text("Logo "),
			Image({ alt: [Text("Roc")], target: { href: "/roc.png", title: None } }),
			Text(" here"),
		]
}

## Autolinks and bare URLs parse as inline links.
expect {
	actual = String.parse_str(Markdown.inlines, "<https://example.com> and www.example.com")?

	actual
		== [
			Link({ label: [Text("https://example.com")], target: { href: "https://example.com", title: None } }),
			Text(" and "),
			Link({ label: [Text("www.example.com")], target: { href: "www.example.com", title: None } }),
		]
}

## Hard line breaks parse from trailing spaces and backslash newlines.
expect {
	actual = String.parse_str(Markdown.inlines, "one  \ntwo\\\nthree")?

	actual == [Text("one"), HardBreak, Text("two"), HardBreak, Text("three")]
}

## Raw HTML inline spans are preserved.
expect {
	actual = String.parse_str(Markdown.inlines, "Hello <span>world</span>")?

	actual == [Text("Hello "), HtmlInline("<span>"), Text("world"), HtmlInline("</span>")]
}

## Reference links resolve using definitions and definitions are omitted from blocks.
expect {
	text =
		\\[roc]: https://roc-lang.org "Roc"
		\\Read [Roc][roc].

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			Paragraph([
				Text("Read "),
				Link({ label: [Text("Roc")], target: { href: "https://roc-lang.org", title: Some("Roc") } }),
				Text("."),
			]),
		]
}

## Unresolved references remain literal text.
expect {
	actual = String.parse_str(Markdown.all, "Read [Roc][missing].")?

	actual == [Paragraph([Text("Read [Roc][missing].")])]
}

## Frontmatter is preserved only at the start of a document.
expect {
	text =
		\\---
		\\title: Hello
		\\---
		\\Body

	actual = String.parse_str(Markdown.all, text)?

	actual == [Frontmatter({ raw: "title: Hello\n" }), Paragraph([Text("Body")])]
}

## Standalone thematic breaks parse as block nodes.
expect {
	actual = String.parse_str(Markdown.all, "- - -")?

	actual == [ThematicBreak]
}

## Ordered lists preserve their starting number and task list state.
expect {
	text =
		\\3) [x] Done
		\\4) [ ] Later

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Ordered({ start: 3 }),
				loose: Bool.False,
				items: [
					{ task: Checked, blocks: [Paragraph([Text("Done")])] },
					{ task: Unchecked, blocks: [Paragraph([Text("Later")])] },
				],
			}),
		]
}

## Unordered marker variants parse into one unordered list.
expect {
	text =
		\\* One
		\\+ Two

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: Bool.False,
				items: [
					{ task: NoTask, blocks: [Paragraph([Text("One")])] },
					{ task: NoTask, blocks: [Paragraph([Text("Two")])] },
				],
			}),
		]
}

## Blank lines inside lists mark the list as loose.
expect {
	text =
		\\- One
		\\
		\\- Two

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: Bool.True,
				items: [
					{ task: NoTask, blocks: [Paragraph([Text("One")])] },
					{ task: NoTask, blocks: [Paragraph([Text("Two")])] },
				],
			}),
		]
}

## Nested unordered lists parse as child blocks.
expect {
	text =
		\\- One
		\\  - Nested

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: Bool.False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: Bool.False,
								items: [
									{ task: NoTask, blocks: [Paragraph([Text("Nested")])] },
								],
							}),
						],
					},
				],
			}),
		]
}

## Tilde fenced code blocks parse with info strings.
expect {
	text =
		\\~~~roc
		\\main = 1
		\\~~~

	actual = String.parse_str(Markdown.all, text)?

	actual == [Code({ info: "roc", pre: "main = 1\n" })]
}

## Indented code blocks parse from four leading spaces.
expect {
	actual = String.parse_str(Markdown.all, "    main = 1")?

	actual == [Code({ info: "", pre: "main = 1\n" })]
}

## Pipe tables parse header alignment and inline cell content.
expect {
	text =
		\\| Name | Count |
		\\| :--- | ---: |
		\\| **Roc** | `1|2` |

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			Table({
				header: [[Text("Name")], [Text("Count")]],
				align: [Left, Right],
				rows: [
					[
						[Strong([Text("Roc")])],
						[InlineCode("1|2")],
					],
				],
			}),
		]
}

## Malformed tables remain paragraph text.
expect {
	text =
		\\Name | Count
		\\not a delimiter

	actual = String.parse_str(Markdown.all, text)?

	actual == [Paragraph([Text("Name | Count not a delimiter")])]
}

## Raw HTML blocks are preserved without validation.
expect {
	text =
		\\<section>
		\\raw
		\\</section>

	actual = String.parse_str(Markdown.all, text)?

	actual == [HtmlBlock("<section>\nraw\n</section>\n")]
}

## Unclosed fenced code blocks fail document parsing.
expect {
	text =
		\\```roc
		\\main = 1

	String.parse_str(Markdown.all, text).is_err()
}

## Article body markdown parses into structured blocks without TODO fallbacks.
expect {
	text =
		\\---
		\\title: Article
		\\---
		\\# Title with **style**
		\\
		\\Intro with **bold**, `code`, and [a link](https://example.com).
		\\
		\\![alt text](/image.png)
		\\
		\\```roc
		\\main = 1
		\\```
		\\
		\\- One
		\\  - Nested
		\\
		\\> Quote with **strong** text

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			Frontmatter({ raw: "title: Article\n" }),
			Heading({ level: One, content: [Text("Title with "), Strong([Text("style")])] }),
			Paragraph([
				Text("Intro with "),
				Strong([Text("bold")]),
				Text(", "),
				InlineCode("code"),
				Text(", and "),
				Link({ label: [Text("a link")], target: { href: "https://example.com", title: None } }),
				Text("."),
			]),
			Paragraph([Image({ alt: [Text("alt text")], target: { href: "/image.png", title: None } })]),
			Code({ info: "roc", pre: "main = 1\n" }),
			ListBlock({
				kind: Unordered,
				loose: Bool.False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: Bool.False,
								items: [
									{ task: NoTask, blocks: [Paragraph([Text("Nested")])] },
								],
							}),
						],
					},
				],
			}),
			Blockquote([
				Paragraph([
					Text("Quote with "),
					Strong([Text("strong")]),
					Text(" text"),
				]),
			]),
		]
}

chomp_until_code_block_end : Parser(String.Utf8, Str)
chomp_until_code_block_end =
	Parser.build_primitive_parser(
		|input| {
			chomp_to_code_block_end_help({ val: List.with_capacity(1000), input })
		},
	)
		.map(String.str_from_utf8)

chomp_to_code_block_end_help : { val : String.Utf8, input : String.Utf8 } -> Parser.ParseResult(String.Utf8, String.Utf8)
chomp_to_code_block_end_help = |{ val, input }| {
	match input {
		[] => Err(ParsingFailure("expected ```, ran out of input"))
		['`', '`', '`', .. as rest] => Ok({ val, input: rest })
		[first, .. as rest] => chomp_to_code_block_end_help({ val: val.append(first), input: rest })
	}
}

## Chomping code block contents stops before the closing backticks.
expect {
	val = "".to_utf8()
	input = "some code\n```".to_utf8()
	expected = "some code\n".to_utf8()
	a = chomp_to_code_block_end_help({ val, input })?
	a == { val: expected, input: [] }
}
