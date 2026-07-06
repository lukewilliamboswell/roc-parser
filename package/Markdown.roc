import Parser
import String

# # Content values
Markdown := [
	Heading(Level, List(Inline)),
	Paragraph(List(Inline)),
	Blockquote(List(Markdown)),
	UnorderedList(List(Markdown)),
	OrderedList(List(Markdown)),
	ListItem(List(Inline), List(Markdown)),
	HorizontalRule,
	Link({ alt : Str, href : Str }),
	Image({ alt : Str, href : Str }),
	Code({ ext : Str, pre : Str }),
	TODO(Str),
].{
	Level : [One, Two, Three, Four, Five, Six]

	Inline := [
		Text(Str),
		Strong(List(Inline)),
		Emphasis(List(Inline)),
		InlineCode(Str),
		InlineLink({ alt : List(Inline), href : Str }),
	]

	all : Parser(String.Utf8, List(Markdown))
	all = parse_all

	inlines : Parser(String.Utf8, List(Inline))
	inlines = parse_inlines_parser

	## Headings
	##
	## ```
	## expect String.parse_str(heading, "# Foo Bar") == Ok(Heading(One, [Text("Foo Bar")]))
	## expect String.parse_str(heading, "Foo Bar\n---") == Ok(Heading(Two, [Text("Foo Bar")]))
	## ```
	heading : Parser(String.Utf8, Markdown)
	heading =
		Parser.one_of(
			[
				inline_heading,
				two_line_heading_level_one,
				two_line_heading_level_two,
			],
		)

	## Links
	##
	## ```roc
	## expect String.parse_str(link, "[roc](https://roc-lang.org)") == Ok(Link({ alt: "roc", href: "https://roc-lang.org" }))
	## ```
	link : Parser(String.Utf8, Markdown)
	link =
		Parser.const(
			|alt| {
				|href| {
					Link({ alt, href })
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

	## Images
	##
	## ```roc
	## expect String.parse_str(image, "![alt text](/images/logo.png)") == Ok(Image({ alt: "alt text", href: "/images/logo.png" }))
	## ```
	image : Parser(String.Utf8, Markdown)
	image =
		Parser.const(
			|alt| {
				|href| {
					Image({ alt, href })
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

	## Parse code blocks using triple backticks
	## supports block extension e.g. ```roc
	##
	## ```roc
	## expect {
	##     text =
	##         \\```roc
	##         \\# some code
	##         \\foo = bar
	##         \\```
	##
	##     a = String.parse_str(code, text)
	##     a == Ok(Code({ ext: "roc", pre: "# some code\nfoo = bar\n" }))
	## }
	## ```
	code : Parser(String.Utf8, Markdown)
	code =
		Parser.const(|ext| |pre| Code({ ext, pre }))
			.keep(
				Parser.one_of(
					[
						# parse backticks with ext e.g. ```roc
						Parser.const(|i| i)
							.skip(String.string("```"))
							.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
							.skip(end_of_line),

						# parse just backticks e.g. ```
						Parser.const("")
							.skip(String.string("```")),
					],
				),
			)
			.keep(chomp_until_code_block_end)
}

Line : { raw : String.Utf8 }

parse_all : Parser(String.Utf8, List(Markdown))
parse_all =
	Parser.build_primitive_parser(
		|input| {
			lines = split_lines(input)
			parsed = parse_blocks_from_lines(lines, 0)?

			match parsed.input {
				[] =>
					Ok({ val: parsed.val, input: [] })

				_ =>
					Err(ParsingFailure("unexpected unparsed markdown lines"))
			}
		},
	)

parse_blocks_from_lines : List(Line), U64 -> Try({ val : List(Markdown), input : List(Line) }, [ParsingFailure(Str)])
parse_blocks_from_lines = |lines, min_indent| {
	parse_blocks_help(lines, min_indent, [])
}

parse_blocks_help : List(Line), U64, List(Markdown) -> Try({ val : List(Markdown), input : List(Line) }, [ParsingFailure(Str)])
parse_blocks_help = |lines, min_indent, blocks| {
	match lines {
		[] =>
			Ok({ val: blocks, input: [] })

		[line, .. as rest] if line_is_blank(line) =>
			parse_blocks_help(rest, min_indent, blocks)

		[line, ..] if line_indent(line) < min_indent =>
			Ok({ val: blocks, input: lines })

		_ => {
			parsed = parse_one_block(lines, min_indent)?
			parse_blocks_help(parsed.input, min_indent, blocks.append(parsed.val))
		}
	}
}

parse_one_block : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_one_block = |lines, min_indent| {
	match lines {
		[line, underline, .. as rest_after_heading] if can_be_setext_heading_text(line, min_indent) => {
			match underline_level(underline, min_indent) {
				Ok(level) =>
					Ok({ val: Heading(level, parse_inlines(strip_indent(line, min_indent))), input: rest_after_heading })

				Err(_) =>
					parse_one_block_without_setext(lines, min_indent)
			}
		}

		_ =>
			parse_one_block_without_setext(lines, min_indent)
	}
}

parse_one_block_without_setext : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_one_block_without_setext = |lines, min_indent| {
	match lines {
		[] =>
			Err(ParsingFailure("expected a markdown block"))

		[line, .. as rest] => {
			content = strip_indent(line, min_indent)

			match parse_hash_heading_line(content) {
				Ok(block) =>
					Ok({ val: block, input: rest })

				Err(_) if is_code_fence_line(line, min_indent) =>
					parse_code_block(lines, min_indent)

				Err(_) if is_horizontal_rule_line(line, min_indent) =>
					Ok({ val: HorizontalRule, input: rest })

				Err(_) => {
					match parse_image_line(content) {
						Ok(block) =>
							Ok({ val: block, input: rest })

						Err(_) => {
							if is_blockquote_line(line, min_indent) {
								parse_blockquote(lines, min_indent)
							} else if is_unordered_list_item_line(line, min_indent) {
								parse_unordered_list(lines, min_indent)
							} else if is_ordered_list_item_line(line, min_indent) {
								parse_ordered_list(lines, min_indent)
							} else {
								parse_paragraph(lines, min_indent)
							}
						}
					}
				}
			}
		}
	}
}

parse_paragraph : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_paragraph = |lines, min_indent| {
	collected = collect_paragraph_lines(lines, min_indent, [])

	if collected.val.is_empty() {
		Err(ParsingFailure("expected a paragraph line"))
	} else {
		text = join_lines_with_spaces(collected.val)
		Ok({ val: Paragraph(parse_inlines(text)), input: collected.input })
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

parse_code_block : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_code_block = |lines, min_indent| {
	match lines {
		[line, .. as rest] => {
			fence = strip_indent(line, min_indent)
			ext = String.str_from_utf8(fence.drop_first(3))
			code = collect_code_lines(rest, min_indent, [])?

			Ok({ val: Code({ ext, pre: String.str_from_utf8(code.val) }), input: code.input })
		}

		[] =>
			Err(ParsingFailure("expected a code fence"))
	}
}

collect_code_lines : List(Line), U64, String.Utf8 -> Try({ val : String.Utf8, input : List(Line) }, [ParsingFailure(Str)])
collect_code_lines = |lines, min_indent, pre| {
	match lines {
		[] =>
			Err(ParsingFailure("expected closing ```"))

		[line, .. as rest] if is_code_fence_line(line, min_indent) =>
			Ok({ val: pre, input: rest })

		[line, .. as rest] => {
			body_line = 
				if line_indent(line) >= min_indent {
					strip_indent(line, min_indent)
				} else {
					line.raw
				}

			collect_code_lines(rest, min_indent, append_line(pre, body_line))
		}
	}
}

parse_blockquote : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_blockquote = |lines, min_indent| {
	quoted = collect_blockquote_lines(lines, min_indent, [])
	inner = parse_blocks_from_lines(quoted.val, 0)?

	Ok({ val: Blockquote(inner.val), input: quoted.input })
}

collect_blockquote_lines : List(Line), U64, List(Line) -> { val : List(Line), input : List(Line) }
collect_blockquote_lines = |lines, min_indent, acc| {
	match lines {
		[] =>
			{ val: acc, input: [] }

		[line, .. as rest] if is_blockquote_line(line, min_indent) => {
			content = strip_blockquote_marker(line, min_indent)
			collect_blockquote_lines(rest, min_indent, acc.append({ raw: content }))
		}

		_ =>
			{ val: acc, input: lines }
	}
}

parse_unordered_list : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_unordered_list = |lines, min_indent| {
	parsed = parse_list_items(lines, min_indent, [], unordered_list_item_content)?
	Ok({ val: UnorderedList(parsed.val), input: parsed.input })
}

parse_ordered_list : List(Line), U64 -> Try({ val : Markdown, input : List(Line) }, [ParsingFailure(Str)])
parse_ordered_list = |lines, min_indent| {
	parsed = parse_list_items(lines, min_indent, [], ordered_list_item_content)?
	Ok({ val: OrderedList(parsed.val), input: parsed.input })
}

parse_list_items : List(Line), U64, List(Markdown), (Line, U64 -> Try(String.Utf8, [NotFound])) -> Try({ val : List(Markdown), input : List(Line) }, [ParsingFailure(Str)])
parse_list_items = |lines, min_indent, items, list_item_content| {
	match lines {
		[line, .. as rest] => {
			match list_item_content(line, min_indent) {
				Ok(content) => {
					content_indent = min_indent + 2
					continuation = collect_list_item_continuation(rest, content_indent, [content])
					children = parse_blocks_from_lines(continuation.input, content_indent)?
					item = ListItem(parse_inlines(join_lines_with_spaces(continuation.val)), children.val)

					parse_list_items(children.input, min_indent, items.append(item), list_item_content)
				}

				Err(_) =>
					Ok({ val: items, input: lines })
			}
		}

		_ =>
			Ok({ val: items, input: lines })
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

is_block_start : Line, U64 -> Bool
is_block_start = |line, min_indent| {
	if line_indent(line) < min_indent {
		Bool.False
	} else {
		content = strip_indent(line, min_indent)

		is_hash_heading_line(content)
			or is_horizontal_rule_line(line, min_indent)
			or is_code_fence_line(line, min_indent)
			or parse_image_line(content).is_ok()
			or is_blockquote_line(line, min_indent)
			or is_unordered_list_item_line(line, min_indent)
			or is_ordered_list_item_line(line, min_indent)
	}
}

can_be_setext_heading_text : Line, U64 -> Bool
can_be_setext_heading_text = |line, min_indent| {
	(!line_is_blank(line)) and line_indent(line) >= min_indent and !is_block_start(line, min_indent)
}

underline_level : Line, U64 -> Try(Level, [NotFound])
underline_level = |line, min_indent| {
	if line_indent(line) != min_indent {
		Err(NotFound)
	} else {
		content = strip_indent(line, min_indent)

		match content {
			['=', '=', .. as rest] if all_bytes_are(rest, '=') =>
				Ok(One)

			['-', '-', .. as rest] if all_bytes_are(rest, '-') =>
				Ok(Two)

			_ =>
				Err(NotFound)
		}
	}
}

parse_hash_heading_line : String.Utf8 -> Try(Markdown, [NotFound])
parse_hash_heading_line = |content| {
	match content {
		['#', ' ', .. as text] =>
			Ok(Heading(One, parse_inlines(text)))

		['#', '#', ' ', .. as text] =>
			Ok(Heading(Two, parse_inlines(text)))

		['#', '#', '#', ' ', .. as text] =>
			Ok(Heading(Three, parse_inlines(text)))

		['#', '#', '#', '#', ' ', .. as text] =>
			Ok(Heading(Four, parse_inlines(text)))

		['#', '#', '#', '#', '#', ' ', .. as text] =>
			Ok(Heading(Five, parse_inlines(text)))

		['#', '#', '#', '#', '#', '#', ' ', .. as text] =>
			Ok(Heading(Six, parse_inlines(text)))

		_ =>
			Err(NotFound)
	}
}

is_hash_heading_line : String.Utf8 -> Bool
is_hash_heading_line = |content| {
	parse_hash_heading_line(content).is_ok()
}

parse_image_line : String.Utf8 -> Try(Markdown, [NotFound])
parse_image_line = |content| {
	match content {
		['!', '[', .. as rest] => {
			label = find_sequence(rest, "](".to_utf8())?
			href = find_sequence(label.after, ")".to_utf8())?

			if href.after.is_empty() {
				Ok(Image({ alt: String.str_from_utf8(label.before), href: String.str_from_utf8(href.before) }))
			} else {
				Err(NotFound)
			}
		}

		_ =>
			Err(NotFound)
	}
}

parse_link_line : String.Utf8 -> Try(Markdown, [NotFound])
parse_link_line = |content| {
	match content {
		['[', .. as rest] => {
			label = find_sequence(rest, "](".to_utf8())?
			href = find_sequence(label.after, ")".to_utf8())?

			if href.after.is_empty() {
				Ok(Link({ alt: String.str_from_utf8(label.before), href: String.str_from_utf8(href.before) }))
			} else {
				Err(NotFound)
			}
		}

		_ =>
			Err(NotFound)
	}
}

is_code_fence_line : Line, U64 -> Bool
is_code_fence_line = |line, min_indent| {
	line_indent(line) >= min_indent and starts_with_bytes(strip_indent(line, min_indent), "```".to_utf8())
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

is_unordered_list_item_line : Line, U64 -> Bool
is_unordered_list_item_line = |line, min_indent| {
	unordered_list_item_content(line, min_indent).is_ok()
}

unordered_list_item_content : Line, U64 -> Try(String.Utf8, [NotFound])
unordered_list_item_content = |line, min_indent| {
	if line_indent(line) == min_indent and starts_with_bytes(strip_indent(line, min_indent), "- ".to_utf8()) {
		Ok(strip_indent(line, min_indent).drop_first(2))
	} else {
		Err(NotFound)
	}
}

is_ordered_list_item_line : Line, U64 -> Bool
is_ordered_list_item_line = |line, min_indent| {
	ordered_list_item_content(line, min_indent).is_ok()
}

ordered_list_item_content : Line, U64 -> Try(String.Utf8, [NotFound])
ordered_list_item_content = |line, min_indent| {
	if line_indent(line) == min_indent {
		ordered_marker_content(strip_indent(line, min_indent))
	} else {
		Err(NotFound)
	}
}

ordered_marker_content : String.Utf8 -> Try(String.Utf8, [NotFound])
ordered_marker_content = |content| {
	match content {
		[first, .. as rest] if is_digit_byte(first) =>
			ordered_marker_content_help(rest)

		_ =>
			Err(NotFound)
	}
}

ordered_marker_content_help : String.Utf8 -> Try(String.Utf8, [NotFound])
ordered_marker_content_help = |content| {
	match content {
		['.', ' ', .. as rest] =>
			Ok(rest)

		[first, .. as rest] if is_digit_byte(first) =>
			ordered_marker_content_help(rest)

		_ =>
			Err(NotFound)
	}
}

is_horizontal_rule_line : Line, U64 -> Bool
is_horizontal_rule_line = |line, min_indent| {
	if line_indent(line) != min_indent {
		Bool.False
	} else {
		content = strip_indent(line, min_indent)

		match content {
			['-', '-', '-', .. as rest] if all_bytes_are(rest, '-') =>
				Bool.True

			['*', '*', '*', .. as rest] if all_bytes_are(rest, '*') =>
				Bool.True

			['_', '_', '_', .. as rest] if all_bytes_are(rest, '_') =>
				Bool.True

			_ =>
				Bool.False
		}
	}
}

is_digit_byte : U8 -> Bool
is_digit_byte = |byte| {
	byte >= '0' and byte <= '9'
}

parse_inlines_parser : Parser(String.Utf8, List(Inline))
parse_inlines_parser =
	Parser.build_primitive_parser(
		|input| {
			Ok({ val: parse_inlines(input), input: [] })
		},
	)

parse_inlines : String.Utf8 -> List(Inline)
parse_inlines = |input| {
	parse_inlines_help(input, [], [])
}

parse_inlines_help : String.Utf8, String.Utf8, List(Inline) -> List(Inline)
parse_inlines_help = |input, text, nodes| {
	match input {
		[] =>
			flush_text(text, nodes)

		['\\', escaped, .. as rest] if is_escapable_inline_byte(escaped) =>
			parse_inlines_help(rest, text.append(escaped), nodes)

		['\\', .. as rest] =>
			parse_inlines_help(rest, text.append('\\'), nodes)

		['*', '*', .. as rest] => {
			match find_unescaped_sequence(rest, "**".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Strong(parse_inlines(found.before)))
					parse_inlines_help(found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(rest, text.append('*').append('*'), nodes)
			}
		}

		['*', .. as rest] => {
			match find_unescaped_sequence(rest, "*".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Emphasis(parse_inlines(found.before)))
					parse_inlines_help(found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(rest, text.append('*'), nodes)
			}
		}

		['_', .. as rest] => {
			match find_unescaped_sequence(rest, "_".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(Emphasis(parse_inlines(found.before)))
					parse_inlines_help(found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(rest, text.append('_'), nodes)
			}
		}

		['`', .. as rest] => {
			match find_unescaped_sequence(rest, "`".to_utf8()) {
				Ok(found) => {
					next_nodes = flush_text(text, nodes).append(InlineCode(String.str_from_utf8(found.before)))
					parse_inlines_help(found.after, [], next_nodes)
				}

				Err(_) =>
					parse_inlines_help(rest, text.append('`'), nodes)
			}
		}

		['[', .. as rest] => {
			match find_unescaped_sequence(rest, "](".to_utf8()) {
				Ok(label) => {
					match find_unescaped_sequence(label.after, ")".to_utf8()) {
						Ok(href) => {
							next_nodes =
								flush_text(text, nodes)
									.append(InlineLink({ alt: parse_inlines(label.before), href: String.str_from_utf8(href.before) }))

							parse_inlines_help(href.after, [], next_nodes)
						}

						Err(_) =>
							parse_inlines_help(rest, text.append('['), nodes)
					}
				}

				Err(_) =>
					parse_inlines_help(rest, text.append('['), nodes)
			}
		}

		[first, .. as rest] =>
			parse_inlines_help(rest, text.append(first), nodes)
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
}

flush_text : String.Utf8, List(Inline) -> List(Inline)
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

starts_with_bytes : String.Utf8, String.Utf8 -> Bool
starts_with_bytes = |input, prefix| {
	{ before: start, others: _ } = input.split_at(prefix.len())
	start == prefix
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

line_indent : Line -> U64
line_indent = |line| {
	count_leading_spaces(line.raw, 0)
}

count_leading_spaces : String.Utf8, U64 -> U64
count_leading_spaces = |input, count| {
	match input {
		[' ', .. as rest] =>
			count_leading_spaces(rest, count + 1)

		_ =>
			count
	}
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

all_bytes_are : String.Utf8, U8 -> Bool
all_bytes_are = |bytes, expected| {
	match bytes {
		[] =>
			Bool.True

		[first, .. as rest] =>
			first == expected and all_bytes_are(rest, expected)
	}
}

join_lines_with_spaces : List(String.Utf8) -> String.Utf8
join_lines_with_spaces = |lines| {
	join_lines_with_spaces_help(lines, [])
}

join_lines_with_spaces_help : List(String.Utf8), String.Utf8 -> String.Utf8
join_lines_with_spaces_help = |lines, acc| {
	match lines {
		[] =>
			acc

		[line, .. as rest] if acc.is_empty() =>
			join_lines_with_spaces_help(rest, append_bytes(acc, line))

		[line, .. as rest] =>
			join_lines_with_spaces_help(rest, append_bytes(acc.append(' '), line))
	}
}

append_line : String.Utf8, String.Utf8 -> String.Utf8
append_line = |acc, line| {
	append_bytes(acc, line).append('\n')
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

# # temporyary parser for anything that is not yet supported
# # just parse into a TODO tag for now
todo : Parser(String.Utf8, Markdown)
todo =
	Parser.const(|s| TODO(s))
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

## Unsupported markdown lines can still be preserved as TODO nodes directly.
expect {
	a = String.parse_str(todo, "Foo Bar")?
	a == TODO("Foo Bar")
}

## Multiple markdown lines parse into paragraph nodes and blank lines separate blocks.
expect {
	a = String.parse_str(Markdown.all, "Foo Bar\n\nBaz")?
	a == [Paragraph([Text("Foo Bar")]), Paragraph([Text("Baz")])]
}

end_of_line = Parser.one_of([String.string("\n"), String.string("\r\n")])

not_end_of_line = |b| {
	b != '\n' and b != '\r'
}

## Hash-prefixed headings parse with their heading level.
expect {
	a = String.parse_str(Markdown.heading, "# Foo Bar")?
	a == Heading(One, [Text("Foo Bar")])
}

## Underlined headings parse as level two headings.
expect {
	a = String.parse_str(Markdown.heading, "Foo Bar\n---")?
	a == Heading(Two, [Text("Foo Bar")])
}

inline_heading =
	Parser.const(
		|level| {
			|str| {
				Heading(level, parse_inlines(str.to_utf8()))
			}
		},
	)
		.keep(
			Parser.one_of(
				[
					Parser.const(One).skip(String.string("# ")),
					Parser.const(Two).skip(String.string("## ")),
					Parser.const(Three).skip(String.string("### ")),
					Parser.const(Four).skip(String.string("#### ")),
					Parser.const(Five).skip(String.string("##### ")),
					Parser.const(Six).skip(String.string("###### ")),
				],
			),
		)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

## Inline headings capture the heading text after the marker.
expect {
	a = String.parse_str(inline_heading, "# Foo Bar")?
	a == Heading(One, [Text("Foo Bar")])
}

## Inline heading partial parsing leaves the following line untouched.
expect {
	a = String.parse_str_partial(inline_heading, "### Foo Bar\nBaz")?
	a == { val: Heading(Three, [Text("Foo Bar")]), input: "\nBaz" }
}

two_line_heading_level_one =
	Parser.const(
		|str| {
			Heading(One, parse_inlines(str.to_utf8()))
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

## Equal-sign underlines parse as level one headings.
expect {
	a = String.parse_str(two_line_heading_level_one, "Foo Bar\n==")?
	a == Heading(One, [Text("Foo Bar")])
}

## Level one heading partial parsing leaves the trailing newline.
expect {
	a = String.parse_str_partial(two_line_heading_level_one, "Foo Bar\n=============\n")?
	a == { val: Heading(One, [Text("Foo Bar")]), input: "\n" }
}

two_line_heading_level_two =
	Parser.const(
		|str| {
			Heading(Two, parse_inlines(str.to_utf8()))
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

## Dash underlines parse as level two headings.
expect {
	a = String.parse_str(two_line_heading_level_two, "Foo Bar\n---")?
	a == Heading(Two, [Text("Foo Bar")])
}

## Level two heading partial parsing leaves the following line.
expect {
	a = String.parse_str_partial(two_line_heading_level_two, "Foo Bar\n-----\nApples")?
	a == { val: Heading(Two, [Text("Foo Bar")]), input: "\nApples" }
}

## Markdown links capture their label and target URL.
expect {
	a = String.parse_str(Markdown.link, "[roc](https://roc-lang.org)")?
	a == Link({ alt: "roc", href: "https://roc-lang.org" })
}

## Link partial parsing leaves the next line untouched.
expect {
	a = String.parse_str_partial(Markdown.link, "[roc](https://roc-lang.org)\nApples")?
	a == { val: Link({ alt: "roc", href: "https://roc-lang.org" }), input: "\nApples" }
}

## Markdown images capture their alt text and source URL.
expect {
	a = String.parse_str(Markdown.image, "![alt text](/images/logo.png)")?
	a == Image({ alt: "alt text", href: "/images/logo.png" })
}

## Image partial parsing leaves the next line untouched.
expect {
	a = String.parse_str_partial(Markdown.image, "![alt text](/images/logo.png)\nApples")?
	a == { val: Image({ alt: "alt text", href: "/images/logo.png" }), input: "\nApples" }
}

## Code blocks capture their extension and body text.
expect {
	text =
		\\```roc
		\\# some code
		\\foo = bar
		\\```

	a = String.parse_str(Markdown.code, text)?
	a == Code({ ext: "roc", pre: "# some code\nfoo = bar\n" })
}

## Public inline parser parses text, emphasis, strong, code, and prose links.
expect {
	actual = String.parse_str(Markdown.inlines, "Intro with **bold**, *em*, _also em_, `code`, and [a link](https://example.com).")?

	actual
		== [
			Text("Intro with "),
			Strong([Text("bold")]),
			Text(", "),
			Emphasis([Text("em")]),
			Text(", "),
			Emphasis([Text("also em")]),
			Text(", "),
			InlineCode("code"),
			Text(", and "),
			InlineLink({ alt: [Text("a link")], href: "https://example.com" }),
			Text("."),
		]
}

## Escaped inline delimiters parse as literal text.
expect {
	text = "\\*literal\\*, \\_em\\_, \\`code\\`, and \\[a link](https://example.com)"

	actual = String.parse_str(Markdown.inlines, text)?

	actual == [Text("*literal*, _em_, `code`, and [a link](https://example.com)")]
}

## Heading text can contain inline spans.
expect {
	actual = String.parse_str(Markdown.heading, "# Title with **strong** text")?

	actual
		== Heading(
			One,
			[
				Text("Title with "),
				Strong([Text("strong")]),
				Text(" text"),
			],
		)
}

## Standalone links in documents parse as paragraph inline links.
expect {
	actual = String.parse_str(Markdown.all, "[roc](https://roc-lang.org)")?

	actual
		== [
			Paragraph(
				[
					InlineLink({ alt: [Text("roc")], href: "https://roc-lang.org" }),
				],
			),
		]
}

## Horizontal rules parse as block nodes.
expect {
	actual = String.parse_str(Markdown.all, "---")?

	actual == [HorizontalRule]
}

## Ordered list items parse into ordered list blocks.
expect {
	text =
		\\1. One
		\\2. Two

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			OrderedList(
				[
					ListItem([Text("One")], []),
					ListItem([Text("Two")], []),
				],
			),
		]
}

## Indented list continuation lines are folded into the item text.
expect {
	text =
		\\- first line
		\\  continued line

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			UnorderedList(
				[
					ListItem([Text("first line continued line")], []),
				],
			),
		]
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
		\\# Title
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
			Heading(One, [Text("Title")]),
			Paragraph(
				[
					Text("Intro with "),
					Strong([Text("bold")]),
					Text(", "),
					InlineCode("code"),
					Text(", and "),
					InlineLink({ alt: [Text("a link")], href: "https://example.com" }),
					Text("."),
				],
			),
			Image({ alt: "alt text", href: "/image.png" }),
			Code({ ext: "roc", pre: "main = 1\n" }),
			UnorderedList(
				[
					ListItem(
						[Text("One")],
						[
							UnorderedList(
								[
									ListItem([Text("Nested")], []),
								],
							),
						],
					),
				],
			),
			Blockquote(
				[
					Paragraph(
						[
							Text("Quote with "),
							Strong([Text("strong")]),
							Text(" text"),
						],
					),
				],
			),
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
