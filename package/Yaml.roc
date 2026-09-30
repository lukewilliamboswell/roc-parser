import String

## A practical YAML configuration parser.
##
## This module implements a deliberately small YAML 1.2-style subset aimed at
## configuration files and Markdown frontmatter. It supports a single block
## document, nested mappings and sequences, flow collections, comments, block
## scalars, quoted strings, and common null, boolean, integer, and floating-point
## scalars.
##
## Anchors, aliases, tags, directives, complex keys, and multi-document
## streams are rejected with a parse error.
Yaml := [
	Null,
	Bool(Bool),
	Int(I64),
	Float(F64),
	String(Str),
	Sequence(List(Yaml)),
	Mapping(List({ key : Str, value : Yaml })),
].{

	## Location and explanation of invalid YAML input. Lines and columns are one-based.
	Error : { line : U64, column : U64, message : Str }

	## Compare two parsed YAML values structurally.
	is_eq : _

	## Parse one YAML configuration document from a string.
	##
	## Empty input produces `Null`. Invalid input returns `YamlError` with a
	## one-based line and column. See the module description for the supported subset.
	##
	## ```roc
	## expect Yaml.parse_str("draft: false") == Ok(Mapping([{ key: "draft", value: Bool(Bool.False) }]))
	## ```
	parse_str : Str -> Try(Yaml, [YamlError(Error)])
	parse_str = |input| {
		raw_lines = split_lines(input.to_utf8(), 1, [], [])
		lines = prepare_lines(raw_lines)?

		match lines {
			[] => Ok(Null)

			[first, ..] if first.indent != 0 =>
				fail(first.number, 1, "the document root must not be indented")

			[first, ..] => {
				parsed = parse_node(lines, first.indent, 0, raw_lines)?

				match parsed.input {
					[] => Ok(parsed.val)

					[leftover, ..] =>
						fail(leftover.number, leftover.indent + 1, "unexpected content after the document root")
					}
			}
		}
	}

	## Render a parsed YAML value in Roc source-like notation for inspection.
	to_inspect : Yaml -> Str
	to_inspect = |value| inspect_yaml(value)
}

Line : { content : String.Utf8, indent : U64, number : U64 }

ParseResult : { val : Yaml, input : List(Line) }

Quote : [NoQuote, SingleQuote, DoubleQuote]

BlockStyle : [LiteralBlock, FoldedBlock]

BlockChomp : [ClipChomp, StripChomp, KeepChomp]

BlockIndent : [AutoIndent, ExplicitIndent(U64)]

ContentIndent : [PendingIndent, FixedIndent(U64)]

BlockHeader : { style : BlockStyle, chomp : BlockChomp, indent : BlockIndent }

BlockScalarResult : { value : Yaml, input : List(Line) }

parse_node : List(Line), U64, U64, List(Line) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_node = |lines, indent, depth, raw_lines| {
	if depth >= 100 {
		match lines {
			[line, ..] => fail(line.number, line.indent + 1, "YAML nesting exceeds the supported limit of 100 levels")
			[] => fail(1, 1, "YAML nesting exceeds the supported limit of 100 levels")
		}
	} else {
		match lines {
			[] => fail(1, 1, "expected a YAML value")

			[first, ..] if first.indent != indent =>
				fail(first.number, first.indent + 1, "unexpected indentation")

			[first, ..] if is_sequence_line(first.content) =>
				parse_sequence(lines, indent, depth, raw_lines)

			[first, ..] => {
				match split_mapping_entry(first.content) {
					Ok(_) => parse_mapping(lines, indent, depth, raw_lines)
					Err(_) => {
						if starts_block_scalar(first.content) {
							block = parse_block_scalar(lines.drop_first(1), raw_lines, first.content, first.number, first.indent + 1, first.indent)?
							Ok({ val: block.value, input: block.input })
						} else {
							value = parse_inline_value(first.content, first.number, first.indent + 1)?
							Ok({ val: value, input: lines.drop_first(1) })
						}
					}
				}
			}
		}
	}
}

parse_mapping : List(Line), U64, U64, List(Line) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_mapping = |lines, indent, depth, raw_lines| {
	parse_mapping_help(lines, indent, depth, raw_lines, [])
}

parse_mapping_help : List(Line), U64, U64, List(Line), List({ key : Str, value : Yaml }) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_mapping_help = |lines, indent, depth, raw_lines, entries| {
	match lines {
		[] => Ok({ val: Mapping(entries), input: [] })

		[line, ..] if line.indent < indent =>
			Ok({ val: Mapping(entries), input: lines })

		[line, ..] if line.indent > indent =>
			fail(line.number, line.indent + 1, "unexpected indentation after a mapping value")

		[line, ..] if is_sequence_line(line.content) =>
			Ok({ val: Mapping(entries), input: lines })

		[line, .. as rest] => {
			match split_mapping_entry(line.content) {
				Err(_) => Ok({ val: Mapping(entries), input: lines })

				Ok(parts) => {
					key = parse_key(parts.key, line.number, line.indent + 1)?

					if mapping_has_key(entries, key) {
						fail(line.number, line.indent + 1, "duplicate mapping key `${key}`")
					} else if parts.value.is_empty() {
						match rest {
							[next, ..] if next.indent > indent => {
								child = parse_node(rest, next.indent, depth + 1, raw_lines)?
								parse_mapping_help(child.input, indent, depth, raw_lines, entries.append({ key, value: child.val }))
							}

							_ =>
								parse_mapping_help(rest, indent, depth, raw_lines, entries.append({ key, value: Null }))
							}
					} else if starts_block_scalar(parts.value) {
						block = parse_block_scalar(rest, raw_lines, parts.value, line.number, line.indent + parts.value_column, line.indent)?
						parse_mapping_help(block.input, indent, depth, raw_lines, entries.append({ key, value: block.value }))
					} else {
						value = parse_inline_value(parts.value, line.number, line.indent + parts.value_column)?
						parse_mapping_help(rest, indent, depth, raw_lines, entries.append({ key, value }))
					}
				}
			}
		}
	}
}

parse_sequence : List(Line), U64, U64, List(Line) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_sequence = |lines, indent, depth, raw_lines| {
	parse_sequence_help(lines, indent, depth, raw_lines, [])
}

parse_sequence_help : List(Line), U64, U64, List(Line), List(Yaml) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_sequence_help = |lines, indent, depth, raw_lines, values| {
	match lines {
		[] => Ok({ val: Sequence(values), input: [] })

		[line, ..] if line.indent < indent =>
			Ok({ val: Sequence(values), input: lines })

		[line, ..] if line.indent > indent =>
			fail(line.number, line.indent + 1, "unexpected indentation after a sequence value")

		[line, ..] if !is_sequence_line(line.content) =>
			Ok({ val: Sequence(values), input: lines })

		[line, .. as rest] => {
			payload = sequence_payload(line.content)

			if payload.is_empty() {
				match rest {
					[next, ..] if next.indent > indent => {
						child = parse_node(rest, next.indent, depth + 1, raw_lines)?
						parse_sequence_help(child.input, indent, depth, raw_lines, values.append(child.val))
					}

					_ =>
						parse_sequence_help(rest, indent, depth, raw_lines, values.append(Null))
					}
			} else {
				match split_mapping_entry(payload) {
					Ok(_) => {
						virtual = { content: payload, indent: indent + 2, number: line.number }
						child = parse_node(List.prepend(rest, virtual), indent + 2, depth + 1, raw_lines)?
						parse_sequence_help(child.input, indent, depth, raw_lines, values.append(child.val))
					}

					Err(_) => {
						if starts_block_scalar(payload) {
							block = parse_block_scalar(rest, raw_lines, payload, line.number, line.indent + 3, line.indent)?
							parse_sequence_help(block.input, indent, depth, raw_lines, values.append(block.value))
						} else {
							value = parse_inline_value(payload, line.number, line.indent + 3)?
							parse_sequence_help(rest, indent, depth, raw_lines, values.append(value))
						}
					}
				}
			}
		}
	}
}

starts_block_scalar : String.Utf8 -> Bool
starts_block_scalar = |bytes| {
	match trim_spaces(bytes) {
		['|', ..] | ['>', ..] => Bool.True
		_ => Bool.False
	}
}

parse_block_scalar : List(Line), List(Line), String.Utf8, U64, U64, U64 -> Try(BlockScalarResult, [YamlError(Yaml.Error)])
parse_block_scalar = |clean_rest, raw_lines, header_bytes, line, column, base_indent| {
	header = parse_block_header(header_bytes, line, column)?
	raw_tail = drop_lines_before_number(raw_lines, line + 1)
	collected = collect_block_lines(raw_tail, base_indent, header.indent, PendingIndent, [], line)?
	body = render_block_scalar(collected.lines, header.style, header.chomp)
	input = drop_consumed_lines(clean_rest, collected.consumed_through)

	Ok({ value: String(String.str_from_utf8(body)), input })
}

parse_block_header : String.Utf8, U64, U64 -> Try(BlockHeader, [YamlError(Yaml.Error)])
parse_block_header = |raw, line, column| {
	bytes = trim_spaces(raw)

	match bytes {
		['|', .. as rest] =>
			parse_block_header_options(rest, { style: LiteralBlock, chomp: ClipChomp, indent: AutoIndent }, line, column)

		['>', .. as rest] =>
			parse_block_header_options(rest, { style: FoldedBlock, chomp: ClipChomp, indent: AutoIndent }, line, column)

		_ => fail(line, column, "invalid block scalar header")
	}
}

parse_block_header_options : String.Utf8, BlockHeader, U64, U64 -> Try(BlockHeader, [YamlError(Yaml.Error)])
parse_block_header_options = |bytes, header, line, column| {
	match bytes {
		[] => Ok(header)

		['-', .. as rest] => {
			match header.chomp {
				ClipChomp =>
					parse_block_header_options(
						rest,
						{ style: header.style, chomp: StripChomp, indent: header.indent },
						line,
						column,
					)

				_ => fail(line, column, "duplicate block scalar chomping indicator")
			}
		}

		['+', .. as rest] => {
			match header.chomp {
				ClipChomp =>
					parse_block_header_options(
						rest,
						{ style: header.style, chomp: KeepChomp, indent: header.indent },
						line,
						column,
					)

				_ => fail(line, column, "duplicate block scalar chomping indicator")
			}
		}

		[digit, .. as rest] if digit >= '1' and digit <= '9' => {
			match header.indent {
				AutoIndent => {
					indent = block_indent_from_digit(digit)
					parse_block_header_options(rest, { style: header.style, chomp: header.chomp, indent: ExplicitIndent(indent) }, line, column)
				}

				ExplicitIndent(_) => fail(line, column, "duplicate block scalar indentation indicator")
			}
		}

		_ => fail(line, column, "unsupported block scalar header")
	}
}

block_indent_from_digit : U8 -> U64
block_indent_from_digit = |digit| {
	match digit {
		'1' => 1
		'2' => 2
		'3' => 3
		'4' => 4
		'5' => 5
		'6' => 6
		'7' => 7
		'8' => 8
		'9' => 9
		_ => 1
	}
}

collect_block_lines : List(Line), U64, BlockIndent, ContentIndent, List(String.Utf8), U64 -> Try({ lines : List(String.Utf8), consumed_through : U64 }, [YamlError(Yaml.Error)])
collect_block_lines = |raw_lines, base_indent, block_indent, content_indent, out, consumed_through| {
	match raw_lines {
		[] => Ok({ lines: out, consumed_through })

		[line, .. as rest] => {
			indent = count_indent(line.content, 0, line.number)?
			without_indent = line.content.drop_first(indent)

			if trim_spaces(without_indent).is_empty() {
				collect_block_lines(rest, base_indent, block_indent, content_indent, out.append([]), line.number)
			} else if indent <= base_indent {
				Ok({ lines: out, consumed_through })
			} else {
				required_indent =
					match block_indent {
						ExplicitIndent(offset) => base_indent + offset

						AutoIndent =>
							match content_indent {
								PendingIndent => indent
								FixedIndent(value) => value
							}
					}

				if indent < required_indent {
					Ok({ lines: out, consumed_through })
				} else {
					stripped = line.content.drop_first(required_indent)

					next_content_indent =
						match block_indent {
							AutoIndent =>
								match content_indent {
									PendingIndent => FixedIndent(required_indent)
									FixedIndent(_) => content_indent
								}

							ExplicitIndent(_) => content_indent
						}

					collect_block_lines(rest, base_indent, block_indent, next_content_indent, out.append(stripped), line.number)
				}
			}
		}
	}
}

render_block_scalar : List(String.Utf8), BlockStyle, BlockChomp -> String.Utf8
render_block_scalar = |lines, style, chomp| {
	body =
		match style {
			LiteralBlock => join_block_lines(lines)
			FoldedBlock => fold_block_lines(lines)
		}

	with_terminal_newline =
		if lines.is_empty() {
			[]
		} else {
			body.append('\n')
		}

	match chomp {
		KeepChomp => with_terminal_newline

		StripChomp => trim_end_newlines(with_terminal_newline)

		ClipChomp => {
			if with_terminal_newline.is_empty() {
				[]
			} else {
				trim_end_newlines(with_terminal_newline).append('\n')
			}
		}
	}
}

join_block_lines : List(String.Utf8) -> String.Utf8
join_block_lines = |lines| {
	match lines {
		[] => []
		[first, .. as rest] => join_block_lines_help(rest, first)
	}
}

join_block_lines_help : List(String.Utf8), String.Utf8 -> String.Utf8
join_block_lines_help = |lines, out| {
	match lines {
		[] => out
		[line, .. as rest] => join_block_lines_help(rest, append_bytes(out.append('\n'), line))
	}
}

fold_block_lines : List(String.Utf8) -> String.Utf8
fold_block_lines = |lines| {
	match lines {
		[] => []
		[first, .. as rest] => fold_block_lines_help(rest, first, first)
	}
}

fold_block_lines_help : List(String.Utf8), String.Utf8, String.Utf8 -> String.Utf8
fold_block_lines_help = |lines, previous, out| {
	match lines {
		[] => out

		[current, .. as rest] => {
			separator =
				if previous.is_empty() or current.is_empty() or starts_with_space(previous) or starts_with_space(current) {
					'\n'
				} else {
					' '
				}

			next_out = append_bytes(out.append(separator), current)
			fold_block_lines_help(rest, current, next_out)
		}
	}
}

trim_end_newlines : String.Utf8 -> String.Utf8
trim_end_newlines = |bytes| trim_end_newlines_help(bytes, [], [])

trim_end_newlines_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_end_newlines_help = |bytes, out, pending| {
	match bytes {
		[] => out
		['\n', .. as rest] => trim_end_newlines_help(rest, out, pending.append('\n'))
		[first, .. as rest] => trim_end_newlines_help(rest, append_bytes(out, pending).append(first), [])
	}
}

drop_lines_before_number : List(Line), U64 -> List(Line)
drop_lines_before_number = |lines, target| {
	match lines {
		[] => []
		[line, .. as rest] if line.number < target => drop_lines_before_number(rest, target)
		_ => lines
	}
}

drop_consumed_lines : List(Line), U64 -> List(Line)
drop_consumed_lines = |lines, consumed_through| {
	match lines {
		[] => []
		[line, .. as rest] if line.number <= consumed_through => drop_consumed_lines(rest, consumed_through)
		_ => lines
	}
}

parse_inline_value : String.Utf8, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
parse_inline_value = |raw, line, column| {
	bytes = trim_spaces(raw)

	match bytes {
		[] => Ok(Null)

		['[', ..] => parse_flow_sequence(bytes, line, column)

		['{', ..] => parse_flow_mapping(bytes, line, column)

		['|', ..] | ['>', ..] =>
			fail(line, column, "block scalars are only supported as standalone mapping or sequence values")

		['&', ..] | ['*', ..] | ['!', ..] =>
			fail(line, column, "anchors, aliases, and tags are not supported by this YAML subset")

		['%', ..] =>
			fail(line, column, "YAML directives are not supported by this YAML subset")

		['?', ..] =>
			fail(line, column, "complex mapping keys are not supported by this YAML subset")

		['"', ..] =>
			parse_double_quoted(bytes, line, column).map_ok(|text| String(text))

		['\'', ..] =>
			parse_single_quoted(bytes, line, column).map_ok(|text| String(text))

		_ => parse_plain_scalar(bytes, line, column)
	}
}

parse_plain_scalar : String.Utf8, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
parse_plain_scalar = |bytes, line, column| {
	lower = lower_ascii(bytes)
	text = String.str_from_utf8(bytes)

	if lower == "null".to_utf8() or bytes == "~".to_utf8() {
		Ok(Null)
	} else if lower == "true".to_utf8() {
		Ok(Bool(Bool.True))
	} else if lower == "false".to_utf8() {
		Ok(Bool(Bool.False))
	} else if is_decimal_integer(bytes) {
		match I64.from_str(text) {
			Ok(value) => Ok(Int(value))
			Err(_) => fail(line, column, "integer `${text}` is outside the supported I64 range")
		}
	} else if looks_like_float(bytes) {
		match F64.from_str(text) {
			Ok(value) => Ok(Float(value))
			Err(_) => fail(line, column, "invalid floating-point value `${text}`")
		}
	} else {
		Ok(String(text))
	}
}

parse_flow_sequence : String.Utf8, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
parse_flow_sequence = |bytes, line, column| {
	inner = unwrap_flow(bytes, '[', ']', line, column)?

	if trim_spaces(inner).is_empty() {
		Ok(Sequence([]))
	} else {
		parts = split_flow_items(inner, line, column)?
		values = parse_flow_values(parts, line, column, [])?
		Ok(Sequence(values))
	}
}

parse_flow_values : List(String.Utf8), U64, U64, List(Yaml) -> Try(List(Yaml), [YamlError(Yaml.Error)])
parse_flow_values = |parts, line, column, values| {
	match parts {
		[] => Ok(values)
		[part, .. as rest] => {
			value = parse_inline_value(part, line, column)?
			parse_flow_values(rest, line, column, values.append(value))
		}
	}
}

parse_flow_mapping : String.Utf8, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
parse_flow_mapping = |bytes, line, column| {
	inner = unwrap_flow(bytes, '{', '}', line, column)?

	if trim_spaces(inner).is_empty() {
		Ok(Mapping([]))
	} else {
		parts = split_flow_items(inner, line, column)?
		entries = parse_flow_entries(parts, line, column, [])?
		Ok(Mapping(entries))
	}
}

parse_flow_entries : List(String.Utf8), U64, U64, List({ key : Str, value : Yaml }) -> Try(List({ key : Str, value : Yaml }), [YamlError(Yaml.Error)])
parse_flow_entries = |parts, line, column, entries| {
	match parts {
		[] => Ok(entries)

		[part, .. as rest] => {
			match split_mapping_entry(trim_spaces(part)) {
				Err(_) => fail(line, column, "expected a key and value in flow mapping")

				Ok(split) => {
					key = parse_key(split.key, line, column)?

					if mapping_has_key(entries, key) {
						fail(line, column, "duplicate mapping key `${key}`")
					} else {
						value = parse_inline_value(split.value, line, column)?
						parse_flow_entries(rest, line, column, entries.append({ key, value }))
					}
				}
			}
		}
	}
}

parse_key : String.Utf8, U64, U64 -> Try(Str, [YamlError(Yaml.Error)])
parse_key = |raw, line, column| {
	bytes = trim_spaces(raw)

	match bytes {
		[] => fail(line, column, "mapping keys must not be empty")
		['"', ..] => parse_double_quoted(bytes, line, column)
		['\'', ..] => parse_single_quoted(bytes, line, column)
		['[', ..] | ['{', ..] | ['?', ..] => fail(line, column, "complex mapping keys are not supported by this YAML subset")
		_ => Ok(String.str_from_utf8(bytes))
	}
}

parse_single_quoted : String.Utf8, U64, U64 -> Try(Str, [YamlError(Yaml.Error)])
parse_single_quoted = |bytes, line, column| {
	if bytes.len() < 2 or bytes.get(bytes.len() - 1) != Ok('\'') {
		fail(line, column, "unterminated single-quoted string")
	} else {
		inner = bytes.sublist({ start: 1, len: bytes.len() - 2 })
		unescape_single(inner, [], line, column).map_ok(String.str_from_utf8)
	}
}

unescape_single : String.Utf8, String.Utf8, U64, U64 -> Try(String.Utf8, [YamlError(Yaml.Error)])
unescape_single = |bytes, out, line, column| {
	match bytes {
		[] => Ok(out)
		['\'', '\'', .. as rest] => unescape_single(rest, out.append('\''), line, column)
		['\'', ..] => fail(line, column, "a single quote inside a quoted string must be doubled")
		[first, .. as rest] => unescape_single(rest, out.append(first), line, column)
	}
}

parse_double_quoted : String.Utf8, U64, U64 -> Try(Str, [YamlError(Yaml.Error)])
parse_double_quoted = |bytes, line, column| {
	if bytes.len() < 2 or bytes.get(bytes.len() - 1) != Ok('"') {
		fail(line, column, "unterminated double-quoted string")
	} else {
		inner = bytes.sublist({ start: 1, len: bytes.len() - 2 })
		unescape_double(inner, [], line, column).map_ok(String.str_from_utf8)
	}
}

unescape_double : String.Utf8, String.Utf8, U64, U64 -> Try(String.Utf8, [YamlError(Yaml.Error)])
unescape_double = |bytes, out, line, column| {
	match bytes {
		[] => Ok(out)
		['\\', '"', .. as rest] => unescape_double(rest, out.append('"'), line, column)
		['\\', '\\', .. as rest] => unescape_double(rest, out.append('\\'), line, column)
		['\\', 'n', .. as rest] => unescape_double(rest, out.append('\n'), line, column)
		['\\', 'r', .. as rest] => unescape_double(rest, out.append('\r'), line, column)
		['\\', 't', .. as rest] => unescape_double(rest, out.append('\t'), line, column)
		['\\', escaped, ..] => fail(line, column, "unsupported escape sequence `\\${String.str_from_utf8([escaped])}`")
		['\\'] => fail(line, column, "unterminated escape sequence")
		[first, .. as rest] => unescape_double(rest, out.append(first), line, column)
	}
}

prepare_lines : List(Line) -> Try(List(Line), [YamlError(Yaml.Error)])
prepare_lines = |raw_lines| {
	clean = clean_lines(raw_lines, [])?

	without_start =
		match clean {
			[first, .. as rest] if first.indent == 0 and first.content == "---".to_utf8() => rest
			_ => clean
		}

	remove_document_end(without_start, [])
}

clean_lines : List(Line), List(Line) -> Try(List(Line), [YamlError(Yaml.Error)])
clean_lines = |lines, out| {
	match lines {
		[] => Ok(out)

		[line, .. as rest] => {
			indent = count_indent(line.content, 0, line.number)?
			content = trim_end_spaces(strip_comment(line.content.drop_first(indent), NoQuote, Bool.False, Bool.True, []))

			if content.is_empty() {
				clean_lines(rest, out)
			} else {
				clean_lines(rest, out.append({ content, indent, number: line.number }))
			}
		}
	}
}

remove_document_end : List(Line), List(Line) -> Try(List(Line), [YamlError(Yaml.Error)])
remove_document_end = |lines, out| {
	match lines {
		[] => Ok(out)

		[line, .. as rest] if line.indent == 0 and line.content == "...".to_utf8() => {
			match rest {
				[] => Ok(out)
				[next, ..] => fail(next.number, next.indent + 1, "multiple YAML documents are not supported")
			}
		}

		[line, ..] if line.indent == 0 and line.content == "---".to_utf8() =>
			fail(line.number, 1, "multiple YAML documents are not supported")

		[line, .. as rest] => remove_document_end(rest, out.append(line))
	}
}

split_lines : String.Utf8, U64, String.Utf8, List(Line) -> List(Line)
split_lines = |input, number, current, lines| {
	match input {
		[] => lines.append({ content: current, indent: 0, number })
		['\r', '\n', .. as rest] => split_lines(rest, number + 1, [], lines.append({ content: current, indent: 0, number }))
		['\n', .. as rest] => split_lines(rest, number + 1, [], lines.append({ content: current, indent: 0, number }))
		[first, .. as rest] => split_lines(rest, number, current.append(first), lines)
	}
}

count_indent : String.Utf8, U64, U64 -> Try(U64, [YamlError(Yaml.Error)])
count_indent = |bytes, count, line| {
	match bytes {
		[' ', .. as rest] => count_indent(rest, count + 1, line)
		['\t', ..] => fail(line, count + 1, "tabs may not be used for YAML indentation")
		_ => Ok(count)
	}
}

strip_comment : String.Utf8, Quote, Bool, Bool, String.Utf8 -> String.Utf8
strip_comment = |bytes, quote, escaped, separated, out| {
	match bytes {
		[] => out

		[first, ..] if first == '#' and quote == NoQuote and separated => out

		['\\', .. as rest] if quote == DoubleQuote and !escaped =>
			strip_comment(rest, quote, Bool.True, Bool.False, out.append('\\'))

		['"', .. as rest] if quote == NoQuote =>
			strip_comment(rest, DoubleQuote, Bool.False, Bool.False, out.append('"'))

		['"', .. as rest] if quote == DoubleQuote and !escaped =>
			strip_comment(rest, NoQuote, Bool.False, Bool.False, out.append('"'))

		['\'', .. as rest] if quote == NoQuote =>
			strip_comment(rest, SingleQuote, Bool.False, Bool.False, out.append('\''))

		['\'', .. as rest] if quote == SingleQuote =>
			strip_comment(rest, NoQuote, Bool.False, Bool.False, out.append('\''))

		[first, .. as rest] =>
			strip_comment(rest, quote, Bool.False, first == ' ' or first == '\t', out.append(first))
		}
}

split_mapping_entry : String.Utf8 -> Try({ key : String.Utf8, value : String.Utf8, value_column : U64 }, [NotFound])
split_mapping_entry = |bytes| {
	find_mapping_colon(bytes, bytes, NoQuote, Bool.False, 0, 0, 0)
}

find_mapping_colon : String.Utf8, String.Utf8, Quote, Bool, U64, U64, U64 -> Try({ key : String.Utf8, value : String.Utf8, value_column : U64 }, [NotFound])
find_mapping_colon = |all, bytes, quote, escaped, square_depth, curly_depth, index| {
	match bytes {
		[] => Err(NotFound)

		[':', .. as rest] if quote == NoQuote and square_depth == 0 and curly_depth == 0 and (rest.is_empty() or starts_with_space(rest)) =>
			Ok({ key: all.sublist({ start: 0, len: index }), value: trim_start_spaces(rest), value_column: index + 2 })

		['\\', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, quote, Bool.True, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == NoQuote =>
			find_mapping_colon(all, rest, DoubleQuote, Bool.False, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, NoQuote, Bool.False, square_depth, curly_depth, index + 1)

		['\'', .. as rest] if quote == NoQuote =>
			find_mapping_colon(all, rest, SingleQuote, Bool.False, square_depth, curly_depth, index + 1)

		['\'', .. as rest] if quote == SingleQuote =>
			find_mapping_colon(all, rest, NoQuote, Bool.False, square_depth, curly_depth, index + 1)

		['[', .. as rest] if quote == NoQuote => find_mapping_colon(all, rest, quote, Bool.False, square_depth + 1, curly_depth, index + 1)
		[']', .. as rest] if quote == NoQuote and square_depth > 0 => find_mapping_colon(all, rest, quote, Bool.False, square_depth - 1, curly_depth, index + 1)
		['{', .. as rest] if quote == NoQuote => find_mapping_colon(all, rest, quote, Bool.False, square_depth, curly_depth + 1, index + 1)
		['}', .. as rest] if quote == NoQuote and curly_depth > 0 => find_mapping_colon(all, rest, quote, Bool.False, square_depth, curly_depth - 1, index + 1)

		[_, .. as rest] => find_mapping_colon(all, rest, quote, Bool.False, square_depth, curly_depth, index + 1)
	}
}

split_flow_items : String.Utf8, U64, U64 -> Try(List(String.Utf8), [YamlError(Yaml.Error)])
split_flow_items = |bytes, line, column| {
	split_flow_items_help(bytes, [], [], NoQuote, Bool.False, 0, 0, line, column)
}

split_flow_items_help : String.Utf8, String.Utf8, List(String.Utf8), Quote, Bool, U64, U64, U64, U64 -> Try(List(String.Utf8), [YamlError(Yaml.Error)])
split_flow_items_help = |bytes, current, items, quote, escaped, square_depth, curly_depth, line, column| {
	match bytes {
		[] if quote != NoQuote => fail(line, column, "unterminated quoted string in flow collection")
		[] if square_depth != 0 or curly_depth != 0 => fail(line, column, "unterminated nested flow collection")
		[] if trim_spaces(current).is_empty() => fail(line, column, "flow collections may not contain an empty item")
		[] => Ok(items.append(trim_spaces(current)))

		[',', .. as rest] if quote == NoQuote and square_depth == 0 and curly_depth == 0 => {
			if trim_spaces(current).is_empty() {
				fail(line, column, "flow collections may not contain an empty item")
			} else {
				split_flow_items_help(rest, [], items.append(trim_spaces(current)), quote, Bool.False, square_depth, curly_depth, line, column)
			}
		}

		['\\', .. as rest] if quote == DoubleQuote and !escaped => split_flow_items_help(rest, current.append('\\'), items, quote, Bool.True, square_depth, curly_depth, line, column)
		['"', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('"'), items, DoubleQuote, Bool.False, square_depth, curly_depth, line, column)
		['"', .. as rest] if quote == DoubleQuote and !escaped => split_flow_items_help(rest, current.append('"'), items, NoQuote, Bool.False, square_depth, curly_depth, line, column)
		['\'', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('\''), items, SingleQuote, Bool.False, square_depth, curly_depth, line, column)
		['\'', .. as rest] if quote == SingleQuote => split_flow_items_help(rest, current.append('\''), items, NoQuote, Bool.False, square_depth, curly_depth, line, column)
		['[', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('['), items, quote, Bool.False, square_depth + 1, curly_depth, line, column)
		[']', ..] if quote == NoQuote and square_depth == 0 => fail(line, column, "unexpected closing bracket in flow collection")
		[']', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append(']'), items, quote, Bool.False, square_depth - 1, curly_depth, line, column)
		['{', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('{'), items, quote, Bool.False, square_depth, curly_depth + 1, line, column)
		['}', ..] if quote == NoQuote and curly_depth == 0 => fail(line, column, "unexpected closing brace in flow collection")
		['}', .. as rest] if quote == NoQuote => split_flow_items_help(rest, current.append('}'), items, quote, Bool.False, square_depth, curly_depth - 1, line, column)
		[first, .. as rest] => split_flow_items_help(rest, current.append(first), items, quote, Bool.False, square_depth, curly_depth, line, column)
	}
}

unwrap_flow : String.Utf8, U8, U8, U64, U64 -> Try(String.Utf8, [YamlError(Yaml.Error)])
unwrap_flow = |bytes, open, close, line, column| {
	if bytes.len() < 2 or bytes.get(0) != Ok(open) or bytes.get(bytes.len() - 1) != Ok(close) {
		fail(line, column, "unterminated flow collection")
	} else {
		Ok(bytes.sublist({ start: 1, len: bytes.len() - 2 }))
	}
}

is_sequence_line : String.Utf8 -> Bool
is_sequence_line = |bytes| {
	match bytes {
		['-'] => Bool.True
		['-', ' ', ..] => Bool.True
		_ => Bool.False
	}
}

sequence_payload : String.Utf8 -> String.Utf8
sequence_payload = |bytes| {
	match bytes {
		['-'] => []
		['-', ' ', .. as rest] => trim_spaces(rest)
		_ => bytes
	}
}

mapping_has_key : List({ key : Str, value : Yaml }), Str -> Bool
mapping_has_key = |entries, key| {
	match entries {
		[] => Bool.False
		[first, ..] if first.key == key => Bool.True
		[_, .. as rest] => mapping_has_key(rest, key)
	}
}

is_decimal_integer : String.Utf8 -> Bool
is_decimal_integer = |bytes| {
	match bytes {
		['+', .. as rest] | ['-', .. as rest] => !rest.is_empty() and all_digits(rest)
		_ => !bytes.is_empty() and all_digits(bytes)
	}
}

all_digits : String.Utf8 -> Bool
all_digits = |bytes| {
	match bytes {
		[] => Bool.True
		[first, .. as rest] => first >= '0' and first <= '9' and all_digits(rest)
	}
}

looks_like_float : String.Utf8 -> Bool
looks_like_float = |bytes| {
	has_float_marker = contains_byte(bytes, '.') or contains_byte(bytes, 'e') or contains_byte(bytes, 'E')
	has_digit = bytes_contains_digit(bytes)
	valid_characters = all_float_characters(bytes)

	match bytes {
		['+', first, ..] | ['-', first, ..] => has_float_marker and has_digit and valid_characters and (is_digit(first) or first == '.')
		[first, ..] => has_float_marker and has_digit and valid_characters and (is_digit(first) or first == '.')
		[] => Bool.False
	}
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

bytes_contains_digit : String.Utf8 -> Bool
bytes_contains_digit = |bytes| {
	match bytes {
		[] => Bool.False
		[first, ..] if is_digit(first) => Bool.True
		[_, .. as rest] => bytes_contains_digit(rest)
	}
}

all_float_characters : String.Utf8 -> Bool
all_float_characters = |bytes| {
	match bytes {
		[] => Bool.True
		[first, .. as rest] =>
			(is_digit(first) or first == '.' or first == 'e' or first == 'E' or first == '+' or first == '-') and all_float_characters(rest)
		}
}

contains_byte : String.Utf8, U8 -> Bool
contains_byte = |bytes, expected| {
	match bytes {
		[] => Bool.False
		[first, ..] if first == expected => Bool.True
		[_, .. as rest] => contains_byte(rest, expected)
	}
}

starts_with_space : String.Utf8 -> Bool
starts_with_space = |bytes| {
	match bytes {
		[' ', ..] | ['\t', ..] => Bool.True
		_ => Bool.False
	}
}

trim_spaces : String.Utf8 -> String.Utf8
trim_spaces = |bytes| trim_end_spaces(trim_start_spaces(bytes))

trim_start_spaces : String.Utf8 -> String.Utf8
trim_start_spaces = |bytes| {
	match bytes {
		[' ', .. as rest] | ['\t', .. as rest] => trim_start_spaces(rest)
		_ => bytes
	}
}

trim_end_spaces : String.Utf8 -> String.Utf8
trim_end_spaces = |bytes| trim_end_spaces_help(bytes, [], [])

trim_end_spaces_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_end_spaces_help = |bytes, out, pending| {
	match bytes {
		[] => out
		[' ', .. as rest] => trim_end_spaces_help(rest, out, pending.append(' '))
		['\t', .. as rest] => trim_end_spaces_help(rest, out, pending.append('\t'))
		[first, .. as rest] => trim_end_spaces_help(rest, append_bytes(out, pending).append(first), [])
	}
}

append_bytes : String.Utf8, String.Utf8 -> String.Utf8
append_bytes = |left, right| {
	match right {
		[] => left
		[first, .. as rest] => append_bytes(left.append(first), rest)
	}
}

lower_ascii : String.Utf8 -> String.Utf8
lower_ascii = |bytes| {
	match bytes {
		[] => []
		[first, .. as rest] if first >= 'A' and first <= 'Z' => List.prepend(lower_ascii(rest), first + 32)
		[first, .. as rest] => List.prepend(lower_ascii(rest), first)
	}
}

fail : U64, U64, Str -> Try(_, [YamlError(Yaml.Error)])
fail = |line, column, message| Err(YamlError({ line, column, message }))

inspect_yaml : Yaml -> Str
inspect_yaml = |value| {
	match value {
		Null => "Null"
		Bool(boolean) => "Bool(${Str.inspect(boolean)})"
		Int(integer) => "Int(${integer.to_str()})"
		Float(float) => "Float(${float.to_str()})"
		String(text) => "String(${Str.inspect(text)})"
		Sequence(values) => "Sequence([${values.map(inspect_yaml) |> Str.join_with(", ")}])"
		Mapping(entries) => "Mapping([${entries.map(inspect_entry) |> Str.join_with(", ")}])"
	}
}

inspect_entry : { key : Str, value : Yaml } -> Str
inspect_entry = |entry| "{ key: ${Str.inspect(entry.key)}, value: ${inspect_yaml(entry.value)} }"

## Empty documents parse as null.
expect Yaml.parse_str("") == Ok(Null)

## Common frontmatter scalars resolve to useful values.
expect {
	actual =
		Yaml.parse_str(
			\\title: A small article
			\\draft: false
			\\count: 3
			\\rating: 4.5
			\\description: null
			,
		)?

	actual
		== Mapping([
			{ key: "title", value: String("A small article") },
			{ key: "draft", value: Bool(Bool.False) },
			{ key: "count", value: Int(3) },
			{ key: "rating", value: Float(4.5) },
			{ key: "description", value: Null },
		])
}

## Nested mappings and sequences parse by indentation.
expect {
	actual =
		Yaml.parse_str(
			\\site:
			\\  title: Roc
			\\  tags:
			\\    - parser
			\\    - yaml
			,
		)?

	actual
		== Mapping([
			{
				key: "site",
				value: Mapping([
					{ key: "title", value: String("Roc") },
					{ key: "tags", value: Sequence([String("parser"), String("yaml")]) },
				]),
			},
		])
}

## Sequence items may be compact mappings.
expect {
	actual =
		Yaml.parse_str(
			\\people:
			\\  - name: Ada
			\\    active: true
			\\  - name: Grace
			,
		)?

	actual
		== Mapping([
			{
				key: "people",
				value: Sequence([
					Mapping([{ key: "name", value: String("Ada") }, { key: "active", value: Bool(Bool.True) }]),
					Mapping([{ key: "name", value: String("Grace") }]),
				]),
			},
		])
}

## Flow collections support concise config values.
expect {
	actual = Yaml.parse_str("ports: [80, 443]\nlabels: { tier: web, public: true }")?

	actual
		== Mapping([
			{ key: "ports", value: Sequence([Int(80), Int(443)]) },
			{ key: "labels", value: Mapping([{ key: "tier", value: String("web") }, { key: "public", value: Bool(Bool.True) }]) },
		])
}

## Quotes preserve scalar strings and comment markers.
expect {
	actual = Yaml.parse_str("enabled: \"true\"\nmessage: 'it''s # text' # comment")?
	actual == Mapping([{ key: "enabled", value: String("true") }, { key: "message", value: String("it's # text") }])
}

## Optional document markers work for Markdown frontmatter bodies.
expect {
	actual = Yaml.parse_str("---\ntitle: Post\n...")?
	actual == Mapping([{ key: "title", value: String("Post") }])
}

## Duplicate mapping keys fail.
expect Yaml.parse_str("name: first\nname: second").is_err()

## Tabs used for indentation fail.
expect Yaml.parse_str("root:\n\tchild: value").is_err()

## Advanced YAML features fail explicitly.
expect Yaml.parse_str("value: &anchor text").is_err()

## Multiple documents are outside the supported subset.
expect Yaml.parse_str("one: 1\n---\ntwo: 2").is_err()

## Plain strings containing dots or the letter e are not mistaken for floats.
expect {
	actual = Yaml.parse_str("file: .git\nname: release")?
	actual == Mapping([{ key: "file", value: String(".git") }, { key: "name", value: String("release") }])
}

## Syntax errors report their source location.
expect {
	match Yaml.parse_str("root:\n\tchild: value") {
		Err(YamlError(problem)) => problem.line == 2 and problem.column == 1
		Ok(_) => Bool.False
	}
}

## Root sequences and empty entries are supported.
expect {
	actual = Yaml.parse_str("- first\n-\n- third")?
	actual == Sequence([String("first"), Null, String("third")])
}

## CRLF input and comment-only lines are ignored correctly.
expect {
	actual = Yaml.parse_str("# config\r\nname: roc-parser\r\n")?
	actual == Mapping([{ key: "name", value: String("roc-parser") }])
}

## Nested flow collections keep quoted commas inside strings.
expect {
	actual = Yaml.parse_str("value: [{ name: 'one,two' }, [1, 2]]")?
	actual == Mapping([{ key: "value", value: Sequence([Mapping([{ key: "name", value: String("one,two") }]), Sequence([Int(1), Int(2)])]) }])
}

## Double-quoted strings support common escapes.
expect {
	actual = Yaml.parse_str("message: \"first\\nsecond\"")?
	actual == Mapping([{ key: "message", value: String("first\nsecond") }])
}

## Block scalars parse as multiline strings, including folded style.
expect {
	actual =
		Yaml.parse_str(
			\\description: |
			\\  first line
			\\  second line
			\\summary: >
			\\  one
			\\  two
			,
		)?

	actual
		== Mapping([
			{ key: "description", value: String("first line\nsecond line\n") },
			{ key: "summary", value: String("one two\n") },
		])
}

## Chomping indicators are supported for block scalars.
expect {
	actual = Yaml.parse_str("note: |-\n  hello")?
	actual == Mapping([{ key: "note", value: String("hello") }])
}

## Block scalar content keeps blank lines and # characters literally.
expect {
	actual = Yaml.parse_str("text: |\n  # not a comment\n\n  after")?
	actual == Mapping([{ key: "text", value: String("# not a comment\n\nafter\n") }])
}

## Explicit block indentation indicators are supported.
expect {
	actual = Yaml.parse_str("script: |2-\n  echo one\n  echo two\nnext: done")?

	actual
		== Mapping([
			{ key: "script", value: String("echo one\necho two") },
			{ key: "next", value: String("done") },
		])
}

## Folded blocks preserve line breaks around indented continuation lines.
expect {
	actual = Yaml.parse_str("text: >\n  intro\n    code\n  outro")?
	actual == Mapping([{ key: "text", value: String("intro\n  code\noutro\n") }])
}

## Malformed flow collections still fail.
expect Yaml.parse_str("values: [one, two").is_err()
