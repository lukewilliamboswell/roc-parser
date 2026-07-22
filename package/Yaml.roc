import String

# # # YAML configuration parser
# #
# # This module implements a deliberately small YAML 1.2-style subset aimed at
# # configuration files and Markdown frontmatter. It supports a single block
# # document, nested mappings and sequences, flow collections, comments, quoted
# # strings, and common null, boolean, integer, and floating-point scalars.
# #
# # Anchors, aliases, tags, directives, block scalars, complex keys, and
# # multi-document streams are rejected with a parse error.
Yaml := [
	Null,
	Bool(Bool),
	Int(I64),
	Float(F64),
	String(Str),
	Sequence(List(Yaml)),
	Mapping(List({ key : Str, value : Yaml })),
].{
	Error : { line : U64, column : U64, message : Str }
	is_eq : _

	# # Parse one YAML configuration document from a string.
	parse_str : Str -> Try(Yaml, [YamlError(Error)])
	parse_str = |input| {
		lines = prepare_lines(split_lines(input.to_utf8(), 1, [], []))?

		match lines {
			[] => Ok(Null)

			[first, ..] if first.indent != 0 =>
				fail(first.number, 1, "the document root must not be indented")

			[first, ..] => {
				parsed = parse_node(lines, first.indent, 0)?

				match parsed.input {
					[] => Ok(parsed.val)

					[leftover, ..] =>
						fail(leftover.number, leftover.indent + 1, "unexpected content after the document root")
					}
			}
		}
	}

	to_inspect : Yaml -> Str
	to_inspect = |value| inspect_yaml(value)
}

Line : { content : String.Utf8, indent : U64, number : U64 }

ParseResult : { val : Yaml, input : List(Line) }

Quote : [NoQuote, SingleQuote, DoubleQuote]

parse_node : List(Line), U64, U64 -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_node = |lines, indent, depth| {
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
				parse_sequence(lines, indent, depth)

			[first, ..] => {
				match split_mapping_entry(first.content) {
					Ok(_) => parse_mapping(lines, indent, depth)
					Err(_) => {
						value = parse_inline_value(first.content, first.number, first.indent + 1)?
						Ok({ val: value, input: lines.drop_first(1) })
					}
				}
			}
		}
	}
}

parse_mapping : List(Line), U64, U64 -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_mapping = |lines, indent, depth| {
	parse_mapping_help(lines, indent, depth, [])
}

parse_mapping_help : List(Line), U64, U64, List({ key : Str, value : Yaml }) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_mapping_help = |lines, indent, depth, entries| {
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
								child = parse_node(rest, next.indent, depth + 1)?
								parse_mapping_help(child.input, indent, depth, entries.append({ key, value: child.val }))
							}

							_ =>
								parse_mapping_help(rest, indent, depth, entries.append({ key, value: Null }))
							}
					} else {
						value = parse_inline_value(parts.value, line.number, line.indent + parts.value_column)?
						parse_mapping_help(rest, indent, depth, entries.append({ key, value }))
					}
				}
			}
		}
	}
}

parse_sequence : List(Line), U64, U64 -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_sequence = |lines, indent, depth| {
	parse_sequence_help(lines, indent, depth, [])
}

parse_sequence_help : List(Line), U64, U64, List(Yaml) -> Try(ParseResult, [YamlError(Yaml.Error)])
parse_sequence_help = |lines, indent, depth, values| {
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
						child = parse_node(rest, next.indent, depth + 1)?
						parse_sequence_help(child.input, indent, depth, values.append(child.val))
					}

					_ =>
						parse_sequence_help(rest, indent, depth, values.append(Null))
					}
			} else {
				match split_mapping_entry(payload) {
					Ok(_) => {
						virtual = { content: payload, indent: indent + 2, number: line.number }
						child = parse_node(List.prepend(rest, virtual), indent + 2, depth + 1)?
						parse_sequence_help(child.input, indent, depth, values.append(child.val))
					}

					Err(_) => {
						value = parse_inline_value(payload, line.number, line.indent + 3)?
						parse_sequence_help(rest, indent, depth, values.append(value))
					}
				}
			}
		}
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
			fail(line, column, "block scalars are not supported by this YAML subset")

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
		Sequence(values) => "Sequence([${values.map(inspect_yaml)->Str.join_with(", ")}])"
		Mapping(entries) => "Mapping([${entries.map(inspect_entry)->Str.join_with(", ")}])"
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

## Block scalars and malformed flow collections fail rather than degrading to strings.
expect Yaml.parse_str("description: |\n  multiline").is_err()
expect Yaml.parse_str("values: [one, two").is_err()
