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

			[first, ..] if first.tab =>
				fail(first.number, first.indent + 1, "tabs may not be used for YAML indentation")

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

## `terminated` is false only for a final line without a line break. `tab`
## marks a tab right after the indentation, which is an error only where the
## line is block structure rather than block scalar content.
Line : { content : String.Utf8, indent : U64, number : U64, terminated : Bool, tab : Bool }

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

			[first, ..] if first.tab => fail(first.number, first.indent + 1, "tabs may not be used for YAML indentation")

			[first, ..] if first.indent != indent =>
				fail(first.number, first.indent + 1, "unexpected indentation")

			[first, ..] if is_sequence_line(first.content) =>
				parse_sequence(lines, indent, depth, raw_lines)

			[first, ..] => {
				match split_mapping_entry(first.content) {
					Ok(_) => parse_mapping(lines, indent, depth, raw_lines)
					Err(_) => {
						if starts_block_scalar(first.content) {
							# At the document root (indentation -1) content may start in column 0.
							min_indent = if depth == 0 and first.indent == 0 0 else first.indent + 1
							block = parse_block_scalar(lines.drop_first(1), raw_lines, first.content, first.number, first.indent + 1, min_indent)?
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

		[line, ..] if line.tab => fail(line.number, line.indent + 1, "tabs may not be used for YAML indentation")

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

							# A sequence may sit at its key's indentation (YAML 1.2 8.2.1).
							[next, ..] if next.indent == indent and is_sequence_line(next.content) => {
								child = parse_sequence(rest, indent, depth + 1, raw_lines)?
								parse_mapping_help(child.input, indent, depth, raw_lines, entries.append({ key, value: child.val }))
							}

							_ =>
								parse_mapping_help(rest, indent, depth, raw_lines, entries.append({ key, value: Null }))
							}
					} else if starts_block_scalar(parts.value) {
						block = parse_block_scalar(rest, raw_lines, parts.value, line.number, line.indent + parts.value_column, line.indent + 1)?
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

		[line, ..] if line.tab => fail(line.number, line.indent + 1, "tabs may not be used for YAML indentation")

		[line, ..] if line.indent < indent =>
			Ok({ val: Sequence(values), input: lines })

		[line, ..] if line.indent > indent =>
			fail(line.number, line.indent + 1, "unexpected indentation after a sequence value")

		[line, ..] if !is_sequence_line(line.content) =>
			Ok({ val: Sequence(values), input: lines })

		[line, .. as rest] => {
			payload = sequence_payload(line.content)
			# The entry's content starts after "-" and its separating spaces.
			payload_indent = line.indent + 1 + count_spaces(line.content.drop_first(1), 0)

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
				match (if is_sequence_line(payload) Ok({}) else split_mapping_entry(payload).map_ok(|_| {})) {
					Ok(_) => {
						virtual = { content: payload, indent: payload_indent, number: line.number, terminated: line.terminated, tab: Bool.False }
						child = parse_node(List.prepend(rest, virtual), payload_indent, depth + 1, raw_lines)?
						parse_sequence_help(child.input, indent, depth, raw_lines, values.append(child.val))
					}

					Err(_) => {
						if starts_block_scalar(payload) {
							block = parse_block_scalar(rest, raw_lines, payload, line.number, payload_indent + 1, line.indent + 1)?
							parse_sequence_help(block.input, indent, depth, raw_lines, values.append(block.value))
						} else {
							value = parse_inline_value(payload, line.number, payload_indent + 1)?
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
parse_block_scalar = |clean_rest, raw_lines, header_bytes, line, column, min_indent| {
	header = parse_block_header(header_bytes, line, column)?
	raw_tail = drop_lines_before_number(raw_lines, line + 1)
	collected = collect_block_lines(raw_tail, min_indent, header.indent, line)?
	body = render_block_scalar(collected.lines, collected.terminated, header.style, header.chomp)
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

## Collect a block scalar's lines without their content indentation (YAML 1.2
## 8.1.1), following the yaml-test-suite reference behaviour. Lines of spaces
## are empty (`[]`), keeping any spaces beyond the content indentation as
## text. Content must be indented at least `min_indent` spaces: one more than
## the parent node, or zero for a scalar at the document root. `terminated`
## says whether the last content line ended with a line break.
collect_block_lines : List(Line), U64, BlockIndent, U64 -> Try({ lines : List(String.Utf8), consumed_through : U64, terminated : Bool }, [YamlError(Yaml.Error)])
collect_block_lines = |raw_lines, min_indent, block_indent, header_line| {
	var $content_indent =
		match block_indent {
			ExplicitIndent(offset) => FixedIndent(min_indent + offset - 1)
			AutoIndent => PendingIndent
		}
	var $lines = []
	var $consumed_through = header_line
	var $terminated = Bool.True
	var $leading_spaces = 0
	var $leading_line = header_line
	var $done = Bool.False
	var $remaining = raw_lines

	while !$done {
		match $remaining {
			[] => {
				$done = Bool.True
			}

			[line, .. as rest] => {
				spaces = count_spaces(line.content, 0)
				after_spaces = line.content.drop_first(spaces)
				white_only = trim_spaces(after_spaces).is_empty()

				if after_spaces.is_empty() {
					# An empty line, even a final one without a line break.
					text =
						match $content_indent {
							FixedIndent(width) if spaces > width => line.content.drop_first(width)
							_ => []
						}

					# Before the indentation is detected, remember the deepest empty line.
					if $content_indent == PendingIndent and spaces > $leading_spaces {
						$leading_spaces = spaces
						$leading_line = line.number
					}

					$lines = $lines.append(text)
					$consumed_through = line.number
					$terminated = Bool.True
					$remaining = rest
				} else if spaces < min_indent or is_document_marker(line.content) {
					if white_only {
						# Only a tab can follow; it would be indentation.
						return fail(line.number, spaces + 1, "tabs may not be used for YAML indentation")
					}

					$done = Bool.True
				} else {
					required =
						match $content_indent {
							FixedIndent(width) => width
							PendingIndent => spaces
						}

					if spaces < required and white_only {
						$lines = $lines.append([])
						$consumed_through = line.number
						$terminated = Bool.True
						$remaining = rest
					} else if spaces < required {
						$done = Bool.True
					} else {
						$content_indent = FixedIndent(required)
						$lines = $lines.append(line.content.drop_first(required))
						$consumed_through = line.number
						# Trailing white space at the end of input still ends its line.
						$terminated = line.terminated or white_only
						$remaining = rest
					}
				}
			}
		}
	}

	match $content_indent {
		FixedIndent(width) if $leading_spaces > width and block_indent == AutoIndent =>
			fail($leading_line, width + 1, "leading empty lines of a block scalar must not be indented more than its content")

		_ => Ok({ lines: $lines, consumed_through: $consumed_through, terminated: $terminated })
	}
}

## "---" or "..." at the start of a line, alone or followed by white space.
is_document_marker : String.Utf8 -> Bool
is_document_marker = |bytes| {
	match bytes {
		['-', '-', '-'] | ['.', '.', '.'] => Bool.True
		['-', '-', '-', ' ', ..] | ['-', '-', '-', '\t', ..] | ['.', '.', '.', ' ', ..] | ['.', '.', '.', '\t', ..] => Bool.True
		_ => Bool.False
	}
}

count_spaces : String.Utf8, U64 -> U64
count_spaces = |bytes, count| {
	match bytes {
		[' ', .. as rest] => count_spaces(rest, count + 1)
		_ => count
	}
}

## Apply the block style and chomping indicator (YAML 1.2 8.1.1.2). Content
## runs through the last non-empty line; later empty lines are trailing.
render_block_scalar : List(String.Utf8), Bool, BlockStyle, BlockChomp -> String.Utf8
render_block_scalar = |lines, terminated, style, chomp| {
	content_len = last_content_index(lines, 0, 0)
	content = lines.sublist({ start: 0, len: content_len })
	trailing = lines.len() - content_len

	if content_len == 0 {
		match chomp {
			KeepChomp => List.repeat('\n', trailing)
			_ => []
		}
	} else {
		body =
			match style {
				LiteralBlock => join_block_lines(content)
				FoldedBlock => fold_block_lines(content)
			}

		# The last content line has a break unless it ended the input.
		final_break = trailing > 0 or terminated

		match chomp {
			StripChomp => body
			ClipChomp => if final_break body.append('\n') else body
			KeepChomp => append_bytes(if final_break body.append('\n') else body, List.repeat('\n', trailing))
		}
	}
}

last_content_index : List(String.Utf8), U64, U64 -> U64
last_content_index = |lines, index, last| {
	match lines.get(index) {
		Err(_) => last
		Ok(line) if line.is_empty() => last_content_index(lines, index + 1, last)
		Ok(_) => last_content_index(lines, index + 1, index + 1)
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

## Fold lines (YAML 1.2 8.1.3, 6.5): a break between two text lines that do not
## start with white space becomes a space, or is dropped when empty lines
## follow it; breaks next to more-indented lines are kept.
fold_block_lines : List(String.Utf8) -> String.Utf8
fold_block_lines = |lines| {
	var $out = []
	var $previous = Err(NoLine)
	var $empties = 0

	var $index = 0

	while $index < lines.len() {
		line = lines.get($index) ?? []
		$index = $index + 1

		if line.is_empty() {
			$empties = $empties + 1
		} else {
			separator =
				match $previous {
					Err(_) => List.repeat('\n', $empties)
					Ok(before) if !starts_with_space(before) and !starts_with_space(line) =>
						if $empties == 0 [' '] else List.repeat('\n', $empties)

					Ok(_) => List.repeat('\n', $empties + 1)
				}

			$out = append_bytes(append_bytes($out, separator), line)
			$previous = Ok(line)
			$empties = 0
		}
	}

	$out
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

## Resolve a plain scalar with the YAML 1.2 core schema (10.3.2). Anything
## that matches none of its forms is a string.
parse_plain_scalar : String.Utf8, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
parse_plain_scalar = |bytes, line, column| {
	text = String.str_from_utf8(bytes)

	if ["null", "Null", "NULL", "~"].contains(text) {
		Ok(Null)
	} else if ["true", "True", "TRUE"].contains(text) {
		Ok(Bool(Bool.True))
	} else if ["false", "False", "FALSE"].contains(text) {
		Ok(Bool(Bool.False))
	} else if is_decimal_integer(bytes) {
		match I64.from_str(text) {
			Ok(value) => Ok(Int(value))
			Err(_) => fail(line, column, "integer `${text}` is outside the supported I64 range")
		}
	} else {
		match bytes {
			['0', 'o', .. as digits] if !digits.is_empty() and digits.all(|b| b >= '0' and b <= '7') =>
				radix_integer(digits, 8, text, line, column)

			['0', 'x', .. as digits] if !digits.is_empty() and digits.all(is_hex_digit) =>
				radix_integer(digits, 16, text, line, column)

			_ =>
				match special_float(text) {
					Ok(value) => Ok(Float(value))
					Err(_) =>
						match core_float_text(bytes) {
							Ok(canonical) => Ok(Float(F64.from_str(canonical) ?? (if bytes.first() == Ok('-') -F64.infinity else F64.infinity)))
							Err(_) => Ok(String(text))
						}
				}
		}
	}
}

special_float : Str -> Try(F64, [NotSpecial])
special_float = |text| {
	if [".inf", ".Inf", ".INF", "+.inf", "+.Inf", "+.INF"].contains(text) {
		Ok(F64.infinity)
	} else if ["-.inf", "-.Inf", "-.INF"].contains(text) {
		Ok(-F64.infinity)
	} else if [".nan", ".NaN", ".NAN"].contains(text) {
		Ok(F64.nan)
	} else {
		Err(NotSpecial)
	}
}

is_hex_digit : U8 -> Bool
is_hex_digit = |byte| is_digit(byte) or (byte >= 'a' and byte <= 'f') or (byte >= 'A' and byte <= 'F')

radix_integer : String.Utf8, U64, Str, U64, U64 -> Try(Yaml, [YamlError(Yaml.Error)])
radix_integer = |digits, radix, text, line, column| {
	value = digits.fold(Ok(0), |acc, byte| {
		digit =
			if is_digit(byte) {
				U8.to_u64(byte - '0')
			} else if byte >= 'a' {
				U8.to_u64(byte - 'a' + 10)
			} else {
				U8.to_u64(byte - 'A' + 10)
			}

		match acc {
			Ok(total) if total <= (9223372036854775807 - digit) // radix => Ok(total * radix + digit)
			_ => Err(Overflow)
		}
	})

	match value {
		Ok(total) => Ok(Int(U64.to_i64_wrap(total)))
		Err(_) => fail(line, column, "integer `${text}` is outside the supported I64 range")
	}
}

## Match the core schema float form [-+]? ( \. [0-9]+ | [0-9]+ ( \. [0-9]* )? ) ( [eE] [-+]? [0-9]+ )?
## and spell it as sign, digits, ".", digits, exponent for F64.from_str.
core_float_text : String.Utf8 -> Try(Str, [NotFloat])
core_float_text = |bytes| {
	{ sign, unsigned } =
		match bytes {
			['-', .. as rest] => { sign: "-", unsigned: rest }
			['+', .. as rest] => { sign: "", unsigned: rest }
			_ => { sign: "", unsigned: bytes }
		}
	mantissa = unsigned.take_first(count_while(unsigned, |b| b != 'e' and b != 'E'))
	exponent = unsigned.drop_first(mantissa.len())
	whole = mantissa.take_first(count_while(mantissa, is_digit))
	after_whole = mantissa.drop_first(whole.len())
	{ has_point, fraction } =
		match after_whole {
			['.', .. as rest] => { has_point: Bool.True, fraction: rest }
			_ => { has_point: Bool.False, fraction: after_whole }
		}
	exponent_digits =
		match exponent {
			['e', '+', .. as rest] | ['e', '-', .. as rest] | ['E', '+', .. as rest] | ['E', '-', .. as rest] | ['e', .. as rest] | ['E', .. as rest] => rest
			_ => []
		}
	mantissa_ok = fraction.all(is_digit) and (!whole.is_empty() or (has_point and !fraction.is_empty()))
	exponent_ok = exponent.is_empty() or (!exponent_digits.is_empty() and exponent_digits.all(is_digit))
	# A plain integer is not a float; "1." and "1e3" are.
	is_float = has_point or !exponent.is_empty()

	if mantissa_ok and exponent_ok and is_float {
		whole_text = if whole.is_empty() "0" else String.str_from_utf8(whole)
		fraction_text = if fraction.is_empty() "0" else String.str_from_utf8(fraction)
		exponent_text = if exponent.is_empty() "" else String.str_from_utf8(exponent)
		Ok("${sign}${whole_text}.${fraction_text}${exponent_text}")
	} else {
		Err(NotFloat)
	}
}

count_while : String.Utf8, (U8 -> Bool) -> U64
count_while = |bytes, keep| {
	var $count = 0
	while $count < bytes.len() and keep(bytes.get($count) ?? 0) {
		$count = $count + 1
	}
	$count
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
		['\\', escaped, .. as rest] => {
			match simple_escape(escaped) {
				Ok(decoded) => unescape_double(rest, append_bytes(out, decoded), line, column)
				Err(_) => {
					digits =
						match escaped {
							'x' => 2
							'u' => 4
							'U' => 8
							_ => 0
						}

					if digits == 0 {
						fail(line, column, "unsupported escape sequence `\\${escaped_character(bytes.drop_first(1))}`")
					} else {
						encoded = encode_code_point(rest.sublist({ start: 0, len: digits }), digits, line, column)?
						unescape_double(rest.drop_first(digits), append_bytes(out, encoded), line, column)
					}
				}
			}
		}

		['\\'] => fail(line, column, "unterminated escape sequence")
		[first, .. as rest] => unescape_double(rest, out.append(first), line, column)
	}
}

## YAML 1.2 single-character escapes (5.7), as UTF-8.
simple_escape : U8 -> Try(String.Utf8, [NotSimple])
simple_escape = |escaped| {
	match escaped {
		'0' => Ok([0])
		'a' => Ok([7])
		'b' => Ok([8])
		't' | '\t' => Ok(['\t'])
		'n' => Ok(['\n'])
		'v' => Ok([11])
		'f' => Ok([12])
		'r' => Ok(['\r'])
		'e' => Ok([27])
		' ' => Ok([' '])
		'"' => Ok(['"'])
		'/' => Ok(['/'])
		'\\' => Ok(['\\'])
		'N' => Ok([0xC2, 0x85])
		'_' => Ok([0xC2, 0xA0])
		'L' => Ok([0xE2, 0x80, 0xA8])
		'P' => Ok([0xE2, 0x80, 0xA9])
		_ => Err(NotSimple)
	}
}

## The whole (possibly multi-byte) character at the start of `bytes`.
escaped_character : String.Utf8 -> Str
escaped_character = |bytes| {
	width =
		match bytes {
			[first, ..] if first >= 0xF0 => 4
			[first, ..] if first >= 0xE0 => 3
			[first, ..] if first >= 0xC0 => 2
			_ => 1
		}

	Str.from_utf8(bytes.sublist({ start: 0, len: width })) ?? "?"
}

encode_code_point : String.Utf8, U64, U64, U64 -> Try(String.Utf8, [YamlError(Yaml.Error)])
encode_code_point = |hex, digits, line, column| {
	if hex.len() != digits {
		fail(line, column, "escape sequence needs ${digits.to_str()} hexadecimal digits")
	} else {
		match parse_hex(hex, 0) {
			Err(_) => fail(line, column, "escape sequence needs ${digits.to_str()} hexadecimal digits")
			Ok(code) if code >= 0xD800 and code <= 0xDFFF => fail(line, column, "escape sequence is a UTF-16 surrogate, not a character")
			Ok(code) if code > 0x10FFFF => fail(line, column, "escape sequence is beyond the last Unicode character")
			Ok(code) => Ok(utf8_encode(code))
		}
	}
}

parse_hex : String.Utf8, U32 -> Try(U32, [InvalidHex])
parse_hex = |bytes, value| {
	match bytes {
		[] => Ok(value)
		[first, .. as rest] => {
			digit =
				if first >= '0' and first <= '9' {
					Ok(first - '0')
				} else if first >= 'a' and first <= 'f' {
					Ok(first - 'a' + 10)
				} else if first >= 'A' and first <= 'F' {
					Ok(first - 'A' + 10)
				} else {
					Err(InvalidHex)
				}

			parse_hex(rest, value * 16 + U8.to_u32(digit?))
		}
	}
}

utf8_encode : U32 -> String.Utf8
utf8_encode = |code| {
	if code < 0x80 {
		[U32.to_u8_wrap(code)]
	} else if code < 0x800 {
		[U32.to_u8_wrap(0xC0 + code // 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else if code < 0x10000 {
		[U32.to_u8_wrap(0xE0 + code // 4096), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else {
		[U32.to_u8_wrap(0xF0 + code // 262144), U32.to_u8_wrap(0x80 + (code // 4096) % 64), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
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
			indent = count_spaces(line.content, 0)
			after_indent = line.content.drop_first(indent)
			content = trim_end_spaces(strip_comment(after_indent, NoQuote, Bool.False, Bool.True, []))
			tab = after_indent.first() == Ok('\t')

			if content.is_empty() {
				clean_lines(rest, out)
			} else {
				clean_lines(rest, out.append({ content, indent, number: line.number, terminated: line.terminated, tab }))
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
		# A line break ends a line; it does not start an empty final one.
		[] if current.is_empty() and !lines.is_empty() => lines
		[] => lines.append({ content: current, indent: 0, number, terminated: Bool.False, tab: Bool.False })
		['\r', '\n', .. as rest] => split_lines(rest, number + 1, [], lines.append({ content: current, indent: 0, number, terminated: Bool.True, tab: Bool.False }))
		['\n', .. as rest] | ['\r', .. as rest] => split_lines(rest, number + 1, [], lines.append({ content: current, indent: 0, number, terminated: Bool.True, tab: Bool.False }))
		[first, .. as rest] => split_lines(rest, number, current.append(first), lines)
	}
}

strip_comment : String.Utf8, Quote, Bool, Bool, String.Utf8 -> String.Utf8
strip_comment = |bytes, quote, escaped, separated, out| strip_comment_help(bytes, quote, escaped, separated, 0, out)

strip_comment_help : String.Utf8, Quote, Bool, Bool, U64, String.Utf8 -> String.Utf8
strip_comment_help = |bytes, quote, escaped, separated, depth, out| {
	match bytes {
		[] => out

		[first, ..] if first == '#' and quote == NoQuote and separated => out

		['\\', .. as rest] if quote == DoubleQuote and !escaped =>
			strip_comment_help(rest, quote, Bool.True, Bool.False, depth, out.append('\\'))

		['"', .. as rest] if quote == NoQuote and scalar_can_start(out, depth > 0) =>
			strip_comment_help(rest, DoubleQuote, Bool.False, Bool.False, depth, out.append('"'))

		['"', .. as rest] if quote == DoubleQuote and !escaped =>
			strip_comment_help(rest, NoQuote, Bool.False, Bool.False, depth, out.append('"'))

		['\'', .. as rest] if quote == NoQuote and scalar_can_start(out, depth > 0) =>
			strip_comment_help(rest, SingleQuote, Bool.False, Bool.False, depth, out.append('\''))

		['\'', '\'', .. as rest] if quote == SingleQuote =>
			strip_comment_help(rest, quote, Bool.False, Bool.False, depth, out.concat(['\'', '\'']))

		['\'', .. as rest] if quote == SingleQuote =>
			strip_comment_help(rest, NoQuote, Bool.False, Bool.False, depth, out.append('\''))

		[open, .. as rest] if (open == '[' or open == '{') and quote == NoQuote and (depth > 0 or scalar_can_start(out, Bool.False)) =>
			strip_comment_help(rest, quote, Bool.False, Bool.False, depth + 1, out.append(open))

		[close, .. as rest] if (close == ']' or close == '}') and quote == NoQuote and depth > 0 =>
			strip_comment_help(rest, quote, Bool.False, Bool.False, depth - 1, out.append(close))

		[first, .. as rest] =>
			strip_comment_help(rest, quote, Bool.False, first == ' ' or first == '\t', depth, out.append(first))
		}
}

## Whether a scalar or flow collection may start after `prefix`: only white
## space and standalone "-" or "?" indicators may separate it from the start of
## the line, a ": " value indicator, or (in a flow collection) "[", "{" or ",".
## Anywhere else, quotes and brackets are ordinary plain scalar text.
scalar_can_start : String.Utf8, Bool -> Bool
scalar_can_start = |prefix, in_flow| {
	var $index = prefix.len()
	var $answer = Err(Undecided)

	while $answer == Err(Undecided) {
		end = $index
		while $index > 0 and is_white(prefix.get($index - 1) ?? 'x') {
			$index = $index - 1
		}
		separated = $index < end

		if $index == 0 {
			$answer = Ok(Bool.True)
		} else {
			previous = prefix.get($index - 1) ?? 'x'
			standalone = $index == 1 or is_white(prefix.get($index - 2) ?? 'x')

			if in_flow and (previous == '[' or previous == '{' or previous == ',') {
				$answer = Ok(Bool.True)
			} else if previous == ':' and separated {
				$answer = Ok(Bool.True)
			} else if (previous == '-' or previous == '?') and separated and standalone {
				$index = $index - 1
			} else {
				$answer = Ok(Bool.False)
			}
		}
	}

	$answer ?? Bool.False
}

is_white : U8 -> Bool
is_white = |byte| byte == ' ' or byte == '\t'

split_mapping_entry : String.Utf8 -> Try({ key : String.Utf8, value : String.Utf8, value_column : U64 }, [NotFound])
split_mapping_entry = |bytes| {
	find_mapping_colon(bytes, bytes, NoQuote, Bool.False, 0, 0, 0)
}

find_mapping_colon : String.Utf8, String.Utf8, Quote, Bool, U64, U64, U64 -> Try({ key : String.Utf8, value : String.Utf8, value_column : U64 }, [NotFound])
find_mapping_colon = |all, bytes, quote, escaped, square_depth, curly_depth, index| {
	in_flow = square_depth > 0 or curly_depth > 0
	can_start = |_| scalar_can_start(all.sublist({ start: 0, len: index }), in_flow)

	match bytes {
		[] => Err(NotFound)

		[':', .. as rest] if quote == NoQuote and !in_flow and (rest.is_empty() or starts_with_space(rest)) =>
			Ok({ key: all.sublist({ start: 0, len: index }), value: trim_start_spaces(rest), value_column: index + 2 })

		['\\', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, quote, Bool.True, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == NoQuote and can_start({}) =>
			find_mapping_colon(all, rest, DoubleQuote, Bool.False, square_depth, curly_depth, index + 1)

		['"', .. as rest] if quote == DoubleQuote and !escaped =>
			find_mapping_colon(all, rest, NoQuote, Bool.False, square_depth, curly_depth, index + 1)

		['\'', .. as rest] if quote == NoQuote and can_start({}) =>
			find_mapping_colon(all, rest, SingleQuote, Bool.False, square_depth, curly_depth, index + 1)

		['\'', '\'', .. as rest] if quote == SingleQuote =>
			find_mapping_colon(all, rest, quote, Bool.False, square_depth, curly_depth, index + 2)

		['\'', .. as rest] if quote == SingleQuote =>
			find_mapping_colon(all, rest, NoQuote, Bool.False, square_depth, curly_depth, index + 1)

		['[', .. as rest] if quote == NoQuote and (in_flow or can_start({})) => find_mapping_colon(all, rest, quote, Bool.False, square_depth + 1, curly_depth, index + 1)
		[']', .. as rest] if quote == NoQuote and square_depth > 0 => find_mapping_colon(all, rest, quote, Bool.False, square_depth - 1, curly_depth, index + 1)
		['{', .. as rest] if quote == NoQuote and (in_flow or can_start({})) => find_mapping_colon(all, rest, quote, Bool.False, square_depth, curly_depth + 1, index + 1)
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
		['"', .. as rest] if quote == NoQuote and scalar_can_start(current, Bool.True) => split_flow_items_help(rest, current.append('"'), items, DoubleQuote, Bool.False, square_depth, curly_depth, line, column)
		['"', .. as rest] if quote == DoubleQuote and !escaped => split_flow_items_help(rest, current.append('"'), items, NoQuote, Bool.False, square_depth, curly_depth, line, column)
		['\'', .. as rest] if quote == NoQuote and scalar_can_start(current, Bool.True) => split_flow_items_help(rest, current.append('\''), items, SingleQuote, Bool.False, square_depth, curly_depth, line, column)
		['\'', '\'', .. as rest] if quote == SingleQuote => split_flow_items_help(rest, current.concat(['\'', '\'']), items, quote, Bool.False, square_depth, curly_depth, line, column)
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

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

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

## Plain scalars resolve with the YAML 1.2 core schema; everything else is a string.
expect {
	actual = Yaml.parse_str("[1.2.3, 1e, nUlL, tRUE, 0x1F, 0o17, 1., .5, -1.5e2, .inf, -.Inf, 1_000, 0X1]")?
	actual
	== Sequence([
		String("1.2.3"),
		String("1e"),
		String("nUlL"),
		String("tRUE"),
		Int(31),
		Int(15),
		Float(1.0),
		Float(0.5),
		Float(-150.0),
		Float(F64.infinity),
		Float(-F64.infinity),
		String("1_000"),
		String("0X1"),
	])
}

## Not-a-number resolves to a float.
expect {
	match Yaml.parse_str(".nan") {
		Ok(Float(value)) => F64.is_nan(value)
		_ => Bool.False
	}
}

## Sequence entries may be compact nested sequences.
expect {
	actual = Yaml.parse_str("- - a\n  - b\n- - - c\n")?
	actual == Sequence([Sequence([String("a"), String("b")]), Sequence([Sequence([String("c")])])])
}

## Mapping values may be sequences at the key's own indentation.
expect {
	actual = Yaml.parse_str("steps:\n- run: a\n  name: x\n- b\nnext: 1\n")?
	actual
	== Mapping([
		{ key: "steps", value: Sequence([Mapping([{ key: "run", value: String("a") }, { key: "name", value: String("x") }]), String("b")]) },
		{ key: "next", value: Int(1) },
	])
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

## Double-quoted strings support every YAML 1.2 escape, including Unicode.
expect {
	actual = Yaml.parse_str("v: \"\\x41\\u00e9\\U0001F600\\/\\_\\0\"")?
	actual == Mapping([{ key: "v", value: String("Aé😀/\u(a0)\u(0)") }])
}

## Unsupported escapes before a multi-byte character fail instead of crashing.
expect {
	match Yaml.parse_str("v: \"\\é\"") {
		Err(YamlError({ message, .. })) => message == "unsupported escape sequence `\\é`"
		_ => Bool.False
	}
}

## Surrogate and short Unicode escapes are rejected.
expect Yaml.parse_str("v: \"\\uD800\"").is_err() and Yaml.parse_str("v: \"\\u12\"").is_err()

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
			{ key: "summary", value: String("one two") },
		])
}

## A block scalar's final line without a line break gets no newline, even when kept.
expect {
	clip = Yaml.parse_str("a: |\n  x")?
	keep = Yaml.parse_str("a: |+\n  x")?
	clip == Mapping([{ key: "a", value: String("x") }]) and keep == clip
}

## Keep chomping at the end of input keeps exactly the trailing line breaks.
expect {
	actual = Yaml.parse_str("a: |+\n  x\n\n")?
	actual == Mapping([{ key: "a", value: String("x\n\n") }])
}

## Block scalars without content lines are empty unless kept.
expect {
	clip = Yaml.parse_str("a: |\n\n")?
	keep = Yaml.parse_str("a: |+\n\n\n")?
	clip == Mapping([{ key: "a", value: String("") }]) and keep == Mapping([{ key: "a", value: String("\n\n") }])
}

## Tabs after the indentation are block scalar content, not indentation.
expect {
	actual = Yaml.parse_str("a: |\n  x\n  \ty\n")?
	actual == Mapping([{ key: "a", value: String("x\n\ty\n") }])
}

## Whitespace beyond the content indentation on an otherwise empty line is content.
expect {
	actual = Yaml.parse_str("a: |\n  x\n   \t\n  y\n")?
	actual == Mapping([{ key: "a", value: String("x\n \t\ny\n") }])
}

## Folding drops the line break before empty lines between text lines.
expect {
	actual = Yaml.parse_str("a: >\n  a\n\n\n  b\n")?
	actual == Mapping([{ key: "a", value: String("a\n\nb\n") }])
}

## Final whitespace without a line break still ends its line (yaml-test-suite JEF9, L24T).
expect {
	keep = Yaml.parse_str("- |+\n   ")?
	clip = Yaml.parse_str("foo: |\n  x\n   ")?
	keep == Sequence([String("\n")]) and clip == Mapping([{ key: "foo", value: String("x\n \n") }])
}

## Spaces and a tab form a content line that sets the indentation (R4YG, Y79Y).
expect {
	actual = Yaml.parse_str("foo: |\n \t\nbar: 1\n")?
	actual == Mapping([{ key: "foo", value: String("\t\n") }, { key: "bar", value: Int(1) }])
}

## A tab-only line cannot end a block scalar (Y79Y).
expect Yaml.parse_str("foo: |\n\t\nbar: 1\n").is_err()

## Block scalars at the document root may start in column 0, until a document marker.
expect {
	actual = Yaml.parse_str("|\na\n...\n")?
	actual == String("a\n")
}

## Lone carriage returns are line breaks.
expect {
	actual = Yaml.parse_str("value: |\r  one\r  two\r")?
	actual == Mapping([{ key: "value", value: String("one\ntwo\n") }])
}

## Compact mappings in sequence entries are indented by the spaces after "-"
## (YAML 1.2 8.2.1: here the mapping, and so the indicator, is relative to column 4).
expect {
	actual = Yaml.parse_str("-   value: |2\n      a\n    next: done\n")?
	actual == Sequence([Mapping([{ key: "value", value: String("a\n") }, { key: "next", value: String("done") }])])
}

## Quotes and brackets inside plain scalars are ordinary text.
expect {
	actual = Yaml.parse_str("k: it's # comment\nit's: \"q\" # c\na[b: x]{\nlist: [a, 'b''c', \"d, e\"]\n")?
	actual
	== Mapping([
		{ key: "k", value: String("it's") },
		{ key: "it's", value: String("q") },
		{ key: "a[b", value: String("x]{") },
		{ key: "list", value: Sequence([String("a"), String("b'c"), String("d, e")]) },
	])
}

## Indicator-like text inside a plain scalar does not start a quoted scalar.
expect {
	actual = Yaml.parse_str("{_? -  ': 1, b: 2}")?
	actual == Mapping([{ key: "_? -  '", value: Int(1) }, { key: "b", value: Int(2) }])
}

## A dash inside plain text is not an indicator that can start a quoted scalar.
expect {
	actual = Yaml.parse_str("b- \"q: x\n")?
	actual == Mapping([{ key: "b- \"q", value: String("x") }])
}

## A tab before a document marker is not a document marker.
expect Yaml.parse_str("\t---\na: 1").is_err()

## Leading empty lines may not be indented more than detected block content.
expect Yaml.parse_str("a: |\n    \n  x\n").is_err()

## Chomping indicators are supported for block scalars.
expect {
	actual = Yaml.parse_str("note: |-\n  hello")?
	actual == Mapping([{ key: "note", value: String("hello") }])
}

## Block scalar content keeps blank lines and # characters literally.
expect {
	actual = Yaml.parse_str("text: |\n  # not a comment\n\n  after\n")?
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
	actual = Yaml.parse_str("text: >\n  intro\n    code\n  outro\n")?
	actual == Mapping([{ key: "text", value: String("intro\n  code\noutro\n") }])
}

## Malformed flow collections still fail.
expect Yaml.parse_str("values: [one, two").is_err()
