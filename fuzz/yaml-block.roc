app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Yaml

## Block scalar property: bytes choose a literal or folded block scalar, its
## chomping and indentation indicators, the node it sits in, how it ends, and
## its content lines. The expected string is computed from the YAML 1.2 rules
## (8.1.1.2 chomping, 8.1.3 folding), independently of the parser.

Style : [Literal, Folded]

Chomp : [Clip, Strip, Keep]

Context : [Root, MapValue, SeqItem, CompactMap, NestedMap]

Ending : [Eof, EofNoBreak, Sibling, CommentThenSibling, DocumentEnd]

Input : {
	yaml : Str,
	expected : Yaml,
	style : Style,
	chomp : Chomp,
	ending : Ending,
	lines : List(Str),
	trailing : U64,
}

## Content lines without the block indentation. "" is an empty line.
tokens : List(Str)
tokens = ["a", "", "b c", " more", "  code", "# literal", "x: y", "- z", "tail ", "---", "...", "'q", "\"dq", "{f: [1]}", "\ttab", "   ", "a \t b"]

spaces : U64 -> Str
spaces = |count| Str.join_with(List.repeat(" ", count), "")

byte_at : List(U8), U64 -> U64
byte_at = |bytes, index| U8.to_u64(bytes.get(index) ?? 0)

starts_white : Str -> Bool
starts_white = |line| Str.starts_with(line, " ") or Str.starts_with(line, "\t")

## YAML 1.2 8.1.3: breaks between two non-spaced text lines fold; a single
## break becomes a space and is otherwise dropped in favour of the empty lines.
fold : List(Str) -> Str
fold = |lines| {
	var $out = ""
	var $previous = Err(NoLine)
	var $empties = 0
	var $index = 0
	while $index < lines.len() {
		line = lines.get($index) ?? ""
		if line.is_empty() {
			$empties = $empties + 1
		} else {
			separator =
				match $previous {
					Err(_) => Str.join_with(List.repeat("\n", $empties), "")
					Ok(before) =>
						if !starts_white(before) and !starts_white(line) {
							if $empties == 0 " " else Str.join_with(List.repeat("\n", $empties), "")
						} else {
							Str.join_with(List.repeat("\n", $empties + 1), "")
						}
				}
			$out = Str.concat(Str.concat($out, separator), line)
			$previous = Ok(line)
			$empties = 0
		}
		$index = $index + 1
	}
	$out
}

## The scalar's value from its content lines (ending with a non-empty line, or
## empty) and the number of trailing empty lines.
expected_value : Style, Chomp, List(Str), U64, Bool -> Str
expected_value = |style, chomp, lines, trailing, final_break| {
	if lines.is_empty() {
		match chomp {
			Keep => Str.join_with(List.repeat("\n", trailing), "")
			_ => ""
		}
	} else {
		body =
			match style {
				Literal => Str.join_with(lines, "\n")
				Folded => fold(lines)
			}
		breaks = if final_break trailing + 1 else 0
		match chomp {
			Strip => body
			Clip => if final_break Str.concat(body, "\n") else body
			Keep => Str.concat(body, Str.join_with(List.repeat("\n", breaks), ""))
		}
	}
}

generate : List(U8) -> Input
generate = |bytes| {
	shape = byte_at(bytes, 0)
	setup = byte_at(bytes, 1)
	style = if shape % 2 == 0 Literal else Folded
	chomp =
		match (shape / 2) % 3 {
			0 => Clip
			1 => Strip
			_ => Keep
		}
	context =
		match (shape / 6) % 5 {
			0 => Root
			1 => MapValue
			2 => SeqItem
			3 => CompactMap
			_ => NestedMap
		}
	ending =
		match setup % 5 {
			0 => Eof
			4 => EofNoBreak
			1 => if context == Root DocumentEnd else Sibling
			2 => if context == Root DocumentEnd else CommentThenSibling
			_ => DocumentEnd
		}
	# Explicit indicators are relative to the parent node's indentation, which is
	# unambiguous for mapping values; elsewhere the indentation is detected.
	explicit_allowed = context == MapValue or context == CompactMap or context == NestedMap
	offset = (setup / 4) % 3 + 1
	want_explicit = explicit_allowed and (setup / 12) % 2 == 1
	indicator_first = (setup / 24) % 2 == 0
	trailing = (setup / 48) % 4
	empty_pad = shape / 30 % 2 == 1

	chosen_lines = bytes.drop_first(2).sublist({ start: 0, len: 24 }).map(|b| tokens.get(U8.to_u64(b) % tokens.len()) ?? "a")
	# Detected indentation comes from the first content line, so without an
	# explicit indicator that line must not be more indented.
	leads_white = starts_white(chosen_lines.find_first(|line| !line.is_empty()) ?? "a")
	# An empty root scalar before "..." is read differently by YAML parsers.
	no_content = chosen_lines.all(|line| line.is_empty())
	raw_lines = if (!explicit_allowed and leads_white) or (context == Root and no_content) List.prepend(chosen_lines, "lead") else chosen_lines
	# Content is everything up to the last non-empty line; later empties trail.
	last_content = last_non_empty(raw_lines)
	content = raw_lines.sublist({ start: 0, len: last_content })
	trailing_total = trailing + (raw_lines.len() - last_content)

	first_content = content.find_first(|line| !line.is_empty()) ?? "a"
	explicit = want_explicit or (explicit_allowed and starts_white(first_content))

	base =
		match context {
			Root => 0
			MapValue => 0
			SeqItem => 0
			CompactMap => 2
			NestedMap => 2
		}
	indent = base + offset
	pad = spaces(indent)
	style_char = if style == Literal "|" else ">"
	chomp_char =
		match chomp {
			Clip => ""
			Strip => "-"
			Keep => "+"
		}
	indicator = if explicit offset.to_str() else ""
	header = if indicator_first "${style_char}${indicator}${chomp_char}" else "${style_char}${chomp_char}${indicator}"
	body_lines = content.map(|line| if line.is_empty() (if empty_pad pad else "") else Str.concat(pad, line))
	blank_lines = List.repeat(if empty_pad pad else "", if ending == EofNoBreak 0 else trailing_total)
	block = Str.join_with([header].concat(body_lines).concat(blank_lines), "\n")

	# Without a final line break the last content line has none to chomp, and
	# trailing empty lines cannot follow it. White space alone still ends its
	# line at the end of input (yaml-test-suite L24T, JEF9).
	last_white = (content.last() ?? "x").to_utf8().all(|b| b == ' ' or b == '\t')
	no_break = ending == EofNoBreak and !last_white
	trailing_kept = if ending == EofNoBreak 0 else trailing_total
	value = Text(expected_value(style, chomp, content, trailing_kept, !no_break))
	sibling_indent = spaces(base)
	tail =
		match ending {
			Eof => "\n"
			EofNoBreak => ""
			DocumentEnd => "\n...\n"
			Sibling => "\n${sibling_indent}next: 1\n"
			CommentThenSibling => "\n# comment\n${sibling_indent}next: 1\n"
		}
	has_sibling = ending == Sibling or ending == CommentThenSibling
	siblings = if has_sibling [{ key: "next", value: Int(1) }] else []
	entries = [{ key: "k", value }].concat(siblings)
	{ yaml, expected } =
		match context {
			Root => { yaml: "${block}${tail}", expected: value }
			MapValue => { yaml: "k: ${block}${tail}", expected: Mapping(entries) }
			SeqItem => {
				yaml: "- ${block}${if has_sibling Str.replace_each(tail, "next: 1", "- 1") else tail}",
				expected: Sequence([value].concat(if has_sibling [Int(1)] else [])),
			}
			CompactMap => { yaml: "- k: ${block}${tail}", expected: Sequence([Mapping(entries)]) }
			NestedMap => { yaml: "outer:\n  k: ${block}${tail}", expected: Mapping([{ key: "outer", value: Mapping(entries) }]) }
		}
	{ yaml, expected, style, chomp, ending, lines: content, trailing: trailing_total }
}

last_non_empty : List(Str) -> U64
last_non_empty = |lines| {
	var $last = 0
	var $index = 0
	while $index < lines.len() {
		if !(lines.get($index) ?? "").is_empty() {
			$last = $index + 1
		}
		$index = $index + 1
	}
	$last
}

test : Input -> Fuzz.Outcome
test = |input| {
	match Yaml.parse_str(input.yaml) {
		Ok(actual) if actual == input.expected => Fuzz.keep
		actual => crash "block scalar mismatch\n--- yaml ---\n${input.yaml}\n--- expected ---\n${Yaml.to_inspect(input.expected)}\n--- actual ---\n${show_result(actual)}"
	}
}

show_result : Try(Yaml, [InvalidYaml(Yaml.Error)]) -> Str
show_result = |result| {
	match result {
		Ok(value) => Yaml.to_inspect(value)
		Err(InvalidYaml(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

target = Fuzz.target_with({
	name: "yaml-block",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "${input.yaml}\n=> ${Yaml.to_inspect(input.expected)}",
})
