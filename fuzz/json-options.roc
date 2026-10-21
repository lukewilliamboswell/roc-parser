app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.JsonValue
import parser.Utf8

## Arbitrary documents under every JsonValue.Options field.
##
## The first two bytes choose the options and the rest is the document:
## - byte 0, one bit per relaxation: comments (1), trailing_commas (2),
##   bom (4), non_finite (8), control_characters (16) and
##   lone_surrogates Replace (32); bits 6 and 7 are ignored;
## - byte 1, the checks: duplicates in bits 0-1 (Reject, CollapseIdentical,
##   KeepFirst, KeepLast), top_level in bits 2-3 (Any, ObjectOrArray, Object,
##   Any), interoperable_numbers in bit 4, and in bits 5-7 an entry of the
##   fixed `limit_table` (entry 0 has no limits).
##
## Seeds in fuzz/seeds/json-options.json start with these two bytes.
##
## Write S for the chosen options, and compare it with the same options
## without its relaxations (B), without its checks (A), and with
## `duplicates: Reject` (T). A duplicate mode changes the value, so it is held
## fixed when relaxations or checks are removed:
## - relaxations are monotone: if B accepts with v, S accepts with v. With no
##   checks chosen, B is strict parse_str;
## - checks are monotone: if S accepts with v, A accepts with v. With no
##   relaxations chosen and `duplicates: Reject`, A is strict parse_str;
## - Reject is the strictest duplicate mode: if T accepts with v, S accepts
##   with v; if S uses CollapseIdentical and accepts with v, T accepts with v
##   or fails with a duplicate key;
## - every error is located inside the document, and JsonValue.parser_with
##   agrees with JsonValue.parse_with under S.

Parsed : Try(JsonValue, [InvalidJson(JsonValue.Error)])

Duplicates : [Reject, CollapseIdentical, KeepFirst, KeepLast]

TopLevel : [Any, ObjectOrArray, Object]

Limits : { max_depth : U64, max_items : U64, max_string_bytes : U64, max_number_digits : U64 }

Choice : {
	comments : Bool,
	trailing_commas : Bool,
	bom : Bool,
	non_finite : Bool,
	control_characters : Bool,
	replace_surrogates : Bool,
	duplicates : Duplicates,
	top_level : TopLevel,
	interoperable_numbers : Bool,
	limits : Limits,
}

Input : { choice : Choice, document : List(U8) }

no_limits : Limits
no_limits = { max_depth: 128, max_items: U64.highest, max_string_bytes: U64.highest, max_number_digits: U64.highest }

## Entries 1-3 are reachable from seeds, whose bytes are below 0x80.
limit_table : List(Limits)
limit_table = [
	no_limits,
	{ max_depth: 3, max_items: 4, max_string_bytes: 16, max_number_digits: 8 },
	{ ..no_limits, max_depth: 1 },
	{ ..no_limits, max_items: 2 },
	{ ..no_limits, max_string_bytes: 0 },
	{ ..no_limits, max_number_digits: 1 },
	{ ..no_limits, max_depth: 0 },
	{ max_depth: 64, max_items: 0, max_string_bytes: 3, max_number_digits: 17 },
]

## Whether `byte` has the bit whose value is the power of two `mask`.
bit : U8, U8 -> Bool
bit = |byte, mask| (byte // mask) % 2 == 1

choose : U8, U8 -> Choice
choose = |relaxations, checks| {
	duplicates =
		match checks % 4 {
			0 => Reject
			1 => CollapseIdentical
			2 => KeepFirst
			_ => KeepLast
		}
	top_level =
		match (checks // 4) % 4 {
			1 => ObjectOrArray
			2 => Object
			_ => Any
		}
	{
		comments: bit(relaxations, 1),
		trailing_commas: bit(relaxations, 2),
		bom: bit(relaxations, 4),
		non_finite: bit(relaxations, 8),
		control_characters: bit(relaxations, 16),
		replace_surrogates: bit(relaxations, 32),
		duplicates,
		top_level,
		interoperable_numbers: bit(checks, 16),
		limits: limit_table.get(U8.to_u64(checks // 32)) ?? no_limits,
	}
}

generate : List(U8) -> Input
generate = |bytes| {
	choice = choose(bytes.get(0) ?? 0, bytes.get(1) ?? 0)
	{ choice, document: bytes.drop_first(2) }
}

options_of : Choice -> JsonValue.Options
options_of = |choice| {
	max_depth: choice.limits.max_depth,
	max_items: choice.limits.max_items,
	max_string_bytes: choice.limits.max_string_bytes,
	max_number_digits: choice.limits.max_number_digits,
	comments: choice.comments,
	trailing_commas: choice.trailing_commas,
	bom: choice.bom,
	non_finite: choice.non_finite,
	control_characters: choice.control_characters,
	lone_surrogates: if (choice.replace_surrogates) Replace else Reject,
	duplicates: choice.duplicates,
	top_level: choice.top_level,
	interoperable_numbers: choice.interoperable_numbers,
}

## The choice without its relaxations.
without_relaxations : Choice -> Choice
without_relaxations = |choice| {
	..choice,
	comments: False,
	trailing_commas: False,
	bom: False,
	non_finite: False,
	control_characters: False,
	replace_surrogates: False,
}

## The choice without its checks; the duplicate mode stays.
without_checks : Choice -> Choice
without_checks = |choice| { ..choice, top_level: Any, interoperable_numbers: False, limits: no_limits }

has_relaxations : Choice -> Bool
has_relaxations = |choice| without_relaxations(choice) != choice

has_checks : Choice -> Bool
has_checks = |choice| without_checks(choice) != choice

strict : Choice
strict = choose(0, 0)

test : Input -> Fuzz.Outcome
test = |input| {
	match Str.from_utf8(input.document) {
		Err(_) => {
			check_bytes(input)
			Fuzz.keep
		}
		Ok(text) => {
			chosen = input.choice
			parse = |choice| JsonValue.parse_with(text, options_of(choice))
			result = parse(chosen)
			check_location(input, chosen, result)
			check_combinator(input, result)

			# Relaxations are monotone.
			base = without_relaxations(chosen)
			base_result = if (has_relaxations(chosen)) parse(base) else result
			check_location(input, base, base_result)
			if base == strict and JsonValue.parse_str(text) != base_result {
				crash "parse_with(text, {}) differs from parse_str(text)"
			}
			match base_result {
				Ok(value) if result != Ok(value) => crash "relaxations rejected or changed a document that their checks accept\n${Str.inspect(text)}\nwithout relaxations: ${show_result(base_result)}\nwith relaxations:    ${show_result(result)}\n${show_choice(chosen)}"
				_ => {}
			}

			# Checks are monotone.
			unchecked = without_checks(chosen)
			unchecked_result = if (has_checks(chosen)) parse(unchecked) else result
			check_location(input, unchecked, unchecked_result)
			match result {
				Ok(value) if unchecked_result != Ok(value) => crash "a check accepted a document that fails or differs without it\n${Str.inspect(text)}\nwith checks:    ${show_result(result)}\nwithout checks: ${show_result(unchecked_result)}\n${show_choice(chosen)}"
				_ => {}
			}

			# Reject is the strictest duplicate mode.
			if chosen.duplicates != Reject {
				rejecting = { ..chosen, duplicates: Reject }
				rejecting_result = parse(rejecting)
				check_location(input, rejecting, rejecting_result)
				match rejecting_result {
					Ok(value) if result != Ok(value) => crash "a document without repeated keys parsed differently under ${duplicates_flag(chosen.duplicates)}\n${Str.inspect(text)}\nReject: ${show_result(rejecting_result)}\nchosen: ${show_result(result)}\n${show_choice(chosen)}"
					_ => {}
				}
				if chosen.duplicates == CollapseIdentical {
					match (result, rejecting_result) {
						(Ok(value), Ok(other)) if value != other => crash "CollapseIdentical changed a document without repeated keys\n${Str.inspect(text)}"
						(Ok(_), Err(InvalidJson(error))) if !Str.starts_with(error.message, "duplicate key \"") => crash "CollapseIdentical accepted a document that Reject refuses for another reason: ${error.message}\n${Str.inspect(text)}\n${show_choice(chosen)}"
						_ => {}
					}
				}
			}
			Fuzz.keep
		}
	}
}

## Positions

## Line lengths in bytes, where CR LF, CR, and LF each end a line.
line_lengths : List(U8) -> List(U64)
line_lengths = |bytes| {
	var $lengths = []
	var $current = 0
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' or byte == '\r' {
			$lengths = $lengths.append($current)
			$current = 0
			$index = if (byte == '\r' and bytes.get($index + 1) == Ok('\n')) $index + 2 else $index + 1
		} else {
			$current = $current + 1
			$index = $index + 1
		}
	}
	$lengths.append($current)
}

## One-based line and byte column of a byte offset, counting CR LF, CR and
## LF as line ends, independent of the parser's own locator.
line_column : List(U8), U64 -> { line : U64, column : U64 }
line_column = |bytes, offset| {
	var $line = 1
	var $line_start = 0
	var $index = 0
	while $index < offset and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' {
			$line = $line + 1
			$line_start = $index + 1
		} else if byte == '\r' {
			if $index + 1 < offset and bytes.get($index + 1) == Ok('\n') {
				$index = $index + 1
			}
			$line = $line + 1
			$line_start = $index + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: offset - $line_start + 1 }
}

check_location : Input, Choice, Parsed -> {}
check_location = |input, choice, result| {
	match result {
		Ok(_) => {}
		Err(InvalidJson(error)) => {
			context = "${show_result(result)}\n${show_choice(choice)}"
			if error.line == 0 or error.column == 0 {
				crash "JSON error locations must be one-based: ${context}"
			}
			if error.message.is_empty() {
				crash "JSON errors must explain the problem: ${context}"
			}
			lengths = line_lengths(input.document)
			match lengths.get(error.line - 1) {
				Err(_) => crash "error line ${error.line.to_str()} is past the last line (${lengths.len().to_str()}): ${context}"
				Ok(length) =>
					if error.column > length + 1 {
						crash "error column ${error.column.to_str()} is past the end of line ${error.line.to_str()} (${length.to_str()} bytes): ${context}"
					}
			}
		}
	}
}

## Combinator agreement

## Bytes that are not UTF-8: parser_with must not crash (it may read strings
## in such input lossily) and must stay inside the input.
check_bytes : Input -> {}
check_bytes = |input| {
	bytes = input.document
	match Utf8.parse_bytes_partial(JsonValue.parser_with(options_of(input.choice)), bytes) {
		Ok({ value: _, rest }) =>
			if rest.len() > bytes.len() {
				crash "JsonValue.parser_with returned more input than it was given\n${show_choice(input.choice)}"
			}
		Err(ParseError({ message, offset })) =>
			if offset > bytes.len() or message.is_empty() {
				crash "JsonValue.parser_with failed outside the input (offset ${offset.to_str()} of ${bytes.len().to_str()}): ${message}\n${show_choice(input.choice)}"
			}
	}
}

unexpected_content : Str
unexpected_content = "unexpected content after the JSON value"

check_combinator : Input, Parsed -> {}
check_combinator = |input, result| {
	bytes = input.document
	options = options_of(input.choice)
	combinator = Utf8.parse_bytes_partial(JsonValue.parser_with(options), bytes)
	consistent =
		match (result, combinator) {
			(Ok(value), Ok({ value: other, rest })) => rest.is_empty() and value == other
			(Err(InvalidJson(error)), Ok({ value: other, rest })) => {
				consumed = bytes.len() - rest.len()
				location = line_column(bytes, consumed)
				same_place = location.line == error.line and location.column == error.column
				prefix = Str.from_utf8(bytes.sublist({ start: 0, len: consumed })) ?? ""
				!rest.is_empty() and same_place and error.message == unexpected_content and JsonValue.parse_with(prefix, options) == Ok(other)
			}
			(Err(InvalidJson(error)), Err(ParseError({ message, offset }))) => {
				location = line_column(bytes, offset)
				location.line == error.line and location.column == error.column and message == error.message
			}
			_ => False
		}
	if !consistent {
		crash "JsonValue.parser_with disagrees with JsonValue.parse_with\nparse_with: ${show_result(result)}\nparser_with: ${Str.inspect(combinator)}\n${show_choice(input.choice)}"
	}
}

## Display

show_result : Parsed -> Str
show_result = |result| {
	match result {
		Ok(value) => Str.inspect(value)
		Err(InvalidJson(error)) => "error ${error.line.to_str()}:${error.column.to_str()} ${error.message}"
	}
}

duplicates_flag : Duplicates -> Str
duplicates_flag = |duplicates| {
	match duplicates {
		Reject => "reject"
		CollapseIdentical => "collapse-identical"
		KeepFirst => "keep-first"
		KeepLast => "keep-last"
	}
}

## The options as review-probe flags; defaults are left out.
flags : Choice -> Str
flags = |choice| {
	limit = |name, value, default| if (value == default) [] else ["--${name} ${value.to_str()}"]
	switch = |name, on| if (on) ["--${name}"] else []
	top_level =
		match choice.top_level {
			Any => []
			ObjectOrArray => ["--top-level object-or-array"]
			Object => ["--top-level object"]
		}
	duplicates = if (choice.duplicates == Reject) [] else ["--duplicates ${duplicates_flag(choice.duplicates)}"]
	[]
		.concat(limit("max-depth", choice.limits.max_depth, no_limits.max_depth))
		.concat(limit("max-items", choice.limits.max_items, no_limits.max_items))
		.concat(limit("max-string-bytes", choice.limits.max_string_bytes, no_limits.max_string_bytes))
		.concat(limit("max-number-digits", choice.limits.max_number_digits, no_limits.max_number_digits))
		.concat(switch("comments", choice.comments))
		.concat(switch("trailing-commas", choice.trailing_commas))
		.concat(switch("bom", choice.bom))
		.concat(switch("non-finite", choice.non_finite))
		.concat(switch("control-characters", choice.control_characters))
		.concat(if (choice.replace_surrogates) ["--lone-surrogates replace"] else [])
		.concat(duplicates)
		.concat(top_level)
		.concat(switch("interoperable-numbers", choice.interoperable_numbers))
		|> Str.join_with(" ")
}

show_choice : Choice -> Str
show_choice = |choice| "JSON-OPTIONS: ${flags(choice)}"

hex_byte : U8 -> Str
hex_byte = |byte| {
	digits = "0123456789abcdef".to_utf8()
	Str.from_utf8([digits.get(U8.to_u64(byte // 16)) ?? '0', digits.get(U8.to_u64(byte % 16)) ?? '0']) ?? "00"
}

## Show an input the way review_json.py's corpus mode reads it: the
## document's hex and the options as probe flags.
show : Input -> Str
show = |input| {
	hex = input.document.fold("", |acc, byte| Str.concat(acc, hex_byte(byte)))
	"${Str.inspect(Str.from_utf8(input.document))}\nJSON-HEX: ${hex}\n${show_choice(input.choice)}"
}

target = Fuzz.target_with({
	name: "json-options",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show,
})
