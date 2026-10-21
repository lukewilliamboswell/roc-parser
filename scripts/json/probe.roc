app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.JsonValue
import parser.Utf8

## Read one JSON document from stdin and print one line of JSON describing
## what JsonValue does with it. The protocol is written by hand here, never
## with JsonValue's own writer:
##
## - `{"status":"ok","value":TREE,"compact":W,"pretty":W,"sorted":W,"parser_agrees":B}`
##   where TREE is null, true, false, a string, `["number","<lexeme>"]`,
##   `["array",[TREE,...]]` or `["object",[[key,TREE],...]]`, and each W is the
##   written string or `{"error":"InvalidNumber"|"DuplicateKey"|"TooDeep","value":...}`;
## - `{"status":"error","line":L,"column":C,"message":M,"parser_agrees":B}`;
## - for input that is not UTF-8, the result of `JsonValue.parser_with`, with
##   `"offset"` added to an error;
## - `{"status":"usage_error","message":M}` for an unknown or malformed flag.
##
## `compact` is `JsonValue.to_str`, `pretty` is `to_str_with` with the
## `--sort-keys` and `--indent N` flags (indent 2 by default), and `sorted` is
## `to_str_with({ sort_keys: True })`. `parser_agrees` says whether
## `JsonValue.parser_with` gives the same verdict and value as `parse_with`.
##
## Options flags: `--max-depth N`, `--max-items N`, `--max-string-bytes N`,
## `--max-number-digits N`, `--comments`, `--trailing-commas`, `--bom`,
## `--non-finite`, `--control-characters`, `--lone-surrogates reject|replace`,
## `--duplicates reject|collapse-identical|keep-first|keep-last`,
## `--top-level any|object-or-array|object` and `--interoperable-numbers`.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	# basic-cli passes the arguments without the program name.
	match apply_flags(default_flags, args.map(OsStr.display)) {
		Err(message) => Stdout.line!("{\"status\":\"usage_error\",\"message\":${json_string(message)}}")?
		Ok(flags) => {
			bytes = Stdin.read_to_end!()?
			Stdout.line!(run(bytes, flags))?
		}
	}
	Ok({})
}

Flags : {
	max_depth : U64,
	max_items : U64,
	max_string_bytes : U64,
	max_number_digits : U64,
	comments : Bool,
	trailing_commas : Bool,
	bom : Bool,
	non_finite : Bool,
	control_characters : Bool,
	lone_surrogates : [Reject, Replace],
	duplicates : [Reject, CollapseIdentical, KeepFirst, KeepLast],
	top_level : [Any, ObjectOrArray, Object],
	interoperable_numbers : Bool,
	sort_keys : Bool,
	indent : U64,
}

default_flags : Flags
default_flags = {
	max_depth: 128,
	max_items: U64.highest,
	max_string_bytes: U64.highest,
	max_number_digits: U64.highest,
	comments: False,
	trailing_commas: False,
	bom: False,
	non_finite: False,
	control_characters: False,
	lone_surrogates: Reject,
	duplicates: Reject,
	top_level: Any,
	interoperable_numbers: False,
	sort_keys: False,
	indent: 2,
}

apply_flags : Flags, List(Str) -> Try(Flags, Str)
apply_flags = |flags, args| {
	match args {
		[] => Ok(flags)
		["--comments", .. as rest] => apply_flags({ ..flags, comments: True }, rest)
		["--trailing-commas", .. as rest] => apply_flags({ ..flags, trailing_commas: True }, rest)
		["--bom", .. as rest] => apply_flags({ ..flags, bom: True }, rest)
		["--non-finite", .. as rest] => apply_flags({ ..flags, non_finite: True }, rest)
		["--control-characters", .. as rest] => apply_flags({ ..flags, control_characters: True }, rest)
		["--interoperable-numbers", .. as rest] => apply_flags({ ..flags, interoperable_numbers: True }, rest)
		["--sort-keys", .. as rest] => apply_flags({ ..flags, sort_keys: True }, rest)
		["--max-depth", value, .. as rest] => apply_flags({ ..flags, max_depth: count("--max-depth", value)? }, rest)
		["--max-items", value, .. as rest] => apply_flags({ ..flags, max_items: count("--max-items", value)? }, rest)
		["--max-string-bytes", value, .. as rest] => apply_flags({ ..flags, max_string_bytes: count("--max-string-bytes", value)? }, rest)
		["--max-number-digits", value, .. as rest] => apply_flags({ ..flags, max_number_digits: count("--max-number-digits", value)? }, rest)
		["--indent", value, .. as rest] => apply_flags({ ..flags, indent: count("--indent", value)? }, rest)
		["--lone-surrogates", value, .. as rest] => apply_flags({ ..flags, lone_surrogates: lone_surrogates(value)? }, rest)
		["--duplicates", value, .. as rest] => apply_flags({ ..flags, duplicates: duplicates(value)? }, rest)
		["--top-level", value, .. as rest] => apply_flags({ ..flags, top_level: top_level(value)? }, rest)
		[other, ..] => Err("unknown or incomplete flag ${other}")
	}
}

count : Str, Str -> Try(U64, Str)
count = |flag, value| U64.from_str(value).map_err(|_| "${flag} takes a non-negative integer, not ${value}")

lone_surrogates : Str -> Try([Reject, Replace], Str)
lone_surrogates = |value| {
	match value {
		"reject" => Ok(Reject)
		"replace" => Ok(Replace)
		_ => Err("--lone-surrogates takes reject or replace, not ${value}")
	}
}

duplicates : Str -> Try([Reject, CollapseIdentical, KeepFirst, KeepLast], Str)
duplicates = |value| {
	match value {
		"reject" => Ok(Reject)
		"collapse-identical" => Ok(CollapseIdentical)
		"keep-first" => Ok(KeepFirst)
		"keep-last" => Ok(KeepLast)
		_ => Err("--duplicates takes reject, collapse-identical, keep-first or keep-last, not ${value}")
	}
}

top_level : Str -> Try([Any, ObjectOrArray, Object], Str)
top_level = |value| {
	match value {
		"any" => Ok(Any)
		"object-or-array" => Ok(ObjectOrArray)
		"object" => Ok(Object)
		_ => Err("--top-level takes any, object-or-array or object, not ${value}")
	}
}

options_of : Flags -> JsonValue.Options
options_of = |flags| {
	max_depth: flags.max_depth,
	max_items: flags.max_items,
	max_string_bytes: flags.max_string_bytes,
	max_number_digits: flags.max_number_digits,
	comments: flags.comments,
	trailing_commas: flags.trailing_commas,
	bom: flags.bom,
	non_finite: flags.non_finite,
	control_characters: flags.control_characters,
	lone_surrogates: flags.lone_surrogates,
	duplicates: flags.duplicates,
	top_level: flags.top_level,
	interoperable_numbers: flags.interoperable_numbers,
}

run : List(U8), Flags -> Str
run = |bytes, flags| {
	options = options_of(flags)
	match Str.from_utf8(bytes) {
		Ok(input) => {
			combinator = Utf8.parse_str(JsonValue.parser_with(options), input)
			match JsonValue.parse_with(input, options) {
				Ok(value) => {
					tree = encode(value)
					agrees =
						match combinator {
							Ok(other) => encode(other) == tree
							Err(_) => False
						}
					accepted(value, tree, flags, agrees)
				}
				Err(InvalidJson(error)) => {
					location = "\"line\":${error.line.to_str()},\"column\":${error.column.to_str()}"
					"{\"status\":\"error\",${location},\"message\":${json_string(error.message)},\"parser_agrees\":${boolean(combinator.is_err())}}"
				}
			}
		}
		Err(_) => {
			match Utf8.parse_bytes(JsonValue.parser_with(options), bytes) {
				Ok(value) => accepted(value, encode(value), flags, True)
				Err(ParseError({ message, offset })) => {
					at = position(bytes, offset)
					location = "\"line\":${at.line.to_str()},\"column\":${at.column.to_str()},\"offset\":${offset.to_str()}"
					"{\"status\":\"error\",${location},\"message\":${json_string(message)},\"parser_agrees\":true}"
				}
			}
		}
	}
}

accepted : JsonValue, Str, Flags, Bool -> Str
accepted = |value, tree, flags, agrees| {
	compact = written(JsonValue.to_str(value))
	pretty = written(JsonValue.to_str_with(value, { sort_keys: flags.sort_keys, indent: flags.indent }))
	sorted = written(JsonValue.to_str_with(value, { sort_keys: True }))
	"{\"status\":\"ok\",\"value\":${tree},\"compact\":${compact},\"pretty\":${pretty},\"sorted\":${sorted},\"parser_agrees\":${boolean(agrees)}}"
}

written : Try(Str, [InvalidNumber(Str), DuplicateKey(Str), TooDeep]) -> Str
written = |result| {
	match result {
		Ok(text) => json_string(text)
		Err(InvalidNumber(lexeme)) => "{\"error\":\"InvalidNumber\",\"value\":${json_string(lexeme)}}"
		Err(DuplicateKey(key)) => "{\"error\":\"DuplicateKey\",\"value\":${json_string(key)}}"
		Err(TooDeep) => "{\"error\":\"TooDeep\",\"value\":null}"
	}
}

encode : JsonValue -> Str
encode = |value| {
	match value {
		Null => "null"
		Bool(flag) => boolean(flag)
		Number(lexeme) => "[\"number\",${json_string(lexeme)}]"
		String(text) => json_string(text)
		Array(items) => "[\"array\",[${Str.join_with(items.map(encode), ",")}]]"
		Object(members) => "[\"object\",[${Str.join_with(members.map(|member| "[${json_string(member.key)},${encode(member.value)}]"), ",")}]]"
	}
}

boolean : Bool -> Str
boolean = |flag| if (flag) "true" else "false"

## One-based line and byte column of a byte offset, as Literal.position
## computes them, for input that is not a valid Str.
position : List(U8), U64 -> { line : U64, column : U64 }
position = |bytes, offset| {
	end = if (offset < bytes.len()) offset else bytes.len()
	var $index = 0
	var $line = 1
	var $column = 1
	while $index < end {
		byte = bytes.get($index) ?? 0
		if byte == '\r' {
			$line = $line + 1
			$column = 1
			if $index + 1 < end and bytes.get($index + 1) == Ok('\n') {
				$index = $index + 1
			}
		} else if byte == '\n' {
			$line = $line + 1
			$column = 1
		} else {
			$column = $column + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: $column }
}

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, out| {
	match bytes {
		[] => out
		['"', .. as rest] => escape_json(rest, out.concat(['\\', '"']))
		['\\', .. as rest] => escape_json(rest, out.concat(['\\', '\\']))
		[byte, .. as rest] if byte < 32 => {
			hi = if (byte < 16) '0' else '1'
			lo = if (byte < 16) byte else byte - 16
			digit = if (lo < 10) lo + '0' else lo - 10 + 'a'
			escape_json(rest, out.concat(['\\', 'u', '0', '0', hi, digit]))
		}
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}
