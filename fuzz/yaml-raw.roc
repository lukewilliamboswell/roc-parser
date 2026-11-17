app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Yaml
import parser.Parser

## Arbitrary UTF-8 must parse or fail cleanly, and:
## - errors point at a real one-based line and column with a message;
## - LF and CRLF line endings give the same result;
## - a leading "---" marker gives the same value, with errors one line lower.

Parsed : Try(Yaml, [InvalidYaml(Yaml.Error)])

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match Str.from_utf8(bytes) {
		Err(_) => Fuzz.reject
		Ok(input) => {
			result = Yaml.parse_str(input, {})
			check_location(bytes, result)
			_ = show(result)
			if !bytes.contains('\r') {
				check_crlf(input, result)
				check_document_start(bytes, input, result)
			}
			Fuzz.keep
		}
	}
}

## Line lengths in bytes; LF, CRLF and a lone CR each end a line.
line_lengths : List(U8) -> List(U64)
line_lengths = |bytes| {
	var $lengths = []
	var $current = 0
	var $previous_cr = False
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' and $previous_cr {
			# The CR already ended this line.
			$previous_cr = False
		} else if byte == '\n' or byte == '\r' {
			$lengths = $lengths.append($current)
			$current = 0
			$previous_cr = byte == '\r'
		} else {
			$current = $current + 1
			$previous_cr = False
		}
		$index = $index + 1
	}
	$lengths.append($current)
}

check_location : List(U8), Parsed -> {}
check_location = |bytes, result| {
	match result {
		Ok(_) => {}
		Err(InvalidYaml(error)) => {
			match error.location {
				Text(position) if position.offset <= bytes.len() => {}
				_ => crash "YAML source diagnostic must retain an original byte offset inside the input"
			}
			lengths = line_lengths(bytes)
			if source_position(error.location).line == 0 or source_position(error.location).column == 0 {
				crash "YAML error locations must be one-based: ${show(result)}"
			}
			match lengths.get(source_position(error.location).line - 1) {
				Err(_) => crash "error line ${source_position(error.location).line.to_str()} is past the last line (${lengths.len().to_str()}): ${show(result)}"
				Ok(length) =>
					if source_position(error.location).column > length + 1 {
						crash "error column ${source_position(error.location).column.to_str()} is past the end of line ${source_position(error.location).line.to_str()} (${length.to_str()} bytes): ${show(result)}"
					}
			}
		}
	}
}

check_crlf : Str, Parsed -> {}
check_crlf = |input, result| {
	crlf = Yaml.parse_str(Str.replace_each(input, "\n", "\r\n"), {})
	# Compare renderings: NaN floats are never equal to themselves.
	if show(crlf) != show(result) {
		crash "CRLF line endings changed the result\nLF:   ${show(result)}\nCRLF: ${show(crlf)}"
	}
}

## Skipped when the input already has a line starting with "---", where an
## extra marker would start a second document.
check_document_start : List(U8), Str, Parsed -> {}
check_document_start = |bytes, input, result| {
	# A byte order mark is only one at the very start of the input.
	has_marker = Str.starts_with(input, "---") or Str.contains(input, "\n---") or Str.starts_with(input, "\u(FEFF)")
	if !has_marker and !bytes.is_empty() {
		marked = Yaml.parse_str("---\n${input}", {})
		consistent =
			match (result, marked) {
				(Ok(plain), Ok(with_marker)) => Yaml.to_inspect(plain) == Yaml.to_inspect(with_marker)
				(Err(InvalidYaml(plain)), Err(InvalidYaml(with_marker))) => source_position(plain.location).line + 1 == source_position(with_marker.location).line and plain.problem == with_marker.problem
				_ => False
			}
		if !consistent {
			crash "a leading --- changed the result\nplain:  ${show(result)}\nmarked: ${show(marked)}"
		}
	}
}

show : Parsed -> Str
show = |result| {
	match result {
		Ok(value) => Yaml.to_inspect(value)
		Err(InvalidYaml(error)) => "error ${source_position(error.location).line.to_str()}:${source_position(error.location).column.to_str()} ${Str.inspect(error.problem)}"
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "yaml-raw",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(position) => { line: position.line, column: position.column }
	_ => crash "expected retained YAML source text coordinates"
}
