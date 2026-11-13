app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.JsonValue
import parser.Literal
import parser.Markdown
import parser.Xml
import parser.Yaml

## Checked conversion (`validate`, which `from_quote` and quoted values use)
## on arbitrary UTF-8. The first byte chooses XML, YAML, JSON, CSV or Markdown
## and the second a byte offset; the rest is the source.
##
## - With every lint off, `validate` accepts exactly what `parse_str`
##   accepts, with the same value, and its error is `parse_str`'s, written as
##   `name:line:column: FORMAT: message` by Literal.error.
## - Lints only reject: with the default lints, `validate` gives the same
##   result, or rejects a value it accepted with a message naming the lint.
## - Literal.position agrees with an independent line and column count.

name : Str
name = "quoted.src"

lint_names : List(Str)
lint_names = ["non_utf8_declaration", "legacy_booleans", "duplicate_headers", "empty_headers", "unclosed_fences"]

## The checked error for a native error with this location.
expected_error : Str, { line : U64, column : U64, message : Str } -> [BadQuotedBytes(Str)]
expected_error = |format, problem| Literal.error({ name, format, message: problem.message, line: problem.line, column: problem.column })

## Enabling lints may only turn an accepted value into a lint failure.
check_lints : Try(a, [BadQuotedBytes(Str)]), Try(a, [BadQuotedBytes(Str)]), Str -> {}
	where [a.is_eq : a, a -> Bool]
check_lints = |off, on, format| {
	match (off, on) {
		(Ok(a), Ok(b)) =>
			if a != b {
				crash "${format}: lints changed an accepted value"
			}

		(Ok(_), Err(BadQuotedBytes(message))) =>
		# CSV names the record and field before the lint.
			if !message.contains(": ${format}: ") or !lint_names.any(|lint| message.contains(": ${lint}: ")) {
				crash "${format}: an accepted value was rejected without naming a lint: ${message}"
			}

		(Err(a), Err(b)) =>
			if a != b {
				crash "${format}: lints changed the error from ${Str.inspect(a)} to ${Str.inspect(b)}"
			}

		(Err(_), Ok(_)) => crash "${format}: lints accepted a source rejected without them"
	}
}

check_xml : Str -> {}
check_xml = |source| {
	off = Xml.validate({ source, name, lints: { non_utf8_declaration: False } })
	expected = Xml.parse_str(source).map_err(|InvalidXml(problem)| expected_error("XML", problem))
	if off != expected {
		crash "XML: validate gives ${Str.inspect(off)}, parse_str ${Str.inspect(expected)}"
	}
	check_lints(off, Xml.validate({ source, name }), "XML")
}

check_yaml : Str -> {}
check_yaml = |source| {
	off = Yaml.validate({ source, name, lints: { legacy_booleans: False } })
	expected = Yaml.parse_str(source).map_err(|InvalidYaml(problem)| expected_error("YAML", problem))
	if off != expected {
		crash "YAML: validate gives ${Str.inspect(off)}, parse_str ${Str.inspect(expected)}"
	}
	check_lints(off, Yaml.validate({ source, name }), "YAML")
}

check_json : Str -> {}
check_json = |source| {
	checked = JsonValue.validate({ source, name })
	expected = JsonValue.parse_str(source).map_err(|InvalidJsonValue(problem)| expected_error("JSON", problem))
	if checked != expected {
		crash "JSON: validate gives ${Str.inspect(checked)}, parse_str ${Str.inspect(expected)}"
	}
}

check_csv : Str -> {}
check_csv = |source| {
	for header in [Absent, Exact, Normalized] {
		off = CSV.validate({ source, name, header, lints: { duplicate_headers: False, empty_headers: False } })
		on = CSV.validate({ source, name, header })
		check_lints(off, on, "CSV")
		if header != Absent and off.is_ok() != CSV.validate({ source, name }).is_ok() {
			crash "CSV: a header mode changed whether the records are well-formed"
		}
	}
}

check_markdown : Str -> {}
check_markdown = |source| {
	off = Markdown.validate({ source, name, lints: { unclosed_fences: False } })
	if off != Ok(Markdown.parse_str(source)) {
		crash "Markdown: validate without lints differs from parse_str"
	}
	check_lints(off, Markdown.validate({ source, name }), "Markdown")
}

## One-based line and byte column of an offset, clamped to the source, with
## CR LF, CR and LF each ending one line.
position_of : List(U8), U64 -> { line : U64, column : U64 }
position_of = |bytes, offset| {
	end = if (offset < bytes.len()) offset else bytes.len()
	var $line = 1
	var $line_start = 0
	var $index = 0
	while $index < end {
		ends_line =
			match bytes.get($index) {
				Ok('\n') => True
				Ok('\r') => bytes.get($index + 1) != Ok('\n') or $index + 1 >= end
				_ => False
			}
		if ends_line {
			$line = $line + 1
			$line_start = $index + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: end - $line_start + 1 }
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match bytes {
		[format, offset, .. as rest] =>
			match Str.from_utf8(rest) {
				Ok(source) => {
					match format % 5 {
						0 => check_xml(source)
						1 => check_yaml(source)
						2 => check_json(source)
						3 => check_csv(source)
						_ => check_markdown(source)
					}
					at = U8.to_u64(offset) * 17 % (rest.len() + 2)
					if Literal.position(source, at) != position_of(rest, at) {
						crash "Literal.position(${at.to_str()}) is ${Str.inspect(Literal.position(source, at))}, expected ${Str.inspect(position_of(rest, at))}"
					}
					Fuzz.keep
				}

				Err(_) => Fuzz.reject
			}

		_ => Fuzz.reject
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "validate",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect({ format: bytes.first(), offset: bytes.get(1), source: Str.from_utf8(bytes.drop_first(2)) }),
})
