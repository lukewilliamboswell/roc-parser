app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.JsonValue
import parser.Parser
import parser.Markdown
import parser.Xml
import parser.Yaml

## Runtime validation preserves native diagnostics and named source context.
## Enabling lints can only reject an otherwise accepted value with a typed lint.
## Quotation positions agree with an independent line/column model.
name : Str
name = "quoted.src"

check_lints : Try(a, e), Try(a, e), (e -> Bool), Str -> {}
	where [a.is_eq : a, a -> Bool, e.is_eq : e, e -> Bool]
check_lints = |off, on, is_lint, format| {
	match (off, on) {
		(Ok(a), Ok(b)) => if a != b {
			crash "${format}: lints changed an accepted value"
		}
		(Ok(_), Err(problem)) => if !is_lint(problem) {
			crash "${format}: rejected without a typed lint: ${Str.inspect(problem)}"
		}
		(Err(a), Err(b)) => if a != b {
			crash "${format}: lints changed the native error"
		}
		(Err(_), Ok(_)) => crash "${format}: lints accepted an invalid source"
	}
}

check_xml : Str -> {}
check_xml = |source| {
	off = Xml.validate({ source, name, lints: { non_utf8_declaration: False } })
	expected = Xml.parse_str(source, {}).map_err(|InvalidXml(error)| InvalidXml({ ..error, path: error.path.append(Source(name)) }))
	if off != expected {
		crash "XML: validation changed syntax value or diagnostic"
	}
	check_lints(
		off,
		Xml.validate({ source, name }),
		|InvalidXml(error)| match error.problem {
			NonUtf8Encoding(_) => True
			_ => False
		},
		"XML",
	)
}

check_yaml : Str -> {}
check_yaml = |source| {
	off = Yaml.validate({ source, name, lints: { legacy_booleans: False } })
	expected = Yaml.parse_str(source, {}).map_err(|InvalidYaml(error)| InvalidYaml({ ..error, path: error.path.append(Source(name)) }))
	if off != expected {
		crash "YAML: validation changed syntax value or diagnostic: ${Str.inspect({ off, expected })}"
	}
	check_lints(
		off,
		Yaml.validate({ source, name }),
		|InvalidYaml(error)| match error.problem {
			LegacyBoolean(_) => True
			_ => False
		},
		"YAML",
	)
}

check_json : Str -> {}
check_json = |source| {
	checked = JsonValue.validate({ source, name })
	expected = JsonValue.parse_str(source, {}).map_err(|InvalidJsonValue(error)| InvalidJsonValue({ ..error, path: error.path.append(Source(name)) }))
	if checked != expected {
		crash "JSON: validation changed syntax value or diagnostic"
	}
}

check_csv : Str -> {}
check_csv = |source| {
	for header in [Absent, Exact, Normalized] {
		off = CSV.validate({ source, name, header, lints: { duplicate_headers: False, empty_headers: False } })
		on = CSV.validate({ source, name, header })
		expected = CSV.parse_str(source, {}).map_ok(|document| document.records).map_err(|InvalidCsv(error)| InvalidCsv({ ..error, path: error.path.append(Source(name)) }))
		if off != expected {
			crash "CSV: validation changed raw records or diagnostic"
		}
		check_lints(
			off,
			on,
			|InvalidCsv(error)| match error.problem {
				DuplicateHeader(_) | EmptyHeader => True
				_ => False
			},
			"CSV",
		)
		if header != Absent and off.is_ok() != CSV.validate({ source, name }).is_ok() {
			crash "CSV: header mode changed well-formedness"
		}
	}
}

check_markdown : Str -> {}
check_markdown = |source| {
	off = Markdown.validate({ source, name, lints: { unclosed_fences: False } })
	if off != Ok(Markdown.parse_str(source, {})) {
		crash "Markdown: validation without lints changed syntax value"
	}
	check_lints(off, Markdown.validate({ source, name }), |InvalidMarkdown(error)| error.problem == UnclosedFence, "Markdown")
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
					if Parser.Quotation.position(source, at) != position_of(rest, at) {
						crash "Parser.Quotation.position(${at.to_str()}) is ${Str.inspect(Parser.Quotation.position(source, at))}, expected ${Str.inspect(position_of(rest, at))}"
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
