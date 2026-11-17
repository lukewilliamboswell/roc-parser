app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.CSV
import parser.JsonValue
import parser.Parser
import parser.QIF
import parser.OFX
import parser.Yaml

identifier : Str
identifier = "an_identifier_beyond_short_string_storage"

## Keep only the diagnostic's authored payload after dropping a large source.
## Check native tag identity before the measurement crosses the host boundary.
observe : U8, U64 -> List(Str)
observe = |mode, size| {
	key = identifier
	padding = Str.repeat(" ", size)
	match mode {
		0 | 1 | 7 | 8 => {
			source = "${padding}{\"${key}\":0,\"${key}\":1}"
			parsed = if (mode == 8) JsonValue.record_decoder({}).collect([source.to_utf8()].iter()).map_ok(|_| Null) else JsonValue.parse_str(source, { duplicates: if (mode == 1) CollapseIdentical else Reject })
			result = if mode == 7 {
				document = JsonValue.parse_str("${padding}{\"${key}\":0}", {}) ?? crash "valid diagnostic scaffold rejected"
				members = document.as_object() ?? crash "expected object scaffold"
				member = members.first() ?? crash "expected object member"
				JsonValue.encode(Object([member, member]), {}).map_ok(|_| Null)
			} else parsed
			match result {
				Err(InvalidJsonValue(error)) => match error.problem {
					DuplicateKey(text) if mode != 1 and text == key => [text]
					ConflictingDuplicateKey(text) if mode == 1 and text == key => [text]
					_ => crash "JSON diagnostic tag or key changed"
				}
				_ => crash "expected a native JSON diagnostic"
			}
		}
		2 => {
			text = "123456789012345678901234567890123456789"
			match JsonValue.parse_str("${padding}${text}", { interoperable_numbers: True }) {
				Err(InvalidJsonValue({ problem: NonInteroperableNumber({ text: actual, reason: UnsafeInteger }), .. })) if actual == text => [actual]
				_ => crash "numeric diagnostic identity changed"
			}
		}
		3 => match Yaml.parse_str("#${padding}\n${key}: 0\n${key}: 1", {}) {
			Err(InvalidYaml({ problem: DuplicateKey(text), .. })) if text == key => [text]
			_ => crash "YAML duplicate diagnostic changed"
		}
		4 | 9 => {
			source = "#${padding}\ncount: \"${key}\""
			result : Try({ count : U64 }, [InvalidYaml(Yaml.Error)])
			result = Yaml.decode_str(source, {})
			if mode == 9 {
				parser : Parser(Parser.Utf8.Bytes, { count : U64 }, [InvalidYaml(Yaml.Error)])
				parser = Yaml.value_parser({})
				match parser.parse_str(source) {
					Err(ParseError({ problem: InvalidYaml({ problem: TypeMismatch({ expected: IntegerValue, actual: QuotedScalar(text) }), .. }), .. })) if text == key => [text]
					_ => crash "typed parser diagnostic identity changed"
				}
			} else match result {
				Err(InvalidYaml({ problem: TypeMismatch({ expected: IntegerValue, actual: QuotedScalar(text) }), .. })) if text == key => [text]
				_ => crash "typed YAML diagnostic changed"
			}
		}
		5 => {
			result : Try({ tag : [Known] }, [InvalidYaml(Yaml.Error)])
			result = Yaml.decode_str("#${padding}\ntag: ${key}", {})
			match result {
				Err(InvalidYaml({ problem: UnknownTag(text), .. })) if text == key => [text]
				_ => crash "unknown YAML tag diagnostic changed"
			}
		}
		6 => match CSV.validate({ source: "${key},${key}\n${padding}", name: "lifetimes.csv", header: Exact }) {
			Err(InvalidCsv({ problem: DuplicateHeader(text), path, .. })) if text == key and path == [Record(1), Field(2), Source("lifetimes.csv")] => [text]
			_ => crash "CSV header diagnostic changed"
		}
		11 => {
			name = "UNSUPPORTED_IDENTIFIER_BEYOND_SHORT_STORAGE"
			document = QIF.parse_str("!Comment${padding}\n!Type:${name}\nPtest\n^\n", { max_context_bytes: 524288 }) ?? crash "valid raw QIF scaffold rejected"
			match document.transactions({}) {
				Err(InvalidQif({ problem: UnsupportedSection(text), .. })) if text == name => [text]
				_ => crash "QIF section diagnostic changed"
			}
		}
		12 | 13 => {
			name = "UNSUPPORTED_IDENTIFIER_BEYOND_SHORT_STORAGE"
			source = if (mode == 12) "\n${padding}\nOFXHEADER:${name}\n<OFX/>" else "<OFX><${name} ${padding}/></OFX>"
			match OFX.parse_str(source, { max_token_bytes: 524288 }) {
				Err(InvalidOfx({ problem: UnsupportedHeader(text), .. })) if mode == 12 and text == name => [text]
				Err(InvalidOfx({ problem: UnsupportedElement(text), .. })) if mode == 13 and text == name => [text]
				_ => crash "OFX native diagnostic identity changed"
			}
		}

		_ => match Yaml.parser({}).parse_str("#${padding}\n${key}: 0\n${key}: 1") {
			Err(ParseError({ problem: DuplicateKey(text), .. })) if text == key => [text]
			_ => crash "YAML parser diagnostic identity changed"
		}
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	mode = (bytes.first() ?? 0) % 14
	size = [0.U64, 64, 4096, 65536, 262144].get((bytes.last() ?? 0).to_u64() % 5) ?? 0
	measured = Fuzz.measure!("native-diagnostic-payload", || observe(mode, size))
	if measured.stats.exit_live_bytes > measured.stats.entry_live_bytes + 8192 {
		crash "a small diagnostic payload retained its discarded source"
	}
	expected = if mode >= 11 {
		"UNSUPPORTED_IDENTIFIER_BEYOND_SHORT_STORAGE"
	} else if (mode == 2) "123456789012345678901234567890123456789" else identifier
	if measured.value != [expected] {
		crash "retained diagnostic source text changed"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "diagnostic-lifetimes", examples: [0.U8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13].map(|mode| Fuzz.example({ name: "large-source-${mode.to_str()}", revision: "1", make: || [mode, 4.U8] })), generator: Fuzz.raw_bytes, test!, show: Str.inspect })
