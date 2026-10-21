app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdout
import parser.JsonValue
import parser.Literal
import parser.Parser
import parser.Utf8

# tag::document[]
sbom_text =
	\\{
	\\  "bomFormat": "CycloneDX",
	\\  "specVersion": "1.6",
	\\  "components": [
	\\    {
	\\      "bom-ref": "pkg:roc/parser@2.1.0",
	\\      "name": "roc-parser",
	\\      "version": "2.1.0",
	\\      "licenses": [{ "license": { "id": "UPL-1.0" } }]
	\\    },
	\\    {
	\\      "bom-ref": "pkg:roc/unicode@4.3.0",
	\\      "name": "unicode",
	\\      "version": "4.3.0",
	\\      "licenses": [{ "expression": "UPL-1.0 OR MIT" }]
	\\    }
	\\  ]
	\\}

# end::document[]

# tag::walk[]
## A component's licence: an SPDX id, or an SPDX expression.
licence : JsonValue -> Str
licence = |component| {
	match component.get_path(["licenses", "0", "license", "id"]) {
		Ok(String(id)) => id
		_ =>
			match component.get_path(["licenses", "0", "expression"]) {
				Ok(String(expression)) => expression
				_ => "no licence"
			}
	}
}

## One line per component of a CycloneDX document.
components : Str -> Try(List(Str), [InvalidJsonValue(JsonValue.Error), Missing, WrongType])
components = |text| {
	bom = JsonValue.parse_str(text)?
	found = bom.get("components")?
	list = found.as_list()?
	Ok(
		list.map(
			|component| {
				name = component.get("name") ?? Null
				version = component.get("version") ?? Null
				"${name.as_str() ?? "?"} ${version.as_str() ?? "?"}: ${licence(component)}"
			},
		),
	)
}

# end::walk[]

# tag::match[]
## Every string in a value, with the keys and indexes that lead to it.
strings : JsonValue, Str -> List(Str)
strings = |value, path| {
	match value {
		String(text) => ["${path} = ${text}"]
		Array(items) => {
			var $found = []
			var $index = 0.U64
			for item in items {
				$found = $found.concat(strings(item, "${path}/${$index.to_str()}"))
				$index = $index + 1
			}
			$found
		}

		Object(members) => {
			var $found = []
			for member in members {
				$found = $found.concat(strings(member.value, "${path}/${member.key}"))
			}
			$found
		}

		Null | Bool(_) | Number(_) => []
	}
}

# end::match[]

# tag::errors[]
report : Str -> Str
report = |text| {
	match JsonValue.parse_str(text) {
		Ok(_) => "valid"
		Err(InvalidJsonValue({ line, column, message })) => "${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::errors[]

# tag::numbers[]
## How each accessor reads a number's text.
convert : JsonValue -> Str
convert = |value| {
	text =
		match value {
			Number(lexeme) => lexeme
			_ => "not a number"
		}
	"${text}: as_i64 ${Str.inspect(value.as_i64())}, as_u64 ${Str.inspect(value.as_u64())}, as_dec ${Str.inspect(value.as_dec())}"
}

# end::numbers[]

# tag::duplicates[]
## Read `text` with each duplicate mode, and write what each one kept.
modes : Str -> List(Str)
modes = |text| {
	options : List((Str, JsonValue.Options))
	options = [
		("Reject", {}),
		("CollapseIdentical", { duplicates: CollapseIdentical }),
		("KeepFirst", { duplicates: KeepFirst }),
		("KeepLast", { duplicates: KeepLast }),
	]
	options.map(
		|(name, chosen)| {
			match JsonValue.parse_with(text, chosen) {
				Ok(value) => "${name}: ${JsonValue.to_str(value) ?? "?"}"
				Err(InvalidJsonValue({ line, column, message })) => "${name}: ${line.to_str()}:${column.to_str()}: ${message}"
			}
		},
	)
}

# end::duplicates[]

# tag::jsonc[]
tsconfig_text =
	\\{
	\\  // Compiler options for the example project.
	\\  "compilerOptions": {
	\\    "target": "ES2022", /* modern output */
	\\    "strict": true,
	\\  },
	\\  "include": ["src/**/*.ts",],
	\\}

## JSON with comments, as in `tsconfig.json` and editor settings.
read_jsonc : Str -> Try(JsonValue, [InvalidJsonValue(JsonValue.Error)])
read_jsonc = |text| JsonValue.parse_with(text, { comments: True, trailing_commas: True })

# end::jsonc[]

# tag::bom[]
## A configuration file that may start with a byte order mark, as files
## saved by some Windows editors do, and must hold an object.
read_config : Str -> Try(JsonValue, [InvalidJsonValue(JsonValue.Error)])
read_config = |text| JsonValue.parse_with(text, { bom: True, top_level: Object })

# end::bom[]

# tag::non-finite[]
## Read metrics written by Python's `json.dumps`, which writes `NaN` and
## `Infinity` for floats that JSON cannot express.
print_loss! : Str => Try({}, _)
print_loss! = |text| {
	match JsonValue.parse_with(text, { non_finite: True }) {
		Ok(metrics) => {
			loss = metrics.get("loss") ?? Null
			Stdout.line!(Str.inspect(loss))?
			Stdout.line!(Str.inspect(loss.as_dec()))?
			Stdout.line!(Str.inspect(JsonValue.to_str(metrics)))?
		}

		Err(_) => Stdout.line!("invalid JSON")?
	}
	Ok({})
}

# end::non-finite[]

# tag::limits[]
## Options for documents from an untrusted source.
untrusted : JsonValue.Options
untrusted = {
	max_depth: 32,
	max_items: 1000,
	max_string_bytes: 4096,
	max_number_digits: 40,
	top_level: Object,
	interoperable_numbers: True,
}

# end::limits[]

# tag::write[]
release : JsonValue
release = Object([
	{ key: "name", value: String("roc-parser \"JSON\" café") },
	{ key: "version", value: String("2.2.0") },
	{ key: "sizes", value: Array([Number("1.50"), Number("2E3")]) },
	{ key: "draft", value: Bool(False) },
	{ key: "assets", value: Object([]) },
])

# end::write[]

# tag::quoted[]
lock : JsonValue
lock =
	\\{"name": "app",
	\\ "checksum": 340282366920938463463374607431768211455,
	\\ "greeting": "caf\\u00e9 \\ud83d\\ude00"}

# end::quoted[]

# tag::imported[]
import "assets/json-guide-settings.jsonc" as settings_text : Str

settings : JsonValue
settings = Literal.require(
	JsonValue.validate({
		source: settings_text,
		name: "json-guide-settings.jsonc",
		options: { comments: True, trailing_commas: True },
	}),
)

# end::imported[]

# tag::compose[]
## Every value of a JSON Lines (newline-delimited JSON) log.
json_lines : Str -> Try(List(JsonValue), [ParseError({ message : Str, offset : U64 })])
json_lines = |text| Utf8.parse_str(Parser.many(JsonValue.parser), text)

# end::compose[]

print_lines! : List(Str) => Try({}, _)
print_lines! = |lines| {
	for line in lines {
		Stdout.line!(line)?
	}
	Ok({})
}

print_components! : Str => Try({}, _)
print_components! = |text| {
	match components(text) {
		Ok(lines) => print_lines!(lines)?
		Err(problem) => Stdout.line!("cannot read the SBOM: ${Str.inspect(problem)}")?
	}
	Ok({})
}

print_refs! : Str => Try({}, _)
print_refs! = |text| {
	match JsonValue.parse_str(text) {
		Ok(bom) => print_lines!(strings(bom, "").keep_if(|line| line.contains("bom-ref")))?
		Err(_) => Stdout.line!("invalid JSON")?
	}
	Ok({})
}

show! : Try(JsonValue, [InvalidJsonValue(JsonValue.Error)]) => Try({}, _)
show! = |parsed| {
	match parsed {
		Ok(value) => Stdout.line!(JsonValue.to_str(value) ?? "not writable")?
		Err(InvalidJsonValue({ line, column, message })) => Stdout.line!("${line.to_str()}:${column.to_str()}: ${message}")?
	}
	Ok({})
}

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	# tag::walk-run[]
	print_components!(sbom_text)?
	Stdout.line!(Str.inspect(JsonValue.parse_str(sbom_text).map_ok(JsonValue.keys)))?
	# end::walk-run[]

	# tag::match-run[]
	print_refs!(sbom_text)?
	# end::match-run[]

	# tag::errors-run[]
	Stdout.line!(report("{\n  \"name\": \"web\",\n  \"port\": 08080\n}"))?
	Stdout.line!(report("[\"café\", tru]"))?
	Stdout.line!(report("{\"name\": 'web'}"))?
	Stdout.line!(report("[1, 2,]"))?
	Stdout.line!(report("// settings\n{}"))?
	Stdout.line!(report("{\"a\": 1} {\"b\": 2}"))?
	Stdout.line!(report("\"\\ud800\""))?
	# end::errors-run[]

	# tag::numbers-run[]
	for text in ["42", "-0", "1.50", "1e3", "18446744073709551616", "1e400"] {
		Stdout.line!(convert(JsonValue.parse_str(text) ?? Null))?
	}
	Stdout.line!(Str.inspect(JsonValue.parse_str("1.50") == JsonValue.parse_str("1.5")))?
	Stdout.line!(Str.inspect(F64.from_str("1.50")))?
	# end::numbers-run[]

	# tag::duplicates-run[]
	print_lines!(modes("{\"id\": 1, \"name\": \"web\", \"id\": 2}"))?
	print_lines!(modes("{\"extension\": {\"a\": 1}, \"\\u0065xtension\": {\"a\": 1}}"))?
	# end::duplicates-run[]

	# tag::jsonc-run[]
	show!(read_jsonc(tsconfig_text))?
	show!(JsonValue.parse_str(tsconfig_text))?
	# end::jsonc-run[]

	# tag::bom-run[]
	show!(read_config("\u(FEFF){\"port\": 8080}"))?
	show!(JsonValue.parse_str("\u(FEFF){\"port\": 8080}"))?
	show!(read_config("[8080]"))?
	# end::bom-run[]

	# tag::non-finite-run[]
	print_loss!("{\"loss\": NaN, \"best\": -Infinity}")?
	# end::non-finite-run[]

	# tag::limits-run[]
	show!(JsonValue.parse_with("{\"ids\": [1, 2, 3]}", untrusted))?
	show!(JsonValue.parse_with("{\"id\": 9007199254740993}", untrusted))?
	show!(JsonValue.parse_with("{\"a\": \"${Str.repeat("x", 5000)}\"}", untrusted))?
	show!(JsonValue.parse_with("[1, 2, 3]", untrusted))?
	# end::limits-run[]

	# tag::write-run[]
	Stdout.line!(JsonValue.to_str(release) ?? "not writable")?
	Stdout.line!(JsonValue.to_str_with(release, { indent: 2, sort_keys: True }) ?? "not writable")?
	Stdout.line!(Str.inspect(JsonValue.to_str(Array([Number("0x10")]))))?
	Stdout.line!(Str.inspect(JsonValue.to_str(Object([{ key: "a", value: Null }, { key: "a", value: Null }]))))?
	# end::write-run[]

	# tag::compile-time-run[]
	Stdout.line!(Str.inspect(lock.get("checksum")))?
	Stdout.line!(Str.inspect(lock.get("greeting")))?
	Stdout.line!(Str.inspect(settings.get_path(["files.exclude", "**/.roc-parser-tmp"])))?
	Stdout.line!(Str.inspect(JsonValue.validate({ source: "{\"a\": [1,]}", name: "list.json" })))?
	# end::compile-time-run[]

	# tag::compose-run[]
	Stdout.line!(Str.inspect(json_lines("{\"event\": \"start\"}\n{\"event\": \"stop\"}\n")))?
	Stdout.line!(Str.inspect(json_lines("{\"event\": \"start\"}\n{\"event\": }\n")))?
	# end::compose-run[]

	Ok({})
}
