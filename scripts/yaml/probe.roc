app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Yaml
import parser.Utf8

main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"invalid_utf8\"}")?
		Ok(input) => {
			match Yaml.parse_str(input) {
				Ok(value) => Stdout.line!("{\"status\":\"ok\",\"value\":${encode(value)}}")?
				Err(YamlError(error)) =>
					Stdout.line!("{\"status\":\"error\",\"line\":${error.line.to_str()},\"column\":${error.column.to_str()},\"message\":${json_string(error.message)}}")?
			}
		}
	}
	Ok({})
}

encode : Yaml -> Str
encode = |value| {
	match value {
		Null => "[\"null\"]"
		Bool(boolean) => if boolean "[\"bool\",true]" else "[\"bool\",false]"
		Int(integer) => "[\"int\",${json_string(integer.to_str())}]"
		Float(float) => "[\"float\",${json_string(float.to_str())}]"
		String(text) => "[\"string\",${json_string(text)}]"
		Sequence(values) => "[\"sequence\",[${values.map(encode) |> Str.join_with(",")}]]"
		Mapping(entries) => "[\"mapping\",[${entries.map(|entry| "[[\"string\",${json_string(entry.key)}],${encode(entry.value)}]") |> Str.join_with(",")}]]"
	}
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
			hi = if byte < 16 '0' else '1'
			lo = if byte < 16 byte else byte - 16
			digit = if lo < 10 lo + '0' else lo - 10 + 'a'
			escape_json(rest, out.concat(['\\', 'u', '0', '0', hi, digit]))
		}
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}
