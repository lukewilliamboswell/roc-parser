app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.CSV

## Reads CSV text on stdin and prints one JSON line describing the parse
## through the public CSV.parse_records API:
## {"status":"ok","records":[["field",...],...]} or {"status":"error",...}.
main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"invalid_utf8\"}")?
		Ok(input) => {
			match CSV.parse_records(input) {
				Ok(records) => {
					rows = records.map(|fields| "[${fields.map(json_bytes) |> Str.join_with(",")}]") |> Str.join_with(",")
					Stdout.line!("{\"status\":\"ok\",\"records\":[${rows}]}")?
				}
				Err(InvalidCsv({ record, field, line, column, message })) => {
					location = "\"record\":${record.to_str()},\"field\":${field.to_str()},\"line\":${line.to_str()},\"column\":${column.to_str()}"
					Stdout.line!("{\"status\":\"error\",\"message\":${json_bytes(message.to_utf8())},${location}}")?
				}
			}
		}
	}
	Ok({})
}

json_bytes : List(U8) -> Str
json_bytes = |bytes| "\"${Str.from_utf8_lossy(escape_json(bytes, []))}\""

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
