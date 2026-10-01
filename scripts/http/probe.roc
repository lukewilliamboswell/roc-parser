app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.HTTP

## Reads one message from stdin and prints the parser's verdict as JSON.
## The first byte selects the parser: `Q` for a request, `S` for a response.
## The rest of stdin goes to the parser as raw bytes (not via Str), so
## non-UTF-8 input reaches it exactly as a network peer would send it.
## `rest` is the input left after one message (pipelined data).
main! : List(OsStr) => Try({}, _)
main! = |_| {
	all = Stdin.read_to_end!()?
	bytes = all.drop_first(1)
	line =
		match all.first() {
			Ok('Q') => {
				match HTTP.parse_request(bytes) {
					Ok({ request: val, rest: input }) =>
						"{\"status\":\"ok\",\"method\":${json_string(method_name(val.method))},\"target\":${json_string(val.target)},${version(val.version)},${headers(val.headers)},\"body\":${hex(val.body)},\"rest\":${hex(input)}}"
					Err(InvalidHttp({ message, offset })) => error(message, offset)
				}
			}
			Ok('S') => {
				match HTTP.parse_response(bytes) {
					Ok({ response: val, rest: input }) =>
						"{\"status\":\"ok\",\"code\":${val.status_code.to_str()},\"reason\":${json_string(val.reason)},${version(val.version)},${headers(val.headers)},\"body\":${hex(val.body)},\"rest\":${hex(input)}}"
					Err(InvalidHttp({ message, offset })) => error(message, offset)
				}
			}
			_ => "{\"status\":\"bad_mode\"}"
		}
	Stdout.line!(line)?
	Ok({})
}

error : Str, U64 -> Str
error = |message, offset| "{\"status\":\"error\",\"message\":${json_string(message)},\"offset\":${offset.to_str()}}"

## The tag name for a standard method (as h11's method table in
## review_http.py spells it), or the token itself for an extension method.
method_name : HTTP.Method -> Str
method_name = |method| {
	match method {
		Extension(name) => name
		_ => Str.inspect(method)
	}
}

version : HTTP.Version -> Str
version = |v| "\"version\":\"${v.major.to_str()}.${v.minor.to_str()}\""

headers : List(HTTP.Header) -> Str
headers = |fields| {
	encoded = fields.map(|{ name, value }| "[${json_string(name)},${json_string(value)}]")
	"\"headers\":[${Str.join_with(encoded, ",")}]"
}

hex : List(U8) -> Str
hex = |bytes| {
	digits = bytes.fold([], |acc, byte| acc.append(hex_digit(byte // 16)).append(hex_digit(byte % 16)))
	"\"${Str.from_utf8_lossy(digits)}\""
}

hex_digit : U8 -> U8
hex_digit = |n| if n < 10 n + '0' else n - 10 + 'a'

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, out| {
	match bytes {
		[] => out
		['"', .. as rest] => escape_json(rest, out.concat(['\\', '"']))
		['\\', .. as rest] => escape_json(rest, out.concat(['\\', '\\']))
		[byte, .. as rest] if byte < 32 => escape_json(rest, out.concat(['\\', 'u', '0', '0', hex_digit(byte // 16), hex_digit(byte % 16)]))
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}
