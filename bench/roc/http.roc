app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.HTTP
import parser.String
import Bench

## Benchmark driver: one message through HTTP.response when it starts with
## `HTTP/`, otherwise HTTP.request.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	if input.starts_with("HTTP/") {
		match String.parse_str(HTTP.response, input) {
			Ok(response) => Ok(response.headers.len() + response.body.len() + response.status_code.to_u64())
			Err(_) => Err(1)
		}
	} else {
		match String.parse_str(HTTP.request, input) {
			Ok(request) => Ok(request.headers.len() + request.body.len() + request.uri.count_utf8_bytes().to_u64())
			Err(_) => Err(1)
		}
	}
}
