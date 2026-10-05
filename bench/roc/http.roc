app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import parser.HTTP
import Bench

## Benchmark driver: one message through HTTP.parse_response when it starts
## with `HTTP/`, otherwise HTTP.parse_request; bytes left over are an error.
main! : List(OsStr) => Try({}, _)
main! = |args| Bench.run!(args, parse)

parse : Str -> Try(U64, U64)
parse = |input| {
	if input.starts_with("HTTP/") {
		match HTTP.parse_response(input.to_utf8()) {
			Ok({ response, rest: [] }) => Ok(response.headers.len() + response.body.len() + response.status_code.to_u64())
			_ => Err(1)
		}
	} else {
		match HTTP.parse_request(input.to_utf8()) {
			Ok({ request, rest: [] }) => Ok(request.headers.len() + request.body.len() + request.target.count_utf8_bytes().to_u64())
			_ => Err(1)
		}
	}
}
