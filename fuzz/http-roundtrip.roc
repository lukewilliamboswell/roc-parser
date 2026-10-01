app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.HTTP
import parser.Utf8
import HttpGen

## Round-trip property: fuzzer bytes choose a valid HTTP/1.x request or
## response and how to write it (see fuzz/HttpGen.roc). The parser must return
## exactly the expected message. Metamorphic relations:
## - pipelining: bytes appended after a self-delimiting message are left
##   unconsumed and do not change the parse;
## - truncation: dropping the last byte of a self-delimiting message makes it
##   incomplete, so parsing must fail.
## Chunked and Content-Length bodies are generated from the same body bytes, so
## the property also checks that both framings decode to the same body.

Input : { bytes : List(U8), msg : HttpGen.Msg }

generate : List(U8) -> Input
generate = |bytes| {
	out = HttpGen.generate({ bytes, pos: 0 }, Either)
	{ bytes: HttpGen.encode(out.msg), msg: out.msg }
}

next_message : List(U8)
next_message = "GET /next HTTP/1.1\r\nHost: b\r\n\r\n".to_utf8()

test : Input -> Fuzz.Outcome
test = |input| {
	msg = input.msg
	bytes = input.bytes
	match msg.kind {
		Req(_) => {
			expected = HttpGen.expected_request(msg)
			check_request(bytes, Utf8.parse_utf8_partial(HTTP.request, bytes), expected, [])
			check_request(bytes, Utf8.parse_utf8_partial(HTTP.request, bytes.concat(next_message)), expected, next_message)
			check_truncated(bytes, Utf8.parse_utf8_partial(HTTP.request, bytes.drop_last(1)).is_ok())
		}
		Res(_) => {
			expected = HttpGen.expected_response(msg)
			check_response(bytes, Utf8.parse_utf8_partial(HTTP.response, bytes), expected, [])
			if HttpGen.self_delimiting(msg) {
				check_response(bytes, Utf8.parse_utf8_partial(HTTP.response, bytes.concat(next_message)), expected, next_message)
				check_truncated(bytes, Utf8.parse_utf8_partial(HTTP.response, bytes.drop_last(1)).is_ok())
			}
		}
	}
	Fuzz.keep
}

check_request : List(U8), Try({ value : HTTP.Request, rest : List(U8) }, [ParsingFailure(Str)]), Try(HTTP.Request, _), List(U8) -> {}
check_request = |bytes, actual, expected, rest| {
	match (actual, expected) {
		(Ok({ value: val, rest: input }), Ok(want)) if val == want and input == rest => {}
		_ => crash "request mismatch\n--- input ---\n${HttpGen.show_bytes(bytes)}\n--- appended ---\n${HttpGen.show_bytes(rest)}\n--- expected ---\n${Str.inspect(expected)}\n--- actual ---\n${Str.inspect(actual)}"
	}
}

check_response : List(U8), Try({ value : HTTP.Response, rest : List(U8) }, [ParsingFailure(Str)]), Try(HTTP.Response, _), List(U8) -> {}
check_response = |bytes, actual, expected, rest| {
	match (actual, expected) {
		(Ok({ value: val, rest: input }), Ok(want)) if val == want and input == rest => {}
		_ => crash "response mismatch\n--- input ---\n${HttpGen.show_bytes(bytes)}\n--- appended ---\n${HttpGen.show_bytes(rest)}\n--- expected ---\n${Str.inspect(expected)}\n--- actual ---\n${Str.inspect(actual)}"
	}
}

check_truncated : List(U8), Bool -> {}
check_truncated = |bytes, parsed| {
	if parsed {
		crash "a truncated message still parsed\n--- full input ---\n${HttpGen.show_bytes(bytes)}"
	}
}

target = Fuzz.target_with({
	name: "http-roundtrip",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| HttpGen.show_message(input.msg.kind, input.bytes),
})
