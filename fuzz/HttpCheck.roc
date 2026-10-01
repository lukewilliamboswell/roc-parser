import parser.HTTP
import parser.Utf8

## Invariants for arbitrary input to the HTTP parsers, shared by the raw
## robustness targets. Crashes describe the violated property.
##
## Whenever a message parses:
## - the parser consumed a prefix and left the rest untouched, and parsing just
##   that prefix gives the same message with nothing left over;
## - every field name is a token, every value is free of CTLs (other than
##   HTAB) and of surrounding whitespace, the status code is 100-999;
## - re-serializing the parsed message canonically (single SP separators,
##   "name: value" fields, body re-framed as one chunk when chunked) parses
##   back to the same message;
## - a self-delimiting message parses identically with bytes appended, and
##   fails when its last byte is removed.
HttpCheck :: {}.{

	check_request : List(U8) -> {}
	check_request = |bytes| {
		embedded = Utf8.parse_bytes_partial(HTTP.request, bytes)
		direct = HTTP.parse_request(bytes)
		check_agree(bytes, embedded.map_ok(|r| r.rest), direct.map_ok(|r| r.rest))
		match embedded {
			Err(_) => {}
			Ok({ value: val, rest: rest }) => {
				consumed = consumed_prefix(bytes, rest)
				same_request(Utf8.parse_bytes_partial(HTTP.request, consumed), val, [], "re-parsing the consumed prefix", bytes)
				check_fields(val.headers, bytes)
				canonical = serialize_request(val)
				same_request(Utf8.parse_bytes_partial(HTTP.request, canonical), val, [], "re-parsing the canonical form ${show(canonical)}", bytes)
				junk = "GET / HTTP/1.1\r\n".to_utf8()
				same_request(Utf8.parse_bytes_partial(HTTP.request, consumed.concat(junk)), val, junk, "parsing with bytes appended", bytes)
				if Utf8.parse_bytes_partial(HTTP.request, consumed.drop_last(1)).is_ok() {
					crash "dropping the last byte of a request still parsed\n${show(bytes)}"
				}
			}
		}
	}

	check_response : List(U8) -> {}
	check_response = |bytes| {
		embedded = Utf8.parse_bytes_partial(HTTP.response, bytes)
		direct = HTTP.parse_response(bytes)
		check_agree(bytes, embedded.map_ok(|r| r.rest), direct.map_ok(|r| r.rest))
		match embedded {
			Err(_) => {}
			Ok({ value: val, rest: rest }) => {
				consumed = consumed_prefix(bytes, rest)
				same_response(Utf8.parse_bytes_partial(HTTP.response, consumed), val, [], "re-parsing the consumed prefix", bytes)
				check_fields(val.headers, bytes)
				if val.status_code < 100 or val.status_code > 999 {
					crash "status code ${val.status_code.to_str()} is not three digits\n${show(bytes)}"
				}
				if val.reason.to_utf8().any(|b| (b < 0x20 and b != '\t') or b == 0x7F) {
					crash "control character in reason phrase\n${show(bytes)}"
				}
				canonical = serialize_response(val)
				same_response(Utf8.parse_bytes_partial(HTTP.response, canonical), val, [], "re-parsing the canonical form ${show(canonical)}", bytes)
				if response_delimited(val) {
					junk = "HTTP/1.1 200 OK\r\n".to_utf8()
					same_response(Utf8.parse_bytes_partial(HTTP.response, consumed.concat(junk)), val, junk, "parsing with bytes appended", bytes)
					if Utf8.parse_bytes_partial(HTTP.response, consumed.drop_last(1)).is_ok() {
						crash "dropping the last byte of a response still parsed\n${show(bytes)}"
					}
				} else if !rest.is_empty() {
					crash "an unframed response left bytes unconsumed\n${show(bytes)}"
				}
			}
		}
	}

	show : List(U8) -> Str
	show = |bytes| {
		Str.from_utf8_lossy(bytes)
			.replace_each("\\", "\\\\")
			.replace_each("\t", "\\t")
			.replace_each("\r", "\\r")
			.replace_each("\n", "\\n\n")
	}
}

## HTTP.parse_request/parse_response and the embeddable parsers accept the
## same inputs, leave the same rest, and fail at the same offset, which lies
## within the input.
check_agree : List(U8), Try(List(U8), [ParseError({ message : Str, offset : U64 })]), Try(List(U8), [InvalidHttp(HTTP.Error)]) -> {}
check_agree = |bytes, embedded, direct| {
	agree =
		match (embedded, direct) {
			(Ok(a), Ok(b)) => a == b
			(Err(ParseError(a)), Err(InvalidHttp(b))) => a.offset == b.offset and b.offset <= bytes.len() and a.message.ends_with(b.message)
			_ => False
		}
	if !agree {
		crash "the parser and parse function disagree\n--- input ---\n${HttpCheck.show(bytes)}\n--- parser ---\n${Str.inspect(embedded)}\n--- function ---\n${Str.inspect(direct)}"
	}
}

consumed_prefix : List(U8), List(U8) -> List(U8)
consumed_prefix = |bytes, rest| {
	if rest.len() > bytes.len() or bytes.drop_first(bytes.len() - rest.len()) != rest {
		crash "the leftover input is not a suffix of the input\n${HttpCheck.show(bytes)}"
	}
	bytes.sublist({ start: 0, len: bytes.len() - rest.len() })
}

same_request : Try({ value : HTTP.Request, rest : List(U8) }, [ParseError({ message : Str, offset : U64 })]), HTTP.Request, List(U8), Str, List(U8) -> {}
same_request = |actual, expected, rest, what, original| {
	match actual {
		Ok({ value: val, rest: input }) if val == expected and input == rest => {}
		_ => crash "${what} changed the result\n--- original input ---\n${HttpCheck.show(original)}\n--- first parse ---\n${Str.inspect(expected)}\n--- now ---\n${Str.inspect(actual)}"
	}
}

same_response : Try({ value : HTTP.Response, rest : List(U8) }, [ParseError({ message : Str, offset : U64 })]), HTTP.Response, List(U8), Str, List(U8) -> {}
same_response = |actual, expected, rest, what, original| {
	match actual {
		Ok({ value: val, rest: input }) if val == expected and input == rest => {}
		_ => crash "${what} changed the result\n--- original input ---\n${HttpCheck.show(original)}\n--- first parse ---\n${Str.inspect(expected)}\n--- now ---\n${Str.inspect(actual)}"
	}
}

is_tchar : U8 -> Bool
is_tchar = |b| "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".to_utf8().contains(b)

check_fields : List(HTTP.Header), List(U8) -> {}
check_fields = |fields, bytes| {
	fields.fold({}, |_, { name, value }| {
		n = name.to_utf8()
		v = value.to_utf8()
		if n.is_empty() or !n.all(is_tchar) {
			crash "field name ${Str.inspect(name)} is not a token\n${HttpCheck.show(bytes)}"
		}
		if v.any(|b| (b < 0x20 and b != '\t') or b == 0x7F) {
			crash "field value ${Str.inspect(value)} contains a control character\n${HttpCheck.show(bytes)}"
		}
		first = v.first() ?? 'x'
		last = v.last() ?? 'x'
		if first == ' ' or first == '\t' or last == ' ' or last == '\t' {
			crash "field value ${Str.inspect(value)} kept surrounding whitespace\n${HttpCheck.show(bytes)}"
		}
	})
}

lower : Str -> List(U8)
lower = |text| text.to_utf8().map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)

has_field : List(HTTP.Header), Str -> Bool
has_field = |fields, wanted| fields.any(|{ name, value: _ }| lower(name) == wanted.to_utf8())

response_delimited : HTTP.Response -> Bool
response_delimited = |r| r.status_code < 200 or r.status_code == 204 or r.status_code == 304 or has_field(r.headers, "content-length") or has_field(r.headers, "transfer-encoding")

version_bytes : HTTP.Version -> Str
version_bytes = |v| "HTTP/${v.major.to_str()}.${v.minor.to_str()}"

method_name : HTTP.Method -> Str
method_name = |m| {
	match m {
		Options => "OPTIONS"
		Get => "GET"
		Post => "POST"
		Put => "PUT"
		Delete => "DELETE"
		Head => "HEAD"
		Trace => "TRACE"
		Connect => "CONNECT"
		Patch => "PATCH"
		Extension(name) => name
	}
}

serialize_fields : List(HTTP.Header) -> List(U8)
serialize_fields = |fields| fields.fold([], |acc, { name, value }| acc.concat("${name}: ${value}\r\n".to_utf8())).concat(['\r', '\n'])

## The body re-framed: one chunk (plus last-chunk) when chunked, else as is.
frame_body : List(HTTP.Header), List(U8) -> List(U8)
frame_body = |fields, body| {
	if has_field(fields, "transfer-encoding") {
		chunk = if body.is_empty() [] else hex(body.len()).concat(['\r', '\n']).concat(body).concat(['\r', '\n'])
		chunk.concat("0\r\n\r\n".to_utf8())
	} else {
		body
	}
}

hex : U64 -> List(U8)
hex = |n| {
	digits = "0123456789abcdef".to_utf8()
	var $out = []
	var $rest = n
	while $rest > 0 {
		$out = [digits.get($rest % 16) ?? '0'].concat($out)
		$rest = $rest // 16
	}
	if $out.is_empty() ['0'] else $out
}

serialize_request : HTTP.Request -> List(U8)
serialize_request = |r| {
	"${method_name(r.method)} ${r.target} ${version_bytes(r.version)}\r\n".to_utf8()
	.concat(serialize_fields(r.headers))
	.concat(frame_body(r.headers, r.body))
}

serialize_response : HTTP.Response -> List(U8)
serialize_response = |r| {
	no_body = r.status_code < 200 or r.status_code == 204 or r.status_code == 304
	"${version_bytes(r.version)} ${r.status_code.to_str()} ${r.reason}\r\n".to_utf8()
	.concat(serialize_fields(r.headers))
	.concat(if no_body [] else frame_body(r.headers, r.body))
}
