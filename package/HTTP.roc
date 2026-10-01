import Parser
import Utf8

## Parsers and syntax types for HTTP/1.x requests and responses, following the
## message syntax of RFC 9112 and the field syntax of RFC 9110.
##
## Each parser consumes exactly one message and leaves any following bytes
## (a pipelined message) unconsumed, so `Utf8.parse_str` rejects trailing
## data while `Utf8.parse_utf8_partial` returns it.
##
## Message framing (RFC 9112 section 6.3):
## - `Transfer-Encoding: chunked` bodies are decoded; chunk extensions are
##   validated and discarded, and trailer fields are validated and discarded.
## - `Content-Length` bodies are exactly that many bytes.
## - A request with neither has no body. A response with neither runs to the
##   end of the input (the connection close). Responses with a 1xx, 204 or 304
##   status never have a body. Responses to HEAD and 2xx responses to CONNECT
##   also have no body, but this parser cannot see the request: parse those
##   heads with a status that implies no body or strip the framing fields.
##
## Messages whose framing is ambiguous are rejected rather than guessed at,
## because a parser that disagrees with a proxy about where a message ends is
## open to request smuggling: both `Transfer-Encoding` and `Content-Length`,
## conflicting or malformed `Content-Length` values, any transfer coding other
## than a single `chunked`, `Transfer-Encoding` in an HTTP/1.0 message, line
## folding (obs-fold), whitespace between a field name and its colon, control
## characters in field values, and line endings other than CRLF.
##
## Requests must use one of the methods in `Method` (method names are
## case-sensitive), and HTTP/1.1 requests must have exactly one `Host` field.
## Field names keep their case; field values have surrounding whitespace
## removed. Field values and reason phrases must be valid UTF-8 to be
## represented as `Str`; other obs-text bytes are rejected.
HTTP :: {}.{

	## Supported HTTP request methods.
	##
	## Method names are matched case-sensitively, so `get` is rejected; any
	## method not listed here is also rejected.
	Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

	## An HTTP protocol version such as `HTTP/1.1`, as `{ major: 1, minor: 1 }`.
	##
	## Each part is a single digit, as RFC 9112 requires.
	HttpVersion : { major : U8, minor : U8 }

	## One header field: the name as written (case preserved) and the value
	## with surrounding whitespace removed.
	##
	## Fields keep their order in the message, and repeated fields are kept.
	Header : [Header(Str, Str)]

	## A parsed HTTP request message.
	##
	## `uri` is the request-target exactly as written (it is not decoded or
	## normalized). `body` is the message body after any chunked transfer
	## coding has been removed.
	Request : {
		method : Method,
		uri : Str,
		http_version : HttpVersion,
		headers : List(Header),
		body : List(U8),
	}

	## A parsed HTTP response message.
	##
	## `status_code` is the three-digit status code and `status` the reason
	## phrase, which may be empty. `body` is decoded like `Request.body`.
	Response : {
		http_version : HttpVersion,
		status_code : U16,
		status : Str,
		headers : List(Header),
		body : List(U8),
	}

	## Parse one HTTP request: request line, header fields, and the body its
	## framing describes.
	##
	## Failure messages start with `invalid HTTP request:`.
	##
	## ```roc
	## expect {
	##     text = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n"
	##     match Utf8.parse_str(HTTP.request, text) {
	##         Ok(req) => req.method == Get and req.uri == "/hello" and req.headers == [Header("Host", "example.com")]
	##         Err(_) => Bool.False
	##     }
	## }
	## ```
	request : Parser(Utf8.Bytes, Request)
	request =
		Parser.build_primitive_parser(
			|input| {
				parse_request(input).map_err(|message| ParsingFailure("invalid HTTP request: ${message}"))
			},
		)

	## Parse one HTTP response: status line, header fields, and the body its
	## framing describes.
	##
	## Failure messages start with `invalid HTTP response:`. A response with no
	## `Content-Length` or `Transfer-Encoding` takes the rest of the input as
	## its body.
	response : Parser(Utf8.Bytes, Response)
	response =
		Parser.build_primitive_parser(
			|input| {
				parse_response(input).map_err(|message| ParsingFailure("invalid HTTP response: ${message}"))
			},
		)
}

Bytes : List(U8)

Framing : [Length(U64), Chunked, Unframed]

parse_request : Bytes -> Try({ value : HTTP.Request, rest : Bytes }, Str)
parse_request = |input| {
	{ line, rest } = take_line(input)?
	start = parse_request_line(line)?
	head = parse_fields(rest, [])?
	version = start.http_version
	hosts = values_named(head.fields, "host")
	if hosts.len() > 1 {
		Err("more than one Host field")
	} else if hosts.is_empty() and at_least_1_1(version) {
		Err("an HTTP/1.1 request needs a Host field")
	} else {
		framed =
			match framing(head.fields, version)? {
				Unframed => take_body(Length(0), head.rest)?
				other => take_body(other, head.rest)?
			}
		Ok({ value: { method: start.method, uri: start.uri, http_version: version, headers: head.fields, body: framed.body }, rest: framed.rest })
	}
}

parse_response : Bytes -> Try({ value : HTTP.Response, rest : Bytes }, Str)
parse_response = |input| {
	{ line, rest } = take_line(input)?
	start = parse_status_line(line)?
	head = parse_fields(rest, [])?
	version = start.http_version
	body_framing = framing(head.fields, version)?
	code = start.status_code
	framed =
		if code < 200 or code == 204 or code == 304 {
			# RFC 9112 section 6.3 item 1: these never have content.
			{ body: [], rest: head.rest }
		} else {
			take_body(body_framing, head.rest)?
		}
	Ok({ value: { http_version: version, status_code: code, status: start.status, headers: head.fields, body: framed.body }, rest: framed.rest })
}

at_least_1_1 : HTTP.HttpVersion -> Bool
at_least_1_1 = |version| version.major > 1 or (version.major == 1 and version.minor >= 1)

## Split off one line ending in CRLF. A CR or LF anywhere else is an error
## (RFC 9112 section 2.2: bare CR must be rejected; this parser also declines
## the optional leniency for bare LF).
take_line : Bytes -> Try({ line : Bytes, rest : Bytes }, Str)
take_line = |bytes| {
	var $index = 0
	var $result = Err("message ended before CRLF")
	var $searching = Bool.True
	while $searching and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\r' {
			$result =
				if bytes.get($index + 1) == Ok('\n') {
					Ok({ line: bytes.sublist({ start: 0, len: $index }), rest: bytes.drop_first($index + 2) })
				} else {
					Err("CR not followed by LF")
				}
			$searching = Bool.False
		} else if byte == '\n' {
			$result = Err("LF not preceded by CR")
			$searching = Bool.False
		}
		$index = $index + 1
	}
	$result
}

## request-line = method SP request-target SP HTTP-version
parse_request_line : Bytes -> Try({ method : HTTP.Method, uri : Str, http_version : HTTP.HttpVersion }, Str)
parse_request_line = |line| {
	method_len = prefix_len(line, is_tchar)
	method = to_method(line.sublist({ start: 0, len: method_len }))?
	after_method = line.drop_first(method_len)
	match after_method {
		[' ', .. as target_onwards] => {
			target_len = prefix_len(target_onwards, is_vchar)
			if target_len == 0 {
				Err("expected a request target after the method")
			} else {
				match target_onwards.drop_first(target_len) {
					[' ', .. as version_bytes] => {
						http_version = parse_version(version_bytes)?
						uri = Str.from_utf8(target_onwards.sublist({ start: 0, len: target_len })) ?? ""
						Ok({ method, uri, http_version })
					}
					_ => Err("request target must be visible ASCII followed by a single SP")
				}
			}
		}
		_ => Err("method must be followed by a single SP")
	}
}

to_method : Bytes -> Try(HTTP.Method, Str)
to_method = |bytes| {
	match bytes {
		['G', 'E', 'T'] => Ok(Get)
		['P', 'O', 'S', 'T'] => Ok(Post)
		['P', 'U', 'T'] => Ok(Put)
		['D', 'E', 'L', 'E', 'T', 'E'] => Ok(Delete)
		['H', 'E', 'A', 'D'] => Ok(Head)
		['O', 'P', 'T', 'I', 'O', 'N', 'S'] => Ok(Options)
		['T', 'R', 'A', 'C', 'E'] => Ok(Trace)
		['C', 'O', 'N', 'N', 'E', 'C', 'T'] => Ok(Connect)
		['P', 'A', 'T', 'C', 'H'] => Ok(Patch)
		[] => Err("expected a method token")
		_ => Err("unsupported method")
	}
}

## HTTP-version = "HTTP" "/" DIGIT "." DIGIT
parse_version : Bytes -> Try(HTTP.HttpVersion, Str)
parse_version = |bytes| {
	match bytes {
		['H', 'T', 'T', 'P', '/', major, '.', minor] if is_digit(major) and is_digit(minor) =>
			Ok({ major: major - '0', minor: minor - '0' })
		_ => Err("expected an HTTP version like HTTP/1.1")
	}
}

## status-line = HTTP-version SP status-code SP [ reason-phrase ]
##
## Like h11 and llhttp, the SP before an absent reason phrase is optional.
parse_status_line : Bytes -> Try({ http_version : HTTP.HttpVersion, status_code : U16, status : Str }, Str)
parse_status_line = |line| {
	http_version = parse_version(line.sublist({ start: 0, len: 8 }))?
	match line.drop_first(8) {
		[' ', a, b, c, .. as after_code] if is_digit(a) and is_digit(b) and is_digit(c) => {
			status_code = (a - '0').to_u16() * 100 + (b - '0').to_u16() * 10 + (c - '0').to_u16()
			reason =
				match after_code {
					[] => Ok([])
					[' ', .. as phrase] => Ok(phrase)
					_ => Err("status code must be exactly three digits")
				}?
			if status_code < 100 {
				Err("status code must be at least 100")
			} else if !reason.all(|byte| byte == '\t' or byte == ' ' or is_vchar(byte) or byte >= 0x80) {
				Err("control character in reason phrase")
			} else {
				match Str.from_utf8(reason) {
					Ok(status) => Ok({ http_version, status_code, status })
					Err(_) => Err("reason phrase is not valid UTF-8")
				}
			}
		}
		_ => Err("expected SP and a three-digit status code after the version")
	}
}

## Field lines up to and including the empty line that ends them.
parse_fields : Bytes, List(HTTP.Header) -> Try({ fields : List(HTTP.Header), rest : Bytes }, Str)
parse_fields = |bytes, fields| {
	{ line, rest } = take_line(bytes)?
	if line.is_empty() {
		Ok({ fields, rest })
	} else {
		field = parse_field_line(line)?
		parse_fields(rest, fields.append(field))
	}
}

## field-line = field-name ":" OWS field-value OWS
parse_field_line : Bytes -> Try(HTTP.Header, Str)
parse_field_line = |line| {
	name_len = prefix_len(line, is_tchar)
	match line.drop_first(name_len) {
		[' ', ..] | ['\t', ..] if name_len == 0 => Err("obsolete line folding is not allowed")
		[' ', ..] | ['\t', ..] => Err("whitespace between a field name and its colon")
		[':', .. as after_colon] if name_len > 0 => {
			value = trim_ows(after_colon)
			if value.all(is_field_byte) {
				name = Str.from_utf8(line.sublist({ start: 0, len: name_len })) ?? ""
				match Str.from_utf8(value) {
					Ok(text) => Ok(Header(name, text))
					Err(_) => Err("field value is not valid UTF-8")
				}
			} else {
				Err("control character in field value")
			}
		}
		_ => Err("field name must be a token followed by a colon")
	}
}

## The values of every field with this (lowercase) name.
values_named : List(HTTP.Header), Str -> List(Bytes)
values_named = |fields, wanted| {
	target = wanted.to_utf8()
	fields.fold(
		[],
		|found, Header(name, value)| {
			if name.to_utf8().map(to_lower) == target {
				found.append(value.to_utf8())
			} else {
				found
			}
		},
	)
}

## Comma-separated list elements with surrounding whitespace removed.
elements : Bytes -> List(Bytes)
elements = |bytes| {
	var $parts = []
	var $current = []
	var $index = 0
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == ',' {
			$parts = $parts.append(trim_ows($current))
			$current = []
		} else {
			$current = $current.append(byte)
		}
		$index = $index + 1
	}
	$parts.append(trim_ows($current))
}

## How the body is delimited (RFC 9112 section 6.3), rejecting ambiguity.
framing : List(HTTP.Header), HTTP.HttpVersion -> Try(Framing, Str)
framing = |fields, version| {
	transfer_encodings = values_named(fields, "transfer-encoding")
	content_lengths = values_named(fields, "content-length")
	if !transfer_encodings.is_empty() {
		# RFC 9112 6.1 allows a recipient to process Transfer-Encoding over
		# Content-Length, but a message with both "ought to be handled as an
		# error"; llhttp rejects it by default, as does this parser.
		codings =
			transfer_encodings
				.fold([], |all, value| all.concat(elements(value)))
				.map(|coding| coding.map(to_lower))
		if !content_lengths.is_empty() {
			Err("both Transfer-Encoding and Content-Length")
		} else if !at_least_1_1(version) {
			Err("Transfer-Encoding in an HTTP/1.0 message")
		} else if codings == [['c', 'h', 'u', 'n', 'k', 'e', 'd']] {
			# Exactly one element: empty list elements (", chunked") are
			# rejected like h11 does, since intermediaries disagree on them.
			Ok(Chunked)
		} else {
			Err("unsupported transfer coding (only a single chunked is supported)")
		}
	} else if !content_lengths.is_empty() {
		parse_content_length(content_lengths.fold([], |all, value| all.concat(elements(value))))
	} else {
		Ok(Unframed)
	}
}

## Content-Length = 1*DIGIT, possibly repeated as identical list elements
## (RFC 9110 section 8.6), as h11 accepts.
parse_content_length : List(Bytes) -> Try(Framing, Str)
parse_content_length = |values| {
	first = values.first() ?? []
	if values.any(|value| value != first) {
		Err("conflicting Content-Length values")
	} else if first.is_empty() or !first.all(is_digit) {
		Err("Content-Length must be decimal digits")
	} else {
		significant = first.drop_first(prefix_len(first, |byte| byte == '0'))
		if significant.len() > 19 {
			Err("Content-Length is too large")
		} else {
			Ok(Length(significant.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())))
		}
	}
}

take_body : Framing, Bytes -> Try({ body : Bytes, rest : Bytes }, Str)
take_body = |body_framing, bytes| {
	match body_framing {
		Length(length) =>
			if length > bytes.len() {
				Err("body is shorter than Content-Length")
			} else {
				Ok({ body: bytes.sublist({ start: 0, len: length }), rest: bytes.drop_first(length) })
			}
		Chunked => decode_chunked(bytes, [])
		Unframed => Ok({ body: bytes, rest: [] })
	}
}

## chunked-body = *chunk last-chunk trailer-section CRLF
decode_chunked : Bytes, Bytes -> Try({ body : Bytes, rest : Bytes }, Str)
decode_chunked = |bytes, body| {
	{ line, rest } = take_line(bytes)?
	size = parse_chunk_header(line)?
	if size == 0 {
		trailers = parse_fields(rest, [])?
		Ok({ body, rest: trailers.rest })
	} else if size > rest.len() {
		Err("chunk is longer than the remaining input")
	} else {
		match rest.drop_first(size) {
			['\r', '\n', .. as next] => decode_chunked(next, body.concat(rest.sublist({ start: 0, len: size })))
			_ => Err("chunk data must be followed by CRLF")
		}
	}
}

## chunk-size [ chunk-ext ], where chunk-size = 1*HEXDIG
parse_chunk_header : Bytes -> Try(U64, Str)
parse_chunk_header = |line| {
	digits_len = prefix_len(line, is_hex_digit)
	digits = line.sublist({ start: 0, len: digits_len })
	significant = digits.drop_first(prefix_len(digits, |byte| byte == '0'))
	if digits_len == 0 {
		Err("expected a hexadecimal chunk size")
	} else if significant.len() > 15 {
		Err("chunk size is too large")
	} else {
		check_chunk_extensions(line.drop_first(digits_len))?
		Ok(significant.fold(0, |sum, digit| sum * 16 + hex_value(digit)))
	}
}

## chunk-ext = *( BWS ";" BWS chunk-ext-name [ BWS "=" BWS chunk-ext-val ] )
check_chunk_extensions : Bytes -> Try({}, Str)
check_chunk_extensions = |bytes| {
	match trim_start_ows(bytes) {
		[] => if bytes.is_empty() Ok({}) else Err("whitespace after chunk size")
		[';', .. as after_semicolon] => {
			name_onwards = trim_start_ows(after_semicolon)
			name_len = prefix_len(name_onwards, is_tchar)
			if name_len == 0 {
				Err("chunk extension needs a name")
			} else {
				after_name = name_onwards.drop_first(name_len)
				match trim_start_ows(after_name) {
					['=', .. as after_equals] => {
						value = trim_start_ows(after_equals)
						value_len = chunk_ext_value_len(value)?
						check_chunk_extensions(value.drop_first(value_len))
					}
					_ => check_chunk_extensions(after_name)
				}
			}
		}
		_ => Err("invalid chunk extension")
	}
}

## chunk-ext-val = token / quoted-string
chunk_ext_value_len : Bytes -> Try(U64, Str)
chunk_ext_value_len = |bytes| {
	match bytes {
		['"', ..] => quoted_string_len(bytes)
		_ => {
			token_len = prefix_len(bytes, is_tchar)
			if token_len == 0 Err("chunk extension needs a value after =") else Ok(token_len)
		}
	}
}

## quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE, starting at the
## opening quote; returns the length including both quotes.
quoted_string_len : Bytes -> Try(U64, Str)
quoted_string_len = |bytes| {
	var $index = 1
	var $result = Err("unterminated quoted string")
	var $searching = Bool.True
	while $searching and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '"' {
			$result = Ok($index + 1)
			$searching = Bool.False
		} else if byte == '\\' {
			escaped = bytes.get($index + 1) ?? 0
			if $index + 1 < bytes.len() and (escaped == '\t' or escaped == ' ' or is_vchar(escaped) or escaped >= 0x80) {
				$index = $index + 2
			} else {
				$result = Err("invalid quoted-pair in quoted string")
				$searching = Bool.False
			}
		} else if byte == '\t' or byte == ' ' or is_vchar(byte) or byte >= 0x80 {
			$index = $index + 1
		} else {
			$result = Err("control character in quoted string")
			$searching = Bool.False
		}
	}
	$result
}

prefix_len : Bytes, (U8 -> Bool) -> U64
prefix_len = |bytes, matches| {
	var $length = 0
	while $length < bytes.len() and matches(bytes.get($length) ?? 0) {
		$length = $length + 1
	}
	$length
}

is_ows : U8 -> Bool
is_ows = |byte| byte == ' ' or byte == '\t'

trim_start_ows : Bytes -> Bytes
trim_start_ows = |bytes| bytes.drop_first(prefix_len(bytes, is_ows))

trim_ows : Bytes -> Bytes
trim_ows = |bytes| {
	start = trim_start_ows(bytes)
	var $length = start.len()
	while $length > 0 and is_ows(start.get($length - 1) ?? 0) {
		$length = $length - 1
	}
	start.sublist({ start: 0, len: $length })
}

is_digit : U8 -> Bool
is_digit = |byte| byte >= '0' and byte <= '9'

is_hex_digit : U8 -> Bool
is_hex_digit = |byte| is_digit(byte) or (byte >= 'a' and byte <= 'f') or (byte >= 'A' and byte <= 'F')

hex_value : U8 -> U64
hex_value = |byte| {
	if is_digit(byte) {
		(byte - '0').to_u64()
	} else if byte >= 'a' {
		(byte - 'a' + 10).to_u64()
	} else {
		(byte - 'A' + 10).to_u64()
	}
}

to_lower : U8 -> U8
to_lower = |byte| if byte >= 'A' and byte <= 'Z' byte + 32 else byte

## VCHAR = %x21-7E
is_vchar : U8 -> Bool
is_vchar = |byte| byte >= 0x21 and byte <= 0x7E

## field-value bytes after trimming: VCHAR / obs-text / SP / HTAB
is_field_byte : U8 -> Bool
is_field_byte = |byte| is_vchar(byte) or byte >= 0x80 or byte == ' ' or byte == '\t'

## tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
##         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
is_tchar : U8 -> Bool
is_tchar = |byte| {
	(byte >= 'a' and byte <= 'z')
		or (byte >= 'A' and byte <= 'Z')
			or is_digit(byte)
				or ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~'].contains(byte)
}

parse : Parser(Utf8.Bytes, a), Str -> Try(a, [ParsingFailure(Str), ParsingIncomplete(Str)])
parse = |parser, text| Utf8.parse_str(parser, text)

## HTTP version parsing captures the major and minor numbers.
expect parse_version("HTTP/1.1".to_utf8()) == Ok({ major: 1, minor: 1 })

## Oversized HTTP version numbers are rejected.
expect parse(HTTP.request, "GET / HTTP/1.044444444444444444444\r\nHost: a\r\n\r\n").is_err()

## Request parsing captures method, target, version, fields and a sized body.
expect {
	request_text =
		\\POST /things?id=1 HTTP/1.1\r
		\\Host: bar.example\r
		\\Accept-Encoding: gzip, deflate\r
		\\Content-Length: 13\r
		\\\r
		\\Hello, world!
	actual = parse(HTTP.request, request_text)?
	expected : HTTP.Request
	expected = {
		method: Post,
		uri: "/things?id=1",
		http_version: { major: 1, minor: 1 },
		headers: [
			Header("Host", "bar.example"),
			Header("Accept-Encoding", "gzip, deflate"),
			Header("Content-Length", "13"),
		],
		body: "Hello, world!".to_utf8(),
	}
	actual == expected
}

## OPTIONS request parsing supports many headers and an empty body.
expect {
	request_text =
		\\OPTIONS /resources/post-here/ HTTP/1.1\r
		\\Host: bar.example\r
		\\Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r
		\\Accept-Language: en-us,en;q=0.5\r
		\\Origin: https://foo.example\r
		\\Access-Control-Request-Headers: X-PINGOTHER, Content-Type\r
		\\\r\n
	actual = parse(HTTP.request, request_text)?
	expected = {
		method: Options,
		uri: "/resources/post-here/",
		http_version: { major: 1, minor: 1 },
		headers: [
			Header("Host", "bar.example"),
			Header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
			Header("Accept-Language", "en-us,en;q=0.5"),
			Header("Origin", "https://foo.example"),
			Header("Access-Control-Request-Headers", "X-PINGOTHER, Content-Type"),
		],
		body: [],
	}
	actual == expected
}

## Response parsing keeps fields and reads an unframed body to the end.
expect {
	body = "<!DOCTYPE html>\r\n<p>Hello, world!</p>\r\n"
	response_text = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nETag: \"2e77ad1d\"\r\n\r\n${body}"
	actual = parse(HTTP.response, response_text)?
	expected = {
		http_version: { major: 1, minor: 1 },
		status_code: 200,
		status: "OK",
		headers: [Header("Content-Type", "text/html; charset=utf-8"), Header("ETag", "\"2e77ad1d\"")],
		body: body.to_utf8(),
	}
	actual == expected
}

## Field values lose surrounding whitespace and may be empty (RFC 9112 5).
expect {
	actual = parse(HTTP.request, "GET / HTTP/1.1\r\nHost:a\r\nX: \t b  c \t\r\nY:\r\n\r\n")?
	actual.headers == [Header("Host", "a"), Header("X", "b  c"), Header("Y", "")]
}

## A request without framing fields has no body; what follows is the next message.
expect {
	actual = Utf8.parse_utf8_partial(HTTP.request, "GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /2".to_utf8())?
	actual.value.body == [] and actual.rest == "GET /2".to_utf8()
}

## Chunked bodies are decoded; extensions and trailers are validated and dropped.
expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5;a=b ; c=\"q\\\"\"\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n"
	actual = parse(HTTP.request, text)?
	actual.body == "hello world".to_utf8()
}

## Status codes are exactly three digits and never wrap (65736 used to read as 200).
expect parse(HTTP.response, "HTTP/1.1 65736 OK\r\n\r\n").is_err()

## A missing reason phrase is allowed, with or without the SP before it.
expect parse(HTTP.response, "HTTP/1.1 204\r\n\r\n").map_ok(|r| r.status) == Ok("")

## Non-UTF-8 field values are rejected instead of crashing.
expect Utf8.parse_utf8(HTTP.request, ['G', 'E', 'T', ' ', '/', ' ', 'H', 'T', 'T', 'P', '/', '1', '.', '0', '\r', '\n', 'X', ':', 0xFF, '\r', '\n', '\r', '\n']).is_err()

## Request-smuggling constructs are rejected.
expect {
	smuggling = [
		"POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n",
		"POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\nContent-Length: 4\r\n\r\nabcd",
		"POST / HTTP/1.1\r\nHost: a\r\nContent-Length: +4\r\n\r\nabcd",
		"POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 18446744073709551620\r\n\r\nabcd",
		"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked, gzip\r\n\r\n0\r\n\r\n",
		"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding : chunked\r\n\r\n0\r\n\r\n",
		"POST / HTTP/1.1\r\nHost: a\r\nX: y\r\n Transfer-Encoding: chunked\r\n\r\n0\r\n\r\n",
		"POST / HTTP/1.0\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n",
		"POST / HTTP/1.1\r\nHost: a\nContent-Length: 1\r\n\r\nx",
		"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5 \r\nhello\r\n0\r\n\r\n",
		"GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n",
		"GET / HTTP/1.1\r\n\r\n",
	]
	smuggling.all(|text| parse(HTTP.request, text).is_err())
}

## A simple GET request parses as shown in the request docs.
expect {
	text = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n"
	match Utf8.parse_str(HTTP.request, text) {
		Ok(req) => req.method == Get and req.uri == "/hello" and req.headers == [Header("Host", "example.com")]
		Err(_) => Bool.False
	}
}

## Lowercase method names are rejected.
expect Utf8.parse_str(HTTP.request, "get / HTTP/1.1\r\nHost: a\r\n\r\n").is_err()
