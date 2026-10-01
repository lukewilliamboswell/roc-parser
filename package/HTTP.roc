import Parser
import Utf8

## Parse HTTP/1.x requests and responses, following the message syntax of
## RFC 9112 and the field syntax of RFC 9110.
##
## [HTTP.parse_request] and [HTTP.parse_response] read one message from the
## start of a byte list and return it with the bytes that follow (the next
## pipelined message). [HTTP.parse_requests] reads a whole pipeline. They fail
## with `InvalidHttp(Error)`, where the [HTTP.Error] gives the byte offset of
## the problem. [HTTP.request] and [HTTP.response] are the same parsers for
## use inside a larger [Parser].
##
## ```roc
## expect {
##     bytes = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n".to_utf8()
##     match HTTP.parse_request(bytes) {
##         Ok({ request, rest }) => request.method == Get and request.target == "/hello" and rest == []
##         Err(InvalidHttp(_)) => False
##     }
## }
## ```
##
## Message framing (RFC 9112 section 6.3):
## - `Transfer-Encoding: chunked` bodies are decoded; chunk extensions and
##   trailer fields are validated and discarded.
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
## HTTP/1.1 requests must have exactly one `Host` field. Field names keep their
## case; field values have surrounding whitespace removed. Field values and
## reason phrases must be valid UTF-8 to be represented as `Str`; other
## obs-text bytes are rejected.
HTTP :: [].{

	## A request method (RFC 9110 section 9).
	##
	## The methods RFC 9110 and RFC 5789 define get their own tags. Any other
	## method token is kept as `Extension(name)`. Method names are
	## case-sensitive, so `get` is `Extension("get")`, not `Get`.
	Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch, Extension(Str)]

	## An HTTP protocol version such as `HTTP/1.1`, as `{ major: 1, minor: 1 }`.
	##
	## Each part is a single digit, as RFC 9112 requires.
	Version : { major : U8, minor : U8 }

	## One header field: the `name` as written (case preserved) and the `value`
	## with surrounding whitespace removed.
	##
	## Fields keep their order in the message, and repeated fields are kept.
	## Use [HTTP.header] to look one up by name.
	Header : { name : Str, value : Str }

	## A parsed HTTP request message.
	##
	## `target` is the request-target exactly as written (it is not decoded or
	## normalized). `body` is the message body after any chunked transfer
	## coding has been removed.
	Request : {
		method : Method,
		target : Str,
		version : Version,
		headers : List(Header),
		body : List(U8),
	}

	## A parsed HTTP response message.
	##
	## `status_code` is the three-digit status code and `reason` the reason
	## phrase, which may be empty. `body` is decoded like a request body.
	Response : {
		version : Version,
		status_code : U16,
		reason : Str,
		headers : List(Header),
		body : List(U8),
	}

	## Why a message was rejected: `message` names the broken rule and
	## `offset` is the byte offset in the input where the problem is.
	##
	## Problems with the message as a whole, such as a missing `Host` field,
	## are at offset 0, the start of the message.
	Error : { offset : U64, message : Str }

	## The value of the first header with this name, compared
	## case-insensitively, or `Err(Missing)`.
	##
	## ```roc
	## expect {
	##     headers = [{ name: "Content-Type", value: "text/plain" }]
	##     HTTP.header(headers, "content-type") == Ok("text/plain") and HTTP.header(headers, "Host") == Err(Missing)
	## }
	## ```
	header : List(Header), Str -> Try(Str, [Missing])
	header = |headers, name| {
		wanted = name.with_ascii_lowercased()
		var $found = Err(Missing)
		for field in headers {
			if $found == Err(Missing) and field.name.with_ascii_lowercased() == wanted {
				$found = Ok(field.value)
			}
		}
		$found
	}

	## Parse one HTTP request from the start of `bytes`: request line, header
	## fields, and the body its framing describes.
	##
	## Returns the `request` and the `rest` of the bytes after it, which is the
	## next pipelined message, if any.
	##
	## ```roc
	## expect {
	##     bytes = "POST /notes HTTP/1.1\r\nHost: a\r\nContent-Length: 2\r\n\r\nhiGET".to_utf8()
	##     match HTTP.parse_request(bytes) {
	##         Ok({ request, rest }) => request.body == "hi".to_utf8() and rest == "GET".to_utf8()
	##         Err(InvalidHttp(_)) => False
	##     }
	## }
	## ```
	parse_request : List(U8) -> Try({ request : Request, rest : List(U8) }, [InvalidHttp(Error)])
	parse_request = |bytes| {
		match read_request(bytes) {
			Ok({ value, rest }) => Ok({ request: value, rest })
			Err(error) => Err(InvalidHttp(error))
		}
	}

	## Parse one HTTP response from the start of `bytes`: status line, header
	## fields, and the body its framing describes.
	##
	## A response with no `Content-Length` or `Transfer-Encoding` takes the
	## rest of the input as its body, so its `rest` is empty.
	##
	## ```roc
	## expect {
	##     bytes = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n".to_utf8()
	##     match HTTP.parse_response(bytes) {
	##         Ok({ response, rest: _ }) => response.status_code == 404 and response.reason == "Not Found"
	##         Err(InvalidHttp(_)) => False
	##     }
	## }
	## ```
	parse_response : List(U8) -> Try({ response : Response, rest : List(U8) }, [InvalidHttp(Error)])
	parse_response = |bytes| {
		match read_response(bytes) {
			Ok({ value, rest }) => Ok({ response: value, rest })
			Err(error) => Err(InvalidHttp(error))
		}
	}

	## Parse every request in `bytes`, a pipeline of zero or more requests
	## that must end exactly at the end of the last one.
	##
	## A failure's `offset` counts from the start of `bytes`.
	##
	## ```roc
	## expect {
	##     bytes = "GET /a HTTP/1.1\r\nHost: a\r\n\r\nGET /b HTTP/1.1\r\nHost: a\r\n\r\n".to_utf8()
	##     HTTP.parse_requests(bytes).map_ok(|requests| requests.map(|r| r.target)) == Ok(["/a", "/b"])
	## }
	## ```
	parse_requests : List(U8) -> Try(List(Request), [InvalidHttp(Error)])
	parse_requests = |bytes| read_requests(bytes, bytes.len(), [])

	## A [Parser] for one HTTP request, like [HTTP.parse_request], for use
	## inside a larger parser.
	##
	## It leaves the bytes after the message unconsumed. A failure is a
	## `ParseError` whose message starts with `invalid HTTP request:` and whose
	## offset is where the problem is.
	##
	## ```roc
	## expect {
	##     text = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n"
	##     match Utf8.parse_str(HTTP.request, text) {
	##         Ok(req) => req.method == Get and HTTP.header(req.headers, "host") == Ok("example.com")
	##         Err(_) => False
	##     }
	## }
	## ```
	request : Parser(Utf8.Bytes, Request)
	request =
		Parser.custom(
			|input| {
				read_request(input).map_err(|{ offset, message }| ParseError({ message: "invalid HTTP request: ${message}", offset }))
			},
		)

	## A [Parser] for one HTTP response, like [HTTP.parse_response], for use
	## inside a larger parser.
	##
	## A failure is a `ParseError` whose message starts with
	## `invalid HTTP response:` and whose offset is where the problem is.
	response : Parser(Utf8.Bytes, Response)
	response =
		Parser.custom(
			|input| {
				read_response(input).map_err(|{ offset, message }| ParseError({ message: "invalid HTTP response: ${message}", offset }))
			},
		)
}

Bytes : List(U8)

Framing : [Length(U64), Chunked, Unframed]

# A failure, with `offset` relative to the start of the bytes the function
# that produced it was given.
Failure : HTTP.Error

# The value of a field that decides the framing or the Host check, with the
# byte offset of its line so that errors can point at it. Only these fields
# are recorded beside the headers, so the common case allocates nothing extra.
Field : { kind : FieldKind, value : Bytes, offset : U64 }

FieldKind : [Host, TransferEncoding, ContentLength]

# The parsed field lines of a head: every header, the fields that matter for
# framing, and the bytes after the empty line.
Head : { headers : List(HTTP.Header), fields : List(Field), rest : Bytes }

fail : U64, Str -> Try(a, Failure)
fail = |offset, message| Err({ offset, message })

# Move a failure's offset from a slice starting at `base` to the outer input.
shift : Try(a, Failure), U64 -> Try(a, Failure)
shift = |result, base| result.map_err(|{ offset, message }| { offset: base + offset, message })

# The requests in `bytes`, the end of an input `total` bytes long.
read_requests : Bytes, U64, List(HTTP.Request) -> Try(List(HTTP.Request), [InvalidHttp(HTTP.Error)])
read_requests = |bytes, total, requests| {
	if bytes.is_empty() {
		Ok(requests)
	} else {
		match shift(read_request(bytes), total - bytes.len()) {
			Ok({ value, rest }) => read_requests(rest, total, requests.append(value))
			Err(error) => Err(InvalidHttp(error))
		}
	}
}

read_request : Bytes -> Try({ value : HTTP.Request, rest : Bytes }, Failure)
read_request = |input| {
	{ line, rest } = take_line(input)?
	start = parse_request_line(line)?
	head = parse_fields(rest, line.len() + 2, { headers: [], fields: [], rest: [] })?
	version = start.version
	hosts = values_named(head.fields, Host)
	if hosts.len() > 1 {
		fail((hosts.get(1) ?? { value: [], offset: 0 }).offset, "more than one Host field")
	} else if hosts.is_empty() and at_least_1_1(version) {
		fail(0, "an HTTP/1.1 request needs a Host field")
	} else {
		body_start = input.len() - head.rest.len()
		framed =
			match framing(head.fields, version)? {
				Unframed => take_body(Length(0), head.rest, body_start)?
				other => take_body(other, head.rest, body_start)?
			}
		Ok({ value: { method: start.method, target: start.target, version, headers: head.headers, body: framed.body }, rest: framed.rest })
	}
}

read_response : Bytes -> Try({ value : HTTP.Response, rest : Bytes }, Failure)
read_response = |input| {
	{ line, rest } = take_line(input)?
	start = parse_status_line(line)?
	head = parse_fields(rest, line.len() + 2, { headers: [], fields: [], rest: [] })?
	version = start.version
	body_framing = framing(head.fields, version)?
	code = start.status_code
	framed =
		if code < 200 or code == 204 or code == 304 {
			# RFC 9112 section 6.3 item 1: these never have content.
			{ body: [], rest: head.rest }
		} else {
			take_body(body_framing, head.rest, input.len() - head.rest.len())?
		}
	Ok({ value: { version, status_code: code, reason: start.reason, headers: head.headers, body: framed.body }, rest: framed.rest })
}

at_least_1_1 : HTTP.Version -> Bool
at_least_1_1 = |version| version.major > 1 or (version.major == 1 and version.minor >= 1)

# Split off one line ending in CRLF. A CR or LF anywhere else is an error
# (RFC 9112 section 2.2: bare CR must be rejected; this parser also declines
# the optional leniency for bare LF).
take_line : Bytes -> Try({ line : Bytes, rest : Bytes }, Failure)
take_line = |bytes| {
	# Build the error only when there is one: a `Try` holding an error
	# message costs an allocation even on the success path.
	var $index = Utf8.find_line_end(bytes, 0)
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\r' {
			return
				if bytes.get($index + 1) == Ok('\n') {
					Ok({ line: bytes.sublist({ start: 0, len: $index }), rest: bytes.drop_first($index + 2) })
				} else {
					fail($index, "CR not followed by LF")
				}
		} else if byte == '\n' {
			return fail($index, "LF not preceded by CR")
		}
		$index = $index + 1
	}
	fail(bytes.len(), "message ended before CRLF")
}

# request-line = method SP request-target SP HTTP-version
parse_request_line : Bytes -> Try({ method : HTTP.Method, target : Str, version : HTTP.Version }, Failure)
parse_request_line = |line| {
	method_len = Utf8.skip_class(line, 0, tchar_class)
	method = to_method(line.sublist({ start: 0, len: method_len }))?
	match line.drop_first(method_len) {
		[' ', .. as target_onwards] => {
			target_start = method_len + 1
			target_len = prefix_len(target_onwards, is_vchar)
			if target_len == 0 {
				fail(target_start, "expected a request target after the method")
			} else {
				match target_onwards.drop_first(target_len) {
					[' ', .. as version_bytes] => {
						version = shift(parse_version(version_bytes), target_start + target_len + 1)?
						target = Str.from_utf8(target_onwards.sublist({ start: 0, len: target_len })) ?? ""
						Ok({ method, target, version })
					}
					_ => fail(target_start + target_len, "request target must be visible ASCII followed by a single SP")
				}
			}
		}
		_ => fail(method_len, "method must be a token followed by a single SP")
	}
}

to_method : Bytes -> Try(HTTP.Method, Failure)
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
		[] => fail(0, "expected a method token")
		# A token is ASCII, so the fallback is never used.
		_ => Ok(Extension(Str.from_utf8(bytes) ?? ""))
	}
}

# HTTP-version = "HTTP" "/" DIGIT "." DIGIT
parse_version : Bytes -> Try(HTTP.Version, Failure)
parse_version = |bytes| {
	match bytes {
		['H', 'T', 'T', 'P', '/', major, '.', minor] if is_digit(major) and is_digit(minor) =>
			Ok({ major: major - '0', minor: minor - '0' })
		_ => fail(0, "expected an HTTP version like HTTP/1.1")
	}
}

# status-line = HTTP-version SP status-code SP [ reason-phrase ]
#
# Like h11 and llhttp, the SP before an absent reason phrase is optional.
parse_status_line : Bytes -> Try({ version : HTTP.Version, status_code : U16, reason : Str }, Failure)
parse_status_line = |line| {
	version = parse_version(line.sublist({ start: 0, len: 8 }))?
	match line.drop_first(8) {
		[' ', a, b, c, .. as after_code] if is_digit(a) and is_digit(b) and is_digit(c) => {
			status_code = (a - '0').to_u16() * 100 + (b - '0').to_u16() * 10 + (c - '0').to_u16()
			phrase =
				match after_code {
					[] => Ok([])
					[' ', .. as text] => Ok(text)
					_ => fail(12, "status code must be exactly three digits")
				}?
			valid_len = prefix_len(phrase, |byte| byte == '\t' or byte == ' ' or is_vchar(byte) or byte >= 0x80)
			if status_code < 100 {
				fail(9, "status code must be at least 100")
			} else if valid_len < phrase.len() {
				fail(13 + valid_len, "control character in reason phrase")
			} else {
				match Str.from_utf8(phrase) {
					Ok(reason) => Ok({ version, status_code, reason })
					Err(_) => fail(13, "reason phrase is not valid UTF-8")
				}
			}
		}
		_ => fail(8, "expected SP and a three-digit status code after the version")
	}
}

# Field lines up to and including the empty line that ends them. `base` is
# the offset of `bytes` in the message, recorded with each field.
parse_fields : Bytes, U64, Head -> Try(Head, Failure)
parse_fields = |bytes, base, head| {
	{ line, rest } = shift(take_line(bytes), base)?
	if line.is_empty() {
		Ok({ headers: head.headers, fields: head.fields, rest })
	} else {
		parsed = shift(parse_field_line(line), base)?
		fields =
			match parsed.kind {
				Ok(kind) => head.fields.append({ kind, value: parsed.value, offset: base })
				Err(_) => head.fields
			}
		parse_fields(rest, base + line.len() + 2, { headers: head.headers.append(parsed.header), fields, rest: [] })
	}
}

# Which of the fields the parser itself looks at, if any, compared
# case-insensitively without allocating.
field_kind : Bytes -> Try(FieldKind, [Other])
field_kind = |name| {
	if ascii_eq_lower(name, "host") {
		Ok(Host)
	} else if ascii_eq_lower(name, "content-length") {
		Ok(ContentLength)
	} else if ascii_eq_lower(name, "transfer-encoding") {
		Ok(TransferEncoding)
	} else {
		Err(Other)
	}
}

# Whether `bytes` equals the lowercase ASCII `wanted`, ignoring ASCII case.
ascii_eq_lower : Bytes, Str -> Bool
ascii_eq_lower = |bytes, wanted| {
	if bytes.len() != wanted.count_utf8_bytes().to_u64() {
		False
	} else {
		var $index = 0
		var $equal = True
		for expected in wanted.to_utf8() {
			if to_lower(bytes.get($index) ?? 0) != expected {
				$equal = False
			}
			$index = $index + 1
		}
		$equal
	}
}

# field-line = field-name ":" OWS field-value OWS
parse_field_line : Bytes -> Try({ header : HTTP.Header, kind : Try(FieldKind, [Other]), value : Bytes }, Failure)
parse_field_line = |line| {
	name_len = Utf8.skip_class(line, 0, tchar_class)
	match line.drop_first(name_len) {
		[' ', ..] | ['\t', ..] if name_len == 0 => fail(0, "obsolete line folding is not allowed")
		[' ', ..] | ['\t', ..] => fail(name_len, "whitespace between a field name and its colon")
		[':', .. as after_colon] if name_len > 0 => {
			value_start = name_len + 1 + prefix_len(after_colon, is_ows)
			value = trim_ows(after_colon)
			valid_len = Utf8.skip_class(value, 0, field_byte_class)
			if valid_len == value.len() {
				name = Str.from_utf8(line.sublist({ start: 0, len: name_len })) ?? ""
				match Str.from_utf8(value) {
					Ok(text) => Ok({ header: { name, value: text }, kind: field_kind(line.sublist({ start: 0, len: name_len })), value })
					Err(_) => fail(value_start, "field value is not valid UTF-8")
				}
			} else {
				fail(value_start + valid_len, "control character in field value")
			}
		}
		_ => fail(name_len, "field name must be a token followed by a colon")
	}
}

# The values of every field with this (lowercase) name, with the offsets of
# their lines.
values_named : List(Field), FieldKind -> List({ value : Bytes, offset : U64 })
values_named = |fields, wanted| {
	fields.fold(
		[],
		|found, { kind, value, offset }| {
			if kind == wanted {
				found.append({ value, offset })
			} else {
				found
			}
		},
	)
}

# Comma-separated list elements with surrounding whitespace removed.
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

# How the body is delimited (RFC 9112 section 6.3), rejecting ambiguity.
# Failures point at the field line responsible.
framing : List(Field), HTTP.Version -> Try(Framing, Failure)
framing = |fields, version| {
	transfer_encodings = values_named(fields, TransferEncoding)
	content_lengths = values_named(fields, ContentLength)
	match (transfer_encodings.first(), content_lengths.first()) {
		(Ok(te), Ok(cl)) =>
		# RFC 9112 6.1 allows a recipient to process Transfer-Encoding over
		# Content-Length, but a message with both "ought to be handled as an
		# error"; llhttp rejects it by default, as does this parser.
			fail(if te.offset > cl.offset te.offset else cl.offset, "both Transfer-Encoding and Content-Length")
		(Ok(te), Err(_)) => {
			codings =
				transfer_encodings
					.fold([], |all, field| all.concat(elements(field.value)))
					.map(|coding| coding.map(to_lower))
			if !at_least_1_1(version) {
				fail(te.offset, "Transfer-Encoding in an HTTP/1.0 message")
			} else if codings == [['c', 'h', 'u', 'n', 'k', 'e', 'd']] {
				# Exactly one element: empty list elements (", chunked") are
				# rejected like h11 does, since intermediaries disagree on them.
				Ok(Chunked)
			} else {
				fail(te.offset, "unsupported transfer coding (only a single chunked is supported)")
			}
		}
		(Err(_), Ok(cl)) => parse_content_length(content_lengths, cl)
		(Err(_), Err(_)) => Ok(Unframed)
	}
}

# Content-Length = 1*DIGIT, possibly repeated as identical list elements
# (RFC 9110 section 8.6), as h11 accepts.
parse_content_length : List({ value : Bytes, offset : U64 }), { value : Bytes, offset : U64 } -> Try(Framing, Failure)
parse_content_length = |fields, first| {
	wanted = elements(first.value).first() ?? []
	match fields.find_first(|field| elements(field.value).any(|value| value != wanted)) {
		Ok(field) => fail(field.offset, "conflicting Content-Length values")
		Err(_) =>
			if wanted.is_empty() or !wanted.all(is_digit) {
				fail(first.offset, "Content-Length must be decimal digits")
			} else {
				significant = wanted.drop_first(prefix_len(wanted, |byte| byte == '0'))
				if significant.len() > 19 {
					fail(first.offset, "Content-Length is too large")
				} else {
					Ok(Length(significant.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())))
				}
			}
	}
}

# The body, from `bytes` at offset `base` in the message.
take_body : Framing, Bytes, U64 -> Try({ body : Bytes, rest : Bytes }, Failure)
take_body = |body_framing, bytes, base| {
	match body_framing {
		Length(length) =>
			if length > bytes.len() {
				fail(base + bytes.len(), "body is shorter than Content-Length")
			} else {
				Ok({ body: bytes.sublist({ start: 0, len: length }), rest: bytes.drop_first(length) })
			}
		Chunked => decode_chunked(bytes, base, [])
		Unframed => Ok({ body: bytes, rest: [] })
	}
}

# chunked-body = *chunk last-chunk trailer-section CRLF, from `bytes` at
# offset `base` in the message.
decode_chunked : Bytes, U64, Bytes -> Try({ body : Bytes, rest : Bytes }, Failure)
decode_chunked = |bytes, base, body| {
	{ line, rest } = shift(take_line(bytes), base)?
	size = shift(parse_chunk_header(line), base)?
	data_start = base + line.len() + 2
	if size == 0 {
		trailers = parse_fields(rest, data_start, { headers: [], fields: [], rest: [] })?
		Ok({ body, rest: trailers.rest })
	} else if size > rest.len() {
		fail(data_start + rest.len(), "chunk is longer than the remaining input")
	} else {
		match rest.drop_first(size) {
			['\r', '\n', .. as next] => decode_chunked(next, data_start + size + 2, body.concat(rest.sublist({ start: 0, len: size })))
			_ => fail(data_start + size, "chunk data must be followed by CRLF")
		}
	}
}

# chunk-size [ chunk-ext ], where chunk-size = 1*HEXDIG
parse_chunk_header : Bytes -> Try(U64, Failure)
parse_chunk_header = |line| {
	digits_len = prefix_len(line, is_hex_digit)
	digits = line.sublist({ start: 0, len: digits_len })
	significant = digits.drop_first(prefix_len(digits, |byte| byte == '0'))
	if digits_len == 0 {
		fail(0, "expected a hexadecimal chunk size")
	} else if significant.len() > 15 {
		fail(0, "chunk size is too large")
	} else {
		check_chunk_extensions(line.drop_first(digits_len), line.len())?
		Ok(significant.fold(0, |sum, digit| sum * 16 + hex_value(digit)))
	}
}

# chunk-ext = *( BWS ";" BWS chunk-ext-name [ BWS "=" BWS chunk-ext-val ] )
#
# `bytes` is the end of a chunk header `line_len` bytes long, so a failure at
# the start of a suffix `s` is at offset `line_len - s.len()`.
check_chunk_extensions : Bytes, U64 -> Try({}, Failure)
check_chunk_extensions = |bytes, line_len| {
	match trim_start_ows(bytes) {
		[] => if bytes.is_empty() Ok({}) else fail(line_len - bytes.len(), "whitespace after chunk size")
		[';', .. as after_semicolon] => {
			name_onwards = trim_start_ows(after_semicolon)
			name_len = prefix_len(name_onwards, is_tchar)
			if name_len == 0 {
				fail(line_len - name_onwards.len(), "chunk extension needs a name")
			} else {
				after_name = name_onwards.drop_first(name_len)
				match trim_start_ows(after_name) {
					['=', .. as after_equals] => {
						value = trim_start_ows(after_equals)
						value_len = shift(chunk_ext_value_len(value), line_len - value.len())?
						check_chunk_extensions(value.drop_first(value_len), line_len)
					}
					_ => check_chunk_extensions(after_name, line_len)
				}
			}
		}
		other => fail(line_len - other.len(), "invalid chunk extension")
	}
}

# chunk-ext-val = token / quoted-string
chunk_ext_value_len : Bytes -> Try(U64, Failure)
chunk_ext_value_len = |bytes| {
	match bytes {
		['"', ..] => quoted_string_len(bytes)
		_ => {
			token_len = prefix_len(bytes, is_tchar)
			if token_len == 0 fail(0, "chunk extension needs a value after =") else Ok(token_len)
		}
	}
}

# quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE, starting at the
# opening quote; returns the length including both quotes.
quoted_string_len : Bytes -> Try(U64, Failure)
quoted_string_len = |bytes| {
	var $index = 1
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '"' {
			return Ok($index + 1)
		} else if byte == '\\' {
			escaped = bytes.get($index + 1) ?? 0
			if $index + 1 < bytes.len() and (escaped == '\t' or escaped == ' ' or is_vchar(escaped) or escaped >= 0x80) {
				$index = $index + 2
			} else {
				return fail($index, "invalid quoted-pair in quoted string")
			}
		} else if byte == '\t' or byte == ' ' or is_vchar(byte) or byte >= 0x80 {
			$index = $index + 1
		} else {
			return fail($index, "control character in quoted string")
		}
	}
	fail(bytes.len(), "unterminated quoted string")
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

# VCHAR = %x21-7E
is_vchar : U8 -> Bool
is_vchar = |byte| byte >= 0x21 and byte <= 0x7E

# field-value bytes after trimming: VCHAR / obs-text / SP / HTAB
is_field_byte : U8 -> Bool
is_field_byte = |byte| is_vchar(byte) or byte >= 0x80 or byte == ' ' or byte == '\t'

# tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
#         "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
tchar_class : Utf8.ByteClass
tchar_class = Utf8.ByteClass.from_predicate(is_tchar)

field_byte_class : Utf8.ByteClass
field_byte_class = Utf8.ByteClass.from_predicate(is_field_byte)

is_tchar : U8 -> Bool
is_tchar = |byte| {
	(byte >= 'a' and byte <= 'z')
		or (byte >= 'A' and byte <= 'Z')
			or is_digit(byte)
				or ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~'].contains(byte)
}

parse : Parser(Utf8.Bytes, a), Str -> Try(a, [ParseError({ message : Str, offset : U64 })])
parse = |parser, text| Utf8.parse_str(parser, text)

# The error a request is rejected with.
request_error : Str -> Try(HTTP.Error, [Accepted])
request_error = |text| {
	match HTTP.parse_request(text.to_utf8()) {
		Ok(_) => Err(Accepted)
		Err(InvalidHttp(error)) => Ok(error)
	}
}

# HTTP version parsing captures the major and minor numbers.
expect parse_version("HTTP/1.1".to_utf8()) == Ok({ major: 1, minor: 1 })

# Oversized HTTP version numbers are rejected.
expect parse(HTTP.request, "GET / HTTP/1.044444444444444444444\r\nHost: a\r\n\r\n").is_err()

# Request parsing captures method, target, version, fields and a sized body.
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
		target: "/things?id=1",
		version: { major: 1, minor: 1 },
		headers: [
			{ name: "Host", value: "bar.example" },
			{ name: "Accept-Encoding", value: "gzip, deflate" },
			{ name: "Content-Length", value: "13" },
		],
		body: "Hello, world!".to_utf8(),
	}
	actual == expected
}

# OPTIONS request parsing supports many headers and an empty body.
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
		target: "/resources/post-here/",
		version: { major: 1, minor: 1 },
		headers: [
			{ name: "Host", value: "bar.example" },
			{ name: "Accept", value: "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" },
			{ name: "Accept-Language", value: "en-us,en;q=0.5" },
			{ name: "Origin", value: "https://foo.example" },
			{ name: "Access-Control-Request-Headers", value: "X-PINGOTHER, Content-Type" },
		],
		body: [],
	}
	actual == expected
}

# Response parsing keeps fields and reads an unframed body to the end.
expect {
	body = "<!DOCTYPE html>\r\n<p>Hello, world!</p>\r\n"
	response_text = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nETag: \"2e77ad1d\"\r\n\r\n${body}"
	actual = parse(HTTP.response, response_text)?
	expected = {
		version: { major: 1, minor: 1 },
		status_code: 200,
		reason: "OK",
		headers: [{ name: "Content-Type", value: "text/html; charset=utf-8" }, { name: "ETag", value: "\"2e77ad1d\"" }],
		body: body.to_utf8(),
	}
	actual == expected
}

# Field values lose surrounding whitespace and may be empty (RFC 9112 5).
expect {
	actual = parse(HTTP.request, "GET / HTTP/1.1\r\nHost:a\r\nX: \t b  c \t\r\nY:\r\n\r\n")?
	actual.headers == [{ name: "Host", value: "a" }, { name: "X", value: "b  c" }, { name: "Y", value: "" }]
}

# A request without framing fields has no body; what follows is the next message.
expect {
	actual = Utf8.parse_bytes_partial(HTTP.request, "GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /2".to_utf8())?
	actual.value.body == [] and actual.rest == "GET /2".to_utf8()
}

# Chunked bodies are decoded; extensions and trailers are validated and dropped.
expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5;a=b ; c=\"q\\\"\"\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n"
	actual = parse(HTTP.request, text)?
	actual.body == "hello world".to_utf8()
}

# Status codes are exactly three digits and never wrap (65736 used to read as 200).
expect parse(HTTP.response, "HTTP/1.1 65736 OK\r\n\r\n").is_err()

# A missing reason phrase is allowed, with or without the SP before it.
expect parse(HTTP.response, "HTTP/1.1 204\r\n\r\n").map_ok(|r| r.reason) == Ok("")

# Non-UTF-8 field values are rejected instead of crashing.
expect Utf8.parse_bytes(HTTP.request, ['G', 'E', 'T', ' ', '/', ' ', 'H', 'T', 'T', 'P', '/', '1', '.', '0', '\r', '\n', 'X', ':', 0xFF, '\r', '\n', '\r', '\n']).is_err()

# Request-smuggling constructs are rejected.
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

# Methods outside the standard set are extension methods; names are case-sensitive.
expect parse(HTTP.request, "PURGE / HTTP/1.1\r\nHost: a\r\n\r\n").map_ok(|r| r.method) == Ok(Extension("PURGE"))
expect parse(HTTP.request, "get / HTTP/1.1\r\nHost: a\r\n\r\n").map_ok(|r| r.method) == Ok(Extension("get"))

# A method must be a token.
expect request_error("G(T / HTTP/1.1\r\nHost: a\r\n\r\n") == Ok({ offset: 1, message: "method must be a token followed by a single SP" })
expect request_error(" / HTTP/1.1\r\nHost: a\r\n\r\n") == Ok({ offset: 0, message: "expected a method token" })

# Errors point at the offending byte or field line.
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n") == Ok({ offset: 44, message: "both Transfer-Encoding and Content-Length" })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n") == Ok({ offset: 25, message: "more than one Host field" })
expect request_error("GET / HTTP/1.1\r\nHost: a\nX: y\r\n\r\n") == Ok({ offset: 23, message: "LF not preceded by CR" })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nX : y\r\n\r\n") == Ok({ offset: 26, message: "whitespace between a field name and its colon" })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nX: a\u(7)b\r\n\r\n") == Ok({ offset: 29, message: "control character in field value" })
expect request_error("GET / HTTP/1.2x\r\nHost: a\r\n\r\n") == Ok({ offset: 6, message: "expected an HTTP version like HTTP/1.1" })
expect request_error("POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nab") == Ok({ offset: 49, message: "body is shorter than Content-Length" })
expect request_error("POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n2;=\r\nab\r\n0\r\n\r\n") == Ok({ offset: 58, message: "chunk extension needs a name" })
expect request_error("GET / HTTP/1.1\r\n\r\n") == Ok({ offset: 0, message: "an HTTP/1.1 request needs a Host field" })

# The embeddable parser reports the same offset, with a prefixed message.
expect parse(HTTP.request, "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n") == Err(ParseError({ offset: 25, message: "invalid HTTP request: more than one Host field" }))

# A response's status line errors point into the status line.
expect HTTP.parse_response("HTTP/1.1 099 Low\r\n\r\n".to_utf8()) == Err(InvalidHttp({ offset: 9, message: "status code must be at least 100" }))

# A pipeline that ends inside a message reports the offset from the start of the input.
expect {
	bytes = "GET /a HTTP/1.1\r\nHost: a\r\n\r\nGET /b HTTP/1.1\r\n".to_utf8()
	HTTP.parse_requests(bytes) == Err(InvalidHttp({ offset: 45, message: "message ended before CRLF" }))
}

# An empty pipeline has no requests.
expect HTTP.parse_requests([]) == Ok([])

# Header lookup returns the first match.
expect HTTP.header([{ name: "X", value: "1" }, { name: "x", value: "2" }], "x") == Ok("1")

# The module example parses a GET request with nothing left over.
expect {
	bytes = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n".to_utf8()
	match HTTP.parse_request(bytes) {
		Ok({ request, rest }) => request.method == Get and request.target == "/hello" and rest == []
		Err(InvalidHttp(_)) => False
	}
}

# The header example finds a field by any case and reports a missing one.
expect {
	headers = [{ name: "Content-Type", value: "text/plain" }]
	HTTP.header(headers, "content-type") == Ok("text/plain") and HTTP.header(headers, "Host") == Err(Missing)
}

# The parse_request example leaves the next message's bytes in rest.
expect {
	bytes = "POST /notes HTTP/1.1\r\nHost: a\r\nContent-Length: 2\r\n\r\nhiGET".to_utf8()
	match HTTP.parse_request(bytes) {
		Ok({ request, rest }) => request.body == "hi".to_utf8() and rest == "GET".to_utf8()
		Err(InvalidHttp(_)) => False
	}
}

# The parse_response example reads the status code and reason phrase.
expect {
	bytes = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n".to_utf8()
	match HTTP.parse_response(bytes) {
		Ok({ response, rest: _ }) => response.status_code == 404 and response.reason == "Not Found"
		Err(InvalidHttp(_)) => False
	}
}

# The parse_requests example reads both pipelined requests.
expect {
	bytes = "GET /a HTTP/1.1\r\nHost: a\r\n\r\nGET /b HTTP/1.1\r\nHost: a\r\n\r\n".to_utf8()
	HTTP.parse_requests(bytes).map_ok(|requests| requests.map(|r| r.target)) == Ok(["/a", "/b"])
}

# The request example parses a simple GET request.
expect {
	text = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n"
	match Utf8.parse_str(HTTP.request, text) {
		Ok(req) => req.method == Get and HTTP.header(req.headers, "host") == Ok("example.com")
		Err(_) => False
	}
}
