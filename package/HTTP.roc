import Parser
import Streaming
import Utf8
import Literal as Quotation

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

	## Reject repeated identical Content-Length values, including comma lists.
	Lints := { repeated_content_length : Bool ?? True }

	## A complete source message and its quotation validation options.
	ValidationProps := { source : Str, name : Str ?? "<literal>", lints : Lints ?? {} }

	## A complete checked request; HTTP source retains mandatory CRLF endings.
	RequestLiteral := { value : Request }.{

		## Parse and validate a complete request during literal conversion.
		from_quote : Str -> Try(RequestLiteral, [BadQuotedBytes(Str)])
		from_quote = |source| HTTP.validate_request({ source: source }).map_ok(|value| { value: value })
	}

	## A complete checked response, available through `.value`.
	ResponseLiteral := { value : Response }.{

		## Parse and validate a complete response during literal conversion.
		from_quote : Str -> Try(ResponseLiteral, [BadQuotedBytes(Str)])
		from_quote = |source| HTTP.validate_response({ source: source }).map_ok(|value| { value: value })
	}

	## Parse exactly one request, then check enabled lint rules.
	validate_request : ValidationProps -> Try(Request, [BadQuotedBytes(Str)])
	validate_request = |props| {
		parsed = read_request_metadata(props.source.to_utf8()).map_err(|error| quotation_error(props, error))?
		validate_message(props, parsed.rest, parsed.fields)?
		Ok(parsed.value)
	}

	## Parse exactly one response, then check enabled lint rules.
	validate_response : ValidationProps -> Try(Response, [BadQuotedBytes(Str)])
	validate_response = |props| {
		parsed = read_response_metadata(props.source.to_utf8()).map_err(|error| quotation_error(props, error))?
		validate_message(props, parsed.rest, parsed.fields)?
		Ok(parsed.value)
	}

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

	## Bounds for incremental readers. Body fragments never exceed
	## `max_body_chunk_bytes`; heads and trailer sections have independent bounds.
	ReadProps := { max_head_bytes : U64 ?? 65_536, max_line_bytes : U64 ?? 8192, max_headers : U64 ?? 100, max_trailer_bytes : U64 ?? 65_536, max_trailers : U64 ?? 100, max_body_chunk_bytes : U64 ?? 65_536 }

	## A request head, emitted before any body bytes.
	RequestHead : { method : Method, target : Str, version : Version, headers : List(Header) }

	## A response head, emitted before any body bytes.
	ResponseHead : { version : Version, status_code : U16, reason : Str, headers : List(Header) }

	## `MessageEnd` completes one HTTP message, including informational responses.
	## `Finished` from Streaming completes the source. After a 101 response or a
	## successful CONNECT, `ProtocolSwitch` precedes uninterpreted `ProtocolData`.
	Event : [RequestHead(RequestHead), ResponseHead(ResponseHead), Body(List(U8)), Trailer(Header), MessageEnd, ProtocolSwitch, ProtocolData(List(U8))]

	## Incremental state. Retain only the latest state to release consumed bytes.
	Decoder : IncrementalState

	## Begin a request pipeline. Construction does not read a source.
	request_decoder : ReadProps -> Decoder
	request_decoder = |props| incremental_initial(props, Requests, [].iter())

	## Begin responses associated with request methods in encounter order.
	## Informational responses retain the current method. A final response
	## consumes it; unused methods are not pulled at source EOF.
	response_decoder : Iter(Method), ReadProps -> Decoder
	response_decoder = |methods, props| incremental_initial(props, Responses, methods)

	## Consume bytes until one event, failure, or a request for more input.
	## Broken framing and resource limits are terminal; records are never skipped.
	step : Decoder, List(U8), Streaming.InputEnd -> Streaming.Step(Decoder, Event, Error)
	step = incremental_step

	## Lazily read a pure request pipeline from arbitrary byte chunks.
	iter_requests : Iter(List(U8)), ReadProps -> Iter(Try(Streaming.ReadEvent(Event, Error), Streaming.ReadError(Error)))
	iter_requests = |chunks, props| Streaming.iter(chunks, request_decoder(props), step, {})

	## Lazily read effectful request chunks, preserving source errors.
	stream_requests : Stream(Try(List(U8), source_error)), ReadProps -> _
	stream_requests = |chunks, props| Streaming.stream(chunks, request_decoder(props), step, {})

	## Read pure response chunks using the matching request method queue.
	iter_responses : Iter(List(U8)), Iter(Method), ReadProps -> Iter(Try(Streaming.ReadEvent(Event, Error), Streaming.ReadError(Error)))
	iter_responses = |chunks, methods, props| Streaming.iter(chunks, response_decoder(methods, props), step, {})

	## Read effectful response chunks using the matching request method queue.
	stream_responses : Stream(Try(List(U8), source_error)), Iter(Method), ReadProps -> _
	stream_responses = |chunks, methods, props| Streaming.stream(chunks, response_decoder(methods, props), step, {})

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

IncrementalMode : [Requests, Responses]

IncrementalPhase : [Head, Fixed(U64), CloseBody, ChunkSize, ChunkData(U64), ChunkCR, ChunkLF, Trailers, Complete, Switch, Protocol, Closed]

IncrementalContinuation : [NextMessage, SwitchProtocol, EndSource]

IncrementalState : {
	props : HTTP.ReadProps,
	mode : IncrementalMode,
	methods : Iter(HTTP.Method),
	method : Try(HTTP.Method, [Missing]),
	phase : IncrementalPhase,
	continuation : IncrementalContinuation,
	informational : Bool,
	offset : U64,
	section_start : U64,
	buffer : Bytes,
	line_bytes : U64,
	saw_cr : Bool,
	section_bytes : U64,
	lines : U64,
}

incremental_initial : HTTP.ReadProps, IncrementalMode, Iter(HTTP.Method) -> IncrementalState
incremental_initial = |props, mode, methods| {
	{ props, mode, methods, method: Err(Missing), phase: Head, continuation: NextMessage, informational: False, offset: 0, section_start: 0, buffer: [], line_bytes: 0, saw_cr: False, section_bytes: 0, lines: 0 }
}

incremental_failed : IncrementalState, U64, U64, Str -> Streaming.Step(IncrementalState, HTTP.Event, HTTP.Error)
incremental_failed = |state, consumed, offset, message| { state: { ..state, phase: Closed, buffer: [], methods: [].iter() }, consumed, outcome: Failed({ offset, message }) }

incremental_failure : IncrementalState, U64, HTTP.Error -> Streaming.Step(IncrementalState, HTTP.Event, HTTP.Error)
incremental_failure = |state, consumed, error| incremental_failed(state, consumed, error.offset, error.message)

# Each wire byte is scanned once. Complete bounded heads/lines are then parsed
# once by the same grammar and framing functions as the complete-input API.
incremental_step : IncrementalState, Bytes, Streaming.InputEnd -> Streaming.Step(IncrementalState, HTTP.Event, HTTP.Error)
incremental_step = |initial, bytes, end| {
	var $state = initial
	var $used = 0
	if initial.props.max_body_chunk_bytes == 0 {
		return incremental_failed($state, 0, $state.offset, "max_body_chunk_bytes must be positive")
	}
	while True {
		match $state.phase {
			Complete => {
				phase = match $state.continuation {
					NextMessage => Head
					SwitchProtocol => Switch
					EndSource => Closed
				}
				$state = { ..$state, phase, method: if ($state.informational) $state.method else Err(Missing), buffer: [], line_bytes: 0, saw_cr: False, section_bytes: 0, lines: 0, section_start: $state.offset }
				return { state: $state, consumed: $used, outcome: Item(MessageEnd) }
			}
			Switch => {
				$state = { ..$state, phase: Protocol, methods: [].iter() }
				return { state: $state, consumed: $used, outcome: Item(ProtocolSwitch) }
			}
			Closed => {
				if $used < bytes.len() {
					return incremental_failed($state, $used, $state.offset, "bytes after a closed HTTP source")
				}
				return { state: $state, consumed: $used, outcome: if (end == Final) Done else NeedInput }
			}
			Fixed(0) => {
				$state = { ..$state, phase: Complete }
			}
			ChunkData(0) => {
				$state = { ..$state, phase: ChunkCR }
			}
			Fixed(remaining) | ChunkData(remaining) => {
				if $used == bytes.len() {
					if end == Final {
						return incremental_failed($state, $used, $state.offset, "source ended before the body was complete")
					}
					return { state: $state, consumed: $used, outcome: NeedInput }
				}
				available = bytes.len() - $used
				count = U64.min(U64.min(remaining, available), $state.props.max_body_chunk_bytes)
				part = bytes.sublist({ start: $used, len: count })
				phase = match $state.phase {
					Fixed(_) => Fixed(remaining - count)
					_ => ChunkData(remaining - count)
				}
				$state = { ..$state, phase, offset: $state.offset + count }
				$used = $used + count
				return { state: $state, consumed: $used, outcome: Item(Body(part)) }
			}
			CloseBody | Protocol => {
				if $used == bytes.len() {
					if end == More {
						return { state: $state, consumed: $used, outcome: NeedInput }
					}
					if $state.phase == Protocol {
						return { state: { ..$state, phase: Closed }, consumed: $used, outcome: Done }
					}
					$state = { ..$state, phase: Complete, continuation: EndSource }
				} else {
					count = U64.min(bytes.len() - $used, $state.props.max_body_chunk_bytes)
					part = bytes.sublist({ start: $used, len: count })
					event = if ($state.phase == Protocol) ProtocolData(part) else Body(part)
					$state = { ..$state, offset: $state.offset + count }
					$used = $used + count
					return { state: $state, consumed: $used, outcome: Item(event) }
				}
			}
			ChunkCR | ChunkLF => {
				if $used == bytes.len() {
					if end == Final {
						return incremental_failed($state, $used, $state.offset, "source ended before chunk data CRLF")
					}
					return { state: $state, consumed: $used, outcome: NeedInput }
				}
				expected = if ($state.phase == ChunkCR) '\r' else '\n'
				if bytes.get($used) != Ok(expected) {
					return incremental_failed($state, $used, $state.offset, "chunk data must be followed by CRLF")
				}
				phase = if ($state.phase == ChunkCR) ChunkLF else ChunkSize
				$state = { ..$state, phase, offset: $state.offset + 1, section_start: $state.offset + 1 }
				$used = $used + 1
			}
			Head | ChunkSize | Trailers => {
				if $used == bytes.len() {
					if end == More {
						return { state: $state, consumed: $used, outcome: NeedInput }
					}
					if $state.phase == Head and $state.buffer.is_empty() and $state.method.is_err() {
						return { state: { ..$state, phase: Closed, methods: [].iter() }, consumed: $used, outcome: Done }
					}
					return incremental_failed($state, $used, $state.offset, "source ended before an HTTP head, chunk line, or trailer section was complete")
				}
				if $state.phase == Head and $state.mode == Responses and $state.method.is_err() {
					var $found = False
					while !$found {
						match $state.methods.next() {
							Done => return incremental_failed($state, $used, $state.offset, "response has no matching request method")
							Skip({ rest }) => {
								$state = { ..$state, methods: rest }
							}
							One({ item, rest }) => {
								$state = { ..$state, methods: rest, method: Ok(item) }
								$found = True
							}
						}
					}
				}
				byte = bytes.get($used) ?? 0
				if $state.saw_cr and byte != '\n' {
					return incremental_failed($state, $used, $state.offset - 1, "CR not followed by LF")
				}
				if byte == '\n' and !$state.saw_cr {
					return incremental_failed($state, $used, $state.offset, "LF not preceded by CR")
				}
				section_limit = if ($state.phase == Head) $state.props.max_head_bytes else if ($state.phase == Trailers) $state.props.max_trailer_bytes else U64.highest
				if $state.section_bytes >= section_limit {
					return incremental_failed($state, $used, $state.offset, "HTTP head, chunk line, or trailer section exceeds its byte limit")
				}
				if byte != '\r' and byte != '\n' and $state.line_bytes >= $state.props.max_line_bytes {
					return incremental_failed($state, $used, $state.offset, "HTTP line exceeds max_line_bytes")
				}
				$state = { ..$state, offset: $state.offset + 1, section_bytes: $state.section_bytes + 1, buffer: $state.buffer.append(byte) }
				$used = $used + 1
				if byte == '\r' {
					$state = { ..$state, saw_cr: True }
				} else if byte != '\n' {
					$state = { ..$state, line_bytes: $state.line_bytes + 1 }
				} else {
					line_length = $state.line_bytes
					$state = { ..$state, saw_cr: False, line_bytes: 0 }
					match $state.phase {
						Head => {
							if line_length == 0 {
								match incremental_head($state) {
									Err(error) => return incremental_failure($state, $used, error)
									Ok({ state, event }) => return { state, consumed: $used, outcome: Item(event) }
								}
							} else {
								$state = { ..$state, lines: $state.lines + 1 }
								if $state.lines - 1 > $state.props.max_headers {
									return incremental_failed($state, $used, $state.offset - line_length - 2, "HTTP head exceeds max_headers")
								}
							}
						}
						ChunkSize => {
							match shift(parse_chunk_header($state.buffer.drop_last(2)), $state.section_start) {
								Err(error) => return incremental_failure($state, $used, error)
								Ok(size) => {
									$state = { ..$state, phase: if (size == 0) Trailers else ChunkData(size), buffer: [], section_start: $state.offset, section_bytes: 0, lines: 0 }
								}
							}
						}
						Trailers => {
							if line_length == 0 {
								$state = { ..$state, phase: Complete, buffer: [] }
							} else {
								if $state.lines >= $state.props.max_trailers {
									return incremental_failed($state, $used, $state.section_start, "HTTP trailer section exceeds max_trailers")
								}
								match shift(parse_field_line($state.buffer.drop_last(2)), $state.section_start) {
									Err(error) => return incremental_failure($state, $used, error)
									Ok(parsed) => {
										if parsed.kind.is_ok() {
											return incremental_failed($state, $used, $state.section_start, "Host and framing fields are forbidden in trailers")
										}
										$state = { ..$state, buffer: [], section_start: $state.offset, lines: $state.lines + 1 }
										return { state: $state, consumed: $used, outcome: Item(Trailer(parsed.header)) }
									}
								}
							}
						}
						_ => {}
					}
				}
			}
		}
	}
}

incremental_head : IncrementalState -> Try({ state : IncrementalState, event : HTTP.Event }, HTTP.Error)
incremental_head = |state| {
	{ line, rest } = shift(take_line(state.buffer), state.section_start)?
	head = shift(parse_fields(rest, line.len() + 2, { headers: [], fields: [], rest: [] }), state.section_start)?
	if state.mode == Requests {
		start = shift(parse_request_line(line), state.section_start)?
		hosts = values_named(head.fields, Host)
		if hosts.len() > 1 {
			return fail(state.section_start + (hosts.get(1) ?? { value: [], offset: 0 }).offset, "more than one Host field")
		}
		if hosts.is_empty() and at_least_1_1(start.version) {
			return fail(state.section_start, "an HTTP/1.1 request needs a Host field")
		}
		body_framing = shift(framing(head.fields, start.version), state.section_start)?
		phase = match body_framing {
			Length(n) => Fixed(n)
			Chunked => ChunkSize
			Unframed => Complete
		}
		Ok({ state: { ..state, phase, buffer: [], section_start: state.offset, section_bytes: 0, lines: 0, informational: False, continuation: NextMessage }, event: RequestHead({ method: start.method, target: start.target, version: start.version, headers: head.headers }) })
	} else {
		start = shift(parse_status_line(line), state.section_start)?
		body_framing = shift(framing(head.fields, start.version), state.section_start)?
		code = start.status_code
		method = state.method ?? Get
		switched = code == 101 or (method == Connect and code >= 200 and code < 300)
		informational = code < 200 and code != 101
		no_body = method == Head or code < 200 or code == 204 or code == 304 or switched
		phase = if (no_body) Complete else match body_framing {
			Length(n) => Fixed(n)
			Chunked => ChunkSize
			Unframed => CloseBody
		}
		Ok({ state: { ..state, phase, buffer: [], section_start: state.offset, section_bytes: 0, lines: 0, informational, continuation: if (switched) SwitchProtocol else NextMessage }, event: ResponseHead({ version: start.version, status_code: code, reason: start.reason, headers: head.headers }) })
	}
}

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
read_request = |input| read_request_metadata(input).map_ok(|parsed| { value: parsed.value, rest: parsed.rest })

# Preserve framing fields as a private sidecar for quotation linting.
read_request_metadata : Bytes -> Try({ value : HTTP.Request, rest : Bytes, fields : List(Field) }, Failure)
read_request_metadata = |input| {
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
		Ok({ value: { method: start.method, target: start.target, version, headers: head.headers, body: framed.body }, rest: framed.rest, fields: head.fields })
	}
}

read_response : Bytes -> Try({ value : HTTP.Response, rest : Bytes }, Failure)
read_response = |input| read_response_metadata(input).map_ok(|parsed| { value: parsed.value, rest: parsed.rest })

read_response_metadata : Bytes -> Try({ value : HTTP.Response, rest : Bytes, fields : List(Field) }, Failure)
read_response_metadata = |input| {
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
	Ok({ value: { version, status_code: code, reason: start.reason, headers: head.headers, body: framed.body }, rest: framed.rest, fields: head.fields })
}

quotation_error : HTTP.ValidationProps, Failure -> [BadQuotedBytes(Str)]
quotation_error = |props, error| {
	position = Quotation.position(props.source, error.offset)
	Quotation.error({ name: props.name, format: "HTTP", message: error.message, line: position.line, column: position.column })
}

validate_message : HTTP.ValidationProps, Bytes, List(Field) -> Try({}, [BadQuotedBytes(Str)])
validate_message = |props, rest, fields| {
	if !rest.is_empty() {
		return Err(quotation_error(props, { offset: props.source.to_utf8().len() - rest.len(), message: "unexpected content after the HTTP message" }))
	}
	if props.lints.repeated_content_length {
		lengths = values_named(fields, ContentLength)
		if lengths.len() > 1 {
			return Err(quotation_error(props, { offset: (lengths.get(1) ?? { value: [], offset: 0 }).offset, message: "repeated_content_length: repeated Content-Length field" }))
		}
		match lengths.first() {
			Ok(field) if elements(field.value).len() > 1 => return Err(quotation_error(props, { offset: field.offset, message: "repeated_content_length: Content-Length contains repeated values" }))
			_ => {}
		}
	}
	Ok({})
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
			fail(if (te.offset > cl.offset) te.offset else cl.offset, "both Transfer-Encoding and Content-Length")
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
		[] => if (bytes.is_empty()) Ok({}) else fail(line_len - bytes.len(), "whitespace after chunk size")
		[';', .. as after_semicolon] => {
			name_onwards = trim_start_ows(after_semicolon)
			name_len = Utf8.skip_class(name_onwards, 0, tchar_class)
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
			token_len = Utf8.skip_class(bytes, 0, tchar_class)
			if (token_len == 0) fail(0, "chunk extension needs a value after =") else Ok(token_len)
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
to_lower = |byte| if (byte >= 'A' and byte <= 'Z') byte + 32 else byte

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

# Complete quotation adapters preserve ordinary HTTP framing behavior.
expect {
	request : HTTP.RequestLiteral
	request = "GET /quoted HTTP/1.1\r\nHost: a\r\n\r\n"
	response : HTTP.ResponseLiteral
	response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi"
	request.value.target == "/quoted" and response.value.body == "hi".to_utf8()
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 0\r\nContent-Length: 0\r\n\r\n", name: "request.http" }) {
	Err(BadQuotedBytes(message)) => message.contains("request.http:4:1: HTTP: repeated_content_length")
	Ok(_) => False
}
expect HTTP.validate_response({ source: "HTTP/1.1 200 OK\r\nContent-Length: 0, 0\r\n\r\n", lints: { repeated_content_length: False } }).is_ok()
expect match HTTP.validate_response({ source: "HTTP/1.1 200 OK\r\nContent-Length: 0, 0\r\n\r\n" }) {
	Err(BadQuotedBytes(message)) => message.contains("repeated_content_length")
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 0, 1\r\n\r\n", lints: { repeated_content_length: False } }) {
	Err(BadQuotedBytes(message)) => message.contains("conflicting Content-Length")
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /next HTTP/1.1\r\nHost: a\r\n\r\n" }) {
	Err(BadQuotedBytes(message)) => message.contains("unexpected content after the HTTP message")
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\nHost: a\n\n" }) {
	Err(BadQuotedBytes(message)) => message.contains("LF not preceded by CR")
	Ok(_) => False
}

# Normalize only transport-dependent body fragmentation; heads, trailers,
# completion markers and errors must remain in exactly the same order.
HttpTestResult : [Event(HTTP.Event), Finished, Failed(Streaming.ReadError(HTTP.Error))]

normalize_http_test : Iter(Try(Streaming.ReadEvent(HTTP.Event, HTTP.Error), Streaming.ReadError(HTTP.Error))) -> List(HttpTestResult)
normalize_http_test = |input| {
	var $out = []
	for result in input {
		match result {
			Ok(Value(Body(part))) => {
				match $out.last() {
					Ok(Event(Body(before))) => {
						$out = $out.drop_last(1).append(Event(Body(before.concat(part))))
					}
					_ => {
						$out = $out.append(Event(Body(part)))
					}
				}
			}
			Ok(Value(ProtocolData(part))) => {
				match $out.last() {
					Ok(Event(ProtocolData(before))) => {
						$out = $out.drop_last(1).append(Event(ProtocolData(before.concat(part))))
					}
					_ => {
						$out = $out.append(Event(ProtocolData(part)))
					}
				}
			}
			Ok(Value(event)) => {
				$out = $out.append(Event(event))
			}
			Ok(Finished(_)) => {
				$out = $out.append(Finished)
			}
			Err(error) => {
				$out = $out.append(Failed(error))
			}
			Ok(Rejected(error)) => {
				$out = $out.append(Failed(ParseErr(error)))
			}
		}
	}
	$out
}

http_split_requests : Str -> Bool
http_split_requests = |text| {
	bytes = text.to_utf8()
	want = normalize_http_test(HTTP.iter_requests([bytes].iter(), {}))
	var $same = normalize_http_test(HTTP.iter_requests(bytes.map(|byte| [byte]).iter(), {})) == want
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [[], bytes.take_first(split), [], bytes.drop_first(split), []]
		if normalize_http_test(HTTP.iter_requests(parts.iter(), {})) != want {
			$same = False
		}
	}
	$same
}

http_split_responses : Str, List(HTTP.Method) -> Bool
http_split_responses = |text, methods| {
	bytes = text.to_utf8()
	want = normalize_http_test(HTTP.iter_responses([bytes].iter(), methods.iter(), {}))
	var $same = normalize_http_test(HTTP.iter_responses(bytes.map(|byte| [byte]).iter(), methods.iter(), {})) == want
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [[], bytes.take_first(split), [], bytes.drop_first(split), []]
		if normalize_http_test(HTTP.iter_responses(parts.iter(), methods.iter(), {})) != want {
			$same = False
		}
	}
	$same
}

expect {
	text = "POST /one HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nhelloGET /two HTTP/1.1\r\nHost: a\r\n\r\n"
	events = Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), {}))?
	events == [RequestHead({ method: Post, target: "/one", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }, { name: "Content-Length", value: "5" }] }), Body("hello".to_utf8()), MessageEnd, RequestHead({ method: Get, target: "/two", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }] }), MessageEnd]
		and http_split_requests(text)
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5;a=b;c=\"q\\\"\"\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n"
	events = Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), {}))?
	events.drop_first(1) == [Body("hello".to_utf8()), Body(" world".to_utf8()), Trailer({ name: "X-Trailer", value: "t" }), MessageEnd]
		and http_split_requests(text)
}

# All incomplete self-delimiting messages fail rather than finalizing at EOF.
expect [
	"GET / HTTP/1.1\r\nHost: a\r\n\r",
	"POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\n\r\nab",
	"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nab",
	"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r",
	"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nX: y\r\n",
].all(|text| Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), {})).is_err() and http_split_requests(text))

expect [
	"GET / HTTP/1.1\r\nHost: a\r\n\r\ninvalid suffix",
	"GET / HTTP/1.1\nHost: a\r\n\r\n",
	"GET / HTTP/1.1\rX",
	"GET / HTTP/1.1\r\n\r\n",
	"GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n",
	"GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 1\r\nTransfer-Encoding: chunked\r\n\r\n",
	"GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 1, 2\r\n\r\n",
	"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n1;=bad\r\na\r\n0\r\n\r\n",
	"POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nContent-Length: 2\r\n\r\n",
].all(|text| Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), {})).is_err() and http_split_requests(text))

expect {
	text = "HTTP/1.1 200 OK\r\nContent-Length: 999\r\n\r\nHTTP/1.1 204 Empty\r\n\r\nHTTP/1.1 304 Cached\r\n\r\n"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Head, Get, Get].iter(), {}))?
	events.len() == 6 and events.fold(0.U64, |count, event| count + if (event == MessageEnd) 1 else 0) == 3 and http_split_responses(text, [Head, Get, Get])
}

expect {
	text = "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 103 Early Hints\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Post].iter(), {}))?
	events.fold(0.U64, |count, event| count + if (event == MessageEnd) 1 else 0) == 3 and events.drop_first(5) == [Body("hi".to_utf8()), MessageEnd] and http_split_responses(text, [Post])
}

expect {
	text = "HTTP/1.1 200 OK\r\n\r\nbody\r\nHTTP/1.1 is body text"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Get].iter(), {}))?
	events.drop_first(1) == [Body("body\r\nHTTP/1.1 is body text".to_utf8()), MessageEnd] and http_split_responses(text, [Get])
}

expect {
	text = "HTTP/1.1 200 Connected\r\n\r\ntunnel\r\nnot HTTP"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Connect].iter(), {}))?
	events.drop_first(1) == [MessageEnd, ProtocolSwitch, ProtocolData("tunnel\r\nnot HTTP".to_utf8())] and http_split_responses(text, [Connect])
}

expect {
	text = "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n\r\nraw"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Get].iter(), {}))?
	events.drop_first(1) == [MessageEnd, ProtocolSwitch, ProtocolData("raw".to_utf8())] and http_split_responses(text, [Get])
}

expect {
	text = "HTTP/1.1 403 Forbidden\r\nContent-Length: 2\r\n\r\nnoHTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
	Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Connect, Get].iter(), {})).is_ok() and http_split_responses(text, [Connect, Get])
}

expect {
	text = "HTTP/1.1 100 Continue\r\n\r\n"
	Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Post].iter(), {})).is_err() and http_split_responses(text, [Post])
}

expect {
	bytes = "HTTP/1.1 204 Empty\r\n\r\n".to_utf8()
	Streaming.collect(HTTP.iter_responses([bytes].iter(), [].iter(), {})).is_err()
		and Streaming.collect(HTTP.iter_responses([bytes.concat(bytes)].iter(), [Get].iter(), {})).is_err()
}

expect {
	bytes = "GET / HTTP/1.1\r\nHost: a\r\n\r\n".to_utf8()
	Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_head_bytes: bytes.len() })).is_ok()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_head_bytes: bytes.len() - 1 })).is_err()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_line_bytes: 14, max_headers: 1 })).is_ok()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_line_bytes: 13 })).is_err()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_headers: 0 })).is_err()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_headers: U64.highest, max_line_bytes: U64.highest })).is_ok()
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nX: y\r\n\r\n"
	bytes = text.to_utf8()
	Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_trailer_bytes: 8, max_trailers: 1 })).is_ok()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_trailer_bytes: 7 })).is_err()
		and Streaming.collect(HTTP.iter_requests([bytes].iter(), { max_trailers: 0 })).is_err()
}

expect {
	text = "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n0123456789"
	events = Streaming.collect(HTTP.iter_responses([text.to_utf8()].iter(), [Get].iter(), { max_body_chunk_bytes: 3 }))?
	events.drop_first(1) == [Body("012".to_utf8()), Body("345".to_utf8()), Body("678".to_utf8()), Body("9".to_utf8()), MessageEnd]
		and Streaming.collect(HTTP.iter_requests([[]].iter(), { max_body_chunk_bytes: 0 })).is_err()
}

expect {
	# A consumer can stop immediately after the head without pulling any body
	# source chunks or the next request context.
	chunks = Iter.custom(False, Err(Unknown), |started| if (started) crash "pulled body source before demand" else Ok(("HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\n".to_utf8(), True)))
	methods = Iter.custom(False, Err(Unknown), |started| if (started) crash "pulled next method before demand" else Ok((Get, True)))
	match HTTP.iter_responses(chunks, methods, {}).next() {
		One({ item: Ok(Value(ResponseHead(head))), .. }) => head.status_code == 200
		_ => False
	}
}

expect {
	# Completion validates only consumed responses; surplus context is lazy.
	methods = Iter.custom({}, Err(Unknown), |_| crash "pulled unused context at EOF")
	Streaming.collect(HTTP.iter_responses([[], []].iter(), methods, {})) == Ok([])
}

expect {
	chunks = Iter.custom({}, Err(Unknown), |_| crash "read source with invalid fragment limit")
	match HTTP.iter_requests(chunks, { max_body_chunk_bytes: 0 }).next() {
		One({ item: Err(ParseErr(_)), .. }) => True
		_ => False
	}
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n1;abcdefghijklmnop=abcdefghijklmnop\r\nx\r\n0\r\n\r\n"
	Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), { max_line_bytes: 35 })).is_ok()
		and Streaming.collect(HTTP.iter_requests([text.to_utf8()].iter(), { max_line_bytes: 34 })).is_err()
}
