import Diagnostic
import Parser
import Streaming

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
		from_quote = |source| HTTP.validate_request({ source: source }).map_err(|InvalidHttp(error)| quote_http_error(source, error)).map_ok(|value| { value: value })
	}

	## A complete checked response, available through `.value`.
	ResponseLiteral := { value : Response }.{

		## Parse and validate a complete response during literal conversion.
		from_quote : Str -> Try(ResponseLiteral, [BadQuotedBytes(Str)])
		from_quote = |source| HTTP.validate_response({ source: source }).map_err(|InvalidHttp(error)| quote_http_error(source, error)).map_ok(|value| { value: value })
	}

	## Parse exactly one request, then check enabled lint rules.
	validate_request : ValidationProps -> Try(Request, [InvalidHttp(Error)])
	validate_request = |props| {
		parsed = read_request_metadata(props.source.to_utf8()).map_err(|error| InvalidHttp(named_http_error(props.name, error)))?
		validate_message(props, parsed.rest, parsed.fields)?
		Ok(parsed.value)
	}

	## Parse exactly one response, then check enabled lint rules.
	validate_response : ValidationProps -> Try(Response, [InvalidHttp(Error)])
	validate_response = |props| {
		parsed = read_response_metadata(props.source.to_utf8()).map_err(|error| InvalidHttp(named_http_error(props.name, error)))?
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

	## Why a message was rejected: `problem` identifies the broken rule and
	## `offset` is the byte offset in the input where the problem is.
	##
	## Problems with the message as a whole, such as a missing `Host` field,
	## are at offset 0, the start of the message.
	Resource : [SectionBytes, LineBytes, Headers, Trailers]
	Problem : [LimitExceeded({ resource : Resource, max : U64, observed : Diagnostic.Observed }), DuplicateHost, MissingHost, CrWithoutLf, LfWithoutCr, MissingCrlf, MissingTarget, InvalidTarget, InvalidMethod, MissingMethod, InvalidVersion, InvalidStatusWidth, InvalidStatus, InvalidReasonCharacter, InvalidReasonEncoding, MissingStatus, InvalidFieldEncoding, ObsoleteFolding, WhitespaceBeforeColon, InvalidFieldCharacter, InvalidFieldName, TransferEncodingHttp10, ContentLengthOverflow, ConflictingFraming, UnsupportedTransferCoding, ConflictingContentLength, InvalidContentLength, IncompleteBody, IncompleteChunk, MissingChunkCrlf, InvalidChunkSize, ChunkSizeOverflow, WhitespaceAfterChunkSize, MissingExtensionName, InvalidExtension, MissingExtensionValue, InvalidQuotedPair, InvalidQuotedCharacter, UnclosedQuote, TrailingContent, RepeatedContentLength, RepeatedContentLengthValues, InvalidBodyChunkSize, SourceClosed, IncompleteSection, MissingRequestMethod, ForbiddenTrailer]
	Error : Diagnostic(Problem)

	## Bounds for incremental readers. Body fragments never exceed
	## `max_body_chunk_bytes`; heads and trailer sections have independent bounds.
	ReadProps := { max_head_bytes : U64 ?? 65_536, max_line_bytes : U64 ?? 8192, max_headers : U64 ?? 100, max_trailer_bytes : U64 ?? 65_536, max_trailers : U64 ?? 100, max_body_chunk_bytes : U64 ?? 65_536, errors : Streaming.RecoveryProps ?? {} }

	## A request head, emitted before any body bytes.
	RequestHead : { method : Method, target : Str, version : Version, headers : List(Header) }

	## A response head, emitted before any body bytes.
	ResponseHead : { version : Version, status_code : U16, reason : Str, headers : List(Header) }

	## `MessageEnd` completes one HTTP message, including informational responses.
	## `Finished` from Streaming completes the source. After a 101 response or a
	## successful CONNECT, `ProtocolSwitch` precedes uninterpreted `ProtocolData`.
	Event : [RequestHead(RequestHead), ResponseHead(ResponseHead), Body(List(U8)), Trailer(Header), MessageEnd, ProtocolSwitch, ProtocolData(List(U8))]

	## Configure request events for Streaming.read_iter or Streaming.read_stream.
	request_decoder : ReadProps -> Streaming.Decoder(IncrementalState, Event, [InvalidHttp(Error)])
	request_decoder = |props| Streaming.decoder({ initial_state: incremental_initial(props, Requests, [].iter()), step: |state, bytes, end| Streaming.map_step_error(incremental_step(state, bytes, end), |error| InvalidHttp(http_error(error))), errors: props.errors })

	## Associate responses with request methods in encounter order.
	## Informational responses retain the method; a final response consumes it.
	response_decoder : Iter(Method), ReadProps -> Streaming.Decoder(IncrementalState, Event, [InvalidHttp(Error)])
	response_decoder = |methods, props| Streaming.decoder({ initial_state: incremental_initial(props, Responses, methods), step: |state, bytes, end| Streaming.map_step_error(incremental_step(state, bytes, end), |error| InvalidHttp(http_error(error))), errors: props.errors })

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
			Err(error) => Err(InvalidHttp(http_error(error)))
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
			Err(error) => Err(InvalidHttp(http_error(error)))
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
	## `ParseError` preserving the native problem tag and whose
	## offset is where the problem is.
	##
	## ```roc
	## expect {
	##     text = "GET /hello HTTP/1.1\r\nHost: example.com\r\n\r\n"
	##     match (HTTP.request).parse_str(text) {
	##         Ok(req) => req.method == Get and HTTP.header(req.headers, "host") == Ok("example.com")
	##         Err(_) => False
	##     }
	## }
	## ```
	request : Parser(Parser.Utf8.Bytes, Request, Problem)
	request =
		Parser.custom(
			|input| {
				read_request(input).map_err(|{ offset, problem }| ParseError(Diagnostic.{ problem, location: InputOffset(offset), path: [] }))
			},
		)

	## A [Parser] for one HTTP response, like [HTTP.parse_response], for use
	## inside a larger parser.
	##
	## A failure is a `ParseError` with the native problem tag and byte offset.
	response : Parser(Parser.Utf8.Bytes, Response, Problem)
	response =
		Parser.custom(
			|input| {
				read_response(input).map_err(|{ offset, problem }| ParseError(Diagnostic.{ problem, location: InputOffset(offset), path: [] }))
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

incremental_failed : IncrementalState, U64, U64, HTTP.Problem -> Streaming.Step(IncrementalState, HTTP.Event, Failure)
incremental_failed = |state, consumed, offset, problem| { state: { ..state, phase: Closed, buffer: [], methods: [].iter() }, consumed, outcome: Failed({ offset, problem }) }

incremental_failure : IncrementalState, U64, Failure -> Streaming.Step(IncrementalState, HTTP.Event, Failure)
incremental_failure = |state, consumed, error| incremental_failed(state, consumed, error.offset, error.problem)

# Each wire byte is scanned once. Complete bounded heads/lines are then parsed
# once by the same grammar and framing functions as the complete-input API.
incremental_step : IncrementalState, Bytes, Streaming.InputEnd -> Streaming.Step(IncrementalState, HTTP.Event, Failure)
incremental_step = |initial, bytes, end| {
	var $state = initial
	var $used = 0
	if initial.props.max_body_chunk_bytes == 0 {
		return incremental_failed($state, 0, $state.offset, InvalidBodyChunkSize)
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
					return incremental_failed($state, $used, $state.offset, SourceClosed)
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
						return incremental_failed($state, $used, $state.offset, IncompleteBody)
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
						return incremental_failed($state, $used, $state.offset, MissingChunkCrlf)
					}
					return { state: $state, consumed: $used, outcome: NeedInput }
				}
				expected = if ($state.phase == ChunkCR) '\r' else '\n'
				if bytes.get($used) != Ok(expected) {
					return incremental_failed($state, $used, $state.offset, MissingChunkCrlf)
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
					return incremental_failed($state, $used, $state.offset, IncompleteSection)
				}
				if $state.phase == Head and $state.mode == Responses and $state.method.is_err() {
					var $found = False
					while !$found {
						match $state.methods.next() {
							Done => return incremental_failed($state, $used, $state.offset, MissingRequestMethod)
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
				# A complete head already present in the caller's chunk needs no
				# intermediate buffer or per-line decoder updates. The probe accepts
				# only CRLF heads within every metadata budget; other input follows
				# the general path so its exact framing diagnostics are preserved.
				if $state.phase == Head and $state.buffer.is_empty() and !$state.saw_cr {
					remaining = bytes.drop_first($used)
					match complete_head_end(remaining, $state.props) {
						Ok(length) => {
							$state = { ..$state, buffer: remaining.take_first(length), offset: $state.offset + length }
							$used = $used + length
							match incremental_head($state) {
								Ok({ state, event }) => return { state, consumed: $used, outcome: Item(event) }
								Err(error) => return incremental_failure($state, $used, error)
							}
						}
						Err(_) => {}
					}
				}
				# Ordinary metadata bytes can be framed a whole span at a time.
				# CRLF and every boundary/limit still pass through the same state
				# transitions below; field validation occurs before head emission.
				var $bulk = False
				if !$state.saw_cr {
					run_end = Parser.Utf8.find_line_end(bytes, $used)
					count = run_end - $used
					if count > 0 {
						section_limit = if ($state.phase == Head) $state.props.max_head_bytes else if ($state.phase == Trailers) $state.props.max_trailer_bytes else U64.highest
						line_room = $state.props.max_line_bytes - U64.min($state.line_bytes, $state.props.max_line_bytes)
						section_room = section_limit - U64.min($state.section_bytes, section_limit)
						accepted = U64.min(count, U64.min(line_room, section_room))
						if accepted > 0 {
							$state = { ..$state, offset: $state.offset + accepted, section_bytes: $state.section_bytes + accepted, line_bytes: $state.line_bytes + accepted, buffer: $state.buffer.concat(bytes.sublist({ start: $used, len: accepted })) }
							$used = $used + accepted
						}
						if accepted < count {
							problem = if ($state.section_bytes >= section_limit) LimitExceeded({ resource: SectionBytes, max: section_limit, observed: AtLeast((section_limit) + 1) }) else LimitExceeded({ resource: LineBytes, max: $state.props.max_line_bytes, observed: AtLeast(($state.props.max_line_bytes) + 1) })
							return incremental_failed($state, $used, $state.offset, problem)
						}
						$bulk = True
					}
				}
				if !$bulk {
					byte = bytes.get($used) ?? 0
					if $state.saw_cr and byte != '\n' {
						return incremental_failed($state, $used, $state.offset - 1, CrWithoutLf)
					}
					if byte == '\n' and !$state.saw_cr {
						return incremental_failed($state, $used, $state.offset, LfWithoutCr)
					}
					section_limit = if ($state.phase == Head) $state.props.max_head_bytes else if ($state.phase == Trailers) $state.props.max_trailer_bytes else U64.highest
					if $state.section_bytes >= section_limit {
						return incremental_failed($state, $used, $state.offset, LimitExceeded({ resource: SectionBytes, max: section_limit, observed: AtLeast((section_limit) + 1) }))
					}
					if byte != '\r' and byte != '\n' and $state.line_bytes >= $state.props.max_line_bytes {
						return incremental_failed($state, $used, $state.offset, LimitExceeded({ resource: LineBytes, max: $state.props.max_line_bytes, observed: AtLeast(($state.props.max_line_bytes) + 1) }))
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
										return incremental_failed($state, $used, $state.offset - line_length - 2, LimitExceeded({ resource: Headers, max: $state.props.max_headers, observed: AtLeast(($state.props.max_headers) + 1) }))
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
										return incremental_failed($state, $used, $state.section_start, LimitExceeded({ resource: Trailers, max: $state.props.max_trailers, observed: AtLeast(($state.props.max_trailers) + 1) }))
									}
									match shift(parse_field_line($state.buffer.drop_last(2)), $state.section_start) {
										Err(error) => return incremental_failure($state, $used, error)
										Ok(parsed) => {
											if parsed.kind.is_ok() {
												return incremental_failed($state, $used, $state.section_start, ForbiddenTrailer)
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
}

# Probe only a complete bounded CRLF head; incomplete or exceptional input
# retains the incremental byte driver's error positions and precedence.
complete_head_end : Bytes, HTTP.ReadProps -> Try(U64, [Unavailable])
complete_head_end = |bytes, props| {
	bounded = bytes.take_first(props.max_head_bytes)
	var $pos = 0.U64
	var $lines = 0.U64
	while $pos < bounded.len() {
		remaining = bounded.drop_first($pos)
		ordinary = U64.min(remaining.len(), props.max_line_bytes)
		window = remaining.take_first(ordinary + U64.min(2, remaining.len() - ordinary))
		end = Parser.Utf8.find_line_end(window, 0)
		if end > props.max_line_bytes or end >= window.len() {
			return Err(Unavailable)
		}
		if window.get(end) != Ok('\r') or window.get(end + 1) != Ok('\n') {
			return Err(Unavailable)
		}
		line_end = $pos + end
		next = line_end + 2
		if next > props.max_head_bytes {
			return Err(Unavailable)
		}
		if line_end == $pos {
			return Ok(next)
		}
		$lines = $lines + 1
		if $lines - 1 > props.max_headers {
			return Err(Unavailable)
		}
		$pos = next
	}
	Err(Unavailable)
}

incremental_head : IncrementalState -> Try({ state : IncrementalState, event : HTTP.Event }, Failure)
incremental_head = |state| {
	{ line, rest } = shift(take_line(state.buffer), state.section_start)?
	head = shift(parse_fields(rest, line.len() + 2, { headers: List.with_capacity(16), fields: [], rest: [] }), state.section_start)?
	if state.mode == Requests {
		start = shift(parse_request_line(line), state.section_start)?
		_ = shift(check_host(head.fields, start.version), state.section_start)?
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
Failure : { offset : U64, problem : HTTP.Problem }

# The value of a field that decides the framing or the Host check, with the
# byte offset of its line so that errors can point at it. Only these fields
# are recorded beside the headers, so the common case allocates nothing extra.
Field : { kind : FieldKind, value : Bytes, offset : U64 }

FieldKind : [Host, TransferEncoding, ContentLength]

# The parsed field lines of a head: every header, the fields that matter for
# framing, and the bytes after the empty line.
Head : { headers : List(HTTP.Header), fields : List(Field), rest : Bytes }

fail : U64, HTTP.Problem -> Try(a, Failure)
fail = |offset, problem| Err({ offset, problem })

# Move a failure's offset from a slice starting at `base` to the outer input.
shift : Try(a, Failure), U64 -> Try(a, Failure)
shift = |result, base| result.map_err(|{ offset, problem }| { offset: base + offset, problem })

# The requests in `bytes`, the end of an input `total` bytes long.
read_requests : Bytes, U64, List(HTTP.Request) -> Try(List(HTTP.Request), [InvalidHttp(HTTP.Error)])
read_requests = |bytes, total, requests| {
	if bytes.is_empty() {
		Ok(requests)
	} else {
		match shift(read_request(bytes), total - bytes.len()) {
			Ok({ value, rest }) => read_requests(rest, total, requests.append(value))
			Err(error) => Err(InvalidHttp(http_error(error)))
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
	head = parse_fields(rest, line.len() + 2, { headers: List.with_capacity(16), fields: [], rest: [] })?
	version = start.version
	check_host(head.fields, version)?
	{
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
	head = parse_fields(rest, line.len() + 2, { headers: List.with_capacity(16), fields: [], rest: [] })?
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

validate_message : HTTP.ValidationProps, Bytes, List(Field) -> Try({}, [InvalidHttp(HTTP.Error)])
validate_message = |props, rest, fields| {
	if !rest.is_empty() {
		return Err(InvalidHttp(named_http_error(props.name, { offset: props.source.to_utf8().len() - rest.len(), problem: TrailingContent })))
	}
	if props.lints.repeated_content_length {
		lengths = values_named(fields, ContentLength)
		if lengths.len() > 1 {
			return Err(InvalidHttp(named_http_error(props.name, { offset: (lengths.get(1) ?? { value: [], offset: 0 }).offset, problem: RepeatedContentLength })))
		}
		match lengths.first() {
			Ok(field) if elements(field.value).len() > 1 => return Err(InvalidHttp(named_http_error(props.name, { offset: field.offset, problem: RepeatedContentLengthValues })))
			_ => {}
		}
	}
	Ok({})
}

check_host : List(Field), HTTP.Version -> Try({}, Failure)
check_host = |fields, version| {
	var $found = False
	for field in fields {
		if field.kind == Host {
			if $found {
				return fail(field.offset, DuplicateHost)
			}
			$found = True
		}
	}
	if (!$found and at_least_1_1(version)) fail(0, MissingHost) else Ok({})
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
	var $index = Parser.Utf8.find_line_end(bytes, 0)
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\r' {
			return
				if bytes.get($index + 1) == Ok('\n') {
					Ok({ line: bytes.sublist({ start: 0, len: $index }), rest: bytes.drop_first($index + 2) })
				} else {
					fail($index, CrWithoutLf)
				}
		} else if byte == '\n' {
			return fail($index, LfWithoutCr)
		}
		$index = $index + 1
	}
	fail(bytes.len(), MissingCrlf)
}

# request-line = method SP request-target SP HTTP-version
parse_request_line : Bytes -> Try({ method : HTTP.Method, target : Str, version : HTTP.Version }, Failure)
parse_request_line = |line| {
	method_len = skip_tchar(line)
	method = to_method(line.sublist({ start: 0, len: method_len }))?
	match line.drop_first(method_len) {
		[' ', .. as target_onwards] => {
			target_start = method_len + 1
			target_len = prefix_len(target_onwards, is_vchar)
			if target_len == 0 {
				fail(target_start, MissingTarget)
			} else {
				match target_onwards.drop_first(target_len) {
					[' ', .. as version_bytes] => {
						version = shift(parse_version(version_bytes), target_start + target_len + 1)?
						target = Str.from_utf8(target_onwards.sublist({ start: 0, len: target_len })) ?? ""
						Ok({ method, target, version })
					}
					_ => fail(target_start + target_len, InvalidTarget)
				}
			}
		}
		_ => fail(method_len, InvalidMethod)
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
		[] => fail(0, MissingMethod)
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
		_ => fail(0, InvalidVersion)
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
					_ => fail(12, InvalidStatusWidth)
				}?
			valid_len = skip_field_bytes(phrase)
			if status_code < 100 {
				fail(9, InvalidStatus)
			} else if valid_len < phrase.len() {
				fail(13 + valid_len, InvalidReasonCharacter)
			} else {
				match Str.from_utf8(phrase) {
					Ok(reason) => Ok({ version, status_code, reason })
					Err(_) => fail(13, InvalidReasonEncoding)
				}
			}
		}
		_ => fail(8, MissingStatus)
	}
}

# Field lines up to and including the empty line that ends them. `base` is
# the offset of `bytes` in the message, recorded with each field.
parse_fields : Bytes, U64, Head -> Try(Head, Failure)
parse_fields = |bytes, base, initial| {
	var $pos = 0.U64
	var $headers = initial.headers
	var $fields = initial.fields
	while True {
		if bytes.get($pos) == Ok('\r') and bytes.get($pos + 1) == Ok('\n') {
			return Ok({ headers: $headers, fields: $fields, rest: bytes.drop_first($pos + 2) })
		}
		name_end = skip_tchar_at(bytes, $pos)
		if name_end == $pos or bytes.get(name_end) != Ok(':') {
			return parse_fields_slow(bytes.drop_first($pos), base + $pos, { headers: $headers, fields: $fields, rest: [] })
		}
		var $start = name_end + 1
		while $start < bytes.len() and is_ows(bytes.get($start) ?? 0) {
			$start = $start + 1
		}
		value_end = skip_field_bytes_at(bytes, $start)
		if bytes.get(value_end) != Ok('\r') or bytes.get(value_end + 1) != Ok('\n') {
			return parse_fields_slow(bytes.drop_first($pos), base + $pos, { headers: $headers, fields: $fields, rest: [] })
		}
		var $trimmed_end = value_end
		while $trimmed_end > $start and is_ows(bytes.get($trimmed_end - 1) ?? 0) {
			$trimmed_end = $trimmed_end - 1
		}
		name_bytes = bytes.sublist({ start: $pos, len: name_end - $pos })
		value = bytes.sublist({ start: $start, len: $trimmed_end - $start })
		text = Str.from_utf8(value).map_err(|_| { offset: base + $start, problem: InvalidFieldEncoding })?
		name = Str.from_utf8(name_bytes) ?? ""
		$headers = $headers.append({ name, value: text })
		match field_kind(name_bytes) {
			Ok(kind) => {
				$fields = $fields.append({ kind, value, offset: base + $pos })
			}
			Err(_) => {}
		}
		$pos = value_end + 2
	}
}

# Exceptional lines keep the complete parser's diagnostics and error ordering.
parse_fields_slow : Bytes, U64, Head -> Try(Head, Failure)
parse_fields_slow = |bytes, base, head| {
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
	name_len = skip_tchar(line)
	match line.drop_first(name_len) {
		[' ', ..] | ['\t', ..] if name_len == 0 => fail(0, ObsoleteFolding)
		[' ', ..] | ['\t', ..] => fail(name_len, WhitespaceBeforeColon)
		[':', .. as after_colon] if name_len > 0 => {
			value_start = name_len + 1 + prefix_len(after_colon, is_ows)
			value = trim_ows(after_colon)
			valid_len = skip_field_bytes(value)
			if valid_len == value.len() {
				name = Str.from_utf8(line.sublist({ start: 0, len: name_len })) ?? ""
				match Str.from_utf8(value) {
					Ok(text) => Ok({ header: { name, value: text }, kind: field_kind(line.sublist({ start: 0, len: name_len })), value })
					Err(_) => fail(value_start, InvalidFieldEncoding)
				}
			} else {
				fail(value_start + valid_len, InvalidFieldCharacter)
			}
		}
		_ => fail(name_len, InvalidFieldName)
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
	var $start = 0.U64
	var $index = 0.U64
	while $index < bytes.len() {
		if bytes.get($index) == Ok(',') {
			$parts = $parts.append(trim_ows(bytes.sublist({ start: $start, len: $index - $start })))
			$start = $index + 1
		}
		$index = $index + 1
	}
	$parts.append(trim_ows(bytes.drop_first($start)))
}

# How the body is delimited (RFC 9112 section 6.3), rejecting ambiguity.
# Failures point at the field line responsible.
framing : List(Field), HTTP.Version -> Try(Framing, Failure)
framing = |fields, version| match fields {
	[] => Ok(Unframed)
	[field] => framing_single(field, version)
	[first, second] if first.kind == Host => framing_single(second, version)
	[first, second] if second.kind == Host => framing_single(first, version)
	_ => framing_general(fields, version)
}

# A single framing field needs no filtered lists or comma-element allocations.
framing_single : Field, HTTP.Version -> Try(Framing, Failure)
framing_single = |field, version| match field.kind {
	Host => Ok(Unframed)
	TransferEncoding => {
		if !at_least_1_1(version) {
			return fail(field.offset, TransferEncodingHttp10)
		}
		if (ascii_eq_lower(field.value, "chunked")) Ok(Chunked) else framing_general([field], version)
	}
	ContentLength => {
		if field.value.is_empty() or !field.value.all(is_digit) {
			return framing_general([field], version)
		}
		significant = field.value.drop_first(prefix_len(field.value, |byte| byte == '0'))
		if significant.len() > 19 {
			return fail(field.offset, ContentLengthOverflow)
		}
		Ok(Length(significant.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())))
	}
}

# Repeated fields and comma lists retain the exact spelling and conflict checks.
framing_general : List(Field), HTTP.Version -> Try(Framing, Failure)
framing_general = |fields, version| {
	transfer_encodings = values_named(fields, TransferEncoding)
	content_lengths = values_named(fields, ContentLength)
	match (transfer_encodings.first(), content_lengths.first()) {
		(Ok(te), Ok(cl)) =>
		# RFC 9112 6.1 allows a recipient to process Transfer-Encoding over
		# Content-Length, but a message with both "ought to be handled as an
		# error"; llhttp rejects it by default, as does this parser.
			fail(if (te.offset > cl.offset) te.offset else cl.offset, ConflictingFraming)
		(Ok(te), Err(_)) => {
			if !at_least_1_1(version) {
				return fail(te.offset, TransferEncodingHttp10)
			}
			if transfer_encodings.len() == 1 and ascii_eq_lower(te.value, "chunked") {
				return Ok(Chunked)
			}
			codings =
				transfer_encodings
					.fold([], |all, field| all.concat(elements(field.value)))
					.map(|coding| coding.map(to_lower))
			if codings == [['c', 'h', 'u', 'n', 'k', 'e', 'd']] {
				# Exactly one element: empty list elements (", chunked") are
				# rejected like h11 does, since intermediaries disagree on them.
				Ok(Chunked)
			} else {
				fail(te.offset, UnsupportedTransferCoding)
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
	if fields.len() == 1 and !first.value.is_empty() and first.value.all(is_digit) {
		significant = first.value.drop_first(prefix_len(first.value, |byte| byte == '0'))
		if significant.len() > 19 {
			return fail(first.offset, ContentLengthOverflow)
		}
		return Ok(Length(significant.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())))
	}
	wanted = elements(first.value).first() ?? []
	match fields.find_first(|field| elements(field.value).any(|value| value != wanted)) {
		Ok(field) => fail(field.offset, ConflictingContentLength)
		Err(_) =>
			if wanted.is_empty() or !wanted.all(is_digit) {
				fail(first.offset, InvalidContentLength)
			} else {
				significant = wanted.drop_first(prefix_len(wanted, |byte| byte == '0'))
				if significant.len() > 19 {
					fail(first.offset, ContentLengthOverflow)
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
				fail(base + bytes.len(), IncompleteBody)
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
		trailers = parse_fields(rest, data_start, { headers: List.with_capacity(16), fields: [], rest: [] })?
		Ok({ body, rest: trailers.rest })
	} else if size > rest.len() {
		fail(data_start + rest.len(), IncompleteChunk)
	} else {
		match rest.drop_first(size) {
			['\r', '\n', .. as next] => decode_chunked(next, data_start + size + 2, body.concat(rest.sublist({ start: 0, len: size })))
			_ => fail(data_start + size, MissingChunkCrlf)
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
		fail(0, InvalidChunkSize)
	} else if significant.len() > 15 {
		fail(0, ChunkSizeOverflow)
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
		[] => if (bytes.is_empty()) Ok({}) else fail(line_len - bytes.len(), WhitespaceAfterChunkSize)
		[';', .. as after_semicolon] => {
			name_onwards = trim_start_ows(after_semicolon)
			name_len = skip_tchar(name_onwards)
			if name_len == 0 {
				fail(line_len - name_onwards.len(), MissingExtensionName)
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
		other => fail(line_len - other.len(), InvalidExtension)
	}
}

# chunk-ext-val = token / quoted-string
chunk_ext_value_len : Bytes -> Try(U64, Failure)
chunk_ext_value_len = |bytes| {
	match bytes {
		['"', ..] => quoted_string_len(bytes)
		_ => {
			token_len = skip_tchar(bytes)
			if (token_len == 0) fail(0, MissingExtensionValue) else Ok(token_len)
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
				return fail($index, InvalidQuotedPair)
			}
		} else if byte == '\t' or byte == ' ' or is_vchar(byte) or byte >= 0x80 {
			$index = $index + 1
		} else {
			return fail($index, InvalidQuotedCharacter)
		}
	}
	fail(bytes.len(), UnclosedQuote)
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
# Fixed HTTP classes avoid passing the generic scanner's membership table.
# The nibble masks encode RFC 9110 tchar exactly; the scalar tail uses the
# same predicate, checked exhaustively below.
skip_tchar : Bytes -> U64
skip_tchar = |bytes| skip_tchar_at(bytes, 0)

skip_tchar_at : Bytes, U64 -> U64
skip_tchar_at = |bytes, start| {
	lo_table = U8x16.from_u128_bits(0x1C3D1534153D3E3E3F3F3F3F3F3E3F3A)
	hi_table = U8x16.from_u128_bits(0x2010080402010000)
	var $index = start
	while $index + 16 <= bytes.len() {
		v = U8x16.load(bytes, $index) ?? U8x16.splat(0)
		lo = U8x16.table_lookup(lo_table, v.bitwise_and(U8x16.splat(15)))
		hi = U8x16.table_lookup(hi_table, v.shr_zf_wrap(4))
		outside = U8x16.eq_lanes(lo.bitwise_and(hi), U8x16.splat(0)).to_bitmask()
		if outside != 0 {
			return $index + outside.count_trailing_zero_bits().to_u64()
		}
		$index = $index + 16
	}
	while $index < bytes.len() and is_tchar(bytes.get($index) ?? 0) {
		$index = $index + 1
	}
	$index
}

skip_field_bytes : Bytes -> U64
skip_field_bytes = |bytes| skip_field_bytes_at(bytes, 0)

skip_field_bytes_at : Bytes, U64 -> U64
skip_field_bytes_at = |bytes, start| {
	var $index = start
	while $index + 16 <= bytes.len() {
		v = U8x16.load(bytes, $index) ?? U8x16.splat(0)
		control = U8x16.bitwise_or(U8x16.bitwise_and(U8x16.gt_lanes(U8x16.splat(32), v), U8x16.eq_lanes(v, U8x16.splat(9)).bitwise_not()), U8x16.eq_lanes(v, U8x16.splat(127)))
		outside = control.to_bitmask()
		if outside != 0 {
			return $index + outside.count_trailing_zero_bits().to_u64()
		}
		$index = $index + 16
	}
	while $index < bytes.len() and is_field_byte(bytes.get($index) ?? 0) {
		$index = $index + 1
	}
	$index
}

is_tchar : U8 -> Bool
is_tchar = |byte| {
	(byte >= 'a' and byte <= 'z')
		or (byte >= 'A' and byte <= 'Z')
		or is_digit(byte)
		or ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~'].contains(byte)
}

parse : Parser(Parser.Utf8.Bytes, a, HTTP.Problem), Str -> Try(a, [ParseError(Diagnostic(Parser.Problem(HTTP.Problem)))])
parse = |parser, text| (parser).parse_str(text)

# The error a request is rejected with.
request_error : Str -> Try(Failure, [Accepted])
request_error = |text| {
	match HTTP.parse_request(text.to_utf8()) {
		Ok(_) => Err(Accepted)
		Err(InvalidHttp(error)) => match error.location {
			ByteOffset(offset) => Ok({ offset, problem: error.problem })
			_ => crash "HTTP request error must identify original bytes"
		}
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
	actual = (HTTP.request).run("GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /2".to_utf8())?
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
expect (HTTP.request).parse(['G', 'E', 'T', ' ', '/', ' ', 'H', 'T', 'T', 'P', '/', '1', '.', '0', '\r', '\n', 'X', ':', 0xFF, '\r', '\n', '\r', '\n']).is_err()

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
expect request_error("G(T / HTTP/1.1\r\nHost: a\r\n\r\n") == Ok({ offset: 1, problem: InvalidMethod })
expect request_error(" / HTTP/1.1\r\nHost: a\r\n\r\n") == Ok({ offset: 0, problem: MissingMethod })

# Errors point at the offending byte or field line.
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n") == Ok({ offset: 44, problem: ConflictingFraming })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n") == Ok({ offset: 25, problem: DuplicateHost })
expect request_error("GET / HTTP/1.1\r\nHost: a\nX: y\r\n\r\n") == Ok({ offset: 23, problem: LfWithoutCr })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nX : y\r\n\r\n") == Ok({ offset: 26, problem: WhitespaceBeforeColon })
expect request_error("GET / HTTP/1.1\r\nHost: a\r\nX: a\u(7)b\r\n\r\n") == Ok({ offset: 29, problem: InvalidFieldCharacter })
expect request_error("GET / HTTP/1.2x\r\nHost: a\r\n\r\n") == Ok({ offset: 6, problem: InvalidVersion })
expect request_error("POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nab") == Ok({ offset: 49, problem: IncompleteBody })
expect request_error("POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n2;=\r\nab\r\n0\r\n\r\n") == Ok({ offset: 58, problem: MissingExtensionName })
expect request_error("GET / HTTP/1.1\r\n\r\n") == Ok({ offset: 0, problem: MissingHost })

# The embeddable parser preserves the native problem and offset.
expect parse(HTTP.request, "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n") == Err(ParseError(Diagnostic.{ problem: DuplicateHost, location: Text({ offset: 25, line: 3, column: 1 }), path: [] }))

# A response's status line errors point into the status line.
expect HTTP.parse_response("HTTP/1.1 099 Low\r\n\r\n".to_utf8()) == Err(InvalidHttp(Diagnostic.{ location: ByteOffset(9), path: [], problem: InvalidStatus }))

# A pipeline that ends inside a message reports the offset from the start of the input.
expect {
	bytes = "GET /a HTTP/1.1\r\nHost: a\r\n\r\nGET /b HTTP/1.1\r\n".to_utf8()
	HTTP.parse_requests(bytes) == Err(InvalidHttp(Diagnostic.{ location: ByteOffset(45), path: [], problem: MissingCrlf }))
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
	match (HTTP.request).parse_str(text) {
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
	Err(InvalidHttp(error)) => error.problem == RepeatedContentLength
	Ok(_) => False
}
expect HTTP.validate_response({ source: "HTTP/1.1 200 OK\r\nContent-Length: 0, 0\r\n\r\n", lints: { repeated_content_length: False } }).is_ok()
expect match HTTP.validate_response({ source: "HTTP/1.1 200 OK\r\nContent-Length: 0, 0\r\n\r\n" }) {
	Err(InvalidHttp(error)) => error.problem == RepeatedContentLengthValues
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\r\nHost: a\r\nContent-Length: 0, 1\r\n\r\n", lints: { repeated_content_length: False } }) {
	Err(InvalidHttp(error)) => error.problem == ConflictingContentLength
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\r\nHost: a\r\n\r\nGET /next HTTP/1.1\r\nHost: a\r\n\r\n" }) {
	Err(InvalidHttp(error)) => error.problem == TrailingContent
	Ok(_) => False
}
expect match HTTP.validate_request({ source: "GET / HTTP/1.1\nHost: a\n\n" }) {
	Err(InvalidHttp(error)) => error.problem == LfWithoutCr
	Ok(_) => False
}

# Normalize only transport-dependent body fragmentation; heads, trailers,
# completion markers and errors must remain in exactly the same order.
HttpTestResult : [Event(HTTP.Event), Finished, Failed(Streaming.ReadError([InvalidHttp(HTTP.Error)]))]

normalize_http_test : Iter(Try(Streaming.ReadEvent(HTTP.Event, [InvalidHttp(HTTP.Error)]), Streaming.ReadError([InvalidHttp(HTTP.Error)]))) -> List(HttpTestResult)
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
			Ok(Rejected(InvalidHttp(error))) => {
				$out = $out.append(Failed(InvalidHttp(error)))
			}
		}
	}
	$out
}

http_split_requests : Str -> Bool
http_split_requests = |text| {
	bytes = text.to_utf8()
	want = normalize_http_test((HTTP.request_decoder({})).read_iter([bytes].iter()))
	var $same = normalize_http_test((HTTP.request_decoder({})).read_iter(bytes.map(|byte| [byte]).iter())) == want
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [[], bytes.take_first(split), [], bytes.drop_first(split), []]
		if normalize_http_test((HTTP.request_decoder({})).read_iter(parts.iter())) != want {
			$same = False
		}
	}
	$same
}

http_split_responses : Str, List(HTTP.Method) -> Bool
http_split_responses = |text, methods| {
	bytes = text.to_utf8()
	want = normalize_http_test((HTTP.response_decoder(methods.iter(), {})).read_iter([bytes].iter()))
	var $same = normalize_http_test((HTTP.response_decoder(methods.iter(), {})).read_iter(bytes.map(|byte| [byte]).iter())) == want
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [[], bytes.take_first(split), [], bytes.drop_first(split), []]
		if normalize_http_test((HTTP.response_decoder(methods.iter(), {})).read_iter(parts.iter())) != want {
			$same = False
		}
	}
	$same
}

expect {
	text = "POST /one HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nhelloGET /two HTTP/1.1\r\nHost: a\r\n\r\n"
	events = (HTTP.request_decoder({})).collect([text.to_utf8()].iter())?
	events == [RequestHead({ method: Post, target: "/one", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }, { name: "Content-Length", value: "5" }] }), Body("hello".to_utf8()), MessageEnd, RequestHead({ method: Get, target: "/two", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }] }), MessageEnd]
		and http_split_requests(text)
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5;a=b;c=\"q\\\"\"\r\nhello\r\n6\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n"
	events = (HTTP.request_decoder({})).collect([text.to_utf8()].iter())?
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
].all(|text| (HTTP.request_decoder({})).collect([text.to_utf8()].iter()).is_err() and http_split_requests(text))

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
].all(|text| (HTTP.request_decoder({})).collect([text.to_utf8()].iter()).is_err() and http_split_requests(text))

expect {
	text = "HTTP/1.1 200 OK\r\nContent-Length: 999\r\n\r\nHTTP/1.1 204 Empty\r\n\r\nHTTP/1.1 304 Cached\r\n\r\n"
	events = (HTTP.response_decoder([Head, Get, Get].iter(), {})).collect([text.to_utf8()].iter())?
	events.len() == 6 and events.fold(0.U64, |count, event| count + if (event == MessageEnd) 1 else 0) == 3 and http_split_responses(text, [Head, Get, Get])
}

expect {
	text = "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 103 Early Hints\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi"
	events = (HTTP.response_decoder([Post].iter(), {})).collect([text.to_utf8()].iter())?
	events.fold(0.U64, |count, event| count + if (event == MessageEnd) 1 else 0) == 3 and events.drop_first(5) == [Body("hi".to_utf8()), MessageEnd] and http_split_responses(text, [Post])
}

expect {
	text = "HTTP/1.1 200 OK\r\n\r\nbody\r\nHTTP/1.1 is body text"
	events = (HTTP.response_decoder([Get].iter(), {})).collect([text.to_utf8()].iter())?
	events.drop_first(1) == [Body("body\r\nHTTP/1.1 is body text".to_utf8()), MessageEnd] and http_split_responses(text, [Get])
}

expect {
	text = "HTTP/1.1 200 Connected\r\n\r\ntunnel\r\nnot HTTP"
	events = (HTTP.response_decoder([Connect].iter(), {})).collect([text.to_utf8()].iter())?
	events.drop_first(1) == [MessageEnd, ProtocolSwitch, ProtocolData("tunnel\r\nnot HTTP".to_utf8())] and http_split_responses(text, [Connect])
}

expect {
	text = "HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n\r\nraw"
	events = (HTTP.response_decoder([Get].iter(), {})).collect([text.to_utf8()].iter())?
	events.drop_first(1) == [MessageEnd, ProtocolSwitch, ProtocolData("raw".to_utf8())] and http_split_responses(text, [Get])
}

expect {
	text = "HTTP/1.1 403 Forbidden\r\nContent-Length: 2\r\n\r\nnoHTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
	(HTTP.response_decoder([Connect, Get].iter(), {})).collect([text.to_utf8()].iter()).is_ok() and http_split_responses(text, [Connect, Get])
}

expect {
	text = "HTTP/1.1 100 Continue\r\n\r\n"
	(HTTP.response_decoder([Post].iter(), {})).collect([text.to_utf8()].iter()).is_err() and http_split_responses(text, [Post])
}

expect {
	bytes = "HTTP/1.1 204 Empty\r\n\r\n".to_utf8()
	(HTTP.response_decoder([].iter(), {})).collect([bytes].iter()).is_err()
		and (HTTP.response_decoder([Get].iter(), {})).collect([bytes.concat(bytes)].iter()).is_err()
}

expect {
	bytes = "GET / HTTP/1.1\r\nHost: a\r\n\r\n".to_utf8()
	(HTTP.request_decoder({ max_head_bytes: bytes.len() })).collect([bytes].iter()).is_ok()
		and (HTTP.request_decoder({ max_head_bytes: bytes.len() - 1 })).collect([bytes].iter()).is_err()
		and (HTTP.request_decoder({ max_line_bytes: 14, max_headers: 1 })).collect([bytes].iter()).is_ok()
		and (HTTP.request_decoder({ max_line_bytes: 13 })).collect([bytes].iter()).is_err()
		and (HTTP.request_decoder({ max_headers: 0 })).collect([bytes].iter()).is_err()
		and (HTTP.request_decoder({ max_headers: U64.highest, max_line_bytes: U64.highest })).collect([bytes].iter()).is_ok()
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nX: y\r\n\r\n"
	bytes = text.to_utf8()
	(HTTP.request_decoder({ max_trailer_bytes: 8, max_trailers: 1 })).collect([bytes].iter()).is_ok()
		and (HTTP.request_decoder({ max_trailer_bytes: 7 })).collect([bytes].iter()).is_err()
		and (HTTP.request_decoder({ max_trailers: 0 })).collect([bytes].iter()).is_err()
}

expect {
	text = "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n0123456789"
	events = (HTTP.response_decoder([Get].iter(), { max_body_chunk_bytes: 3 })).collect([text.to_utf8()].iter())?
	events.drop_first(1) == [Body("012".to_utf8()), Body("345".to_utf8()), Body("678".to_utf8()), Body("9".to_utf8()), MessageEnd]
		and (HTTP.request_decoder({ max_body_chunk_bytes: 0 })).collect([[]].iter()).is_err()
}

expect {
	# A consumer can stop immediately after the head without pulling any body
	# source chunks or the next request context.
	chunks = Iter.custom(False, Err(Unknown), |started| if (started) crash "pulled body source before demand" else Ok(("HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\n".to_utf8(), True)))
	methods = Iter.custom(False, Err(Unknown), |started| if (started) crash "pulled next method before demand" else Ok((Get, True)))
	match (HTTP.response_decoder(methods, {})).read_iter(chunks).next() {
		One({ item: Ok(Value(ResponseHead(head))), .. }) => head.status_code == 200
		_ => False
	}
}

expect {
	# Completion validates only consumed responses; surplus context is lazy.
	methods = Iter.custom({}, Err(Unknown), |_| crash "pulled unused context at EOF")
	(HTTP.response_decoder(methods, {})).collect([[], []].iter()) == Ok([])
}

expect {
	chunks = Iter.custom({}, Err(Unknown), |_| crash "read source with invalid fragment limit")
	match (HTTP.request_decoder({ max_body_chunk_bytes: 0 })).read_iter(chunks).next() {
		One({ item: Err(InvalidHttp(_)), .. }) => True
		_ => False
	}
}

expect {
	text = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n1;abcdefghijklmnop=abcdefghijklmnop\r\nx\r\n0\r\n\r\n"
	(HTTP.request_decoder({ max_line_bytes: 35 })).collect([text.to_utf8()].iter()).is_ok()
		and (HTTP.request_decoder({ max_line_bytes: 34 })).collect([text.to_utf8()].iter()).is_err()
}

# Fixed SIMD masks must recognize the complete HTTP byte classes, including
# invalid bytes placed after a full vector and in a final partial vector.
expect {
	var $valid = True
	for value in (0.U64).to(255) {
		byte = value.to_u8_wrap()
		for prefix in [0.U64, 1, 15, 16, 31, 32] {
			bytes = List.repeat('a', prefix).append(byte).concat(List.repeat('a', 32))
			if skip_tchar(bytes) != (if (is_tchar(byte)) bytes.len() else prefix) {
				$valid = False
			}
			if skip_field_bytes(bytes) != (if (is_field_byte(byte)) bytes.len() else prefix) {
				$valid = False
			}
		}
	}
	$valid
}

expect {
	prefix = "GET / HTTP/1.1\r\nHost: a\r\nX: ".to_utf8()
	bad_values = [[0.U8], [1], [7], [11], [12], [31], [127], [0xFF], [0xC0, 0x80], [0xE0, 0x80, 0x80], [0xED, 0xA0, 0x80]]
	bad_values.all(
		|value| {
			bytes = prefix.concat(List.repeat('a', 32)).concat(value).concat("\r\n\r\n".to_utf8())
			HTTP.parse_request(bytes).is_err()
				and (HTTP.request_decoder({})).collect([bytes].iter()).is_err()
				and (HTTP.request_decoder({})).collect(bytes.map(|byte| [byte]).iter()).is_err()
		},
	)
}

expect {
	text = "GET / HTTP/1.1\r\nHost: a\r\nX: ${Str.repeat("a", 15)}é${Str.repeat("b", 17)}😀\t \r\n\r\n"
	request = HTTP.parse_request(text.to_utf8())?.request
	HTTP.header(request.headers, "x") == Ok("${Str.repeat("a", 15)}é${Str.repeat("b", 17)}😀") and http_split_requests(text)
}

http_error : Failure -> HTTP.Error
http_error = |error| Diagnostic.{ problem: error.problem, location: ByteOffset(error.offset), path: [] }

named_http_error : Str, Failure -> HTTP.Error
named_http_error = |name, failure| { ..http_error(failure), path: [Source(name)] }

quote_http_error : Str, HTTP.Error -> [BadQuotedBytes(Str)]
quote_http_error = |source, error| {
	offset = Diagnostic.byte_offset(error.location) ?? 0
	position = Parser.Quotation.position(source, offset)
	Parser.Quotation.error({ name: "<literal>", format: "HTTP", message: Str.inspect(error.problem), line: position.line, column: position.column })
}
