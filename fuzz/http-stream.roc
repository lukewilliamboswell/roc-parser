app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.HTTP
import parser.Parser
Observation : [Event(HTTP.Event), Finished, Error(HTTP.Error), SourceError]

normalize : List(Observation) -> List(Observation)
normalize = |observations| {
	var $out = []
	for observation in observations {
		match observation {
			Event(Body(part)) => match $out.last() {
				Ok(Event(Body(before))) => {
					$out = $out.drop_last(1).append(Event(Body(before.concat(part))))
				}
				_ => {
					$out = $out.append(observation)
				}
			}
			Event(ProtocolData(part)) => match $out.last() {
				Ok(Event(ProtocolData(before))) => {
					$out = $out.drop_last(1).append(Event(ProtocolData(before.concat(part))))
				}
				_ => {
					$out = $out.append(observation)
				}
			}
			_ => {
				$out = $out.append(observation)
			}
		}
	}
	$out
}

# Responses deliberately exercise a heterogeneous finite context queue.
methods : List(HTTP.Method)
methods = [Get, Head, Connect, Post]

read : List(List(U8)), Bool, HTTP.ReadProps -> List(Observation)
read = |chunks, responses, props| {
	reader = if (responses) (HTTP.Response.decoder({ methods: methods.iter(), reader: { max_head_bytes: props.max_head_bytes, max_trailer_bytes: props.max_trailer_bytes, max_line_bytes: props.max_line_bytes, max_headers: props.max_headers, max_trailers: props.max_trailers, max_body_chunk_bytes: props.max_body_chunk_bytes }, errors: props.errors })).read_iter(chunks.iter()) else (HTTP.Request.decoder(props)).read_iter(chunks.iter())
	var $out = []
	var $count = 0.U64
	for result in reader {
		match result {
			Ok(Value(event)) => {
				match event {
					Body(bytes) | ProtocolData(bytes) if bytes.is_empty() or bytes.len() > props.max_body_chunk_bytes => crash "unbounded or empty body event"
					_ => {}
				}
				$count = $count + 1
				$out = $out.append(Event(event))
			}
			Ok(Finished(report)) => {
				if report.emitted_items != $count or report.rejected_records != 0 {
					crash "incorrect HTTP completion report"
				}
				$out = $out.append(Finished)
			}
			Err(InvalidHttp(error)) => {
				$out = $out.append(Error(error))
			}
			_ => crash "HTTP produced recovery or decoder protocol error"
		}
	}
	normalize($out)
}

# Drive the decoder with More only: the expected source-error prefix must not
# finalize a truncated message or complete a close-delimited response.
prefix : List(U8), Bool, HTTP.ReadProps -> List(Observation)
prefix = |bytes, responses, props| {
	var $decoder = if (responses) HTTP.Response.decoder({ methods: methods.iter(), reader: { max_head_bytes: props.max_head_bytes, max_trailer_bytes: props.max_trailer_bytes, max_line_bytes: props.max_line_bytes, max_headers: props.max_headers, max_trailers: props.max_trailers, max_body_chunk_bytes: props.max_body_chunk_bytes }, errors: props.errors }) else HTTP.Request.decoder(props)
	var $used = 0.U64
	var $out = []
	while True {
		next = $decoder.step({ bytes: bytes.drop_first($used), end: More })
		if next.consumed > bytes.len() - $used {
			crash "HTTP consumed beyond input"
		}
		$used = $used + next.consumed
		$decoder = next.state
		match next.outcome {
			Item(event) => {
				$out = $out.append(Event(event))
			}
			Failed(InvalidHttp(error)) => return normalize($out.append(Error(error)))
			NeedInput => {
				if $used != bytes.len() {
					crash "HTTP requested input with bytes remaining"
				}
				return normalize($out.append(SourceError))
			}
			_ => crash "HTTP completed or recovered before source EOF"
		}
	}
}

check_source! : List(U8), Bool, HTTP.ReadProps => {}
check_source! = |bytes, responses, props| {
	source : Stream(Try(List(U8), [Injected]))
	source = [Ok(bytes), Err(Injected), Ok("unread".to_utf8())].iter().stream()
	reader = if (responses) (HTTP.Response.decoder({ methods: methods.iter(), reader: { max_head_bytes: props.max_head_bytes, max_trailer_bytes: props.max_trailer_bytes, max_line_bytes: props.max_line_bytes, max_headers: props.max_headers, max_trailers: props.max_trailers, max_body_chunk_bytes: props.max_body_chunk_bytes }, errors: props.errors })).read_stream(source) else (HTTP.Request.decoder(props)).read_stream(source)
	var $out = []
	for! result in reader {
		match result {
			Ok(Value(event)) => {
				$out = $out.append(Event(event))
			}
			Err(InvalidHttp(error)) => {
				$out = $out.append(Error(error))
			}
			Err(SourceErr(Injected)) => {
				$out = $out.append(SourceError)
			}
			_ => crash "HTTP source error finalized the document or lost its error"
		}
	}
	if normalize($out) != prefix(bytes, responses, props) {
		crash "HTTP source failure changed the emitted prefix"
	}
}

check! : List(U8), HTTP.ReadProps => {}
check! = |bytes, props| {
	for responses in [False, True] {
		whole = read([bytes], responses, props)
		parts = bytes.map(|byte| [byte])
		if read([[]].concat(parts).append([]), responses, props) != whole {
			crash "HTTP one-byte chunks changed events or errors"
		}
		split = (bytes.first() ?? 0).to_u64() % (bytes.len() + 1)
		if read([bytes.take_first(split), [], bytes.drop_first(split)], responses, props) != whole {
			crash "HTTP split changed events or errors"
		}
		check_source!(bytes, responses, props)
	}
}

test! = |bytes| {
	# Vary all metadata ceilings independently while keeping body fragments small.
	props : HTTP.ReadProps
	props = { max_head_bytes: 64 + (bytes.first() ?? 0).to_u64(), max_line_bytes: 16 + (bytes.get(1) ?? 0).to_u64(), max_headers: (bytes.get(2) ?? 0).to_u64() % 12, max_trailer_bytes: 2 + (bytes.get(3) ?? 0).to_u64(), max_trailers: (bytes.get(4) ?? 0).to_u64() % 8, max_body_chunk_bytes: 1 + (bytes.get(5) ?? 0).to_u64() % 17 }
	check!(bytes, props)
	check!(bytes, {})
	# Arbitrary binary body bytes must be preserved under both framing modes.
	payload = bytes.take_first(256)
	length = payload.len().to_str()
	wire = "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: ${length}\r\n\r\n".to_utf8().concat(payload)
	want : List(Observation)
	want = [Event(RequestHead({ method: Post, target: "/", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }, { name: "Content-Length", value: length }] }))].concat(if (payload.is_empty()) [] else [Event(Body(payload))]).concat([Event(MessageEnd), Finished])
	if read([wire], False, {}) != want {
		crash "HTTP fixed body differs from independent byte model"
	}
	check!(wire, {})
	chunked = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n".to_utf8().concat(payload.fold([], |all, byte| all.concat("1\r\n".to_utf8()).append(byte).concat("\r\n".to_utf8()))).concat("0\r\nX: y\r\n\r\n".to_utf8())
	chunked_want : List(Observation)
	chunked_want = [Event(RequestHead({ method: Post, target: "/", version: { major: 1, minor: 1 }, headers: [{ name: "Host", value: "a" }, { name: "Transfer-Encoding", value: "chunked" }] }))].concat(if (payload.is_empty()) [] else [Event(Body(payload))]).concat([Event(Trailer({ name: "X", value: "y" })), Event(MessageEnd), Finished])
	if read([chunked], False, {}) != chunked_want {
		crash "HTTP chunked body differs from independent byte model"
	}
	check!(chunked, {})
	Fuzz.keep
}

target = Fuzz.target_with!({
	name: "http-stream",
	examples: [
		Fuzz.example({ name: "chunked-trailer", revision: "1", make: || "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n1\r\nx\r\n0\r\nX: y\r\n\r\n".to_utf8() }),
		Fuzz.example({ name: "response-contexts", revision: "1", make: || "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhiHTTP/1.1 200 OK\r\nContent-Length: 99\r\n\r\nHTTP/1.1 200 Connected\r\n\r\nraw".to_utf8() }),
		Fuzz.example({ name: "close-delimited", revision: "1", make: || "HTTP/1.1 200 OK\r\n\r\nbody".to_utf8() }),
	],
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
})
