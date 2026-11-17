app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}
import parser.Streaming
import cli.OsStr
import parser.HTTP
import Bench

## Arguments: byte chunk width, response context mode, then iterations.
## Zero uses one complete chunk; modes are auto, requests, get, head, connect.
## Metadata budgets are raised explicitly for the large-header corpus.
## All events are consumed and source completion is required; values are discarded.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	width = args.first().map_ok(OsStr.display).map_ok(|text| U64.from_str(text) ?? 0) ?? 0
	mode = args.get(1).map_ok(OsStr.display) ?? "auto"
	Bench.run!(args, |input| parse(input, width, mode))
}

parse : Str, U64, Str -> Try(U64, U64)
parse = |input, width, mode| {
	if !["auto", "requests", "get", "head", "connect"].contains(mode) {
		return Err(1)
	}
	bytes = input.to_utf8()
	chunk_bytes = if (width == 0) U64.max(bytes.len(), 1) else width
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|at| {
			if (at >= bytes.len()) Err(NoMore) else {
				len = U64.min(chunk_bytes, bytes.len() - at)
				Ok((bytes.sublist({ start: at, len }), at + len))
			}
		},
	)
	method : HTTP.Method
	method = if (mode == "head") Head else if (mode == "connect") Connect else Get
	methods = Iter.custom({}, Err(Unknown), |_| Ok((method, {})))
	props : HTTP.ReadProps
	props = { max_head_bytes: 4_194_304, max_line_bytes: 1_048_576, max_headers: 16_384, max_trailer_bytes: 4_194_304, max_trailers: 16_384 }
	responses = if (mode == "auto") input.starts_with("HTTP/") else mode != "requests"
	events = if (responses) Streaming.read_iter(chunks, HTTP.response_decoder(methods, props)) else Streaming.read_iter(chunks, HTTP.request_decoder(props))
	var $sum = 0.U64
	var $finished = False
	for result in events {
		match result {
			Ok(Value(RequestHead(head))) => {
				$sum = $sum + head.headers.len() + head.target.count_utf8_bytes()
			}
			Ok(Value(ResponseHead(head))) => {
				$sum = $sum + head.headers.len() + head.status_code.to_u64()
			}
			Ok(Value(Body(part))) | Ok(Value(ProtocolData(part))) => {
				$sum = $sum + part.len()
			}
			Ok(Value(Trailer(header))) => {
				$sum = $sum + header.name.count_utf8_bytes() + header.value.count_utf8_bytes()
			}
			Ok(Value(MessageEnd)) | Ok(Value(ProtocolSwitch)) => {}
			Ok(Finished(_)) => {
				$finished = True
			}
			_ => return Err(1)
		}
	}
	if ($finished) Ok($sum) else Err(1)
}
