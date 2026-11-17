app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../package/main.roc",
}
import parser.Streaming
import cli.Stdin
import cli.Stdout
import cli.OsStr
import parser.HTTP

## Read request pipelines from stdin, or one response with the matching
## request method: roc http-streaming.roc [get|head|connect].
main! : List(OsStr) => Try({}, _)
main! = |args| {
	events = match args.first().map_ok(OsStr.display) {
		Err(_) => Streaming.read_stream(Stdin.chunks(), HTTP.request_decoder({}))
		Ok("get") => Streaming.read_stream(Stdin.chunks(), HTTP.response_decoder([Get].iter(), {}))
		Ok("head") => Streaming.read_stream(Stdin.chunks(), HTTP.response_decoder([Head].iter(), {}))
		Ok("connect") => Streaming.read_stream(Stdin.chunks(), HTTP.response_decoder([Connect].iter(), {}))
		_ => return Err(Usage("http-streaming.roc [get|head|connect]"))
	}
	var $body_bytes = 0.U64
	var $protocol_bytes = 0.U64
	for! result in events {
		match result? {
			Value(RequestHead(head)) => Stdout.line!("request ${head.target}")?
			Value(ResponseHead(head)) => Stdout.line!("response ${head.status_code.to_str()}")?
			Value(Body(bytes)) => {
				$body_bytes = $body_bytes + bytes.len()
			}
			Value(Trailer(header)) => Stdout.line!("trailer ${header.name}: ${header.value}")?
			Value(MessageEnd) => {
				Stdout.line!("body bytes: ${$body_bytes.to_str()}")?
				$body_bytes = 0
			}
			Value(ProtocolSwitch) => Stdout.line!("protocol switch")?
			Value(ProtocolData(bytes)) => {
				$protocol_bytes = $protocol_bytes + bytes.len()
			}
			Finished(_) => Stdout.line!("finished; protocol bytes: ${$protocol_bytes.to_str()}")?
			Rejected(_) => return Err(UnexpectedRejection)
		}
	}
	Ok({})
}
