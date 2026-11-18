app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.Parser
import parser.HTTP

# tag::method[]
## A method as it appears on the request line.
method_name : HTTP.Method -> Str
method_name = |method| {
	match method {
		Get => "GET"
		Head => "HEAD"
		Post => "POST"
		Put => "PUT"
		Delete => "DELETE"
		Connect => "CONNECT"
		Options => "OPTIONS"
		Trace => "TRACE"
		Patch => "PATCH"
		Extension(name) => name
	}
}

# end::method[]

# tag::embedded[]
## A capture file: a one-line label, then the raw request.
capture : Parser(Parser.Utf8.Bytes, { label : Str, request : HTTP.Request }, _)
capture =
	Parser.const(|label| |request| { label, request })
		.keep(Parser.chomp_while(|byte| byte != '\n').map(|bytes| Str.from_utf8_lossy(bytes)))
		.skip(Parser.Utf8.codeunit('\n'))
		.keep(HTTP.Request.parser({}).map_problem(|problem| HttpProblem(problem)))

# end::embedded[]

main! = |_args| {
	# tag::request[]
	request_text = "POST /notes HTTP/1.1\r\nHost: example.com\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello"

	request : HTTP.Request
	request = HTTP.Request.parse_bytes(request_text.to_utf8(), {})?
	Stdout.line!("method: ${method_name(request.method)}")?
	Stdout.line!("target: ${request.target}")?
	Stdout.line!("version: ${request.version.major.to_str()}.${request.version.minor.to_str()}")?
	Stdout.line!("body: ${Str.from_utf8(request.body) ?? "<binary>"}")?
	# end::request[]

	# tag::header[]
	content_type = request.header("content-type") ?? "none"
	accept = request.header("Accept") ?? "none"
	Stdout.line!("content type: ${content_type}, accept: ${accept}")?
	# end::header[]

	# tag::extension[]
	purge = HTTP.Request.parser({}).run("PURGE /cache/a HTTP/1.1\r\nHost: example.com\r\n\r\n".to_utf8())?
	Stdout.line!("method: ${Str.inspect(purge.value.method)}")?
	# end::extension[]

	# tag::response[]
	response_text = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot here."

	response = HTTP.Response.parse_bytes(response_text.to_utf8(), {})?
	Stdout.line!("status: ${response.status_code.to_str()} ${response.reason}")?
	Stdout.line!("body: ${Str.from_utf8(response.body) ?? "<binary>"}")?
	# end::response[]

	# tag::chunked[]
	chunked_text = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n6;note=x\r\npedia \r\n0\r\nExpires: never\r\n\r\n"

	chunked = HTTP.Response.parser({}).run(chunked_text.to_utf8())?
	Stdout.line!("decoded body: ${Str.from_utf8(chunked.value.body) ?? "<binary>"}")?
	# end::chunked[]

	# tag::pipelined[]
	pipelined = "GET /a HTTP/1.1\r\nHost: example.com\r\n\r\nGET /b HTTP/1.1\r\nHost: example.com\r\n\r\n".to_utf8()

	first = HTTP.Request.parser({}).run(pipelined).map_err(|error| Pipeline(error))?
	second = HTTP.Request.parser({}).run(first.rest).map_err(|error| Pipeline(error))?
	Stdout.line!("first: ${first.value.target}, second: ${second.value.target}, left over: ${second.rest.len().to_str()} bytes")?

	all = HTTP.parse_requests(pipelined, {})?
	Stdout.line!("targets: ${Str.join_with(all.map(|r| r.target), ", ")}")?
	# end::pipelined[]

	# tag::embedded-use[]
	saved = (capture).parse_str("health check\nGET /health HTTP/1.1\r\nHost: example.com\r\n\r\n").map_err(|error| Capture(error))?
	Stdout.line!("${saved.label}: ${saved.request.target}")?
	# end::embedded-use[]

	# tag::rejected[]
	smuggled = "POST / HTTP/1.1\r\nHost: example.com\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n"

	match HTTP.Request.parser({}).run(smuggled.to_utf8()) {
		Ok(_) => Stdout.line!("accepted")?
		Err(ParseError({ location: InputOffset(offset), problem, .. })) => Stdout.line!("rejected at byte ${offset.to_str()}: ${Str.inspect(problem)}")?
		Err(ParseError(error)) => Stdout.line!(Str.inspect(error))?
	}
	# end::rejected[]

	Ok({})
}
