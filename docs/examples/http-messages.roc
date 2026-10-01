app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.String
import parser.HTTP

# tag::header[]
## The value of the first header with this name, compared case-insensitively.
header_value : List(HTTP.Header), Str -> [Some(Str), None]
header_value = |headers, wanted| {
	var $found = None
	for Header(name, value) in headers {
		if $found == None and name.with_ascii_lowercased() == wanted.with_ascii_lowercased() {
			$found = Some(value)
		}
	}
	$found
}

# end::header[]

main! = |_args| {
	# tag::request[]
	request_text = "POST /notes HTTP/1.1\r\nHost: example.com\r\nContent-Type: text/plain\r\nContent-Length: 5\r\n\r\nhello"

	request = String.parse_str(HTTP.request, request_text)?
	Stdout.line!("method: ${Str.inspect(request.method)}")?
	Stdout.line!("uri: ${request.uri}")?
	Stdout.line!("version: ${request.http_version.major.to_str()}.${request.http_version.minor.to_str()}")?
	Stdout.line!("content type: ${Str.inspect(header_value(request.headers, "content-type"))}")?
	Stdout.line!("body: ${Str.from_utf8(request.body) ?? "<binary>"}")?
	# end::request[]

	# tag::response[]
	response_text = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot here."

	response = String.parse_str(HTTP.response, response_text)?
	Stdout.line!("status: ${response.status_code.to_str()} ${response.status}")?
	Stdout.line!("body: ${Str.from_utf8(response.body) ?? "<binary>"}")?
	# end::response[]

	# tag::chunked[]
	chunked_text = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n6;note=x\r\npedia \r\n0\r\nExpires: never\r\n\r\n"

	chunked = String.parse_str(HTTP.response, chunked_text)?
	Stdout.line!("decoded body: ${Str.from_utf8(chunked.body) ?? "<binary>"}")?
	# end::chunked[]

	# tag::pipelined[]
	pipelined = "GET /a HTTP/1.1\r\nHost: example.com\r\n\r\nGET /b HTTP/1.1\r\nHost: example.com\r\n\r\n".to_utf8()

	first = String.parse_utf8_partial(HTTP.request, pipelined).map_err(|ParsingFailure(message)| BadMessage(message))?
	second = String.parse_utf8_partial(HTTP.request, first.input).map_err(|ParsingFailure(message)| BadMessage(message))?
	Stdout.line!("first: ${first.val.uri}, second: ${second.val.uri}, left over: ${second.input.len().to_str()} bytes")?
	# end::pipelined[]

	# tag::rejected[]
	smuggled = "POST / HTTP/1.1\r\nHost: example.com\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n"

	match String.parse_str(HTTP.request, smuggled) {
		Ok(_) => Stdout.line!("accepted")?
		Err(ParsingFailure(message)) => Stdout.line!("rejected: ${message}")?
		Err(ParsingIncomplete(_)) => Stdout.line!("trailing data")?
	}
	# end::rejected[]

	Ok({})
}
