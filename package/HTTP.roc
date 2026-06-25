import Parser
import String

HTTP :: {}.{
	Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

	HttpVersion : { major : U8, minor : U8 }

	Header : [Header(Str, Str)]

	Request : {
		method : Method,
		uri : Str,
		http_version : HttpVersion,
		headers : List(Header),
		body : List(U8),
	}

	Response : {
		http_version : HttpVersion,
		status_code : U16,
		status : Str,
		headers : List(Header),
		body : List(U8),
	}

	request : Parser(String.Utf8, Request)
	request = 
		Parser.const(
			|m| {
				|u| {
					|hv| {
						|hs| {
							|b| {
								{ method: m, uri: u, http_version: hv, headers: hs, body: b }
							}
						}
					}
				}
			},
		)
			.keep(method)
			.skip(sp)
			.keep(request_uri)
			.skip(sp)
			.keep(http_version)
			.skip(crlf)
			.keep(Parser.many(header))
			.skip(crlf)
			.keep(String.any_thing)

	response : Parser(String.Utf8, Response)
	response = 
		Parser.const(
			|hv| {
				|sc| {
					|s| {
						|hs| {
							|b| {
								{ http_version: hv, status_code: sc, status: s, headers: hs, body: b }
							}
						}
					}
				}
			},
		)
			.keep(http_version)
			.skip(sp)
			.keep((String.digits).map(U64.to_u16_wrap)) # TODO: change to to_u16_try
			.skip(sp)
			.keep(string_without_cr)
			.skip(crlf)
			.keep(Parser.many(header))
			.skip(crlf)
			.keep(String.any_thing)
}

method : Parser(String.Utf8, HTTP.Method)
method = 
	String.one_of(
		[
			String.string("OPTIONS").map(
				|_| {
					Options
				},
			),
			String.string("GET").map(
				|_| {
					Get
				},
			),
			String.string("POST").map(
				|_| {
					Post
				},
			),
			String.string("PUT").map(
				|_| {
					Put
				},
			),
			String.string("DELETE").map(
				|_| {
					Delete
				},
			),
			String.string("HEAD").map(
				|_| {
					Head
				},
			),
			String.string("TRACE").map(
				|_| {
					Trace
				},
			),
			String.string("CONNECT").map(
				|_| {
					Connect
				},
			),
			String.string("PATCH").map(
				|_| {
					Patch
				},
			),
		],
	)

expect String.parse_str(method, "GET") == Ok(Get)
expect String.parse_str(method, "DELETE") == Ok(Delete)

# TODO: do we want more structure in the URI, or is Str actually what programs want anyway?
# This is not a full URL!
#        Request-URI    = "*" | absoluteURI | abs_path | authority
RequestUri : Str

request_uri : Parser(String.Utf8, RequestUri)
request_uri = 
	String.codeunit_satisfies(
		|c| {
			c != ' '
		},
	)
		.one_or_more()
		.map(String.str_from_utf8)

sp = String.codeunit(' ')

crlf = String.string("\r\n")

http_version : Parser(String.Utf8, HTTP.HttpVersion)
http_version = 
	Parser.const(
		|major| {
			|minor| {
				{ major, minor }
			}
		},
	)
		.skip(String.string("HTTP/"))
		.keep((String.digits).map(U64.to_u8_wrap)) # TODO: change to to_u8_try
		.skip(String.codeunit('.'))
		.keep((String.digits).map(U64.to_u8_wrap)) # TODO: change to to_u8_try

expect {
	actual = String.parse_str(http_version, "HTTP/1.1")
	expected = Ok({ major: 1, minor: 1 })
	actual == expected
}

string_without_colon : Parser(String.Utf8, Str)
string_without_colon = 
	String.codeunit_satisfies(
		|c| {
			c != ':'
		},
	)
		.one_or_more()
		.map(String.str_from_utf8)

string_without_cr : Parser(String.Utf8, Str)
string_without_cr = 
	String.codeunit_satisfies(
		|c| {
			c != '\r'
		},
	)
		.one_or_more()
		.map(String.str_from_utf8)

header : Parser(String.Utf8, HTTP.Header)
header = 
	Parser.const(
		|k| {
			|v| {
				Header(k, v)
			}
		},
	)
		.keep(string_without_colon)
		.skip(String.string(": "))
		.keep(string_without_cr)
		.skip(crlf)

expect {
	actual = String.parse_str(header, "Accept-Encoding: gzip, deflate\r\n")
	expected = Ok(Header("Accept-Encoding", "gzip, deflate"))
	actual == expected
}

expect {
	request_text = 
		\\GET /things?id=1 HTTP/1.1\r
		\\Host: bar.example\r
		\\Accept-Encoding: gzip, deflate\r
		\\\r
		\\Hello, world!
	actual = 
		String.parse_str(HTTP.request, request_text)

	expected : Try(HTTP.Request, [ParsingFailure(Str), ParsingIncomplete(Str)])
	expected = Ok(
		{
			method: Get,
			uri: "/things?id=1",
			http_version: { major: 1, minor: 1 },
			headers: [
				Header("Host", "bar.example"),
				Header("Accept-Encoding", "gzip, deflate"),
			],
			body: "Hello, world!".to_utf8(),
		},
	)
	actual == expected
}

expect {
	request_text = 
		\\OPTIONS /resources/post-here/ HTTP/1.1\r
		\\Host: bar.example\r
		\\Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r
		\\Accept-Language: en-us,en;q=0.5\r
		\\Accept-Encoding: gzip,deflate\r
		\\Connection: Parser.keep-alive\r
		\\Origin: https://foo.example\r
		\\Access-Control-Request-Method: POST\r
		\\Access-Control-Request-Headers: X-PINGOTHER, Content-Type\r
		\\\r\n
	actual = 
		String.parse_str(HTTP.request, request_text)
	expected = Ok(
		{
			method: Options,
			uri: "/resources/post-here/",
			http_version: { major: 1, minor: 1 },
			headers: [
				Header("Host", "bar.example"),
				Header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
				Header("Accept-Language", "en-us,en;q=0.5"),
				Header("Accept-Encoding", "gzip,deflate"),
				Header("Connection", "Parser.keep-alive"),
				Header("Origin", "https://foo.example"),
				Header("Access-Control-Request-Method", "POST"),
				Header("Access-Control-Request-Headers", "X-PINGOTHER, Content-Type"),
			],
			body: [],
		},
	)
	actual == expected
}

expect {
	body = 
		\\<!DOCTYPE html>\r
		\\<html lang="en">\r
		\\<head>\r
		\\<meta charset="utf-8">\r
		\\<title>A simple webpage</title>\r
		\\</head>\r
		\\<body>\r
		\\<h1>Simple HTML webpage</h1>\r
		\\<p>Hello, world!</p>\r
		\\</body>\r
		\\</html>\r\n
	response_text = 
		\\HTTP/1.1 200 OK\r
		\\Content-Type: text/html; charset=utf-8\r
		\\Content-Length: 55743\r
		\\Connection: Parser.keep-alive\r
		\\Cache-Control: s-maxage=300, public, max-age=0\r
		\\Content-Language: en-US\r
		\\Date: Thu, 06 Dec 2018 17:37:18 GMT\r
		\\ETag: "2e77ad1dc6ab0b53a2996dfd4653c1c3"\r
		\\Server: meinheld/0.6.1\r
		\\Strict-Transport-Security: max-age=63072000\r
		\\X-Content-Type-Options: nosniff\r
		\\X-Frame-Options: DENY\r
		\\X-XSS-Protection: 1; mode=block\r
		\\Vary: Accept-Encoding,Cookie\r
		\\Age: 7\r
		\\\r
		\\${body}
	actual = 
		String.parse_str(HTTP.response, response_text)
	expected = 
		Ok(
			{
				http_version: { major: 1, minor: 1 },
				status_code: 200,
				status: "OK",
				headers: [
					Header("Content-Type", "text/html; charset=utf-8"),
					Header("Content-Length", "55743"),
					Header("Connection", "Parser.keep-alive"),
					Header("Cache-Control", "s-maxage=300, public, max-age=0"),
					Header("Content-Language", "en-US"),
					Header("Date", "Thu, 06 Dec 2018 17:37:18 GMT"),
					Header("ETag", "\"2e77ad1dc6ab0b53a2996dfd4653c1c3\""),
					Header("Server", "meinheld/0.6.1"),
					Header("Strict-Transport-Security", "max-age=63072000"),
					Header("X-Content-Type-Options", "nosniff"),
					Header("X-Frame-Options", "DENY"),
					Header("X-XSS-Protection", "1; mode=block"),
					Header("Vary", "Accept-Encoding,Cookie"),
					Header("Age", "7"),
				],
				body: body.to_utf8(),
			},
		)
	actual == expected
}
