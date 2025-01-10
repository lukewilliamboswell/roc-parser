module [
    Request,
    Response,
    request,
    response,
]

import Parser exposing [Parser]
import String

# https://www.ietf.org/rfc/rfc2616.txt
Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

HttpVersion : { major : U8, minor : U8 }

Request : {
    method : Method,
    uri : Str,
    http_version : HttpVersion,
    headers : List [Header Str Str],
    body : List U8,
}

Response : {
    http_version : HttpVersion,
    status_code : U16,
    status : Str,
    headers : List [Header Str Str],
    body : List U8,
}

method : Parser String.Utf8 Method
method =
    String.one_of(
        [
            String.string("OPTIONS") |> Parser.map(\_ -> Options),
            String.string("GET") |> Parser.map(\_ -> Get),
            String.string("POST") |> Parser.map(\_ -> Post),
            String.string("PUT") |> Parser.map(\_ -> Put),
            String.string("DELETE") |> Parser.map(\_ -> Delete),
            String.string("HEAD") |> Parser.map(\_ -> Head),
            String.string("TRACE") |> Parser.map(\_ -> Trace),
            String.string("CONNECT") |> Parser.map(\_ -> Connect),
            String.string("PATCH") |> Parser.map(\_ -> Patch),
        ],
    )

expect String.parse_str(method, "GET") == Ok(Get)
expect String.parse_str(method, "DELETE") == Ok(Delete)

# TODO: do we want more structure in the URI, or is Str actually what programs want anyway?
# This is not a full URL!
#        Request-URI    = "*" | absoluteURI | abs_path | authority
RequestUri : Str

request_uri : Parser String.Utf8 RequestUri
request_uri =
    String.codeunit_satisfies(\c -> c != ' ')
    |> Parser.one_or_more
    |> Parser.map(String.str_from_utf8)

sp = String.codeunit(' ')
crlf = String.string("\r\n")

http_version : Parser String.Utf8 HttpVersion
http_version =
    Parser.const(\major -> \minor -> { major, minor })
    |> Parser.skip(String.string("HTTP/"))
    |> Parser.keep((String.digits |> Parser.map(Num.to_u8)))
    |> Parser.skip(String.codeunit('.'))
    |> Parser.keep((String.digits |> Parser.map(Num.to_u8)))

expect
    actual = String.parse_str(http_version, "HTTP/1.1")
    expected = Ok({ major: 1, minor: 1 })
    actual == expected

Header : [Header Str Str]

string_without_colon : Parser String.Utf8 Str
string_without_colon =
    String.codeunit_satisfies(\c -> c != ':')
    |> Parser.one_or_more
    |> Parser.map(String.str_from_utf8)

string_without_cr : Parser String.Utf8 Str
string_without_cr =
    String.codeunit_satisfies(\c -> c != '\r')
    |> Parser.one_or_more
    |> Parser.map(String.str_from_utf8)

header : Parser String.Utf8 Header
header =
    Parser.const(\k -> \v -> Header(k, v))
    |> Parser.keep(string_without_colon)
    |> Parser.skip(String.string(": "))
    |> Parser.keep(string_without_cr)
    |> Parser.skip(crlf)

expect
    actual = String.parse_str(header, "Accept-Encoding: gzip, deflate\r\n")
    expected = Ok(Header("Accept-Encoding", "gzip, deflate"))
    actual == expected

request : Parser String.Utf8 Request
request =
    Parser.const(\m -> \u -> \hv -> \hs -> \b -> { method: m, uri: u, http_version: hv, headers: hs, body: b })
    |> Parser.keep(method)
    |> Parser.skip(sp)
    |> Parser.keep(request_uri)
    |> Parser.skip(sp)
    |> Parser.keep(http_version)
    |> Parser.skip(crlf)
    |> Parser.keep(Parser.many(header))
    |> Parser.skip(crlf)
    |> Parser.keep(String.any_thing)

expect
    request_text =
        """
        GET /things?id=1 HTTP/1.1\r
        Host: bar.example\r
        Accept-Encoding: gzip, deflate\r
        \r
        Hello, world!
        """
    actual =
        String.parse_str(request, request_text)

    expected : Result Request [ParsingFailure Str, ParsingIncomplete Str]
    expected = Ok(
        {
            method: Get,
            uri: "/things?id=1",
            http_version: { major: 1, minor: 1 },
            headers: [
                Header("Host", "bar.example"),
                Header("Accept-Encoding", "gzip, deflate"),
            ],
            body: "Hello, world!" |> Str.to_utf8,
        },
    )
    actual == expected

expect
    request_text =
        """
        OPTIONS /resources/post-here/ HTTP/1.1\r
        Host: bar.example\r
        Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r
        Accept-Language: en-us,en;q=0.5\r
        Accept-Encoding: gzip,deflate\r
        Connection: Parser.keep-alive\r
        Origin: https://foo.example\r
        Access-Control-Request-Method: POST\r
        Access-Control-Request-Headers: X-PINGOTHER, Content-Type\r
        \r\n
        """
    actual =
        String.parse_str(request, request_text)
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

response : Parser String.Utf8 Response
response =
    Parser.const(\hv -> \sc -> \s -> \hs -> \b -> { http_version: hv, status_code: sc, status: s, headers: hs, body: b })
    |> Parser.keep(http_version)
    |> Parser.skip(sp)
    |> Parser.keep((String.digits |> Parser.map(Num.to_u16)))
    |> Parser.skip(sp)
    |> Parser.keep(string_without_cr)
    |> Parser.skip(crlf)
    |> Parser.keep(Parser.many(header))
    |> Parser.skip(crlf)
    |> Parser.keep(String.any_thing)

expect
    body =
        """
        <!DOCTYPE html>\r
        <html lang="en">\r
        <head>\r
        <meta charset="utf-8">\r
        <title>A simple webpage</title>\r
        </head>\r
        <body>\r
        <h1>Simple HTML webpage</h1>\r
        <p>Hello, world!</p>\r
        </body>\r
        </html>\r\n
        """
    response_text =
        """
        HTTP/1.1 200 OK\r
        Content-Type: text/html; charset=utf-8\r
        Content-Length: 55743\r
        Connection: Parser.keep-alive\r
        Cache-Control: s-maxage=300, public, max-age=0\r
        Content-Language: en-US\r
        Date: Thu, 06 Dec 2018 17:37:18 GMT\r
        ETag: "2e77ad1dc6ab0b53a2996dfd4653c1c3"\r
        Server: meinheld/0.6.1\r
        Strict-Transport-Security: max-age=63072000\r
        X-Content-Type-Options: nosniff\r
        X-Frame-Options: DENY\r
        X-XSS-Protection: 1; mode=block\r
        Vary: Accept-Encoding,Cookie\r
        Age: 7\r
        \r
        $(body)
        """
    actual =
        String.parse_str(response, response_text)
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
                body: Str.to_utf8(body),
            },
        )
    actual == expected
