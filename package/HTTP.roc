module [
    Request,
    Response,
    request,
    response,
]

import Core
import String

# https://www.ietf.org/rfc/rfc2616.txt
Method : [Options, Get, Post, Put, Delete, Head, Trace, Connect, Patch]

HttpVersion : { major : U8, minor : U8 }

Request : {
    method : Method,
    uri : Str,
    httpVersion : HttpVersion,
    headers : List [Header Str Str],
    body : List U8,
}

Response : {
    httpVersion : HttpVersion,
    statusCode : U16,
    status : Str,
    headers : List [Header Str Str],
    body : List U8,
}

method : Core.Parser String.Utf8 Method
method =
    String.oneOf [
        String.string "OPTIONS" |> Core.map \_ -> Options,
        String.string "GET" |> Core.map \_ -> Get,
        String.string "POST" |> Core.map \_ -> Post,
        String.string "PUT" |> Core.map \_ -> Put,
        String.string "DELETE" |> Core.map \_ -> Delete,
        String.string "HEAD" |> Core.map \_ -> Head,
        String.string "TRACE" |> Core.map \_ -> Trace,
        String.string "CONNECT" |> Core.map \_ -> Connect,
        String.string "PATCH" |> Core.map \_ -> Patch,
    ]

expect String.parseStr method "GET" == Ok Get
expect String.parseStr method "DELETE" == Ok Delete

# TODO: do we want more structure in the URI, or is Str actually what programs want anyway?
# This is not a full URL!
#        Request-URI    = "*" | absoluteURI | abs_path | authority
RequestUri : Str

requestUri : Core.Parser String.Utf8 RequestUri
requestUri =
    String.codeunitSatisfies \c -> c != ' '
    |> Core.oneOrMore
    |> Core.map String.strFromUtf8

sp = String.codeunit ' '
crlf = String.string "\r\n"

httpVersion : Core.Parser String.Utf8 HttpVersion
httpVersion =
    Core.const (\major -> \minor -> { major, minor })
    |> Core.skip (String.string "HTTP/")
    |> Core.keep (String.digits |> Core.map Num.toU8)
    |> Core.skip (String.codeunit '.')
    |> Core.keep (String.digits |> Core.map Num.toU8)

expect
    actual = String.parseStr httpVersion "HTTP/1.1"
    expected = Ok { major: 1, minor: 1 }
    actual == expected

Header : [Header Str Str]

stringWithoutColon : Core.Parser String.Utf8 Str
stringWithoutColon =
    String.codeunitSatisfies \c -> c != ':'
    |> Core.oneOrMore
    |> Core.map String.strFromUtf8

stringWithoutCr : Core.Parser String.Utf8 Str
stringWithoutCr =
    String.codeunitSatisfies \c -> c != '\r'
    |> Core.oneOrMore
    |> Core.map String.strFromUtf8

header : Core.Parser String.Utf8 Header
header =
    Core.const (\k -> \v -> Header k v)
    |> Core.keep stringWithoutColon
    |> Core.skip (String.string ": ")
    |> Core.keep stringWithoutCr
    |> Core.skip crlf

expect
    actual = String.parseStr header "Accept-Encoding: gzip, deflate\r\n"
    expected = Ok (Header "Accept-Encoding" "gzip, deflate")
    actual == expected

request : Core.Parser String.Utf8 Request
request =
    Core.const (\m -> \u -> \hv -> \hs -> \b -> { method: m, uri: u, httpVersion: hv, headers: hs, body: b })
    |> Core.keep method
    |> Core.skip sp
    |> Core.keep requestUri
    |> Core.skip sp
    |> Core.keep httpVersion
    |> Core.skip crlf
    |> Core.keep (Core.many header)
    |> Core.skip crlf
    |> Core.keep String.anyThing

expect
    requestText =
        """
        GET /things?id=1 HTTP/1.1\r
        Host: bar.example\r
        Accept-Encoding: gzip, deflate\r
        \r
        Hello, world!
        """
    actual =
        String.parseStr request requestText

    expected : Result Request [ParsingFailure Str, ParsingIncomplete Str]
    expected = Ok {
        method: Get,
        uri: "/things?id=1",
        httpVersion: { major: 1, minor: 1 },
        headers: [
            Header "Host" "bar.example",
            Header "Accept-Encoding" "gzip, deflate",
        ],
        body: "Hello, world!" |> Str.toUtf8,
    }
    actual == expected

expect
    requestText =
        """
        OPTIONS /resources/post-here/ HTTP/1.1\r
        Host: bar.example\r
        Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r
        Accept-Language: en-us,en;q=0.5\r
        Accept-Encoding: gzip,deflate\r
        Connection: Core.keep-alive\r
        Origin: https://foo.example\r
        Access-Control-Request-Method: POST\r
        Access-Control-Request-Headers: X-PINGOTHER, Content-Type\r
        \r\n
        """
    actual =
        String.parseStr request requestText
    expected = Ok {
        method: Options,
        uri: "/resources/post-here/",
        httpVersion: { major: 1, minor: 1 },
        headers: [
            Header "Host" "bar.example",
            Header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            Header "Accept-Language" "en-us,en;q=0.5",
            Header "Accept-Encoding" "gzip,deflate",
            Header "Connection" "Core.keep-alive",
            Header "Origin" "https://foo.example",
            Header "Access-Control-Request-Method" "POST",
            Header "Access-Control-Request-Headers" "X-PINGOTHER, Content-Type",
        ],
        body: [],
    }
    actual == expected

response : Core.Parser String.Utf8 Response
response =
    Core.const (\hv -> \sc -> \s -> \hs -> \b -> { httpVersion: hv, statusCode: sc, status: s, headers: hs, body: b })
    |> Core.keep httpVersion
    |> Core.skip sp
    |> Core.keep (String.digits |> Core.map Num.toU16)
    |> Core.skip sp
    |> Core.keep stringWithoutCr
    |> Core.skip crlf
    |> Core.keep (Core.many header)
    |> Core.skip crlf
    |> Core.keep String.anyThing

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
    responseText =
        """
        HTTP/1.1 200 OK\r
        Content-Type: text/html; charset=utf-8\r
        Content-Length: 55743\r
        Connection: Core.keep-alive\r
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
        String.parseStr response responseText
    expected =
        Ok {
            httpVersion: { major: 1, minor: 1 },
            statusCode: 200,
            status: "OK",
            headers: [
                Header "Content-Type" "text/html; charset=utf-8",
                Header "Content-Length" "55743",
                Header "Connection" "Core.keep-alive",
                Header "Cache-Control" "s-maxage=300, public, max-age=0",
                Header "Content-Language" "en-US",
                Header "Date" "Thu, 06 Dec 2018 17:37:18 GMT",
                Header "ETag" "\"2e77ad1dc6ab0b53a2996dfd4653c1c3\"",
                Header "Server" "meinheld/0.6.1",
                Header "Strict-Transport-Security" "max-age=63072000",
                Header "X-Content-Type-Options" "nosniff",
                Header "X-Frame-Options" "DENY",
                Header "X-XSS-Protection" "1; mode=block",
                Header "Vary" "Accept-Encoding,Cookie",
                Header "Age" "7",
            ],
            body: Str.toUtf8 body,
        }
    actual == expected

