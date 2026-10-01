import parser.HTTP

## Shared generator for the HTTP fuzz targets.
##
## Fuzzer bytes choose an HTTP/1.x message *and* how to write it: method,
## request target, version, field names and values with optional whitespace,
## Host placement, and body framing (none, Content-Length in its accepted
## repeated forms, or chunked with random chunk sizes, hex case, leading
## zeros, extensions and trailers). The expected parse is built alongside the
## bytes from the RFC 9112 / RFC 9110 grammar, never by calling the parser.
##
## Messages are kept in structured form (`Msg`) so the smuggling target can
## inject exactly one ambiguous construct before encoding.
HttpGen :: {}.{

	Cur : { bytes : List(U8), pos : U64 }

	## One field line as written (without CRLF) and the header it must parse to.
	Field : { line : List(U8), header : HTTP.Header }

	Kind : [Req({ method : HTTP.Method, uri : Str }), Res({ code : U16, reason : Str })]

	Framing : [NoFraming, Length, Chunked]

	Msg : {
		kind : Kind,
		start : List(U8),
		version : HTTP.HttpVersion,
		fields : List(Field),
		framing : Framing,
		## Bytes after the empty line that belong to this message.
		payload : List(U8),
		## The decoded body the parser must return.
		body : List(U8),
		## For chunked messages: the payload split into chunk records, so
		## mutations can edit sizes and extensions.
		chunks : List({ size_line : List(U8), data : List(U8) }),
		trailer : List(U8),
	}

	pick : Cur, U64 -> { n : U64, cur : Cur }
	pick = |cur, count| {
		byte = cur.bytes.get(cur.pos) ?? 0
		{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
	}

	## The input as one byte list.
	encode : Msg -> List(U8)
	encode = |msg| {
		head = msg.fields.fold(msg.start.concat(['\r', '\n']), |acc, field| acc.concat(field.line).concat(['\r', '\n']))
		head.concat(['\r', '\n']).concat(msg.payload)
	}

	## Rebuild the chunked payload after a mutation edited `chunks`/`trailer`.
	encode_chunks : List({ size_line : List(U8), data : List(U8) }), List(U8) -> List(U8)
	encode_chunks = |chunks, trailer| {
		chunks.fold([], |acc, chunk| {
			with_line = acc.concat(chunk.size_line).concat(['\r', '\n'])
			if chunk.data.is_empty() with_line else with_line.concat(chunk.data).concat(['\r', '\n'])
		})
		.concat(trailer)
	}

	expected_request : Msg -> Try(HTTP.Request, [NotARequest])
	expected_request = |msg| {
		match msg.kind {
			Req({ method, uri }) => Ok({ method, uri, http_version: msg.version, headers: msg.fields.map(|f| f.header), body: msg.body })
			Res(_) => Err(NotARequest)
		}
	}

	expected_response : Msg -> Try(HTTP.Response, [NotAResponse])
	expected_response = |msg| {
		match msg.kind {
			Res({ code, reason }) => Ok({ http_version: msg.version, status_code: code, status: reason, headers: msg.fields.map(|f| f.header), body: msg.body })
			Req(_) => Err(NotAResponse)
		}
	}

	## Does the message end where its framing says (so appended bytes are left
	## over), rather than at the end of the input?
	self_delimiting : Msg -> Bool
	self_delimiting = |msg| {
		match msg.kind {
			Req(_) => Bool.True
			Res(r) => msg.framing != NoFraming or no_body_status(r.code)
		}
	}

	no_body_status : U16 -> Bool
	no_body_status = |code| code < 200 or code == 204 or code == 304

	at_least_1_1 : HTTP.HttpVersion -> Bool
	at_least_1_1 = |v| v.major > 1 or (v.major == 1 and v.minor >= 1)

	## A message chosen by the fuzzer bytes. `want` forces a request or a response.
	generate : Cur, [Request, Response, Either] -> { msg : Msg, cur : Cur }
	generate = |start, want| {
		which = pick(start, 2)
		is_request =
			match want {
				Request => Bool.True
				Response => Bool.False
				Either => which.n == 0
			}
		version_choice = gen_version(which.cur)
		version = version_choice.version
		var $cur = version_choice.cur
		start_line =
			if is_request {
				m = pick($cur, methods.len())
				t = gen_target(m.cur)
				$cur = t.cur
				entry = methods.get(m.n) ?? { name: "GET", tag: Get }
				{ kind: Req({ method: entry.tag, uri: t.text }), bytes: Str.concat(entry.name, " ${t.text} ${version_text(version)}").to_utf8() }
			} else {
				c = gen_status(version, $cur)
				$cur = c.cur
				{ kind: c.kind, bytes: c.line }
			}

		# Ordinary fields.
		count = pick($cur, 5)
		$cur = count.cur
		many = pick($cur, 16)
		$cur = many.cur
		# Occasionally emit hundreds of fields to expose super-linear parsing.
		field_count = if many.n == 15 count.n * 200 + 50 else count.n
		var $fields = []
		var $index = 0
		while $index < field_count {
			f = gen_field($cur)
			$fields = $fields.append(f.field)
			$cur = f.cur
			$index = $index + 1
		}

		# Host: exactly one in HTTP/1.1+ requests, optional in HTTP/1.0.
		host_choice = pick($cur, 3)
		$cur = host_choice.cur
		if is_request and (at_least_1_1(version) or host_choice.n == 0) {
			h = gen_framing_field($cur, "Host", gen_host_value)
			placed = insert_at(h.cur, $fields, h.field)
			$fields = placed.fields
			$cur = placed.cur
		}

		# Body and framing.
		body_choice = gen_body($cur)
		$cur = body_choice.cur
		frame_choice = pick($cur, 3)
		$cur = frame_choice.cur
		framing =
			match frame_choice.n {
				0 => NoFraming
				1 => Length
				_ => if at_least_1_1(version) Chunked else Length
			}
		no_body =
			match start_line.kind {
				Res(r) => no_body_status(r.code)
				Req(_) => Bool.False
			}
		body =
			if no_body or (is_request and framing == NoFraming) {
				[]
			} else {
				body_choice.body
			}
		var $payload = if no_body [] else body
		var $chunks = []
		var $trailer = []
		match framing {
			NoFraming => {}
			Length => {
				# A no-body status may still declare a length (e.g. 304 says what
				# a GET would have returned); the parser must ignore it.
				declared = if no_body body_choice.body.len() else body.len()
				cl = gen_content_length($cur, declared)
				$cur = cl.cur
				var $i = 0
				while $i < cl.fields.len() {
					placed = insert_at($cur, $fields, cl.fields.get($i) ?? { line: [], header: Header("", "") })
					$fields = placed.fields
					$cur = placed.cur
					$i = $i + 1
				}
			}
			Chunked => {
				te = gen_framing_field($cur, "Transfer-Encoding", gen_chunked_value)
				placed = insert_at(te.cur, $fields, te.field)
				$fields = placed.fields
				$cur = placed.cur
				if !no_body {
					encoded = gen_chunks($cur, body)
					$cur = encoded.cur
					$chunks = encoded.chunks
					$trailer = encoded.trailer
					$payload = encode_chunks($chunks, $trailer)
				}
			}
		}

		msg = {
			kind: start_line.kind,
			start: start_line.bytes,
			version,
			fields: $fields,
			framing,
			payload: $payload,
			body,
			chunks: $chunks,
			trailer: $trailer,
		}
		{ msg, cur: $cur }
	}

	## Byte-level helpers shared with the targets.
	ows_trim : List(U8) -> List(U8)
	ows_trim = |bytes| trim(bytes)

	is_tchar : U8 -> Bool
	is_tchar = |b| tchar_bytes.contains(b)

	insert_bytes : List(U8), U64, List(U8) -> List(U8)
	insert_bytes = |bytes, at, extra| {
		index = if at < bytes.len() at else bytes.len()
		bytes.sublist({ start: 0, len: index }).concat(extra).concat(bytes.drop_first(index))
	}

	## Readable rendering plus an exact `hex:` line (first char Q/S selects the
	## parser) that scripts/review_http.py --fuzz-show cross-checks against h11.
	show_message : Kind, List(U8) -> Str
	show_message = |kind, bytes| {
		mode =
			match kind {
				Req(_) => "Q"
				Res(_) => "S"
			}
		digits = bytes.fold([], |acc, b| acc.append(hex_digit(b // 16)).append(hex_digit(b % 16)))
		"${show_bytes(bytes)}\nhex: ${mode}${Str.from_utf8(digits) ?? ""}"
	}

	show_bytes : List(U8) -> Str
	show_bytes = |bytes| {
		Str.from_utf8_lossy(bytes)
		|> Str.replace_each("\\", "\\\\")
		|> Str.replace_each("\t", "\\t")
		|> Str.replace_each("\r", "\\r")
		|> Str.replace_each("\n", "\\n\n")
	}
}

hex_digit : U8 -> U8
hex_digit = |n| if n < 10 n + '0' else n - 10 + 'a'

methods : List({ name : Str, tag : HTTP.Method })
methods = [
	{ name: "GET", tag: Get },
	{ name: "POST", tag: Post },
	{ name: "PUT", tag: Put },
	{ name: "DELETE", tag: Delete },
	{ name: "HEAD", tag: Head },
	{ name: "OPTIONS", tag: Options },
	{ name: "TRACE", tag: Trace },
	{ name: "CONNECT", tag: Connect },
	{ name: "PATCH", tag: Patch },
]

version_text : HTTP.HttpVersion -> Str
version_text = |v| "HTTP/${v.major.to_str()}.${v.minor.to_str()}"

gen_version : HttpGen.Cur -> { version : HTTP.HttpVersion, cur : HttpGen.Cur }
gen_version = |cur| {
	choice = HttpGen.pick(cur, 8)
	digits = HttpGen.pick(choice.cur, 100)
	version =
		match choice.n {
			0 | 1 | 2 | 3 => { major: 1, minor: 1 }
			4 | 5 => { major: 1, minor: 0 }
			_ => { major: (digits.n // 10).to_u8_wrap(), minor: (digits.n % 10).to_u8_wrap() }
		}
	{ version, cur: digits.cur }
}

## request-target: any VCHARs; the forms of RFC 9112 3.2 plus arbitrary ones.
gen_target : HttpGen.Cur -> { text : Str, cur : HttpGen.Cur }
gen_target = |cur| {
	form = HttpGen.pick(cur, 5)
	tail = gen_from(form.cur, target_alphabet, 12)
	text =
		match form.n {
			0 => "*"
			1 => "/${tail.text}"
			2 => "http://example.com/${tail.text}"
			3 => "example.com:443"
			_ => if tail.text.is_empty() "/" else tail.text
		}
	{ text, cur: tail.cur }
}

target_alphabet : List(Str)
target_alphabet = ["a", "Z", "0", "/", "?", "=", "&", "%", "2", "F", ".", "-", "_", "~", ":", "@", "!", "$", "'", "(", ")", "*", "+", ",", ";", "#", "[", "]", "\"", "<", ">", "\\", "^", "`", "{", "|", "}"]

gen_from : HttpGen.Cur, List(Str), U64 -> { text : Str, cur : HttpGen.Cur }
gen_from = |cur, alphabet, max_len| {
	size = HttpGen.pick(cur, max_len + 1)
	var $cur = size.cur
	var $text = ""
	var $index = 0
	while $index < size.n {
		c = HttpGen.pick($cur, alphabet.len())
		$text = Str.concat($text, alphabet.get(c.n) ?? "a")
		$cur = c.cur
		$index = $index + 1
	}
	{ text: $text, cur: $cur }
}

## status-line = HTTP-version SP status-code SP [ reason-phrase ]
gen_status : HTTP.HttpVersion, HttpGen.Cur -> { kind : HttpGen.Kind, line : List(U8), cur : HttpGen.Cur }
gen_status = |version, cur| {
	common = HttpGen.pick(cur, 4)
	raw_code = HttpGen.pick(common.cur, 900)
	code_n =
		match common.n {
			0 => [100, 101, 204, 304, 200, 404].get(raw_code.n % 6) ?? 200
			_ => raw_code.n + 100
		}
	code = code_n.to_u16_wrap()
	reason = gen_from(raw_code.cur, reason_alphabet, 10)
	space = HttpGen.pick(reason.cur, 2)
	sep = if reason.text.is_empty() and space.n == 0 "" else " "
	line = "${version_text(version)} ${code_n.to_str()}${sep}${reason.text}".to_utf8()
	{ kind: Res({ code, reason: reason.text }), line, cur: space.cur }
}

reason_alphabet : List(Str)
reason_alphabet = ["O", "K", "a", " ", "\t", "-", "!", "/", "é", "😀", "\u(80)", "~", "\"", ":"]

tchar_bytes : List(U8)
tchar_bytes = "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".to_utf8()

name_alphabet : List(Str)
name_alphabet = ["a", "b", "X", "-", "_", "0", "9", "!", "#", "$", "%", "&", "'", "*", "+", ".", "^", "`", "|", "~", "Z"]

value_alphabet : List(Str)
value_alphabet = ["a", "Z", "0", " ", "\t", ":", ",", ";", "=", "\"", "\\", "/", "(", ")", "é", "😀", "\u(80)", "\u(ff)", "~", "!", "{", "}"]

reserved : List(List(U8))
reserved = ["host", "content-length", "transfer-encoding"].map(|s| s.to_utf8())

lower : List(U8) -> List(U8)
lower = |bytes| bytes.map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)

is_ows : U8 -> Bool
is_ows = |b| b == ' ' or b == '\t'

trim : List(U8) -> List(U8)
trim = |bytes| {
	var $start = 0
	while $start < bytes.len() and is_ows(bytes.get($start) ?? 0) {
		$start = $start + 1
	}
	var $end = bytes.len()
	while $end > $start and is_ows(bytes.get($end - 1) ?? 0) {
		$end = $end - 1
	}
	bytes.sublist({ start: $start, len: $end - $start })
}

ows_options : List(Str)
ows_options = ["", "", " ", "  ", "\t", " \t "]

## field-line = field-name ":" OWS field-value OWS, value trimmed in the expectation.
gen_field : HttpGen.Cur -> { field : HttpGen.Field, cur : HttpGen.Cur }
gen_field = |cur| {
	name_part = gen_from(cur, name_alphabet, 8)
	name0 = if name_part.text.is_empty() "X" else name_part.text
	name = if reserved.contains(lower(name0.to_utf8())) "X-${name0}" else name0
	value = gen_from(name_part.cur, value_alphabet, 12)
	field = make_field(value.cur, name, value.text)
	{ field: field.field, cur: field.cur }
}

## A field with a given name and value, written with random OWS around the value.
make_field : HttpGen.Cur, Str, Str -> { field : HttpGen.Field, cur : HttpGen.Cur }
make_field = |cur, name, value| {
	left = HttpGen.pick(cur, ows_options.len())
	right = HttpGen.pick(left.cur, ows_options.len())
	line = "${name}:${ows_options.get(left.n) ?? ""}${value}${ows_options.get(right.n) ?? ""}".to_utf8()
	expected = Str.from_utf8(trim(value.to_utf8())) ?? ""
	{ field: { line, header: Header(name, expected) }, cur: right.cur }
}

## A framing field with randomly cased name.
gen_framing_field : HttpGen.Cur, Str, (HttpGen.Cur -> { text : Str, cur : HttpGen.Cur }) -> { field : HttpGen.Field, cur : HttpGen.Cur }
gen_framing_field = |cur, canonical, gen_value| {
	cased = random_case(cur, canonical)
	value = gen_value(cased.cur)
	make_field(value.cur, cased.text, value.text)
}

random_case : HttpGen.Cur, Str -> { text : Str, cur : HttpGen.Cur }
random_case = |cur, text| {
	style = HttpGen.pick(cur, 4)
	bytes = text.to_utf8()
	cased =
		match style.n {
			0 => lower(bytes)
			1 => bytes.map(|b| if b >= 'a' and b <= 'z' b - 32 else b)
			2 => bytes.map_with_index(|b, i| if i % 2 == 0 and b >= 'a' and b <= 'z' b - 32 else if b >= 'A' and b <= 'Z' b + 32 else b)
			_ => bytes
		}
	{ text: Str.from_utf8(cased) ?? text, cur: style.cur }
}

gen_host_value : HttpGen.Cur -> { text : Str, cur : HttpGen.Cur }
gen_host_value = |cur| {
	choice = HttpGen.pick(cur, 4)
	text = ["example.com", "a", "[::1]:8080", ""].get(choice.n) ?? "a"
	{ text, cur: choice.cur }
}

gen_chunked_value : HttpGen.Cur -> { text : Str, cur : HttpGen.Cur }
gen_chunked_value = |cur| random_case(cur, "chunked")

## Content-Length forms that RFC 9110 8.6 lets a recipient accept: one value
## with optional leading zeros, an identical comma list, or identical fields.
gen_content_length : HttpGen.Cur, U64 -> { fields : List(HttpGen.Field), cur : HttpGen.Cur }
gen_content_length = |cur, length| {
	zeros = HttpGen.pick(cur, 3)
	digits = Str.concat(Str.from_utf8(List.repeat('0', zeros.n)) ?? "", length.to_str())
	form = HttpGen.pick(zeros.cur, 4)
	name = random_case(form.cur, "Content-Length")
	match form.n {
		0 => {
			f = make_field(name.cur, name.text, "${digits}, ${digits}")
			{ fields: [f.field], cur: f.cur }
		}
		1 => {
			a = make_field(name.cur, name.text, digits)
			b = make_field(a.cur, "content-length", digits)
			{ fields: [a.field, b.field], cur: b.cur }
		}
		_ => {
			f = make_field(name.cur, name.text, digits)
			{ fields: [f.field], cur: f.cur }
		}
	}
}

insert_at : HttpGen.Cur, List(HttpGen.Field), HttpGen.Field -> { fields : List(HttpGen.Field), cur : HttpGen.Cur }
insert_at = |cur, fields, field| {
	at = HttpGen.pick(cur, fields.len() + 1)
	{ fields: fields.sublist({ start: 0, len: at.n }).append(field).concat(fields.drop_first(at.n)), cur: at.cur }
}

## Arbitrary body bytes, occasionally large.
gen_body : HttpGen.Cur -> { body : List(U8), cur : HttpGen.Cur }
gen_body = |cur| {
	size = HttpGen.pick(cur, 24)
	var $cur = size.cur
	var $body = []
	var $index = 0
	while $index < size.n {
		b = HttpGen.pick($cur, 256)
		$body = $body.append(b.n.to_u8_wrap())
		$cur = b.cur
		$index = $index + 1
	}
	big = HttpGen.pick($cur, 32)
	body = if big.n == 31 and !$body.is_empty() List.repeat($body, 4000 // $body.len() + 1).fold([], |acc, part| acc.concat(part)) else $body
	{ body, cur: big.cur }
}

hex_upper : List(U8)
hex_upper = "0123456789ABCDEF".to_utf8()

hex_lower : List(U8)
hex_lower = "0123456789abcdef".to_utf8()

to_hex : U64, Bool -> List(U8)
to_hex = |n, upper| {
	digits = if upper hex_upper else hex_lower
	var $out = []
	var $rest = n
	while $rest > 0 {
		$out = [digits.get($rest % 16) ?? '0'].concat($out)
		$rest = $rest // 16
	}
	if $out.is_empty() ['0'] else $out
}

## chunk-ext with optional BWS, token or quoted-string values (RFC 9112 7.1.1).
gen_extensions : HttpGen.Cur -> { text : List(U8), cur : HttpGen.Cur }
gen_extensions = |cur| {
	count = HttpGen.pick(cur, 4)
	var $cur = count.cur
	var $text = []
	var $index = 0
	# Most chunk lines get no extension.
	limit = if count.n == 3 2 else 0
	while $index < limit {
		e = HttpGen.pick($cur, ext_forms.len())
		$text = $text.concat((ext_forms.get(e.n) ?? ";a").to_utf8())
		$cur = e.cur
		$index = $index + 1
	}
	{ text: $text, cur: $cur }
}

ext_forms : List(Str)
ext_forms = [";a", ";name=value", ";q=\"quoted string\"", ";e=\"esc\\\"aped\\\\\"", " ; bws = 1", "\t;t\t=\t\"\"", ";x=\"é\""]

## Split a body into chunks and append last-chunk, trailers and the final CRLF.
gen_chunks : HttpGen.Cur, List(U8) -> { chunks : List({ size_line : List(U8), data : List(U8) }), trailer : List(U8), cur : HttpGen.Cur }
gen_chunks = |cur, body| {
	var $cur = cur
	var $chunks = []
	var $offset = 0
	while $offset < body.len() {
		s = HttpGen.pick($cur, 8)
		remaining = body.len() - $offset
		# Size 7 means "the rest"; big bodies use 1 KiB chunks.
		wanted = if s.n == 7 remaining else if body.len() > 100 1024 else s.n + 1
		size = if wanted < remaining wanted else remaining
		line = gen_size_line(s.cur, size)
		$chunks = $chunks.append({ size_line: line.text, data: body.sublist({ start: $offset, len: size }) })
		$cur = line.cur
		$offset = $offset + size
	}
	last = gen_size_line($cur, 0)
	$chunks = $chunks.append({ size_line: last.text, data: [] })
	trailers = HttpGen.pick(last.cur, 4)
	$cur = trailers.cur
	var $trailer = []
	var $index = 0
	while $index < trailers.n // 2 {
		f = gen_field($cur)
		$trailer = $trailer.concat(f.field.line).concat(['\r', '\n'])
		$cur = f.cur
		$index = $index + 1
	}
	{ chunks: $chunks, trailer: $trailer.concat(['\r', '\n']), cur: $cur }
}

gen_size_line : HttpGen.Cur, U64 -> { text : List(U8), cur : HttpGen.Cur }
gen_size_line = |cur, size| {
	style = HttpGen.pick(cur, 8)
	zeros = List.repeat('0', if style.n >= 6 style.n - 5 else 0)
	ext = gen_extensions(style.cur)
	{ text: zeros.concat(to_hex(size, style.n % 2 == 0)).concat(ext.text), cur: ext.cur }
}
