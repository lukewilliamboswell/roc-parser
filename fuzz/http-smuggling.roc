app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.HTTP
import parser.Utf8
import HttpGen

## Smuggling / ambiguity property: fuzzer bytes choose a valid message (see
## fuzz/HttpGen.roc) and inject exactly one construct that RFC 9112 says makes
## the framing ambiguous or the message invalid. The parser must reject it
## (no message at all, not even a prefix): a parser that guesses here can
## disagree with a proxy about where a message ends.
##
## Each mutation cites the rule it relies on. Where RFC 9112 merely permits
## rejection (obs-fold, bare LF, both Content-Length and Transfer-Encoding),
## this library has chosen to reject; see the module docs of HTTP.

Mutated : { bytes : List(U8), original : List(U8), kind : HttpGen.Kind, label : Str }

Cur : HttpGen.Cur

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| HttpGen.pick(cur, count)

generate : List(U8) -> Try(Mutated, [NotApplicable])
generate = |bytes| {
	# The first byte picks the mutation so it is stable as the message changes.
	choice = pick({ bytes, pos: 0 }, mutation_count)
	out = HttpGen.generate(choice.cur, Either)
	mutated = mutate(choice.n, out.msg, out.cur)?
	Ok({ bytes: mutated.bytes, original: HttpGen.encode(out.msg), kind: out.msg.kind, label: mutated.label })
}

mutation_count : U64
mutation_count = 16

Result : Try({ bytes : List(U8), label : Str }, [NotApplicable])

done : HttpGen.Msg, Str -> Result
done = |msg, label| Ok({ bytes: HttpGen.encode(msg), label })

field_named : Str, Str -> HttpGen.Field
field_named = |name, value| { line: "${name}: ${value}".to_utf8(), header: Header(name, value) }

name_is : HttpGen.Field, Str -> Bool
name_is = |field, wanted| {
	match field.header {
		Header(name, _) => name.to_utf8().map(|b| if b >= 'A' and b <= 'Z' b + 32 else b) == wanted.to_utf8()
	}
}

without : List(HttpGen.Field), Str -> List(HttpGen.Field)
without = |fields, name| fields.drop_if(|f| name_is(f, name))

## Insert `field` at a chosen position.
place : Cur, List(HttpGen.Field), HttpGen.Field -> List(HttpGen.Field)
place = |cur, fields, field| {
	at = pick(cur, fields.len() + 1)
	fields.sublist({ start: 0, len: at.n }).append(field).concat(fields.drop_first(at.n))
}

pick_from : Cur, List(Str) -> Str
pick_from = |cur, options| options.get(pick(cur, options.len()).n) ?? ""

is_request : HttpGen.Msg -> Bool
is_request = |msg| {
	match msg.kind {
		Req(_) => Bool.True
		Res(_) => Bool.False
	}
}

## Responses whose status forbids content still have their framing fields
## validated, but chunk-level mutations need a chunked payload.
has_chunks : HttpGen.Msg -> Bool
has_chunks = |msg| msg.framing == Chunked and !msg.chunks.is_empty()

mutate : U64, HttpGen.Msg, Cur -> Result
mutate = |which, msg, cur| {
	choice = pick(cur, 256)
	n = choice.n
	next = choice.cur
	match which {
		0 => {
			# RFC 9110 8.6 / RFC 9112 6.3 item 5: differing Content-Length values
			# are an unrecoverable framing error.
			base = msg.body.len()
			a = field_named("Content-Length", base.to_str())
			b = field_named(pick_from(next, ["Content-Length", "content-length", "CONTENT-LENGTH"]), (base + 1 + n % 7).to_str())
			form = n % 3
			fields =
				if form == 0 {
					place(next, without(msg.fields, "content-length"), field_named("Content-Length", "${base.to_str()}, ${(base + 1).to_str()}"))
				} else {
					place(next, place(cur, without(msg.fields, "content-length"), a), b)
				}
			done({ ..msg, fields }, "conflicting Content-Length")
		}
		1 => {
			# RFC 9112 6.1: Content-Length with Transfer-Encoding "ought to be
			# handled as an error" (llhttp rejects; so does this parser).
			cl = field_named("Content-Length", (n % 10).to_str())
			te = field_named(pick_from(next, ["Transfer-Encoding", "transfer-encoding"]), pick_from(cur, ["chunked", "CHUNKED"]))
			fields =
				match msg.framing {
					Chunked => place(next, msg.fields, cl)
					Length => place(next, msg.fields, te)
					NoFraming => place(cur, place(next, msg.fields, cl), te)
				}
			done({ ..msg, fields }, "Content-Length with Transfer-Encoding")
		}
		2 => {
			# RFC 9110 8.6: Content-Length = 1*DIGIT; anything else is invalid.
			if msg.framing == Chunked {
				Err(NotApplicable)
			} else {
				value = pick_from(next, bad_lengths)
				fields = place(next, without(msg.fields, "content-length"), field_named("Content-Length", value))
				done({ ..msg, fields }, "invalid Content-Length ${Str.inspect(value)}")
			}
		}
		3 => {
			# RFC 9112 6.1/6.3: a coding list whose final coding is not chunked
			# cannot be framed (request) or is unsupported here; chunked must
			# not be applied twice.
			if msg.framing == Length {
				Err(NotApplicable)
			} else {
				value = pick_from(next, bad_codings)
				fields = place(next, without(msg.fields, "transfer-encoding"), field_named("Transfer-Encoding", value))
				done({ ..msg, fields }, "unsupported Transfer-Encoding ${Str.inspect(value)}")
			}
		}
		4 => {
			# RFC 9112 5.1: no whitespace is allowed between field name and colon.
			edit_field(msg, cur, |line| {
				colon = index_of(line, ':')
				HttpGen.insert_bytes(line, colon, if n % 2 == 0 [' '] else ['\t', ' '])
			}, "whitespace before colon")
		}
		5 => {
			# RFC 9112 5.2: obs-fold; this parser rejects it (a server MAY).
			edit_field(msg, cur, |line| line.concat(['\r', '\n', if n % 2 == 0 ' ' else '\t', 'x']), "obs-fold")
		}
		6 => {
			# RFC 9112 2.2: bare CR must be rejected; bare LF is rejected here.
			replace_line_end(msg, next, if n % 2 == 0 ['\n'] else ['\r'])
		}
		7 => {
			# RFC 9110 5.5: field values exclude CTLs (only SP/HTAB allowed),
			# and field names are tokens.
			ctl = ctl_bytes.get(n % ctl_bytes.len()) ?? 0
			edit_field(msg, next, |line| {
				spot = pick(cur, line.len() + 1).n
				HttpGen.insert_bytes(line, spot, [ctl])
			}, "control character 0x${ctl.to_str()} in field line")
		}
		8 => {
			# RFC 9112 3.2: an HTTP/1.1 request needs exactly one Host.
			if !is_request(msg) {
				Err(NotApplicable)
			} else if n % 2 == 0 and HttpGen.at_least_1_1(msg.version) {
				done({ ..msg, fields: without(msg.fields, "host") }, "missing Host")
			} else {
				fields = place(next, msg.fields, field_named(pick_from(cur, ["Host", "host"]), pick_from(next, ["other.example", "a", ""])))
				if msg.fields.any(|f| name_is(f, "host")) {
					done({ ..msg, fields }, "duplicate Host")
				} else {
					done({ ..msg, fields: place(cur, fields, field_named("Host", "x")) }, "duplicate Host")
				}
			}
		}
		9 => {
			# RFC 9112 6.1: Transfer-Encoding in an HTTP/1.0 message means faulty framing.
			start = with_version(msg, "HTTP/1.0")
			fields = place(next, without(without(msg.fields, "transfer-encoding"), "content-length"), field_named("Transfer-Encoding", "chunked"))
			done({ ..msg, start, fields, payload: "0\r\n\r\n".to_utf8() }, "Transfer-Encoding in HTTP/1.0")
		}
		10 => {
			# RFC 9112 7.1: chunk-size is 1*HEXDIG, then chunk-ext, then CRLF.
			if !has_chunks(msg) or msg.body.any(|b| b == '\r' or b == '\n') {
				Err(NotApplicable)
			} else {
				index = pick(next, msg.chunks.len()).n
				chunk = msg.chunks.get(index) ?? { size_line: [], data: [] }
				size = chunk.data.len()
				hex = hex_of(size)
				line =
					match n % 12 {
						0 => hex_of(size + 1)
						1 => hex_of(size + 2)
						2 => if size > 1 hex_of(size - 1) else ['G']
						3 => ['0', 'x'].concat(hex)
						4 => ['+'].concat(hex)
						5 => hex.concat([' '])
						6 => [' '].concat(hex)
						7 => hex.concat([';'])
						8 => hex.concat([';', '=', 'x'])
						9 => hex.concat([';', 'a', '=', '"', 'x'])
						10 => ['1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0'].concat(hex)
						_ => hex.concat(['\t'])
					}
				if size == 0 and n % 12 <= 2 {
					# Changing the last-chunk size just starts a data chunk.
					Err(NotApplicable)
				} else {
					chunks = msg.chunks.set(index, { ..chunk, size_line: line }) ?? msg.chunks
					done({ ..msg, payload: HttpGen.encode_chunks(chunks, msg.trailer) }, "bad chunk size line ${Str.inspect(Str.from_utf8_lossy(line))}")
				}
			}
		}
		11 => {
			# RFC 9112 7.1: chunk-data is followed by exactly CRLF.
			data_chunks = msg.chunks.drop_last(1)
			if !has_chunks(msg) or data_chunks.is_empty() {
				Err(NotApplicable)
			} else {
				index = pick(next, data_chunks.len()).n
				ending = [['\n'], ['\r'], [], ['X', 'X'], ['\r', ' ', '\n']].get(n % 5) ?? []
				payload = data_chunks.map_with_index(|chunk, i| {
					line = chunk.size_line.concat(['\r', '\n']).concat(chunk.data)
					if i == index line.concat(ending) else line.concat(['\r', '\n'])
				}).fold([], |acc, part| acc.concat(part))
				last = msg.chunks.last() ?? { size_line: ['0'], data: [] }
				done({ ..msg, payload: payload.concat(last.size_line).concat(['\r', '\n']).concat(msg.trailer) }, "bad CRLF after chunk data")
			}
		}
		12 => {
			# RFC 9112 7.1: a chunked body ends with last-chunk, trailers and CRLF.
			if !has_chunks(msg) {
				Err(NotApplicable)
			} else {
				payload = HttpGen.encode_chunks(msg.chunks.drop_last(1), [])
				ending = if n % 2 == 0 payload else HttpGen.encode_chunks(msg.chunks, msg.trailer).drop_last(2)
				done({ ..msg, payload: ending }, "unterminated chunked body")
			}
		}
		13 => {
			# Trailer fields use field-line syntax (RFC 9112 7.1.2).
			if !has_chunks(msg) {
				Err(NotApplicable)
			} else {
				bad = pick_from(next, ["A : b\r\n", "A: b\r\n c\r\n", " A: b\r\n", "A\r\n", "A: b\u(1)\r\n", "A: b\nB: c\r\n"]).to_utf8()
				done({ ..msg, payload: HttpGen.encode_chunks(msg.chunks, bad.concat(msg.trailer)) }, "invalid trailer field")
			}
		}
		14 => {
			# A self-delimiting message cut short is incomplete.
			if !HttpGen.self_delimiting(msg) {
				Err(NotApplicable)
			} else {
				full = HttpGen.encode(msg)
				cut = 1 + n % 8
				Ok({ bytes: full.drop_last(if cut < full.len() cut else full.len()), label: "truncated by ${cut.to_str()}" })
			}
		}
		_ => {
			# RFC 9112 3 / 4: start-line syntax.
			bad =
				if is_request(msg) {
					bad_request_line(msg, n)
				} else {
					bad_status_line(msg, n)
				}
			done({ ..msg, start: bad }, "bad start line")
		}
	}
}

bad_lengths : List(Str)
bad_lengths = ["+5", "-1", "0x10", "1 2", "1,2", "5,", ",5", "", "abc", "1.0", "99999999999999999999999", "18446744073709551616", "1_0", "\u(ff11)", "5;", "\"5\""]

bad_codings : List(Str)
bad_codings = ["chunked, gzip", "gzip, chunked", "gzip", "identity", "chunked, chunked", "xchunked", "chunked;a=b", "\"chunked\"", ", chunked", "chunked,", "chunk", "chunked chunked", "chunkedx", "chunked;"]

ctl_bytes : List(U8)
ctl_bytes = [0, 1, 7, 8, 11, 12, 14, 27, 31, 127]

index_of : List(U8), U8 -> U64
index_of = |bytes, wanted| {
	var $index = 0
	while $index < bytes.len() and bytes.get($index) != Ok(wanted) {
		$index = $index + 1
	}
	$index
}

edit_field : HttpGen.Msg, Cur, (List(U8) -> List(U8)), Str -> Result
edit_field = |msg, cur, edit, label| {
	if msg.fields.is_empty() {
		Err(NotApplicable)
	} else {
		index = pick(cur, msg.fields.len()).n
		field = msg.fields.get(index) ?? { line: [], header: Header("", "") }
		done({ ..msg, fields: msg.fields.set(index, { ..field, line: edit(field.line) }) ?? msg.fields }, label)
	}
}

## Replace one CRLF in the head with `ending`.
replace_line_end : HttpGen.Msg, Cur, List(U8) -> Result
replace_line_end = |msg, cur, ending| {
	lines = [msg.start].concat(msg.fields.map(|f| f.line)).append([])
	index = pick(cur, lines.len()).n
	head = lines.map_with_index(|line, i| line.concat(if i == index ending else ['\r', '\n'])).fold([], |acc, part| acc.concat(part))
	# A CR directly followed by the LF of the next line would just be CRLF.
	following = if index + 1 < lines.len() (lines.get(index + 1) ?? []).first() else msg.payload.first()
	if ending == ['\r'] and (following == Ok('\n') or (index + 2 == lines.len() and msg.payload.is_empty())) {
		Err(NotApplicable)
	} else {
		Ok({ bytes: head.concat(msg.payload), label: "line ending ${Str.inspect(Str.from_utf8_lossy(ending))} on line ${index.to_str()}" })
	}
}

hex_of : U64 -> List(U8)
hex_of = |n| {
	digits = "0123456789abcdef".to_utf8()
	var $out = []
	var $rest = n
	while $rest > 0 {
		$out = [digits.get($rest % 16) ?? '0'].concat($out)
		$rest = $rest // 16
	}
	if $out.is_empty() ['0'] else $out
}

with_version : HttpGen.Msg, Str -> List(U8)
with_version = |msg, version| {
	v = version.to_utf8()
	if is_request(msg) {
		msg.start.drop_last(8).concat(v)
	} else {
		v.concat(msg.start.drop_first(8))
	}
}

bad_request_line : HttpGen.Msg, U64 -> List(U8)
bad_request_line = |msg, n| {
	start = msg.start
	space = index_of(start, ' ')
	match n % 9 {
		0 => HttpGen.insert_bytes(start, space, [' '])
		1 => start.set(space, '\t') ?? start
		2 => start.map_with_index(|b, i| if i < space and b >= 'A' and b <= 'Z' b + 32 else b)
		3 => with_version(msg, "HTTP/1.10").drop_last(1).concat(['1', '0'])
		4 => with_version(msg, "http/1.1")
		5 => start.append(' ')
		6 => HttpGen.insert_bytes(start, space + 1, [0x7F])
		7 => HttpGen.insert_bytes(start, space + 1, [0xC3, 0xA9])
		_ => [' '].concat(start)
	}
}

bad_status_line : HttpGen.Msg, U64 -> List(U8)
bad_status_line = |msg, n| {
	start = msg.start
	match n % 7 {
		0 => HttpGen.insert_bytes(start, 9, ['0'])
		1 => start.sublist({ start: 0, len: 10 }).concat(start.drop_first(11))
		2 => HttpGen.insert_bytes(start, 8, [' '])
		3 => with_version(msg, "http/1.1")
		4 => start.concat([' ', 0x01])
		5 => start.sublist({ start: 0, len: 8 })
		_ => with_version(msg, "HTTP/1.").concat([])
	}
}

test : Try(Mutated, [NotApplicable]) -> Fuzz.Outcome
test = |input| {
	match input {
		Err(NotApplicable) => Fuzz.reject
		Ok(mutated) => {
			accepted =
				match mutated.kind {
					Req(_) => Utf8.parse_bytes_partial(HTTP.request, mutated.bytes).map_ok(|r| Str.inspect(r))
					Res(_) => Utf8.parse_bytes_partial(HTTP.response, mutated.bytes).map_ok(|r| Str.inspect(r))
				}
			match accepted {
				Err(_) => Fuzz.keep
				Ok(parsed) => crash "accepted an invalid message (${mutated.label})\n--- input ---\n${HttpGen.show_bytes(mutated.bytes)}\n--- before mutation ---\n${HttpGen.show_bytes(mutated.original)}\n--- parsed ---\n${parsed}"
			}
		}
	}
}

show : Try(Mutated, [NotApplicable]) -> Str
show = |input| {
	match input {
		Err(NotApplicable) => "(mutation not applicable)"
		Ok(mutated) => "${mutated.label}\n${HttpGen.show_message(mutated.kind, mutated.bytes)}"
	}
}

target = Fuzz.target_with({
	name: "http-smuggling",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show,
})
