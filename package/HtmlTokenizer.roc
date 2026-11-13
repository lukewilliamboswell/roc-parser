import MarkdownEntities

## HTML tokenization primitives. Offsets always address the unmodified input.
## Tree construction supplies the content model and foreign-content flag.
HtmlTokenizer :: [].{
	Span : { start : U64, end : U64 }
	Attribute : { name : Str, value : Str, span : Span }
	Tag : { name : Str, attributes : List(Attribute), self_closing : Bool }
	Kind : [Start(Tag), End(Str), Characters(Str), Comment(Str), Doctype({ name : Str, public_id : Str, system_id : Str, quirks : Bool }), Eof]
	Token : { kind : Kind, span : Span }
	Code : [UnexpectedNull, EofInTag, EofInComment, IncorrectComment, BogusComment, InvalidTagOpen, DuplicateAttribute(Str), MissingAttributeValue, MissingWhitespace, InvalidAttributeCharacter, EndTagAttributes, InvalidSolidus, MissingSemicolon, UnknownReference, InvalidCharacterReference, MissingDoctypeName, InvalidDoctype, EofInDoctype]
	Diagnostic : { code : Code, offset : U64 }
	Mode : [Data, Rcdata(Str), Rawtext(Str), Script, Plaintext]
	Reader : { bytes : List(U8), pos : U64, diagnostics : List(Diagnostic) }

	next : Reader, Mode, Bool -> { token : Token, reader : Reader }
	next = |reader, mode, foreign| {
		bytes = reader.bytes
		start = reader.pos
		if start >= bytes.len() {
			return { token: { kind: Eof, span: { start, end: start } }, reader }
		}
		match mode {
			Plaintext => return characters(reader, bytes.len(), False, False)
			Rawtext(name) => {
				if !end_tag_at(bytes, start, name) {
					return raw_characters(reader, name, False, False)
				}
			}
			Rcdata(name) => {
				if !end_tag_at(bytes, start, name) {
					return raw_characters(reader, name, True, False)
				}
			}
			Script => {
				if !end_tag_at(bytes, start, "script") {
					return raw_characters(reader, "script", False, True)
				}
			}
			Data => {}
		}
		if byte_at(bytes, start) != '<' {
			var $end = start + 1
			while $end < bytes.len() and byte_at(bytes, $end) != '<' {
				$end = $end + 1
			}
			return characters(reader, $end, True, True)
		}
		if starts(bytes, start, "<!--") {
			return comment(reader)
		}
		if starts_caseless(bytes, start, "<!doctype") {
			return doctype(reader)
		}
		if starts(bytes, start, "<![CDATA[") and foreign {
			var $end = start + 9
			while $end < bytes.len() and !starts(bytes, $end, "]]>") {
				$end = $end + 1
			}
			text = normalized(bytes.sublist({ start: start + 9, len: $end - start - 9 }), False, False)
			end = if ($end < bytes.len()) $end + 3 else $end
			return { token: { kind: Characters(text.text), span: { start, end } }, reader: { ..reader, pos: end } }
		}
		if starts(bytes, start, "<!") or starts(bytes, start, "<?") {
			var $end = start + 2
			while $end < bytes.len() and byte_at(bytes, $end) != '>' {
				$end = $end + 1
			}
			end = if ($end < bytes.len()) $end + 1 else $end
			return { token: { kind: Comment(text_at(bytes, start + 2, $end)), span: { start, end } }, reader: { ..reader, pos: end, diagnostics: reader.diagnostics.append({ code: BogusComment, offset: start }) } }
		}
		closing = byte_at(bytes, start + 1) == '/'
		name_start = start + if (closing) 2 else 1
		if !ascii_alpha(byte_at(bytes, name_start)) {
			return { token: { kind: Characters("<"), span: { start, end: start + 1 } }, reader: { ..reader, pos: start + 1, diagnostics: reader.diagnostics.append({ code: InvalidTagOpen, offset: start }) } }
		}
		var $pos = name_start
		while $pos < bytes.len() and !space(byte_at(bytes, $pos)) and !['/', '>'].contains(byte_at(bytes, $pos)) {
			$pos = $pos + 1
		}
		name = lower(text_at(bytes, name_start, $pos))
		var $attributes = []
		var $diagnostics = reader.diagnostics
		var $self_closing = False
		var $done = False
		while $pos < bytes.len() and !$done {
			before = $pos
			while $pos < bytes.len() and space(byte_at(bytes, $pos)) {
				$pos = $pos + 1
			}
			c = byte_at(bytes, $pos)
			if c == '>' {
				$pos = $pos + 1
				$done = True
			}
			else if c == '/' {
				$pos = $pos + 1
				if byte_at(bytes, $pos) == '>' {
					$self_closing = True
					$pos = $pos + 1
					$done = True
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidSolidus, offset: $pos - 1 })
				}
			} else if $pos < bytes.len() {
				if before == $pos and !$attributes.is_empty() {
					$diagnostics = $diagnostics.append({ code: MissingWhitespace, offset: $pos })
				}
				attr_start = $pos
				while $pos < bytes.len() and !space(byte_at(bytes, $pos)) and !['=', '/', '>'].contains(byte_at(bytes, $pos)) {
					if ['"', '\'', '<', 0].contains(byte_at(bytes, $pos)) {
						$diagnostics = $diagnostics.append({ code: InvalidAttributeCharacter, offset: $pos })
					}
					$pos = $pos + 1
				}
				if $pos == attr_start {
					$pos = $pos + 1
				}
				attr_name = lower(text_at(bytes, attr_start, $pos))
				after_name = $pos
				while $pos < bytes.len() and space(byte_at(bytes, $pos)) {
					$pos = $pos + 1
				}
				var $value = ""
				if byte_at(bytes, $pos) == '=' {
					$pos = $pos + 1
					while $pos < bytes.len() and space(byte_at(bytes, $pos)) {
						$pos = $pos + 1
					}
					quote = byte_at(bytes, $pos)
					quoted = quote == '"' or quote == '\''
					if quoted {
						$pos = $pos + 1
					}
					value_start = $pos
					while $pos < bytes.len() and (if (quoted) byte_at(bytes, $pos) != quote else !space(byte_at(bytes, $pos)) and byte_at(bytes, $pos) != '>') {
						if !quoted and ['"', '\'', '<', '=', '`'].contains(byte_at(bytes, $pos)) {
							$diagnostics = $diagnostics.append({ code: InvalidAttributeCharacter, offset: $pos })
						}
						$pos = $pos + 1
					}
					decoded = normalized(bytes.sublist({ start: value_start, len: $pos - value_start }), True, True)
					$value = decoded.text
					$diagnostics = $diagnostics.concat(decoded.diagnostics.map(|d| { ..d, offset: value_start + d.offset }))
					if quoted and $pos < bytes.len() {
						$pos = $pos + 1
					}
					if !quoted and value_start == $pos {
						$diagnostics = $diagnostics.append({ code: MissingAttributeValue, offset: value_start })
					}
				}
				if byte_at(bytes, after_name) != '=' and $value == "" {
					lookahead = after_name
					var $scan = lookahead
					while $scan < bytes.len() and space(byte_at(bytes, $scan)) {
						$scan = $scan + 1
					}
					if byte_at(bytes, $scan) != '=' {
						$pos = after_name
					}
				}
				if $attributes.any(|a| a.name == attr_name) {
					$diagnostics = $diagnostics.append({ code: DuplicateAttribute(attr_name), offset: attr_start })
				}
				else {
					$attributes = $attributes.append({ name: attr_name, value: $value, span: { start: attr_start, end: $pos } })
				}
			}
		}
		if !$done {
			$diagnostics = $diagnostics.append({ code: EofInTag, offset: start })
			return { token: { kind: Eof, span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
		}
		if closing and (!$attributes.is_empty() or $self_closing) {
			$diagnostics = $diagnostics.append({ code: EndTagAttributes, offset: start })
		}
		{ token: { kind: if (closing) End(name) else Start({ name, attributes: $attributes, self_closing: $self_closing }), span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
	}

	lower : Str -> Str
	lower = |s| Str.from_utf8_lossy(s.to_utf8().map(|b| if (b >= 'A' and b <= 'Z') b + 32 else b))
	space : U8 -> Bool
	space = |c| [' ', '\t', '\n', '\r', 12].contains(c)
	ascii_alpha : U8 -> Bool
	ascii_alpha = |c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')
	text_at : List(U8), U64, U64 -> Str
	text_at = |bytes, start, end| Str.from_utf8_lossy(bytes.sublist({ start, len: end - start }))
	byte_at : List(U8), U64 -> U8
	byte_at = |bytes, at| bytes.get(at) ?? 0
	starts : List(U8), U64, Str -> Bool
	starts = |bytes, at, value| bytes.sublist({ start: at, len: value.count_utf8_bytes() }) == value.to_utf8()
	starts_caseless : List(U8), U64, Str -> Bool
	starts_caseless = |bytes, at, value| lower(text_at(bytes, at, at + value.count_utf8_bytes())) == value
	end_tag_at : List(U8), U64, Str -> Bool
	end_tag_at = |bytes, at, name| starts(bytes, at, "</") and starts_caseless(bytes, at + 2, name) and (space(byte_at(bytes, at + 2 + name.count_utf8_bytes())) or ['/', '>'].contains(byte_at(bytes, at + 2 + name.count_utf8_bytes())))

	## HTML input preprocessing and character-reference consumption.
	normalized : List(U8), Bool, Bool -> { text : Str, diagnostics : List(Diagnostic) }
	normalized = |bytes, references, attribute| normalized_with_nulls(bytes, references, attribute, True)

	normalized_with_nulls : List(U8), Bool, Bool, Bool -> { text : Str, diagnostics : List(Diagnostic) }
	normalized_with_nulls = |bytes, references, attribute, replace_nulls| {
		var $out = []
		var $diagnostics = []
		var $pos = 0
		while $pos < bytes.len() {
			c = byte_at(bytes, $pos)
			if c == '\r' {
				$out = $out.append('\n')
				$pos = $pos + if (byte_at(bytes, $pos + 1) == '\n') 2 else 1
			} else if c == 0 {
				$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				$out = if (replace_nulls) $out.concat([239, 191, 189]) else $out.append(0)
				$pos = $pos + 1
			} else if c == '&' and references {
				reference = consume_reference(bytes, $pos, attribute)
				$out = $out.concat(reference.bytes)
				$diagnostics = $diagnostics.concat(reference.diagnostics)
				$pos = reference.end
			} else {
				$out = $out.append(c)
				$pos = $pos + 1
			}
		}
		{ text: Str.from_utf8_lossy($out), diagnostics: $diagnostics }
	}
}

characters = |reader, end, references, data| {
	decoded = HtmlTokenizer.normalized_with_nulls(reader.bytes.sublist({ start: reader.pos, len: end - reader.pos }), references, False, !data)
	{ token: { kind: Characters(decoded.text), span: { start: reader.pos, end } }, reader: { ..reader, pos: end, diagnostics: reader.diagnostics.concat(decoded.diagnostics.map(|d| { ..d, offset: reader.pos + d.offset })) } }
}

raw_characters = |reader, name, references, script| {
	bytes = reader.bytes
	var $end = reader.pos
	var $escaped = False
	var $double = False
	while $end < bytes.len() {
		if script and HtmlTokenizer.starts(bytes, $end, "<!--") {
			$escaped = True
			$end = $end + 4
		}
		else if script and $escaped and HtmlTokenizer.starts(bytes, $end, "-->") {
			$escaped = False
			$double = False
			$end = $end + 3
		}
		else if script and $escaped and !$double and HtmlTokenizer.starts_caseless(bytes, $end, "<script") and (HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $end + 7)) or ['/', '>'].contains(HtmlTokenizer.byte_at(bytes, $end + 7))) {
			$double = True
			$end = $end + 7
		}
		else if HtmlTokenizer.end_tag_at(bytes, $end, name) {
			if $double {
				$double = False
				$end = $end + 2 + name.count_utf8_bytes()
			}
			else {
				break
			}
		} else {
			$end = $end + 1
		}
	}
	characters(reader, $end, references, False)
}

comment = |reader| {
	bytes = reader.bytes
	start = reader.pos
	var $end = start + 4
	var $diagnostics = reader.diagnostics
	if HtmlTokenizer.byte_at(bytes, $end) == '>' {
		return { token: { kind: Comment(""), span: { start, end: $end + 1 } }, reader: { ..reader, pos: $end + 1, diagnostics: $diagnostics.append({ code: IncorrectComment, offset: start }) } }
	}
	while $end < bytes.len() and !HtmlTokenizer.starts(bytes, $end, "-->") and !HtmlTokenizer.starts(bytes, $end, "--!>") {
		if HtmlTokenizer.starts(bytes, $end, "<!--") {
			$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $end })
		}
		$end = $end + 1
	}
	text_end = $end
	if $end >= bytes.len() {
		$diagnostics = $diagnostics.append({ code: EofInComment, offset: start })
	}
	else if HtmlTokenizer.starts(bytes, $end, "--!>") {
		$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $end })
		$end = $end + 4
	}
	else {
		$end = $end + 3
	}
	decoded = HtmlTokenizer.normalized(bytes.sublist({ start: start + 4, len: text_end - start - 4 }), False, False)
	{ token: { kind: Comment(decoded.text), span: { start, end: $end } }, reader: { ..reader, pos: $end, diagnostics: $diagnostics.concat(decoded.diagnostics.map(|d| { ..d, offset: start + 4 + d.offset })) } }
}

doctype = |reader| {
	bytes = reader.bytes
	start = reader.pos
	var $pos = start + 9
	var $diagnostics = reader.diagnostics
	if !HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
		$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
	}
	while $pos < bytes.len() and HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
		$pos = $pos + 1
	}
	name_start = $pos
	while $pos < bytes.len() and !HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) and HtmlTokenizer.byte_at(bytes, $pos) != '>' {
		$pos = $pos + 1
	}
	name = HtmlTokenizer.lower(HtmlTokenizer.text_at(bytes, name_start, $pos))
	var $quirks = name != "html"
	if name == "" {
		$diagnostics = $diagnostics.append({ code: MissingDoctypeName, offset: name_start })
	}
	while $pos < bytes.len() and HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
		$pos = $pos + 1
	}
	var $public_id = ""
	var $system_id = ""
	public = HtmlTokenizer.starts_caseless(bytes, $pos, "public")
	system = HtmlTokenizer.starts_caseless(bytes, $pos, "system")
	if public or system {
		$pos = $pos + 6
		while $pos < bytes.len() and HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
			$pos = $pos + 1
		}
		id = quoted_identifier(bytes, $pos)
		$pos = id.end
		if public {
			$public_id = id.text
		} else {
			$system_id = id.text
		}
		if !id.valid {
			$quirks = True
			$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
		}
		while $pos < bytes.len() and HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
			$pos = $pos + 1
		}
		if public and ['"', '\''].contains(HtmlTokenizer.byte_at(bytes, $pos)) {
			second = quoted_identifier(bytes, $pos)
			$system_id = second.text
			$pos = second.end
		}
	}
	if HtmlTokenizer.byte_at(bytes, $pos) != '>' {
		$quirks = True
		$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
		while $pos < bytes.len() and HtmlTokenizer.byte_at(bytes, $pos) != '>' {
			$pos = $pos + 1
		}
	}
	if $pos == bytes.len() {
		$diagnostics = $diagnostics.append({ code: EofInDoctype, offset: $pos })
	}
	else {
		$pos = $pos + 1
	}
	{ token: { kind: Doctype({ name, public_id: $public_id, system_id: $system_id, quirks: $quirks }), span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
}

quoted_identifier = |bytes, start| {
	quote = HtmlTokenizer.byte_at(bytes, start)
	if !['"', '\''].contains(quote) {
		return { text: "", end: start, valid: False }
	}
	var $end = start + 1
	while $end < bytes.len() and HtmlTokenizer.byte_at(bytes, $end) != quote and HtmlTokenizer.byte_at(bytes, $end) != '>' {
		$end = $end + 1
	}
	valid = HtmlTokenizer.byte_at(bytes, $end) == quote
	{ text: HtmlTokenizer.text_at(bytes, start + 1, $end), end: if (valid) $end + 1 else $end, valid }
}

consume_reference = |bytes, start, attribute| {
	if HtmlTokenizer.byte_at(bytes, start + 1) == '#' {
		hex = ['x', 'X'].contains(HtmlTokenizer.byte_at(bytes, start + 2))
		first = start + if (hex) 3 else 2
		var $pos = first
		var $number = 0.U64
		base = if (hex) 16 else 10
		while $pos < bytes.len() {
			c = HtmlTokenizer.byte_at(bytes, $pos)
			digit = if (c >= '0' and c <= '9') c - '0' else if (c >= 'a' and c <= 'f') c - 'a' + 10 else if (c >= 'A' and c <= 'F') c - 'A' + 10 else 255
			if digit.to_u64() >= base {
				break
			}
			$number = if ($number < 0x110000) $number * base + digit.to_u64() else 0x110000
			$pos = $pos + 1
		}
		if $pos == first {
			return { bytes: ['&'], end: start + 1, diagnostics: [{ code: InvalidCharacterReference, offset: start }] }
		}
		var $diagnostics = []
		if HtmlTokenizer.byte_at(bytes, $pos) == ';' {
			$pos = $pos + 1
		} else {
			$diagnostics = $diagnostics.append({ code: MissingSemicolon, offset: start })
		}
		var $scalar = $number
		if $scalar == 0 or $scalar > 0x10FFFF or ($scalar >= 0xD800 and $scalar <= 0xDFFF) {
			$scalar = 0xFFFD
			$diagnostics = $diagnostics.append({ code: InvalidCharacterReference, offset: start })
		} else if $scalar >= 0x80 and $scalar <= 0x9F {
			$scalar = c1.get($scalar - 0x80) ?? $scalar
			$diagnostics = $diagnostics.append({ code: InvalidCharacterReference, offset: start })
		} else if ($scalar >= 0xFDD0 and $scalar <= 0xFDEF) or ($scalar % 65536 >= 65534) or ($scalar < 32 and ![9, 10, 12, 13].contains($scalar)) or $scalar == 127 {
			$diagnostics = $diagnostics.append({ code: InvalidCharacterReference, offset: start })
		}
		return { bytes: encode_scalar($scalar), end: $pos, diagnostics: $diagnostics }
	}
	var $end = start + 1
	while $end < bytes.len() and $end - start <= 32 and (HtmlTokenizer.ascii_alpha(HtmlTokenizer.byte_at(bytes, $end)) or (HtmlTokenizer.byte_at(bytes, $end) >= '0' and HtmlTokenizer.byte_at(bytes, $end) <= '9')) {
		$end = $end + 1
	}
	if HtmlTokenizer.byte_at(bytes, $end) == ';' {
		match MarkdownEntities.lookup(bytes.sublist({ start: start + 1, len: $end - start - 1 })) {
			Ok(replacement) => return { bytes: replacement, end: $end + 1, diagnostics: [] }
			Err(_) => {}
		}
	}
	var $candidate = $end
	while $candidate > start + 1 {
		name = HtmlTokenizer.text_at(bytes, start + 1, $candidate)
		next = HtmlTokenizer.byte_at(bytes, $candidate)
		if legacy.contains("|${name}|") and !(attribute and (HtmlTokenizer.ascii_alpha(next) or (next >= '0' and next <= '9') or next == '=')) {
			match MarkdownEntities.lookup(name.to_utf8()) {
				Ok(replacement) => return { bytes: replacement, end: $candidate, diagnostics: [{ code: MissingSemicolon, offset: start }] }
				Err(_) => {}
			}
		}
		$candidate = $candidate - 1
	}
	{ bytes: ['&'], end: start + 1, diagnostics: if (HtmlTokenizer.byte_at(bytes, $end) == ';') [{ code: UnknownReference, offset: start }] else [] }
}

encode_scalar : U64 -> List(U8)
encode_scalar = |n| {
	if n < 0x80 {
		[n.to_u8_wrap()]
	}
	else if n < 0x800 {
		[(0xC0 + n // 64).to_u8_wrap(), (0x80 + n % 64).to_u8_wrap()]
	}
	else if n < 0x10000 {
		[(0xE0 + n // 4096).to_u8_wrap(), (0x80 + n // 64 % 64).to_u8_wrap(), (0x80 + n % 64).to_u8_wrap()]
	}
	else {
		[(0xF0 + n // 262144).to_u8_wrap(), (0x80 + n // 4096 % 64).to_u8_wrap(), (0x80 + n // 64 % 64).to_u8_wrap(), (0x80 + n % 64).to_u8_wrap()]
	}
}

c1 : List(U64)
c1 = [0x20AC, 0x81, 0x201A, 0x192, 0x201E, 0x2026, 0x2020, 0x2021, 0x2C6, 0x2030, 0x160, 0x2039, 0x152, 0x8D, 0x17D, 0x8F, 0x90, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x2DC, 0x2122, 0x161, 0x203A, 0x153, 0x9D, 0x17E, 0x178]

legacy = "|AElig||AMP||Aacute||Acirc||Agrave||Aring||Atilde||Auml||COPY||Ccedil||ETH||Eacute||Ecirc||Egrave||Euml||GT||Iacute||Icirc||Igrave||Iuml||LT||Ntilde||Oacute||Ocirc||Ograve||Oslash||Otilde||Ouml||QUOT||REG||THORN||Uacute||Ucirc||Ugrave||Uuml||Yacute||aacute||acirc||acute||aelig||agrave||amp||aring||atilde||auml||brvbar||ccedil||cedil||cent||copy||curren||deg||divide||eacute||ecirc||egrave||eth||euml||frac12||frac14||frac34||gt||iacute||icirc||iexcl||igrave||iquest||iuml||laquo||lt||macr||micro||middot||nbsp||not||ntilde||oacute||ocirc||ograve||ordf||ordm||oslash||otilde||ouml||para||plusmn||pound||quot||raquo||reg||sect||shy||sup1||sup2||sup3||szlig||thorn||times||uacute||ucirc||ugrave||uml||uuml||yacute||yen||yuml|"

expect HtmlTokenizer.normalized("a\r\nb&#x80;&amp;".to_utf8(), True, False).text == "a\nb€&"
expect HtmlTokenizer.normalized("&notit;".to_utf8(), True, False).text == "¬it;"
expect HtmlTokenizer.normalized("&notit;".to_utf8(), True, True).text == "&notit;"
expect {
	r = HtmlTokenizer.next({ bytes: "<INPUT A='x&gt;y' a=z disabled>".to_utf8(), pos: 0, diagnostics: [] }, Data, False)
	match r.token.kind {
		Start(t) => t.name == "input" and t.attributes.map(|a| (a.name, a.value)) == [("a", "x>y"), ("disabled", "")] and r.reader.diagnostics.any(|d| d.code == DuplicateAttribute("a"))
		_ => False
	}
}
