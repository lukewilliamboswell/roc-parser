import MarkdownEntities
import MarkupHtmlError

## HTML tokenization primitives. Offsets always address the unmodified input.
## Tree construction supplies the content model and foreign-content flag.
HtmlTokenizer :: [].{
	Span : { start : U64, end : U64 }
	Attribute : { name : Str, value : Str, span : Span }
	Tag : { name : Str, attributes : List(Attribute), self_closing : Bool }
	Kind : [Start(Tag), End(Str), Characters(Str), Comment(Str), ProcessingInstruction({ target : Str, data : Str }), Doctype({ name : Str, public_id : Str, system_id : Str, has_public : Bool, has_system : Bool, quirks : Bool }), Eof]
	Token : { kind : Kind, span : Span }
	Code : MarkupHtmlError.Code
	Diagnostic : { code : Code, offset : U64 }
	## Data keeps whitespace/null/other runs separate for tree-construction
	## states that distinguish them. DataMixed combines a normal text run when
	## the caller can process all three character classes together.
	Mode : [Data, DataMixed, Rcdata(Str), Rawtext(Str), Script, RcdataFragment, RawtextFragment, ScriptFragment, Plaintext]
	Reader : { bytes : List(U8), pos : U64, diagnostics : List(Diagnostic) }

	## Read one token. Low-level callers choose allocation ceilings with next_with.
	next : Reader, Mode, Bool -> { token : Token, reader : Reader }
	next = |reader, mode, foreign| {
		result = next_with(reader, mode, foreign, { max_diagnostics: 18_446_744_073_709_551_615, max_attributes: 18_446_744_073_709_551_615 })
		{ token: result.token, reader: result.reader }
	}

	## Tokenizer ceilings. One excess attribute/diagnostic is retained as a
	## witness so the document reader can reject the exceeded resource limit.
	Limits := { max_attributes : U64 ?? 1024, max_diagnostics : U64 ?? 1024 }

	## Bound allocation while reading a token, before building a giant tag or
	## diagnostic list. Diagnostic exhaustion emits Eof with an excess witness;
	## attribute exhaustion emits a partial start tag with max_attributes+1 attrs.
	next_with : Reader, Mode, Bool, Limits -> { token : Token, reader : Reader, limited : [NoLimit, Attributes, Diagnostics] }
	next_with = |reader, mode, foreign, limits| {
		result = scan_next({ bytes: reader.bytes, pos: reader.pos, diagnostics: reader.diagnostics, max_diagnostics: limits.max_diagnostics, max_attributes: limits.max_attributes, attribute_limit: False }, mode, foreign)
		token = if result.reader.diagnostics.len() > limits.max_diagnostics {
			{ ..result.token, kind: Eof }
		} else result.token
		limited = if (result.reader.diagnostics.len() > limits.max_diagnostics) Diagnostics else if (result.reader.attribute_limit) Attributes else NoLimit
		diagnostics = result.reader.diagnostics.take_first(if (limits.max_diagnostics < 18_446_744_073_709_551_615) limits.max_diagnostics + 1 else limits.max_diagnostics)
		{ token, reader: { bytes: result.reader.bytes, pos: result.reader.pos, diagnostics }, limited }
	}

	scan_next : ScanReader, Mode, Bool -> { token : Token, reader : ScanReader }
	scan_next = |reader, mode, foreign| {
		bytes = reader.bytes
		start = reader.pos
		if start >= bytes.len() or reader.diagnostics.len() > reader.max_diagnostics {
			return { token: { kind: Eof, span: { start, end: start } }, reader }
		}
		match mode {
			Plaintext | RawtextFragment => return characters(reader, bytes.len(), False, False)
			RcdataFragment => return characters(reader, bytes.len(), True, False)
			ScriptFragment => return script_characters_with(reader, False)
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
			Data | DataMixed => {}
		}
		if byte_at(bytes, start) != '<' {
			if mode == DataMixed {
				var $end = start
				var $normalize = False
				while $end < bytes.len() and byte_at(bytes, $end) != '<' {
					byte = byte_at(bytes, $end)
					if byte == '&' or byte == '\r' or byte == 0 {
						$normalize = True
					}
					$end = $end + 1
				}
				if $normalize {
					return characters(reader, $end, True, True)
				}
				# Ordinary text already has its final bytes. Avoid rebuilding it
				# one byte at a time through the normalization buffer.
				text = Str.from_utf8_lossy(bytes.sublist({ start, len: $end - start }))
				return { token: { kind: Characters(text), span: { start, end: $end } }, reader: { ..reader, pos: $end } }
			}
			return data_characters(reader)
		}

		next_byte = byte_at(bytes, start + 1)
		if next_byte == '!' {
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
				text = preprocessed_text(bytes.sublist({ start: start + 9, len: $end - start - 9 }))
				end = if ($end < bytes.len()) $end + 3 else $end
				return { token: { kind: Characters(text), span: { start, end } }, reader: { ..reader, pos: end } }
			}
			return bogus_comment(reader, start + 2, BogusComment, start)
		}
		if next_byte == '?' {
			return processing_instruction(reader)
		}

		closing = next_byte == '/'

		name_start = start + if (closing) 2 else 1
		if closing and !ascii_alpha(byte_at(bytes, name_start)) {
			if byte_at(bytes, name_start) == '>' {
				return { token: { kind: Characters(""), span: { start, end: name_start + 1 } }, reader: { ..reader, pos: name_start + 1, diagnostics: reader.diagnostics.append({ code: InvalidTagOpen, offset: start }) } }
			}
			if name_start < bytes.len() {
				return bogus_comment(reader, name_start, BogusComment, start)
			}
			return { token: { kind: Characters("</"), span: { start, end: name_start } }, reader: { ..reader, pos: name_start, diagnostics: reader.diagnostics.append({ code: InvalidTagOpen, offset: start }) } }
		}
		if !ascii_alpha(byte_at(bytes, name_start)) {
			return { token: { kind: Characters("<"), span: { start, end: start + 1 } }, reader: { ..reader, pos: start + 1, diagnostics: reader.diagnostics.append({ code: InvalidTagOpen, offset: start }) } }
		}
		var $pos = name_start
		while $pos < bytes.len() and !space(byte_at(bytes, $pos)) and !(
			match byte_at(bytes, $pos) {
				'/' | '>' => True
				_ => False
			}
		) {
			$pos = $pos + 1
		}
		normalized_name = normalized_limited(bytes.sublist({ start: name_start, len: $pos - name_start }), False, False, True, diagnostic_budget(reader))
		name = lower(normalized_name.text)
		var $attributes = []
		var $diagnostics = reader.diagnostics.concat(normalized_name.diagnostics.map(|d| { ..d, offset: name_start + d.offset }))
		var $self_closing = False
		var $done = False
		while $pos < bytes.len() and !$done {
			if $diagnostics.len() > reader.max_diagnostics {
				return exhausted(reader, $pos, $diagnostics)
			}
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
				if byte_at(bytes, $pos) == '=' {
					$diagnostics = $diagnostics.append({ code: InvalidAttributeCharacter, offset: $pos })
					$pos = $pos + 1
				}
				while $pos < bytes.len() and !space(byte_at(bytes, $pos)) and !(
					match byte_at(bytes, $pos) {
						'=' | '/' | '>' => True
						_ => False
					}
				) {
					if $diagnostics.len() > reader.max_diagnostics {
						return exhausted(reader, $pos, $diagnostics)
					}
					if match byte_at(bytes, $pos) {
						'"' | '\'' | '<' => True
						_ => False
					} {
						$diagnostics = $diagnostics.append({ code: InvalidAttributeCharacter, offset: $pos })
					}
					$pos = $pos + 1
				}
				if $pos == attr_start {
					$pos = $pos + 1
				}
				normalized_attr = normalized_limited(bytes.sublist({ start: attr_start, len: $pos - attr_start }), False, False, True, remaining_diagnostics(reader.max_diagnostics, $diagnostics.len()))
				attr_name = lower(normalized_attr.text)
				$diagnostics = $diagnostics.concat(normalized_attr.diagnostics.map(|d| { ..d, offset: attr_start + d.offset }))
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
						if !quoted and (
							match byte_at(bytes, $pos) {
								'"' | '\'' | '<' | '=' | '`' => True
								_ => False
							}
						) {
							$diagnostics = $diagnostics.append({ code: InvalidAttributeCharacter, offset: $pos })
						}
						$pos = $pos + 1
					}
					decoded = normalized_limited(bytes.sublist({ start: value_start, len: $pos - value_start }), True, True, True, remaining_diagnostics(reader.max_diagnostics, $diagnostics.len()))
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
					if $attributes.len() > reader.max_attributes {
						return { token: { kind: if (closing) End(name) else Start({ name, attributes: $attributes, self_closing: False }), span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics, attribute_limit: True } }
					}
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

	## Input-stream parse errors from original UTF-8, before references expand.
	## Call once when creating a reader. Nulls are handled by individual states;
	## decoded string input retains U+FEFF rather than repeating byte decoding.
	input_diagnostics : List(U8) -> List(Diagnostic)
	input_diagnostics = |bytes| input_diagnostics_with(bytes, 1024)

	## Stop after the first diagnostic past the budget. That extra witness lets
	## the tree reader reject exhaustion without silently accepting truncation.
	input_diagnostics_with : List(U8), U64 -> List(Diagnostic)
	input_diagnostics_with = |bytes, limit| {
		var $diagnostics = []
		var $pos = 0
		while $pos < bytes.len() {
			lead = byte_at(bytes, $pos).to_u64()
			width = if (lead >= 0xF0) 4 else if (lead >= 0xE0) 3 else if (lead >= 0xC0) 2 else 1
			var $scalar = if (width == 1) lead else if (width == 2) lead % 32 else if (width == 3) lead % 16 else lead % 8
			var $i = 1
			while $i < width {
				$scalar = $scalar * 64 + byte_at(bytes, $pos + $i).to_u64() % 64
				$i = $i + 1
			}
			if ($scalar >= 1 and $scalar <= 8) or $scalar == 11 or ($scalar >= 14 and $scalar <= 31) or ($scalar >= 127 and $scalar <= 159) {
				$diagnostics = $diagnostics.append({ code: ControlCharacterInInputStream, offset: $pos })
			} else if ($scalar >= 0xFDD0 and $scalar <= 0xFDEF) or $scalar % 65536 >= 65534 {
				$diagnostics = $diagnostics.append({ code: NoncharacterInInputStream, offset: $pos })
			}
			if $diagnostics.len() > limit {
				break
			}
			$pos = $pos + width
		}
		$diagnostics
	}

	lower : Str -> Str
	lower = |s| {
		var $bytes = s.to_utf8()
		var $changed = False
		var $at = 0
		while $at < $bytes.len() {
			byte = $bytes.get($at) ?? 0
			if byte >= 'A' and byte <= 'Z' {
				$bytes = $bytes.set($at, byte + 32) ?? []
				$changed = True
			}
			$at = $at + 1
		}
		if ($changed) Str.from_utf8_lossy($bytes) else s
	}
	space : U8 -> Bool
	space = |c| match c {
		' ' | '\t' | '\n' | '\r' | 12 => True
		_ => False
	}
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
	end_tag_at = |bytes, at, name| starts(bytes, at, "</") and starts_caseless(bytes, at + 2, name) and (
		space(byte_at(bytes, at + 2 + name.count_utf8_bytes())) or (
			match byte_at(bytes, at + 2 + name.count_utf8_bytes()) {
				'/' | '>' => True
				_ => False
			}
		)
	)

	## HTML input preprocessing and character-reference consumption.
	normalized : List(U8), Bool, Bool -> { text : Str, diagnostics : List(Diagnostic) }
	normalized = |bytes, references, attribute| normalized_with_nulls(bytes, references, attribute, True)

	normalized_with_nulls : List(U8), Bool, Bool, Bool -> { text : Str, diagnostics : List(Diagnostic) }
	normalized_with_nulls = |bytes, references, attribute, replace_nulls| normalized_limited(bytes, references, attribute, replace_nulls, 18_446_744_073_709_551_615)

	normalized_limited : List(U8), Bool, Bool, Bool, U64 -> { text : Str, diagnostics : List(Diagnostic) }
	normalized_limited = |bytes, references, attribute, replace_nulls, limit| {
		var $out = []
		var $diagnostics = []
		var $pos = 0
		while $pos < bytes.len() {
			if $diagnostics.len() > limit {
				break
			}
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
		{ text: Str.from_utf8_lossy($out), diagnostics: $diagnostics.take_first(if (limit < 18_446_744_073_709_551_615) limit + 1 else limit) }
	}
}

characters = |reader, end, references, data| {
	decoded = HtmlTokenizer.normalized_limited(reader.bytes.sublist({ start: reader.pos, len: end - reader.pos }), references, False, !data, diagnostic_budget(reader))
	{ token: { kind: Characters(decoded.text), span: { start: reader.pos, end } }, reader: { ..reader, pos: end, diagnostics: reader.diagnostics.concat(decoded.diagnostics.map(|d| { ..d, offset: reader.pos + d.offset })) } }
}

raw_characters = |reader, name, references, script| {
	if script {
		script_characters_with(reader, True)
	} else {
		var $end = reader.pos
		while $end < reader.bytes.len() and !HtmlTokenizer.end_tag_at(reader.bytes, $end, name) {
			$end = $end + 1
		}
		characters(reader, $end, references, False)
	}
}

script_characters_with = |reader, recognize_end_tags| {
	scanned = script_end(reader.bytes, reader.pos, recognize_end_tags)
	result = characters(reader, scanned.end, False, False)
	if scanned.escaped_eof {
		{ ..result, reader: { ..result.reader, diagnostics: result.reader.diagnostics.append({ code: EofInScriptCommentLikeText, offset: scanned.end }) } }
	} else result
}

# Script data's escaped/double-escaped states cannot be modelled as a search
# for comment delimiters: the dash/dash-dash and temporary-buffer states alter
# which </script> is an appropriate end tag. Scan them explicitly once.
script_end = |bytes, start, recognize_end_tags| {
	var $pos = start
	var $state = ScriptData
	var $buffer_len = 0
	var $buffer_matches = True
	while $pos < bytes.len() {
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		var $consume = True
		match $state {
			ScriptData => {
				if byte == '<' {
					if recognize_end_tags and HtmlTokenizer.end_tag_at(bytes, $pos, "script") {
						break
					}
					if HtmlTokenizer.byte_at(bytes, $pos + 1) == '!' {
						$pos = $pos + 1
						$state = EscapeStart
					}
				}
			}
			EscapeStart => {
				if byte == '-' {
					$state = EscapeStartDash
				} else {
					$state = ScriptData
					$consume = False
				}
			}
			EscapeStartDash => {
				if byte == '-' {
					$state = EscapedDashDash
				} else {
					$state = ScriptData
					$consume = False
				}
			}
			Escaped => {
				if byte == '-' {
					$state = EscapedDash
				}
				else if byte == '<' {
					$state = EscapedLessThan
				}
			}
			EscapedDash => {
				if byte == '-' {
					$state = EscapedDashDash
				}
				else if byte == '<' {
					$state = EscapedLessThan
				}
				else {
					$state = Escaped
				}
			}
			EscapedDashDash => {
				if byte == '<' {
					$state = EscapedLessThan
				}
				else if byte == '>' {
					$state = ScriptData
				}
				else if byte != '-' {
					$state = Escaped
				}
			}
			EscapedLessThan => {
				if recognize_end_tags and byte == '/' and HtmlTokenizer.end_tag_at(bytes, $pos - 1, "script") {
					$pos = $pos - 1
					break
				}
				if HtmlTokenizer.ascii_alpha(byte) {
					$buffer_len = 0
					$buffer_matches = True
					$state = DoubleEscapeStart
					$consume = False
				}
				else {
					$state = Escaped
					$consume = False
				}
			}
			DoubleEscapeStart | DoubleEscapeEnd => {
				if HtmlTokenizer.ascii_alpha(byte) {
					if (if (byte >= 'A' and byte <= 'Z') byte + 32 else byte) != HtmlTokenizer.byte_at("script".to_utf8(), $buffer_len) {
						$buffer_matches = False
					}
					$buffer_len = $buffer_len + 1
				}
				else if HtmlTokenizer.space(byte) or byte == '/' or byte == '>' {
					exact = $buffer_matches and $buffer_len == 6
					$state = if $state == DoubleEscapeStart {
						if (exact) DoubleEscaped else Escaped
					} else {
						if (exact) Escaped else DoubleEscaped
					}
				}
				else {
					$state = if ($state == DoubleEscapeStart) Escaped else DoubleEscaped
					$consume = False
				}
			}
			DoubleEscaped => {
				if byte == '-' {
					$state = DoubleEscapedDash
				}
				else if byte == '<' {
					$state = DoubleEscapedLessThan
				}
			}
			DoubleEscapedDash => {
				if byte == '-' {
					$state = DoubleEscapedDashDash
				}
				else if byte == '<' {
					$state = DoubleEscapedLessThan
				}
				else {
					$state = DoubleEscaped
				}
			}
			DoubleEscapedDashDash => {
				if byte == '<' {
					$state = DoubleEscapedLessThan
				}
				else if byte == '>' {
					$state = ScriptData
				}
				else if byte != '-' {
					$state = DoubleEscaped
				}
			}
			DoubleEscapedLessThan => {
				if byte == '/' {
					$buffer_len = 0
					$buffer_matches = True
					$state = DoubleEscapeEnd
				}
				else {
					$state = DoubleEscaped
					$consume = False
				}
			}
		}
		if $consume {
			$pos = $pos + 1
		}
	}
	{ end: $pos, escaped_eof: $pos >= bytes.len() and $state != ScriptData and $state != EscapeStart and $state != EscapeStartDash }
}

comment = |reader| {
	bytes = reader.bytes
	start = reader.pos
	var $pos = start + 4
	var $state = CommentStart
	var $out = []
	var $diagnostics = reader.diagnostics
	var $done = False
	while !$done {
		if $diagnostics.len() > reader.max_diagnostics {
			return exhausted(reader, $pos, $diagnostics)
		}
		if $pos >= bytes.len() {
			$diagnostics = $diagnostics.append({ code: EofInComment, offset: $pos })
			break
		}
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		var $consume = True
		match $state {
			CommentStart => {
				if byte == '-' {
					$state = StartDash
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $pos })
					$done = True
				}
				else {
					$state = CommentData
					$consume = False
				}
			}
			StartDash => {
				if byte == '-' {
					$state = CommentEnd
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $pos })
					$done = True
				}
				else {
					$out = $out.append('-')
					$state = CommentData
					$consume = False
				}
			}
			CommentData => {
				if byte == '<' {
					$out = $out.append('<')
					$state = LessThan
				}
				else if byte == '-' {
					$state = EndDash
				}
				else if byte == 0 {
					$out = $out.concat([239, 191, 189])
					$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				}
				else {
					$out = $out.append(byte)
				}
			}
			LessThan => {
				if byte == '!' {
					$out = $out.append('!')
					$state = Bang
				}
				else if byte == '<' {
					$out = $out.append('<')
				}
				else {
					$state = CommentData
					$consume = False
				}
			}
			Bang => {
				if byte == '-' {
					$state = BangDash
				} else {
					$state = CommentData
					$consume = False
				}
			}
			BangDash => {
				if byte == '-' {
					$state = BangDashDash
				} else {
					$state = EndDash
					$consume = False
				}
			}
			BangDashDash => {
				if byte != '>' {
					$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $pos })
				}
				$state = CommentEnd
				$consume = False
			}
			EndDash => {
				if byte == '-' {
					$state = CommentEnd
				} else {
					$out = $out.append('-')
					$state = CommentData
					$consume = False
				}
			}
			CommentEnd => {
				if byte == '>' {
					$done = True
				}
				else if byte == '!' {
					$state = EndBang
				}
				else if byte == '-' {
					$out = $out.append('-')
				}
				else {
					$out = $out.concat(['-', '-'])
					$state = CommentData
					$consume = False
				}
			}
			EndBang => {
				if byte == '-' {
					$out = $out.concat(['-', '-', '!'])
					$state = EndDash
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: IncorrectComment, offset: $pos })
					$done = True
				}
				else {
					$out = $out.concat(['-', '-', '!'])
					$state = CommentData
					$consume = False
				}
			}
		}
		if $consume {
			$pos = $pos + 1
		}
	}
	decoded = HtmlTokenizer.normalized($out, False, False)
	{ token: { kind: Comment(decoded.text), span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
}

bogus_comment = |reader, content_start, code, offset| {
	bytes = reader.bytes
	start = reader.pos
	var $end = content_start
	while $end < bytes.len() and HtmlTokenizer.byte_at(bytes, $end) != '>' {
		$end = $end + 1
	}
	decoded = HtmlTokenizer.normalized_limited(bytes.sublist({ start: content_start, len: $end - content_start }), False, False, True, diagnostic_budget(reader))
	end = if ($end < bytes.len()) $end + 1 else $end
	{ token: { kind: Comment(decoded.text), span: { start, end } }, reader: { ..reader, pos: end, diagnostics: reader.diagnostics.append({ code, offset }).concat(decoded.diagnostics.map(|d| { ..d, offset: content_start + d.offset })) } }
}

doctype = |reader| {
	bytes = reader.bytes
	start = reader.pos
	var $pos = start + 9
	var $diagnostics = reader.diagnostics
	var $state = BeforeName
	var $name = []
	var $public = []
	var $system = []
	var $has_public = False
	var $has_system = False
	var $quirks = False
	var $quote = 0.U8
	var $done = False
	if $pos < bytes.len() and !HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) and HtmlTokenizer.byte_at(bytes, $pos) != '>' {
		$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
	}
	while !$done {
		if $diagnostics.len() > reader.max_diagnostics {
			return exhausted(reader, $pos, $diagnostics)
		}
		if $pos >= bytes.len() {
			if $state != Bogus {
				$diagnostics = $diagnostics.append({ code: EofInDoctype, offset: $pos })
				$quirks = True
			}
			break
		}
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		var $consume = True
		match $state {
			BeforeName => {
				if (HtmlTokenizer.space(byte)) {}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: MissingDoctypeName, offset: $pos })
					$quirks = True
					$done = True
				}
				else {
					$state = Name
					$consume = False
				}
			}
			Name => {
				if HtmlTokenizer.space(byte) {
					$state = AfterName
				}
				else if byte == '>' {
					$done = True
				}
				else if byte == 0 {
					$name = $name.concat([239, 191, 189])
					$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				}
				else {
					$name = $name.append(if (byte >= 'A' and byte <= 'Z') byte + 32 else byte)
				}
			}
			AfterName => {
				if (HtmlTokenizer.space(byte)) {}
				else if byte == '>' {
					$done = True
				}
				else if HtmlTokenizer.starts_caseless(bytes, $pos, "public") {
					$pos = $pos + 6
					$state = AfterPublicKeyword
					$consume = False
				}
				else if HtmlTokenizer.starts_caseless(bytes, $pos, "system") {
					$pos = $pos + 6
					$state = AfterSystemKeyword
					$consume = False
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$state = Bogus
					$consume = False
				}
			}
			AfterPublicKeyword | BeforePublicIdentifier => {
				if HtmlTokenizer.space(byte) {
					$state = BeforePublicIdentifier
				}
				else if byte == '\'' or byte == '"' {
					if $state == AfterPublicKeyword {
						$diagnostics = $diagnostics.append({ code: MissingWhitespace, offset: $pos })
					}
					$has_public = True
					$quote = byte
					$state = PublicIdentifier
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$done = True
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$state = Bogus
					$consume = False
				}
			}
			PublicIdentifier => {
				if byte == $quote {
					$state = AfterPublicIdentifier
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$done = True
				}
				else if byte == 0 {
					$public = $public.concat([239, 191, 189])
					$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				}
				else {
					$public = $public.append(byte)
				}
			}
			AfterPublicIdentifier | BetweenIdentifiers => {
				if HtmlTokenizer.space(byte) {
					$state = BetweenIdentifiers
				}
				else if byte == '>' {
					$done = True
				}
				else if byte == '\'' or byte == '"' {
					if $state == AfterPublicIdentifier {
						$diagnostics = $diagnostics.append({ code: MissingWhitespace, offset: $pos })
					}
					$has_system = True
					$quote = byte
					$state = SystemIdentifier
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$state = Bogus
					$consume = False
				}
			}
			AfterSystemKeyword | BeforeSystemIdentifier => {
				if HtmlTokenizer.space(byte) {
					$state = BeforeSystemIdentifier
				}
				else if byte == '\'' or byte == '"' {
					if $state == AfterSystemKeyword {
						$diagnostics = $diagnostics.append({ code: MissingWhitespace, offset: $pos })
					}
					$has_system = True
					$quote = byte
					$state = SystemIdentifier
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$done = True
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$state = Bogus
					$consume = False
				}
			}
			SystemIdentifier => {
				if byte == $quote {
					$state = AfterSystemIdentifier
				}
				else if byte == '>' {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$quirks = True
					$done = True
				}
				else if byte == 0 {
					$system = $system.concat([239, 191, 189])
					$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				}
				else {
					$system = $system.append(byte)
				}
			}
			AfterSystemIdentifier => {
				if (HtmlTokenizer.space(byte)) {}
				else if byte == '>' {
					$done = True
				}
				else {
					$diagnostics = $diagnostics.append({ code: InvalidDoctype, offset: $pos })
					$state = Bogus
					$consume = False
				}
			}
			Bogus => {
				if byte == '>' {
					$done = True
				}
				else if byte == 0 {
					$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
				}
			}
		}
		if $consume {
			$pos = $pos + 1
		}
	}
	public_id = HtmlTokenizer.normalized($public, False, False).text
	system_id = HtmlTokenizer.normalized($system, False, False).text
	{ token: { kind: Doctype({ name: Str.from_utf8_lossy($name), public_id, system_id, has_public: $has_public, has_system: $has_system, quirks: $quirks }), span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
}

consume_reference = |bytes, start, attribute| {
	if HtmlTokenizer.byte_at(bytes, start + 1) == '#' {
		hex = (
			match HtmlTokenizer.byte_at(bytes, start + 2) {
				'x' | 'X' => True
				_ => False
			}
		)
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
		} else if ($scalar >= 0xFDD0 and $scalar <= 0xFDEF) or ($scalar % 65536 >= 65534) or $scalar == 13 or (
			$scalar < 32 and !(
				match $scalar {
					9 | 10 | 12 | 13 => True
					_ => False
				}
			)
		) or $scalar == 127 {
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

# The HTML processing-instruction states preserve target case and use HTML's
# target grammar. Disallowed/invalid targets fall back to a bogus comment;
# unlike XML, either > or ?> closes a valid instruction.
processing_instruction = |reader| {
	bytes = reader.bytes
	start = reader.pos
	var $pos = start + 2
	var $diagnostics = reader.diagnostics
	if $pos >= bytes.len() {
		return { token: { kind: Eof, span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics.append({ code: EofInProcessingInstruction, offset: $pos }) } }
	}
	if !HtmlTokenizer.ascii_alpha(HtmlTokenizer.byte_at(bytes, $pos)) and HtmlTokenizer.byte_at(bytes, $pos) != '_' {
		return pi_bogus(reader, InvalidFirstCharacterOfProcessingInstructionTarget, $pos)
	}
	target_start = $pos
	while $pos < bytes.len() {
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		if HtmlTokenizer.ascii_alpha(byte) or (byte >= '0' and byte <= '9') or byte == '-' or byte == '_' {
			$pos = $pos + 1
		} else {
			break
		}
	}
	if $pos >= bytes.len() {
		return { token: { kind: Eof, span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics.append({ code: EofInProcessingInstruction, offset: $pos }) } }
	}
	boundary = HtmlTokenizer.byte_at(bytes, $pos)
	if !HtmlTokenizer.space(boundary) and boundary != '?' and boundary != '>' {
		return pi_bogus(reader, InvalidProcessingInstructionTarget, $pos)
	}
	target = HtmlTokenizer.text_at(bytes, target_start, $pos)
	if match HtmlTokenizer.lower(target) {
		"xml" | "xml-stylesheet" => True
		_ => False
	} {
		return pi_bogus(reader, DisallowedProcessingInstructionTarget, $pos)
	}
	while $pos < bytes.len() and HtmlTokenizer.space(HtmlTokenizer.byte_at(bytes, $pos)) {
		$pos = $pos + 1
	}
	data_start = $pos
	while $pos < bytes.len() and HtmlTokenizer.byte_at(bytes, $pos) != '>' {
		$pos = $pos + 1
	}
	if $pos >= bytes.len() {
		return { token: { kind: Eof, span: { start, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics.append({ code: EofInProcessingInstruction, offset: $pos }) } }
	}
	data_end = if ($pos > data_start and HtmlTokenizer.byte_at(bytes, $pos - 1) == '?') $pos - 1 else $pos
	# Input preprocessing normalizes newlines. PI data has no reference or
	# null-replacement state: all other input characters are appended verbatim.
	data = preprocessed_text(bytes.sublist({ start: data_start, len: data_end - data_start }))
	{ token: { kind: ProcessingInstruction({ target, data }), span: { start, end: $pos + 1 } }, reader: { ..reader, pos: $pos + 1, diagnostics: $diagnostics } }
}

pi_bogus = |reader, code, offset| bogus_comment(reader, reader.pos + 1, code, offset)

# Processing-instruction targets and data use the current HTML grammar,
# including invalid-target bogus comments and EOF without an emitted PI.
expect token_kind("<?something>") == ProcessingInstruction({ target: "something", data: "" })
expect token_kind("<?something good>") == ProcessingInstruction({ target: "something", data: "good" })
expect token_kind("<?UPPERCASE   data?>") == ProcessingInstruction({ target: "UPPERCASE", data: "data" })
expect token_kind("<?_prefix-42 d >") == ProcessingInstruction({ target: "_prefix-42", data: "d " })
expect token_kind("<?hey?there>") == ProcessingInstruction({ target: "hey", data: "?there" })
expect token_kind("<?hey??>") == ProcessingInstruction({ target: "hey", data: "?" })
expect token_kind("<?hey\t\r\n\u(C)there=1?>") == ProcessingInstruction({ target: "hey", data: "there=1" })
expect token_kind("<?hey a\r\nb\rc>") == ProcessingInstruction({ target: "hey", data: "a\nb\nc" })
expect token_kind("<?hey &amp;<x>>") == ProcessingInstruction({ target: "hey", data: "&amp;<x" })
expect token_kind("<?xml?>") == Comment("?xml?")
expect token_kind("<?XML-stylesheet version='1.0'>") == Comment("?XML-stylesheet version='1.0'")
expect token_kind("<?1st-place>") == Comment("?1st-place")
expect token_kind("<?namespace:tag>") == Comment("?namespace:tag")
expect token_kind("<?user@domain>") == Comment("?user@domain")
expect token_kind("<? >") == Comment("? ")
expect token_kind("<? ") == Comment("? ")
expect token_kind("<?start") == Eof
expect token_kind("<?start?") == Eof
expect token_kind("<?start data") == Eof
expect token_kind("<?") == Eof
expect {
	result = HtmlTokenizer.next({ bytes: "<?pi?>tail".to_utf8(), pos: 0, diagnostics: [] }, Data, False)
	result.reader.pos == 6 and result.token.span == { start: 0, end: 6 } and result.reader.diagnostics.is_empty()
}
expect {
	result = HtmlTokenizer.next({ bytes: "<?xml?>".to_utf8(), pos: 0, diagnostics: [] }, Data, False)
	result.reader.diagnostics == [{ code: DisallowedProcessingInstructionTarget, offset: 5 }]
}

token_kind : Str -> HtmlTokenizer.Kind
token_kind = |input| HtmlTokenizer.next({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Data, False).token.kind

# Tree-construction states distinguish whitespace, nulls, and other character
# tokens. Emit homogeneous decoded runs, preserving source intervals even for
# references that become whitespace, rather than bundling unlike characters.
data_characters = |reader| {
	bytes = reader.bytes
	var $pos = reader.pos
	var $out = []
	var $diagnostics = reader.diagnostics
	var $class = Unset
	while $pos < bytes.len() and HtmlTokenizer.byte_at(bytes, $pos) != '<' {
		if $diagnostics.len() > reader.max_diagnostics {
			return exhausted(reader, $pos, $diagnostics)
		}
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		if byte != '&' and byte != '\r' {
			next_class = if (byte == 0) Null else if (HtmlTokenizer.space(byte)) Whitespace else Other
			if $class != Unset and $class != next_class {
				break
			}
			$class = next_class
			$out = $out.append(byte)
			if byte == 0 {
				$diagnostics = $diagnostics.append({ code: UnexpectedNull, offset: $pos })
			}
			$pos = $pos + 1
		} else {
			unit = data_unit(bytes, $pos)
			next_class = if (unit.bytes.len() == 1 and unit.bytes.first() == Ok(0)) Null else if (unit.bytes.all(HtmlTokenizer.space)) Whitespace else Other
			if $class != Unset and $class != next_class {
				break
			}
			$class = next_class
			$out = $out.concat(unit.bytes)
			$diagnostics = $diagnostics.concat(unit.diagnostics)
			$pos = unit.end
		}
	}
	{ token: { kind: Characters(Str.from_utf8_lossy($out)), span: { start: reader.pos, end: $pos } }, reader: { ..reader, pos: $pos, diagnostics: $diagnostics } }
}

data_unit = |bytes, pos| {
	byte = HtmlTokenizer.byte_at(bytes, pos)
	if byte == '&' {
		consume_reference(bytes, pos, False)
	}
	else if byte == '\r' {
		{ bytes: ['\n'], end: pos + if (HtmlTokenizer.byte_at(bytes, pos + 1) == '\n') 2 else 1, diagnostics: [] }
	}
	else {
		{ bytes: [byte], end: pos + 1, diagnostics: if (byte == 0) [{ code: UnexpectedNull, offset: pos }] else [] }
	}
}

expect {
	first = HtmlTokenizer.next({ bytes: "&#32;x\u(0) y".to_utf8(), pos: 0, diagnostics: [] }, Data, False)
	second = HtmlTokenizer.next(first.reader, Data, False)
	third = HtmlTokenizer.next(second.reader, Data, False)
	fourth = HtmlTokenizer.next(third.reader, Data, False)
	first.token.kind == Characters(" ") and first.token.span == { start: 0, end: 5 } and second.token.kind == Characters("x") and third.token.kind == Characters("\u(0)") and fourth.token.kind == Characters(" ")
}
expect {
	result = HtmlTokenizer.next({ bytes: "<![CDATA[a\u(0)b]]>".to_utf8(), pos: 0, diagnostics: [] }, Data, True)
	result.token.kind == Characters("a\u(0)b")
}

expect token_kind("<!--->tail") == Comment("")
expect token_kind("<!-->tail") == Comment("")
expect token_kind("<!--x-") == Comment("x")
expect token_kind("<!--x--") == Comment("x")
expect token_kind("<!--x--!") == Comment("x")
expect token_kind("<!--x--!>tail") == Comment("x")
expect token_kind("<!--x--!-y-->") == Comment("x--!-y")
expect token_kind("<!--a<!--b-->") == Comment("a<!--b")
expect token_kind("<!--a\r\nb\rc-->") == Comment("a\nb\nc")
expect token_kind("<!--a\u(0)b-->") == Comment("a�b")
expect token_kind("<!a\u(0)b>") == Comment("a�b")
expect token_kind("</a\u(0)b>") == End("a�b")
expect token_kind("</#\u(0)>") == Comment("#�")
expect token_kind("</") == Characters("</")
expect {
	result = HtmlTokenizer.next({ bytes: "<A\u(0)B c\u(0)d='a\u(0)b'>".to_utf8(), pos: 0, diagnostics: [] }, Data, False)
	match result.token.kind {
		Start(tag) => tag.name == "a�b" and tag.attributes.map(|a| (a.name, a.value)) == [("c�d", "a�b")] and result.reader.diagnostics.len() == 3
		_ => False
	}
}
expect {
	match token_kind("<a =x=y>") {
		Start(tag) => tag.attributes.map(|a| (a.name, a.value)) == [("=x", "y")]
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html PUBLIC '' ''>") {
		Doctype(d) => d.has_public and d.has_system and d.public_id == "" and d.system_id == ""
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html>") {
		Doctype(d) => !d.has_public and !d.has_system
		_ => False
	}
}
expect {
	decoded = HtmlTokenizer.normalized("&#13;".to_utf8(), True, False)
	decoded.text == "\r" and decoded.diagnostics == [{ code: InvalidCharacterReference, offset: 0 }]
}

expect {
	match token_kind("<!DOCTYPE HTML PUBLIC'public'\"system\">") {
		Doctype(d) => d.name == "html" and d.public_id == "public" and d.system_id == "system" and d.has_public and d.has_system and !d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html SYSTEM 'closed' trailing>") {
		Doctype(d) => d.system_id == "closed" and d.has_system and !d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html SYSTEM 'closed' trailing") {
		Doctype(d) => d.system_id == "closed" and d.has_system and !d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html SYSTEM 'closed'") {
		Doctype(d) => d.system_id == "closed" and d.has_system and d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html SYSTEM 'a&amp;\r\nb\u(0)'>") {
		Doctype(d) => d.system_id == "a&amp;\nb�" and d.has_system and !d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE html PUBLIC 'ab>ignored") {
		Doctype(d) => d.public_id == "ab" and d.has_public and !d.has_system and d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE root-element>") {
		Doctype(d) => d.name == "root-element" and !d.quirks
		_ => False
	}
}
expect {
	match token_kind("<!DOCTYPE h\u(0)tml>") {
		Doctype(d) => d.name == "h�tml" and !d.quirks
		_ => False
	}
}

script_characters : Str -> HtmlTokenizer.Kind
script_characters = |input| HtmlTokenizer.next({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Script, False).token.kind

expect script_characters("<!--><script></script>tail") == Characters("<!--><script>")
expect script_characters("<!--<script></script>--></script>tail") == Characters("<!--<script></script>-->")
expect script_characters("<!--<script></script></script>tail") == Characters("<!--<script></script>")
expect script_characters("<!--<script>--></script>tail") == Characters("<!--<script>-->")
expect script_characters("<!--<ScRiPtX></script>tail") == Characters("<!--<ScRiPtX>")
expect script_characters("<!--<script!></script>tail") == Characters("<!--<script!>")
expect script_characters("<!--<<script></script>--></script>tail") == Characters("<!--<<script></script>-->")
expect script_characters("<x</scriptfoo></script>tail") == Characters("<x</scriptfoo>")
expect script_characters("<!--<script/></script>tail") == Characters("<!--<script/></script>tail")
expect {
	result = HtmlTokenizer.next({ bytes: "<!--".to_utf8(), pos: 0, diagnostics: [] }, Script, False)
	result.reader.diagnostics == [{ code: EofInScriptCommentLikeText, offset: 4 }]
}
expect {
	result = HtmlTokenizer.next({ bytes: "<!-->".to_utf8(), pos: 0, diagnostics: [] }, Script, False)
	result.reader.diagnostics.is_empty()
}

expect HtmlTokenizer.input_diagnostics("x\u(B)y\u(80)z\u(FDD0)\u(10FFFF)".to_utf8()) == [{ code: ControlCharacterInInputStream, offset: 1 }, { code: ControlCharacterInInputStream, offset: 3 }, { code: NoncharacterInInputStream, offset: 6 }, { code: NoncharacterInInputStream, offset: 9 }]
expect HtmlTokenizer.input_diagnostics("\r\n\t\u(C)\u(0)\u(FEFF)".to_utf8()).is_empty()

expect HtmlTokenizer.input_diagnostics_with("\u(B)\u(B)\u(B)".to_utf8(), 0).len() == 1
expect HtmlTokenizer.input_diagnostics_with("\u(B)\u(B)\u(B)".to_utf8(), 1).len() == 2
expect token_kind("\u(FEFF)<p>") == Characters("\u(FEFF)")

expect HtmlTokenizer.next({ bytes: "foo</textarea><b>&amp;".to_utf8(), pos: 0, diagnostics: [] }, RcdataFragment, False).token.kind == Characters("foo</textarea><b>&")
expect HtmlTokenizer.next({ bytes: "foo</style><b>&amp;".to_utf8(), pos: 0, diagnostics: [] }, RawtextFragment, False).token.kind == Characters("foo</style><b>&amp;")
expect HtmlTokenizer.next({ bytes: "<!-- inside </script> -->".to_utf8(), pos: 0, diagnostics: [] }, ScriptFragment, False).token.kind == Characters("<!-- inside </script> -->")
expect HtmlTokenizer.next({ bytes: "</script><b>&amp;".to_utf8(), pos: 0, diagnostics: [] }, ScriptFragment, False).token.kind == Characters("</script><b>&amp;")

ScanReader : { bytes : List(U8), pos : U64, diagnostics : List(HtmlTokenizer.Diagnostic), max_diagnostics : U64, max_attributes : U64, attribute_limit : Bool }

remaining_diagnostics : U64, U64 -> U64
remaining_diagnostics = |limit, count| if (count < limit) limit - count else 0

diagnostic_budget : ScanReader -> U64
diagnostic_budget = |reader| remaining_diagnostics(reader.max_diagnostics, reader.diagnostics.len())

exhausted : ScanReader, U64, List(HtmlTokenizer.Diagnostic) -> { token : HtmlTokenizer.Token, reader : ScanReader }
exhausted = |reader, end, diagnostics| { token: { kind: Eof, span: { start: reader.pos, end } }, reader: { ..reader, pos: end, diagnostics } }

# Newline preprocessing without diagnosing/replacing literal nulls. These
# characters belong to PI data or CDATA, whose tree-construction context
# determines their treatment rather than a character-reference state.
preprocessed_text : List(U8) -> Str
preprocessed_text = |bytes| {
	var $out = []
	var $pos = 0
	while $pos < bytes.len() {
		byte = HtmlTokenizer.byte_at(bytes, $pos)
		if byte == '\r' {
			$out = $out.append('\n')
			$pos = $pos + if (HtmlTokenizer.byte_at(bytes, $pos + 1) == '\n') 2 else 1
		} else {
			$out = $out.append(byte)
			$pos = $pos + 1
		}
	}
	Str.from_utf8_lossy($out)
}

expect {
	input = "<a a=1 b=2 c=3>"
	result = HtmlTokenizer.next_with({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_attributes: 0 })
	result.limited == Attributes and result.reader.pos < input.count_utf8_bytes() and (
		match result.token.kind {
			Start(tag) => tag.attributes.len() == 1
			_ => False
		}
	)
}
expect {
	result = HtmlTokenizer.next_with({ bytes: "</a a=1 b=2>".to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_attributes: 0 })
	result.limited == Attributes
}
expect {
	input = "<!--${Str.repeat("\u(0)", 4096)}-->"
	result = HtmlTokenizer.next_with({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_diagnostics: 0 })
	result.limited == Diagnostics and result.reader.diagnostics.len() == 1 and result.reader.pos < input.count_utf8_bytes() and result.token.kind == Eof
}
expect {
	input = "<a x='${Str.repeat("&#0;", 4096)}'>"
	result = HtmlTokenizer.next_with({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_diagnostics: 0 })
	result.limited == Diagnostics and result.reader.diagnostics.len() == 1
}
expect {
	input = "<a ${Str.repeat("\"", 4096)}>"
	result = HtmlTokenizer.next_with({ bytes: input.to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_diagnostics: 0 })
	result.limited == Diagnostics and result.reader.diagnostics.len() == 1 and result.reader.pos < input.count_utf8_bytes()
}
expect {
	result = HtmlTokenizer.next_with({ bytes: "<a x=1 y=2>".to_utf8(), pos: 0, diagnostics: [] }, Data, False, { max_attributes: 2, max_diagnostics: 0 })
	result.limited == NoLimit and result.reader.diagnostics.is_empty()
}

expect {
	r = HtmlTokenizer.next({ bytes: "a &#32;b\u(0)c<em>".to_utf8(), pos: 0, diagnostics: [] }, DataMixed, False)
	r.token.kind == Characters("a  b\u(0)c") and r.token.span == { start: 0, end: 10 } and r.reader.diagnostics == [{ code: UnexpectedNull, offset: 8 }]
}
