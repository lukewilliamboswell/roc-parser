import parser.Xml

## Shared generator for the XML fuzz targets.
##
## Fuzzer bytes choose an XML document *and* how to write it: declaration
## variants, comments, processing instructions, CDATA, entity and character
## references, quote styles, line endings, and whitespace. The tree XML 1.0
## (Fifth Edition) says a processor must report is built alongside the text,
## never by calling the parser:
##
## - 2.11: literal CR LF and lone CR become LF (also inside CDATA);
## - 3.3.3: in attribute values each literal whitespace character becomes a
##   space, while character references keep the character they name;
## - 4.6: the five predefined entities; 4.1: decimal and hexadecimal
##   character references;
## - comments and processing instructions are not part of the tree, and
##   adjacent character data (text, CDATA, references) is one Text node.
##
## The document is kept as pieces with zero-width slots marking where the
## malformed target can insert a well-formedness violation.
XmlGen := {}.{

	## Where a mutation may be inserted.
	Slot : [TextSlot, AttrSlot, TagSlot, CommentSlot, EpilogSlot, EndSlot]

	Piece : [Lit(Str), Mark(Slot), EndTag({ name : Str, space : Str })]

	Doc : { pieces : List(Piece), expected : Xml }

	Cur : { bytes : List(U8), pos : U64 }

	pick : Cur, U64 -> { n : U64, cur : Cur }
	pick = |cur, count| {
		byte = cur.bytes.get(cur.pos) ?? 0
		{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
	}

	## Render pieces to document text; slots are empty.
	render : List(Piece) -> Str
	render = |pieces| {
		pieces.fold(
			"",
			|text, piece| {
				match piece {
					Lit(lit) => Str.concat(text, lit)
					Mark(_) => text
					EndTag({ name, space }) => Str.concat(text, "</${name}${space}>")
				}
			},
		)
	}

	## One-based line and byte column of a byte offset, treating CR LF, CR,
	## and LF as line ends (independent of the parser's own locator).
	line_column : Str, U64 -> { line : U64, column : U64 }
	line_column = |text, offset| {
		bytes = text.to_utf8()
		var $line = 1
		var $start = 0
		var $index = 0
		while $index < offset {
			byte = bytes.get($index) ?? 0
			crlf = byte == '\r' and bytes.get($index + 1) == Ok('\n') and $index + 1 < offset
			if byte == '\n' or byte == '\r' {
				$index = if crlf $index + 2 else $index + 1
				$line = $line + 1
				$start = $index
			} else {
				$index = $index + 1
			}
		}
		{ line: $line, column: offset - $start + 1 }
	}

	generate : List(U8) -> Doc
	generate = |bytes| {
		start = { bytes, pos: 0 }
		declaration = gen_declaration(start)
		prolog = gen_misc(declaration.cur)
		root = gen_element(prolog.cur, 0)
		epilog = gen_misc(root.cur)
		pieces =
			declaration.pieces
				.concat(prolog.pieces)
				.concat(root.pieces)
				.append(Mark(EpilogSlot))
				.concat(epilog.pieces)
				.append(Mark(EndSlot))
		{ pieces, expected: { declaration: declaration.value, root: root.node } }
	}

	## Show a document the way review_xml.py's corpus mode reads it.
	show : Str -> Str
	show = |text| {
		hex = text.to_utf8().fold("", |acc, byte| Str.concat(acc, hex_byte(byte)))
		"${Str.inspect(text)}\nXML-HEX: ${hex}"
	}
}



pick = XmlGen.pick

choose : Cur, List(Str) -> { s : Str, cur : Cur }
choose = |cur, options| {
	chosen = pick(cur, options.len())
	{ s: options.get(chosen.n) ?? "", cur: chosen.cur }
}

## Characters with their code points, so references can name them.
Char : { text : Str, codes : List(U32) }

text_alphabet : List(Char)
text_alphabet = [
	{ text: "a", codes: [0x61] },
	{ text: "Z", codes: [0x5A] },
	{ text: "1", codes: [0x31] },
	{ text: " ", codes: [0x20] },
	{ text: "é", codes: [0xE9] },
	{ text: "😀", codes: [0x1F600] },
	{ text: "<", codes: [0x3C] },
	{ text: "&", codes: [0x26] },
	{ text: ">", codes: [0x3E] },
	{ text: "\"", codes: [0x22] },
	{ text: "'", codes: [0x27] },
	{ text: "\t", codes: [0x9] },
	{ text: "\n", codes: [0xA] },
	{ text: "\r\n", codes: [0xD, 0xA] },
	{ text: "\r", codes: [0xD] },
	{ text: "]", codes: [0x5D] },
	{ text: "-", codes: [0x2D] },
	{ text: "?", codes: [0x3F] },
	{ text: "\u(85)", codes: [0x85] },
	{ text: "\u(2028)", codes: [0x2028] },
	{ text: "\u(7F)", codes: [0x7F] },
	{ text: "\u(FFFD)", codes: [0xFFFD] },
	{ text: "\u(10FFFF)", codes: [0x10FFFF] },
]

element_names : List(Str)
element_names = ["a", "b", "x1", "h1", "ns:tag", "_u", "données", "Ω", "x.y-z", "A", "\u(10000)x", "a·b", "e\u(301)"]

attribute_names : List(Str)
attribute_names = ["id", "class", "x:y", "b", "é", "data-1", "_", "xml:lang", "xmlns"]

gen_declaration : Cur -> { pieces : List(Piece), value : Try(Xml.Declaration, [Missing]), cur : Cur }
gen_declaration = |start| {
	kind = pick(start, 4)
	bom = pick(kind.cur, 4)
	bom_text = if bom.n == 0 "\u(FEFF)" else ""
	if kind.n == 0 {
		{ pieces: [Lit(bom_text)], value: Err(Missing), cur: bom.cur }
	} else {
		quote = choose(bom.cur, ["\"", "'"])
		eq = choose(quote.cur, ["=", " = ", "\t=\n"])
		version = pick(eq.cur, 4)
		minor = [0, 1, 9, 10].get(version.n) ?? 0
		encoding = choose(version.cur, ["", "UTF-8", "utf-8", "Utf-8", "ISO-8859-1", "x_y.z-1"])
		standalone = choose(encoding.cur, ["", "yes", "no"])
		trailing = choose(standalone.cur, ["", " ", "\n"])
		q = quote.s
		encoding_text = if encoding.s.is_empty() "" else " encoding${eq.s}${q}${encoding.s}${q}"
		standalone_text = if standalone.s.is_empty() "" else " standalone${eq.s}${q}${standalone.s}${q}"
		text = "${bom_text}<?xml version${eq.s}${q}1.${minor.to_str()}${q}${encoding_text}${standalone_text}${trailing.s}?>"
		encoding_value =
			if encoding.s.is_empty() {
				Err(Missing)
			} else if ascii_lower(encoding.s) == "utf-8" {
				Ok(Utf8Encoding)
			} else {
				Ok(OtherEncoding(encoding.s))
			}
		{
			pieces: [Lit(text)],
			value: Ok({ version: { major: 1, minor }, encoding: encoding_value }),
			cur: trailing.cur,
		}
	}
}

ascii_lower : Str -> Str
ascii_lower = |text| Str.from_utf8(text.to_utf8().map(|b| if b >= 'A' and b <= 'Z' b + 32 else b)) ?? text

## Whitespace, comments, and processing instructions outside the root.
gen_misc : Cur -> { pieces : List(Piece), cur : Cur }
gen_misc = |start| {
	count = pick(start, 4)
	var $cur = count.cur
	var $pieces = []
	var $index = 0
	while $index < count.n {
		kind = pick($cur, 3)
		item =
			match kind.n {
				0 => {
					space = choose(kind.cur, ["\n", " ", "\r\n", "\t", "\r"])
					{ pieces: [Lit(space.s)], cur: space.cur }
				}
				1 => gen_comment(kind.cur)
				_ => gen_pi(kind.cur)
			}
		$pieces = $pieces.concat(item.pieces)
		$cur = item.cur
		$index = $index + 1
	}
	{ pieces: $pieces, cur: $cur }
}

## Comment text never contains "--" or ends with "-" (XML 1.0 [15]).
gen_comment : Cur -> { pieces : List(Piece), cur : Cur }
gen_comment = |start| {
	body = gen_raw(start, True)
	{ pieces: [Lit("<!--"), Mark(CommentSlot), Lit("${body.s}-->")], cur: body.cur }
}

## Processing instruction targets other than [Xx][Mm][Ll] (XML 1.0 [17]);
## content never contains "?>".
gen_pi : Cur -> { pieces : List(Piece), cur : Cur }
gen_pi = |start| {
	target = choose(start, ["pi", "x-y", "xml-stylesheet", "a1", "xmlfoo", "é"])
	form = pick(target.cur, 3)
	match form.n {
		0 => { pieces: [Lit("<?${target.s}?>")], cur: form.cur }
		_ => {
			space = choose(form.cur, [" ", "\n", "\t  "])
			body = gen_raw(space.cur, False)
			{ pieces: [Lit("<?${target.s}${space.s}${body.s}?>")], cur: body.cur }
		}
	}
}

## Literal characters for comments and PIs: '-' only before a letter, so no
## "--" and no trailing '-'; '?' only before a letter, so no "?>".
gen_raw : Cur, Bool -> { s : Str, cur : Cur }
gen_raw = |start, _comment| {
	count = pick(start, 6)
	var $cur = count.cur
	var $text = ""
	var $index = 0
	while $index < count.n {
		chosen = pick($cur, text_alphabet.len())
		char = text_alphabet.get(chosen.n) ?? { text: "a", codes: [] }
		$text =
			if char.text == "-" or char.text == "?" {
				Str.concat($text, "${char.text}a")
			} else {
				Str.concat($text, char.text)
			}
		$cur = chosen.cur
		$index = $index + 1
	}
	{ s: $text, cur: $cur }
}

hex_digits : U32, Bool -> Str
hex_digits = |code, upper| {
	digits = if upper "0123456789ABCDEF".to_utf8() else "0123456789abcdef".to_utf8()
	var $out = []
	var $rest = code
	while $rest > 0 or $out.is_empty() {
		$out = $out.prepend(digits.get(U32.to_u64($rest % 16)) ?? '0')
		$rest = $rest // 16
	}
	Str.from_utf8($out) ?? "0"
}

## A character reference for each code point, decimal or hexadecimal.
char_refs : List(U32), U64 -> Str
char_refs = |codes, style| {
	codes.fold(
		"",
		|text, code| {
			ref =
				match style % 4 {
					0 => "&#${code.to_str()};"
					1 => "&#000${code.to_str()};"
					2 => "&#x${hex_digits(code, False)};"
					_ => "&#x${hex_digits(code, True)};"
				}
			Str.concat(text, ref)
		},
	)
}

entity_for : Str -> Try(Str, [NoEntity])
entity_for = |text| {
	match text {
		"<" => Ok("&lt;")
		">" => Ok("&gt;")
		"&" => Ok("&amp;")
		"\"" => Ok("&quot;")
		"'" => Ok("&apos;")
		_ => Err(NoEntity)
	}
}

## Raw-text state shared by adjacent character data, so that the generator
## never writes CR followed by LF unless it means one line end, nor "]]>".
Raw : { cr : Bool, brackets : U64 }

fresh_raw : Raw
fresh_raw = { cr: False, brackets: 0 }

after_literal : Raw, Str -> Raw
after_literal = |raw, text| {
	bytes = text.to_utf8()
	last = bytes.last() ?? 0
	brackets = if text == "]" raw.brackets + 1 else 0
	{ cr: last == '\r', brackets }
}

## Character data: each character is literal (if allowed), an entity
## reference, or a character reference. Returns the text and its value.
gen_char_data : Cur, Raw -> { text : Str, value : Str, raw : Raw, cur : Cur }
gen_char_data = |start, raw_start| {
	count = pick(start, 7)
	var $cur = count.cur
	var $text = ""
	var $value = ""
	var $raw = raw_start
	var $index = 0
	while $index < count.n {
		chosen = pick($cur, text_alphabet.len())
		style = pick(chosen.cur, 6)
		$cur = style.cur
		char = text_alphabet.get(chosen.n) ?? { text: "a", codes: [0x61] }
		forbidden =
			char.text == "<"
			or char.text == "&"
			or (char.text == ">" and $raw.brackets >= 2)
			or ($raw.cr and Str.starts_with(char.text, "\n"))
		if style.n <= 2 and !forbidden {
			$text = Str.concat($text, char.text)
			$value = Str.concat($value, normalize_line_ends(char.text))
			$raw = after_literal($raw, char.text)
		} else {
			reference =
				match entity_for(char.text) {
					Ok(entity) if style.n == 3 => entity
					_ => char_refs(char.codes, style.n)
				}
			$text = Str.concat($text, reference)
			$value = Str.concat($value, char.text)
			$raw = fresh_raw
		}
		$index = $index + 1
	}
	{ text: $text, value: $value, raw: $raw, cur: $cur }
}

normalize_line_ends : Str -> Str
normalize_line_ends = |text| text |> Str.replace_each("\r\n", "\n") |> Str.replace_each("\r", "\n")

## CDATA content is literal; "]]>" is avoided and line ends are normalised.
gen_cdata : Cur -> { text : Str, value : Str, cur : Cur }
gen_cdata = |start| {
	count = pick(start, 6)
	var $cur = count.cur
	var $text = ""
	var $raw = fresh_raw
	var $index = 0
	while $index < count.n {
		chosen = pick($cur, text_alphabet.len())
		$cur = chosen.cur
		char = text_alphabet.get(chosen.n) ?? { text: "a", codes: [] }
		ok = !(char.text == ">" and $raw.brackets >= 2) and !($raw.cr and Str.starts_with(char.text, "\n"))
		if ok {
			$text = Str.concat($text, char.text)
			$raw = after_literal($raw, char.text)
		}
		$index = $index + 1
	}
	# A trailing CR stays a lone CR: "]]>" follows it.
	{ text: "<![CDATA[${$text}]]>", value: normalize_line_ends($text), cur: $cur }
}

## Attribute values per XML 1.0 3.3.3 for CDATA attributes.
gen_attribute_value : Cur, Str -> { text : Str, value : Str, cur : Cur }
gen_attribute_value = |start, quote| {
	count = pick(start, 6)
	var $cur = count.cur
	var $text = ""
	var $value = ""
	var $cr = False
	var $index = 0
	while $index < count.n {
		chosen = pick($cur, text_alphabet.len())
		style = pick(chosen.cur, 6)
		$cur = style.cur
		char = text_alphabet.get(chosen.n) ?? { text: "a", codes: [0x61] }
		forbidden = char.text == "<" or char.text == "&" or char.text == quote or ($cr and Str.starts_with(char.text, "\n"))
		$cr = False
		if style.n <= 2 and !forbidden {
			$text = Str.concat($text, char.text)
			$cr = Str.ends_with(char.text, "\r")
			normalized =
				match char.text {
					"\r\n" | "\r" | "\n" | "\t" => " "
					other => other
				}
			$value = Str.concat($value, normalized)
		} else {
			reference =
				match entity_for(char.text) {
					Ok(entity) if style.n == 3 => entity
					_ => char_refs(char.codes, style.n)
				}
			$text = Str.concat($text, reference)
			$value = Str.concat($value, char.text)
		}
		$index = $index + 1
	}
	{ text: $text, value: $value, cur: $cur }
}

push_text : List(Xml.Node), Str -> List(Xml.Node)
push_text = |children, text| {
	if text.is_empty() {
		children
	} else {
		match children.last() {
			Ok(Text(previous)) => children.drop_last(1).append(Text(Str.concat(previous, text)))
			_ => children.append(Text(text))
		}
	}
}

max_depth : U64
max_depth = 4

gen_element : Cur, U64 -> { pieces : List(Piece), node : Xml.Node, cur : Cur }
gen_element = |start, depth| {
	name = choose(start, element_names)
	attribute_count = pick(name.cur, 4)
	var $cur = attribute_count.cur
	var $pieces = [Lit("<${name.s}")]
	var $attributes = []
	var $index = 0
	while $index < attribute_count.n {
		attribute_name = choose($cur, attribute_names)
		space = choose(attribute_name.cur, [" ", "\n", "\t", "  ", "\r\n"])
		eq = choose(space.cur, ["=", " = ", "\t=\r\n"])
		quote = choose(eq.cur, ["\"", "'"])
		value = gen_attribute_value(quote.cur, quote.s)
		$cur = value.cur
		if !$attributes.any(|attribute| attribute.name == attribute_name.s) {
			$pieces = $pieces.concat([Lit("${space.s}${attribute_name.s}${eq.s}${quote.s}"), Mark(AttrSlot), Lit("${value.text}${quote.s}")])
			$attributes = $attributes.append({ name: attribute_name.s, value: value.value })
		}
		$index = $index + 1
	}
	trailing = choose($cur, ["", "", " ", "\n"])
	$pieces = $pieces.append(Mark(TagSlot)).append(Lit(trailing.s))
	child_count = pick(trailing.cur, if depth >= max_depth 4 else 7)
	$cur = child_count.cur
	if child_count.n == 0 {
		form = pick($cur, 2)
		if form.n == 0 {
			return { pieces: $pieces.append(Lit("/>")), node: Element({ name: name.s, attributes: $attributes, children: [] }), cur: form.cur }
		}
		$cur = form.cur
	}
	$pieces = $pieces.append(Lit(">"))
	var $children = []
	var $raw = fresh_raw
	$index = 0
	while $index < child_count.n {
		kind = pick($cur, if depth >= max_depth 4 else 6)
		$cur = kind.cur
		match kind.n {
			0 | 1 => {
				data = gen_char_data($cur, $raw)
				$pieces = $pieces.append(Mark(TextSlot)).append(Lit(data.text))
				$children = push_text($children, data.value)
				$raw = data.raw
				$cur = data.cur
			}
			2 => {
				cdata = gen_cdata($cur)
				$pieces = $pieces.append(Lit(cdata.text))
				$children = push_text($children, cdata.value)
				$raw = fresh_raw
				$cur = cdata.cur
			}
			3 => {
				misc = pick($cur, 2)
				item = if misc.n == 0 gen_comment(misc.cur) else gen_pi(misc.cur)
				$pieces = $pieces.concat(item.pieces)
				$raw = fresh_raw
				$cur = item.cur
			}
			_ => {
				child = gen_element($cur, depth + 1)
				$pieces = $pieces.concat(child.pieces)
				$children = $children.append(child.node)
				$raw = fresh_raw
				$cur = child.cur
			}
		}
		$index = $index + 1
	}
	end_space = choose($cur, ["", "", " ", "\n"])
	$pieces = $pieces.append(Mark(TextSlot)).append(EndTag({ name: name.s, space: end_space.s }))
	{ pieces: $pieces, node: Element({ name: name.s, attributes: $attributes, children: $children }), cur: end_space.cur }
}

hex_byte : U8 -> Str
hex_byte = |byte| {
	digits = "0123456789abcdef".to_utf8()
	Str.from_utf8([digits.get(U8.to_u64(byte // 16)) ?? '0', digits.get(U8.to_u64(byte % 16)) ?? '0']) ?? "00"
}
