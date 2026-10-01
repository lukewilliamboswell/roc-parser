import Parser
import Utf8

## XML document tree and parser based on the XML 1.0 (Fifth Edition)
## specification.
##
## The parser checks well-formedness for documents without a document type
## declaration. It supports an optional XML declaration, nested elements,
## attributes, character data, CDATA sections, the five predefined entity
## references (`&lt;` `&gt;` `&amp;` `&apos;` `&quot;`), and decimal and
## hexadecimal character references. Comments and processing instructions are
## checked and then dropped from the tree.
##
## The tree is normalised the way an XML processor reports it: line endings
## become `\n` (XML 1.0 section 2.11), attribute values are normalised as
## CDATA attributes (section 3.3.3), references are replaced by their text,
## and adjacent character data (including CDATA sections and text on both
## sides of a comment) is merged into one `Text` node.
##
## Document type declarations (`<!DOCTYPE ...>`) are rejected, so entities
## other than the predefined ones are never declared and referring to one is
## a well-formedness error. Namespaces are not processed: a name such as
## `svg:path` is kept as written. Input is already decoded text, so a declared
## encoding is reported but not used to decode.
##
## Originally written by [Johannes Maas](https://github.com/j-maas).
Xml := {
	xml_declaration : [Given(Xml.Declaration), Missing],
	root : Xml.Node,
}.{

	## Compare two XML documents structurally (declaration and tree).
	is_eq : _

	## An XML attribute name and decoded value.
	##
	## The name is kept as written, including any namespace prefix. The value
	## has references replaced and whitespace normalised. Attributes keep their
	## source order; duplicate names are a well-formedness error.
	Attribute : { name : Str, value : Str }

	## Text encoding declared by an XML declaration.
	## Encoding names are compared case-insensitively, so `UTF-8` is `Utf8Encoding`.
	TextEncoding : [
		Utf8Encoding,
		OtherEncoding(Str),
	]

	## Version and optional encoding from an XML declaration.
	Declaration : {
		version : Version,
		encoding : [Given(TextEncoding), Missing],
	}

	## An XML 1.x version, storing the number after `1.`.
	Version :: {
		after_dot : U8,
	}.{

		## Compare two XML versions.
		is_eq : _

		## Construct an XML 1.x version from the number after `1.`.
		new : U8 -> Version
		new = |after_dot| {
			{ after_dot }
		}
	}

	## An XML element or text node.
	##
	## `Element(name, attributes, children)` holds the children in document
	## order. `Text(str)` is decoded character data; adjacent text, CDATA and
	## references are merged into one `Text`, and whitespace-only text between
	## elements is kept.
	Node := [
		Element(Str, List({ name : Str, value : Str }), List(Node)),
		Text(Str),
	].{

		## Compare two XML nodes structurally.
		is_eq : _
	}

	## Location and explanation of malformed XML. Lines and columns are
	## one-based; columns count UTF-8 bytes, and `\r\n`, `\r`, and `\n` each
	## end a line.
	Error : { line : U64, column : U64, message : Str }

	## Parse one complete XML document.
	##
	## Leading and trailing whitespace, comments and processing instructions
	## around the root element are allowed; anything else after it is an error.
	## A leading byte order mark is accepted.
	##
	## ```roc
	## expect Xml.parse_str("<a>x &amp; y</a>").map_ok(|xml| xml.root) == Ok(Element("a", [], [Text("x & y")]))
	##
	## expect {
	##     match Xml.parse_str("<a>&nbsp;</a>") {
	##         Err(XmlError({ line, column, message: _ })) => line == 1 and column == 4
	##         Ok(_) => Bool.False
	##     }
	## }
	## ```
	parse_str : Str -> Try(Xml, [XmlError(Error)])
	parse_str = |input| {
		bytes = input.to_utf8()
		match parse_document(bytes) {
			Ok({ val, pos }) =>
				if pos == bytes.len() {
					Ok(val)
				} else {
					Err(XmlError(locate(bytes, pos, "unexpected content after the root element")))
				}

			Err(XmlFail(failure)) => Err(XmlError(locate(bytes, failure.offset, failure.message)))
		}
	}

	## Parse one XML document, including an optional declaration and any
	## trailing comments, processing instructions, and whitespace. Input left
	## after that is returned to the caller. Failures read `line:column: message`.
	##
	## Use this to embed an XML document in a larger parser; otherwise prefer
	## `parse_str`, which reports a structured `Error`.
	##
	## ```roc
	## expect Utf8.parse_str(Xml.xml_parser, "<a/> <b/>") == Err(ParseError({ message: "unexpected input", offset: 5 }))
	## ```
	xml_parser : Parser(Utf8.Bytes, Xml)
	xml_parser =
		Parser.build_primitive_parser(
			|input| {
				match parse_document(input) {
					Ok({ val, pos }) => Ok({ value: val, rest: input.drop_first(pos) })
					Err(XmlFail(failure)) => {
						error = locate(input, failure.offset, failure.message)
						Err(ParseError({ message: "${error.line.to_str()}:${error.column.to_str()}: ${error.message}", offset: failure.offset }))
					}
				}
			},
		)
}

Failure : { offset : U64, message : Str }

Parsed(a) : Try({ val : a, pos : U64 }, [XmlFail(Failure)])

Frame : {
	name : Str,
	attributes : List(Xml.Attribute),
	children : List(Xml.Node),
	text : List(U8),
}

StartTag : {
	name : Str,
	attributes : List(Xml.Attribute),
	empty : Bool,
	pos : U64,
}

fail : U64, Str -> Try(_, [XmlFail(Failure)])
fail = |offset, message| Err(XmlFail({ offset, message }))

locate : List(U8), U64, Str -> Xml.Error
locate = |bytes, offset, message| {
	var $line = 1
	var $line_start = 0
	var $index = 0
	while $index < offset and $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if byte == '\n' {
			$line = $line + 1
			$line_start = $index + 1
		} else if byte == '\r' {
			if $index + 1 < offset and bytes.get($index + 1) == Ok('\n') {
				$index = $index + 1
			}
			$line = $line + 1
			$line_start = $index + 1
		}
		$index = $index + 1
	}
	{ line: $line, column: offset - $line_start + 1, message }
}

# See https://www.w3.org/TR/xml/#NT-document
parse_document : List(U8) -> Parsed(Xml)
parse_document = |bytes| {
	var $pos = if starts_with_at(bytes, 0, [0xEF, 0xBB, 0xBF]) 3 else 0
	var $declaration = Missing
	if starts_with_at(bytes, $pos, "<?xml".to_utf8()) and is_space_at(bytes, $pos + 5) {
		parsed = parse_xml_declaration(bytes, $pos)?
		$declaration = Given(parsed.val)
		$pos = parsed.pos
	}
	$pos = skip_misc(bytes, $pos)?
	if starts_with_at(bytes, $pos, "<!DOCTYPE".to_utf8()) {
		return fail($pos, "document type declarations are not supported")
	}
	if bytes.get($pos) != Ok('<') {
		return if $pos >= bytes.len() {
			fail($pos, "expected a root element")
		} else {
			fail($pos, "expected a root element, but found text")
		}
	}
	root = parse_element(bytes, $pos)?
	end = skip_misc(bytes, root.pos)?
	Ok({ val: { xml_declaration: $declaration, root: root.val }, pos: end })
}

# See https://www.w3.org/TR/xml/#NT-XMLDecl
parse_xml_declaration : List(U8), U64 -> Parsed(Xml.Declaration)
parse_xml_declaration = |bytes, start| {
	var $pos = skip_space(bytes, start + 5)
	if !starts_with_at(bytes, $pos, "version".to_utf8()) {
		return fail($pos, "expected version information in the XML declaration")
	}
	$pos = skip_eq(bytes, $pos + 7)?
	version_quote = bytes.get($pos) ?? 0
	if version_quote != '"' and version_quote != '\'' {
		return fail($pos, "expected a quoted XML version")
	}
	if !starts_with_at(bytes, $pos + 1, "1.".to_utf8()) or !is_digit_at(bytes, $pos + 3) {
		return fail($pos + 1, "expected an XML version of the form 1.x")
	}
	var $after_dot = 0
	$pos = $pos + 3
	while is_digit_at(bytes, $pos) {
		next_value = $after_dot * 10 + U8.to_u64((bytes.get($pos) ?? '0') - '0')
		$after_dot = if next_value > 1000 1000 else next_value
		$pos = $pos + 1
	}
	if bytes.get($pos) != Ok(version_quote) {
		return fail($pos, "expected the XML version to end with a matching quote")
	}
	version = match U64.to_u8_try($after_dot) {
		Ok(digit) => Xml.Version.new(digit)
		Err(_) => return fail(start, "XML versions above 1.255 are not supported")
	}
	$pos = $pos + 1

	var $encoding = Missing
	after_version = skip_space(bytes, $pos)
	$pos = after_version
	if starts_with_at(bytes, $pos, "encoding".to_utf8()) {
		if after_version == skip_space_start(bytes, after_version) {
			return fail($pos, "expected whitespace before the encoding declaration")
		}
		$pos = skip_eq(bytes, $pos + 8)?
		quote = bytes.get($pos) ?? 0
		if quote != '"' and quote != '\'' {
			return fail($pos, "expected a quoted encoding name")
		}
		name_start = $pos + 1
		if !(is_alphabetical(bytes.get(name_start) ?? 0)) {
			return fail(name_start, "encoding names must start with a letter")
		}
		$pos = name_start + 1
		while is_encoding_char(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if bytes.get($pos) != Ok(quote) {
			return fail($pos, "expected the encoding name to end with a matching quote")
		}
		name = bytes.sublist({ start: name_start, len: $pos - name_start })
		$encoding =
			if ascii_lowercase(name) == "utf-8".to_utf8() {
				Given(Utf8Encoding)
			} else {
				Given(OtherEncoding(Str.from_utf8(name) ?? ""))
			}
		$pos = skip_space(bytes, $pos + 1)
	}
	if starts_with_at(bytes, $pos, "standalone".to_utf8()) {
		if $pos == skip_space_start(bytes, $pos) {
			return fail($pos, "expected whitespace before the standalone declaration")
		}
		$pos = skip_eq(bytes, $pos + 10)?
		quote = bytes.get($pos) ?? 0
		if quote != '"' and quote != '\'' {
			return fail($pos, "expected a quoted standalone value")
		}
		value_start = $pos + 1
		$pos =
			if starts_with_at(bytes, value_start, "yes".to_utf8()) {
				value_start + 3
			} else if starts_with_at(bytes, value_start, "no".to_utf8()) {
				value_start + 2
			} else {
				return fail(value_start, "standalone must be \"yes\" or \"no\"")
			}
		if bytes.get($pos) != Ok(quote) {
			return fail($pos, "expected the standalone value to end with a matching quote")
		}
		$pos = skip_space(bytes, $pos + 1)
	}
	if !starts_with_at(bytes, $pos, "?>".to_utf8()) {
		return fail($pos, "expected '?>' to close the XML declaration")
	}
	Ok({ val: { version, encoding: $encoding }, pos: $pos + 2 })
}

## The first position of the whitespace run that ends at `end`.
skip_space_start : List(U8), U64 -> U64
skip_space_start = |bytes, end| {
	var $start = end
	while $start > 0 and is_space(bytes.get($start - 1) ?? 0) {
		$start = $start - 1
	}
	$start
}

# See https://www.w3.org/TR/xml/#NT-Eq
skip_eq : List(U8), U64 -> Try(U64, [XmlFail(Failure)])
skip_eq = |bytes, start| {
	pos = skip_space(bytes, start)
	if bytes.get(pos) == Ok('=') {
		Ok(skip_space(bytes, pos + 1))
	} else {
		fail(pos, "expected '='")
	}
}

# See https://www.w3.org/TR/xml/#NT-Misc
skip_misc : List(U8), U64 -> Try(U64, [XmlFail(Failure)])
skip_misc = |bytes, start| {
	var $pos = start
	var $done = Bool.False
	while !$done {
		if is_space_at(bytes, $pos) {
			$pos = $pos + 1
		} else if starts_with_at(bytes, $pos, "<!--".to_utf8()) {
			$pos = skip_comment(bytes, $pos)?
		} else if starts_with_at(bytes, $pos, "<?".to_utf8()) {
			$pos = skip_processing_instruction(bytes, $pos)?
		} else {
			$done = Bool.True
		}
	}
	Ok($pos)
}

# See https://www.w3.org/TR/xml/#NT-Comment
skip_comment : List(U8), U64 -> Try(U64, [XmlFail(Failure)])
skip_comment = |bytes, start| {
	var $pos = start + 4
	while $pos < bytes.len() {
		if starts_with_at(bytes, $pos, "--".to_utf8()) {
			return if bytes.get($pos + 2) == Ok('>') {
				Ok($pos + 3)
			} else {
				fail($pos, "'--' is not allowed inside a comment")
			}
		}
		$pos = check_char(bytes, $pos)?
	}
	fail(start, "unterminated comment")
}

# See https://www.w3.org/TR/xml/#NT-PI
skip_processing_instruction : List(U8), U64 -> Try(U64, [XmlFail(Failure)])
skip_processing_instruction = |bytes, start| {
	target =
		match parse_name(bytes, start + 2) {
			Ok(name) => name
			Err(_) => return fail(start + 2, "expected a processing instruction target")
		}
	if ascii_lowercase(target.val.to_utf8()) == "xml".to_utf8() {
		return if target.val == "xml" {
			fail(start, "the XML declaration is only allowed at the very start of the document")
		} else {
			fail(start + 2, "processing instruction targets matching 'xml' are reserved")
		}
	}
	if starts_with_at(bytes, target.pos, "?>".to_utf8()) {
		return Ok(target.pos + 2)
	}
	if !is_space_at(bytes, target.pos) {
		return fail(target.pos, "expected whitespace after the processing instruction target")
	}
	var $pos = target.pos
	while $pos < bytes.len() {
		if starts_with_at(bytes, $pos, "?>".to_utf8()) {
			return Ok($pos + 2)
		}
		$pos = check_char(bytes, $pos)?
	}
	fail(start, "unterminated processing instruction")
}

# See https://www.w3.org/TR/xml/#NT-CDSect
parse_cdata : List(U8), U64, List(U8) -> Parsed(List(U8))
parse_cdata = |bytes, start, text| {
	var $pos = start + 9
	var $text = text
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == ']' and starts_with_at(bytes, $pos, "]]>".to_utf8()) {
			return Ok({ val: $text, pos: $pos + 3 })
		} else if byte == '\r' {
			$text = $text.append('\n')
			$pos = if bytes.get($pos + 1) == Ok('\n') $pos + 2 else $pos + 1
		} else {
			next = check_char(bytes, $pos)?
			$text = $text.concat(bytes.sublist({ start: $pos, len: next - $pos }))
			$pos = next
		}
	}
	fail(start, "unterminated CDATA section")
}

# See https://www.w3.org/TR/xml/#NT-element
#
# Elements are parsed with an explicit stack so deep nesting cannot exhaust
# the call stack.
parse_element : List(U8), U64 -> Parsed(Xml.Node)
parse_element = |bytes, start| {
	first = parse_start_tag(bytes, start)?
	if first.empty {
		return Ok({ val: Element(first.name, first.attributes, []), pos: first.pos })
	}
	var $stack = []
	var $current = { name: first.name, attributes: first.attributes, children: [], text: [] }
	var $pos = first.pos
	while Bool.True {
		byte =
			match bytes.get($pos) {
				Ok(b) => b
				Err(_) => return fail($pos, "expected </${$current.name}> before the end of the document")
			}
		if byte == '<' {
			next = bytes.get($pos + 1) ?? 0
			if next == '/' {
				name =
					match parse_name(bytes, $pos + 2) {
						Ok(parsed) => parsed
						Err(_) => return fail($pos + 2, "expected an element name in the end tag")
					}
				if name.val != $current.name {
					return fail($pos, "end tag </${name.val}> does not match start tag <${$current.name}>")
				}
				close = skip_space(bytes, name.pos)
				if bytes.get(close) != Ok('>') {
					return fail(close, "expected '>' to close the end tag")
				}
				$pos = close + 1
				node = Element($current.name, $current.attributes, flush_text($current.children, $current.text))
				match $stack.last() {
					Err(_) => return Ok({ val: node, pos: $pos })
					Ok(parent) => {
						$stack = $stack.drop_last(1)
						$current = { name: parent.name, attributes: parent.attributes, children: flush_text(parent.children, parent.text).append(node), text: [] }
					}
				}
			} else if starts_with_at(bytes, $pos, "<!--".to_utf8()) {
				$pos = skip_comment(bytes, $pos)?
			} else if starts_with_at(bytes, $pos, "<![CDATA[".to_utf8()) {
				parsed = parse_cdata(bytes, $pos, $current.text)?
				$current = { ..$current, text: parsed.val }
				$pos = parsed.pos
			} else if next == '?' {
				$pos = skip_processing_instruction(bytes, $pos)?
			} else if next == '!' {
				return fail($pos, "expected a comment or CDATA section after '<!'")
			} else {
				tag = parse_start_tag(bytes, $pos)?
				$pos = tag.pos
				if tag.empty {
					$current = { ..$current, children: flush_text($current.children, $current.text).append(Element(tag.name, tag.attributes, [])), text: [] }
				} else {
					$stack = $stack.append($current)
					$current = { name: tag.name, attributes: tag.attributes, children: [], text: [] }
				}
			}
		} else if byte == '&' {
			reference = parse_reference(bytes, $pos)?
			$current = { ..$current, text: $current.text.concat(reference.val) }
			$pos = reference.pos
		} else if byte == ']' and starts_with_at(bytes, $pos, "]]>".to_utf8()) {
			return fail($pos, "']]>' is not allowed in character data")
		} else if byte == '\r' {
			$current = { ..$current, text: $current.text.append('\n') }
			$pos = if bytes.get($pos + 1) == Ok('\n') $pos + 2 else $pos + 1
		} else if byte >= 0x20 and byte < 0x80 {
			$current = { ..$current, text: $current.text.append(byte) }
			$pos = $pos + 1
		} else {
			next = check_char(bytes, $pos)?
			$current = { ..$current, text: $current.text.concat(bytes.sublist({ start: $pos, len: next - $pos })) }
			$pos = next
		}
	}
	crash "unreachable: the element loop only exits by returning"
}

flush_text : List(Xml.Node), List(U8) -> List(Xml.Node)
flush_text = |children, text| {
	if text.is_empty() {
		children
	} else {
		children.append(Text(Str.from_utf8(text) ?? ""))
	}
}

# See https://www.w3.org/TR/xml/#NT-STag and https://www.w3.org/TR/xml/#NT-EmptyElemTag
parse_start_tag : List(U8), U64 -> Try(StartTag, [XmlFail(Failure)])
parse_start_tag = |bytes, start| {
	name =
		match parse_name(bytes, start + 1) {
			Ok(parsed) => parsed
			Err(_) => return fail(start + 1, "expected an element name after '<'")
		}
	var $attributes = []
	# A set keeps the Unique Att Spec check linear in the attribute count.
	var $seen = Set.empty()
	var $pos = name.pos
	while Bool.True {
		before_space = $pos
		$pos = skip_space(bytes, $pos)
		if bytes.get($pos) == Ok('>') {
			return Ok({ name: name.val, attributes: $attributes, empty: Bool.False, pos: $pos + 1 })
		}
		if starts_with_at(bytes, $pos, "/>".to_utf8()) {
			return Ok({ name: name.val, attributes: $attributes, empty: Bool.True, pos: $pos + 2 })
		}
		if $pos >= bytes.len() {
			return fail($pos, "expected '>' to close the start tag <${name.val}>")
		}
		if $pos == before_space {
			return fail($pos, "expected whitespace, '>' or '/>' in the start tag <${name.val}>")
		}
		attribute_name =
			match parse_name(bytes, $pos) {
				Ok(parsed) => parsed
				Err(_) => return fail($pos, "expected an attribute name, '>' or '/>' in the start tag <${name.val}>")
			}
		if $seen.contains(attribute_name.val) {
			return fail($pos, "duplicate attribute ${attribute_name.val}")
		}
		$pos = skip_eq(bytes, attribute_name.pos)?
		value = parse_attribute_value(bytes, $pos)?
		$attributes = $attributes.append({ name: attribute_name.val, value: value.val })
		$seen = $seen.insert(attribute_name.val)
		$pos = value.pos
	}
	crash "unreachable: the start tag loop only exits by returning"
}

# See https://www.w3.org/TR/xml/#NT-AttValue and https://www.w3.org/TR/xml/#AVNormalize
parse_attribute_value : List(U8), U64 -> Parsed(Str)
parse_attribute_value = |bytes, start| {
	quote = bytes.get(start) ?? 0
	if quote != '"' and quote != '\'' {
		return fail(start, "attribute values must be quoted")
	}
	var $value = []
	var $pos = start + 1
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == quote {
			return Ok({ val: Str.from_utf8($value) ?? "", pos: $pos + 1 })
		} else if byte == '<' {
			return fail($pos, "'<' is not allowed in attribute values")
		} else if byte == '&' {
			reference = parse_reference(bytes, $pos)?
			$value = $value.concat(reference.val)
			$pos = reference.pos
		} else if byte == '\r' {
			$value = $value.append(' ')
			$pos = if bytes.get($pos + 1) == Ok('\n') $pos + 2 else $pos + 1
		} else if byte == '\n' or byte == '\t' {
			$value = $value.append(' ')
			$pos = $pos + 1
		} else {
			next = check_char(bytes, $pos)?
			$value = $value.concat(bytes.sublist({ start: $pos, len: next - $pos }))
			$pos = next
		}
	}
	fail(start, "unterminated attribute value")
}

# See https://www.w3.org/TR/xml/#NT-Reference
parse_reference : List(U8), U64 -> Parsed(List(U8))
parse_reference = |bytes, start| {
	if starts_with_at(bytes, start, "&#x".to_utf8()) {
		parse_char_reference(bytes, start, start + 3, 16)
	} else if starts_with_at(bytes, start, "&#".to_utf8()) {
		parse_char_reference(bytes, start, start + 2, 10)
	} else {
		name =
			match parse_name(bytes, start + 1) {
				Ok(parsed) => parsed
				Err(_) => return fail(start, "'&' must start a reference; write &amp; for a literal ampersand")
			}
		if bytes.get(name.pos) != Ok(';') {
			return fail(name.pos, "expected ';' to end the entity reference")
		}
		replacement =
			match name.val {
				"lt" => "<"
				"gt" => ">"
				"amp" => "&"
				"apos" => "'"
				"quot" => "\""
				other => return fail(start, "undeclared entity &${other};")
			}
		Ok({ val: replacement.to_utf8(), pos: name.pos + 1 })
	}
}

# See https://www.w3.org/TR/xml/#NT-CharRef
parse_char_reference : List(U8), U64, U64, U32 -> Parsed(List(U8))
parse_char_reference = |bytes, start, digits_start, base| {
	var $code = 0
	var $pos = digits_start
	var $done = Bool.False
	while !$done {
		digit = digit_value(bytes.get($pos) ?? 0)
		if digit < base {
			next_code = $code * base + digit
			$code = if next_code > 0x110000 0x110000 else next_code
			$pos = $pos + 1
		} else {
			$done = Bool.True
		}
	}
	if $pos == digits_start {
		return fail(digits_start, if base == 16 "expected hexadecimal digits in the character reference" else "expected decimal digits in the character reference")
	}
	if bytes.get($pos) != Ok(';') {
		return fail($pos, "expected ';' to end the character reference")
	}
	if !is_xml_char($code) {
		return fail(start, "character reference to a character that is not allowed in XML")
	}
	Ok({ val: utf8_encode($code), pos: $pos + 1 })
}

## The value of a hexadecimal digit, or 255 for any other byte.
digit_value : U8 -> U32
digit_value = |byte| {
	if byte >= '0' and byte <= '9' {
		U8.to_u32(byte - '0')
	} else if byte >= 'a' and byte <= 'f' {
		U8.to_u32(byte - 'a' + 10)
	} else if byte >= 'A' and byte <= 'F' {
		U8.to_u32(byte - 'A' + 10)
	} else {
		255
	}
}

# See https://www.w3.org/TR/xml/#NT-Name
parse_name : List(U8), U64 -> Try({ val : Str, pos : U64 }, [NotAName])
parse_name = |bytes, start| {
	first = decode_scalar(bytes, start) ?? { code: 0, len: 0 }
	if !is_name_start_char(first.code) {
		return Err(NotAName)
	}
	var $pos = start + first.len
	var $done = Bool.False
	while !$done {
		scalar = decode_scalar(bytes, $pos) ?? { code: 0, len: 0 }
		if is_name_char(scalar.code) {
			$pos = $pos + scalar.len
		} else {
			$done = Bool.True
		}
	}
	Ok({ val: Str.from_utf8(bytes.sublist({ start, len: $pos - start })) ?? "", pos: $pos })
}

is_name_start_char : U32 -> Bool
is_name_start_char = |c| {
	(c >= 'a' and c <= 'z')
		or (c >= 'A' and c <= 'Z')
			or c == ':'
				or c == '_'
					or (c >= 0xC0 and c <= 0xD6)
						or (c >= 0xD8 and c <= 0xF6)
							or (c >= 0xF8 and c <= 0x2FF)
								or (c >= 0x370 and c <= 0x37D)
									or (c >= 0x37F and c <= 0x1FFF)
										or (c >= 0x200C and c <= 0x200D)
											or (c >= 0x2070 and c <= 0x218F)
												or (c >= 0x2C00 and c <= 0x2FEF)
													or (c >= 0x3001 and c <= 0xD7FF)
														or (c >= 0xF900 and c <= 0xFDCF)
															or (c >= 0xFDF0 and c <= 0xFFFD)
																or (c >= 0x10000 and c <= 0xEFFFF)
}

is_name_char : U32 -> Bool
is_name_char = |c| {
	is_name_start_char(c)
		or c == '-'
			or c == '.'
				or (c >= '0' and c <= '9')
					or c == 0xB7
						or (c >= 0x300 and c <= 0x36F)
							or (c >= 0x203F and c <= 0x2040)
}

# See https://www.w3.org/TR/xml/#NT-Char
is_xml_char : U32 -> Bool
is_xml_char = |c| {
	c == 0x9
		or c == 0xA
			or c == 0xD
				or (c >= 0x20 and c <= 0xD7FF)
					or (c >= 0xE000 and c <= 0xFFFD)
						or (c >= 0x10000 and c <= 0x10FFFF)
}

## Check that a legal XML character starts at `pos` and return the position after it.
check_char : List(U8), U64 -> Try(U64, [XmlFail(Failure)])
check_char = |bytes, pos| {
	match decode_scalar(bytes, pos) {
		Err(_) => fail(pos, "invalid UTF-8")
		Ok(scalar) =>
			if is_xml_char(scalar.code) {
				Ok(pos + scalar.len)
			} else {
				fail(pos, "character U+${hex_code(scalar.code)} is not allowed in XML")
			}
	}
}

hex_code : U32 -> Str
hex_code = |code| {
	digits = "0123456789ABCDEF".to_utf8()
	var $out = []
	var $rest = code
	while $rest > 0 or $out.len() < 4 {
		$out = $out.prepend(digits.get(U32.to_u64($rest % 16)) ?? '0')
		$rest = $rest // 16
	}
	Str.from_utf8($out) ?? ""
}

## Decode one UTF-8 scalar, rejecting overlong forms, surrogates, and truncation.
decode_scalar : List(U8), U64 -> Try({ code : U32, len : U64 }, [InvalidUtf8])
decode_scalar = |bytes, pos| {
	b0 = U8.to_u32(bytes.get(pos) ?? return Err(InvalidUtf8))
	cont = |offset| {
		byte = bytes.get(pos + offset) ?? 0
		if byte >= 0x80 and byte < 0xC0 Ok(U8.to_u32(byte - 0x80)) else Err(InvalidUtf8)
	}
	if b0 < 0x80 {
		Ok({ code: b0, len: 1 })
	} else if b0 >= 0xC2 and b0 <= 0xDF {
		Ok({ code: (b0 - 0xC0) * 64 + cont(1)?, len: 2 })
	} else if b0 >= 0xE0 and b0 <= 0xEF {
		code = (b0 - 0xE0) * 4096 + cont(1)? * 64 + cont(2)?
		if code < 0x800 or (code >= 0xD800 and code <= 0xDFFF) Err(InvalidUtf8) else Ok({ code, len: 3 })
	} else if b0 >= 0xF0 and b0 <= 0xF4 {
		code = (b0 - 0xF0) * 262144 + cont(1)? * 4096 + cont(2)? * 64 + cont(3)?
		if code < 0x10000 or code > 0x10FFFF Err(InvalidUtf8) else Ok({ code, len: 4 })
	} else {
		Err(InvalidUtf8)
	}
}

utf8_encode : U32 -> List(U8)
utf8_encode = |code| {
	if code < 0x80 {
		[U32.to_u8_wrap(code)]
	} else if code < 0x800 {
		[U32.to_u8_wrap(0xC0 + code // 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else if code < 0x10000 {
		[U32.to_u8_wrap(0xE0 + code // 4096), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
	} else {
		[U32.to_u8_wrap(0xF0 + code // 262144), U32.to_u8_wrap(0x80 + (code // 4096) % 64), U32.to_u8_wrap(0x80 + (code // 64) % 64), U32.to_u8_wrap(0x80 + code % 64)]
	}
}

starts_with_at : List(U8), U64, List(U8) -> Bool
starts_with_at = |bytes, pos, prefix| {
	bytes.sublist({ start: pos, len: prefix.len() }) == prefix
}

skip_space : List(U8), U64 -> U64
skip_space = |bytes, start| {
	var $pos = start
	while is_space_at(bytes, $pos) {
		$pos = $pos + 1
	}
	$pos
}

# See https://www.w3.org/TR/xml/#NT-S
is_space : U8 -> Bool
is_space = |byte| byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r'

is_space_at : List(U8), U64 -> Bool
is_space_at = |bytes, pos| {
	match bytes.get(pos) {
		Ok(byte) => is_space(byte)
		Err(_) => Bool.False
	}
}

is_digit_at : List(U8), U64 -> Bool
is_digit_at = |bytes, pos| {
	match bytes.get(pos) {
		Ok(byte) => byte >= '0' and byte <= '9'
		Err(_) => Bool.False
	}
}

is_alphabetical : U8 -> Bool
is_alphabetical = |c| (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')

# See https://www.w3.org/TR/xml/#NT-EncName
is_encoding_char : U8 -> Bool
is_encoding_char = |c| is_alphabetical(c) or (c >= '0' and c <= '9') or c == '.' or c == '_' or c == '-'

ascii_lowercase : List(U8) -> List(U8)
ascii_lowercase = |bytes| bytes.map(|c| if c >= 'A' and c <= 'Z' c + 32 else c)

v1_dot0 : Xml.Version
v1_dot0 = Xml.Version.new(0)

root_of : Str -> Try(Xml.Node, [XmlError(Xml.Error)])
root_of = |input| Xml.parse_str(input).map_ok(|xml| xml.root)

error_at : Str -> Try({ line : U64, column : U64 }, [Parsed])
error_at = |input| {
	match Xml.parse_str(input) {
		Ok(_) => Err(Parsed)
		Err(XmlError(error)) => Ok({ line: error.line, column: error.column })
	}
}

test_xml =
	\\<?xml version=\"1.0\" encoding=\"utf-8\"?>
	\\<root>
	\\    <element arg=\"value\" />
	\\</root>

## Full XML parsing captures the declaration and root element.
expect {
	result = Utf8.parse_str(Xml.xml_parser, test_xml)?

	result
		== {
			xml_declaration: Given({
				version: v1_dot0,
				encoding: Given(Utf8Encoding),
			}),
			root: Element(
				"root",
				[],
				[
					Text("\n    "),
					Element(
						"element",
						[{ name: "arg", value: "value" }],
						[],
					),
					Text("\n"),
				],
			),
		}
}

## XML parsing accepts documents without a prolog.
expect {
	result = Utf8.parse_str(Xml.xml_parser, "<element />")?

	result
		== {
			xml_declaration: Missing,
			root: Element("element", [], []),
		}
}

## Empty elements can omit whitespace before the self-closing marker.
expect root_of("<element/>") == Ok(Element("element", [], []))

## Empty elements can carry attributes.
expect root_of("<element arg=\"value\"/>") == Ok(Element("element", [{ name: "arg", value: "value" }], []))

## Explicit start and end tags can represent an empty element.
expect root_of("<element></element>") == Ok(Element("element", [], []))

## Elements can parse multiple attributes and text content.
expect {
	result = root_of("<element firstArg=\"one\" secondArg='two'>text content</element>")
	result
		== Ok(
			Element(
				"element",
				[
					{ name: "firstArg", value: "one" },
					{ name: "secondArg", value: "two" },
				],
				[Text("text content")],
			),
		)
}

## CDATA sections parse into text nodes.
expect root_of("<element><![CDATA[<literal />]]></element>") == Ok(Element("element", [], [Text("<literal />")]))

## Partial CDATA closing text is preserved until the real close marker.
expect root_of("<element><![CDATA[this is ]] not ]> the end]]></element>") == Ok(Element("element", [], [Text("this is ]] not ]> the end")]))

## Nested elements preserve attributes on parent and child nodes.
expect {
	result = root_of("<parent argParent=\"outer\"><child argChild=\"inner\" /></parent>")
	result == Ok(Element("parent", [{ name: "argParent", value: "outer" }], [Element("child", [{ name: "argChild", value: "inner" }], [])]))
}

## Nested element parsing preserves whitespace text nodes.
expect root_of("<parent>\n    <child />\n</parent>") == Ok(Element("parent", [], [Text("\n    "), Element("child", [], []), Text("\n")]))

## Elements can parse a diverse set of child nodes.
expect {
	result = root_of(
		\\<feed xmlns="http://www.w3.org/2005/Atom">
		\\    <title>Atom Feed</title>
		\\    <link rel="self" type="application/atom+xml" href="http://example.org" />
		\\    <updated>2024-02-23T20:38:24Z</updated>
		\\</feed>
		,
	)

	result
		== Ok(
			Element(
				"feed",
				[{ name: "xmlns", value: "http://www.w3.org/2005/Atom" }],
				[
					Text("\n    "),
					Element("title", [], [Text("Atom Feed")]),
					Text("\n    "),
					Element(
						"link",
						[
							{ name: "rel", value: "self" },
							{ name: "type", value: "application/atom+xml" },
							{ name: "href", value: "http://example.org" },
						],
						[],
					),
					Text("\n    "),
					Element("updated", [], [Text("2024-02-23T20:38:24Z")]),
					Text("\n"),
				],
			),
		)
}

## Full XML parsing ignores trailing whitespace after the root, and matches
## encoding names case-insensitively (XML 1.0 section 4.3.3).
expect {
	result = Xml.parse_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<root><Example></Example></root>\n")
	result
		== Ok({
			xml_declaration: Given({ version: v1_dot0, encoding: Given(Utf8Encoding) }),
			root: Element("root", [], [Element("Example", [], [])]),
		})
}

## Malformed input ending in a multibyte scalar returns an error instead of crashing
## while rendering a parser failure from a mid-scalar byte position.
expect Utf8.parse_str(Xml.xml_parser, "<ӿ").is_err()

## End tags must match their start tag (WFC: Element Type Match).
expect error_at("<a><b></a></b>") == Ok({ line: 1, column: 7 })

## Names may contain digits and non-ASCII letters.
expect root_of("<h1 données-2='x'>é</h1>") == Ok(Element("h1", [{ name: "données-2", value: "x" }], [Text("é")]))

## Predefined entity and character references are replaced in text and attributes.
expect root_of("<a t='&lt;&#65;&#x1F600;&quot;'>&amp;&gt;&apos;</a>") == Ok(Element("a", [{ name: "t", value: "<A😀\"" }], [Text("&>'")]))

## Undeclared entities, bare ampersands, and references to illegal characters are errors.
expect error_at("<a>&nbsp;</a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>AT&T</a>") == Ok({ line: 1, column: 8 })
expect error_at("<a>&#0;</a>").is_ok()
expect error_at("<a>&#xD800;</a>").is_ok()
expect error_at("<a>&#99999999999999999999;</a>").is_ok()

## '<' is not allowed in attribute values, and attributes need quotes,
## separating whitespace, and unique names.
expect error_at("<a b='<'/>") == Ok({ line: 1, column: 7 })
expect error_at("<a b=c/>") == Ok({ line: 1, column: 6 })
expect error_at("<a b='1'c='2'/>") == Ok({ line: 1, column: 9 })
expect error_at("<a b='1' b='2'/>") == Ok({ line: 1, column: 10 })

## Attribute values are normalised: literal whitespace becomes a space, while
## character references keep the character they name.
expect root_of("<a b='x\r\ny\tz\n&#10;&#9;'/>") == Ok(Element("a", [{ name: "b", value: "x y z \n\t" }], []))

## Line endings in content are normalised to LF (XML 1.0 section 2.11).
expect root_of("<a>1\r\n2\r3<![CDATA[\r\n]]></a>") == Ok(Element("a", [], [Text("1\n2\n3\n")]))

## Comments and processing instructions are accepted everywhere Misc is
## allowed, and text on both sides of them is merged.
expect {
	result = Xml.parse_str("<!-- c --><?pi data?>\n<a>x<!-- y -->z<?p?><![CDATA[w]]>&amp;</a><!---->\n<?q ?>")
	result.map_ok(|xml| xml.root) == Ok(Element("a", [], [Text("xzw&")]))
}

## Comments may not contain '--', and PI targets may not be 'xml'.
expect error_at("<a><!-- a -- b --></a>") == Ok({ line: 1, column: 11 })
expect error_at("<a><!-- a ---></a>") == Ok({ line: 1, column: 11 })
expect error_at("<a/>\n<?xml version='1.0'?>") == Ok({ line: 2, column: 1 })
expect error_at("<a><?XmL x?></a>") == Ok({ line: 1, column: 6 })

## ']]>' is not allowed in character data, and control characters are not XML characters.
expect error_at("<a>]]></a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>\u(1)</a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>\u(FFFE)</a>") == Ok({ line: 1, column: 4 })

## Exactly one root element is required.
expect error_at("") == Ok({ line: 1, column: 1 })
expect error_at("<a/><b/>") == Ok({ line: 1, column: 5 })
expect error_at("<a/>text") == Ok({ line: 1, column: 5 })
expect error_at("<a>") == Ok({ line: 1, column: 4 })

## Document type declarations are outside the supported subset.
expect error_at("<!DOCTYPE a><a/>") == Ok({ line: 1, column: 1 })

## The XML declaration accepts standalone, requires whitespace between its
## parts, and must be first.
expect Xml.parse_str("<?xml version='1.0' standalone='yes'?><a/>").is_ok()
expect error_at("<?xml version='1.0'encoding='UTF-8'?><a/>") == Ok({ line: 1, column: 20 })
expect error_at(" <?xml version='1.0'?><a/>") == Ok({ line: 1, column: 2 })
expect error_at("<?xml version='2.0'?><a/>") == Ok({ line: 1, column: 16 })

## A byte order mark may start the document.
expect Xml.parse_str("\u(FEFF)<a/>").is_ok()

## Error lines count CRLF, CR, and LF line endings.
expect error_at("<a>\r\n\r\n<b>\r</a>") == Ok({ line: 4, column: 1 })

## Deep nesting does not exhaust the call stack.
expect {
	depth = 20000
	input = Str.concat(Str.repeat("<a>", depth), Str.repeat("</a>", depth))
	Xml.parse_str(input).is_ok()
}

## The parser combinator reports leftover input after the document.
expect Utf8.parse_str(Xml.xml_parser, "<a/> <b/>") == Err(ParseError({ message: "unexpected input", offset: 5 }))

## Duplicate attribute detection stays fast with many attributes.
expect {
	count = 20000
	attributes = List.repeat(0, count).map_with_index(|_, index| " a${index.to_str()}=''") |> Str.join_with("")
	parsed = Xml.parse_str("<e${attributes}/>")
	duplicate = Xml.parse_str("<e${attributes} a${(count - 1).to_str()}=''/>")
	parsed.is_ok() and duplicate.is_err()
}

## Parse examples in the module docs: text references and error positions.
expect Xml.parse_str("<a>x &amp; y</a>").map_ok(|xml| xml.root) == Ok(Element("a", [], [Text("x & y")]))
expect {
	match Xml.parse_str("<a>&nbsp;</a>") {
		Err(XmlError({ line, column, message: _ })) => line == 1 and column == 4
		Ok(_) => Bool.False
	}
}
