import Parser
import Utf8
import Literal as Quotation

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
## `svg:path` is kept as written; [XmlNs] resolves them. Input is already
## decoded text, so a declared encoding is reported but not used to decode.
##
## Read a document with [Xml.parse_str], or with [Xml.parse] and limits for
## untrusted input, then walk it with pattern matching or the [Xml.Node]
## helpers `name`, `attribute`, `children_named` and `text`. [Xml.reader]
## reads a document one event at a time instead:
##
## ```roc
## expect {
##     xml = Xml.parse_str("<feed><title>News</title><link href='/a'/></feed>")?
##     titles = xml.root.children_named("title").map(|title| title.text())
##     links = xml.root.children_named("link").map(|link| link.attribute("href"))
##     titles == ["News"] and links == [Ok("/a")]
## }
## ```
##
## Originally written by [Johannes Maas](https://github.com/j-maas).
Xml := {
	declaration : Try(Xml.Declaration, [Missing]),
	root : Xml.Node,
}.{

	## Compare two XML documents structurally (declaration and tree).
	is_eq : _

	## Hash an XML document, so documents can be `Dict` keys and `Set` members.
	to_hash : _

	## Optional checks for suspicious XML that is otherwise well formed.
	Lints := { non_utf8_declaration : Bool ?? True }

	## Checked source and enabled lint rules. A source name appears in errors.
	ValidationProps := { source : Str, name : Str ?? "<literal>", lints : Lints ?? {} }

	## Parse complete XML, rejecting enabled lints after native parse errors.
	## Input is already UTF-8, so a different declared encoding is suspicious.
	validate : ValidationProps -> Try(Xml, [BadQuotedBytes(Str)])
	validate = |props| {
		xml = Xml.parse_str(props.source).map_err(|InvalidXml(error)| Quotation.error({ name: props.name, format: "XML", message: error.message, line: error.line, column: error.column }))?
		if props.lints.non_utf8_declaration {
			match xml.declaration {
				Ok({ encoding: Ok(OtherEncoding(encoding)), .. }) => return Err(Quotation.error({ name: props.name, format: "XML", line: 1, message: "non_utf8_declaration: input is UTF-8 but declares ${encoding}" }))
				_ => {}
			}
		}
		Ok(xml)
	}

	## Use a well-formed, lint-checked XML document as a quoted value.
	from_quote : Str -> Try(Xml, [BadQuotedBytes(Str)])
	from_quote = |source| Xml.validate({ source: source })

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

	## Version and optional encoding from an XML declaration. An absent
	## encoding is `Err(Missing)`.
	Declaration : {
		version : Version,
		encoding : Try(TextEncoding, [Missing]),
	}

	## An XML version from the declaration, such as `{ major: 1, minor: 0 }`
	## for `version="1.0"`. Only `1.x` versions are accepted, so `major` is
	## always `1`.
	Version : { major : U64, minor : U64 }

	## An XML element or text node.
	##
	## An `Element` holds its `name` as written, its `attributes` in source
	## order, and its `children` in document order. `Text(str)` is decoded
	## character data; adjacent text, CDATA and references are merged into one
	## `Text`, and whitespace-only text between elements is kept.
	Node := [
		Element({ name : Str, attributes : List(Xml.Attribute), children : List(Node) }),
		Text(Str),
	].{

		## Compare two XML nodes structurally.
		is_eq : _

		## Hash an XML node, so nodes can be `Dict` keys and `Set` members.
		to_hash : _

		## The name of an element, or `Err(NotAnElement)` for a text node.
		##
		## ```roc
		## expect Xml.parse_str("<svg:rect/>").map_ok(|xml| xml.root.name()) == Ok(Ok("svg:rect"))
		## ```
		name : Node -> Try(Str, [NotAnElement])
		name = |node| {
			match node {
				Element(element) => Ok(element.name)
				Text(_) => Err(NotAnElement)
			}
		}

		## The value of the element's attribute with this exact name, or
		## `Err(Missing)` when there is none or the node is text.
		##
		## ```roc
		## expect {
		##     root = Xml.parse_str("<a href='/x'/>")?.root
		##     root.attribute("href") == Ok("/x") and root.attribute("title") == Err(Missing)
		## }
		## ```
		attribute : Node, Str -> Try(Str, [Missing])
		attribute = |node, attribute_name| {
			match node {
				Element(element) =>
					match element.attributes.find_first(|attr| attr.name == attribute_name) {
						Ok(attr) => Ok(attr.value)
						Err(_) => Err(Missing)
					}

				Text(_) => Err(Missing)
			}
		}

		## The element's direct child elements with this exact name, in
		## document order. Empty for a text node.
		##
		## ```roc
		## expect {
		##     root = Xml.parse_str("<list><item>1</item><other/><item>2</item></list>")?.root
		##     root.children_named("item").map(|item| item.text()) == ["1", "2"]
		## }
		## ```
		children_named : Node, Str -> List(Node)
		children_named = |node, child_name| {
			match node {
				Element(element) =>
					element.children.keep_if(
						|child| {
							match child {
								Element(child_element) => child_element.name == child_name
								Text(_) => False
							}
						},
					)

				Text(_) => []
			}
		}

		## All character data in the node and its descendants, concatenated in
		## document order (like the DOM's `textContent`).
		##
		## ```roc
		## expect Xml.parse_str("<p>Hello, <b>world</b>!</p>").map_ok(|xml| xml.root.text()) == Ok("Hello, world!")
		## ```
		text : Node -> Str
		text = |node| {
			# An explicit work list keeps deep trees from exhausting the call stack.
			var $out = []
			var $pending = [node]
			while !$pending.is_empty() {
				match $pending.last() {
					Ok(Text(str)) => {
						$out = $out.concat(str.to_utf8())
						$pending = $pending.drop_last(1)
					}

					Ok(Element(element)) => {
						$pending = $pending.drop_last(1)
						var $index = element.children.len()
						while $index > 0 {
							$index = $index - 1
							match element.children.get($index) {
								Ok(child) => {
									$pending = $pending.append(child)
								}

								Err(_) => {}
							}
						}
					}

					Err(_) => {
						$pending = []
					}
				}
			}
			Str.from_utf8($out) ?? ""
		}
	}

	## Location and explanation of malformed XML. Lines and columns are
	## one-based; columns count UTF-8 bytes, and `\r\n`, `\r`, and `\n` each
	## end a line.
	Error : { line : U64, column : U64, message : Str }

	## Parse one complete XML document.
	##
	## Leading and trailing whitespace, comments and processing instructions
	## around the root element are allowed; anything else after it is an error.
	## A leading byte order mark is accepted. No limits apply: use [Xml.parse]
	## for untrusted input.
	##
	## ```roc
	## expect Xml.parse_str("<a>x &amp; y</a>").map_ok(|xml| xml.root) == Ok(Element({ name: "a", attributes: [], children: [Text("x & y")] }))
	##
	## expect Xml.parse_str("<a>&nbsp;</a>") == Err(InvalidXml({ line: 1, column: 4, message: "undeclared entity &nbsp;" }))
	## ```
	parse_str : Str -> Try(Xml, [InvalidXml(Error)])
	parse_str = |input| {
		bytes = input.to_utf8()
		match read_document(input, bytes, unbounded(bytes)) {
			Ok({ val, pos }) =>
				if pos == bytes.len() {
					Ok(val)
				} else {
					Err(InvalidXml(locate(bytes, pos, "unexpected content after the root element")))
				}

			Err(XmlFail(failure)) => Err(InvalidXml(locate(bytes, failure.offset, failure.message)))
			Err(LimitFail(exceeded)) => Err(InvalidXml(locate(bytes, exceeded.offset, "limit exceeded")))
		}
	}

	## Parse one complete XML document from untrusted input, within the
	## limits of `props`. A document that would exceed one fails with
	## `LimitExceeded`, located at the start tag or attribute that would
	## exceed it, before anything is built for it.
	##
	## ```roc
	## expect Xml.parse("<a><b/></a>", { max_depth: 1 }) == Err(LimitExceeded({ limit: Depth, max: 1, line: 1, column: 4 }))
	##
	## expect Xml.parse("<a><b/></a>", {}).is_ok()
	## ```
	parse : Str, ReaderProps -> Try(Xml, ReadError)
	parse = |input, props| {
		bytes = input.to_utf8()
		match read_document(input, bytes, reader_limits(props)) {
			Ok({ val, pos }) =>
				if pos == bytes.len() {
					Ok(val)
				} else {
					Err(InvalidXml(locate(bytes, pos, "unexpected content after the root element")))
				}

			Err(failure) => Err(read_error(bytes, failure))
		}
	}

	## Parse one XML document, including an optional declaration and any
	## trailing comments, processing instructions and whitespace, and leave
	## the input after that for the next parser.
	##
	## A failure is `ParseError({ message, offset })` with the same message
	## [Xml.parse_str] reports and the byte offset of the problem. Use this to
	## embed an XML document in a larger parser; otherwise prefer
	## [Xml.parse_str], which reports a line and column.
	##
	## ```roc
	## expect Utf8.parse_str(Xml.parser, "<a></b>") == Err(ParseError({ message: "end tag </b> does not match start tag <a>", offset: 3 }))
	##
	## expect Utf8.parse_str(Xml.parser, "<a/> <b/>") == Err(ParseError({ message: "unexpected input", offset: 5 }))
	## ```
	parser : Parser(Utf8.Bytes, Xml)
	parser =
		Parser.custom(
			|input| {
				# Validate once, so names and text can be cut from the string
				# without validating each one again.
				match read_document(Str.from_utf8(input) ?? "", input, unbounded(input)) {
					Ok({ val, pos }) => Ok({ value: val, rest: input.drop_first(pos) })
					Err(XmlFail(failure)) => Err(ParseError({ message: failure.message, offset: failure.offset }))
					Err(LimitFail(exceeded)) => Err(ParseError({ message: "limit exceeded", offset: exceeded.offset }))
				}
			},
		)

	## Limits for untrusted input, checked while reading.
	##
	## A document deeper than `max_depth` elements, with more than
	## `max_elements` elements, or with an element that has more than
	## `max_attributes` attributes fails with `LimitExceeded`. The defaults
	## admit large real documents: at about 28 bytes per element,
	## 4,194,304 elements is a 117 MB document.
	##
	## `namespaces` also rejects processing instruction targets that contain
	## a colon, as [Namespaces in XML 1.0](https://www.w3.org/TR/xml-names/#Conformance)
	## requires. [XmlNs] sets it; names are still reported as written.
	ReaderProps := { max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_attributes : U64 ?? 1024, namespaces : Bool ?? False }

	## A limit of [Xml.ReaderProps].
	Limit : [Depth, Elements, Attributes]

	## Why reading stopped: malformed XML, or a document that would exceed a
	## limit of [Xml.ReaderProps]. `max` is the limit, and the line and column
	## locate the start tag (or, for `Attributes`, the attribute) that would
	## exceed it.
	ReadError : [
		InvalidXml(Xml.Error),
		LimitExceeded({ limit : Limit, max : U64, line : U64, column : U64 }),
	]

	## One step through a document, in document order.
	##
	## Events match the tree [Xml.parse_str] builds: an empty-element tag
	## such as `<a/>` gives `Start` then `End`, adjacent character data, CDATA
	## sections and references form one `Text`, and comments and processing
	## instructions are checked and then dropped. Text before and after the
	## root element is not reported.
	Event : [
		Start({ name : Str, attributes : List(Xml.Attribute) }),
		End(Str),
		Text(Str),
	]

	## A pull reader over one document, for reading a large document an
	## element at a time without building its whole tree.
	##
	## Get one with [Xml.reader], then call `next` until it returns `Done`.
	## `element`, called directly after a `Start`, builds that element's
	## subtree and continues after its end tag. A reader is a plain value:
	## keep only the latest one, and `return` from the loop's `Done` arm, so
	## each step updates the reader's stack of open elements in place.
	##
	## ```roc
	## expect {
	##     count_items = |reader| {
	##         var $reader = reader
	##         var $count = 0
	##         while True {
	##             match $reader.next() {
	##                 Event(Start({ name: "item", .. }), rest) => {
	##                     $reader = rest
	##                     $count = $count + 1
	##                 }
	##
	##                 Event(_, rest) => {
	##                     $reader = rest
	##                 }
	##
	##                 Done => return Ok($count)
	##                 Failed(error) => return Err(error)
	##             }
	##         }
	##     }
	##     count_items(Xml.reader("<list><item/><item>x</item></list>", {})?) == Ok(2)
	## }
	## ```
	Reader :: { src : Str, bytes : List(U8), limits : Limits, declaration : Box(Try(Xml.Declaration, [Missing])), cursor : Cursor }.{

		## The next event, `Done` once the root element has ended and the rest
		## of the input is only comments, processing instructions and
		## whitespace, or `Failed` at the first problem.
		##
		## ```roc
		## expect {
		##     reader = Xml.reader("<a>x</a>", {})?
		##     match reader.next() {
		##         Event(Start({ name, .. }), rest) =>
		##             match rest.next() {
		##                 Event(Text(text), _) => name == "a" and text == "x"
		##                 _ => False
		##             }
		##
		##         _ => False
		##     }
		## }
		## ```
		next : Reader -> [Event(Xml.Event, Reader), Done, Failed(Xml.ReadError)]
		next = |reader| {
			bytes = reader.bytes
			match step(reader.src, bytes, reader.limits, reader.cursor) {
				Ok(Ev(event, cursor)) => Event(event, { ..reader, cursor })
				Ok(Finished(pos)) =>
					if pos == bytes.len() {
						Done
					} else {
						Failed(InvalidXml(locate(bytes, pos, "unexpected content after the root element")))
					}

				Err(failure) => Failed(read_error(bytes, failure))
			}
		}

		## Directly after a `Start` event, that element and its subtree, and
		## the reader after its end tag. `NotAfterStart` when the most recent
		## event was not a `Start`.
		##
		## ```roc
		## expect {
		##     reader = Xml.reader("<doc><p>one</p><p>two</p></doc>", {})?
		##     match reader.next() {
		##         Event(Start(_), after_doc) =>
		##             match after_doc.next() {
		##                 Event(Start(_), after_p) =>
		##                     match after_p.element() {
		##                         Element(p, _) => p.text() == "one"
		##                         _ => False
		##                     }
		##
		##                 _ => False
		##             }
		##
		##         _ => False
		##     }
		## }
		## ```
		element : Reader -> [Element(Xml.Node, Reader), Failed(Xml.ReadError), NotAfterStart]
		element = |reader| {
			if reader.cursor.start == no_start {
				return NotAfterStart
			}
			bytes = reader.bytes
			tag =
				match parse_start_tag(reader.src, bytes, reader.cursor.start, reader.limits.attributes) {
					Ok(parsed) => parsed
					Err(failure) => return Failed(read_error(bytes, failure))
				}
			match build_element(reader.src, bytes, reader.limits, reader.cursor, tag.name, tag.attributes) {
				Ok(built) => Element(built.node, { ..reader, cursor: built.cursor })
				Err(failure) => Failed(read_error(bytes, failure))
			}
		}

		## Directly after a `Start` event, builds that element's subtree as
		## your own type, as `element` builds an [Xml.Node], and returns the
		## reader after its end tag. Use it to build a model of the document
		## without an [Xml.Node] tree beside it.
		##
		## `first` stands for the element whose `Start` was just read. Each
		## element inside it is passed to `start` with its name and attributes,
		## which may fail with your own error; `text` makes a node of text; and
		## `end` makes an element's node from what `start` (or `first`) gave and
		## its children. `context` is threaded through `start` and `end` in
		## document order. A failure of `start` is `Rejected` with the location
		## of that start tag.
		##
		## ```roc
		## expect {
		##     reader = Xml.reader("<doc><p>one</p><p>two<b/></p></doc>", {})?
		##     match reader.next() {
		##         Event(Start(_), rest) =>
		##             match rest.element_with({ context: 0, first: "doc", start: |count, name, _| Ok({ context: count + 1, open: name }), text: |text| text, end: |count, name, children| { context: count, node: "${name}(${Str.join_with(children, ",")})" } }) {
		##                 Element(tree, count, _) => tree == "doc(p(one),p(two,b()))" and count == 3
		##                 _ => False
		##             }
		##
		##         _ => False
		##     }
		## }
		## ```
		element_with : Reader, { context : ctx, first : open, start : ctx, Str, List(Xml.Attribute) -> Try({ context : ctx, open : open }, err), text : Str -> node, end : ctx, open, List(node) -> { context : ctx, node : node } } -> [Element(node, ctx, Reader), Failed(Xml.ReadError), Rejected(err, { line : U64, column : U64 }), NotAfterStart]
		element_with = |reader, builder| {
			if reader.cursor.start == no_start {
				return NotAfterStart
			}
			bytes = reader.bytes
			match build_with(reader.src, bytes, reader.limits, reader.cursor, builder.context, builder.first, builder.start, builder.text, builder.end) {
				Built(built) => Element(built.node, built.context, { ..reader, cursor: built.cursor })
				Unread(failure) => Failed(read_error(bytes, failure))
				Rejected(error, offset) => {
					at = locate(bytes, offset, "")
					Rejected(error, { line: at.line, column: at.column })
				}
			}
		}

		## The document's XML declaration, or `Err(Missing)` when it has none.
		declaration : Reader -> Try(Xml.Declaration, [Missing])
		declaration = |reader| Box.unbox(reader.declaration)

		## Where the most recent event starts: the `<` of its tag, or the
		## first byte of its text. Before the first event, the root's start
		## tag. Lines and columns count as in [Xml.Error].
		location : Reader -> { line : U64, column : U64 }
		location = |reader| {
			at = locate(reader.bytes, reader.cursor.at, "")
			{ line: at.line, column: at.column }
		}
	}

	## A [Xml.Reader] positioned before the root element, after reading and
	## checking the document's prolog.
	##
	## ```roc
	## expect Xml.reader("<?xml version='1.0'?><a/>", {}).map_ok(|reader| reader.declaration().is_ok()) == Ok(True)
	##
	## expect Xml.reader("text", {}).map_ok(|_| {}) == Err(InvalidXml({ line: 1, column: 1, message: "expected a root element, but found text" }))
	## ```
	reader : Str, ReaderProps -> Try(Reader, ReadError)
	reader = |input, props| {
		bytes = input.to_utf8()
		open_reader(input, bytes, reader_limits(props)).map_err(|failure| read_error(bytes, failure))
	}
}

Failure : { offset : U64, message : Str }

ReadFailure : [XmlFail(Failure), LimitFail({ limit : Xml.Limit, max : U64, offset : U64 })]

Parsed(a) : Try({ val : a, pos : U64 }, ReadFailure)

Limits : { depth : U64, elements : U64, attributes : U64, namespaces : Bool }

## A reader holds the input and limits directly, which are only read, and
## its declaration boxed, which is rarely read: then a step moves no more
## than it must and touches no reference counts to reach the input. The tree
## builder passes the same parts as plain parameters.
##
## Where a reader is: its position, where the names of the open elements
## are in the input (offsets, so that no step touches a reference count),
## whether
## the most recent start tag was an empty-element tag whose end is still to
## be reported, whether the root has ended, where the most recent `Start`
## (or `no_start`) and the most recent event began, and the element count.
Cursor : { pos : U64, open : List({ start : U64, end : U64 }), pending_end : Bool, closed : Bool, start : U64, at : U64, elements : U64 }

StartTag : {
	name : Str,
	attributes : List(Xml.Attribute),
	empty : Bool,
	pos : U64,
}

## An element whose end tag has not been read yet. Its children so far are
## the entries of the shared node list from `first_child` on.
Open : {
	name : Str,
	attributes : List(Xml.Attribute),
	range : { start : U64, end : U64 },
	first_child : U64,
}

## A reader event, or the position after the root element and the comments,
## processing instructions and whitespace that follow it.
Step : [Ev(Xml.Event, Cursor), Finished(U64)]

fail : U64, Str -> Try(_, ReadFailure)
fail = |offset, message| Err(XmlFail({ offset, message }))

read_error : List(U8), ReadFailure -> Xml.ReadError
read_error = |bytes, failure| {
	match failure {
		XmlFail(problem) => InvalidXml(locate(bytes, problem.offset, problem.message))
		LimitFail(exceeded) => {
			at = locate(bytes, exceeded.offset, "")
			LimitExceeded({ limit: exceeded.limit, max: exceeded.max, line: at.line, column: at.column })
		}
	}
}

reader_limits : Xml.ReaderProps -> Limits
reader_limits = |props| { depth: props.max_depth, elements: props.max_elements, attributes: props.max_attributes, namespaces: props.namespaces }

# Limits no document can reach: an element takes at least four bytes, a
# level of depth at least seven and an attribute at least five.
unbounded : List(U8) -> Limits
unbounded = |bytes| { depth: bytes.len(), elements: bytes.len(), attributes: bytes.len(), namespaces: False }

# `start` when the most recent event was not `Start`.
no_start : U64
no_start = 18_446_744_073_709_551_615

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
#
# The tree is built from the reader's events, so well-formedness and limits
# are checked in one place. `src` is the input as a `Str` when it is valid
# UTF-8, or "" when it is not; names and text are then cut from it instead
# of being validated one by one.
read_document : Str, List(U8), Limits -> Parsed(Xml)
read_document = |src, bytes, limits| {
	opened = open_reader(src, bytes, limits)?
	declaration = Box.unbox(opened.declaration)
	match step(src, bytes, limits, opened.cursor)? {
		Ev(Start(root), after_start) => {
			built = build_element(src, bytes, limits, after_start, root.name, root.attributes)?
			match step(src, bytes, limits, built.cursor)? {
				Finished(end) => Ok({ val: { declaration, root: built.node }, pos: end })
				Ev(_, _) => fail(0, "internal error: an event after the root element")
			}
		}

		_ => fail(0, "internal error: the document did not start with its root element")
	}
}

# Reads the prolog up to the root element's start tag.
open_reader : Str, List(U8), Limits -> Try(Xml.Reader, ReadFailure)
open_reader = |src, bytes, limits| {
	var $pos = if (starts_with_at(bytes, 0, [0xEF, 0xBB, 0xBF])) 3 else 0
	var $declaration = Err(Missing)
	if starts_with_at(bytes, $pos, "<?xml".to_utf8()) and is_space_at(bytes, $pos + 5) {
		parsed = parse_xml_declaration(bytes, $pos)?
		$declaration = Ok(parsed.val)
		$pos = parsed.pos
	}
	$pos = skip_misc(bytes, $pos, limits.namespaces)?
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
	Ok({ src, bytes, limits, declaration: Box.box($declaration), cursor: { pos: $pos, open: [], pending_end: False, closed: False, start: no_start, at: $pos, elements: 0 } })
}

# One event. Fields are read before the cursor is updated, so its stack of
# open element names stays uniquely referenced and is updated in place.
# Every event is built here, once.
step : Str, List(U8), Limits, Cursor -> Try(Step, ReadFailure)
step = |src, bytes, limits, r| {
	if r.pending_end {
		# The end of an empty-element tag.
		name = open_name(src, bytes, r.open)
		open = r.open.drop_last(1)
		closed = open.is_empty()
		return Ok(Ev(End(name), { ..r, open, pending_end: False, closed, start: no_start }))
	}
	if r.closed {
		return Ok(Finished(skip_misc(bytes, r.pos, limits.namespaces)?))
	}
	at =
		if r.open.is_empty() {
			r.pos
		} else {
			match scan(src, bytes, limits.namespaces, r.pos)? {
				TagAt(tag) => tag
				TextUntil(text, pos) => return Ok(Ev(Text(text), { ..r, pos, start: no_start, at: r.pos }))
				EndOfInput(pos) => return fail(pos, "expected </${open_name(src, bytes, r.open)}> before the end of the document")
			}
		}
	if bytes.get(at + 1) == Ok('/') {
		end_tag = read_end_tag(src, bytes, at, r.open.last() ?? { start: 0, end: 0 })?
		open = r.open.drop_last(1)
		closed = open.is_empty()
		return Ok(Ev(End(slice_str(src, bytes, at + 2, end_tag.name_end)), { ..r, pos: end_tag.pos, open, closed, start: no_start, at }))
	}
	check_start(limits, r.open.len(), r.elements, at)?
	tag = parse_start_tag(src, bytes, at, limits.attributes)?
	elements = r.elements + 1
	open = r.open.append({ start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() })
	Ok(Ev(Start({ name: tag.name, attributes: tag.attributes }), { ..r, pos: tag.pos, open, pending_end: tag.empty, start: at, at, elements }))
}

# The name of the innermost open element.
open_name : Str, List(U8), List({ start : U64, end : U64 }) -> Str
open_name = |src, bytes, open| {
	name = open.last() ?? { start: 0, end: 0 }
	slice_str(src, bytes, name.start, name.end)
}

# `scan_content`, without its call for the common cases: a tag, or plain
# ASCII text up to a tag. Small enough to be inlined into the loops.
scan : Str, List(U8), Bool, U64 -> Try(Scanned, ReadFailure)
scan = |src, bytes, namespaces, start| {
	byte = bytes.get(start) ?? 0
	if byte == '<' and starts_tag(bytes, start) {
		Ok(TagAt(start))
	} else {
		end = Utf8.skip_class(bytes, start, text_class)
		if end > start and bytes.get(end) == Ok('<') and starts_tag(bytes, end) {
			Ok(TextUntil(slice_str(src, bytes, start, end), end))
		} else {
			scan_content(src, bytes, namespaces, start)
		}
	}
}

# Whether the '<' at `pos` starts a start or end tag rather than markup that
# text continues across.
starts_tag : List(U8), U64 -> Bool
starts_tag = |bytes, pos| {
	next = bytes.get(pos + 1) ?? 0
	next != '!' and next != '?'
}

## What follows in element content: text ending before a tag, a start or
## end tag, or the end of the input.
Scanned : [TextUntil(Str, U64), TagAt(U64), EndOfInput(U64)]

# See https://www.w3.org/TR/xml/#NT-content
#
# Text is merged across character data, references, CDATA sections,
# comments and processing instructions, and reported before the next tag.
# Pending text is `$text` followed by the input slice [$run_start, $run_end).
# Bytes kept as they are extend the slice; a rewrite (a reference, a line
# ending or a gap left by markup) copies the slice once and starts a new
# one, so decoding stays linear in the text's length.
scan_content : Str, List(U8), Bool, U64 -> Try(Scanned, ReadFailure)
scan_content = |src, bytes, namespaces, start| {
	var $text = []
	var $run_start = start
	var $run_end = start
	var $pos = start
	while True {
		byte =
			match bytes.get($pos) {
				Ok(b) => b
				Err(_) => return Ok(EndOfInput($pos))
			}
		if byte == '<' {
			next = bytes.get($pos + 1) ?? 0
			pending = $run_end > $run_start or !$text.is_empty()
			if (next != '!' and next != '?') and pending {
				return Ok(TextUntil(text_str(src, bytes, $text, $run_start, $run_end), $pos))
			}
			if next == '/' {
				return Ok(TagAt($pos))
			} else if next == '!' and starts_with_at(bytes, $pos, "<!--".to_utf8()) {
				$pos = skip_comment(bytes, $pos)?
			} else if next == '!' and starts_with_at(bytes, $pos, "<![CDATA[".to_utf8()) {
				# See https://www.w3.org/TR/xml/#NT-CDSect
				cdata_start = $pos
				$pos = $pos + 9
				var $open = True
				while $open {
					cdata_byte =
						match bytes.get($pos) {
							Ok(b) => b
							Err(_) => return fail(cdata_start, "unterminated CDATA section")
						}
					if cdata_byte == ']' and starts_with_at(bytes, $pos, "]]>".to_utf8()) {
						$pos = $pos + 3
						$open = False
					} else if cdata_byte == '\r' {
						$text = materialize(bytes, $text, $run_start, $run_end).append('\n')
						$pos = if (bytes.get($pos + 1) == Ok('\n')) $pos + 2 else $pos + 1
						$run_start = $pos
						$run_end = $pos
					} else {
						end =
							if cdata_byte >= 0x20 and cdata_byte < 0x80 and cdata_byte != ']' {
								Utf8.skip_class(bytes, $pos + 1, cdata_class)
							} else {
								check_char(bytes, $pos)?
							}
						if $run_end != $pos {
							$text = materialize(bytes, $text, $run_start, $run_end)
							$run_start = $pos
						}
						$run_end = end
						$pos = end
					}
				}
			} else if next == '?' {
				$pos = skip_processing_instruction(bytes, $pos, namespaces)?
			} else if next == '!' {
				return fail($pos, "expected a comment or CDATA section after '<!'")
			} else {
				return Ok(TagAt($pos))
			}
		} else if byte == '&' {
			reference = parse_reference(bytes, $pos)?
			$text = append_utf8(materialize(bytes, $text, $run_start, $run_end), reference.val)
			$pos = reference.pos
			$run_start = $pos
			$run_end = $pos
		} else if byte == ']' and starts_with_at(bytes, $pos, "]]>".to_utf8()) {
			return fail($pos, "']]>' is not allowed in character data")
		} else if byte == '\r' {
			$text = materialize(bytes, $text, $run_start, $run_end).append('\n')
			$pos = if (bytes.get($pos + 1) == Ok('\n')) $pos + 2 else $pos + 1
			$run_start = $pos
			$run_end = $pos
		} else {
			# Plain ASCII found 16 bytes at a time, or one checked character.
			end =
				if byte >= 0x20 and byte < 0x80 {
					Utf8.skip_class(bytes, $pos + 1, text_class)
				} else {
					check_char(bytes, $pos)?
				}
			if $run_end != $pos {
				$text = materialize(bytes, $text, $run_start, $run_end)
				$run_start = $pos
			}
			$run_end = end
			$pos = end
		}
	}
}

# See https://www.w3.org/TR/xml/#NT-ETag and the Element Type Match
# constraint: the end tag at `at` must name the innermost open element,
# whose name is the input range `expected`.
read_end_tag : Str, List(U8), U64, { start : U64, end : U64 } -> Try({ name_end : U64, pos : U64 }, ReadFailure)
read_end_tag = |src, bytes, at, expected| {
	end =
		match name_end(bytes, at + 2) {
			Ok(pos) => pos
			Err(_) => return fail(at + 2, "expected an element name in the end tag")
		}
	len = end - (at + 2)
	if len != expected.end - expected.start or !same_bytes(bytes, at + 2, expected.start, len) {
		return fail(at, "end tag </${slice_str(src, bytes, at + 2, end)}> does not match start tag <${slice_str(src, bytes, expected.start, expected.end)}>")
	}
	close = skip_space(bytes, end)
	if bytes.get(close) != Ok('>') {
		return fail(close, "expected '>' to close the end tag")
	}
	Ok({ name_end: end, pos: close + 1 })
}

# See https://www.w3.org/TR/xml/#NT-STag. Limits are checked before the tag
# at `at` is parsed, so nothing is built for a tag that would exceed one.
check_start : Limits, U64, U64, U64 -> Try({}, ReadFailure)
check_start = |limits, depth, elements, at| {
	if depth >= limits.depth {
		Err(LimitFail({ limit: Depth, max: limits.depth, offset: at }))
	} else if elements >= limits.elements {
		Err(LimitFail({ limit: Elements, max: limits.elements, offset: at }))
	} else {
		Ok({})
	}
}

# Builds the element whose start tag was the reader's most recent event, and
# returns the cursor after its end tag. It reads content with the helpers
# `step` uses, so both check the same rules; fuzzing compares the trees they
# give. Going around the events keeps a cursor and an event from being made
# for every tag and text.
#
# The children of every open element live in one flat `$nodes` list (each
# `Open` frame remembers where its children start). It is a plain variable
# that nothing else refers to, so appending updates it in place; an
# element's children are copied out once, when its end tag is read.
build_element : Str, List(U8), Limits, Cursor, Str, List(Xml.Attribute) -> Try({ node : Xml.Node, cursor : Cursor }, ReadFailure)
build_element = |src, bytes, limits, r, name, attributes| {
	outer = r.open.drop_last(1)
	if r.pending_end {
		closed = outer.is_empty()
		return Ok({ node: Element({ name, attributes, children: [] }), cursor: { ..r, open: outer, pending_end: False, closed, start: no_start } })
	}
	var $open = []
	var $name = name
	var $attributes = attributes
	var $range = r.open.last() ?? { start: 0, end: 0 }
	var $first_child = 0
	var $nodes = []
	var $elements = r.elements
	var $pos = r.pos
	while True {
		match scan(src, bytes, limits.namespaces, $pos)? {
			TextUntil(text, pos) => {
				$nodes = $nodes.append(Text(text))
				$pos = pos
			}

			EndOfInput(pos) => return fail(pos, "expected </${slice_str(src, bytes, $range.start, $range.end)}> before the end of the document")
			TagAt(at) =>
				if bytes.get(at + 1) == Ok('/') {
					end_tag = read_end_tag(src, bytes, at, $range)?
					node = Element({ name: $name, attributes: $attributes, children: copy_from($nodes, $first_child) })
					match $open.last() {
						Err(_) => {
							closed = outer.is_empty()
							return Ok({ node, cursor: { ..r, pos: end_tag.pos, open: outer, closed, start: no_start, at, elements: $elements } })
						}

						Ok(parent) => {
							$open = $open.drop_last(1)
							$nodes = truncate($nodes, $first_child).append(node)
							$name = parent.name
							$attributes = parent.attributes
							$range = parent.range
							$first_child = parent.first_child
							$pos = end_tag.pos
						}
					}
				} else {
					check_start(limits, r.open.len() + $open.len(), $elements, at)?
					tag = parse_start_tag(src, bytes, at, limits.attributes)?
					$elements = $elements + 1
					$pos = tag.pos
					if tag.empty {
						$nodes = $nodes.append(Element({ name: tag.name, attributes: tag.attributes, children: [] }))
					} else {
						$open = $open.append({ name: $name, attributes: $attributes, range: $range, first_child: $first_child })
						$name = tag.name
						$attributes = tag.attributes
						$range = { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() }
						$first_child = $nodes.len()
					}
				}
		}
	}
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
	var $minor = 0
	$pos = $pos + 3
	while is_digit_at(bytes, $pos) {
		if $minor >= 100_000_000_000_000_000 {
			return fail(start, "the XML version number is too large")
		}
		$minor = $minor * 10 + U8.to_u64((bytes.get($pos) ?? '0') - '0')
		$pos = $pos + 1
	}
	if bytes.get($pos) != Ok(version_quote) {
		return fail($pos, "expected the XML version to end with a matching quote")
	}
	version = { major: 1, minor: $minor }
	$pos = $pos + 1

	var $encoding = Err(Missing)
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
				Ok(Utf8Encoding)
			} else {
				Ok(OtherEncoding(Str.from_utf8(name) ?? ""))
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
skip_eq : List(U8), U64 -> Try(U64, ReadFailure)
skip_eq = |bytes, start| {
	pos = skip_space(bytes, start)
	if bytes.get(pos) == Ok('=') {
		Ok(skip_space(bytes, pos + 1))
	} else {
		fail(pos, "expected '='")
	}
}

# See https://www.w3.org/TR/xml/#NT-Misc
skip_misc : List(U8), U64, Bool -> Try(U64, ReadFailure)
skip_misc = |bytes, start, namespaces| {
	var $pos = start
	var $done = False
	while !$done {
		if is_space_at(bytes, $pos) {
			$pos = $pos + 1
		} else if starts_with_at(bytes, $pos, "<!--".to_utf8()) {
			$pos = skip_comment(bytes, $pos)?
		} else if starts_with_at(bytes, $pos, "<?".to_utf8()) {
			$pos = skip_processing_instruction(bytes, $pos, namespaces)?
		} else {
			$done = True
		}
	}
	Ok($pos)
}

# See https://www.w3.org/TR/xml/#NT-Comment
skip_comment : List(U8), U64 -> Try(U64, ReadFailure)
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

# See https://www.w3.org/TR/xml/#NT-PI. With namespaces, a target may not
# contain a colon (https://www.w3.org/TR/xml-names/#Conformance).
skip_processing_instruction : List(U8), U64, Bool -> Try(U64, ReadFailure)
skip_processing_instruction = |bytes, start, namespaces| {
	target =
		match parse_name("", bytes, start + 2) {
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
	if namespaces and target.val.contains(":") {
		return fail(start + 2, "processing instruction targets may not contain ':' when namespaces are processed")
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

# The pending text as a `Str`: a slice of the input when nothing was
# rewritten, else `text` followed by the pending slice.
text_str : Str, List(U8), List(U8), U64, U64 -> Str
text_str = |src, bytes, text, run_start, run_end| {
	if text.is_empty() {
		slice_str(src, bytes, run_start, run_end)
	} else {
		Str.from_utf8(materialize(bytes, text, run_start, run_end)) ?? ""
	}
}

# `text` followed by the input slice [start, end).
materialize : List(U8), List(U8), U64, U64 -> List(U8)
materialize = |bytes, text, start, end| {
	if end > start {
		text.concat(bytes.sublist({ start, len: end - start }))
	} else {
		text
	}
}

# The input bytes [start, end) as a `Str`. With the validated input string
# this is a slice of it, checked only at its two ends; without it (invalid
# UTF-8 elsewhere in the input) the bytes are validated.
slice_str : Str, List(U8), U64, U64 -> Str
slice_str = |src, bytes, start, end| {
	if src.is_empty() {
		Str.from_utf8(bytes.sublist({ start, len: end - start })) ?? ""
	} else {
		match src.drop_first_bytes(start) {
			Ok(tail) => tail.drop_last_bytes(bytes.len() - end) ?? ""
			Err(_) => ""
		}
	}
}

# `build_element` for a tree of the caller's type: `start` and `end` make
# what an element is, and `text` what text is. The same helpers check the
# same rules; only what is built differs.
build_with : Str, List(U8), Limits, Cursor, ctx, open, (ctx, Str, List(Xml.Attribute) -> Try({ context : ctx, open : open }, err)), (Str -> node), (ctx, open, List(node) -> { context : ctx, node : node }) -> [Built({ node : node, context : ctx, cursor : Cursor }), Unread(ReadFailure), Rejected(err, U64)]
build_with = |src, bytes, limits, r, context, first, on_start, on_text, on_end| {
	outer = r.open.drop_last(1)
	if r.pending_end {
		closed = outer.is_empty()
		ended = on_end(context, first, [])
		return Built({ node: ended.node, context: ended.context, cursor: { ..r, open: outer, pending_end: False, closed, start: no_start } })
	}
	var $context = context
	var $frames = []
	var $current = first
	var $range = r.open.last() ?? { start: 0, end: 0 }
	var $first_child = 0
	var $nodes = []
	var $elements = r.elements
	var $pos = r.pos
	while True {
		scanned =
			match scan(src, bytes, limits.namespaces, $pos) {
				Ok(next) => next
				Err(failure) => return Unread(failure)
			}
		match scanned {
			TextUntil(text, pos) => {
				$nodes = $nodes.append(on_text(text))
				$pos = pos
			}

			EndOfInput(pos) => return Unread(XmlFail({ offset: pos, message: "expected </${slice_str(src, bytes, $range.start, $range.end)}> before the end of the document" }))
			TagAt(at) =>
				if bytes.get(at + 1) == Ok('/') {
					end_tag =
						match read_end_tag(src, bytes, at, $range) {
							Ok(tag) => tag
							Err(failure) => return Unread(failure)
						}
					ended = on_end($context, $current, copy_from($nodes, $first_child))
					$context = ended.context
					match $frames.last() {
						Err(_) => {
							closed = outer.is_empty()
							return Built({ node: ended.node, context: $context, cursor: { ..r, pos: end_tag.pos, open: outer, closed, start: no_start, at, elements: $elements } })
						}

						Ok(parent) => {
							$frames = $frames.drop_last(1)
							$nodes = truncate($nodes, $first_child).append(ended.node)
							$current = parent.open
							$range = parent.range
							$first_child = parent.first_child
							$pos = end_tag.pos
						}
					}
				} else {
					match check_start(limits, r.open.len() + $frames.len(), $elements, at) {
						Ok({}) => {}
						Err(failure) => return Unread(failure)
					}
					tag =
						match parse_start_tag(src, bytes, at, limits.attributes) {
							Ok(parsed) => parsed
							Err(failure) => return Unread(failure)
						}
					started =
						match on_start($context, tag.name, tag.attributes) {
							Ok(value) => value
							Err(error) => return Rejected(error, at)
						}
					$elements = $elements + 1
					$pos = tag.pos
					if tag.empty {
						ended = on_end(started.context, started.open, [])
						$context = ended.context
						$nodes = $nodes.append(ended.node)
					} else {
						$context = started.context
						$frames = $frames.append({ open: $current, range: $range, first_child: $first_child })
						$current = started.open
						$range = { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() }
						$first_child = $nodes.len()
					}
				}
		}
	}
}

## A fresh copy of the nodes from `start` on, so the shared list stays
## uniquely referenced.
copy_from : List(a), U64 -> List(a)
copy_from = |nodes, start| {
	var $out = List.with_capacity(nodes.len() - start)
	var $index = start
	while $index < nodes.len() {
		match nodes.get($index) {
			Ok(node) => {
				$out = $out.append(node)
			}

			Err(_) => {}
		}
		$index = $index + 1
	}
	$out
}

## Drop the nodes from `len` on, one at a time from the end so the list
## keeps its allocation.
truncate : List(a), U64 -> List(a)
truncate = |nodes, len| {
	var $out = nodes
	while $out.len() > len {
		$out = $out.drop_last(1)
	}
	$out
}

# See https://www.w3.org/TR/xml/#NT-STag and https://www.w3.org/TR/xml/#NT-EmptyElemTag
parse_start_tag : Str, List(U8), U64, U64 -> Try(StartTag, ReadFailure)
parse_start_tag = |src, bytes, start, max_attributes| {
	name =
		match parse_name(src, bytes, start + 1) {
			Ok(parsed) => parsed
			Err(_) => return fail(start + 1, "expected an element name after '<'")
		}
	var $attributes = []
	# The Unique Att Spec check scans the few attributes most elements have,
	# and switches to a set once there are many, so it stays linear without
	# hashing every name of every element.
	var $seen = Set.empty()
	var $pos = name.pos
	while True {
		before_space = $pos
		$pos = skip_space(bytes, $pos)
		if bytes.get($pos) == Ok('>') {
			return Ok({ name: name.val, attributes: $attributes, empty: False, pos: $pos + 1 })
		}
		if bytes.get($pos) == Ok('/') and bytes.get($pos + 1) == Ok('>') {
			return Ok({ name: name.val, attributes: $attributes, empty: True, pos: $pos + 2 })
		}
		if $pos >= bytes.len() {
			return fail($pos, "expected '>' to close the start tag <${name.val}>")
		}
		if $pos == before_space {
			return fail($pos, "expected whitespace, '>' or '/>' in the start tag <${name.val}>")
		}
		attribute_name =
			match parse_name(src, bytes, $pos) {
				Ok(parsed) => parsed
				Err(_) => return fail($pos, "expected an attribute name, '>' or '/>' in the start tag <${name.val}>")
			}
		if $attributes.len() >= max_attributes {
			return Err(LimitFail({ limit: Attributes, max: max_attributes, offset: $pos }))
		}
		duplicate =
			if $attributes.len() <= small_attribute_count {
				$attributes.any(|attribute| attribute.name == attribute_name.val)
			} else {
				$seen.contains(attribute_name.val)
			}
		if duplicate {
			return fail($pos, "duplicate attribute ${attribute_name.val}")
		}
		$pos = skip_eq(bytes, attribute_name.pos)?
		value = parse_attribute_value(src, bytes, $pos)?
		$attributes = $attributes.append({ name: attribute_name.val, value: value.val })
		if $attributes.len() > small_attribute_count {
			$seen =
				if $seen.is_empty() {
					Set.from_list($attributes.map(|attribute| attribute.name))
				} else {
					$seen.insert(attribute_name.val)
				}
		}
		$pos = value.pos
	}
}

# Up to this many attributes, duplicates are found by scanning the list.
small_attribute_count : U64
small_attribute_count = 16

# See https://www.w3.org/TR/xml/#NT-AttValue and https://www.w3.org/TR/xml/#AVNormalize
#
# The value is `$value` followed by the input slice [$run_start, $run_end),
# as for character data: bytes kept as they are extend the slice, and a
# reference or normalised whitespace copies it once.
parse_attribute_value : Str, List(U8), U64 -> Parsed(Str)
parse_attribute_value = |src, bytes, start| {
	quote = bytes.get(start) ?? 0
	if quote != '"' and quote != '\'' {
		return fail(start, "attribute values must be quoted")
	}
	plain = if (quote == '"') double_quoted_class else single_quoted_class
	# Most values are plain ASCII up to the closing quote: find it 16 bytes
	# at a time and return a slice of the input.
	plain_end = Utf8.skip_class(bytes, start + 1, plain)
	if bytes.get(plain_end) == Ok(quote) {
		return Ok({ val: slice_str(src, bytes, start + 1, plain_end), pos: plain_end + 1 })
	}
	var $value = []
	var $run_start = start + 1
	var $run_end = plain_end
	var $pos = plain_end
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == quote {
			return Ok({ val: text_str(src, bytes, $value, $run_start, $run_end), pos: $pos + 1 })
		} else if byte == '<' {
			return fail($pos, "'<' is not allowed in attribute values")
		} else if byte == '&' {
			reference = parse_reference(bytes, $pos)?
			$value = append_utf8(materialize(bytes, $value, $run_start, $run_end), reference.val)
			$pos = reference.pos
			$run_start = $pos
			$run_end = $pos
		} else if byte == '\r' or byte == '\n' or byte == '\t' {
			$value = materialize(bytes, $value, $run_start, $run_end).append(' ')
			$pos = if (byte == '\r' and bytes.get($pos + 1) == Ok('\n')) $pos + 2 else $pos + 1
			$run_start = $pos
			$run_end = $pos
		} else {
			$pos =
				if byte >= 0x20 and byte < 0x80 {
					Utf8.skip_class(bytes, $pos + 1, plain)
				} else {
					check_char(bytes, $pos)?
				}
			$run_end = $pos
		}
	}
	fail(start, "unterminated attribute value")
}

# See https://www.w3.org/TR/xml/#NT-Reference
#
# Returns the code point the reference stands for, so the caller can append
# its bytes without building a list for each reference.
parse_reference : List(U8), U64 -> Parsed(U32)
parse_reference = |bytes, start| {
	if bytes.get(start + 1) == Ok('#') and bytes.get(start + 2) == Ok('x') {
		parse_char_reference(bytes, start, start + 3, 16)
	} else if bytes.get(start + 1) == Ok('#') {
		parse_char_reference(bytes, start, start + 2, 10)
	} else {
		name =
			match parse_name("", bytes, start + 1) {
				Ok(parsed) => parsed
				Err(_) => return fail(start, "'&' must start a reference; write &amp; for a literal ampersand")
			}
		if bytes.get(name.pos) != Ok(';') {
			return fail(name.pos, "expected ';' to end the entity reference")
		}
		replacement =
			match name.val {
				"lt" => '<'
				"gt" => '>'
				"amp" => '&'
				"apos" => '\''
				"quot" => '"'
				other => return fail(start, "undeclared entity &${other};")
			}
		Ok({ val: replacement, pos: name.pos + 1 })
	}
}

# See https://www.w3.org/TR/xml/#NT-CharRef
parse_char_reference : List(U8), U64, U64, U32 -> Parsed(U32)
parse_char_reference = |bytes, start, digits_start, base| {
	var $code = 0
	var $pos = digits_start
	var $done = False
	while !$done {
		digit = digit_value(bytes.get($pos) ?? 0)
		if digit < base {
			next_code = $code * base + digit
			$code = if (next_code > 0x110000) 0x110000 else next_code
			$pos = $pos + 1
		} else {
			$done = True
		}
	}
	if $pos == digits_start {
		return fail(digits_start, if (base == 16) "expected hexadecimal digits in the character reference" else "expected decimal digits in the character reference")
	}
	if bytes.get($pos) != Ok(';') {
		return fail($pos, "expected ';' to end the character reference")
	}
	if !is_xml_char($code) {
		return fail(start, "character reference to a character that is not allowed in XML")
	}
	Ok({ val: $code, pos: $pos + 1 })
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
parse_name : Str, List(U8), U64 -> Try({ val : Str, pos : U64 }, [NotAName])
parse_name = |src, bytes, start| {
	end = name_end(bytes, start)?
	Ok({ val: slice_str(src, bytes, start, end), pos: end })
}

# The end of the name starting at `start`, without building it.
name_end : List(U8), U64 -> Try(U64, [NotAName])
name_end = |bytes, start| {
	first = decode_scalar(bytes, start) ?? { code: 0, len: 0 }
	if !is_name_start_char(first.code) {
		return Err(NotAName)
	}
	var $pos = Utf8.skip_class(bytes, start + first.len, ascii_name_class)
	var $done = (bytes.get($pos) ?? 0) < 0x80
	while !$done {
		scalar = decode_scalar(bytes, $pos) ?? { code: 0, len: 0 }
		if is_name_char(scalar.code) {
			$pos = $pos + scalar.len
		} else {
			$done = True
		}
	}
	Ok($pos)
}

# Whether the input bytes at `a` and `b` agree for `len` bytes.
same_bytes : List(U8), U64, U64, U64 -> Bool
same_bytes = |bytes, a, b, len| {
	var $index = 0
	while $index < len {
		if bytes.get(a + $index) != bytes.get(b + $index) {
			return False
		}
		$index = $index + 1
	}
	True
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
check_char : List(U8), U64 -> Try(U64, ReadFailure)
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
		if (byte >= 0x80 and byte < 0xC0) Ok(U8.to_u32(byte - 0x80)) else Err(InvalidUtf8)
	}
	if b0 < 0x80 {
		Ok({ code: b0, len: 1 })
	} else if b0 >= 0xC2 and b0 <= 0xDF {
		Ok({ code: (b0 - 0xC0) * 64 + cont(1)?, len: 2 })
	} else if b0 >= 0xE0 and b0 <= 0xEF {
		code = (b0 - 0xE0) * 4096 + cont(1)? * 64 + cont(2)?
		if (code < 0x800 or (code >= 0xD800 and code <= 0xDFFF)) Err(InvalidUtf8) else Ok({ code, len: 3 })
	} else if b0 >= 0xF0 and b0 <= 0xF4 {
		code = (b0 - 0xF0) * 262144 + cont(1)? * 4096 + cont(2)? * 64 + cont(3)?
		if (code < 0x10000 or code > 0x10FFFF) Err(InvalidUtf8) else Ok({ code, len: 4 })
	} else {
		Err(InvalidUtf8)
	}
}

## `out` with the UTF-8 encoding of `code` appended.
append_utf8 : List(U8), U32 -> List(U8)
append_utf8 = |out, code| {
	if code < 0x80 {
		out.append(U32.to_u8_wrap(code))
	} else if code < 0x800 {
		out.append(U32.to_u8_wrap(0xC0 + code // 64)).append(U32.to_u8_wrap(0x80 + code % 64))
	} else if code < 0x10000 {
		out.append(U32.to_u8_wrap(0xE0 + code // 4096)).append(U32.to_u8_wrap(0x80 + (code // 64) % 64)).append(U32.to_u8_wrap(0x80 + code % 64))
	} else {
		out.append(U32.to_u8_wrap(0xF0 + code // 262144)).append(U32.to_u8_wrap(0x80 + (code // 4096) % 64)).append(U32.to_u8_wrap(0x80 + (code // 64) % 64)).append(U32.to_u8_wrap(0x80 + code % 64))
	}
}

# Character data that needs no handling: tab, line feed, and printable
# ASCII but '<', '&' and ']' (which may start "]]>").
text_class : Utf8.ByteClass
text_class = Utf8.ByteClass.from_predicate(|b| b == '\t' or b == '\n' or (b >= 0x20 and b < 0x80 and b != '<' and b != '&' and b != ']'))

# CDATA section content that needs no handling: tab, line feed, and
# printable ASCII but ']'
# (which may start "]]>").
cdata_class : Utf8.ByteClass
cdata_class = Utf8.ByteClass.from_predicate(|b| b == '\t' or b == '\n' or (b >= 0x20 and b < 0x80 and b != ']'))

# Attribute value bytes that need no handling, inside each kind of quote.
double_quoted_class : Utf8.ByteClass
double_quoted_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '<' and b != '&' and b != '"')

single_quoted_class : Utf8.ByteClass
single_quoted_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '<' and b != '&' and b != '\'')

# ASCII name characters: a run of these needs no UTF-8 decoding.
ascii_name_class : Utf8.ByteClass
ascii_name_class = Utf8.ByteClass.from_predicate(|b| (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') or (b >= '0' and b <= '9') or b == ':' or b == '_' or b == '-' or b == '.')

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
		Err(_) => False
	}
}

is_digit_at : List(U8), U64 -> Bool
is_digit_at = |bytes, pos| {
	match bytes.get(pos) {
		Ok(byte) => byte >= '0' and byte <= '9'
		Err(_) => False
	}
}

is_alphabetical : U8 -> Bool
is_alphabetical = |c| (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z')

# See https://www.w3.org/TR/xml/#NT-EncName
is_encoding_char : U8 -> Bool
is_encoding_char = |c| is_alphabetical(c) or (c >= '0' and c <= '9') or c == '.' or c == '_' or c == '-'

ascii_lowercase : List(U8) -> List(U8)
ascii_lowercase = |bytes| bytes.map(|c| if (c >= 'A' and c <= 'Z') c + 32 else c)

v1_0 : Xml.Version
v1_0 = { major: 1, minor: 0 }

element : Str, List(Xml.Attribute), List(Xml.Node) -> Xml.Node
element = |name, attributes, children| Element({ name, attributes, children })

root_of : Str -> Try(Xml.Node, [InvalidXml(Xml.Error)])
root_of = |input| Xml.parse_str(input).map_ok(|xml| xml.root)

error_at : Str -> Try({ line : U64, column : U64 }, [Parsed])
error_at = |input| {
	match Xml.parse_str(input) {
		Ok(_) => Err(Parsed)
		Err(InvalidXml(error)) => Ok({ line: error.line, column: error.column })
	}
}

test_xml =
	\\<?xml version=\"1.0\" encoding=\"utf-8\"?>
	\\<root>
	\\    <element arg=\"value\" />
	\\</root>

# Full XML parsing captures the declaration and root element.
expect {
	result = Utf8.parse_str(Xml.parser, test_xml)?

	result
		== {
			declaration: Ok({
				version: v1_0,
				encoding: Ok(Utf8Encoding),
			}),
			root: element(
				"root",
				[],
				[
					Text("\n    "),
					element("element", [{ name: "arg", value: "value" }], []),
					Text("\n"),
				],
			),
		}
}

# XML parsing accepts documents without a prolog.
expect {
	result = Utf8.parse_str(Xml.parser, "<element />")?
	result == { declaration: Err(Missing), root: element("element", [], []) }
}

# A declaration without an encoding reports it as missing, and versions
# keep every digit after the dot.
expect Xml.parse_str("<?xml version='1.10'?><a/>").map_ok(|xml| xml.declaration) == Ok(Ok({ version: { major: 1, minor: 10 }, encoding: Err(Missing) }))
expect Xml.parse_str("<?xml version='1.0' encoding='latin1'?><a/>").map_ok(|xml| xml.declaration) == Ok(Ok({ version: v1_0, encoding: Ok(OtherEncoding("latin1")) }))
expect error_at("<?xml version='1.99999999999999999999'?><a/>") == Ok({ line: 1, column: 1 })

# Empty elements can omit whitespace before the self-closing marker.
expect root_of("<element/>") == Ok(element("element", [], []))

# Empty elements can carry attributes.
expect root_of("<element arg=\"value\"/>") == Ok(element("element", [{ name: "arg", value: "value" }], []))

# Explicit start and end tags can represent an empty element.
expect root_of("<element></element>") == Ok(element("element", [], []))

# Elements can parse multiple attributes and text content.
expect {
	result = root_of("<element firstArg=\"one\" secondArg='two'>text content</element>")
	result
		== Ok(
			element(
				"element",
				[
					{ name: "firstArg", value: "one" },
					{ name: "secondArg", value: "two" },
				],
				[Text("text content")],
			),
		)
}

# CDATA sections parse into text nodes.
expect root_of("<element><![CDATA[<literal />]]></element>") == Ok(element("element", [], [Text("<literal />")]))

# Partial CDATA closing text is preserved until the real close marker.
expect root_of("<element><![CDATA[this is ]] not ]> the end]]></element>") == Ok(element("element", [], [Text("this is ]] not ]> the end")]))

# Nested elements preserve attributes on parent and child nodes.
expect {
	result = root_of("<parent argParent=\"outer\"><child argChild=\"inner\" /></parent>")
	result == Ok(element("parent", [{ name: "argParent", value: "outer" }], [element("child", [{ name: "argChild", value: "inner" }], [])]))
}

# Nested element parsing preserves whitespace text nodes.
expect root_of("<parent>\n    <child />\n</parent>") == Ok(element("parent", [], [Text("\n    "), element("child", [], []), Text("\n")]))

# Siblings and nested children end up under the right parents.
expect root_of("<a>1<b>2<c/>3</b>4<d><e>5</e></d>6</a>") == Ok(element("a", [], [Text("1"), element("b", [], [Text("2"), element("c", [], []), Text("3")]), Text("4"), element("d", [], [element("e", [], [Text("5")])]), Text("6")]))

# Elements can parse a diverse set of child nodes.
expect {
	result = root_of(
		\\<feed xmlns="http://www.w3.org/2005/Atom">
		\\    <title>Atom Feed</title>
		\\    <link rel="self" type="application/atom+xml" href="http://example.org" />
		\\    <updated>2024-02-23T20:38:24Z</updated>
		\\</feed>
	)

	result
		== Ok(
			element(
				"feed",
				[{ name: "xmlns", value: "http://www.w3.org/2005/Atom" }],
				[
					Text("\n    "),
					element("title", [], [Text("Atom Feed")]),
					Text("\n    "),
					element(
						"link",
						[
							{ name: "rel", value: "self" },
							{ name: "type", value: "application/atom+xml" },
							{ name: "href", value: "http://example.org" },
						],
						[],
					),
					Text("\n    "),
					element("updated", [], [Text("2024-02-23T20:38:24Z")]),
					Text("\n"),
				],
			),
		)
}

# Full XML parsing ignores trailing whitespace after the root, and matches
# encoding names case-insensitively (XML 1.0 section 4.3.3).
expect {
	result = Xml.parse_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<root><Example></Example></root>\n")
	result
		== Ok({
			declaration: Ok({ version: v1_0, encoding: Ok(Utf8Encoding) }),
			root: element("root", [], [element("Example", [], [])]),
		})
}

# Malformed input ending in a multibyte scalar returns an error instead of crashing
# while rendering a parser failure from a mid-scalar byte position.
expect Utf8.parse_str(Xml.parser, "<ӿ").is_err()

# End tags must match their start tag (WFC: Element Type Match).
expect error_at("<a><b></a></b>") == Ok({ line: 1, column: 7 })

# Names may contain digits and non-ASCII letters.
expect root_of("<h1 données-2='x'>é</h1>") == Ok(element("h1", [{ name: "données-2", value: "x" }], [Text("é")]))

# Predefined entity and character references are replaced in text and attributes.
expect root_of("<a t='&lt;&#65;&#x1F600;&quot;'>&amp;&gt;&apos;</a>") == Ok(element("a", [{ name: "t", value: "<A😀\"" }], [Text("&>'")]))

# Undeclared entities, bare ampersands, and references to illegal characters are errors.
expect error_at("<a>&nbsp;</a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>AT&T</a>") == Ok({ line: 1, column: 8 })
expect error_at("<a>&#0;</a>").is_ok()
expect error_at("<a>&#xD800;</a>").is_ok()
expect error_at("<a>&#99999999999999999999;</a>").is_ok()

# '<' is not allowed in attribute values, and attributes need quotes,
# separating whitespace, and unique names.
expect error_at("<a b='<'/>") == Ok({ line: 1, column: 7 })
expect error_at("<a b=c/>") == Ok({ line: 1, column: 6 })
expect error_at("<a b='1'c='2'/>") == Ok({ line: 1, column: 9 })
expect error_at("<a b='1' b='2'/>") == Ok({ line: 1, column: 10 })

# Attribute values are normalised: literal whitespace becomes a space, while
# character references keep the character they name.
expect root_of("<a b='x\r\ny\tz\n&#10;&#9;'/>") == Ok(element("a", [{ name: "b", value: "x y z \n\t" }], []))

# Line endings in content are normalised to LF (XML 1.0 section 2.11).
expect root_of("<a>1\r\n2\r3<![CDATA[\r\n]]></a>") == Ok(element("a", [], [Text("1\n2\n3\n")]))

# Comments and processing instructions are accepted everywhere Misc is
# allowed, and text on both sides of them is merged.
expect {
	result = Xml.parse_str("<!-- c --><?pi data?>\n<a>x<!-- y -->z<?p?><![CDATA[w]]>&amp;</a><!---->\n<?q ?>")
	result.map_ok(|xml| xml.root) == Ok(element("a", [], [Text("xzw&")]))
}

# Comments may not contain '--', and PI targets may not be 'xml'.
expect error_at("<a><!-- a -- b --></a>") == Ok({ line: 1, column: 11 })
expect error_at("<a><!-- a ---></a>") == Ok({ line: 1, column: 11 })
expect error_at("<a/>\n<?xml version='1.0'?>") == Ok({ line: 2, column: 1 })
expect error_at("<a><?XmL x?></a>") == Ok({ line: 1, column: 6 })

# ']]>' is not allowed in character data, and control characters are not XML characters.
expect error_at("<a>]]></a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>\u(1)</a>") == Ok({ line: 1, column: 4 })
expect error_at("<a>\u(FFFE)</a>") == Ok({ line: 1, column: 4 })

# Exactly one root element is required.
expect error_at("") == Ok({ line: 1, column: 1 })
expect error_at("<a/><b/>") == Ok({ line: 1, column: 5 })
expect error_at("<a/>text") == Ok({ line: 1, column: 5 })
expect error_at("<a>") == Ok({ line: 1, column: 4 })

# Document type declarations are outside the supported subset.
expect error_at("<!DOCTYPE a><a/>") == Ok({ line: 1, column: 1 })

# The XML declaration accepts standalone, requires whitespace between its
# parts, and must be first.
expect Xml.parse_str("<?xml version='1.0' standalone='yes'?><a/>").is_ok()
expect error_at("<?xml version='1.0'encoding='UTF-8'?><a/>") == Ok({ line: 1, column: 20 })
expect error_at(" <?xml version='1.0'?><a/>") == Ok({ line: 1, column: 2 })
expect error_at("<?xml version='2.0'?><a/>") == Ok({ line: 1, column: 16 })

# A byte order mark may start the document.
expect Xml.parse_str("\u(FEFF)<a/>").is_ok()

# Error lines count CRLF, CR, and LF line endings.
expect error_at("<a>\r\n\r\n<b>\r</a>") == Ok({ line: 4, column: 1 })

# Deep nesting does not exhaust the call stack, in the parser or in `text`.
expect {
	depth = 20000
	input = Str.concat(Str.repeat("<a>", depth), Str.concat("x", Str.repeat("</a>", depth)))
	Xml.parse_str(input).map_ok(|xml| xml.root.text()) == Ok("x")
}

# Many siblings are parsed in linear time (each append updates in place).
expect {
	count = 50000
	parsed = Xml.parse_str("<r>${Str.repeat("<i>x</i>", count)}</r>")
	match parsed {
		Ok(xml) => xml.root.children_named("i").len() == count
		Err(_) => False
	}
}

# The parser combinator reports leftover input after the document.
expect Utf8.parse_str(Xml.parser, "<a/> <b/>") == Err(ParseError({ message: "unexpected input", offset: 5 }))

# The parser combinator reports structured failures with a byte offset.
expect Utf8.parse_str(Xml.parser, "<a></b>") == Err(ParseError({ message: "end tag </b> does not match start tag <a>", offset: 3 }))

# The parser leaves the input after the document for the next parser.
expect Utf8.parse_str_partial(Xml.parser, "<a/>\n<!-- c -->rest").map_ok(|{ value: _, rest }| rest) == Ok("rest")

# Duplicate attribute detection stays fast with many attributes.
expect {
	count = 20000
	attributes = Str.join_with(List.repeat(0, count).map_with_index(|_, index| " a${index.to_str()}=''"), "")
	parsed = Xml.parse_str("<e${attributes}/>")
	duplicate = Xml.parse_str("<e${attributes} a${(count - 1).to_str()}=''/>")
	parsed.is_ok() and duplicate.is_err()
}

# Node helpers: name, attribute, children_named and text.
expect {
	match Xml.parse_str("<p id='x'>Hi <b>there</b><b/>!</p>") {
		Ok(xml) => {
			root = xml.root
			leaf : Xml.Node
			leaf = Text("t")
			root.name() == Ok("p")
				and root.attribute("id") == Ok("x")
				and root.attribute("class") == Err(Missing)
				and root.children_named("b").len() == 2
				and root.children_named("i") == []
				and root.text() == "Hi there!"
				and leaf.name() == Err(NotAnElement)
				and leaf.attribute("id") == Err(Missing)
				and leaf.children_named("b") == []
				and leaf.text() == "t"
		}

		Err(_) => False
	}
}

# Equal trees hash equally, so documents work as Set members and Dict keys.
expect {
	one = Xml.parse_str("<a b='1'>x</a>")
	two = Xml.parse_str("<a b='1'>x</a>")
	match (one, two) {
		(Ok(first), Ok(second)) => Set.from_list([first, second]).len() == 1 and Set.from_list([first.root, second.root, Text("y")]).len() == 2
		_ => False
	}
}

# Doc examples: module header.
expect {
	match Xml.parse_str("<feed><title>News</title><link href='/a'/></feed>") {
		Ok(xml) => {
			titles = xml.root.children_named("title").map(|title| title.text())
			links = xml.root.children_named("link").map(|link| link.attribute("href"))
			titles == ["News"] and links == [Ok("/a")]
		}

		Err(_) => False
	}
}

# Doc examples: Node helpers.
expect Xml.parse_str("<svg:rect/>").map_ok(|xml| xml.root.name()) == Ok(Ok("svg:rect"))
expect {
	match Xml.parse_str("<a href='/x'/>") {
		Ok(xml) => xml.root.attribute("href") == Ok("/x") and xml.root.attribute("title") == Err(Missing)
		Err(_) => False
	}
}
expect {
	match Xml.parse_str("<list><item>1</item><other/><item>2</item></list>") {
		Ok(xml) => xml.root.children_named("item").map(|item| item.text()) == ["1", "2"]
		Err(_) => False
	}
}
expect Xml.parse_str("<p>Hello, <b>world</b>!</p>").map_ok(|xml| xml.root.text()) == Ok("Hello, world!")

# Doc examples: parse_str and parser.
expect Xml.parse_str("<a>x &amp; y</a>").map_ok(|xml| xml.root) == Ok(Element({ name: "a", attributes: [], children: [Text("x & y")] }))
expect Xml.parse_str("<a>&nbsp;</a>") == Err(InvalidXml({ line: 1, column: 4, message: "undeclared entity &nbsp;" }))

# Quotation validation keeps native XML failures ahead of lint failures.
expect {
	xml : Xml
	xml = "<root><child>quoted</child></root>"
	xml.root.text() == "quoted"
}
expect match Xml.validate({ source: "<?xml version='1.0' encoding='latin1'?><a/>", name: "feed.xml" }) {
	Err(BadQuotedBytes(message)) => message.contains("feed.xml:1: XML: non_utf8_declaration")
	Ok(_) => False
}
expect Xml.validate({ source: "<?xml version='1.0' encoding='latin1'?><a/>", lints: { non_utf8_declaration: False } }).is_ok()
expect match Xml.validate({ source: "<?xml version='1.0' encoding='latin1'?><a></b>" }) {
	Err(BadQuotedBytes(message)) => !message.contains("non_utf8_declaration") and message.contains("XML")
	Ok(_) => False
}

# All events of a document, or the first failure.
events_of : Str, Xml.ReaderProps -> Try(List(Xml.Event), Xml.ReadError)
events_of = |input, props| {
	var $reader = Xml.reader(input, props)?
	var $events = []
	while True {
		match $reader.next() {
			Event(event, rest) => {
				$reader = rest
				$events = $events.append(event)
			}

			Done => return Ok($events)
			Failed(error) => return Err(error)
		}
	}
}

is_event : [Event(Xml.Event, Xml.Reader), Done, Failed(Xml.ReadError)] -> Bool
is_event = |result| match result {
	Event(_, _) => True
	_ => False
}

is_done : [Event(Xml.Event, Xml.Reader), Done, Failed(Xml.ReadError)] -> Bool
is_done = |result| match result {
	Done => True
	_ => False
}

not_after_start : [Element(Xml.Node, Xml.Reader), Failed(Xml.ReadError), NotAfterStart] -> Bool
not_after_start = |result| match result {
	NotAfterStart => True
	_ => False
}

# The tree built from a document's events, as a consumer of the reader would.
tree_of_events : List(Xml.Event) -> Try(Xml.Node, [NoRoot])
tree_of_events = |events| {
	var $stack = []
	var $current = { name: "", attributes: [], children: [] }
	for event in events {
		match event {
			Start(tag) => {
				$stack = $stack.append($current)
				$current = { name: tag.name, attributes: tag.attributes, children: [] }
			}

			Text(text) => {
				$current = { ..$current, children: $current.children.append(Text(text)) }
			}

			End(_) => {
				node = Element($current)
				parent = $stack.last() ?? { name: "", attributes: [], children: [] }
				$stack = $stack.drop_last(1)
				$current = { ..parent, children: parent.children.append(node) }
			}
		}
	}
	$current.children.first().map_err(|_| NoRoot)
}

# Events match the tree: an empty-element tag is a start and an end, and
# text, references and CDATA are merged across comments and processing
# instructions.
expect events_of("<a x='1'>t<b/>&amp;<![CDATA[c]]><!--x--><?p?>d</a>", {}) == Ok([Start({ name: "a", attributes: [{ name: "x", value: "1" }] }), Text("t"), Start({ name: "b", attributes: [] }), End("b"), Text("&cd"), End("a")])
expect events_of("<?xml version='1.0'?>\n<!-- c -->\n<a/>\n<?pi?>\n", {}) == Ok([Start({ name: "a", attributes: [] }), End("a")])

# Building a tree from the events gives the tree parse_str builds.
expect {
	inputs = ["<a>1<b>2<c/>3</b>4<d><e>5</e></d>6</a>", "<p id='x'>Hi <b>there</b><b/>!</p>", "<a>1\r\n2\r3<![CDATA[\r\n]]>&#x1F600;</a>", "<r>\n  <i k='a&amp;b'/>\n</r>"]
	inputs.all(|input| events_of(input, {}).map_ok(tree_of_events) == Xml.parse_str(input).map_ok(|xml| Ok(xml.root)))
}

# Failures match parse_str's, including content after the root element.
expect events_of("<a></b>", {}) == Err(InvalidXml({ line: 1, column: 4, message: "end tag </b> does not match start tag <a>" }))
expect events_of("<a/><b/>", {}) == Err(InvalidXml({ line: 1, column: 5, message: "unexpected content after the root element" }))
expect events_of("<a>x", {}) == Err(InvalidXml({ line: 1, column: 5, message: "expected </a> before the end of the document" }))

# `element` builds the subtree directly after a start, then reading
# continues after its end tag; anywhere else it reports NotAfterStart.
expect {
	match Xml.reader("<doc><p>one<b>!</b></p><p>two</p></doc>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(_), after_doc) =>
					match (after_doc.element(), after_doc.next()) {
						(Element(doc, _), Event(Start(_), after_p)) =>
							match after_p.element() {
								Element(p, rest) => doc.children_named("p").len() == 2 and p == Element({ name: "p", attributes: [], children: [Text("one"), Element({ name: "b", attributes: [], children: [Text("!")] })] }) and is_event(rest.next()) and not_after_start(rest.element())
								_ => False
							}

						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}
expect {
	match Xml.reader("<a/>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(_), rest) =>
					match rest.element() {
						Element(node, after) => node == Element({ name: "a", attributes: [], children: [] }) and is_done(after.next())
						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}

# The location of the most recent event.
expect {
	match Xml.reader("<a>\n  <b/></a>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(_, after_a) =>
					match after_a.next() {
						Event(Text(_), after_text) =>
							match after_text.next() {
								Event(Start(_), after_b) => after_a.location() == { line: 1, column: 1 } and after_text.location() == { line: 1, column: 4 } and after_b.location() == { line: 2, column: 3 }
								_ => False
							}

						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}

# Each limit admits a document exactly at it and rejects one past it, at
# the start tag or attribute that would exceed it.
expect Xml.parse("<a><b><c/></b></a>", { max_depth: 3 }).is_ok()
expect Xml.parse("<a><b><c/></b></a>", { max_depth: 2 }) == Err(LimitExceeded({ limit: Depth, max: 2, line: 1, column: 7 }))
expect Xml.parse("<a><b/>\n<c/></a>", { max_elements: 3 }).is_ok()
expect Xml.parse("<a><b/>\n<c/></a>", { max_elements: 2 }) == Err(LimitExceeded({ limit: Elements, max: 2, line: 2, column: 1 }))
expect Xml.parse("<a x='1' y='2'/>", { max_attributes: 2 }).is_ok()
expect Xml.parse("<a x='1' y='2'/>", { max_attributes: 1 }) == Err(LimitExceeded({ limit: Attributes, max: 1, line: 1, column: 10 }))
expect events_of("<a><b/></a>", { max_depth: 1 }) == Err(LimitExceeded({ limit: Depth, max: 1, line: 1, column: 4 }))
expect Xml.reader("<a/>", { max_elements: 0 }).is_ok() and events_of("<a/>", { max_elements: 0 }) == Err(LimitExceeded({ limit: Elements, max: 0, line: 1, column: 1 }))

# The default limits admit the deep document parse_str reads; a lower one
# stops it at the first start tag past the limit.
expect {
	depth = 20000
	input = Str.concat(Str.repeat("<a>", depth), Str.concat("x", Str.repeat("</a>", depth)))
	Xml.parse(input, { max_depth: depth }).map_ok(|xml| xml.root.text()) == Ok("x") and Xml.parse(input, {}) == Err(LimitExceeded({ limit: Depth, max: 1024, line: 1, column: 3073 }))
}

# With namespaces, processing instruction targets may not contain a colon.
expect Xml.parse("<a><?x:y?></a>", {}).is_ok()
expect Xml.parse("<a><?x:y?></a>", { namespaces: True }) == Err(InvalidXml({ line: 1, column: 6, message: "processing instruction targets may not contain ':' when namespaces are processed" }))

# Text after a rewrite keeps growing in place: these take linear time.
expect {
	count = 100000
	text = Xml.parse_str("<a>&amp;${Str.repeat("é", count)}<![CDATA[${Str.repeat("x]", count)}]]>${Str.repeat("&lt;b", count)}\r\n</a>").map_ok(|xml| xml.root.text().count_utf8_bytes())
	value = Xml.parse_str("<a v='&amp;${Str.repeat("é\t", count)}'/>").map_ok(|xml| xml.root.attribute("v").map_ok(|v| v.count_utf8_bytes()))
	text == Ok(1 + 2 * count + 2 * count + 2 * count + 1) and value == Ok(Ok(1 + 3 * count))
}

# Doc examples: parse, Reader and reader.
expect Xml.parse("<a><b/></a>", { max_depth: 1 }) == Err(LimitExceeded({ limit: Depth, max: 1, line: 1, column: 4 }))
expect Xml.parse("<a><b/></a>", {}).is_ok()
expect {
	count_items = |reader| {
		var $reader = reader
		var $count = 0
		while True {
			match $reader.next() {
				Event(Start({ name: "item", .. }), rest) => {
					$reader = rest
					$count = $count + 1
				}

				Event(_, rest) => {
					$reader = rest
				}

				Done => return Ok($count)
				Failed(error) => return Err(error)
			}
		}
	}
	match Xml.reader("<list><item/><item>x</item></list>", {}) {
		Ok(reader) => count_items(reader) == Ok(2)
		Err(_) => False
	}
}
expect {
	match Xml.reader("<a>x</a>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start({ name, .. }), rest) =>
					match rest.next() {
						Event(Text(text), _) => name == "a" and text == "x"
						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}
expect {
	match Xml.reader("<doc><p>one</p><p>two</p></doc>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(_), after_doc) =>
					match after_doc.next() {
						Event(Start(_), after_p) =>
							match after_p.element() {
								Element(p, _) => p.text() == "one"
								_ => False
							}

						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}
expect Xml.reader("<?xml version='1.0'?><a/>", {}).map_ok(|reader| reader.declaration().is_ok()) == Ok(True)
expect Xml.reader("text", {}).map_ok(|_| {}) == Err(InvalidXml({ line: 1, column: 1, message: "expected a root element, but found text" }))

# Doc examples: element_with.
expect {
	match Xml.reader("<doc><p>one</p><p>two<b/></p></doc>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(_), rest) =>
					match rest.element_with({ context: 0, first: "doc", start: |count, name, _| Ok({ context: count + 1, open: name }), text: |text| text, end: |count, name, children| { context: count, node: "${name}(${Str.join_with(children, ",")})" } }) {
						Element(tree, count, _) => tree == "doc(p(one),p(two,b()))" and count == 3
						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}

# A rejecting start fails at its start tag.
expect {
	match Xml.reader("<doc>\n  <ok/><no/></doc>", {}) {
		Ok(reader) =>
			match reader.next() {
				Event(Start(_), rest) =>
					match rest.element_with({ context: {}, first: {}, start: |_, name, _| if (name == "no") Err(Refused) else Ok({ context: {}, open: {} }), text: |_| {}, end: |_, _, _| { context: {}, node: {} } }) {
						Rejected(Refused, at) => at == { line: 2, column: 8 }
						_ => False
					}

				_ => False
			}

		Err(_) => False
	}
}
