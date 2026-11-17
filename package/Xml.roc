import XmlTypes
import Streaming
import Parser
import Utf8
import MarkupScan
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
		xml = Xml.parse_str(props.source).map_err(|InvalidXml(error)| Quotation.error({ name: props.name, format: "XML", message: Str.inspect(error.problem), line: error.line, column: error.column }))?
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
	Attribute : XmlTypes.Attribute

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
	Node : XmlTypes.Node

	## Location and explanation of malformed XML. Lines and columns are
	## one-based; columns count UTF-8 bytes, and `\r\n`, `\r`, and `\n` each
	## end a line.
	## XML syntax, resource and incremental-input problems. Authored names,
	## code points and configured limits remain available without parsing text.
	Problem : [UnexpectedEventAfterRoot, MissingRootEvent, MissingRootElement, TrailingContent, ExpectedRoot, RootText, DoctypeUnsupported, UnclosedElement(Str), UnclosedCData, ExpectedMarkupDeclaration, ForbiddenCDataEnd, ExpectedEndTagName, EndTagMismatch({ actual : Str, expected : Str }), ExpectedEndTagClose, ExpectedVersion, ExpectedQuotedVersion, BadVersion, VersionOverflow, ExpectedVersionEndQuote, ExpectedEncodingSpace, ExpectedQuotedEncoding, BadEncodingStart, ExpectedEncodingEndQuote, ExpectedStandaloneSpace, ExpectedQuotedStandalone, BadStandalone, ExpectedStandaloneEndQuote, ExpectedDeclarationClose, ExpectedEquals, CommentDoubleHyphen, UnclosedComment, ExpectedInstructionTarget, DeclarationNotAtStart, ReservedInstructionTarget(Str), NamespacedInstructionTarget(Str), ExpectedInstructionSpace(Str), UnclosedInstruction, ExpectedStartTagName, ExpectedStartTagClose(Str), ExpectedStartTagSpace(Str), ExpectedAttributeName(Str), DuplicateAttribute(Str), ExpectedQuotedAttribute, AttributeContainsLt, UnclosedAttribute, ExpectedReference, ExpectedEntitySemicolon, UnknownEntity(Str), ExpectedCharacterDigits([Decimal, Hexadecimal]), ExpectedCharacterSemicolon, InvalidCharacterReference(U32), InvalidUtf8, InvalidCharacter(U32), ResourceLimit({ limit : Limit, max : U64 }), NonUtf8Encoding(Str), UnexpectedEndTag, IncompleteMarkup, BadTextFragmentSize({ minimum : U64, actual : U64 }), InvalidBom(List(U8)), CDataOutsideRoot, UnfinishedDocument({ construct : Construct, expected_end_tag : Try(Str, [Missing]) })]

	Construct : [Document, ByteOrderMark, Text, Tag, Comment, Instruction, CData, Reference]

	Error : { line : U64, column : U64, problem : Parser.Problem(Xml.Problem) }

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
	## expect Xml.parse_str("<a>&nbsp;</a>") == Err(InvalidXml({ line: 1, column: 4, problem: UnknownEntity("nbsp") }))
	## ```
	parse_str : Str -> Try(Xml, [InvalidXml(Error)])
	parse_str = |input| {
		bytes = input.to_utf8()
		match read_document(input, bytes, unbounded(bytes)) {
			Ok({ val, pos }) =>
				if pos == bytes.len() {
					Ok(val)
				} else {
					Err(InvalidXml(locate(bytes, pos, TrailingContent)))
				}

			Err(XmlFail(failure)) => Err(InvalidXml(locate(bytes, failure.offset, failure.problem)))
			Err(LimitFail(exceeded)) => Err(InvalidXml(locate(bytes, exceeded.offset, ResourceLimit({ limit: exceeded.limit, max: exceeded.max }))))
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
					Err(InvalidXml(locate(bytes, pos, TrailingContent)))
				}

			Err(failure) => Err(read_error(bytes, failure))
		}
	}

	## Parse one XML document, including an optional declaration and any
	## trailing comments, processing instructions and whitespace, and leave
	## the input after that for the next parser.
	##
	## A failure is `ParseError({ problem, offset })` with the same native problem
	## [Xml.parse_str] reports and the byte offset of the problem. Use this to
	## embed an XML document in a larger parser; otherwise prefer
	## [Xml.parse_str], which reports a line and column.
	##
	## ```roc
	## expect Utf8.parse_str(Xml.parser, "<a></b>") == Err(ParseError({ problem: EndTagMismatch({ actual: "b", expected: "a" }), offset: 3 }))
	##
	## expect Utf8.parse_str(Xml.parser, "<a/> <b/>") == Err(ParseError({ problem: UnexpectedInput, offset: 5 }))
	## ```
	parser : Parser(Utf8.Bytes, Xml, Problem)
	parser =
		Parser.custom(
			|input| {
				# Validate once, so names and text can be cut from the string
				# without validating each one again.
				match read_document(Str.from_utf8(input) ?? "", input, unbounded(input)) {
					Ok({ val, pos }) => Ok({ value: val, rest: input.drop_first(pos) })
					Err(XmlFail(failure)) => Err(ParseError({ problem: failure.problem, offset: failure.offset }))
					Err(LimitFail(exceeded)) => Err(ParseError({ problem: ResourceLimit({ limit: exceeded.limit, max: exceeded.max }), offset: exceeded.offset }))
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
	Limit : [Depth, Elements, Attributes, TokenBytes, ReferenceBytes, ContextBytes, TextBytes]

	## Why reading stopped: malformed XML, or a document that would exceed a
	## limit of [Xml.ReaderProps]. `max` is the limit, and the line and column
	## locate the start tag (or, for `Attributes`, the attribute) that would
	## exceed it.
	ReadError : [
		InvalidXml(Xml.Error),
		LimitExceeded({ limit : Limit, max : U64, line : U64, column : U64 }),
	]

	## Bounds for chunked XML. Tokens include complete tags, comments, and PIs.
	## Text and CDATA may be split into multiple Text events, independently of
	## input chunk boundaries. The decoder accepts UTF-8 bytes only.
	StreamProps := { max_depth : U64 ?? 1024, max_elements : U64 ?? 4_194_304, max_attributes : U64 ?? 1024, max_token_bytes : U64 ?? 65_536, max_text_bytes : U64 ?? 65_536, max_context_bytes : U64 ?? 4_194_304, namespaces : Bool ?? False }

	## Original byte offset and one-based physical line and byte column.
	StreamError : { offset : U64, line : U64, column : U64, problem : Parser.Problem(Xml.Problem) }

	## Configured byte decoder for the common Streaming readers.
	Decoder : Streaming.Decoder(ChunkDecoder, Event, StreamError)

	## Construct a decoder; feed it with Streaming.read_iter/read_stream or
	## call its step method directly. Only source exhaustion supplies Final.
	decoder : StreamProps -> Decoder
	decoder = |props| Streaming.decoder(if (props.namespaces) Single(chunk_initial(props)) else Batch(chunk_batch_initial(props)), chunk_decoder_step)

	## Position of the most recently emitted event, in original input bytes.
	decoder_location : Decoder -> { offset : U64, line : U64, column : U64 }
	decoder_location = |current| match current.state() {
		Single(state) => state.event_at
		Batch(batch) => batch.event_at
	}

	## Declaration parsed so far; non-UTF-8 declarations are rejected.
	decoder_declaration : Decoder -> Try(Declaration, [Missing])
	decoder_declaration = |current| match current.state() {
		Single(state) => Box.unbox(state.declaration)
		Batch(batch) => Box.unbox(Box.unbox(batch.decoder).declaration)
	}

	## An exact byte interval in XML source.
	SourceSpan : { start : U64, end : U64 }

	## A start tag with a source range for each attribute, in the same order.
	SourceStart : { name : Str, attributes : List(Attribute), attribute_spans : List(SourceSpan), span : SourceSpan, empty : Bool }

	## Validated lexical events retaining comments, processing instructions,
	## CDATA boundaries, and attribute ranges. Gap contains document whitespace
	## or a byte order mark outside the root; Declaration is the XML declaration.
	## A self-closing Start has empty=True and no corresponding End event.
	SourceEvent : [
		Start(SourceStart),
		End(SourceSpan),
		Text({ value : Str, span : SourceSpan }),
		CData({ value : Str, span : SourceSpan }),
		Comment({ value : Str, span : SourceSpan }),
		ProcessingInstruction({ target : Str, data : Str, span : SourceSpan }),
		Declaration(SourceSpan),
		Gap(SourceSpan),
	]

	## Fold validated authored events without constructing an XML tree.
	## Callback rejection stops at that event's source location. Existing reader
	## events and tree parsing retain their coalescing and omission behavior.
	fold_source : Str, ReaderProps, state, (state, SourceEvent -> Try(state, problem)) -> [Folded(state), Failed(ReadError), Rejected(problem, { line : U64, column : U64 })]
	fold_source = |input, props, initial, on_event| {
		bytes = input.to_utf8()
		match fold_source_impl(input, bytes, reader_limits(props), initial, on_event) {
			Ok(value) => Folded(value)
			Err(Failed(failure)) => Failed(read_error(bytes, failure))
			Err(Rejected(problem, offset)) => {
				at = position(bytes, offset)
				Rejected(problem, { line: at.line, column: at.column })
			}
		}
	}

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
		next = |input_reader| {
			bytes = input_reader.bytes
			match step(input_reader.src, bytes, input_reader.limits, input_reader.cursor) {
				Ok(Ev(event, cursor)) => Event(event, { ..input_reader, cursor })
				Ok(Finished(pos)) =>
					if pos == bytes.len() {
						Done
					} else {
						Failed(InvalidXml(locate(bytes, pos, TrailingContent)))
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
		element = |input_reader| {
			if input_reader.cursor.start == no_start {
				return NotAfterStart
			}
			bytes = input_reader.bytes
			tag =
				match parse_start_tag(input_reader.src, bytes, input_reader.cursor.start, input_reader.limits.attributes) {
					Ok(parsed) => parsed
					Err(failure) => return Failed(read_error(bytes, failure))
				}
			match build_element(input_reader.src, bytes, input_reader.limits, input_reader.cursor, tag.name, tag.attributes) {
				Ok(built) => Element(built.node, { ..input_reader, cursor: built.cursor })
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
		element_with = |input_reader, builder| {
			if input_reader.cursor.start == no_start {
				return NotAfterStart
			}
			bytes = input_reader.bytes
			match build_with(input_reader.src, bytes, input_reader.limits, input_reader.cursor, builder.context, builder.first, builder.start, builder.text, builder.end) {
				Built(built) => Element(built.node, built.context, { ..input_reader, cursor: built.cursor })
				Unread(failure) => Failed(read_error(bytes, failure))
				Rejected(error, offset) => {
					at = position(bytes, offset)
					Rejected(error, { line: at.line, column: at.column })
				}
			}
		}

		## The document's XML declaration, or `Err(Missing)` when it has none.
		declaration : Reader -> Try(Xml.Declaration, [Missing])
		declaration = |input_reader| Box.unbox(input_reader.declaration)

		## Where the most recent event starts: the `<` of its tag, or the
		## first byte of its text. Before the first event, the root's start
		## tag. Lines and columns count as in [Xml.Error].
		location : Reader -> { line : U64, column : U64 }
		location = |input_reader| {
			at = position(input_reader.bytes, input_reader.cursor.at)
			{ line: at.line, column: at.column }
		}
	}

	## A [Xml.Reader] positioned before the root element, after reading and
	## checking the document's prolog.
	##
	## ```roc
	## expect Xml.reader("<?xml version='1.0'?><a/>", {}).map_ok(|reader| reader.declaration().is_ok()) == Ok(True)
	##
	## expect Xml.reader("text", {}).map_ok(|_| {}) == Err(InvalidXml({ line: 1, column: 1, problem: RootText }))
	## ```
	reader : Str, ReaderProps -> Try(Reader, ReadError)
	reader = |input, props| {
		bytes = input.to_utf8()
		open_reader(input, bytes, reader_limits(props)).map_err(|failure| read_error(bytes, failure))
	}
	# Internal scanner operations for arena construction. Bytes must come from
	# src; token prefixes and expected name ranges are verified by the caller.
	# The root-close predicate selects a route; the full parser checks content.
	SourceLex :: [].{
		StartTag : { name : Str, attributes : List(Xml.Attribute), attribute_spans : List(Xml.SourceSpan), empty : Bool, pos : U64 }
		read_start : Str, List(U8), U64, U64 -> Try(StartTag, [Fallback])
		read_start = |src, bytes, start, maximum| parse_source_start_tag(src, bytes, start, maximum).map_err(|_| Fallback)
		read_head : Str, List(U8), U64 -> Try([Complete({ name : Str, empty : Bool, pos : U64 }), Attributes({ val : Str, pos : U64 })], [Fallback])
		read_head = |src, bytes, start| parse_source_start_head(src, bytes, start).map_err(|_| Fallback)
		read_attributes_with : Str, List(U8), { val : Str, pos : U64 }, U64, state, (state, Str, U64 -> Try(prepared, problem)), (state, prepared, Str, Xml.SourceSpan -> state) -> Try({ name : Str, state : state, empty : Bool, pos : U64 }, [Fallback])
		read_attributes_with = |src, bytes, name, maximum, initial, on_name, on_attribute| parse_source_attributes_with(src, bytes, name, maximum, initial, on_name, on_attribute).map_err(|_| Fallback)
		read_text : Str, List(U8), U64 -> Try({ val : Str, pos : U64 }, [Fallback])
		read_text = |src, bytes, start| read_source_text(src, bytes, start).map_err(|_| Fallback)
		read_end : Str, List(U8), U64, Xml.SourceSpan -> Try({ name_end : U64, pos : U64 }, [Fallback])
		read_end = |src, bytes, start, expected| read_end_tag(src, bytes, start, expected).map_err(|_| Fallback)
		closes_document : Str, U64 -> Bool
		closes_document = |src, maximum_attributes| {
			bytes = src.to_utf8()
			var $start = 0.U64
			while $start < bytes.len() and is_space(bytes.get($start) ?? 0) {
				$start = $start + 1
			}
			if bytes.get($start) != Ok('<') or bytes.get($start + 1) == Ok('!') or bytes.get($start + 1) == Ok('?') or bytes.get($start + 1) == Ok('/') {
				return False
			}
			root = parse_source_start_tag(src, bytes, $start, maximum_attributes) ?? return False
			if root.empty {
				return True
			}
			last_tag = src.split_last("<") ?? return False
			closing_start = last_tag.before.count_utf8_bytes()
			if bytes.get(closing_start + 1) != Ok('/') {
				return False
			}
			closing = read_end_tag(src, bytes, closing_start, { start: $start + 1, end: $start + 1 + root.name.count_utf8_bytes() }) ?? return False
			var $trailing = closing.pos
			while $trailing < bytes.len() and is_space(bytes.get($trailing) ?? 0) {
				$trailing = $trailing + 1
			}
			$trailing == bytes.len()
		}

		allows : U64, U64, U64, U64 -> Bool
		allows = |maximum_depth, maximum_elements, depth, elements| check_start({ depth: maximum_depth, elements: maximum_elements, attributes: 0, namespaces: True }, depth, elements, 0).is_ok()
	}

}

Failure : { offset : U64, problem : Parser.Problem(Xml.Problem) }

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
Cursor : { pos : U64, open : List(Str), pending_end : Bool, closed : Bool, start : U64, at : U64, elements : U64 }

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

fail : U64, Parser.Problem(Xml.Problem) -> Try(_, ReadFailure)
fail = |offset, problem| Err(XmlFail({ offset, problem }))

read_error : List(U8), ReadFailure -> Xml.ReadError
read_error = |bytes, failure| {
	match failure {
		XmlFail(problem) => InvalidXml(locate(bytes, problem.offset, problem.problem))
		LimitFail(exceeded) => {
			at = position(bytes, exceeded.offset)
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

locate : List(U8), U64, Parser.Problem(Xml.Problem) -> Xml.Error
locate = |bytes, offset, problem| {
	at = position(bytes, offset)
	{ line: at.line, column: at.column, problem }
}

position : List(U8), U64 -> { line : U64, column : U64 }
position = |bytes, offset| {
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
	{ line: $line, column: offset - $line_start + 1 }
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
				Ev(_, _) => fail(0, UnexpectedEventAfterRoot)
			}
		}

		_ => fail(0, MissingRootEvent)
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
		return fail($pos, DoctypeUnsupported)
	}
	if bytes.get($pos) != Ok('<') {
		return if $pos >= bytes.len() {
			fail($pos, ExpectedRoot)
		} else {
			fail($pos, RootText)
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
		name = reader_open_name(r.open)
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
				EndOfInput(pos) => return fail(pos, UnclosedElement(reader_open_name(r.open)))
			}
		}
	if bytes.get(at + 1) == Ok('/') {
		opened = r.open.last() ?? ""
		end_tag = read_end_named(src, bytes, at, opened)?
		open = r.open.drop_last(1)
		closed = open.is_empty()
		return Ok(Ev(End(opened), { ..r, pos: end_tag.pos, open, closed, start: no_start, at }))
	}
	check_start(limits, r.open.len(), r.elements, at)?
	tag = parse_start_tag(src, bytes, at, limits.attributes)?
	elements = r.elements + 1
	open = r.open.append(tag.name)
	Ok(Ev(Start({ name: tag.name, attributes: tag.attributes }), { ..r, pos: tag.pos, open, pending_end: tag.empty, start: at, at, elements }))
}

reader_open_name : List(Str) -> Str
reader_open_name = |open| open.last() ?? ""

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
	if byte == '\n' or byte == ' ' or byte == '\t' {
		var $indent_end = start + 1
		while $indent_end < bytes.len() and $indent_end - start < 8 {
			part = bytes.get($indent_end) ?? 0
			if part == ' ' or part == '\t' or part == '\n' {
				$indent_end = $indent_end + 1
			} else {
				break
			}
		}
		if bytes.get($indent_end) == Ok('<') and starts_tag(bytes, $indent_end) {
			return Ok(TextUntil(slice_str(src, bytes, start, $indent_end), $indent_end))
		}
	}
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
							Err(_) => return fail(cdata_start, UnclosedCData)
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
				return fail($pos, ExpectedMarkupDeclaration)
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
			return fail($pos, ForbiddenCDataEnd)
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
	# The expected name has already passed XML name validation. A matching
	# spelling immediately followed by '>' needs neither a second name scan
	# nor whitespace handling. Other inputs take the diagnostic path below.
	known_end = at + 2 + expected.end - expected.start
	if expected.end > expected.start and bytes.get(known_end) == Ok('>') and same_bytes(bytes, at + 2, expected.start, expected.end - expected.start) {
		return Ok({ name_end: known_end, pos: known_end + 1 })
	}
	end =
		match name_end(bytes, at + 2) {
			Ok(pos) => pos
			Err(_) => return fail(at + 2, ExpectedEndTagName)
		}
	len = end - (at + 2)
	if len != expected.end - expected.start or !same_bytes(bytes, at + 2, expected.start, len) {
		return fail(at, EndTagMismatch({ actual: slice_str(src, bytes, at + 2, end), expected: slice_str(src, bytes, expected.start, expected.end) }))
	}
	close = skip_space(bytes, end)
	if bytes.get(close) != Ok('>') {
		return fail(close, ExpectedEndTagClose)
	}
	Ok({ name_end: end, pos: close + 1 })
}

read_end_named : Str, List(U8), U64, Str -> Try({ name_end : U64, pos : U64 }, ReadFailure)
read_end_named = |src, bytes, at, expected| {
	known_end = at + 2 + expected.count_utf8_bytes()
	if !expected.is_empty() and bytes.get(known_end) == Ok('>') {
		var $position = at + 2
		var $same = True
		for byte in expected.iter_utf8() {
			$same = $same and bytes.get($position) == Ok(byte)
			$position = $position + 1
		}
		if $same {
			return Ok({ name_end: known_end, pos: known_end + 1 })
		}
	}
	end = name_end(bytes, at + 2) ?? return fail(at + 2, ExpectedEndTagName)
	actual = slice_str(src, bytes, at + 2, end)
	if actual != expected {
		return fail(at, EndTagMismatch({ actual, expected }))
	}
	close = skip_space(bytes, end)
	if bytes.get(close) != Ok('>') {
		return fail(close, ExpectedEndTagClose)
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
	var $range = { start: r.start + 1, end: r.start + 1 + (r.open.last() ?? "").count_utf8_bytes() }
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

			EndOfInput(pos) => return fail(pos, UnclosedElement(slice_str(src, bytes, $range.start, $range.end)))
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
						# A text-only child can be finished directly. It uses the same
						# content and closing-tag validators, without putting its text
						# into the scratch node list and then copying it out again.
						leaf = scan(src, bytes, limits.namespaces, tag.pos)?
						match leaf {
							TextUntil(text, close_at) if bytes.get(close_at + 1) == Ok('/') => {
								closed = read_end_tag(src, bytes, close_at, { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() })?
								$nodes = $nodes.append(Element({ name: tag.name, attributes: tag.attributes, children: [Text(text)] }))
								$pos = closed.pos
							}
							TagAt(close_at) if bytes.get(close_at + 1) == Ok('/') => {
								closed = read_end_tag(src, bytes, close_at, { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() })?
								$nodes = $nodes.append(Element({ name: tag.name, attributes: tag.attributes, children: [] }))
								$pos = closed.pos
							}
							_ => {
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
	}
}

# See https://www.w3.org/TR/xml/#NT-XMLDecl
parse_xml_declaration : List(U8), U64 -> Parsed(Xml.Declaration)
parse_xml_declaration = |bytes, start| {
	var $pos = skip_space(bytes, start + 5)
	if !starts_with_at(bytes, $pos, "version".to_utf8()) {
		return fail($pos, ExpectedVersion)
	}
	$pos = skip_eq(bytes, $pos + 7)?
	version_quote = bytes.get($pos) ?? 0
	if version_quote != '"' and version_quote != '\'' {
		return fail($pos, ExpectedQuotedVersion)
	}
	if !starts_with_at(bytes, $pos + 1, "1.".to_utf8()) or !is_digit_at(bytes, $pos + 3) {
		return fail($pos + 1, BadVersion)
	}
	var $minor = 0
	$pos = $pos + 3
	while is_digit_at(bytes, $pos) {
		if $minor >= 100_000_000_000_000_000 {
			return fail(start, VersionOverflow)
		}
		$minor = $minor * 10 + U8.to_u64((bytes.get($pos) ?? '0') - '0')
		$pos = $pos + 1
	}
	if bytes.get($pos) != Ok(version_quote) {
		return fail($pos, ExpectedVersionEndQuote)
	}
	version = { major: 1, minor: $minor }
	$pos = $pos + 1

	var $encoding = Err(Missing)
	after_version = skip_space(bytes, $pos)
	$pos = after_version
	if starts_with_at(bytes, $pos, "encoding".to_utf8()) {
		if after_version == skip_space_start(bytes, after_version) {
			return fail($pos, ExpectedEncodingSpace)
		}
		$pos = skip_eq(bytes, $pos + 8)?
		quote = bytes.get($pos) ?? 0
		if quote != '"' and quote != '\'' {
			return fail($pos, ExpectedQuotedEncoding)
		}
		name_start = $pos + 1
		if !(is_alphabetical(bytes.get(name_start) ?? 0)) {
			return fail(name_start, BadEncodingStart)
		}
		$pos = name_start + 1
		while is_encoding_char(bytes.get($pos) ?? 0) {
			$pos = $pos + 1
		}
		if bytes.get($pos) != Ok(quote) {
			return fail($pos, ExpectedEncodingEndQuote)
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
			return fail($pos, ExpectedStandaloneSpace)
		}
		$pos = skip_eq(bytes, $pos + 10)?
		quote = bytes.get($pos) ?? 0
		if quote != '"' and quote != '\'' {
			return fail($pos, ExpectedQuotedStandalone)
		}
		value_start = $pos + 1
		$pos =
			if starts_with_at(bytes, value_start, "yes".to_utf8()) {
				value_start + 3
			} else if starts_with_at(bytes, value_start, "no".to_utf8()) {
				value_start + 2
			} else {
				return fail(value_start, BadStandalone)
			}
		if bytes.get($pos) != Ok(quote) {
			return fail($pos, ExpectedStandaloneEndQuote)
		}
		$pos = skip_space(bytes, $pos + 1)
	}
	if !starts_with_at(bytes, $pos, "?>".to_utf8()) {
		return fail($pos, ExpectedDeclarationClose)
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
		fail(pos, ExpectedEquals)
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
				fail($pos, CommentDoubleHyphen)
			}
		}
		$pos = check_char(bytes, $pos)?
	}
	fail(start, UnclosedComment)
}

# See https://www.w3.org/TR/xml/#NT-PI. With namespaces, a target may not
# contain a colon (https://www.w3.org/TR/xml-names/#Conformance).
skip_processing_instruction : List(U8), U64, Bool -> Try(U64, ReadFailure)
skip_processing_instruction = |bytes, start, namespaces| {
	target =
		match parse_name("", bytes, start + 2) {
			Ok(name) => name
			Err(_) => return fail(start + 2, ExpectedInstructionTarget)
		}
	if ascii_lowercase(target.val.to_utf8()) == "xml".to_utf8() {
		return if target.val == "xml" {
			fail(start, DeclarationNotAtStart)
		} else {
			fail(start + 2, ReservedInstructionTarget(target.val))
		}
	}
	if namespaces and target.val.contains(":") {
		return fail(start + 2, NamespacedInstructionTarget(target.val))
	}
	if starts_with_at(bytes, target.pos, "?>".to_utf8()) {
		return Ok(target.pos + 2)
	}
	if !is_space_at(bytes, target.pos) {
		return fail(target.pos, ExpectedInstructionSpace(target.val))
	}
	var $pos = target.pos
	while $pos < bytes.len() {
		if starts_with_at(bytes, $pos, "?>".to_utf8()) {
			return Ok($pos + 2)
		}
		$pos = check_char(bytes, $pos)?
	}
	fail(start, UnclosedInstruction)
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
		text.append_sublist(bytes, { start, len: end - start })
	} else {
		text
	}
}

# The input bytes [start, end) as a `Str`. With the validated input string
# this is a slice of it, checked only at its two ends; without it (invalid
# UTF-8 elsewhere in the input) the bytes are validated.
slice_str : Str, List(U8), U64, U64 -> Str
slice_str = |src, bytes, start, end| {
	value = if src.is_empty() {
		Str.from_utf8(bytes.sublist({ start, len: end - start })) ?? ""
	} else {
		match src.drop_first_bytes(start) {
			Ok(tail) => tail.drop_last_bytes(bytes.len() - end) ?? ""
			Err(_) => ""
		}
	}
	# Eleven bytes fit inline on both 32-bit and 64-bit targets. Short names
	# and values then avoid shared-string reference counts during tree building.
	if value.count_utf8_bytes() <= 11 {
		value.release_excess_capacity()
	} else value
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
	var $range = { start: r.start + 1, end: r.start + 1 + (r.open.last() ?? "").count_utf8_bytes() }
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

			EndOfInput(pos) => return Unread(XmlFail({ offset: pos, problem: UnclosedElement(slice_str(src, bytes, $range.start, $range.end)) }))
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
						leaf = match scan(src, bytes, limits.namespaces, tag.pos) {
							Ok(value) => value
							Err(failure) => return Unread(failure)
						}
						match leaf {
							TextUntil(text, close_at) if bytes.get(close_at + 1) == Ok('/') => {
								# Keep callback order: text is observed before validating
								# its following end tag, and end runs only after success.
								child = on_text(text)
								closed = match read_end_tag(src, bytes, close_at, { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() }) {
									Ok(value) => value
									Err(failure) => return Unread(failure)
								}
								ended = on_end(started.context, started.open, [child])
								$context = ended.context
								$nodes = $nodes.append(ended.node)
								$pos = closed.pos
							}
							TagAt(close_at) if bytes.get(close_at + 1) == Ok('/') => {
								closed = match read_end_tag(src, bytes, close_at, { start: at + 1, end: at + 1 + tag.name.count_utf8_bytes() }) {
									Ok(value) => value
									Err(failure) => return Unread(failure)
								}
								ended = on_end(started.context, started.open, [])
								$context = ended.context
								$nodes = $nodes.append(ended.node)
								$pos = closed.pos
							}
							_ => {
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
			Err(_) => return fail(start + 1, ExpectedStartTagName)
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
			return fail($pos, ExpectedStartTagClose(name.val))
		}
		if $pos == before_space {
			return fail($pos, ExpectedStartTagSpace(name.val))
		}
		attribute_name =
			match parse_name(src, bytes, $pos) {
				Ok(parsed) => parsed
				Err(_) => return fail($pos, ExpectedAttributeName(name.val))
			}
		if $attributes.len() >= max_attributes {
			return Err(LimitFail({ limit: Attributes, max: max_attributes, offset: $pos }))
		}
		duplicate =
			if $attributes.len() <= small_attribute_count {
				has_attribute_name($attributes, attribute_name.val)
			} else {
				$seen.contains(attribute_name.val)
			}
		if duplicate {
			return fail($pos, DuplicateAttribute(attribute_name.val))
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

# Project only the name: comparing an attribute never copies or retains its
# value, even when that value owns a large normalized string.
has_attribute_name : List(Xml.Attribute), Str -> Bool
has_attribute_name = |attributes, wanted| {
	var $index = 0.U64
	while $index < attributes.len() {
		name = match attributes.get($index) {
			Ok({ name: present, .. }) => present
			_ => ""
		}
		if name == wanted {
			return True
		}
		$index = $index + 1
	}
	False
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
		return fail(start, ExpectedQuotedAttribute)
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
			return fail($pos, AttributeContainsLt)
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
	fail(start, UnclosedAttribute)
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
				Err(_) => return fail(start, ExpectedReference)
			}
		if bytes.get(name.pos) != Ok(';') {
			return fail(name.pos, ExpectedEntitySemicolon)
		}
		replacement =
			match name.val {
				"lt" => '<'
				"gt" => '>'
				"amp" => '&'
				"apos" => '\''
				"quot" => '"'
				other => return fail(start, UnknownEntity(other))
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
		return fail(digits_start, if (base == 16) ExpectedCharacterDigits(Hexadecimal) else ExpectedCharacterDigits(Decimal))
	}
	if bytes.get($pos) != Ok(';') {
		return fail($pos, ExpectedCharacterSemicolon)
	}
	if !is_xml_char($code) {
		return fail(start, InvalidCharacterReference($code))
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
	byte = bytes.get(start) ?? 0
	if (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or byte == '_' or byte == ':' {
		var $pos = start + 1
		while $pos < bytes.len() and $pos - start < 16 {
			part = bytes.get($pos) ?? 0
			if (part >= 'a' and part <= 'z') or (part >= 'A' and part <= 'Z') or (part >= '0' and part <= '9') or part == '_' or part == ':' or part == '-' or part == '.' {
				$pos = $pos + 1
			} else if part < 0x80 {
				return Ok($pos)
			} else {
				return name_end_general(bytes, start)
			}
		}
		if ($pos < bytes.len()) name_end_general(bytes, start) else Ok($pos)
	} else name_end_general(bytes, start)
}

name_end_general : List(U8), U64 -> Try(U64, [NotAName])
name_end_general = |bytes, start| {
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
		Err(_) => fail(pos, InvalidUtf8)
		Ok(scalar) =>
			if is_xml_char(scalar.code) {
				Ok(pos + scalar.len)
			} else {
				fail(pos, InvalidCharacter(scalar.code))
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

example_element : Str, List(Xml.Attribute), List(Xml.Node) -> Xml.Node
example_element = |name, attributes, children| Element({ name, attributes, children })

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
			root: example_element(
				"root",
				[],
				[
					Text("\n    "),
					example_element("element", [{ name: "arg", value: "value" }], []),
					Text("\n"),
				],
			),
		}
}

# XML parsing accepts documents without a prolog.
expect {
	result = Utf8.parse_str(Xml.parser, "<element />")?
	result == { declaration: Err(Missing), root: example_element("element", [], []) }
}

# A declaration without an encoding reports it as missing, and versions
# keep every digit after the dot.
expect Xml.parse_str("<?xml version='1.10'?><a/>").map_ok(|xml| xml.declaration) == Ok(Ok({ version: { major: 1, minor: 10 }, encoding: Err(Missing) }))
expect Xml.parse_str("<?xml version='1.0' encoding='latin1'?><a/>").map_ok(|xml| xml.declaration) == Ok(Ok({ version: v1_0, encoding: Ok(OtherEncoding("latin1")) }))
expect error_at("<?xml version='1.99999999999999999999'?><a/>") == Ok({ line: 1, column: 1 })

# Empty elements can omit whitespace before the self-closing marker.
expect root_of("<element/>") == Ok(example_element("element", [], []))

# Empty elements can carry attributes.
expect root_of("<element arg=\"value\"/>") == Ok(example_element("element", [{ name: "arg", value: "value" }], []))

# Explicit start and end tags can represent an empty element.
expect root_of("<element></element>") == Ok(example_element("element", [], []))

# Elements can parse multiple attributes and text content.
expect {
	result = root_of("<element firstArg=\"one\" secondArg='two'>text content</element>")
	result
		== Ok(
			example_element(
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
expect root_of("<element><![CDATA[<literal />]]></element>") == Ok(example_element("element", [], [Text("<literal />")]))

# Partial CDATA closing text is preserved until the real close marker.
expect root_of("<element><![CDATA[this is ]] not ]> the end]]></element>") == Ok(example_element("element", [], [Text("this is ]] not ]> the end")]))

# Nested elements preserve attributes on parent and child nodes.
expect {
	result = root_of("<parent argParent=\"outer\"><child argChild=\"inner\" /></parent>")
	result == Ok(example_element("parent", [{ name: "argParent", value: "outer" }], [example_element("child", [{ name: "argChild", value: "inner" }], [])]))
}

# Nested element parsing preserves whitespace text nodes.
expect root_of("<parent>\n    <child />\n</parent>") == Ok(example_element("parent", [], [Text("\n    "), example_element("child", [], []), Text("\n")]))

# Siblings and nested children end up under the right parents.
expect root_of("<a>1<b>2<c/>3</b>4<d><e>5</e></d>6</a>") == Ok(example_element("a", [], [Text("1"), example_element("b", [], [Text("2"), example_element("c", [], []), Text("3")]), Text("4"), example_element("d", [], [example_element("e", [], [Text("5")])]), Text("6")]))

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
			example_element(
				"feed",
				[{ name: "xmlns", value: "http://www.w3.org/2005/Atom" }],
				[
					Text("\n    "),
					example_element("title", [], [Text("Atom Feed")]),
					Text("\n    "),
					example_element(
						"link",
						[
							{ name: "rel", value: "self" },
							{ name: "type", value: "application/atom+xml" },
							{ name: "href", value: "http://example.org" },
						],
						[],
					),
					Text("\n    "),
					example_element("updated", [], [Text("2024-02-23T20:38:24Z")]),
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
			root: example_element("root", [], [example_element("Example", [], [])]),
		})
}

# Malformed input ending in a multibyte scalar returns an error instead of crashing
# while rendering a parser failure from a mid-scalar byte position.
expect Utf8.parse_str(Xml.parser, "<ӿ").is_err()

# End tags must match their start tag (WFC: Element Type Match).
expect error_at("<a><b></a></b>") == Ok({ line: 1, column: 7 })

# Names may contain digits and non-ASCII letters.
expect root_of("<h1 données-2='x'>é</h1>") == Ok(example_element("h1", [{ name: "données-2", value: "x" }], [Text("é")]))

# Predefined entity and character references are replaced in text and attributes.
expect root_of("<a t='&lt;&#65;&#x1F600;&quot;'>&amp;&gt;&apos;</a>") == Ok(example_element("a", [{ name: "t", value: "<A😀\"" }], [Text("&>'")]))

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
expect root_of("<a b='x\r\ny\tz\n&#10;&#9;'/>") == Ok(example_element("a", [{ name: "b", value: "x y z \n\t" }], []))

# Line endings in content are normalised to LF (XML 1.0 section 2.11).
expect root_of("<a>1\r\n2\r3<![CDATA[\r\n]]></a>") == Ok(example_element("a", [], [Text("1\n2\n3\n")]))

# Comments and processing instructions are accepted everywhere Misc is
# allowed, and text on both sides of them is merged.
expect {
	result = Xml.parse_str("<!-- c --><?pi data?>\n<a>x<!-- y -->z<?p?><![CDATA[w]]>&amp;</a><!---->\n<?q ?>")
	result.map_ok(|xml| xml.root) == Ok(example_element("a", [], [Text("xzw&")]))
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
expect Utf8.parse_str(Xml.parser, "<a/> <b/>") == Err(ParseError({ problem: UnexpectedInput, offset: 5 }))

# The parser combinator reports structured failures with a byte offset.
expect Utf8.parse_str(Xml.parser, "<a></b>") == Err(ParseError({ problem: EndTagMismatch({ actual: "b", expected: "a" }), offset: 3 }))

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
expect Xml.parse_str("<a>&nbsp;</a>") == Err(InvalidXml({ line: 1, column: 4, problem: UnknownEntity("nbsp") }))

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
expect events_of("<a></b>", {}) == Err(InvalidXml({ line: 1, column: 4, problem: EndTagMismatch({ actual: "b", expected: "a" }) }))
expect events_of("<a/><b/>", {}) == Err(InvalidXml({ line: 1, column: 5, problem: TrailingContent }))
expect events_of("<a>x", {}) == Err(InvalidXml({ line: 1, column: 5, problem: UnclosedElement("a") }))

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
expect Xml.parse("<a><?x:y?></a>", { namespaces: True }) == Err(InvalidXml({ line: 1, column: 6, problem: NamespacedInstructionTarget("x:y") }))

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
expect Xml.reader("text", {}).map_ok(|_| {}) == Err(InvalidXml({ line: 1, column: 1, problem: RootText }))

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

parse_source_start_tag : Str, List(U8), U64, U64 -> Try({ name : Str, attributes : List(Xml.Attribute), attribute_spans : List(Xml.SourceSpan), empty : Bool, pos : U64 }, ReadFailure)
parse_source_start_tag = |src, bytes, start, max_attributes| {
	name = match parse_source_start_head(src, bytes, start)? {
		Complete(tag) => return Ok({ name: tag.name, attributes: [], attribute_spans: [], empty: tag.empty, pos: tag.pos })
		Attributes(parsed) => parsed
	}
	parsed = parse_source_attributes_with(src, bytes, name, max_attributes, { attributes: [], spans: [], seen: Set.empty() }, source_attribute_name, source_attribute_append).map_err(
		|problem| match problem {
			Failed(failure) => failure
			Rejected(failure) => failure
		},
	)?
	Ok({ name: parsed.name, attributes: parsed.state.attributes, attribute_spans: parsed.state.spans, empty: parsed.empty, pos: parsed.pos })
}

parse_source_start_head : Str, List(U8), U64 -> Try([Complete({ name : Str, empty : Bool, pos : U64 }), Attributes({ val : Str, pos : U64 })], ReadFailure)
parse_source_start_head = |src, bytes, start| {
	name = match parse_name(src, bytes, start + 1) {
		Ok(parsed) => parsed
		Err(_) => return fail(start + 1, ExpectedStartTagName)
	}
	if bytes.get(name.pos) == Ok('>') {
		return Ok(Complete({ name: name.val, empty: False, pos: name.pos + 1 }))
	}
	if bytes.get(name.pos) == Ok('/') and bytes.get(name.pos + 1) == Ok('>') {
		return Ok(Complete({ name: name.val, empty: True, pos: name.pos + 2 }))
	}
	Ok(Attributes(name))
}

SourceAttributes : { attributes : List(Xml.Attribute), spans : List(Xml.SourceSpan), seen : Set(Str) }

source_attribute_name : SourceAttributes, Str, U64 -> Try(Str, ReadFailure)
source_attribute_name = |state, written, offset| {
	duplicate = if state.attributes.len() <= small_attribute_count {
		has_attribute_name(state.attributes, written)
	} else state.seen.contains(written)
	if duplicate {
		return fail(offset, DuplicateAttribute(written))
	}
	Ok(written)
}

source_attribute_append : SourceAttributes, Str, Str, Xml.SourceSpan -> SourceAttributes
source_attribute_append = |state, written, value, span| {
	attributes = state.attributes.append({ name: written, value })
	seen = if attributes.len() > small_attribute_count {
		if state.seen.is_empty() {
			Set.from_list(attributes.map(|attribute| attribute.name))
		} else state.seen.insert(written)
	} else state.seen
	{ attributes, spans: state.spans.append(span), seen }
}

parse_source_attributes_with : Str, List(U8), { val : Str, pos : U64 }, U64, state, (state, Str, U64 -> Try(prepared, problem)), (state, prepared, Str, Xml.SourceSpan -> state) -> Try({ name : Str, state : state, empty : Bool, pos : U64 }, [Failed(ReadFailure), Rejected(problem)])
parse_source_attributes_with = |src, bytes, name, max_attributes, initial, on_name, on_attribute| {
	var $state = initial
	var $count = 0.U64
	var $pos = name.pos
	while True {
		before_space = $pos
		$pos = skip_space(bytes, $pos)
		if bytes.get($pos) == Ok('>') {
			return Ok({ name: name.val, state: $state, empty: False, pos: $pos + 1 })
		}
		if bytes.get($pos) == Ok('/') and bytes.get($pos + 1) == Ok('>') {
			return Ok({ name: name.val, state: $state, empty: True, pos: $pos + 2 })
		}
		if $pos >= bytes.len() {
			return fail($pos, ExpectedStartTagClose(name.val)).map_err(|failure| Failed(failure))
		}
		if $pos == before_space {
			return fail($pos, ExpectedStartTagSpace(name.val)).map_err(|failure| Failed(failure))
		}
		attribute_start = $pos
		attribute_name = match parse_name(src, bytes, $pos) {
			Ok(parsed) => parsed
			Err(_) => return fail($pos, ExpectedAttributeName(name.val)).map_err(|failure| Failed(failure))
		}
		if $count >= max_attributes {
			return Err(Failed(LimitFail({ limit: Attributes, max: max_attributes, offset: $pos })))
		}
		prepared = on_name($state, attribute_name.val, $pos).map_err(|problem| Rejected(problem))?
		$pos = skip_eq(bytes, attribute_name.pos).map_err(|failure| Failed(failure))?
		value = parse_attribute_value(src, bytes, $pos).map_err(|failure| Failed(failure))?
		$state = on_attribute($state, prepared, value.val, { start: attribute_start, end: value.pos })
		$count = $count + 1
		$pos = value.pos
	}
}

# The authored fold shares the XML name, attribute, reference, character,
# end-tag, comment, PI and declaration validators with the existing reader.
fold_source_impl : Str, List(U8), Limits, state, (state, Xml.SourceEvent -> Try(state, problem)) -> Try(state, [Failed(ReadFailure), Rejected(problem, U64)])
fold_source_impl = |src, bytes, limits, initial, on_event| {
	var $state = initial
	var $pos = 0
	var $open = []
	var $closed = False
	var $elements = 0
	if starts_with_at(bytes, 0, [0xEF, 0xBB, 0xBF]) {
		$state = on_event($state, Gap({ start: 0, end: 3 })).map_err(|problem| Rejected(problem, 0))?
		$pos = 3
	}
	if starts_with_at(bytes, $pos, "<?xml".to_utf8()) and is_space_at(bytes, $pos + 5) {
		declaration = parse_xml_declaration(bytes, $pos).map_err(|failure| Failed(failure))?
		$state = on_event($state, Declaration({ start: $pos, end: declaration.pos })).map_err(|problem| Rejected(problem, $pos))?
		$pos = declaration.pos
	}
	while True {
		start = $pos
		leading = bytes.get(start) ?? 0
		following = bytes.get(start + 1) ?? 0
		if $open.is_empty() and is_space(leading) {
			$pos = skip_space(bytes, start)
			$state = on_event($state, Gap({ start, end: $pos })).map_err(|problem| Rejected(problem, start))?
		} else if leading == '<' and following == '!' and starts_with_at(bytes, start, "<!--".to_utf8()) {
			$pos = skip_comment(bytes, start).map_err(|failure| Failed(failure))?
			value = source_lines(slice_str(src, bytes, start + 4, $pos - 3))
			$state = on_event($state, Comment({ value, span: { start, end: $pos } })).map_err(|problem| Rejected(problem, start))?
		} else if leading == '<' and following == '?' {
			$pos = skip_processing_instruction(bytes, start, limits.namespaces).map_err(|failure| Failed(failure))?
			target = parse_name(src, bytes, start + 2) ?? { val: "", pos: start + 2 }
			data_start = skip_space(bytes, target.pos)
			data = source_lines(slice_str(src, bytes, if (data_start < $pos - 2) data_start else $pos - 2, $pos - 2))
			$state = on_event($state, ProcessingInstruction({ target: target.val, data, span: { start, end: $pos } })).map_err(|problem| Rejected(problem, start))?
		} else if $open.is_empty() and $closed {
			if start == bytes.len() {
				return Ok($state)
			}
			return Err(Failed(XmlFail({ offset: start, problem: TrailingContent })))
		} else if $open.is_empty() and leading == '<' and following == '!' and starts_with_at(bytes, start, "<!DOCTYPE".to_utf8()) {
			return Err(Failed(XmlFail({ offset: start, problem: DoctypeUnsupported })))
		} else if start >= bytes.len() {
			if $open.is_empty() {
				return Err(Failed(XmlFail({ offset: start, problem: ExpectedRoot })))
			}
			return Err(Failed(XmlFail({ offset: start, problem: UnclosedElement(open_name(src, bytes, $open)) })))
		} else if $open.is_empty() and leading != '<' {
			return Err(Failed(XmlFail({ offset: start, problem: RootText })))
		} else if !$open.is_empty() and leading == '<' and following == '!' and starts_with_at(bytes, start, "<![CDATA[".to_utf8()) {
			parsed = read_source_cdata(src, bytes, start).map_err(|failure| Failed(failure))?
			$pos = parsed.pos
			$state = on_event($state, CData({ value: parsed.val, span: { start, end: $pos } })).map_err(|problem| Rejected(problem, start))?
		} else if !$open.is_empty() and leading != '<' {
			parsed = read_source_text(src, bytes, start).map_err(|failure| Failed(failure))?
			$pos = parsed.pos
			$state = on_event($state, Text({ value: parsed.val, span: { start, end: $pos } })).map_err(|problem| Rejected(problem, start))?
		} else if !$open.is_empty() and following == '!' {
			return Err(Failed(XmlFail({ offset: start, problem: ExpectedMarkupDeclaration })))
		} else if following == '/' {
			end_tag = read_end_tag(src, bytes, start, $open.last() ?? { start: 0, end: 0 }).map_err(|failure| Failed(failure))?
			$pos = end_tag.pos
			$open = $open.drop_last(1)
			$closed = $open.is_empty()
			$state = on_event($state, End({ start, end: $pos })).map_err(|problem| Rejected(problem, start))?
		} else {
			check_start(limits, $open.len(), $elements, start).map_err(|failure| Failed(failure))?
			tag = parse_source_start_tag(src, bytes, start, limits.attributes).map_err(|failure| Failed(failure))?
			$pos = tag.pos
			$elements = $elements + 1
			$state = on_event($state, Start({ name: tag.name, attributes: tag.attributes, attribute_spans: tag.attribute_spans, empty: tag.empty, span: { start, end: $pos } })).map_err(|problem| Rejected(problem, start))?
			if tag.empty {
				$closed = $open.is_empty()
			} else {
				$open = $open.append({ start: start + 1, end: start + 1 + tag.name.count_utf8_bytes() })
			}
		}
	}
}

source_lines : Str -> Str
source_lines = |value| if value.contains("\r") {
	Str.join_with(Str.join_with(value.split_on("\r\n"), "\n").split_on("\r"), "\n")
} else value

read_source_text : Str, List(U8), U64 -> Parsed(Str)
read_source_text = |src, bytes, start| {
	first_byte = bytes.get(start) ?? 0
	if first_byte >= 0x20 and first_byte < 0x80 and first_byte != '<' and first_byte != '&' and first_byte != ']' and bytes.get(start + 1) == Ok('<') {
		return Ok({ val: slice_str(src, bytes, start, start + 1), pos: start + 1 })
	}
	var $text = []
	var $run_start = start
	var $run_end = start
	var $pos = start
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == '<' {
			break
		} else if byte == '&' {
			reference = parse_reference(bytes, $pos)?
			$text = append_utf8(materialize(bytes, $text, $run_start, $run_end), reference.val)
			$pos = reference.pos
			$run_start = $pos
			$run_end = $pos
		} else if byte == ']' and starts_with_at(bytes, $pos, "]]>".to_utf8()) {
			return fail($pos, ForbiddenCDataEnd)
		} else if byte == '\r' {
			$text = materialize(bytes, $text, $run_start, $run_end).append('\n')
			$pos = if (bytes.get($pos + 1) == Ok('\n')) $pos + 2 else $pos + 1
			$run_start = $pos
			$run_end = $pos
		} else {
			end = if byte >= 0x20 and byte < 0x80 {
				MarkupScan.xml_text(bytes, $pos + 1)
			} else check_char(bytes, $pos)?
			$run_end = end
			$pos = end
		}
	}
	Ok({ val: text_str(src, bytes, $text, $run_start, $run_end), pos: $pos })
}

read_source_cdata : Str, List(U8), U64 -> Parsed(Str)
read_source_cdata = |src, bytes, start| {
	var $text = []
	var $run_start = start + 9
	var $run_end = start + 9
	var $pos = start + 9
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == ']' and bytes.get($pos + 1) == Ok(']') and bytes.get($pos + 2) == Ok('>') {
			return Ok({ val: text_str(src, bytes, $text, $run_start, $run_end), pos: $pos + 3 })
		} else if byte == '\r' {
			$text = materialize(bytes, $text, $run_start, $run_end).append('\n')
			$pos = if (bytes.get($pos + 1) == Ok('\n')) $pos + 2 else $pos + 1
			$run_start = $pos
			$run_end = $pos
		} else {
			end = if byte >= 0x20 and byte < 0x80 and byte != ']' {
				MarkupScan.cdata_text(bytes, $pos + 1)
			} else check_char(bytes, $pos)?
			$run_end = end
			$pos = end
		}
	}
	fail(start, UnclosedCData)
}

expect Xml.fold_source(
	"<x/>\n<!--z-->",
	{},
	0.U64,
	|count, event| match event {
		Comment(_) => Err(Stopped)
		_ => Ok(count + 1)
	},
) == Rejected(Stopped, { line: 2, column: 1 })

# Chunk framing is separate from token validation. Each token is scanned once
# to find its end and once by the same parser as the complete-input reader.
ChunkAt : { offset : U64, line : U64, column : U64 }

ChunkMode : [Initial, Bom, Data, Markup([Prefix, Tag, Comment, Instruction]), CData]

ChunkState : {
	props : Box(Xml.StreamProps),
	mode : ChunkMode,
	quote : U8,
	buffer : List(U8),
	piece_at : ChunkAt,
	event_at : ChunkAt,
	utf8_left : U8,
	reference : Bool,
	reference_bytes : U64,
	brackets : U8,
	cdata_brackets : U64,
	skip_text_lf : Bool,
	open : List({ name : Str, retained_bytes : U64 }),
	context_bytes : U64,
	elements : U64,
	closed : Bool,
	pending_end : Bool,
	declaration : Box(Try(Xml.Declaration, [Missing])),
	offset : U64,
	line : U64,
	column : U64,
	previous_cr : Bool,
	done : Bool,
	bom : Bool,
}

chunk_initial : Xml.StreamProps -> ChunkState
chunk_initial = |props| {
	at = { offset: 0, line: 1, column: 1 }
	{ props: Box.box(props), mode: Initial, buffer: [], quote: 0, piece_at: at, event_at: at, utf8_left: 0, reference: False, reference_bytes: 0, brackets: 0, cdata_brackets: 0, skip_text_lf: False, open: [], context_bytes: 0, elements: 0, closed: False, pending_end: False, declaration: Box.box(Err(Missing)), offset: 0, line: 1, column: 1, previous_cr: False, done: False, bom: False }
}

chunk_at : ChunkState -> ChunkAt
chunk_at = |state| { offset: state.offset, line: state.line, column: state.column }

chunk_advance : ChunkState, U8 -> ChunkState
chunk_advance = |state, byte| {
	if byte == '\r' {
		{ ..state, offset: state.offset + 1, line: state.line + 1, column: 1, previous_cr: True }
	} else if byte == '\n' {
		{ ..state, offset: state.offset + 1, line: state.line + (if (state.previous_cr) 0 else 1), column: 1, previous_cr: False }
	} else {
		{ ..state, offset: state.offset + 1, column: state.column + 1, previous_cr: False }
	}
}

chunk_error : ChunkAt, Parser.Problem(Xml.Problem) -> Xml.StreamError
chunk_error = |at, problem| { offset: at.offset, line: at.line, column: at.column, problem }

chunk_fail : ChunkState, U64, ChunkAt, Parser.Problem(Xml.Problem) -> Streaming.Step(ChunkState, Xml.Event, Xml.StreamError)
chunk_fail = |state, consumed, at, problem| { state: { ..state, done: True }, consumed, outcome: Failed(chunk_error(at, problem)) }

chunk_locate : ChunkAt, List(U8), ReadFailure -> Xml.StreamError
chunk_locate = |base, bytes, failure| {
	problem = match failure {
		XmlFail(value) => value
		LimitFail(value) => { offset: value.offset, problem: ResourceLimit({ limit: value.limit, max: value.max }) }
	}
	at = locate(bytes, problem.offset, problem.problem)
	{ offset: base.offset + problem.offset, line: base.line + at.line - 1, column: if (at.line == 1) base.column + at.column - 1 else at.column, problem: at.problem }
}

chunk_utf8_left : U8, U8 -> U8
chunk_utf8_left = |left, byte| {
	if left > 0 {
		if (byte >= 0x80 and byte < 0xC0) left - 1 else 0
	} else if byte >= 0xC2 and byte <= 0xDF {
		1
	} else if byte >= 0xE0 and byte <= 0xEF {
		2
	} else if byte >= 0xF0 and byte <= 0xF4 {
		3
	} else 0
}

chunk_append_text : ChunkState, U8 -> ChunkState
chunk_append_text = |state, byte| {
	at = if (state.buffer.is_empty()) chunk_at(state) else state.piece_at
	{ ..state, buffer: state.buffer.append(byte), piece_at: at, utf8_left: chunk_utf8_left(state.utf8_left, byte) }
}

chunk_emit_text : ChunkState, U64, Bool -> Streaming.Step(ChunkState, Xml.Event, Xml.StreamError)
chunk_emit_text = |state, consumed, cdata| {
	bytes = state.buffer
	parsed = if cdata {
		wrapped = "<![CDATA[".to_utf8().concat(bytes).concat("]]>".to_utf8())
		read_source_cdata("", wrapped, 0).map_err(
			|failure| {
				problem = match failure {
					XmlFail(value) => { ..value, offset: if (value.offset >= 9) value.offset - 9 else 0 }
					LimitFail(value) => { offset: 0, problem: ResourceLimit({ limit: value.limit, max: value.max }) }
				}
				chunk_locate(state.piece_at, bytes, XmlFail(problem))
			},
		)
	} else {
		read_source_text("", bytes, 0).map_err(|failure| chunk_locate(state.piece_at, bytes, failure))
	}
	match parsed {
		Ok(value) => { state: { ..state, buffer: [], event_at: state.piece_at, skip_text_lf: bytes.last() == Ok('\r') and (if (cdata) state.mode == CData else state.mode == Data) }, consumed, outcome: Item(Text(value.val)) }
		Err(error) => { state: { ..state, done: True }, consumed, outcome: Failed(error) }
	}
}

chunk_prefix : List(U8), Str -> Bool
chunk_prefix = |bytes, target| target.to_utf8().take_first(bytes.len()) == bytes and bytes.len() <= target.count_utf8_bytes()

chunk_suffix : List(U8), Str -> Bool
chunk_suffix = |bytes, target| bytes.len() >= target.count_utf8_bytes() and bytes.drop_first(bytes.len() - target.count_utf8_bytes()) == target.to_utf8()

chunk_finish_markup : ChunkState -> Try({ state : ChunkState, event : [Emit(Xml.Event), Silent] }, Xml.StreamError)
chunk_finish_markup = |state| {
	props = Box.unbox(state.props)
	bytes = state.buffer
	at = state.piece_at
	finish = { ..state, mode: Data, buffer: [], quote: 0, brackets: 0 }
	match state.mode {
		Markup(Comment) => {
			_ = skip_comment(bytes, 0).map_err(|failure| chunk_locate(at, bytes, failure))?
			Ok({ state: finish, event: Silent })
		}
		Markup(Instruction) => {
			if starts_with_at(bytes, 0, "<?xml".to_utf8()) and is_space_at(bytes, 5) and (at.offset == 0 or (state.bom and at.offset == 3)) {
				declaration = parse_xml_declaration(bytes, 0).map_err(|failure| chunk_locate(at, bytes, failure))?
				match declaration.val.encoding {
					Ok(OtherEncoding(encoding)) => return Err(chunk_error(at, NonUtf8Encoding(encoding)))
					_ => {}
				}
				Ok({ state: { ..finish, declaration: Box.box(Ok(declaration.val)) }, event: Silent })
			} else {
				_ = skip_processing_instruction(bytes, 0, props.namespaces).map_err(|failure| chunk_locate(at, bytes, failure))?
				Ok({ state: finish, event: Silent })
			}
		}
		Markup(Tag) => {
			if bytes.get(1) == Ok('/') {
				opened = state.open.last().map_err(|_| chunk_error(at, UnexpectedEndTag))?
				# The shared validator compares names by ranges in one byte list.
				comparison = bytes.concat(opened.name.to_utf8())
				_ = read_end_tag("", comparison, 0, { start: bytes.len(), end: comparison.len() }).map_err(|failure| chunk_locate(at, bytes, failure))?
				open = state.open.drop_last(1)
				Ok({ state: { ..finish, open, context_bytes: state.context_bytes - opened.retained_bytes, closed: open.is_empty(), event_at: at }, event: Emit(End(opened.name)) })
			} else {
				if state.closed {
					return Err(chunk_error(at, TrailingContent))
				}
				if state.open.len() >= props.max_depth {
					return Err(chunk_error(at, ResourceLimit({ limit: Depth, max: props.max_depth })))
				}
				if state.elements >= props.max_elements {
					return Err(chunk_error(at, ResourceLimit({ limit: Elements, max: props.max_elements })))
				}
				if bytes.len() > props.max_context_bytes - U64.min(state.context_bytes, props.max_context_bytes) {
					return Err(chunk_error(at, ResourceLimit({ limit: ContextBytes, max: props.max_context_bytes })))
				}
				tag = parse_start_tag("", bytes, 0, props.max_attributes).map_err(|failure| chunk_locate(at, bytes, failure))?
				Ok({ state: { ..finish, open: state.open.append({ name: tag.name, retained_bytes: bytes.len() }), context_bytes: state.context_bytes + bytes.len(), elements: state.elements + 1, pending_end: tag.empty, event_at: at }, event: Emit(Start({ name: tag.name, attributes: tag.attributes })) })
			}
		}
		_ => Err(chunk_error(at, IncompleteMarkup))
	}
}

chunk_step : ChunkState, List(U8), Streaming.InputEnd -> Streaming.Step(ChunkState, Xml.Event, Xml.StreamError)
chunk_step = |initial, bytes, end| {
	props = Box.unbox(initial.props)
	var $state = initial
	var $index = 0.U64
	if $state.done {
		return { state: $state, consumed: 0, outcome: Done }
	}
	if props.max_text_bytes < 4 {
		return chunk_fail($state, 0, chunk_at($state), BadTextFragmentSize({ minimum: 4, actual: props.max_text_bytes }))
	}
	if $state.pending_end {
		opened = $state.open.last() ?? { name: "", retained_bytes: 0 }
		open = $state.open.drop_last(1)
		return { state: { ..$state, open, context_bytes: $state.context_bytes - opened.retained_bytes, closed: open.is_empty(), pending_end: False }, consumed: 0, outcome: Item(End(opened.name)) }
	}
	while $index < bytes.len() {
		byte = bytes.get($index) ?? 0
		if $state.mode == Data and $state.buffer.is_empty() and !$state.reference and byte == '<' {
			match chunk_direct_tag(bytes, $index, props.max_token_bytes, props.max_attributes) {
				Start(tag) if !$state.closed and $state.open.len() < props.max_depth and $state.elements < props.max_elements and tag.pos - $index <= props.max_context_bytes - U64.min($state.context_bytes, props.max_context_bytes) => {
					length = tag.pos - $index
					at = chunk_at($state)
					end_at = position(bytes.sublist({ start: $index, len: length }), length)
					name = chunk_owned_text(tag.name)
					empty = tag.empty
					attributes = chunk_owned_attributes(tag.attributes)
					return { state: { ..$state, open: $state.open.append({ name, retained_bytes: length }), context_bytes: $state.context_bytes + length, elements: $state.elements + 1, pending_end: empty, event_at: at, skip_text_lf: False, brackets: 0, offset: $state.offset + length, line: $state.line + end_at.line - 1, column: if (end_at.line == 1) $state.column + length else end_at.column, previous_cr: False }, consumed: $index + length, outcome: Item(Start({ name, attributes })) }
				}
				End(tag) => {
					match $state.open.last() {
						Ok(opened) if opened.name == tag.name => {
							length = tag.pos - $index
							at = chunk_at($state)
							end_at = position(bytes.sublist({ start: $index, len: length }), length)
							open = $state.open.drop_last(1)
							return { state: { ..$state, open, context_bytes: $state.context_bytes - opened.retained_bytes, closed: open.is_empty(), event_at: at, skip_text_lf: False, brackets: 0, offset: $state.offset + length, line: $state.line + end_at.line - 1, column: if (end_at.line == 1) $state.column + length else end_at.column, previous_cr: False }, consumed: $index + length, outcome: Item(End(opened.name)) }
						}
						_ => {}
					}
				}
				_ => {}
			}
		}
		match chunk_bulk($state, bytes, $index) {
			TextRun(len) | CDataRun(len) => {
				cdata = $state.mode == CData
				at = if ($state.buffer.is_empty()) chunk_at($state) else $state.piece_at
				$state = { ..$state, buffer: $state.buffer.concat(bytes.sublist({ start: $index, len })), piece_at: at, offset: $state.offset + len, column: $state.column + len, previous_cr: False, skip_text_lf: False, reference_bytes: 0 }
				$index = $index + len
				if $state.buffer.len() >= props.max_text_bytes - 3 {
					return chunk_emit_text($state, $index, cdata)
				}
			}
			TokenRun(len) => {
				$state = { ..$state, buffer: $state.buffer.concat(bytes.sublist({ start: $index, len })), offset: $state.offset + len, column: $state.column + len, previous_cr: False }
				$index = $index + len
			}
			Slow => match $state.mode {
				Initial => {
					$state = { ..$state, mode: if (byte == 0xEF) Bom else Data }
				}
				Bom => {
					$state = chunk_advance({ ..$state, buffer: $state.buffer.append(byte) }, byte)
					$index = $index + 1
					if $state.buffer.len() == 3 {
						if $state.buffer != [0xEF, 0xBB, 0xBF] {
							return chunk_fail($state, $index, { offset: 0, line: 1, column: 1 }, InvalidBom($state.buffer))
						}
						$state = { ..$state, mode: Data, buffer: [], bom: True }
					}
				}
				Data => {
					if $state.skip_text_lf and byte == '\n' {
						$state = chunk_advance({ ..$state, skip_text_lf: False }, byte)
						$index = $index + 1
					} else if byte == '<' and !$state.reference {
						if !$state.buffer.is_empty() {
							return chunk_emit_text($state, $index, False)
						}
						match chunk_complete_tag(bytes, $index, props.max_token_bytes) {
							Ok(stop) => {
								# concat produces an owned bounded token; names cannot retain
								# the potentially much larger source chunk after it is read.
								token = [].concat(bytes.sublist({ start: $index, len: stop - $index }))
								at = position(token, token.len())
								$state = { ..$state, mode: Markup(Tag), buffer: token, piece_at: chunk_at($state), skip_text_lf: False, brackets: 0, offset: $state.offset + stop - $index, line: $state.line + at.line - 1, column: if (at.line == 1) $state.column + at.column - 1 else at.column, previous_cr: False }
								$index = stop
								match chunk_finish_markup($state) {
									Ok({ state, event: Emit(event) }) => return { state, consumed: $index, outcome: Item(event) }
									Ok({ state, event: Silent }) => {
										$state = state
									}
									Err(error) => return { state: { ..$state, done: True }, consumed: $index, outcome: Failed(error) }
								}
							}
							Err(Unfinished) => {
								$state = chunk_advance({ ..$state, mode: Markup(Prefix), buffer: ['<'], piece_at: chunk_at($state), skip_text_lf: False, brackets: 0 }, byte)
								$index = $index + 1
							}
						}
					} else if $state.open.is_empty() {
						if !is_space(byte) {
							return chunk_fail($state, $index, chunk_at($state), if ($state.closed) TrailingContent else RootText)
						}
						$state = chunk_advance($state, byte)
						$index = $index + 1
					} else {
						if $state.reference and $state.reference_bytes >= props.max_token_bytes {
							return chunk_fail($state, $index, chunk_at($state), ResourceLimit({ limit: ReferenceBytes, max: props.max_token_bytes }))
						}
						if byte == '>' and $state.brackets >= 2 {
							return chunk_fail($state, $index, { offset: $state.offset - 2, line: $state.line, column: $state.column - 2 }, ForbiddenCDataEnd)
						}
						if $state.buffer.is_empty() {
							$state = { ..$state, piece_at: chunk_at($state) }
						}
						$state = chunk_append_text({ ..$state, skip_text_lf: False }, byte)
						$state = { ..$state, reference: if ($state.reference) byte != ';' else byte == '&', reference_bytes: if ($state.reference) $state.reference_bytes + 1 else if (byte == '&') 1 else 0, brackets: if (byte == ']') U8.min($state.brackets + 1, 2) else 0 }
						$state = chunk_advance($state, byte)
						$index = $index + 1
						if !$state.reference and $state.utf8_left == 0 and $state.buffer.len() >= props.max_text_bytes - 3 {
							return chunk_emit_text($state, $index, False)
						}
					}
				}
				Markup(kind) => {
					if $state.buffer.len() >= props.max_token_bytes {
						return chunk_fail($state, $index, $state.piece_at, ResourceLimit({ limit: TokenBytes, max: props.max_token_bytes }))
					}
					$state = chunk_advance({ ..$state, buffer: $state.buffer.append(byte) }, byte)
					$index = $index + 1
					var $complete = False
					match kind {
						Prefix => {
							if $state.buffer.len() == 2 and byte == '?' {
								$state = { ..$state, mode: Markup(Instruction) }
							} else if $state.buffer.len() == 2 and byte != '!' {
								$state = { ..$state, mode: Markup(Tag) }
								$complete = byte == '>'
							} else if $state.buffer == "<!--".to_utf8() {
								$state = { ..$state, mode: Markup(Comment) }
							} else if $state.buffer == "<![CDATA[".to_utf8() {
								if $state.open.is_empty() {
									return chunk_fail($state, $index, $state.piece_at, CDataOutsideRoot)
								}
								$state = { ..$state, mode: CData, buffer: [], cdata_brackets: 0, skip_text_lf: False }
							} else if !chunk_prefix($state.buffer, "<!--") and !chunk_prefix($state.buffer, "<![CDATA[") {
								return chunk_fail($state, $index, $state.piece_at, ExpectedMarkupDeclaration)
							}
						}
						Tag => {
							if $state.quote != 0 {
								if byte == $state.quote {
									$state = { ..$state, quote: 0 }
								}
							} else if byte == '\'' or byte == '"' {
								$state = { ..$state, quote: byte }
							} else {
								$complete = byte == '>'
							}
						}
						Comment => {
							$complete = $state.buffer.len() >= 7 and chunk_suffix($state.buffer, "-->")
						}
						Instruction => {
							$complete = chunk_suffix($state.buffer, "?>")
						}
					}
					if $complete {
						match chunk_finish_markup($state) {
							Ok(result) => {
								$state = result.state
								match result.event {
									Emit(event) => return { state: $state, consumed: $index, outcome: Item(event) }
									Silent => {}
								}
							}
							Err(error) => return { state: { ..$state, done: True }, consumed: $index, outcome: Failed(error) }
						}
					}
				}
				CData => {
					# Append delayed brackets directly in record updates so the
					# unfinished text buffer stays uniquely owned.
					if $state.skip_text_lf and byte == '\n' {
						$state = chunk_advance({ ..$state, skip_text_lf: False }, byte)
						$index = $index + 1
					} else if byte == ']' {
						# Only the last two brackets can belong to the terminator.
						if $state.cdata_brackets == 2 {
							at = if ($state.buffer.is_empty()) { offset: $state.offset - $state.cdata_brackets, line: $state.line, column: $state.column - $state.cdata_brackets } else $state.piece_at
							left = chunk_utf8_left($state.utf8_left, ']')
							$state = { ..$state, buffer: $state.buffer.append(']'), piece_at: at, utf8_left: left }
						} else {
							$state = { ..$state, cdata_brackets: $state.cdata_brackets + 1 }
						}
						$state = chunk_advance({ ..$state, skip_text_lf: False }, byte)
						$index = $index + 1
					} else if byte == '>' and $state.cdata_brackets == 2 {
						$state = chunk_advance({ ..$state, mode: Data, cdata_brackets: 0, skip_text_lf: False, brackets: 0 }, byte)
						$index = $index + 1
						if !$state.buffer.is_empty() {
							return chunk_emit_text($state, $index, True)
						}
					} else {
						while $state.cdata_brackets > 0 {
							at = if ($state.buffer.is_empty()) { offset: $state.offset - $state.cdata_brackets, line: $state.line, column: $state.column - $state.cdata_brackets } else $state.piece_at
							remaining = $state.cdata_brackets - 1
							left = chunk_utf8_left($state.utf8_left, ']')
							$state = { ..$state, buffer: $state.buffer.append(']'), piece_at: at, utf8_left: left, cdata_brackets: remaining }
							if $state.buffer.len() >= props.max_text_bytes - 3 {
								return chunk_emit_text($state, $index, True)
							}
						}
						$state = chunk_append_text({ ..$state, skip_text_lf: False }, byte)
						$state = chunk_advance($state, byte)
						$index = $index + 1
					}
					if $state.mode == CData and $state.utf8_left == 0 and $state.buffer.len() >= props.max_text_bytes - 3 {
						return chunk_emit_text($state, $index, True)
					}
				}
			}
		}
	}
	if end == More {
		return { state: $state, consumed: $index, outcome: NeedInput }
	}
	if $state.mode == Data and !$state.buffer.is_empty() {
		return chunk_emit_text($state, $index, False)
	}
	if $state.mode != Data or !$state.closed {
		return chunk_fail($state, $index, chunk_at($state), unfinished_problem($state))
	}
	{ state: { ..$state, done: True }, consumed: $index, outcome: Done }
}

chunk_test_read : List(List(U8)), Xml.StreamProps -> _
chunk_test_read = |chunks, props| Streaming.collect(Streaming.read_iter(chunks.iter(), Xml.decoder(props)))

chunk_test_splits : Str, Xml.StreamProps -> Bool
chunk_test_splits = |source, props| {
	bytes = source.to_utf8()
	wanted = chunk_test_read([bytes], props)
	var $same = chunk_test_read([[]].concat(bytes.map(|byte| [byte])).append([]), props) == wanted
	for split in (0.U64).to(bytes.len() + 1) {
		$same = $same and chunk_test_read([bytes.take_first(split), [], bytes.drop_first(split)], props) == wanted
	}
	$same
}

expect chunk_test_read(["<a x='&amp;'>hello<b/>world</a>".to_utf8()], {}) == Ok([
	Start({ name: "a", attributes: [{ name: "x", value: "&" }] }),
	Text("hello"),
	Start({ name: "b", attributes: [] }),
	End("b"),
	Text("world"),
	End("a"),
])

expect [
	"<a/>",
	"\u(FEFF)<?xml version='1.0' encoding='UTF-8'?><a x='é&#13;\r\nx'>😀&amp;\r\ntext<![CDATA[<&😀\r\n]]]>]</a><!-- tail -->",
	"<?before ok?><a>before<!-- a > b -->after<?inside data?><b></b></a><?after?>",
	"<a>abc\r\nxyz&#13;\n<![CDATA[\r\n]]>&#x1F600;</a>",
	"<a><![CDATA[]]]]]]]]]]]]]]]]]]]]]]]]]]]]></a>",
].all(|source| chunk_test_read([source.to_utf8()], { max_text_bytes: 4 }).is_ok() and chunk_test_splits(source, { max_text_bytes: 4 }))

expect {
	values = chunk_test_read(["<a>A\r\nB\r&#10;C\r<![CDATA[\n]]>D</a>".to_utf8()], { max_text_bytes: 4 })?
	Str.join_with(
		values.map(
			|event| match event {
				Text(value) => value
				_ => ""
			},
		),
		"",
	) == "A\nB\n\nC\n\nD"
}

expect [
	"",
	"<a>",
	"<a></b>",
	"<a/><b/>",
	"<a/>x",
	"<a/>\u(FEFF)",
	"<a x='1' x='2'/>",
	"<a>&unknown;</a>",
	"<a>&#0;</a>",
	"<a>]]></a>",
	"<a>abc]]></a>",
	"<a><!-- bad -- comment --></a>",
	"<a><?xml version='1.0'?></a>",
	"   <?xml version='1.0'?><a/>",
	"<?xml version='1.0' encoding='UTF-16'?><a/>",
	"<!DOCTYPE a><a/>",
	"<![CDATA[x]]><a/>",
	"<a><![CDATA[unterminated</a>",
	"<a x='unterminated>",
	"<a><!--unterminated",
	"<a><?unfinished",
	"<a>&unfinished",
].all(|source| chunk_test_read([source.to_utf8()], { max_text_bytes: 4 }).is_err() and chunk_test_splits(source, { max_text_bytes: 4 }))

expect [[0xEF.U8], [0xEF, 0xBB], [0xEF, 0xBB, 0], "<a>".to_utf8().concat([0xF0, 0x9F]).concat("</a>".to_utf8()), "<a>".to_utf8().concat([0xFF]).concat("</a>".to_utf8())].all(
	|bytes| {
		chunk_test_read([bytes], {}).is_err() and chunk_test_read(bytes.map(|byte| [byte]), {}).is_err()
	},
)

expect {
	read = |source, props| chunk_test_read([source.to_utf8()], props)
	read("<a/>", { max_token_bytes: 4 }).is_ok()
		and read("<a/>", { max_token_bytes: 3 }).is_err()
		and read("<a><b/></a>", { max_depth: 1 }).is_err()
		and read("<a><b/></a>", { max_depth: 2 }).is_ok()
		and read("<a><b/></a>", { max_elements: 1 }).is_err()
		and read("<a x='1'/>", { max_attributes: 0 }).is_err()
		and read("<a/>", { max_context_bytes: 4 }).is_ok()
		and read("<a><b/></a>", { max_context_bytes: 6 }).is_err()
		and read("<a>&#00000000000065;</a>", { max_token_bytes: 8 }).is_err()
		and read("<a/>", { max_text_bytes: 3 }).is_err()
}

expect {
	source = "<root><![CDATA[${Str.repeat("x", 10000)}]]>${Str.repeat("é", 10000)}</root>"
	values = chunk_test_read([source.to_utf8()], { max_text_bytes: 32, max_token_bytes: 16 })?
	values.all(
		|event| match event {
			Text(text) => text.count_utf8_bytes() <= 32
			_ => True
		},
	)
		and Str.join_with(
			values.map(
				|event| match event {
					Text(text) => text
					_ => ""
				},
			),
			"",
		).count_utf8_bytes() == 30000
}

expect {
	first = (Xml.decoder({})).step("<root/>unread".to_utf8(), More)
	match first.outcome {
		Item(Start({ name: "root", .. })) => first.consumed == 7
		_ => False
	}
}

expect {
	result = chunk_test_read(["<a>\r\n<b/>\r\n</wrong>".to_utf8()], {})
	match result {
		Err(ReadFailed(ParseErr(error))) => error.offset == 11 and error.line == 3 and error.column == 1
		_ => False
	}
}

expect ["<r><![CDATA[\r]]>\n</r>", "<r>\r<![CDATA[\n]]></r>"].all(
	|source| {
		parsed = chunk_test_read([source.to_utf8()], { max_text_bytes: 4 }) ?? return False
		Str.join_with(
			parsed.map(
				|event| match event {
					Text(value) => value
					_ => ""
				},
			),
			"",
		) == "\n\n"
			and chunk_test_splits(source, { max_text_bytes: 4 })
	},
)

expect {
	source = "<r><![CDATA[${Str.repeat("x", 28)}]]😀]]></r>"
	parsed = chunk_test_read([source.to_utf8()], { max_text_bytes: 32 })?
	parsed.all(
		|event| match event {
			Text(value) => value.count_utf8_bytes() <= 32
			_ => True
		},
	)
		and chunk_test_splits(source, { max_text_bytes: 4 })
}

expect chunk_test_read(["\r\r<A /><!-->Z1\r\n1-->".to_utf8()], {}).is_ok()
	and chunk_test_splits("\r\r<A /><!-->Z1\r\n1-->", {})

expect {
	match chunk_test_read(["<a>]]></a>".to_utf8()], { max_text_bytes: 4 }) {
		Err(ReadFailed(ParseErr({ offset: 3, line: 1, column: 4, .. }))) => True
		_ => False
	}
}

expect {
	source = "<r>${Str.repeat("<i/>", 1000)}</r>"
	chunk_test_read([source.to_utf8()], { max_context_bytes: 7, max_token_bytes: 4 }).is_ok()
}

expect {
	first = (Xml.decoder({})).step("<?xml version='1.0'?>\n<r/>".to_utf8(), More)
	Xml.decoder_declaration(first.state) == Ok({ version: { major: 1, minor: 0 }, encoding: Err(Missing) })
		and Xml.decoder_location(first.state) == { offset: 22, line: 2, column: 1 }
}

ChunkRun : [TextRun(U64), CDataRun(U64), TokenRun(U64), Slow]

chunk_tag_class : Utf8.ByteClass
chunk_tag_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '>' and b != '"' and b != '\'')

chunk_double_class : Utf8.ByteClass
chunk_double_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '"')

chunk_single_class : Utf8.ByteClass
chunk_single_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '\'')

chunk_comment_class : Utf8.ByteClass
chunk_comment_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '-' and b != '>')

chunk_instruction_class : Utf8.ByteClass
chunk_instruction_class = Utf8.ByteClass.from_predicate(|b| b >= 0x20 and b < 0x80 and b != '?' and b != '>')

# Bulk runs have no line endings, UTF-8 boundaries or framing transitions.
# Their cap is the same byte at which scalar scanning would emit or fail.
chunk_bulk : ChunkState, List(U8), U64 -> ChunkRun
chunk_bulk = |state, bytes, index| {
	props = Box.unbox(state.props)
	match state.mode {
		Data if !state.open.is_empty() and !state.reference and state.utf8_left == 0 and state.brackets == 0 and state.buffer.len() < props.max_text_bytes - 3 => {
			cap = index + U64.min(bytes.len() - index, props.max_text_bytes - 3 - state.buffer.len())
			end = MarkupScan.xml_text(bytes.take_first(cap), index)
			if (end > index) TextRun(end - index) else Slow
		}
		CData if state.cdata_brackets == 0 and state.utf8_left == 0 and state.buffer.len() < props.max_text_bytes - 3 => {
			cap = index + U64.min(bytes.len() - index, props.max_text_bytes - 3 - state.buffer.len())
			end = MarkupScan.cdata_text(bytes.take_first(cap), index)
			if (end > index) CDataRun(end - index) else Slow
		}
		Markup(kind) if kind != Prefix and state.buffer.len() < props.max_token_bytes => {
			class = match kind {
				Tag => if (state.quote == '"') chunk_double_class else if (state.quote == '\'') chunk_single_class else chunk_tag_class
				Comment => chunk_comment_class
				_ => chunk_instruction_class
			}
			cap = index + U64.min(bytes.len() - index, props.max_token_bytes - state.buffer.len())
			end = Utf8.skip_class(bytes.take_first(cap), index, class)
			if (end > index) TokenRun(end - index) else Slow
		}
		_ => Slow
	}
}

chunk_tag_boundaries : Utf8.ByteClass
chunk_tag_boundaries = Utf8.ByteClass.from_bytes(['>', '"', '\''])

chunk_double_boundary : Utf8.ByteClass
chunk_double_boundary = Utf8.ByteClass.from_bytes(['"'])

chunk_single_boundary : Utf8.ByteClass
chunk_single_boundary = Utf8.ByteClass.from_bytes(['\''])

# Only use this path when a complete bounded ordinary tag is available.
# Comments, declarations and unfinished tags retain the incremental scanner.
chunk_complete_tag : List(U8), U64, U64 -> Try(U64, [Unfinished])
chunk_complete_tag = |bytes, start, maximum| {
	first = bytes.get(start + 1) ?? 0
	if first == '!' or first == '?' or first == '\'' or first == '"' {
		return Err(Unfinished)
	}
	cap = start + U64.min(bytes.len() - start, maximum)
	bounded = bytes.take_first(cap)
	var $index = start + 1
	var $quote = 0.U8
	while $index < cap {
		class = if ($quote == '"') chunk_double_boundary else if ($quote == '\'') chunk_single_boundary else chunk_tag_boundaries
		$index = Utf8.find_any(bounded, $index, class)
		if $index >= cap {
			return Err(Unfinished)
		}
		byte = bytes.get($index) ?? 0
		if $quote != 0 {
			$quote = 0
		} else if byte == '>' {
			return Ok($index + 1)
		} else {
			$quote = byte
		}
		$index = $index + 1
	}
	Err(Unfinished)
}

expect [
	"<r><a>text&amp;<![CDATA[\r\n]]>more<!-- tail --></a><b></b></r>",
	"<r><a x='one>two'>text</a>\r\n<b y=\"three>four\"/></r>",
	"<r><a>before<b/>after</a><é a='v'>😀</é></r>",
	"<r><?i a>b?><!-- c > d --><a/></r>",
].all(|source| chunk_test_read([source.to_utf8()], {}).is_ok() and chunk_test_splits(source, {}))

expect Xml.parse("<r>\n<a>x</a>\n<b/></r>", { max_elements: 2 }) == Err(LimitExceeded({ limit: Elements, max: 2, line: 3, column: 1 }))

expect Xml.parse("<r>\n<a>x</a></r>", { max_depth: 1 }) == Err(LimitExceeded({ limit: Depth, max: 1, line: 2, column: 1 }))

expect Xml.parse_str("</>") == Err(InvalidXml({ line: 1, column: 3, problem: ExpectedEndTagName }))

expect ["<''a>", "<\"\"a>", "<p:a>", "<a>tail"].all(|source| chunk_test_read([source.to_utf8()], {}).is_err() and chunk_test_splits(source, {}))

expect chunk_test_read(["<r><a>text</a><b/></r>".to_utf8()], { max_token_bytes: U64.highest, max_text_bytes: U64.highest }).is_ok()
	and chunk_test_splits("<r><a>text</a><b/></r>", { max_token_bytes: U64.highest, max_text_bytes: U64.highest })

# Each entity has its own byte budget, including '&' and ';'. A completed
# entity cannot spend the following entity's allowance across any boundary.
expect [
	{ source: "<a>&amp;&amp;</a>", budget: 5.U64, text: "&&" },
	{ source: "<a>&#65;&#66;</a>", budget: 5, text: "AB" },
	{ source: "<a>&amp;<b/>&amp;</a>", budget: 5, text: "&&" },
	{ source: "<a>&amp;<b></b>&amp;</a>", budget: 5, text: "&&" },
	{ source: "<a>&#065;&#066;</a>", budget: 6, text: "AB" },
	{ source: "<a>&#x41;&#x42;</a>", budget: 6, text: "AB" },
	{ source: "<a>&amp;&apos;&quot;</a>", budget: 6, text: "&'\"" },
	{ source: "<a>&#065;<b/>&#066;</a>", budget: 6, text: "AB" },
	{ source: "<a>&#000065;<![CDATA[]]>&#000066;</a>", budget: 9, text: "AB" },
	{ source: "<a>&#000065;<![CDATA[x]]>&#000066;</a>", budget: 9, text: "AxB" },
	{ source: "<a>&#000065;<![CDATA[\r\n]]>&#000066;</a>", budget: 9, text: "A\nB" },
].all(
	|case| {
		props : Xml.StreamProps
		props = { max_token_bytes: case.budget, max_text_bytes: 4 }
		values = chunk_test_read([case.source.to_utf8()], props) ?? return False
		text = Str.join_with(
			values.map(
				|event| match event {
					Text(value) => value
					_ => ""
				},
			),
			"",
		)
		text == case.text and chunk_test_splits(case.source, props)
	},
)

expect [
	{ source: "<a>&amp;&#065;</a>", budget: 5.U64, offset: 13.U64 },
	{ source: "<a>&apos;&#0065;</a>", budget: 6, offset: 15 },
	{ source: "<a>&#000065;<![CDATA[]]>&#0000066;</a>", budget: 9, offset: 33 },
].all(
	|case| {
		props : Xml.StreamProps
		props = { max_token_bytes: case.budget, max_text_bytes: 4 }
		match chunk_test_read([case.source.to_utf8()], props) {
			Err(ReadFailed(ParseErr(problem))) =>
				problem.problem == ResourceLimit({ limit: ReferenceBytes, max: case.budget })
					and problem.offset == case.offset
					and chunk_test_splits(case.source, props)
			_ => False
		}
	},
)

DirectTag : [Start(StartTag), End({ name : Str, pos : U64 }), Fallback]

# Parse directly from the current bounded source. Only a complete successful
# token takes this path; unfinished or malformed syntax keeps the incremental
# framing path and its diagnostics.
chunk_direct_tag : List(U8), U64, U64, U64 -> DirectTag
chunk_direct_tag = |bytes, start, token_limit, attribute_limit| {
	first = bytes.get(start + 1) ?? 0
	if first == '!' or first == '?' or first == '\'' or first == '"' {
		return Fallback
	}
	cap = start + U64.min(bytes.len() - start, token_limit)
	bounded = bytes.take_first(cap)
	if first == '/' {
		name = parse_name("", bounded, start + 2) ?? return Fallback
		close = skip_space(bounded, name.pos)
		if bounded.get(close) != Ok('>') {
			return Fallback
		}
		End({ name: name.val, pos: close + 1 })
	} else {
		match parse_start_tag("", bounded, start, attribute_limit) {
			Ok(tag) => Start(tag)
			Err(_) => Fallback
		}
	}
}

# Short names/values are already inline. A long emitted string must own its
# bounded token bytes rather than retain the caller's entire source chunk.
chunk_owned_text : Str -> Str
chunk_owned_text = |text| if (text.count_utf8_bytes() > 11) text.release_excess_capacity() else text

chunk_owned_attributes : List(Xml.Attribute) -> List(Xml.Attribute)
chunk_owned_attributes = |attributes| {
	if attributes.all(|attribute| attribute.name.count_utf8_bytes() <= 11 and attribute.value.count_utf8_bytes() <= 11) {
		attributes
	} else attributes.map(|attribute| { name: chunk_owned_text(attribute.name), value: chunk_owned_text(attribute.value) })
}

# Namespace resolution consumes raw events individually, allowing its names and
# attributes to transfer into resolved events without a second retained queue.
ChunkDecoder : [Single(ChunkState), Batch(ChunkBatch)]

chunk_decoder_step : ChunkDecoder, List(U8), Streaming.InputEnd -> Streaming.Step(ChunkDecoder, Xml.Event, Xml.StreamError)
chunk_decoder_step = |current, bytes, end| match current {
	Single(state) => {
		next = chunk_step(state, bytes, end)
		{ state: Single(next.state), consumed: next.consumed, outcome: next.outcome }
	}
	Batch(batch) => {
		next = chunk_batch_step(batch, bytes, end)
		{ state: Batch(next.state), consumed: next.consumed, outcome: next.outcome }
	}
}

ChunkEvent : { event : Xml.Event, location : ChunkAt }

ChunkBatch : { decoder : Box(ChunkState), events : List(ChunkEvent), index : U64, event_at : ChunkAt }

chunk_batch_initial : Xml.StreamProps -> ChunkBatch
chunk_batch_initial = |props| { decoder: Box.box(chunk_initial(props)), events: [], index: 0, event_at: { offset: 0, line: 1, column: 1 } }

chunk_batch_step : ChunkBatch, List(U8), Streaming.InputEnd -> Streaming.Step(ChunkBatch, Xml.Event, Xml.StreamError)
chunk_batch_step = |batch, bytes, end| {
	if batch.index < batch.events.len() {
		entry = batch.events.get(batch.index) ?? { event: Text(""), location: batch.event_at }
		return { state: { ..batch, index: batch.index + 1, event_at: entry.location }, consumed: 0, outcome: Item(entry.event) }
	}
	decoded = Box.unbox(batch.decoder)
	var $empty = batch.events
	while !$empty.is_empty() {
		$empty = $empty.drop_last(1)
	}
	filled = chunk_fill(decoded, bytes, $empty)
	if !filled.events.is_empty() {
		first = filled.events.first() ?? { event: Text(""), location: batch.event_at }
		return { state: { decoder: Box.box(filled.state), events: filled.events, index: 1, event_at: first.location }, consumed: filled.consumed, outcome: Item(first.event) }
	}
	next = chunk_step(filled.state, bytes, end)
	{ state: { decoder: Box.box(next.state), events: filled.events, index: 0, event_at: next.state.event_at }, consumed: next.consumed, outcome: next.outcome }
}

# Parse a small bounded event batch using scalar cursor fields, so common
# complete tokens do not move the full decoder through a Step per event.
# Anything unfinished, normalized or malformed uses the incremental decoder.
chunk_fill : ChunkState, List(U8), List(ChunkEvent) -> { state : ChunkState, consumed : U64, events : List(ChunkEvent) }
chunk_fill = |initial, bytes, empty| {
	props = Box.unbox(initial.props)
	if initial.done or initial.mode != Data or !initial.buffer.is_empty() or initial.reference or initial.utf8_left != 0 or initial.skip_text_lf or props.max_text_bytes < 4 {
		return { state: initial, consumed: 0, events: empty }
	}
	var $open = initial.open
	var $context_bytes = initial.context_bytes
	var $elements = initial.elements
	var $pending_end = initial.pending_end
	var $closed = initial.closed
	var $offset = initial.offset
	var $line = initial.line
	var $column = initial.column
	var $previous_cr = initial.previous_cr
	var $event_at = initial.event_at
	var $piece_at = initial.piece_at
	var $index = 0.U64
	var $events = empty
	while $events.len() < 8 {
		if $pending_end {
			opened = $open.last() ?? { name: "", retained_bytes: 0 }
			$open = $open.drop_last(1)
			$context_bytes = $context_bytes - opened.retained_bytes
			$pending_end = False
			$closed = $open.is_empty()
			$events = $events.append({ event: End(opened.name), location: $event_at })
		} else if $index >= bytes.len() {
			break
		} else if bytes.get($index) == Ok('<') {
			match chunk_direct_tag(bytes, $index, props.max_token_bytes, props.max_attributes) {
				Start(tag) if !$closed and $open.len() < props.max_depth and $elements < props.max_elements and tag.pos - $index <= props.max_context_bytes - U64.min($context_bytes, props.max_context_bytes) => {
					length = tag.pos - $index
					$event_at = { offset: $offset, line: $line, column: $column }
					$piece_at = $event_at
					at = position(bytes.sublist({ start: $index, len: length }), length)
					name = chunk_owned_text(tag.name)
					$pending_end = tag.empty
					stop = tag.pos
					attributes = chunk_owned_attributes(tag.attributes)
					$open = $open.append({ name, retained_bytes: length })
					$context_bytes = $context_bytes + length
					$elements = $elements + 1
					$offset = $offset + length
					$line = $line + at.line - 1
					$column = if (at.line == 1) $column + length else at.column
					$previous_cr = False
					$index = stop
					$events = $events.append({ event: Start({ name, attributes }), location: $event_at })
				}
				End(tag) => {
					match $open.last() {
						Ok(opened) if opened.name == tag.name => {
							length = tag.pos - $index
							$event_at = { offset: $offset, line: $line, column: $column }
							$piece_at = $event_at
							at = position(bytes.sublist({ start: $index, len: length }), length)
							$open = $open.drop_last(1)
							$context_bytes = $context_bytes - opened.retained_bytes
							$closed = $open.is_empty()
							$offset = $offset + length
							$line = $line + at.line - 1
							$column = if (at.line == 1) $column + length else at.column
							$previous_cr = False
							$index = tag.pos
							$events = $events.append({ event: End(opened.name), location: $event_at })
						}
						_ => break
					}
				}
				_ => break
			}
		} else if !$open.is_empty() and initial.brackets == 0 {
			text = chunk_direct_text(bytes, $index, props.max_text_bytes, props.max_token_bytes) ?? break
			length = text.pos - $index
			part = bytes.sublist({ start: $index, len: length })
			at = position(part, length)
			$event_at = { offset: $offset, line: $line, column: $column }
			$piece_at = $event_at
			$offset = $offset + length
			$line = $line + at.line - 1 - (if ($previous_cr and part.first() == Ok('\n')) 1 else 0)
			$column = if (at.line == 1) $column + length else at.column
			$previous_cr = part.last() == Ok('\r')
			$index = text.pos
			$events = $events.append({ event: Text(text.value), location: $event_at })
		} else {
			break
		}
	}
	{ state: { ..initial, open: $open, context_bytes: $context_bytes, elements: $elements, pending_end: $pending_end, closed: $closed, offset: $offset, line: $line, column: $column, previous_cr: $previous_cr, event_at: $event_at, piece_at: $piece_at }, consumed: $index, events: $events }
}

chunk_text_boundaries : Utf8.ByteClass
chunk_text_boundaries = Utf8.ByteClass.from_bytes(['<', '&', '\r'])

chunk_reference_boundary : Utf8.ByteClass
chunk_reference_boundary = Utf8.ByteClass.from_bytes([';'])

# Frame a complete text event before decoding it. Entity limits are checked
# before semantic parsing can construct an error or normalized output.
chunk_direct_text : List(U8), U64, U64, U64 -> Try({ value : Str, pos : U64 }, [Fallback])
chunk_direct_text = |bytes, start, maximum, reference_limit| {
	cap = start + U64.min(bytes.len() - start, maximum - 3)
	bounded = bytes.take_first(cap)
	var $index = start
	var $rewritten = False
	while $index < cap {
		$index = Utf8.find_any(bounded, $index, chunk_text_boundaries)
		if $index >= cap {
			return Err(Fallback)
		}
		byte = bytes.get($index) ?? 0
		if byte == '<' {
			parsed = read_source_text("", bounded, start).map_err(|_| Fallback)?
			if parsed.pos != $index {
				return Err(Fallback)
			}
			return Ok({ value: if ($rewritten) parsed.val else chunk_owned_text(parsed.val), pos: parsed.pos })
		} else if byte == '&' {
			limit = $index + U64.min(cap - $index, reference_limit)
			end = Utf8.find_any(bytes.take_first(limit), $index + 1, chunk_reference_boundary)
			if end >= limit {
				return Err(Fallback)
			}
			$index = end + 1
			$rewritten = True
		} else {
			$index = $index + 1
			$rewritten = True
		}
	}
	Err(Fallback)
}

unfinished_problem : ChunkState -> Parser.Problem(Xml.Problem)
unfinished_problem = |state| {
	construct = match state.mode {
		Initial => Document
		Bom => ByteOrderMark
		Data => if (state.reference) Reference else Text
		Markup(Comment) => Comment
		Markup(Instruction) => Instruction
		Markup(_) => Tag
		CData => CData
	}
	expected_end_tag = state.open.last().map_ok(|entry| entry.name).map_err(|_| Missing)
	UnfinishedDocument({ construct, expected_end_tag })
}

## A malformed suffix cannot suppress already validated batched events.
expect {
	bytes = "<r><a/><b/></bad>".to_utf8()
	var $decoder = Xml.decoder({})
	var $at = 0.U64
	var $events = []
	var $locations = []
	var $failure = Err(Missing)
	for _ in (0.U64).to(8) {
		next = $decoder.step(bytes.drop_first($at), More)
		$decoder = next.state
		$at = $at + next.consumed
		match next.outcome {
			Item(event) => {
				$events = $events.append(event)
				$locations = $locations.append(Xml.decoder_location($decoder).offset)
			}
			Failed(problem) => {
				$failure = Ok(problem)
				break
			}
			_ => break
		}
	}
	$events == [Start({ name: "r", attributes: [] }), Start({ name: "a", attributes: [] }), End("a"), Start({ name: "b", attributes: [] }), End("b")]
		and $locations == [0, 3, 3, 7, 7]
		and match $failure {
			Ok({ problem: EndTagMismatch({ actual: "bad", expected: "r" }), offset: 11, .. }) => True
			_ => False
		}
}
