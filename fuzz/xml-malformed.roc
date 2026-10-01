app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Utf8
import parser.Xml
import XmlGen

## Not-well-formed property: take a well-formed generated document (see
## XmlGen), apply one well-formedness violation at a known place, and require
## Xml.parse_str to reject it with an error at that place. Each mutation names
## the XML 1.0 (5th ed.) production or constraint it breaks. Xml.parser
## must reject it too (or stop before the violation, leaving input over).

Mutation : {
	name : Str,
	xml : Str,
	## Byte offsets where the error may be reported (inclusive).
	low : U64,
	high : U64,
}

Located : { piece : XmlGen.Piece, index : U64, offset : U64 }

locate_pieces : List(XmlGen.Piece) -> List(Located)
locate_pieces = |pieces| {
	var $out = []
	var $offset = 0
	var $index = 0
	while $index < pieces.len() {
		piece = pieces.get($index) ?? Lit("")
		$out = $out.append({ piece, index: $index, offset: $offset })
		$offset = $offset + Str.count_utf8_bytes(XmlGen.render([piece]))
		$index = $index + 1
	}
	$out
}

## Insertions: slot kind, text, and the error offset range relative to the slot.
Insertion : { name : Str, slot : XmlGen.Slot, text : Str, low : U64, high : U64 }

insertions : List(Insertion)
insertions = [
	{ name: "bare & in text [68]", slot: TextSlot, text: "& ", low: 0, high: 0 },
	{ name: "undeclared entity in text (WFC: Entity Declared)", slot: TextSlot, text: "&nbsp;", low: 0, high: 0 },
	{ name: "missing ; after reference [68]", slot: TextSlot, text: "&amp ", low: 4, high: 4 },
	{ name: "char ref to NUL (WFC: Legal Character)", slot: TextSlot, text: "&#0;", low: 0, high: 0 },
	{ name: "char ref to U+FFFE (WFC: Legal Character)", slot: TextSlot, text: "&#xFFFE;", low: 0, high: 0 },
	{ name: "char ref to a surrogate (WFC: Legal Character)", slot: TextSlot, text: "&#xD800;", low: 0, high: 0 },
	{ name: "char ref beyond U+10FFFF (WFC: Legal Character)", slot: TextSlot, text: "&#1114112;", low: 0, high: 0 },
	{ name: "char ref without digits [66]", slot: TextSlot, text: "&#x;", low: 3, high: 3 },
	{ name: "control character in text [2]", slot: TextSlot, text: "\u(1)", low: 0, high: 0 },
	{ name: "U+FFFF in text [2]", slot: TextSlot, text: "\u(FFFF)", low: 0, high: 0 },
	{ name: "]]> in text [14]", slot: TextSlot, text: "]]>", low: 0, high: 2 },
	{ name: "PI target xml in content [17]", slot: TextSlot, text: "<?xml version='1.0'?>", low: 0, high: 0 },
	{ name: "PI target XmL in content [17]", slot: TextSlot, text: "<?XmL x?>", low: 2, high: 2 },
	{ name: "PI without target [16]", slot: TextSlot, text: "<? x?>", low: 2, high: 2 },
	{ name: "DOCTYPE in content [43]", slot: TextSlot, text: "<!DOCTYPE a>", low: 0, high: 0 },
	{ name: "element name starting with a digit [5]", slot: TextSlot, text: "<1a/>", low: 1, high: 1 },
	{ name: "space before element name [40]", slot: TextSlot, text: "< a/>", low: 1, high: 1 },
	{ name: "stray end tag (WFC: Element Type Match)", slot: TextSlot, text: "</zz>", low: 0, high: 0 },
	{ name: "< in attribute value [10]", slot: AttrSlot, text: "<", low: 0, high: 0 },
	{ name: "bare & in attribute value [10]", slot: AttrSlot, text: "& ", low: 0, high: 0 },
	{ name: "undeclared entity in attribute (WFC: Entity Declared)", slot: AttrSlot, text: "&nbsp;", low: 0, high: 0 },
	{ name: "control character in attribute [2]", slot: AttrSlot, text: "\u(8)", low: 0, high: 0 },
	{ name: "char ref to NUL in attribute (WFC: Legal Character)", slot: AttrSlot, text: "&#x0;", low: 0, high: 0 },
	{ name: "duplicate attribute (WFC: Unique Att Spec)", slot: TagSlot, text: " dup='1' dup=\"2\"", low: 9, high: 9 },
	{ name: "no space between attributes [40]", slot: TagSlot, text: " p='1'q='2'", low: 6, high: 6 },
	{ name: "unquoted attribute value [10]", slot: TagSlot, text: " u=v", low: 3, high: 3 },
	{ name: "attribute without value [41]", slot: TagSlot, text: " u>", low: 2, high: 2 },
	{ name: "-- in comment [15]", slot: CommentSlot, text: "-- ", low: 0, high: 0 },
	{ name: "control character in comment [2]", slot: CommentSlot, text: "\u(1F)", low: 0, high: 0 },
	{ name: "second root element [1]", slot: EpilogSlot, text: "<b/>", low: 0, high: 0 },
	{ name: "text after the root element [1]", slot: EpilogSlot, text: "x", low: 0, high: 0 },
	{ name: "CDATA after the root element [1]", slot: EpilogSlot, text: "<![CDATA[x]]>", low: 0, high: 0 },
	{ name: "reference after the root element [1]", slot: EndSlot, text: "&amp;", low: 0, high: 0 },
	{ name: "unterminated comment [15]", slot: EndSlot, text: "<!--", low: 0, high: 0 },
	{ name: "unterminated PI [16]", slot: EndSlot, text: "<?pi x", low: 0, high: 0 },
	{ name: "-- in trailing comment [15]", slot: EndSlot, text: "<!-- a--b -->", low: 6, high: 6 },
]

generate : List(U8) -> Mutation
generate = |bytes| {
	choice = bytes.get(0) ?? 0
	doc = XmlGen.generate(bytes.drop_first(1))
	located = locate_pieces(doc.pieces)
	kind = U8.to_u64(choice) % (insertions.len() + 4)
	match insertions.get(kind) {
		Ok(insertion) => insert(doc.pieces, located, insertion, U8.to_u64(bytes.last() ?? 0))
		Err(_) =>
			match kind - insertions.len() {
				0 => rename_end_tag(doc.pieces, located, U8.to_u64(bytes.last() ?? 0))
				1 => drop_root_end_tag(doc.pieces, located)
				2 => {
					text = XmlGen.render(doc.pieces)
					{ name: "text before the root element [1]", xml: "x${text}", low: 0, high: 0 }
				}
				_ => {
					text = XmlGen.render(doc.pieces)
					if Str.starts_with(text, "<?xml version") {
						{ name: "XML declaration not at the start [23]", xml: " ${text}", low: 1, high: 1 }
					} else {
						{ name: "empty document [1]", xml: "", low: 0, high: 0 }
					}
				}
			}
	}
}

insert : List(XmlGen.Piece), List(Located), Insertion, U64 -> Mutation
insert = |pieces, located, insertion, which| {
	candidates = located.keep_if(
		|entry| {
			match entry.piece {
				Mark(slot) => slot == insertion.slot
				_ => False
			}
		},
	)
	match candidates.get(which % at_least_one(candidates.len())) {
		Err(_) => {
			# No such slot in this document: a stray end tag after the root.
			text = XmlGen.render(pieces)
			{ name: "end tag after the root element [1]", xml: "${text}</a>", low: Str.count_utf8_bytes(text), high: Str.count_utf8_bytes(text) }
		}
		Ok(entry) => {
			xml = XmlGen.render((pieces.set(entry.index, Lit(insertion.text)) ?? pieces))
			# "]]>" may complete brackets written just before it.
			low = if insertion.low == 0 and insertion.high > 0 entry.offset - (if entry.offset < insertion.high entry.offset else insertion.high) else entry.offset + insertion.low
			high = if insertion.low == 0 and insertion.high > 0 entry.offset else entry.offset + insertion.high
			{ name: insertion.name, xml, low, high }
		}
	}
}

rename_end_tag : List(XmlGen.Piece), List(Located), U64 -> Mutation
rename_end_tag = |pieces, located, which| {
	tags = located.keep_if(
		|entry| {
			match entry.piece {
				EndTag(_) => True
				_ => False
			}
		},
	)
	match tags.get(which % at_least_one(tags.len())) {
		Ok({ piece: EndTag(tag), index, offset }) => {
			xml = XmlGen.render((pieces.set(index, EndTag({ name: "zz", space: tag.space })) ?? pieces))
			{ name: "end tag name mismatch (WFC: Element Type Match)", xml, low: offset, high: offset }
		}
		_ => {
			text = XmlGen.render(pieces)
			{ name: "end tag after the root element [1]", xml: "${text}</a>", low: Str.count_utf8_bytes(text), high: Str.count_utf8_bytes(text) }
		}
	}
}

## Without the root's end tag the document ends inside the root element.
drop_root_end_tag : List(XmlGen.Piece), List(Located) -> Mutation
drop_root_end_tag = |pieces, located| {
	last_end =
		located.fold(
			Err(NoEndTag),
			|found, entry| {
				match entry.piece {
					EndTag(_) => Ok(entry.index)
					_ => found
				}
			},
		)
	match last_end {
		Ok(index) => {
			xml = XmlGen.render((pieces.set(index, Lit("")) ?? pieces))
			{ name: "unclosed root element [39]", xml, low: Str.count_utf8_bytes(xml), high: Str.count_utf8_bytes(xml) }
		}
		Err(_) => {
			text = XmlGen.render(pieces)
			{ name: "end tag after the root element [1]", xml: "${text}</a>", low: Str.count_utf8_bytes(text), high: Str.count_utf8_bytes(text) }
		}
	}
}

test : Mutation -> Fuzz.Outcome
test = |mutation| {
	match Xml.parse_str(mutation.xml) {
		Ok(_) => crash "accepted a document with ${mutation.name}: ${Str.inspect(mutation.xml)}"
		Err(InvalidXml(error)) => {
			low = XmlGen.line_column(mutation.xml, mutation.low)
			high = XmlGen.line_column(mutation.xml, mutation.high)
			at = { line: error.line, column: error.column }
			in_range = (at.line > low.line or (at.line == low.line and at.column >= low.column)) and (at.line < high.line or (at.line == high.line and at.column <= high.column))
			if !in_range or error.message.is_empty() {
				crash "${mutation.name}: error ${error.line.to_str()}:${error.column.to_str()} \"${error.message}\" not at ${low.line.to_str()}:${low.column.to_str()}..${high.line.to_str()}:${high.column.to_str()} in ${Str.inspect(mutation.xml)}"
			}
		}
	}
	if Utf8.parse_str(Xml.parser, mutation.xml).is_ok() {
		crash "Xml.parser accepted a document with ${mutation.name}: ${Str.inspect(mutation.xml)}"
	}
	Fuzz.keep
}

target = Fuzz.target_with({
	name: "xml-malformed",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |mutation| "${mutation.name} at bytes ${mutation.low.to_str()}..${mutation.high.to_str()}\n${XmlGen.show(mutation.xml)}",
})

at_least_one : U64 -> U64
at_least_one = |count| if count == 0 1 else count
