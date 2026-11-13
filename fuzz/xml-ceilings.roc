app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import parser.XmlNs

## Allocation ceilings for reading XML. The first byte chooses a document
## shape and the second its size `n` (1-256 repetitions of a unit); each shape
## is one that once decoded with a `concat` per character or per segment.
## Every shape's allocator traffic and peak held bytes must stay within a
## line in the document's length, so the cost of a document grows linearly:
## a large example of each shape is declared next to a small one.
##
## Other shapes pin the reader and namespaces: a loop over `next` that keeps
## no events holds a constant number of bytes plus a multiple of the depth
## and its largest event, whatever the length, and XmlNs.parse_str holds and
## requests at most three times what Xml.parse_str does, since only one tree
## is built.
##
## Ceilings only move down. Raising one needs its cause stated in the pull
## request that raises it.

shape_count : U8
shape_count = 11

## A shape's document for size `n`.
document : U8, U64 -> Str
document = |shape, n| {
	repeat = |unit| Str.repeat(unit, n * 16)
	match shape {
		# A long CDATA section, with ']' that does not end it.
		0 => "<a><![CDATA[${repeat("x]")}]]></a>"
		# One reference, then non-ASCII characters.
		1 => "<a>&amp;${repeat("é")}</a>"
		# References between short ASCII runs.
		2 => "<a>${repeat("ab&lt;")}</a>"
		# CRLF line endings in text.
		3 => "<a>${repeat("ab\r\n")}</a>"
		# The same in attribute values.
		4 => "<a v='&amp;${repeat("é")}'/>"
		5 => "<a v='${repeat("ab&lt;")}'/>"
		6 => "<a v='${repeat("a\tb\r\n")}'/>"
		# Many siblings, many attributes, and deep nesting.
		7 => "<r>${repeat("<i>x</i>")}</r>"
		8 => "<r${Str.join_with(List.repeat(0, n * 16).map_with_index(|_, index| " a${index.to_str()}='v'"), "")}/>"
		9 => "${repeat("<a>")}${repeat("</a>")}"
		# Namespaced siblings.
		_ => "<w:r xmlns:w='urn:w'>${repeat("<w:p w:k='v'>x</w:p>")}</w:r>"
	}
}

## Allocator traffic, and bytes held at the peak beyond those live on entry
## (the input), allowed per byte of each shape's document past a fixed
## allowance. Each is about a quarter above the worst case over every size
## on nightly-2026-11-07-ad3a5d2, which comes just after a list doubles its
## capacity: requested 2.9 and held 1.5 for text, and requested 82, 39, 64 and
## 36 and held 42, 20, 34 and 20 for the four trees.
ceiling : U8 -> { requested : U64, held : U64 }
ceiling = |shape| {
	match shape {
		0 | 1 | 2 | 3 | 4 | 5 | 6 => { requested: 4, held: 2 }
		7 => { requested: 104, held: 52 }
		8 => { requested: 48, held: 26 }
		9 => { requested: 80, held: 42 }
		_ => { requested: 46, held: 26 }
	}
}

## The bytes one event can hold, per byte of the document: a shape's whole
## text or attribute list is one event.
largest_event : U8 -> U64
largest_event = |shape| {
	match shape {
		0 | 1 | 2 | 3 | 4 | 5 | 6 => 2
		8 => 24
		_ => 0
	}
}

held : Fuzz.Stats -> U64
held = |stats| stats.peak_live_bytes - stats.entry_live_bytes

allowance : U64
allowance = 8192

within : Fuzz.Stats, U64, { requested : U64, held : U64 } -> Try({}, Str)
within = |stats, size, per_byte| {
	requested = allowance + per_byte.requested * size
	most = allowance + per_byte.held * size
	if stats.requested_bytes > requested {
		Err("requested ${stats.requested_bytes.to_str()} bytes, over the ceiling ${requested.to_str()}")
	} else if held(stats) > most {
		Err("held ${held(stats).to_str()} bytes, over the ceiling ${most.to_str()}")
	} else {
		Ok({})
	}
}

## Reads every event and keeps none of them.
drain : Xml.Reader -> U64
drain = |reader| {
	var $reader = reader
	var $count = 0
	while True {
		match $reader.next() {
			Event(_, rest) => {
				$reader = rest
				$count = $count + 1
			}

			Done => return $count
			Failed(_) => return 0
		}
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	shape = (bytes.first() ?? 0) % shape_count
	n = U8.to_u64(bytes.get(1) ?? 0) + 1
	input = document(shape, n)
	size = input.count_utf8_bytes()
	parsed = Fuzz.measure!("parse_str", || Xml.parse_str(input))
	if parsed.value.is_err() {
		crash "shape ${shape.to_str()} did not parse: ${Str.inspect(parsed.value)}"
	}
	match within(parsed.stats, size, ceiling(shape)) {
		Ok({}) => {}
		Err(problem) => crash "Xml.parse_str of shape ${shape.to_str()} (${size.to_str()} bytes) ${problem}"
	}

	# A loop over `next` holds the reader's stack (about 28 bytes a level)
	# and one event, not the document.
	depth = if (shape == 9) n * 16 else 2
	drained = Fuzz.measure!("reader", || Xml.reader(input, { max_depth: depth, max_attributes: n * 16 }).map_ok(drain))
	if drained.value.map_ok(|count| count > 0) != Ok(True) {
		crash "the reader did not read shape ${shape.to_str()}: ${Str.inspect(drained.value)}"
	}
	reader_most = allowance + 48 * depth + largest_event(shape) * size
	if held(drained.stats) > reader_most {
		crash "a loop over next held ${held(drained.stats).to_str()} bytes for shape ${shape.to_str()} (${size.to_str()} bytes), over ${reader_most.to_str()}"
	}

	# One namespaced tree costs a bounded multiple of the plain tree: about
	# twice, measured.
	if shape == 10 {
		namespaced = Fuzz.measure!("xmlns", || XmlNs.parse_str(input))
		if namespaced.value.is_err() or held(namespaced.stats) > allowance + 3 * held(parsed.stats) or namespaced.stats.requested_bytes > allowance + 3 * parsed.stats.requested_bytes {
			crash "XmlNs.parse_str held ${held(namespaced.stats).to_str()} bytes and requested ${namespaced.stats.requested_bytes.to_str()}; Xml.parse_str held ${held(parsed.stats).to_str()} and requested ${parsed.stats.requested_bytes.to_str()}"
		}
	}
	Fuzz.keep
}

examples : List(Fuzz.Example(List(U8)))
examples = {
	var $examples = []
	var $shape = 0
	while $shape < shape_count {
		small = [$shape, 0]
		large = [$shape, 255]
		$examples = $examples.append(Fuzz.example({ name: "shape-${$shape.to_str()}-small", revision: "1", make: || small }))
		$examples = $examples.append(Fuzz.example({ name: "shape-${$shape.to_str()}-large", revision: "1", make: || large }))
		$shape = $shape + 1
	}
	$examples
}

target = Fuzz.target_with!({
	examples,
	name: "xml-ceilings",
	generator: Fuzz.raw_bytes,
	test!,
	show: |bytes| {
		shape = (bytes.first() ?? 0) % shape_count
		n = U8.to_u64(bytes.get(1) ?? 0) + 1
		"shape ${shape.to_str()}, n ${n.to_str()}: ${Str.inspect(document(shape, n))}"
	},
})
