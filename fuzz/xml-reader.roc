app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import XmlGen

## The pull reader agrees with the tree parser on arbitrary UTF-8:
## - when Xml.parse_str accepts a document, Xml.reader's events build the same
##   tree and end with Done; when it rejects one, the reader fails with the
##   same error;
## - Reader.element at the k-th Start (k chosen from the input) builds that
##   element's subtree, and the events after it are the events that follow
##   the subtree's End;
## - every start and end event is located at a '<' of the input;
## - Xml.parse with the default limits agrees with Xml.parse_str, since no
##   4096-byte input can reach them.

Read : { events : List({ event : Xml.Event, location : { line : U64, column : U64 } }), ending : [Done, Failed(Xml.ReadError)] }

read_all : Xml.Reader -> Read
read_all = |reader| {
	var $reader = reader
	var $events = []
	while True {
		match $reader.next() {
			Event(event, rest) => {
				$events = $events.append({ event, location: rest.location() })
				$reader = rest
			}

			Done => return { events: $events, ending: Done }
			Failed(error) => return { events: $events, ending: Failed(error) }
		}
	}
}

## Builds a tree from events the way a reader's consumer would.
tree_of : List(Xml.Event) -> Try(Xml.Node, [Unbalanced])
tree_of = |events| {
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

			End(name) => {
				if name != $current.name {
					return Err(Unbalanced)
				}
				parent = $stack.last() ?? { name: "", attributes: [], children: [] }
				$stack = $stack.drop_last(1)
				$current = { ..parent, children: parent.children.append(Element($current)) }
			}
		}
	}
	if !$stack.is_empty() {
		return Err(Unbalanced)
	}
	$current.children.first().map_err(|_| Unbalanced)
}

## The elements of a tree in document order.
elements_of : Xml.Node -> List(Xml.Node)
elements_of = |root| {
	var $out = []
	var $pending = [root]
	while !$pending.is_empty() {
		node = $pending.last() ?? Text("")
		$pending = $pending.drop_last(1)
		match node {
			Element(element) => {
				$out = $out.append(node)
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

			Text(_) => {}
		}
	}
	$out
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	input =
		match Str.from_utf8(bytes) {
			Ok(text) => text
			Err(_) => return Fuzz.reject
		}
	expected = Xml.parse_str(input)
	read =
		match Xml.reader(input, {}) {
			Ok(reader) => read_all(reader)
			Err(error) => { events: [], ending: Failed(error) }
		}
	match expected {
		Ok(xml) => {
			if read.ending != Done {
				crash "the reader failed on a document parse_str accepts: ${Str.inspect(read.ending)}"
			}
			events = read.events.map(|item| item.event)
			if tree_of(events) != Ok(xml.root) {
				crash "the reader's events build a different tree:\n${Str.inspect(events)}\nexpected ${Str.inspect(xml.root)}"
			}
			check_locations(input, read.events)
			check_element(input, xml.root, events, bytes)
			if Xml.parse(input, {}) != Ok(xml) {
				crash "Xml.parse with default limits disagrees with parse_str"
			}
		}

		Err(InvalidXml(error)) => {
			if read.ending != Failed(InvalidXml(error)) {
				crash "the reader and parse_str disagree on a malformed document: ${Str.inspect(read.ending)} versus ${Str.inspect(error)}"
			}
			if Xml.parse(input, {}) != Err(InvalidXml(error)) {
				crash "Xml.parse with default limits disagrees with parse_str on an error"
			}
		}
	}
	Fuzz.keep
}

## Each event starts where its location says: a tag at '<', text at a byte
## that is not '<' unless markup (a comment, CDATA or PI) starts the text.
check_locations : Str, List({ event : Xml.Event, location : { line : U64, column : U64 } }) -> {}
check_locations = |input, events| {
	bytes = input.to_utf8()
	var $tags = []
	var $index = 0
	while $index < bytes.len() {
		if bytes.get($index) == Ok('<') {
			$tags = $tags.append(XmlGen.line_column(input, $index))
		}
		$index = $index + 1
	}
	for item in events {
		match item.event {
			Start(_) | End(_) => {
				found = $tags.contains(item.location)
				if !found {
					crash "no tag starts at the location ${Str.inspect(item.location)} of ${Str.inspect(item.event)}"
				}
			}

			Text(_) => {}
		}
	}
}

## Reader.element at the k-th Start gives the k-th element in document order,
## and reading continues with the events after that element's End.
check_element : Str, Xml.Node, List(Xml.Event), List(U8) -> {}
check_element = |input, root, events, bytes| {
	elements = elements_of(root)
	k = bytes.fold(0, |sum, byte| (sum * 31 + U8.to_u64(byte)) % 1_000_003) % elements.len()
	var $reader =
		match Xml.reader(input, {}) {
			Ok(reader) => reader
			Err(_) => crash "the reader failed to open"
		}
	var $starts = 0
	var $consumed = 0
	while True {
		match $reader.next() {
			Event(Start(_), rest) => {
				$consumed = $consumed + 1
				if $starts == k {
					check_element_with(rest, elements.get(k))
					match rest.element() {
						Element(node, after) => {
							if Ok(node) != elements.get(k) {
								crash "element() at start ${k.to_str()} built ${Str.inspect(node)}"
							}
							subtree_events = count_events(node)
							remaining = read_all(after)
							if remaining.ending != Done or remaining.events.map(|item| item.event) != events.drop_first($consumed + subtree_events - 1) {
								crash "the events after element() at start ${k.to_str()} differ"
							}
							return {}
						}

						other => crash "element() failed directly after a start: ${Str.inspect(other)}"
					}
				}
				$starts = $starts + 1
				$reader = rest
			}

			Event(_, rest) => {
				match rest.element() {
					NotAfterStart => {}
					_ => crash "element() did not report NotAfterStart after a non-start event"
				}
				$consumed = $consumed + 1
				$reader = rest
			}

			_ => crash "the document ended before start ${k.to_str()}"
		}
	}
}

## Reader.element_with builds the same subtree as element when it builds
## Xml nodes, counts the elements it starts, and reports a rejecting start
## at the first start tag inside the subtree.
check_element_with : Xml.Reader, Try(Xml.Node, [OutOfBounds]) -> {}
check_element_with = |reader, expected| {
	first = match expected {
		Ok(Element(element)) => { name: element.name, attributes: element.attributes }
		_ => crash "the chosen start is not an element"
	}
	built = reader.element_with({
		context: 0,
		first,
		start: |count, name, attributes| Ok({ context: count + 1, open: { name, attributes } }),
		text: |text| Xml.Node.Text(text),
		end: |count, open, children| { context: count, node: Xml.Node.Element({ name: open.name, attributes: open.attributes, children }) },
	})
	inner = match expected {
		Ok(node) => elements_of(node).len() - 1
		Err(_) => 0
	}
	match built {
		Element(node, count, _) =>
			if Ok(node) != expected or count != inner {
				crash "element_with built ${Str.inspect(node)} with ${count.to_str()} starts"
			}

		other => crash "element_with failed where element succeeds: ${Str.inspect(other)}"
	}
	rejected = reader.element_with({ context: {}, first: {}, start: |_, _, _| Err(NoElements), text: |_| {}, end: |_, _, _| { context: {}, node: {} } })
	match rejected {
		Element(_, _, _) =>
			if inner > 0 {
				crash "a rejecting start was not called"
			}

		Rejected(NoElements, _) =>
			if inner == 0 {
				crash "a rejecting start was called for an element with no child elements"
			}

		other => crash "element_with with a rejecting start gave ${Str.inspect(other)}"
	}
}

## The number of events an element's subtree gives, its Start and End included.
count_events : Xml.Node -> U64
count_events = |node| {
	var $count = 0
	var $pending = [node]
	while !$pending.is_empty() {
		current = $pending.last() ?? Text("")
		$pending = $pending.drop_last(1)
		match current {
			Element(element) => {
				$count = $count + 2
				$pending = $pending.concat(element.children)
			}

			Text(_) => {
				$count = $count + 1
			}
		}
	}
	$count
}

target = Fuzz.target_with({
	examples: [],
	name: "xml-reader",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
