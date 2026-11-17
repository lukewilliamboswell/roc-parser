app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import parser.XmlNs
import parser.Streaming
import XmlGen

## Chunk boundaries cannot change events or errors. Complete-input readers
## independently check acceptance and event semantics after text coalescing.
coalesce : List(Xml.Event) -> List(Xml.Event)
coalesce = |events| {
	var $out = []
	for event in events {
		match (event, $out.last()) {
			(Text(text), Ok(Text(before))) => {
				$out = $out.drop_last(1).append(Text("${before}${text}"))
			}
			_ => {
				$out = $out.append(event)
			}
		}
	}
	$out
}

reader_events : Str -> _
reader_events = |source| {
	var $reader = Xml.reader(source, {})?
	var $events = []
	while True {
		match $reader.next() {
			Event(event, rest) => {
				$reader = rest
				$events = $events.append(event)
			}
			Done => return Ok($events)
			Failed(problem) => return Err(problem)
		}
	}
}

## An independent entity-size model: each reference fits its own limit,
## regardless of earlier references, emitted text, tags or CDATA boundaries.
check_reference_budget : List(U8) -> {}
check_reference_budget = |choices| {
	shape = (choices.first() ?? 0) % 3
	budget : U64
	budget = if (shape == 2) 9 else 5 + (choices.get(1) ?? 0).to_u64() % 2
	entity = if (budget == 5) "&#65;" else if (budget == 6) "&#x41;" else "&#000065;"
	too_large = if (budget == 5) "&#065;" else if (budget == 6) "&#0065;" else "&#0000065;"
	separator = if (shape == 0) "" else if (shape == 1) "<b/>" else "<![CDATA[]]>"
	count = 2 + (choices.get(2) ?? 0).to_u64() % 16
	source = "<a>${Str.join_with(List.repeat(entity, count), separator)}</a>".to_utf8()
	props : Xml.StreamProps
	props = { max_token_bytes: budget, max_text_bytes: 4 }
	whole = Streaming.collect(Xml.parse_iter([source].iter(), props))
	match whole {
		Ok(events) => {
			text = Str.join_with(
				events.map(
					|event| match event {
						Text(value) => value
						_ => ""
					},
				),
				"",
			)
			if text != Str.repeat("A", count) {
				crash "entity byte budgets changed decoded text"
			}
		}
		Err(_) => crash "a completed entity spent the following entity's byte allowance"
	}
	if Streaming.collect(Xml.parse_iter(source.map(|byte| [byte]).iter(), props)) != whole {
		crash "entity byte budgets depend on source chunks"
	}
	oversize = "<a>${entity}${separator}${too_large}</a>".to_utf8()
	match Streaming.collect(Xml.parse_iter([oversize].iter(), props)) {
		Err(ReadFailed(ParseErr(problem))) if problem.message == "XML reference byte limit exceeded" and problem.offset == 3 + entity.count_utf8_bytes() + separator.count_utf8_bytes() + budget => {}
		_ => crash "an oversized following entity escaped its own byte budget"
	}
}

test : List(U8) -> Fuzz.Outcome
test = |input| {
	check_reference_budget(input)
	bytes = input.take_first(2048)
	props : Xml.StreamProps
	props = { max_text_bytes: 4 + (input.first() ?? 0).to_u64() % 32, max_token_bytes: 4096 }
	whole = Streaming.collect(Xml.parse_iter([bytes].iter(), props))
	pieces = [[]].concat(bytes.map(|byte| [byte])).append([])
	if Streaming.collect(Xml.parse_iter(pieces.iter(), props)) != whole {
		crash "XML events/errors depend on chunk boundaries"
	}
	split = if (bytes.is_empty()) 0 else (input.get(1) ?? 0).to_u64() % (bytes.len() + 1)
	parts = [bytes.take_first(split), [], bytes.drop_first(split)]
	if Streaming.collect(Xml.parse_iter(parts.iter(), props)) != whole {
		crash "XML split changed result"
	}
	namespaced = Streaming.collect(XmlNs.parse_iter([bytes].iter(), props))
	if Streaming.collect(XmlNs.parse_iter(pieces.iter(), props)) != namespaced {
		crash "namespace events/errors depend on chunk boundaries"
	}
	match Str.from_utf8(bytes) {
		Ok(source) => {
			# Byte streaming intentionally refuses non-UTF-8 declarations.
			declares_other = match Xml.reader(source, {}) {
				Ok(reader) => match reader.declaration() {
					Ok({ encoding: Ok(OtherEncoding(_)), .. }) => True
					_ => False
				}
				Err(_) => False
			}
			if !declares_other {
				old = reader_events(source)
				if old.is_ok() != whole.is_ok() {
					crash "XML chunk reader disagrees with complete reader acceptance"
				}
				match (old, whole) {
					(Ok(expected), Ok(actual)) => {
						if expected != coalesce(actual) {
							crash "XML chunk events disagree with complete reader"
						}
					}
					_ => {}
				}
				if XmlNs.parse_str(source).is_ok() != namespaced.is_ok() {
					crash "namespace chunk reader disagrees with complete parser acceptance"
				}
			}
		}
		Err(_) => {
			if whole.is_ok() or namespaced.is_ok() {
				crash "chunk reader accepted invalid UTF-8"
			}
		}
	}
	limited : Xml.StreamProps
	limited = { max_depth: 1 + (input.first() ?? 0).to_u64() % 8, max_elements: (input.get(1) ?? 0).to_u64() % 32, max_attributes: (input.get(2) ?? 0).to_u64() % 8, max_token_bytes: 8 + (input.get(3) ?? 0).to_u64() % 64, max_text_bytes: props.max_text_bytes, max_context_bytes: (input.get(4) ?? 0).to_u64() }
	if Streaming.collect(Xml.parse_iter([bytes].iter(), limited)) != Streaming.collect(Xml.parse_iter(pieces.iter(), limited))
		or Streaming.collect(XmlNs.parse_iter([bytes].iter(), limited)) != Streaming.collect(XmlNs.parse_iter(pieces.iter(), limited)) {
		crash "resource limits depend on source chunk boundaries"
	}
	match whole {
		Ok(events) => {
			if !events.all(
				|event| match event {
					Text(text) => text.count_utf8_bytes() <= props.max_text_bytes
					_ => True
				},
			) {
				crash "text fragment exceeded its configured byte bound"
			}
		}
		_ => {}
	}
	generated = XmlGen.generate(input.take_first(256))
	generated_source = XmlGen.render(generated.pieces)
	other_encoding = match generated.expected.declaration {
		Ok({ encoding: Ok(OtherEncoding(_)), .. }) => True
		_ => False
	}
	if !other_encoding {
		generated_bytes = generated_source.to_utf8()
		chunks = generated_bytes.map(|byte| [byte])
		actual = Streaming.collect(Xml.parse_iter(chunks.iter(), props))
		expected = reader_events(generated_source)
		match (expected, actual) {
			(Ok(wanted), Ok(got)) if wanted == coalesce(got) => {}
			_ => crash "generated XML changed during incremental decoding: ${Str.inspect(generated_source)} expected ${Str.inspect(expected)} actual ${Str.inspect(actual)}"
		}
	}
	Fuzz.keep
}

test! = |bytes| {
	_ = test(bytes)
	source : Stream(Try(List(U8), [Injected]))
	source = [Ok("<a/>".to_utf8()), Err(Injected)].iter().stream()
	var $last = Ok(Finished({ emitted_items: 0.U64, rejected_records: 0.U64 }))
	for! event in Xml.parse_stream(source, {}) {
		$last = event
	}
	if $last != Err(SourceErr(Injected)) {
		crash "XML source failure was not preserved after the root closed"
	}
	namespaced_source : Stream(Try(List(U8), [Injected]))
	namespaced_source = [Ok("<r xmlns='u'/>".to_utf8()), Err(Injected)].iter().stream()
	var $namespace_last = Ok(Finished({ emitted_items: 0.U64, rejected_records: 0.U64 }))
	for! event in XmlNs.parse_stream(namespaced_source, {}) {
		$namespace_last = event
	}
	if $namespace_last != Err(SourceErr(Injected)) {
		crash "namespace source failure was not preserved"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "xml-stream", examples: [], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
