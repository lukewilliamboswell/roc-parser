app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import parser.Parser

## Build source chunks on demand inside the measured region and discard
## events immediately. Growth adds siblings or text, never an input list.
## CDATA bracket runs exercise delayed-character ownership; comments and
## attributes check long tokens as well as text split across source chunks.
chunks : U64, U64 -> Iter(List(U8))
chunks = |n, shape| Iter.custom(
	0.U64,
	Err(Unknown),
	|index| {
		if index == 0 {
			Ok(((if (shape == 3) "<r><![CDATA[" else "<r>").to_utf8(), 1))
		} else if index <= n {
			part = match shape {
				0 => "<i k='${index.to_str()}'>x</i>"
				1 => "<p:i xmlns:p='urn:${index.to_str()}' p:k='v'>x</p:i>"
				2 => Str.repeat(if (index % 2 == 0) "é😀" else "ö😁", 128)
				3 => Str.repeat(if (index % 2 == 0) "x]]]\r\n" else "y]]]\r\n", 128)
				4 => "<!--${Str.repeat(if (index % 2 == 0) "x" else "y", 1024)}-->"
				_ => "<i k='${Str.repeat(if (index % 2 == 0) "x" else "y", 1024)}'/>"
			}
			Ok((part.to_utf8(), index + 1))
		} else if index == n + 1 {
			Ok(((if (shape == 3) "]]></r>" else "</r>").to_utf8(), index + 1))
		} else Err(NoMore)
	},
)

drain : U64, U64 -> U64
drain = |n, shape| {
	var $count = 0.U64
	var $finished = False
	if shape == 1 {
		for result in (Xml.Namespaced.decoder({ max_text_bytes: 1024 })).read_iter(chunks(n, shape)) {
			match result {
				Ok(Value(Start({ name: { namespace: Uri(_), local: "i", .. }, .. }))) => {
					$count = $count + 1
				}
				Ok(Finished(report)) => {
					$finished = True
					if shape == 4 {
						$count = report.emitted_items
					}
				}
				Ok(Value(_)) => {}
				_ => crash "generated namespace stream was rejected"
			}
		}
	} else {
		for result in (Xml.decoder({ max_text_bytes: 1024 })).read_iter(chunks(n, shape)) {
			match result {
				Ok(Value(End("i"))) if shape == 0 or shape == 5 => {
					$count = $count + 1
				}
				Ok(Value(Text(text))) if shape == 2 or shape == 3 => {
					$count = $count + text.count_utf8_bytes()
				}
				Ok(Finished(report)) => {
					$finished = True
					if shape == 4 {
						$count = report.emitted_items
					}
				}
				Ok(Value(_)) => {}
				_ => crash "generated XML stream was rejected"
			}
		}
	}
	if !$finished {
		crash "XML stream omitted successful completion"
	}
	$count
}

## Retain only an early name from a much larger single input chunk. Its
## owning tag must stay bounded after the document and source are dropped.
retain_name : U64 -> Str
retain_name = |n| {
	name = "an_element_name_long_enough_to_require_shared_storage"
	source_chunks = Iter.custom(
		False,
		Err(Unknown),
		|used| {
			if (used) Err(NoMore) else {
				source = "<${name}>${Str.repeat("x", n * 1024)}</${name}>"
				Ok((source.to_utf8(), True))
			}
		},
	)
	var $retained = ""
	for event in (Xml.decoder({ max_text_bytes: 1024 })).read_iter(source_chunks) {
		match event {
			Ok(Value(Start(tag))) => {
				$retained = tag.name
			}
			Ok(Value(_)) => {}
			Ok(Finished(_)) => return $retained
			_ => crash "generated owned-name document was rejected"
		}
	}
	crash "owned-name stream omitted successful completion"
}

## Keep only a failure after all source, document and decoder values leave scope.
## Long identifiers must remain complete while their much larger source is released.
root_name = "an_element_name_long_enough_to_require_shared_storage"

other_name = "a_different_element_name_long_enough_to_require_shared_storage"

long_prefix = "an_undeclared_namespace_prefix_long_enough_to_require_shared_storage"

mismatch_source = |n| "<${root_name}>${Str.repeat("x", n * 1024)}</${other_name}>"

raw_payloads : Xml.Error -> List(Str)
raw_payloads = |error| match error.problem {
	EndTagMismatch(names) if names.actual == other_name and names.expected == root_name => [names.actual, names.expected]
	NonUtf8Encoding(value) if value == "an_encoding_name_long_enough_to_require_shared_storage" => [value]
	_ => crash "native XML problem changed identifier meaning: ${Str.inspect(error.problem)}"
}

retained_raw_payloads : U64, U64 -> List(Str)
retained_raw_payloads = |n, mode| {
	if mode == 0 {
		return match Xml.parse_str(mismatch_source(n), {}) {
			Err(InvalidXml(error)) => raw_payloads(error)
			Ok(_) => crash "malformed XML was accepted"
		}
	}
	if mode == 1 {
		for event in Xml.decoder({}).read_iter([mismatch_source(n).to_utf8()].iter()) {
			match event {
				Err(InvalidXml(error)) => return raw_payloads(error)
				Err(_) => crash "unexpected raw decoder failure"
				Ok(_) => {}
			}
		}
		crash "raw decoder omitted its failure"
	}
	if mode == 2 {
		var $reader = Xml.reader(mismatch_source(n), {}) ?? crash "valid XML opening was rejected"
		while True {
			match $reader.next() {
				Event(_, rest) => {
					$reader = rest
				}
				Failed(InvalidXml(error)) => return raw_payloads(error)
				Done => crash "raw pull reader accepted malformed XML"
			}
		}
	}
	source = "<?xml version='1.0' encoding='an_encoding_name_long_enough_to_require_shared_storage'?><r>${Str.repeat("x", n * 1024)}</r>"
	match Xml.validate({ source: source }) {
		Err(InvalidXml(error)) => raw_payloads(error)
		Ok(_) => crash "non-UTF-8 lint was lost"
	}
}

retained_composed_payloads : U64 -> List(Str)
retained_composed_payloads = |n| match Xml.parser({}).parse_str(mismatch_source(n)) {
	Err(ParseError({ problem: EndTagMismatch(names), .. })) if names.actual == other_name and names.expected == root_name => [names.actual, names.expected]
	_ => crash "composed XML diagnostic changed identifier meaning"
}

namespace_payloads : Xml.Namespaced.NamespaceProblem -> List(Str)
namespace_payloads = |error| match error.problem {
	UndeclaredPrefix(value) if value == long_prefix => [value]
	DuplicateAttribute(name) if name.local == "an_attribute_long_enough_for_shared_storage" and name.namespace == Uri("urn:a_namespace_long_enough_for_shared_storage") and name.prefix == Prefix("prefix_two_long_enough_for_shared_storage") => [
		match name.namespace {
			Uri(value) => value
			NoNamespace => ""
		},
		name.local,
		match name.prefix {
			Prefix(value) => value
			Unprefixed => ""
		},
	]
	_ => crash "namespace diagnostic changed identifier meaning"
}

retained_namespace_payloads : U64, U64 -> List(Str)
retained_namespace_payloads = |n, mode| {
	padding = Str.repeat("x", n * 1024)
	if mode == 3 {
		source = "<r xmlns:prefix_one_long_enough_for_shared_storage='urn:a_namespace_long_enough_for_shared_storage' xmlns:prefix_two_long_enough_for_shared_storage='urn:a_namespace_long_enough_for_shared_storage'>${padding}<child prefix_one_long_enough_for_shared_storage:an_attribute_long_enough_for_shared_storage='1' prefix_two_long_enough_for_shared_storage:an_attribute_long_enough_for_shared_storage='2'/></r>"
		return match Xml.Namespaced.parse_str(source, {}) {
			Err(InvalidNamespace(error)) => namespace_payloads(error)
			_ => crash "duplicate expanded attribute was accepted"
		}
	}
	source = "<r>${padding}<${long_prefix}:child/></r>"
	if mode == 0 {
		return match Xml.Namespaced.parse_str(source, {}) {
			Err(InvalidNamespace(error)) => namespace_payloads(error)
			_ => crash "undeclared prefix was accepted"
		}
	}
	if mode == 1 {
		for event in Xml.Namespaced.decoder({}).read_iter([source.to_utf8()].iter()) {
			match event {
				Err(InvalidNamespace(error)) => return namespace_payloads(error)
				Err(_) => crash "unexpected namespace decoder failure"
				Ok(_) => {}
			}
		}
		crash "namespace decoder omitted its failure"
	}
	var $reader = Xml.Namespaced.reader(source, {}) ?? crash "valid namespaced opening was rejected"
	while True {
		match $reader.next() {
			Event(_, rest) => {
				$reader = rest
			}
			Failed(InvalidNamespace(error)) => return namespace_payloads(error)
			_ => crash "namespace pull reader lost its failure"
		}
	}
}

retained_encode_payloads : U64 -> List(Str)
retained_encode_payloads = |n| {
	doc = Xml.Namespaced.parse_str("<${root_name}><${other_name}>${Str.repeat("x", n * 1024)}</${other_name}></${root_name}>", {}) ?? crash "valid generated XML was rejected"
	root = match doc.root {
		Element(value) => value
		_ => crash "generated root is not an element"
	}
	child = match root.children.first() {
		Ok(Element(value)) => value
		_ => crash "generated child is not an element"
	}
	invalid : Xml.Namespaced
	invalid = { declaration: doc.declaration, root: Element({ ..root, children: [Element({ ..child, children: [Text("\u(1)")] })] }) }
	match invalid.encode({}) {
		Err(Unrenderable(error)) if error.problem == InvalidChar(1) and error.path == [Key(root_name), Key(other_name)] => error.path.map(
			|part| match part {
				Key(value) => value
				_ => crash "unexpected encoder path"
			},
		)
		_ => crash "XML encoding failure changed path meaning"
	}
}

## Each endpoint retains original payload strings, without rendering or copying.
retained_error_payloads : U64, U64 -> List(Str)
retained_error_payloads = |n, mode| {
	if mode <= 3 {
		retained_raw_payloads(n, mode)
	}
	else if mode == 4 {
		retained_composed_payloads(n)
	}
	else if mode <= 8 {
		retained_namespace_payloads(n, mode - 5)
	}
	else {
		retained_encode_payloads(n)
	}
}

expected_error_payloads : U64 -> List(Str)
expected_error_payloads = |mode| {
	if mode <= 2 or mode == 4 {
		[other_name, root_name]
	}
	else if mode == 3 {
		["an_encoding_name_long_enough_to_require_shared_storage"]
	}
	else if mode <= 7 {
		[long_prefix]
	}
	else if mode == 8 {
		["urn:a_namespace_long_enough_for_shared_storage", "an_attribute_long_enough_for_shared_storage", "prefix_two_long_enough_for_shared_storage"]
	}
	else {
		[root_name, other_name]
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	choice = (bytes.first() ?? 0).to_u64()
	shape = choice % 7
	# Smoke exploration uses one byte and stays below 64 KiB. Named examples
	# use a second byte to check much larger documents without slowing every run.
	n = if (bytes.get(1) == Ok(255)) 2048 else [1.U64, 8, 32, 64].get(choice // 7 % 4) ?? 1
	if choice == 7 {
		for mode in (0.U64).to(9) {
			retained = Fuzz.measure!("xml-error-lifetime", || retained_error_payloads(n, mode))
			if retained.stats.exit_live_bytes > retained.stats.entry_live_bytes + 1024 {
				crash "XML diagnostic retained its source: mode ${mode.to_str()}, held ${retained.stats.exit_live_bytes.to_str()} bytes"
			}
			if retained.value != expected_error_payloads(mode) {
				crash "XML error ownership changed identifier meaning: mode ${mode.to_str()}, ${Str.inspect(retained.value)}"
			}
		}
		return Fuzz.keep
	}

	if shape == 6 {
		retained = Fuzz.measure!("xml-stream-owned-name", || retain_name(n))
		if retained.value != "an_element_name_long_enough_to_require_shared_storage" {
			crash "retained XML name changed"
		}
		if retained.stats.exit_live_bytes > retained.stats.entry_live_bytes + 1024 {
			crash "a bounded XML name retained its much larger source chunk"
		}
		if retained.stats.requested_bytes > 8192 + n * 16_384 {
			crash "owned-name stream exceeded linear allocation traffic"
		}
		return Fuzz.keep
	}
	name = ["xml-stream-elements", "xml-stream-namespaces", "xml-stream-text", "xml-stream-cdata", "xml-stream-comments", "xml-stream-attributes"].get(shape) ?? "xml-stream"
	measured = Fuzz.measure!(name, || drain(n, shape))
	expected = match shape {
		2 => n * 768
		3 => n * 640
		4 => 2
		_ => n
	}
	if measured.value != expected {
		crash "XML stream lost elements or text"
	}
	held = measured.stats.peak_live_bytes.minus_saturated(measured.stats.entry_live_bytes)
	if held > 16_384 {
		crash "XML stream retained completed input: ${held.to_str()} bytes"
	}
	per_chunk = [512.U64, 2048, 4096, 12_288, 24_576, 8192].get(shape) ?? 8192
	if measured.stats.requested_bytes > 8192 + per_chunk * n {
		crash "XML allocation traffic exceeded its linear ceiling: ${measured.stats.requested_bytes.to_str()} bytes"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	name: "xml-stream-ceilings",
	examples: [
		Fuzz.example({ name: "small-elements", revision: "1", make: || [0.U8] }),
		Fuzz.example({ name: "large-elements", revision: "1", make: || [0.U8, 255] }),
		Fuzz.example({ name: "small-namespaces", revision: "1", make: || [1.U8] }),
		Fuzz.example({ name: "large-namespaces", revision: "1", make: || [1.U8, 255] }),
		Fuzz.example({ name: "small-text", revision: "1", make: || [2.U8] }),
		Fuzz.example({ name: "large-text", revision: "1", make: || [2.U8, 255] }),
		Fuzz.example({ name: "small-cdata", revision: "1", make: || [3.U8] }),
		Fuzz.example({ name: "large-cdata", revision: "1", make: || [3.U8, 255] }),
		Fuzz.example({ name: "small-comments", revision: "1", make: || [4.U8] }),
		Fuzz.example({ name: "large-comments", revision: "1", make: || [4.U8, 255] }),
		Fuzz.example({ name: "small-attributes", revision: "1", make: || [5.U8] }),
		Fuzz.example({ name: "large-attributes", revision: "1", make: || [5.U8, 255] }),
		Fuzz.example({ name: "small-owned-name", revision: "1", make: || [6.U8] }),
		Fuzz.example({ name: "large-owned-name", revision: "1", make: || [6.U8, 255] }),
		Fuzz.example({ name: "error-lifetime", revision: "1", make: || [7.U8] }),
	],
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
})
