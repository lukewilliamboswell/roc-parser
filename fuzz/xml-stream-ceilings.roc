app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Xml
import parser.XmlNs

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
		for result in XmlNs.parse_iter(chunks(n, shape), { max_text_bytes: 1024 }) {
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
		for result in Xml.parse_iter(chunks(n, shape), { max_text_bytes: 1024 }) {
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

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	choice = (bytes.first() ?? 0).to_u64()
	shape = choice % 6
	# Smoke exploration uses one byte and stays below 64 KiB. Named examples
	# use a second byte to check much larger documents without slowing every run.
	n = if (bytes.get(1) == Ok(255)) 2048 else [1.U64, 8, 32, 64].get(choice // 6 % 4) ?? 1
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
	],
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
})
