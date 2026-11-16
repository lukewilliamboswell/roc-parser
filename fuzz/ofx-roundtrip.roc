app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.OFX
import parser.Streaming
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |choices| {
	generated = OfxGen.generate(choices)
	if OFX.parse_bytes(generated.bytes, {}) != Ok(generated.expected) {
		crash "independent OFX model disagrees"
	}
	for syntax in [Xml203, Sgml102] {
		for encoding in [Utf8, Windows1252] {
			encoded = OFX.to_bytes(generated.expected, { syntax, encoding }) ?? crash "valid OFX model rejected by writer"
			if OFX.parse_bytes(encoded, {}) != Ok(generated.expected) {
				crash "OFX semantic roundtrip changed values"
			}
		}
		events = OfxGen.values(OFX.parse_iter(OfxGen.chunks(generated.bytes, choices).iter(), {}))
		encoded_chunks = OfxGen.unwrap_bytes(OFX.write_iter(events, { syntax: syntax }, 7))
		decoded = Streaming.collect(OFX.parse_iter(encoded_chunks, {})) ?? crash "OFX pipeline reparse failed"
		baseline = Streaming.collect(OFX.parse_iter([generated.bytes].iter(), {})) ?? crash "baseline rejected"
		if decoded != baseline {
			crash "incremental reader/writer pipeline changed events"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-roundtrip", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
