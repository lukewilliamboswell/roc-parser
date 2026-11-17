app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.OFX
import parser.Parser
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |choices| {
	generated = OfxGen.generate(choices)
	if OFX.parse_bytes(generated.bytes, {}) != Ok(generated.expected) {
		crash "independent OFX model disagrees"
	}
	for syntax in [Xml203, Sgml102] {
		for encoding in [Utf8, Windows1252] {
			encoded = OFX.encode(generated.expected, { syntax, encoding }) ?? crash "valid OFX model rejected by writer"
			if OFX.parse_bytes(encoded, {}) != Ok(generated.expected) {
				crash "OFX semantic roundtrip changed values"
			}
		}
		events = OfxGen.values((OFX.decoder({})).read_iter(OfxGen.chunks(generated.bytes, choices).iter()))
		encoded_chunks = OfxGen.unwrap_bytes((OFX.encoder({ syntax: syntax, chunk_bytes: 7 })).write_iter(events))
		decoded = (OFX.decoder({})).collect(encoded_chunks) ?? crash "OFX pipeline reparse failed"
		baseline = (OFX.decoder({})).collect([generated.bytes].iter()) ?? crash "baseline rejected"
		if decoded != baseline {
			crash "incremental reader/writer pipeline changed events"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-roundtrip", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
