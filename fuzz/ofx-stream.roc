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
	expected = List.from_iter(OFX.parse_iter([generated.bytes].iter(), {}))
	actual = List.from_iter(OFX.parse_iter(OfxGen.chunks(generated.bytes, choices).iter(), {}))
	if actual != expected {
		crash "chunking changed OFX events or errors"
	}
	if OFX.parse_bytes(generated.bytes, {}) != Ok(generated.expected) {
		crash "OFX disagrees with independent model"
	}
	for prefix in [0, generated.bytes.len() // 2, generated.bytes.len() - 1] {
		if OFX.parse_bytes(generated.bytes.take_first(prefix), {}).is_ok() {
			crash "truncated OFX accepted"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-stream", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
