import fuzz.Fuzz
import parser.OFX
import parser.Parser
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |choices| {
	generated = OfxGen.generate(choices)
	expected = List.from_iter((OFX.decoder({})).read_iter([generated.bytes].iter()))
	actual = List.from_iter((OFX.decoder({})).read_iter(OfxGen.chunks(generated.bytes, choices).iter()))
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

property_target = Fuzz.target_with({ name: "ofx-stream", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })

OfxStreamProperty :: [].{
	target = property_target
}
