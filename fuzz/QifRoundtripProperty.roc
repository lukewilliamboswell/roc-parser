import fuzz.Fuzz
import QifCheck

property_target = Fuzz.target_with({
	name: "qif-roundtrip",
	examples: [],
	generator: Fuzz.raw_bytes,
	test: |bytes| {
		QifCheck.roundtrip(bytes)
		Fuzz.keep
	},
	show: Str.inspect,
})

QifRoundtripProperty :: [].{
	target = property_target
}
