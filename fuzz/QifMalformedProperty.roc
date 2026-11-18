import fuzz.Fuzz
import QifCheck

property_target = Fuzz.target_with({
	name: "qif-malformed",
	examples: [],
	generator: Fuzz.raw_bytes,
	test: |bytes| {
		QifCheck.malformed(bytes)
		Fuzz.keep
	},
	show: Str.inspect,
})

QifMalformedProperty :: [].{
	target = property_target
}
