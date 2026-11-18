import fuzz.Fuzz
import QifCheck

property_target = Fuzz.target_with({
	name: "qif-raw",
	examples: [],
	generator: Fuzz.raw_bytes,
	test: |bytes| {
		QifCheck.raw(bytes)
		Fuzz.keep
	},
	show: Str.inspect,
})

QifRawProperty :: [].{
	target = property_target
}
