import fuzz.Fuzz
import QifCheck

property_target = Fuzz.target_with({
	name: "qif-stream",
	examples: [],
	generator: Fuzz.raw_bytes,
	test: |bytes| {
		QifCheck.raw(bytes)
		Fuzz.keep
	},
	show: Str.inspect,
})

QifStreamProperty :: [].{
	target = property_target
}
