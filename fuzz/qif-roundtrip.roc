app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import QifCheck

target = Fuzz.target_with({
	name: "qif-roundtrip",
	examples: [],
	generator: Fuzz.raw_bytes,
	test: |bytes| {
		QifCheck.roundtrip(bytes)
		Fuzz.keep
	},
	show: Str.inspect,
})
