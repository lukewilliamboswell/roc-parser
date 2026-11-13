app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Yaml

## Diagnostic only: `measure` reports the allocations of one UTF-8 YAML input.
## Never include this target in fuzz campaigns.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	input = Str.from_utf8(bytes) ?? ""
	_measured = Fuzz.measure!("parse", || Yaml.parse_str(input))
	Fuzz.keep
}

target = Fuzz.target_with!({
	examples: [],
	name: "yaml-alloc-diagnostic",
	generator: Fuzz.raw_bytes,
	test!,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
