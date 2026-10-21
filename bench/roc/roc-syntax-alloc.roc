app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## One-shot allocation diagnostic. Same format-prefixed replay protocol as
## alloc.roc; the minimal driver avoids compiling unrelated formats.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	{ after, .. } = bytes.split_first('\n') ?? { before: [], after: [] }
	input = Str.from_utf8(after) ?? ""
	measured = Fuzz.measure_allocs!(|{}| RocSyntax.parse_str(input))
	crash "ALLOCATION_DIAGNOSTIC allocations=${measured.allocations.to_str()} status=${Str.inspect(measured.value.status)}"
}

target = Fuzz.target_with!({ name: "roc-syntax-alloc", generator: Fuzz.raw_bytes, test!, show: Str.inspect })
