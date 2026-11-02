app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.5.0/9WT86CuwM7ZJBSs8iTd1jLyrUFTTiGp5hcJE63TwswGE.tar.zst",
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
	_measured = Fuzz.measure!("parse", || RocSyntax.parse_str(input))
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "roc-syntax-alloc", examples: [], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
