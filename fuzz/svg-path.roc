app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.1/GDoZFRDnzMRiVk9nE8wdWn52btGsM1GhcY9VDKqy9f1Y.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.SvgPath

## Independent path-command models exercise all command families, relative and
## absolute spelling, implicit move-to groups and argument/flag rejection.
Sample : { source : Str, command : U8, arguments : List(F64) }

samples : List(Sample)
samples = [
	{ source: "L1-2", command: 'L', arguments: [1, -2] },
	{ source: "l.5.25", command: 'l', arguments: [0.5, 0.25] },
	{ source: "H-2e1", command: 'H', arguments: [-20] },
	{ source: "h1", command: 'h', arguments: [1] },
	{ source: "V2", command: 'V', arguments: [2] },
	{ source: "v-3", command: 'v', arguments: [-3] },
	{ source: "C1 2 3 4 5 6", command: 'C', arguments: [1, 2, 3, 4, 5, 6] },
	{ source: "c-1-2-3-4-5-6", command: 'c', arguments: [-1, -2, -3, -4, -5, -6] },
	{ source: "S1 2 3 4", command: 'S', arguments: [1, 2, 3, 4] },
	{ source: "s1,2,3,4", command: 's', arguments: [1, 2, 3, 4] },
	{ source: "Q1 2 3 4", command: 'Q', arguments: [1, 2, 3, 4] },
	{ source: "q1,2,3,4", command: 'q', arguments: [1, 2, 3, 4] },
	{ source: "T1 2", command: 'T', arguments: [1, 2] },
	{ source: "t1,2", command: 't', arguments: [1, 2] },
	{ source: "A1 2 30 0110 20", command: 'A', arguments: [1, 2, 30, 0, 1, 10, 20] },
	{ source: "a1 2 30 1 0-10-20", command: 'a', arguments: [1, 2, 30, 1, 0, -10, -20] },
	{ source: "Z", command: 'Z', arguments: [] },
	{ source: "z", command: 'z', arguments: [] },
]

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	var $source = "M0 0 1 2"
	var $expected = [('M', [0.F64, 0]), ('L', [1.F64, 2])]
	for byte in bytes.take_first(64) {
		sample = samples.get(U8.to_u64(byte) % samples.len()) ?? { source: "L1 2", command: 'L', arguments: [1, 2] }
		$source = "${$source}${sample.source}"
		$expected = $expected.append((sample.command, sample.arguments))
	}
	match SvgPath.parse_str($source) {
		Ok(path) => {
			if path.commands.map(|c| (c.command, c.arguments)) != $expected {
				crash "path differs from independent command model: ${$source}"
			}
		}
		Err(error) => crash "generated path rejected: ${$source}: ${Str.inspect(error)}"
	}
	for tail in ["L", "A1 2 3 2 0 1 2", "A-1 2 3 0 0 1 2", "Q1 2 3", "L1."] {
		if SvgPath.parse_str("${$source}${tail}").is_ok() {
			crash "malformed path accepted: ${$source}${tail}"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "svg-path", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes), examples: [] })
