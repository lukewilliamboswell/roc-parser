app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.5.0/9WT86CuwM7ZJBSs8iTd1jLyrUFTTiGp5hcJE63TwswGE.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Yaml

test : Str -> Fuzz.Outcome
test = |input| {
	match Yaml.parse_str(input) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "yaml",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
