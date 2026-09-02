app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.3.0/FTcKnkDxL1ZXfKsxeLmNKZ6XKnuKDd47Gv79ThxLYSfw.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.String
import parser.Xml

test : Str -> Fuzz.Outcome
test = |input| {
	match String.parse_str(Xml.xml_parser, input) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

target = Fuzz.target_with({
	name: "xml",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
