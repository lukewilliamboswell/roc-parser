app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV

test : Str -> Fuzz.Outcome
test = |input| {
	match CSV.parse_str_to_csv(input) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

target = Fuzz.target_with({
	name: "csv",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
