app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV

## Any text parses or fails cleanly; a failure names a real position with a
## structured syntax problem.
test : Str -> Fuzz.Outcome
test = |input| {
	match CSV.parse_records(input) {
		Ok(_) => Fuzz.keep
		Err(InvalidCsv({ record, field, line, column, problem })) => {
			if record == 0 or field == 0 or line == 0 or column == 0 or column > input.count_utf8_bytes() + 1 {
				crash "error position out of range: ${Str.inspect({ record, field, line, column })}\n${Str.inspect(input)}"
			}
			match problem {
				Syntax(UnterminatedQuotedField) | Syntax(TextAfterQuote) => Fuzz.keep
				_ => crash "unexpected raw CSV failure: ${Str.inspect(problem)}"
			}
		}
	}
}

target = Fuzz.target_with({
	examples: [],
	name: "csv",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
