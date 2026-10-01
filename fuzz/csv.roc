app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV

## Any text parses or fails cleanly; a failure names a real position with a
## bounded message.
test : Str -> Fuzz.Outcome
test = |input| {
	match CSV.parse_records(input) {
		Ok(_) => Fuzz.keep
		Err(InvalidCsv({ record, field, line, column, message })) => {
			if record == 0 or field == 0 or line == 0 or column == 0 or column > input.count_utf8_bytes() + 1 {
				crash "error position out of range: ${Str.inspect({ record, field, line, column })}\n${Str.inspect(input)}"
			}
			if message.count_utf8_bytes() > 200 {
				crash "unbounded message: ${message}"
			}
			Fuzz.keep
		}
	}
}

target = Fuzz.target_with({
	name: "csv",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})
