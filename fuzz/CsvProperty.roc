import fuzz.Fuzz
import parser.CSV
import parser.Parser

## Any text parses or fails cleanly; a failure names a real position with a
## structured syntax problem.
test : Str -> Fuzz.Outcome
test = |input| {
	match CSV.parse_str(input, {}).map_ok(|document| document.records) {
		Ok(_) => Fuzz.keep
		Err(InvalidCsv(error)) => {
			at = source_position(error.location)
			logical = logical_position(error.path)
			if logical.record == 0 or logical.field == 0 or at.line == 0 or at.column == 0 or at.column > input.count_utf8_bytes() + 1 {
				crash "error position out of range: ${Str.inspect(error)}\n${Str.inspect(input)}"
			}
			match error.problem {
				Syntax(UnterminatedQuotedField) | Syntax(TextAfterQuote) => Fuzz.keep
				_ => crash "unexpected raw CSV failure: ${Str.inspect(error.problem)}"
			}
		}
	}
}

property_target = Fuzz.target_with({
	examples: [],
	name: "csv",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source text coordinates"
}

logical_position : List(Parser.PathSegment) -> { record : U64, field : U64 }
logical_position = |path| path.fold(
	{ record: 0.U64, field: 0.U64 },
	|position, segment| match segment {
		Record(index) => { ..position, record: index }
		Field(index) => { ..position, field: index }
		_ => position
	},
)

CsvProperty :: [].{
	target = property_target
}
