app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.AsciiDoc
import parser.Parser

# tag::example[]
source = "= Guide\n\nIntroduction\n\n== Getting started\n\nHello\n"

describe : Str -> Str
describe = |text| {
	match AsciiDoc.parse_str(text, {}) {
		Ok(document) => "${document.title}: ${document.blocks.len().to_str()} top-level blocks"
		Err(Unsupported(error)) => "Unsupported on line ${source_position(error.location).line.to_str()}: ${Str.inspect(error.problem)}"
		Err(InvalidAsciiDoc(error)) => "Invalid on line ${source_position(error.location).line.to_str()}: ${Str.inspect(error.problem)}"
	}
}

main! = |_| Stdout.line!(describe(source))

# end::example[]

source_position : Parser.Location -> { offset : U64, line : U64, column : U64 }
source_position = |location| match location {
	Text(position) => position
	_ => crash "expected retained AsciiDoc source byte and line coordinates"
}
