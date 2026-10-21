app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.AsciiDoc

# tag::example[]
source = "= Guide\n\nIntroduction\n\n== Getting started\n\nHello\n"

describe : Str -> Str
describe = |text| {
	match AsciiDoc.parse_str(text) {
		Ok(document) => "${document.title}: ${document.blocks.len().to_str()} top-level blocks"
		Err(Unsupported(error)) => "Unsupported on line ${error.line.to_str()}: ${error.message}"
		Err(InvalidAsciiDoc(error)) => "Invalid on line ${error.line.to_str()}: ${error.message}"
	}
}

main! = |_| Stdout.line!(describe(source))
# end::example[]
