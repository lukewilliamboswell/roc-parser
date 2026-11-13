app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	parser: "../../package/main.roc",
}
import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Monotonic
import parser.Html
import parser.Svg
import parser.MathML
import parser.Markup
import parser.MarkupTemplate

## Local timing boundary. Preparation, stdin and output lie outside the timer.
main! : List(OsStr) => Try({}, _)
main! = |arguments| {
	args = arguments.map(OsStr.display)
	operation = args.get(0) ?? "svg"
	iterations = U64.from_str(args.get(1) ?? "1") ?? 1
	input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
	# Binding measures already parsed structure, independently from plan creation.
	segments = if (operation == "segments" or operation == "bind") input.split_on("\u(0)") else ["<text>", "</text>"]
	prepared = Svg.parse_segments(segments, {}) ?? crash "invalid benchmark template"
	values = List.repeat("<path/>", prepared.slots.len())
	start = Monotonic.now!()
	var $checksum = 0.U64
	var $successes = 0.U64
	var $index = 0.U64
	while $index < iterations {
		result = if operation == "source" {
			source = Markup.Source.from_str(input)
			Ok(source.tokens.len() + source.bytes.len())
		} else if operation == "bind" {
			MarkupTemplate.bind(prepared, values.iter()).map_ok(|bound| bound.values.fold(bound.plan.parsed.tree.nodes.len(), |sum, value| sum + value.count_utf8_bytes())).map_err(|_| 1.U64)
		} else if operation == "segments" {
			Svg.parse_segments(segments, {}).map_ok(|plan| plan.slots.len() + plan.parsed.tree.nodes.len()).map_err(|_| 1.U64)
		} else {
			parsed = if operation == "html" {
				Html.parse_fragment(input, {}).map_ok(|value| value.parsed)
			}
			else if operation == "recover" {
				Html.parse_fragment(input, { recovery: Browser, allow_unknown_elements: True, allow_unknown_attributes: True, check_values: False }).map_ok(|value| value.parsed)
			}
			else if operation == "mathml" {
				MathML.parse_fragment(input, {}).map_ok(|value| value.parsed)
			}
			else {
				Svg.parse_fragment(input, {}).map_ok(|value| value.parsed)
			}
			parsed.map_ok(|value| checksum(value.tree)).map_err(|_| 1.U64)
		}
		match result {
			Ok(value) => {
				$successes = $successes + 1
				$checksum = $checksum + value
			}
			Err(value) => {
				$checksum = $checksum + value
			}
		}
		$index = $index + 1
	}
	elapsed = start.elapsed!().to_nanos()
	Stdout.line!("{\"iterations\":${iterations.to_str()},\"elapsed_ns\":${elapsed.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()}}")?
	Ok({})
}

checksum : Markup.Tree -> U64
checksum = |tree| tree.nodes.fold(
	0,
	|sum, node| {
		text = match node.kind {
			Element(name) => name.local.count_utf8_bytes()
			Text(value) | Comment(value) => value.count_utf8_bytes()
			_ => 1
		}
		sum + text + node.attributes.fold(0, |count, attribute| count + attribute.name.local.count_utf8_bytes() + attribute.value.count_utf8_bytes()) + node.children.len()
	},
)
