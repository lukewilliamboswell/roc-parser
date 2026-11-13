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
import parser.JsonValue

## Local DOM comparison: input decoding and output are outside the timer.
main! : List(OsStr) => Try({}, _)
main! = |arguments| {
	args = arguments.map(OsStr.display)
	operation = args.get(0) ?? "svg-full"
	iterations = U64.from_str(args.get(1) ?? "1") ?? 1
	input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
	if iterations == 0 {
		parsed = parse(input, operation) ?? crash "invalid canonical benchmark input"
		encoded = Str.join_with(parsed.tree.roots.map(|index| encode_node(parsed.tree, index, False)), ",")
		Stdout.line!("[${encoded}]")?
		return Ok({})
	}
	start = Monotonic.now!()
	var $checksum = 0.U64
	var $successes = 0.U64
	var $index = 0.U64
	while $index < iterations {
		parsed = parse(input, operation)
		match parsed {
			Ok(value) => {
				$successes = $successes + 1
				$checksum = $checksum + checksum(value.tree)
			}
			Err(_) => {}
		}
		$index = $index + 1
	}
	elapsed = start.elapsed!().to_nanos()
	Stdout.line!("{\"iterations\":${iterations.to_str()},\"elapsed_ns\":${elapsed.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()}}")?
	Ok({})
}

checksum : Markup.Tree -> U64
checksum = |tree| tree.roots.fold(0, |sum, index| sum + checksum_node(tree, index))

checksum_node : Markup.Tree, U64 -> U64
checksum_node = |tree, index| {
	node = tree.nodes.get(index) ?? crash "invalid benchmark tree edge"
	weight = match node.kind {
		Element(name) => 1 + name.local.count_utf8_bytes() + node.attributes.fold(0, |count, attr| {
			if namespace_declaration(attr.name) { count }
			else { count + 1 + attr.name.local.count_utf8_bytes() + attr.value.count_utf8_bytes() }
		})
		Text(value) => 1 + value.count_utf8_bytes()
		_ => 0
	}
	weight + node.children.fold(0, |sum, child| sum + checksum_node(tree, child))
}

parse : Str, Str -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
parse = |input, operation| match operation {
	"xml-raw" => Markup.parse_xml(input, {})
	"html-document-raw" => Html.parse_source(input, { recovery: Browser }, False)
	"html-fragment-raw" => Html.parse_source(input, { recovery: Browser }, True)
	"html-document-full" => Html.parse_with(input, {}).map_ok(|value| value.parsed)
	"html-fragment-full" => Html.parse_fragment(input, {}).map_ok(|value| value.parsed)
	"mathml-full" => MathML.parse_with(input, {}).map_ok(|value| value.parsed)
	_ => Svg.parse_with(input, {}).map_ok(|value| value.parsed)
}

quote = |value| JsonValue.to_str(JsonValue.String(value)) ?? "\"\""

namespace_uri = |namespace| match namespace {
	Uri(uri) => uri
	NoNamespace => ""
}

namespace_declaration : Markup.Name -> Bool
namespace_declaration = |name| name.namespace == Uri("http://www.w3.org/2000/xmlns/") or (name.namespace == NoNamespace and name.local == "xmlns")

encode_node : Markup.Tree, U64, Bool -> Str
encode_node = |tree, index, bindings| {
	match tree.nodes.get(index) {
		Err(_) => "null"
		Ok(node) => match node.kind {
			Text(text) => "[\"text\",${quote(text)}]"
			Comment(text) => "[\"comment\",${quote(text)}]"
			Doctype(doctype) => "[\"doctype\",${quote(doctype.name)},${quote(doctype.public_id)},${quote(doctype.system_id)}]"
			ProcessingInstruction(pi) => "[\"pi\",${quote(pi.target)},${quote(pi.data)}]"
			Element(name) => {
				attributes = node.attributes.keep_if(|attribute| bindings or !namespace_declaration(attribute.name))
				attrs = Str.join_with(attributes.map(|attribute| "[${quote(namespace_uri(attribute.name.namespace))},${quote(attribute.name.local)},${quote(attribute.value)}]"), ",")
				children = Str.join_with(node.children.map(|child| encode_node(tree, child, bindings)), ",")
				"[\"element\",${quote(namespace_uri(name.namespace))},${quote(name.local)},[${attrs}],[${children}]]"
			}
		}
	}
}

