app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.22.0/F1JVZPYfWP71s8vk6tHcV1Qx1Ef6CZkwswGoCn8VHZmL.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.HTTP
import parser.Markdown
import parser.Parser
import parser.String
import parser.Xml
import parser.Yaml

# tag::entry-points[]
# Parser and String: build a parser from combinators, run it on a Str.
pair : Parser(String.Utf8, (U64, U64))
pair = Parser.const(|a| |b| (a, b)).keep(String.digits).skip(String.codeunit(',')).keep(String.digits)

# CSV: decode every record with a record parser.
row : Parser(CSV.CSVRecord, { name : Str, age : U64 })
row = CSV.record(|name| |age| { name, age }).keep(CSV.field(CSV.string)).keep(CSV.field(CSV.u64))

# HTTP: parse one message; bytes after it are left for the next message.
http_target : Str -> Str
http_target = |text| {
	match String.parse_str(HTTP.request, text) {
		Ok(req) => req.uri
		Err(_) => "invalid request"
	}
}

# Xml and Yaml: whole-document functions that report line and column.
xml_root : Str -> Str
xml_root = |text| {
	match Xml.parse_str(text) {
		Ok(doc) => Str.inspect(doc.root)
		Err(XmlError(e)) => "${e.line.to_str()}:${e.column.to_str()}: ${e.message}"
	}
}

yaml_value : Str -> Str
yaml_value = |text| {
	match Yaml.parse_str(text) {
		Ok(value) => Yaml.to_inspect(value)
		Err(YamlError(e)) => "${e.line.to_str()}:${e.column.to_str()}: ${e.message}"
	}
}

# Markdown: the `all` parser accepts every complete document.
markdown_blocks : Str -> Str
markdown_blocks = |text| {
	match String.parse_str(Markdown.all, text) {
		Ok(blocks) => blocks.map(Markdown.to_debug_str) |> Str.join_with("\n")
		Err(_) => "unreachable"
	}
}

# end::entry-points[]

main! = |_args| {
	Stdout.line!(Str.inspect(String.parse_str(pair, "3,4")))?
	Stdout.line!(Str.inspect(CSV.parse_str(row, "Ada,36\nAlan,41\n")))?
	Stdout.line!(http_target("GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n"))?
	Stdout.line!(xml_root("<greeting lang=\"en\">hi</greeting>"))?
	Stdout.line!(xml_root("<a><b></a>"))?
	Stdout.line!(yaml_value("title: Notes\ndraft: false\n"))?
	Stdout.line!(markdown_blocks("# Title\n\nSome *text*.\n"))?
	Ok({})
}
