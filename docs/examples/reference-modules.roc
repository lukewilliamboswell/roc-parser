app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.HTTP
import parser.Markdown
import parser.Parser
import parser.Utf8
import parser.Xml
import parser.Yaml

# tag::entry-points[]
# Parser and Utf8: build a parser from combinators, run it on a Str.
pair : Parser(Utf8.Bytes, (U64, U64))
pair = Parser.const(|a| |b| (a, b)).keep(Utf8.digits).skip(Utf8.codeunit(',')).keep(Utf8.digits)

# CSV: decode each row into a record type the compiler infers from use.
people : Str -> Try(List({ name : Str, age : U64 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
people = |text| CSV.parse(text)

# HTTP: parse one message; bytes after it are left for the next message.
http_target : Str -> Str
http_target = |text| {
	match HTTP.parse_request(text.to_utf8()) {
		Ok({ request, rest: _ }) => request.target
		Err(InvalidHttp(e)) => "invalid request at byte ${e.offset.to_str()}"
	}
}

# Xml and Yaml: whole-document functions that report line and column.
xml_root : Str -> Str
xml_root = |text| {
	match Xml.parse_str(text) {
		Ok(doc) => Str.inspect(doc.root)
		Err(InvalidXml(e)) => "${e.line.to_str()}:${e.column.to_str()}: ${e.message}"
	}
}

yaml_value : Str -> Str
yaml_value = |text| {
	match Yaml.parse_str(text) {
		Ok(value) => value.to_inspect()
		Err(InvalidYaml(e)) => "${e.line.to_str()}:${e.column.to_str()}: ${e.message}"
	}
}

# Yaml can also decode straight into a record.
config : Str -> Try({ title : Str, draft : Bool }, [InvalidYaml(Yaml.Error), MissingRequiredField(Str)])
config = |text| Yaml.decode(text)

# Markdown: parsing never fails, so `parse_str` returns the blocks directly.
markdown_blocks : Str -> Str
markdown_blocks = |text| Str.join_with(Markdown.parse_str(text).map(Str.inspect), "\n")

# end::entry-points[]

main! = |_args| {
	Stdout.line!(Str.inspect(Utf8.parse_str(pair, "3,4")))?
	Stdout.line!(Str.inspect(people("name,age\nAda,36\nAlan,41\n")))?
	Stdout.line!(http_target("GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n"))?
	Stdout.line!(xml_root("<greeting lang=\"en\">hi</greeting>"))?
	Stdout.line!(xml_root("<a><b></a>"))?
	Stdout.line!(yaml_value("title: Notes\ndraft: false\n"))?
	Stdout.line!(Str.inspect(config("title: Notes\ndraft: false\n")))?
	Stdout.line!(markdown_blocks("# Title\n\nSome *text*.\n"))?
	Ok({})
}
