app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.HTTP
import parser.Parser
import parser.Utf8
import parser.Xml
import parser.Yaml

# tag::located[]
yaml_message : Str, Str -> Str
yaml_message = |file_name, source| {
	match Yaml.parse_str(source) {
		Ok(_) => "${file_name}: ok"
		Err(YamlError({ line, column, message })) =>
			"${file_name}:${line.to_str()}:${column.to_str()}: ${message}"
	}
}

xml_message : Str, Str -> Str
xml_message = |file_name, source| {
	match Xml.parse_str(source) {
		Ok(_) => "${file_name}: ok"
		Err(XmlError({ line, column, message })) =>
			"${file_name}:${line.to_str()}:${column.to_str()}: ${message}"
	}
}

# end::located[]

# tag::csv[]
Row : { name : Str, count : U64 }

row : Parser(CSV.CSVRecord, Row)
row =
	CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string))
		.keep(CSV.field(CSV.u64))

csv_message : Str -> Str
csv_message = |source| {
	match CSV.parse_str(row, source) {
		Ok(rows) => "${rows.len().to_str()} rows"
		Err(SyntaxError(rest)) => "not valid CSV from here: ${rest.trim_end()}"
		Err(ParsingFailure(message)) => "a field did not match: ${message}"
		Err(ParsingIncomplete(extra)) => "a row has ${extra.len().to_str()} extra fields"
	}
}

# end::csv[]

# tag::http[]
http_message : Str -> Str
http_message = |source| {
	match Utf8.parse_str(HTTP.request, source) {
		Ok(request) => "request for ${request.uri}"
		Err(ParsingFailure(message)) => "400 Bad Request: ${message}"
		Err(ParsingIncomplete(_)) => "a second message follows the first"
	}
}

# end::http[]

main! = |_args| {
	Stdout.line!(yaml_message("config.yaml", "name: demo\nport: [8080\n"))?
	Stdout.line!(xml_message("feed.xml", "<feed>\n  <entry></feed>"))?
	Stdout.line!(csv_message("apples,3\npears,\"2\n"))?
	Stdout.line!(csv_message("apples,3\npears,many\n"))?
	Stdout.line!(csv_message("apples,3,red\n"))?
	Stdout.line!(http_message("GET / HTTP/1.1\r\n\r\n"))
}
