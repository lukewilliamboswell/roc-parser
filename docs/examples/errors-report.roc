app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	parser: "../../package/main.roc",
}

import cli.Stdout
import parser.CSV
import parser.HTTP
import parser.JsonValue
import parser.Parser
import parser.Xml
import parser.Yaml

# tag::located[]
yaml_message : Str, Str -> Str
yaml_message = |file_name, source| {
	match Yaml.parse_str(source) {
		Ok(_) => "${file_name}: ok"
		Err(InvalidYaml({ line, column, problem })) =>
			"${file_name}:${line.to_str()}:${column.to_str()}: ${Str.inspect(problem)}"
	}
}

xml_message : Str, Str -> Str
xml_message = |file_name, source| {
	match Xml.parse_str(source) {
		Ok(_) => "${file_name}: ok"
		Err(InvalidXml({ line, column, problem })) =>
			"${file_name}:${line.to_str()}:${column.to_str()}: ${Str.inspect(problem)}"
	}
}

json_message : Str, Str -> Str
json_message = |file_name, source| {
	match JsonValue.parse_str(source) {
		Ok(_) => "${file_name}: ok"
		Err(InvalidJsonValue({ line, column, problem })) =>
			"${file_name}:${line.to_str()}:${column.to_str()}: ${Str.inspect(problem)}"
	}
}

# end::located[]

# tag::csv[]
Row : { name : Str, count : U64 }

row : Parser(CSV.Record, Row, _)
row =
	CSV.record(|name| |count| { name, count })
		.keep(CSV.field(CSV.string({})))
		.keep(CSV.field(CSV.u64({})))

csv_message : Str -> Str
csv_message = |source| {
	match CSV.parse_with(row, source) {
		Ok(rows) => "${rows.len().to_str()} rows"
		Err(InvalidCsv({ line, column, record, field, problem })) =>
			"line ${line.to_str()}, column ${column.to_str()} (record ${record.to_str()}, field ${field.to_str()}): ${Str.inspect(problem)}"
		Err(InvalidRow({ line, column, record, field, problem })) =>
			"line ${line.to_str()}, column ${column.to_str()} (record ${record.to_str()}, field ${field.to_str()}): ${Str.inspect(problem)}"
	}
}

# end::csv[]

# tag::http[]
http_message : Str -> Str
http_message = |source| {
	match HTTP.parse_request(source.to_utf8()) {
		Ok({ request, rest: _ }) => "request for ${request.target}"
		Err(InvalidHttp({ problem, offset })) => "400 Bad Request (byte ${offset.to_str()}): ${Str.inspect(problem)}"
	}
}

# end::http[]

main! = |_args| {
	Stdout.line!(yaml_message("config.yaml", "name: demo\nport: [8080\n"))?
	Stdout.line!(xml_message("feed.xml", "<feed>\n  <entry></feed>"))?
	Stdout.line!(json_message("package.json", "{\n  \"port\": 8080,\n}"))?
	Stdout.line!(csv_message("apples,3\npears,\"2\n"))?
	Stdout.line!(csv_message("apples,3\npears,many\n"))?
	Stdout.line!(csv_message("apples,3,red\n"))?
	Stdout.line!(http_message("GET / HTTP/1.1\r\n\r\n"))
}
