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
location_text : Parser.Location -> Str
location_text = |location| match location {
	Text({ line, column, .. }) => "${line.to_str()}:${column.to_str()}"
	LineColumn({ line, column }) => "${line.to_str()}:${column.to_str()}"
	ByteOffset(offset) => "byte ${offset.to_str()}"
	InputOffset(offset) => "input ${offset.to_str()}"
	Unavailable(reason) => "location unavailable (${Str.inspect(reason)})"
}

render_error : Str, Parser.Diagnostic(problem) -> Str
render_error = |name, error| {
	path = if (error.path.is_empty()) "" else " (path ${Str.inspect(error.path)})"
	"${name}:${location_text(error.location)}: ${Str.inspect(error.problem)}${path}"
}

yaml_message : Str, Str -> Str
yaml_message = |file_name, source| match Yaml.parse_str(source, {}) {
	Ok(_) => "${file_name}: ok"
	Err(InvalidYaml(error)) => render_error(file_name, error)
}

xml_message : Str, Str -> Str
xml_message = |file_name, source| match Xml.parse_str(source, {}) {
	Ok(_) => "${file_name}: ok"
	Err(InvalidXml(error)) => render_error(file_name, error)
}

json_message : Str, Str -> Str
json_message = |file_name, source| match JsonValue.parse_str(source, {}) {
	Ok(_) => "${file_name}: ok"
	Err(InvalidJsonValue(error)) => render_error(file_name, error)
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
	match CSV.rows_parser({ row: row }).parse_str(source) {
		Ok(rows) => "${rows.len().to_str()} rows"
		Err(ParseError({ problem: InvalidCsv(error), .. })) => render_error("CSV", error)
		Err(ParseError({ problem: InvalidRow(error), .. })) => render_error("CSV row", error)
		Err(ParseError(error)) => render_error("CSV parser", error)
	}
}

# end::csv[]

# tag::http[]
http_message : Str -> Str
http_message = |source| {
	match HTTP.Request.parse_bytes(source.to_utf8(), {}) {
		Ok(request) => "request for ${request.target}"
		Err(InvalidHttp(error)) => render_error("400 Bad Request", error)
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
