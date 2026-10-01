app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Xml
import parser.Utf8

## Read one XML document from stdin and print the parse result as JSON:
## {"status":"ok","declaration":...,"root":NODE} with NODE either
## ["element",name,[[name,value],...],[NODE,...]] or ["text",text], or
## {"status":"error","line":L,"column":C,"message":M}.
main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"invalid_utf8\"}")?
		Ok(input) => {
			match Xml.parse_str(input) {
				Ok(xml) => Stdout.line!("{\"status\":\"ok\",\"declaration\":${encode_declaration(xml.declaration)},\"root\":${encode(xml.root)}}")?
				Err(InvalidXml(error)) =>
					Stdout.line!("{\"status\":\"error\",\"line\":${error.line.to_str()},\"column\":${error.column.to_str()},\"message\":${json_string(error.message)}}")?
			}
		}
	}
	Ok({})
}

encode_declaration : Try(Xml.Declaration, [Missing]) -> Str
encode_declaration = |declaration| {
	match declaration {
		Err(Missing) => "null"
		Ok(given) => {
			encoding =
				match given.encoding {
					Err(Missing) => "null"
					Ok(Utf8Encoding) => "\"utf-8\""
					Ok(OtherEncoding(name)) => json_string(name)
				}
			"{\"encoding\":${encoding}}"
		}
	}
}

encode : Xml.Node -> Str
encode = |node| {
	match node {
		Text(text) => "[\"text\",${json_string(text)}]"
		Element({ name, attributes, children }) => {
			attrs = Str.join_with(attributes.map(|attribute| "[${json_string(attribute.name)},${json_string(attribute.value)}]"), ",")
			"[\"element\",${json_string(name)},[${attrs}],[${Str.join_with(children.map(encode), ",")}]]"
		}
	}
}

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, out| {
	match bytes {
		[] => out
		['"', .. as rest] => escape_json(rest, out.concat(['\\', '"']))
		['\\', .. as rest] => escape_json(rest, out.concat(['\\', '\\']))
		[byte, .. as rest] if byte < 32 => {
			hi = if byte < 16 '0' else '1'
			lo = if byte < 16 byte else byte - 16
			digit = if lo < 10 lo + '0' else lo - 10 + 'a'
			escape_json(rest, out.concat(['\\', 'u', '0', '0', hi, digit]))
		}
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}
