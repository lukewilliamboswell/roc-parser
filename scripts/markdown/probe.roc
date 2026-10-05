app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Markdown

## Batch probe for scripts/review_markdown.py.
##
## stdin is a sequence of frames `<mode> <byte length>\n<bytes>`, where mode is
## `inline` (parse with `Markdown.parse_inlines`) or `doc` (parse with `Markdown.parse_str`
## and report paragraphs). One JSON line is printed per frame.
main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	var $pos = 0
	while $pos < bytes.len() {
		header_end = find_newline(bytes, $pos)
		header = bytes.sublist({ start: $pos, len: header_end - $pos })
		parts = split_space(header)
		size = parse_decimal(parts.second)
		body = bytes.sublist({ start: header_end + 1, len: size })
		Stdout.line!(run_case(parts.first, body))?
		$pos = header_end + 1 + size
	}
	Ok({})
}

run_case : List(U8), List(U8) -> Str
run_case = |mode, body| {
	text = Str.from_utf8_lossy(body)
	if mode == "count".to_utf8() {
		# Timing mode: parse and count nodes without building JSON.
		"{\"status\":\"ok\",\"nodes\":${count_nodes(Markdown.parse_inlines(text)).to_str()}}"
	} else if mode == "inline".to_utf8() {
		"{\"status\":\"ok\",\"inlines\":${encode_inlines(Markdown.parse_inlines(text))}}"
	} else {
		"{\"status\":\"ok\",\"blocks\":[${Str.join_with(Markdown.parse_str(text).map(encode_block), ",")}]}"
	}
}

encode_block : Markdown -> Str
encode_block = |block| {
	match block {
		Paragraph(inlines) => "[\"p\",${encode_inlines(inlines)}]"
		Heading({ level, content }) => "[\"h\",${level.to_str()},${encode_inlines(content)}]"
		_ => "[\"other\",${json_string(Str.inspect(block))}]"
	}
}

encode_inlines : List(Markdown.Inline) -> Str
encode_inlines = |nodes| "[${Str.join_with(nodes.map(encode_inline), ",")}]"

encode_inline : Markdown.Inline -> Str
encode_inline = |node| {
	match node {
		Text(text) => "[\"text\",${json_string(text)}]"
		Strong(children) => "[\"strong\",${encode_inlines(children)}]"
		Emphasis(children) => "[\"emph\",${encode_inlines(children)}]"
		Strikethrough(children) => "[\"del\",${encode_inlines(children)}]"
		InlineCode(code) => "[\"code\",${json_string(code)}]"
		Link({ label, target }) => "[\"link\",${json_string(target.href)},${encode_title(target.title)},${encode_inlines(label)}]"
		Image({ alt, target }) => "[\"image\",${json_string(target.href)},${encode_title(target.title)},${encode_inlines(alt)}]"
		HardBreak => "[\"br\"]"
		HtmlInline(html) => "[\"html\",${json_string(html)}]"
	}
}

count_nodes : List(Markdown.Inline) -> U64
count_nodes = |nodes| {
	nodes.fold(
		0,
		|sum, node| {
			children =
				match node {
					Strong(inner) => count_nodes(inner)
					Emphasis(inner) => count_nodes(inner)
					Strikethrough(inner) => count_nodes(inner)
					Link({ label, .. }) => count_nodes(label)
					Image({ alt, .. }) => count_nodes(alt)
					_ => 0
				}
			sum + 1 + children
		},
	)
}

encode_title : Try(Str, [Missing]) -> Str
encode_title = |title| {
	match title {
		Ok(text) => json_string(text)
		Err(Missing) => "null"
	}
}

find_newline : List(U8), U64 -> U64
find_newline = |bytes, from| {
	var $index = from
	while $index < bytes.len() and (bytes.get($index) ?? '\n') != '\n' {
		$index = $index + 1
	}
	$index
}

split_space : List(U8) -> { first : List(U8), second : List(U8) }
split_space = |bytes| {
	match bytes.find_first_index(|b| b == ' ') {
		Ok(index) => { first: bytes.take_first(index), second: bytes.drop_first(index + 1) }
		Err(_) => { first: bytes, second: [] }
	}
}

parse_decimal : List(U8) -> U64
parse_decimal = |digits| digits.fold(0, |sum, digit| sum * 10 + (digit - '0').to_u64())

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, acc| {
	var $out = acc
	for byte in bytes {
		if byte == '"' {
			$out = $out.concat(['\\', '"'])
		} else if byte == '\\' {
			$out = $out.concat(['\\', '\\'])
		} else if byte < 32 {
			hi = if byte < 16 '0' else '1'
			lo = if byte < 16 byte else byte - 16
			digit = if lo < 10 lo + '0' else lo - 10 + 'a'
			$out = $out.concat(['\\', 'u', '0', '0', hi, digit])
		} else {
			$out = $out.append(byte)
		}
	}
	$out
}
