app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.AsciiDoc

## Read one complete UTF-8 AsciiDoc source from stdin, returning one JSON line.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	bytes = Stdin.read_to_end!()?
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"invalid_utf8\"}")?
		Ok(source) => {
			request = if (args.any(|arg| OsStr.display(arg) == "--request")) decode_request(source.to_utf8()) else { source, includes: [], expand: False }
			match AsciiDoc.parse_with(request.source, { includes: request.includes, attributes: [], max_depth: 16, expand_includes: request.expand }) {
				Ok(document) => Stdout.line!("{\"status\":\"ok\",\"projection\":{\"title\":${json_string(document.title)},\"blocks\":${encode_blocks(document.blocks)}},\"html\":${json_string(AsciiDoc.render(document))}}")?
				Err(Unsupported(error)) => Stdout.line!(encode_error("unsupported", error))?
				Err(InvalidAsciiDoc(error)) => Stdout.line!(encode_error("error", error))?
			}
		}
	}
	Ok({})
}

encode_error : Str, AsciiDoc.Error -> Str
encode_error = |status, error| "{\"status\":${json_string(status)},\"reason\":${json_string(error.message)},\"message\":${json_string(error.message)},\"offset\":${error.offset.to_str()},\"line\":${error.line.to_str()}}"

encode_blocks : List(AsciiDoc.Block) -> Str
encode_blocks = |blocks| "[${
	Str.join_with(
		blocks.keep_if(
			|block| match block {
				Rich(n) => n.context != "__document_header"
				_ => True
			},
		).map(encode_block),
		",",
	)
}]"

encode_block : AsciiDoc.Block -> Str
encode_block = |block| {
	match block {
		Paragraph(source) => encode_leaf("paragraph", source)
		Listing(source) => encode_leaf("listing", source)
		Literal(source) => encode_leaf("literal", source)
		ThematicBreak => encode_leaf("thematic_break", "")
		Rich(node) => {
			context = node.context
			level = if (context == "section") node.level else 0
			source = if (["list_item", "image", "video", "audio", "table"].contains(context)) "" else node.source
			title = if (context == "list_item") "" else AsciiDoc.render_inline_attributes(node.title, node.attrs)
			var $blocks = []
			for item in node.blocks {
				if context == "dlist" and has_definition(item) {
					$blocks = $blocks.append(AsciiDoc.Block.Rich({ context: "list_item", source: "", title: "", style: "", id: "", attrs: AsciiDoc.attributes([]), blocks: [], level: 0 }))
				}
				$blocks = $blocks.append(item)
			}
			"{\"context\":${json_string(context)},\"level\":${level.to_str()},\"title\":${json_string(title)},\"source\":${json_string(source)},\"blocks\":${encode_blocks($blocks)}}"
		}
		Section({ level, title, blocks }) => "{\"context\":\"section\",\"level\":${level.to_str()},\"title\":${json_string(AsciiDoc.render_inline(title, []))},\"source\":\"\",\"blocks\":${encode_blocks(blocks)}}"
	}
}

encode_leaf : Str, Str -> Str
encode_leaf = |context, source| "{\"context\":${json_string(context)},\"level\":0,\"title\":\"\",\"source\":${json_string(source)},\"blocks\":[]}"

json_string : Str -> Str
json_string = |text| "\"${Str.from_utf8_lossy(escape_json(text.to_utf8(), []))}\""

escape_json : List(U8), List(U8) -> List(U8)
escape_json = |bytes, out| {
	match bytes {
		[] => out
		['"', .. as rest] => escape_json(rest, out.concat(['\\', '"']))
		['\\', .. as rest] => escape_json(rest, out.concat(['\\', '\\']))
		[byte, .. as rest] if byte < 32 => {
			hi = if (byte < 16) '0' else '1'
			lo = if (byte < 16) byte else byte - 16
			digit = if (lo < 10) lo + '0' else lo - 10 + 'a'
			escape_json(rest, out.concat(['\\', 'u', '0', '0', hi, digit]))
		}
		[byte, .. as rest] => escape_json(rest, out.append(byte))
	}
}

# Requests use length framing, so virtual include text can contain arbitrary newlines.
decode_request : List(U8) -> { source : Str, includes : List({ path : Str, source : Str }), expand : Bool }
decode_request = |bytes| {
	if !Str.from_utf8_lossy(bytes.take_first(6)).starts_with("ADOC1\n") {
		return { source: Str.from_utf8_lossy(bytes), includes: [], expand: False }
	}
	count = read_number(bytes.drop_first(6))
	var $rest = count.rest
	var $includes = []
	var $i = 0.U64
	while $i < count.value and $i < 128 {
		path_len = read_number($rest)
		path = Str.from_utf8_lossy(path_len.rest.take_first(path_len.value))
		source_len = read_number(path_len.rest.drop_first(path_len.value))
		source = Str.from_utf8_lossy(source_len.rest.take_first(source_len.value))
		$rest = source_len.rest.drop_first(source_len.value)
		$includes = $includes.append({ path, source })
		$i = $i + 1
	}
	{ source: Str.from_utf8_lossy($rest), includes: $includes, expand: True }
}

read_number : List(U8) -> { value : U64, rest : List(U8) }
read_number = |bytes| {
	var $n = 0.U64
	var $i = 0.U64
	while $i < bytes.len() and (bytes.get($i) ?? 10) != 10 {
		b = bytes.get($i) ?? 0
		if b < 48 or b > 57 or $n > 262144 {
			return { value: 0, rest: [] }
		}
		$n = $n * 10 + (b - 48).to_u64()
		$i = $i + 1
	}
	{ value: $n, rest: bytes.drop_first($i + 1) }
}

has_definition : AsciiDoc.Block -> Bool
has_definition = |block| match block {
	Rich(n) => n.source != "" or !n.blocks.is_empty()
	_ => False
}
