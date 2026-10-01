app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Markdown
import parser.String

## Reads a Markdown document from stdin and prints the parsed block tree as
## JSON for scripts/review_markdown_blocks.py. Node shapes:
##   ["heading", level, inlines]      ["paragraph", inlines]
##   ["blockquote", blocks]           ["code", info, text]
##   ["list", "bullet" | start, tight, [[task, blocks], ...]]
##   ["hr"]  ["html", text]  ["frontmatter", raw]
##   ["table", [align...], [cell...], [[cell...]...]]
## Inlines: ["text", s] ["strong", is] ["emph", is] ["del", is] ["code", s]
##   ["link", href, title|null, is] ["image", href, title|null, is] ["br"] ["html", s]
main! : List(OsStr) => Try({}, _)
main! = |_| {
	bytes = Stdin.read_to_end!()?
	match Str.from_utf8(bytes) {
		Err(_) => Stdout.line!("{\"status\":\"invalid_utf8\"}")?
		Ok(input) => {
			match String.parse_str(Markdown.all, input) {
				Ok(blocks) => Stdout.line!("{\"status\":\"ok\",\"value\":${encode_blocks(blocks)}}")?
				Err(_) => Stdout.line!("{\"status\":\"error\"}")?
			}
		}
	}
	Ok({})
}

encode_blocks : List(Markdown) -> Str
encode_blocks = |blocks| "[${blocks.map(encode_block) |> Str.join_with(",")}]"

encode_inlines : List(Markdown.Inline) -> Str
encode_inlines = |inlines| "[${inlines.map(encode_inline) |> Str.join_with(",")}]"

encode_block : Markdown -> Str
encode_block = |block| {
	match block {
		Heading({ level, content }) => "[\"heading\",${level.to_str()},${encode_inlines(content)}]"
		Paragraph(content) => "[\"paragraph\",${encode_inlines(content)}]"
		Blockquote(children) => "[\"blockquote\",${encode_blocks(children)}]"
		ListBlock({ kind, loose, items }) => {
			kind_json =
				match kind {
					Unordered => "\"bullet\""
					Ordered({ start }) => start.to_str()
				}
			items_json = items.map(|item| "[\"${item.task.to_str()}\",${encode_blocks(item.blocks)}]") |> Str.join_with(",")
			"[\"list\",${kind_json},${if loose "false" else "true"},[${items_json}]]"
		}
		Code({ info, pre }) => "[\"code\",${json_string(info)},${json_string(pre)}]"
		ThematicBreak => "[\"hr\"]"
		Table({ header, align, rows }) => {
			align_json = align.map(|a| json_string(a.to_str())) |> Str.join_with(",")
			row_json = |cells| "[${cells.map(encode_inlines) |> Str.join_with(",")}]"
			"[\"table\",[${align_json}],${row_json(header)},[${rows.map(row_json) |> Str.join_with(",")}]]"
		}
		HtmlBlock(text) => "[\"html\",${json_string(text)}]"
		Frontmatter({ raw }) => "[\"frontmatter\",${json_string(raw)}]"
		TODO(text) => "[\"todo\",${json_string(text)}]"
	}
}

encode_inline : Markdown.Inline -> Str
encode_inline = |inline| {
	match inline {
		Text(text) => "[\"text\",${json_string(text)}]"
		Strong(children) => "[\"strong\",${encode_inlines(children)}]"
		Emphasis(children) => "[\"emph\",${encode_inlines(children)}]"
		Strikethrough(children) => "[\"del\",${encode_inlines(children)}]"
		InlineCode(text) => "[\"code\",${json_string(text)}]"
		Link({ label, target }) => "[\"link\",${json_string(target.href)},${encode_title(target.title)},${encode_inlines(label)}]"
		Image({ alt, target }) => "[\"image\",${json_string(target.href)},${encode_title(target.title)},${encode_inlines(alt)}]"
		HardBreak => "[\"br\"]"
		HtmlInline(text) => "[\"html\",${json_string(text)}]"
	}
}

encode_title : [Some(Str), None] -> Str
encode_title = |title| {
	match title {
		Some(text) => json_string(text)
		None => "null"
	}
}

json_string : Str -> Str
json_string = |text| "\"${String.str_from_utf8(escape_json(text.to_utf8(), []))}\""

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
