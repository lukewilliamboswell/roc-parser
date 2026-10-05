app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.24.0/AEjfyaMFFbh8FJrkkHJy68riVNPr3Qp6c6PawWQjBwMH.tar.zst",
	parser: "../../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.Markdown

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
			Stdout.line!("{\"status\":\"ok\",\"value\":${encode_blocks(Markdown.parse_str(input))}}")?
		}
	}
	Ok({})
}

encode_blocks : List(Markdown) -> Str
encode_blocks = |blocks| "[${Str.join_with(blocks.map(encode_block), ",")}]"

encode_inlines : List(Markdown.Inline) -> Str
encode_inlines = |inlines| "[${Str.join_with(inlines.map(encode_inline), ",")}]"

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
			items_json = Str.join_with(items.map(|item| "[\"${item.task.to_str()}\",${encode_blocks(item.blocks)}]"), ",")
			"[\"list\",${kind_json},${if loose "false" else "true"},[${items_json}]]"
		}
		Code({ info, pre }) => "[\"code\",${json_string(info)},${json_string(pre)}]"
		ThematicBreak => "[\"hr\"]"
		Table({ header, align, rows }) => {
			align_json = Str.join_with(align.map(|a| json_string(a.to_str())), ",")
			row_json = |cells| "[${Str.join_with(cells.map(encode_inlines), ",")}]"
			"[\"table\",[${align_json}],${row_json(header)},[${Str.join_with(rows.map(row_json), ",")}]]"
		}
		HtmlBlock(text) => "[\"html\",${json_string(text)}]"
		Frontmatter(raw) => "[\"frontmatter\",${json_string(raw)}]"
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

encode_title : Try(Str, [Missing]) -> Str
encode_title = |title| {
	match title {
		Ok(text) => json_string(text)
		Err(Missing) => "null"
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
