app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.String

## Arbitrary UTF-8 is a Markdown document (CommonMark has no syntax errors), and:
## - the result is well formed: table rows are as wide as the header, lists
##   have items, code and HTML block text ends with a line ending, and
##   frontmatter can only come first;
## - LF, CRLF and CR line endings give the same result;
## - a final line ending, or a blank first line, changes nothing;
## - expanding tabs in each line's indentation to spaces (tab stops of 4)
##   gives the same block structure. Only code and raw HTML keep a tab's
##   leftover columns verbatim, so their text is not compared.
##
## Corpus files are raw UTF-8 Markdown, so scripts/review_markdown_blocks.py can also
## compare a corpus with cmark-gfm.

Parsed : List(Markdown)

parse : Str -> Parsed
parse = |text| {
	match String.parse_str(Markdown.all, text) {
		Ok(blocks) => blocks
		Err(_) => crash "Markdown.all rejected a document:\n${Str.inspect(text)}"
	}
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match Str.from_utf8(bytes) {
		Err(_) => Fuzz.reject
		Ok(input) => {
			result = parse(input)
			check_well_formed(input, result)
			if !bytes.contains('\r') {
				check_same(input, "CRLF line endings", Str.replace_each(input, "\n", "\r\n"), result)
				check_same(input, "CR line endings", Str.replace_each(input, "\n", "\r"), result)
			}
			last = bytes.last() ?? '\n'
			if last != '\n' and last != '\r' {
				check_same(input, "a final line ending", Str.concat(input, "\n"), result)
			}
			if !Str.starts_with(input, "---") {
				check_same(input, "a blank first line", Str.concat("\n", input), result)
			}
			if bytes.contains('\t') {
				expanded = parse(expand_indentation_tabs(input))
				if skeleton(expanded) != skeleton(result) {
					crash "expanding indentation tabs changed the block structure\ninput: ${Str.inspect(input)}\nplain:    ${show(result)}\nexpanded: ${show(expanded)}"
				}
			}
			Fuzz.keep
		}
	}
}

check_same : Str, Str, Str, Parsed -> {}
check_same = |input, what, variant, result| {
	other = parse(variant)
	if other != result {
		crash "${what} changed the result\ninput: ${Str.inspect(input)}\nbefore: ${show(result)}\nafter:  ${show(other)}"
	}
}

check_well_formed : Str, Parsed -> {}
check_well_formed = |input, blocks| {
	for block in blocks.drop_first(1) {
		match block {
			Frontmatter(_) => crash "frontmatter after the first block: ${Str.inspect(input)}"
			_ => {}
		}
	}
	check_blocks(input, blocks)
}

check_blocks : Str, List(Markdown) -> {}
check_blocks = |input, blocks| {
	for block in blocks {
		match block {
			Blockquote(children) => check_blocks(input, children)
			ListBlock(list) => {
				items = list.items
				if items.is_empty() {
					crash "a list without items: ${Str.inspect(input)}"
				}
				for item in items {
					check_blocks(input, item.blocks)
				}
			}
			Table({ header, align, rows }) => {
				if header.len() != align.len() or rows.any(|row| row.len() != align.len()) or align.is_empty() {
					crash "a table row is not as wide as the delimiter row: ${Str.inspect(input)}"
				}
			}
			Code(code) =>
				if !code.pre.is_empty() and !Str.ends_with(code.pre, "\n") {
					crash "code text must end with a line ending: ${Str.inspect(input)}"
				}
			HtmlBlock(text) =>
				if !Str.ends_with(text, "\n") {
					crash "HTML block text must end with a line ending: ${Str.inspect(input)}"
				}
			_ => {}
		}
	}
}

## Replace the tabs in each line's leading spaces and tabs by spaces up to the
## next multiple of four columns.
expand_indentation_tabs : Str -> Str
expand_indentation_tabs = |text| {
	var $out = []
	var $column = 0
	var $leading = Bool.True
	for byte in text.to_utf8() {
		if byte == '\n' or byte == '\r' {
			$out = $out.append(byte)
			$column = 0
			$leading = Bool.True
		} else if $leading and byte == '\t' {
			width = 4 - ($column % 4)
			$out = $out.concat(List.repeat(' ', width))
			$column = $column + width
		} else if $leading and byte == ' ' {
			$out = $out.append(' ')
			$column = $column + 1
		} else {
			$out = $out.append(byte)
			$leading = Bool.False
		}
	}
	Str.from_utf8($out) ?? text
}

## The block structure without the verbatim text of code, raw HTML and
## frontmatter.
skeleton : List(Markdown) -> List(Markdown)
skeleton = |blocks| {
	blocks.map(
		|block| {
			match block {
				Code(code) => Code({ info: code.info, pre: "" })
				HtmlBlock(_) => HtmlBlock("")
				Frontmatter(_) => Frontmatter({ raw: "" })
				Blockquote(children) => Blockquote(skeleton(children))
				ListBlock({ kind, loose, items }) => ListBlock({ kind, loose, items: items.map(|item| { task: item.task, blocks: skeleton(item.blocks) }) })
				other => other
			}
		},
	)
}

show : Parsed -> Str
show = |blocks| Str.join_with(blocks.map(Markdown.to_debug_str), "\n")

target = Fuzz.target_with({
	name: "markdown-document",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
