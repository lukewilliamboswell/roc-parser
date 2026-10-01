app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.Utf8

## Link reference definition property (CommonMark 4.7). Fuzzer bytes choose a
## few definitions and how each is written: label case and inner whitespace,
## a label split over two lines, pointy or plain destinations with escapes and
## balanced parentheses, an optional title in any of the three quote styles
## (possibly on the next line or spanning lines), and the spaces, tabs and line
## endings between the parts. Some are deliberately invalid (no destination,
## unbalanced parentheses, a control character, junk after the title, an empty
## or bracketed label, or a definition that would interrupt a paragraph).
## Definitions sit at the start of paragraphs, in block quotes or list items,
## possibly followed by ordinary text.
##
## The document ends with one paragraph that refers to every label as
## `[text][label]`. The expectation, built from the spec and never from the
## parser: valid definitions disappear and the first definition of a label
## wins; each reference becomes a link with the definition's unescaped
## destination and title; references to invalid or missing definitions stay
## literal text; text after the definitions remains a paragraph.

Cur : { bytes : List(U8), pos : U64 }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

labels : List(Str)
labels = ["foo", "Bar baz", "ROC", "a b c", "x"]

## How a label is written in the definition: same text in another case and
## with its inner spaces stretched or broken over a line.
write_label : Str, U64 -> Str
write_label = |label, style| {
	match style {
		0 => label
		1 => Str.with_ascii_uppercased(label)
		2 => Str.replace_each(label, " ", "  \t ")
		3 => " ${Str.with_ascii_lowercased(label)} "
		_ => Str.replace_each(label, " ", "\n")
	}
}

Dest : { text : Str, href : Str }

destinations : List(Dest)
destinations = [
	{ text: "/url", href: "/url" },
	{ text: "https://roc-lang.org/a?b=c#d", href: "https://roc-lang.org/a?b=c#d" },
	{ text: "<my url>", href: "my url" },
	{ text: "<>", href: "" },
	{ text: "foo(and(bar))", href: "foo(and(bar))" },
	{ text: "\\(foo\\)", href: "(foo)" },
	{ text: "<a\\>b>", href: "a>b" },
	{ text: "/u\\*r\\l", href: "/u*r\\l" },
	{ text: "\"quoted\"", href: "\"quoted\"" },
]

Title : { text : Str, value : [Some(Str), None] }

titles : List(Title)
titles = [
	{ text: "", value: None },
	{ text: "\"title\"", value: Some("title") },
	{ text: "'it''s'", value: None },
	{ text: "'single'", value: Some("single") },
	{ text: "(paren)", value: Some("paren") },
	{ text: "\"two\nlines\"", value: Some("two\nlines") },
	{ text: "\"esc \\\" q\"", value: Some("esc \" q") },
	{ text: "(a \\) b)", value: Some("a ) b") },
	{ text: "\"\"", value: Some("") },
]

## Separators between the label's colon, the destination and the title: some
## spaces or tabs, possibly with one line ending.
separators : List(Str)
separators = [" ", "\t", "  ", "\n", " \n  ", "\t\n"]

Def : { text : Str, label : Str, target : [Valid(Markdown.LinkTarget), Invalid], junk : Bool }

gen_def : Cur -> { def : Def, cur : Cur }
gen_def = |start| {
	label_choice = pick(start, labels.len())
	label = labels.get(label_choice.n) ?? "foo"
	style = pick(label_choice.cur, 5)
	dest_choice = pick(style.cur, destinations.len())
	dest = destinations.get(dest_choice.n) ?? { text: "/url", href: "/url" }
	title_choice = pick(dest_choice.cur, titles.len())
	title = titles.get(title_choice.n) ?? { text: "", value: None }
	sep1 = pick(title_choice.cur, separators.len())
	sep2 = pick(sep1.cur, separators.len())
	trail = pick(sep2.cur, 3)
	broken = pick(trail.cur, 12)
	written_label = write_label(label, style.n)
	before_dest = separators.get(sep1.n) ?? " "
	before_title = separators.get(sep2.n) ?? " "
	# (No trailing spaces after a junk title: they would make a hard break.)
	trailing = if title.text == "'it''s'" "" else if trail.n == 1 "  " else if trail.n == 2 "\t" else ""
	# 'it''s' is not a title (it ends at the second quote and junk follows), so
	# if it starts on the destination's line the definition is invalid; on the
	# next line the definition stands without a title and the junk is text.
	junk_title = title.text == "'it''s'"
	title_part = if title.text.is_empty() "" else "${before_title}${title.text}"
	title_value = if junk_title None else title.value
	valid_shape = !(junk_title and !Str.contains(before_title, "\n"))
	text_ok = "[${written_label}]:${before_dest}${dest.text}${title_part}${trailing}"
	target = { href: dest.href, title: title_value }
	{ text, valid } =
		match broken.n {
			0 => { text: "[${written_label}]: <a\nb>", valid: Bool.False }
			1 => { text: "[${written_label}]:${before_dest}(open${title_part}", valid: Bool.False }
			2 => { text: "[${written_label}]:${before_dest}/a\u(7)b${title_part}", valid: Bool.False }
			3 => { text: "[${written_label}]:${before_dest}${dest.text} \"t\" junk", valid: Bool.False }
			4 => { text: "[${written_label}]:${before_dest}<bar>(baz)", valid: Bool.False }
			5 => { text: "[${written_label}[x]]:${before_dest}${dest.text}", valid: Bool.False }
			_ => { text: text_ok, valid: valid_shape }
		}
	junk = valid and junk_title and broken.n > 5
	{ def: { text, label, target: if valid Valid(target) else Invalid, junk }, cur: broken.cur }
}

Input : { markdown : Str, expected : List(Markdown) }

Block : { lines : List(Str), defs : List(Def), node : [Some(Markdown), None] }

## One paragraph that starts with definitions, possibly followed by text.
gen_block : Cur -> { block : Block, cur : Cur }
gen_block = |start| {
	count = pick(start, 3)
	var $cur = count.cur
	var $defs = []
	var $index = 0
	while $index <= count.n {
		generated = gen_def($cur)
		$cur = generated.cur
		$defs = $defs.append(generated.def)
		$index = $index + 1
	}
	text_choice = pick($cur, 3)
	$cur = text_choice.cur
	# Only the definitions before the first invalid one, or up to one whose
	# junk title line ends the run, are definitions; the rest is text.
	valid_prefix = take_valid($defs)
	all_valid = valid_prefix.len() == $defs.len()
	tail_text = if text_choice.n == 0 "tail words" else ""
	junk = match valid_prefix.last() {
		Ok(def) if all_valid and def.junk => "'it''s'"
		_ => ""
	}
	lines = $defs.map(|def| def.text).concat(if tail_text.is_empty() [] else [tail_text]) |> Str.join_with("\n") |> Str.split_on("\n")
	# Only an all-valid run followed by plain text gives a predictable node;
	# otherwise the paragraph's content is checked loosely (see `test`).
	node =
		if all_valid {
			rest = [junk, tail_text].keep_if(|part| !part.is_empty())
			if rest.is_empty() None else Some(Paragraph([Text(Str.join_with(rest, "\n"))]))
		} else {
			Some(Paragraph([Text("?")]))
		}
	{ block: { lines, defs: valid_prefix, node }, cur: $cur }
}

take_valid : List(Def) -> List(Def)
take_valid = |defs| {
	var $out = []
	var $going = Bool.True
	for def in defs {
		match def.target {
			Valid(_) if $going => {
				$out = $out.append(def)
				$going = !def.junk
			}

			_ => {
				$going = Bool.False
			}
		}
	}
	$out
}

## Put a block's lines in a container: 0 none, 1 block quote, 2 list item
## (with a different bullet each time, so neighbouring lists stay apart).
contain : List(Str), U64, U64 -> List(Str)
contain = |lines, container, index| {
	bullet = ["-", "*", "+"].get(index % 3) ?? "-"
	match container {
		1 => lines.map(|line| "> ${line}")
		2 => lines.map_with_index(|line, i| if i == 0 "${bullet} ${line}" else "  ${line}")
		_ => lines
	}
}

generate : List(U8) -> Input
generate = |bytes| {
	start = { bytes, pos: 0 }
	count = pick(start, 3)
	var $cur = count.cur
	var $chunks = []
	var $expected = []
	var $defs = []
	var $index = 0
	while $index <= count.n {
		generated = gen_block($cur)
		container = pick(generated.cur, 3)
		$cur = container.cur
		block = generated.block
		$chunks = $chunks.append(Str.join_with(contain(block.lines, container.n, $index), "\n"))
		node =
			match block.node {
				Some(paragraph) =>
					match container.n {
						1 => Some(Blockquote([paragraph]))
						2 => Some(ListBlock({ kind: Unordered, loose: Bool.False, items: [{ task: NoTask, blocks: [paragraph] }] }))
						_ => Some(paragraph)
					}

				None =>
					match container.n {
						1 => Some(Blockquote([]))
						2 => Some(ListBlock({ kind: Unordered, loose: Bool.False, items: [{ task: NoTask, blocks: [] }] }))
						_ => None
					}
			}
		$expected =
			match node {
				Some(n) => $expected.append(n)
				None => $expected
			}
		$defs = $defs.concat(block.defs)
		$index = $index + 1
	}
	# A definition cannot interrupt a paragraph: one written right after text
	# is text.
	interrupt = pick($cur, 4)
	$cur = interrupt.cur
	late = if interrupt.n == 0 "para\n[late]: /late\n\n" else ""
	late_node = if interrupt.n == 0 [Paragraph([Text("para\n[late]: /late")])] else []
	uses = labels.map_with_index(|label, index| "[t${index.to_str()}][${label}]")
	use_line = Str.join_with(uses, " ")
	use_inlines = labels.map_with_index(
		|label, index| {
			match find_def($defs, label) {
				Ok(target) => [Link({ label: [Text("t${index.to_str()}")], target })]
				Err(_) => [Text("[t${index.to_str()}][${label}]")]
			}
		},
	)
	joined = join_inlines(use_inlines)
	markdown = "${Str.join_with($chunks, "\n\n")}\n\n${late}${use_line}\n"
	{ markdown, expected: $expected.concat(late_node).append(Paragraph(joined)) }
}

## The first definition of a label wins; labels match case-insensitively with
## whitespace collapsed (all labels here are ASCII).
find_def : List(Def), Str -> Try(Markdown.LinkTarget, [NotFound])
find_def = |defs, label| {
	match defs.find_first(|def| Str.with_ascii_lowercased(def.label) == Str.with_ascii_lowercased(label)) {
		Ok(def) =>
			match def.target {
				Valid(target) => Ok(target)
				Invalid => Err(NotFound)
			}

		Err(_) => Err(NotFound)
	}
}

join_inlines : List(List(Markdown.Inline)) -> List(Markdown.Inline)
join_inlines = |parts| {
	var $out = []
	var $index = 0
	for part in parts {
		if $index > 0 {
			$out = $out.append(Text(" "))
		}
		$out = $out.concat(part)
		$index = $index + 1
	}
	merge_text($out)
}

merge_text : List(Markdown.Inline) -> List(Markdown.Inline)
merge_text = |inlines| {
	var $out = []
	for inline in inlines {
		match (inline, $out.last()) {
			(Text(more), Ok(Text(before))) => {
				$out = $out.drop_last(1).append(Text(Str.concat(before, more)))
			}

			(Text(""), _) => {}
			_ => {
				$out = $out.append(inline)
			}
		}
	}
	$out
}

normalize : List(Markdown) -> List(Markdown)
normalize = |blocks| {
	blocks.map(
		|block| {
			match block {
				Paragraph(inlines) => Paragraph(merge_text(inlines))
				Blockquote(children) => Blockquote(normalize(children))
				ListBlock(list) => ListBlock({ ..list, items: list.items.map(|item| { ..item, blocks: normalize(item.blocks) }) })
				other => other
			}
		},
	)
}

## `Paragraph([Text("?")])` in the expectation stands for "some paragraph".
matches : List(Markdown), List(Markdown) -> Bool
matches = |expected, actual| {
	if expected.len() != actual.len() {
		Bool.False
	} else {
		List.map2(expected, actual, |e, a| block_matches(e, a)).all(|ok| ok)
	}
}

block_matches : Markdown, Markdown -> Bool
block_matches = |expected, actual| {
	match (expected, actual) {
		(Paragraph([Text("?")]), Paragraph(_)) => Bool.True
		(Blockquote(e), Blockquote(a)) => matches(e, a)
		(ListBlock(e), ListBlock(a)) =>
			e.kind == a.kind and e.loose == a.loose and e.items.len() == a.items.len() and List.map2(e.items, a.items, |x, y| x.task == y.task and matches(x.blocks, y.blocks)).all(|ok| ok)
		_ => expected == actual
	}
}

test : Input -> Fuzz.Outcome
test = |input| {
	actual =
		match Utf8.parse_str(Markdown.all, input.markdown) {
			Ok(blocks) => normalize(blocks)
			Err(_) => crash "Markdown.all failed"
		}
	if !matches(input.expected, actual) {
		crash "reference definition mismatch\n--- markdown ---\n${input.markdown}\n--- expected ---\n${show(input.expected)}\n--- actual ---\n${show(actual)}"
	}
	Fuzz.keep
}

show : List(Markdown) -> Str
show = |blocks| Str.join_with(blocks.map(Markdown.to_debug_str), "\n")

## `show` prints one JSON object, {"markdown", "expected"}, in
## scripts/markdown/blocks/probe.roc's shape for `review_markdown_blocks.py crosscheck`; a
## paragraph of just "?" stands for any paragraph.
encode_blocks : List(Markdown) -> Str
encode_blocks = |blocks| "[${blocks.map(encode_block) |> Str.join_with(",")}]"

encode_inlines : List(Markdown.Inline) -> Str
encode_inlines = |inlines| "[${inlines.map(encode_inline) |> Str.join_with(",")}]"

encode_block : Markdown -> Str
encode_block = |block| {
	match block {
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
		other => "[\"other\",${json_string(Markdown.to_debug_str(other))}]"
	}
}

encode_inline : Markdown.Inline -> Str
encode_inline = |inline| {
	match inline {
		Text(text) => "[\"text\",${json_string(text)}]"
		Link({ label, target }) => {
			title =
				match target.title {
					Some(text) => json_string(text)
					None => "null"
				}
			"[\"link\",${json_string(target.href)},${title},${encode_inlines(label)}]"
		}
		_ => "[\"other\",${json_string(Markdown.inline_to_debug_str(inline))}]"
	}
}

json_string : Str -> Str
json_string = |text| {
	escaped = text.to_utf8().fold(
		[],
		|out, byte| {
			if byte == '"' or byte == '\\' {
				out.concat(['\\', byte])
			} else if byte < 0x20 {
				hex = |n| if n < 10 '0' + n else 'a' + n - 10
				out.concat(['\\', 'u', '0', '0', hex(byte // 16), hex(byte % 16)])
			} else {
				out.append(byte)
			}
		},
	)
	"\"${Str.from_utf8(escaped) ?? ""}\""
}

target = Fuzz.target_with({
	name: "markdown-refdefs",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "{\"markdown\":${json_string(input.markdown)},\"expected\":${encode_blocks(input.expected)}}",
})
