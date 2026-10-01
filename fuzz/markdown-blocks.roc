app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.Utf8

## Block-structure property: fuzzer bytes choose a tree of Markdown blocks
## (paragraphs, ATX and setext headings, thematic breaks, fenced and indented
## code, HTML blocks, GFM tables, block quotes, bullet/ordered/task lists) *and*
## how to write it (indentation, marker widths, fence lengths, closing
## sequences, lazy continuation lines, blank-line placement). The parser must
## return exactly that tree.
##
## The expected tree is built alongside the text, never by calling the parser.
## Every writing choice follows CommonMark 0.31.2 and the GFM table/task list
## rules: for example a list is loose exactly when the generator put a blank
## line between two of its items or between two blocks directly inside one
## item, and a paragraph continuation line may drop the prefixes of the
## containers it sits in (a lazy continuation line).
##
## Inline content is restricted to plain words so that the inline parser's
## result is known: `Text` with the words, a line feed for each soft break.
##
## Metamorphic relations: CRLF and CR line endings, and a missing final line
## ending, give the same tree.

Cur : { bytes : List(U8), pos : U64 }

pick : Cur, U64 -> { n : U64, cur : Cur }
pick = |cur, count| {
	byte = cur.bytes.get(cur.pos) ?? 0
	{ n: U8.to_u64(byte) % count, cur: { bytes: cur.bytes, pos: cur.pos + 1 } }
}

spaces : U64 -> Str
spaces = |count| Str.join_with(List.repeat(" ", count), "")

repeat : Str, U64 -> Str
repeat = |text, count| Str.join_with(List.repeat(text, count), "")

## A rendered line, before the prefixes of enclosing containers are added.
## `lazy` marks a paragraph continuation line that still has no prefix, so an
## enclosing container may leave its own prefix off.
Line : { text : Str, lazy : Bool, blank : Bool }

text_line : Str -> Line
text_line = |text| { text, lazy: False, blank: False }

blank_line : Line
blank_line = { text: "", lazy: False, blank: True }

## What a following sibling must take into account.
Kind : [
	KParagraph,
	KAtx,
	KSetext,
	KBreak,
	KFenced,
	KIndented,
	KHtmlComment,
	KHtmlBlankEnded,
	KTable,
	KQuote,
	KList({ ordered : Bool, marker : U8 }),
]

Gen : { lines : List(Line), node : Markdown, kind : Kind, cur : Cur }

## How a block starts, for checking whether it may follow a paragraph directly
## (interrupt a paragraph) or appear on a list item's marker line.
Start : [
	SParagraph,
	SSetext,
	SAtx,
	SBreak(U8),
	SFenced,
	SIndented,
	SHtml({ index : U64, html_type : U64 }),
	STable,
	SQuote,
	SList({ ordered : Bool, marker : U8, start : U64, first_blank : Bool }),
]

words : List(Str)
words = ["a", "foo", "bar", "baz", "roc", "qux", "zed", "lorem"]

gen_words : Cur, U64 -> { text : Str, cur : Cur }
gen_words = |start, max| {
	count = pick(start, max)
	var $cur = count.cur
	var $out = []
	var $index = 0
	while $index <= count.n {
		chosen = pick($cur, words.len())
		$out = $out.append(words.get(chosen.n) ?? "a")
		$cur = chosen.cur
		$index = $index + 1
	}
	{ text: Str.join_with($out, " "), cur: $cur }
}

inline_text : Str -> List(Markdown.Inline)
inline_text = |text| if text.is_empty() [] else [Text(text)]

## --- Leaf blocks -----------------------------------------------------------

gen_paragraph : Cur, Bool -> Gen
gen_paragraph = |start, flush| {
	count = pick(start, 3)
	var $cur = count.cur
	var $lines = []
	var $texts = []
	var $index = 0
	while $index <= count.n {
		ws = gen_words($cur, 3)
		indent = pick(ws.cur, 8)
		trail = pick(indent.cur, 3)
		$cur = trail.cur
		# Up to three leading spaces; one trailing space (two would be a hard
		# break), any number on the last line.
		lead = if indent.n < 4 and !(flush and $index == 0) spaces(indent.n) else ""
		last = $index == count.n
		tail = if trail.n == 1 " " else if trail.n == 2 and last "   " else ""
		$lines = $lines.append({ text: "${lead}${ws.text}${tail}", lazy: $index > 0, blank: False })
		$texts = $texts.append(ws.text)
		$index = $index + 1
	}
	{ lines: $lines, node: Paragraph(inline_text(Str.join_with($texts, "\n"))), kind: KParagraph, cur: $cur }
}

gen_atx : Cur, Bool -> Gen
gen_atx = |start, flush| {
	level = pick(start, 6)
	indent = pick(level.cur, 4)
	empty = pick(indent.cur, 6)
	ws = gen_words(empty.cur, 3)
	closing = pick(ws.cur, 4)
	close_len = pick(closing.cur, 8)
	trail = pick(close_len.cur, 3)
	content = if empty.n == 0 "" else ws.text
	hashes = repeat("#", level.n + 1)
	closer =
		if closing.n == 0 {
			" ${repeat("#", close_len.n + 1)}${spaces(trail.n)}"
		} else {
			spaces(trail.n)
		}
	body = if content.is_empty() closer else " ${content}${closer}"
	node = Heading({ level: level_of(level.n), content: inline_text(content) })
	{ lines: [text_line("${spaces(if flush 0 else indent.n)}${hashes}${body}")], node, kind: KAtx, cur: trail.cur }
}

level_of : U64 -> Markdown.Level
level_of = |n| {
	match n {
		0 => One
		1 => Two
		2 => Three
		3 => Four
		4 => Five
		_ => Six
	}
}

gen_setext : Cur, Bool -> Gen
gen_setext = |start, flush| {
	para = gen_paragraph(start, flush)
	style = pick(para.cur, 2)
	len = pick(style.cur, 4)
	indent = pick(len.cur, 4)
	trail = pick(indent.cur, 3)
	marker = if style.n == 0 "=" else "-"
	underline = "${spaces(indent.n)}${repeat(marker, len.n + 1)}${spaces(trail.n)}"
	content =
		match para.node {
			Paragraph(inlines) => inlines
			_ => []
		}
	# Continuation lines of a setext heading are kept with their prefixes.
	lines = para.lines.map(|line| { ..line, lazy: False }).append(text_line(underline))
	{ lines, node: Heading({ level: if style.n == 0 One else Two, content }), kind: KSetext, cur: trail.cur }
}

gen_break : Cur, U8, Bool -> Gen
gen_break = |start, marker, flush| {
	count = pick(start, 3)
	gap = pick(count.cur, 3)
	indent = pick(gap.cur, 4)
	marker_text = Str.from_utf8([marker]) ?? "*"
	text = "${spaces(if flush 0 else indent.n)}${Str.join_with(List.repeat(marker_text, count.n + 3), spaces(gap.n))}"
	{ lines: [text_line(text)], node: ThematicBreak, kind: KBreak, cur: indent.cur }
}

## Code lines; none of them can close the fence they sit in.
code_tokens : List(Str)
code_tokens = ["x = 1", "", "# not a heading", "- not a list", "> not a quote", "    four", "  two", "<div>", "``", "~~", "| a |", "***", "foo  ", "\tTab"]

gen_code_lines : Cur, U64 -> { lines : List(Str), cur : Cur }
gen_code_lines = |start, max| {
	count = pick(start, max)
	var $cur = count.cur
	var $lines = []
	var $index = 0
	while $index < count.n {
		chosen = pick($cur, code_tokens.len())
		$lines = $lines.append(code_tokens.get(chosen.n) ?? "x")
		$cur = chosen.cur
		$index = $index + 1
	}
	{ lines: $lines, cur: $cur }
}

leading_spaces : Str -> U64
leading_spaces = |text| {
	bytes = text.to_utf8()
	var $count = 0
	while (bytes.get($count) ?? 'x') == ' ' {
		$count = $count + 1
	}
	$count
}

gen_fenced : Cur, Bool, Bool -> Gen
gen_fenced = |start, may_be_unclosed, flush| {
	style = pick(start, 2)
	fence_char = if style.n == 0 "`" else "~"
	len = pick(style.cur, 3)
	fence_len = len.n + 3
	indent_choice = pick(len.cur, 4)
	indent = { ..indent_choice, n: if flush 0 else indent_choice.n }
	info_choice = pick(indent.cur, 4)
	info_word = words.get(info_choice.n) ?? "roc"
	info = if info_choice.n == 0 "" else if info_choice.n == 3 "${info_word} x=1" else info_word
	info_gap = pick(info_choice.cur, 3)
	# A tab would be partly consumed by the indentation removal below.
	body_raw = gen_code_lines(info_gap.cur, 4)
	body = { ..body_raw, lines: body_raw.lines.map(|line| if indent.n > 0 Str.replace_each(line, "\t", "  ") else line) }
	close_extra = pick(body.cur, 3)
	close_indent = pick(close_extra.cur, 4)
	close_trail = pick(close_indent.cur, 3)
	unclosed = pick(close_trail.cur, 4)
	is_unclosed = may_be_unclosed and unclosed.n == 0
	open = "${spaces(indent.n)}${repeat(fence_char, fence_len)}${spaces(info_gap.n)}${info}"
	close = "${spaces(close_indent.n)}${repeat(fence_char, fence_len + close_extra.n)}${spaces(close_trail.n)}"
	# Up to the opening fence's indentation is removed from each code line.
	expected_lines = body.lines.map(
		|line| {
			lead = leading_spaces(line)
			strip = if lead < indent.n lead else indent.n
			Str.from_utf8(line.to_utf8().drop_first(strip)) ?? line
		},
	)
	code_lines = body.lines.map(|line| if line.is_empty() blank_line else text_line(line))
	lines = [text_line(open)].concat(code_lines).concat(if is_unclosed [] else [text_line(close)])
	pre = Str.join_with(expected_lines.map(|line| "${line}\n"), "")
	{ lines, node: Code({ info, pre }), kind: KFenced, cur: unclosed.cur }
}

gen_indented : Cur -> Gen
gen_indented = |start| {
	count = pick(start, 3)
	var $cur = count.cur
	var $lines = []
	var $expected = []
	var $index = 0
	while $index <= count.n {
		kind = pick($cur, 6)
		ws = gen_words(kind.cur, 2)
		$cur = ws.cur
		inner = $index > 0 and $index < count.n
		if inner and kind.n == 0 {
			$lines = $lines.append(blank_line)
			$expected = $expected.append("")
		} else {
			extra = if kind.n == 1 "  " else if kind.n == 2 "\t" else ""
			$lines = $lines.append(text_line("    ${extra}${ws.text}"))
			$expected = $expected.append("${extra}${ws.text}")
		}
		$index = $index + 1
	}
	pre = Str.join_with($expected.map(|line| "${line}\n"), "")
	{ lines: $lines, node: Code({ info: "", pre }), kind: KIndented, cur: $cur }
}

html_openers : List({ text : Str, html_type : U64 })
html_openers = [
	{ text: "<div>", html_type: 6 },
	{ text: "<DIV class=\"x\">", html_type: 6 },
	{ text: "</p>", html_type: 6 },
	{ text: "<table><tr><td>", html_type: 6 },
	{ text: "<section/>", html_type: 6 },
	{ text: "<my-tag data-x='1'>", html_type: 7 },
	{ text: "</custom>", html_type: 7 },
	{ text: "</TEXTAREA >", html_type: 7 },
	{ text: "<a href=\"x\" title=y>", html_type: 7 },
	{ text: "<!-- comment", html_type: 2 },
	{ text: "<?php", html_type: 3 },
	{ text: "<!DOCTYPE html", html_type: 4 },
	{ text: "<![CDATA[", html_type: 5 },
	{ text: "<pre class=\"p\">", html_type: 1 },
	{ text: "<script>", html_type: 1 },
]

html_closer : U64 -> Str
html_closer = |html_type| {
	match html_type {
		1 => "</pre></script>"
		2 => "-->"
		3 => "?>"
		4 => ">"
		_ => "]]>"
	}
}

gen_html : Cur, U64, Bool -> Gen
gen_html = |start, opener_index, flush| {
	opener = html_openers.get(opener_index) ?? { text: "<div>", html_type: 6 }
	indent_choice = pick(start, 4)
	indent = { ..indent_choice, n: if flush 0 else indent_choice.n }
	count = pick(indent.cur, 3)
	var $cur = count.cur
	var $body = []
	var $index = 0
	while $index < count.n {
		ws = gen_words($cur, 3)
		blank = pick(ws.cur, 4)
		$cur = blank.cur
		# Blank lines end types 6 and 7; types 1-5 keep them.
		if opener.html_type <= 5 and blank.n == 0 {
			$body = $body.append("")
		} else {
			$body = $body.append(ws.text)
		}
		$index = $index + 1
	}
	first = "${spaces(indent.n)}${opener.text}"
	all = if opener.html_type <= 5 [first].concat($body).append("end ${html_closer(opener.html_type)}") else [first].concat($body)
	lines = all.map(|line| if line.is_empty() blank_line else text_line(line))
	text = Str.join_with(all.map(|line| "${line}\n"), "")
	kind = if opener.html_type <= 5 KHtmlComment else KHtmlBlankEnded
	{ lines, node: HtmlBlock(text), kind, cur: $cur }
}

## A GFM table. Rows may have fewer or more cells than the header; the parser
## pads or cuts them to the header's width.
gen_table : Cur -> Gen
gen_table = |start| {
	cols = pick(start, 3)
	columns = cols.n + 1
	style = pick(cols.cur, 4)
	var $cur = style.cur
	var $header = []
	var $delims = []
	var $align = []
	var $index = 0
	while $index < columns {
		ws = gen_words($cur, 2)
		a = pick(ws.cur, 4)
		dashes = pick(a.cur, 3)
		pad = pick(dashes.cur, 2)
		$cur = pad.cur
		d = repeat("-", dashes.n + 1)
		delim =
			match a.n {
				0 => d
				1 => ":${d}"
				2 => "${d}:"
				_ => ":${d}:"
			}
		$header = $header.append(ws.text)
		$delims = $delims.append(if pad.n == 0 delim else " ${delim} ")
		$align = $align.append(alignment_of(a.n))
		$index = $index + 1
	}
	row_count = pick($cur, 4)
	$cur = row_count.cur
	var $rows = []
	var $expected_rows = []
	var $r = 0
	while $r < row_count.n {
		cells = pick($cur, 5)
		$cur = cells.cur
		var $row = []
		var $c = 0
		while $c <= cells.n {
			empty = pick($cur, 5)
			ws = gen_words(empty.cur, 2)
			$cur = ws.cur
			$row = $row.append(if empty.n == 0 "" else ws.text)
			$c = $c + 1
		}
		$rows = $rows.append($row)
		$expected_rows = $expected_rows.append(fit_cells($row, columns))
		$r = $r + 1
	}
	# Leading and trailing pipes are optional, but a one-column header or
	# delimiter row needs at least one pipe (`-` alone is a setext underline).
	outer = style.n
	header_line = render_row($header, outer, columns == 1)
	# Without a leading pipe, `- | -` would start a list item: write the
	# delimiter cells unpadded then.
	delim_line =
		if outer == 0 or outer == 1 or (outer == 3 and columns == 1) {
			render_row($delims, outer, True)
		} else {
			"${Str.join_with($delims.map(Str.trim), "|")}${if outer == 2 "|" else ""}"
		}
	lines = [text_line(header_line), text_line(delim_line)].concat($rows.map(|row| text_line(render_row(row, outer, True))))
	node = Table({ header: $header.map(inline_text), align: $align, rows: $expected_rows })
	{ lines, node, kind: KTable, cur: $cur }
}

fit_cells : List(Str), U64 -> List(List(Markdown.Inline))
fit_cells = |cells, columns| {
	var $out = []
	var $index = 0
	while $index < columns {
		$out = $out.append(inline_text(cells.get($index) ?? ""))
		$index = $index + 1
	}
	$out
}

## outer: 0 both pipes, 1 leading only, 2 trailing only, 3 none (unless a pipe
## is required, or a cell is empty and would vanish).
render_row : List(Str), U64, Bool -> Str
render_row = |cells, outer, needs_pipe| {
	body = Str.join_with(cells.map(|cell| " ${cell} "), "|")
	ends_empty = (cells.last() ?? "x").is_empty()
	starts_empty = (cells.first() ?? "x").is_empty()
	single = cells.len() == 1
	leading = outer == 0 or outer == 1 or starts_empty or ((single or needs_pipe) and outer == 3)
	trailing = outer == 0 or outer == 2 or ends_empty
	# Without a leading pipe the row starts at its first cell, so that it can
	# sit directly after a list marker.
	"${if leading "|" else ""}${if leading body else Str.drop_prefix(body, " ")}${if trailing "|" else ""}"
}

alignment_of : U64 -> Markdown.Alignment
alignment_of = |n| {
	match n {
		0 => Default
		1 => Left
		2 => Right
		_ => Center
	}
}

## --- Sibling sequences --------------------------------------------------

## May `next` follow `prev` directly, without a blank line?
may_touch : Kind, Start -> Bool
may_touch = |prev, next| {
	match prev {
		KParagraph =>
			match next {
				SAtx | SFenced | SQuote | STable => True
				SBreak(c) => c != '-'
				SHtml(h) => h.html_type <= 6
				SList(l) => !l.first_blank and (!l.ordered or l.start == 1)
				_ => False
			}

		KAtx | KSetext | KBreak | KFenced | KHtmlComment =>
			match next {
				# A paragraph directly after a fence could be read as a setext
				# underline's text only when it is followed by one; fine here.
				_ => True
			}

		KIndented =>
			match next {
				SIndented => False
				_ => True
			}

		_ => False
	}
}

## May `next` follow `prev` at all (even after a blank line)?
may_follow : Kind, Start -> Bool
may_follow = |prev, next| {
	match (prev, next) {
		(KIndented, SIndented) => False
		(KList(_), SIndented) => False
		(KList(a), SList(b)) => !(a.ordered == b.ordered and a.marker == b.marker)
		_ => True
	}
}

Choice : { start : Start, cur : Cur }

## Choose the next block's kind (and the details that matter to its neighbours)
## before generating it.
choose_start : Cur, U64, Bool -> Choice
choose_start = |start, depth, allow_indented| {
	kind = pick(start, if depth >= 3 9 else 11)
	detail = pick(kind.cur, 256)
	cur = detail.cur
	chosen =
		match kind.n {
			0 => SParagraph
			1 => SParagraph
			2 => SAtx
			3 => SSetext
			4 => SBreak(['*', '-', '_'].get(detail.n % 3) ?? '*')
			5 => SFenced
			6 => if allow_indented SIndented else SParagraph
			7 => {
				index = detail.n % html_openers.len()
				SHtml({ index, html_type: (html_openers.get(index) ?? { text: "", html_type: 6 }).html_type })
			}
			8 => STable
			9 => SQuote
			_ => {
				ordered = detail.n % 2 == 0
				marker = if ordered (if detail.n % 4 == 0 '.' else ')') else (['-', '+', '*'].get(detail.n % 3) ?? '-')
				SList({ ordered, marker, start: if detail.n % 5 == 0 7 else if detail.n % 3 == 0 0 else 1, first_blank: detail.n % 7 == 0 })
			}
		}
	{ start: chosen, cur }
}

## `flush`: the block's first line may not be indented (it follows a list
## marker, or a list whose last item could otherwise absorb it).
gen_block : Choice, U64, Bool, Bool -> Gen
gen_block = |choice, depth, last_in_document, flush| {
	cur = choice.cur
	match choice.start {
		SParagraph => gen_paragraph(cur, flush)
		SAtx => gen_atx(cur, flush)
		SSetext => gen_setext(cur, flush)
		SBreak(c) => gen_break(cur, c, flush)
		SFenced => gen_fenced(cur, last_in_document, flush)
		SIndented => gen_indented(cur)
		SHtml(h) => gen_html(cur, h.index, flush)
		STable => gen_table(cur)
		SQuote => gen_quote(cur, depth + 1, flush)
		SList(info) => gen_list(cur, depth + 1, info, flush)
	}
}

Seq : { lines : List(Line), nodes : List(Markdown), blank_between : Bool, cur : Cur, last_kind : Kind, first_start : Start }

## A sequence of sibling blocks. `blank_between` reports whether any blank line
## separates two of them (what makes a list item loose).
gen_sequence : Cur, U64, U64, Bool, Bool -> Seq
gen_sequence = |start, depth, max, top_level, first_on_marker_line| {
	count = pick(start, max)
	var $cur = count.cur
	var $lines = []
	var $nodes = []
	var $blank_between = False
	var $prev = Err(NoPrevious)
	var $first_start = SParagraph
	var $index = 0
	while $index <= count.n {
		first = $index == 0
		var $choice = choose_start($cur, depth, !(first and first_on_marker_line))
		$cur = $choice.cur
		# Restrictions on what may appear first on a list item's marker line.
		if first and first_on_marker_line {
			$choice = { ..$choice, start: marker_line_safe($choice.start) }
		}
		follows =
			match $prev {
				Ok(kind) => may_follow(kind, $choice.start)
				Err(_) => True
			}
		if !follows {
			$choice = { ..$choice, start: SParagraph }
		}
		last = $index == count.n
		after_list =
			match $prev {
				Ok(KList(_)) => True
				_ => False
			}
		block = gen_block($choice, depth, top_level and last, (first and first_on_marker_line) or after_list)
		$cur = block.cur
		if first {
			$first_start = $choice.start
		}
		separator =
			match $prev {
				Err(_) => []
				Ok(kind) => {
					touch = pick($cur, 3)
					$cur = touch.cur
					if may_touch(kind, $choice.start) and touch.n == 0 {
						[]
					} else {
						$blank_between = True
						[blank_line]
					}
				}
			}
		$lines = $lines.concat(separator).concat(block.lines)
		$nodes = $nodes.append(block.node)
		$prev = Ok(block.kind)
		$index = $index + 1
	}
	last_kind =
		match $prev {
			Ok(kind) => kind
			Err(_) => KParagraph
		}
	{ lines: $lines, nodes: $nodes, blank_between: $blank_between, cur: $cur, last_kind, first_start: $first_start }
}

## On a list item's marker line: no indented code (the padding rule would make
## it part of the item's indentation), no thematic break made of the list's own
## marker (it would be read as a break instead of an item), and no nested list
## starting empty (`* * *` is a thematic break too).
marker_line_safe : Start -> Start
marker_line_safe = |s| {
	match s {
		SIndented => SParagraph
		SBreak(_) => SBreak('_')
		SList(l) => SList({ ..l, first_blank: False })
		_ => s
	}
}

## --- Containers ---------------------------------------------------------

## Add a container prefix to the lines of its content. A lazy line may lose
## the prefix when `drop` says so; it then stays lazy for outer containers.
prefix_lines : List(Line), Str, Str, Str, Cur -> { lines : List(Line), cur : Cur }
prefix_lines = |lines, first_prefix, rest_prefix, blank_prefix, start| {
	var $cur = start
	var $out = []
	var $index = 0
	while $index < lines.len() {
		line = lines.get($index) ?? blank_line
		drop = pick($cur, 3)
		$cur = drop.cur
		prefixed =
			if $index == 0 {
				{ text: "${first_prefix}${line.text}", lazy: False, blank: False }
			} else if line.blank {
				{ text: blank_prefix, lazy: False, blank: True }
			} else if line.lazy and drop.n == 0 {
				line
			} else {
				{ text: "${rest_prefix}${line.text}", lazy: False, blank: False }
			}
		$out = $out.append(prefixed)
		$index = $index + 1
	}
	{ lines: $out, cur: $cur }
}

gen_quote : Cur, U64, Bool -> Gen
gen_quote = |start, depth, flush| {
	style = pick(start, 4)
	inner = gen_sequence(style.cur, depth, 3, False, False)
	# `>` then an optional space; content starting with a space needs the
	# space written out so the content keeps its indentation.
	lead = if style.n == 3 and !flush " " else ""
	var $cur = inner.cur
	var $out = []
	for line in inner.lines {
		form = pick($cur, 3)
		drop = pick(form.cur, 3)
		$cur = drop.cur
		starts_space = Str.starts_with(line.text, " ") or Str.starts_with(line.text, "\t")
		prefixed =
			if line.blank {
				# Not blank any more for enclosing containers: the `>` must stay.
				{ text: if form.n == 0 "${lead}> " else "${lead}>", lazy: False, blank: False }
			} else if line.lazy and drop.n == 0 and !$out.is_empty() {
				line
			} else if form.n == 0 and !starts_space {
				{ text: "${lead}>${line.text}", lazy: False, blank: False }
			} else {
				{ text: "${lead}> ${line.text}", lazy: False, blank: False }
			}
		$out = $out.append(prefixed)
	}
	{ lines: $out, node: Blockquote(inner.nodes), kind: KQuote, cur: $cur }
}

ListStart : { ordered : Bool, marker : U8, start : U64, first_blank : Bool }

gen_list : Cur, U64, ListStart, Bool -> Gen
gen_list = |start, depth, info, flush| {
	count = pick(start, 3)
	var $cur = count.cur
	var $lines = []
	var $items = []
	var $loose = False
	var $prev_content = 4
	var $index = 0
	marker_char = Str.from_utf8([info.marker]) ?? "-"
	while $index <= count.n {
		number = if $index == 0 info.start else info.start + $index
		marker = if info.ordered "${number.to_str()}${marker_char}" else marker_char
		indent_choice = pick($cur, 4)
		gap = pick(indent_choice.cur, 4)
		shape = pick(gap.cur, 8)
		task_choice = pick(shape.cur, 6)
		$cur = task_choice.cur
		# A later marker must be indented less than the previous item's content,
		# or it would start a nested list.
		limit = if $prev_content - 1 < 3 $prev_content - 1 else 3
		marker_indent = if flush and $index == 0 0 else if indent_choice.n < limit indent_choice.n else limit
		marker_width = marker.to_utf8().len()
		# The first item may start empty or blank only when the list does not
		# interrupt a paragraph (`first_blank`).
		empty = if $index == 0 info.first_blank and shape.n % 2 == 0 else shape.n == 7
		blank_start = !empty and (if $index == 0 info.first_blank else shape.n == 6)
		item =
			if empty {
				empty_item = { task: NoTask, blocks: [] }
				{ lines: [text_line("${spaces(marker_indent)}${marker}")], item: empty_item, loose: False, content: marker_indent + marker_width + 1 }
			} else {
				inner = gen_sequence($cur, depth, 2, False, !blank_start)
				$cur = inner.cur
				content_width = marker_indent + marker_width + (if blank_start 1 else gap.n + 1)
				first_paragraph =
					match inner.first_start {
						SParagraph => True
						_ => False
					}
				task = if first_paragraph and !blank_start and task_choice.n < 3 task_choice.n else 3
				task_text = if task == 0 "[ ] " else if task == 1 "[x] " else if task == 2 "[X] " else ""
				task_state = if task == 0 Unchecked else if task < 3 Checked else NoTask
				marker_prefix = "${spaces(marker_indent)}${marker}${spaces(gap.n + 1)}${task_text}"
				prefixed = prefix_lines(inner.lines, marker_prefix, spaces(content_width), "", $cur)
				$cur = prefixed.cur
				body_lines =
					if blank_start {
						[text_line("${spaces(marker_indent)}${marker}")].concat(prefix_lines(inner.lines, spaces(content_width), spaces(content_width), "", $cur).lines)
					} else {
						prefixed.lines
					}
				{ lines: body_lines, item: { task: task_state, blocks: inner.nodes }, loose: inner.blank_between, content: content_width }
			}
		separator =
			if $index == 0 {
				[]
			} else {
				blank = pick($cur, 3)
				$cur = blank.cur
				if blank.n == 0 {
					$loose = True
					[blank_line]
				} else {
					[]
				}
			}
		$lines = $lines.concat(separator).concat(item.lines)
		$items = $items.append(item.item)
		$loose = $loose or item.loose
		$prev_content = item.content
		$index = $index + 1
	}
	kind = if info.ordered Ordered({ start: info.start }) else Unordered
	{ lines: $lines, node: ListBlock({ kind, loose: $loose, items: $items }), kind: KList({ ordered: info.ordered, marker: info.marker }), cur: $cur }
}

## --- The property -------------------------------------------------------

Input : { markdown : Str, expected : List(Markdown) }

generate : List(U8) -> Input
generate = |bytes| {
	seq = gen_sequence({ bytes, pos: 0 }, 0, 5, True, False)
	# A first line of exactly `---` with another later would be frontmatter.
	lines = seq.lines.map(|line| line.text)
	safe = if lines.first() == Ok("---") List.set(lines, 0, "***") ?? lines else lines
	markdown = Str.join_with(safe.map(|line| "${line}\n"), "")
	{ markdown, expected: seq.nodes }
}

parse : Str -> List(Markdown)
parse = |text| {
	match Utf8.parse_str(Markdown.parser, text) {
		Ok(blocks) => blocks
		Err(_) => crash "Markdown.parser failed on:\n${text}"
	}
}

test : Input -> Fuzz.Outcome
test = |input| {
	actual = parse(input.markdown)
	if actual != input.expected {
		crash "block structure mismatch\n--- markdown ---\n${input.markdown}\n--- expected ---\n${show_blocks(input.expected)}\n--- actual ---\n${show_blocks(actual)}"
	}
	crlf = parse(Str.replace_each(input.markdown, "\n", "\r\n"))
	if crlf != actual {
		crash "CRLF line endings changed the result\n${input.markdown}\nCRLF: ${show_blocks(crlf)}"
	}
	cr = parse(Str.replace_each(input.markdown, "\n", "\r"))
	if cr != actual {
		crash "CR line endings changed the result\n${input.markdown}\nCR: ${show_blocks(cr)}"
	}
	bytes = input.markdown.to_utf8()
	unterminated = parse(Str.from_utf8(bytes.drop_last(1)) ?? "")
	# A blank last line of an unclosed fence is code; keep it.
	if !bytes.is_empty() and !Str.ends_with(input.markdown, "\n\n") and unterminated != actual {
		crash "dropping the final line ending changed the result\n${input.markdown}\nwithout: ${show_blocks(unterminated)}"
	}
	Fuzz.keep
}

show_blocks : List(Markdown) -> Str
show_blocks = |blocks| Str.join_with(blocks.map(Str.inspect), "\n")

## `show` prints one JSON object, {"markdown", "expected"}, with `expected` in
## scripts/markdown/blocks/probe.roc's shape, so scripts/review_markdown_blocks.py crosscheck
## can compare the generator's expectations with cmark-gfm.
show : Input -> Str
show = |input| "{\"markdown\":${json_string(input.markdown)},\"expected\":${encode_blocks(input.expected)}}"

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
		_ => "[\"other\",${json_string(Str.inspect(inline))}]"
	}
}

json_string : Str -> Str
json_string = |text| {
	escaped = text.to_utf8().fold(
		[],
		|out, byte| {
			match byte {
				'"' => out.concat(['\\', '"'])
				'\\' => out.concat(['\\', '\\'])
				'\n' => out.concat(['\\', 'n'])
				'\r' => out.concat(['\\', 'r'])
				'\t' => out.concat(['\\', 't'])
				_ => out.append(byte)
			}
		},
	)
	"\"${Str.from_utf8(escaped) ?? ""}\""
}

target = Fuzz.target_with({
	name: "markdown-blocks",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show,
})
