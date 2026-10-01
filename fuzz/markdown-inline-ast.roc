app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.String

## Typed inline property: bytes choose an inline syntax tree and how to write
## it (delimiter characters, link destination and title forms, reference
## links, code fences, escapes, entities, soft and hard breaks). The generator
## builds the Markdown text and the expected tree together, never by calling
## the parser, using these CommonMark 0.31.2 facts:
##
## * every ASCII punctuation character in text is backslash-escaped, so text
##   never forms syntax; entities decode to their characters;
## * text runs start and end with a letter or digit and alternate with other
##   nodes, so every delimiter run is flanked by text or by a space;
## * a `*`/`~` node may sit inside a word (both-flanking delimiters) only when
##   it has text-only content and no enclosing node uses the same character;
##   otherwise it is separated by spaces, so its delimiters are only
##   left-flanking (opening) or right-flanking (closing) and pair like
##   brackets, without the "multiple of 3" rule ever applying;
## * `_` and `__` are always separated by spaces;
## * links never contain links; images may;
## * code fences are longer than any backtick run in the content, padded when
##   the content starts or ends with a backtick or is wrapped in spaces.
##
## In document mode the text is parsed with Markdown.all and links may be
## written as full, collapsed or shortcut references with definitions after
## the paragraph.

Cursor : { bytes : List(U8), pos : U64 }

Context : {
	depth : U64,
	in_link : Bool,
	ancestors : List(U8),
	doc : Bool,
}

Built : { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor }

Input : { md : Str, doc : Bool, expected : List(Markdown.Inline) }

pick : Cursor, U64 -> { value : U64, cursor : Cursor }
pick = |cursor, n| {
	byte = cursor.bytes.get(cursor.pos) ?? 0
	{ value: if n == 0 0 else U8.to_u64(byte) % n, cursor: { ..cursor, pos: cursor.pos + 1 } }
}

remaining : Cursor -> U64
remaining = |cursor| if cursor.pos >= cursor.bytes.len() 0 else cursor.bytes.len() - cursor.pos

words : List(Str)
words = ["a", "foo", "Bar", "x1", "42", "é", "straße", "Ωmega", "日本", "z"]

## Separators between words: the Markdown spelling and the decoded text.
separators : Bool -> List({ md : Str, text : Str })
separators = |allow_newline| {
	base = [
		{ md: " ", text: " " },
		{ md: " ", text: " " },
		{ md: "\\*", text: "*" },
		{ md: "\\_", text: "_" },
		{ md: "\\[", text: "[" },
		{ md: "\\]", text: "]" },
		{ md: "\\\\", text: "\\" },
		{ md: "\\`", text: "`" },
		{ md: "\\<", text: "<" },
		{ md: "\\&", text: "&" },
		{ md: "\\~", text: "~" },
		{ md: "\\!", text: "!" },
		{ md: "\\(", text: "(" },
		{ md: "\\.", text: "." },
		{ md: "\\:", text: ":" },
		{ md: "\\#", text: "#" },
		{ md: "&amp;", text: "&" },
		{ md: "&lt;", text: "<" },
		{ md: "&#42;", text: "*" },
		{ md: "&#x3A9;", text: "Ω" },
		{ md: "&copy;", text: "©" },
		{ md: "&ngE;", text: "≧̸" },
		{ md: "€", text: "€" },
		{ md: "\u(A0)", text: "\u(A0)" },
		{ md: " — ", text: " — " },
		{ md: "-", text: "-" },
	]
	if allow_newline base.append({ md: "\n", text: "\n" }) else base
}

## A text run that starts and ends with a word.
gen_text : Cursor, Context -> { text : Str, md : Str, cursor : Cursor }
gen_text = |cursor, ctx| {
	first = pick(cursor, words.len())
	var $text = words.get(first.value) ?? "a"
	var $md = $text
	count = pick(first.cursor, 4)
	var $cursor = count.cursor
	seps = separators(!ctx.doc)
	var $index = 0
	while $index < count.value {
		sep = pick($cursor, seps.len())
		word = pick(sep.cursor, words.len())
		chosen_sep = seps.get(sep.value) ?? { md: " ", text: " " }
		chosen_word = words.get(word.value) ?? "a"
		$text = Str.concat(Str.concat($text, chosen_sep.text), chosen_word)
		$md = Str.concat(Str.concat($md, chosen_sep.md), chosen_word)
		$cursor = word.cursor
		$index = $index + 1
	}
	{ text: $text, md: $md, cursor: $cursor }
}

## A sequence: text (node text)*.
gen_sequence : Cursor, Context, U64 -> Built
gen_sequence = |cursor, ctx, max_nodes| {
	first = gen_text(cursor, ctx)
	var $nodes = [Markdown.Inline.Text(first.text)]
	var $md = first.md
	var $defs = []
	count = pick(first.cursor, max_nodes + 1)
	var $cursor = count.cursor
	var $index = 0
	while $index < count.value and remaining($cursor) > 0 {
		node = gen_node($cursor, ctx)
		after = gen_text(node.cursor, ctx)
		before_text = if node.spaced " " else ""
		after_text = if node.spaced " " else ""
		$nodes = $nodes.append(Text(before_text)).concat(node.nodes).append(Text(Str.concat(after_text, after.text)))
		$md = Str.concat(Str.concat(Str.concat($md, before_text), node.md), Str.concat(after_text, after.md))
		$defs = $defs.concat(node.defs)
		$cursor = after.cursor
		$index = $index + 1
	}
	{ nodes: merge($nodes), md: $md, defs: $defs, cursor: $cursor }
}

## One non-text node; `spaced` asks the caller to surround it with spaces.
gen_node : Cursor, Context -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_node = |cursor, ctx| {
	kind = pick(cursor, if ctx.depth >= 3 5 else 11)
	c = kind.cursor
	match kind.value {
		0 => gen_code(c)
		1 => gen_html(c, ctx.doc)
		2 => if ctx.doc gen_code(c) else gen_break(c)
		3 => gen_autolink(c, ctx)
		4 => gen_code(c)
		5 => gen_delimited(c, ctx, Emph)
		6 => gen_delimited(c, ctx, Strong)
		7 => gen_delimited(c, ctx, Del)
		8 => if ctx.in_link gen_delimited(c, ctx, Emph) else gen_link(c, ctx, Bool.False)
		9 => gen_link(c, ctx, Bool.True)
		_ => if ctx.in_link gen_delimited(c, ctx, Strong) else gen_link(c, ctx, Bool.False)
	}
}

leaf : Cursor, List(Markdown.Inline), Str -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
leaf = |cursor, nodes, md| { nodes, md, defs: [], cursor, spaced: Bool.False }

gen_break : Cursor -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_break = |cursor| {
	form = pick(cursor, 3)
	md =
		match form.value {
			0 => "\\\n"
			1 => "  \n"
			_ => "   \n  "
		}
	leaf(form.cursor, [HardBreak], md)
}

html_samples : List(Str)
html_samples = ["<span>", "</span>", "<a href=\"x\" title='y z'>", "<br/>", "<!-- c -->", "<!---->", "<?p x ?>", "<![CDATA[ a ]]>", "<!DOCTYPE html>", "<x-y data-a=b\nc>"]

gen_html : Cursor, Bool -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_html = |cursor, doc| {
	# The last sample spans two lines; documents stay on one line.
	choice = pick(cursor, if doc html_samples.len() - 1 else html_samples.len())
	html = html_samples.get(choice.value) ?? "<span>"
	leaf(choice.cursor, [HtmlInline(html)], html)
}

code_chars : List(Str)
code_chars = ["a", " ", "`", "*", "\\", "[", "]", "<", "&amp;", "_", "é", "``"]

gen_code : Cursor -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_code = |cursor| {
	count = pick(cursor, 6)
	var $content = ""
	var $cursor = count.cursor
	var $index = 0
	while $index <= count.value {
		ch = pick($cursor, code_chars.len())
		$content = Str.concat($content, code_chars.get(ch.value) ?? "a")
		$cursor = ch.cursor
		$index = $index + 1
	}
	bytes = $content.to_utf8()
	fence = Str.repeat("`", longest_run(bytes, '`') + 1)
	first = bytes.first() ?? 0
	last = bytes.last() ?? 0
	all_spaces = bytes.all(|b| b == ' ')
	pad = first == '`' or last == '`' or (first == ' ' and last == ' ' and !all_spaces)
	inner = if pad " ${$content} " else $content
	leaf($cursor, [InlineCode($content)], "${fence}${inner}${fence}")
}

longest_run : List(U8), U8 -> U64
longest_run = |bytes, target| {
	var $best = 0
	var $current = 0
	for byte in bytes {
		if byte == target {
			$current = $current + 1
			if $current > $best {
				$best = $current
			}
		} else {
			$current = 0
		}
	}
	$best
}

gen_autolink : Cursor, Context -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_autolink = |cursor, ctx| {
	form = pick(cursor, 4)
	word = pick(form.cursor, 3)
	host = (["ex.com", "a-b.org/p?q=1", "x.y/(z)"].get(word.value)) ?? "ex.com"
	if ctx.in_link {
		gen_code(word.cursor)
	} else {
		match form.value {
			0 => leaf(word.cursor, [Link({ label: [Text("https://${host}")], target: { href: "https://${host}", title: None } })], "<https://${host}>")
			1 => leaf(word.cursor, [Link({ label: [Text("irc:${host}")], target: { href: "irc:${host}", title: None } })], "<irc:${host}>")
			2 => leaf(word.cursor, [Link({ label: [Text("me.too+x@ex.com")], target: { href: "mailto:me.too+x@ex.com", title: None } })], "<me.too+x@ex.com>")
			_ => leaf(word.cursor, [Link({ label: [Text("MAILTO:a@b")], target: { href: "MAILTO:a@b", title: None } })], "<MAILTO:a@b>")
		}
	}
}

DelimKind : [Emph, Strong, Del]

gen_delimited : Cursor, Context, DelimKind -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_delimited = |cursor, ctx, kind| {
	form = pick(cursor, 2)
	marker =
		match kind {
			Emph => if form.value == 0 "*" else "_"
			Strong => if form.value == 0 "**" else "__"
			Del => if form.value == 0 "~~" else "~"
		}
	char = (marker.to_utf8().first()) ?? '*'
	inner_ctx = { ..ctx, depth: ctx.depth + 1, ancestors: ctx.ancestors.append(char) }
	content = gen_sequence(form.cursor, inner_ctx, 2)
	text_only = content.nodes.all(is_text)
	intraword = char != '_' and text_only and !ctx.ancestors.contains(char)
	node =
		match kind {
			Emph => Markdown.Inline.Emphasis(content.nodes)
			Strong => Strong(content.nodes)
			Del => Strikethrough(content.nodes)
		}
	{ nodes: [node], md: "${marker}${content.md}${marker}", defs: content.defs, cursor: content.cursor, spaced: !intraword }
}

is_text : Markdown.Inline -> Bool
is_text = |node| {
	match node {
		Text(_) => Bool.True
		_ => Bool.False
	}
}

href_chars : List(Str)
href_chars = ["a", "/", ".", "b", "?", "=", "#", " ", "(", ")", "<", ">", "\\", "\"", "'", "&", "*", "é", "%20", "[", "]"]

gen_href : Cursor -> { href : Str, cursor : Cursor }
gen_href = |cursor| {
	count = pick(cursor, 7)
	var $href = ""
	var $cursor = count.cursor
	var $index = 0
	while $index < count.value {
		ch = pick($cursor, href_chars.len())
		$href = Str.concat($href, href_chars.get(ch.value) ?? "a")
		$cursor = ch.cursor
		$index = $index + 1
	}
	{ href: $href, cursor: $cursor }
}

title_chars : List(Str)
title_chars = ["t", " ", "\"", "'", "(", ")", "&", "\\", "*", "ü"]

gen_title : Cursor -> { title : [Some(Str), None], cursor : Cursor }
gen_title = |cursor| {
	present = pick(cursor, 2)
	if present.value == 0 {
		{ title: None, cursor: present.cursor }
	} else {
		count = pick(present.cursor, 5)
		var $title = ""
		var $cursor = count.cursor
		var $index = 0
		while $index < count.value {
			ch = pick($cursor, title_chars.len())
			$title = Str.concat($title, title_chars.get(ch.value) ?? "t")
			$cursor = ch.cursor
			$index = $index + 1
		}
		{ title: Some($title), cursor: $cursor }
	}
}

## Backslash-escape every ASCII punctuation character.
escape_punct : Str -> Str
escape_punct = |text| {
	var $out = []
	for byte in text.to_utf8() {
		if (byte >= 33 and byte <= 47) or (byte >= 58 and byte <= 64) or (byte >= 91 and byte <= 96) or (byte >= 123 and byte <= 126) {
			$out = $out.append('\\').append(byte)
		} else {
			$out = $out.append(byte)
		}
	}
	Str.from_utf8_lossy($out)
}

render_destination : Str, U64 -> Str
render_destination = |href, form| {
	bytes = href.to_utf8()
	plain_ok = !bytes.is_empty() and !bytes.contains(' ')
	if form == 0 or !plain_ok {
		"<${escape_punct(href)}>"
	} else {
		escape_punct(href)
	}
}

render_title : [Some(Str), None], U64 -> Str
render_title = |title, form| {
	match title {
		None => ""
		Some(text) =>
			match form {
				0 => " \"${escape_punct(text)}\""
				1 => " '${escape_punct(text)}'"
				_ => " (${escape_punct(text)})"
			}
	}
}

gen_link : Cursor, Context, Bool -> { nodes : List(Markdown.Inline), md : Str, defs : List(Str), cursor : Cursor, spaced : Bool }
gen_link = |cursor, ctx, image| {
	label = gen_sequence(cursor, { ..ctx, depth: ctx.depth + 1, in_link: ctx.in_link or !image }, 2)
	dest = gen_href(label.cursor)
	title = gen_title(dest.cursor)
	form = pick(title.cursor, 8)
	# Spaces around a destination are not part of the URL.
	target = { href: Str.trim(dest.href), title: title.title }
	node = if image Image({ alt: label.nodes, target }) else Link({ label: label.nodes, target })
	open = if image "![" else "["
	destination = render_destination(dest.href, form.value % 2)
	title_md = render_title(title.title, form.value % 3)
	# Shortcut and collapsed references use the label text itself as the
	# label, so it must not contain brackets (no code, HTML, links or images).
	plain_label = label.nodes.all(|n| is_text(n) or is_delimited(n))
	unique = "q${ctx.depth.to_str()}x${cursor.pos.to_str()}"
	if ctx.doc and form.value >= 4 {
		match form.value {
			4 => {
				key = "ref${cursor.pos.to_str()}"
				{ nodes: [node], md: "${open}${label.md}][${key}]", defs: label.defs.append("[${key}]: ${destination}${title_md}"), cursor: form.cursor, spaced: Bool.False }
			}

			_ if plain_label => {
				labelled = prefix_label(label.nodes, "${unique} ")
				labelled_node = if image Image({ alt: labelled, target }) else Link({ label: labelled, target })
				raw = "${unique} ${label.md}"
				suffix = if form.value == 5 "[]" else ""
				{ nodes: [labelled_node], md: "${open}${raw}]${suffix}", defs: label.defs.append("[${raw}]: ${destination}${title_md}"), cursor: form.cursor, spaced: Bool.False }
			}

			_ =>
				{ nodes: [node], md: "${open}${label.md}](${destination}${title_md})", defs: label.defs, cursor: form.cursor, spaced: Bool.False }
		}
	} else {
		{ nodes: [node], md: "${open}${label.md}](${destination}${title_md})", defs: label.defs, cursor: form.cursor, spaced: Bool.False }
	}
}

is_delimited : Markdown.Inline -> Bool
is_delimited = |node| {
	match node {
		Emphasis(children) => children.all(|n| is_text(n) or is_delimited(n))
		Strong(children) => children.all(|n| is_text(n) or is_delimited(n))
		Strikethrough(children) => children.all(|n| is_text(n) or is_delimited(n))
		_ => Bool.False
	}
}

prefix_label : List(Markdown.Inline), Str -> List(Markdown.Inline)
prefix_label = |nodes, prefix| merge(List.prepend(nodes, Text(prefix)))

## Merge adjacent text nodes and drop empty ones.
merge : List(Markdown.Inline) -> List(Markdown.Inline)
merge = |nodes| {
	var $out = []
	for node in nodes {
		match node {
			Text(text) if text.is_empty() => {}
			Text(text) =>
				match $out.last() {
					Ok(Text(previous)) => {
						$out = $out.drop_last(1).append(Text(Str.concat(previous, text)))
					}

					_ => {
						$out = $out.append(node)
					}
				}

			_ => {
				$out = $out.append(node)
			}
		}
	}
	$out
}

generate : List(U8) -> Input
generate = |bytes| {
	mode = pick({ bytes, pos: 0 }, 4)
	doc = mode.value == 0
	built = gen_sequence(mode.cursor, { depth: 0, in_link: Bool.False, ancestors: [], doc }, 4)
	md =
		if built.defs.is_empty() {
			built.md
		} else {
			Str.concat(Str.concat(built.md, "\n\n"), Str.join_with(built.defs, "\n"))
		}
	{ md, doc, expected: built.nodes }
}

test : Input -> Fuzz.Outcome
test = |input| {
	if input.doc {
		match String.parse_str(Markdown.all, input.md) {
			Ok([Paragraph(actual)]) if actual == input.expected => Fuzz.keep
			Ok(blocks) => crash "document mismatch\n--- markdown ---\n${input.md}\n--- expected ---\n${show(input.expected)}\n--- actual ---\n${blocks.map(Markdown.to_debug_str) |> Str.join_with("\n")}"
			Err(_) => crash "document failed to parse\n${input.md}"
		}
	} else {
		match String.parse_str(Markdown.inlines, input.md) {
			Ok(actual) if actual == input.expected => Fuzz.keep
			Ok(actual) => crash "inline mismatch\n--- markdown ---\n${input.md}\n--- expected ---\n${show(input.expected)}\n--- actual ---\n${show(actual)}"
			Err(_) => crash "inline failed to parse\n${input.md}"
		}
	}
}

show : List(Markdown.Inline) -> Str
show = |nodes| "[${nodes.map(Markdown.inline_to_debug_str) |> Str.join_with(", ")}]"

target = Fuzz.target_with({
	name: "markdown-inline-ast",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "${if input.doc "doc" else "inline"}: ${Str.inspect(input.md)}\n=> ${show(input.expected)}",
})
