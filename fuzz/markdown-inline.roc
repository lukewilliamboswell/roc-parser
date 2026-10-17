app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.Parser
import parser.Utf8

## Arbitrary bytes through Markdown.inline_parser. Parsing never fails or crashes
## (also on invalid UTF-8), and for valid UTF-8:
##
## - the tree is well formed: no empty or adjacent text nodes, no link inside
##   a link's label, no line ending inside a code span, no hard break at the
##   end of the input;
## - CRLF line endings give the same tree as LF;
## - escaping every ASCII punctuation character (after replacing line
##   endings by spaces and dropping `@`, which GFM email autolinks match in
##   decoded text) gives exactly one text node with the input's text;
## - that escaped text inside `*x...x*` or `[x...x](u)` gives exactly that
##   emphasis or link, and the raw text inside a long enough code fence gives
##   exactly that code span.

Tree : List(Markdown.Inline)

parse : List(U8) -> Tree
parse = |bytes| {
	match Utf8.parse_bytes(Markdown.inline_parser, bytes) {
		Ok(nodes) => nodes
		Err(_) => crash "Markdown.inline_parser must accept every input: ${Str.inspect(Str.from_utf8_lossy(bytes))}"
	}
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	tree = parse(bytes)
	match Str.from_utf8(bytes) {
		Err(_) => Fuzz.keep
		Ok(input) => {
			check_well_formed(input, tree)
			check_crlf(input, tree)
			check_escaped(input)
			Fuzz.keep
		}
	}
}

show : Tree -> Str
show = |nodes| "[${Str.join_with(nodes.map(Str.inspect), ", ")}]"

fail : Str, Str, Tree -> {}
fail = |message, input, tree| {
	crash "${message}\ninput: ${Str.inspect(input)}\ntree:  ${show(tree)}"
}

## ---------------------------------------------------------------------------
## Well-formedness
## ---------------------------------------------------------------------------

check_well_formed : Str, Tree -> {}
check_well_formed = |input, tree| {
	match well_formed(tree, False) {
		Ok({}) => {}
		Err(problem) => fail(problem, input, tree)
	}
	match tree.last() {
		Ok(HardBreak) => fail("a hard break cannot end the input", input, tree)
		_ => {}
	}
}

well_formed : Tree, Bool -> Try({}, Str)
well_formed = |nodes, in_link| {
	var $previous_text = False
	var $result = Ok({})
	for node in nodes {
		problem =
			match node {
				Text(text) =>
					if text.is_empty() {
						Err("empty text node")
					} else if $previous_text {
						Err("adjacent text nodes")
					} else {
						Ok({})
					}

				InlineCode(code) =>
					if Str.contains(code, "\n") or Str.contains(code, "\r") {
						Err("line ending inside a code span")
					} else {
						Ok({})
					}

				Strong(children) => well_formed(children, in_link)
				Emphasis(children) => well_formed(children, in_link)
				Strikethrough(children) => well_formed(children, in_link)

				# Autolinks may appear in link text (cmark and markdown-it agree);
				# bracketed links may not.
				Link({ label, target }) =>
					if in_link and !is_autolink_shaped(label, target.href) {
						Err("link inside a link label")
					} else {
						well_formed(label, True)
					}

				Image({ alt, .. }) => well_formed(alt, in_link)
				HardBreak => Ok({})
				HtmlInline(html) => if html.is_empty() Err("empty raw HTML") else Ok({})
			}
		$previous_text =
			match node {
				Text(_) => True
				_ => False
			}
		if $result == Ok({}) {
			$result = problem
		}
	}
	$result
}

is_autolink_shaped : Tree, Str -> Bool
is_autolink_shaped = |label, href| {
	match label {
		[Text(text)] => text == href or Str.concat("mailto:", text) == href or Str.concat("http://", text) == href
		_ => False
	}
}

## ---------------------------------------------------------------------------
## CRLF
## ---------------------------------------------------------------------------

## Raw HTML and titles keep their line endings verbatim; compare them as LF.
lf : Tree -> Tree
lf = |nodes| nodes.map(lf_node)

lf_node : Markdown.Inline -> Markdown.Inline
lf_node = |node| {
	match node {
		HtmlInline(html) => HtmlInline(Str.replace_each(html, "\r\n", "\n"))
		Strong(children) => Strong(lf(children))
		Emphasis(children) => Emphasis(lf(children))
		Strikethrough(children) => Strikethrough(lf(children))
		Link({ label, target }) => Link({ label: lf(label), target: lf_target(target) })
		Image({ alt, target }) => Image({ alt: lf(alt), target: lf_target(target) })
		_ => node
	}
}

lf_target : Markdown.LinkTarget -> Markdown.LinkTarget
lf_target = |target| {
	match target.title {
		Ok(title) => { ..target, title: Ok(Str.replace_each(title, "\r\n", "\n")) }
		Err(Missing) => target
	}
}

check_crlf : Str, Tree -> {}
check_crlf = |input, tree| {
	if Str.contains(input, "\n") and !Str.contains(input, "\r") {
		crlf = parse(Str.replace_each(input, "\n", "\r\n").to_utf8())
		if lf(crlf) != tree {
			crash "CRLF line endings changed the tree\ninput: ${Str.inspect(input)}\nLF:   ${show(tree)}\nCRLF: ${show(crlf)}"
		}
	}
}

## ---------------------------------------------------------------------------
## Escaped text
## ---------------------------------------------------------------------------

is_ascii_punctuation : U8 -> Bool
is_ascii_punctuation = |byte| {
	(byte >= 33 and byte <= 47) or (byte >= 58 and byte <= 64) or (byte >= 91 and byte <= 96) or (byte >= 123 and byte <= 126)
}

## One line of text: line endings become spaces, `@` is dropped and U+0000
## becomes U+FFFD (CommonMark 2.3).
flatten : Str -> List(U8)
flatten = |input| {
	var $out = []
	for byte in input.to_utf8() {
		if byte == '\n' or byte == '\r' {
			$out = $out.append(' ')
		} else if byte == '@' {
			{}
		} else if byte == 0 {
			$out = $out.concat([0xEF, 0xBF, 0xBD])
		} else {
			$out = $out.append(byte)
		}
	}
	$out
}

escape : List(U8) -> List(U8)
escape = |bytes| {
	var $out = []
	for byte in bytes {
		if is_ascii_punctuation(byte) {
			$out = $out.append('\\').append(byte)
		} else {
			$out = $out.append(byte)
		}
	}
	$out
}

## Paragraph content drops leading spaces and tabs and trailing whitespace.
trim_paragraph : List(U8) -> List(U8)
trim_paragraph = |bytes| {
	var $start = 0
	while $start < bytes.len() and ((bytes.get($start) ?? 0) == ' ' or (bytes.get($start) ?? 0) == '\t') {
		$start = $start + 1
	}
	var $end = bytes.len()
	while $end > $start and is_trailing_space(bytes.get($end - 1) ?? 0) {
		$end = $end - 1
	}
	bytes.sublist({ start: $start, len: $end - $start })
}

## Only spaces and tabs: vertical tab and form feed are ordinary characters.
is_trailing_space : U8 -> Bool
is_trailing_space = |byte| byte == ' ' or byte == '\t'

text_tree : List(U8) -> Tree
text_tree = |bytes| if bytes.is_empty() [] else [Text(Str.from_utf8_lossy(bytes))]

check_escaped : Str -> {}
check_escaped = |input| {
	flat = flatten(input)
	escaped = escape(flat)
	text = Str.from_utf8_lossy(flat)

	plain = parse(escaped)
	expected_plain = text_tree(trim_paragraph(flat))
	if plain != expected_plain {
		mismatch("escaped text", escaped, expected_plain, plain)
	}

	emphasis_input = ['*', 'x'].concat(escaped).concat(['x', '*'])
	expected_emphasis = [Emphasis([Text("x${text}x")])]
	emphasis = parse(emphasis_input)
	if emphasis != expected_emphasis {
		mismatch("escaped text inside emphasis", emphasis_input, expected_emphasis, emphasis)
	}

	link_input = ['[', 'x'].concat(escaped).concat("x](u)".to_utf8())
	expected_link = [Link({ label: [Text("x${text}x")], target: { href: "u", title: Err(Missing) } })]
	link = parse(link_input)
	if link != expected_link {
		mismatch("escaped text inside a link", link_input, expected_link, link)
	}

	fence = List.repeat('`', longest_backtick_run(flat) + 1)
	code_input = fence.append(' ').concat(flat).append(' ').concat(fence)
	all_spaces = flat.all(|b| b == ' ')
	expected_code = [InlineCode(if all_spaces " ${text} " else text)]
	code = parse(code_input)
	if code != expected_code {
		mismatch("raw text inside a code span", code_input, expected_code, code)
	}
}

longest_backtick_run : List(U8) -> U64
longest_backtick_run = |bytes| {
	var $best = 0
	var $current = 0
	for byte in bytes {
		if byte == '`' {
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

mismatch : Str, List(U8), Tree, Tree -> {}
mismatch = |what, input, expected, actual| {
	crash "${what} did not parse as expected\ninput:    ${Str.inspect(Str.from_utf8_lossy(input))}\nexpected: ${show(expected)}\nactual:   ${show(actual)}"
}

target = Fuzz.target_with({
	name: "markdown-inline",
	generator: Fuzz.raw_bytes,
	test,
	show: |bytes| Str.inspect(Str.from_utf8_lossy(bytes)),
})
