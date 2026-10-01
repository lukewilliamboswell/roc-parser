app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.2/9weENCAXVZV14WFpwHqP3rpn46EDJWQQknSLpa1Hg5nL.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.Markdown
import parser.Utf8

## Performance property: inline inputs built by repeating pathological
## patterns (those of cmark's test/pathological_tests.py plus GFM autolink,
## entity and raw HTML scanners) thousands of times must parse within the
## runner's --timeout, i.e. without quadratic or exponential blowups. Bytes
## choose two patterns, how they interleave, the repetition count (up to
## 8191) and a filler string spliced into the patterns.

Input : { text : Str, name : Str, count : U64 }

## Each pattern is { prefix, repeat, middle, repeat_after, suffix }: the text
## is prefix + repeat*n + middle + repeat_after*n + suffix.
Pattern : { name : Str, prefix : Str, repeat : Str, middle : Str, after : Str, suffix : Str }

pattern : Str, Str, Str, Str, Str, Str -> Pattern
pattern = |name, prefix, repeat, middle, after, suffix| { name, prefix, repeat, middle, after, suffix }

patterns : Str -> List(Pattern)
patterns = |x| [
	pattern("nested strong emph", "", "*a **a ", "b", " a** a*", ""),
	pattern("emph closers without openers", "", "a_ ", "", "", ""),
	pattern("emph openers without closers", "", "_a ", "", "", ""),
	pattern("link closers without openers", "", "a]", "", "", ""),
	pattern("link openers without closers", "", "[a", "", "", ""),
	pattern("mismatched openers and closers", "", "*a_ ", "", "", ""),
	pattern("openers and closers multiple of 3", "a**b", "c* ", "", "", ""),
	pattern("link openers and emph closers", "", "[ a_", "", "", ""),
	pattern("pattern [ (](", "", "[ (](", "", "", ""),
	pattern("nested brackets", "", "[", "a", "]", ""),
	pattern("nested images", "", "![", "a", "](u)", ""),
	pattern("U+0000 in input", "", "abc\u(0)de\u(0)", "", "", ""),
	pattern("backtick runs", "", "e`", "", "e``", ""),
	pattern("unclosed links A", "", "[a](<b", "", "", ""),
	pattern("unclosed links B", "", "[a](b", "", "", ""),
	pattern("unclosed links C", "", "[a](b \"", "", "", ""),
	pattern("unclosed comments", "</", "<!--", "", "", ""),
	pattern("unclosed processing instructions", "", "<?", "", "", ""),
	pattern("unclosed CDATA", "", "<![CDATA[", "", "", ""),
	pattern("unclosed declarations", "", "<!A", "", "", ""),
	pattern("unclosed open tags", "", "<a b='", "", "", ""),
	pattern("unclosed autolinks", "", "<http:a", "", "", ""),
	pattern("nested parentheses in destination", "[a](", "(", "b", ")", ")"),
	pattern("tilde runs", "", "~a ", "", " a~", ""),
	pattern("mixed delimiters", "", "*_~", "x", "~_*", ""),
	pattern("emphasis in links", "", "[*a", "", "*](u)", ""),
	pattern("entity prefixes", "", "&#", "", "&a", ""),
	pattern("www autolinks", "", " www.a${x}.b(", "", ")", ""),
	pattern("scheme autolinks", "", "http://a.b/${x}?", "", "", ""),
	pattern("email autolinks", "", "a.b@c${x}", "", "@d.e ", ""),
	pattern("many at signs", "", "@", "a", ".b@", ""),
	pattern("long words before colon", "", "abc", "://", ":", ""),
	pattern("reference-like brackets", "", "[a][", "", "]", ""),
	pattern("hard breaks", "", "a  \n", "", "b\\\n", ""),
	pattern("filler", "", x, "", x, ""),
]

generate : List(U8) -> Input
generate = |bytes| {
	first = U8.to_u64(bytes.get(0) ?? 0)
	second = U8.to_u64(bytes.get(1) ?? 0)
	high = U8.to_u64(bytes.get(2) ?? 0)
	low = U8.to_u64(bytes.get(3) ?? 0)
	count = (high * 256 + low) % 8192
	filler = Str.from_utf8_lossy(bytes.drop_first(4).take_first(8))
	all = patterns(filler)
	a = all.get(first % all.len()) ?? pattern("empty", "", "", "", "", "")
	b = all.get(second % all.len()) ?? a
	interleave = (first / all.len()) % 2 == 1
	text =
		if interleave {
			# Alternate the two patterns' repeated units.
			unit = Str.concat(a.repeat, b.repeat)
			"${a.prefix}${b.prefix}${Str.repeat(unit, count)}${a.middle}${b.middle}${Str.repeat(Str.concat(b.after, a.after), count)}${b.suffix}${a.suffix}"
		} else {
			"${a.prefix}${Str.repeat(a.repeat, count)}${a.middle}${Str.repeat(a.after, count)}${a.suffix}"
		}
	{ text, name: if interleave "${a.name} + ${b.name}" else a.name, count }
}

test : Input -> Fuzz.Outcome
test = |input| {
	match Utf8.parse_str(Markdown.inline_parser, input.text) {
		Ok(nodes) => {
			# Walk the whole tree (linearly) so no work is skipped.
			if count_nodes(nodes) == 0 and !Str.is_empty(Str.trim(input.text)) {
				crash "${input.name}: non-blank input produced no nodes"
			}
			Fuzz.keep
		}

		Err(_) => crash "Markdown.inline_parser failed on ${input.name} x${input.count.to_str()}"
	}
}

## Count every node with an explicit stack: the parser bounds nesting at
## 1,000 levels, but the harness should not rely on the stack to walk a tree.
count_nodes : List(Markdown.Inline) -> U64
count_nodes = |nodes| {
	var $pending = [nodes]
	var $count = 0
	while !$pending.is_empty() {
		level = $pending.last() ?? []
		$pending = $pending.drop_last(1)
		for node in level {
			$count = $count + 1
			match node {
				Strong(children) => {
					$pending = $pending.append(children)
				}
				Emphasis(children) => {
					$pending = $pending.append(children)
				}
				Strikethrough(children) => {
					$pending = $pending.append(children)
				}
				Link({ label, .. }) => {
					$pending = $pending.append(label)
				}
				Image({ alt, .. }) => {
					$pending = $pending.append(alt)
				}
				_ => {}
			}
		}
	}
	$count
}

target = Fuzz.target_with({
	name: "markdown-inline-pathological",
	generator: Fuzz.map(Fuzz.raw_bytes, generate),
	test,
	show: |input| "${input.name} x${input.count.to_str()} (${Str.count_utf8_bytes(input.text).to_str()} bytes): ${Str.inspect(Str.from_utf8_lossy(input.text.to_utf8().take_first(200)))}",
})
