import MarkdownEntities
import Parser
import Utf8
import unicode.Case
import unicode.GeneralCategory
import unicode.Scalar

## A Markdown document as a tree of blocks with inline content.
##
## The parser follows [CommonMark 0.31.2](https://spec.commonmark.org/0.31.2/)
## plus these GitHub Flavored Markdown extensions: tables, task list items,
## strikethrough (`~~text~~`) and extended autolinks (`www.example.com`,
## `https://example.com` and `me@example.com` without angle brackets). It also
## accepts one extension of its own: a frontmatter block between two `---`
## lines at the very start of a document, kept raw as `Frontmatter`.
##
## Markdown has no syntax errors, so parsing never fails: [Markdown.parse_str]
## returns `List(Markdown)` directly, and text that looks like broken syntax is
## kept as text. Soft line breaks are kept as `"\n"` inside `Text`. Block
## quotes and lists nest at most 1,000 levels deep, and so do emphasis,
## strikethrough, links and images; deeper markers, delimiters and brackets
## are read as text.
##
## **Security:** raw HTML passes through unchanged as `HtmlBlock` and
## `HtmlInline`, with no GFM tagfilter, and link destinations are not checked
## (a `javascript:` URL is kept as written). If you render untrusted Markdown
## to HTML, sanitize the HTML or drop those nodes and unsafe URLs yourself.
Markdown := [
	Heading({ level : Markdown.Level, content : List(Markdown.Inline) }),
	Paragraph(List(Markdown.Inline)),
	Blockquote(List(Markdown)),
	ListBlock({ kind : Markdown.ListKind, loose : Bool, items : List({ task : Markdown.TaskState, blocks : List(Markdown) }) }),
	Code({ info : Str, pre : Str }),
	ThematicBreak,
	Table({ header : List(List(Markdown.Inline)), align : List(Markdown.Alignment), rows : List(List(List(Markdown.Inline))) }),
	HtmlBlock(Str),
	Frontmatter(Str),
].{

	## Render a Markdown block in Roc source-like notation for inspection.
	to_inspect : Markdown -> Str
	to_inspect = |node| {
		inspect_markdown(node)
	}

	## Compare two Markdown syntax trees structurally.
	is_eq : _

	## Heading levels from one through six.
	Level := [One, Two, Three, Four, Five, Six].{

		## Render a heading level for inspection.
		to_inspect : Level -> Str
		to_inspect = |level| {
			inspect_level(level)
		}

		## Convert a heading level to its decimal representation.
		to_str : Level -> Str
		to_str = |level| {
			level_to_str(level)
		}

		## Compare two heading levels.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	## The marker and starting number of a Markdown list.
	ListKind := [
		Unordered,
		Ordered({ start : U64 }),
	].{

		## Render a list kind for inspection.
		to_inspect : ListKind -> Str
		to_inspect = |kind| {
			inspect_list_kind(kind)
		}

		## Convert a list kind to a compact string.
		to_str : ListKind -> Str
		to_str = |kind| {
			list_kind_to_str(kind)
		}

		## Compare two list kinds structurally.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	## Whether a list item is a task and, if so, whether it is checked.
	TaskState := [
		NoTask,
		Unchecked,
		Checked,
	].{

		## Render a task state for inspection.
		to_inspect : TaskState -> Str
		to_inspect = |task| {
			inspect_task_state(task)
		}

		## Convert a task state to a compact string.
		to_str : TaskState -> Str
		to_str = |task| {
			task_state_to_str(task)
		}

		## Compare two task states.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	## Column alignment declared by a Markdown table delimiter row.
	Alignment := [
		Default,
		Left,
		Center,
		Right,
	].{

		## Render a table alignment for inspection.
		to_inspect : Alignment -> Str
		to_inspect = |alignment| {
			inspect_alignment(alignment)
		}

		## Convert a table alignment to a compact string.
		to_str : Alignment -> Str
		to_str = |alignment| {
			alignment_to_str(alignment)
		}

		## Compare two table alignments.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	## Destination and optional title of a link or image.
	LinkTarget : {
		href : Str,
		title : Try(Str, [Missing]),
	}

	## Inline Markdown syntax nodes.
	Inline := [
		Text(Str),
		Strong(List(Inline)),
		Emphasis(List(Inline)),
		Strikethrough(List(Inline)),
		InlineCode(Str),
		Link({ label : List(Inline), target : LinkTarget }),
		Image({ alt : List(Inline), target : LinkTarget }),
		HardBreak,
		HtmlInline(Str),
	].{

		## Render an inline node in Roc source-like notation for inspection.
		to_inspect : Inline -> Str
		to_inspect = |inline| {
			inspect_inline(inline)
		}

		## Compare two inline syntax trees structurally.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	## Hash a Markdown syntax tree, so that it can be a `Dict` key or `Set` element.
	to_hash : _

	## Parse a whole Markdown document into its blocks.
	##
	## Every input is a document, so this never fails. A leading frontmatter
	## block becomes the first block, `Frontmatter`.
	##
	## ```roc
	## expect Markdown.parse_str("# Hi\n\nSome *text*.") == [
	##     Heading({ level: One, content: [Text("Hi")] }),
	##     Paragraph([Text("Some "), Emphasis([Text("text")]), Text(".")]),
	## ]
	## ```
	parse_str : Str -> List(Markdown)
	parse_str = |text| parse_document(text.to_utf8())

	## Parse inline content, such as a single line of a table cell or a title,
	## without any block structure. Reference links cannot resolve here, because
	## their definitions are blocks: use [Markdown.parse_str] for those.
	##
	## ```roc
	## expect Markdown.parse_inlines("**Bold** and `code`") == [
	##     Strong([Text("Bold")]),
	##     Text(" and "),
	##     InlineCode("code"),
	## ]
	## ```
	parse_inlines : Str -> List(Inline)
	parse_inlines = |text| parse_inline_bytes(text.to_utf8())

	## [Markdown.parse_str] as a [Parser] over bytes, for use with the
	## combinators. It consumes all of its input and never fails.
	parser : Parser(Utf8.Bytes, List(Markdown))
	parser = Parser.custom(|input| Ok({ value: parse_document(input), rest: [] }))

	## [Markdown.parse_inlines] as a [Parser] over bytes. It consumes all of its
	## input and never fails.
	inline_parser : Parser(Utf8.Bytes, List(Inline))
	inline_parser = Parser.custom(|input| Ok({ value: parse_inline_bytes(input), rest: [] }))

	## The raw text of the document's frontmatter, or `Err(Missing)` when the
	## document has none.
	##
	## ```roc
	## expect Markdown.frontmatter(Markdown.parse_str("---\ntitle: Hi\n---\nBody")) == Ok("title: Hi\n")
	## expect Markdown.frontmatter(Markdown.parse_str("Body")) == Err(Missing)
	## ```
	frontmatter : List(Markdown) -> Try(Str, [Missing])
	frontmatter = |blocks| {
		match blocks.first() {
			Ok(Frontmatter(raw)) => Ok(raw)
			_ => Err(Missing)
		}
	}

}

inspect_markdown : Markdown -> Str
inspect_markdown = |node| {
	match node {
		Heading({ level, content }) =>
			"Heading({ level: ${Str.inspect(level)}, content: ${Str.inspect(content)} })"

		Paragraph(content) =>
			"Paragraph(${Str.inspect(content)})"

		Blockquote(children) =>
			"Blockquote(${Str.inspect(children)})"

		ListBlock({ kind, loose, items }) =>
			"ListBlock({ kind: ${Str.inspect(kind)}, loose: ${Str.inspect(loose)}, items: ${Str.inspect(items)} })"

		Code({ info, pre }) =>
			"Code({ info: ${Str.inspect(info)}, pre: ${Str.inspect(pre)} })"

		ThematicBreak =>
			"ThematicBreak"

		Table({ header, align, rows }) =>
			"Table({ header: ${Str.inspect(header)}, align: ${Str.inspect(align)}, rows: ${Str.inspect(rows)} })"

		HtmlBlock(raw) =>
			"HtmlBlock(${Str.inspect(raw)})"

		Frontmatter(raw) =>
			"Frontmatter(${Str.inspect(raw)})"
	}
}

inspect_level : Markdown.Level -> Str
inspect_level = |level| {
	match level {
		One => "One"
		Two => "Two"
		Three => "Three"
		Four => "Four"
		Five => "Five"
		Six => "Six"
	}
}

level_to_str : Markdown.Level -> Str
level_to_str = |level| {
	match level {
		One => "1"
		Two => "2"
		Three => "3"
		Four => "4"
		Five => "5"
		Six => "6"
	}
}

inspect_list_kind : Markdown.ListKind -> Str
inspect_list_kind = |kind| {
	match kind {
		Unordered =>
			"Unordered"

		Ordered({ start }) =>
			"Ordered({ start: ${start.to_str()} })"
	}
}

list_kind_to_str : Markdown.ListKind -> Str
list_kind_to_str = |kind| {
	match kind {
		Unordered =>
			"unordered"

		Ordered({ start }) =>
			"ordered:${start.to_str()}"
	}
}

inspect_task_state : Markdown.TaskState -> Str
inspect_task_state = |task| {
	match task {
		NoTask => "NoTask"
		Unchecked => "Unchecked"
		Checked => "Checked"
	}
}

task_state_to_str : Markdown.TaskState -> Str
task_state_to_str = |task| {
	match task {
		NoTask => "none"
		Unchecked => "unchecked"
		Checked => "checked"
	}
}

inspect_alignment : Markdown.Alignment -> Str
inspect_alignment = |alignment| {
	match alignment {
		Default => "Default"
		Left => "Left"
		Center => "Center"
		Right => "Right"
	}
}

alignment_to_str : Markdown.Alignment -> Str
alignment_to_str = |alignment| {
	match alignment {
		Default => "default"
		Left => "left"
		Center => "center"
		Right => "right"
	}
}

inspect_inline : Markdown.Inline -> Str
inspect_inline = |inline| {
	match inline {
		Text(text) =>
			"Text(${Str.inspect(text)})"

		Strong(children) =>
			"Strong(${Str.inspect(children)})"

		Emphasis(children) =>
			"Emphasis(${Str.inspect(children)})"

		Strikethrough(children) =>
			"Strikethrough(${Str.inspect(children)})"

		InlineCode(code) =>
			"InlineCode(${Str.inspect(code)})"

		Link({ label, target }) =>
			"Link({ label: ${Str.inspect(label)}, target: ${inspect_link_target(target)} })"

		Image({ alt, target }) =>
			"Image({ alt: ${Str.inspect(alt)}, target: ${inspect_link_target(target)} })"

		HardBreak =>
			"HardBreak"

		HtmlInline(raw) =>
			"HtmlInline(${Str.inspect(raw)})"
	}
}

inspect_link_target : Markdown.LinkTarget -> Str
inspect_link_target = |target| {
	"{ href: ${Str.inspect(target.href)}, title: ${inspect_link_title(target.title)} }"
}

inspect_link_title : Try(Str, [Missing]) -> Str
inspect_link_title = |title| {
	match title {
		Ok(value) =>
			"Ok(${Str.inspect(value)})"

		Err(Missing) =>
			"Err(Missing)"
	}
}

ReferenceDefinition : {
	label : Str,
	target : Markdown.LinkTarget,
}

## Block structure follows the two-phase strategy of CommonMark 0.31.2
## (appendix "A parsing strategy"): lines are consumed one at a time against a
## stack of open blocks, then inline content is parsed once every link
## reference definition is known. GFM tables and task list items are layered
## on in the same places cmark-gfm hooks them in.
##
## The first phase records what happens as a flat list of events (a container
## opens or closes, a leaf block is complete, definitions were found), and the
## tree is built from them afterwards. Only the innermost leaf block collects
## lines while the document is read; they are kept outside the open-block
## stack. That keeps the whole parse linear in the input: a list growing inside
## a record that is itself stored elsewhere would be copied on every append.
ListInfo : { ordered : Bool, marker : U8, start : U64 }

OpenKind : [
	DocumentBlock,
	QuoteBlock,
	ListContainer(ListInfo),
	ItemBlock({ info : ListInfo, marker_offset : U64, padding : U64 }),
	ParagraphBlock,
	FencedBlock({ fence_char : U8, fence_len : U64, fence_offset : U64, info : Str }),
	IndentedBlock,
	HtmlBlockOpen(U64),
	TableBlock({ align : List(Markdown.Alignment), header : List(Utf8.Bytes) }),
]

## First and last source line of a closed block, used to decide list looseness:
## two siblings are separated by a blank line exactly when their spans leave a
## gap.
Span : { start : U64, end : U64 }

Open : {
	kind : OpenKind,
	start : U64,
	# The last line that belongs to a leaf block (an indented code block's
	# trailing blank lines do not).
	last : U64,
	has_children : Bool,
	# How many block quotes and lists enclose this block, itself included.
	nesting : U64,
}

## Block quotes and lists nest at most this deep. Deeper `>` and list markers
## are read as text, so that the tree stays shallow enough for recursive
## code (including `Str.inspect` and equality) to walk it.
max_nesting : U64
max_nesting = 1000

Event : [
	OpenContainer(OpenKind, U64),
	CloseContainer(U64),
	ItemTask(Markdown.TaskState),
	Leaf(Markdown, Span),
	Definitions(List(ReferenceDefinition)),
]

BlockState : {
	# The open blocks are the first `depth` entries of `stack`; the entries
	# after them are stale. Closing a block only lowers `depth`, because a list
	# that has been shortened is copied by the next append.
	stack : List(Open),
	depth : U64,
	line : Utf8.Bytes,
	# For each byte offset of the line: the offset of the next byte that is not
	# a space or tab, and the column of each offset (tab stops of 4). Computed
	# only for lines that start inside more than `index_depth` open blocks, so
	# that finding the next non-space character stays constant time however
	# deeply the containers nest. Otherwise both are empty and `find_nonspace`
	# scans, which costs at most `index_depth` passes over a run of spaces.
	next_nonspace : List(U64),
	columns : List(U64),
	line_number : U64,
	offset : U64,
	column : U64,
	partial_tab : Bool,
	all_closed : Bool,
	last_matched : U64,
	# The innermost leaf block's lines from earlier lines (read only), whether
	# that leaf was closed on this line, and the lines added on this line.
	leaf_lines : List(Utf8.Bytes),
	leaf_reset : Bool,
	leaf_added : List(Utf8.Bytes),
	events : List(Event),
}

Nonspace : { pos : U64, column : U64, indent : U64, blank : Bool }

Continuation : [Matched(BlockState), NotMatched, Consumed(BlockState)]

StartResult : [NoStart(BlockState), StartedContainer(BlockState), StartedLeaf(BlockState), LineDone(BlockState), StopStarts(BlockState)]

## Markdown has no syntax errors: every input is a document.
parse_document : Utf8.Bytes -> List(Markdown)
parse_document = |input| {
	all_lines = split_document_lines(input)
	front = take_frontmatter(all_lines)
	events = parse_block_lines(front.lines)
	refs = collect_definitions(events)
	blocks = List.from_iter(build_blocks(events, 0).blocks.iter().map(|block| resolve_inlines(block, refs)))

	match front.frontmatter {
		Ok(raw) => List.prepend(blocks, Frontmatter(raw))
		Err(_) => blocks
	}
}

## Line endings are LF, CRLF, or a lone CR; U+0000 becomes U+FFFD. A final line
## ending does not start another line.
split_document_lines : Utf8.Bytes -> List(Utf8.Bytes)
split_document_lines = |input| {
	# Lines are slices of the input; only a line with U+0000 is copied.
	var $lines = []
	var $start = 0
	var $has_nul = False
	var $index = 0
	len = input.len()
	while $index < len {
		$index = Utf8.find_any(input, $index, line_end_or_nul)
		byte = input.get($index) ?? 0
		if $index >= len {
			{}
		} else if byte == '\n' or byte == '\r' {
			$lines = $lines.append(document_line(input, $start, $index, $has_nul))
			if byte == '\r' and (input.get($index + 1) ?? 0) == '\n' {
				$index = $index + 1
			}
			$start = $index + 1
			$has_nul = False
		} else {
			$has_nul = True
		}
		$index = $index + 1
	}
	if $start < len {
		$lines.append(document_line(input, $start, len, $has_nul))
	} else {
		$lines
	}
}

line_end_or_nul : Utf8.ByteClass
line_end_or_nul = Utf8.ByteClass.from_bytes(['\n', '\r', 0])

document_line : Utf8.Bytes, U64, U64, Bool -> Utf8.Bytes
document_line = |input, start, end, has_nul| {
	line = input.sublist({ start, len: end - start })
	if has_nul {
		var $out = List.with_capacity(line.len() + 8)
		for byte in line {
			$out = if byte == 0 $out.concat([0xEF, 0xBF, 0xBD]) else $out.append(byte)
		}
		$out
	} else {
		line
	}
}

## Extension: a first line of exactly `---` up to the next line of exactly
## `---` is raw frontmatter, not Markdown. Without a closing line the document
## is ordinary Markdown.
take_frontmatter : List(Utf8.Bytes) -> { frontmatter : Try(Str, [NotFound]), lines : List(Utf8.Bytes) }
take_frontmatter = |lines| {
	if (lines.first() ?? []) != "---".to_utf8() {
		{ frontmatter: Err(NotFound), lines }
	} else {
		match drop_n(lines, 1).find_first_index(|line| line == "---".to_utf8()) {
			Ok(index) => {
				raw = lines.sublist({ start: 1, len: index }).fold([], |acc, line| acc.concat(line).append('\n'))
				{ frontmatter: Ok(bytes_to_str(raw)), lines: drop_n(lines, index + 2) }
			}

			Err(_) =>
				{ frontmatter: Err(NotFound), lines }
		}
	}
}

new_open : OpenKind, U64 -> Open
new_open = |kind, line_number| {
	{ kind, start: line_number, last: line_number, has_children: False, nesting: 0 }
}

parse_block_lines : List(Utf8.Bytes) -> List(Event)
parse_block_lines = |lines| {
	var $state = {
		stack: [new_open(DocumentBlock, 0)],
		depth: 1,
		line: [],
		next_nonspace: [],
		columns: [],
		line_number: 0,
		offset: 0,
		column: 0,
		partial_tab: False,
		all_closed: True,
		last_matched: 0,
		leaf_lines: [],
		leaf_reset: False,
		leaf_added: [],
		events: [],
	}
	var $events = []
	var $leaf = []
	var $reset = False
	var $added = []
	for line in lines {
		# The previous line's result is gone by now, so the leaf's lines are
		# not shared and grow in place.
		$leaf = if $reset $added else $leaf.concat($added)
		index = if $state.depth > index_depth index_line(line) else { next_nonspace: [], columns: [] }
		result = process_line({ ..$state, line, next_nonspace: index.next_nonspace, columns: index.columns, line_number: $state.line_number + 1, leaf_lines: $leaf, leaf_reset: False, leaf_added: [], events: [] })
		$events = $events.concat(result.events)
		$reset = result.leaf_reset
		$added = result.leaf_added
		$state = { ..result, leaf_lines: [], leaf_added: [], events: [] }
	}
	$leaf = if $reset $added else $leaf.concat($added)
	var $closing = { ..$state, leaf_lines: $leaf, leaf_reset: False, leaf_added: [], events: [] }
	while $closing.depth > 1 {
		$closing = close_tip($closing, $closing.line_number)
	}
	$events.concat($closing.events)
}

tip_kind : BlockState -> OpenKind
tip_kind = |state| {
	tip_open(state).kind
}

is_paragraph_kind : OpenKind -> Bool
is_paragraph_kind = |kind| {
	match kind {
		ParagraphBlock => True
		_ => False
	}
}

## Leaf blocks that take whole lines: no block starts are tried inside them.
accepts_lines : OpenKind -> Bool
accepts_lines = |kind| {
	match kind {
		FencedBlock(_) => True
		IndentedBlock => True
		HtmlBlockOpen(_) => True
		_ => False
	}
}

process_line : BlockState -> BlockState
process_line = |initial| {
	var $s = { ..initial, offset: 0, column: 0, partial_tab: False }

	# 1. Match the line against each open block's continuation condition.
	var $container = 0
	var $index = 1
	var $matching = True
	var $consumed = False
	while $matching and $index < $s.depth {
		open = $s.stack.get($index) ?? new_open(DocumentBlock, 0)
		match continue_block($s, open, $index) {
			Matched(next) => {
				$s = next
				$container = $index
				$index = $index + 1
			}

			NotMatched => {
				$matching = False
			}

			Consumed(next) => {
				$s = next
				$matching = False
				$consumed = True
			}
		}
	}

	if $consumed {
		return $s
	}

	$s = { ..$s, all_closed: $container == $s.depth - 1, last_matched: $container }

	# 2. Try block starts, unless the matched block is a leaf taking raw lines.
	container_kind = ($s.stack.get($container) ?? new_open(DocumentBlock, 0)).kind
	var $starting = !accepts_lines(container_kind)
	var $done = False
	while $starting {
		ns = find_nonspace($s)
		match try_block_starts($s, $container, ns) {
			NoStart(next) => {
				$s = advance_to_nonspace(next, ns)
				$starting = False
			}

			StartedContainer(next) => {
				$s = next
				$container = next.depth - 1
			}

			StopStarts(next) => {
				$s = advance_to_nonspace(next, find_nonspace(next))
				$container = next.depth - 1
				$starting = False
			}

			StartedLeaf(next) => {
				$s = next
				$container = next.depth - 1
				$starting = False
			}

			LineDone(next) => {
				$s = next
				$starting = False
				$done = True
			}
		}
	}

	if $done {
		return $s
	}

	# 3. Add the rest of the line to the right block.
	blank = find_nonspace($s).blank
	if !$s.all_closed and !blank and is_paragraph_kind(tip_kind($s)) {
		# Lazy continuation line.
		add_line_to_tip($s)
	} else {
		$s = close_unmatched($s)
		match tip_kind($s) {
			ParagraphBlock => add_line_to_tip($s)
			FencedBlock(_) => add_line_to_tip($s)
			IndentedBlock => add_line_to_tip($s)
			HtmlBlockOpen(html_type) => {
				rest = drop_n($s.line, $s.offset)
				added = add_line_to_tip($s)
				if html_type <= 5 and html_block_ends(html_type, rest) {
					close_tip(added, added.line_number)
				} else {
					added
				}
			}

			TableBlock(_) => add_line_to_tip($s)
			_ =>
				if blank {
					$s
				} else {
					add_line_to_tip(add_child($s, ParagraphBlock))
				}
		}
	}
}

index_line : Utf8.Bytes -> { next_nonspace : List(U64), columns : List(U64) }
index_line = |line| {
	len = line.len()
	var $columns = List.with_capacity(len + 1)
	var $column = 0
	for byte in line {
		$columns = $columns.append($column)
		$column = if byte == '\t' $column + (4 - ($column % 4)) else $column + 1
	}
	$columns = $columns.append($column)

	var $next = List.repeat(len, len + 1)
	var $index = len
	while $index > 0 {
		$index = $index - 1
		if is_space_or_tab(line.get($index) ?? 'x') {
			$next = List.set($next, $index, $next.get($index + 1) ?? len) ?? $next
		} else {
			$next = List.set($next, $index, $index) ?? $next
		}
	}
	{ next_nonspace: $next, columns: $columns }
}

## The first character from the current offset that is not a space or tab. A
## partly consumed tab at the offset counts only its remaining columns.
find_nonspace : BlockState -> Nonspace
find_nonspace = |s| {
	len = s.line.len()
	if s.next_nonspace.is_empty() {
		pos = if s.offset >= len len else Utf8.skip_class(s.line, s.offset, line_spaces)
		# The column after the skipped spaces and tabs. A partly consumed tab
		# at the offset ends at the same tab stop as a whole one would.
		var $column = s.column
		var $index = s.offset
		while $index < pos {
			$column = if byte_at(s.line, $index) == '\t' $column + (4 - ($column % 4)) else $column + 1
			$index = $index + 1
		}
		column = $column
		{ pos, column, indent: column - s.column, blank: pos >= len }
	} else {
		pos = if s.offset >= len len else s.next_nonspace.get(s.offset) ?? len
		column = s.columns.get(pos) ?? s.column
		{ pos, column, indent: if column > s.column column - s.column else 0, blank: pos >= len }
	}
}

## Lines inside more open blocks than this get a precomputed whitespace index.
index_depth : U64
index_depth = 8

line_spaces : Utf8.ByteClass
line_spaces = Utf8.ByteClass.from_bytes([' ', '\t'])

advance_to_nonspace : BlockState, Nonspace -> BlockState
advance_to_nonspace = |s, ns| {
	{ ..s, offset: ns.pos, column: ns.column, partial_tab: False }
}

## Advance by `count` bytes, or by `count` columns when `columns` is set, in
## which case a tab may be only partly consumed (CommonMark 2.2 Tabs).
advance_offset : BlockState, U64, Bool -> BlockState
advance_offset = |s, count, columns| {
	var $count = count
	var $offset = s.offset
	var $column = s.column
	var $partial = s.partial_tab
	while $count > 0 and $offset < s.line.len() {
		byte = s.line.get($offset) ?? 0
		if byte == '\t' {
			to_tab = 4 - ($column % 4)
			if columns {
				$partial = to_tab > $count
				step = if to_tab > $count $count else to_tab
				$column = $column + step
				$offset = if $partial $offset else $offset + 1
				$count = $count - step
			} else {
				$partial = False
				$column = $column + to_tab
				$offset = $offset + 1
				$count = $count - 1
			}
		} else {
			$partial = False
			$offset = $offset + 1
			$column = $column + 1
			$count = $count - 1
		}
	}
	{ ..s, offset: $offset, column: $column, partial_tab: $partial }
}

has_children : BlockState, U64, Open -> Bool
has_children = |s, index, open| {
	open.has_children or index + 1 < s.depth
}

continue_block : BlockState, Open, U64 -> Continuation
continue_block = |s, open, index| {
	ns = find_nonspace(s)

	match open.kind {
		QuoteBlock =>
			if ns.indent <= 3 and (s.line.get(ns.pos) ?? 0) == '>' {
				Matched(skip_quote_marker(advance_to_nonspace(s, ns)))
			} else {
				NotMatched
			}

		ItemBlock(item) =>
			if ns.indent >= item.marker_offset + item.padding {
				Matched(advance_offset(s, item.marker_offset + item.padding, True))
			} else if ns.blank and has_children(s, index, open) {
				Matched(advance_to_nonspace(s, ns))
			} else {
				NotMatched
			}

		FencedBlock(fence) => {
			rest = drop_n(s.line, ns.pos)
			if ns.indent <= 3 and is_closing_fence(rest, fence.fence_char, fence.fence_len) {
				closed = close_tip(set_tip_last(s, s.line_number), s.line_number)
				Consumed(closed)
			} else {
				var $next = s
				var $remaining = fence.fence_offset
				while $remaining > 0 and is_space_or_tab($next.line.get($next.offset) ?? 'x') {
					$next = advance_offset($next, 1, True)
					$remaining = $remaining - 1
				}
				Matched($next)
			}
		}

		IndentedBlock =>
			if ns.indent >= 4 {
				Matched(advance_offset(s, 4, True))
			} else if ns.blank {
				Matched(advance_to_nonspace(s, ns))
			} else {
				NotMatched
			}

		HtmlBlockOpen(html_type) =>
			if ns.blank and html_type >= 6 {
				NotMatched
			} else {
				Matched(s)
			}

		ParagraphBlock =>
			if ns.blank NotMatched else Matched(s)

		TableBlock(_) =>
			if ns.blank or split_table_row(drop_n(s.line, ns.pos)).is_empty() {
				NotMatched
			} else {
				Matched(s)
			}

		_ =>
			Matched(s)
	}
}

skip_quote_marker : BlockState -> BlockState
skip_quote_marker = |s| {
	after = advance_offset(s, 1, False)
	if is_space_or_tab(after.line.get(after.offset) ?? 'x') {
		advance_offset(after, 1, True)
	} else {
		after
	}
}

set_tip_last : BlockState, U64 -> BlockState
set_tip_last = |s, line_number| update_tip(s, |open| { ..open, last: line_number })

## Close blocks left unmatched by a line that is not a lazy continuation.
close_unmatched : BlockState -> BlockState
close_unmatched = |s| {
	if s.all_closed {
		s
	} else {
		var $next = s
		while $next.depth - 1 > s.last_matched {
			$next = close_tip($next, s.line_number - 1)
		}
		{ ..$next, all_closed: True }
	}
}

can_contain : OpenKind, OpenKind -> Bool
can_contain = |parent, child| {
	match parent {
		DocumentBlock => !is_item_kind(child)
		QuoteBlock => !is_item_kind(child)
		ItemBlock(_) => !is_item_kind(child)
		ListContainer(_) => is_item_kind(child)
		_ => False
	}
}

is_item_kind : OpenKind -> Bool
is_item_kind = |kind| {
	match kind {
		ItemBlock(_) => True
		_ => False
	}
}

add_child : BlockState, OpenKind -> BlockState
add_child = |s, kind| {
	var $next = s
	while !can_contain(tip_kind($next), kind) {
		$next = close_tip($next, $next.line_number - 1)
	}
	parent = update_tip($next, |open| { ..open, has_children: True })
	opened =
		match kind {
			QuoteBlock | ListContainer(_) | ItemBlock(_) => { ..parent, events: parent.events.append(OpenContainer(kind, parent.line_number)) }
			_ => parent
		}
	push_open(opened, new_open(kind, opened.line_number))
}

## Attach a block that is complete as soon as it starts (headings, breaks).
add_closed_child : BlockState, Markdown -> BlockState
add_closed_child = |s, block| {
	var $next = s
	while !can_contain(tip_kind($next), ParagraphBlock) {
		$next = close_tip($next, $next.line_number - 1)
	}
	span = { start: $next.line_number, end: $next.line_number }
	parent = update_tip($next, |open| { ..open, has_children: True })
	{ ..parent, events: parent.events.append(Leaf(block, span)) }
}

## The innermost open block.
tip_open : BlockState -> Open
tip_open = |s| s.stack.get(s.depth - 1) ?? new_open(DocumentBlock, 0)

update_tip : BlockState, (Open -> Open) -> BlockState
update_tip = |s, f| { ..s, stack: s.stack.update(s.depth - 1, f) ?? s.stack }

push_open : BlockState, Open -> BlockState
push_open = |s, open_block| {
	parent = tip_open(s).nesting
	nesting =
		match open_block.kind {
			QuoteBlock | ListContainer(_) => parent + 1
			_ => parent
		}
	open = { ..open_block, nesting }
	stack = if s.depth < s.stack.len() s.stack.set(s.depth, open) ?? s.stack else s.stack.append(open)
	{ ..s, stack, depth: s.depth + 1 }
}

## Forget the innermost open block.
pop_open : BlockState -> BlockState
pop_open = |s| { ..s, depth: s.depth - 1 }

## The rest of the line from the current offset; a partly consumed tab
## contributes its remaining columns as spaces.
line_rest : BlockState -> Utf8.Bytes
line_rest = |s| {
	if s.partial_tab {
		List.repeat(' ', 4 - (s.column % 4)).concat(drop_n(s.line, s.offset + 1))
	} else {
		drop_n(s.line, s.offset)
	}
}

add_line_to_tip : BlockState -> BlockState
add_line_to_tip = |s| {
	content = line_rest(s)
	counts = !(is_indented_kind(tip_kind(s)) and bytes_are_blank(content))
	added = { ..s, leaf_added: s.leaf_added.append(content) }
	if counts set_tip_last(added, added.line_number) else added
}

## The innermost leaf block's lines so far.
tip_lines : BlockState -> List(Utf8.Bytes)
tip_lines = |s| if s.leaf_reset s.leaf_added else s.leaf_lines.concat(s.leaf_added)

tip_last_line : BlockState -> Utf8.Bytes
tip_last_line = |s| {
	match s.leaf_added.last() {
		Ok(line) => line
		Err(_) => if s.leaf_reset [] else s.leaf_lines.last() ?? []
	}
}

## Forget the innermost leaf block's lines (it was closed or emptied).
reset_leaf : BlockState -> BlockState
reset_leaf = |s| { ..s, leaf_reset: True, leaf_added: [] }

is_indented_kind : OpenKind -> Bool
is_indented_kind = |kind| {
	match kind {
		IndentedBlock => True
		_ => False
	}
}

## Pop the innermost open block, finish it, and record it.
close_tip : BlockState, U64 -> BlockState
close_tip = |s, line_number| {
	open = tip_open(s)
	match open.kind {
		DocumentBlock => s
		QuoteBlock | ListContainer(_) | ItemBlock(_) => pop_open({ ..s, events: s.events.append(CloseContainer(line_number)) })
		_ => {
			finished = finish_leaf(open, tip_lines(s))
			reset_leaf(pop_open({ ..s, events: s.events.concat(finished) }))
		}
	}
}

has_gap : List(Span) -> Bool
has_gap = |spans| {
	var $index = 1
	var $gap = False
	while !$gap and $index < spans.len() {
		before = spans.get($index - 1) ?? { start: 0, end: 0 }
		after = spans.get($index) ?? { start: 0, end: 0 }
		if after.start > before.end + 1 {
			$gap = True
		}
		$index = $index + 1
	}
	$gap
}

placeholder : Utf8.Bytes -> List(Markdown.Inline)
placeholder = |raw| [Text(bytes_to_str(raw))]

## The events for a closed leaf block with these lines.
finish_leaf : Open, List(Utf8.Bytes) -> List(Event)
finish_leaf = |open, lines| {
	span = { start: open.start, end: open.last }
	match open.kind {
		ParagraphBlock => {
			extracted = extract_reference_definitions(join_with_newlines(lines), [])
			content = trim_end_spaces(extracted.rest)
			definitions = if extracted.refs.is_empty() [] else [Definitions(extracted.refs)]
			if content.is_empty() definitions else definitions.append(Leaf(Paragraph(placeholder(content)), span))
		}

		FencedBlock(fence) =>
			[Leaf(Code({ info: fence.info, pre: bytes_to_str(join_lines_with_newlines(lines)) }), span)]

		IndentedBlock => {
			var $lines = lines
			while bytes_are_blank($lines.last() ?? [0]) {
				$lines = drop_last_n($lines, 1)
			}
			[Leaf(Code({ info: "", pre: bytes_to_str(join_lines_with_newlines($lines)) }), span)]
		}

		HtmlBlockOpen(_) =>
			[Leaf(HtmlBlock(bytes_to_str(join_lines_with_newlines(lines))), span)]

		TableBlock(table) => {
			columns = table.align.len()
			fit = |cells| {
				var $out = []
				var $column = 0
				while $column < columns {
					$out = $out.append(placeholder(cells.get($column) ?? []))
					$column = $column + 1
				}
				$out
			}
			rows = lines.map(|line| fit(split_table_row(line)))
			[Leaf(Table({ header: fit(table.header), align: table.align, rows }), span)]
		}

		_ => []
	}
}

## Rebuild the tree from the events, starting at `start` and stopping at the
## close of the enclosing container. Looseness comes from the children's spans.
Built : [BuiltBlock(Markdown, Span), BuiltItem({ task : Markdown.TaskState, blocks : List(Markdown), span : Span, inner_gap : Bool })]

build_blocks : List(Event), U64 -> { blocks : List(Markdown), built : List(Built), index : U64, end : U64 }
build_blocks = |events, start| {
	var $index = start
	var $blocks = []
	var $built = []
	var $end = 0
	var $running = True
	while $running and $index < events.len() {
		event = events.get($index) ?? CloseContainer(0)
		match event {
			Leaf(block, span) => {
				$blocks = $blocks.append(block)
				$built = $built.append(BuiltBlock(block, span))
				$index = $index + 1
			}

			OpenContainer(kind, line) => {
				{ task, inner_start } =
					match events.get($index + 1) {
						Ok(ItemTask(state)) => { task: state, inner_start: $index + 2 }
						_ => { task: NoTask, inner_start: $index + 1 }
					}
				inner = build_blocks(events, inner_start)
				spans = inner.built.map(built_span)
				child =
					match kind {
						ItemBlock(_) => {
							end = (spans.last() ?? { start: line, end: line }).end
							BuiltItem({ task, blocks: inner.blocks, span: { start: line, end }, inner_gap: has_gap(spans) })
						}

						ListContainer(info) => {
							items = inner.built.keep_if(is_built_item)
							loose = items.any(built_inner_gap) or has_gap(spans)
							list_kind = if info.ordered Ordered({ start: info.start }) else Unordered
							block = ListBlock({ kind: list_kind, loose, items: items.map(built_item) })
							BuiltBlock(block, { start: line, end: (spans.last() ?? { start: line, end: line }).end })
						}

						_ =>
							BuiltBlock(Blockquote(inner.blocks), { start: line, end: inner.end })
					}
				match child {
					BuiltBlock(block, _) => {
						$blocks = $blocks.append(block)
					}

					_ => {}
				}
				$built = $built.append(child)
				$index = inner.index + 1
			}

			CloseContainer(line) => {
				$end = line
				$running = False
			}

			_ => {
				$index = $index + 1
			}
		}
	}
	{ blocks: $blocks, built: $built, index: $index, end: $end }
}

built_span : Built -> Span
built_span = |built| {
	match built {
		BuiltBlock(_, span) => span
		BuiltItem(item) => item.span
	}
}

is_built_item : Built -> Bool
is_built_item = |built| {
	match built {
		BuiltItem(_) => True
		_ => False
	}
}

built_inner_gap : Built -> Bool
built_inner_gap = |built| {
	match built {
		BuiltItem(item) => item.inner_gap
		_ => False
	}
}

built_item : Built -> { task : Markdown.TaskState, blocks : List(Markdown) }
built_item = |built| {
	match built {
		BuiltItem(item) => { task: item.task, blocks: item.blocks }
		BuiltBlock(_, _) => { task: NoTask, blocks: [] }
	}
}

## Every definition, in document order; the first one for a label wins.
collect_definitions : List(Event) -> List(ReferenceDefinition)
collect_definitions = |events| {
	var $refs = []
	for event in events {
		match event {
			Definitions(found) => {
				for def in found {
					if !$refs.any(|ref| ref.label == def.label) {
						$refs = $refs.append(def)
					}
				}
			}

			_ => {}
		}
	}
	$refs
}

join_with_newlines : List(Utf8.Bytes) -> Utf8.Bytes
join_with_newlines = |lines| {
	var $out = List.with_capacity(lines.fold(0, |sum, line| sum + line.len() + 1))
	for line in lines {
		if !$out.is_empty() {
			$out = $out.append('\n')
		}
		$out = $out.concat(line)
	}
	$out
}

## Block starts, in the order of CommonMark appendix A / cmark-gfm.
try_block_starts : BlockState, U64, Nonspace -> StartResult
try_block_starts = |s, container, ns| {
	container_kind = (s.stack.get(container) ?? new_open(DocumentBlock, 0)).kind
	rest = drop_n(s.line, ns.pos)
	indented = ns.indent >= 4
	first = rest.first() ?? 0

	nesting = (s.stack.get(container) ?? new_open(DocumentBlock, 0)).nesting
	if !indented and first == '>' and nesting < max_nesting {
		started = add_child(close_unmatched(skip_quote_marker(advance_to_nonspace(s, ns))), QuoteBlock)
		return StartedContainer(started)
	}

	if !indented and first == '#' {
		match parse_atx_heading(rest) {
			Ok(heading) => return LineDone(add_closed_child(close_unmatched(s), heading))
			Err(_) => {}
		}
	}

	if !indented and (first == '`' or first == '~') {
		match parse_fence_open(rest) {
			Ok(fence) => {
				kind = FencedBlock({ ..fence, fence_offset: ns.indent })
				return LineDone(add_child(close_unmatched(s), kind))
			}

			Err(_) => {}
		}
	}

	if !indented and first == '<' {
		lazy_paragraph = !s.all_closed and !ns.blank and is_paragraph_kind(tip_kind(s))
		allow_type_7 = !is_paragraph_kind(container_kind) and !lazy_paragraph
		match html_block_start(rest, allow_type_7) {
			Ok(html_type) => return StartedLeaf(add_child(close_unmatched(s), HtmlBlockOpen(html_type)))
			Err(_) => {}
		}
	}

	if !indented and is_paragraph_kind(container_kind) and (first == '=' or first == '-') {
		match setext_level(rest) {
			Ok(level) => {
				closed = close_unmatched(s)
				paragraph = tip_open(closed)
				extracted = extract_reference_definitions(join_with_newlines(tip_lines(closed)), [])
				definitions = if extracted.refs.is_empty() [] else [Definitions(extracted.refs)]
				if !extracted.rest.is_empty() {
					heading = Heading({ level, content: placeholder(trim_end_spaces(extracted.rest)) })
					span = { start: paragraph.start, end: closed.line_number }
					events = closed.events.concat(definitions).append(Leaf(heading, span))
					return LineDone(reset_leaf(pop_open({ ..closed, events })))
				} else {
					# Only reference definitions: keep the (now empty) paragraph and
					# let the underline be read as something else.
					return try_after_setext(reset_leaf({ ..closed, events: closed.events.concat(definitions) }), container, ns)
				}
			}

			Err(_) => {}
		}
	}

	try_after_setext(s, container, ns)
}

try_after_setext : BlockState, U64, Nonspace -> StartResult
try_after_setext = |s, container, ns| {
	container_open = s.stack.get(container) ?? new_open(DocumentBlock, 0)
	container_kind = container_open.kind
	rest = drop_n(s.line, ns.pos)
	indented = ns.indent >= 4

	if !indented and is_thematic_break(rest) {
		return LineDone(add_closed_child(close_unmatched(s), ThematicBreak))
	}

	# Another item of the matched list does not nest deeper.
	may_nest = container_open.nesting < max_nesting or is_list_kind(container_kind)
	if !indented and may_nest {
		match parse_list_marker(rest, is_paragraph_kind(container_kind)) {
			Ok(marker) => return start_list_item(s, container_kind, ns, marker)
			Err(_) => {}
		}
	}

	if indented and !is_paragraph_kind(tip_kind(s)) and !ns.blank {
		started = add_child(close_unmatched(advance_offset(s, 4, True)), IndentedBlock)
		return StartedLeaf(started)
	}

	if !indented and is_paragraph_kind(container_kind) and s.all_closed {
		match parse_table_delimiter_row(rest) {
			Ok(align) => {
				header = split_table_row(tip_last_line(s))
				if !header.is_empty() and header.len() == align.len() {
					# The paragraph keeps its other lines; its last line is the header.
					lines = tip_lines(s)
					before = { ..set_tip_last(s, s.line_number - 2), leaf_reset: True, leaf_added: drop_last_n(lines, 1) }
					closed = close_tip(before, s.line_number - 2)
					table = { ..new_open(TableBlock({ align, header }), s.line_number - 1), last: s.line_number }
					return LineDone(push_open(closed, table))
				}
			}

			Err(_) => {}
		}
	}

	NoStart(s)
}

ListMarker : { info : ListInfo, width : U64 }

start_list_item : BlockState, OpenKind, Nonspace, ListMarker -> StartResult
start_list_item = |s, container_kind, ns, marker| {
	marker_offset = ns.indent
	at_marker = advance_offset(advance_to_nonspace(s, ns), marker.width, True)
	spaces_start = at_marker
	var $probe = advance_offset(at_marker, 1, True)
	while $probe.column - spaces_start.column < 5 and is_space_or_tab($probe.line.get($probe.offset) ?? 'x') {
		$probe = advance_offset($probe, 1, True)
	}
	blank_item = $probe.offset >= $probe.line.len()
	spaces_after = $probe.column - spaces_start.column
	{ after_marker, padding } =
		if spaces_after >= 5 or spaces_after < 1 or blank_item {
			skipped = if is_space_or_tab(spaces_start.line.get(spaces_start.offset) ?? 'x') advance_offset(spaces_start, 1, True) else spaces_start
			{ after_marker: skipped, padding: marker.width + 1 }
		} else {
			{ after_marker: $probe, padding: marker.width + spaces_after }
		}

	closed = close_unmatched(after_marker)
	with_list =
		match container_kind {
			ListContainer(info) if lists_match(info, marker.info) and is_list_tip(closed) => closed
			_ => add_child(closed, ListContainer(marker.info))
		}
	item = add_child(with_list, ItemBlock({ info: marker.info, marker_offset, padding }))

	# GFM task list item: `[ ]`, `[x]` or `[X]` then a space or tab opens the
	# item's first paragraph.
	task_ns = find_nonspace(item)
	task_rest = drop_n(item.line, task_ns.pos)
	match task_rest {
		['[', mark, ']', after, ..] if task_ns.indent < 4 and (mark == ' ' or mark == 'x' or mark == 'X') and is_space_or_tab(after) => {
			task = if mark == ' ' Unchecked else Checked
			marked = { ..item, events: item.events.append(ItemTask(task)) }
			StopStarts(advance_offset(advance_to_nonspace(marked, task_ns), 3, False))
		}

		_ =>
			StartedContainer(item)
	}
}

is_list_kind : OpenKind -> Bool
is_list_kind = |kind| {
	match kind {
		ListContainer(_) => True
		_ => False
	}
}

is_list_tip : BlockState -> Bool
is_list_tip = |s| {
	match tip_kind(s) {
		ListContainer(_) => True
		_ => False
	}
}

lists_match : ListInfo, ListInfo -> Bool
lists_match = |a, b| {
	a.ordered == b.ordered and a.marker == b.marker
}

is_space_or_tab : U8 -> Bool
is_space_or_tab = |byte| byte == ' ' or byte == '\t'

## List item marker at the first non-space (CommonMark 5.2). When it would
## interrupt a paragraph, the item may not start blank and an ordered list
## must start at 1.
parse_list_marker : Utf8.Bytes, Bool -> Try(ListMarker, [NotFound])
parse_list_marker = |rest, interrupts_paragraph| {
	first = rest.first() ?? 0
	parsed =
		if first == '-' or first == '+' or first == '*' {
			Ok({ info: { ordered: False, marker: first, start: 1 }, width: 1 })
		} else {
			digits = count_leading_digits(rest)
			delimiter = rest.get(digits) ?? 0
			if digits >= 1 and digits <= 9 and (delimiter == '.' or delimiter == ')') {
				Ok({ info: { ordered: True, marker: delimiter, start: digits_to_u64(take_n(rest, digits)) }, width: digits + 1 })
			} else {
				Err(NotFound)
			}
		}
	marker = parsed?
	after = drop_n(rest, marker.width)
	next = after.first() ?? ' '
	if !is_space_or_tab(next) {
		Err(NotFound)
	} else if interrupts_paragraph and (bytes_are_blank(after) or (marker.info.ordered and marker.info.start != 1)) {
		Err(NotFound)
	} else {
		Ok(marker)
	}
}

count_leading_digits : Utf8.Bytes -> U64
count_leading_digits = |bytes| {
	var $count = 0
	while is_digit_byte(bytes.get($count) ?? 'x') {
		$count = $count + 1
	}
	$count
}

## ATX heading (CommonMark 4.2): 1-6 `#` then a space, tab, or end of line.
parse_atx_heading : Utf8.Bytes -> Try(Markdown, [NotFound])
parse_atx_heading = |rest| {
	hashes = count_leading_byte(rest, '#', 0)
	after = drop_n(rest, hashes)
	level = heading_level_from_count(hashes)?
	if !after.is_empty() and !is_space_or_tab(after.first() ?? 0) {
		Err(NotFound)
	} else {
		content = trim_spaces(after)
		closing = count_trailing_byte(content, '#')
		without_closing =
			if closing == content.len() {
				[]
			} else if closing > 0 and is_space_or_tab(content.get(content.len() - closing - 1) ?? 0) {
				trim_end_spaces(drop_last_n(content, closing))
			} else {
				content
			}
		Ok(Heading({ level, content: placeholder(without_closing) }))
	}
}

count_trailing_byte : Utf8.Bytes, U8 -> U64
count_trailing_byte = |bytes, expected| {
	var $count = 0
	while $count < bytes.len() and (bytes.get(bytes.len() - 1 - $count) ?? 0) == expected {
		$count = $count + 1
	}
	$count
}

## Setext underline (CommonMark 4.3): `=`s or `-`s, then only spaces or tabs.
setext_level : Utf8.Bytes -> Try(Markdown.Level, [NotFound])
setext_level = |rest| {
	marker = rest.first() ?? 0
	run = count_leading_byte(rest, marker, 0)
	if run == 0 or !bytes_are_blank(drop_n(rest, run)) {
		Err(NotFound)
	} else if marker == '=' {
		Ok(One)
	} else if marker == '-' {
		Ok(Two)
	} else {
		Err(NotFound)
	}
}

## Thematic break (CommonMark 4.1): three or more matching `-`, `_` or `*`,
## optionally separated by spaces or tabs.
is_thematic_break : Utf8.Bytes -> Bool
is_thematic_break = |rest| {
	marker = rest.first() ?? 0
	if marker != '-' and marker != '_' and marker != '*' {
		False
	} else {
		rest.all(|byte| byte == marker or is_space_or_tab(byte)) and rest.count_if(|byte| byte == marker) >= 3
	}
}

## Opening code fence (CommonMark 4.5). A backtick fence's info string may not
## contain backticks. Backslash escapes and entity references in the info
## string are resolved.
parse_fence_open : Utf8.Bytes -> Try({ fence_char : U8, fence_len : U64, fence_offset : U64, info : Str }, [NotFound])
parse_fence_open = |rest| {
	fence_char = rest.first() ?? 0
	fence_len = count_leading_byte(rest, fence_char, 0)
	info = trim_spaces(drop_n(rest, fence_len))
	if fence_len < 3 or (fence_char == '`' and has_any(info, backtick)) {
		Err(NotFound)
	} else {
		Ok({ fence_char, fence_len, fence_offset: 0, info: unescape_link_text(info) })
	}
}

is_closing_fence : Utf8.Bytes, U8, U64 -> Bool
is_closing_fence = |rest, fence_char, fence_len| {
	run = count_leading_byte(rest, fence_char, 0)
	run >= fence_len and bytes_are_blank(drop_n(rest, run))
}

## --- HTML blocks (CommonMark 4.6) -------------------------------------------

html_type_1_tags : List(Str)
html_type_1_tags = ["pre", "script", "style", "textarea"]

html_type_6_tags : List(Str)
html_type_6_tags = [
	"address",
	"article",
	"aside",
	"base",
	"basefont",
	"blockquote",
	"body",
	"caption",
	"center",
	"col",
	"colgroup",
	"dd",
	"details",
	"dialog",
	"dir",
	"div",
	"dl",
	"dt",
	"fieldset",
	"figcaption",
	"figure",
	"footer",
	"form",
	"frame",
	"frameset",
	"h1",
	"h2",
	"h3",
	"h4",
	"h5",
	"h6",
	"head",
	"header",
	"hr",
	"html",
	"iframe",
	"legend",
	"li",
	"link",
	"main",
	"menu",
	"menuitem",
	"nav",
	"noframes",
	"ol",
	"optgroup",
	"option",
	"p",
	"param",
	"search",
	"section",
	"summary",
	"table",
	"tbody",
	"td",
	"tfoot",
	"th",
	"thead",
	"title",
	"tr",
	"track",
	"ul",
]

lowercase_ascii : Utf8.Bytes -> Utf8.Bytes
lowercase_ascii = |bytes| bytes.map(|byte| if byte >= 'A' and byte <= 'Z' byte + 32 else byte)

html_block_start : Utf8.Bytes, Bool -> Try(U64, [NotFound])
html_block_start = |rest, allow_type_7| {
	lower = lowercase_ascii(rest)
	tag_end = |name_len| {
		next = rest.get(name_len) ?? ' '
		next == ' ' or next == '\t' or next == '>'
	}
	if html_type_1_tags.any(|tag| lower.starts_with("<${tag}".to_utf8()) and tag_end(tag.to_utf8().len() + 1)) {
		Ok(1)
	} else if lower.starts_with("<!--".to_utf8()) {
		Ok(2)
	} else if lower.starts_with("<?".to_utf8()) {
		Ok(3)
	} else if lower.starts_with("<![cdata[".to_utf8()) {
		Ok(5)
	} else if lower.starts_with("<!".to_utf8()) and is_alphabetic_byte(rest.get(2) ?? 0) {
		Ok(4)
	} else if is_type_6_start(lower) {
		Ok(6)
	} else if allow_type_7 and is_type_7_start(rest) {
		Ok(7)
	} else {
		Err(NotFound)
	}
}

is_type_6_start : Utf8.Bytes -> Bool
is_type_6_start = |lower| {
	name_start = if lower.starts_with("</".to_utf8()) 2 else 1
	name = take_tag_name(drop_n(lower, name_start))
	after = drop_n(lower, name_start + name.len())
	next = after.first() ?? ' '
	html_type_6_tags.any(|tag| tag.to_utf8() == name)
		and (next == ' ' or next == '\t' or next == '>' or after.starts_with("/>".to_utf8()))
}

take_tag_name : Utf8.Bytes -> Utf8.Bytes
take_tag_name = |bytes| {
	match bytes.first() {
		Ok(first) if is_alphabetic_byte(first) => {
			var $len = 1
			while is_tag_name_byte(bytes.get($len) ?? ' ') {
				$len = $len + 1
			}
			take_n(bytes, $len)
		}

		_ => []
	}
}

is_tag_name_byte : U8 -> Bool
is_tag_name_byte = |byte| is_alphabetic_byte(byte) or is_digit_byte(byte) or byte == '-'

## Type 7: a complete closing tag, or a complete open tag other than the
## type 1 tags (`<pre>` and friends open type 1 blocks instead), alone on the
## line.
is_type_7_start : Utf8.Bytes -> Bool
is_type_7_start = |rest| {
	end =
		if rest.starts_with("</".to_utf8()) {
			scan_closing_tag(rest, 2)
		} else {
			name = lowercase_ascii(take_tag_name(drop_n(rest, 1)))
			if html_type_1_tags.any(|tag| tag.to_utf8() == name) Err(NotFound) else scan_open_tag(rest, 1)
		}
	match end {
		Ok(len) => bytes_are_blank(drop_n(rest, len))
		Err(_) => False
	}
}

html_block_ends : U64, Utf8.Bytes -> Bool
html_block_ends = |html_type, rest| {
	lower = lowercase_ascii(rest)
	match html_type {
		1 => ["</pre>", "</script>", "</style>", "</textarea>"].any(|end| contains_bytes(lower, end.to_utf8()))
		2 => contains_bytes(rest, "-->".to_utf8())
		3 => contains_bytes(rest, "?>".to_utf8())
		4 => has_any(rest, greater_than)
		5 => contains_bytes(rest, "]]>".to_utf8())
		_ => False
	}
}

contains_bytes : Utf8.Bytes, Utf8.Bytes -> Bool
contains_bytes = |haystack, needle| {
	var $index = 0
	var $found = False
	while !$found and $index + needle.len() <= haystack.len() {
		if haystack.sublist({ start: $index, len: needle.len() }) == needle {
			$found = True
		}
		$index = $index + 1
	}
	$found
}

## --- GFM tables -----------------------------------------------------------

## Cells of a table row: an optional leading and trailing pipe, cells split on
## unescaped pipes (even inside code spans), `\|` unescaped to `|`, and each
## cell trimmed. A row needs at least one cell.
split_table_row : Utf8.Bytes -> List(Utf8.Bytes)
split_table_row = |line| {
	trimmed = trim_spaces(line)
	body = if trimmed.first() == Ok('|') drop_n(trimmed, 1) else trimmed
	len = body.len()
	# Cells are slices of the line; only a cell with `\|` is copied.
	var $cells = []
	var $start = 0
	var $escaped = False
	var $index = Utf8.find_any(body, 0, pipe_or_backslash)
	while $index < len {
		if byte_at(body, $index) == '\\' {
			escapes_pipe = byte_at(body, $index + 1) == '|'
			$escaped = $escaped or escapes_pipe
			$index = Utf8.find_any(body, $index + (if escapes_pipe 2 else 1), pipe_or_backslash)
		} else {
			$cells = $cells.append(table_cell(body, $start, $index, $escaped))
			$start = $index + 1
			$escaped = False
			$index = Utf8.find_any(body, $start, pipe_or_backslash)
		}
	}
	if $start < len and !bytes_are_blank(body.sublist({ start: $start, len: len - $start })) {
		$cells.append(table_cell(body, $start, len, $escaped))
	} else {
		$cells
	}
}

pipe_or_backslash : Utf8.ByteClass
pipe_or_backslash = Utf8.ByteClass.from_bytes(['|', '\\'])

## One trimmed cell, with `\|` unescaped to `|`.
table_cell : Utf8.Bytes, U64, U64, Bool -> Utf8.Bytes
table_cell = |body, start, end, escaped| {
	raw = body.sublist({ start, len: end - start })
	if escaped {
		var $out = List.with_capacity(raw.len())
		var $index = 0
		while $index < raw.len() {
			byte = byte_at(raw, $index)
			if byte == '\\' and byte_at(raw, $index + 1) == '|' {
				$out = $out.append('|')
				$index = $index + 2
			} else {
				$out = $out.append(byte)
				$index = $index + 1
			}
		}
		trim_spaces($out)
	} else {
		trim_spaces(raw)
	}
}

## Delimiter row: cells of `:?-+:?` surrounded by optional spaces or tabs.
parse_table_delimiter_row : Utf8.Bytes -> Try(List(Markdown.Alignment), [NotFound])
parse_table_delimiter_row = |rest| {
	# Most lines fail on their first byte, so test the bytes before the `-`.
	if rest.any(|byte| !(byte == '-' or byte == ':' or byte == '|' or is_space_or_tab(byte))) or !has_any(rest, dash) {
		return Err(NotFound)
	}
	cells = split_table_row(rest)
	if cells.is_empty() {
		Err(NotFound)
	} else {
		var $align = []
		for cell in cells {
			$align = $align.append(parse_alignment_cell(cell)?)
		}
		Ok($align)
	}
}

parse_alignment_cell : Utf8.Bytes -> Try(Markdown.Alignment, [NotFound])
parse_alignment_cell = |cell| {
	left = cell.first() == Ok(':')
	right = cell.len() > 1 and cell.last() == Ok(':')
	dashes = drop_last_n(drop_n(cell, if left 1 else 0), if right 1 else 0)
	if dashes.is_empty() or dashes.any(|byte| byte != '-') {
		Err(NotFound)
	} else if left and right {
		Ok(Center)
	} else if left {
		Ok(Left)
	} else if right {
		Ok(Right)
	} else {
		Ok(Default)
	}
}

## --- Link reference definitions (CommonMark 4.7) ----------------------------

## Remove the link reference definitions that begin a paragraph's raw content,
## registering each (the first definition of a label wins).
extract_reference_definitions : Utf8.Bytes, List(ReferenceDefinition) -> { rest : Utf8.Bytes, refs : List(ReferenceDefinition) }
extract_reference_definitions = |content, refs| {
	var $rest = content
	var $refs = refs
	var $scanning = True
	while $scanning {
		match parse_reference_definition($rest) {
			Ok(found) => {
				$refs = if $refs.any(|ref| ref.label == found.def.label) $refs else $refs.append(found.def)
				$rest = drop_n($rest, found.consumed)
			}

			Err(_) => {
				$scanning = False
			}
		}
	}
	{ rest: $rest, refs: $refs }
}

## Spaces or tabs, then at most one line ending, then spaces or tabs.
skip_spnl : Utf8.Bytes, U64 -> U64
skip_spnl = |bytes, start| {
	first = skip_spaces_tabs(bytes, start)
	if (bytes.get(first) ?? 0) == '\n' skip_spaces_tabs(bytes, first + 1) else first
}

## One definition at the start of `bytes` (CommonMark 4.7), using the inline
## parser's label, destination and title scanners. It may span lines but must
## end at a line ending; a title followed by anything else is dropped and the
## definition ends after its destination if that ends its line.
parse_reference_definition : Utf8.Bytes -> Try({ def : ReferenceDefinition, consumed : U64 }, [NotFound])
parse_reference_definition = |bytes| {
	label = scan_link_label(bytes, 0)?
	if label.raw.is_empty() or (bytes.get(label.end) ?? 0) != ':' {
		return Err(NotFound)
	}
	dest_start = skip_spnl(bytes, label.end + 1)
	destination = scan_link_destination(bytes, dest_start)?
	pointy = (bytes.get(dest_start) ?? 0) == '<'
	if destination.end == dest_start or (!pointy and destination.raw.is_empty()) {
		return Err(NotFound)
	}
	before_title = destination.end
	title_start = skip_spnl(bytes, before_title)
	title =
		if title_start > before_title {
			match scan_link_title(bytes, title_start) {
				Ok(found) if at_line_end(bytes, skip_spaces_tabs(bytes, found.end)) => Ok(found)
				_ => Err(NotFound)
			}
		} else {
			Err(NotFound)
		}
	{ end, title_value } =
		match title {
			Ok(found) => { end: skip_spaces_tabs(bytes, found.end), title_value: Ok(unescape_link_text(found.raw)) }
			Err(_) => { end: skip_spaces_tabs(bytes, before_title), title_value: Err(Missing) }
		}
	if !at_line_end(bytes, end) {
		return Err(NotFound)
	}
	consumed = if end < bytes.len() end + 1 else end
	href = unescape_link_text(trim_cmark_space(destination.raw))
	Ok({ def: { label: normalize_reference_label(label.raw), target: { href, title: title_value } }, consumed })
}

at_line_end : Utf8.Bytes, U64 -> Bool
at_line_end = |bytes, index| index >= bytes.len() or (bytes.get(index) ?? 0) == '\n'

## --- Inline phase ---------------------------------------------------------

## Replace the raw text placeholders left by the block phase with parsed
## inline content, now that every reference definition is known.
resolve_inlines : Markdown, List(ReferenceDefinition) -> Markdown
resolve_inlines = |block, refs| {
	inline = |content| parse_inlines_with_refs(refs, placeholder_raw(content))
	match block {
		Heading({ level, content }) => Heading({ level, content: inline(content) })
		Paragraph(content) => Paragraph(inline(content))
		Blockquote(children) => Blockquote(children.map(|child| resolve_inlines(child, refs)))
		ListBlock({ kind, loose, items }) =>
			ListBlock({ kind, loose, items: items.map(|item| { task: item.task, blocks: item.blocks.map(|child| resolve_inlines(child, refs)) }) })
		Table({ header, align, rows }) =>
			Table({ header: header.map(inline), align, rows: rows.map(|row| row.map(inline)) })
		other => other
	}
}

placeholder_raw : List(Markdown.Inline) -> Utf8.Bytes
placeholder_raw = |content| {
	match content {
		[Text(raw)] => raw.to_utf8()
		_ => []
	}
}

heading_level_from_count : U64 -> Try(Markdown.Level, [NotFound])
heading_level_from_count = |count| {
	match count {
		1 => Ok(One)
		2 => Ok(Two)
		3 => Ok(Three)
		4 => Ok(Four)
		5 => Ok(Five)
		6 => Ok(Six)
		_ => Err(NotFound)
	}
}

## ---------------------------------------------------------------------------
## Inline parsing.
##
## Implements CommonMark 0.31.2 section 6 (backslash escapes, entity and numeric
## character references, code spans, emphasis via the delimiter-run algorithm
## of appendix "Process emphasis", links and images including reference links,
## autolinks, raw HTML, hard and soft line breaks) plus the GFM strikethrough
## and extended-autolink extensions as implemented by cmark-gfm.
##
## Soft line breaks are kept as "\n" inside `Text`; adjacent text is merged.
## ---------------------------------------------------------------------------

## One piece of scanned inline content before emphasis is resolved.
InlineItem : [
	Chars(List(U8)),
	Node(Markdown.Inline),
	# A resolved link or image and its depth (see `max_inline_nesting`).
	Nested(Markdown.Inline, U64),
	Delim(InlineDelim),
]

## Emphasis, strikethrough, links and images nest at most this deep (a text
## node has depth 0). Deeper delimiters and brackets are read as text, so
## that the tree stays shallow enough for recursive code (including
## `Str.inspect`, equality and the GFM email autolink pass) to walk it
## without overflowing the stack. Like `max_nesting` for containers.
max_inline_nesting : U64
max_inline_nesting = 1000

## A run of `*`, `_` or `~` that may open or close emphasis or strikethrough.
## `length` is the original run length (used by the "multiple of 3" rule);
## `count` is how many delimiter characters are still unused.
InlineDelim : { char : U8, length : U64, count : U64, can_open : Bool, can_close : Bool }

## An entry of the bracket stack: the `[` or `![` item, the input offset just
## after the bracket, and its push sequence number (a later bracket was pushed
## when the push counter has moved on by more than one), and the greatest
## depth of the links and images resolved after it.
InlineBracket : { item : U64, start : U64, image : Bool, sequence : U64, depth : U64 }

## A maximal run of backticks in the input.
TickRun : { start : U64, len : U64 }

parse_inline_bytes : Utf8.Bytes -> List(Markdown.Inline)
parse_inline_bytes = |input| {
	parse_inlines_with_refs([], input)
}

parse_inlines_with_refs : List(ReferenceDefinition), Utf8.Bytes -> List(Markdown.Inline)
parse_inlines_with_refs = |refs, input| {
	scan_inlines(prepare_inline_input(input), refs)
}

## Normalize paragraph content: replace U+0000 with U+FFFD, strip the leading
## spaces and tabs of every line, drop leading blank lines and strip trailing
## whitespace.
prepare_inline_input : List(U8) -> List(U8)
prepare_inline_input = |input| {
	len = input.len()
	var $out = List.with_capacity(len)
	var $pos = 0
	while $pos < len {
		# Each step starts a line: skip its indentation, then copy up to and
		# including its line ending in one piece.
		start = Utf8.skip_class(input, $pos, line_spaces)
		if $out.is_empty() and is_line_ending(byte_at(input, start)) and start < len {
			# Leading blank lines are dropped.
			$pos = start + 1
		} else {
			line_end = Utf8.find_line_end(input, start)
			end = if line_end < len line_end + 1 else len
			$out = append_without_nul($out, input.sublist({ start, len: end - start }))
			$pos = end
		}
	}
	trim_trailing_whitespace($out)
}

is_line_ending : U8 -> Bool
is_line_ending = |byte| byte == '\n' or byte == '\r'

## `List.contains` on bytes is several times slower than a ByteClass scan.
has_any : List(U8), Utf8.ByteClass -> Bool
has_any = |bytes, class| Utf8.find_any(bytes, 0, class) < bytes.len()

dash : Utf8.ByteClass
dash = Utf8.ByteClass.from_bytes(['-'])

greater_than : Utf8.ByteClass
greater_than = Utf8.ByteClass.from_bytes(['>'])

nul : Utf8.ByteClass
nul = Utf8.ByteClass.from_bytes([0])

## Append bytes with U+0000 replaced by U+FFFD.
append_without_nul : List(U8), List(U8) -> List(U8)
append_without_nul = |out, piece| {
	if Utf8.find_any(piece, 0, nul) < piece.len() {
		piece.fold(out, |acc, b| if b == 0 acc.concat([0xEF, 0xBF, 0xBD]) else acc.append(b))
	} else {
		out.concat(piece)
	}
}

trim_trailing_whitespace : List(U8) -> List(U8)
trim_trailing_whitespace = |bytes| {
	var $len = bytes.len()
	while $len > 0 and is_cmark_space(byte_at(bytes, $len - 1)) {
		$len = $len - 1
	}
	take_n(bytes, $len)
}

## Trailing spaces and tabs before a line ending are not part of the text.
## (Vertical tab and form feed are ordinary characters in CommonMark.)
trim_trailing_line_space : List(U8) -> List(U8)
trim_trailing_line_space = |bytes| {
	var $len = bytes.len()
	while $len > 0 and is_line_space(byte_at(bytes, $len - 1)) {
		$len = $len - 1
	}
	take_n(bytes, $len)
}

is_line_space : U8 -> Bool
is_line_space = |byte| byte == ' ' or byte == '\t'

## Spaces, tabs and line endings (cmark's `cmark_isspace`).
is_cmark_space : U8 -> Bool
is_cmark_space = |byte| byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r'

byte_at : List(U8), U64 -> U8
byte_at = |bytes, index| bytes.get(index) ?? 0

## The main inline scanner.
scan_inlines : List(U8), List(ReferenceDefinition) -> List(Markdown.Inline)
scan_inlines = |input, refs| {
	len = input.len()
	ticks = backtick_runs(input)
	var $pos = 0
	var $items = []
	var $text = []
	var $brackets = []
	var $bracket_pushes = 0
	# Non-image brackets below this stack index are inactive (CommonMark 6.3:
	# links may not contain other links).
	var $link_floor = 0
	# Once a scan for the end of a comment, processing instruction, CDATA
	# section or declaration fails, later scans would fail too (cmark does the
	# same to avoid quadratic behaviour).
	var $html_skip = { comment: False, pi: False, cdata: False, declaration: False }

	while $pos < len {
		byte = byte_at(input, $pos)

		if byte == '\n' or byte == '\r' {
			hard = $pos >= 2 and byte_at(input, $pos - 1) == ' ' and byte_at(input, $pos - 2) == ' '
			$text = trim_trailing_line_space($text)
			if hard {
				$items = flush_chars($items, $text).append(Node(HardBreak))
				$text = []
			} else {
				$text = $text.append('\n')
			}
			$pos = skip_spaces_tabs(input, skip_line_ending(input, $pos))
		} else if byte == '\\' {
			next = byte_at(input, $pos + 1)
			if $pos + 1 < len and is_ascii_punctuation(next) {
				$items = flush_chars($items, $text).append(Chars([next]))
				$text = []
				$pos = $pos + 2
			} else if $pos + 1 < len and (next == '\n' or next == '\r') {
				$items = flush_chars($items, $text).append(Node(HardBreak))
				$text = []
				$pos = skip_spaces_tabs(input, skip_line_ending(input, $pos + 1))
			} else {
				$text = $text.append('\\')
				$pos = $pos + 1
			}
		} else if byte == '`' {
			run_end = skip_byte_run(input, $pos, '`')
			count = run_end - $pos
			match find_closing_ticks(ticks, run_end, count) {
				Ok(close) => {
					content = normalize_code_span(input.sublist({ start: run_end, len: close - run_end }))
					$items = flush_chars($items, $text).append(Node(InlineCode(bytes_to_str(content))))
					$text = []
					$pos = close + count
				}

				Err(_) => {
					$text = $text.concat(List.repeat('`', count))
					$pos = run_end
				}
			}
		} else if byte == '*' or byte == '_' or byte == '~' {
			run_end = skip_byte_run(input, $pos, byte)
			count = run_end - $pos
			delim = scan_delimiter_run(input, $pos, run_end, byte)
			$items = flush_chars($items, $text)
			$text = []
			$items =
				match delim {
					Ok(d) => $items.append(Delim(d))
					Err(_) => $items.append(Chars(List.repeat(byte, count)))
				}
			$pos = run_end
		} else if byte == '!' and byte_at(input, $pos + 1) == '[' {
			$items = flush_chars($items, $text).append(Chars(['!', '[']))
			$text = []
			$brackets = $brackets.append({ item: $items.len() - 1, start: $pos + 2, image: True, sequence: $bracket_pushes, depth: 0 })
			$bracket_pushes = $bracket_pushes + 1
			$pos = $pos + 2
		} else if byte == '[' {
			$items = flush_chars($items, $text).append(Chars(['[']))
			$text = []
			$brackets = $brackets.append({ item: $items.len() - 1, start: $pos + 1, image: False, sequence: $bracket_pushes, depth: 0 })
			$bracket_pushes = $bracket_pushes + 1
			$pos = $pos + 1
		} else if byte == ']' {
			$items = flush_chars($items, $text)
			$text = []
			after = $pos + 1
			match $brackets.last() {
				Err(_) => {
					$items = $items.append(Chars([']']))
					$pos = after
				}

				Ok(opener) => {
					depth = $brackets.len() - 1
					active = opener.image or depth >= $link_floor
					$brackets = drop_last_n($brackets, 1)
					$link_floor = min_u64($link_floor, $brackets.len())
					# A link whose content is already as deep as allowed stays text.
					resolved =
						if active and opener.depth < max_inline_nesting {
							resolve_link(input, after, opener, $bracket_pushes > opener.sequence + 1, $pos, refs)
						} else {
							Err(NotFound)
						}
					match resolved {
						Ok(found) => {
							# Consume the content slice before truncating, so the
							# item list stays uniquely owned and is not copied.
							content = process_emphasis(drop_n($items, opener.item + 1), max_inline_nesting - 1)
							$items = take_n($items, opener.item)
							node =
								if opener.image {
									Image({ alt: content.nodes, target: found.target })
								} else {
									Link({ label: content.nodes, target: found.target })
								}
							if !opener.image {
								$link_floor = $brackets.len()
							}
							$brackets = raise_bracket_depth($brackets, content.depth + 1)
							$items = $items.append(Nested(node, content.depth + 1))
							$pos = found.end
						}

						Err(_) => {
							$brackets = raise_bracket_depth($brackets, opener.depth)
							$items = $items.append(Chars([']']))
							$pos = after
						}
					}
				}
			}
		} else if byte == '<' {
			match scan_autolink(input, $pos) {
				Ok(found) => {
					$items = flush_chars($items, $text).append(Nested(found.node, 1))
					$text = []
					$pos = found.end
				}

				Err(_) => {
					html = scan_raw_html(input, $pos, $html_skip)
					$html_skip = html.skip
					match html.end {
						Ok(end) => {
							raw = input.sublist({ start: $pos, len: end - $pos })
							$items = flush_chars($items, $text).append(Node(HtmlInline(bytes_to_str(raw))))
							$text = []
							$pos = end
						}

						Err(_) => {
							$text = $text.append('<')
							$pos = $pos + 1
						}
					}
				}
			}
		} else if byte == '&' {
			match scan_entity(input, $pos) {
				Ok(found) => {
					$items = flush_chars($items, $text).append(Chars(found.bytes))
					$text = []
					$pos = found.end
				}

				Err(_) => {
					$text = $text.append('&')
					$pos = $pos + 1
				}
			}
		} else if byte == 'w' and $brackets.is_empty() {
			match match_www_autolink(input, $pos) {
				Ok(end) => {
					raw = input.sublist({ start: $pos, len: end - $pos })
					label = bytes_to_str(raw)
					node = Link({ label: [Text(label)], target: { href: Str.concat("http://", label), title: Err(Missing) } })
					$items = flush_chars($items, $text).append(Nested(node, 1))
					$text = []
					$pos = end
				}

				Err(_) => {
					$text = $text.append('w')
					$pos = $pos + 1
				}
			}
		} else if byte == ':' and $brackets.is_empty() {
			match match_url_autolink(input, $pos, $text.len()) {
				Ok(found) => {
					raw = input.sublist({ start: $pos - found.rewind, len: found.end - ($pos - found.rewind) })
					url = bytes_to_str(raw)
					node = Link({ label: [Text(url)], target: { href: url, title: Err(Missing) } })
					$items = flush_chars($items, drop_last_n($text, found.rewind)).append(Nested(node, 1))
					$text = []
					$pos = found.end
				}

				Err(_) => {
					$text = $text.append(':')
					$pos = $pos + 1
				}
			}
		} else {
			end = find_special_byte(input, $pos + 1)
			$text = $text.concat(input.sublist({ start: $pos, len: end - $pos }))
			$pos = end
		}
	}

	nodes = process_emphasis(flush_chars($items, $text), max_inline_nesting).nodes
	# GFM email autolinks need an `@` in the text.
	if Utf8.find_any(input, 0, at_sign) < len autolink_emails_in(nodes) else nodes
}

at_sign : Utf8.ByteClass
at_sign = Utf8.ByteClass.from_bytes(['@'])

## Bytes that may start an inline construct (cmark's SPECIAL_CHARS plus the
## extension triggers `~`, `w` and `:`).
is_special_byte : U8 -> Bool
is_special_byte = |byte| {
	match byte {
		'\n' | '\r' | '\\' | '`' | '*' | '_' | '~' | '!' | '[' | ']' | '<' | '&' | 'w' | ':' => True
		_ => False
	}
}

special_bytes : Utf8.ByteClass
special_bytes = Utf8.ByteClass.from_predicate(is_special_byte)

find_special_byte : List(U8), U64 -> U64
find_special_byte = |input, from| Utf8.find_any(input, from, special_bytes)

flush_chars : List(InlineItem), List(U8) -> List(InlineItem)
flush_chars = |items, text| {
	if text.is_empty() {
		items
	} else {
		items.append(Chars(text))
	}
}

skip_byte_run : List(U8), U64, U8 -> U64
skip_byte_run = |input, from, expected| {
	var $index = from
	while $index < input.len() and byte_at(input, $index) == expected {
		$index = $index + 1
	}
	$index
}

skip_spaces_tabs : List(U8), U64 -> U64
skip_spaces_tabs = |input, from| {
	var $index = from
	while $index < input.len() and (byte_at(input, $index) == ' ' or byte_at(input, $index) == '\t') {
		$index = $index + 1
	}
	$index
}

## Skip one line ending (`\n`, `\r\n` or `\r`) at `from`.
skip_line_ending : List(U8), U64 -> U64
skip_line_ending = |input, from| {
	if byte_at(input, from) == '\r' and byte_at(input, from + 1) == '\n' {
		from + 2
	} else if from < input.len() and (byte_at(input, from) == '\n' or byte_at(input, from) == '\r') {
		from + 1
	} else {
		from
	}
}

is_ascii_punctuation : U8 -> Bool
is_ascii_punctuation = |byte| {
	(byte >= 33 and byte <= 47) or (byte >= 58 and byte <= 64) or (byte >= 91 and byte <= 96) or (byte >= 123 and byte <= 126)
}

min_u64 : U64, U64 -> U64
min_u64 = |a, b| if a < b a else b

is_ascii_alpha : U8 -> Bool
is_ascii_alpha = |byte| (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z')

is_ascii_alnum : U8 -> Bool
is_ascii_alnum = |byte| is_ascii_alpha(byte) or (byte >= '0' and byte <= '9')

## ---------------------------------------------------------------------------
## Unicode classification (CommonMark 0.31.2 section 2.1).
## ---------------------------------------------------------------------------

## Decode the UTF-8 scalar starting at `index`; invalid sequences decode to
## U+FFFD. Returns the scalar and its byte length.
decode_utf8 : List(U8), U64 -> { scalar : U32, width : U64 }
decode_utf8 = |bytes, index| {
	b0 = byte_at(bytes, index).to_u32()
	invalid = { scalar: 0xFFFD, width: 1 }
	if index >= bytes.len() {
		invalid
	} else if b0 < 0x80 {
		{ scalar: b0, width: 1 }
	} else {
		width =
			if b0 >= 0xC2 and b0 <= 0xDF {
				2
			} else if b0 >= 0xE0 and b0 <= 0xEF {
				3
			} else if b0 >= 0xF0 and b0 <= 0xF4 {
				4
			} else {
				0
			}
		if width == 0 or index + width > bytes.len() {
			invalid
		} else {
			var $value = if width == 2 b0 - 0xC0 else if width == 3 b0 - 0xE0 else b0 - 0xF0
			var $ok = True
			var $offset = 1
			while $offset < width {
				continuation = byte_at(bytes, index + $offset).to_u32()
				if continuation < 0x80 or continuation > 0xBF {
					$ok = False
				}
				$value = $value * 64 + (continuation % 64)
				$offset = $offset + 1
			}
			minimum = if width == 2 0x80 else if width == 3 0x800 else 0x10000
			if !$ok or $value < minimum or $value > 0x10FFFF or ($value >= 0xD800 and $value <= 0xDFFF) {
				invalid
			} else {
				{ scalar: $value, width }
			}
		}
	}
}

## The scalar that ends just before `index`, or a line feed at the start.
scalar_before : List(U8), U64 -> U32
scalar_before = |bytes, index| {
	if index == 0 {
		'\n'
	} else {
		var $start = index - 1
		while $start > 0 and index - $start < 4 and byte_at(bytes, $start) >= 0x80 and byte_at(bytes, $start) <= 0xBF {
			$start = $start - 1
		}
		decoded = decode_utf8(bytes, $start)
		if $start + decoded.width == index {
			decoded.scalar
		} else {
			0xFFFD
		}
	}
}

## The scalar at `index`, or a line feed at the end of input.
scalar_at : List(U8), U64 -> U32
scalar_at = |bytes, index| {
	if index >= bytes.len() {
		'\n'
	} else {
		decode_utf8(bytes, index).scalar
	}
}

general_category : U32 -> Try(GeneralCategory.Value, [InvalidScalar])
general_category = |scalar| {
	Scalar.from_u32(scalar).map_ok(GeneralCategory.of_scalar)
}

## Unicode whitespace: Zs, tab, line feed, form feed or carriage return.
is_unicode_whitespace : U32 -> Bool
is_unicode_whitespace = |scalar| {
	if scalar < 128 {
		scalar == ' ' or scalar == '\t' or scalar == '\n' or scalar == 0x0C or scalar == '\r'
	} else {
		match general_category(scalar) {
			Ok(Zs) => True
			_ => False
		}
	}
}

## Unicode punctuation: general category P or S (CommonMark 0.31.2).
is_unicode_punctuation : U32 -> Bool
is_unicode_punctuation = |scalar| {
	if scalar < 128 {
		is_ascii_punctuation(scalar.to_u8_wrap())
	} else {
		match general_category(scalar) {
			Ok(Pc) | Ok(Pd) | Ok(Pe) | Ok(Pf) | Ok(Pi) | Ok(Po) | Ok(Ps) => True
			Ok(Sc) | Ok(Sk) | Ok(Sm) | Ok(So) => True
			_ => False
		}
	}
}

## ---------------------------------------------------------------------------
## Delimiter runs and emphasis (CommonMark 6.2 and appendix A).
## ---------------------------------------------------------------------------

scan_delimiter_run : List(U8), U64, U64, U8 -> Try(InlineDelim, [NotDelimiter])
scan_delimiter_run = |input, start, end, char| {
	before = scalar_before(input, start)
	after = scalar_at(input, end)
	before_space = is_unicode_whitespace(before)
	after_space = is_unicode_whitespace(after)
	before_punct = is_unicode_punctuation(before)
	after_punct = is_unicode_punctuation(after)
	left = !after_space and (!after_punct or before_space or before_punct)
	right = !before_space and (!before_punct or after_space or after_punct)
	length = end - start
	{ can_open, can_close, eligible } =
		if char == '_' {
			o = left and (!right or before_punct)
			c = right and (!left or after_punct)
			{ can_open: o, can_close: c, eligible: o or c }
		} else if char == '~' {
			# GFM strikethrough: runs of one or two tildes that flank text.
			{ can_open: left, can_close: right, eligible: (left or right) and (length == 1 or length == 2) }
		} else {
			{ can_open: left, can_close: right, eligible: left or right }
		}
	if eligible {
		Ok({ char, length, count: length, can_open, can_close })
	} else {
		Err(NotDelimiter)
	}
}

## Index into the `openers_bottom` table: delimiter character, whether the
## closer can also open, and the closer's original length modulo 3.
opener_bottom_key : InlineDelim -> U64
opener_bottom_key = |delim| {
	base =
		if delim.char == '*' {
			0
		} else if delim.char == '_' {
			6
		} else {
			12
		}
	base + (if delim.can_open 3 else 0) + delim.length % 3
}

EmphOpener : { delim : InlineDelim, at : U64 }

## Resolve emphasis and strikethrough in a run of scanned items. Unmatched
## delimiters become text. Each potential opener on `stack` owns an empty
## placeholder `Text` in `out` (at index `at`); its remaining delimiter
## characters are written there only when it leaves the stack unmatched, so
## partially used runs are not re-rendered on every match.
## Record that the innermost open bracket's content holds a node this deep.
raise_bracket_depth : List(InlineBracket), U64 -> List(InlineBracket)
raise_bracket_depth = |brackets, depth| {
	match brackets.last() {
		Ok(top) if top.depth < depth => brackets.set(brackets.len() - 1, { ..top, depth }) ?? brackets
		_ => brackets
	}
}

## Resolve emphasis and strikethrough, creating no node deeper than `cap`
## (the delimiters of a deeper pair stay text). Returns the nodes and the
## greatest depth among them; `$depths` runs parallel to `$out`.
process_emphasis : List(InlineItem), U64 -> { nodes : List(Markdown.Inline), depth : U64 }
process_emphasis = |items, cap| {
	var $out = []
	var $depths = []
	# Openers before this index of `$out` enclose a pair found too deep, so
	# they are too deep as well (the ranges only grow).
	var $deep_end = 0
	var $stack = []
	var $bottoms = List.repeat(0, 18)
	for item in items {
		match item {
			Chars(bytes) => {
				$out = $out.append(Text(bytes_to_str(bytes)))
				$depths = $depths.append(0)
			}

			Node(node) => {
				$out = $out.append(node)
				$depths = $depths.append(0)
			}

			Nested(node, depth) => {
				$out = $out.append(node)
				$depths = $depths.append(depth)
			}

			Delim(delim) => {
				var $closer = delim
				var $searching = delim.can_close
				while $searching {
					match find_opener($stack, $bottoms, $closer) {
						Err(_) => {
							$bottoms = $bottoms.set(opener_bottom_key($closer), $stack.len()) ?? $bottoms
							$searching = False
						}

						Ok(index) => {
							opener = $stack.get(index) ?? { delim: $closer, at: $out.len() }
							# Delimiters between the opener and the closer become text.
							$out = fill_placeholders($out, drop_n($stack, index + 1))
							$bottoms = $bottoms.map(|bottom| min_u64(bottom, index))
							$stack = take_n($stack, index)
							depth =
								if opener.at < $deep_end {
									cap + 1
								} else {
									1 + max_depth(drop_n($depths, opener.at + 1))
								}
							if depth > cap {
								$deep_end = opener.at + 1
								# Too deep: the opener stays text and the closer
								# looks further down the stack.
								$out = fill_placeholders($out, [opener])
								$searching = $closer.char != '~'
							} else if $closer.char == '~' {
								if opener.delim.count == $closer.count {
									children = merge_text_nodes(drop_n($out, opener.at + 1))
									$out = take_n($out, opener.at).append(Strikethrough(children))
									$depths = take_n($depths, opener.at).append(depth)
									$closer = { ..$closer, count: 0 }
								} else {
									# cmark-gfm: tilde runs of different lengths do not
									# pair; both, and the delimiters between them, stay text.
									$out = fill_placeholders($out, [opener])
									$closer = { ..$closer, can_open: False }
								}
								$searching = False
							} else {
								used = if $closer.count >= 2 and opener.delim.count >= 2 2 else 1
								children = merge_text_nodes(drop_n($out, opener.at + 1))
								node = if used == 2 Strong(children) else Emphasis(children)
								remaining = opener.delim.count - used
								if remaining == 0 {
									$out = take_n($out, opener.at).append(node)
									$depths = take_n($depths, opener.at).append(depth)
								} else {
									$out = take_n($out, opener.at).append(Text("")).append(node)
									$depths = take_n($depths, opener.at).append(0).append(depth)
									$stack = $stack.append({ delim: { ..opener.delim, count: remaining }, at: opener.at })
								}
								$closer = { ..$closer, count: $closer.count - used }
								$searching = $closer.count > 0
							}
						}
					}
				}
				if $closer.count > 0 {
					if $closer.can_open {
						$stack = $stack.append({ delim: $closer, at: $out.len() })
						$out = $out.append(Text(""))
					} else {
						$out = $out.append(delimiter_text($closer.char, $closer.count))
					}
					$depths = $depths.append(0)
				}
			}
		}
	}
	{ nodes: merge_text_nodes(fill_placeholders($out, $stack)), depth: max_depth($depths) }
}

max_depth : List(U64) -> U64
max_depth = |depths| depths.fold(0, |a, b| if a > b a else b)

## Write the remaining delimiter characters of openers leaving the stack.
fill_placeholders : List(Markdown.Inline), List(EmphOpener) -> List(Markdown.Inline)
fill_placeholders = |out, openers| {
	var $out = out
	for opener in openers {
		$out =
			match $out.set(opener.at, delimiter_text(opener.delim.char, opener.delim.count)) {
				Ok(updated) => updated
				Err(_) => []
			}
	}
	$out
}

delimiter_text : U8, U64 -> Markdown.Inline
delimiter_text = |char, count| Text(bytes_to_str(List.repeat(char, count)))

find_opener : List(EmphOpener), List(U64), InlineDelim -> Try(U64, [NotFound])
find_opener = |stack, bottoms, closer| {
	bottom = bottoms.get(opener_bottom_key(closer)) ?? 0
	var $index = stack.len()
	var $found = Err(NotFound)
	while $index > bottom {
		$index = $index - 1
		match stack.get($index) {
			Ok(entry) => {
				opener = entry.delim
				if opener.can_open and opener.char == closer.char {
					odd_match = (closer.can_open or opener.can_close) and closer.length % 3 != 0 and (opener.length + closer.length) % 3 == 0
					if !odd_match {
						$found = Ok($index)
						$index = bottom
					}
				}
			}

			Err(_) => {}
		}
	}
	$found
}

## Merge adjacent `Text` nodes and drop empty ones.
merge_text_nodes : List(Markdown.Inline) -> List(Markdown.Inline)
merge_text_nodes = |nodes| {
	var $out = List.with_capacity(nodes.len())
	# Adjacent text is gathered here, so a long run of text nodes (as when
	# delimiters nested too deeply stay text) is joined in linear time.
	var $pending = ""
	for node in nodes {
		match node {
			Text(text) => {
				$pending = if $pending.is_empty() text else Str.concat($pending, text)
			}

			_ => {
				if !$pending.is_empty() {
					$out = $out.append(Text($pending))
					$pending = ""
				}
				$out = $out.append(node)
			}
		}
	}
	if $pending.is_empty() $out else $out.append(Text($pending))
}

## ---------------------------------------------------------------------------
## Code spans (CommonMark 6.1).
## ---------------------------------------------------------------------------

backtick : Utf8.ByteClass
backtick = Utf8.ByteClass.from_bytes(['`'])

## All maximal backtick runs, sorted by length and then position.
backtick_runs : List(U8) -> List(TickRun)
backtick_runs = |input| {
	var $runs = []
	var $index = Utf8.find_any(input, 0, backtick)
	while $index < input.len() {
		end = skip_byte_run(input, $index, '`')
		$runs = $runs.append({ start: $index, len: end - $index })
		$index = Utf8.find_any(input, end, backtick)
	}
	if $runs.len() < 2 {
		return $runs
	}
	$runs.sort_with(
		|a, b| {
			if a.len < b.len {
				Before
			} else if a.len > b.len {
				After
			} else if a.start < b.start {
				Before
			} else if a.start > b.start {
				After
			} else {
				Same
			}
		},
	)
}

## The start of the first maximal backtick run of exactly `count` backticks
## that starts at or after `from`.
find_closing_ticks : List(TickRun), U64, U64 -> Try(U64, [NotFound])
find_closing_ticks = |runs, from, count| {
	# Binary search for the first run not ordered before (count, from).
	var $low = 0
	var $high = runs.len()
	while $low < $high {
		middle = ($low + $high) // 2
		run = runs.get(middle) ?? { start: 0, len: 0 }
		if run.len < count or (run.len == count and run.start < from) {
			$low = middle + 1
		} else {
			$high = middle
		}
	}
	match runs.get($low) {
		Ok(run) if run.len == count => Ok(run.start)
		_ => Err(NotFound)
	}
}

## Line endings become spaces; one leading and one trailing space are removed
## when both are present and the content is not only spaces.
normalize_code_span : List(U8) -> List(U8)
normalize_code_span = |content| {
	if Utf8.find_line_end(content, 0) >= content.len() {
		return strip_code_span_spaces(content)
	}
	var $out = List.with_capacity(content.len())
	var $index = 0
	while $index < content.len() {
		byte = byte_at(content, $index)
		if byte == '\r' and byte_at(content, $index + 1) == '\n' {
			{}
		} else if byte == '\r' or byte == '\n' {
			$out = $out.append(' ')
		} else {
			$out = $out.append(byte)
		}
		$index = $index + 1
	}
	strip_code_span_spaces($out)
}

strip_code_span_spaces : List(U8) -> List(U8)
strip_code_span_spaces = |out| {
	if out.len() >= 2 and byte_at(out, 0) == ' ' and byte_at(out, out.len() - 1) == ' ' and out.any(|b| b != ' ') {
		out.sublist({ start: 1, len: out.len() - 2 })
	} else {
		out
	}
}

## ---------------------------------------------------------------------------
## Entity and numeric character references (CommonMark 6.2... section 2.5).
## ---------------------------------------------------------------------------

scan_entity : List(U8), U64 -> Try({ bytes : List(U8), end : U64 }, [NotFound])
scan_entity = |input, start| {
	if byte_at(input, start + 1) == '#' {
		hex = byte_at(input, start + 2) == 'x' or byte_at(input, start + 2) == 'X'
		digits_start = if hex start + 3 else start + 2
		max_digits = if hex 6 else 7
		var $index = digits_start
		var $value = 0
		while $index < input.len() and $index - digits_start < max_digits and is_reference_digit(byte_at(input, $index), hex) {
			$value = $value * (if hex 16 else 10) + digit_value(byte_at(input, $index))
			$index = $index + 1
		}
		if $index > digits_start and byte_at(input, $index) == ';' {
			Ok({ bytes: encode_utf8(sanitize_scalar($value)), end: $index + 1 })
		} else {
			Err(NotFound)
		}
	} else {
		name_start = start + 1
		var $index = name_start
		while $index < input.len() and $index - name_start < 32 and is_ascii_alnum(byte_at(input, $index)) {
			$index = $index + 1
		}
		if $index > name_start and byte_at(input, $index) == ';' {
			name = input.sublist({ start: name_start, len: $index - name_start })
			match MarkdownEntities.lookup(name) {
				Ok(value) => Ok({ bytes: value, end: $index + 1 })
				Err(_) => Err(NotFound)
			}
		} else {
			Err(NotFound)
		}
	}
}

is_reference_digit : U8, Bool -> Bool
is_reference_digit = |byte, hex| {
	(byte >= '0' and byte <= '9') or (hex and ((byte >= 'a' and byte <= 'f') or (byte >= 'A' and byte <= 'F')))
}

digit_value : U8 -> U32
digit_value = |byte| {
	if byte >= '0' and byte <= '9' {
		(byte - '0').to_u32()
	} else if byte >= 'a' and byte <= 'f' {
		(byte - 'a').to_u32() + 10
	} else {
		(byte - 'A').to_u32() + 10
	}
}

## U+0000, surrogates and out-of-range values become U+FFFD.
sanitize_scalar : U32 -> U32
sanitize_scalar = |value| {
	if value == 0 or (value >= 0xD800 and value <= 0xDFFF) or value > 0x10FFFF {
		0xFFFD
	} else {
		value
	}
}

encode_utf8 : U32 -> List(U8)
encode_utf8 = |scalar| {
	if scalar < 0x80 {
		[scalar.to_u8_wrap()]
	} else if scalar < 0x800 {
		[(0xC0 + scalar // 64).to_u8_wrap(), (0x80 + scalar % 64).to_u8_wrap()]
	} else if scalar < 0x10000 {
		[(0xE0 + scalar // 4096).to_u8_wrap(), (0x80 + (scalar // 64) % 64).to_u8_wrap(), (0x80 + scalar % 64).to_u8_wrap()]
	} else {
		[(0xF0 + scalar // 262144).to_u8_wrap(), (0x80 + (scalar // 4096) % 64).to_u8_wrap(), (0x80 + (scalar // 64) % 64).to_u8_wrap(), (0x80 + scalar % 64).to_u8_wrap()]
	}
}

## Decode backslash escapes and entity references in a link destination or
## title, in a single left-to-right pass.
unescape_link_text : List(U8) -> Str
unescape_link_text = |bytes| {
	var $out = List.with_capacity(bytes.len())
	var $index = 0
	while $index < bytes.len() {
		byte = byte_at(bytes, $index)
		if byte == '\\' and is_ascii_punctuation(byte_at(bytes, $index + 1)) and $index + 1 < bytes.len() {
			$out = $out.append(byte_at(bytes, $index + 1))
			$index = $index + 2
		} else if byte == '&' {
			match scan_entity(bytes, $index) {
				Ok(found) => {
					$out = $out.concat(found.bytes)
					$index = found.end
				}

				Err(_) => {
					$out = $out.append('&')
					$index = $index + 1
				}
			}
		} else {
			$out = $out.append(byte)
			$index = $index + 1
		}
	}
	bytes_to_str($out)
}

## Decode entity references only (autolinks keep backslashes literally).
unescape_entities : List(U8) -> Str
unescape_entities = |bytes| {
	var $out = List.with_capacity(bytes.len())
	var $index = 0
	while $index < bytes.len() {
		byte = byte_at(bytes, $index)
		if byte == '&' {
			match scan_entity(bytes, $index) {
				Ok(found) => {
					$out = $out.concat(found.bytes)
					$index = found.end
				}

				Err(_) => {
					$out = $out.append('&')
					$index = $index + 1
				}
			}
		} else {
			$out = $out.append(byte)
			$index = $index + 1
		}
	}
	bytes_to_str($out)
}

## ---------------------------------------------------------------------------
## Links and images (CommonMark 6.3 and 6.4).
## ---------------------------------------------------------------------------

## Decide whether the `]` at `close` (with `after` just past it) closes a link
## or image: an inline link, then a full, collapsed or shortcut reference.
## `bracket_after` tells whether another bracket was opened after `opener`,
## in which case its text cannot serve as a (collapsed or shortcut) label.
resolve_link : List(U8), U64, InlineBracket, Bool, U64, List(ReferenceDefinition) -> Try({ target : Markdown.LinkTarget, end : U64 }, [NotFound])
resolve_link = |input, after, opener, bracket_after, close, refs| {
	inline =
		if byte_at(input, after) == '(' {
			scan_inline_link_tail(input, after)
		} else {
			Err(NotFound)
		}
	match inline {
		Ok(found) =>
			Ok(found)

		Err(_) => {
			if refs.is_empty() {
				Err(NotFound)
			} else {
				own_label = input.sublist({ start: opener.start, len: close - opener.start })
				candidate =
					match scan_link_label(input, after) {
						Ok(label) if !label.raw.is_empty() => Ok({ label: label.raw, end: label.end })
						Ok(label) if !bracket_after => Ok({ label: own_label, end: label.end })
						Ok(_) => Err(NotFound)
						Err(_) if !bracket_after => Ok({ label: own_label, end: after })
						Err(_) => Err(NotFound)
					}
				match candidate {
					Ok(found) => {
						target = lookup_reference(refs, found.label)?
						Ok({ target, end: found.end })
					}

					Err(_) =>
						Err(NotFound)
				}
			}
		}
	}
}

lookup_reference : List(ReferenceDefinition), List(U8) -> Try(Markdown.LinkTarget, [NotFound])
lookup_reference = |refs, raw_label| {
	if raw_label.is_empty() or raw_label.len() > 1000 {
		Err(NotFound)
	} else {
		label = normalize_reference_label(raw_label)
		if label.is_empty() {
			Err(NotFound)
		} else {
			match refs.find_first(|ref| ref.label == label) {
				Ok(ref) => Ok(ref.target)
				Err(_) => Err(NotFound)
			}
		}
	}
}

## A link label `[...]` starting at `start`: no unescaped brackets, at most
## 1000 bytes. The returned label is trimmed.
scan_link_label : List(U8), U64 -> Try({ raw : List(U8), end : U64 }, [NotFound])
scan_link_label = |input, start| {
	if byte_at(input, start) != '[' {
		Err(NotFound)
	} else {
		var $index = start + 1
		var $result = Err(NotFound)
		var $scanning = True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == ']' {
				raw = input.sublist({ start: start + 1, len: $index - start - 1 })
				$result = Ok({ raw: trim_cmark_space(raw), end: $index + 1 })
				$scanning = False
			} else if byte == '[' {
				$scanning = False
			} else {
				$index =
					if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
						$index + 2
					} else {
						$index + 1
					}
				if $index - start - 1 > 1000 {
					$scanning = False
				}
			}
		}
		$result
	}
}

trim_cmark_space : List(U8) -> List(U8)
trim_cmark_space = |bytes| {
	var $start = 0
	while $start < bytes.len() and is_cmark_space(byte_at(bytes, $start)) {
		$start = $start + 1
	}
	var $end = bytes.len()
	while $end > $start and is_cmark_space(byte_at(bytes, $end - 1)) {
		$end = $end - 1
	}
	bytes.sublist({ start: $start, len: $end - $start })
}

## Skip spaces and tabs with at most one line ending among them.
skip_link_space : List(U8), U64 -> U64
skip_link_space = |input, from| {
	first = skip_spaces_tabs(input, from)
	after_line = skip_line_ending(input, first)
	if after_line == first {
		first
	} else {
		skip_spaces_tabs(input, after_line)
	}
}

## `(destination "title")` after the `]` of an inline link, starting at `(`.
scan_inline_link_tail : List(U8), U64 -> Try({ target : Markdown.LinkTarget, end : U64 }, [NotFound])
scan_inline_link_tail = |input, open| {
	dest_start = skip_link_space(input, open + 1)
	dest = scan_link_destination(input, dest_start)?
	title_start = skip_link_space(input, dest.end)
	title =
		if title_start == dest.end {
			Err(NotFound)
		} else {
			scan_link_title(input, title_start)
		}
	{ title_value, title_end } =
		match title {
			Ok(found) => { title_value: Ok(unescape_link_text(found.raw)), title_end: found.end }
			Err(_) => { title_value: Err(Missing), title_end: dest.end }
		}
	close = skip_link_space(input, title_end)
	if byte_at(input, close) == ')' and close < input.len() {
		Ok({ target: { href: unescape_link_text(trim_cmark_space(dest.raw)), title: title_value }, end: close + 1 })
	} else {
		Err(NotFound)
	}
}

## A link destination: `<...>` without line endings or unescaped `<`/`>`, or a
## non-empty run without spaces or ASCII controls whose parentheses balance.
scan_link_destination : List(U8), U64 -> Try({ raw : List(U8), end : U64 }, [NotFound])
scan_link_destination = |input, start| {
	if byte_at(input, start) == '<' {
		var $index = start + 1
		var $result = Err(NotFound)
		var $scanning = True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '>' {
				$result = Ok({ raw: input.sublist({ start: start + 1, len: $index - start - 1 }), end: $index + 1 })
				$scanning = False
			} else if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == '\n' or byte == '\r' or byte == '<' {
				$scanning = False
			} else {
				$index = $index + 1
			}
		}
		$result
	} else {
		var $index = start
		var $depth = 0
		var $failed = False
		var $scanning = True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == '(' {
				$depth = $depth + 1
				$index = $index + 1
				if $depth > 32 {
					$failed = True
					$scanning = False
				}
			} else if byte == ')' {
				if $depth == 0 {
					$scanning = False
				} else {
					$depth = $depth - 1
					$index = $index + 1
				}
			} else if byte <= ' ' or byte == 0x7F {
				$scanning = False
			} else {
				$index = $index + 1
			}
		}
		if $failed or $depth != 0 {
			Err(NotFound)
		} else {
			Ok({ raw: input.sublist({ start, len: $index - start }), end: $index })
		}
	}
}

## A link title in `"..."`, `'...'` or `(...)`; returns the content between the
## delimiters.
scan_link_title : List(U8), U64 -> Try({ raw : List(U8), end : U64 }, [NotFound])
scan_link_title = |input, start| {
	open = byte_at(input, start)
	close =
		if open == '"' {
			'"'
		} else if open == '\'' {
			'\''
		} else if open == '(' {
			')'
		} else {
			0
		}
	if close == 0 or start >= input.len() {
		Err(NotFound)
	} else {
		var $index = start + 1
		var $result = Err(NotFound)
		var $scanning = True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == close {
				$result = Ok({ raw: input.sublist({ start: start + 1, len: $index - start - 1 }), end: $index + 1 })
				$scanning = False
			} else if open == '(' and byte == '(' {
				$scanning = False
			} else {
				$index = $index + 1
			}
		}
		$result
	}
}

## ---------------------------------------------------------------------------
## Autolinks (CommonMark 6.5) and raw HTML (CommonMark 6.6).
## ---------------------------------------------------------------------------

scan_autolink : List(U8), U64 -> Try({ node : Markdown.Inline, end : U64 }, [NotFound])
scan_autolink = |input, start| {
	match scan_autolink_uri(input, start + 1) {
		Ok(end) => {
			raw = input.sublist({ start: start + 1, len: end - start - 2 })
			text = unescape_entities(raw)
			Ok({ node: Link({ label: [Text(text)], target: { href: text, title: Err(Missing) } }), end })
		}

		Err(_) => {
			end = scan_autolink_email(input, start + 1)?
			raw = input.sublist({ start: start + 1, len: end - start - 2 })
			text = unescape_entities(raw)
			Ok({ node: Link({ label: [Text(text)], target: { href: Str.concat("mailto:", text), title: Err(Missing) } }), end })
		}
	}
}

## `scheme:rest>` where scheme is 2-32 characters. Returns the offset after `>`.
scan_autolink_uri : List(U8), U64 -> Try(U64, [NotFound])
scan_autolink_uri = |input, start| {
	if !is_ascii_alpha(byte_at(input, start)) {
		Err(NotFound)
	} else {
		var $index = start + 1
		while $index < input.len() and $index - start < 33 and is_scheme_byte(byte_at(input, $index)) {
			$index = $index + 1
		}
		scheme_len = $index - start
		if scheme_len < 2 or scheme_len > 32 or byte_at(input, $index) != ':' {
			Err(NotFound)
		} else {
			var $rest = $index + 1
			while $rest < input.len() and is_uri_byte(byte_at(input, $rest)) {
				$rest = $rest + 1
			}
			if byte_at(input, $rest) == '>' and $rest < input.len() {
				Ok($rest + 1)
			} else {
				Err(NotFound)
			}
		}
	}
}

is_scheme_byte : U8 -> Bool
is_scheme_byte = |byte| is_ascii_alnum(byte) or byte == '+' or byte == '.' or byte == '-'

is_uri_byte : U8 -> Bool
is_uri_byte = |byte| byte > ' ' and byte != '<' and byte != '>' and byte != 0x7F

## `local@domain>` following the HTML5 email grammar used by CommonMark.
scan_autolink_email : List(U8), U64 -> Try(U64, [NotFound])
scan_autolink_email = |input, start| {
	var $index = start
	while $index < input.len() and is_email_local_byte(byte_at(input, $index)) {
		$index = $index + 1
	}
	if $index == start or byte_at(input, $index) != '@' {
		Err(NotFound)
	} else {
		var $cursor = $index + 1
		var $result = Err(NotFound)
		var $scanning = True
		while $scanning {
			label_start = $cursor
			while $cursor < input.len() and (is_ascii_alnum(byte_at(input, $cursor)) or byte_at(input, $cursor) == '-') {
				$cursor = $cursor + 1
			}
			label_len = $cursor - label_start
			valid_label = label_len >= 1 and label_len <= 63 and is_ascii_alnum(byte_at(input, label_start)) and is_ascii_alnum(byte_at(input, $cursor - 1))
			if !valid_label {
				$scanning = False
			} else if byte_at(input, $cursor) == '.' and $cursor < input.len() {
				$cursor = $cursor + 1
			} else if byte_at(input, $cursor) == '>' and $cursor < input.len() {
				$result = Ok($cursor + 1)
				$scanning = False
			} else {
				$scanning = False
			}
		}
		$result
	}
}

is_email_local_byte : U8 -> Bool
is_email_local_byte = |byte| {
	if is_ascii_alnum(byte) {
		True
	} else {
		match byte {
			'.' | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '/' | '=' | '?' | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '-' => True
			_ => False
		}
	}
}

HtmlSkip : { comment : Bool, pi : Bool, cdata : Bool, declaration : Bool }

## Raw HTML starting at the `<` at `start`; returns the offset just after it.
scan_raw_html : List(U8), U64, HtmlSkip -> { end : Try(U64, [NotFound]), skip : HtmlSkip }
scan_raw_html = |input, start, skip| {
	second = byte_at(input, start + 1)
	if second == '!' {
		if byte_at(input, start + 2) == '-' and byte_at(input, start + 3) == '-' {
			if byte_at(input, start + 4) == '>' {
				{ end: Ok(start + 5), skip }
			} else if byte_at(input, start + 4) == '-' and byte_at(input, start + 5) == '>' {
				{ end: Ok(start + 6), skip }
			} else if skip.comment {
				{ end: Err(NotFound), skip }
			} else {
				match find_bytes(input, start + 4, "-->".to_utf8()) {
					Ok(found) => { end: Ok(found + 3), skip }
					Err(_) => { end: Err(NotFound), skip: { ..skip, comment: True } }
				}
			}
		} else if starts_with_at(input, start + 2, "[CDATA[".to_utf8()) {
			if skip.cdata {
				{ end: Err(NotFound), skip }
			} else {
				match find_bytes(input, start + 9, "]]>".to_utf8()) {
					Ok(found) => { end: Ok(found + 3), skip }
					Err(_) => { end: Err(NotFound), skip: { ..skip, cdata: True } }
				}
			}
		} else if is_ascii_alpha(byte_at(input, start + 2)) {
			if skip.declaration {
				{ end: Err(NotFound), skip }
			} else {
				match find_bytes(input, start + 3, ">".to_utf8()) {
					Ok(found) => { end: Ok(found + 1), skip }
					Err(_) => { end: Err(NotFound), skip: { ..skip, declaration: True } }
				}
			}
		} else {
			{ end: Err(NotFound), skip }
		}
	} else if second == '?' {
		if skip.pi {
			{ end: Err(NotFound), skip }
		} else {
			match find_bytes(input, start + 2, "?>".to_utf8()) {
				Ok(found) => { end: Ok(found + 2), skip }
				Err(_) => { end: Err(NotFound), skip: { ..skip, pi: True } }
			}
		}
	} else if second == '/' {
		{ end: scan_closing_tag(input, start + 2), skip }
	} else {
		{ end: scan_open_tag(input, start + 1), skip }
	}
}

starts_with_at : List(U8), U64, List(U8) -> Bool
starts_with_at = |input, start, prefix| {
	input.sublist({ start, len: prefix.len() }) == prefix
}

find_bytes : List(U8), U64, List(U8) -> Try(U64, [NotFound])
find_bytes = |input, from, needle| {
	first = byte_at(needle, 0)
	var $index = from
	var $result = Err(NotFound)
	while $index + needle.len() <= input.len() {
		if byte_at(input, $index) == first and starts_with_at(input, $index, needle) {
			$result = Ok($index)
			$index = input.len()
		} else {
			$index = $index + 1
		}
	}
	$result
}

## Tag name: an ASCII letter followed by letters, digits or `-`.
scan_tag_name : List(U8), U64 -> Try(U64, [NotFound])
scan_tag_name = |input, start| {
	if !is_ascii_alpha(byte_at(input, start)) or start >= input.len() {
		Err(NotFound)
	} else {
		var $index = start + 1
		while $index < input.len() and (is_ascii_alnum(byte_at(input, $index)) or byte_at(input, $index) == '-') {
			$index = $index + 1
		}
		Ok($index)
	}
}

## HTML whitespace: spaces, tabs and at most one line ending.
skip_html_space : List(U8), U64 -> U64
skip_html_space = |input, from| {
	skip_link_space(input, from)
}

scan_closing_tag : List(U8), U64 -> Try(U64, [NotFound])
scan_closing_tag = |input, start| {
	after_name = scan_tag_name(input, start)?
	close = skip_html_space(input, after_name)
	if byte_at(input, close) == '>' and close < input.len() {
		Ok(close + 1)
	} else {
		Err(NotFound)
	}
}

scan_open_tag : List(U8), U64 -> Try(U64, [NotFound])
scan_open_tag = |input, start| {
	var $index = scan_tag_name(input, start)?
	var $result = Err(NotFound)
	var $scanning = True
	while $scanning {
		after_space = skip_html_space(input, $index)
		next = byte_at(input, after_space)
		if next == '>' and after_space < input.len() {
			$result = Ok(after_space + 1)
			$scanning = False
		} else if next == '/' and byte_at(input, after_space + 1) == '>' {
			$result = Ok(after_space + 2)
			$scanning = False
		} else if after_space > $index and is_attribute_name_start(next) {
			match scan_attribute(input, after_space) {
				Ok(end) => {
					$index = end
				}
				Err(_) => {
					$scanning = False
				}
			}
		} else {
			$scanning = False
		}
	}
	$result
}

is_attribute_name_start : U8 -> Bool
is_attribute_name_start = |byte| is_ascii_alpha(byte) or byte == '_' or byte == ':'

is_attribute_name_byte : U8 -> Bool
is_attribute_name_byte = |byte| is_ascii_alnum(byte) or byte == '_' or byte == '.' or byte == ':' or byte == '-'

## An attribute name with an optional value specification.
scan_attribute : List(U8), U64 -> Try(U64, [NotFound])
scan_attribute = |input, start| {
	var $name_end = start + 1
	while $name_end < input.len() and is_attribute_name_byte(byte_at(input, $name_end)) {
		$name_end = $name_end + 1
	}
	equals = skip_html_space(input, $name_end)
	if byte_at(input, equals) != '=' {
		Ok($name_end)
	} else {
		value_start = skip_html_space(input, equals + 1)
		quote = byte_at(input, value_start)
		if quote == '"' or quote == '\'' {
			match find_bytes(input, value_start + 1, [quote]) {
				Ok(close) => Ok(close + 1)
				Err(_) => Err(NotFound)
			}
		} else {
			var $index = value_start
			while $index < input.len() and is_unquoted_attribute_byte(byte_at(input, $index)) {
				$index = $index + 1
			}
			if $index == value_start {
				Err(NotFound)
			} else {
				Ok($index)
			}
		}
	}
}

is_unquoted_attribute_byte : U8 -> Bool
is_unquoted_attribute_byte = |byte| {
	match byte {
		' ' | '\t' | '\n' | '\r' | '"' | '\'' | '=' | '<' | '>' | '`' => False
		_ => True
	}
}

## ---------------------------------------------------------------------------
## GFM extended autolinks (cmark-gfm extensions/autolink.c).
## ---------------------------------------------------------------------------

## cmark-gfm host characters: not whitespace and not Unicode punctuation
## (general category P) nor ASCII punctuation.
is_gfm_host_byte_at : List(U8), U64 -> Bool
is_gfm_host_byte_at = |input, index| {
	byte = byte_at(input, index)
	if byte >= 0x80 and byte <= 0xBF {
		# A continuation byte does not start a valid scalar.
		False
	} else {
		decoded = decode_utf8(input, index)
		if index >= input.len() or (decoded.scalar == 0xFFFD and decoded.width == 1) {
			False
		} else if decoded.scalar < 128 {
			!is_unicode_whitespace(decoded.scalar) and !is_ascii_punctuation(byte)
		} else if is_unicode_whitespace(decoded.scalar) {
			False
		} else {
			match general_category(decoded.scalar) {
				Ok(Pc) | Ok(Pd) | Ok(Pe) | Ok(Pf) | Ok(Pi) | Ok(Po) | Ok(Ps) => False
				_ => True
			}
		}
	}
}

## cmark-gfm `check_domain`: returns the offset (relative to `start`) where
## the domain scan stopped, or 0 when the domain is rejected.
gfm_check_domain : List(U8), U64, U64, Bool -> U64
gfm_check_domain = |input, start, size, allow_short| {
	var $index = 1
	var $periods = 0
	var $underscores_previous = 0
	var $underscores = 0
	var $scanning = True
	while $scanning and $index + 1 < size {
		if byte_at(input, start + $index) == '\\' and $index + 2 < size {
			$index = $index + 1
		}
		byte = byte_at(input, start + $index)
		if byte == '_' {
			$underscores = $underscores + 1
		} else if byte == '.' {
			$underscores_previous = $underscores
			$underscores = 0
			$periods = $periods + 1
		} else if !is_gfm_host_byte_at(input, start + $index) and byte != '-' {
			$scanning = False
		}
		if $scanning {
			$index = $index + 1
		}
	}
	if ($underscores_previous > 0 or $underscores > 0) and $periods <= 10 {
		0
	} else if allow_short {
		$index
	} else if $periods > 0 {
		$index
	} else {
		0
	}
}

## cmark-gfm `autolink_delim`: trim trailing punctuation, unbalanced `)` and
## entity-like suffixes from a candidate link `input[start .. start + end]`.
gfm_autolink_delim : List(U8), U64, U64 -> U64
gfm_autolink_delim = |input, start, link_end| {
	var $end = link_end
	var $opening = 0
	var $closing = 0
	var $index = 0
	while $index < $end {
		byte = byte_at(input, start + $index)
		if byte == '<' {
			$end = $index
		} else if byte == '(' {
			$opening = $opening + 1
			$index = $index + 1
		} else if byte == ')' {
			$closing = $closing + 1
			$index = $index + 1
		} else {
			$index = $index + 1
		}
	}
	var $trimming = True
	while $trimming and $end > 0 {
		last = byte_at(input, start + $end - 1)
		if last == ')' {
			if $closing <= $opening {
				$trimming = False
			} else {
				$closing = $closing - 1
				$end = $end - 1
			}
		} else if last == '?' or last == '!' or last == '.' or last == ',' or last == ':' or last == '*' or last == '_' or last == '~' or last == '\'' or last == '"' {
			$end = $end - 1
		} else if last == ';' {
			if $end >= 2 {
				var $new_end = $end - 2
				while $new_end > 0 and is_ascii_alpha(byte_at(input, start + $new_end)) {
					$new_end = $new_end - 1
				}
				if $new_end < $end - 2 and byte_at(input, start + $new_end) == '&' {
					$end = $new_end
				} else {
					$end = $end - 1
				}
			} else {
				$end = $end - 1
			}
		} else {
			$trimming = False
		}
	}
	$end
}

## Extend a candidate link to the next whitespace or `<`.
gfm_extend_link : List(U8), U64, U64 -> U64
gfm_extend_link = |input, start, from| {
	var $end = from
	while start + $end < input.len() and !is_cmark_space(byte_at(input, start + $end)) and byte_at(input, start + $end) != '<' {
		$end = $end + 1
	}
	$end
}

## `www.` autolink at `start`; returns the end offset.
match_www_autolink : List(U8), U64 -> Try(U64, [NotFound])
match_www_autolink = |input, start| {
	before = if start == 0 0 else byte_at(input, start - 1)
	allowed_before = start == 0 or before == '*' or before == '_' or before == '~' or before == '(' or is_cmark_space(before)
	size = input.len() - start
	if !allowed_before or size < 4 or !starts_with_at(input, start, "www.".to_utf8()) {
		Err(NotFound)
	} else {
		domain = gfm_check_domain(input, start, size, False)
		if domain == 0 {
			Err(NotFound)
		} else {
			end = gfm_autolink_delim(input, start, gfm_extend_link(input, start, domain))
			if end == 0 {
				Err(NotFound)
			} else {
				Ok(start + end)
			}
		}
	}
}

## `scheme://` autolink whose `:` is at `colon`; the scheme letters precede it
## and are taken back from the pending text. Returns how many bytes to take
## back and the end offset.
match_url_autolink : List(U8), U64, U64 -> Try({ rewind : U64, end : U64 }, [NotFound])
match_url_autolink = |input, colon, pending| {
	size = input.len() - colon
	if size < 4 or byte_at(input, colon + 1) != '/' or byte_at(input, colon + 2) != '/' {
		Err(NotFound)
	} else {
		var $rewind = 0
		while $rewind < colon and is_ascii_alpha(byte_at(input, colon - $rewind - 1)) {
			$rewind = $rewind + 1
		}
		scheme = input.sublist({ start: colon - $rewind, len: $rewind }).map(lower_ascii_byte)
		known = scheme == "http".to_utf8() or scheme == "https".to_utf8() or scheme == "ftp".to_utf8()
		if !known or $rewind > pending or !is_gfm_host_byte_at(input, colon + 3) {
			Err(NotFound)
		} else {
			domain = gfm_check_domain(input, colon + 3, size - 3, True)
			if domain == 0 {
				Err(NotFound)
			} else {
				end = gfm_autolink_delim(input, colon, gfm_extend_link(input, colon, 3 + domain))
				if end == 0 {
					Err(NotFound)
				} else {
					Ok({ rewind: $rewind, end: colon + end })
				}
			}
		}
	}
}

## Apply GFM email autolinks to every text node outside links, in one walk.
## Text nodes are never adjacent, so the pieces of a split text need no
## merging with their neighbours.
autolink_emails_in : List(Markdown.Inline) -> List(Markdown.Inline)
autolink_emails_in = |nodes| {
	var $out = List.with_capacity(nodes.len())
	for node in nodes {
		match node {
			Text(text) if Str.contains(text, "@") => {
				$out = $out.concat(autolink_email_text(text))
			}

			_ => {
				$out = $out.append(autolink_emails(node))
			}
		}
	}
	$out
}

autolink_emails : Markdown.Inline -> Markdown.Inline
autolink_emails = |node| {
	match node {
		Strong(children) => Strong(autolink_emails_in(children))
		Emphasis(children) => Emphasis(autolink_emails_in(children))
		Strikethrough(children) => Strikethrough(autolink_emails_in(children))
		Image({ alt, target }) => Image({ alt: autolink_emails_in(alt), target })
		_ => node
	}
}

## cmark-gfm `postprocess_text`: split a text node around email autolinks
## (`user@example.com`, `mailto:user@example.com`, `xmpp:user@example.com/r`).
autolink_email_text : Str -> List(Markdown.Inline)
autolink_email_text = |text| {
	data = text.to_utf8()
	var $out = []
	var $start = 0
	var $offset = 0
	var $remaining = data.len()
	var $running = True
	while $running {
		if $offset >= $remaining {
			$running = False
		} else {
			match find_bytes(take_n(data, $start + $remaining), $start + $offset, ['@']) {
				Err(_) => {
					$running = False
				}

				Ok(at) => {
					var $max_rewind = at - ($start + $offset)
					var $auto_mailto = True
					var $is_xmpp = False
					var $periods = 0
					var $rewind = 0
					var $link_end = 0
					var $retry = True
					var $outcome = Skip(0)
					while $retry {
						$retry = False
						at_index = $start + $offset + $max_rewind
						$rewind = 0
						var $rewinding = True
						while $rewinding and $rewind < $max_rewind {
							c = byte_at(data, at_index - $rewind - 1)
							if is_ascii_alnum(c) or c == '.' or c == '+' or c == '-' or c == '_' {
								$rewind = $rewind + 1
							} else if c == ':' and gfm_validate_protocol("mailto:".to_utf8(), data, at_index, $rewind, $max_rewind) {
								$auto_mailto = False
								$rewind = $rewind + 1
							} else if c == ':' and gfm_validate_protocol("xmpp:".to_utf8(), data, at_index, $rewind, $max_rewind) {
								$auto_mailto = False
								$is_xmpp = True
								$rewind = $rewind + 1
							} else {
								$rewinding = False
							}
						}
						if $rewind == 0 {
							$outcome = Skip($max_rewind + 1)
						} else {
							limit = $remaining - $offset - $max_rewind
							$link_end = 1
							var $forward = True
							while $forward and $link_end < limit {
								c = byte_at(data, at_index + $link_end)
								if is_ascii_alnum(c) {
									$link_end = $link_end + 1
								} else if c == '@' {
									$offset = $offset + $max_rewind + 1
									$max_rewind = $link_end - 1
									$retry = True
									$forward = False
								} else if c == '.' and $link_end < limit - 1 and is_ascii_alnum(byte_at(data, at_index + $link_end + 1)) {
									$periods = $periods + 1
									$link_end = $link_end + 1
								} else if c == '/' and $is_xmpp {
									$link_end = $link_end + 1
								} else if c != '-' and c != '_' {
									$forward = False
								} else {
									$link_end = $link_end + 1
								}
							}
							if !$retry {
								last = byte_at(data, at_index + $link_end - 1)
								if $link_end < 2 or $periods == 0 or (!is_ascii_alpha(last) and last != '.') {
									$outcome = Skip($max_rewind + $link_end)
								} else {
									trimmed = gfm_autolink_delim(data, at_index, $link_end)
									if trimmed == 0 {
										$outcome = Skip($max_rewind + 1)
									} else {
										$link_end = trimmed
										$outcome = Found
									}
								}
							}
						}
					}
					match $outcome {
						Skip(amount) => {
							$offset = $offset + amount
						}

						Found => {
							at_index = $start + $offset + $max_rewind
							link_start = at_index - $rewind
							email = bytes_to_str(data.sublist({ start: link_start, len: $link_end + $rewind }))
							href = if $auto_mailto Str.concat("mailto:", email) else email
							before = data.sublist({ start: $start, len: link_start - $start })
							$out = $out.append(Text(bytes_to_str(before))).append(Link({ label: [Text(email)], target: { href, title: Err(Missing) } }))
							consumed = $offset + $max_rewind + $link_end
							$start = $start + consumed
							$remaining = $remaining - consumed
							$offset = 0
						}
					}
				}
			}
		}
	}
	rest = data.sublist({ start: $start, len: $remaining })
	merge_text_nodes($out.append(Text(bytes_to_str(rest))))
}

gfm_validate_protocol : List(U8), List(U8), U64, U64, U64 -> Bool
gfm_validate_protocol = |protocol, data, at_index, rewind, max_rewind| {
	len = protocol.len()
	if len > max_rewind - rewind {
		False
	} else if data.sublist({ start: at_index - rewind - len, len }) != protocol {
		False
	} else if len == max_rewind - rewind {
		True
	} else {
		!is_ascii_alnum(byte_at(data, at_index - rewind - len - 1))
	}
}

bytes_are_blank : Utf8.Bytes -> Bool
bytes_are_blank = |bytes| {
	match bytes {
		[] =>
			True

		[' ', .. as rest] =>
			bytes_are_blank(rest)

		['\t', .. as rest] =>
			bytes_are_blank(rest)

		_ =>
			False
	}
}

append_bytes : Utf8.Bytes, Utf8.Bytes -> Utf8.Bytes
append_bytes = |left, right| left.concat(right)

join_lines_with_newlines : List(Utf8.Bytes) -> Utf8.Bytes
join_lines_with_newlines = |lines| {
	var $total = 0
	for line in lines {
		$total = $total + line.len() + 1
	}
	var $acc = List.with_capacity($total)
	for line in lines {
		$acc = $acc.concat(line).append('\n')
	}
	$acc
}

trim_spaces : Utf8.Bytes -> Utf8.Bytes
trim_spaces = |bytes| {
	trim_end_spaces(trim_start_spaces(bytes))
}

trim_start_spaces : Utf8.Bytes -> Utf8.Bytes
trim_start_spaces = |bytes| {
	len = bytes.len()
	var $start = 0
	while $start < len and is_space_or_tab_at(bytes, $start) {
		$start = $start + 1
	}
	if $start == 0 {
		bytes
	} else {
		bytes.sublist({ start: $start, len: len - $start })
	}
}

trim_end_spaces : Utf8.Bytes -> Utf8.Bytes
trim_end_spaces = |bytes| {
	len = bytes.len()
	var $end = len
	while $end > 0 and is_space_or_tab_at(bytes, $end - 1) {
		$end = $end - 1
	}
	if $end == len {
		bytes
	} else {
		bytes.sublist({ start: 0, len: $end })
	}
}

is_space_or_tab_at : Utf8.Bytes, U64 -> Bool
is_space_or_tab_at = |bytes, index| {
	match bytes.get(index) {
		Ok(b) => b == ' ' or b == '\t'
		Err(_) => False
	}
}

## Link label matching (CommonMark 4.7): Unicode case fold, strip leading and
## trailing whitespace, and collapse internal whitespace runs to one space.
normalize_reference_label : Utf8.Bytes -> Str
normalize_reference_label = |label| {
	source = bytes_to_str(label)
	folded =
		match Case.fold(source, Case.full, Case.unlimited_limits) {
			Ok(result) => Case.result_text(result)
			Err(_) => source
		}
	var $out = []
	var $pending_space = False
	for byte in folded.to_utf8() {
		if is_cmark_space(byte) {
			$pending_space = True
		} else {
			if $pending_space and !$out.is_empty() {
				$out = $out.append(' ')
			}
			$pending_space = False
			$out = $out.append(byte)
		}
	}
	bytes_to_str($out)
}

lower_ascii_byte : U8 -> U8
lower_ascii_byte = |byte| {
	if byte >= 'A' and byte <= 'Z' {
		byte + 32
	} else {
		byte
	}
}

count_leading_byte : Utf8.Bytes, U8, U64 -> U64
count_leading_byte = |bytes, expected, count| {
	match bytes {
		[first, .. as rest] if first == expected =>
			count_leading_byte(rest, expected, count + 1)

		_ =>
			count
	}
}

digits_to_u64 : Utf8.Bytes -> U64
digits_to_u64 = |digits| {
	digits.fold(
		0,
		|sum, digit| {
			sum * 10 + (digit - '0').to_u64()
		},
	)
}

is_digit_byte : U8 -> Bool
is_digit_byte = |byte| {
	byte >= '0' and byte <= '9'
}

is_alphabetic_byte : U8 -> Bool
is_alphabetic_byte = |byte| {
	(byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z')
}

# Small nominal support types expose equality, debug, and string helpers.
expect {
	level : Markdown.Level
	level = Two

	kind : Markdown.ListKind
	kind = Ordered({ start: 3 })

	task : Markdown.TaskState
	task = Checked

	alignment : Markdown.Alignment
	alignment = Right

	actual =
		\\level inspect: ${Str.inspect(level)}
		\\level str: ${level.to_str()}
		\\level eq: ${Str.inspect(level == Two)}
		\\kind inspect: ${Str.inspect(kind)}
		\\kind str: ${kind.to_str()}
		\\kind eq same: ${Str.inspect(kind == Ordered({ start: 3 }))}
		\\kind eq different: ${Str.inspect(kind == Ordered({ start: 4 }))}
		\\task inspect: ${Str.inspect(task)}
		\\task str: ${task.to_str()}
		\\task eq: ${Str.inspect(task == Checked)}
		\\alignment inspect: ${Str.inspect(alignment)}
		\\alignment str: ${alignment.to_str()}
		\\alignment eq: ${Str.inspect(alignment == Right)}

	expected =
		\\level inspect: Two
		\\level str: 2
		\\level eq: True
		\\kind inspect: Ordered({ start: 3 })
		\\kind str: ordered:3
		\\kind eq same: True
		\\kind eq different: False
		\\task inspect: Checked
		\\task str: checked
		\\task eq: True
		\\alignment inspect: Right
		\\alignment str: right
		\\alignment eq: True

	actual == expected
}

# Inline and block AST nodes expose useful inspect strings and structural equality.
expect {
	inline : Markdown.Inline
	inline = Strong([Text("Roc")])

	block : Markdown
	block = Heading({ level: Two, content: [Text("Roc")] })

	actual =
		\\inline inspect: ${Str.inspect(inline)}
		\\inline eq same: ${Str.inspect(inline == Strong([Text("Roc")]))}
		\\inline eq different: ${Str.inspect(inline == Emphasis([Text("Roc")]))}
		\\block inspect: ${Str.inspect(block)}
		\\block eq same: ${Str.inspect(block == Heading({ level: Two, content: [Text("Roc")] }))}
		\\block eq different: ${Str.inspect(block == Paragraph([Text("Roc")]))}

	expected =
		\\inline inspect: Strong([Text("Roc")])
		\\inline eq same: True
		\\inline eq different: False
		\\block inspect: Heading({ level: Two, content: [Text("Roc")] })
		\\block eq same: True
		\\block eq different: False

	actual == expected
}

# Public inline parser parses emphasis, strong, strikethrough, code, and links.
expect {
	actual = Markdown.parse_inlines("Intro with **bold**, *em*, ~~gone~~, `code`, and [a link](https://example.com).")

	actual
		== [
			Text("Intro with "),
			Strong([Text("bold")]),
			Text(", "),
			Emphasis([Text("em")]),
			Text(", "),
			Strikethrough([Text("gone")]),
			Text(", "),
			InlineCode("code"),
			Text(", and "),
			Link({ label: [Text("a link")], target: { href: "https://example.com", title: Err(Missing) } }),
			Text("."),
		]
}

# Escaped inline delimiters parse as literal text.
expect {
	text = "\\*literal\\*, \\_em\\_, \\`code\\`, and \\[a link](target)"

	actual = Markdown.parse_inlines(text)

	actual == [Text("*literal*, _em_, `code`, and [a link](target)")]
}

# Inline images parse inside prose.
expect {
	actual = Markdown.parse_inlines("Logo ![Roc](/roc.png) here")

	actual
		== [
			Text("Logo "),
			Image({ alt: [Text("Roc")], target: { href: "/roc.png", title: Err(Missing) } }),
			Text(" here"),
		]
}

# Autolinks and bare URLs parse as inline links.
expect {
	actual = Markdown.parse_inlines("<https://example.com> and www.example.com")

	actual
		== [
			Link({ label: [Text("https://example.com")], target: { href: "https://example.com", title: Err(Missing) } }),
			Text(" and "),
			Link({ label: [Text("www.example.com")], target: { href: "http://www.example.com", title: Err(Missing) } }),
		]
}

# Hard line breaks parse from trailing spaces and backslash newlines.
expect {
	actual = Markdown.parse_inlines("one  \ntwo\\\nthree")

	actual == [Text("one"), HardBreak, Text("two"), HardBreak, Text("three")]
}

# Raw HTML inline spans are preserved.
expect {
	actual = Markdown.parse_inlines("Hello <span>world</span>")

	actual == [Text("Hello "), HtmlInline("<span>"), Text("world"), HtmlInline("</span>")]
}

inline_test : Str -> List(Markdown.Inline)
inline_test = |text| Markdown.parse_inlines(text)

## Emphasis follows the delimiter-run algorithm (CommonMark 6.2): flanking,
# intraword underscores, the rule of three and nesting.
expect inline_test("*foo bar *") == [Text("*foo bar *")]
expect inline_test("foo_bar_") == [Text("foo_bar_")]
expect inline_test("*foo**bar**baz*") == [Emphasis([Text("foo"), Strong([Text("bar")]), Text("baz")])]
expect inline_test("*foo**bar*") == [Emphasis([Text("foo**bar")])]
expect inline_test("***foo** bar*") == [Emphasis([Strong([Text("foo")]), Text(" bar")])]
expect inline_test("**foo*") == [Text("*"), Emphasis([Text("foo")])]
expect inline_test("foo******bar*********baz") == [Text("foo"), Strong([Strong([Strong([Text("bar")])])]), Text("***baz")]
expect inline_test("*(*foo*)*") == [Emphasis([Text("("), Emphasis([Text("foo")]), Text(")")])]

## Unicode punctuation includes symbols (CommonMark 0.31.2), so a currency sign
# next to `*` makes the run left-flanking only.
expect inline_test("*£*bravo.") == [Text("*£*bravo.")]
expect inline_test("a\u(A0)*b*") == [Text("a\u(A0)"), Emphasis([Text("b")])]

# Code spans take precedence over emphasis and links; backtick runs must match.
expect inline_test("*a `*` b*") == [Emphasis([Text("a "), InlineCode("*"), Text(" b")])]
expect inline_test("``foo`bar``") == [InlineCode("foo`bar")]
expect inline_test("`a``b`` c") == [Text("`a"), InlineCode("b"), Text(" c")]
expect inline_test("` `` `") == [InlineCode("``")]
expect inline_test("`foo\nbar`") == [InlineCode("foo bar")]

# Backslash escapes and entity references decode to text.
expect inline_test("\\*not emphasis\\* \\a") == [Text("*not emphasis* \\a")]
expect inline_test("&copy; &#35; &#X22; &#0; &nosuch;") == [Text("© # \" \u(FFFD) &nosuch;")]
expect inline_test("&amp;ouml;") == [Text("&ouml;")]

# Links: destinations, titles, nesting rules and precedence.
expect inline_test("[a](<b c> \"t\")") == [Link({ label: [Text("a")], target: { href: "b c", title: Ok("t") } })]
expect inline_test("[a](b(c)d)") == [Link({ label: [Text("a")], target: { href: "b(c)d", title: Err(Missing) } })]

## Whitespace around a `<...>` destination is not part of the URL (as in
# cmark and markdown-it, and WHATWG URL parsing); escaped content is kept.
expect inline_test("[a](<  b c  >) [d](< \\) >)") == [Link({ label: [Text("a")], target: { href: "b c", title: Err(Missing) } }), Text(" "), Link({ label: [Text("d")], target: { href: ")", title: Err(Missing) } })]
expect inline_test("[a](/u\\*v &auml;)") == [Text("[a](/u*v ä)")]
expect inline_test("[a](/u\\*v '&auml;')") == [Link({ label: [Text("a")], target: { href: "/u*v", title: Ok("ä") } })]
expect inline_test("[foo [bar](/u)](/v)") == [Text("[foo "), Link({ label: [Text("bar")], target: { href: "/u", title: Err(Missing) } }), Text("](/v)")]
expect inline_test("![a [b](/u)](/v)") == [Image({ alt: [Text("a "), Link({ label: [Text("b")], target: { href: "/u", title: Err(Missing) } })], target: { href: "/v", title: Err(Missing) } })]
expect inline_test("*[foo*](/u)") == [Text("*"), Link({ label: [Text("foo*")], target: { href: "/u", title: Err(Missing) } })]
expect inline_test("[foo`](/u)`") == [Text("[foo"), InlineCode("](/u)")]

# Reference labels match case-insensitively after Unicode case folding.
expect {
	actual = Markdown.parse_str("[ẞ]\n\n[SS]: /url")
	actual == [Paragraph([Link({ label: [Text("ẞ")], target: { href: "/url", title: Err(Missing) } })])]
}

# Reference definitions decode escapes and entities and reject bracketed labels.
expect {
	actual = Markdown.parse_str("[foo]\n\n[foo]: /f&ouml;\\* \"t\\*\"")
	actual == [Paragraph([Link({ label: [Text("foo")], target: { href: "/fö*", title: Ok("t*") } })])]
}

expect {
	actual = Markdown.parse_str("[a[b]\n\n[a[b]: /u")
	actual == [Paragraph([Text("[a[b]")]), Paragraph([Text("[a[b]: /u")])]
}

# Autolinks and raw HTML follow the CommonMark grammars.
expect inline_test("<http://a.b/c?d=1&amp;e> <a@b.co>") == [Link({ label: [Text("http://a.b/c?d=1&e")], target: { href: "http://a.b/c?d=1&e", title: Err(Missing) } }), Text(" "), Link({ label: [Text("a@b.co")], target: { href: "mailto:a@b.co", title: Err(Missing) } })]
expect inline_test("<a href=\"x\" b> <!-- c --> <?p?> <!X y> <![CDATA[z]]> </a >") == [HtmlInline("<a href=\"x\" b>"), Text(" "), HtmlInline("<!-- c -->"), Text(" "), HtmlInline("<?p?>"), Text(" "), HtmlInline("<!X y>"), Text(" "), HtmlInline("<![CDATA[z]]>"), Text(" "), HtmlInline("</a >")]
expect inline_test("<a b='c' d> <1a> <a =b>") == [HtmlInline("<a b='c' d>"), Text(" <1a> <a =b>")]

## Pathological shapes stay linear: many open brackets, deeply nested images
# and long delimiter runs that match two characters at a time.
expect inline_test(Str.repeat("[a", 20000)) == [Text(Str.repeat("[a", 20000))]
expect inline_depth(inline_test("${Str.repeat("![", 5000)}a${Str.repeat("](u)", 5000)}")) == max_inline_nesting
expect inline_depth(inline_test("${Str.repeat("*", 20000)}a${Str.repeat("*", 20000)}")) == max_inline_nesting

## Emphasis, links and images nest at most `max_inline_nesting` deep; deeper
# delimiters and brackets stay text. The email autolink pass and `==` walk
# these trees recursively, which overflowed the stack on Linux for inputs
# like `@![` repeated 8,191 times (fuzz seed "A\n\u001f\u007f").
expect {
	nodes = inline_test("${Str.repeat("*a **a ", 4000)}b@c${Str.repeat(" a** a*", 4000)}")
	inline_depth(nodes) == max_inline_nesting and nodes == nodes
}
expect inline_depth(inline_test("${Str.repeat("@![", 3000)}a${Str.repeat("](u)", 3000)}")) == max_inline_nesting
expect inline_depth(inline_test("${Str.repeat("[*a ", 3000)}${Str.repeat("~b~](u)", 3000)}")) <= max_inline_nesting
expect {
	nodes = inline_test("${Str.repeat("*", 999)}a${Str.repeat("*", 999)}")
	inline_depth(nodes) == 500
}

## The depth of the deepest node (text has depth 0), without recursion.
inline_depth : List(Markdown.Inline) -> U64
inline_depth = |nodes| {
	var $pending = [{ nodes, depth: 0 }]
	var $max = 0
	while !$pending.is_empty() {
		level = $pending.last() ?? { nodes: [], depth: 0 }
		$pending = drop_last_n($pending, 1)
		for node in level.nodes {
			children =
				match node {
					Strong(inner) => inner
					Emphasis(inner) => inner
					Strikethrough(inner) => inner
					Link({ label, .. }) => label
					Image({ alt, .. }) => alt
					_ => []
				}
			depth = level.depth + 1
			if depth > $max and !children.is_empty() {
				$max = depth
			}
			$pending = $pending.append({ nodes: children, depth })
		}
	}
	$max
}

# Soft and hard line breaks.
expect inline_test("a  \n   b\\\nc \nd") == [Text("a"), HardBreak, Text("b"), HardBreak, Text("c\nd")]
expect inline_test("  a  ") == [Text("a")]

## Only spaces and tabs are stripped around line endings; vertical tab and
# form feed are text. Leading blank lines are not paragraph content.
expect inline_test("\n\r\na\u(B)\nb\u(C)") == [Text("a\u(B)\nb\u(C)")]

# GFM strikethrough pairs runs of equal length (one or two tildes).
expect inline_test("~~a~~ ~b~ ~~~c~~~") == [Strikethrough([Text("a")]), Text(" "), Strikethrough([Text("b")]), Text(" ~~~c~~~")]
expect inline_test("~~a~ b~~") == [Text("~~a~ b~~")]

# GFM extended autolinks: www., scheme:// and bare email addresses.
expect inline_test("see www.a.b/c). and http://x.y/z?") == [Text("see "), Link({ label: [Text("www.a.b/c")], target: { href: "http://www.a.b/c", title: Err(Missing) } }), Text("). and "), Link({ label: [Text("http://x.y/z")], target: { href: "http://x.y/z", title: Err(Missing) } }), Text("?")]
expect inline_test("mail a.b+c@d.ef.") == [Text("mail "), Link({ label: [Text("a.b+c@d.ef")], target: { href: "mailto:a.b+c@d.ef", title: Err(Missing) } }), Text(".")]
expect inline_test("[x www.a.b](/u)") == [Link({ label: [Text("x www.a.b")], target: { href: "/u", title: Err(Missing) } })]

# Reference links resolve using definitions and definitions are omitted from blocks.
expect {
	text =
		\\[roc]: https://roc-lang.org "Roc"
		\\Read [Roc][roc].

	actual = Markdown.parse_str(text)

	actual
		== [
			Paragraph([
				Text("Read "),
				Link({ label: [Text("Roc")], target: { href: "https://roc-lang.org", title: Ok("Roc") } }),
				Text("."),
			]),
		]
}

# Unresolved references remain literal text.
expect {
	actual = Markdown.parse_str("Read [Roc][missing].")

	actual == [Paragraph([Text("Read [Roc][missing].")])]
}

# Frontmatter is preserved only at the start of a document.
expect {
	text =
		\\---
		\\title: Hello
		\\---
		\\Body

	actual = Markdown.parse_str(text)

	actual == [Frontmatter("title: Hello\n"), Paragraph([Text("Body")])]
}

# Standalone thematic breaks parse as block nodes.
expect {
	actual = Markdown.parse_str("- - -")

	actual == [ThematicBreak]
}

# Ordered lists preserve their starting number and task list state.
expect {
	text =
		\\3) [x] Done
		\\4) [ ] Later

	actual = Markdown.parse_str(text)

	actual
		== [
			ListBlock({
				kind: Ordered({ start: 3 }),
				loose: False,
				items: [
					{ task: Checked, blocks: [Paragraph([Text("Done")])] },
					{ task: Unchecked, blocks: [Paragraph([Text("Later")])] },
				],
			}),
		]
}

# Changing the bullet character starts a new list (CommonMark example 301).
expect {
	text =
		\\* One
		\\+ Two

	actual = Markdown.parse_str(text)

	actual
		== [
			ListBlock({ kind: Unordered, loose: False, items: [{ task: NoTask, blocks: [Paragraph([Text("One")])] }] }),
			ListBlock({ kind: Unordered, loose: False, items: [{ task: NoTask, blocks: [Paragraph([Text("Two")])] }] }),
		]
}

# Blank lines inside lists mark the list as loose.
expect {
	text =
		\\- One
		\\
		\\- Two

	actual = Markdown.parse_str(text)

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: True,
				items: [
					{ task: NoTask, blocks: [Paragraph([Text("One")])] },
					{ task: NoTask, blocks: [Paragraph([Text("Two")])] },
				],
			}),
		]
}

# Nested unordered lists parse as child blocks.
expect {
	text =
		\\- One
		\\  - Nested

	actual = Markdown.parse_str(text)

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: False,
								items: [
									{ task: NoTask, blocks: [Paragraph([Text("Nested")])] },
								],
							}),
						],
					},
				],
			}),
		]
}

# Tilde fenced code blocks parse with info strings.
expect {
	text =
		\\~~~roc
		\\main = 1
		\\~~~

	actual = Markdown.parse_str(text)

	actual == [Code({ info: "roc", pre: "main = 1\n" })]
}

# Indented code blocks parse from four leading spaces.
expect {
	actual = Markdown.parse_str("    main = 1")

	actual == [Code({ info: "", pre: "main = 1\n" })]
}

## Pipe tables parse header alignment and inline cell content. A pipe inside a
# code span must still be escaped (GFM example 200).
expect {
	text =
		\\| Name | Count |
		\\| :--- | ---: |
		\\| **Roc** | `1\\|2` |

	actual = Markdown.parse_str(text)

	actual
		== [
			Table({
				header: [[Text("Name")], [Text("Count")]],
				align: [Left, Right],
				rows: [
					[
						[Strong([Text("Roc")])],
						[InlineCode("1|2")],
					],
				],
			}),
		]
}

# Malformed tables remain paragraph text.
expect {
	text =
		\\Name | Count
		\\not a delimiter

	actual = Markdown.parse_str(text)

	actual == [Paragraph([Text("Name | Count\nnot a delimiter")])]
}

# Raw HTML blocks are preserved without validation.
expect {
	text =
		\\<section>
		\\raw
		\\</section>

	actual = Markdown.parse_str(text)

	actual == [HtmlBlock("<section>\nraw\n</section>\n")]
}

## An unclosed fenced code block runs to the end of the document (CommonMark
# example 96); Markdown documents never fail to parse.
expect {
	text =
		\\```roc
		\\main = 1

	actual = Markdown.parse_str(text)

	actual == [Code({ info: "roc", pre: "main = 1\n" })]
}

## A closing `</pre>`, `</script>`, `</style>` or `</textarea>` tag alone on a
## line starts a type 7 HTML block; only the open tags are excluded from type
# 7 (CommonMark 4.6).
expect {
	actual = Markdown.parse_str("a\n\n</textarea>\nb\n\n<pre class=\"x\">\n")

	actual == [Paragraph([Text("a")]), HtmlBlock("</textarea>\nb\n"), HtmlBlock("<pre class=\"x\">\n")]
}

# Article body markdown parses into structured blocks fully.
expect {
	text =
		\\---
		\\title: Article
		\\---
		\\# Title with **style**
		\\
		\\Intro with **bold**, `code`, and [a link](https://example.com).
		\\
		\\![alt text](/image.png)
		\\
		\\```roc
		\\main = 1
		\\```
		\\
		\\- One
		\\  - Nested
		\\
		\\> Quote with **strong** text

	actual = Markdown.parse_str(text)

	actual
		== [
			Frontmatter("title: Article\n"),
			Heading({ level: One, content: [Text("Title with "), Strong([Text("style")])] }),
			Paragraph([
				Text("Intro with "),
				Strong([Text("bold")]),
				Text(", "),
				InlineCode("code"),
				Text(", and "),
				Link({ label: [Text("a link")], target: { href: "https://example.com", title: Err(Missing) } }),
				Text("."),
			]),
			Paragraph([Image({ alt: [Text("alt text")], target: { href: "/image.png", title: Err(Missing) } })]),
			Code({ info: "roc", pre: "main = 1\n" }),
			ListBlock({
				kind: Unordered,
				loose: False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: False,
								items: [
									{ task: NoTask, blocks: [Paragraph([Text("Nested")])] },
								],
							}),
						],
					},
				],
			}),
			Blockquote([
				Paragraph([
					Text("Quote with "),
					Strong([Text("strong")]),
					Text(" text"),
				]),
			]),
		]
}

# -------------------- example snippets used in docs --------------------

# Markdown.parse_str doc example.
expect Markdown.parse_str("# Hi\n\nSome *text*.") == [
	Heading({ level: One, content: [Text("Hi")] }),
	Paragraph([Text("Some "), Emphasis([Text("text")]), Text(".")]),
]

# Markdown.parse_inlines doc example.
expect Markdown.parse_inlines("**Bold** and `code`") == [
	Strong([Text("Bold")]),
	Text(" and "),
	InlineCode("code"),
]

# Markdown.frontmatter doc examples.
expect Markdown.frontmatter(Markdown.parse_str("---\ntitle: Hi\n---\nBody")) == Ok("title: Hi\n")
expect Markdown.frontmatter(Markdown.parse_str("Body")) == Err(Missing)

# The parsers consume their whole input and never fail.
expect Utf8.parse_str(Markdown.parser, "a\n\n> b") == Ok([Paragraph([Text("a")]), Blockquote([Paragraph([Text("b")])])])
expect Utf8.parse_str(Markdown.inline_parser, "*a") == Ok([Text("*a")])

# A link title is optional.
expect Markdown.parse_inlines("[a](/u \"t\") [b](/v)") == [
	Link({ label: [Text("a")], target: { href: "/u", title: Ok("t") } }),
	Text(" "),
	Link({ label: [Text("b")], target: { href: "/v", title: Err(Missing) } }),
]

# Syntax trees can be hashed, so they can be Set elements.
expect Set.from_list(Markdown.parse_str("a\n\n---\n\na")).len() == 2

# Block quotes nest at most 1000 deep; deeper markers are text.
expect {
	blocks = Markdown.parse_str(Str.concat(Str.repeat(">", 1002), " a"))
	quote_depth(blocks) == 1000 and innermost(blocks) == [Paragraph([Text(">> a")])]
}

# Lists stop nesting at the limit too.
expect Markdown.parse_str(Str.repeat("- ", 1500)).len() == 1

quote_depth : List(Markdown) -> U64
quote_depth = |blocks| {
	match blocks {
		[Blockquote(children)] => 1 + quote_depth(children)
		_ => 0
	}
}

innermost : List(Markdown) -> List(Markdown)
innermost = |blocks| {
	match blocks {
		[Blockquote(children)] => innermost(children)
		_ => blocks
	}
}

## Str.from_utf8_lossy is about 10x slower than Str.from_utf8 on valid input
## (roc#11966), and Markdown input is almost always valid UTF-8.
bytes_to_str : Utf8.Bytes -> Str
bytes_to_str = |bytes| Str.from_utf8(bytes) ?? Str.from_utf8_lossy(bytes)

## `List.take_first`, `drop_first` and `drop_last` copy the list instead of
## slicing it once they have several call sites (roc-lang/roc#11965);
## `List.sublist` does not.
take_n : List(a), U64 -> List(a)
take_n = |list, n| list.sublist({ start: 0, len: n })

drop_n : List(a), U64 -> List(a)
drop_n = |list, n| {
	len = list.len()
	if n >= len [] else list.sublist({ start: n, len: len - n })
}

drop_last_n : List(a), U64 -> List(a)
drop_last_n = |list, n| {
	len = list.len()
	if n >= len [] else list.sublist({ start: 0, len: len - n })
}
