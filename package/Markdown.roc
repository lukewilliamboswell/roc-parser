import MarkdownEntities
import Parser
import String
import unicode.Case
import unicode.GeneralCategory
import unicode.Scalar

## Markdown syntax tree and parsers for documents and inline content.
##
## The block parser preserves frontmatter, headings, paragraphs, blockquotes,
## lists, code blocks, thematic breaks, tables, and raw HTML. Inline parsing
## supports emphasis, links, images, code, hard breaks, and raw HTML.
Markdown := [
	Heading({ level : Markdown.Level, content : List(Markdown.Inline) }),
	Paragraph(List(Markdown.Inline)),
	Blockquote(List(Markdown)),
	ListBlock({ kind : Markdown.ListKind, loose : Bool, items : List({ task : Markdown.TaskState, blocks : List(Markdown) }) }),
	Code({ info : Str, pre : Str }),
	ThematicBreak,
	Table({ header : List(List(Markdown.Inline)), align : List(Markdown.Alignment), rows : List(List(List(Markdown.Inline))) }),
	HtmlBlock(Str),
	Frontmatter({ raw : Str }),
	TODO(Str),
].{

	## Render a Markdown block in Roc source-like notation for inspection.
	to_inspect : Markdown -> Str
	to_inspect = |node| {
		inspect_markdown(node)
	}

	## Compare two Markdown syntax trees structurally.
	is_eq : _

	## Render a Markdown block as a stable debug string.
	to_debug_str : Markdown -> Str
	to_debug_str = |node| {
		inspect_markdown(node)
	}

	## Render an inline node as a stable debug string.
	inline_to_debug_str : Markdown.Inline -> Str
	inline_to_debug_str = |inline| {
		inspect_inline(inline)
	}

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
	}

	## Destination and optional title of a link or image.
	LinkTarget : {
		href : Str,
		title : [Some(Str), None],
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
	}

	## Parse a complete Markdown document into block nodes.
	all : Parser(String.Utf8, List(Markdown))
	all = parse_all

	## Parse inline Markdown content.
	inlines : Parser(String.Utf8, List(Inline))
	inlines = parse_inlines_parser

	## Parse an ATX or Setext heading.
	heading : Parser(String.Utf8, Markdown)
	heading =
		Parser.one_of([
			inline_heading,
			two_line_heading_level_one,
			two_line_heading_level_two,
		])

	## Parse an inline link (`[label](destination "title")`) at the start of the input.
	link : Parser(String.Utf8, Inline)
	link = Parser.build_primitive_parser(|input| parse_leading_link(input, Bool.False))

	## Parse an inline image (`![alt](destination "title")`) at the start of the input.
	image : Parser(String.Utf8, Inline)
	image = Parser.build_primitive_parser(|input| parse_leading_link(input, Bool.True))

	## Parse a fenced code block delimited by triple backticks.
	code : Parser(String.Utf8, Markdown)
	code =
		Parser.const(|info| |pre| Code({ info: info, pre: pre }))
			.keep(
				Parser.one_of([
					Parser.const(|i| i)
						.skip(String.string("```"))
						.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
						.skip(end_of_line),
					Parser.const("")
						.skip(String.string("```")),
				]),
			)
			.keep(chomp_until_code_block_end)
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

		Frontmatter({ raw }) =>
			"Frontmatter({ raw: ${Str.inspect(raw)} })"

		TODO(line) =>
			"TODO(${Str.inspect(line)})"
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

inspect_link_title : [Some(Str), None] -> Str
inspect_link_title = |title| {
	match title {
		Some(value) =>
			"Some(${Str.inspect(value)})"

		None =>
			"None"
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
ListInfo : { ordered : Bool, marker : U8, start : U64 }

OpenKind : [
	DocumentBlock,
	QuoteBlock,
	ListContainer(ListInfo),
	ItemBlock({ info : ListInfo, marker_offset : U64, padding : U64, task : Markdown.TaskState }),
	ParagraphBlock,
	FencedBlock({ fence_char : U8, fence_len : U64, fence_offset : U64, info : Str }),
	IndentedBlock,
	HtmlBlockOpen(U64),
	TableBlock({ align : List(Markdown.Alignment), header : List(String.Utf8), rows : List(List(String.Utf8)) }),
]

## First and last source line of a closed block, used to decide list looseness:
## two siblings are separated by a blank line exactly when their spans leave a
## gap.
Span : { start : U64, end : U64 }

DoneItem : { task : Markdown.TaskState, blocks : List(Markdown), span : Span, inner_gap : Bool }

Open : {
	kind : OpenKind,
	start : U64,
	last : U64,
	children : List(Markdown),
	spans : List(Span),
	items : List(DoneItem),
	lines : List(String.Utf8),
}

BlockState : {
	stack : List(Open),
	refs : List(ReferenceDefinition),
	line : String.Utf8,
	line_number : U64,
	offset : U64,
	column : U64,
	partial_tab : Bool,
	all_closed : Bool,
	last_matched : U64,
}

Nonspace : { pos : U64, column : U64, indent : U64, blank : Bool }

Continuation : [Matched(BlockState), NotMatched, Consumed(BlockState)]

StartResult : [NoStart, StartedContainer(BlockState), StartedLeaf(BlockState), LineDone(BlockState), StopStarts(BlockState)]

parse_all : Parser(String.Utf8, List(Markdown))
parse_all =
	Parser.build_primitive_parser(
		|input| {
			Ok({ val: parse_document(input), input: [] })
		},
	)

## Markdown has no syntax errors: every input is a document.
parse_document : String.Utf8 -> List(Markdown)
parse_document = |input| {
	all_lines = split_document_lines(input)
	front = take_frontmatter(all_lines)
	parsed = parse_block_lines(front.lines)
	blocks = List.from_iter(parsed.blocks.iter().map(|block| resolve_inlines(block, parsed.refs)))

	match front.frontmatter {
		Ok(raw) => List.prepend(blocks, Frontmatter({ raw: raw }))
		Err(_) => blocks
	}
}

## Line endings are LF, CRLF, or a lone CR; U+0000 becomes U+FFFD. A final line
## ending does not start another line.
split_document_lines : String.Utf8 -> List(String.Utf8)
split_document_lines = |input| {
	var $lines = []
	var $current = []
	var $index = 0
	len = input.len()
	while $index < len {
		byte = input.get($index) ?? 0
		if byte == '\n' {
			$lines = $lines.append($current)
			$current = []
		} else if byte == '\r' {
			$lines = $lines.append($current)
			$current = []
			if (input.get($index + 1) ?? 0) == '\n' {
				$index = $index + 1
			}
		} else if byte == 0 {
			$current = $current.concat([0xEF, 0xBF, 0xBD])
		} else {
			$current = $current.append(byte)
		}
		$index = $index + 1
	}
	if $current.is_empty() {
		$lines
	} else {
		$lines.append($current)
	}
}

## Extension: a first line of exactly `---` up to the next line of exactly
## `---` is raw frontmatter, not Markdown. Without a closing line the document
## is ordinary Markdown.
take_frontmatter : List(String.Utf8) -> { frontmatter : Try(Str, [NotFound]), lines : List(String.Utf8) }
take_frontmatter = |lines| {
	if (lines.first() ?? []) != "---".to_utf8() {
		{ frontmatter: Err(NotFound), lines }
	} else {
		match lines.drop_first(1).find_first_index(|line| line == "---".to_utf8()) {
			Ok(index) => {
				raw = lines.sublist({ start: 1, len: index }).fold([], |acc, line| acc.concat(line).append('\n'))
				{ frontmatter: Ok(String.str_from_utf8(raw)), lines: lines.drop_first(index + 2) }
			}

			Err(_) =>
				{ frontmatter: Err(NotFound), lines }
			}
	}
}

new_open : OpenKind, U64 -> Open
new_open = |kind, line_number| {
	{ kind, start: line_number, last: line_number, children: [], spans: [], items: [], lines: [] }
}

parse_block_lines : List(String.Utf8) -> { blocks : List(Markdown), refs : List(ReferenceDefinition) }
parse_block_lines = |lines| {
	var $state = {
		stack: [new_open(DocumentBlock, 0)],
		refs: [],
		line: [],
		line_number: 0,
		offset: 0,
		column: 0,
		partial_tab: Bool.False,
		all_closed: Bool.True,
		last_matched: 0,
	}
	for line in lines {
		$state = process_line({ ..$state, line, line_number: $state.line_number + 1 })
	}
	while $state.stack.len() > 1 {
		$state = close_tip($state, $state.line_number)
	}
	document = $state.stack.first() ?? new_open(DocumentBlock, 0)
	{ blocks: document.children, refs: $state.refs }
}

tip_kind : BlockState -> OpenKind
tip_kind = |state| {
	match state.stack.last() {
		Ok(open) => open.kind
		Err(_) => DocumentBlock
	}
}

is_paragraph_kind : OpenKind -> Bool
is_paragraph_kind = |kind| {
	match kind {
		ParagraphBlock => Bool.True
		_ => Bool.False
	}
}

## Leaf blocks that take whole lines: no block starts are tried inside them.
accepts_lines : OpenKind -> Bool
accepts_lines = |kind| {
	match kind {
		FencedBlock(_) => Bool.True
		IndentedBlock => Bool.True
		HtmlBlockOpen(_) => Bool.True
		_ => Bool.False
	}
}

process_line : BlockState -> BlockState
process_line = |initial| {
	var $s = { ..initial, offset: 0, column: 0, partial_tab: Bool.False }

	# 1. Match the line against each open block's continuation condition.
	var $container = 0
	var $index = 1
	var $matching = Bool.True
	var $consumed = Bool.False
	while $matching and $index < $s.stack.len() {
		open = $s.stack.get($index) ?? new_open(DocumentBlock, 0)
		match continue_block($s, open, $index) {
			Matched(next) => {
				$s = next
				$container = $index
				$index = $index + 1
			}

			NotMatched => {
				$matching = Bool.False
			}

			Consumed(next) => {
				$s = next
				$matching = Bool.False
				$consumed = Bool.True
			}
		}
	}

	if $consumed {
		return $s
	}

	$s = { ..$s, all_closed: $container == $s.stack.len() - 1, last_matched: $container }

	# 2. Try block starts, unless the matched block is a leaf taking raw lines.
	container_kind = ($s.stack.get($container) ?? new_open(DocumentBlock, 0)).kind
	var $starting = !accepts_lines(container_kind)
	var $done = Bool.False
	while $starting {
		ns = find_nonspace($s)
		match try_block_starts($s, $container, ns) {
			NoStart => {
				$s = advance_to_nonspace($s, ns)
				$starting = Bool.False
			}

			StartedContainer(next) => {
				$s = next
				$container = next.stack.len() - 1
			}

			StopStarts(next) => {
				$s = advance_to_nonspace(next, find_nonspace(next))
				$container = next.stack.len() - 1
				$starting = Bool.False
			}

			StartedLeaf(next) => {
				$s = next
				$container = next.stack.len() - 1
				$starting = Bool.False
			}

			LineDone(next) => {
				$s = next
				$starting = Bool.False
				$done = Bool.True
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
				rest = $s.line.drop_first($s.offset)
				added = add_line_to_tip($s)
				if html_type <= 5 and html_block_ends(html_type, rest) {
					close_tip(added, added.line_number)
				} else {
					added
				}
			}

			TableBlock(_) => add_table_row($s)
			_ =>
				if blank {
					$s
				} else {
					add_line_to_tip(add_child($s, ParagraphBlock))
				}
		}
	}
}

find_nonspace : BlockState -> Nonspace
find_nonspace = |s| {
	var $pos = s.offset
	var $column = s.column
	var $scanning = Bool.True
	while $scanning {
		byte = s.line.get($pos) ?? 'x'
		if byte == ' ' {
			$pos = $pos + 1
			$column = $column + 1
		} else if byte == '\t' and $pos < s.line.len() {
			$pos = $pos + 1
			$column = $column + (4 - ($column % 4))
		} else {
			$scanning = Bool.False
		}
	}
	{ pos: $pos, column: $column, indent: $column - s.column, blank: $pos >= s.line.len() }
}

advance_to_nonspace : BlockState, Nonspace -> BlockState
advance_to_nonspace = |s, ns| {
	{ ..s, offset: ns.pos, column: ns.column, partial_tab: Bool.False }
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
				$partial = Bool.False
				$column = $column + to_tab
				$offset = $offset + 1
				$count = $count - 1
			}
		} else {
			$partial = Bool.False
			$offset = $offset + 1
			$column = $column + 1
			$count = $count - 1
		}
	}
	{ ..s, offset: $offset, column: $column, partial_tab: $partial }
}

has_children : BlockState, U64, Open -> Bool
has_children = |s, index, open| {
	!open.children.is_empty() or index + 1 < s.stack.len()
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
				Matched(advance_offset(s, item.marker_offset + item.padding, Bool.True))
			} else if ns.blank and has_children(s, index, open) {
				Matched(advance_to_nonspace(s, ns))
			} else {
				NotMatched
			}

		FencedBlock(fence) => {
			rest = s.line.drop_first(ns.pos)
			if ns.indent <= 3 and is_closing_fence(rest, fence.fence_char, fence.fence_len) {
				closed = close_tip({ ..s, stack: set_tip_last(s.stack, s.line_number) }, s.line_number)
				Consumed(closed)
			} else {
				var $next = s
				var $remaining = fence.fence_offset
				while $remaining > 0 and is_space_or_tab($next.line.get($next.offset) ?? 'x') {
					$next = advance_offset($next, 1, Bool.True)
					$remaining = $remaining - 1
				}
				Matched($next)
			}
		}

		IndentedBlock =>
			if ns.indent >= 4 {
				Matched(advance_offset(s, 4, Bool.True))
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
			if ns.blank or split_table_row(s.line.drop_first(ns.pos)).is_empty() {
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
	after = advance_offset(s, 1, Bool.False)
	if is_space_or_tab(after.line.get(after.offset) ?? 'x') {
		advance_offset(after, 1, Bool.True)
	} else {
		after
	}
}

set_tip_last : List(Open), U64 -> List(Open)
set_tip_last = |stack, line_number| {
	match stack.last() {
		Ok(open) => stack.drop_last(1).append({ ..open, last: line_number })
		Err(_) => stack
	}
}

## Close blocks left unmatched by a line that is not a lazy continuation.
close_unmatched : BlockState -> BlockState
close_unmatched = |s| {
	if s.all_closed {
		s
	} else {
		var $next = s
		while $next.stack.len() - 1 > s.last_matched {
			$next = close_tip($next, s.line_number - 1)
		}
		{ ..$next, all_closed: Bool.True }
	}
}

can_contain : OpenKind, OpenKind -> Bool
can_contain = |parent, child| {
	match parent {
		DocumentBlock => !is_item_kind(child)
		QuoteBlock => !is_item_kind(child)
		ItemBlock(_) => !is_item_kind(child)
		ListContainer(_) => is_item_kind(child)
		_ => Bool.False
	}
}

is_item_kind : OpenKind -> Bool
is_item_kind = |kind| {
	match kind {
		ItemBlock(_) => Bool.True
		_ => Bool.False
	}
}

add_child : BlockState, OpenKind -> BlockState
add_child = |s, kind| {
	var $next = s
	while !can_contain(tip_kind($next), kind) {
		$next = close_tip($next, $next.line_number - 1)
	}
	{ ..$next, stack: $next.stack.append(new_open(kind, $next.line_number)) }
}

## Attach a block that is complete as soon as it starts (headings, breaks).
add_closed_child : BlockState, Markdown -> BlockState
add_closed_child = |s, block| {
	var $next = s
	while !can_contain(tip_kind($next), ParagraphBlock) {
		$next = close_tip($next, $next.line_number - 1)
	}
	span = { start: $next.line_number, end: $next.line_number }
	{ ..$next, stack: update_tip($next.stack, |open| { ..open, children: open.children.append(block), spans: open.spans.append(span) }) }
}

update_tip : List(Open), (Open -> Open) -> List(Open)
update_tip = |stack, f| {
	match stack.last() {
		Ok(open) => stack.drop_last(1).append(f(open))
		Err(_) => stack
	}
}

## The rest of the line from the current offset; a partly consumed tab
## contributes its remaining columns as spaces.
line_rest : BlockState -> String.Utf8
line_rest = |s| {
	if s.partial_tab {
		List.repeat(' ', 4 - (s.column % 4)).concat(s.line.drop_first(s.offset + 1))
	} else {
		s.line.drop_first(s.offset)
	}
}

add_line_to_tip : BlockState -> BlockState
add_line_to_tip = |s| {
	content = line_rest(s)
	counts = !(is_indented_kind(tip_kind(s)) and bytes_are_blank(content))
	{ ..s, stack: update_tip(s.stack, |open| { ..open, lines: open.lines.append(content), last: if counts s.line_number else open.last }) }
}

is_indented_kind : OpenKind -> Bool
is_indented_kind = |kind| {
	match kind {
		IndentedBlock => Bool.True
		_ => Bool.False
	}
}

add_table_row : BlockState -> BlockState
add_table_row = |s| {
	cells = split_table_row(s.line.drop_first(s.offset))
	{
		..s,
		stack: update_tip(
			s.stack,
			|open| {
				match open.kind {
					TableBlock(table) => { ..open, kind: TableBlock({ ..table, rows: table.rows.append(cells) }), last: s.line_number }
					_ => open
				}
			},
		),
	}
}

## Pop the innermost open block, finish it, and attach it to its parent.
close_tip : BlockState, U64 -> BlockState
close_tip = |s, line_number| {
	match s.stack.last() {
		Err(_) => s
		Ok(open) => {
			rest = s.stack.drop_last(1)
			finished = finish_block(open, line_number, s.refs)
			stack =
				match finished.result {
					Block(block, span) => update_tip(rest, |parent| { ..parent, children: parent.children.append(block), spans: parent.spans.append(span) })
					Item(item) => update_tip(rest, |parent| { ..parent, items: parent.items.append(item) })
					Nothing => rest
				}
			{ ..s, stack, refs: finished.refs }
		}
	}
}

has_gap : List(Span) -> Bool
has_gap = |spans| {
	var $index = 1
	var $gap = Bool.False
	while !$gap and $index < spans.len() {
		before = spans.get($index - 1) ?? { start: 0, end: 0 }
		after = spans.get($index) ?? { start: 0, end: 0 }
		if after.start > before.end + 1 {
			$gap = Bool.True
		}
		$index = $index + 1
	}
	$gap
}

placeholder : String.Utf8 -> List(Markdown.Inline)
placeholder = |raw| [Text(String.str_from_utf8(raw))]

finish_block : Open, U64, List(ReferenceDefinition) -> { result : [Block(Markdown, Span), Item(DoneItem), Nothing], refs : List(ReferenceDefinition) }
finish_block = |open, line_number, refs| {
	leaf_span = { start: open.start, end: open.last }
	match open.kind {
		DocumentBlock =>
			{ result: Nothing, refs }

		QuoteBlock =>
			{ result: Block(Blockquote(open.children), { start: open.start, end: line_number }), refs }

		ItemBlock(item) => {
			end = (open.spans.last() ?? { start: open.start, end: open.start }).end
			{ result: Item({ task: item.task, blocks: open.children, span: { start: open.start, end }, inner_gap: has_gap(open.spans) }), refs }
		}

		ListContainer(info) => {
			loose = open.items.any(|item| item.inner_gap) or has_gap(open.items.map(|item| item.span))
			kind = if info.ordered Ordered({ start: info.start }) else Unordered
			items = open.items.map(|item| { task: item.task, blocks: item.blocks })
			end = (open.items.last() ?? { task: NoTask, blocks: [], span: { start: open.start, end: open.start }, inner_gap: Bool.False }).span.end
			{ result: Block(ListBlock({ kind, loose, items }), { start: open.start, end }), refs }
		}

		ParagraphBlock => {
			extracted = extract_reference_definitions(join_with_newlines(open.lines), refs)
			content = trim_end_spaces(extracted.rest)
			if content.is_empty() {
				{ result: Nothing, refs: extracted.refs }
			} else {
				{ result: Block(Paragraph(placeholder(content)), leaf_span), refs: extracted.refs }
			}
		}

		FencedBlock(fence) =>
			{ result: Block(Code({ info: fence.info, pre: String.str_from_utf8(join_lines_with_newlines(open.lines)) }), leaf_span), refs }

		IndentedBlock => {
			var $lines = open.lines
			while bytes_are_blank($lines.last() ?? [0]) {
				$lines = $lines.drop_last(1)
			}
			{ result: Block(Code({ info: "", pre: String.str_from_utf8(join_lines_with_newlines($lines)) }), leaf_span), refs }
		}

		HtmlBlockOpen(_) =>
			{ result: Block(HtmlBlock(String.str_from_utf8(join_lines_with_newlines(open.lines))), leaf_span), refs }

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
			{ result: Block(Table({ header: fit(table.header), align: table.align, rows: table.rows.map(fit) }), leaf_span), refs }
		}
	}
}

join_with_newlines : List(String.Utf8) -> String.Utf8
join_with_newlines = |lines| {
	var $out = []
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
	rest = s.line.drop_first(ns.pos)
	indented = ns.indent >= 4
	first = rest.first() ?? 0

	if !indented and first == '>' {
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
				paragraph = closed.stack.last() ?? new_open(ParagraphBlock, 0)
				extracted = extract_reference_definitions(join_with_newlines(paragraph.lines), closed.refs)
				if !extracted.rest.is_empty() {
					heading = Heading({ level, content: placeholder(trim_end_spaces(extracted.rest)) })
					span = { start: paragraph.start, end: closed.line_number }
					stack = update_tip(closed.stack.drop_last(1), |parent| { ..parent, children: parent.children.append(heading), spans: parent.spans.append(span) })
					return LineDone({ ..closed, stack, refs: extracted.refs })
				} else {
					# Only reference definitions: keep the (now empty) paragraph and
					# let the underline be read as something else.
					emptied = update_tip(closed.stack, |open| { ..open, lines: [] })
					return try_after_setext({ ..closed, stack: emptied, refs: extracted.refs }, container, ns)
				}
			}

			Err(_) => {}
		}
	}

	try_after_setext(s, container, ns)
}

try_after_setext : BlockState, U64, Nonspace -> StartResult
try_after_setext = |s, container, ns| {
	container_kind = (s.stack.get(container) ?? new_open(DocumentBlock, 0)).kind
	rest = s.line.drop_first(ns.pos)
	indented = ns.indent >= 4

	if !indented and is_thematic_break(rest) {
		return LineDone(add_closed_child(close_unmatched(s), ThematicBreak))
	}

	if !indented {
		match parse_list_marker(rest, is_paragraph_kind(container_kind)) {
			Ok(marker) => return start_list_item(s, container_kind, ns, marker)
			Err(_) => {}
		}
	}

	if indented and !is_paragraph_kind(tip_kind(s)) and !ns.blank {
		started = add_child(close_unmatched(advance_offset(s, 4, Bool.True)), IndentedBlock)
		return StartedLeaf(started)
	}

	if !indented and is_paragraph_kind(container_kind) and s.all_closed {
		match parse_table_delimiter_row(rest) {
			Ok(align) => {
				paragraph = s.stack.last() ?? new_open(ParagraphBlock, 0)
				header_line = paragraph.lines.last() ?? []
				header = split_table_row(header_line)
				if !header.is_empty() and header.len() == align.len() {
					before = update_tip(s.stack, |open| { ..open, lines: open.lines.drop_last(1), last: s.line_number - 2 })
					closed = close_tip({ ..s, stack: before }, s.line_number - 2)
					table = { ..new_open(TableBlock({ align, header, rows: [] }), s.line_number - 1), last: s.line_number }
					return LineDone({ ..closed, stack: closed.stack.append(table) })
				}
			}

			Err(_) => {}
		}
	}

	NoStart
}

ListMarker : { info : ListInfo, width : U64 }

start_list_item : BlockState, OpenKind, Nonspace, ListMarker -> StartResult
start_list_item = |s, container_kind, ns, marker| {
	marker_offset = ns.indent
	at_marker = advance_offset(advance_to_nonspace(s, ns), marker.width, Bool.True)
	spaces_start = at_marker
	var $probe = advance_offset(at_marker, 1, Bool.True)
	while $probe.column - spaces_start.column < 5 and is_space_or_tab($probe.line.get($probe.offset) ?? 'x') {
		$probe = advance_offset($probe, 1, Bool.True)
	}
	blank_item = $probe.offset >= $probe.line.len()
	spaces_after = $probe.column - spaces_start.column
	{ after_marker, padding } =
		if spaces_after >= 5 or spaces_after < 1 or blank_item {
			skipped = if is_space_or_tab(spaces_start.line.get(spaces_start.offset) ?? 'x') advance_offset(spaces_start, 1, Bool.True) else spaces_start
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
	item = add_child(with_list, ItemBlock({ info: marker.info, marker_offset, padding, task: NoTask }))

	# GFM task list item: `[ ]`, `[x]` or `[X]` then a space or tab opens the
	# item's first paragraph.
	task_ns = find_nonspace(item)
	task_rest = item.line.drop_first(task_ns.pos)
	match task_rest {
		['[', mark, ']', after, ..] if task_ns.indent < 4 and (mark == ' ' or mark == 'x' or mark == 'X') and is_space_or_tab(after) => {
			task = if mark == ' ' Unchecked else Checked
			marked = { ..item, stack: update_tip(item.stack, |open| { ..open, kind: ItemBlock({ info: marker.info, marker_offset, padding, task }) }) }
			StopStarts(advance_offset(advance_to_nonspace(marked, task_ns), 3, Bool.False))
		}

		_ =>
			StartedContainer(item)
	}
}

is_list_tip : BlockState -> Bool
is_list_tip = |s| {
	match tip_kind(s) {
		ListContainer(_) => Bool.True
		_ => Bool.False
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
parse_list_marker : String.Utf8, Bool -> Try(ListMarker, [NotFound])
parse_list_marker = |rest, interrupts_paragraph| {
	first = rest.first() ?? 0
	parsed =
		if first == '-' or first == '+' or first == '*' {
			Ok({ info: { ordered: Bool.False, marker: first, start: 1 }, width: 1 })
		} else {
			digits = count_leading_digits(rest)
			delimiter = rest.get(digits) ?? 0
			if digits >= 1 and digits <= 9 and (delimiter == '.' or delimiter == ')') {
				Ok({ info: { ordered: Bool.True, marker: delimiter, start: digits_to_u64(rest.take_first(digits)) }, width: digits + 1 })
			} else {
				Err(NotFound)
			}
		}
	marker = parsed?
	after = rest.drop_first(marker.width)
	next = after.first() ?? ' '
	if !is_space_or_tab(next) {
		Err(NotFound)
	} else if interrupts_paragraph and (bytes_are_blank(after) or (marker.info.ordered and marker.info.start != 1)) {
		Err(NotFound)
	} else {
		Ok(marker)
	}
}

count_leading_digits : String.Utf8 -> U64
count_leading_digits = |bytes| {
	var $count = 0
	while is_digit_byte(bytes.get($count) ?? 'x') {
		$count = $count + 1
	}
	$count
}

## ATX heading (CommonMark 4.2): 1-6 `#` then a space, tab, or end of line.
parse_atx_heading : String.Utf8 -> Try(Markdown, [NotFound])
parse_atx_heading = |rest| {
	hashes = count_leading_byte(rest, '#', 0)
	after = rest.drop_first(hashes)
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
				trim_end_spaces(content.drop_last(closing))
			} else {
				content
			}
		Ok(Heading({ level, content: placeholder(without_closing) }))
	}
}

count_trailing_byte : String.Utf8, U8 -> U64
count_trailing_byte = |bytes, expected| {
	var $count = 0
	while $count < bytes.len() and (bytes.get(bytes.len() - 1 - $count) ?? 0) == expected {
		$count = $count + 1
	}
	$count
}

## Setext underline (CommonMark 4.3): `=`s or `-`s, then only spaces or tabs.
setext_level : String.Utf8 -> Try(Markdown.Level, [NotFound])
setext_level = |rest| {
	marker = rest.first() ?? 0
	run = count_leading_byte(rest, marker, 0)
	if run == 0 or !bytes_are_blank(rest.drop_first(run)) {
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
is_thematic_break : String.Utf8 -> Bool
is_thematic_break = |rest| {
	marker = rest.first() ?? 0
	if marker != '-' and marker != '_' and marker != '*' {
		Bool.False
	} else {
		rest.all(|byte| byte == marker or is_space_or_tab(byte)) and rest.count_if(|byte| byte == marker) >= 3
	}
}

## Opening code fence (CommonMark 4.5). A backtick fence's info string may not
## contain backticks. Backslash escapes and entity references in the info
## string are resolved.
parse_fence_open : String.Utf8 -> Try({ fence_char : U8, fence_len : U64, fence_offset : U64, info : Str }, [NotFound])
parse_fence_open = |rest| {
	fence_char = rest.first() ?? 0
	fence_len = count_leading_byte(rest, fence_char, 0)
	info = trim_spaces(rest.drop_first(fence_len))
	if fence_len < 3 or (fence_char == '`' and info.contains('`')) {
		Err(NotFound)
	} else {
		Ok({ fence_char, fence_len, fence_offset: 0, info: unescape_link_text(info) })
	}
}

is_closing_fence : String.Utf8, U8, U64 -> Bool
is_closing_fence = |rest, fence_char, fence_len| {
	run = count_leading_byte(rest, fence_char, 0)
	run >= fence_len and bytes_are_blank(rest.drop_first(run))
}

## --- HTML blocks (CommonMark 4.6) -------------------------------------------

html_type_1_tags : List(Str)
html_type_1_tags = ["pre", "script", "style", "textarea"]

html_type_6_tags : List(Str)
html_type_6_tags = [
	"address", "article", "aside", "base", "basefont", "blockquote", "body", "caption", "center", "col", "colgroup", "dd", "details", "dialog", "dir", "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer", "form", "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hr", "html", "iframe", "legend", "li", "link", "main", "menu", "menuitem", "nav", "noframes", "ol", "optgroup", "option", "p", "param", "search", "section", "summary", "table", "tbody", "td", "tfoot", "th", "thead", "title", "tr", "track", "ul",
]

lowercase_ascii : String.Utf8 -> String.Utf8
lowercase_ascii = |bytes| bytes.map(|byte| if byte >= 'A' and byte <= 'Z' byte + 32 else byte)

html_block_start : String.Utf8, Bool -> Try(U64, [NotFound])
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

is_type_6_start : String.Utf8 -> Bool
is_type_6_start = |lower| {
	name_start = if lower.starts_with("</".to_utf8()) 2 else 1
	name = take_tag_name(lower.drop_first(name_start))
	after = lower.drop_first(name_start + name.len())
	next = after.first() ?? ' '
	html_type_6_tags.any(|tag| tag.to_utf8() == name)
	and (next == ' ' or next == '\t' or next == '>' or after.starts_with("/>".to_utf8()))
}

take_tag_name : String.Utf8 -> String.Utf8
take_tag_name = |bytes| {
	match bytes.first() {
		Ok(first) if is_alphabetic_byte(first) => {
			var $len = 1
			while is_tag_name_byte(bytes.get($len) ?? ' ') {
				$len = $len + 1
			}
			bytes.take_first($len)
		}

		_ => []
	}
}

is_tag_name_byte : U8 -> Bool
is_tag_name_byte = |byte| is_alphabetic_byte(byte) or is_digit_byte(byte) or byte == '-'

## Type 7: a complete closing tag, or a complete open tag other than the
## type 1 tags (`<pre>` and friends open type 1 blocks instead), alone on the
## line.
is_type_7_start : String.Utf8 -> Bool
is_type_7_start = |rest| {
	end =
		if rest.starts_with("</".to_utf8()) {
			scan_closing_tag(rest, 2)
		} else {
			name = lowercase_ascii(take_tag_name(rest.drop_first(1)))
			if html_type_1_tags.any(|tag| tag.to_utf8() == name) Err(NotFound) else scan_open_tag(rest, 1)
		}
	match end {
		Ok(len) => bytes_are_blank(rest.drop_first(len))
		Err(_) => Bool.False
	}
}

html_block_ends : U64, String.Utf8 -> Bool
html_block_ends = |html_type, rest| {
	lower = lowercase_ascii(rest)
	match html_type {
		1 => ["</pre>", "</script>", "</style>", "</textarea>"].any(|end| contains_bytes(lower, end.to_utf8()))
		2 => contains_bytes(rest, "-->".to_utf8())
		3 => contains_bytes(rest, "?>".to_utf8())
		4 => rest.contains('>')
		5 => contains_bytes(rest, "]]>".to_utf8())
		_ => Bool.False
	}
}

contains_bytes : String.Utf8, String.Utf8 -> Bool
contains_bytes = |haystack, needle| {
	var $index = 0
	var $found = Bool.False
	while !$found and $index + needle.len() <= haystack.len() {
		if haystack.sublist({ start: $index, len: needle.len() }) == needle {
			$found = Bool.True
		}
		$index = $index + 1
	}
	$found
}

## --- GFM tables -----------------------------------------------------------

## Cells of a table row: an optional leading and trailing pipe, cells split on
## unescaped pipes (even inside code spans), `\|` unescaped to `|`, and each
## cell trimmed. A row needs at least one cell.
split_table_row : String.Utf8 -> List(String.Utf8)
split_table_row = |line| {
	trimmed = trim_spaces(line)
	body = if trimmed.first() == Ok('|') trimmed.drop_first(1) else trimmed
	var $cells = []
	var $current = []
	var $pending = Bool.False
	var $index = 0
	while $index < body.len() {
		byte = body.get($index) ?? 0
		if byte == '\\' and (body.get($index + 1) ?? 0) == '|' {
			$current = $current.append('|')
			$pending = Bool.True
			$index = $index + 2
		} else if byte == '|' {
			$cells = $cells.append(trim_spaces($current))
			$current = []
			$pending = Bool.False
			$index = $index + 1
		} else {
			$current = $current.append(byte)
			$pending = Bool.True
			$index = $index + 1
		}
	}
	if $pending and !bytes_are_blank($current) {
		$cells.append(trim_spaces($current))
	} else {
		$cells
	}
}

## Delimiter row: cells of `:?-+:?` surrounded by optional spaces or tabs.
parse_table_delimiter_row : String.Utf8 -> Try(List(Markdown.Alignment), [NotFound])
parse_table_delimiter_row = |rest| {
	if !rest.contains('-') or rest.any(|byte| !(byte == '-' or byte == ':' or byte == '|' or is_space_or_tab(byte))) {
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

parse_alignment_cell : String.Utf8 -> Try(Markdown.Alignment, [NotFound])
parse_alignment_cell = |cell| {
	left = cell.first() == Ok(':')
	right = cell.len() > 1 and cell.last() == Ok(':')
	dashes = cell.drop_first(if left 1 else 0).drop_last(if right 1 else 0)
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
extract_reference_definitions : String.Utf8, List(ReferenceDefinition) -> { rest : String.Utf8, refs : List(ReferenceDefinition) }
extract_reference_definitions = |content, refs| {
	var $rest = content
	var $refs = refs
	var $scanning = Bool.True
	while $scanning {
		match parse_reference_definition($rest) {
			Ok(found) => {
				$refs = if $refs.any(|ref| ref.label == found.def.label) $refs else $refs.append(found.def)
				$rest = $rest.drop_first(found.consumed)
			}

			Err(_) => {
				$scanning = Bool.False
			}
		}
	}
	{ rest: $rest, refs: $refs }
}

## Spaces or tabs, then at most one line ending, then spaces or tabs.
skip_spnl : String.Utf8, U64 -> U64
skip_spnl = |bytes, start| {
	first = skip_spaces_tabs(bytes, start)
	if (bytes.get(first) ?? 0) == '\n' skip_spaces_tabs(bytes, first + 1) else first
}

## One definition at the start of `bytes` (CommonMark 4.7), using the inline
## parser's label, destination and title scanners. It may span lines but must
## end at a line ending; a title followed by anything else is dropped and the
## definition ends after its destination if that ends its line.
parse_reference_definition : String.Utf8 -> Try({ def : ReferenceDefinition, consumed : U64 }, [NotFound])
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
			Ok(found) => { end: skip_spaces_tabs(bytes, found.end), title_value: Some(unescape_link_text(found.raw)) }
			Err(_) => { end: skip_spaces_tabs(bytes, before_title), title_value: None }
		}
	if !at_line_end(bytes, end) {
		return Err(NotFound)
	}
	consumed = if end < bytes.len() end + 1 else end
	href = unescape_link_text(trim_cmark_space(destination.raw))
	Ok({ def: { label: normalize_reference_label(label.raw), target: { href, title: title_value } }, consumed })
}

at_line_end : String.Utf8, U64 -> Bool
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

placeholder_raw : List(Markdown.Inline) -> String.Utf8
placeholder_raw = |content| {
	match content {
		[Text(raw)] => raw.to_utf8()
		_ => []
	}
}

inline_heading : Parser(String.Utf8, Markdown)
inline_heading =
	Parser.const(
		|level| {
			|str| {
				Heading({ level: level, content: parse_inlines(trim_closing_heading_marker(str.to_utf8())) })
			}
		},
	)
		.keep(
			Parser.one_of([
				Parser.const(One).skip(String.string("# ")),
				Parser.const(Two).skip(String.string("## ")),
				Parser.const(Three).skip(String.string("### ")),
				Parser.const(Four).skip(String.string("#### ")),
				Parser.const(Five).skip(String.string("##### ")),
				Parser.const(Six).skip(String.string("###### ")),
			]),
		)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

two_line_heading_level_one : Parser(String.Utf8, Markdown)
two_line_heading_level_one =
	Parser.const(
		|str| {
			Heading({ level: One, content: parse_inlines(str.to_utf8()) })
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("=="))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '='
				},
			),
		)

two_line_heading_level_two : Parser(String.Utf8, Markdown)
two_line_heading_level_two =
	Parser.const(
		|str| {
			Heading({ level: Two, content: parse_inlines(str.to_utf8()) })
		},
	)
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))
		.skip(end_of_line)
		.skip(String.string("--"))
		.skip(
			Parser.chomp_while(
				|b| {
					not_end_of_line(b) and b == '-'
				},
			),
		)

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
	Delim(InlineDelim),
]

## A run of `*`, `_` or `~` that may open or close emphasis or strikethrough.
## `length` is the original run length (used by the "multiple of 3" rule);
## `count` is how many delimiter characters are still unused.
InlineDelim : { char : U8, length : U64, count : U64, can_open : Bool, can_close : Bool }

## An entry of the bracket stack: the `[` or `![` item, the input offset just
## after the bracket, and its push sequence number (a later bracket was pushed
## when the push counter has moved on by more than one).
InlineBracket : { item : U64, start : U64, image : Bool, sequence : U64 }

## A maximal run of backticks in the input.
TickRun : { start : U64, len : U64 }

InlineStop : [Finished, Stopped({ node : Markdown.Inline, end : U64 }), Failed]

parse_inlines_parser : Parser(String.Utf8, List(Markdown.Inline))
parse_inlines_parser =
	Parser.build_primitive_parser(
		|input| {
			Ok({ val: parse_inlines(input), input: [] })
		},
	)

parse_inlines : String.Utf8 -> List(Markdown.Inline)
parse_inlines = |input| {
	parse_inlines_with_refs([], input)
}

parse_inlines_with_refs : List(ReferenceDefinition), String.Utf8 -> List(Markdown.Inline)
parse_inlines_with_refs = |refs, input| {
	scan_inlines(prepare_inline_input(input), refs, Bool.False).nodes
}

## Parse a link or image that starts at the beginning of `input`, returning the
## node and the unconsumed input. Used by the `Markdown.link`/`Markdown.image`
## parsers.
parse_leading_link : String.Utf8, Bool -> Try({ val : Markdown.Inline, input : String.Utf8 }, [ParsingFailure(Str)])
parse_leading_link = |input, image| {
	opens =
		if image {
			byte_at(input, 0) == '!' and byte_at(input, 1) == '['
		} else {
			byte_at(input, 0) == '['
		}

	if !opens {
		Err(ParsingFailure(if image "expected ![" else "expected ["))
	} else {
		match scan_inlines(input, [], Bool.True).stop {
			Stopped({ node, end }) => {
				matches_kind =
					match node {
						Image(_) => image
						Link(_) => !image
						_ => Bool.False
					}
				if matches_kind {
					Ok({ val: node, input: input.drop_first(end) })
				} else {
					Err(ParsingFailure("expected an inline link"))
				}
			}

			_ =>
				Err(ParsingFailure("expected an inline link"))
			}
	}
}

## Normalize paragraph content: replace U+0000 with U+FFFD, strip the leading
## spaces and tabs of every line, drop leading blank lines and strip trailing
## whitespace.
prepare_inline_input : List(U8) -> List(U8)
prepare_inline_input = |input| {
	var $out = List.with_capacity(input.len())
	var $line_start = Bool.True
	for byte in input {
		if $line_start and (byte == ' ' or byte == '\t') {
			{}
		} else if $out.is_empty() and (byte == '\n' or byte == '\r') {
			{}
		} else if byte == 0 {
			$out = $out.concat([0xEF, 0xBF, 0xBD])
			$line_start = Bool.False
		} else {
			$out = $out.append(byte)
			$line_start = byte == '\n' or byte == '\r'
		}
	}
	trim_trailing_whitespace($out)
}

trim_trailing_whitespace : List(U8) -> List(U8)
trim_trailing_whitespace = |bytes| {
	var $len = bytes.len()
	while $len > 0 and is_cmark_space(byte_at(bytes, $len - 1)) {
		$len = $len - 1
	}
	bytes.take_first($len)
}

## Trailing spaces and tabs before a line ending are not part of the text.
## (Vertical tab and form feed are ordinary characters in CommonMark.)
trim_trailing_line_space : List(U8) -> List(U8)
trim_trailing_line_space = |bytes| {
	var $len = bytes.len()
	while $len > 0 and is_line_space(byte_at(bytes, $len - 1)) {
		$len = $len - 1
	}
	bytes.take_first($len)
}

is_line_space : U8 -> Bool
is_line_space = |byte| byte == ' ' or byte == '\t'

## Spaces, tabs and line endings (cmark's `cmark_isspace`).
is_cmark_space : U8 -> Bool
is_cmark_space = |byte| byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r'

byte_at : List(U8), U64 -> U8
byte_at = |bytes, index| bytes.get(index) ?? 0

## The main inline scanner. With `leading_link`, stop as soon as the bracket at
## offset 0 is resolved (used to parse a single leading link or image).
scan_inlines : List(U8), List(ReferenceDefinition), Bool -> { nodes : List(Markdown.Inline), stop : InlineStop }
scan_inlines = |input, refs, leading_link| {
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
	var $html_skip = { comment: Bool.False, pi: Bool.False, cdata: Bool.False, declaration: Bool.False }
	var $stop = Finished
	var $running = Bool.True

	while $running and $pos < len {
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
					$items = flush_chars($items, $text).append(Node(InlineCode(Str.from_utf8_lossy(content))))
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
			$brackets = $brackets.append({ item: $items.len() - 1, start: $pos + 2, image: Bool.True, sequence: $bracket_pushes })
			$bracket_pushes = $bracket_pushes + 1
			$pos = $pos + 2
		} else if byte == '[' {
			$items = flush_chars($items, $text).append(Chars(['[']))
			$text = []
			$brackets = $brackets.append({ item: $items.len() - 1, start: $pos + 1, image: Bool.False, sequence: $bracket_pushes })
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
					$brackets = $brackets.drop_last(1)
					$link_floor = min_u64($link_floor, $brackets.len())
					resolved =
						if active {
							resolve_link(input, after, opener, $bracket_pushes > opener.sequence + 1, $pos, refs)
						} else {
							Err(NotFound)
						}
					match resolved {
						Ok(found) => {
							# Consume the content slice before truncating, so the
							# item list stays uniquely owned and is not copied.
							children = process_emphasis($items.drop_first(opener.item + 1))
							$items = $items.take_first(opener.item)
							node =
								if opener.image {
									Image({ alt: children, target: found.target })
								} else {
									Link({ label: children, target: found.target })
								}
							if !opener.image {
								$link_floor = $brackets.len()
							}
							if leading_link and opener.item == 0 {
								$stop = Stopped({ node: autolink_emails(node), end: found.end })
								$running = Bool.False
							} else {
								$items = $items.append(Node(node))
								$pos = found.end
							}
						}

						Err(_) => {
							if leading_link and opener.item == 0 {
								$stop = Failed
								$running = Bool.False
							} else {
								$items = $items.append(Chars([']']))
								$pos = after
							}
						}
					}
				}
			}
		} else if byte == '<' {
			match scan_autolink(input, $pos) {
				Ok(found) => {
					$items = flush_chars($items, $text).append(Node(found.node))
					$text = []
					$pos = found.end
				}

				Err(_) => {
					html = scan_raw_html(input, $pos, $html_skip)
					$html_skip = html.skip
					match html.end {
						Ok(end) => {
							raw = input.sublist({ start: $pos, len: end - $pos })
							$items = flush_chars($items, $text).append(Node(HtmlInline(Str.from_utf8_lossy(raw))))
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
					label = Str.from_utf8_lossy(raw)
					node = Link({ label: [Text(label)], target: { href: Str.concat("http://", label), title: None } })
					$items = flush_chars($items, $text).append(Node(node))
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
					url = Str.from_utf8_lossy(raw)
					node = Link({ label: [Text(url)], target: { href: url, title: None } })
					$items = flush_chars($items, $text.drop_last(found.rewind)).append(Node(node))
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

	match $stop {
		Finished => {
			nodes = process_emphasis(flush_chars($items, $text))
			# GFM email autolinks need an `@` in the text.
			{ nodes: if input.contains('@') autolink_emails_in(nodes) else nodes, stop: Finished }
		}

		_ =>
			{ nodes: [], stop: $stop }
		}
}

## Bytes that may start an inline construct (cmark's SPECIAL_CHARS plus the
## extension triggers `~`, `w` and `:`).
is_special_byte : U8 -> Bool
is_special_byte = |byte| {
	match byte {
		'\n' | '\r' | '\\' | '`' | '*' | '_' | '~' | '!' | '[' | ']' | '<' | '&' | 'w' | ':' => Bool.True
		_ => Bool.False
	}
}

find_special_byte : List(U8), U64 -> U64
find_special_byte = |input, from| {
	var $index = from
	while $index < input.len() and !is_special_byte(byte_at(input, $index)) {
		$index = $index + 1
	}
	$index
}

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
			var $ok = Bool.True
			var $offset = 1
			while $offset < width {
				continuation = byte_at(bytes, index + $offset).to_u32()
				if continuation < 0x80 or continuation > 0xBF {
					$ok = Bool.False
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
			Ok(Zs) => Bool.True
			_ => Bool.False
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
			Ok(Pc) | Ok(Pd) | Ok(Pe) | Ok(Pf) | Ok(Pi) | Ok(Po) | Ok(Ps) => Bool.True
			Ok(Sc) | Ok(Sk) | Ok(Sm) | Ok(So) => Bool.True
			_ => Bool.False
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
process_emphasis : List(InlineItem) -> List(Markdown.Inline)
process_emphasis = |items| {
	var $out = []
	var $stack = []
	var $bottoms = List.repeat(0, 18)
	for item in items {
		match item {
			Chars(bytes) => {
				$out = $out.append(Text(Str.from_utf8_lossy(bytes)))
			}

			Node(node) => {
				$out = $out.append(node)
			}

			Delim(delim) => {
				var $closer = delim
				var $searching = delim.can_close
				while $searching {
					match find_opener($stack, $bottoms, $closer) {
						Err(_) => {
							$bottoms = $bottoms.set(opener_bottom_key($closer), $stack.len()) ?? $bottoms
							$searching = Bool.False
						}

						Ok(index) => {
							opener = $stack.get(index) ?? { delim: $closer, at: $out.len() }
							# Delimiters between the opener and the closer become text.
							$out = fill_placeholders($out, $stack.drop_first(index + 1))
							$bottoms = $bottoms.map(|bottom| min_u64(bottom, index))
							$stack = $stack.take_first(index)
							if $closer.char == '~' {
								if opener.delim.count == $closer.count {
									children = merge_text_nodes($out.drop_first(opener.at + 1))
									$out = $out.take_first(opener.at).append(Strikethrough(children))
									$closer = { ..$closer, count: 0 }
								} else {
									# cmark-gfm: tilde runs of different lengths do not
									# pair; both, and the delimiters between them, stay text.
									$out = fill_placeholders($out, [opener])
									$closer = { ..$closer, can_open: Bool.False }
								}
								$searching = Bool.False
							} else {
								used = if $closer.count >= 2 and opener.delim.count >= 2 2 else 1
								children = merge_text_nodes($out.drop_first(opener.at + 1))
								node = if used == 2 Strong(children) else Emphasis(children)
								remaining = opener.delim.count - used
								if remaining == 0 {
									$out = $out.take_first(opener.at).append(node)
								} else {
									$out = $out.take_first(opener.at).append(Text("")).append(node)
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
				}
			}
		}
	}
	merge_text_nodes(fill_placeholders($out, $stack))
}

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
delimiter_text = |char, count| Text(Str.from_utf8_lossy(List.repeat(char, count)))

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
	for node in nodes {
		match node {
			Text(text) if text.is_empty() =>
				{}

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

## ---------------------------------------------------------------------------
## Code spans (CommonMark 6.1).
## ---------------------------------------------------------------------------

## All maximal backtick runs, sorted by length and then position.
backtick_runs : List(U8) -> List(TickRun)
backtick_runs = |input| {
	var $runs = []
	var $index = 0
	while $index < input.len() {
		if byte_at(input, $index) == '`' {
			end = skip_byte_run(input, $index, '`')
			$runs = $runs.append({ start: $index, len: end - $index })
			$index = end
		} else {
			$index = $index + 1
		}
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
	if $out.len() >= 2 and byte_at($out, 0) == ' ' and byte_at($out, $out.len() - 1) == ' ' and $out.any(|b| b != ' ') {
		$out.sublist({ start: 1, len: $out.len() - 2 })
	} else {
		$out
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
	Str.from_utf8_lossy($out)
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
	Str.from_utf8_lossy($out)
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
		var $scanning = Bool.True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == ']' {
				raw = input.sublist({ start: start + 1, len: $index - start - 1 })
				$result = Ok({ raw: trim_cmark_space(raw), end: $index + 1 })
				$scanning = Bool.False
			} else if byte == '[' {
				$scanning = Bool.False
			} else {
				$index =
					if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
						$index + 2
					} else {
						$index + 1
					}
				if $index - start - 1 > 1000 {
					$scanning = Bool.False
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
			Ok(found) => { title_value: Some(unescape_link_text(found.raw)), title_end: found.end }
			Err(_) => { title_value: None, title_end: dest.end }
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
		var $scanning = Bool.True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '>' {
				$result = Ok({ raw: input.sublist({ start: start + 1, len: $index - start - 1 }), end: $index + 1 })
				$scanning = Bool.False
			} else if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == '\n' or byte == '\r' or byte == '<' {
				$scanning = Bool.False
			} else {
				$index = $index + 1
			}
		}
		$result
	} else {
		var $index = start
		var $depth = 0
		var $failed = Bool.False
		var $scanning = Bool.True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == '(' {
				$depth = $depth + 1
				$index = $index + 1
				if $depth > 32 {
					$failed = Bool.True
					$scanning = Bool.False
				}
			} else if byte == ')' {
				if $depth == 0 {
					$scanning = Bool.False
				} else {
					$depth = $depth - 1
					$index = $index + 1
				}
			} else if byte <= ' ' or byte == 0x7F {
				$scanning = Bool.False
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
		var $scanning = Bool.True
		while $scanning and $index < input.len() {
			byte = byte_at(input, $index)
			if byte == '\\' and is_ascii_punctuation(byte_at(input, $index + 1)) and $index + 1 < input.len() {
				$index = $index + 2
			} else if byte == close {
				$result = Ok({ raw: input.sublist({ start: start + 1, len: $index - start - 1 }), end: $index + 1 })
				$scanning = Bool.False
			} else if open == '(' and byte == '(' {
				$scanning = Bool.False
			} else {
				$index = $index + 1
			}
		}
		$result
	}
}

## The destination and optional title of a link reference definition
## (CommonMark 4.7) given the text after `]:`. The destination must be
## non-empty unless written `<>`, a title must be separated from it by
## whitespace, and nothing may follow the title.
parse_reference_target : List(U8) -> Try(Markdown.LinkTarget, [NotFound])
parse_reference_target = |text| {
	start = skip_link_space(text, 0)
	dest = scan_link_destination(text, start)?
	if dest.end == start {
		Err(NotFound)
	} else {
		href = unescape_link_text(trim_cmark_space(dest.raw))
		title_start = skip_link_space(text, dest.end)
		if skip_spaces_tabs(text, dest.end) >= text.len() {
			Ok({ href, title: None })
		} else if title_start == dest.end {
			Err(NotFound)
		} else {
			title = scan_link_title(text, title_start)?
			if skip_spaces_tabs(text, title.end) >= text.len() {
				Ok({ href, title: Some(unescape_link_text(title.raw)) })
			} else {
				Err(NotFound)
			}
		}
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
			Ok({ node: Link({ label: [Text(text)], target: { href: text, title: None } }), end })
		}

		Err(_) => {
			end = scan_autolink_email(input, start + 1)?
			raw = input.sublist({ start: start + 1, len: end - start - 2 })
			text = unescape_entities(raw)
			Ok({ node: Link({ label: [Text(text)], target: { href: Str.concat("mailto:", text), title: None } }), end })
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
		var $scanning = Bool.True
		while $scanning {
			label_start = $cursor
			while $cursor < input.len() and (is_ascii_alnum(byte_at(input, $cursor)) or byte_at(input, $cursor) == '-') {
				$cursor = $cursor + 1
			}
			label_len = $cursor - label_start
			valid_label = label_len >= 1 and label_len <= 63 and is_ascii_alnum(byte_at(input, label_start)) and is_ascii_alnum(byte_at(input, $cursor - 1))
			if !valid_label {
				$scanning = Bool.False
			} else if byte_at(input, $cursor) == '.' and $cursor < input.len() {
				$cursor = $cursor + 1
			} else if byte_at(input, $cursor) == '>' and $cursor < input.len() {
				$result = Ok($cursor + 1)
				$scanning = Bool.False
			} else {
				$scanning = Bool.False
			}
		}
		$result
	}
}

is_email_local_byte : U8 -> Bool
is_email_local_byte = |byte| {
	if is_ascii_alnum(byte) {
		Bool.True
	} else {
		match byte {
			'.' | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '/' | '=' | '?' | '^' | '_' | '`' | '{' | '|' | '}' | '~' | '-' => Bool.True
			_ => Bool.False
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
					Err(_) => { end: Err(NotFound), skip: { ..skip, comment: Bool.True } }
				}
			}
		} else if starts_with_at(input, start + 2, "[CDATA[".to_utf8()) {
			if skip.cdata {
				{ end: Err(NotFound), skip }
			} else {
				match find_bytes(input, start + 9, "]]>".to_utf8()) {
					Ok(found) => { end: Ok(found + 3), skip }
					Err(_) => { end: Err(NotFound), skip: { ..skip, cdata: Bool.True } }
				}
			}
		} else if is_ascii_alpha(byte_at(input, start + 2)) {
			if skip.declaration {
				{ end: Err(NotFound), skip }
			} else {
				match find_bytes(input, start + 3, ">".to_utf8()) {
					Ok(found) => { end: Ok(found + 1), skip }
					Err(_) => { end: Err(NotFound), skip: { ..skip, declaration: Bool.True } }
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
				Err(_) => { end: Err(NotFound), skip: { ..skip, pi: Bool.True } }
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
	var $scanning = Bool.True
	while $scanning {
		after_space = skip_html_space(input, $index)
		next = byte_at(input, after_space)
		if next == '>' and after_space < input.len() {
			$result = Ok(after_space + 1)
			$scanning = Bool.False
		} else if next == '/' and byte_at(input, after_space + 1) == '>' {
			$result = Ok(after_space + 2)
			$scanning = Bool.False
		} else if after_space > $index and is_attribute_name_start(next) {
			match scan_attribute(input, after_space) {
				Ok(end) => { $index = end }
				Err(_) => { $scanning = Bool.False }
			}
		} else {
			$scanning = Bool.False
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
		' ' | '\t' | '\n' | '\r' | '"' | '\'' | '=' | '<' | '>' | '`' => Bool.False
		_ => Bool.True
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
		Bool.False
	} else {
		decoded = decode_utf8(input, index)
		if index >= input.len() or (decoded.scalar == 0xFFFD and decoded.width == 1) {
			Bool.False
		} else if decoded.scalar < 128 {
			!is_unicode_whitespace(decoded.scalar) and !is_ascii_punctuation(byte)
		} else if is_unicode_whitespace(decoded.scalar) {
			Bool.False
		} else {
			match general_category(decoded.scalar) {
				Ok(Pc) | Ok(Pd) | Ok(Pe) | Ok(Pf) | Ok(Pi) | Ok(Po) | Ok(Ps) => Bool.False
				_ => Bool.True
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
	var $scanning = Bool.True
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
			$scanning = Bool.False
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
	var $trimming = Bool.True
	while $trimming and $end > 0 {
		last = byte_at(input, start + $end - 1)
		if last == ')' {
			if $closing <= $opening {
				$trimming = Bool.False
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
			$trimming = Bool.False
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
		domain = gfm_check_domain(input, start, size, Bool.False)
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
			domain = gfm_check_domain(input, colon + 3, size - 3, Bool.True)
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
	var $running = Bool.True
	while $running {
		if $offset >= $remaining {
			$running = Bool.False
		} else {
			match find_bytes(data.take_first($start + $remaining), $start + $offset, ['@']) {
				Err(_) => {
					$running = Bool.False
				}

				Ok(at) => {
					var $max_rewind = at - ($start + $offset)
					var $auto_mailto = Bool.True
					var $is_xmpp = Bool.False
					var $periods = 0
					var $rewind = 0
					var $link_end = 0
					var $retry = Bool.True
					var $outcome = Skip(0)
					while $retry {
						$retry = Bool.False
						at_index = $start + $offset + $max_rewind
						$rewind = 0
						var $rewinding = Bool.True
						while $rewinding and $rewind < $max_rewind {
							c = byte_at(data, at_index - $rewind - 1)
							if is_ascii_alnum(c) or c == '.' or c == '+' or c == '-' or c == '_' {
								$rewind = $rewind + 1
							} else if c == ':' and gfm_validate_protocol("mailto:".to_utf8(), data, at_index, $rewind, $max_rewind) {
								$auto_mailto = Bool.False
								$rewind = $rewind + 1
							} else if c == ':' and gfm_validate_protocol("xmpp:".to_utf8(), data, at_index, $rewind, $max_rewind) {
								$auto_mailto = Bool.False
								$is_xmpp = Bool.True
								$rewind = $rewind + 1
							} else {
								$rewinding = Bool.False
							}
						}
						if $rewind == 0 {
							$outcome = Skip($max_rewind + 1)
						} else {
							limit = $remaining - $offset - $max_rewind
							$link_end = 1
							var $forward = Bool.True
							while $forward and $link_end < limit {
								c = byte_at(data, at_index + $link_end)
								if is_ascii_alnum(c) {
									$link_end = $link_end + 1
								} else if c == '@' {
									$offset = $offset + $max_rewind + 1
									$max_rewind = $link_end - 1
									$retry = Bool.True
									$forward = Bool.False
								} else if c == '.' and $link_end < limit - 1 and is_ascii_alnum(byte_at(data, at_index + $link_end + 1)) {
									$periods = $periods + 1
									$link_end = $link_end + 1
								} else if c == '/' and $is_xmpp {
									$link_end = $link_end + 1
								} else if c != '-' and c != '_' {
									$forward = Bool.False
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
							email = Str.from_utf8_lossy(data.sublist({ start: link_start, len: $link_end + $rewind }))
							href = if $auto_mailto Str.concat("mailto:", email) else email
							before = data.sublist({ start: $start, len: link_start - $start })
							$out = $out.append(Text(Str.from_utf8_lossy(before))).append(Link({ label: [Text(email)], target: { href, title: None } }))
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
	merge_text_nodes($out.append(Text(Str.from_utf8_lossy(rest))))
}

gfm_validate_protocol : List(U8), List(U8), U64, U64, U64 -> Bool
gfm_validate_protocol = |protocol, data, at_index, rewind, max_rewind| {
	len = protocol.len()
	if len > max_rewind - rewind {
		Bool.False
	} else if data.sublist({ start: at_index - rewind - len, len }) != protocol {
		Bool.False
	} else if len == max_rewind - rewind {
		Bool.True
	} else {
		!is_ascii_alnum(byte_at(data, at_index - rewind - len - 1))
	}
}

parse_link_target : String.Utf8 -> Markdown.LinkTarget
parse_link_target = |raw| {
	clean = trim_spaces(raw)
	parts = split_first_space(clean)

	title =
		if parts.rest.is_empty() {
			None
		} else {
			Some(String.str_from_utf8(strip_wrapping_quotes(trim_spaces(parts.rest))))
		}

	{ href: String.str_from_utf8(parts.first), title }
}

find_sequence : String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_sequence = |input, needle| {
	find_sequence_help(input, needle, [])
}

find_sequence_help : String.Utf8, String.Utf8, String.Utf8 -> Try({ before : String.Utf8, after : String.Utf8 }, [NotFound])
find_sequence_help = |input, needle, acc| {
	if starts_with_bytes(input, needle) {
		Ok({ before: acc, after: input.drop_first(needle.len()) })
	} else {
		match input {
			[] =>
				Err(NotFound)

			[first, .. as rest] =>
				find_sequence_help(rest, needle, acc.append(first))
			}
	}
}

bytes_are_blank : String.Utf8 -> Bool
bytes_are_blank = |bytes| {
	match bytes {
		[] =>
			Bool.True

		[' ', .. as rest] =>
			bytes_are_blank(rest)

		['\t', .. as rest] =>
			bytes_are_blank(rest)

		_ =>
			Bool.False
		}
}

starts_with_bytes : String.Utf8, String.Utf8 -> Bool
starts_with_bytes = |input, prefix| {
	{ before: start, others: _ } = input.split_at(prefix.len())
	start == prefix
}

ends_with_byte : String.Utf8, U8 -> Bool
ends_with_byte = |bytes, expected| {
	ends_with_byte_help(bytes, expected, Err(NotFound))
}

ends_with_byte_help : String.Utf8, U8, Try(U8, [NotFound]) -> Bool
ends_with_byte_help = |bytes, expected, last| {
	match bytes {
		[] =>
			last == Ok(expected)

		[first, .. as rest] =>
			ends_with_byte_help(rest, expected, Ok(first))
		}
}

append_bytes : String.Utf8, String.Utf8 -> String.Utf8
append_bytes = |left, right| {
	match right {
		[] =>
			left

		[first, .. as rest] =>
			append_bytes(left.append(first), rest)
		}
}

join_lines_with_newlines : List(String.Utf8) -> String.Utf8
join_lines_with_newlines = |lines| {
	join_lines_with_newlines_help(lines, [])
}

join_lines_with_newlines_help : List(String.Utf8), String.Utf8 -> String.Utf8
join_lines_with_newlines_help = |lines, acc| {
	match lines {
		[] =>
			acc

		[line, .. as rest] =>
			join_lines_with_newlines_help(rest, append_bytes(acc, line).append('\n'))
		}
}

trim_spaces : String.Utf8 -> String.Utf8
trim_spaces = |bytes| {
	trim_end_spaces(trim_start_spaces(bytes))
}

trim_start_spaces : String.Utf8 -> String.Utf8
trim_start_spaces = |bytes| {
	match bytes {
		[' ', .. as rest] =>
			trim_start_spaces(rest)

		['\t', .. as rest] =>
			trim_start_spaces(rest)

		_ =>
			bytes
		}
}

trim_end_spaces : String.Utf8 -> String.Utf8
trim_end_spaces = |bytes| {
	trim_end_spaces_help(bytes, [], [])
}

trim_end_spaces_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_end_spaces_help = |bytes, out, pending_spaces| {
	match bytes {
		[] =>
			out

		[' ', .. as rest] =>
			trim_end_spaces_help(rest, out, pending_spaces.append(' '))

		['\t', .. as rest] =>
			trim_end_spaces_help(rest, out, pending_spaces.append('\t'))

		[first, .. as rest] =>
			trim_end_spaces_help(rest, append_bytes(out, pending_spaces).append(first), [])
		}
}

trim_closing_heading_marker : String.Utf8 -> String.Utf8
trim_closing_heading_marker = |bytes| {
	trim_closing_heading_marker_help(trim_spaces(bytes), [], [])
}

trim_closing_heading_marker_help : String.Utf8, String.Utf8, String.Utf8 -> String.Utf8
trim_closing_heading_marker_help = |bytes, out, pending_hashes| {
	match bytes {
		[] if pending_hashes.is_empty() =>
			out

		[] =>
			trim_spaces(out)

		['#', .. as rest] =>
			trim_closing_heading_marker_help(rest, out, pending_hashes.append('#'))

		[first, .. as rest] =>
			trim_closing_heading_marker_help(rest, append_bytes(out, pending_hashes).append(first), [])
		}
}

split_first_space : String.Utf8 -> { first : String.Utf8, rest : String.Utf8 }
split_first_space = |bytes| {
	split_first_space_help(bytes, [])
}

split_first_space_help : String.Utf8, String.Utf8 -> { first : String.Utf8, rest : String.Utf8 }
split_first_space_help = |bytes, first_part| {
	match bytes {
		[] =>
			{ first: first_part, rest: [] }

		[' ', .. as rest] =>
			{ first: first_part, rest }

		['\t', .. as rest] =>
			{ first: first_part, rest }

		[first, .. as rest] =>
			split_first_space_help(rest, first_part.append(first))
		}
}

strip_wrapping_quotes : String.Utf8 -> String.Utf8
strip_wrapping_quotes = |bytes| {
	match bytes {
		['"', .. as rest] => {
			found = find_sequence(rest, "\"".to_utf8()) ?? { before: rest, after: [] }
			found.before
		}

		['\'', .. as rest] => {
			found = find_sequence(rest, "'".to_utf8()) ?? { before: rest, after: [] }
			found.before
		}

		_ =>
			bytes
		}
}

## Link label matching (CommonMark 4.7): Unicode case fold, strip leading and
## trailing whitespace, and collapse internal whitespace runs to one space.
normalize_reference_label : String.Utf8 -> Str
normalize_reference_label = |label| {
	source = Str.from_utf8_lossy(label)
	folded =
		match Case.fold(source, Case.full, Case.unlimited_limits) {
			Ok(result) => Case.result_text(result)
			Err(_) => source
		}
	var $out = []
	var $pending_space = Bool.False
	for byte in folded.to_utf8() {
		if is_cmark_space(byte) {
			$pending_space = Bool.True
		} else {
			if $pending_space and !$out.is_empty() {
				$out = $out.append(' ')
			}
			$pending_space = Bool.False
			$out = $out.append(byte)
		}
	}
	Str.from_utf8_lossy($out)
}

lower_ascii_bytes : String.Utf8 -> String.Utf8
lower_ascii_bytes = |bytes| {
	List.from_iter(bytes.iter().map(lower_ascii_byte))
}

lower_ascii_byte : U8 -> U8
lower_ascii_byte = |byte| {
	if byte >= 'A' and byte <= 'Z' {
		byte + 32
	} else {
		byte
	}
}

collapse_reference_whitespace : String.Utf8, String.Utf8, Bool -> String.Utf8
collapse_reference_whitespace = |bytes, out, pending_space| {
	match bytes {
		[] =>
			out

		[first, .. as rest] if first == ' ' or first == '\t' or first == '\n' =>
			collapse_reference_whitespace(rest, out, Bool.True)

		[first, .. as rest] if pending_space and !out.is_empty() =>
			collapse_reference_whitespace(rest, out.append(' ').append(first), Bool.False)

		[first, .. as rest] =>
			collapse_reference_whitespace(rest, out.append(first), Bool.False)
		}
}

count_leading_byte : String.Utf8, U8, U64 -> U64
count_leading_byte = |bytes, expected, count| {
	match bytes {
		[first, .. as rest] if first == expected =>
			count_leading_byte(rest, expected, count + 1)

		_ =>
			count
		}
}

digits_to_u64 : String.Utf8 -> U64
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

end_of_line : Parser(String.Utf8, Str)
end_of_line = Parser.one_of([String.string("\n"), String.string("\r\n")])

not_end_of_line : U8 -> Bool
not_end_of_line = |b| {
	b != '\n' and b != '\r'
}

todo : Parser(String.Utf8, Markdown)
todo =
	Parser.const(|s| TODO(s))
		.keep(Parser.chomp_while(not_end_of_line).map(String.str_from_utf8))

## Unsupported markdown lines can still be preserved as TODO nodes directly.
expect {
	a = String.parse_str(todo, "Foo Bar")?
	a == TODO("Foo Bar")
}

## Small nominal support types expose equality, debug, and string helpers.
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

## Inline and block AST nodes expose useful inspect strings and structural equality.
expect {
	inline : Markdown.Inline
	inline = Strong([Text("Roc")])

	block : Markdown
	block = Heading({ level: Two, content: [Text("Roc")] })

	actual =
		\\inline inspect: ${Str.inspect(inline)}
		\\inline debug: ${Markdown.inline_to_debug_str(inline)}
		\\inline eq same: ${Str.inspect(inline == Strong([Text("Roc")]))}
		\\inline eq different: ${Str.inspect(inline == Emphasis([Text("Roc")]))}
		\\block inspect: ${Str.inspect(block)}
		\\block debug: ${Markdown.to_debug_str(block)}
		\\block eq same: ${Str.inspect(block == Heading({ level: Two, content: [Text("Roc")] }))}
		\\block eq different: ${Str.inspect(block == Paragraph([Text("Roc")]))}

	expected =
		\\inline inspect: Strong([Text("Roc")])
		\\inline debug: Strong([Text("Roc")])
		\\inline eq same: True
		\\inline eq different: False
		\\block inspect: Heading({ level: Two, content: [Text("Roc")] })
		\\block debug: Heading({ level: Two, content: [Text("Roc")] })
		\\block eq same: True
		\\block eq different: False

	actual == expected
}

## Hash-prefixed headings parse with inline content.
expect {
	a = String.parse_str(Markdown.heading, "# Foo **Bar** #")?
	a == Heading({ level: One, content: [Text("Foo "), Strong([Text("Bar")])] })
}

## Underlined headings parse as level two headings.
expect {
	a = String.parse_str(Markdown.heading, "Foo Bar\n---")?
	a == Heading({ level: Two, content: [Text("Foo Bar")] })
}

## Markdown links parse as inline link nodes.
expect {
	a = String.parse_str(Markdown.link, "[roc](https://roc-lang.org \"Roc\")")?
	a == Link({ label: [Text("roc")], target: { href: "https://roc-lang.org", title: Some("Roc") } })
}

## Markdown images parse as inline image nodes.
expect {
	a = String.parse_str(Markdown.image, "![alt text](/images/logo.png)")?
	a == Image({ alt: [Text("alt text")], target: { href: "/images/logo.png", title: None } })
}

## Code blocks capture their info string and body text.
expect {
	text =
		\\```roc
		\\# some code
		\\foo = bar
		\\```

	a = String.parse_str(Markdown.code, text)?
	a == Code({ info: "roc", pre: "# some code\nfoo = bar\n" })
}

## Public inline parser parses emphasis, strong, strikethrough, code, and links.
expect {
	actual = String.parse_str(Markdown.inlines, "Intro with **bold**, *em*, ~~gone~~, `code`, and [a link](https://example.com).")?

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
			Link({ label: [Text("a link")], target: { href: "https://example.com", title: None } }),
			Text("."),
		]
}

## Escaped inline delimiters parse as literal text.
expect {
	text = "\\*literal\\*, \\_em\\_, \\`code\\`, and \\[a link](target)"

	actual = String.parse_str(Markdown.inlines, text)?

	actual == [Text("*literal*, _em_, `code`, and [a link](target)")]
}

## Inline images parse inside prose.
expect {
	actual = String.parse_str(Markdown.inlines, "Logo ![Roc](/roc.png) here")?

	actual
		== [
			Text("Logo "),
			Image({ alt: [Text("Roc")], target: { href: "/roc.png", title: None } }),
			Text(" here"),
		]
}

## Autolinks and bare URLs parse as inline links.
expect {
	actual = String.parse_str(Markdown.inlines, "<https://example.com> and www.example.com")?

	actual
		== [
			Link({ label: [Text("https://example.com")], target: { href: "https://example.com", title: None } }),
			Text(" and "),
			Link({ label: [Text("www.example.com")], target: { href: "http://www.example.com", title: None } }),
		]
}

## Hard line breaks parse from trailing spaces and backslash newlines.
expect {
	actual = String.parse_str(Markdown.inlines, "one  \ntwo\\\nthree")?

	actual == [Text("one"), HardBreak, Text("two"), HardBreak, Text("three")]
}

## Raw HTML inline spans are preserved.
expect {
	actual = String.parse_str(Markdown.inlines, "Hello <span>world</span>")?

	actual == [Text("Hello "), HtmlInline("<span>"), Text("world"), HtmlInline("</span>")]
}

inline_test : Str -> List(Markdown.Inline)
inline_test = |text| String.parse_str(Markdown.inlines, text) ?? [Text("<parse error>")]

## Emphasis follows the delimiter-run algorithm (CommonMark 6.2): flanking,
## intraword underscores, the rule of three and nesting.
expect inline_test("*foo bar *") == [Text("*foo bar *")]
expect inline_test("foo_bar_") == [Text("foo_bar_")]
expect inline_test("*foo**bar**baz*") == [Emphasis([Text("foo"), Strong([Text("bar")]), Text("baz")])]
expect inline_test("*foo**bar*") == [Emphasis([Text("foo**bar")])]
expect inline_test("***foo** bar*") == [Emphasis([Strong([Text("foo")]), Text(" bar")])]
expect inline_test("**foo*") == [Text("*"), Emphasis([Text("foo")])]
expect inline_test("foo******bar*********baz") == [Text("foo"), Strong([Strong([Strong([Text("bar")])])]), Text("***baz")]
expect inline_test("*(*foo*)*") == [Emphasis([Text("("), Emphasis([Text("foo")]), Text(")")])]

## Unicode punctuation includes symbols (CommonMark 0.31.2), so a currency sign
## next to `*` makes the run left-flanking only.
expect inline_test("*£*bravo.") == [Text("*£*bravo.")]
expect inline_test("a\u(A0)*b*") == [Text("a\u(A0)"), Emphasis([Text("b")])]

## Code spans take precedence over emphasis and links; backtick runs must match.
expect inline_test("*a `*` b*") == [Emphasis([Text("a "), InlineCode("*"), Text(" b")])]
expect inline_test("``foo`bar``") == [InlineCode("foo`bar")]
expect inline_test("`a``b`` c") == [Text("`a"), InlineCode("b"), Text(" c")]
expect inline_test("` `` `") == [InlineCode("``")]
expect inline_test("`foo\nbar`") == [InlineCode("foo bar")]

## Backslash escapes and entity references decode to text.
expect inline_test("\\*not emphasis\\* \\a") == [Text("*not emphasis* \\a")]
expect inline_test("&copy; &#35; &#X22; &#0; &nosuch;") == [Text("© # \" \u(FFFD) &nosuch;")]
expect inline_test("&amp;ouml;") == [Text("&ouml;")]

## Links: destinations, titles, nesting rules and precedence.
expect inline_test("[a](<b c> \"t\")") == [Link({ label: [Text("a")], target: { href: "b c", title: Some("t") } })]
expect inline_test("[a](b(c)d)") == [Link({ label: [Text("a")], target: { href: "b(c)d", title: None } })]

## Whitespace around a `<...>` destination is not part of the URL (as in
## cmark and markdown-it, and WHATWG URL parsing); escaped content is kept.
expect inline_test("[a](<  b c  >) [d](< \\) >)") == [Link({ label: [Text("a")], target: { href: "b c", title: None } }), Text(" "), Link({ label: [Text("d")], target: { href: ")", title: None } })]
expect inline_test("[a](/u\\*v &auml;)") == [Text("[a](/u*v ä)")]
expect inline_test("[a](/u\\*v '&auml;')") == [Link({ label: [Text("a")], target: { href: "/u*v", title: Some("ä") } })]
expect inline_test("[foo [bar](/u)](/v)") == [Text("[foo "), Link({ label: [Text("bar")], target: { href: "/u", title: None } }), Text("](/v)")]
expect inline_test("![a [b](/u)](/v)") == [Image({ alt: [Text("a "), Link({ label: [Text("b")], target: { href: "/u", title: None } })], target: { href: "/v", title: None } })]
expect inline_test("*[foo*](/u)") == [Text("*"), Link({ label: [Text("foo*")], target: { href: "/u", title: None } })]
expect inline_test("[foo`](/u)`") == [Text("[foo"), InlineCode("](/u)")]

## Public link/image parsers consume one leading inline link.
expect String.parse_str_partial(Markdown.link, "[a *b*](/u) rest") == Ok({ val: Link({ label: [Text("a "), Emphasis([Text("b")])], target: { href: "/u", title: None } }), input: " rest" })
expect String.parse_str_partial(Markdown.image, "![a](/i.png \"T\")!") == Ok({ val: Image({ alt: [Text("a")], target: { href: "/i.png", title: Some("T") } }), input: "!" })
expect String.parse_str_partial(Markdown.link, "[a][b]").is_err()

## Reference labels match case-insensitively after Unicode case folding.
expect {
	actual = String.parse_str(Markdown.all, "[ẞ]\n\n[SS]: /url")?
	actual == [Paragraph([Link({ label: [Text("ẞ")], target: { href: "/url", title: None } })])]
}

## Reference definitions decode escapes and entities and reject bracketed labels.
expect {
	actual = String.parse_str(Markdown.all, "[foo]\n\n[foo]: /f&ouml;\\* \"t\\*\"")?
	actual == [Paragraph([Link({ label: [Text("foo")], target: { href: "/fö*", title: Some("t*") } })])]
}

expect {
	actual = String.parse_str(Markdown.all, "[a[b]\n\n[a[b]: /u")?
	actual == [Paragraph([Text("[a[b]")]), Paragraph([Text("[a[b]: /u")])]
}

## Autolinks and raw HTML follow the CommonMark grammars.
expect inline_test("<http://a.b/c?d=1&amp;e> <a@b.co>") == [Link({ label: [Text("http://a.b/c?d=1&e")], target: { href: "http://a.b/c?d=1&e", title: None } }), Text(" "), Link({ label: [Text("a@b.co")], target: { href: "mailto:a@b.co", title: None } })]
expect inline_test("<a href=\"x\" b> <!-- c --> <?p?> <!X y> <![CDATA[z]]> </a >") == [HtmlInline("<a href=\"x\" b>"), Text(" "), HtmlInline("<!-- c -->"), Text(" "), HtmlInline("<?p?>"), Text(" "), HtmlInline("<!X y>"), Text(" "), HtmlInline("<![CDATA[z]]>"), Text(" "), HtmlInline("</a >")]
expect inline_test("<a b='c' d> <1a> <a =b>") == [HtmlInline("<a b='c' d>"), Text(" <1a> <a =b>")]

## Pathological shapes stay linear: many open brackets, deeply nested images
## and long delimiter runs that match two characters at a time.
expect inline_test(Str.repeat("[a", 20000)) == [Text(Str.repeat("[a", 20000))]
expect inline_test("${Str.repeat("![", 5000)}a${Str.repeat("](u)", 5000)}").len() == 1
expect inline_test("${Str.repeat("*", 20000)}a${Str.repeat("*", 20000)}").len() == 1

## Soft and hard line breaks.
expect inline_test("a  \n   b\\\nc \nd") == [Text("a"), HardBreak, Text("b"), HardBreak, Text("c\nd")]
expect inline_test("  a  ") == [Text("a")]

## Only spaces and tabs are stripped around line endings; vertical tab and
## form feed are text. Leading blank lines are not paragraph content.
expect inline_test("\n\r\na\u(B)\nb\u(C)") == [Text("a\u(B)\nb\u(C)")]

## GFM strikethrough pairs runs of equal length (one or two tildes).
expect inline_test("~~a~~ ~b~ ~~~c~~~") == [Strikethrough([Text("a")]), Text(" "), Strikethrough([Text("b")]), Text(" ~~~c~~~")]
expect inline_test("~~a~ b~~") == [Text("~~a~ b~~")]

## GFM extended autolinks: www., scheme:// and bare email addresses.
expect inline_test("see www.a.b/c). and http://x.y/z?") == [Text("see "), Link({ label: [Text("www.a.b/c")], target: { href: "http://www.a.b/c", title: None } }), Text("). and "), Link({ label: [Text("http://x.y/z")], target: { href: "http://x.y/z", title: None } }), Text("?")]
expect inline_test("mail a.b+c@d.ef.") == [Text("mail "), Link({ label: [Text("a.b+c@d.ef")], target: { href: "mailto:a.b+c@d.ef", title: None } }), Text(".")]
expect inline_test("[x www.a.b](/u)") == [Link({ label: [Text("x www.a.b")], target: { href: "/u", title: None } })]

## Reference links resolve using definitions and definitions are omitted from blocks.
expect {
	text =
		\\[roc]: https://roc-lang.org "Roc"
		\\Read [Roc][roc].

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			Paragraph([
				Text("Read "),
				Link({ label: [Text("Roc")], target: { href: "https://roc-lang.org", title: Some("Roc") } }),
				Text("."),
			]),
		]
}

## Unresolved references remain literal text.
expect {
	actual = String.parse_str(Markdown.all, "Read [Roc][missing].")?

	actual == [Paragraph([Text("Read [Roc][missing].")])]
}

## Frontmatter is preserved only at the start of a document.
expect {
	text =
		\\---
		\\title: Hello
		\\---
		\\Body

	actual = String.parse_str(Markdown.all, text)?

	actual == [Frontmatter({ raw: "title: Hello\n" }), Paragraph([Text("Body")])]
}

## Standalone thematic breaks parse as block nodes.
expect {
	actual = String.parse_str(Markdown.all, "- - -")?

	actual == [ThematicBreak]
}

## Ordered lists preserve their starting number and task list state.
expect {
	text =
		\\3) [x] Done
		\\4) [ ] Later

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Ordered({ start: 3 }),
				loose: Bool.False,
				items: [
					{ task: Checked, blocks: [Paragraph([Text("Done")])] },
					{ task: Unchecked, blocks: [Paragraph([Text("Later")])] },
				],
			}),
		]
}

## Changing the bullet character starts a new list (CommonMark example 301).
expect {
	text =
		\\* One
		\\+ Two

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({ kind: Unordered, loose: Bool.False, items: [{ task: NoTask, blocks: [Paragraph([Text("One")])] }] }),
			ListBlock({ kind: Unordered, loose: Bool.False, items: [{ task: NoTask, blocks: [Paragraph([Text("Two")])] }] }),
		]
}

## Blank lines inside lists mark the list as loose.
expect {
	text =
		\\- One
		\\
		\\- Two

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: Bool.True,
				items: [
					{ task: NoTask, blocks: [Paragraph([Text("One")])] },
					{ task: NoTask, blocks: [Paragraph([Text("Two")])] },
				],
			}),
		]
}

## Nested unordered lists parse as child blocks.
expect {
	text =
		\\- One
		\\  - Nested

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			ListBlock({
				kind: Unordered,
				loose: Bool.False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: Bool.False,
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

## Tilde fenced code blocks parse with info strings.
expect {
	text =
		\\~~~roc
		\\main = 1
		\\~~~

	actual = String.parse_str(Markdown.all, text)?

	actual == [Code({ info: "roc", pre: "main = 1\n" })]
}

## Indented code blocks parse from four leading spaces.
expect {
	actual = String.parse_str(Markdown.all, "    main = 1")?

	actual == [Code({ info: "", pre: "main = 1\n" })]
}

## Pipe tables parse header alignment and inline cell content. A pipe inside a
## code span must still be escaped (GFM example 200).
expect {
	text =
		\\| Name | Count |
		\\| :--- | ---: |
		\\| **Roc** | `1\\|2` |

	actual = String.parse_str(Markdown.all, text)?

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

## Malformed tables remain paragraph text.
expect {
	text =
		\\Name | Count
		\\not a delimiter

	actual = String.parse_str(Markdown.all, text)?

	actual == [Paragraph([Text("Name | Count\nnot a delimiter")])]
}

## Raw HTML blocks are preserved without validation.
expect {
	text =
		\\<section>
		\\raw
		\\</section>

	actual = String.parse_str(Markdown.all, text)?

	actual == [HtmlBlock("<section>\nraw\n</section>\n")]
}

## An unclosed fenced code block runs to the end of the document (CommonMark
## example 96); Markdown documents never fail to parse.
expect {
	text =
		\\```roc
		\\main = 1

	actual = String.parse_str(Markdown.all, text)?

	actual == [Code({ info: "roc", pre: "main = 1\n" })]
}

## A closing `</pre>`, `</script>`, `</style>` or `</textarea>` tag alone on a
## line starts a type 7 HTML block; only the open tags are excluded from type
## 7 (CommonMark 4.6).
expect {
	actual = String.parse_str(Markdown.all, "a\n\n</textarea>\nb\n\n<pre class=\"x\">\n")?

	actual == [Paragraph([Text("a")]), HtmlBlock("</textarea>\nb\n"), HtmlBlock("<pre class=\"x\">\n")]
}

## Article body markdown parses into structured blocks without TODO fallbacks.
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

	actual = String.parse_str(Markdown.all, text)?

	actual
		== [
			Frontmatter({ raw: "title: Article\n" }),
			Heading({ level: One, content: [Text("Title with "), Strong([Text("style")])] }),
			Paragraph([
				Text("Intro with "),
				Strong([Text("bold")]),
				Text(", "),
				InlineCode("code"),
				Text(", and "),
				Link({ label: [Text("a link")], target: { href: "https://example.com", title: None } }),
				Text("."),
			]),
			Paragraph([Image({ alt: [Text("alt text")], target: { href: "/image.png", title: None } })]),
			Code({ info: "roc", pre: "main = 1\n" }),
			ListBlock({
				kind: Unordered,
				loose: Bool.False,
				items: [
					{
						task: NoTask,
						blocks: [
							Paragraph([Text("One")]),
							ListBlock({
								kind: Unordered,
								loose: Bool.False,
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

chomp_until_code_block_end : Parser(String.Utf8, Str)
chomp_until_code_block_end =
	Parser.build_primitive_parser(
		|input| {
			chomp_to_code_block_end_help({ val: List.with_capacity(1000), input })
		},
	)
		.map(String.str_from_utf8)

chomp_to_code_block_end_help : { val : String.Utf8, input : String.Utf8 } -> Parser.ParseResult(String.Utf8, String.Utf8)
chomp_to_code_block_end_help = |{ val, input }| {
	match input {
		[] => Err(ParsingFailure("expected ```, ran out of input"))
		['`', '`', '`', .. as rest] => Ok({ val, input: rest })
		[first, .. as rest] => chomp_to_code_block_end_help({ val: val.append(first), input: rest })
	}
}

## Chomping code block contents stops before the closing backticks.
expect {
	val = "".to_utf8()
	input = "some code\n```".to_utf8()
	expected = "some code\n".to_utf8()
	a = chomp_to_code_block_end_help({ val, input })?
	a == { val: expected, input: [] }
}
