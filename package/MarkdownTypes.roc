# TODO(https://github.com/roc-lang/roc/issues/13023): return these backing declarations to Markdown.roc
# once public nominal aliases are recognized by exposed-field validation.
# Keeping them in a separate module preserves the public Markdown aliases and
# their nominal types without a false private-type warning.
MarkdownTypes :: [].{
	LinkTarget : { href : Str, title : Try(Str, [Missing]) }

	Inline := [
		Text(Str),
		Strong(List(MarkdownTypes.Inline)),
		Emphasis(List(MarkdownTypes.Inline)),
		Strikethrough(List(MarkdownTypes.Inline)),
		InlineCode(Str),
		Link({ label : List(MarkdownTypes.Inline), target : MarkdownTypes.LinkTarget }),
		Image({ alt : List(MarkdownTypes.Inline), target : MarkdownTypes.LinkTarget }),
		HardBreak,
		HtmlInline(Str),
	].{

		## Render an inline node in Roc source-like notation for inspection.
		to_inspect : MarkdownTypes.Inline -> Str
		to_inspect = |inline| {
			inspect_inline(inline)
		}

		## Compare two inline syntax trees structurally.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	Alignment := [
		Default,
		Left,
		Center,
		Right,
	].{

		## Render a table alignment for inspection.
		to_inspect : MarkdownTypes.Alignment -> Str
		to_inspect = |alignment| {
			inspect_alignment(alignment)
		}

		## Convert a table alignment to a compact string.
		to_str : MarkdownTypes.Alignment -> Str
		to_str = |alignment| {
			alignment_to_str(alignment)
		}

		## Compare two table alignments.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	TaskState := [
		NoTask,
		Unchecked,
		Checked,
	].{

		## Render a task state for inspection.
		to_inspect : MarkdownTypes.TaskState -> Str
		to_inspect = |task| {
			inspect_task_state(task)
		}

		## Convert a task state to a compact string.
		to_str : MarkdownTypes.TaskState -> Str
		to_str = |task| {
			task_state_to_str(task)
		}

		## Compare two task states.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	ListKind := [
		Unordered,
		Ordered({ start : U64 }),
	].{

		## Render a list kind for inspection.
		to_inspect : MarkdownTypes.ListKind -> Str
		to_inspect = |kind| {
			inspect_list_kind(kind)
		}

		## Convert a list kind to a compact string.
		to_str : MarkdownTypes.ListKind -> Str
		to_str = |kind| {
			list_kind_to_str(kind)
		}

		## Compare two list kinds structurally.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

	Level := [One, Two, Three, Four, Five, Six].{

		## Render a heading level for inspection.
		to_inspect : MarkdownTypes.Level -> Str
		to_inspect = |level| {
			inspect_level(level)
		}

		## Convert a heading level to its decimal representation.
		to_str : MarkdownTypes.Level -> Str
		to_str = |level| {
			level_to_str(level)
		}

		## Compare two heading levels.
		is_eq : _

		## Hash this value, so that it can be a `Dict` key or `Set` element.
		to_hash : _
	}

}

inspect_level : MarkdownTypes.Level -> Str
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

level_to_str : MarkdownTypes.Level -> Str
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

inspect_list_kind : MarkdownTypes.ListKind -> Str
inspect_list_kind = |kind| {
	match kind {
		Unordered =>
			"Unordered"

		Ordered({ start }) =>
			"Ordered({ start: ${start.to_str()} })"
	}
}

list_kind_to_str : MarkdownTypes.ListKind -> Str
list_kind_to_str = |kind| {
	match kind {
		Unordered =>
			"unordered"

		Ordered({ start }) =>
			"ordered:${start.to_str()}"
	}
}

inspect_task_state : MarkdownTypes.TaskState -> Str
inspect_task_state = |task| {
	match task {
		NoTask => "NoTask"
		Unchecked => "Unchecked"
		Checked => "Checked"
	}
}

task_state_to_str : MarkdownTypes.TaskState -> Str
task_state_to_str = |task| {
	match task {
		NoTask => "none"
		Unchecked => "unchecked"
		Checked => "checked"
	}
}

inspect_alignment : MarkdownTypes.Alignment -> Str
inspect_alignment = |alignment| {
	match alignment {
		Default => "Default"
		Left => "Left"
		Center => "Center"
		Right => "Right"
	}
}

alignment_to_str : MarkdownTypes.Alignment -> Str
alignment_to_str = |alignment| {
	match alignment {
		Default => "default"
		Left => "left"
		Center => "center"
		Right => "right"
	}
}

inspect_inline : MarkdownTypes.Inline -> Str
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

inspect_link_target : MarkdownTypes.LinkTarget -> Str
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
