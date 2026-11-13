import Markup
import HtmlTokenizer as Tok
import Literal
import MarkupHtmlError

## HTML tree construction. The open-elements and active-formatting stacks are
## separate from the indexed output tree so recovery can reparent existing nodes.
HtmlTree :: [].{
	Context := { name : Str ?? "body", namespace : Str ?? "http://www.w3.org/1999/xhtml", scripting : Bool ?? False }
	parse : Str, Bool, Context, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	parse = |source, fragment, context, props| {
		if props.max_diagnostics == 0 {
			return Err(InvalidMarkup({ problem: ResourceLimit("max_diagnostics"), line: 1, column: 1 }))
		}
		if source.count_utf8_bytes() > props.max_source_bytes {
			return Err(InvalidMarkup({ problem: SourceTooLarge(props.max_source_bytes), line: 1, column: 1 }))
		}
		initial : State
		initial = { nodes: [], roots: [], stack: [], active: [], modes: [], mode: Initial, head: Err(Missing), form: Err(Missing), fragment, context, diagnostics: [], max_diagnostics: props.max_diagnostics, foster: False, frameset_ok: True, skip_lf: False, work: 0, work_limit: work_limit(props.max_work_per_byte, source.count_utf8_bytes()), element_count: 0, diagnostics_exceeded: False }
		var $state = initial
		if fragment {
			$state = insert_element($state, "html", Markup.html_namespace, [], Implied(0), True)
			$state = { ..$state, mode: context_mode(context.name) }
		}
		var $reader = { bytes: source.to_utf8(), pos: 0, diagnostics: [] }
		var $done = False
		while !$done {
			mode = content_mode($state)
			foreign = adjusted_namespace($state) != Markup.html_namespace
			next = Tok.next($reader, mode, foreign)
			$reader = next.reader
			if $state.skip_lf {
				$state = { ..$state, skip_lf: False }
				match next.token.kind {
					Characters(text) => {
						bytes = text.to_utf8()
						if bytes.first() == Ok('\n') {
							trimmed = { ..next.token, kind: Characters(Str.from_utf8_lossy(bytes.drop_first(1))) }
							$state = process($state, trimmed, context.scripting)
						} else {
							$state = process($state, next.token, context.scripting)
						}
					}
					_ => {
						$state = process($state, next.token, context.scripting)
					}
				}
			} else {
				$state = process($state, next.token, context.scripting)
			}
			at = Literal.position(source, next.token.span.start)
			if $state.stack.len() > props.max_depth {
				return Err(InvalidMarkup({ problem: LimitExceeded(Depth), line: at.line, column: at.column }))
			}
			if $state.element_count > props.max_elements {
				return Err(InvalidMarkup({ problem: LimitExceeded(Elements), line: at.line, column: at.column }))
			}
			if $state.nodes.len() > props.max_nodes {
				return Err(InvalidMarkup({ problem: ResourceLimit("max_nodes"), line: at.line, column: at.column }))
			}
			if $state.diagnostics_exceeded or $reader.diagnostics.len() + $state.diagnostics.len() > props.max_diagnostics {
				return Err(InvalidMarkup({ problem: ResourceLimit("max_diagnostics"), line: at.line, column: at.column }))
			}
			if $state.work > $state.work_limit {
				return Err(InvalidMarkup({ problem: ResourceLimit("max_work_per_byte"), line: at.line, column: at.column }))
			}
			match next.token.kind {
				Start(tag) if tag.attributes.len() > props.max_attributes => return Err(InvalidMarkup({ problem: LimitExceeded(Attributes), line: at.line, column: at.column }))
				Eof => {
					$done = True
				}
				_ => {}
			}
		}
		var $diagnostics = $state.diagnostics
		for d in $reader.diagnostics {
			if $diagnostics.len() < props.max_diagnostics {
				$diagnostics = $diagnostics.append({ problem: HtmlSyntax(d.code), line: 0, column: d.offset })
			}
		}
		diagnostics = $diagnostics.map(
			|d| {
				if d.line != 0 {
					d
				} else {
					at = Literal.position(source, d.column)
					{ ..d, line: at.line, column: at.column }
				}
			},
		)
		if props.recovery == Strict and !diagnostics.is_empty() {
			return Err(InvalidMarkup(diagnostics.first() ?? { problem: HtmlSyntax(InvalidHtml), line: 1, column: 1 }))
		}
		roots = if fragment {
			(node_at($state, 0)).children
		} else $state.roots
		Ok({ source: Markup.Source.from_str(source), tree: { nodes: $state.nodes, roots }, diagnostics })
	}
}

Mode : [Initial, BeforeHtml, BeforeHead, InHead, AfterHead, InBody, InTable, InCaption, InColgroup, InTableBody, InRow, InCell, InSelect, InSelectTable, InTemplate, AfterBody, AfterAfterBody, InFrameset, AfterFrameset]

Active : [Entry(U64), Marker]

State : { nodes : List(Markup.Node), roots : List(U64), stack : List(U64), active : List(Active), modes : List(Mode), mode : Mode, head : Try(U64, [Missing]), form : Try(U64, [Missing]), fragment : Bool, context : HtmlTree.Context, diagnostics : List(Markup.Error), max_diagnostics : U64, foster : Bool, frameset_ok : Bool, skip_lf : Bool, work : U64, work_limit : U64, element_count : U64, diagnostics_exceeded : Bool }

empty_node : Markup.Node
empty_node = { kind: Text(""), attributes: [], children: [], origin: Implied(0) }

node_at : State, U64 -> Markup.Node
node_at = |s, i| s.nodes.get(i) ?? empty_node

name_at : State, U64 -> Str
name_at = |s, i| match (node_at(s, i)).kind {
	Element(n) => n.local
	_ => ""
}

namespace_at : State, U64 -> Str
namespace_at = |s, i| match (node_at(s, i)).kind {
	Element({ namespace: Uri(n), .. }) => n
	_ => ""
}

current : State -> Str
current = |s| match s.stack.last() {
	Ok(i) => name_at(s, i)
	Err(_) => ""
}

adjusted_namespace = |s| if (s.fragment and s.stack.len() == 1) s.context.namespace else match s.stack.last() {
	Ok(i) => namespace_at(s, i)
	Err(_) => Markup.html_namespace
}

adjusted_name = |s| if (s.fragment and s.stack.len() == 1) s.context.name else current(s)

problem : State, MarkupHtmlError.Code, U64 -> State
problem = |s, code, offset| if (s.diagnostics.len() < s.max_diagnostics) { ..s, diagnostics: s.diagnostics.append({ problem: HtmlSyntax(code), line: 0, column: offset }) } else { ..s, diagnostics_exceeded: True }

append_node : State, Markup.Node -> State
append_node = |s, node| {
	index = s.nodes.len()
	var $s = {
		..s,
		nodes: s.nodes.append(node),
		work: s.work + 1,
		element_count: s.element_count + (
			match node.kind {
				Element(_) => 1
				_ => 0
			}
		),
	}
	if s.foster and ["table", "tbody", "tfoot", "thead", "tr"].contains(current(s)) {
		table = last_named(s, "table")
		match table {
			Ok(t) => {
				parent = find_parent(s, t)
				match parent {
					Ok(p) => {
						pn = node_at($s, p)
						at = index_of(pn.children, t) ?? pn.children.len()
						$s = { ..$s, nodes: $s.nodes.set(p, { ..pn, children: pn.children.insert(at, index) ?? pn.children.append(index) }) ?? $s.nodes }
					}
					Err(_) => {
						before = stack_position(s, t) ?? 0
						if before > 0 {
							p = s.stack.get(before - 1) ?? 0
							pn = node_at($s, p)
							$s = { ..$s, nodes: $s.nodes.set(p, { ..pn, children: pn.children.append(index) }) ?? $s.nodes }
						} else {
							$s = { ..$s, roots: $s.roots.append(index) }
						}
					}
				}
				return $s
			}
			Err(_) => {}
		}
	}
	match s.stack.last() {
		Ok(parent) => {
			pn = node_at($s, parent)
			$s = { ..$s, nodes: $s.nodes.set(parent, { ..pn, children: pn.children.append(index) }) ?? $s.nodes }
		}
		Err(_) => {
			$s = { ..$s, roots: $s.roots.append(index) }
		}
	}
	$s
}

insert_element = |s, name, namespace, attrs, origin, push| {
	index = s.nodes.len()
	next = append_node(s, { kind: Element({ local: name, namespace: Uri(namespace), prefix: Unprefixed }), attributes: attrs, children: [], origin })
	if (push) { ..next, stack: next.stack.append(index) } else next
}

insert_tag = |s, tag, namespace, span, push| insert_element(s, adjust_tag(tag.name, namespace), namespace, tag.attributes.map(|a| { name: attribute_name(a.name, namespace), value: a.value, span: a.span }), Authored(span), push)

insert_text = |s, text, span| {
	if text == "" {
		return s
	}
	# Adjacent source text shares a DOM text node. Foster parenting can select a
	# different parent, in which case append_node computes the insertion point.
	if !s.foster or !["table", "tbody", "tfoot", "thead", "tr"].contains(current(s)) {
		siblings = match s.stack.last() {
			Ok(p) => (node_at(s, p)).children
			Err(_) => s.roots
		}
		match siblings.last() {
			Ok(last) => {
				previous = node_at(s, last)
				match previous.kind {
					Text(before) => {
						origin = match previous.origin {
							Authored(p) => Authored({ start: p.start, end: span.end })
							_ => Authored(span)
						}
						return { ..s, nodes: s.nodes.set(last, { ..previous, kind: Text(Str.concat(before, text)), origin }) ?? s.nodes }
					}
					_ => {}
				}
			}
			Err(_) => {}
		}
	}
	if s.foster and ["table", "tbody", "tfoot", "thead", "tr"].contains(current(s)) {
		match last_named(s, "table") {
			Ok(table_id) => {
				siblings = match find_parent(s, table_id) {
					Ok(p) => (node_at(s, p)).children
					_ => s.roots
				}
				at = index_of(siblings, table_id) ?? 0
				if at > 0 {
					previous_id = siblings.get(at - 1) ?? 0
					previous = node_at(s, previous_id)
					match previous.kind {
						Text(before) => return { ..s, nodes: s.nodes.set(previous_id, { ..previous, kind: Text(Str.concat(before, text)), origin: Implied(origin_offset(previous.origin)) }) ?? s.nodes }
						_ => {}
					}
				}
			}
			_ => {}
		}
	}
	append_node(s, { kind: Text(text), attributes: [], children: [], origin: Authored(span) })
}

close_to = |s, name| {
	match last_named(s, name) {
		Ok(id) => { ..s, stack: s.stack.take_first(stack_position(s, id) ?? 0) }
		Err(_) => s
	}
}

last_named : State, Str -> Try(U64, [Missing])
last_named = |s, name| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		if name_at(s, id) == name {
			return Ok(id)
		}
	}
	Err(Missing)
}

stack_position = |s, id| index_of(s.stack, id)

index_of : List(U64), U64 -> Try(U64, [Missing])
index_of = |items, value| {
	var $i = 0
	while $i < items.len() {
		if items.get($i) == Ok(value) {
			return Ok($i)
		}
		$i = $i + 1
	}
	Err(Missing)
}

find_parent : State, U64 -> Try(U64, [Missing])
find_parent = |s, id| {
	var $i = 0
	while $i < s.nodes.len() {
		if (node_at(s, $i)).children.contains(id) {
			return Ok($i)
		}
		$i = $i + 1
	}
	Err(Missing)
}

in_scope = |s, name, kind| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		n = name_at(s, id)
		if n == name {
			return True
		}
		if kind == SelectScope {
			if n != "option" and n != "optgroup" {
				return False
			}
		}
		else if n == "html" or n == "table" or n == "template" {
			return False
		}
		else if kind != TableScope and ["applet", "caption", "td", "th", "marquee", "object"].contains(n) {
			return False
		}
		else if kind == ListScope and ["ol", "ul"].contains(n) {
			return False
		}
		else if kind == ButtonScope and n == "button" {
			return False
		}
	}
	False
}

implied = |s, except| {
	var $s = s
	while ["dd", "dt", "li", "optgroup", "option", "p", "rb", "rp", "rt", "rtc"].contains(current($s)) and current($s) != except {
		$s = { ..$s, stack: $s.stack.drop_last(1) }
	}
	$s
}

close_p = |s| close_to(implied(s, "p"), "p")

context_mode = |name| match name {
	"select" => InSelect
	"td" | "th" => InCell
	"tr" => InRow
	"tbody" | "thead" | "tfoot" => InTableBody
	"caption" => InCaption
	"colgroup" => InColgroup
	"table" => InTable
	"template" => InTemplate
	"head" => InHead
	"frameset" => InFrameset
	_ => InBody
}

reset_mode = |s| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		n = if ($i == 0 and s.fragment) s.context.name else name_at(s, s.stack.get($i) ?? 0)
		if ["select", "td", "th", "tr", "tbody", "thead", "tfoot", "caption", "colgroup", "table", "template", "head", "frameset"].contains(n) {
			return { ..s, mode: context_mode(n) }
		}
		if n == "body" or n == "html" {
			return { ..s, mode: InBody }
		}
	}
	{ ..s, mode: InBody }
}

content_mode = |s| {
	n = adjusted_name(s)
	if adjusted_namespace(s) != Markup.html_namespace {
		return Data
	}
	if n == "title" or n == "textarea" {
		Rcdata(n)
	}
	else if n == "script" {
		Script
	}
	else if n == "plaintext" {
		Plaintext
	}
	else if ["style", "xmp", "iframe", "noembed", "noframes"].contains(n) or (n == "noscript" and s.context.scripting) {
		Rawtext(n)
	}
	else {
		Data
	}
}

process : State, Tok.Token, Bool -> State
process = |state, token, scripting| {
	if state.work > state.work_limit {
		return state
	}
	if foreign_token(state, token) {
		return foreign_process(state, token)
	}
	process_html(state, token, scripting)
}

process_html : State, Tok.Token, Bool -> State
process_html = |state, token, scripting| {
	if state.work > state.work_limit {
		return state
	}
	var $s = { ..state, work: state.work + 1 }
	var $reprocess = True
	var $count = 0.U64
	while $reprocess and $count < 32 {
		$count = $count + 1
		$s = { ..$s, work: $s.work + 1 }
		if $s.work > $s.work_limit {
			return $s
		}
		$reprocess = False
		mode = $s.mode
		match (mode, token.kind) {
			(_, Comment(text)) => {
				$s = append_node($s, { kind: Comment(text), attributes: [], children: [], origin: Authored(token.span) })
			}
			(Initial, Characters(text)) if whitespace(text) => {}
			(Initial, Doctype(d)) => {
				$s = append_node($s, { kind: Doctype({ name: d.name, public_id: d.public_id, system_id: d.system_id }), attributes: [], children: [], origin: Authored(token.span) })
				if d.quirks {
					$s = problem($s, NonConformingDoctype, token.span.start)
				}
				$s = { ..$s, mode: BeforeHtml }
			}
			(Initial, _) => {
				$s = problem($s, MissingDoctype, token.span.start)
				$s = { ..$s, mode: BeforeHtml }
				$reprocess = True
			}
			(_, Doctype(_)) => {
				$s = problem($s, UnexpectedDoctype, token.span.start)
			}
			(BeforeHtml, Characters(t)) if whitespace(t) => {}
			(BeforeHtml, Start(t)) if t.name == "html" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: BeforeHead }
			}
			(BeforeHtml, _) => {
				$s = insert_element($s, "html", Markup.html_namespace, [], Implied(token.span.start), True)
				$s = { ..$s, mode: BeforeHead }
				$reprocess = True
			}
			(BeforeHead, Characters(t)) if whitespace(t) => {}
			(BeforeHead, Start(t)) if t.name == "head" => {
				id = $s.nodes.len()
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, head: Ok(id), mode: InHead }
			}
			(BeforeHead, _) => {
				id = $s.nodes.len()
				$s = insert_element($s, "head", Markup.html_namespace, [], Implied(token.span.start), True)
				$s = { ..$s, head: Ok(id), mode: InHead }
				$reprocess = True
			}
			(InHead, Characters(t)) if whitespace(t) => {
				$s = insert_text($s, t, token.span)
			}
			(InHead, Start(t)) if t.name == "html" => {
				$s = body($s, token)
			}
			(InHead, Start(t)) if ["base", "basefont", "bgsound", "link", "meta"].contains(t.name) => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
			}
			(InHead, Start(t)) if ["title", "style", "script", "noframes"].contains(t.name) or (t.name == "noscript" and scripting) => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			}
			(InHead, End("head")) => {
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: AfterHead }
			}
			(InHead, End(n)) if ["title", "style", "script", "noframes", "noscript"].contains(n) and current($s) == n => {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			(InHead, Start(t)) if t.name == "template" => {
				$s = start_template($s, t, token.span)
			}
			(_, End("template")) => {
				$s = end_template($s, token.span.start)
			}
			(InHead, Characters(t)) if current($s) != "head" => {
				$s = insert_text($s, t, token.span)
			}
			(InHead, _) => {
				$s = close_to($s, "head")
				$s = { ..$s, mode: AfterHead }
				$reprocess = True
			}
			(AfterHead, Characters(t)) if whitespace(t) => {
				$s = insert_text($s, t, token.span)
			}
			(AfterHead, Start(t)) if t.name == "body" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InBody }
			}
			(AfterHead, Start(t)) if t.name == "frameset" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InFrameset }
			}
			(AfterHead, Start(t)) if ["base", "link", "meta", "script", "style", "template", "title"].contains(t.name) => {
				$s = problem($s, HeadContentAfterHead, token.span.start)
				match $s.head {
					Ok(h) => {
						old = $s.stack
						$s = { ..$s, stack: old.append(h), mode: InHead }
						$s = process($s, token, scripting)
						$s = { ..$s, stack: $s.stack.keep_if(|i| i != h), mode: AfterHead }
					}
					_ => {}
				}
			}
			(AfterHead, _) => {
				$s = insert_element($s, "body", Markup.html_namespace, [], Implied(token.span.start), True)
				$s = { ..$s, mode: InBody }
				$reprocess = True
			}
			(InBody, _) => {
				$s = body($s, token)
			}
			(InTable, _) => {
				$s = table($s, token)
			}
			(InTableBody, _) => {
				$s = table_body($s, token)
			}
			(InRow, _) => {
				$s = row($s, token)
			}
			(InCell, _) => {
				match token.kind {
					End(n) if n == "td" or n == "th" => {
						if in_scope($s, n, TableScope) {
							$s = close_to(implied($s, ""), n)
							$s = clear_active($s)
							$s = { ..$s, mode: InRow }
						} else {
							$s = problem($s, UnexpectedCellEnd, token.span.start)
						}
					}
					Start(t) if ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(t.name) => {
						$s = close_cell($s)
						$reprocess = True
					}
					End(n) if ["table", "tbody", "tfoot", "thead", "tr"].contains(n) and in_scope($s, n, TableScope) => {
						$s = close_cell($s)
						$reprocess = True
					}
					_ => {
						$s = body($s, token)
					}
				}
			}
			(InCaption, _) => {
				match token.kind {
					End("caption") => {
						$s = close_to(implied($s, ""), "caption")
						$s = clear_active($s)
						$s = { ..$s, mode: InTable }
					}
					Start(t) if ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(t.name) => {
						$s = close_to(implied($s, ""), "caption")
						$s = clear_active($s)
						$s = { ..$s, mode: InTable }
						$reprocess = True
					}
					End("table") => {
						$s = close_to(implied($s, ""), "caption")
						$s = clear_active($s)
						$s = { ..$s, mode: InTable }
						$reprocess = True
					}
					_ => {
						$s = body($s, token)
					}
				}
			}
			(InColgroup, _) => {
				match token.kind {
					Characters(t) if whitespace(t) => {
						$s = insert_text($s, t, token.span)
					}
					Start(t) if t.name == "col" => {
						$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
					}
					End("colgroup") => {
						$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTable }
					}
					End("col") => {
						$s = problem($s, UnexpectedColEnd, token.span.start)
					}
					Eof => {}
					_ => {
						$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTable }
						$reprocess = True
					}
				}
			}
			(InSelect, _) | (InSelectTable, _) => {
				$s = select($s, token)
			}
			(InTemplate, _) => {
				match token.kind {
					Characters(_) => {
						$s = body($s, token)
					}
					Start(t) if ["base", "link", "meta", "script", "style", "template", "title"].contains(t.name) => {
						old = $s.mode
						$s = { ..$s, mode: InHead }
						$s = process($s, token, scripting)
						if $s.mode == InHead {
							$s = { ..$s, mode: old }
						}
					}
					Eof => {
						if last_named($s, "template").is_ok() {
							$s = problem($s, EofInTemplate, token.span.start)
							$s = end_template($s, token.span.start)
							$reprocess = True
						}
					}
					Start(t) => {
						next = if (["caption", "colgroup", "tbody", "tfoot", "thead"].contains(t.name)) InTable else if (t.name == "col") InColgroup else if (t.name == "tr") InTableBody else if (t.name == "td" or t.name == "th") InRow else InBody
						$s = { ..$s, mode: next, modes: $s.modes.drop_last(1).append(next) }
						$reprocess = True
					}
					_ => {
						$s = problem($s, UnexpectedTemplateToken, token.span.start)
					}
				}
			}
			(AfterBody, Characters(t)) if whitespace(t) => {
				$s = body($s, token)
			}
			(AfterBody, End("html")) => {
				$s = { ..$s, mode: AfterAfterBody }
			}
			(AfterBody, Eof) | (AfterAfterBody, Eof) => {}
			(AfterAfterBody, Characters(t)) if whitespace(t) => {
				$s = body($s, token)
			}
			(AfterBody, _) | (AfterAfterBody, _) => {
				$s = problem($s, ContentAfterBody, token.span.start)
				$s = { ..$s, mode: InBody }
				$reprocess = True
			}
			(InFrameset, Characters(t)) if whitespace(t) => {
				$s = insert_text($s, t, token.span)
			}
			(InFrameset, Start(t)) if t.name == "frameset" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			}
			(InFrameset, Start(t)) if t.name == "frame" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
			}
			(InFrameset, End("frameset")) => {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
				if current($s) != "frameset" {
					$s = { ..$s, mode: AfterFrameset }
				}
			}
			(InFrameset, Eof) | (AfterFrameset, Eof) => {}
			(AfterFrameset, End("html")) => {
				$s = { ..$s, mode: AfterAfterBody }
			}
			(InFrameset, _) | (AfterFrameset, _) => {
				$s = problem($s, UnexpectedFramesetContent, token.span.start)
			}
		}
	}
	$s
}

body : State, Tok.Token -> State
body = |state, token| {
	if state.work > state.work_limit {
		return state
	}
	var $s = { ..state, work: state.work + state.stack.len() + state.active.len() + 1 }
	match token.kind {
		Characters(raw_text) => {
			text = Str.join_with(raw_text.split_on("\u(0)"), "")
			$s = reconstruct($s)
			$s = insert_text($s, text, token.span)
			if !whitespace(text) {
				$s = { ..$s, frameset_ok: False }
			}
		}
		Eof => {
			if $s.stack.any(|i| !["dd", "dt", "li", "optgroup", "option", "p", "rb", "rp", "rt", "rtc", "tbody", "td", "tfoot", "th", "thead", "tr", "body", "html"].contains(name_at($s, i))) {
				$s = problem($s, EofWithOpenElements, token.span.start)
			}
		}
		Start(tag) => {
			name = tag.name
			if name == "html" {
				if !$s.stack.is_empty() {
					$s = merge_attributes($s, 0, tag)
				}
			} else if name == "body" {
				match last_named($s, "body") {
					Ok(id) => {
						$s = merge_attributes($s, id, tag)
					}
					_ => {
						$s = problem($s, UnexpectedBodyStart, token.span.start)
					}
				}
			} else if name == "head" {
				$s = problem($s, UnexpectedHeadStart, token.span.start)
			}
			else if ["base", "basefont", "bgsound", "link", "meta"].contains(name) {
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, False)
			}
			else if name == "template" {
				$s = start_template($s, tag, token.span)
			}
			else if ["address", "article", "aside", "blockquote", "center", "details", "dialog", "dir", "div", "dl", "fieldset", "figcaption", "figure", "footer", "header", "hgroup", "main", "menu", "nav", "ol", "p", "search", "section", "summary", "ul"].contains(name) {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if ["h1", "h2", "h3", "h4", "h5", "h6"].contains(name) {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				if ["h1", "h2", "h3", "h4", "h5", "h6"].contains(current($s)) {
					$s = problem($s, NestedHeading, token.span.start)
					$s = { ..$s, stack: $s.stack.drop_last(1) }
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if name == "pre" or name == "listing" {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, skip_lf: True, frameset_ok: False }
			} else if name == "form" {
				if $s.form.is_ok() and last_named($s, "template").is_err() {
					$s = problem($s, NestedForm, token.span.start)
				}
				else {
					if in_scope($s, "p", ButtonScope) {
						$s = close_p($s)
					}
					id = $s.nodes.len()
					$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
					if last_named($s, "template").is_err() {
						$s = { ..$s, form: Ok(id) }
					}
				}
			} else if name == "li" or name == "dd" or name == "dt" {
				var $i = $s.stack.len()
				while $i > 0 {
					$i = $i - 1
					n = name_at($s, $s.stack.get($i) ?? 0)
					if n == name or (name != "li" and (n == "dd" or n == "dt")) {
						$s = close_to(implied($s, n), n)
						break
					}
					if special(n) and !["address", "div", "p"].contains(n) {
						break
					}
				}
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, frameset_ok: False }
			} else if name == "plaintext" {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if name == "button" {
				if in_scope($s, "button", DefaultScope) {
					$s = problem($s, NestedButton, token.span.start)
					$s = close_to(implied($s, ""), "button")
				}
				$s = reconstruct($s)
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, frameset_ok: False }
			} else if formatting(name) {
				if name == "a" {
					match active_named($s, "a") {
						Ok(id) => {
							$s = problem($s, NestedAnchor, token.span.start)
							$s = adoption($s, "a", token.span.start)
							$s = { ..$s, active: $s.active.keep_if(|a| a != Entry(id)), stack: $s.stack.keep_if(|i| i != id) }
						}
						_ => {}
					}
				}
				if name == "nobr" and in_scope($s, "nobr", DefaultScope) {
					$s = problem($s, NestedNobr, token.span.start)
					$s = adoption($s, "nobr", token.span.start)
				}
				$s = reconstruct($s)
				id = $s.nodes.len()
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				# Noah's Ark clause: retain at most three equivalent entries after
				# the last marker, even while older elements remain open.
				var $same = []
				var $active_at = $s.active.len()
				fresh = node_at($s, id)
				while $active_at > 0 {
					$active_at = $active_at - 1
					match $s.active.get($active_at) ?? Marker {
						Marker => break
						Entry(previous_id) => {
							previous = node_at($s, previous_id)
							if previous.kind == fresh.kind and previous.attributes.map(|a| (a.name, a.value)) == fresh.attributes.map(|a| (a.name, a.value)) {
								$same = $same.append(previous_id)
							}
						}
					}
				}
				if $same.len() >= 3 {
					oldest = $same.last() ?? id
					$s = { ..$s, active: $s.active.keep_if(|a| a != Entry(oldest)) }
				}
				$s = { ..$s, active: $s.active.append(Entry(id)) }
			} else if name == "applet" or name == "marquee" or name == "object" {
				$s = reconstruct($s)
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, active: $s.active.append(Marker), frameset_ok: False }
			} else if name == "table" {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InTable, frameset_ok: False }
			} else if void(name) {
				if name == "hr" and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = reconstruct($s)
				adjusted = if (name == "image") { ..tag, name: "img" } else tag
				$s = insert_tag($s, adjusted, Markup.html_namespace, token.span, False)
				if name == "hr" and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				if name != "input" or !tag.attributes.any(|a| a.name == "type" and Tok.lower(a.value) == "hidden") {
					$s = { ..$s, frameset_ok: False }
				}
			} else if name == "textarea" {
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, skip_lf: True, frameset_ok: False }
			} else if ["xmp", "iframe", "noembed", "noscript", "script", "style", "title", "noframes"].contains(name) {
				if name == "xmp" and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, frameset_ok: False }
			} else if name == "select" {
				$s = reconstruct($s)
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				next = if ([InTable, InCaption, InTableBody, InRow, InCell].contains($s.mode)) InSelectTable else InSelect
				$s = { ..$s, mode: next, frameset_ok: False }
			} else if name == "option" or name == "optgroup" {
				if current($s) == "option" {
					$s = { ..$s, stack: $s.stack.drop_last(1) }
				}
				if name == "optgroup" and current($s) == "optgroup" {
					$s = { ..$s, stack: $s.stack.drop_last(1) }
				}
				$s = reconstruct($s)
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if ["rb", "rtc", "rp", "rt"].contains(name) {
				if in_scope($s, "ruby", DefaultScope) {
					$s = implied($s, if (name == "rt" or name == "rp") "rtc" else "")
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if name == "math" or name == "svg" {
				$s = reconstruct($s)
				$s = insert_tag($s, tag, if (name == "svg") Markup.svg_namespace else Markup.mathml_namespace, token.span, !tag.self_closing)
			} else if ["caption", "col", "colgroup", "frame", "head", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(name) {
				$s = problem($s, UnexpectedTableElement, token.span.start)
			} else {
				$s = reconstruct($s)
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				if tag.self_closing {
					$s = problem($s, NonVoidHtmlElementStartTagWithTrailingSolidus, token.span.start)
				}
			}
		}
		End(name) => {
			if name == "body" or name == "html" {
				if in_scope($s, "body", DefaultScope) {
					$s = { ..$s, mode: if (name == "html") AfterAfterBody else AfterBody }
				}
				else {
					$s = problem($s, UnexpectedBodyEnd, token.span.start)
				}
			} else if name == "p" {
				if !in_scope($s, "p", ButtonScope) {
					$s = problem($s, UnexpectedPEnd, token.span.start)
					$s = insert_element($s, "p", Markup.html_namespace, [], Implied(token.span.start), True)
				}
				$s = close_p($s)
			} else if ["address", "article", "aside", "blockquote", "center", "details", "dialog", "dir", "div", "dl", "fieldset", "figcaption", "figure", "footer", "header", "hgroup", "listing", "main", "menu", "nav", "ol", "pre", "search", "section", "summary", "ul"].contains(name) {
				if !in_scope($s, name, DefaultScope) {
					$s = problem($s, UnexpectedEndTag, token.span.start)
				}
				else {
					$s = implied($s, "")
					if current($s) != name {
						$s = problem($s, MisnestedEndTag, token.span.start)
					}
					$s = close_to($s, name)
				}
			} else if formatting(name) {
				$s = adoption($s, name, token.span.start)
			}
			else if name == "form" and last_named($s, "template").is_err() {
				match $s.form {
					Ok(id) => {
						if !s_in_scope_id($s, id) {
							$s = problem($s, UnexpectedFormEnd, token.span.start)
						} else {
							$s = implied($s, "")
							$s = { ..$s, stack: $s.stack.keep_if(|i| i != id) }
						}
					}
					_ => {
						$s = problem($s, UnexpectedFormEnd, token.span.start)
					}
				}
				$s = { ..$s, form: Err(Missing) }
			} else if name == "br" {
				$s = problem($s, EndTagBr, token.span.start)
				$s = body($s, { ..token, kind: Start({ name: "br", attributes: [], self_closing: False }) })
			} else if name == "li" or name == "dd" or name == "dt" or name == "button" or name == "applet" or name == "marquee" or name == "object" {
				scope = if (name == "li") ListScope else DefaultScope
				if !in_scope($s, name, scope) {
					$s = problem($s, UnexpectedEndTag, token.span.start)
				}
				else {
					$s = close_to(implied($s, name), name)
					if name == "applet" or name == "marquee" or name == "object" {
						$s = clear_active($s)
					}
				}
			} else if ["h1", "h2", "h3", "h4", "h5", "h6"].contains(name) {
				var $found = ""
				for heading in ["h1", "h2", "h3", "h4", "h5", "h6"] {
					if in_scope($s, heading, DefaultScope) {
						$found = heading
					}
				}
				if $found == "" {
					$s = problem($s, UnexpectedHeadingEnd, token.span.start)
				}
				else {
					if $found != name {
						$s = problem($s, MismatchedHeadingEnd, token.span.start)
					}
					$s = close_to(implied($s, ""), $found)
				}
			} else {
				$s = generic_end($s, name, token.span.start)
			}
		}
		_ => {}
	}
	$s
}

s_in_scope_id = |s, id| s.stack.contains(id) and in_scope(s, name_at(s, id), DefaultScope)

merge_attributes = |s, id, tag| {
	node = node_at(s, id)
	extra = tag.attributes.keep_if(|a| !node.attributes.any(|b| b.name.local == a.name)).map(|a| { name: attribute_name(a.name, Markup.html_namespace), value: a.value, span: a.span })
	{ ..s, nodes: s.nodes.set(id, { ..node, attributes: node.attributes.concat(extra) }) ?? s.nodes }
}

generic_end = |s, name, offset| {
	var $s = s
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		n = name_at(s, id)
		if n == name {
			$s = implied($s, name)
			if current($s) != name {
				$s = problem($s, MisnestedEndTag, offset)
			}
			return { ..$s, stack: $s.stack.take_first($i) }
		}
		if special(n) {
			return problem($s, UnexpectedEndTag, offset)
		}
	}
	problem($s, UnexpectedEndTag, offset)
}

start_template = |s, tag, span| {
	next = insert_tag(s, tag, Markup.html_namespace, span, True)
	{ ..next, active: next.active.append(Marker), modes: next.modes.append(InTemplate), mode: InTemplate, frameset_ok: False }
}

end_template = |s, offset| {
	if last_named(s, "template").is_err() {
		return problem(s, UnexpectedTemplateEnd, offset)
	}
	next = clear_active(close_to(implied(s, ""), "template"))
	reset_mode({ ..next, modes: next.modes.drop_last(1) })
}

clear_active = |s| {
	var $active = s.active
	while !$active.is_empty() {
		last = $active.last() ?? Marker
		$active = $active.drop_last(1)
		if last == Marker {
			break
		}
	}
	{ ..s, active: $active }
}

reconstruct = |s| {
	if s.active.is_empty() {
		return s
	}
	last = s.active.last() ?? Marker
	match last {
		Marker => return s
		Entry(id) if s.stack.contains(id) => return s
		_ => {}
	}
	var $i = s.active.len() - 1
	while $i > 0 {
		match s.active.get($i - 1) ?? Marker {
			Marker => break
			Entry(id) if s.stack.contains(id) => break
			_ => {
				$i = $i - 1
			}
		}
	}
	var $s = s
	while $i < $s.active.len() {
		match $s.active.get($i) ?? Marker {
			Entry(id) => {
				original = node_at($s, id)
				new = $s.nodes.len()
				$s = append_node($s, { ..original, children: [], origin: Implied(origin_offset(original.origin)) })
				$s = { ..$s, stack: $s.stack.append(new), active: $s.active.set($i, Entry(new)) ?? $s.active }
			}
			_ => {}
		}
		$i = $i + 1
	}
	$s
}

origin_offset = |origin| match origin {
	Authored(span) => span.start
	Implied(offset) => offset
}

active_named : State, Str -> Try(U64, [Missing])
active_named = |s, name| {
	var $i = s.active.len()
	while $i > 0 {
		$i = $i - 1
		match s.active.get($i) ?? Marker {
			Marker => break
			Entry(id) if name_at(s, id) == name => return Ok(id)
			_ => {}
		}
	}
	Err(Missing)
}

# Adoption agency algorithm, including the furthest-block reconstruction and
# reparenting path
# Eight outer iterations bound pathological formatting.
adoption = |s, name, offset| {
	var $s = s
	var $outer = 0.U64
	while $outer < 8 {
		$outer = $outer + 1
		$s = { ..$s, work: $s.work + $s.stack.len() + $s.active.len() + 1 }
		if $s.work > $s.work_limit {
			return $s
		}
		match active_named($s, name) {
			Err(_) => return generic_end($s, name, offset)
			Ok(formatting_id) => {
				if !$s.stack.contains(formatting_id) {
					return { ..problem($s, FormattingElementNotOpen, offset), active: $s.active.keep_if(|a| a != Entry(formatting_id)) }
				}
				if !s_in_scope_id($s, formatting_id) {
					return problem($s, FormattingElementOutOfScope, offset)
				}
				if ($s.stack.last() ?? 0) != formatting_id {
					$s = problem($s, MisnestedFormatting, offset)
				}
				at = stack_position($s, formatting_id) ?? 0
				var $furthest = Err(Missing)
				var $i = at + 1
				while $i < $s.stack.len() {
					id = $s.stack.get($i) ?? 0
					if special(name_at($s, id)) {
						$furthest = Ok(id)
						break
					}
					$i = $i + 1
				}
				match $furthest {
					Err(_) => return { ..$s, stack: $s.stack.take_first(at), active: $s.active.keep_if(|a| a != Entry(formatting_id)) }
					Ok(block) => {
						common = if (at > 0) $s.stack.get(at - 1) ?? 0 else 0
						var $bookmark = active_position($s, formatting_id) ?? 0
						var $last = block
						var $node_pos = stack_position($s, block) ?? 0
						var $inner = 0.U64
						while $node_pos > at + 1 {
							$inner = $inner + 1
							$s = { ..$s, work: $s.work + $s.nodes.len() + 1 }
							if $s.work > $s.work_limit {
								return $s
							}
							$node_pos = $node_pos - 1
							node_id = $s.stack.get($node_pos) ?? 0
							if $inner > 3 {
								$s = { ..$s, active: $s.active.keep_if(|a| a != Entry(node_id)) }
							}
							match active_position($s, node_id) {
								Err(_) => {
									$s = { ..$s, stack: $s.stack.take_first($node_pos).concat($s.stack.drop_first($node_pos + 1)) }
								}
								Ok(active_at) => {
									original = node_at($s, node_id)
									new_id = $s.nodes.len()
									$s = { ..$s, nodes: $s.nodes.append({ ..original, children: [], origin: Implied(offset) }), stack: $s.stack.set($node_pos, new_id) ?? $s.stack, active: $s.active.set(active_at, Entry(new_id)) ?? $s.active }
									if $last == block {
										$bookmark = active_at + 1
									}
									$s = reparent($s, $last, new_id)
									$last = new_id
								}
							}
						}
						if ["table", "tbody", "tfoot", "thead", "tr"].contains(name_at($s, common)) {
							moved = node_at($s, $last)
							$s = detach($s, $last)
							# append_node allocates a new index, so use a temporary
							# append and transfer its computed insertion position.
							old_count = $s.nodes.len()
							temporary = append_node({ ..$s, foster: True }, moved)
							$s = replace_child_id({ ..temporary, foster: False, nodes: temporary.nodes.take_first(old_count) }, old_count, $last)
						} else {
							$s = reparent($s, $last, common)
						}
						original = node_at($s, formatting_id)
						new_formatting = $s.nodes.len()
						block_node = node_at($s, block)
						$s = { ..$s, nodes: $s.nodes.append({ ..original, children: block_node.children, origin: Implied(offset) }) }
						$s = { ..$s, nodes: $s.nodes.set(block, { ..block_node, children: [new_formatting] }) ?? $s.nodes }
						active = $s.active.keep_if(|a| a != Entry(formatting_id))
						bookmark = if ($bookmark > active.len()) active.len() else $bookmark
						$s = { ..$s, active: active.insert(bookmark, Entry(new_formatting)) ?? active.append(Entry(new_formatting)), stack: $s.stack.keep_if(|id| id != formatting_id) }
						block_at = stack_position($s, block) ?? 0
						$s = { ..$s, stack: $s.stack.insert(block_at + 1, new_formatting) ?? $s.stack.append(new_formatting) }
					}
				}
			}
		}
	}
	$s
}

active_position : State, U64 -> Try(U64, [Missing])
active_position = |s, id| {
	var $i = 0
	while $i < s.active.len() {
		if s.active.get($i) == Ok(Entry(id)) {
			return Ok($i)
		}
		$i = $i + 1
	}
	Err(Missing)
}

detach = |s, id| {
	match find_parent(s, id) {
		Ok(parent) => {
			n = node_at(s, parent)
			{ ..s, nodes: s.nodes.set(parent, { ..n, children: n.children.keep_if(|child| child != id) }) ?? s.nodes }
		}
		Err(_) => { ..s, roots: s.roots.keep_if(|child| child != id) }
	}
}

reparent = |s, id, parent| {
	detached = detach(s, id)
	n = node_at(detached, parent)
	{ ..detached, nodes: detached.nodes.set(parent, { ..n, children: n.children.append(id) }) ?? detached.nodes }
}

replace_child_id = |s, old, new| { ..s, roots: s.roots.map(|id| if (id == old) new else id), nodes: s.nodes.map(|n| { ..n, children: n.children.map(|id| if (id == old) new else id) }) }

table = |state, token| {
	var $s = state
	match token.kind {
		Characters(t) if whitespace(t) => {
			$s = insert_text($s, t, token.span)
		}
		Start(t) if t.name == "caption" => {
			$s = clear_table_stack($s)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			$s = { ..$s, active: $s.active.append(Marker), mode: InCaption }
		}
		Start(t) if t.name == "colgroup" => {
			$s = clear_table_stack($s)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			$s = { ..$s, mode: InColgroup }
		}
		Start(t) if t.name == "col" => {
			$s = clear_table_stack($s)
			$s = insert_element($s, "colgroup", Markup.html_namespace, [], Implied(token.span.start), True)
			$s = { ..$s, mode: InColgroup }
			$s = process($s, token, $s.context.scripting)
		}
		Start(t) if ["tbody", "tfoot", "thead"].contains(t.name) => {
			$s = clear_table_stack($s)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			$s = { ..$s, mode: InTableBody }
		}
		Start(t) if ["tr", "td", "th"].contains(t.name) => {
			$s = clear_table_stack($s)
			$s = insert_element($s, "tbody", Markup.html_namespace, [], Implied(token.span.start), True)
			$s = { ..$s, mode: InTableBody }
			$s = process($s, token, $s.context.scripting)
		}
		Start(t) if t.name == "table" => {
			$s = problem($s, NestedTable, token.span.start)
			if in_scope($s, "table", TableScope) {
				$s = reset_mode(close_to($s, "table"))
				$s = process($s, token, $s.context.scripting)
			}
		}
		End("table") => {
			if in_scope($s, "table", TableScope) {
				$s = reset_mode(close_to($s, "table"))
			} else {
				$s = problem($s, UnexpectedTableEnd, token.span.start)
			}
		}
		End(n) if ["body", "caption", "col", "colgroup", "html", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(n) => {
			$s = problem($s, UnexpectedTableEndTag, token.span.start)
		}
		Start(t) if t.name == "input" and t.attributes.any(|a| a.name == "type" and Tok.lower(a.value) == "hidden") => {
			$s = problem($s, HiddenInputInTable, token.span.start)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
		}
		Start(t) if t.name == "form" => {
			$s = problem($s, FormInTable, token.span.start)
			if $s.form.is_err() {
				id = $s.nodes.len()
				$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
				$s = { ..$s, form: Ok(id) }
			}
		}
		Start(t) if ["style", "script", "template"].contains(t.name) => {
			$s = body($s, token)
		}
		Eof => {
			$s = body($s, token)
		}
		_ => {
			$s = problem($s, FosterParentedContent, token.span.start)
			$s = body({ ..$s, foster: True }, token)
			$s = { ..$s, foster: False }
		}
	}
	$s
}

clear_table_stack = |s| {
	var $s = s
	while !$s.stack.is_empty() and !["table", "template", "html"].contains(current($s)) {
		$s = { ..$s, stack: $s.stack.drop_last(1) }
	}
	$s
}

clear_section_stack = |s| {
	var $s = s
	while !$s.stack.is_empty() and !["tbody", "tfoot", "thead", "template", "html"].contains(current($s)) {
		$s = { ..$s, stack: $s.stack.drop_last(1) }
	}
	$s
}

clear_row_stack = |s| {
	var $s = s
	while !$s.stack.is_empty() and !["tr", "template", "html"].contains(current($s)) {
		$s = { ..$s, stack: $s.stack.drop_last(1) }
	}
	$s
}

table_body = |state, token| {
	var $s = state
	match token.kind {
		Start(t) if t.name == "tr" => {
			$s = clear_section_stack($s)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			$s = { ..$s, mode: InRow }
		}
		Start(t) if t.name == "td" or t.name == "th" => {
			$s = problem($s, CellWithoutRow, token.span.start)
			$s = clear_section_stack($s)
			$s = insert_element($s, "tr", Markup.html_namespace, [], Implied(token.span.start), True)
			$s = { ..$s, mode: InRow }
			$s = process($s, token, $s.context.scripting)
		}
		End(n) if ["tbody", "tfoot", "thead"].contains(n) => {
			if in_scope($s, n, TableScope) {
				$s = clear_section_stack($s)
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTable }
			} else {
				$s = problem($s, UnexpectedTableSectionEnd, token.span.start)
			}
		}
		Start(t) if ["caption", "col", "colgroup", "tbody", "tfoot", "thead"].contains(t.name) => {
			$s = clear_section_stack($s)
			if current($s) != "html" {
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTable }
				$s = process($s, token, $s.context.scripting)
			}
		}
		End("table") => {
			$s = clear_section_stack($s)
			if current($s) != "html" {
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTable }
				$s = process($s, token, $s.context.scripting)
			}
		}
		End(n) if ["body", "caption", "col", "colgroup", "html", "td", "th", "tr"].contains(n) => {
			$s = problem($s, UnexpectedTableSectionToken, token.span.start)
		}
		_ => {
			$s = table($s, token)
		}
	}
	$s
}

row = |state, token| {
	var $s = state
	match token.kind {
		Start(t) if t.name == "td" or t.name == "th" => {
			$s = clear_row_stack($s)
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			$s = { ..$s, active: $s.active.append(Marker), mode: InCell }
		}
		End("tr") => {
			if in_scope($s, "tr", TableScope) {
				$s = clear_row_stack($s)
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTableBody }
			} else {
				$s = problem($s, UnexpectedRowEnd, token.span.start)
			}
		}
		Start(t) if ["caption", "col", "colgroup", "tbody", "tfoot", "thead", "tr"].contains(t.name) => {
			if in_scope($s, "tr", TableScope) {
				$s = clear_row_stack($s)
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTableBody }
				$s = process($s, token, $s.context.scripting)
			}
		}
		End(n) if ["table", "tbody", "tfoot", "thead"].contains(n) => {
			if in_scope($s, n, TableScope) {
				$s = clear_row_stack($s)
				$s = { ..$s, stack: $s.stack.drop_last(1), mode: InTableBody }
				$s = process($s, token, $s.context.scripting)
			}
		}
		End(n) if ["body", "caption", "col", "colgroup", "html", "td", "th"].contains(n) => {
			$s = problem($s, UnexpectedRowToken, token.span.start)
		}
		_ => {
			$s = table($s, token)
		}
	}
	$s
}

close_cell = |s| {
	name = if (in_scope(s, "td", TableScope)) "td" else "th"
	next = clear_active(close_to(implied(s, ""), name))
	{ ..next, mode: InRow }
}

select = |state, token| {
	var $s = state
	match token.kind {
		Characters(t) => {
			$s = insert_text($s, t, token.span)
		}
		Start(t) if t.name == "option" => {
			if current($s) == "option" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
		}
		Start(t) if t.name == "optgroup" => {
			if current($s) == "option" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			if current($s) == "optgroup" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
		}
		End("option") => {
			if current($s) == "option" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			} else {
				$s = problem($s, UnexpectedOptionEnd, token.span.start)
			}
		}
		End("optgroup") => {
			if current($s) == "option" and name_at($s, $s.stack.get($s.stack.len() - 2) ?? 0) == "optgroup" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			if current($s) == "optgroup" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			} else {
				$s = problem($s, UnexpectedOptgroupEnd, token.span.start)
			}
		}
		End("select") => {
			if in_scope($s, "select", SelectScope) {
				$s = reset_mode(close_to($s, "select"))
			} else {
				$s = problem($s, UnexpectedSelectEnd, token.span.start)
			}
		}
		Start(t) if ["input", "keygen", "textarea", "select"].contains(t.name) => {
			$s = problem($s, UnexpectedSelectContent, token.span.start)
			if in_scope($s, "select", SelectScope) {
				$s = reset_mode(close_to($s, "select"))
				if t.name != "select" {
					$s = process($s, token, $s.context.scripting)
				}
			}
		}
		Start(t) if t.name == "hr" => {
			if current($s) == "option" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			if current($s) == "optgroup" {
				$s = { ..$s, stack: $s.stack.drop_last(1) }
			}
			$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
		}
		Start(t) if ["script", "template"].contains(t.name) => {
			$s = body($s, token)
		}
		Eof => {
			$s = body($s, token)
		}
		_ => {
			if $s.mode == InSelectTable and table_token(token.kind) and in_scope($s, "select", SelectScope) {
				$s = problem($s, TableContentInSelect, token.span.start)
				$s = reset_mode(close_to($s, "select"))
				$s = process($s, token, $s.context.scripting)
			}
			else {
				$s = problem($s, UnexpectedSelectToken, token.span.start)
			}
		}
	}
	$s
}

table_token = |kind| match kind {
	Start(t) => ["caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th"].contains(t.name)
	End(n) => ["caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th"].contains(n)
	_ => False
}

foreign_token = |s, token| {
	namespace = adjusted_namespace(s)
	if namespace == Markup.html_namespace {
		return False
	}
	match token.kind {
		Eof => return False
		_ => {}
	}
	name = adjusted_name(s)
	if namespace == Markup.mathml_namespace and ["mi", "mo", "mn", "ms", "mtext"].contains(name) {
		match token.kind {
			Characters(_) => return False
			Start(t) if t.name != "mglyph" and t.name != "malignmark" => return False
			_ => {}
		}
	}
	if namespace == Markup.mathml_namespace and name == "annotation-xml" {
		match token.kind {
			Start(t) if t.name == "svg" => return False
			_ => {}
		}
	}
	if integration(s) {
		match token.kind {
			Characters(_) | Start(_) => return False
			_ => {}
		}
	}
	True
}

integration = |s| {
	ns = adjusted_namespace(s)
	name = adjusted_name(s)
	if ns == Markup.svg_namespace and ["foreignObject", "desc", "title"].contains(name) {
		return True
	}
	if ns == Markup.mathml_namespace and name == "annotation-xml" {
		n = node_at(s, s.stack.last() ?? 0)
		return n.attributes.any(|a| a.name.local == "encoding" and ["text/html", "application/xhtml+xml"].contains(Tok.lower(a.value)))
	}
	False
}

foreign_process = |state, token| {
	var $s = state
	match token.kind {
		Characters(t) => {
			$s = insert_text($s, Str.join_with(t.split_on("\u(0)"), "\u(FFFD)"), token.span)
		}
		Comment(t) => {
			$s = append_node($s, { kind: Comment(t), attributes: [], children: [], origin: Authored(token.span) })
		}
		Start(t) => {
			breakout = ["b", "big", "blockquote", "body", "br", "center", "code", "dd", "div", "dl", "dt", "em", "embed", "h1", "h2", "h3", "h4", "h5", "h6", "head", "hr", "i", "img", "li", "listing", "menu", "meta", "nobr", "ol", "p", "pre", "ruby", "s", "small", "span", "strong", "strike", "sub", "sup", "table", "tt", "u", "ul", "var"].contains(t.name) or (t.name == "font" and t.attributes.any(|a| ["color", "face", "size"].contains(a.name)))
			if breakout {
				$s = problem($s, HtmlStartTagInForeignContent, token.span.start)
				while !$s.stack.is_empty() and namespace_at($s, $s.stack.last() ?? 0) != Markup.html_namespace and !integration($s) {
					$s = { ..$s, stack: $s.stack.drop_last(1) }
				}
				$s = process_html($s, token, $s.context.scripting)
			} else {
				$s = insert_tag($s, t, adjusted_namespace($s), token.span, !t.self_closing)
			}
		}
		End(n) => {
			var $i = $s.stack.len()
			var $found = False
			while $i > 0 {
				$i = $i - 1
				id = $s.stack.get($i) ?? 0
				if Tok.lower(name_at($s, id)) == n {
					$s = { ..$s, stack: $s.stack.take_first($i) }
					$found = True
					break
				}
				if namespace_at($s, id) == Markup.html_namespace {
					$s = process_html($s, token, $s.context.scripting)
					$found = True
					break
				}
			}
			if !$found {
				$s = problem($s, UnexpectedForeignEndTag, token.span.start)
			}
		}
		Doctype(_) => {
			$s = problem($s, DoctypeInForeignContent, token.span.start)
		}
		Eof => {}
	}
	$s
}

attribute_name = |raw, namespace| {
	if namespace != Markup.html_namespace {
		if raw == "xmlns" {
			return { namespace: Uri("http://www.w3.org/2000/xmlns/"), local: "xmlns", prefix: Unprefixed }
		}
		if raw.starts_with("xmlns:") {
			return { namespace: Uri("http://www.w3.org/2000/xmlns/"), local: Str.from_utf8_lossy(raw.to_utf8().drop_first(6)), prefix: Prefix("xmlns") }
		}
		if raw.starts_with("xlink:") {
			return { namespace: Uri("http://www.w3.org/1999/xlink"), local: Str.from_utf8_lossy(raw.to_utf8().drop_first(6)), prefix: Prefix("xlink") }
		}
		if raw.starts_with("xml:") {
			return { namespace: Uri("http://www.w3.org/XML/1998/namespace"), local: Str.from_utf8_lossy(raw.to_utf8().drop_first(4)), prefix: Prefix("xml") }
		}
	}
	local = if (namespace == Markup.svg_namespace) adjust_svg_attribute(raw) else if (namespace == Markup.mathml_namespace and raw == "definitionurl") "definitionURL" else raw
	{ namespace: NoNamespace, local, prefix: Unprefixed }
}

adjust_tag = |raw, namespace| if (namespace == Markup.svg_namespace) adjusted(raw, svg_tags) else raw

adjust_svg_attribute = |raw| adjusted(raw, svg_attributes)

adjusted : Str, List((Str, Str)) -> Str
adjusted = |raw, pairs| {
	for pair in pairs {
		if pair.0 == raw {
			return pair.1
		}
	}
	raw
}

svg_tags = [("altglyph", "altGlyph"), ("altglyphdef", "altGlyphDef"), ("altglyphitem", "altGlyphItem"), ("animatecolor", "animateColor"), ("animatemotion", "animateMotion"), ("animatetransform", "animateTransform"), ("clippath", "clipPath"), ("feblend", "feBlend"), ("fecolormatrix", "feColorMatrix"), ("fecomponenttransfer", "feComponentTransfer"), ("fecomposite", "feComposite"), ("feconvolvematrix", "feConvolveMatrix"), ("fediffuselighting", "feDiffuseLighting"), ("fedisplacementmap", "feDisplacementMap"), ("fedistantlight", "feDistantLight"), ("fedropshadow", "feDropShadow"), ("feflood", "feFlood"), ("fefunca", "feFuncA"), ("fefuncb", "feFuncB"), ("fefuncg", "feFuncG"), ("fefuncr", "feFuncR"), ("fegaussianblur", "feGaussianBlur"), ("feimage", "feImage"), ("femerge", "feMerge"), ("femergenode", "feMergeNode"), ("femorphology", "feMorphology"), ("feoffset", "feOffset"), ("fepointlight", "fePointLight"), ("fespecularlighting", "feSpecularLighting"), ("fespotlight", "feSpotLight"), ("fetile", "feTile"), ("feturbulence", "feTurbulence"), ("foreignobject", "foreignObject"), ("glyphref", "glyphRef"), ("lineargradient", "linearGradient"), ("radialgradient", "radialGradient"), ("textpath", "textPath")]

svg_attributes = [("attributename", "attributeName"), ("attributetype", "attributeType"), ("basefrequency", "baseFrequency"), ("baseprofile", "baseProfile"), ("calcmode", "calcMode"), ("clippathunits", "clipPathUnits"), ("diffuseconstant", "diffuseConstant"), ("edgemode", "edgeMode"), ("filterunits", "filterUnits"), ("glyphref", "glyphRef"), ("gradienttransform", "gradientTransform"), ("gradientunits", "gradientUnits"), ("kernelmatrix", "kernelMatrix"), ("kernelunitlength", "kernelUnitLength"), ("keypoints", "keyPoints"), ("keysplines", "keySplines"), ("keytimes", "keyTimes"), ("lengthadjust", "lengthAdjust"), ("limitingconeangle", "limitingConeAngle"), ("markerheight", "markerHeight"), ("markerunits", "markerUnits"), ("markerwidth", "markerWidth"), ("maskcontentunits", "maskContentUnits"), ("maskunits", "maskUnits"), ("numoctaves", "numOctaves"), ("pathlength", "pathLength"), ("patterncontentunits", "patternContentUnits"), ("patterntransform", "patternTransform"), ("patternunits", "patternUnits"), ("pointsatx", "pointsAtX"), ("pointsaty", "pointsAtY"), ("pointsatz", "pointsAtZ"), ("preservealpha", "preserveAlpha"), ("preserveaspectratio", "preserveAspectRatio"), ("primitiveunits", "primitiveUnits"), ("refx", "refX"), ("refy", "refY"), ("repeatcount", "repeatCount"), ("repeatdur", "repeatDur"), ("requiredextensions", "requiredExtensions"), ("requiredfeatures", "requiredFeatures"), ("specularconstant", "specularConstant"), ("specularexponent", "specularExponent"), ("spreadmethod", "spreadMethod"), ("startoffset", "startOffset"), ("stddeviation", "stdDeviation"), ("stitchtiles", "stitchTiles"), ("surfacescale", "surfaceScale"), ("systemlanguage", "systemLanguage"), ("tablevalues", "tableValues"), ("targetx", "targetX"), ("targety", "targetY"), ("textlength", "textLength"), ("viewbox", "viewBox"), ("viewtarget", "viewTarget"), ("xchannelselector", "xChannelSelector"), ("ychannelselector", "yChannelSelector"), ("zoomandpan", "zoomAndPan")]

whitespace = |text| text.to_utf8().all(Tok.space)

void = |n| ["area", "base", "basefont", "bgsound", "br", "col", "embed", "hr", "img", "image", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"].contains(n)

formatting = |n| ["a", "b", "big", "code", "em", "font", "i", "nobr", "s", "small", "strike", "strong", "tt", "u"].contains(n)

special = |n| ["address", "applet", "area", "article", "aside", "base", "basefont", "bgsound", "blockquote", "body", "br", "button", "caption", "center", "col", "colgroup", "dd", "details", "dir", "div", "dl", "dt", "embed", "fieldset", "figcaption", "figure", "footer", "form", "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", "hr", "html", "iframe", "img", "input", "li", "link", "listing", "main", "marquee", "menu", "meta", "nav", "noembed", "noframes", "noscript", "object", "ol", "p", "param", "plaintext", "pre", "script", "search", "section", "select", "source", "style", "summary", "table", "tbody", "td", "template", "textarea", "tfoot", "th", "thead", "title", "tr", "track", "ul", "wbr", "xmp"].contains(n)

expect {
	parsed = HtmlTree.parse("<!doctype html><p>a<div>b</div>", False, {}, {})?
	names(parsed.tree) == ["html", "head", "body", "p", "div"] and texts(parsed.tree) == ["a", "b"]
}
expect {
	parsed = HtmlTree.parse("<ul><li>a<li>b</ul>", True, {}, {})?
	names(parsed.tree) == ["html", "ul", "li", "li"] and parsed.diagnostics.is_empty()
}
expect {
	parsed = HtmlTree.parse("<table><tr><td>a<td>b</table>", True, {}, {})?
	names(parsed.tree) == ["html", "table", "tbody", "tr", "td", "td"]
}
expect {
	parsed = HtmlTree.parse("<table>outside<tr><td>cell</td></tr></table>", True, {}, { recovery: Browser })?
	root_names = parsed.tree.roots.map(
		|i| match (parsed.tree.nodes.get(i) ?? empty_node).kind {
			Element(n) => n.local
			Text(t) => t
			_ => "?"
		},
	)
	root_names == ["outside", "table"] and !parsed.diagnostics.is_empty()
}
expect {
	parsed = HtmlTree.parse("<svg viewbox='0 0 10 10'><lineargradient/><foreignObject><div>x</div></foreignObject></svg><math><mtext><b>y</b></mtext></math>", True, {}, {})?
	named = parsed.tree.nodes.keep_if(
		|n| match n.kind {
			Element(_) => True
			_ => False
		},
	).map(
		|n| match n.kind {
			Element(name) => (name.local, name.namespace)
			_ => ("", NoNamespace)
		},
	)
	named.contains(("linearGradient", Uri(Markup.svg_namespace))) and named.contains(("div", Uri(Markup.html_namespace))) and named.contains(("b", Uri(Markup.html_namespace)))
}
expect {
	parsed = HtmlTree.parse("<textarea>\n&lt;b&gt;</textarea><script>if (a < b) x='&amp;'</script>", True, {}, {})?
	texts(parsed.tree) == ["<b>", "if (a < b) x='&amp;'"]
}
expect {
	parsed = HtmlTree.parse("<b><i>x</b>y</i>", True, {}, { recovery: Browser })?
	names(parsed.tree) == ["html", "b", "i", "i"] and texts(parsed.tree) == ["x", "y"] and !parsed.diagnostics.is_empty()
}
expect {
	parsed = HtmlTree.parse("<template><table><tr><td>x</td></tr></table></template>", True, {}, {})?
	names(parsed.tree) == ["html", "template", "table", "tbody", "tr", "td"]
}
expect HtmlTree.parse("<div/>", True, {}, {}).is_err()
expect HtmlTree.parse("<div>x</div>", True, {}, { max_depth: 1 }).is_err()
expect HtmlTree.parse("abc", True, {}, { max_source_bytes: 2 }).is_err()

names = |tree| tree.nodes.keep_if(
	|n| match n.kind {
		Element(_) => True
		_ => False
	},
).map(
	|n| match n.kind {
		Element(name) => name.local
		_ => ""
	},
)

texts = |tree| tree.nodes.keep_if(
	|n| match n.kind {
		Text(_) => True
		_ => False
	},
).map(
	|n| match n.kind {
		Text(text) => text
		_ => ""
	},
)

expect HtmlTree.parse("<span/>x", True, {}, { max_diagnostics: 0 }).is_err()
expect HtmlTree.parse("<div>\n<span>x</span></div>", True, {}, { max_depth: 2 }).map_err(|InvalidMarkup(e)| (e.line, e.column)) == Err((2, 1))
expect {
	p = HtmlTree.parse("a\u(0)b", True, {}, { recovery: Browser })?
	texts(p.tree) == ["ab"] and !p.diagnostics.is_empty()
}
expect {
	p = HtmlTree.parse("<svg>a\u(0)b</svg>", True, {}, { recovery: Browser })?
	texts(p.tree) == ["a\u(FFFD)b"]
}
expect HtmlTree.parse("<!--x--><!--y-->", True, {}, { max_nodes: 2 }).is_err()

work_limit : U64, U64 -> U64
work_limit = |per_byte, size| {
	maximum = 0xFFFFFFFFFFFFFFFF.U64
	if (per_byte > (maximum - 4096) // (size + 1)) maximum else 4096 + per_byte * (size + 1)
}
expect HtmlTree.parse("<p>x</p>", True, {}, { max_work_per_byte: 0xFFFFFFFFFFFFFFFF }).is_ok()
expect {
	p = HtmlTree.parse("<table><div>x</div><tr><td>y</table>", True, {}, { recovery: Browser })?
	div = p.tree.nodes.get(p.tree.roots.first()?)?
	div.children.len() == 1 and Markup.text(p.tree, p.tree.roots.first()?) == "x"
}

expect {
	p = HtmlTree.parse("<g></path>X", True, { namespace: Markup.svg_namespace, name: "path" }, { recovery: Browser })?
	Markup.text(p.tree, p.tree.roots.first()?) == "X"
}
expect HtmlTree.parse("</path>X", True, { namespace: Markup.svg_namespace, name: "path" }, { recovery: Browser }).is_ok()
expect HtmlTree.parse("</foreignObject>X", True, { namespace: Markup.svg_namespace, name: "foreignObject" }, { recovery: Browser }).is_ok()
expect HtmlTree.parse("</math>X", True, { namespace: Markup.mathml_namespace, name: "math" }, { recovery: Browser }).is_ok()
