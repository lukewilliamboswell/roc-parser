import Markup
import HtmlTokenizer as Tok
import Literal
import MarkupHtmlError

## HTML tree construction. The open-elements and active-formatting stacks are
## separate from the indexed output tree so recovery can reparent existing nodes.
HtmlTree :: [].{
	Context := { name : Str ?? "body", namespace : Str ?? "http://www.w3.org/1999/xhtml", scripting : Bool ?? True }
	parse : Str, Bool, Context, Markup.Props -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	parse = |source, fragment, context, props| {
		if source.count_utf8_bytes() > props.max_source_bytes {
			return Err(InvalidMarkup({ problem: SourceTooLarge(props.max_source_bytes), line: 1, column: 1 }))
		}
		initial : State
		initial = { nodes: [], roots: [], parents: Dict.empty(), stack: [], active: [], modes: [], mode: Initial, head: Err(Missing), form: Err(Missing), fragment, context, diagnostics: [], max_diagnostics: props.max_diagnostics, foster: False, frameset_ok: True, skip_lf: False, work: 0, work_limit: work_limit(props.max_work_per_byte, source.count_utf8_bytes()), element_count: 0, diagnostics_exceeded: False, quirks: False, token_span: { start: 0, end: 0 }, token_end_name: "", plaintext: False, pending_text: [] }
		var $state = initial
		if fragment {
			$state = insert_element($state, "html", Markup.html_namespace, [], Implied(0), True)
			$state = { ..$state, mode: context_mode(context.name) }
		}
		var $reader = { bytes: source.to_utf8(), pos: 0, diagnostics: Tok.input_diagnostics_with(source.to_utf8(), props.max_diagnostics) }
		var $done = False
		var $source_regions = []
		while !$done {
			mode = content_mode($state)
			foreign = adjusted_namespace($state) != Markup.html_namespace
			next = Tok.next($reader, mode, foreign)
			$reader = next.reader
			match source_token(next.token, $reader.bytes) {
				Ok(region) => {
					$source_regions = $source_regions.append(region)
				}
				Err(_) => {}
			}

			if $state.skip_lf {
				$state = { ..$state, skip_lf: False }
				match next.token.kind {
					Characters(text) => {
						bytes = text.to_utf8()
						if bytes.first() == Ok('\n') {
							trimmed = { ..next.token, kind: Characters(Str.from_utf8_lossy(bytes.drop_first(1))), span: { start: skipped_lf_end($reader.bytes, next.token.span), end: next.token.span.end } }
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
			if ($state.stack.len() - if (fragment) 1 else 0) > props.max_depth {
				return Err(InvalidMarkup({ problem: LimitExceeded(Depth), line: at.line, column: at.column }))
			}
			if ($state.element_count - if (fragment) 1 else 0) > props.max_elements {
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
		remaining_open = $state.stack
		for id in remaining_open {
			$state = finish_node($state, id)
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
		Ok({ source: Markup.Source.{ bytes: source.to_utf8(), tokens: $source_regions }, tree: { nodes: $state.nodes, roots }, diagnostics })
	}
}

Mode : [Initial, BeforeHtml, BeforeHead, InHead, InHeadNoscript, AfterHead, InBody, InTable, InCaption, InColgroup, InTableBody, InRow, InCell, InTemplate, AfterBody, AfterAfterBody, InFrameset, AfterFrameset, AfterAfterFrameset]

Active : [Entry(U64), Marker]

State : { nodes : List(Markup.Node), roots : List(U64), parents : Dict(U64, U64), stack : List(U64), active : List(Active), modes : List(Mode), mode : Mode, head : Try(U64, [Missing]), form : Try(U64, [Missing]), fragment : Bool, context : HtmlTree.Context, diagnostics : List(Markup.Error), max_diagnostics : U64, foster : Bool, frameset_ok : Bool, skip_lf : Bool, work : U64, work_limit : U64, element_count : U64, diagnostics_exceeded : Bool, quirks : Bool, token_span : Markup.Span, token_end_name : Str, plaintext : Bool, pending_text : List({ text : Str, span : Markup.Span }) }

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
	foster = s.foster and ["table", "tbody", "tfoot", "thead", "tr"].contains(current(s))
	var $parent = s.stack.last().map_err(|_| Missing)
	var $before = Err(Missing)
	if foster {
		template_result = last_named(s, "template")
		table_result = last_named(s, "table")
		use_template = match (template_result, table_result) {
			(Ok(template_id), Ok(table_id)) => (stack_position(s, template_id) ?? 0) > (stack_position(s, table_id) ?? 0)
			(Ok(_), Err(_)) => True
			_ => False
		}
		if use_template {
			$parent = template_result
		} else {
			match table_result {
				Ok(table_id) => {
					match find_parent(s, table_id) {
						Ok(parent) => {
							$parent = Ok(parent)
							$before = index_of((node_at(s, parent)).children, table_id)
						}
						Err(_) => {
							position = stack_position(s, table_id) ?? 0
							$parent = if (position > 0) s.stack.get(position - 1).map_err(|_| Missing) else Err(Missing)
						}
					}
				}
				_ => {}
			}
		}
	}
	added_element = match node.kind {
		Element(_) => 1
		_ => 0
	}
	parents = match $parent {
		Ok(parent) => s.parents.insert(index, parent)
		Err(_) => s.parents
	}
	next = { ..s, nodes: s.nodes.append(node), parents, work: s.work + 1, element_count: s.element_count + added_element }
	match $parent {
		Ok(parent) => {
			before = $before
			edit_node(
				next,
				parent,
				|previous| {
					children = match before {
						Ok(at) => previous.children.insert(at, index) ?? []
						Err(_) => previous.children.append(index)
					}
					{ ..previous, children }
				},
			)
		}
		Err(_) => { ..next, roots: next.roots.append(index) }
	}
}

# Move the old node out before editing its child/attribute lists. Otherwise the
# node retained in the arena forces every append through copy-on-write.
edit_node : State, U64, (Markup.Node -> Markup.Node) -> State
edit_node = |s, index, edit| {
	if index >= s.nodes.len() {
		return s
	}
	moved = s.nodes.replace(index, empty_node) ?? { list: [], prev: empty_node }
	replacement = edit(moved.prev)
	{ ..s, nodes: moved.list.set(index, replacement) ?? [] }
}

insert_element = |s, name, namespace, attrs, origin, push| {
	index = s.nodes.len()
	next = append_node(s, { kind: Element({ local: name, namespace: Uri(namespace), prefix: Unprefixed }), attributes: attrs, children: [], origin })
	if (push) { ..next, stack: next.stack.append(index) } else next
}

insert_tag = |s, tag, namespace, span, push| {
	next = insert_element(s, adjust_tag(tag.name, namespace), namespace, tag.attributes.map(|a| { name: attribute_name(a.name, namespace), value: a.value, span: a.span }), Authored(span), push)
	if namespace == Markup.html_namespace and tag.self_closing and push {
		problem(next, NonVoidHtmlElementStartTagWithTrailingSolidus, span.start)
	} else {
		next
	}
}

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
						return { ..s, nodes: s.nodes.set(last, { ..previous, kind: Text(Str.concat(before, text)), origin }) ?? [] }
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
						Text(before) => return { ..s, nodes: s.nodes.set(previous_id, { ..previous, kind: Text(Str.concat(before, text)), origin: Implied(origin_offset(previous.origin)) }) ?? [] }
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
		Ok(id) => close_from(s, stack_position(s, id) ?? 0)
		Err(_) => s
	}
}

close_from : State, U64 -> State
close_from = |s, position| {
	closing = s.stack.drop_first(position)
	remaining = s.stack.take_first(position)
	var $next = { ..s, stack: remaining }
	for id in closing {
		$next = finish_node($next, id)
	}
	$next
}

finish_node : State, U64 -> State
finish_node = |s, id| {
	token_span = s.token_span
	token_name = s.token_end_name
	edit_node(
		s,
		id,
		|node| {
			match node.origin {
				Authored(span) => {
					explicit = match node.kind {
						Element(name) => Tok.lower(name.local) == token_name and token_name != ""
						_ => False
					}
					end = if (explicit) token_span.end else token_span.start
					{ ..node, origin: Authored({ start: span.start, end: if (end > span.end) end else span.end }) }
				}
				_ => node
			}
		},
	)
}

pop_current : State -> State
pop_current = |s| if (s.stack.is_empty()) s else close_from(s, s.stack.len() - 1)

pop_mode = |s, mode| {
	popped = pop_current(s)
	{ ..popped, mode }
}

last_named : State, Str -> Try(U64, [Missing])
last_named = |s, name| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		if namespace_at(s, id) == Markup.html_namespace and name_at(s, id) == name {
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
find_parent = |s, id| s.parents.get(id).map_err(|_| Missing)

in_scope = |s, name, kind| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		n = name_at(s, id)
		ns = namespace_at(s, id)
		if ns == Markup.html_namespace and n == name {
			return True
		}
		if ns == Markup.html_namespace {
			if ["html", "table", "template"].contains(n) {
				return False
			}
			if kind != TableScope and ["applet", "caption", "td", "th", "marquee", "object", "select"].contains(n) {
				return False
			}
			if kind == ListScope and ["ol", "ul"].contains(n) {
				return False
			}
			if kind == ButtonScope and n == "button" {
				return False
			}
		} else if kind != TableScope {
			if ns == Markup.mathml_namespace and ["mi", "mo", "mn", "ms", "mtext", "annotation-xml"].contains(n) {
				return False
			}
			if ns == Markup.svg_namespace and ["foreignObject", "desc", "title"].contains(n) {
				return False
			}
		}
	}
	False
}

implied = |s, except| {
	var $s = s
	while ["dd", "dt", "li", "optgroup", "option", "p", "rb", "rp", "rt", "rtc"].contains(current($s)) and current($s) != except {
		$s = pop_current($s)
	}
	$s
}

close_p = |s| close_to(implied(s, "p"), "p")

context_mode = |name| match name {
	"select" => InBody
	"td" | "th" => InCell
	"tr" => InRow
	"tbody" | "thead" | "tfoot" => InTableBody
	"caption" => InCaption
	"colgroup" => InColgroup
	"table" => InTable
	"template" => InTemplate
	"head" => InBody
	"html" => BeforeHead
	"frameset" => InFrameset
	_ => InBody
}

reset_mode = |s| {
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		n = if ($i == 0 and s.fragment) s.context.name else name_at(s, s.stack.get($i) ?? 0)
		if ["td", "th", "tr", "tbody", "thead", "tfoot", "caption", "colgroup", "table", "template", "head", "frameset"].contains(n) {
			return { ..s, mode: if (n == "template") (s.modes.last() ?? InTemplate) else if (n == "head") InHead else context_mode(n) }
		}
		if n == "body" {
			return { ..s, mode: InBody }
		}
		if n == "html" {
			return { ..s, mode: if (s.fragment) context_mode(s.context.name) else if (s.head.is_ok()) AfterHead else BeforeHead }
		}
	}
	{ ..s, mode: InBody }
}

content_mode = |s| {
	if s.plaintext {
		return Plaintext
	}
	if s.fragment and s.stack.len() == 1 and s.context.namespace == Markup.html_namespace {
		n = s.context.name
		if n == "title" or n == "textarea" {
			return RcdataFragment
		}
		if n == "script" {
			return ScriptFragment
		}
		if n == "plaintext" {
			return Plaintext
		}
		if ["style", "xmp", "iframe", "noembed", "noframes"].contains(n) or (n == "noscript" and s.context.scripting) {
			return RawtextFragment
		}
	}

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
process = |initial_state, token, scripting| {
	end_name = match token.kind {
		End(name) => name
		_ => ""
	}
	var $prepared = initial_state
	match token.kind {
		Characters(_) => {}
		_ => {
			$prepared = flush_table_text($prepared)
		}
	}
	state = { ..$prepared, token_span: token.span, token_end_name: end_name }
	match token.kind {
		Characters(text) if [InTable, InTableBody, InRow].contains(state.mode) and namespace_at(state, state.stack.last() ?? 0) == Markup.html_namespace and ["table", "tbody", "tfoot", "thead", "tr"].contains(current(state)) => {
			return { ..state, pending_text: state.pending_text.append({ text, span: token.span }) }
		}
		_ => {}
	}
	if state.work > state.work_limit {
		return state
	}
	# Raw text is a tokenizer mode and a tree-construction text mode. The
	# surrounding insertion mode (table/head/body) resumes after its end tag.
	if !state.stack.is_empty() and namespace_at(state, state.stack.last() ?? 0) == Markup.html_namespace and content_mode(state) != Data and !state.plaintext and current(state) != "html" {
		match token.kind {
			Characters(text) => return insert_text(state, text, token.span)
			End(name) if name == current(state) => return pop_current(state)
			Eof => {
				next = problem(state, EofWithOpenElements, token.span.start)
				return process_html(pop_current(next), token, scripting)
			}
			_ => {}
		}
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
	while $reprocess {
		$count = $count + 1
		$s = { ..$s, work: $s.work + 1 }
		if $s.work > $s.work_limit {
			return $s
		}
		$reprocess = False
		mode = $s.mode
		match (mode, token.kind) {
			(_, Eof) if last_named($s, "template").is_ok() => {
				$s = problem($s, EofInTemplate, token.span.start)
				$s = end_template($s, token.span.start)
				$reprocess = True
			}
			(_, ProcessingInstruction(pi)) => {
				$s = append_misc($s, { kind: ProcessingInstruction(pi), attributes: [], children: [], origin: Authored(token.span) })
			}
			(_, Comment(text)) => {
				$s = append_misc($s, { kind: Comment(text), attributes: [], children: [], origin: Authored(token.span) })
			}
			(Initial, Characters(text)) if whitespace(text) => {}
			(Initial, Doctype(d)) => {
				$s = append_node($s, { kind: Doctype({ name: d.name, public_id: d.public_id, system_id: d.system_id }), attributes: [], children: [], origin: Authored(token.span) })
				if d.name != "html" or d.has_public or (d.has_system and d.system_id != "about:legacy-compat") {
					$s = problem($s, NonConformingDoctype, token.span.start)
				}
				$s = { ..$s, mode: BeforeHtml, quirks: doctype_quirks(d) }
			}
			(Initial, _) => {
				$s = problem($s, MissingDoctype, token.span.start)
				$s = { ..$s, mode: BeforeHtml, quirks: True }
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
			(BeforeHtml, End(n)) if !["head", "body", "html", "br"].contains(n) => {
				$s = problem($s, UnexpectedEndTag, token.span.start)
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
			(BeforeHead, End(n)) if !["head", "body", "html", "br"].contains(n) => {
				$s = problem($s, UnexpectedEndTag, token.span.start)
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
				$s = pop_mode($s, AfterHead)
			}
			(InHead, End(n)) if ["title", "style", "script", "noframes", "noscript"].contains(n) and current($s) == n => {
				$s = pop_current($s)
			}
			(InHead, Start(t)) if t.name == "noscript" and !scripting => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InHeadNoscript }
			}
			(InHead, Start(t)) if t.name == "head" => {
				$s = problem($s, UnexpectedHeadStart, token.span.start)
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
			(InHead, End(n)) if !["body", "html", "br"].contains(n) => {
				$s = problem($s, UnexpectedEndTag, token.span.start)
			}
			(InHeadNoscript, End("noscript")) => {
				$s = pop_mode($s, InHead)
			}
			(InHeadNoscript, Characters(t)) if whitespace(t) => {
				$s = insert_text($s, t, token.span)
			}
			(InHeadNoscript, Start(t)) if t.name == "html" => {
				$s = body($s, token)
			}
			(InHeadNoscript, Start(t)) if ["basefont", "bgsound", "link", "meta", "noframes", "style"].contains(t.name) => {
				$s = process_html({ ..$s, mode: InHead }, token, scripting)
				$s = { ..$s, mode: InHeadNoscript }
			}
			(InHeadNoscript, Start(t)) if t.name == "head" or t.name == "noscript" => {
				$s = problem($s, UnexpectedHeadStart, token.span.start)
			}
			(InHeadNoscript, End(n)) if n != "br" => {
				$s = problem($s, UnexpectedEndTag, token.span.start)
			}
			(InHeadNoscript, _) => {
				$s = problem($s, UnexpectedHeadStart, token.span.start)
				$s = pop_mode($s, InHead)
				$reprocess = True
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
				$s = { ..$s, mode: InBody, frameset_ok: False }
			}
			(AfterHead, Start(t)) if t.name == "frameset" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InFrameset }
			}
			(AfterHead, Start(t)) if ["base", "basefont", "bgsound", "link", "meta", "script", "style", "template", "title"].contains(t.name) => {
				$s = problem($s, HeadContentAfterHead, token.span.start)
				match $s.head {
					Ok(h) => {
						old = $s.stack
						$s = { ..$s, stack: old.append(h), mode: InHead }
						$s = process($s, token, scripting)
						$s = { ..$s, stack: $s.stack.keep_if(|i| i != h), mode: if ($s.mode == InHead) AfterHead else $s.mode }
					}
					_ => {}
				}
			}
			(AfterHead, End(n)) if !["body", "html", "br"].contains(n) => {
				$s = problem($s, UnexpectedEndTag, token.span.start)
			}
			(AfterHead, _) => {
				$s = insert_element($s, "body", Markup.html_namespace, [], Implied(token.span.start), True)
				$s = { ..$s, mode: InBody, frameset_ok: True }
				$reprocess = True
			}
			(InBody, Eof) if last_named($s, "template").is_ok() => {
				$s = problem($s, EofInTemplate, token.span.start)
				$s = end_template($s, token.span.start)
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
					Start(t) if ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(t.name) and !in_scope($s, "td", TableScope) and !in_scope($s, "th", TableScope) => {
						$s = problem($s, UnexpectedCellEnd, token.span.start)
					}
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
					End("caption") | End("table") if !in_scope($s, "caption", TableScope) => {
						$s = problem($s, UnexpectedTableEnd, token.span.start)
					}
					Start(t) if ["caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr"].contains(t.name) and !in_scope($s, "caption", TableScope) => {
						$s = problem($s, UnexpectedTableElement, token.span.start)
					}
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
					Start(t) if t.name == "template" => {
						$s = start_template($s, t, token.span)
					}
					Characters(t) if whitespace(t) => {
						$s = insert_text($s, t, token.span)
					}
					Start(t) if t.name == "col" => {
						$s = insert_tag($s, t, Markup.html_namespace, token.span, False)
					}
					End("colgroup") if current($s) != "colgroup" => {
						$s = problem($s, UnexpectedColEnd, token.span.start)
					}
					End("colgroup") => {
						$s = pop_mode($s, InTable)
					}
					End("col") => {
						$s = problem($s, UnexpectedColEnd, token.span.start)
					}
					Eof => {}
					_ if current($s) != "colgroup" => {
						$s = problem($s, UnexpectedTableElement, token.span.start)
					}
					_ => {
						$s = pop_mode($s, InTable)
						$reprocess = True
					}
				}
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
			(AfterBody, Start(t)) | (AfterAfterBody, Start(t)) | (InFrameset, Start(t)) | (AfterFrameset, Start(t)) if t.name == "html" => {
				$s = body($s, token)
			}
			(AfterBody, Characters(t)) if whitespace(t) => {
				$s = body($s, token)
			}
			(AfterBody, End("html")) => {
				if $s.fragment {
					$s = problem($s, UnexpectedEndTag, token.span.start)
				} else {
					$s = { ..$s, mode: AfterAfterBody }
				}
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
			(InFrameset, End("frameset")) if current($s) == "html" => {
				$s = problem($s, UnexpectedFramesetContent, token.span.start)
			}
			(InFrameset, End("frameset")) => {
				$s = pop_current($s)
				if current($s) != "frameset" {
					$s = { ..$s, mode: AfterFrameset }
				}
			}
			(AfterFrameset, Characters(t)) if whitespace(t) => {
				$s = insert_text($s, t, token.span)
			}
			(InFrameset, Start(t)) | (AfterFrameset, Start(t)) if t.name == "noframes" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			}
			(InFrameset, Eof) | (AfterFrameset, Eof) => {}
			(AfterFrameset, End("html")) => {
				$s = { ..$s, mode: AfterAfterFrameset }
			}
			(AfterAfterFrameset, Eof) => {}
			(AfterAfterFrameset, Characters(t)) if whitespace(t) => {
				$s = body($s, token)
			}
			(AfterAfterFrameset, Start(t)) if t.name == "html" => {
				$s = body($s, token)
			}
			(AfterAfterFrameset, Start(t)) if t.name == "noframes" => {
				$s = insert_tag($s, t, Markup.html_namespace, token.span, True)
			}
			(AfterAfterFrameset, _) => {
				$s = problem($s, UnexpectedFramesetContent, token.span.start)
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
				if parsing_template($s) {
					return problem($s, UnexpectedBodyStart, token.span.start)
				}
				if !$s.stack.is_empty() {
					$s = merge_attributes($s, $s.stack.first() ?? 0, tag)
				}
			} else if name == "body" {
				if parsing_template($s) {
					return problem($s, UnexpectedBodyStart, token.span.start)
				}
				match last_named($s, "body") {
					Ok(id) => {
						$s = merge_attributes($s, id, tag)
						$s = { ..$s, frameset_ok: False }
					}
					_ => {
						$s = problem($s, UnexpectedBodyStart, token.span.start)
					}
				}
			} else if name == "frameset" {
				if $s.frameset_ok and last_named($s, "body").is_ok() and !parsing_template($s) {
					body_id = last_named($s, "body") ?? 0
					$s = detach($s, body_id)
					$s = { ..$s, stack: $s.stack.take_first(1) }
					$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
					$s = { ..$s, mode: InFrameset }
				} else {
					$s = problem($s, UnexpectedFramesetContent, token.span.start)
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
					$s = pop_current($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
			} else if name == "pre" or name == "listing" {
				if in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, skip_lf: True, frameset_ok: False }
			} else if name == "form" {
				if $s.form.is_ok() and !parsing_template($s) {
					$s = problem($s, NestedForm, token.span.start)
				}
				else {
					if in_scope($s, "p", ButtonScope) {
						$s = close_p($s)
					}
					id = $s.nodes.len()
					$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
					if !parsing_template($s) {
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
					if special_at($s, $s.stack.get($i) ?? 0) and !["address", "div", "p"].contains(n) {
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
				$s = { ..$s, plaintext: True }
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
				if !$s.quirks and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
				$s = { ..$s, mode: InTable, frameset_ok: False }
			} else if name == "input" and $s.fragment and $s.context.name == "select" {
				$s = problem($s, UnexpectedSelectContent, token.span.start)
			} else if void(name) {
				if name == "hr" and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				if name == "input" and in_scope($s, "select", DefaultScope) {
					$s = problem($s, UnexpectedSelectContent, token.span.start)
					$s = close_to($s, "select")
				}
				if name == "hr" and in_scope($s, "select", DefaultScope) {
					$s = implied($s, "")
				}
				$s = reconstruct($s)
				adjusted = if (name == "image") { ..tag, name: "img" } else tag
				$s = insert_tag($s, adjusted, Markup.html_namespace, token.span, False)
				if name == "hr" and in_scope($s, "p", ButtonScope) {
					$s = close_p($s)
				}
				if !["param", "source", "track"].contains(name) and (name != "input" or !tag.attributes.any(|a| a.name == "type" and Tok.lower(a.value) == "hidden")) {
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
				if $s.fragment and $s.context.name == "select" {
					$s = problem($s, UnexpectedSelectContent, token.span.start)
				} else if in_scope($s, "select", DefaultScope) {
					$s = problem($s, UnexpectedSelectContent, token.span.start)
					$s = close_to($s, "select")
				} else {
					$s = reconstruct($s)
					$s = insert_tag($s, tag, Markup.html_namespace, token.span, True)
					$s = { ..$s, frameset_ok: False }
				}
			} else if name == "option" or name == "optgroup" {
				if in_scope($s, "select", DefaultScope) {
					$s = implied($s, if (name == "option") "optgroup" else "")
					if in_scope($s, "option", DefaultScope) or (name == "optgroup" and in_scope($s, "optgroup", DefaultScope)) {
						$s = problem($s, UnexpectedSelectContent, token.span.start)
					}
				} else if current($s) == "option" {
					$s = pop_current($s)
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
			}
		}
		End(name) => {
			if name == "body" or name == "html" {
				if in_scope($s, "body", DefaultScope) {
					$s = { ..$s, mode: if (name == "html" and !$s.fragment) AfterAfterBody else AfterBody }
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
			} else if ["address", "article", "aside", "blockquote", "center", "details", "dialog", "dir", "div", "dl", "fieldset", "figcaption", "figure", "footer", "header", "hgroup", "listing", "main", "menu", "nav", "ol", "pre", "search", "section", "select", "summary", "ul"].contains(name) {
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
			else if name == "form" and !parsing_template($s) {
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
			} else if name == "form" {
				if in_scope($s, "form", DefaultScope) {
					$s = close_to(implied($s, ""), "form")
				} else {
					$s = problem($s, UnexpectedFormEnd, token.span.start)
				}
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
	{ ..s, nodes: s.nodes.set(id, { ..node, attributes: node.attributes.concat(extra) }) ?? [] }
}

generic_end = |s, name, offset| {
	var $s = s
	var $i = s.stack.len()
	while $i > 0 {
		$i = $i - 1
		id = s.stack.get($i) ?? 0
		n = name_at(s, id)
		if namespace_at($s, id) == Markup.html_namespace and n == name {
			$s = implied($s, name)
			if current($s) != name {
				$s = problem($s, MisnestedEndTag, offset)
			}
			return close_from($s, $i)
		}
		if special_at($s, id) {
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
					if special_at($s, id) {
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
							original_stack = $s.stack
							target_position = stack_position($s, common) ?? 0
							temporary = append_node({ ..$s, foster: True, stack: original_stack.take_first(target_position + 1) }, moved)
							$s = replace_child_id({ ..temporary, stack: original_stack, foster: False, nodes: temporary.nodes.take_first(old_count) }, old_count, $last)
						} else {
							$s = reparent($s, $last, common)
						}
						original = node_at($s, formatting_id)
						new_formatting = $s.nodes.len()
						block_node = node_at($s, block)
						$s = { ..$s, nodes: $s.nodes.append({ ..original, children: block_node.children, origin: Implied(offset) }) }
						parents = block_node.children.fold($s.parents, |dict, child| dict.insert(child, new_formatting)).insert(new_formatting, block)
						$s = { ..$s, parents, nodes: $s.nodes.set(block, { ..block_node, children: [new_formatting] }) ?? [] }
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
	previous_parent = find_parent(s, id)
	removed = { ..s, parents: s.parents.remove(id) }
	match previous_parent {
		Ok(parent) => edit_node(removed, parent, |previous| { ..previous, children: previous.children.keep_if(|child| child != id) })
		Err(_) => { ..removed, roots: removed.roots.keep_if(|child| child != id) }
	}
}

reparent = |s, id, parent| {
	detached = detach(s, id)
	edit_node({ ..detached, parents: detached.parents.insert(id, parent) }, parent, |previous| { ..previous, children: previous.children.append(id) })
}

replace_child_id = |s, old, new| {
	parent = s.parents.get(old)
	parents = match parent {
		Ok(p) => s.parents.remove(old).insert(new, p)
		_ => s.parents.remove(old)
	}
	{ ..s, parents, roots: s.roots.map(|id| if (id == old) new else id), nodes: s.nodes.map(|n| { ..n, children: n.children.map(|id| if (id == old) new else id) }) }
}

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
		$s = pop_current($s)
	}
	$s
}

clear_section_stack = |s| {
	var $s = s
	while !$s.stack.is_empty() and !["tbody", "tfoot", "thead", "template", "html"].contains(current($s)) {
		$s = pop_current($s)
	}
	$s
}

clear_row_stack = |s| {
	var $s = s
	while !$s.stack.is_empty() and !["tr", "template", "html"].contains(current($s)) {
		$s = pop_current($s)
	}
	$s
}

table_body = |state, token| {
	var $s = state
	match token.kind {
		Start(t) if ["caption", "col", "colgroup", "tbody", "tfoot", "thead"].contains(t.name) and !["tbody", "tfoot", "thead"].any(|n| in_scope($s, n, TableScope)) => {
			$s = problem($s, UnexpectedTableSectionToken, token.span.start)
		}
		End("table") if !["tbody", "tfoot", "thead"].any(|n| in_scope($s, n, TableScope)) => {
			$s = problem($s, UnexpectedTableSectionEnd, token.span.start)
		}

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
				$s = pop_mode($s, InTable)
			} else {
				$s = problem($s, UnexpectedTableSectionEnd, token.span.start)
			}
		}
		Start(t) if ["caption", "col", "colgroup", "tbody", "tfoot", "thead"].contains(t.name) => {
			$s = clear_section_stack($s)
			if current($s) != "html" {
				$s = pop_mode($s, InTable)
				$s = process($s, token, $s.context.scripting)
			}
		}
		End("table") => {
			$s = clear_section_stack($s)
			if current($s) != "html" {
				$s = pop_mode($s, InTable)
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
				$s = pop_mode($s, InTableBody)
			} else {
				$s = problem($s, UnexpectedRowEnd, token.span.start)
			}
		}
		Start(t) if ["caption", "col", "colgroup", "tbody", "tfoot", "thead", "tr"].contains(t.name) => {
			if in_scope($s, "tr", TableScope) {
				$s = clear_row_stack($s)
				$s = pop_mode($s, InTableBody)
				$s = process($s, token, $s.context.scripting)
			}
		}
		End(n) if ["table", "tbody", "tfoot", "thead"].contains(n) => {
			if in_scope($s, n, TableScope) {
				$s = clear_row_stack($s)
				$s = pop_mode($s, InTableBody)
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
			if !whitespace(Str.join_with(t.split_on("\u(0)"), "")) {
				$s = { ..$s, frameset_ok: False }
			}
			$s = insert_text($s, Str.join_with(t.split_on("\u(0)"), "\u(FFFD)"), token.span)
		}
		ProcessingInstruction(pi) => {
			$s = append_node($s, { kind: ProcessingInstruction(pi), attributes: [], children: [], origin: Authored(token.span) })
		}
		Comment(t) => {
			$s = append_node($s, { kind: Comment(t), attributes: [], children: [], origin: Authored(token.span) })
		}
		Start(t) => {
			breakout = ["b", "big", "blockquote", "body", "br", "center", "code", "dd", "div", "dl", "dt", "em", "embed", "h1", "h2", "h3", "h4", "h5", "h6", "head", "hr", "i", "img", "li", "listing", "menu", "meta", "nobr", "ol", "p", "pre", "ruby", "s", "small", "span", "strong", "strike", "sub", "sup", "table", "tt", "u", "ul", "var"].contains(t.name) or (t.name == "font" and t.attributes.any(|a| ["color", "face", "size"].contains(a.name)))
			if breakout {
				$s = problem($s, HtmlStartTagInForeignContent, token.span.start)
				while !$s.stack.is_empty() and namespace_at($s, $s.stack.last() ?? 0) != Markup.html_namespace and !integration($s) {
					$s = pop_current($s)
				}
				$s = process_html($s, token, $s.context.scripting)
			} else {
				$s = insert_tag($s, t, adjusted_namespace($s), token.span, !t.self_closing)
			}
		}
		End(n) if n == "p" or n == "br" => {
			$s = problem($s, HtmlStartTagInForeignContent, token.span.start)
			while !$s.stack.is_empty() and namespace_at($s, $s.stack.last() ?? 0) != Markup.html_namespace and !integration($s) {
				$s = pop_current($s)
			}
			$s = process_html($s, token, $s.context.scripting)
		}
		End(n) => {
			var $i = $s.stack.len()
			var $found = False
			while $i > 0 {
				$i = $i - 1
				id = $s.stack.get($i) ?? 0
				if namespace_at($s, id) == Markup.html_namespace {
					$s = process_html($s, token, $s.context.scripting)
					$found = True
					break
				}
				if Tok.lower(name_at($s, id)) == n {
					$s = close_from($s, $i)
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
		if raw == "xmlns:xlink" {
			return { namespace: Uri("http://www.w3.org/2000/xmlns/"), local: Str.from_utf8_lossy(raw.to_utf8().drop_first(6)), prefix: Prefix("xmlns") }
		}
		if ["xlink:actuate", "xlink:arcrole", "xlink:href", "xlink:role", "xlink:show", "xlink:title", "xlink:type"].contains(raw) {
			return { namespace: Uri("http://www.w3.org/1999/xlink"), local: Str.from_utf8_lossy(raw.to_utf8().drop_first(6)), prefix: Prefix("xlink") }
		}
		if ["xml:lang", "xml:space"].contains(raw) {
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

void = |n| ["area", "base", "basefont", "bgsound", "br", "embed", "hr", "img", "image", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"].contains(n)

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
expect HtmlTree.parse("<div><span>x</span></div>", True, {}, { max_depth: 1 }).is_err()
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
expect HtmlTree.parse("<div>\n<span>x</span></div>", True, {}, { max_depth: 1 }).map_err(|InvalidMarkup(e)| (e.line, e.column)) == Err((2, 1))
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

append_misc : State, Markup.Node -> State
append_misc = |s, node| {
	stack = s.stack
	if s.mode == AfterBody {
		n = append_node({ ..s, stack: s.stack.take_first(1) }, node)
		{ ..n, stack }
	} else if s.mode == AfterAfterBody or s.mode == AfterAfterFrameset {
		n = append_node({ ..s, stack: [] }, node)
		{ ..n, stack }
	} else {
		append_node(s, node)
	}
}
expect {
	p = HtmlTree.parse("<head><noscript><!--foo--></noscript></head>", False, { scripting: False }, { recovery: Browser })?
	head = p.tree.nodes.get(1)?
	head.children.len() == 1
}
expect {
	p = HtmlTree.parse("<!doctype html><html></p><!--foo-->", False, {}, { recovery: Browser })?
	names(p.tree) == ["html", "head", "body"]
}
expect {
	p = HtmlTree.parse("<!doctype html><div></body><!--foo-->", False, {}, { recovery: Browser })?
	html = p.tree.nodes.get(1)?
	html.children.len() == 3
}

expect {
	p = HtmlTree.parse("<!doctype html><?a first?><html><body><?b second></body><?c third?></html><?d?>", False, {}, {})?
	pis = p.tree.nodes.keep_oks(
		|n| match n.kind {
			ProcessingInstruction(pi) => Ok(pi.target)
			_ => Err(Skip)
		},
	)
	pis == ["a", "b", "c", "d"]
}

special_at = |s, id| {
	ns = namespace_at(s, id)
	n = name_at(s, id)
	if ns == Markup.html_namespace {
		special(n)
	}
	else if ns == Markup.mathml_namespace {
		["mi", "mo", "mn", "ms", "mtext", "annotation-xml"].contains(n)
	}
	else if ns == Markup.svg_namespace {
		["foreignObject", "desc", "title"].contains(n)
	}
	else {
		False
	}
}
expect {
	p = HtmlTree.parse("<select><div><i></div><option>option", True, {}, { recovery: Browser })?
	names(p.tree).contains("div") and names(p.tree).contains("i")
}
expect {
	p = HtmlTree.parse("<p><math><mi><p><h1>", True, {}, { recovery: Browser })?
	outer = p.tree.nodes.get(p.tree.roots.first()?)?
	outer.children.len() == 1 and p.tree.roots.len() == 1
}

expect {
	p = HtmlTree.parse("<p><table>", False, {}, { recovery: Browser })?
	root = p.tree.nodes.get(3)?
	root.children.len() == 1
}
expect {
	p = HtmlTree.parse("<svg></p><foo>", True, {}, { recovery: Browser })?
	p.tree.roots.len() == 3
}

expect {
	p = HtmlTree.parse("<table><script>foo</script><tr><td>x</table>", True, {}, {})?
	texts(p.tree) == ["foo", "x"] and p.tree.roots.len() == 1
}
expect HtmlTree.parse("<script/>", True, {}, {}).is_err()
expect HtmlTree.parse("<table><script>foo", True, {}, { recovery: Browser }).is_ok()

doctype_quirks = |d| {
	public_id = Tok.lower(d.public_id)
	system_id = Tok.lower(d.system_id)
	d.quirks or d.name != "html" or ["-//w3o//dtd w3 html strict 3.0//en//", "-/w3c/dtd html 4.0 transitional/en", "html"].contains(public_id) or system_id == "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd" or quirks_public_prefixes.any(|prefix| public_id.starts_with(prefix)) or (d.system_id == "" and ["-//w3c//dtd html 4.01 frameset//", "-//w3c//dtd html 4.01 transitional//"].any(|prefix| public_id.starts_with(prefix)))
}

quirks_public_prefixes = ["+//silmaril//dtd html pro v0r11 19970101//", "-//as//dtd html 3.0 aswedit + extensions//", "-//advasoft ltd//dtd html 3.0 aswedit + extensions//", "-//ietf//dtd html 2.0 level 1//", "-//ietf//dtd html 2.0 level 2//", "-//ietf//dtd html 2.0 strict level 1//", "-//ietf//dtd html 2.0 strict level 2//", "-//ietf//dtd html 2.0 strict//", "-//ietf//dtd html 2.0//", "-//ietf//dtd html 2.1e//", "-//ietf//dtd html 3.0//", "-//ietf//dtd html 3.2 final//", "-//ietf//dtd html 3.2//", "-//ietf//dtd html 3//", "-//ietf//dtd html level 0//", "-//ietf//dtd html level 1//", "-//ietf//dtd html level 2//", "-//ietf//dtd html level 3//", "-//ietf//dtd html strict level 0//", "-//ietf//dtd html strict level 1//", "-//ietf//dtd html strict level 2//", "-//ietf//dtd html strict level 3//", "-//ietf//dtd html strict//", "-//ietf//dtd html//", "-//metrius//dtd metrius presentational//", "-//microsoft//dtd internet explorer 2.0 html strict//", "-//microsoft//dtd internet explorer 2.0 html//", "-//microsoft//dtd internet explorer 2.0 tables//", "-//microsoft//dtd internet explorer 3.0 html strict//", "-//microsoft//dtd internet explorer 3.0 html//", "-//microsoft//dtd internet explorer 3.0 tables//", "-//netscape comm. corp.//dtd html//", "-//netscape comm. corp.//dtd strict html//", "-//o'reilly and associates//dtd html 2.0//", "-//o'reilly and associates//dtd html extended 1.0//", "-//o'reilly and associates//dtd html extended relaxed 1.0//", "-//sq//dtd html 2.0 hotmetal + extensions//", "-//softquad software//dtd hotmetal pro 6.0::19990601::extensions to html 4.0//", "-//softquad//dtd hotmetal pro 4.0::19971010::extensions to html 4.0//", "-//spyglass//dtd html 2.0 extended//", "-//sun microsystems corp.//dtd hotjava html//", "-//sun microsystems corp.//dtd hotjava strict html//", "-//w3c//dtd html 3 1995-03-24//", "-//w3c//dtd html 3.2 draft//", "-//w3c//dtd html 3.2 final//", "-//w3c//dtd html 3.2//", "-//w3c//dtd html 3.2s draft//", "-//w3c//dtd html 4.0 frameset//", "-//w3c//dtd html 4.0 transitional//", "-//w3c//dtd html experimental 19960712//", "-//w3c//dtd html experimental 970421//", "-//w3c//dtd w3 html//", "-//w3o//dtd w3 html 3.0//", "-//webtechs//dtd mozilla html 2.0//", "-//webtechs//dtd mozilla html//"]

expect {
	p = HtmlTree.parse("<table><template><tr><div>x</div></tr></template></table>", True, {}, { recovery: Browser })?
	table_node = p.tree.nodes.get(p.tree.roots.first()?)?
	template = p.tree.nodes.get(table_node.children.first()?)?
	template.children.len() == 2
}
expect {
	p = HtmlTree.parse("<body><template><tr></tr><tbody><tr></tr></template>", False, {}, { recovery: Browser })?
	names(p.tree).contains("template")
}

expect {
	p = HtmlTree.parse("<table><a>1<p>2</a>3</p>", True, {}, { recovery: Browser })?
	p.tree.roots.len() == 3 and Markup.text(p.tree, p.tree.roots.get(1)?) == "23"
}

expect {
	p = HtmlTree.parse("<div><span>x</span></div>", True, {}, {})?
	div = p.tree.nodes.get(p.tree.roots.first()?)?
	span = p.tree.nodes.get(div.children.first()?)?
	div.origin == Authored({ start: 0, end: 25 }) and span.origin == Authored({ start: 5, end: 19 })
}
expect {
	p = HtmlTree.parse("<p>a<div>b</div>", True, {}, {})?
	pnode = p.tree.nodes.get(p.tree.roots.first()?)?
	pnode.origin == Authored({ start: 0, end: 4 })
}
expect HtmlTree.parse("<p>\u(B)</p>", True, {}, {}).is_err()
expect HtmlTree.parse("<p>\u(FDD0)</p>", True, {}, {}).is_err()

skipped_lf_end : List(U8), Markup.Span -> U64
skipped_lf_end = |bytes, span| {
	first = bytes.get(span.start) ?? 0
	if first == '\r' {
		span.start + if (bytes.get(span.start + 1) == Ok('\n')) 2 else 1
	}
	else if first == '&' {
		var $end = span.start + 1
		while $end < span.end and bytes.get($end) != Ok(';') {
			$end = $end + 1
		}
		if ($end < span.end) $end + 1 else $end
	} else {
		span.start + 1
	}
}
expect {
	p = HtmlTree.parse("<textarea>\r\nAB</textarea>", True, {}, {})?
	root = p.tree.nodes.get(p.tree.roots.first()?)?
	text = p.tree.nodes.get(root.children.first()?)?
	text.origin == Authored({ start: 12, end: 14 })
}
expect {
	p = HtmlTree.parse("<textarea>&#10;AB</textarea>", True, {}, {})?
	root = p.tree.nodes.get(p.tree.roots.first()?)?
	text = p.tree.nodes.get(root.children.first()?)?
	text.origin == Authored({ start: 15, end: 17 })
}

expect HtmlTree.parse("<p>x</p>", True, {}, { max_diagnostics: 0 }).is_ok()

parsing_template = |s| last_named(s, "template").is_ok() or (s.fragment and s.context.name == "template" and s.context.namespace == Markup.html_namespace)

flush_table_text : State -> State
flush_table_text = |s| {
	if s.pending_text.is_empty() {
		return s
	}
	pending = s.pending_text
	nonwhite = pending.any(|part| !whitespace(Str.join_with(part.text.split_on("\u(0)"), "")))
	var $next = { ..s, pending_text: [] }
	if nonwhite {
		first = pending.first() ?? { text: "", span: { start: 0, end: 0 } }
		$next = problem($next, FosterParentedContent, first.span.start)
		for part in pending {
			$next = body({ ..$next, foster: True }, { kind: Characters(part.text), span: part.span })
		}
		{ ..$next, foster: False }
	} else {
		for part in pending {
			$next = insert_text($next, Str.join_with(part.text.split_on("\u(0)"), ""), part.span)
		}
		$next
	}
}
expect {
	p = HtmlTree.parse("<table> x </table>", True, {}, { recovery: Browser })?
	Markup.text(p.tree, p.tree.roots.first()?) == " x "
}
expect {
	p = HtmlTree.parse("<html a=b><template><col></col><html b=c><col></col></template>", False, {}, { recovery: Browser })?
	p.tree.roots.len() == 1 and names(p.tree).contains("body")
}
expect {
	p = HtmlTree.parse("<form><form>", True, { name: "template" }, { recovery: Browser })?
	names(p.tree) == ["html", "form", "form"]
}

source_token : Tok.Token, List(U8) -> Try(Markup.Token, [Empty])
source_token = |token, bytes| {
	if token.span.end == token.span.start {
		return Err(Empty)
	}
	kind = match token.kind {
		Start(_) | End(_) => Tag
		Characters(_) => if (Tok.starts(bytes, token.span.start, "<![CDATA[")) CData else Text
		Comment(_) => Comment
		Doctype(_) => Declaration
		ProcessingInstruction(_) => ProcessingInstruction
		Eof => Invalid
	}
	Ok({ kind, span: token.span })
}
expect {
	p = HtmlTree.parse("<script>if(a<b)x()</script><p>text</p>", True, {}, {})?
	p.source.tokens.map(|t| t.kind) == [Tag, Text, Tag, Tag, Text, Tag] and p.source.tokens.fold([], |out, t| out.concat(p.source.slice(t.span))) == p.source.bytes
}

expect HtmlTree.parse("<div>x</div>", True, {}, { max_depth: 1, max_elements: 1 }).is_ok()
expect HtmlTree.parse("", True, {}, { max_depth: 0, max_elements: 0 }).is_ok()
expect HtmlTree.parse("<div/>", True, {}, { recovery: Browser, max_depth: 0 }).is_err()
expect {
	p = HtmlTree.parse("<head></head><template>Foo</template>", False, {}, { recovery: Browser })?
	names(p.tree) == ["html", "head", "template", "body"]
}
expect {
	p = HtmlTree.parse("<div><svg><path><foreignObject><math></div>a", True, {}, { recovery: Browser })?
	p.tree.roots.len() == 1 and Markup.text(p.tree, p.tree.roots.first()?) == "a"
}
