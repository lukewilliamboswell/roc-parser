import AsciiDocDict
import AsciiDocSource
import AsciiDocAttributes
import unicode.GeneralCategory
import unicode.Scalar

## Inline substitutions for AsciiDoc HTML5 output. Attributes are supplied by
## the document processor, keeping this module independent of filesystem IO.
AsciiDocInline :: [].{
	render : Str, List({ name : Str, value : Str }) -> Str
	render = |source, attrs| render_with(source, attrs, { subs: "normal" })

	## Substitution groups: normal, verbatim, none, specialcharacters, quotes,
	## attributes, replacements, macros and post_replacements
	## +/- modifiers.
	render_with : Str, List({ name : Str, value : Str }), { subs : Str } -> Str
	render_with = |source, attrs, options| {
		render_state(source, attrs, options, initial_state).html
	}

	## Apply definition-time substitutions: ordinary values use header
	## specialcharacters and attributes; a whole-value pass macro chooses its
	## own substitution list. Values are bounded as in the reference engine.
	attribute_value : Str, List({ name : Str, value : Str }) -> Str
	attribute_value = |source, attrs| attribute_value_using(source, attrs, Err(Missing))

	## Definition-time source reads reuse the immutable persistent index.
	attribute_value_source : Str, AsciiDocSource.Store -> Str
	attribute_value_source = |source, store| {
		match simple_stored_reference(source, store) {
			Ok(value) => bounded_attribute(value)
			Err(_) => {
				key = AsciiDocSource.key(store)
				attribute_value_using(source, [], Ok({ first: AsciiDocSource.index(store), token: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(store), before: [], after: [] }))
			}
		}
	}

	State : { snapshot_before : List({ name : Str, value : Str }), snapshot_after : List({ name : Str, value : Str }), trusted_snapshot_generation : U64, trusted_snapshot_token : Str, dynamic_tags_possible : Bool, emitted_footnotes : List(U64), footnotes : List({ name : Str, text : Str }), note_index : Dict(Str, U64), indexed_note_count : U64, refs : List({ id : Str, text : Str }), ref_index : Dict(Str, Str), indexed_ref_count : U64, attributes : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), snapshot : List({ name : Str, value : Str }), snapshot_index : AsciiDocSource.Index, tag_prefix : Str }

	## Start a new document; reuse the returned state between its blocks.
	initial_state : State
	initial_state = { snapshot_before: [], snapshot_after: [], trusted_snapshot_generation: 0, trusted_snapshot_token: "", dynamic_tags_possible: True, emitted_footnotes: [], footnotes: [], note_index: Dict.empty(), indexed_note_count: 0, refs: [], ref_index: Dict.empty(), indexed_ref_count: 0, attributes: AsciiDocDict.empty(), removed: AsciiDocDict.empty(), snapshot: [], snapshot_index: AsciiDocSource.from_dict(Dict.empty()), tag_prefix: "\u(1)" }

	## Replace the reference registry and build its lookup index once.
	with_refs : State, List({ id : Str, text : Str }) -> State
	with_refs = |state, refs| {
		index = refs.fold(Dict.empty(), |found, ref| if (found.contains(ref.id)) found else found.insert(ref.id, ref.text))
		{ ..state, refs, ref_index: index, indexed_ref_count: refs.len() }
	}

	## Carry footnote numbering and references across blocks in document order.
	render_state : Str, List({ name : Str, value : Str }), { subs : Str }, State -> { html : Str, state : State }
	render_state = |input, attrs, options, state| render_dispatch(input, attrs, options, state, Err(Missing))

	## Internal document-renderer bridge. The caller supplies the complete
	## first-winner semantic index and an exact, stable snapshot token.
	render_indexed_state : Str, List({ name : Str, value : Str }), { subs : Str }, State, { first : Dict(Str, Str), token : Str, prefix : Str } -> { html : Str, state : State }
	render_indexed_state = |input, attrs, options, state, index| {
		{ html, state: next } = render_dispatch(input, attrs, options, state, Ok({ first: AsciiDocSource.from_dict(index.first), token: index.token, generation: 0, prefix: index.prefix, before: [], after: [] }))
		# Shared recursive stages always receive these same semantic bindings.
		# Legacy nested calls deliberately invalidate their own trusted token.
		{ html, state: { ..next, trusted_snapshot_generation: 0, trusted_snapshot_token: index.token } }
	}

	## Internal layered source entry. Base and local overlays stay immutable;
	## no combined binding list or dictionary is required for each block.
	render_layered_state : Str, List({ name : Str, value : Str }), { subs : Str }, State, { first : Dict(Str, Str), token : Str, prefix : Str, before : List({ name : Str, value : Str }), after : List({ name : Str, value : Str }) } -> { html : Str, state : State }
	render_layered_state = |input, attrs, options, state, index| {
		{ html, state: next } = render_dispatch(input, attrs, options, state, Ok({ first: AsciiDocSource.from_dict(index.first), token: index.token, generation: 0, prefix: index.prefix, before: index.before, after: index.after }))
		{ html, state: { ..next, trusted_snapshot_generation: 0, trusted_snapshot_token: index.token } }
	}

	## Parsed source readers carry the persistent index directly; source
	## identity includes both the exact input namespace and update generation.
	render_source_state : Str, List({ name : Str, value : Str }), { subs : Str }, State, { first : AsciiDocSource.Index, token : Str, generation : U64, prefix : Str, before : List({ name : Str, value : Str }), after : List({ name : Str, value : Str }) } -> { html : Str, state : State }
	render_source_state = |input, attrs, options, state, index| {
		{ html, state: next } = render_dispatch(input, attrs, options, state, Ok(index))
		{ html, state: { ..next, trusted_snapshot_token: index.token, trusted_snapshot_generation: index.generation } }
	}

	## Stateless normal rendering for table captions and other scoped readers.
	render_view : Str, AsciiDocAttributes.View -> Str
	render_view = |input, view| {
		if input == "" {
			return ""
		}
		if view.indexed {
			render_source_state(input, view.semantic, { subs: "normal" }, initial_state, { first: view.first, token: view.key, generation: view.generation, prefix: view.prefix, before: view.before, after: view.after }).html
		} else {
			render(input, view.semantic)
		}
	}
}

SourceHint : Try({ first : AsciiDocSource.Index, token : Str, generation : U64, prefix : Str, before : List({ name : Str, value : Str }), after : List({ name : Str, value : Str }) }, [Missing])

# Definition-time only: an exact ordinary reference to an inert stored leaf
# cannot escape, extract passes, mutate counters or introduce private slots.
# Unknown/builtin fallback and every unproved spelling keep the canonical path.
simple_stored_reference : Str, AsciiDocSource.Store -> Try(Str, [Missing])
simple_stored_reference = |source, store| {
	bytes = source.to_utf8()
	if bytes.len() < 3 or bytes.get(0) != Ok('{') or bytes.last() != Ok('}') {
		return Err(Missing)
	}
	name_bytes = bytes.sublist({ start: 1, len: bytes.len() - 2 })
	first = name_bytes.first() ?? 0
	if !((first >= 'a' and first <= 'z') or first == '_') {
		return Err(Missing)
	}
	if !name_bytes.all(|b| (b >= 'a' and b <= 'z') or (b >= '0' and b <= '9') or b == '_' or b == '-' or b == '.') {
		return Err(Missing)
	}
	name = Str.from_utf8_lossy(name_bytes)
	match AsciiDocSource.lookup(store, name) {
		Ok(value) => {
			# Bound speculative work; longer leaves use the canonical engine.
			if value.count_utf8_bytes() > 256 {
				Err(Missing)
			} else if value.to_utf8().all(inert_attribute_byte) {
				Ok(value)
			} else {
				Err(Missing)
			}
		}
		Err(_) => Err(Missing)
	}
}

# Deliberately narrow. Unicode literal bytes are unchanged by attribute-only
# substitution; ASCII controls, quotes/passes/macros/entities and escapes fall
# back. The full result still passes through the existing UTF8-aware cap.
inert_attribute_byte : U8 -> Bool
inert_attribute_byte = |b| b >= 128 or (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') or (b >= '0' and b <= '9') or b == ' ' or b == '_' or b == '-' or b == '.' or b == '/'

attribute_value_using : Str, List({ name : Str, value : Str }), SourceHint -> Str
attribute_value_using = |source, attrs, hint| {
	value = if source.starts_with("pass:") and source.ends_with("]") {
		match find(source, "[", 5) {
			Ok(open) => {
				subs = slice(source, 5, open - 5)
				body = slice(source, open + 1, source.to_utf8().len() - open - 2)
				if (subs == "") body else render_dispatch(body, attrs, { subs: subs }, AsciiDocInline.initial_state, hint).html
			}
			Err(_) => render_dispatch(escape(source), attrs, { subs: "attributes" }, AsciiDocInline.initial_state, hint).html
		}
	} else if !source.contains("{") {
		escape(source)
	} else {
		render_dispatch(escape(source), attrs, { subs: "attributes" }, AsciiDocInline.initial_state, hint).html
	}
	bounded_attribute(value)
}

hint_token : SourceHint -> Str
hint_token = |hint| match hint {
	Ok(index) => index.token
	Err(_) => ""
}

hint_generation : SourceHint -> U64
hint_generation = |hint| match hint {
	Ok(index) => index.generation
	Err(_) => 0
}

hint_prefix : SourceHint -> Str
hint_prefix = |hint| match hint {
	Ok(index) => index.prefix
	Err(_) => ""
}

hint_before : SourceHint -> List({ name : Str, value : Str })
hint_before = |hint| match hint {
	Ok(index) => index.before
	Err(_) => []
}

hint_after : SourceHint -> List({ name : Str, value : Str })
hint_after = |hint| match hint {
	Ok(index) => index.after
	Err(_) => []
}

# Only immutable source metadata enters this hint, never owned registries.
context_hint : RenderContext -> SourceHint
context_hint = |context| {
	if !AsciiDocSource.is_persistent(context.snapshot_index) and context.trusted_snapshot_token == "" and context.snapshot_before.is_empty() and context.snapshot_after.is_empty() {
		Err(Missing)
	} else {
		Ok({ first: context.snapshot_index, token: context.trusted_snapshot_token, generation: context.trusted_snapshot_generation, prefix: context.source_prefix, before: context.snapshot_before, after: context.snapshot_after })
	}
}

# FIRST source precedence is leading metadata, base bindings, then trailing
# metadata. Synthetic scanner bindings follow all three source layers.
source_value : List({ name : Str, value : Str }), AsciiDocSource.Index, List({ name : Str, value : Str }), List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
source_value = |source, index, before, after, name| {
	match direct(before, name) {
		Ok(value) => Ok(value)
		Err(_) => match (if (index.is_empty()) direct(source, name) else index.get(name).map_err(|_| Missing)) {
			Ok(value) => Ok(value)
			Err(_) => direct(after, name)
		}
	}
}

render_dispatch : Str, List({ name : Str, value : Str }), { subs : Str }, AsciiDocInline.State, SourceHint -> { html : Str, state : AsciiDocInline.State }
render_dispatch = |input, attrs, options, state, hint| {
	# The common pipeline retains its fused scanner. Explicit selections
	# use the engine's ordered substitution list, including modifiers.
	if options.subs == "normal" or options.subs == "n" {
		return render_state_indexed(input, attrs, options, state, False, hint)
	}
	# Header attributes follow special characters, so inserted attribute
	# values must not be escaped again. This pair needs one state pass.
	if options.subs == "specialcharacters,attributes" {
		return render_state_indexed(escape(input), attrs, { subs: "attributes" }, state, True, hint)
	}
	stages = substitution_stages(options.subs)
	if stages == normal_stages {
		return render_state_indexed(input, attrs, { subs: "normal" }, state, False, hint)
	}
	# A lone stage without pass extraction has no outer quote/tag work.
	# Dispatch here so no ordered-pipeline State frame surrounds mutation.
	if stages.len() == 1 and !(stages.contains("macros") and quote_needed(input, { ..empty, macros: True })) {
		return render_state_indexed(input, attrs, { subs: stages.first() ?? "none" }, state, True, hint)
	}
	render_ordered_indexed(input, attrs, stages, state, hint)
}

# Experimental ownership boundary: formatting/reference context cannot retain
# the dynamic dictionaries or note collections owned by the mutation phases.
RenderContext : { snapshot_before : List({ name : Str, value : Str }), snapshot_after : List({ name : Str, value : Str }), source_prefix : Str, trusted_snapshot_generation : U64, trusted_snapshot_token : Str, dynamic_tags_possible : Bool, emitted_footnotes : List(U64), refs : List({ id : Str, text : Str }), ref_index : Dict(Str, Str), indexed_ref_count : U64, snapshot : List({ name : Str, value : Str }), snapshot_index : AsciiDocSource.Index }

# Macro scanners borrow bindings without enumerating the document's counters.
# Appended synthetic bindings follow all existing effective source bindings.
AttributeView : { source_before : List({ name : Str, value : Str }), source_after : List({ name : Str, value : Str }), source_prefix : Str, dynamic_tags_possible : Bool, source : List({ name : Str, value : Str }), source_index : AsciiDocSource.Index, appended : List({ name : Str, value : Str }), values : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), prefix : Str }

static_view : List({ name : Str, value : Str }) -> AttributeView
static_view = |attrs| { source_before: [], source_after: [], source_prefix: "", dynamic_tags_possible: False, source: attrs, source_index: AsciiDocSource.from_dict(Dict.empty()), appended: [], values: AsciiDocDict.empty(), removed: AsciiDocDict.empty(), prefix: "\u(1)" }

append_view : AttributeView, List({ name : Str, value : Str }) -> AttributeView
append_view = |view, attrs| { ..view, appended: view.appended.concat(attrs) }

lookup_view : AttributeView, Str -> Try(Str, [Missing])
lookup_view = |view, key| {
	match view.values.get(key) {
		Ok(value) => Ok(value)
		Err(_) => {
			original = if (view.removed.contains(key)) Err(Missing) else source_value(view.source, view.source_index, view.source_before, view.source_after, key)
			match original {
				Ok(value) => Ok(value)
				Err(_) => match direct(view.appended, key) {
					Ok(value) => Ok(value)
					Err(_) => lookup([], key)
				}
			}
		}
	}
}

materialize_view : AttributeView -> List({ name : Str, value : Str })
materialize_view = |view| merged_attributes(view.source_before.concat(AsciiDocSource.semantic(view.source_index, view.source)).concat(view.source_after), view.values, view.removed).concat(view.appended)

# Immediate passthroughs have isolated writes, as render_with did previously.
# Sharing the read-only Dict is cheap; a child mutation deliberately copies it.
# Filter only bounded source bindings, not the growing dynamic registry.
render_with_view : Str, AttributeView, { subs : Str } -> Str
render_with_view = |input, view, options| {
	stages = substitution_stages(options.subs)
	if stages.len() == 1 {
		stage = stages.first() ?? "none"
		if stage == "quotes" or stage == "specialcharacters" or stage == "replacements" or stage == "post_replacements" {
			# These stages cannot mutate attributes, notes or references. Their
			# scanner already performs exactly one prepared substitution stage.
			return scan_view(input, view, groups(stage), 0)
		}
	}
	certificate = view_certificate(view)
	reader = AsciiDocSource.from_view({ first: view.source_index, source: view.source, before: view.source_before, after: view.source_after, values: view.values, removed: view.removed, appended: view.appended, prefix: certificate, dynamic_tags_possible: view.dynamic_tags_possible })
	render_dispatch(input, [], options, AsciiDocInline.initial_state, Ok({ first: reader, token: "", generation: 0, prefix: certificate, before: [], after: [] })).html
}

view_certificate : AttributeView -> Str
view_certificate = |view| {
	var $prefix = view.source_prefix
	if $prefix == "" {
		$prefix = AsciiDocSource.certificate(view.source_index)
		if $prefix == "" {
			$prefix = layered_delimiter("", view.source, view.source_before, view.source_after, "")
		} else {
			while view.source_before.any(|attr| attr.value.contains($prefix)) or view.source_after.any(|attr| attr.value.contains($prefix)) {
				$prefix = $prefix.concat("\u(1)")
			}
		}
	}
	known_dynamic = !view.dynamic_tags_possible and view.prefix != "" and view.prefix.to_utf8().all(|byte| byte == 1)
	if known_dynamic {
		if view.prefix.count_utf8_bytes() > $prefix.count_utf8_bytes() {
			$prefix = view.prefix
		}
	} else {
		for (_, value) in view.values.to_list() {
			while value.contains($prefix) {
				$prefix = $prefix.concat("\u(1)")
			}
		}
	}
	while view.appended.any(|attr| attr.value.contains($prefix)) {
		$prefix = $prefix.concat("\u(1)")
	}
	$prefix
}

assemble_state : RenderContext, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str, List({ name : Str, text : Str }), Dict(Str, U64), U64 -> AsciiDocInline.State
assemble_state = |context, attributes, removed, tag_prefix, footnotes, note_index, indexed_note_count| {
	{ snapshot_before: context.snapshot_before, snapshot_after: context.snapshot_after, trusted_snapshot_generation: context.trusted_snapshot_generation, trusted_snapshot_token: context.trusted_snapshot_token, dynamic_tags_possible: context.dynamic_tags_possible, emitted_footnotes: context.emitted_footnotes, refs: context.refs, ref_index: context.ref_index, indexed_ref_count: context.indexed_ref_count, snapshot: context.snapshot, snapshot_index: context.snapshot_index, attributes, removed, tag_prefix, footnotes, note_index, indexed_note_count }
}

render_state_fixed : Str, List({ name : Str, value : Str }), { subs : Str }, AsciiDocInline.State, Bool -> { html : Str, state : AsciiDocInline.State }
render_state_fixed = |input, attrs, options, state, prepared| render_state_indexed(input, attrs, options, state, prepared, Err(Missing))

render_state_indexed : Str, List({ name : Str, value : Str }), { subs : Str }, AsciiDocInline.State, Bool, SourceHint -> { html : Str, state : AsciiDocInline.State }
render_state_indexed = |input, attrs, options, state, prepared, hint| {
	selected = if (options.subs == "normal" or options.subs == "n") normal else groups(options.subs)
	if !needs_substitutions(input, selected) {
		# Plain input still updates source snapshots and caller-supplied refs.
		render_plain_indexed(input, attrs, state, hint)
	} else {
		render_fixed_owned(input, attrs, selected, prepared, state, hint)
	}
}

render_plain_indexed : Str, List({ name : Str, value : Str }), AsciiDocInline.State, SourceHint -> { html : Str, state : AsciiDocInline.State }
render_plain_indexed = |input, attrs, state, hint| {
	indexed = if (state.indexed_ref_count == state.refs.len()) state else AsciiDocInline.with_refs(state, state.refs)
	{ html: input, state: refresh_state_indexed(attrs, indexed, hint) }
}

# All fixed substitution selections use the same independently owned phases.
# Explicit stage order is supplied by render_ordered; prepared stages suppress
# early macro extraction exactly as in the canonical pipeline.
render_fixed_owned : Str, List({ name : Str, value : Str }), Groups, Bool, AsciiDocInline.State, SourceHint -> { html : Str, state : AsciiDocInline.State }
render_fixed_owned = |input, attrs, selected, prepared, state, hint| {
	{ snapshot_before: incoming_before, snapshot_after: incoming_after, trusted_snapshot_generation, trusted_snapshot_token, dynamic_tags_possible, attributes: incoming_attributes, removed: incoming_removed, tag_prefix: incoming_prefix, footnotes: incoming_notes, note_index: incoming_names, indexed_note_count: incoming_note_count, emitted_footnotes, refs, ref_index: incoming_refs, indexed_ref_count, snapshot: incoming_snapshot, snapshot_index: incoming_snapshot_index } = state
	{ attributes, removed, snapshot, snapshot_index: cached_index, snapshot_before, snapshot_after } = refresh_hint(attrs, incoming_attributes, incoming_removed, incoming_snapshot, incoming_snapshot_index, incoming_before, incoming_after, trusted_snapshot_token, trusted_snapshot_generation, hint)
	snapshot_index = if (cached_index.is_empty() and selected.macros and macro_candidate(input)) AsciiDocSource.from_dict(source_attribute_index(attrs)) else cached_index
	ref_index = if (indexed_ref_count == refs.len()) incoming_refs else refs.fold(Dict.empty(), |found, ref| if (found.contains(ref.id)) found else found.insert(ref.id, ref.text))
	context = { snapshot_before, snapshot_after, source_prefix: hint_prefix(hint), trusted_snapshot_generation: hint_generation(hint), trusted_snapshot_token: hint_token(hint), dynamic_tags_possible: dynamic_tags_possible and !attributes.is_empty(), emitted_footnotes, refs, ref_index, indexed_ref_count: refs.len(), snapshot, snapshot_index }
	g = { ..selected, ref_index }
	prior_notes = incoming_notes.len()
	quote_groups = if (prepared) { ..g, macros: False } else g
	before = if ((g.quotes or g.macros) and !g.attr_quotes and quote_needed(input, quote_groups)) quote_owned(input, attrs, snapshot_index, hint, dynamic_tags_possible, quote_groups, attributes, removed, incoming_prefix) else { source: input, prefix: "", tags: [], deferred: [] }
	{ source: mutated_source, context: mutated_context, attributes: mutated_attributes, removed: mutated_removed, tag_prefix, footnotes: mutated_notes, note_index: mutated_names, indexed_note_count: mutated_note_count, tags: mutated_tags, prefix: mutated_prefix } = mutate_owned_attributes(before.source, attrs, g, attributes, removed, incoming_prefix, incoming_notes, incoming_names, incoming_note_count, context, before.prefix, before.tags)
	if !g.attr_quotes and mutated_tags.is_empty() and before.deferred.is_empty() and !needs_substitutions(mutated_source, { ..g, quotes: False, attributes: False }) {
		return { html: mutated_source, state: assemble_state(mutated_context, mutated_attributes, mutated_removed, tag_prefix, mutated_notes, mutated_names, mutated_note_count) }
	}
	quotes = if (g.quotes and g.attr_quotes) quote_owned(mutated_source, attrs, mutated_context.snapshot_index, hint, mutated_context.dynamic_tags_possible, g, mutated_attributes, mutated_removed, tag_prefix) else { source: mutated_source, prefix: mutated_prefix, tags: mutated_tags, deferred: before.deferred }
	quoted = if (g.macros and quotes.prefix == "" and quotes.source.contains("footnote:")) { ..quotes, prefix: layered_delimiter(quotes.source, attrs, hint_before(hint), hint_after(hint), hint_prefix(hint)) } else quotes
	scan_groups = { ..g, quotes: False, attributes: False, passes_done: True }
	{ html, footnotes, note_index, indexed_note_count: note_count, tags, notes_start } = render_read_phase(quoted.source, attrs, mutated_context.snapshot_index, hint, mutated_context.dynamic_tags_possible, mutated_attributes, mutated_removed, tag_prefix, scan_groups, quoted, mutated_notes, mutated_names, mutated_note_count, prior_notes)
	finish_owned(html, attrs, mutated_context, mutated_attributes, mutated_removed, tag_prefix, footnotes, note_index, note_count, { ..quoted, tags }, notes_start)
}

# End the borrowed attribute view before deferred substitutions can mutate
# the document dictionaries. Note registries remain independently owned.
render_read_phase : Str, List({ name : Str, value : Str }), AsciiDocSource.Index, SourceHint, Bool, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str, Groups, QuoteResult, List({ name : Str, text : Str }), Dict(Str, U64), U64, U64 -> { html : Str, footnotes : List({ name : Str, text : Str }), note_index : Dict(Str, U64), indexed_note_count : U64, tags : List(Str), notes_start : U64 }
render_read_phase = |source, attrs, source_index, hint, dynamic_tags_possible, attributes, removed, tag_prefix, g, quoted, notes, names, indexed_note_count, prior_notes| {
	reference_attrs = { source_before: hint_before(hint), source_after: hint_after(hint), source_prefix: hint_prefix(hint), dynamic_tags_possible, source: attrs, source_index, appended: if (source.contains("mailto:")) tag_attributes(quoted.prefix, quoted.tags) else [], values: attributes, removed, prefix: tag_prefix }
	if !g.macros or !source.contains("footnote:") {
		return { html: scan_view_context(source, reference_attrs, g, 0, { prefix: quoted.prefix, tags: quoted.tags }), footnotes: notes, note_index: names, indexed_note_count, tags: quoted.tags, notes_start: prior_notes }
	}
	notes_start = notes.len()
	{ html, footnotes, note_index, indexed_note_count: note_count, tags } = render_owned_notes(source, reference_attrs, g, quoted, notes, names, indexed_note_count)
	{ html, footnotes, note_index, indexed_note_count: note_count, tags, notes_start }
}

RefreshResult : { attributes : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), snapshot : List({ name : Str, value : Str }), snapshot_index : AsciiDocSource.Index, snapshot_before : List({ name : Str, value : Str }), snapshot_after : List({ name : Str, value : Str }) }

# Base token equality and exact small overlay equality certify the effective
# snapshot without comparing or materializing its full binding list.
refresh_hint : List({ name : Str, value : Str }), AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), List({ name : Str, value : Str }), AsciiDocSource.Index, List({ name : Str, value : Str }), List({ name : Str, value : Str }), Str, U64, SourceHint -> RefreshResult
refresh_hint = |attrs, attributes, removed, snapshot, snapshot_index, before, after, trusted_snapshot_token, trusted_snapshot_generation, hint| {
	match hint {
		Err(_) => refresh_owned_attributes(attrs, attributes, removed, snapshot, snapshot_index, before, after)
		Ok(index) => {
			# Parsed identity is trusted only on the canonical processor lineage.
			# Ordinary Store updates can fork at the same namespace/generation.
			identity_safe = if (AsciiDocSource.is_persistent(index.first) or AsciiDocSource.is_persistent(snapshot_index)) AsciiDocSource.trusted_history(index.first) and AsciiDocSource.trusted_history(snapshot_index) else True
			base_same = identity_safe and index.token != "" and index.token == trusted_snapshot_token and index.generation == trusted_snapshot_generation
			if (base_same and index.before == before and index.after == after) or (attributes.is_empty() and removed.is_empty()) {
				return { attributes, removed, snapshot: attrs, snapshot_index: index.first, snapshot_before: index.before, snapshot_after: index.after }
			}
			previous = if (snapshot_index.is_empty() and !AsciiDocSource.is_persistent(snapshot_index)) AsciiDocSource.from_dict(source_attribute_index(snapshot)) else snapshot_index
			# Source deltas include raw names and revision aliases. Local overlays
			# must retain both too: a reserved revision name can itself be live.
			overlay_names = [before, after, index.before, index.after].fold([], |found, part| part.fold(found, |names, attr| if (attr.name.starts_with("__revision:")) names.append(attr.name).append(drop(attr.name, 11)) else names.append(attr.name)))
			names = if base_same {
				overlay_names
			} else match AsciiDocSource.changed_names(previous, index.first) {
				Ok(changed) => changed.concat(overlay_names)
				Err(_) => attributes.keys().concat(removed.keys())
			}
			var $attributes = attributes
			var $removed = removed
			for name in names {
				if $attributes.contains(name) or $removed.contains(name) {
					old = source_value(snapshot, previous, before, after, name)
					next = source_value(attrs, index.first, index.before, index.after, name)
					old_revision = source_value(snapshot, previous, before, after, "__revision:${name}")
					next_revision = source_value(attrs, index.first, index.before, index.after, "__revision:${name}")
					if next != old or next_revision != old_revision {
						$attributes = $attributes.remove(name)
						$removed = $removed.remove(name)
					}
				}
			}
			{ attributes: $attributes, removed: $removed, snapshot: attrs, snapshot_index: index.first, snapshot_before: index.before, snapshot_after: index.after }
		}
	}
}

# Legacy/manual calls have a full flat input list and empty new overlays.
# Prior layered metadata still participates in exact reset semantics.
refresh_owned_attributes : List({ name : Str, value : Str }), AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), List({ name : Str, value : Str }), AsciiDocSource.Index, List({ name : Str, value : Str }), List({ name : Str, value : Str }) -> RefreshResult
refresh_owned_attributes = |attrs, attributes, removed, snapshot, snapshot_index, before, after| {
	if !AsciiDocSource.is_persistent(snapshot_index) and before.is_empty() and after.is_empty() {
		if attributes.is_empty() and removed.is_empty() {
			return { attributes, removed, snapshot: attrs, snapshot_index: if (!snapshot_index.is_empty() and attrs == snapshot) snapshot_index else AsciiDocSource.from_dict(Dict.empty()), snapshot_before: [], snapshot_after: [] }
		}
		if attrs == snapshot {
			return { attributes, removed, snapshot, snapshot_index, snapshot_before: [], snapshot_after: [] }
		}
	} else if attributes.is_empty() and removed.is_empty() {
		return { attributes, removed, snapshot: attrs, snapshot_index: AsciiDocSource.from_dict(Dict.empty()), snapshot_before: [], snapshot_after: [] }
	}
	next = AsciiDocSource.from_dict(source_attribute_index(attrs))
	previous = if (snapshot_index.is_empty()) AsciiDocSource.from_dict(source_attribute_index(snapshot)) else snapshot_index
	var $attributes = attributes
	var $removed = removed
	if AsciiDocSource.is_persistent(previous) {
		for name in attributes.keys().concat(removed.keys()) {
			if next.get(name) != source_value(snapshot, previous, before, after, name) or next.get("__revision:${name}") != source_value(snapshot, previous, before, after, "__revision:${name}") {
				$attributes = $attributes.remove(name)
				$removed = $removed.remove(name)
			}
		}
		return { attributes: $attributes, removed: $removed, snapshot: attrs, snapshot_index: next, snapshot_before: [], snapshot_after: [] }
	}
	for source in [before, snapshot, after, attrs] {
		for attr in source {
			name = if (attr.name.starts_with("__revision:")) drop(attr.name, 11) else attr.name
			if next.get(name).map_err(|_| Missing) != source_value(snapshot, previous, before, after, name) or next.get("__revision:${name}").map_err(|_| Missing) != source_value(snapshot, previous, before, after, "__revision:${name}") {
				$attributes = $attributes.remove(name)
				$removed = $removed.remove(name)
			}
		}
	}
	{ attributes: $attributes, removed: $removed, snapshot: attrs, snapshot_index: next, snapshot_before: [], snapshot_after: [] }
}

render_owned_notes : Str, AttributeView, Groups, QuoteResult, List({ name : Str, text : Str }), Dict(Str, U64), U64 -> { html : Str, footnotes : List({ name : Str, text : Str }), note_index : Dict(Str, U64), indexed_note_count : U64, tags : List(Str) }
render_owned_notes = |source, reference_attrs, g, quoted, notes, names, indexed_note_count| {
	var $notes = notes
	var $names = if (indexed_note_count == notes.len()) names else {
		var $index = Dict.empty()
		var $number = 0.U64
		for note in notes {
			$number = $number + 1
			if note.name != "" and !$index.contains(note.name) {
				$index = $index.insert(note.name, $number)
			}
		}
		$index
	}
	var $render_tags = quoted.tags
	var $prepared = ""
	var $pos = 0
	bytes = source.to_utf8()
	marker = "footnote:".to_utf8()
	while $pos < bytes.len() {
		escaped = $pos > 0 and (bytes.get($pos - 1) ?? 0) == '\\'
		var $handled = False
		if !escaped and bytes.drop_first($pos).starts_with(marker) {
			var $open = $pos + marker.len()
			while $open < bytes.len() and (bytes.get($open) ?? 0) != '[' and !space(bytes.get($open) ?? 0) {
				$open = $open + 1
			}
			if (bytes.get($open) ?? 0) == '[' {
				match footnote_end_view(source, bytes, $open, reference_attrs, g) {
					Ok(end) => {
						name = slice(source, $pos + marker.len(), $open - $pos - marker.len())
						text = slice(source, $open + 1, end - $open - 1)
						existing = $names.get(name) ?? 0
						reference = text == "" or existing > 0
						number = if (reference) existing else $notes.len() + 1
						if number > 0 {
							if !reference {
								$notes = $notes.append({ name, text: scan_view(text, reference_attrs, g, 0) })
								if name != "" {
									$names = $names.insert(name, number)
								}
							}
							n = number.to_str()
							id = if (name == "") "" else " id=\"_footnote_${escape(name)}\""
							html = if (reference) "<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_${n}\" title=\"View footnote.\">${n}</a>]</sup>" else "<sup class=\"footnote\"${id}>[<a id=\"_footnoteref_${n}\" class=\"footnote\" href=\"#_footnotedef_${n}\" title=\"View footnote.\">${n}</a>]</sup>"
							$prepared = $prepared.concat(token(quoted.prefix, $render_tags.len()))
							$render_tags = $render_tags.append(html)
						} else {
							$prepared = $prepared.concat(token(quoted.prefix, $render_tags.len()))
							$render_tags = $render_tags.append("<sup class=\"footnoteref red\" title=\"Unresolved footnote reference.\">[${escape(name)}]</sup>")
						}
						$pos = end + 1
						$handled = True
					}
					Err(_) => {}
				}
			}
		}
		if !$handled {
			end = find_bytes(bytes, marker, $pos + 1) ?? bytes.len()
			$prepared = $prepared.concat(slice(source, $pos, end - $pos))
			$pos = end
		}
	}
	{ html: scan_view_context($prepared, reference_attrs, g, 0, { prefix: quoted.prefix, tags: $render_tags }), footnotes: $notes, note_index: $names, indexed_note_count: $notes.len(), tags: $render_tags }
}

# Deferred passthroughs use the established full-State path. The ordinary
# path restores new note text while the note list is still independently owned,
# then reconstructs State after every collection update has finished.
finish_owned : Str, List({ name : Str, value : Str }), RenderContext, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str, List({ name : Str, text : Str }), Dict(Str, U64), U64, QuoteResult, U64 -> { html : Str, state : AsciiDocInline.State }
finish_owned = |html, attrs, context, attributes, removed, tag_prefix, notes, names, indexed_note_count, quoted, notes_start| {
	if !quoted.deferred.is_empty() {
		return finish_render(html, attrs, assemble_state(context, attributes, removed, tag_prefix, notes, names, indexed_note_count), quoted, notes_start, context_hint(context))
	}
	var $notes = notes
	var $index = notes_start
	while $index < $notes.len() {
		match $notes.get($index) {
			Ok(note) => {
				if quoted.prefix != "" and note.text.contains(quoted.prefix) {
					$notes = $notes.set($index, { name: note.name, text: restore_tags(note.text, quoted.prefix, quoted.tags) }) ?? []
				}
			}
			Err(_) => {}
		}
		$index = $index + 1
	}
	{ html: restore_tags(html, quoted.prefix, quoted.tags), state: assemble_state(context, attributes, removed, tag_prefix, $notes, names, indexed_note_count) }
}

Groups : { special : Bool, quotes : Bool, attributes : Bool, replacements : Bool, macros : Bool, post : Bool, passes_done : Bool, attr_quotes : Bool, ref_index : Dict(Str, Str) }

normal : Groups
normal = { special: True, quotes: True, attributes: True, replacements: True, macros: True, post: True, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }

empty : Groups
empty = { special: False, quotes: False, attributes: False, replacements: False, macros: False, post: False, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }

groups : Str -> Groups
groups = |spec| {
	var $g = if (spec.starts_with("+") or spec.starts_with("-")) normal else empty
	for raw in spec.split_on(",") {
		name = raw.trim()
		enabled = !name.starts_with("-")
		key = if (name.starts_with("+") or name.starts_with("-")) drop(name, 1) else name
		$g = match key {
			"normal" => normal
			"n" => normal
			"none" => empty
			"v" => { ..empty, special: True }
			"verbatim" => { special: True, quotes: False, attributes: False, replacements: False, macros: False, post: False, passes_done: False, attr_quotes: False, ref_index: Dict.empty() }
			"specialcharacters" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"specialchars" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"q" => { special: $g.special, quotes: enabled, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"c" => { special: enabled, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"quotes" => { special: $g.special, quotes: enabled, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"a" => { ..$g, attributes: enabled }
			"r" => { ..$g, replacements: enabled }
			"m" => { ..$g, macros: enabled }
			"p" => { ..$g, post: enabled }
			"attributes" => { special: $g.special, quotes: $g.quotes, attributes: enabled, replacements: $g.replacements, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"replacements" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: enabled, macros: $g.macros, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"macros" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: enabled, post: $g.post, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			"post_replacements" => { special: $g.special, quotes: $g.quotes, attributes: $g.attributes, replacements: $g.replacements, macros: $g.macros, post: enabled, passes_done: $g.passes_done, attr_quotes: $g.attr_quotes, ref_index: $g.ref_index }
			_ => $g
		}
	}
	ordering = spec.split_on(",").map(|part| part.trim())
	var $seen_attributes = False
	var $attr_quotes = False
	for part in ordering {
		if part == "attributes" or part == "+attributes" or part == "a" {
			$seen_attributes = True
		}
		if $seen_attributes and (part == "quotes" or part == "+quotes" or part == "q") {
			$attr_quotes = True
		}
	}
	{ ..$g, attr_quotes: $attr_quotes }
}

drop : Str, U64 -> Str
drop = |s, n| if (n == 0) s else Str.from_utf8_lossy(s.to_utf8().drop_first(n))

take : Str, U64 -> Str
take = |s, n| Str.from_utf8_lossy(s.to_utf8().take_first(n))

slice : Str, U64, U64 -> Str
slice = |s, start, len| Str.from_utf8_lossy(s.to_utf8().sublist({ start, len }))

find : Str, Str, U64 -> Try(U64, [Missing])
find = |s, needle, start| find_bytes(s.to_utf8(), needle.to_utf8(), start)

space : U8 -> Bool
space = |c| c == ' ' or c == '\n' or c == '\t' or c == '\r'

word : U8 -> Bool
word = |c| (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9') or c == '_'

lookup : List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
lookup = |attrs, key| {
	for attr in attrs {
		if attr.name == key {
			return Ok(attr.value)
		}
	}
	match key {
		"blank" => Ok("")
		"sp" => Ok(" ")
		"nbsp" => Ok("&#160;")
		"zwsp" => Ok("&#8203;")
		"apos" => Ok("&#39;")
		"quot" => Ok("&#34;")
		"amp" => Ok("&")
		"lt" => Ok("<")
		"gt" => Ok(">")
		"plus" => Ok("&#43;")
		"vbar" => Ok("|")
		"brvbar" => Ok("&#166;")
		"cpp" => Ok("C&#43;&#43;")
		"empty" => Ok("")
		"backslash" => Ok("\\")
		"backtick" => Ok("`")
		"asterisk" => Ok("*")
		"two-colons" => Ok("::")
		"two-semicolons" => Ok(";;")
		"startsb" => Ok("[")
		"endsb" => Ok("]")
		"deg" => Ok("&#176;")
		"wj" => Ok("&#8288;")
		_ => Err(Missing)
	}
}

escape : Str -> Str
escape = |s| s.replace_each("&", "&amp;").replace_each("<", "&lt;").replace_each(">", "&gt;")

repl : Str, Str, Str -> Str
repl = |s, before, after| Str.join_with(s.split_on(before), after)

scan : Str, List({ name : Str, value : Str }), Groups, U64 -> Str
scan = |input, attrs, options, depth| scan_view(input, static_view(attrs), options, depth)

scan_view : Str, AttributeView, Groups, U64 -> Str
scan_view = |input, incoming_attrs, options, depth| scan_view_context(input, incoming_attrs, options, depth, { prefix: "", tags: [] })

scan_view_context : Str, AttributeView, Groups, U64, { prefix : Str, tags : List(Str) } -> Str
scan_view_context = |input, incoming_attrs, options, depth, protected| {
	if depth > 24 {
		return escape(input)
	}
	quoted = if (options.quotes) quote_view(input, incoming_attrs, options) else { source: input, prefix: "", tags: [], deferred: [] }
	source = quoted.source
	url_slots = if (quoted.prefix != "") { prefix: quoted.prefix, tags: quoted.tags } else protected
	attrs = append_view(incoming_attrs, if (source.contains("mailto:")) tag_attributes(quoted.prefix, quoted.tags) else [])
	g = { ..options, quotes: False, passes_done: options.passes_done or options.quotes }
	bytes = source.to_utf8()
	has_email = bytes.contains('@')
	has_colon = bytes.contains(':')
	var $i = 0
	var $remaining = source
	var $angle_close = Err(Missing)
	var $angle_close_known = False
	var $entity_angle_close = Err(Missing)
	var $entity_angle_close_known = False
	var $bracket_cursor = empty_bracket_cursor
	var $literal_cursors = empty_literal_cursors
	var $out = ""
	while $i < bytes.len() {
		previous_i = $i
		var $consumed = ""
		simple = fixed_replacement(bytes, $i, g)
		if simple.used > 0 {
			$out = $out.concat(simple.html)
			$i = $i + simple.used
		} else if !syntax_candidate(bytes, $i, g, has_email, has_colon) {
			var $end = $i + 1
			while $end < bytes.len() and !syntax_candidate(bytes, $end, g, has_email, has_colon) {
				$end = $end + 1
			}
			literal = slice(source, $i, $end - $i)
			$out = $out.concat(literal)
			$consumed = literal
			$i = $end
		} else {
			for _ in [0] {
				rest = $remaining
				c = bytes.get($i) ?? 0
				next = bytes.get($i + 1) ?? 0
				prev = if ($i == 0) 0 else bytes.get($i - 1) ?? 0
				if g.macros and g.special and rest.starts_with("\\&lt;") {
					$out = $out.concat("&amp;lt;")
					$i = $i + 5
					break
				}
				if g.macros and g.special and rest.starts_with("\\<") {
					inside = rest.drop_prefix("\\<")
					if url_scheme(inside) or (inside.starts_with("\\") and url_scheme(inside.drop_prefix("\\"))) {
						stale = match $angle_close {
							Ok(position) => position <= $i
							Err(_) => False
						}
						if !$angle_close_known or stale {
							$angle_close = find(source, ">", $i + 2)
							$angle_close_known = True
						}
						match $angle_close {
							Ok(position) => {
								$out = $out.concat(escape(slice(source, $i + 1, position - $i)))
								$i = position + 1
								break
							}
							Err(_) => {}
						}
					}
				}
				if c == '\\' and $i + 1 < bytes.len() and (g.quotes or g.macros or g.attributes) {
					escaped_rest = rest.drop_prefix("\\")
					escaped_result = parse_macro_context(escaped_rest, attrs, g, depth, $i + 1, $bracket_cursor, $literal_cursors, if (prev == '<') UrlAny else url_context(bytes, $i, url_slots))
					$bracket_cursor = escaped_result.cursor
					$literal_cursors = escaped_result.literals
					escaped_macro = escaped_result.macro
					if escaped_macro.used > 0 and !(escaped_rest.starts_with("link:") and url_scheme(escaped_rest.drop_prefix("link:"))) {
						$out = $out.concat(escape(slice(rest, 1, escaped_macro.used)))
						$i = $i + escaped_macro.used + 1
						break
					}
					if escaped_rest.starts_with("pass:") {
						match find(rest, "]", 1) {
							Ok(end) => {
								$out = $out.concat(escape(slice(rest, 1, end)))
								$i = $i + end + 1
								break
							}
							Err(_) => {}
						}
					}
				}
				if g.attributes and c == '{' {
					match find(source, "}", $i + 1) {
						Ok(end) => {
							key = slice(source, $i + 1, end - $i - 1).with_ascii_lowercased()
							match lookup_view(attrs, key) {
								Ok(value) => {
									$out = $out.concat(if (lookup([], key).is_ok()) value else scan_view(value, attrs, { special: False, quotes: g.attr_quotes, attributes: False, replacements: g.replacements, macros: g.macros, post: g.post, passes_done: g.passes_done, attr_quotes: g.attr_quotes, ref_index: g.ref_index }, depth + 1))
									$i = end + 1
									break
								}
								Err(_) => {}
							}
						}
						Err(_) => {}
					}
				}
				if g.macros and !word(prev) and prev != ':' and prev != '/' and prev != '>' and (word(c)) {
					email = email_link(rest)
					if email.used > 0 {
						$out = $out.concat(email.html)
						$i = $i + email.used
						break
					}
				}
				if !g.passes_done and g.macros and (rest.starts_with("+++") or rest.starts_with("++") or rest.starts_with("pass:")) {
					marker = if (rest.starts_with("+++")) "+++" else if (rest.starts_with("++")) "++" else "pass:"
					if marker == "pass:" {
						match find(source, "[", $i + 5) {
							Ok(open) => match bracket_end(source, open) {
								Ok(end) => {
									sub = slice(source, $i + 5, open - $i - 5)
									$out = $out.concat(scan_view(slice(source, open + 1, end - open - 1), attrs, groups(sub), depth + 1))
									$i = end + 1
									break
								}
								Err(_) => {}
							}
							Err(_) => {}
						}
					} else {
						n = marker.to_utf8().len()
						match find(source, marker, $i + n) {
							Ok(end) => {
								content = slice(source, $i + n, end - $i - n)
								$out = $out.concat(if (n == 3) content else escape(content))
								$i = end + n
								break
							}
							Err(_) => {}
						}
					}
				}
				if g.macros and rest.starts_with("&lt;") and url_scheme(rest.drop_prefix("&lt;")) {
					stale = match $entity_angle_close {
						Ok(position) => position <= $i
						Err(_) => False
					}
					if !$entity_angle_close_known or stale {
						$entity_angle_close = find(source, "&gt;", $i + 4)
						$entity_angle_close_known = True
					}
					match $entity_angle_close {
						Ok(position) => {
							$out = $out.concat(scan_view(slice(source, $i + 4, position - $i - 4), attrs, g, depth + 1))
							$i = position + 4
							break
						}
						Err(_) => {}
					}
				}
				if g.macros and c == '<' {
					angle_rest = rest.drop_prefix("<")
					if angle_rest.starts_with("https://") or angle_rest.starts_with("http://") {
						# The source is immutable and the cursor advances. A known
						# closing position is reusable until a scan skips past it;
						# a missing closer remains missing for every later suffix.
						stale = match $angle_close {
							Ok(position) => position <= $i
							Err(_) => False
						}
						if !$angle_close_known or stale {
							$angle_close = find(source, ">", $i + 1)
							$angle_close_known = True
						}
						match $angle_close {
							Ok(position) => {
								end = position - $i
								$out = $out.concat(scan_view(slice(rest, 1, end - 1), attrs, g, depth + 1))
								$i = $i + end + 1
								break
							}
							Err(_) => {}
						}
					}
				}
				if g.macros {
					macro_result = if (prev == '<' and (rest.starts_with("https://") or rest.starts_with("http://"))) { macro: { used: 0, html: "" }, cursor: $bracket_cursor, literals: $literal_cursors } else parse_macro_context(rest, attrs, g, depth, $i, $bracket_cursor, $literal_cursors, url_context(bytes, $i, url_slots))
					$bracket_cursor = macro_result.cursor
					$literal_cursors = macro_result.literals
					macro = macro_result.macro
					if macro.used > 0 {
						$out = $out.concat(macro.html)
						$i = $i + macro.used
						break
					}
				}
				if g.post and rest == " +" {
					$out = $out.concat("<br>")
					$i = $i + 2
					break
				}
				if g.post and rest.starts_with(" +\n") {
					$out = $out.concat("<br>\n")
					$i = $i + 3
					break
				}
				if g.replacements and rest.starts_with("&amp;") {
					match find(rest, ";", 5) {
						Ok(end) if end > 5 and end < 40 => {
							entity = slice(rest, 5, end - 5)
							if g.replacements and entity.to_utf8().all(|b| word(b) or b == '#') {
								$out = $out.concat("&${entity};")
								$i = $i + end + 1
								break
							}
						}
						_ => {}
					}
				}
				if g.replacements {
					var $used = 0
					for rule in [{ source: "(C)", target: "&#169;" }, { source: "(R)", target: "&#174;" }, { source: "(TM)", target: "&#8482;" }, { source: "...", target: "&#8230;&#8203;" }, { source: "->", target: "&#8594;" }, { source: "<-", target: "&#8592;" }, { source: "=>", target: "&#8658;" }, { source: "<=", target: "&#8656;" }] {
						if rest.starts_with(rule.source) {
							$out = $out.concat(rule.target)
							$used = rule.source.to_utf8().len()
							break
						}
					}
					if $used > 0 {
						$i = $i + $used
						break
					}
					if rest.starts_with(" -- ") or rest.starts_with(" --\n") {
						$out = $out.concat("&#8201;&#8212;&#8201;")
						$i = $i + 4
						break
					}
					if rest.starts_with("--") and word(prev) and word(bytes.get($i + 2) ?? 0) {
						$out = $out.concat("&#8212;&#8203;")
						$i = $i + 2
						break
					}
					if c == '\'' and word(prev) and word(next) {
						$out = $out.concat("&#8217;")
						$i = $i + 1
						break
					}
				}
				if g.special {
					if c == '<' {
						$out = $out.concat("&lt;")
						$i = $i + 1
						break
					}
					if c == '>' {
						$out = $out.concat("&gt;")
						$i = $i + 1
						break
					}
					if c == '&' {
						match find(source, ";", $i + 1) {
							Ok(end) if end - $i < 32 => {
								entity = slice(source, $i + 1, end - $i - 1)
								if g.replacements and entity.to_utf8().all(|b| word(b) or b == '#') {
									$out = $out.concat(slice(source, $i, end - $i + 1))
									$i = end + 1
									break
								}
							}
							_ => {}
						}
						$out = $out.concat("&amp;")
						$i = $i + 1
						break
					}
				}
				n = if (c < 128) 1 else if (c < 224) 2 else if (c < 240) 3 else 4
				var $end = $i + n
				while $end < bytes.len() {
					b = bytes.get($end) ?? 0
					before = bytes.get($end - 1) ?? 0
					if " \n=\\{[<&>+.'\"-(".to_utf8().contains(b) or (g.macros and word(b) and !word(before)) {
						break
					}
					$end = $end + 1
				}
				literal = slice(source, $i, $end - $i)
				$out = $out.concat(literal)
				$consumed = literal
				$i = $end
			}
		}
		if $i < bytes.len() {
			# Every branch advances across an original UTF-8 span. Keep the
			# suffix after inner-for breaks without validating future input.
			consumed = if ($consumed != "") $consumed else slice(source, previous_i, $i - previous_i)
			$remaining = $remaining.drop_prefix(consumed)
		}
	}
	restore_tags($out, quoted.prefix, quoted.tags)
}

Macro : { used : U64, html : Str }

# A macro target ends before the first label bracket or existing whitespace.
# Other control bytes retain their prior treatment by the macro parser.
macro_label_open : Str, U64 -> Try(U64, [Missing])
macro_label_open = |source, start| {
	bytes = source.to_utf8()
	var $pos = start
	while $pos < bytes.len() {
		byte = bytes.get($pos) ?? 0
		if byte == '[' {
			return Ok($pos)
		}
		if space(byte) {
			return Err(Missing)
		}
		$pos = $pos + 1
	}
	Err(Missing)
}

parse_macro_view : Str, AttributeView, Groups, U64 -> Macro
parse_macro_view = |source, attrs, g, depth| parse_macro_cached(source, attrs, g, depth, 0, empty_bracket_cursor).macro

BracketCursor : { known : Bool, searched_after : U64, closing : Try(U64, [Missing]) }

empty_bracket_cursor : BracketCursor
empty_bracket_cursor = { known: False, searched_after: 0, closing: Err(Missing) }

# Closing indexes are absolute within one scan's immutable source. Earlier
# queries fall back to the unchanged matcher; no monotonicity assumption leaks
# across escaped/normal macro probes or independent scan invocations.
cached_bracket_end : Str, U64, U64, BracketCursor -> { end : Try(U64, [Missing]), cursor : BracketCursor }
cached_bracket_end = |source, open, base, cursor| {
	absolute_open = base + open
	if cursor.known and absolute_open >= cursor.searched_after {
		match cursor.closing {
			Ok(position) if position > absolute_open => return { end: Ok(position - base), cursor }
			Err(_) => return { end: Err(Missing), cursor }
			_ => {}
		}
	}
	end = first_bracket_end(source, open)
	closing = match end {
		Ok(position) => Ok(base + position)
		Err(_) => Err(Missing)
	}
	{ end, cursor: { known: True, searched_after: absolute_open, closing } }
}

# Every cursor belongs to one exact needle and one immutable scanner source.
# Cache only positions: no source buffers or mutable document registries escape.
LiteralCursors : { index2 : BracketCursor, index3 : BracketCursor, anchor2 : BracketCursor, anchor3 : BracketCursor, xref : BracketCursor, entity_xref : BracketCursor, newline : BracketCursor }

empty_literal_cursors : LiteralCursors
empty_literal_cursors = { index2: empty_bracket_cursor, index3: empty_bracket_cursor, anchor2: empty_bracket_cursor, anchor3: empty_bracket_cursor, xref: empty_bracket_cursor, entity_xref: empty_bracket_cursor, newline: empty_bracket_cursor }

cached_literal_end : Str, Str, U64, U64, BracketCursor -> { end : Try(U64, [Missing]), cursor : BracketCursor }
cached_literal_end = |source, needle, start, base, cursor| {
	absolute_start = base + start
	if cursor.known and absolute_start >= cursor.searched_after {
		match cursor.closing {
			Ok(position) if position >= absolute_start => return { end: Ok(position - base), cursor }
			Err(_) => return { end: Err(Missing), cursor }
			_ => {}
		}
	}
	end = find(source, needle, start)
	closing = match end {
		Ok(position) => Ok(base + position)
		Err(_) => Err(Missing)
	}
	{ end, cursor: { known: True, searched_after: absolute_start, closing } }
}

parse_macro_cached : Str, AttributeView, Groups, U64, U64, BracketCursor -> { macro : Macro, cursor : BracketCursor }
parse_macro_cached = |source, attrs, g, depth, base, incoming_cursor| {
	result = parse_macro_context(source, attrs, g, depth, base, incoming_cursor, empty_literal_cursors, UrlAny)
	{ macro: result.macro, cursor: result.cursor }
}

# The standalone macro adapter starts a fresh source. Scanner calls supply the
# actual URL prefix context; other macro kinds retain their prior boundaries.
UrlContext : [UrlAny, UrlFormal, UrlNone]

url_scheme : Str -> Bool
url_scheme = |source| source.starts_with("https://") or source.starts_with("http://") or source.starts_with("ftp://") or source.starts_with("irc://")

url_context : List(U8), U64, { prefix : Str, tags : List(Str) } -> UrlContext
url_context = |bytes, pos, slots| {
	if pos == 0 {
		return UrlAny
	}
	prev = bytes.get(pos - 1) ?? 0
	if prev == '"' or prev == '\'' {
		return UrlFormal
	}
	if prev == ' ' or prev == '\t' or prev == '\n' or prev == '>' or prev == '(' or prev == ')' or prev == '[' or prev == ']' or prev == ';' {
		return UrlAny
	}
	if prev == 1 and slots.prefix != "" {
		prefix = slots.prefix.to_utf8()
		width = prefix.len()
		if pos >= width and bytes.sublist({ start: pos - width, len: width }) == prefix {
			end = pos - width
			var $start = end
			while $start > 0 and (bytes.get($start - 1) ?? 0) >= '0' and (bytes.get($start - 1) ?? 0) <= '9' {
				$start = $start - 1
			}
			if $start < end and $start >= width and bytes.sublist({ start: $start - width, len: width }) == prefix {
				var $index = 0.U64
				var $cursor = $start
				while $cursor < end {
					if $index > slots.tags.len() {
						return UrlNone
					}
					$index = $index * 10 + (bytes.get($cursor) ?? '0').to_u64() - 48
					$cursor = $cursor + 1
				}
				match slots.tags.get($index) {
					Ok(tag) if tag.ends_with(">") or tag.ends_with(";") => return UrlAny
					_ => {}
				}
			}
		}
	}
	if prev < 128 {
		return UrlNone
	}
	var $start = pos - 1
	while $start > 0 and pos - $start < 4 and (bytes.get($start) ?? 0) >= 128 and (bytes.get($start) ?? 0) < 192 {
		$start = $start - 1
	}
	first = bytes.get($start) ?? 0
	width = if (first < 224) 2 else if (first < 240) 3 else 4
	var $scalar = if (width == 2) first.to_u32() - 192 else if (width == 3) first.to_u32() - 224 else first.to_u32() - 240
	var $offset = 1
	while $offset < width {
		$scalar = $scalar * 64 + ((bytes.get($start + $offset) ?? 0).to_u32() % 64)
		$offset = $offset + 1
	}
	match unicode_category($scalar) {
		Ok(Zs) => UrlAny
		_ => UrlNone
	}
}

parse_macro_context : Str, AttributeView, Groups, U64, U64, BracketCursor, LiteralCursors, UrlContext -> { macro : Macro, cursor : BracketCursor, literals : LiteralCursors }
parse_macro_context = |source, attrs, g, depth, base, incoming_cursor, incoming_literals, url_prefix| {
	var $cursor = incoming_cursor
	var $literals = incoming_literals
	none = { used: 0, html: "" }
	if source.starts_with("(((") or source.starts_with("((") {
		n = if (source.starts_with("(((")) 3 else 2
		searched = cached_literal_end(source, if (n == 3) ")))" else "))", n, base, if (n == 3) $literals.index3 else $literals.index2)
		$literals = if (n == 3) { ..$literals, index3: searched.cursor } else { ..$literals, index2: searched.cursor }
		match searched.end {
			Ok(end) => {
				return { macro: { used: end + n, html: if (n == 3) "" else scan_view(slice(source, n, end - n), attrs, g, depth + 1) }, cursor: $cursor, literals: $literals }
			}
			Err(_) => {}
		}
	}
	if source.starts_with("[[[") {
		searched = cached_literal_end(source, "]]]", 3, base, $literals.anchor3)
		$literals = { ..$literals, anchor3: searched.cursor }
		line_end = cached_literal_end(source, "\n", 0, base, $literals.newline)
		$literals = { ..$literals, newline: line_end.cursor }
		match searched.end {
			Ok(end) if (line_end.end ?? source.count_utf8_bytes()) >= end => {
				id = slice(source, 3, end - 3).split_on(",").first() ?? ""
				return {
					macro: {
						used: end + 3,
						html: if (lookup_view(attrs, "style") ?? "") == "bibliography" or lookup_view(attrs, "bibliography").is_ok() {
							label = if (slice(source, 3, end - 3).contains(",")) Str.join_with(slice(source, 3, end - 3).split_on(",").drop_first(1), ",").trim() else id
							"<a id=\"${escape(id)}\"></a>[${label}]"
						} else "[<a id=\"${escape(id)}\"></a>]",
					},
					cursor: $cursor,
					literals: $literals,
				}
			}
			_ => {}
		}
	}
	xref_open = if (source.starts_with("&lt;&lt;")) 8 else if (g.special and source.starts_with("<<")) 2 else 0
	if xref_open > 0 {
		searched = cached_literal_end(source, if (xref_open == 8) "&gt;&gt;" else ">>", xref_open, base, if (xref_open == 8) $literals.entity_xref else $literals.xref)
		$literals = if (xref_open == 8) { ..$literals, entity_xref: searched.cursor } else { ..$literals, xref: searched.cursor }
		match searched.end {
			Ok(end) => {
				parts = slice(source, xref_open, end - xref_open).split_on(",")
				target = parts.first() ?? ""
				text = if (parts.len() > 1) Str.join_with(parts.drop_first(1), ",").trim() else reference_label(attrs, g, target) ?? "[${target}]"
				return { macro: { used: end + xref_open, html: "<a href=\"#${if (g.special) escape_entities(target) else target}\">${scan_view(text, attrs, if (parts.len() > 1) g else { ..normal, ref_index: g.ref_index }, depth + 1)}</a>" }, cursor: $cursor, literals: $literals }
			}
			_ => {}
		}
	}
	if source.starts_with("[[") {
		searched = cached_literal_end(source, "]]", 2, base, $literals.anchor2)
		$literals = { ..$literals, anchor2: searched.cursor }
		line_end = cached_literal_end(source, "\n", 0, base, $literals.newline)
		$literals = { ..$literals, newline: line_end.cursor }
		match searched.end {
			Ok(end) if (line_end.end ?? source.count_utf8_bytes()) >= end => {
				id = (slice(source, 2, end - 2).split_on(",").first() ?? "").trim()
				return { macro: { used: end + 2, html: "<a id=\"${escape(id)}\"></a>" }, cursor: $cursor, literals: $literals }
			}
			_ => {}
		}
	}
	var $prefix = ""
	for prefix in ["indexterm:", "indexterm2:", "https://", "http://", "ftp://", "irc://", "link:", "mailto:", "image:", "xref:", "anchor:", "footnote:", "footnoteref:", "kbd:", "btn:", "menu:", "icon:"] {
		if source.starts_with(prefix) {
			$prefix = prefix
			break
		}
	}
	if $prefix == "" {
		return { macro: none, cursor: $cursor, literals: $literals }
	}
	if !$prefix.ends_with("//") and (source.to_utf8().get($prefix.count_utf8_bytes()) ?? 0) == ':' {
		return { macro: none, cursor: $cursor, literals: $literals }
	}
	is_url = $prefix.ends_with("//")
	if is_url and url_prefix == UrlNone {
		return { macro: none, cursor: $cursor, literals: $literals }
	}
	if is_url and lookup_view(attrs, "__suppress-url-label").is_ok() {
		return { macro: none, cursor: $cursor, literals: $literals }
	}
	match macro_label_open(source, $prefix.count_utf8_bytes()) {
		Ok(open) => {
			searched = cached_bracket_end(source, open, base, $cursor)
			$cursor = searched.cursor
			match searched.end {
				Ok(end) => {
					target = if (is_url) take(source, open) else slice(source, $prefix.to_utf8().len(), open - $prefix.to_utf8().len())
					if is_url and target == $prefix {
						return { macro: none, cursor: $cursor, literals: $literals }
					}
					label = repl(slice(source, open + 1, end - open - 1), "\\]", "]")
					parts = if ((($prefix == "link:" or is_url or $prefix == "xref:") and !label.contains("=")) or ($prefix == "mailto:" and !label.contains(","))) [label] else macro_parts(label)
					positional = parts
					first_raw = (positional.first() ?? "").trim()
					first = if (named_item(first_raw) and ($prefix != "mailto:" or label.contains(","))) "" else if ($prefix == "image:" or label.contains("=") or ($prefix == "mailto:" and label.contains(","))) unquote_attribute(first_raw) else first_raw
					text = if (first == "") escape_entities(target) else scan_view(first, if (is_url) append_view(attrs, [{ name: "__suppress-url-label", value: "" }]) else attrs, g, depth + 1)
					html = match $prefix {
						"indexterm:" => ""
						"indexterm2:" => scan_view(label, attrs, g, depth + 1)
						"image:" => {
							default_alt = repl(repl((target.split_on("/").last() ?? target), "_", " "), "-", " ").split_on(".").first() ?? ""
							alt = named(parts, "alt") ?? if (first == "" or first.contains("=")) default_alt else first
							width_value = named(parts, "width") ?? if (positional.len() > 1 and !named_item(positional.get(1) ?? "")) unquote_attribute((positional.get(1) ?? "").trim()) else ""
							height_value = named(parts, "height") ?? if (positional.len() > 2 and !named_item(positional.get(2) ?? "")) unquote_attribute((positional.get(2) ?? "").trim()) else ""
							width = if (width_value == "") "" else " width=\"${escape_entities(width_value)}\""
							height = if (height_value == "") "" else " height=\"${escape_entities(height_value)}\""
							title = match named(parts, "title") {
								Ok(value) => " title=\"${html_attribute(value)}\""
								Err(_) => ""
							}
							image_dir = lookup_view(attrs, "imagesdir") ?? ""
							src = if (image_dir == "" or target.contains("://") or target.starts_with("/")) target else "${image_dir}/${target}"
							image = "<img src=\"${html_attribute(src)}\" alt=\"${html_attribute(alt)}\"${width}${height}${title}>"
							content = match named(parts, "link") {
								Ok(link) => "<a class=\"image\" href=\"${html_attribute(link)}\">${image}</a>"
								Err(_) => image
							}
							"<span class=\"image${
								match named(parts, "role") {
									Ok(role) => " ${html_attribute(role)}"
									Err(_) => ""
								}
							}\">${content}</span>"
						}
						"xref:" => {
							href = if (target.contains(".adoc")) repl(target, ".adoc", ".html") else if (target.contains("#") or path_has_extension(target)) target else "#${target}"
							display = if first == "" {
								if href.starts_with("#") {
									id = drop(href, 1)
									match reference_label(attrs, g, id) {
										Ok(ref_label) => scan_view(ref_label, attrs, { ..normal, ref_index: g.ref_index }, depth + 1)
										Err(_) => "[${id}]"
									}
								} else href.split_on("#").first() ?? href
							} else if (label.contains(",%")) scan_view(label, attrs, g, depth + 1) else text
							"<a href=\"${escape(href)}\"${
								match named(parts, "role") {
									Ok(role) => " class=\"${html_attribute(role)}\""
									Err(_) => ""
								}
							}>${display}</a>"
						}
						"anchor:" => "<a id=\"${escape_entities(target)}\"></a>"
						"mailto:" => {
							subject = match positional.get(1) {
								Ok(value) if !named_item(value) => Ok(value)
								_ => Err(Missing)
							}
							body = match positional.get(2) {
								Ok(value) if !named_item(value) => Ok(value)
								_ => Err(Missing)
							}
							query = match subject {
								Ok(value) => "?subject=${uri_component(restore_view_tags(scan_view(unquote_attribute(value.trim()), attrs, { ..empty, special: g.special, replacements: g.replacements }, depth + 1), attrs))}${
									match body {
										Ok(body_value) => "&amp;body=${uri_component(restore_view_tags(scan_view(unquote_attribute(body_value.trim()), attrs, { ..empty, special: g.special, replacements: g.replacements }, depth + 1), attrs))}"
										Err(_) => ""
									}
								}"
								Err(_) => ""
							}
							"<a href=\"mailto:${escape_entities(target)}${query}\"${link_attributes(if (label.contains(",")) parts else [], False, label.ends_with("^"))}>${text}</a>"
						}
						"kbd:" => {
							if lookup_view(attrs, "experimental").is_err() {
								return { macro: none, cursor: $cursor, literals: $literals }
							}
							keys = if (label.ends_with("++")) repl(take(label, label.to_utf8().len() - 2), "+", ",").split_on(",").append("+") else repl(label, "+", ",").split_on(",")
							if (keys.len() == 1) "<kbd>${escape(label)}</kbd>" else "<span class=\"keyseq\">${Str.join_with(keys.map(|key| "<kbd>${escape(key.trim())}</kbd>"), "+")}</span>"
						}
						"btn:" => {
							if lookup_view(attrs, "experimental").is_err() {
								return { macro: none, cursor: $cursor, literals: $literals }
							}
							"<b class=\"button\">${escape(label)}</b>"
						}
						"menu:" => {
							if lookup_view(attrs, "experimental").is_err() {
								return { macro: none, cursor: $cursor, literals: $literals }
							}
							items = label.split_on(">").map(|item| item.trim())
							arrow = "&#160;<b class=\"caret\">&#8250;</b> "
							if (label == "") "<b class=\"menuref\">${escape_entities(target)}</b>" else {
								middle = if (items.len() > 1) Str.join_with(items.take_first(items.len() - 1).map(|item| "<b class=\"submenu\">${escape(item)}</b>"), arrow).concat(arrow) else ""
								"<span class=\"menuseq\"><b class=\"menu\">${escape_entities(target)}</b>${arrow}${middle}<b class=\"menuitem\">${escape(items.last() ?? "")}</b></span>"
							}
						}
						"footnote:" => {
							if label == "" {
								"<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
							} else {
								id = if (target == "") "" else " id=\"_footnote_${escape_entities(target)}\""
								"<sup class=\"footnote\"${id}>[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
							}
						}
						"footnoteref:" => "<sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
						"icon:" => "<span class=\"icon\">[${escape_entities(target)}&#93;</span>"
						_ => {
							href = repl(escape_entities(target), " ", "%20")
							attr_text = link_attributes(parts, first == "", label.ends_with("^"))
							display = if (label.ends_with("^")) take(text, text.to_utf8().len() - 1) else if (label.contains(",%")) scan_view(label, attrs, g, depth + 1) else text
							"<a href=\"${href}\"${attr_text}>${display}</a>"
						}
					}
					return { macro: { used: end + 1, html }, cursor: $cursor, literals: $literals }
				}
				Err(_) => {}
			}
		}
		_ => {}
	}
	if is_url {
		if url_prefix == UrlFormal {
			return { macro: none, cursor: $cursor, literals: $literals }
		}
		var $end = 0
		for b in source.to_utf8() {
			if b < 32 or space(b) or b == '<' or b == '>' or b == '[' or b == ']' {
				break
			}
			$end = $end + 1
		}
		lexical_end = $end
		while $end > 0 and ".,;:!?)".to_utf8().contains(source.to_utf8().get($end - 1) ?? 0) {
			$end = $end - 1
		}
		target = take(source, $end)
		if target == $prefix {
			return { macro: none, cursor: $cursor, literals: $literals }
		}
		rendered_target = if g.special {
			escaped = escape_entities(take(source, lexical_end))
			take(escaped, escaped.count_utf8_bytes() - (lexical_end - $end))
		} else target
		return { macro: { used: $end, html: "<a href=\"${rendered_target}\" class=\"bare\">${rendered_target}</a>" }, cursor: $cursor, literals: $literals }
	}
	{ macro: none, cursor: $cursor, literals: $literals }
}

expect AsciiDocInline.render("*bold* _em_ `code` #mark# ^up^ ~down~", []) == "<strong>bold</strong> <em>em</em> <code>code</code> <mark>mark</mark> <sup>up</sup> <sub>down</sub>"
expect AsciiDocInline.render("a**b**c a*b*c", []) == "a<strong>b</strong>c a*b*c"
expect AsciiDocInline.render("*one _two_*", []) == "<strong>one <em>two</em></strong>"
expect AsciiDocInline.render("α < & &copy;", []) == "α &lt; &amp; &copy;"
expect AsciiDocInline.render("{name}", [{ name: "name", value: "*Roc*" }]) == "*Roc*"
expect AsciiDocInline.render("+++<i>x</i>+++ ++<b>++", []) == "<i>x</i> &lt;b&gt;"
expect AsciiDocInline.render("https://example.org[Roc]", []) == "<a href=\"https://example.org\">Roc</a>"
expect AsciiDocInline.render_with("*x* & <", [], { subs: "none" }) == "*x* & <"

# Oracle case 0
expect AsciiDocInline.render("A *bold* word", []) == "A <strong>bold</strong> word"

# Oracle case 1
expect AsciiDocInline.render("A _quiet_ word", []) == "A <em>quiet</em> word"

# Oracle case 2
expect AsciiDocInline.render("Type `code` now", []) == "Type <code>code</code> now"

# Oracle case 3
expect AsciiDocInline.render("pre**bold**post", []) == "pre<strong>bold</strong>post"

# Oracle case 4
expect AsciiDocInline.render("A *_nested_* word", []) == "A <strong><em>nested</em></strong> word"

# Oracle case 5
expect AsciiDocInline.render("x^2^", []) == "x<sup>2</sup>"

# Oracle case 6
expect AsciiDocInline.render("H~2~O", []) == "H<sub>2</sub>O"

# Oracle case 7
expect AsciiDocInline.render("A #marked# word", []) == "A <mark>marked</mark> word"

# Oracle case 8
expect AsciiDocInline.render("A \\*literal* word", []) == "A *literal* word"

# Oracle case 9
expect AsciiDocInline.render("A +<tag>+ word", []) == "A &lt;tag&gt; word"

# Oracle case 10
expect AsciiDocInline.render("pass:[<kbd>K</kbd>]", []) == "<kbd>K</kbd>"

# Oracle case 11
expect AsciiDocInline.render("First +\nSecond", []) == "First<br>\nSecond"

# Oracle case 12
expect AsciiDocInline.render("A & B < C", []) == "A &amp; B &lt; C"

# Oracle case 13
expect AsciiDocInline.render("https://example.org[Example]", []) == "<a href=\"https://example.org\">Example</a>"

# Oracle case 14
expect AsciiDocInline.render("link:guide.html[Guide]", []) == "<a href=\"guide.html\">Guide</a>"

# Oracle case 15
expect AsciiDocInline.render("mailto:a@example.org[Mail]", []) == "<a href=\"mailto:a@example.org\">Mail</a>"

# Oracle case 16
expect AsciiDocInline.render("A image:fox.png[Fox] here", []) == "A <span class=\"image\"><img src=\"fox.png\" alt=\"Fox\"></span> here"

# Oracle case 17
expect AsciiDocInline.render("xref:other.adoc[Other]", []) == "<a href=\"other.html\">Other</a>"

# Oracle case 18
expect AsciiDocInline.render("A footnote:[A note.]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Oracle case 19
expect AsciiDocInline.render("Copyright (C), trademark (TM), registered (R).", []) == "Copyright &#169;, trademark &#8482;, registered &#174;."

# Oracle case 20
expect AsciiDocInline.render("a*b*c a_b_c a`b`c", []) == "a*b*c a_b_c a`b`c"

# Oracle case 21
expect AsciiDocInline.render("**bold _italic_**", []) == "<strong>bold <em>italic</em></strong>"

# Oracle case 22
expect AsciiDocInline.render("x _a *b* c_", []) == "x <em>a <strong>b</strong> c</em>"

# Oracle case 23
expect AsciiDocInline.render("a &copy; &#34; &#x1f600; &bad; &", []) == "a &copy; &#34; &#x1f600; &bad; &amp;"

# Oracle case 24
expect AsciiDocInline.render("`a < b & c`", []) == "<code>a &lt; b &amp; c</code>"

# Oracle case 25
expect AsciiDocInline.render("a +\nb", []) == "a<br>\nb"

# Oracle case 26
expect AsciiDocInline.render("\\*literal*", []) == "*literal*"

# Oracle case 27
expect AsciiDocInline.render("\\{name}", []) == "{name}"

# Oracle case 28
expect AsciiDocInline.render("don't can't isn't", []) == "don&#8217;t can&#8217;t isn&#8217;t"

# Oracle case 29
expect AsciiDocInline.render("one -- two and a--b", []) == "one&#8201;&#8212;&#8201;two and a&#8212;&#8203;b"

# Oracle case 30
expect AsciiDocInline.render("\"`hello`\" and '`world`'", []) == "&#8220;hello&#8221; and &#8216;world&#8217;"

# Oracle case 31
expect AsciiDocInline.render("link:guide.html[Go]", []) == "<a href=\"guide.html\">Go</a>"

# Oracle case 32
expect AsciiDocInline.render("mailto:hi@example.org[Email]", []) == "<a href=\"mailto:hi@example.org\">Email</a>"

# Oracle case 33
expect AsciiDocInline.render("mailto:hi@example.org[]", []) == "<a href=\"mailto:hi@example.org\">hi@example.org</a>"

# Oracle case 34
expect AsciiDocInline.render("https://example.org", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Oracle case 35
expect AsciiDocInline.render("https://example.org[]", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Oracle case 36
expect AsciiDocInline.render("xref:guide.adoc[Guide]", []) == "<a href=\"guide.html\">Guide</a>"

# Oracle case 37
expect AsciiDocInline.render("<<unknown>>", []) == "<a href=\"#unknown\">[unknown]</a>"

# Oracle case 38
expect AsciiDocInline.render("[[here]]text", []) == "<a id=\"here\"></a>text"

# Oracle case 39
expect AsciiDocInline.render("image:pic.png[Alt,20,30]", []) == "<span class=\"image\"><img src=\"pic.png\" alt=\"Alt\" width=\"20\" height=\"30\"></span>"

# Oracle case 40
expect AsciiDocInline.render("pass:quotes[*bold*]", []) == "<strong>bold</strong>"

# Oracle case 41
expect AsciiDocInline.render("+<x>+", []) == "&lt;x&gt;"

# Oracle case 42
expect AsciiDocInline.render("footnote:[One note.]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Oracle case 43
expect AsciiDocInline.render("text footnote:a[One note.] again footnote:a[]", []) == "text <sup class=\"footnote\" id=\"_footnote_a\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> again <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

expect {
	first = AsciiDocInline.render_state("A footnote:first[One *note*.]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("Another footnote:[Two.] and footnote:first[]", [], { subs: "normal" }, first.state)
	first.state.footnotes == [{ name: "first", text: "One <strong>note</strong>." }] and second.state.footnotes.len() == 2 and second.html.contains("_footnotedef_2") and second.html.contains("_footnotedef_1")
}
expect AsciiDocInline.render_state("<<intro>>", [], { subs: "normal" }, { ..AsciiDocInline.initial_state, refs: [{ id: "intro", text: "Introduction" }] }).html == "<a href=\"#intro\">Introduction</a>"

# Expanded oracle case 0
expect AsciiDocInline.render("[.red]#red#", []) == "<span class=\"red\">red</span>"

# Expanded oracle case 1
expect AsciiDocInline.render("[.red]**red**", []) == "<strong class=\"red\">red</strong>"

# Expanded oracle case 2
expect AsciiDocInline.render("[#foo]#text#", []) == "<span id=\"foo\">text</span>"

# Expanded oracle case 3
expect AsciiDocInline.render("[.a.b]#two roles#", []) == "<span class=\"a b\">two roles</span>"

# Expanded oracle case 4
expect AsciiDocInline.render("[role=\"red\"]#text#", []) == "<span class=\"role=\"red\"\">text</span>"

# Expanded oracle case 5
expect AsciiDocInline.render("[.line-through]#strike#", []) == "<span class=\"line-through\">strike</span>"

# Expanded oracle case 6
expect AsciiDocInline.render("[.big]*bold*", []) == "<strong class=\"big\">bold</strong>"

# Expanded oracle case 7
expect AsciiDocInline.render("a^b c^", []) == "a^b c^"

# Expanded oracle case 8
expect AsciiDocInline.render("a~b c~", []) == "a~b c~"

# Expanded oracle case 9
expect AsciiDocInline.render("``double code``", []) == "<code>double code</code>"

# Expanded oracle case 10
expect AsciiDocInline.render("_a_ _b_", []) == "<em>a</em> <em>b</em>"

# Expanded oracle case 11
expect AsciiDocInline.render("**a** **b**", []) == "<strong>a</strong> <strong>b</strong>"

# Expanded oracle case 12
expect AsciiDocInline.render("foo__bar__baz", []) == "foo<em>bar</em>baz"

# Expanded oracle case 13
expect AsciiDocInline.render("foo##bar##baz", []) == "foo<mark>bar</mark>baz"

# Expanded oracle case 14
expect AsciiDocInline.render("\"`double`\" '`single`'", []) == "&#8220;double&#8221; &#8216;single&#8217;"

# Expanded oracle case 15
expect AsciiDocInline.render("\\https://example.org", []) == "https://example.org"

# Expanded oracle case 16
expect AsciiDocInline.render("\\link:a[Text]", []) == "link:a[Text]"

# Expanded oracle case 17
expect AsciiDocInline.render("\\image:a.png[]", []) == "image:a.png[]"

# Expanded oracle case 18
expect AsciiDocInline.render("\\pass:[<b>]", []) == "pass:[&lt;b&gt;]"

# Expanded oracle case 19
expect AsciiDocInline.render("https://example.org.", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>."

# Expanded oracle case 20
expect AsciiDocInline.render("(https://example.org)", []) == "(<a href=\"https://example.org\" class=\"bare\">https://example.org</a>)"

# Expanded oracle case 21
expect AsciiDocInline.render("https://example.org/a(b)", []) == "<a href=\"https://example.org/a(b\" class=\"bare\">https://example.org/a(b</a>)"

# Expanded oracle case 22
expect AsciiDocInline.render("https://example.org?q=a&b=c", []) == "<a href=\"https://example.org?q=a&amp;b=c\" class=\"bare\">https://example.org?q=a&amp;b=c</a>"

# Expanded oracle case 23
expect AsciiDocInline.render("<https://example.org>", []) == "<a href=\"https://example.org\" class=\"bare\">https://example.org</a>"

# Expanded oracle case 24
expect AsciiDocInline.render("https://example.org[Link^]", []) == "<a href=\"https://example.org\" target=\"_blank\" rel=\"noopener\">Link</a>"

# Expanded oracle case 25
expect AsciiDocInline.render("https://example.org[\"Link, with comma\"]", []) == "<a href=\"https://example.org\">\"Link, with comma\"</a>"

# Expanded oracle case 26
expect AsciiDocInline.render("link:some%20file.html[Some File]", []) == "<a href=\"some%20file.html\">Some File</a>"

# Expanded oracle case 27
expect AsciiDocInline.render("link:some file.html[Some]", []) == "link:some file.html[Some]"

# Expanded oracle case 28
expect AsciiDocInline.render("mailto:a@b.com[Email,subject=\"Hello\",body=\"Body text\"]", []) == "<a href=\"mailto:a@b.com\">Email</a>"

# Expanded oracle case 29
expect AsciiDocInline.render("a@b.com", []) == "<a href=\"mailto:a@b.com\">a@b.com</a>"

# Expanded oracle case 30
expect AsciiDocInline.render("xref:guide.adoc#anchor[Link]", []) == "<a href=\"guide.html#anchor\">Link</a>"

# Expanded oracle case 31
expect AsciiDocInline.render("xref:guide.adoc[]", []) == "<a href=\"guide.html\">guide.html</a>"

# Expanded oracle case 32
expect AsciiDocInline.render("<<id,Some *text*>>", []) == "<a href=\"#id\">Some <strong>text</strong></a>"

# Expanded oracle case 33
expect AsciiDocInline.render("[[id,Ref Text]]text", []) == "<a id=\"id\"></a>text"

# Expanded oracle case 34
expect AsciiDocInline.render("anchor:id[Ref Text]", []) == "<a id=\"id\"></a>"

# Expanded oracle case 35
expect AsciiDocInline.render("image:foo-bar_baz.svg[]", []) == "<span class=\"image\"><img src=\"foo-bar_baz.svg\" alt=\"foo bar baz\"></span>"

# Expanded oracle case 36
expect AsciiDocInline.render("image:cat.jpg[\"A cat, sleeping\",120,80]", []) == "<span class=\"image\"><img src=\"cat.jpg\" alt=\"A cat, sleeping\" width=\"120\" height=\"80\"></span>"

# Expanded oracle case 37
expect AsciiDocInline.render("image:foo.png[alt=Foo,width=100]", []) == "<span class=\"image\"><img src=\"foo.png\" alt=\"Foo\" width=\"100\"></span>"

# Expanded oracle case 38
expect AsciiDocInline.render("image:foo.png[Foo,link=https://example.org]", []) == "<span class=\"image\"><a class=\"image\" href=\"https://example.org\"><img src=\"foo.png\" alt=\"Foo\"></a></span>"

# Expanded oracle case 39
expect AsciiDocInline.render("image:foo.png[Foo,title=Hi]", []) == "<span class=\"image\"><img src=\"foo.png\" alt=\"Foo\" title=\"Hi\"></span>"

# Expanded oracle case 40
expect AsciiDocInline.render("icon:heart[]", []) == "<span class=\"icon\">[heart&#93;</span>"

# Expanded oracle case 41
expect AsciiDocInline.render("footnote:a[Note.] footnote:a[]", []) == "<sup class=\"footnote\" id=\"_footnote_a\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Expanded oracle case 42
expect AsciiDocInline.render("footnote:[A.] footnote:[B.]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_2\" class=\"footnote\" href=\"#_footnotedef_2\" title=\"View footnote.\">2</a>]</sup>"

# Expanded oracle case 43
expect AsciiDocInline.render("pass:attributes[<b>{foo}</b>]", []) == "<b>{foo}</b>"

# Expanded oracle case 44
expect AsciiDocInline.render("pass:q[*bold*]", []) == "<strong>bold</strong>"

# Expanded oracle case 45
expect AsciiDocInline.render("pass:c[<b>]", []) == "&lt;b&gt;"

# Expanded oracle case 46
expect AsciiDocInline.render("++a & b++", []) == "a &amp; b"

# Expanded oracle case 47
expect AsciiDocInline.render("+++a & b+++", []) == "a & b"

# Expanded oracle case 48
expect AsciiDocInline.render("+a *b*+", []) == "a *b*"

# Expanded oracle case 49
expect AsciiDocInline.render("a +\nb +\nc", []) == "a<br>\nb<br>\nc"

# Expanded oracle case 50
expect AsciiDocInline.render("a +", []) == "a<br>"

# Expanded oracle case 51
expect AsciiDocInline.render("(c) (r) (tm)", []) == "(c) (r) (tm)"

# Expanded oracle case 52
expect AsciiDocInline.render("(C) (R) (TM)", []) == "&#169; &#174; &#8482;"

# Expanded oracle case 53
expect AsciiDocInline.render("a -> b <- c => d <= e", []) == "a &#8594; b &#8592; c &#8658; d &#8656; e"

# Expanded oracle case 55
expect AsciiDocInline.render("a -- b", []) == "a&#8201;&#8212;&#8201;b"

# Expanded oracle case 56
expect AsciiDocInline.render("--begin", []) == "--begin"

# Expanded oracle case 57
expect AsciiDocInline.render("begin--", []) == "begin--"

# Expanded oracle case 58
expect AsciiDocInline.render("a--b", []) == "a&#8212;&#8203;b"

# Expanded oracle case 59
expect AsciiDocInline.render("a --\nb", []) == "a&#8201;&#8212;&#8201;b"

# Expanded oracle case 60
expect AsciiDocInline.render("`+x+`", []) == "<code>x</code>"

# Expanded oracle case 61
expect AsciiDocInline.render("`a *b*`", []) == "<code>a <strong>b</strong></code>"

# Expanded oracle case 62
expect AsciiDocInline.render("`a _b_`", []) == "<code>a <em>b</em></code>"

# Expanded oracle case 63
expect AsciiDocInline.render("*a\nb*", []) == "<strong>a\nb</strong>"

# Expanded oracle case 64
expect AsciiDocInline.render("foo\\_bar_", []) == "foo_bar_"

# Expanded oracle case 65
expect AsciiDocInline.render("*a*word", []) == "*a*word"

# Expanded oracle case 66
expect AsciiDocInline.render("*a*!", []) == "<strong>a</strong>!"

# Expanded oracle case 67
expect AsciiDocInline.render("a*one*", []) == "a*one*"

# Expanded oracle case 68
expect AsciiDocInline.render("[.red]_red_", []) == "<em class=\"red\">red</em>"

# Expanded oracle case 69
expect AsciiDocInline.render("kbd:[Ctrl+C]", []) == "kbd:[Ctrl+C]"

# Expanded oracle case 70
expect AsciiDocInline.render("btn:[OK]", []) == "btn:[OK]"

# Expanded oracle case 71
expect AsciiDocInline.render("menu:File[Open]", []) == "menu:File[Open]"

# Expanded oracle case 72
expect AsciiDocInline.render("{nbsp} {lt} {amp} {plus} {vbar} {cpp}", []) == "&#160; < & &#43; | C&#43;&#43;"

macro_parts : Str -> List(Str)
macro_parts = |incoming_source| {
	source = repl(repl(incoming_source, "\r\n", " "), "\n", " ")
	bytes = source.to_utf8()
	var $parts = []
	var $quote = 0.U8
	var $start = 0
	var $pos = 0
	var $closed = False
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		previous = if ($pos == 0) 0 else bytes.get($pos - 1) ?? 0
		if $closed and (b == ',' or space(b)) {
			$start = $pos + 1
			if b == ',' {
				$closed = False
			}
		} else if $quote != 0 and b == $quote and previous != '\\' {
			$quote = 0
			$parts = $parts.append(slice(source, $start, $pos - $start + 1))
			$start = $pos + 1
			$closed = True
		} else {
			$closed = False
			if $quote == 0 and (b == '\'' or b == '"') and ($pos == $start or previous == '=') {
				$quote = b
			}
			if b == ',' and $quote == 0 {
				$parts = $parts.append(slice(source, $start, $pos - $start))
				$start = $pos + 1
			}
		}
		$pos = $pos + 1
	}
	if $start < bytes.len() or !$closed {
		$parts = $parts.append(drop(source, $start))
	}
	$parts
}

named : List(Str), Str -> Try(Str, [Missing])
named = |parts, name| {
	for part in parts {
		p = part.trim()
		if p.starts_with("${name}=") {
			return Ok(unquote_attribute(drop(p, name.to_utf8().len() + 1).trim()))
		}
	}
	Err(Missing)
}

email_link : Str -> Macro
email_link = |source| {
	var $end = 0
	for b in source.to_utf8() {
		if !(word(b) or b == '@' or b == '.' or b == '-' or b == '+') {
			break
		}
		$end = $end + 1
	}
	while $end > 0 and (source.to_utf8().get($end - 1) ?? 0) == '.' {
		$end = $end - 1
	}
	email = take(source, $end)
	parts = email.split_on("@")
	if parts.len() == 2 and (parts.get(1) ?? "").contains(".") and (parts.first() ?? "") != "" {
		{ used: $end, html: "<a href=\"mailto:${escape(email)}\">${escape(email)}</a>" }
	} else {
		{ used: 0, html: "" }
	}
}

expect AsciiDocInline.render("link:outside.adoc[role=include]", []) == "<a href=\"outside.adoc\" class=\"bare include\">outside.adoc</a>"
expect AsciiDocInline.render("{x}", [{ name: "x", value: "*Roc* &lt;b&gt; &amp; &amp;copy; &amp;madeup;" }]) == "*Roc* &lt;b&gt; &amp; &copy; &madeup;"

# Inline interaction oracle case 0
expect AsciiDocInline.render("Test *a* end.", []) == "Test <strong>a</strong> end."

# Inline interaction oracle case 1
expect AsciiDocInline.render("Test *a b* end.", []) == "Test <strong>a b</strong> end."

# Inline interaction oracle case 2
expect AsciiDocInline.render("Test *a\nb* end.", []) == "Test <strong>a\nb</strong> end."

# Inline interaction oracle case 3
expect AsciiDocInline.render("Test *a *bold* z* end.", []) == "Test <strong>a *bold</strong> z* end."

# Inline interaction oracle case 4
expect AsciiDocInline.render("Test *<x> & y* end.", []) == "Test <strong>&lt;x&gt; &amp; y</strong> end."

# Inline interaction oracle case 5
expect AsciiDocInline.render("Test **a** end.", []) == "Test <strong>a</strong> end."

# Inline interaction oracle case 6
expect AsciiDocInline.render("Test **a b** end.", []) == "Test <strong>a b</strong> end."

# Inline interaction oracle case 7
expect AsciiDocInline.render("Test **a\nb** end.", []) == "Test <strong>a\nb</strong> end."

# Inline interaction oracle case 8
expect AsciiDocInline.render("Test **a *bold* z** end.", []) == "Test <strong>a <strong>bold</strong> z</strong> end."

# Inline interaction oracle case 9
expect AsciiDocInline.render("Test **<x> & y** end.", []) == "Test <strong>&lt;x&gt; &amp; y</strong> end."

# Inline interaction oracle case 10
expect AsciiDocInline.render("Test _a_ end.", []) == "Test <em>a</em> end."

# Inline interaction oracle case 11
expect AsciiDocInline.render("Test _a b_ end.", []) == "Test <em>a b</em> end."

# Inline interaction oracle case 12
expect AsciiDocInline.render("Test _a\nb_ end.", []) == "Test <em>a\nb</em> end."

# Inline interaction oracle case 13
expect AsciiDocInline.render("Test _a *bold* z_ end.", []) == "Test <em>a <strong>bold</strong> z</em> end."

# Inline interaction oracle case 14
expect AsciiDocInline.render("Test _<x> & y_ end.", []) == "Test <em>&lt;x&gt; &amp; y</em> end."

# Inline interaction oracle case 15
expect AsciiDocInline.render("Test __a__ end.", []) == "Test <em>a</em> end."

# Inline interaction oracle case 16
expect AsciiDocInline.render("Test __a b__ end.", []) == "Test <em>a b</em> end."

# Inline interaction oracle case 17
expect AsciiDocInline.render("Test __a\nb__ end.", []) == "Test <em>a\nb</em> end."

# Inline interaction oracle case 18
expect AsciiDocInline.render("Test __a *bold* z__ end.", []) == "Test <em>a <strong>bold</strong> z</em> end."

# Inline interaction oracle case 19
expect AsciiDocInline.render("Test __<x> & y__ end.", []) == "Test <em>&lt;x&gt; &amp; y</em> end."

# Inline interaction oracle case 20
expect AsciiDocInline.render("Test `a` end.", []) == "Test <code>a</code> end."

# Inline interaction oracle case 21
expect AsciiDocInline.render("Test `a b` end.", []) == "Test <code>a b</code> end."

# Inline interaction oracle case 22
expect AsciiDocInline.render("Test `a\nb` end.", []) == "Test <code>a\nb</code> end."

# Inline interaction oracle case 23
expect AsciiDocInline.render("Test `a *bold* z` end.", []) == "Test <code>a <strong>bold</strong> z</code> end."

# Inline interaction oracle case 24
expect AsciiDocInline.render("Test `<x> & y` end.", []) == "Test <code>&lt;x&gt; &amp; y</code> end."

# Inline interaction oracle case 25
expect AsciiDocInline.render("Test ``a`` end.", []) == "Test <code>a</code> end."

# Inline interaction oracle case 26
expect AsciiDocInline.render("Test ``a b`` end.", []) == "Test <code>a b</code> end."

# Inline interaction oracle case 27
expect AsciiDocInline.render("Test ``a\nb`` end.", []) == "Test <code>a\nb</code> end."

# Inline interaction oracle case 28
expect AsciiDocInline.render("Test ``a *bold* z`` end.", []) == "Test <code>a <strong>bold</strong> z</code> end."

# Inline interaction oracle case 29
expect AsciiDocInline.render("Test ``<x> & y`` end.", []) == "Test <code>&lt;x&gt; &amp; y</code> end."

# Inline interaction oracle case 30
expect AsciiDocInline.render("Test #a# end.", []) == "Test <mark>a</mark> end."

# Inline interaction oracle case 31
expect AsciiDocInline.render("Test #a b# end.", []) == "Test <mark>a b</mark> end."

# Inline interaction oracle case 32
expect AsciiDocInline.render("Test #a\nb# end.", []) == "Test <mark>a\nb</mark> end."

# Inline interaction oracle case 33
expect AsciiDocInline.render("Test #a *bold* z# end.", []) == "Test <mark>a <strong>bold</strong> z</mark> end."

# Inline interaction oracle case 34
expect AsciiDocInline.render("Test #<x> & y# end.", []) == "Test <mark>&lt;x&gt; &amp; y</mark> end."

# Inline interaction oracle case 35
expect AsciiDocInline.render("Test ##a## end.", []) == "Test <mark>a</mark> end."

# Inline interaction oracle case 36
expect AsciiDocInline.render("Test ##a b## end.", []) == "Test <mark>a b</mark> end."

# Inline interaction oracle case 37
expect AsciiDocInline.render("Test ##a\nb## end.", []) == "Test <mark>a\nb</mark> end."

# Inline interaction oracle case 38
expect AsciiDocInline.render("Test ##a *bold* z## end.", []) == "Test <mark>a <strong>bold</strong> z</mark> end."

# Inline interaction oracle case 39
expect AsciiDocInline.render("Test ##<x> & y## end.", []) == "Test <mark>&lt;x&gt; &amp; y</mark> end."

# Inline interaction oracle case 40
expect AsciiDocInline.render("Test ^a^ end.", []) == "Test <sup>a</sup> end."

# Inline interaction oracle case 41
expect AsciiDocInline.render("Test ^a b^ end.", []) == "Test ^a b^ end."

# Inline interaction oracle case 42
expect AsciiDocInline.render("Test ^a\nb^ end.", []) == "Test ^a\nb^ end."

# Inline interaction oracle case 43
expect AsciiDocInline.render("Test ^a *bold* z^ end.", []) == "Test ^a <strong>bold</strong> z^ end."

# Inline interaction oracle case 44
expect AsciiDocInline.render("Test ^<x> & y^ end.", []) == "Test ^&lt;x&gt; &amp; y^ end."

# Inline interaction oracle case 45
expect AsciiDocInline.render("Test ~a~ end.", []) == "Test <sub>a</sub> end."

# Inline interaction oracle case 46
expect AsciiDocInline.render("Test ~a b~ end.", []) == "Test ~a b~ end."

# Inline interaction oracle case 47
expect AsciiDocInline.render("Test ~a\nb~ end.", []) == "Test ~a\nb~ end."

# Inline interaction oracle case 48
expect AsciiDocInline.render("Test ~a *bold* z~ end.", []) == "Test ~a <strong>bold</strong> z~ end."

# Inline interaction oracle case 49
expect AsciiDocInline.render("Test ~<x> & y~ end.", []) == "Test ~&lt;x&gt; &amp; y~ end."

# Inline interaction oracle case 50
expect AsciiDocInline.render("Test +a+ end.", []) == "Test a end."

# Inline interaction oracle case 51
expect AsciiDocInline.render("Test +a b+ end.", []) == "Test a b end."

# Inline interaction oracle case 52
expect AsciiDocInline.render("Test +a\nb+ end.", []) == "Test a\nb end."

# Inline interaction oracle case 53
expect AsciiDocInline.render("Test +a *bold* z+ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 54
expect AsciiDocInline.render("Test +<x> & y+ end.", []) == "Test &lt;x&gt; &amp; y end."

# Inline interaction oracle case 55
expect AsciiDocInline.render("Test ++a++ end.", []) == "Test a end."

# Inline interaction oracle case 56
expect AsciiDocInline.render("Test ++a b++ end.", []) == "Test a b end."

# Inline interaction oracle case 57
expect AsciiDocInline.render("Test ++a\nb++ end.", []) == "Test a\nb end."

# Inline interaction oracle case 58
expect AsciiDocInline.render("Test ++a *bold* z++ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 59
expect AsciiDocInline.render("Test ++<x> & y++ end.", []) == "Test &lt;x&gt; &amp; y end."

# Inline interaction oracle case 60
expect AsciiDocInline.render("Test +++a+++ end.", []) == "Test a end."

# Inline interaction oracle case 61
expect AsciiDocInline.render("Test +++a b+++ end.", []) == "Test a b end."

# Inline interaction oracle case 62
expect AsciiDocInline.render("Test +++a\nb+++ end.", []) == "Test a\nb end."

# Inline interaction oracle case 63
expect AsciiDocInline.render("Test +++a *bold* z+++ end.", []) == "Test a *bold* z end."

# Inline interaction oracle case 64
expect AsciiDocInline.render("Test +++<x> & y+++ end.", []) == "Test <x> & y end."

# Inline interaction oracle case 65
expect AsciiDocInline.render("A footnote:[A *bold* note and https://example.org[link].]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Inline interaction oracle case 66
expect AsciiDocInline.render("footnote:[one] footnote:[two] footnote:[three]", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_2\" class=\"footnote\" href=\"#_footnotedef_2\" title=\"View footnote.\">2</a>]</sup> <sup class=\"footnote\">[<a id=\"_footnoteref_3\" class=\"footnote\" href=\"#_footnotedef_3\" title=\"View footnote.\">3</a>]</sup>"

# Inline interaction oracle case 67
expect AsciiDocInline.render("footnote:n[One.] footnote:n[Two.] footnote:n[]", []) == "<sup class=\"footnote\" id=\"_footnote_n\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

# Inline interaction oracle case 68
expect AsciiDocInline.render("footnote:missing[]", []) == "<sup class=\"footnoteref red\" title=\"Unresolved footnote reference.\">[missing]</sup>"

# Inline interaction oracle case 69
expect AsciiDocInline.render("pass:[footnote:[raw]]", []) == "footnote:[raw]"

# Inline interaction oracle case 70
expect AsciiDocInline.render("+++footnote:[raw]+++", []) == "footnote:[raw]"

# Inline interaction oracle case 71
expect AsciiDocInline.render("+++*a*+++ and *b*", []) == "*a* and <strong>b</strong>"

# Inline interaction oracle case 72
expect AsciiDocInline.render("[.small]`code`", []) == "<code class=\"small\">code</code>"

# Inline interaction oracle case 73
expect AsciiDocInline.render("[foo]#bar#", []) == "<span class=\"foo\">bar</span>"

# Inline interaction oracle case 74
expect AsciiDocInline.render("[red]##bar##", []) == "<span class=\"red\">bar</span>"

# Inline interaction oracle case 75
expect AsciiDocInline.render("[%option]#bar#", []) == "<span class=\"%option\">bar</span>"

# Inline interaction oracle case 76
expect AsciiDocInline.render("[#id.red.big]#bar#", []) == "<span id=\"id\" class=\"red big\">bar</span>"

# Inline interaction oracle case 77
expect AsciiDocInline.render("A [[[ref]]] reference.", []) == "A [<a id=\"ref\"></a>] reference."

# Inline interaction oracle case 78
expect AsciiDocInline.render("indexterm:[sunset]", []) == ""

# Inline interaction oracle case 79
expect AsciiDocInline.render("indexterm2:[sunset]", []) == "sunset"

# Inline interaction oracle case 80
expect AsciiDocInline.render("((sunset))", []) == "sunset"

# Inline interaction oracle case 81
expect AsciiDocInline.render("(((sunset)))", []) == ""

# Inline interaction oracle case 82
expect AsciiDocInline.render("http://example.org[path,role=external]", []) == "<a href=\"http://example.org\" class=\"external\">path</a>"

# Inline interaction oracle case 83
expect AsciiDocInline.render("https://example.org[Link,window=_blank]", []) == "<a href=\"https://example.org\" target=\"_blank\" rel=\"noopener\">Link</a>"

# Inline interaction oracle case 84
expect AsciiDocInline.render("https://example.org[Link,%nofollow]", []) == "<a href=\"https://example.org\">Link,%nofollow</a>"

# Inline interaction oracle case 85
expect AsciiDocInline.render("link:path[Link,role=red]", []) == "<a href=\"path\" class=\"red\">Link</a>"

# Inline interaction oracle case 86
expect AsciiDocInline.render("xref:guide.adoc#part[]", []) == "<a href=\"guide.html#part\">guide.html</a>"

# Inline interaction oracle case 87
expect AsciiDocInline.render("xref:#part[]", []) == "<a href=\"#part\">[part]</a>"

# Inline interaction oracle case 88
expect AsciiDocInline.render("&apos; &quot; &#123;", []) == "&apos; &quot; &#123;"

# Inline interaction oracle case 89
expect AsciiDocInline.render("{empty}{sp}{zwsp}{wj}{deg}", []) == " &#8203;&#8288;&#176;"

# Inline interaction oracle case 90
expect AsciiDocInline.render("http://example.org/path?q=a&amp;b=c", []) == "<a href=\"http://example.org/path?q=a&amp;b=c\" class=\"bare\">http://example.org/path?q=a&amp;b=c</a>"

expect AsciiDocInline.render("kbd:[Ctrl+C]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>C</kbd></span>"

expect AsciiDocInline.render("kbd:[Ctrl,Alt,Del]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd></span>"

expect AsciiDocInline.render("kbd:[Ctrl++]", [{ name: "experimental", value: "" }]) == "<span class=\"keyseq\"><kbd>Ctrl</kbd>+<kbd>+</kbd></span>"

expect AsciiDocInline.render("kbd:[Enter]", [{ name: "experimental", value: "" }]) == "<kbd>Enter</kbd>"

expect AsciiDocInline.render("btn:[Save]", [{ name: "experimental", value: "" }]) == "<b class=\"button\">Save</b>"

expect AsciiDocInline.render("menu:File[New > Project]", [{ name: "experimental", value: "" }]) == "<span class=\"menuseq\"><b class=\"menu\">File</b>&#160;<b class=\"caret\">&#8250;</b> <b class=\"submenu\">New</b>&#160;<b class=\"caret\">&#8250;</b> <b class=\"menuitem\">Project</b></span>"

expect AsciiDocInline.render("menu:View[]", [{ name: "experimental", value: "" }]) == "<b class=\"menuref\">View</b>"

# Existing character references survive the specialcharacter substitution.
escape_entities : Str -> Str
escape_entities = |source| {
	bytes = source.to_utf8()
	var $out = []
	var $pos = 0
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '&' {
			var $end = $pos + 1
			while $end < bytes.len() and $end - $pos < 32 and (word(bytes.get($end) ?? 0) or (bytes.get($end) ?? 0) == '#') {
				$end = $end + 1
			}
			if $end > $pos + 1 and (bytes.get($end) ?? 0) == ';' {
				$out = $out.concat(bytes.sublist({ start: $pos, len: $end - $pos + 1 }))
				$pos = $end
			} else {
				$out = $out.concat("&amp;".to_utf8())
			}
		} else if b == '<' {
			$out = $out.concat("&lt;".to_utf8())
		}
		else if b == '>' {
			$out = $out.concat("&gt;".to_utf8())
		}
		else {
			$out = $out.append(b)
		}
		$pos = $pos + 1
	}
	Str.from_utf8_lossy($out)
}

bracket_end : Str, U64 -> Try(U64, [Missing])
bracket_end = |source, open| {
	bytes = source.to_utf8()
	var $depth = 1
	var $pos = open + 1
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '[' {
			$depth = $depth + 1
		}
		if b == ']' {
			$depth = $depth - 1
		}
		if $depth == 0 {
			return Ok($pos)
		}
		$pos = $pos + 1
	}
	Err(Missing)
}

expect AsciiDocInline.render_with("{who}", [{ name: "who", value: "*Roc*" }], { subs: "attributes,quotes" }) == "<strong>Roc</strong>"
expect AsciiDocInline.render_with("{who}", [{ name: "who", value: "*Roc*" }], { subs: "quotes,attributes" }) == "*Roc*"

QuoteStage : { open : Str, close : Str, tag : Str, unconstrained : Bool, no_space : Bool }

QuoteMatch : { used : U64, body : Str, role : Str, escaped : Bool, escaped_role : Bool }

# Generated tags use a per-input delimiter that cannot collide with source or
# attributes. Keeping tags separate lets later quote passes see the surrounding
# text without reinterpreting generated HTML attribute values.
token : Str, U64 -> Str
token = |prefix, index| "${prefix}${index.to_str()}${prefix}"

pass_needed : Str, Groups -> Bool
pass_needed = |input, g| g.macros and (input.contains("+") or (input.contains(":") and (input.contains("pass:") or input.contains("stem:") or input.contains("latexmath:") or input.contains("asciimath:"))) or input.contains("$$"))

quote_needed : Str, Groups -> Bool
quote_needed = |input, g| {
	quoted = g.quotes and (input.contains("*") or input.contains("_") or input.contains("`") or input.contains("#") or input.contains("^") or input.contains("~"))
	quoted or pass_needed(input, g)
}

# Formatting reads attribute values only to reserve a private delimiter.
# Carry that delimiter across dynamic writes rather than materialize every
# stored counter. Passthrough substitution still receives the full context.
quote_owned : Str, List({ name : Str, value : Str }), AsciiDocSource.Index, SourceHint, Bool, Groups, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str -> QuoteResult
quote_owned = |input, attrs, source_index, hint, dynamic_tags_possible, g, attributes, removed, tag_prefix| {
	# The carried prefix already excludes every dynamic value. Pass matching
	# requests effective bindings only when an immediate pass must be rendered.
	quote_view(input, { source_before: hint_before(hint), source_after: hint_after(hint), source_prefix: hint_prefix(hint), dynamic_tags_possible, source: attrs, source_index, appended: [], values: attributes, removed, prefix: tag_prefix }, g)
}

macro_candidate : Str -> Bool
macro_candidate = |source| source.contains(":") or source.contains("@") or source.contains("+") or source.contains("[") or source.contains("$") or source.contains("((") or source.contains("<<") or source.contains("&lt;&lt;")

quote_all : Str, List({ name : Str, value : Str }), Groups, U64 -> QuoteResult
quote_all = |input, attrs, g, _depth| quote_prefixed(input, attrs, g, "\u(1)")

quote_prefixed : Str, List({ name : Str, value : Str }), Groups, Str -> QuoteResult
quote_prefixed = |input, attrs, g, prefix| quote_view(input, { ..static_view(attrs), prefix }, g)

# Track only literal output: protected pass bodies cannot terminate an outer
# set reference. The matcher spans emitted chunks and resets at line boundaries.
capture_set : Str, { matched : U64, active : Bool, previous : U8 } -> { matched : U64, active : Bool, previous : U8 }
capture_set = |text, state| {
	var $matched = state.matched
	var $active = state.active
	var $previous = state.previous
	pattern = "{set:".to_utf8()
	for byte in text.to_utf8() {
		if byte == '\n' or byte == '}' {
			$active = False
		}
		if byte == (pattern.get($matched) ?? 0) and !($matched == 0 and $previous == '\\') {
			$matched = $matched + 1
			if $matched == 5 {
				$active = True
				$matched = 0
			}
		} else {
			$matched = if (byte == '{' and $previous != '\\') 1 else 0
		}
		$previous = byte
	}
	{ matched: $matched, active: $active, previous: $previous }
}

quote_view : Str, AttributeView, Groups -> QuoteResult
quote_view = |input, attrs, g| {
	# Most attribute references and URLs need no private quote slots.
	if !quote_needed(input, g) {
		return { source: input, prefix: "", tags: [], deferred: [] }
	}
	certified = attrs.source_prefix != "" and attrs.prefix != "" and attrs.prefix.to_utf8().all(|byte| byte == 1)
	var $prefix = if (certified and attrs.source_prefix.count_utf8_bytes() > attrs.prefix.count_utf8_bytes()) attrs.source_prefix else attrs.prefix
	while input.contains($prefix) or (!certified and (attrs.source_before.any(|attr| attr.value.contains($prefix)) or attrs.source.any(|attr| attr.value.contains($prefix)) or attrs.source_after.any(|attr| attr.value.contains($prefix)))) or attrs.appended.any(|attr| attr.value.contains($prefix)) {
		$prefix = $prefix.concat("\u(1)")
	}
	var $tags = []
	var $deferred = []
	# Attribute set captures restore slot bodies before deferred evaluation.
	# Keep their preview semantics; other deferred bodies are overwritten.
	track_capture = input.contains("{set:")
	var $text = input
	# Unconstrained passthroughs are extracted before single-plus spans. A
	# single-plus span may enclose an already extracted passthrough slot.
	for single in [False, True] {
		needed = g.macros and ($text.contains("+") or (!single and ($text.contains("pass:") or $text.contains("stem:") or $text.contains("latexmath:") or $text.contains("asciimath:") or $text.contains("$$"))))
		if needed {
			pass_source = $text
			var $remaining = pass_source
			$text = ""
			var $pos = 0
			bytes = pass_source.to_utf8()
			var $capture = { matched: 0, active: False, previous: 0 }
			var $pass_bracket_cursor = empty_bracket_cursor
			var $pass_role_cursor = empty_role_cursor
			while $pos < bytes.len() {
				rest = $remaining
				pending_result = if (g.macros and !single) deferred_pass_cached(rest, $pos, $pass_bracket_cursor) else { result: Err(Missing), cursor: $pass_bracket_cursor }
				$pass_bracket_cursor = pending_result.cursor
				pending = pending_result.result
				pass_result = match pending {
					Ok(deferred) if !track_capture or !$capture.active => { matched: { used: deferred.used, body: "", role: "", escaped: False, escaped_role: False }, cursor: $pass_bracket_cursor, role_cursor: $pass_role_cursor }
					_ => if (g.macros) pass_match_cached(rest, if ($pos == 0) 0 else bytes.get($pos - 1) ?? 0, { ..attrs, prefix: $prefix }, single, word_before(bytes, $pos), $pos, $pass_bracket_cursor, $pass_role_cursor) else { matched: { used: 0, body: "", role: "", escaped: False, escaped_role: False }, cursor: $pass_bracket_cursor, role_cursor: $pass_role_cursor }
				}
				$pass_bracket_cursor = pass_result.cursor
				$pass_role_cursor = pass_result.role_cursor
				pass = pass_result.matched
				if pass.used > 0 {
					if pass.escaped {
						$text = $text.concat(pass.body)
						if track_capture and !single {
							$capture = capture_set(pass.body, $capture)
						}
					} else {
						body = pass.body
						html = if (pass.role == "" or pass.escaped_role) body else "<span${quote_attributes(pass.role)}>${body}</span>"
						slot = if (pass.escaped_role) "[${pass.role}]${token($prefix, $tags.len())}" else token($prefix, $tags.len())
						$text = $text.concat(slot)
						if track_capture and !single {
							$capture = capture_set(slot, $capture)
						}
						match pending {
							Ok(deferred) if !single => {
								$deferred = $deferred.append({ slot: $tags.len(), source: deferred.source, subs: deferred.subs })
							}
							_ => {}
						}
						$tags = $tags.append(html)
					}
					previous_pos = $pos
					$pos = $pos + pass.used
					if $pos < bytes.len() {
						# Match endpoints follow ASCII delimiters. Validate only
						# the consumed prefix, retaining the untouched UTF-8 suffix.
						$remaining = $remaining.drop_prefix(slice(pass_source, previous_pos, pass.used))
					}
				} else {
					c = bytes.get($pos) ?? 0
					n = if (c < 128) 1 else if (c < 224) 2 else if (c < 240) 3 else 4
					var $end = $pos + n
					while $end < bytes.len() and !possible_pass(bytes, $end, single) {
						$end = $end + 1
					}
					literal = slice(pass_source, $pos, $end - $pos)
					$text = $text.concat(literal)
					if track_capture and !single {
						$capture = capture_set(literal, $capture)
					}
					$pos = $end
					if $pos < bytes.len() {
						$remaining = $remaining.drop_prefix(literal)
					}
				}
			}
		}
	}
	stages = [
		{ open: "**", close: "**", tag: "strong", unconstrained: True, no_space: False },
		{ open: "*", close: "*", tag: "strong", unconstrained: False, no_space: False },
		{ open: "\"`", close: "`\"", tag: "double", unconstrained: False, no_space: False },
		{ open: "'`", close: "`'", tag: "single", unconstrained: False, no_space: False },
		{ open: "``", close: "``", tag: "code", unconstrained: True, no_space: False },
		{ open: "`", close: "`", tag: "code", unconstrained: False, no_space: False },
		{ open: "__", close: "__", tag: "em", unconstrained: True, no_space: False },
		{ open: "_", close: "_", tag: "em", unconstrained: False, no_space: False },
		{ open: "##", close: "##", tag: "mark", unconstrained: True, no_space: False },
		{ open: "#", close: "#", tag: "mark", unconstrained: False, no_space: False },
		{ open: "^", close: "^", tag: "sup", unconstrained: True, no_space: True },
		{ open: "~", close: "~", tag: "sub", unconstrained: True, no_space: True },
	]
	if g.quotes {
		for stage in stages {
			if $text.contains(stage.open) {
				source = $text
				bs = source.to_utf8()
				marker_open = stage.open.to_utf8()
				marker_close = stage.close.to_utf8()
				first_marker = marker_open.first() ?? 0
				closers = quote_closers(bs, stage, marker_close)
				var $next = ""
				var $i = 0
				var $boundary = bs.len() + 1
				var $quote_role_cursor = empty_bracket_cursor
				while $i < bs.len() {
					result = if (!stage.unconstrained and $i == $boundary) { matched: { used: 0, body: "", role: "", escaped: False, escaped_role: False }, cursor: $quote_role_cursor } else quote_match_cached(source, bs, $i, stage, marker_open, marker_close, closers, g.special, $quote_role_cursor)
					$quote_role_cursor = result.cursor
					matched = result.matched
					if matched.used > 0 {
						if matched.escaped {
							$next = $next.concat(matched.body)
						} else {
							tag = if (stage.tag == "mark" and matched.role != "") "span" else stage.tag
							opening = if (tag == "double") "&#8220;" else if (tag == "single") "&#8216;" else "<${tag}${quote_attributes(if (matched.escaped_role) "" else matched.role)}>"
							closing = if (tag == "double") "&#8221;" else if (tag == "single") "&#8217;" else "</${tag}>"
							before = token($prefix, $tags.len())
							after = token($prefix, $tags.len() + 1)
							$tags = $tags.append(opening).append(closing)
							$next = $next.concat("${if (matched.escaped_role) "[${matched.role}]" else ""}${before}${matched.body}${after}")
						}
						$i = $i + matched.used
						$boundary = $i
					} else {
						c = bs.get($i) ?? 0
						n = if (c < 128) 1 else if (c < 224) 2 else if (c < 240) 3 else 4
						var $end = $i + n
						while $end < bs.len() and (bs.get($end) ?? 0) != first_marker and (bs.get($end) ?? 0) != '[' and (bs.get($end) ?? 0) != '\\' {
							$end = $end + 1
						}
						$next = $next.concat(slice(source, $i, $end - $i))
						$i = $end
					}
				}
				$text = $next
			}
		}
	}
	{ source: $text, prefix: $prefix, tags: $tags, deferred: $deferred }
}

quote_match : Str, List(U8), U64, QuoteStage, List(U8), List(U8), List(U64), Bool -> QuoteMatch
quote_match = |source, bs, position, stage, opening, closing, closers, special| quote_match_cached(source, bs, position, stage, opening, closing, closers, special, empty_bracket_cursor).matched

# Raw quote roles close at the first literal ], independently of escape/body
# rules. Each quote stage owns a fresh cursor over its immutable byte source.
cached_quote_role_end : List(U8), U64, BracketCursor -> { end : Try(U64, [Missing]), cursor : BracketCursor }
cached_quote_role_end = |bs, open, cursor| {
	if cursor.known and open >= cursor.searched_after {
		match cursor.closing {
			Ok(position) if position > open => return { end: Ok(position), cursor }
			Err(_) => return { end: Err(Missing), cursor }
			_ => {}
		}
	}
	end = find_bytes(bs, [93], open + 1)
	{ end, cursor: { known: True, searched_after: open, closing: end } }
}

quote_match_cached : Str, List(U8), U64, QuoteStage, List(U8), List(U8), List(U64), Bool, BracketCursor -> { matched : QuoteMatch, cursor : BracketCursor }
quote_match_cached = |source, bs, position, stage, opening, closing, closers, special, incoming_cursor| {
	var $cursor = incoming_cursor
	none = { used: 0, body: "", role: "", escaped: False, escaped_role: False }
	first = bs.get(position) ?? 0
	if first != (opening.first() ?? 0) and first != '[' and first != '\\' {
		return { matched: none, cursor: $cursor }
	}
	escaped = first == '\\'
	var $offset = if (escaped) 1 else 0
	var $role_start = 0
	var $role_end = 0
	if (bs.get(position + $offset) ?? 0) == '[' {
		searched = cached_quote_role_end(bs, position + $offset, $cursor)
		$cursor = searched.cursor
		match searched.end {
			Ok(end) => {
				$role_start = position + $offset + 1
				$role_end = end
				$offset = end + 1 - position
			}
			Err(_) => return { matched: none, cursor: $cursor }
		}
	}
	if !bs.drop_first(position + $offset).starts_with(opening) {
		return { matched: none, cursor: $cursor }
	}
	previous = if (position == 0 or escaped) 0 else bs.get(position - 1) ?? 0
	if !stage.unconstrained {
		if (!escaped and word_before(bs, position)) or (special and (previous == '<' or previous == '>' or previous == '&')) or previous == ';' or previous == ':' or previous == '}' or (stage.tag == "mark" and previous == '&') or (stage.tag == "code" and (previous == '\'' or previous == '"' or previous == '`')) {
			return { matched: none, cursor: $cursor }
		}
	}
	start = position + $offset + opening.len()
	if start >= bs.len() or ((!stage.unconstrained or stage.no_space) and space(bs.get(start) ?? 0)) {
		return { matched: none, cursor: $cursor }
	}
	# All invalid closing boundaries were filtered once for the whole pass.
	# Binary search avoids rescanning a suffix for every unmatched opener.
	var $low = 0
	var $high = closers.len()
	while $low < $high {
		middle = $low + (($high - $low) // 2)
		if (closers.get(middle) ?? 0) <= start {
			$low = middle + 1
		} else {
			$high = middle
		}
	}
	match closers.get($low) {
		Err(_) => { matched: none, cursor: $cursor }
		Ok(end) => {
			if stage.no_space and bs.sublist({ start, len: end - start }).any(space) {
				return { matched: none, cursor: $cursor }
			}
			used = end + closing.len() - position
			role = if ($role_start == 0) "" else slice(source, $role_start, $role_end - $role_start)
			escaped_role = escaped and role != "" and !stage.unconstrained
			content = slice(source, start, end - start)
			{ matched: { used, body: if (escaped and !escaped_role) slice(source, position + 1, used - 1) else content, role, escaped: escaped and !escaped_role, escaped_role }, cursor: $cursor }
		}
	}
}

quote_attributes : Str -> Str
quote_attributes = |role| {
	if role == "" {
		return ""
	}
	id = if (role.starts_with("#")) drop(role, 1).split_on(".").first() ?? "" else ""
	classes = if (role.starts_with("#")) Str.join_with(role.split_on(".").drop_first(1), " ") else if (role.starts_with(".")) Str.join_with(drop(role, 1).split_on("."), " ") else role
	"${if (id == "") "" else " id=\"${id}\""}${if (classes == "") "" else " class=\"${classes}\""}"
}

RoleCursor : { known : Bool, searched_after : U64, closing : Try(U64, [Missing]), after : Str }

empty_role_cursor : RoleCursor
empty_role_cursor = { known: False, searched_after: 0, closing: Err(Missing), after: "" }

# Raw role syntax closes at the next literal ], even after a backslash. Keep
# its bounded cursor separate from first_bracket_end's escaped-body cursor.
cached_role_end : Str, U64, U64, RoleCursor -> { end : Try(U64, [Missing]), cursor : RoleCursor }
cached_role_end = |source, open, base, cursor| {
	absolute_open = base + open
	if cursor.known and absolute_open >= cursor.searched_after {
		match cursor.closing {
			Ok(position) if position > absolute_open => return { end: Ok(position - base), cursor }
			Err(_) => return { end: Err(Missing), cursor }
			_ => {}
		}
	}
	end = find(source, "]", open + 1)
	closing = match end {
		Ok(position) => Ok(base + position)
		Err(_) => Err(Missing)
	}
	# This tail belongs to the immutable pass-source phase, not to its owned
	# attribute state. Reuse it for later role opens sharing the same raw closer.
	after = match end {
		Ok(position) => source.drop_prefix(take(source, position + 1))
		Err(_) => ""
	}
	{ end, cursor: { known: True, searched_after: absolute_open, closing, after } }
}

pass_match : Str, U8, AttributeView, Bool, Bool -> QuoteMatch
pass_match = |source, prev, attrs, single, prev_word| pass_match_cached(source, prev, attrs, single, prev_word, 0, empty_bracket_cursor, empty_role_cursor).matched

pass_match_cached : Str, U8, AttributeView, Bool, Bool, U64, BracketCursor, RoleCursor -> { matched : QuoteMatch, cursor : BracketCursor, role_cursor : RoleCursor }
pass_match_cached = |source, prev, attrs, single, prev_word, base, incoming_cursor, incoming_role_cursor| {
	var $cursor = incoming_cursor
	var $role_cursor = incoming_role_cursor
	none = { used: 0, body: "", role: "", escaped: False, escaped_role: False }
	escaped = source.starts_with("\\")
	var $offset = if (escaped) 1 else 0
	var $role_start = 0
	var $role_end = 0
	if (source.to_utf8().get($offset) ?? 0) == '[' {
		searched_role = cached_role_end(source, $offset, base, $role_cursor)
		$role_cursor = searched_role.cursor
		match searched_role.end {
			Ok(end) => {
				$role_start = $offset + 1
				$role_end = end
				$offset = end + 1
			}
			Err(_) => return { matched: none, cursor: $cursor, role_cursor: $role_cursor }
		}
	}

	rest = if ($role_start != 0) $role_cursor.after else if (escaped) source.drop_prefix("\\") else source
	var $math_prefix = ""
	for prefix in ["stem:", "latexmath:", "asciimath:"] {
		if rest.starts_with(prefix) {
			$math_prefix = prefix
			break
		}
	}
	if !single and $math_prefix != "" {
		match find(rest, "[", $math_prefix.to_utf8().len()) {
			Ok(open) => {
				searched = cached_bracket_end(rest, open, base + $offset, $cursor)
				$cursor = searched.cursor
				match searched.end {
					Ok(end) => {
						used = $offset + end + 1
						if escaped {
							return { matched: { used, body: drop(take(source, used), 1), role: "", escaped: True, escaped_role: False }, cursor: $cursor, role_cursor: $role_cursor }
						}
						subs = slice(rest, $math_prefix.to_utf8().len(), open - $math_prefix.to_utf8().len())
						raw = repl(slice(rest, open + 1, end - open - 1), "\\]", "]")
						dialect = if ($math_prefix == "stem:") lookup_view(attrs, "stem") ?? "" else $math_prefix
						latex = dialect == "latexmath:" or dialect == "latexmath" or dialect == "latex" or dialect == "tex"
						content = if (latex and raw.starts_with("$") and raw.ends_with("$")) slice(raw, 1, raw.to_utf8().len() - 2) else raw
						html = render_with_view(content, attrs, { subs: if (subs == "") "specialcharacters" else subs })
						body = if (latex) "\\(${html}\\)" else "\\$${html}\\$"
						return { matched: { used, body, role: "", escaped: False, escaped_role: False }, cursor: $cursor, role_cursor: $role_cursor }
					}
					Err(_) => return { matched: none, cursor: $cursor, role_cursor: $role_cursor }
				}
			}
			Err(_) => return { matched: none, cursor: $cursor, role_cursor: $role_cursor }
		}
	}
	if !single and rest.starts_with("pass:") {
		match find(rest, "[", 5) {
			Ok(open) => {
				searched = cached_bracket_end(rest, open, base + $offset, $cursor)
				$cursor = searched.cursor
				match searched.end {
					Ok(end) => {
						used = $offset + end + 1
						if escaped {
							return { matched: { ..none, used, body: take(source, used), escaped: True }, cursor: $cursor, role_cursor: $role_cursor }
						}
						subs = slice(rest, 5, open - 5)
						content = repl(slice(rest, open + 1, end - open - 1), "\\]", "]")
						return { matched: { used, body: render_with_view(content, attrs, { subs: subs }), role: "", escaped: False, escaped_role: False }, cursor: $cursor, role_cursor: $role_cursor }
					}
					Err(_) => return { matched: none, cursor: $cursor, role_cursor: $role_cursor }
				}
			}
			Err(_) => return { matched: none, cursor: $cursor, role_cursor: $role_cursor }
		}
	}
	choices = if single {
		if (rest.starts_with("+")) [1] else []
	} else if (rest.starts_with("+++")) [3, 2] else if (rest.starts_with("++") or rest.starts_with("$$")) [2] else []
	for n in choices {
		if !(n == 1 and (prev_word or prev == ';' or prev == ':' or prev == '\\')) {
			marker = take(rest, n)
			var $search = n
			var $done = False
			while $search <= rest.to_utf8().len() and !$done {
				match find(rest, marker, $search) {
					Ok(end) => {
						content = slice(rest, n, end - n)
						if n > 1 or (end > n and !space(content.to_utf8().first() ?? 0) and !space(content.to_utf8().last() ?? 0) and !word_at(rest.to_utf8(), end + n)) {
							used = $offset + end + n
							role = if ($role_start == 0) "" else slice(source, $role_start, $role_end - $role_start)
							body = if (escaped and role == "") drop(take(source, used), 1) else if (n == 3) content else escape(content)
							return { matched: { used, body, role, escaped: escaped and role == "", escaped_role: escaped and role != "" }, cursor: $cursor, role_cursor: $role_cursor }
						}
						$search = end + n
					}
					Err(_) => {
						$done = True
					}
				}
			}
		}
	}
	{ matched: none, cursor: $cursor, role_cursor: $role_cursor }
}

restore_tags : Str, Str, List(Str) -> Str
restore_tags = |source, prefix, tags| restore_tags_depth(source, prefix, tags, 0)

restore_tags_depth : Str, Str, List(Str), U64 -> Str
restore_tags_depth = |source, prefix, tags, depth| {
	if depth > 64 {
		return source
	}
	if prefix == "" or tags.is_empty() {
		return source
	}
	var $out = ""
	var $pos = 0
	bytes = source.to_utf8()
	prefix_len = prefix.to_utf8().len()
	while $pos < bytes.len() {
		match find(source, prefix, $pos) {
			Ok(start) => {
				$out = $out.concat(slice(source, $pos, start - $pos))
				match find(source, prefix, start + prefix_len) {
					Ok(end) => {
						index = U64.from_str(slice(source, start + prefix_len, end - start - prefix_len))
						match index {
							Ok(value) => {
								$out = $out.concat(
									match tags.get(value) {
										Ok(tag) => restore_tags_depth(tag, prefix, tags, depth + 1)
										Err(_) => slice(source, start, end + prefix_len - start)
									},
								)
							}
							Err(_) => {
								$out = $out.concat(slice(source, start, end + prefix_len - start))
							}
						}
						$pos = end + prefix_len
					}
					Err(_) => {
						$out = $out.concat(drop(source, start))
						$pos = bytes.len()
					}
				}
			}
			Err(_) => {
				$out = $out.concat(drop(source, $pos))
				$pos = bytes.len()
			}
		}
	}
	$out
}

expect AsciiDocInline.render("\\^alpha beta^", []) == "\\^alpha beta^"

expect AsciiDocInline.render("\\*alpha beta*_.", []) == "\\*alpha beta*_."

expect AsciiDocInline.render("\\**α β***", []) == "<strong>*α β</strong>**"

expect AsciiDocInline.render("[.red]++x *bold* y++!", []) == "<span class=\"red\">x *bold* y</span>!"

expect AsciiDocInline.render("[.red]+++alpha+++).", []) == "<span class=\"red\">alpha</span>)."

expect AsciiDocInline.render("[.red]_α β__.", []) == "<em class=\"red\">α β_</em>."

expect AsciiDocInline.render("\\##x_y##.", []) == "<mark>#x_y</mark>#."

expect AsciiDocInline.render("\\+alpha+_.", []) == "\\+alpha+_."

expect AsciiDocInline.render("[.red]^_em_^x.", []) == "<sup class=\"red\"><em>em</em></sup>x."

expect AsciiDocInline.render("\\``alpha beta``).", []) == "<code>`alpha beta`</code>)."

expect AsciiDocInline.render("___em___ end.", []) == "<em><em>em</em></em> end."

expect AsciiDocInline.render("\\**a\\nb**).", []) == "<strong>*a\\nb</strong>*)."

expect AsciiDocInline.render("\\__alpha__*.", []) == "<em>_alpha_</em>*."

expect AsciiDocInline.render("[.red]_x_y_*.", []) == "<em class=\"red\">x_y</em>*."

expect AsciiDocInline.render("\\*x *bold* y*x.", []) == "*x *bold* y*x."

expect AsciiDocInline.render("\\++x *bold* y++ end.", []) == "+x *bold* y+ end."

expect AsciiDocInline.render("\\+++x *bold* y+++ end.", []) == "++x *bold* y++ end."

expect AsciiDocInline.render("[[[ref,Label]]] A book", []) == "[<a id=\"ref\"></a>] A book"

expect AsciiDocInline.render("A >*bold* and <_em_ and &#62;*bold*.", []) == "A &gt;*bold* and &lt;_em_ and &#62;*bold*."

expect AsciiDocInline.render("Test ** a **.", []) == "Test <strong> a </strong>."

expect AsciiDocInline.render("__ a __", []) == "<em> a </em>"

expect AsciiDocInline.render("[[[ref,Label]]] A book", [{ name: "style", value: "bibliography" }]) == "<a id=\"ref\"></a>[Label] A book"
expect AsciiDocInline.render("A *bold* \u(1) and _em_.", []) == "A <strong>bold</strong> \u(1) and <em>em</em>."

expect AsciiDocInline.render("A stem:[x + y] formula.", []) == "A \\$x + y\\$ formula."
expect AsciiDocInline.render("A latexmath:[x^2 + y_1] formula.", []) == "A \\(x^2 + y_1\\) formula."
expect AsciiDocInline.render("A stem:[x^2] formula.", [{ name: "stem", value: "tex" }]) == "A \\(x^2\\) formula."
expect AsciiDocInline.render("A latexmath:[$x$] formula.", []) == "A \\(x\\) formula."
expect AsciiDocInline.render("A stem:[a < b & c] formula.", []) == "A \\$a &lt; b &amp; c\\$ formula."
expect AsciiDocInline.render("A latexmath:q[*x*] formula.", []) == "A \\(<strong>x</strong>\\) formula."

expect AsciiDocInline.render("Hello {NAME}.", [{ name: "name", value: "Roc" }]) == "Hello Roc."

# Snapshot revisions allow a source definition to reset a dynamic attribute,
# including a definition whose text equals its previous definition.
refresh_state : List({ name : Str, value : Str }), AsciiDocInline.State -> AsciiDocInline.State
refresh_state = |attrs, state| refresh_state_indexed(attrs, state, Err(Missing))

refresh_state_indexed : List({ name : Str, value : Str }), AsciiDocInline.State, SourceHint -> AsciiDocInline.State
refresh_state_indexed = |attrs, state, hint| {
	{ snapshot_before: incoming_before, snapshot_after: incoming_after, trusted_snapshot_generation, trusted_snapshot_token, dynamic_tags_possible, attributes: incoming_attributes, removed: incoming_removed, snapshot: incoming_snapshot, snapshot_index: incoming_index, tag_prefix, footnotes, note_index, indexed_note_count, emitted_footnotes, refs, ref_index, indexed_ref_count } = state
	{ attributes, removed, snapshot, snapshot_index, snapshot_before, snapshot_after } = refresh_hint(attrs, incoming_attributes, incoming_removed, incoming_snapshot, incoming_index, incoming_before, incoming_after, trusted_snapshot_token, trusted_snapshot_generation, hint)
	{ snapshot_before, snapshot_after, trusted_snapshot_generation: hint_generation(hint), trusted_snapshot_token: hint_token(hint), dynamic_tags_possible: dynamic_tags_possible and !attributes.is_empty(), attributes, removed, snapshot, snapshot_index, tag_prefix, footnotes, note_index, indexed_note_count, emitted_footnotes, refs, ref_index, indexed_ref_count }
}

# Source attributes use their first matching binding, just like direct().
source_attribute_index : List({ name : Str, value : Str }) -> Dict(Str, Str)
source_attribute_index = |attrs| {
	var $index = Dict.empty()
	for attr in attrs {
		if !$index.contains(attr.name) {
			$index = $index.insert(attr.name, attr.value)
		}
	}
	$index
}

direct : List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
direct = |attrs, name| {
	for attr in attrs {
		if attr.name == name {
			return Ok(attr.value)
		}
	}
	Err(Missing)
}

merged_attributes : List({ name : Str, value : Str }), AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool) -> List({ name : Str, value : Str })
merged_attributes = |attrs, attributes, removed| {
	if attributes.is_empty() and removed.is_empty() {
		return attrs
	}
	attributes.to_list().map(|(name, value)| { name, value }).concat(attrs.keep_if(|attr| !removed.contains(attr.name) and !attributes.contains(attr.name)))
}

# Dynamic values override the source snapshot, then removed values remain
# hidden. Built-in attribute defaults are applied by the ordinary lookup.
dynamic_value : List({ name : Str, value : Str }), RenderContext, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str -> Try(Str, [Missing])
dynamic_value = |attrs, context, attributes, removed, name| {
	match attributes.get(name) {
		Ok(value) => Ok(value)
		Err(_) => if (removed.contains(name)) Err(Missing) else source_value(attrs, context.snapshot_index, context.snapshot_before, context.snapshot_after, name)
	}
}

# A longer delimiter stays safe for every value that was previously stored.
# Removing or redefining attributes therefore never requires a dictionary scan.
safe_prefix : Str, Str -> Str
safe_prefix = |value, prefix| {
	var $prefix = prefix
	while value.contains($prefix) {
		$prefix = $prefix.concat("\u(1)")
	}
	$prefix
}

# A trusted source certificate already excludes every static value. The
# footnote slot delimiter still has to exclude the actual substituted source.
layered_delimiter : Str, List({ name : Str, value : Str }), List({ name : Str, value : Str }), List({ name : Str, value : Str }), Str -> Str
layered_delimiter = |source, attrs, before, after, certificate| {
	var $prefix = if (certificate == "") "\u(1)" else certificate
	while source.contains($prefix) or (certificate == "" and [before, attrs, after].any(|part| part.any(|attr| attr.value.contains($prefix)))) {
		$prefix = $prefix.concat("\u(1)")
	}
	$prefix
}

certified_delimiter : Str, List({ name : Str, value : Str }), Str -> Str
certified_delimiter = |source, attrs, certificate| {
	if certificate == "" {
		return attribute_delimiter(source, attrs, "")
	}
	var $prefix = certificate
	while source.contains($prefix) {
		$prefix = $prefix.concat("\u(1)")
	}
	$prefix
}

attribute_delimiter : Str, List({ name : Str, value : Str }), Str -> Str
attribute_delimiter = |source, attrs, prefix| {
	if prefix != "" {
		return prefix
	}
	var $delimiter = "\u(1)"
	while source.contains($delimiter) or attrs.any(|attr| attr.value.contains($delimiter)) {
		$delimiter = $delimiter.concat("\u(1)")
	}
	$delimiter
}

# Only for the protected inserted-value subscan: quote/attribute/special
# processing is disabled and pass extraction stays disabled. Keep a conservative
# set of possible handler bytes; outer protection and restoration are unchanged.
inserted_scan_needed : Str, Groups -> Bool
inserted_scan_needed = |value, g| {
	if g.replacements and (value.contains("&") or value.contains(".") or value.contains("-") or value.contains("<") or value.contains("=") or value.contains("(") or value.contains("'")) {
		return True
	}
	if g.post and value.contains(" +") {
		return True
	}
	g.macros and (value.contains(":") or value.contains("@") or value.contains("[") or value.contains("(") or value.contains("<") or value.contains("&") or value.contains("\\") or value.contains("$"))
}

mutate_owned_attributes : Str, List({ name : Str, value : Str }), Groups, AsciiDocDict.Map(Str), AsciiDocDict.Map(Bool), Str, List({ name : Str, text : Str }), Dict(Str, U64), U64, RenderContext, Str, List(Str) -> { source : Str, context : RenderContext, attributes : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), tag_prefix : Str, footnotes : List({ name : Str, text : Str }), note_index : Dict(Str, U64), indexed_note_count : U64, tags : List(Str), prefix : Str }
mutate_owned_attributes = |source, attrs, g, attributes, removed, tag_prefix, footnotes, note_index, indexed_note_count, context, prefix, tags| {
	if !g.attributes or !source.contains("{") {
		return { source, context, attributes, removed, tag_prefix, footnotes, note_index, indexed_note_count, tags, prefix }
	}
	# Each growing collection is an owned argument. The read-only context
	# contains neither attribute dictionaries nor footnote/name registries.
	var $context = context
	var $delimiter = prefix
	var $attributes = attributes
	var $removed = removed
	var $tag_prefix = tag_prefix
	var $footnotes = footnotes
	var $note_index = note_index
	var $indexed_note_count = indexed_note_count
	var $tags = tags
	var $lines = []
	for line in source.split_on("\n") {
		bytes = line.to_utf8()
		var $out = ""
		var $pos = 0
		var $drop_line = False
		var $drop_empty = False
		while $pos < bytes.len() {
			byte = bytes.get($pos) ?? 0
			# Escaped references are protected from this stage, then unescaped
			# by the ordinary scanner.
			if byte == '\\' and (bytes.get($pos + 1) ?? 0) == '{' {
				match find_bytes(bytes, "}".to_utf8(), $pos + 2) {
					Ok(end) => {
						$delimiter = if ($delimiter != "") $delimiter else layered_delimiter(source, attrs, $context.snapshot_before, $context.snapshot_after, $context.source_prefix)
						$out = $out.concat(token($delimiter, $tags.len()))
						$tags = $tags.append(slice(line, $pos + 1, end - $pos))
						$pos = end + 1
					}
					Err(_) => {
						$out = $out.concat(drop(line, $pos))
						$pos = bytes.len()
					}
				}
			} else if byte == '{' {
				match find_bytes(bytes, "}".to_utf8(), $pos + 1) {
					Ok(end) => {
						key = slice(line, $pos + 1, end - $pos - 1)
						if key.contains("{") and !key.starts_with("set:") and !key.starts_with("counter:") and !key.starts_with("counter2:") {
							$out = $out.concat("{")
							$pos = $pos + 1
						} else {
							parts = key.split_on(":")
							kind = parts.first() ?? ""
							name = (parts.get(1) ?? "").with_ascii_lowercased()
							if (kind == "counter" or kind == "counter2") and name != "" {
								current = dynamic_value(attrs, $context, $attributes, $removed, name)
								value = match current {
									Ok(text) if text != "" => successor(text)
									_ => if (parts.len() > 2) Str.join_with(parts.drop_first(2), ":") else "1"
								}
								$tag_prefix = safe_prefix(value, $tag_prefix)
								$attributes = $attributes.insert(name, value)
								if name.starts_with("__inline-tag:") {
									$context = { ..$context, dynamic_tags_possible: True }
								}
								$removed = $removed.remove(name)
								if kind == "counter" {
									$out = $out.concat(value)
								} else {
									$drop_empty = True
								}
							} else if kind == "set" and name != "" {
								if name.ends_with("!") {
									unset = take(name, name.to_utf8().len() - 1)
									$attributes = $attributes.remove(unset)
									$removed = $removed.insert(unset, True)
									$drop_line = True
								} else {
									raw = Str.join_with(parts.drop_first(2), ":")
									value = scan(restore_tags(raw, prefix, tags), [], { ..empty, special: g.special }, 0)
									$tag_prefix = safe_prefix(value, $tag_prefix)
									$attributes = $attributes.insert(name, value)
									if name.starts_with("__inline-tag:") {
										$context = { ..$context, dynamic_tags_possible: True }
									}
									$removed = $removed.remove(name)
									$drop_empty = True
								}
							} else {
								normalized = key.with_ascii_lowercased()
								resolved = match dynamic_value(attrs, $context, $attributes, $removed, normalized) {
									Ok(value) => Ok(value)
									Err(_) => lookup([], normalized)
								}
								match resolved {
									Ok(value) => {
										if g.attr_quotes or !value.to_utf8().any(|b| b == '<' or b == '>' or b == '&' or b == '+') {
											$out = $out.concat(value)
										} else {
											subs = Str.join_with((if (g.replacements) ["replacements"] else []).concat(if (g.macros) ["macros"] else []).concat(if (g.post) ["post_replacements"] else []), ",")
											var $rendered_html = ""
											if !value.contains("footnote:") {
												effective = { source_before: $context.snapshot_before, source_after: $context.snapshot_after, source_prefix: $context.source_prefix, dynamic_tags_possible: $context.dynamic_tags_possible, source: attrs, source_index: $context.snapshot_index, appended: [], values: $attributes, removed: $removed, prefix: $tag_prefix }
												# Without a possible handler, the protected subscan is identity.
												$rendered_html = if (!inserted_scan_needed(value, g)) value else scan_view(value, effective, { ..g, quotes: False, attributes: False, special: False, passes_done: True }, 0)
											} else {
												nested_hint = context_hint($context)
												shared = assemble_state($context, $attributes, $removed, $tag_prefix, $footnotes, $note_index, $indexed_note_count)
												$attributes = AsciiDocDict.empty()
												$removed = AsciiDocDict.empty()
												$footnotes = []
												$note_index = Dict.empty()
												{ html, state: { snapshot_before: next_before, snapshot_after: next_after, trusted_snapshot_generation: next_generation, trusted_snapshot_token: next_snapshot_token, dynamic_tags_possible: next_dynamic_tags, attributes: next_attributes, removed: next_removed, tag_prefix: next_prefix, footnotes: next_notes, note_index: next_names, indexed_note_count: next_count, emitted_footnotes: next_emitted, refs: next_refs, ref_index: next_ref_index, indexed_ref_count: next_ref_count, snapshot: next_snapshot, snapshot_index: next_snapshot_index } } = render_dispatch(value, attrs, { subs: subs }, shared, nested_hint)
												$rendered_html = html
												$attributes = next_attributes
												$removed = next_removed
												$tag_prefix = next_prefix
												$footnotes = next_notes
												$note_index = next_names
												$indexed_note_count = next_count
												$context = { snapshot_before: next_before, snapshot_after: next_after, source_prefix: $context.source_prefix, trusted_snapshot_generation: next_generation, trusted_snapshot_token: next_snapshot_token, dynamic_tags_possible: next_dynamic_tags, emitted_footnotes: next_emitted, refs: next_refs, ref_index: next_ref_index, indexed_ref_count: next_ref_count, snapshot: next_snapshot, snapshot_index: next_snapshot_index }
											}
											$delimiter = if ($delimiter != "") $delimiter else layered_delimiter(source, attrs, $context.snapshot_before, $context.snapshot_after, $context.source_prefix)
											$out = $out.concat(token($delimiter, $tags.len()))
											$tags = $tags.append($rendered_html)
										}
									}
									Err(_) => {
										$out = $out.concat(slice(line, $pos, end - $pos + 1))
									}
								}
							}
							$pos = end + 1
						}
					}
					Err(_) => {
						$out = $out.concat(drop(line, $pos))
						$pos = bytes.len()
					}
				}
			} else {
				var $end = $pos + 1
				while $end < bytes.len() and (bytes.get($end) ?? 0) != '{' and (bytes.get($end) ?? 0) != '\\' {
					$end = $end + 1
				}
				$out = $out.concat(slice(line, $pos, $end - $pos))
				$pos = $end
			}
		}
		if !$drop_line and !($drop_empty and $out.trim() == "") {
			$lines = $lines.append($out)
		}
	}
	# Return the delimiter alongside source by retaining one identity slot when
	# no quote stage supplied it. The caller restores this same tag registry.
	{ source: Str.join_with($lines, "\n"), context: $context, attributes: $attributes, removed: $removed, tag_prefix: $tag_prefix, footnotes: $footnotes, note_index: $note_index, indexed_note_count: $indexed_note_count, tags: $tags, prefix: $delimiter }
}

normalize_number : Str -> Try(Str, [Missing])
normalize_number = |source| {
	text = source.trim()
	negative = text.starts_with("-")
	digits = if (negative or text.starts_with("+")) drop(text, 1) else text
	if digits == "" or !digits.to_utf8().all(|b| b >= '0' and b <= '9') {
		return Err(Missing)
	}
	var $start = 0
	while $start + 1 < digits.to_utf8().len() and (digits.to_utf8().get($start) ?? 0) == '0' {
		$start = $start + 1
	}
	number = drop(digits, $start)
	Ok(if (negative and number != "0") "-${number}" else number)
}

successor : Str -> Str
successor = |source| {
	match normalize_number(source) {
		Ok(number) if number == source => {
			negative = number.starts_with("-")
			var $digits = (if (negative) drop(number, 1) else number).to_utf8()
			var $pos = $digits.len()
			var $carry = True
			while $pos > 0 and $carry {
				$pos = $pos - 1
				b = $digits.get($pos) ?? '0'
				value = if negative {
					if (b == '0') '9' else b - 1
				} else if (b == '9') '0' else b + 1
				$carry = if (negative) b == '0' else b == '9'
				$digits = $digits.set($pos, value) ?? $digits
			}
			if negative {
				normalize_number("-${Str.from_utf8_lossy($digits)}") ?? "0"
			}
			else {
				if ($carry) "1${Str.from_utf8_lossy($digits)}" else Str.from_utf8_lossy($digits)
			}
		}
		_ => string_successor(source)
	}
}

string_successor : Str -> Str
string_successor = |source| {
	var $bytes = source.to_utf8()
	if $bytes.is_empty() {
		return ""
	}
	var $pos = $bytes.len()
	var $carry = True
	var $first = $bytes.len()
	var $prefix = '1'
	while $pos > 0 and $carry {
		$pos = $pos - 1
		b = $bytes.get($pos) ?? 0
		if (b >= '0' and b <= '9') or (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z') {
			$first = $pos
			$prefix = if (b >= 'a' and b <= 'z') 'a' else if (b >= 'A' and b <= 'Z') 'A' else '1'
			wrap = b == '9' or b == 'z' or b == 'Z'
			replacement = if (b == '9') '0' else if (b == 'z') 'a' else if (b == 'Z') 'A' else b + 1
			$bytes = $bytes.set($pos, replacement) ?? $bytes
			$carry = wrap
		}
	}
	if $first == $bytes.len() {
		last = ($bytes.last() ?? 0)
		$bytes = $bytes.set($bytes.len() - 1, if (last == 255) 0 else last + 1) ?? $bytes
	} else if $carry {
		$bytes = $bytes.take_first($first).append($prefix).concat($bytes.drop_first($first))
	}
	Str.from_utf8_lossy($bytes)
}

expect AsciiDocInline.render("{counter:n} {counter:n} {counter:n}", []) == "1 2 3"

expect AsciiDocInline.render("{counter:n:5} {counter:n} {counter:n:10} {counter:n}", []) == "5 6 7 8"

expect AsciiDocInline.render("{counter:n:A} {counter:n} {counter:n}", []) == "A B C"

expect AsciiDocInline.render("{counter:n:Z} {counter:n} {counter:n}", []) == "Z AA AB"

expect AsciiDocInline.render("{counter:n:a} {counter:n} {counter:n}", []) == "a b c"

expect AsciiDocInline.render("{counter:n:z} {counter:n} {counter:n}", []) == "z aa ab"

expect AsciiDocInline.render("{counter:n:foo} {counter:n}", []) == "foo fop"

expect AsciiDocInline.render("{counter:n:-2} {counter:n}", []) == "-2 -1"

expect AsciiDocInline.render("{counter:n:1.5} {counter:n}", []) == "1.5 1.6"

expect AsciiDocInline.render("before{counter2:n}after {counter:n}", []) == "beforeafter 2"

expect AsciiDocInline.render("{set:name:}{name}after", []) == "after"

expect AsciiDocInline.render("{set:x:*bold*}{x}", []) == "*bold*"

expect AsciiDocInline.render("{set:x:**bold**}{x}", []) == "&lt;strong&gt;bold&lt;/strong&gt;"

expect AsciiDocInline.render("{set:x:<b> &}{x}", []) == "&lt;b&gt; &amp;"

expect AsciiDocInline.render("{set:a:{b}}{a}", []) == "}{b"

expect AsciiDocInline.render("{set:n:4}{counter:n}", []) == "5"

expect AsciiDocInline.render("\\{counter:n} {counter:n}", []) == "{counter:n} 1"

expect AsciiDocInline.render("{counter:n:999999999999999999999} {counter:n}", []) == "999999999999999999999 1000000000000000000000"

expect AsciiDocInline.render("{counter:n:-999999999999999999999} {counter:n}", []) == "-999999999999999999999 -999999999999999999998"

expect AsciiDocInline.render("{counter:n:005} {counter:n}", []) == "005 006"

expect AsciiDocInline.render("{counter:n:9.9} {counter:n}", []) == "9.9 10.0"

expect AsciiDocInline.render("{set:x:footnote:[Note.]}{x}", []) == "<sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

expect AsciiDocInline.render("A footnote:[Note {counter:n}.] and {n}", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup> and 1"

expect AsciiDocInline.render("*{counter:n}* and {counter:n}", []) == "<strong>1</strong> and 2"

expect AsciiDocInline.render("{counter2:n}\nthen {counter:n}", []) == "then 2"

expect AsciiDocInline.render("{set:x!}\nNext {x}", []) == "Next {x}"

expect {
	first = AsciiDocInline.render_state("{counter:n} {counter:n}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{counter:n} {n}", [], { subs: "normal" }, first.state)
	first.html == "1 2" and second.html == "3 3"
}
expect {
	first = AsciiDocInline.render_state("{counter:n}", [{ name: "n", value: "1" }, { name: "__revision:n", value: "1" }], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{counter:n}", [{ name: "n", value: "1" }, { name: "__revision:n", value: "2" }], { subs: "normal" }, first.state)
	first.html == "2" and second.html == "2"
}
expect {
	first = AsciiDocInline.render_state("{set:x:new}{x}", [{ name: "x", value: "old" }], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{x}", [{ name: "x", value: "old" }], { subs: "normal" }, first.state)
	third = AsciiDocInline.render_state("{set:x!}", [{ name: "x", value: "old" }], { subs: "normal" }, second.state)
	fourth = AsciiDocInline.render_state("{x}", [{ name: "x", value: "old" }], { subs: "normal" }, third.state)
	first.html == "new" and second.html == "new" and third.html == "" and fourth.html == "{x}"
}
expect AsciiDocInline.render_state("<<custom>>", [], { subs: "normal" }, { ..AsciiDocInline.initial_state, refs: [{ id: "custom", text: "A *bold* title" }] }).html == "<a href=\"#custom\">A <strong>bold</strong> title</a>"

needs_substitutions : Str, Groups -> Bool
needs_substitutions = |source, g| {
	if g.special and (source.contains("&") or source.contains("<") or source.contains(">")) {
		return True
	}
	if g.quotes and (source.contains("*") or source.contains("_") or source.contains("`") or source.contains("#") or source.contains("^") or source.contains("~")) {
		return True
	}
	if g.attributes and source.contains("{") {
		return True
	}
	if g.macros and (source.contains(":") or source.contains("@") or source.contains("+") or source.contains("[") or source.contains("$")) {
		return True
	}
	if g.macros and ((source.contains("((") and source.contains("))")) or source.contains("<<")) {
		return True
	}
	if g.replacements and (source.contains("'") or source.contains("&amp;") or source.contains("...") or source.contains("->") or source.contains("<-") or source.contains("=>") or source.contains("<=") or source.contains("(C)") or source.contains("(R)") or source.contains("(TM)") or (source.contains("--") and (source.contains("\\--") or has_dash_replacement(source.to_utf8())))) {
		return True
	}
	if (g.quotes or g.attributes or g.macros) and source.contains("\\") {
		return True
	}
	g.post and source.contains(" +")
}

possible_pass : List(U8), U64, Bool -> Bool
possible_pass = |bytes, pos, single| {
	b = bytes.get(pos) ?? 0
	if b == '+' or b == '[' or b == '\\' {
		return True
	}
	if single {
		return False
	}
	if b == '$' {
		return True
	}
	if b == 'p' {
		return (bytes.get(pos + 1) ?? 0) == 'a' and (bytes.get(pos + 2) ?? 0) == 's' and (bytes.get(pos + 3) ?? 0) == 's' and (bytes.get(pos + 4) ?? 0) == ':'
	}
	if b == 's' {
		return (bytes.get(pos + 1) ?? 0) == 't' and (bytes.get(pos + 2) ?? 0) == 'e' and (bytes.get(pos + 3) ?? 0) == 'm' and (bytes.get(pos + 4) ?? 0) == ':'
	}
	if b == 'l' {
		return bytes.drop_first(pos).starts_with([108, 97, 116, 101, 120, 109, 97, 116, 104, 58])
	}
	if b == 'a' {
		return bytes.drop_first(pos).starts_with([97, 115, 99, 105, 105, 109, 97, 116, 104, 58])
	}
	False
}

# Source suffix strings are materialized only at actual substitution sites.
# Most input is copied in a single contiguous UTF-8 slice.
syntax_candidate : List(U8), U64, Groups, Bool, Bool -> Bool
syntax_candidate = |bytes, pos, g, has_email, has_colon| {
	b = bytes.get(pos) ?? 0
	next = bytes.get(pos + 1) ?? 0
	prev = if (pos == 0) 0 else bytes.get(pos - 1) ?? 0
	if b == '\\' and (g.macros or g.attributes or g.quotes or (g.replacements and (next == '-' or next == '.'))) {
		return True
	}
	if g.special and (b == '&' or b == '<' or b == '>') {
		return True
	}
	if g.attributes and b == '{' {
		return True
	}
	if g.post and b == ' ' and next == '+' {
		return True
	}
	if g.replacements {
		if (b == '&' and bytes.drop_first(pos).starts_with("&amp;".to_utf8())) or (b == '(' and (bytes.drop_first(pos).starts_with("(C)".to_utf8()) or bytes.drop_first(pos).starts_with("(R)".to_utf8()) or bytes.drop_first(pos).starts_with("(TM)".to_utf8()))) {
			return True
		}
		if (b == '.' and next == '.' and (bytes.get(pos + 2) ?? 0) == '.') or (b == '-' and (next == '-' or next == '>')) or (b == '<' and (next == '-' or next == '=')) or (b == '=' and next == '>') or ((b == ' ' or b == '\n') and next == '-' and (bytes.get(pos + 2) ?? 0) == '-') or (b == '\'' and word(prev) and word(next)) {
			return True
		}
	}
	if g.macros {
		if (b == '&' and bytes.drop_first(pos).starts_with([38, 108, 116, 59, 38, 108, 116, 59])) or (b == '<' and next == '<') or (b == '[' and next == '[') or (b == '(' and next == '(') or b == '+' or b == '$' {
			return True
		}
		if has_colon and macro_name_at(bytes, pos) {
			return True
		}
		if has_email and word(b) and !word(prev) {
			var $end = pos
			var $at = False
			while $end < bytes.len() {
				c = bytes.get($end) ?? 0
				if !(word(c) or c == '@' or c == '.' or c == '-' or c == '+') {
					break
				}
				if c == '@' {
					$at = True
				}
				$end = $end + 1
			}
			if $at {
				return True
			}
		}
	}
	False
}

expect AsciiDocInline.render_with("Text *raw* pass:q[*bold*] stem:[x^2].", [], { subs: "macros" }) == "Text *raw* <strong>bold</strong> \\$x^2\\$."

QuoteResult : { source : Str, prefix : Str, tags : List(Str), deferred : List({ slot : U64, source : Str, subs : Str }) }

# Pass substitutions are restored after the enclosing text's attribute and
# macro stages. They share the same counters and footnote registry.
finish_render : Str, List({ name : Str, value : Str }), AsciiDocInline.State, QuoteResult, U64, SourceHint -> { html : Str, state : AsciiDocInline.State }
finish_render = |html, attrs, state, quoted, notes_start, hint| {
	var $state = state
	var $tags = quoted.tags
	for pass in quoted.deferred {
		rendered = render_dispatch(pass.source, attrs, { subs: pass.subs }, $state, hint)
		$state = rendered.state
		$tags = $tags.set(pass.slot, rendered.html) ?? $tags
	}
	var $notes = $state.footnotes
	$state = { ..$state, footnotes: [] }
	var $index = notes_start
	while $index < $notes.len() {
		match $notes.get($index) {
			Ok(note) => {
				if quoted.prefix != "" and note.text.contains(quoted.prefix) {
					# The loop bounds guarantee a valid index. Do not retain
					# an alias of the old list for an unreachable fallback.
					$notes = $notes.set($index, { name: note.name, text: restore_tags(note.text, quoted.prefix, $tags) }) ?? []
				}
			}
			Err(_) => {}
		}
		$index = $index + 1
	}
	{ html: restore_tags(html, quoted.prefix, $tags), state: { ..$state, footnotes: $notes } }
}

deferred_pass : Str -> Try({ source : Str, subs : Str, used : U64 }, [Missing])
deferred_pass = |source| deferred_pass_cached(source, 0, empty_bracket_cursor).result

deferred_pass_cached : Str, U64, BracketCursor -> { result : Try({ source : Str, subs : Str, used : U64 }, [Missing]), cursor : BracketCursor }
deferred_pass_cached = |source, base, cursor| {
	if !source.starts_with("pass:") {
		return { result: Err(Missing), cursor }
	}
	match find(source, "[", 5) {
		Ok(open) => {
			subs = slice(source, 5, open - 5)
			selected = groups(subs)
			# Pure quote/special-character passes are never deferred. Reject
			# before searching their body; pass_match retains the actual match.
			if !selected.attributes and !selected.macros {
				return { result: Err(Missing), cursor }
			}
			searched = cached_bracket_end(source, open, base, cursor)
			result = match searched.end {
				Ok(end) => Ok({ source: slice(source, open + 1, end - open - 1), subs, used: end + 1 })
				Err(_) => Err(Missing)
			}
			{ result, cursor: searched.cursor }
		}
		Err(_) => { result: Err(Missing), cursor }
	}
}

expect AsciiDocInline.render("A {scheme}://example.org link.", [{ name: "scheme", value: "https" }]) == "A <a href=\"https://example.org\" class=\"bare\">https://example.org</a> link."
expect AsciiDocInline.render("{scheme}://example.org[Link]", [{ name: "scheme", value: "https" }]) == "<a href=\"https://example.org\">Link</a>"
expect AsciiDocInline.render("A {x} end.", [{ name: "x", value: "++*text*++" }]) == "A ++*text*++ end."
expect AsciiDocInline.render("pass:a[{counter:n}] {counter:n}", []) == "2 1"
expect AsciiDocInline.render("{counter:n} pass:a[{counter:n}]", []) == "1 2"
expect AsciiDocInline.render("pass:a[{set:x:new}] {x}", []) == " {x}"
expect AsciiDocInline.render("pass:a[{set:x:new}] {x}", [{ name: "x", value: "old" }]) == " old"

first_bracket_end : Str, U64 -> Try(U64, [Missing])
first_bracket_end = |source, open| {
	bytes = source.to_utf8()
	var $pos = open + 1
	while $pos < bytes.len() {
		if (bytes.get($pos) ?? 0) == ']' and (bytes.get($pos - 1) ?? 0) != '\\' {
			return Ok($pos)
		}
		$pos = $pos + 1
	}
	Err(Missing)
}
expect AsciiDocInline.render("+pass:a[{counter:n}]+ {counter:n}", []) == "2 1"
expect AsciiDocInline.render("A pass:a[{counter:n}] +pass:a[{counter:n}]+ {counter:n}", []) == "A 2 3 1"
expect AsciiDocInline.render("pass:m[footnote:[A.]] footnote:[B.]", []) == "footnote:[A.] <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"

bounded_attribute : Str -> Str
bounded_attribute = |source| {
	if source.to_utf8().len() <= 4096 {
		return source
	}
	var $bytes = source.to_utf8().take_first(4096)
	while !$bytes.is_empty() {
		match Str.from_utf8($bytes) {
			Ok(value) => return value
			Err(_) => {
				$bytes = $bytes.take_first($bytes.len() - 1)
			}
		}
	}
	""
}

expect AsciiDocInline.attribute_value("*Roc* <b> & &copy;", []) == "*Roc* &lt;b&gt; &amp; &amp;copy;"
expect AsciiDocInline.attribute_value("pass:q[*Roc*]", []) == "<strong>Roc</strong>"
expect AsciiDocInline.attribute_value("pass:[<b>raw</b>]", []) == "<b>raw</b>"
expect AsciiDocInline.attribute_value("pass:a[{name}]", [{ name: "name", value: "Roc" }]) == "Roc"
expect AsciiDocInline.attribute_value("prefix {name}", [{ name: "name", value: "Roc" }]) == "prefix Roc"
expect AsciiDocInline.render_with("&copy; &#123; <b>", [], { subs: "specialcharacters" }) == "&amp;copy; &amp;#123; &lt;b&gt;"

expect AsciiDocInline.render("image::/cat.png[]*", []) == "image::/cat.png[]*"
expect AsciiDocInline.render("image::cat.png\nimage::cat.png[]", []) == "image::cat.png\nimage::cat.png[]"
expect AsciiDocInline.render("image::cat.png[alt=Cat,wi****\n=80]", []) == "image::cat.png[alt=Cat,wi*<strong>*</strong>\n=80]"

expect AsciiDocInline.attribute_value("${Str.join_with(List.repeat("a", 4095), "")}é", []).to_utf8().len() == 4095

expect AsciiDocInline.render("{src}/docs/a.roc[Source]", [{ name: "src", value: "https://example.org/repo" }]) == "<a href=\"https://example.org/repo/docs/a.roc\">Source</a>"
expect AsciiDocInline.render("link:{src}/docs/a.roc[Source]", [{ name: "src", value: "https://example.org/repo" }]) == "<a href=\"https://example.org/repo/docs/a.roc\">Source</a>"
expect AsciiDocInline.render("`http://` and `https://`.", []) == "<code>http://</code> and <code>https://</code>."
expect AsciiDocInline.render("`https://example.org`.", []) == "<code><a href=\"https://example.org\" class=\"bare\">https://example.org</a></code>."

expect AsciiDocInline.render("`\\\\`", []) == "<code>\\\\</code>"
expect AsciiDocInline.render("Hello {NAM {NAME}.", [{ name: "name", value: "Roc" }]) == "Hello {NAM Roc."
expect AsciiDocInline.render("Test _____ end.", []) == "Test <em>_</em> end."
expect AsciiDocInline.render("pass:a[{set:x:new}] { {x}", [{ name: "x", value: "old" }]) == " { old"

find_bytes : List(U8), List(U8), U64 -> Try(U64, [Missing])
find_bytes = |bytes, needle, start| {
	if start > bytes.len() or needle.len() > bytes.len() - start {
		return Err(Missing)
	}
	if needle.is_empty() {
		return Ok(start)
	}
	first = needle.get(0) ?? 0
	last = bytes.len() - needle.len()
	var $pos = start
	while $pos <= last {
		if (bytes.get($pos) ?? 0) == first {
			var $matched = 1.U64
			while $matched < needle.len() and bytes.get($pos + $matched) == needle.get($matched) {
				$matched = $matched + 1
			}
			if $matched == needle.len() {
				return Ok($pos)
			}
		}
		$pos = $pos + 1
	}
	Err(Missing)
}

expect find("abc", "", 0) == Ok(0)
expect find("abc", "", 3) == Ok(3)
expect find("abc", "", 4) == Err(Missing)
expect find("abc", "bc", 1) == Ok(1)
expect find("abc", "bc", 2) == Err(Missing)
expect find("aaabaaab", "aab", 0) == Ok(1)
expect find("aaabaaab", "aab", 2) == Ok(5)
expect find("café café", "é", 4) == Ok(9)
expect find("abc", "abcd", 0) == Err(Missing)

# Inspect only the few bytes of a possible macro name. Avoid allocating a
# byte list for every macro spelling at every ordinary character position.
macro_name_at : List(U8), U64 -> Bool
macro_name_at = |bytes, pos| {
	first = bytes.get(pos) ?? 0
	second = bytes.get(pos + 1) ?? 0
	rest = bytes.drop_first(pos)
	match first {
		'h' if second == 't' => rest.starts_with([104, 116, 116, 112, 58, 47, 47]) or rest.starts_with([104, 116, 116, 112, 115, 58, 47, 47])
		'f' if second == 't' or second == 'o' => rest.starts_with([102, 116, 112, 58, 47, 47]) or rest.starts_with([102, 111, 111, 116, 110, 111, 116, 101, 58]) or rest.starts_with([102, 111, 111, 116, 110, 111, 116, 101, 114, 101, 102, 58])
		'i' if second == 'r' or second == 'm' or second == 'n' or second == 'c' => rest.starts_with([105, 114, 99, 58, 47, 47]) or rest.starts_with([105, 109, 97, 103, 101, 58]) or rest.starts_with([105, 110, 100, 101, 120, 116, 101, 114, 109, 58]) or rest.starts_with([105, 110, 100, 101, 120, 116, 101, 114, 109, 50, 58]) or rest.starts_with([105, 99, 111, 110, 58])
		'l' if second == 'i' => rest.starts_with([108, 105, 110, 107, 58])
		'm' if second == 'a' or second == 'e' => rest.starts_with([109, 97, 105, 108, 116, 111, 58]) or rest.starts_with([109, 101, 110, 117, 58])
		'x' if second == 'r' => rest.starts_with([120, 114, 101, 102, 58])
		'a' if second == 'n' => rest.starts_with([97, 110, 99, 104, 111, 114, 58])
		'k' if second == 'b' => rest.starts_with([107, 98, 100, 58])
		'b' if second == 't' => rest.starts_with([98, 116, 110, 58])
		'p' if second == 'a' => rest.starts_with([112, 97, 115, 115, 58])
		_ => False
	}
}

has_dash_replacement : List(U8) -> Bool
has_dash_replacement = |bytes| {
	var $pos = 0
	while $pos + 1 < bytes.len() {
		if (bytes.get($pos) ?? 0) == '-' and (bytes.get($pos + 1) ?? 0) == '-' {
			before = if ($pos == 0) 0 else bytes.get($pos - 1) ?? 0
			after = bytes.get($pos + 2) ?? 0
			if (word(before) and word(after)) or (($pos == 0 or space(before)) and ($pos + 2 == bytes.len() or space(after))) {
				return True
			}
		}
		$pos = $pos + 1
	}
	False
}

fixed_replacement : List(U8), U64, Groups -> Macro
fixed_replacement = |bytes, pos, g| {
	none = { used: 0, html: "" }
	b = bytes.get(pos) ?? 0
	next = bytes.get(pos + 1) ?? 0
	third = bytes.get(pos + 2) ?? 0
	prev = if (pos == 0) 0 else bytes.get(pos - 1) ?? 0
	if g.post and b == ' ' and next == '+' {
		if third == '\n' {
			return { used: 3, html: "<br>\n" }
		}
		if pos + 2 == bytes.len() {
			return { used: 2, html: "<br>" }
		}
	}
	if !g.replacements {
		return none
	}
	if b == '\\' and next == '.' and third == '.' and (bytes.get(pos + 3) ?? 0) == '.' {
		return { used: 4, html: "..." }
	}
	if b == '\\' and next == '-' and third == '-' and (bytes.get(pos + 3) ?? 0) != '-' {
		return { used: 3, html: "--" }
	}
	if b == '.' and next == '.' and third == '.' {
		return { used: 3, html: "&#8230;&#8203;" }
	}
	if b == '-' and next == '>' {
		return { used: 2, html: "&#8594;" }
	}
	if b == '<' and next == '-' {
		return { used: 2, html: "&#8592;" }
	}
	if b == '=' and next == '>' {
		return { used: 2, html: "&#8658;" }
	}
	if b == '<' and next == '=' {
		return { used: 2, html: "&#8656;" }
	}
	if b == '\'' and word(prev) and word(next) {
		return { used: 1, html: "&#8217;" }
	}
	if b == '(' and third == ')' {
		if next == 'C' {
			return { used: 3, html: "&#169;" }
		}
		if next == 'R' {
			return { used: 3, html: "&#174;" }
		}
	}
	if b == '(' and next == 'T' and third == 'M' and (bytes.get(pos + 3) ?? 0) == ')' {
		return { used: 4, html: "&#8482;" }
	}
	if b == '-' and next == '-' {
		if word(prev) and word(third) {
			return { used: 2, html: "&#8212;&#8203;" }
		}
		if pos == 0 and (space(third) or pos + 2 == bytes.len()) {
			return { used: if (pos + 2 == bytes.len()) 2 else 3, html: "&#8201;&#8212;&#8201;" }
		}
	}
	if (b == ' ' or b == '\n') and next == '-' and third == '-' {
		after = bytes.get(pos + 3) ?? 0
		if space(after) or pos + 3 == bytes.len() {
			return { used: if (pos + 3 == bytes.len()) 3 else 4, html: "&#8201;&#8212;&#8201;" }
		}
	}
	none
}

# Retained mutation expectations remain active across the scanner optimization.
expect AsciiDocInline.render("Test !+++", []) == "Test !+"
expect AsciiDocInline.render("Test [.red]+++<b>+++<b>+++.", []) == "Test <span class=\"red\"><b></span>&lt;b&gt;+."
expect AsciiDocInline.render("A footnote:[A note. footnote:[A note.]", []) == "A <sup class=\"footnote\">[<a id=\"_footnoteref_1\" class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
expect AsciiDocInline.render("Test (+. ++x_y+++!.", []) == "Test (. x_y!."
expect AsciiDocInline.render("See <https://examp====\n.", []) == "See &lt;https://examp====\n."
expect AsciiDocInline.render("Test !**x *bold* [[target]]\ny**_.", []) == "Test !<strong>x <strong>bold</strong> <a id=\"target\"></a>\ny</strong>_."
expect AsciiDocInline.render("Test \\++alpha+", []) == "Test ++alpha+"
expect AsciiDocInline.render("One +++<b>raw<e +++<b>raw</b>+++ end.", []) == "One <b>raw<e &lt;b&gt;raw&lt;/b&gt;+ end."
expect AsciiDocInline.render("B*bold*Definition", []) == "B*bold*Definition"
expect AsciiDocInline.render("****", []) == "<strong>*</strong>*"

expect AsciiDocInline.render("--", []) == "&#8201;&#8212;&#8201;"
expect AsciiDocInline.render("----", []) == "----"
expect AsciiDocInline.render("a\n--\nb", []) == "a&#8201;&#8212;&#8201;b"
expect AsciiDocInline.render("Prefix ((((((( suffix", []) == "Prefix ((((((( suffix"

quote_closers : List(U8), QuoteStage, List(U8) -> List(U64)
quote_closers = |bytes, stage, closing| {
	var $positions = []
	var $pos = 0
	first = closing.first() ?? 0
	while $pos + closing.len() <= bytes.len() {
		if (bytes.get($pos) ?? 0) == first and bytes.drop_first($pos).starts_with(closing) {
			after = bytes.get($pos + closing.len()) ?? 0
			before = if ($pos == 0) 0 else bytes.get($pos - 1) ?? 0
			if stage.unconstrained or (!space(before) and !word_at(bytes, $pos + closing.len()) and !(stage.tag == "code" and (after == '"' or after == '\'' or after == '`'))) {
				$positions = $positions.append($pos)
			}
		}
		$pos = $pos + 1
	}
	$positions
}

# Ruby's Unicode Word class includes letters, marks, numbers and connector
# punctuation. Emoji and other symbols form valid constrained-span boundaries.
word_at : List(U8), U64 -> Bool
word_at = |bytes, pos| {
	if pos >= bytes.len() {
		return False
	}
	first = bytes.get(pos) ?? 0
	if first < 128 {
		return word(first)
	}
	width = if (first < 224) 2 else if (first < 240) 3 else 4
	var $scalar = if (width == 2) first.to_u32() - 192 else if (width == 3) first.to_u32() - 224 else first.to_u32() - 240
	var $offset = 1
	while $offset < width {
		$scalar = $scalar * 64 + ((bytes.get(pos + $offset) ?? 0).to_u32() % 64)
		$offset = $offset + 1
	}
	match unicode_category($scalar) {
		Ok(Lu) | Ok(Ll) | Ok(Lt) | Ok(Lm) | Ok(Lo) | Ok(Mn) | Ok(Mc) | Ok(Me) | Ok(Nd) | Ok(Nl) | Ok(No) | Ok(Pc) => True
		_ => False
	}
}

word_before : List(U8), U64 -> Bool
word_before = |bytes, pos| {
	if pos == 0 {
		return False
	}
	var $start = pos - 1
	b = bytes.get($start) ?? 0
	if b < 128 {
		return word(b)
	}
	while $start > 0 and pos - $start < 4 and (bytes.get($start) ?? 0) >= 128 and (bytes.get($start) ?? 0) < 192 {
		$start = $start - 1
	}
	word_at(bytes, $start)
}

expect AsciiDocInline.render("Test (😀+a & b+.", []) == "Test (😀a &amp; b."
expect AsciiDocInline.render("😀*bold*😀", []) == "😀<strong>bold</strong>😀"
expect AsciiDocInline.render("α*bold*β", []) == "α*bold*β"
expect AsciiDocInline.render("😀_em_😀", []) == "😀<em>em</em>😀"

unicode_category : U32 -> Try(GeneralCategory.Value, [InvalidScalar])
unicode_category = |scalar| Scalar.from_u32(scalar).map_ok(GeneralCategory.of_scalar)

normal_stages : List(Str)
normal_stages = ["specialcharacters", "quotes", "attributes", "replacements", "macros", "post_replacements"]

substitution_stages : Str -> List(Str)
substitution_stages = |spec| {
	var $stages = []
	var $started = False
	for raw in spec.split_on(",") {
		name = repl(raw.trim(), " ", "")
		append = name.starts_with("+")
		remove = name.starts_with("-")
		prepend = !append and !remove and name.ends_with("+")
		key = if (append or remove) drop(name, 1) else if (prepend) take(name, name.to_utf8().len() - 1) else name
		if !$started {
			$stages = if (append or remove or prepend) normal_stages else []
			$started = True
		}
		resolved = match key {
			"normal" | "n" => normal_stages
			"none" => []
			"verbatim" | "v" | "specialcharacters" | "specialchars" | "c" => ["specialcharacters"]
			"quotes" | "q" => ["quotes"]
			"attributes" | "a" => ["attributes"]
			"replacements" | "r" => ["replacements"]
			"macros" | "m" => ["macros"]
			"post_replacements" | "p" => ["post_replacements"]
			_ => []
		}
		$stages = if (remove) $stages.keep_if(|stage| !resolved.contains(stage)) else if (prepend) resolved.concat($stages) else $stages.concat(resolved)
	}
	var $unique = []
	for stage in $stages {
		if !$unique.contains(stage) {
			$unique = $unique.append(stage)
		}
	}
	$unique
}

render_ordered_indexed : Str, List({ name : Str, value : Str }), List(Str), AsciiDocInline.State, SourceHint -> { html : Str, state : AsciiDocInline.State }
render_ordered_indexed = |input, attrs, stages, state, hint| {
	extract_passes = stages.contains("macros") and quote_needed(input, { ..empty, macros: True })
	{ snapshot_before: incoming_before, snapshot_after: incoming_after, trusted_snapshot_generation, trusted_snapshot_token, dynamic_tags_possible, attributes: incoming_attributes, removed: incoming_removed, tag_prefix, footnotes, note_index, indexed_note_count, emitted_footnotes, refs, ref_index: incoming_refs, indexed_ref_count, snapshot: incoming_snapshot, snapshot_index: incoming_snapshot_index } = state
	{ attributes, removed, snapshot, snapshot_index: cached_index, snapshot_before, snapshot_after } = refresh_hint(attrs, incoming_attributes, incoming_removed, incoming_snapshot, incoming_snapshot_index, incoming_before, incoming_after, trusted_snapshot_token, trusted_snapshot_generation, hint)
	snapshot_index = if (cached_index.is_empty() and extract_passes) AsciiDocSource.from_dict(source_attribute_index(attrs)) else cached_index
	ref_index = if (indexed_ref_count == refs.len()) incoming_refs else refs.fold(Dict.empty(), |found, ref| if (found.contains(ref.id)) found else found.insert(ref.id, ref.text))
	context = { snapshot_before, snapshot_after, source_prefix: hint_prefix(hint), trusted_snapshot_generation: hint_generation(hint), trusted_snapshot_token: hint_token(hint), dynamic_tags_possible: dynamic_tags_possible and !attributes.is_empty(), emitted_footnotes, refs, ref_index, indexed_ref_count: refs.len(), snapshot, snapshot_index }
	prior_notes = footnotes.len()
	macro_groups = { ..empty, macros: True, ref_index }
	extracted = if (extract_passes) quote_owned(input, attrs, snapshot_index, hint, dynamic_tags_possible, macro_groups, attributes, removed, tag_prefix) else { source: input, prefix: "", tags: [], deferred: [] }
	var $text = extracted.source
	var $state = assemble_state(context, attributes, removed, tag_prefix, footnotes, note_index, indexed_note_count)
	for stage in stages {
		{ html, state: next } = render_state_indexed($text, attrs, { subs: stage }, $state, True, hint)
		$text = html
		$state = next
	}
	finish_render($text, attrs, $state, extracted, prior_notes, hint)
}

expect AsciiDocInline.render_with("*<b>*", [], { subs: "quotes,specialcharacters" }) == "&lt;strong&gt;&lt;b&gt;&lt;/strong&gt;"
expect AsciiDocInline.render_with("...", [], { subs: "replacements,specialcharacters" }) == "&amp;#8230;&amp;#8203;"
expect AsciiDocInline.render_with("*word*", [], { subs: "quotes,quotes" }) == "<strong>word</strong>"
expect AsciiDocInline.render_with("*word*", [], { subs: "quotes,-quotes" }) == "*word*"
expect AsciiDocInline.render_with("*word*", [], { subs: "none,+quotes" }) == "<strong>word</strong>"
expect AsciiDocInline.render("*a**b*", []) == "<strong>a</strong>*b*"
expect AsciiDocInline.render("#a##b#", []) == "<mark>a</mark>#b#"
expect AsciiDocInline.render("Test \\##x_y##._y##.", []) == "Test <mark>#x_y</mark>#._y##."
expect AsciiDocInline.render("Test \\[source,roc]\n`α β``.", []) == "Test \\[source,roc]\n<code>α β`</code>."
expect AsciiDocInline.render("Visit https://example.org[Example,endif::[]\nrole=external].", []) == "Visit <a href=\"https://example.org\">Example,endif::[</a>\nrole=external]."
expect AsciiDocInline.render("[[first\nsecond]]", []) == "[[first\nsecond]]"
expect AsciiDocInline.render("\\[plain]", []) == "\\[plain]"
expect AsciiDocInline.render("\\<b>", []) == "\\&lt;b&gt;"

named_item : Str -> Bool
named_item = |part| {
	text = part.trim()
	!text.starts_with("\"") and !text.starts_with("'") and text.contains("=")
}

unquote_attribute : Str -> Str
unquote_attribute = |text| {
	n = text.to_utf8().len()
	if n >= 2 and ((text.starts_with("\"") and text.ends_with("\"")) or (text.starts_with("'") and text.ends_with("'"))) {
		repl(repl(slice(text, 1, n - 2), "\\\"", "\""), "\\'", "'")
	} else text
}

html_attribute : Str -> Str
html_attribute = |text| repl(escape_entities(text), "\"", "&quot;")

link_attributes : List(Str), Bool, Bool -> Str
link_attributes = |parts, bare, new_window| {
	id = named(parts, "id") ?? ""
	role = named(parts, "role") ?? ""
	title = named(parts, "title") ?? ""
	classes = if (bare and role != "") "bare ${role}" else if (bare) "bare" else role
	window = named(parts, "window") ?? if (new_window) "_blank" else ""
	"${if (id == "") "" else " id=\"${html_attribute(id)}\""}${if (classes == "") "" else " class=\"${html_attribute(classes)}\""}${if (title == "") "" else " title=\"${html_attribute(title)}\""}${if (window == "") "" else " target=\"${html_attribute(window)}\"${if (window == "_blank") " rel=\"noopener\"" else ""}"}"
}

uri_component : Str -> Str
uri_component = |source| {
	hex = "0123456789ABCDEF".to_utf8()
	var $out = []
	for byte in source.to_utf8() {
		if (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z') or (byte >= '0' and byte <= '9') or byte == '-' or byte == '_' or byte == '.' or byte == '~' {
			$out = $out.append(byte)
		} else {
			$out = $out.append('%').append(hex.get((byte // 16).to_u64()) ?? 0).append(hex.get((byte % 16).to_u64()) ?? 0)
		}
	}
	Str.from_utf8_lossy($out)
}

expect AsciiDocInline.render("https://example.test[role=external]", []) == "<a href=\"https://example.test\" class=\"bare external\">https://example.test</a>"

expect AsciiDocInline.render("link:doc.html[id=topic]", []) == "<a href=\"doc.html\" id=\"topic\" class=\"bare\">doc.html</a>"

expect AsciiDocInline.render("https://example.test[\"a=b\"]", []) == "<a href=\"https://example.test\">a=b</a>"

expect AsciiDocInline.render("https://example.test[A, B]", []) == "<a href=\"https://example.test\">A, B</a>"

expect AsciiDocInline.render("https://example.test[A, B,role=x]", []) == "<a href=\"https://example.test\" class=\"x\">A</a>"

expect AsciiDocInline.render("image:cat.png[alt=\"A, B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A, B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt='A, B']", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A, B\"></span>"

expect AsciiDocInline.render("image:cat.png[\"A & B\",20,30]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\" width=\"20\" height=\"30\"></span>"

expect AsciiDocInline.render("https://example.test[Example,role='red big']", []) == "<a href=\"https://example.test\" class=\"red big\">Example</a>"

expect AsciiDocInline.render("https://example.test[Example,id=topic,role=x]", []) == "<a href=\"https://example.test\" id=\"topic\" class=\"x\">Example</a>"

expect AsciiDocInline.render("mailto:a@example.test[Name,Subject,Body]", []) == "<a href=\"mailto:a@example.test?subject=Subject&amp;body=Body\">Name</a>"

expect AsciiDocInline.render("link:doc.html[]", []) == "<a href=\"doc.html\" class=\"bare\">doc.html</a>"

expect AsciiDocInline.render("https://example.test[window=\"_blank\"]", []) == "<a href=\"https://example.test\" class=\"bare\" target=\"_blank\" rel=\"noopener\">https://example.test</a>"

expect AsciiDocInline.render("https://example.test[Example,window=_self]", []) == "<a href=\"https://example.test\" target=\"_self\">Example</a>"

expect AsciiDocInline.render("https://example.test[role=x,Label]", []) == "<a href=\"https://example.test\" class=\"bare x\">https://example.test</a>"

expect AsciiDocInline.render("https://example.test[\"a,b=c\",role=x]", []) == "<a href=\"https://example.test\" class=\"x\">a,b=c</a>"

expect AsciiDocInline.render("image:cat.png[\"A \\\"quote\\\" B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &quot;quote&quot; B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt='A \"quote\" B']", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &quot;quote&quot; B\"></span>"

expect AsciiDocInline.render("image:cat.png[alt=Pic,20,30]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"Pic\" width=\"20\" height=\"30\"></span>"

expect AsciiDocInline.render("mailto:a@example.test[Name,A topic,Line one & two]", []) == "<a href=\"mailto:a@example.test?subject=A%20topic&amp;body=Line%20one%20%26amp%3B%20two\">Name</a>"

expect AsciiDocInline.render("mailto:a@example.test[\"Full name\",\"Roc & λ\",\"Body + more\"]", []) == "<a href=\"mailto:a@example.test?subject=Roc%20%26amp%3B%20%CE%BB&amp;body=Body%20%2B%20more\">Full name</a>"

expect AsciiDocInline.render("mailto:a@example.test[role=x]", []) == "<a href=\"mailto:a@example.test\">role=x</a>"

expect AsciiDocInline.render("mailto:a@example.test[Name,id=mail,role=contact,window=_blank]", []) == "<a href=\"mailto:a@example.test\" id=\"mail\" class=\"contact\" target=\"_blank\" rel=\"noopener\">Name</a>"

expect AsciiDocInline.render("xref:topic[role=ref]", []) == "<a href=\"#topic\" class=\"ref\">[topic]</a>"

expect AsciiDocInline.render("xref:topic[Text,id=ref,role=ref]", []) == "<a href=\"#topic\" class=\"ref\">Text</a>"

expect AsciiDocInline.render("https://example.test[Example,title=Help]", []) == "<a href=\"https://example.test\" title=\"Help\">Example</a>"

expect AsciiDocInline.render("image:cat.png[Cat,title=\"Helpful image\",role=thumb,id=photo]", []) == "<span class=\"image thumb\"><img src=\"cat.png\" alt=\"Cat\" title=\"Helpful image\"></span>"

tag_attributes : Str, List(Str) -> List({ name : Str, value : Str })
tag_attributes = |prefix, tags| {
	var $index = 0
	var $attrs = []
	for value in tags {
		$attrs = $attrs.append({ name: "__inline-tag:${token(prefix, $index)}", value })
		$index = $index + 1
	}
	$attrs
}

restore_view_tags : Str, AttributeView -> Str
restore_view_tags = |input, attrs| {
	if attrs.dynamic_tags_possible {
		# Retain exact Dict iteration order for arbitrary user replacements.
		return restore_tag_attributes(input, materialize_view(attrs))
	}
	# Ordinary counters cannot add reserved tag bindings. Avoid enumerating
	# their entire growing dictionary for every mailto subject/body.
	tags = [attrs.source_before, (if (AsciiDocSource.is_persistent(attrs.source_index)) AsciiDocSource.tag_attributes(attrs.source_index) else attrs.source), attrs.source_after].fold([], |found, part| found.concat(part.keep_if(|attr| attr.name.starts_with("__inline-tag:") and !attrs.removed.contains(attr.name) and !attrs.values.contains(attr.name)))).concat(attrs.appended)
	restore_tag_attributes(input, tags)
}

restore_tag_attributes : Str, List({ name : Str, value : Str }) -> Str
restore_tag_attributes = |input, attrs| {
	var $text = input
	var $depth = 0
	while $depth < 64 {
		previous = $text
		for attr in attrs {
			if attr.name.starts_with("__inline-tag:") {
				$text = repl($text, drop(attr.name, 13), attr.value)
			}
		}
		if $text == previous {
			break
		}
		$depth = $depth + 1
	}
	$text
}

expect AsciiDocInline.render("mailto:a@example.test[Name,*bold* topic,_body_]", []) == "<a href=\"mailto:a@example.test?subject=%3Cstrong%3Ebold%3C%2Fstrong%3E%20topic&amp;body=%3Cem%3Ebody%3C%2Fem%3E\">Name</a>"

expect AsciiDocInline.render("image:cat.png[*Bold* alt]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"<strong>Bold</strong> alt\"></span>"

expect AsciiDocInline.render("https://example.test[Name,role=*red*]", []) == "<a href=\"https://example.test\" class=\"<strong>red</strong>\">Name</a>"

expect AsciiDocInline.render("image:cat.png[alt=\"A &amp; B\"]", []) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\"></span>"

expect AsciiDocInline.render_with("image:cat.png[A & B]", [], { subs: "specialcharacters,macros" }) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; B\"></span>"

reference_label : AttributeView, Groups, Str -> Try(Str, [Missing])
reference_label = |attrs, g, id| {
	match lookup_view(attrs, "ref:${id}") {
		Ok(text) => Ok(text)
		Err(_) => match g.ref_index.get(id) {
			Ok(text) => Ok(text)
			Err(_) => Err(Missing)
		}
	}
}
expect {
	refs = [{ id: "first", text: "First" }, { id: "second", text: "Second" }]
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, refs)
	first = AsciiDocInline.render_state("See <<second>>", [], { subs: "normal" }, state)
	next = AsciiDocInline.render_state("xref:first[]", [], { subs: "normal" }, first.state)
	first.html == "See <a href=\"#second\">Second</a>" and next.html == "<a href=\"#first\">First</a>"
}
expect {
	initial = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "same", text: "Old" }])
	replaced = AsciiDocInline.with_refs(initial, [{ id: "same", text: "New" }])
	AsciiDocInline.render_state("<<same>>", [], { subs: "normal" }, replaced).html == "<a href=\"#same\">New</a>"
}

expect {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "ref", text: "A *formatted* title" }])
	result = AsciiDocInline.render_state("xref:ref[]", [], { subs: "specialcharacters,macros" }, state)
	result.html == "<a href=\"#ref\">A <strong>formatted</strong> title</a>"
}
expect {
	state = AsciiDocInline.with_refs(AsciiDocInline.initial_state, [{ id: "ref", text: "Registry" }])
	AsciiDocInline.render_state("<<ref>>", [{ name: "ref:ref", value: "Explicit" }], { subs: "normal" }, state).html == "<a href=\"#ref\">Explicit</a>"
}

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exates\"]\n*strong* <raw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,post_replacements,macros,replacements,specialcharacters,attributes" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://exates\"\" class=\"bare\"&gt;https://exates\"&lt;/a&gt;]\n&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;label&lt;/strong&gt;&lt;/a&gt; *attribute*&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exampl|label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "attributes,replacements,macros,quotes,post_replacements,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://exampl|label*\" class=\"bare\"&gt;https://exampl|label*&lt;/a&gt;] &lt;strong&gt;attribute&lt;/strong&gt;&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://exaue} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,replacements,post_replacements,attributes,specialcharacters,macros" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; <a href=\"https://exaue}&lt;br&gt\" class=\"bare\">https://exaue}&lt;br&gt</a>;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("mailto:a@example.test[Name,", [], { subs: "normal" }) == "mailto:a@example.test[Name,"

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attri&lt;&lt;target&gt;&gt;nts,post_replacements\"]" }], { subs: "normal" }) == "<strong>strong</strong> &lt;raw&gt; &copy; &#8230;&#8203; <a href=\"https://example.test\"><strong>label</strong></a> *attri<a href=\"#target\">[target]</a>nts,post_replacements\"]<br>\nnext <strong>passed</strong>"

expect AsciiDocInline.render("mailto:a@example.test[Name,", []) == "mailto:a@example.test[Name,"

expect AsciiDocInline.render("https://example.test]", []) == "<a href=\"https://example.test\" class=\"bare\">https://example.test</a>]"

# Find a footnote's closing bracket over the original bytes. Only actual
# nested macros require a bounded substring; ordinary text never copies a
# source suffix or validates the remainder of the paragraph again.
footnote_end_view : Str, List(U8), U64, AttributeView, Groups -> Try(U64, [Missing])
footnote_end_view = |source, bytes, open, attrs, g| {
	var $pos = open + 1
	note = "footnote:".to_utf8()
	note_ref = "footnoteref:".to_utf8()
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '\\' and (bytes.get($pos + 1) ?? 0) == ']' {
			$pos = $pos + 2
		}
		else if b == ']' {
			return Ok($pos)
		}
		else {
			nested = if macro_name_at(bytes, $pos) and !bytes.drop_first($pos).starts_with(note) and !bytes.drop_first($pos).starts_with(note_ref) {
				var $end = $pos
				while $end < bytes.len() {
					c = bytes.get($end) ?? 0
					if c == '\\' and (bytes.get($end + 1) ?? 0) == ']' {
						$end = $end + 2
					}
					else if c == ']' {
						break
					}
					else {
						$end = $end + 1
					}
				}
				parse_macro_view(slice(source, $pos, $end - $pos + if ($end < bytes.len()) 1 else 0), attrs, g, 0).used
			} else 0
			$pos = $pos + if (nested > 0) nested else 1
		}
	}
	Err(Missing)
}

expect {
	first = AsciiDocInline.render_state("footnote:[literal \u(1)0\u(1)]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("*bold* footnote:[next]", [], { subs: "normal" }, first.state)
	(second.state.footnotes.first() ?? { name: "", text: "" }).text == "literal \u(1)0\u(1)"
}

expect {
	var $source = ""
	var $i = 0.U64
	while $i < 128 {
		$source = $source.concat("word footnote:[*note*] ")
		$i = $i + 1
	}
	result = AsciiDocInline.render_state($source, [], { subs: "normal" }, AsciiDocInline.initial_state)
	result.state.footnotes.len() == 128 and result.state.footnotes.all(|note| note.text == "<strong>note</strong>") and result.html.contains("_footnoteref_128")
}

path_has_extension : Str -> Bool
path_has_extension = |target| {
	basename = (target.split_on("#").first() ?? target).split_on("/").last() ?? ""
	if basename == "." or basename == ".." {
		return False
	}
	parts = basename.split_on(".")
	parts.len() > 1 and !(parts.len() == 2 and parts.first() == Ok(""))
}

expect AsciiDocInline.render_with("*strong* <raw> &copy; ... https://example.test[*labelw> &copy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "quotes,attributes,post_replacements,macros,replacements,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;copy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;labelw&gt; &amp;copy; &amp;#8230;&amp;#8203; https://example.test[*label&lt;/strong&gt;&lt;/a&gt; *attribute*&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render_with("xref:other.a<<target>>doc[Other]", [], { subs: "normal" }) == "<a href=\"other.a&lt;&lt;target&gt;&gt;doc\">Other</a>"

expect AsciiDocInline.render_with("mailto:a@example.test[N\r\ne,Subject,Body]", [], { subs: "normal" }) == "<a href=\"mailto:a@example.test?subject=Subject&amp;body=Body\">N e</a>"

expect AsciiDocInline.render_with("image:cat.png[alt=\"A &amp; age:cat.png[alt=\"A &amp; B\"]", [], { subs: "normal" }) == "<span class=\"image\"><img src=\"cat.png\" alt=\"A &amp; age:cat.png[alt=\" width=\"A &amp; B\"\"></span>"

expect AsciiDocInline.render_with("*strong* <raw> &c<<target>>opy; ... https://example.test[*label*] {value} +\nnext pass:q[*passed*]", [{ name: "value", value: "*attribute*" }], { subs: "attributes,post_replacements,replacements,macros,quotes,specialcharacters" }) == "&lt;strong&gt;strong&lt;/strong&gt; &lt;raw&gt; &amp;c&lt;&lt;target&gt;&gt;opy; &amp;#8230;&amp;#8203; &lt;a href=\"https://example.test\"&gt;&lt;strong&gt;label&lt;/strong&gt;&lt;/a&gt; &lt;strong&gt;attribute&lt;/strong&gt;&lt;br&gt;\nnext <strong>passed</strong>"

expect AsciiDocInline.render("--\\--\nRoc", []) == "----\nRoc"
expect AsciiDocInline.render("\\...", []) == "..."
expect AsciiDocInline.render("\\....", []) == "...."
expect AsciiDocInline.render_with("\\--", [], { subs: "replacements" }) == "--"

expect [2, 3, 4, 5].all(
	|n| {
		run = Str.join_with(List.repeat("-", n), "")
		escaped = "\\${run}"
		expected = if (n == 2) run else escaped
		AsciiDocInline.render(escaped, []) == expected and AsciiDocInline.render("A${escaped}B", []) == "A${expected}B" and AsciiDocInline.render("--${escaped}", []) == "--${expected}"
	},
)

expect {
	result = AsciiDocInline.render_state("{note}", [{ name: "note", value: "footnote:[A note.]" }], { subs: "normal" }, AsciiDocInline.initial_state)
	result.state.footnotes == [{ name: "", text: "A note." }] and result.html.contains("_footnoteref_1")
}

expect {
	var $state = AsciiDocInline.initial_state
	var $correct = True
	var $number = 0.U64
	while $number < 128 {
		rendered = AsciiDocInline.render_state("{counter:key${$number.to_str()}}", [], { subs: "normal" }, $state)
		$correct = $correct and rendered.html == "1"
		$state = rendered.state
		$number = $number + 1
	}
	final = AsciiDocInline.render_state("{counter:key0} {counter:key127} {key64}", [{ name: "role", value: "unrelated" }], { subs: "normal" }, $state)
	$correct and final.html == "2 2 1"
}

expect AsciiDocInline.render("*bold* {mark}", [{ name: "mark", value: "\u(1)" }]) == "<strong>bold</strong> \u(1)"
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {mark}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{counter:key:\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {key}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("{set:mark!}", [], { subs: "normal" }, first.state)
	AsciiDocInline.render_state("*bold* {counter:key_0}", [], { subs: "normal" }, second.state).html == "<strong>bold</strong> 1"
}
expect {
	first = AsciiDocInline.render_state("pass:a[{set:mark:\u(1)}]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* {mark}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1)"
}
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)}footnote:note[{mark}]", [], { subs: "normal" }, AsciiDocInline.initial_state)
	second = AsciiDocInline.render_state("*bold* footnote:note[] {mark}", [], { subs: "normal" }, first.state)
	second.state.footnotes == [{ name: "note", text: "\u(1)" }] and second.html.contains("<strong>bold</strong>") and second.html.contains("_footnotedef_1") and second.html.ends_with(" \u(1)")
}

expect {
	attrs = [{ name: "n", value: "4" }, { name: "n", value: "99" }, { name: "__revision:n", value: "1" }]
	plain = AsciiDocInline.render_state("plain", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, plain.state)
	redefined = [{ name: "n", value: "4" }, { name: "n", value: "99" }, { name: "__revision:n", value: "2" }]
	reset = AsciiDocInline.render_state("{counter:n}", redefined, { subs: "normal" }, first.state)
	first.html == "5" and reset.html == "5"
}
expect {
	attrs = [{ name: "n", value: "4" }, { name: "__revision:n", value: "1" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	next = AsciiDocInline.render_state("{counter:n}", attrs.append({ name: "role", value: "metadata" }), { subs: "normal" }, first.state)
	next.html == "6"
}
expect {
	plain = AsciiDocInline.render_state("plain", [{ name: "n", value: "4" }], { subs: "normal" }, AsciiDocInline.initial_state)
	attrs = [{ name: "n", value: "8" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, plain.state)
	second = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, first.state)
	first.html == "9" and second.html == "10"
}

# Nested attribute-inserted notes and ordinary counters share their registries.
expect {
	attrs = [{ name: "note", value: "footnote:shared[Nested note.]" }]
	first = AsciiDocInline.render_state("{note} {counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	next = AsciiDocInline.render_state("*{counter:n}* footnote:shared[]", attrs, { subs: "normal" }, first.state)
	first.state.footnotes == [{ name: "shared", text: "Nested note." }] and next.state.footnotes == first.state.footnotes and next.state.attributes.get("n") == Ok("2") and next.html == "<strong>2</strong> <sup class=\"footnoteref\">[<a class=\"footnote\" href=\"#_footnotedef_1\" title=\"View footnote.\">1</a>]</sup>"
}

# Crossing the passthrough boundary preserves source revisions and resets.
expect {
	attrs = [{ name: "n", value: "4" }, { name: "__revision:n", value: "1" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	passed = AsciiDocInline.render_state("pass:a[{counter:n}]", attrs, { subs: "normal" }, first.state)
	redefined = [{ name: "n", value: "4" }, { name: "__revision:n", value: "2" }]
	next = AsciiDocInline.render_state("{counter:n}", redefined, { subs: "normal" }, passed.state)
	first.html == "5" and passed.html == "6" and next.html == "5" and next.state.snapshot == redefined
}

# The nested semantic path also preserves emissions/reference/source metadata.
expect {
	attrs = [{ name: "note", value: "footnote:shared[Nested note.]" }]
	refs = [{ id: "go", text: "Go" }]
	initial = { ..AsciiDocInline.initial_state, emitted_footnotes: [7], refs }
	result = AsciiDocInline.render_state("{note}", attrs, { subs: "normal" }, initial)
	result.state.emitted_footnotes == [7] and result.state.refs == refs and result.state.ref_index.get("go") == Ok("Go") and result.state.indexed_ref_count == 1 and result.state.snapshot == attrs and result.state.indexed_note_count == 1
}

# Explicit attribute stages carry distinct counters through repeated calls.
expect {
	var $state = AsciiDocInline.initial_state
	var $index = 0.U64
	while $index < 64 {
		{ html: _, state: next } = AsciiDocInline.render_state("{counter:key${$index.to_str()}}", [], { subs: "attributes,replacements" }, $state)
		$state = next
		$index = $index + 1
	}
	result = AsciiDocInline.render_state("{key0} {key63} {counter:key0}", [], { subs: "attributes" }, $state)
	result.html == "1 1 2" and result.state.attributes.len() == 64
}

# Macro-only stages reuse the document note registry without renumbering refs.
expect {
	var $state = AsciiDocInline.initial_state
	var $index = 0.U64
	while $index < 64 {
		{ html: _, state: next } = AsciiDocInline.render_state("footnote:note${$index.to_str()}[Note ${$index.to_str()}.]", [], { subs: "macros" }, $state)
		$state = next
		$index = $index + 1
	}
	result = AsciiDocInline.render_state("footnote:note0[] footnote:note63[]", [], { subs: "macros" }, $state)
	result.state.footnotes.len() == 64 and result.state.note_index.get("note0") == Ok(1) and result.state.note_index.get("note63") == Ok(64) and result.html.contains("href=\"#_footnotedef_1\"") and result.html.contains("href=\"#_footnotedef_64\"")
}

# Preserve preview capture behavior during this ownership change. Native
# Asciidoctor currently captures 6 here; the established parser captures 5.
# This known mismatch is retained separately, not claimed as oracle parity.
expect {
	result = AsciiDocInline.render_state("{set:captured:pass:a[{counter:n}]} {captured} {counter:n}", [{ name: "n", value: "4" }], { subs: "normal" }, AsciiDocInline.initial_state)
	result.html == " 5 5" and result.state.attributes.get("captured") == Ok("5") and result.state.attributes.get("n") == Ok("6")
}

# Immediate stem rendering still reads dynamic dialect attributes.
expect {
	first = AsciiDocInline.render_state("{set:stem:latexmath}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("stem:[x < y]", [], { subs: "normal" }, first.state).html == "\\(x &lt; y\\)"
}

# Role-prefixed and escaped passes retain immediate matching behavior.
expect {
	first = AsciiDocInline.render_state("{set:x:dynamic}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("Text [role]pass:a[{x}] and \\pass:a[{x}] and pass:a[{x}]", [], { subs: "normal" }, first.state).html == "Text dynamic and pass:a[dynamic] and dynamic"
}

# A deferred pass enclosed in a single-plus span shares the cached delimiter.
expect {
	first = AsciiDocInline.render_state("{set:mark:\u(1)}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("*bold* +pass:a[{mark}]+ {counter:n}", [], { subs: "normal" }, first.state).html == "<strong>bold</strong> \u(1) 1"
}

# Dynamic macro settings override the first source binding and can hide it.
expect {
	attrs = [{ name: "imagesdir", value: "first" }, { name: "imagesdir", value: "second" }]
	original = AsciiDocInline.render_state("image:cat.png[]", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	set = AsciiDocInline.render_state("{set:imagesdir:dynamic}", attrs, { subs: "normal" }, original.state)
	changed = AsciiDocInline.render_state("image:cat.png[]", attrs, { subs: "normal" }, set.state)
	unset = AsciiDocInline.render_state("{set:imagesdir!}", attrs, { subs: "normal" }, changed.state)
	hidden = AsciiDocInline.render_state("image:cat.png[]", attrs, { subs: "normal" }, unset.state)
	original.html.contains("src=\"first/cat.png\"") and changed.html.contains("src=\"dynamic/cat.png\"") and hidden.html.contains("src=\"cat.png\"")
}

# Empty dynamic values still enable presence-based macro settings.
expect {
	set = AsciiDocInline.render_state("{set:experimental:}", [], { subs: "normal" }, AsciiDocInline.initial_state)
	AsciiDocInline.render_state("kbd:[Enter]", [], { subs: "normal" }, set.state).html == "<kbd>Enter</kbd>"
}

# An immediate role-prefixed pass reads the dynamic value, but its counter
# writes remain local; deferred passes intentionally update the shared state.
expect {
	attrs = [{ name: "n", value: "4" }]
	first = AsciiDocInline.render_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	immediate = AsciiDocInline.render_state("Text [role]pass:a[{counter:n}] {counter:n}", attrs, { subs: "normal" }, first.state)
	deferred = AsciiDocInline.render_state("Text pass:a[{counter:n}] {counter:n}", attrs, { subs: "normal" }, immediate.state)
	first.html == "5" and immediate.html == "Text 6 6" and immediate.state.attributes.get("n") == Ok("6") and deferred.html == "Text 8 7" and deferred.state.attributes.get("n") == Ok("8")
}

# A pure immediate stage has the same ordered scanner semantics without a
# fresh document state, including aliases and source/value collisions.
expect {
	view = { ..static_view([{ name: "mark", value: "\u(1)" }]), values: AsciiDocDict.empty().insert("n", "4") }
	["q", "specialcharacters", "replacements", "post_replacements"].all(|stage|
		render_with_view("*word* < & ... +\nnext", view, { subs: stage }) == AsciiDocInline.render_with("*word* < & ... +\nnext", materialize_view(view), { subs: stage }))
}

# Preview only a slot captured by a literal set expression. Unrelated sets,
# escaped openings and a line break must not force discarded child renders.
expect {
	view = static_view([{ name: "n", value: "4" }])
	captured = quote_view("{set:x:pass:a[{counter:n}]}", view, normal)
	before = quote_view("{set:x:value} pass:a[{counter:n}]", view, normal)
	after = quote_view("pass:a[{counter:n}] {set:x:value}", view, normal)
	escaped = quote_view("\\{set:x:pass:a[{counter:n}]}", view, normal)
	newline = quote_view("{set:x:\npass:a[{counter:n}]}", view, normal)
	captured.tags == ["5"] and before.tags == [""] and after.tags == [""] and escaped.tags == [""] and newline.tags == [""]
}

# Chunk boundaries cannot interrupt a set prefix, while protected slot bytes
# must not close a capture containing another protected pass.
expect {
	first = capture_set("{se", { matched: 0, active: False, previous: 0 })
	second = capture_set("t:x:", first)
	third = capture_set(token("\u(1)", 0), second)
	end = capture_set("} pass", third)
	second.active and third.active and !end.active
}

# Cached source lookup keeps the first binding and invalidates when a later
# snapshot changes it, even with no dynamic overrides to reconcile.
expect {
	attrs = [{ name: "imagesdir", value: "first" }, { name: "imagesdir", value: "second" }]
	first = AsciiDocInline.render_state("image:cat.png[]", attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	same = AsciiDocInline.render_state("image:cat.png[]", attrs, { subs: "normal" }, first.state)
	next = AsciiDocInline.render_state("image:cat.png[]", [{ name: "imagesdir", value: "new" }], { subs: "normal" }, same.state)
	first.state.snapshot_index.get("imagesdir") == Ok("first") and same.html == first.html and next.html.contains("src=\"new/cat.png\"")
}

# Query restoration preserves duplicate source entries and appended tag order
# while excluding overridden/removed source bindings from the effective view.
expect {
	view = { ..static_view([{ name: "__inline-tag:x", value: "y" }, { name: "__inline-tag:y", value: "z" }]), values: AsciiDocDict.empty().insert("counter", "1"), appended: [{ name: "__inline-tag:z", value: "done" }] }
	restore_view_tags("x", view) == "done" and restore_view_tags("x", view) == restore_tag_attributes("x", materialize_view(view))
}

# Arbitrary dynamic replacement names retain their original dictionary order,
# including a dynamic binding that shadows a source tag binding.
expect {
	view = { ..static_view([{ name: "__inline-tag:x", value: "wrong" }, { name: "__inline-tag:y", value: "done" }]), dynamic_tags_possible: True, values: AsciiDocDict.empty().insert("__inline-tag:x", "y").insert("unrelated", "1") }
	restore_view_tags("x", view) == "done" and restore_view_tags("x", view) == restore_tag_attributes("x", materialize_view(view))
}

expect {
	view = { ..static_view([{ name: "__inline-tag:x", value: "wrong" }]), removed: AsciiDocDict.empty().insert("__inline-tag:x", True), appended: [{ name: "__inline-tag:x", value: "right" }] }
	restore_view_tags("x", view) == "right"
}

# A caller may seed arbitrary private bindings through the initial state.
# The initial cache is conservative until an empty registry is observed.
expect {
	seeded = { ..AsciiDocInline.initial_state, attributes: AsciiDocDict.empty().insert("__inline-tag:X", "Y") }
	result = AsciiDocInline.render_state("mailto:a@example.test[Name,X]", [], { subs: "normal" }, seeded)
	result.html == "<a href=\"mailto:a@example.test?subject=Y\">Name</a>"
}

# Trusted document snapshots reuse source lookup without structural List
# equality, including deferred nested calls and same-value source revisions.
expect {
	attrs = [{ name: "n", value: "4" }, { name: "__revision:n", value: "1" }]
	index = { first: source_attribute_index(attrs), token: "snapshot-one", prefix: "\u(1)" }
	first = AsciiDocInline.render_indexed_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state, index)
	passed = AsciiDocInline.render_indexed_state("pass:a[{counter:n}]", attrs, { subs: "normal" }, first.state, index)
	redefined = [{ name: "n", value: "4" }, { name: "__revision:n", value: "2" }]
	next = AsciiDocInline.render_indexed_state("{counter:n}", redefined, { subs: "normal" }, passed.state, { first: source_attribute_index(redefined), token: "snapshot-two", prefix: "\u(1)" })
	first.html == "5" and passed.html == "6" and passed.state.snapshot == attrs and passed.state.trusted_snapshot_token == "snapshot-one" and next.html == "5" and next.state.trusted_snapshot_token == "snapshot-two"
}

# A legacy/manual render invalidates the trusted token, so returning to an
# indexed snapshot reconciles a changed value rather than accepting stale state.
expect {
	attrs = [{ name: "n", value: "4" }]
	index = { first: source_attribute_index(attrs), token: "snapshot-one", prefix: "\u(1)" }
	first = AsciiDocInline.render_indexed_state("{counter:n}", attrs, { subs: "normal" }, AsciiDocInline.initial_state, index)
	manual = AsciiDocInline.render_state("{counter:n}", [{ name: "n", value: "7" }], { subs: "normal" }, first.state)
	next = AsciiDocInline.render_indexed_state("{counter:n}", attrs, { subs: "normal" }, manual.state, index)
	manual.html == "8" and manual.state.trusted_snapshot_token == "" and next.html == "5"
}

# Stage loops and source removals keep the indexed entry equivalent to the
# conservative entry while preserving the caller's final semantic snapshot.
expect {
	attrs = [{ name: "imagesdir", value: "first" }, { name: "imagesdir", value: "second" }]
	index = { first: source_attribute_index(attrs), token: "snapshot-one", prefix: "\u(1)" }
	input = "{set:imagesdir!} image:cat.png[]"
	indexed = AsciiDocInline.render_indexed_state(input, attrs, { subs: "attributes,macros" }, AsciiDocInline.initial_state, index)
	legacy = AsciiDocInline.render_state(input, attrs, { subs: "attributes,macros" }, AsciiDocInline.initial_state)
	indexed.html == legacy.html and indexed.state.removed == legacy.state.removed and indexed.state.snapshot == attrs and indexed.state.trusted_snapshot_token == index.token
}

# The source certificate includes shadowed and removed values, preserving the
# existing conservative collision boundary rather than just effective values.
expect {
	view = { ..static_view([{ name: "mark", value: "\u(1)\u(1)\u(1)\u(1)" }]), values: AsciiDocDict.empty().insert("mark", "dynamic"), removed: AsciiDocDict.empty().insert("mark", True) }
	certified = { ..view, source_prefix: "\u(1)\u(1)\u(1)\u(1)\u(1)" }
	quote_view("*word*", view, normal) == quote_view("*word*", certified, normal)
}

# Input and synthetic appended values still extend an already certified
# prefix; the source certificate does not certify either of these inputs.
expect {
	view = { ..static_view([{ name: "mark", value: "\u(1)" }]), appended: [{ name: "__inline-tag:x", value: "\u(1)\u(1)" }] }
	certified = { ..view, source_prefix: "\u(1)\u(1)" }
	input = "*word* \u(1)\u(1)\u(1)"
	quote_view(input, view, normal) == quote_view(input, certified, normal)
}

# A dynamic value can require a longer prefix than the static certificate.
# Immediate pure-stage passes and ordered stages retain the borrowed proof.
expect {
	attrs = [{ name: "source-mark", value: "\u(1)" }]
	index = { first: source_attribute_index(attrs), token: "certified-source", prefix: "\u(1)\u(1)" }
	first = AsciiDocInline.render_indexed_state("{set:dynamic-mark:\u(1)\u(1)\u(1)}", attrs, { subs: "normal" }, AsciiDocInline.initial_state, index)
	input = "*word* pass:q[_nested_] {dynamic-mark}"
	indexed = AsciiDocInline.render_indexed_state(input, attrs, { subs: "quotes,attributes,macros" }, first.state, index)
	legacy = AsciiDocInline.render_state(input, attrs, { subs: "quotes,attributes,macros" }, first.state)
	indexed.html == legacy.html and indexed.state.attributes == legacy.state.attributes
}

# Arbitrary private caller prefixes cannot use the repeated-control length
# proof and therefore retain the complete legacy source collision scan.
expect {
	view = { ..static_view([{ name: "mark", value: "CUSTOM" }]), prefix: "CUSTOM" }
	certified = { ..view, source_prefix: "\u(1)" }
	quote_view("*word*", view, normal) == quote_view("*word*", certified, normal)
}

# Anonymous/named footnote extraction reserves slots even without quoted
# syntax. A source certificate preserves collisions in both text and values.
expect {
	attrs = [{ name: "mark", value: "\u(1)\u(1)" }]
	index = { first: source_attribute_index(attrs), token: "note-source", prefix: "\u(1)\u(1)\u(1)" }
	input = "\u(1)\u(1)\u(1) footnote:shared[A note {mark}.] footnote:shared[]"
	indexed = AsciiDocInline.render_indexed_state(input, attrs, { subs: "normal" }, AsciiDocInline.initial_state, index)
	legacy = AsciiDocInline.render_state(input, attrs, { subs: "normal" }, AsciiDocInline.initial_state)
	indexed.html == legacy.html and indexed.state.footnotes == legacy.state.footnotes and indexed.state.note_index == legacy.state.note_index
}

# Layered lookup is exactly FIRST before -> base -> after, followed by
# dynamic overrides/removals and the scanner's lower-priority appended tags.
expect {
	base = [{ name: "name", value: "base" }, { name: "imagesdir", value: "base-images" }]
	before = [{ name: "name", value: "leading" }, { name: "name", value: "second-leading" }]
	after = [{ name: "name", value: "trailing" }, { name: "experimental", value: "" }]
	view = { ..static_view(base), source_index: AsciiDocSource.from_dict(source_attribute_index(base)), source_before: before, source_after: after, appended: [{ name: "name", value: "synthetic" }] }
	changed = { ..view, values: AsciiDocDict.empty().insert("name", "dynamic") }
	hidden = { ..view, removed: AsciiDocDict.empty().insert("name", True) }
	lookup_view(view, "name") == Ok("leading") and lookup_view(changed, "name") == Ok("dynamic") and lookup_view(hidden, "name") == Ok("synthetic") and lookup_view(view, "experimental") == Ok("")
}

# Reusing a base token must still compare small overlays. A same-value
# revision resets the counter, while changing unrelated metadata does not.
expect {
	base = [{ name: "n", value: "4" }]
	first_index = source_attribute_index(base)
	before = [{ name: "n", value: "8" }, { name: "__revision:n", value: "one" }]
	hint = { first: first_index, token: "layer-base", prefix: "\u(1)", before, after: [] }
	first = AsciiDocInline.render_layered_state("{counter:n}", base, { subs: "normal" }, AsciiDocInline.initial_state, hint)
	second = AsciiDocInline.render_layered_state("{counter:n}", base, { subs: "normal" }, first.state, { ..hint, after: [{ name: "role", value: "new" }] })
	revised = [{ name: "n", value: "8" }, { name: "__revision:n", value: "two" }]
	third = AsciiDocInline.render_layered_state("{counter:n}", base, { subs: "normal" }, second.state, { ..hint, before: revised })
	first.html == "9" and second.html == "10" and third.html == "9" and third.state.snapshot == base and third.state.snapshot_before == revised
}

# Deferred passes, explicit stage order, shared inserted notes and isolated
# immediate previews all see local overlays, matching the old flat source.
expect {
	base = [{ name: "n", value: "4" }, { name: "note", value: "footnote:shared[An inserted note.]" }]
	before = [{ name: "n", value: "8" }]
	after = [{ name: "imagesdir", value: "assets" }]
	hint = { first: source_attribute_index(base), token: "layer-base", prefix: "\u(1)", before, after }
	flat = before.concat(base).concat(after)
	["normal", "quotes,attributes,macros"].all(
		|subs| {
			input = "pass:a[{counter:n}] [role]pass:a[{counter:n}] {note} image:cat.png[]"
			layered = AsciiDocInline.render_layered_state(
				input,
				base,
				{ subs: subs },
				AsciiDocInline.initial_state,
				hint,
			)
			legacy = AsciiDocInline.render_state(
				input,
				flat,
				{ subs: subs },
				AsciiDocInline.initial_state,
			)
			layered.html == legacy.html and layered.state.attributes == legacy.state.attributes and layered.state.footnotes == legacy.state.footnotes and layered.state.snapshot_before == before and layered.state.snapshot_after == after
		},
	)
}

# Legacy calls drop old overlays only after reconciling their source values.
expect {
	base = [{ name: "n", value: "4" }]
	hint = { first: source_attribute_index(base), token: "layer-base", prefix: "\u(1)", before: [{ name: "n", value: "8" }], after: [] }
	first = AsciiDocInline.render_layered_state("{counter:n}", base, { subs: "normal" }, AsciiDocInline.initial_state, hint)
	legacy = AsciiDocInline.render_state("{counter:n}", base, { subs: "normal" }, first.state)
	first.html == "9" and legacy.html == "5" and legacy.state.snapshot_before.is_empty() and legacy.state.snapshot_after.is_empty() and legacy.state.trusted_snapshot_token == ""
}

# An uncertified layered source still checks every overlay value for private
# slot collisions, even when that binding is shadowed by the dynamic registry.
expect {
	base = [{ name: "x", value: "base" }]
	before = [{ name: "mark", value: "\u(1)\u(1)" }]
	view = { ..static_view(base), source_index: AsciiDocSource.from_dict(source_attribute_index(base)), source_before: before, values: AsciiDocDict.empty().insert("mark", "dynamic") }
	flat = { ..static_view(before.concat(base)), values: view.values }
	quote_view("*word* pass:q[_nested_]", view, normal) == quote_view("*word* pass:q[_nested_]", flat, normal)
}

# Query replacement order remains the complete effective before/base/after
# order, including duplicates and appended synthetic replacement attributes.
expect {
	view = { ..static_view([{ name: "__inline-tag:y", value: "z" }]), source_before: [{ name: "__inline-tag:x", value: "y" }], source_after: [{ name: "__inline-tag:z", value: "done" }], appended: [{ name: "__inline-tag:done", value: "final" }] }
	restore_view_tags("x", view) == "final" and restore_view_tags("x", view) == restore_tag_attributes("x", materialize_view(view))
}

# Source generations distinguish same-value revisions without canonicalizing
# the full snapshot; changing an unrelated source binding preserves overrides.
expect {
	first_store = AsciiDocSource.new("counter-document", [{ name: "n", value: "4" }])
	first_key = AsciiDocSource.key(first_store)
	first_hint = { first: AsciiDocSource.index(first_store), token: first_key.namespace, generation: first_key.generation, prefix: AsciiDocSource.prefix(first_store), before: [], after: [] }
	first = AsciiDocInline.render_source_state("{counter:n}", [], { subs: "normal" }, AsciiDocInline.initial_state, first_hint)
	revised = AsciiDocSource.define(first_store, "n", "4", False)
	revised_key = AsciiDocSource.key(revised)
	revised_hint = { first: AsciiDocSource.index(revised), token: revised_key.namespace, generation: revised_key.generation, prefix: AsciiDocSource.prefix(revised), before: [], after: [] }
	second = AsciiDocInline.render_source_state("{counter:n}", [], { subs: "normal" }, first.state, revised_hint)
	unrelated = AsciiDocSource.define(revised, "other", "value", False)
	third = AsciiDocInline.render_source_state("{counter:n}", [], { subs: "normal" }, second.state, { ..revised_hint, first: AsciiDocSource.index(unrelated), generation: AsciiDocSource.key(unrelated).generation })
	first.html == "5" and second.html == "5" and third.html == "6" and third.state.snapshot.is_empty() and third.state.trusted_snapshot_generation == 2 and AsciiDocSource.lookup(first_store, "__revision:n").is_err()
}

# Empty fallback Lists cannot make a parsed source equal to a legacy empty
# environment; dropping that source resets live overrides before substitution.
expect {
	store = AsciiDocSource.new("counter-document", [{ name: "n", value: "4" }])
	key = AsciiDocSource.key(store)
	first = AsciiDocInline.render_source_state("{counter:n}", [], { subs: "normal" }, AsciiDocInline.initial_state, { first: AsciiDocSource.index(store), token: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(store), before: [], after: [] })
	legacy = AsciiDocInline.render_state("{counter:n}", [], { subs: "normal" }, first.state)
	legacy.html == "1" and !AsciiDocSource.is_persistent(legacy.state.snapshot_index) and legacy.state.trusted_snapshot_generation == 0 and legacy.state.trusted_snapshot_token == ""
}

# Immediate children read parent values through an immutable view; their
# counter/set/note writes remain isolated while deferred passes share state.
expect {
	store = AsciiDocSource.new("preview-document", [{ name: "n", value: "4" }])
	key = AsciiDocSource.key(store)
	hint = { first: AsciiDocSource.index(store), token: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(store), before: [], after: [] }
	initial = AsciiDocInline.render_source_state("{counter:n}", [], { subs: "normal" }, AsciiDocInline.initial_state, hint)
	input = "[role]pass:a[{counter:n}] pass:a[{counter:n}] {counter:n}"
	result = AsciiDocInline.render_source_state(input, [], { subs: "normal" }, initial.state, hint)
	legacy_initial = AsciiDocInline.render_state("{counter:n}", AsciiDocSource.materialize(store), { subs: "normal" }, AsciiDocInline.initial_state)
	legacy = AsciiDocInline.render_state(input, AsciiDocSource.materialize(store), { subs: "normal" }, legacy_initial.state)
	result.html == legacy.html and result.state.attributes == legacy.state.attributes and result.state.snapshot_index == AsciiDocSource.index(store) and initial.state.attributes.get("n") == Ok("5")
}

# Isolated preview lookup and query restoration retain parent overrides,
# removals, duplicate Flat source tags and appended tag replacement order.
expect {
	store = AsciiDocSource.new("preview-tags", [{ name: "__inline-tag:X", value: "Y" }, { name: "__inline-tag:Y", value: "Z" }])
	view = { ..static_view([]), source_index: AsciiDocSource.index(store), source_prefix: AsciiDocSource.prefix(store), source_after: [{ name: "__inline-tag:Z", value: "done" }], appended: [{ name: "__inline-tag:done", value: "final" }], values: AsciiDocDict.empty().insert("n", "40"), dynamic_tags_possible: False }
	query = render_with_view("mailto:a@example.test[Name,X]", view, { subs: "macros" })
	counter = render_with_view("{counter:n} {set:local:value} {local}", view, { subs: "attributes" })
	legacy = AsciiDocInline.render_with("{counter:n} {set:local:value} {local}", materialize_view(view), { subs: "attributes" })
	query == "<a href=\"mailto:a@example.test?subject=final\">Name</a>" and counter == legacy and view.values.get("n") == Ok("40") and !view.values.contains("local")
}

# Definition-time readers preserve ordinary escaping, whole-value pass
# selection, unknown/escaped references and bounded nested source expansion.
expect {
	attrs = [{ name: "name", value: "Roc" }, { name: "quoted", value: "*bold*" }, { name: "mark", value: "\u(1)\u(1)" }]
	store = AsciiDocSource.new("definition-document", attrs)
	["{name} < &", "{quoted}", "pass:q[*{name}*]", "pass:a[{name}]", "\\{name} {missing}", "{mark} *x*"].all(|source| AsciiDocInline.attribute_value_source(source, store) == AsciiDocInline.attribute_value(source, attrs))
}

# Delta-driven reset decisions match the conservative flat transition across
# skipped versions, same-value definitions, tombstones and temporary restores.
expect {
	initial = AsciiDocSource.new_parsed("delta-parity", [{ name: "n", value: "4" }, { name: "gone", value: "source" }, { name: "blocked", value: "source" }])
	unrelated = AsciiDocSource.define_parsed(initial, "other", "one", False)
	same = AsciiDocSource.define_parsed(unrelated, "n", "4", False)
	unset = AsciiDocSource.define_parsed(same, "gone", "", True)
	readded = AsciiDocSource.define_parsed(unset, "gone", "source", False)
	temporary = AsciiDocSource.set_parsed(readded, "leveloffset", "2")
	restored = AsciiDocSource.unset_parsed(temporary, "leveloffset")
	changed_reserved = AsciiDocSource.define_parsed(restored, "__revision:n", "17", False)
	values = AsciiDocDict.empty().insert("n", "8").insert("gone", "local").insert("leveloffset", "9").insert("__revision:n", "local revision")
	removed = AsciiDocDict.empty().insert("blocked", True)
	[(initial, unrelated), (initial, same), (initial, unset), (initial, readded), (initial, temporary), (initial, restored), (initial, changed_reserved), (temporary, restored), (changed_reserved, initial)].all(
		|(previous, next)| {
			old_key = AsciiDocSource.key(previous)
			next_key = AsciiDocSource.key(next)
			actual = refresh_hint([], values, removed, [], AsciiDocSource.index(previous), [], [], old_key.namespace, old_key.generation, Ok({ first: AsciiDocSource.index(next), token: next_key.namespace, generation: next_key.generation, prefix: AsciiDocSource.prefix(next), before: [], after: [] }))
			old_flat = AsciiDocSource.materialize(previous)
			new_flat = AsciiDocSource.materialize(next)
			expected = refresh_hint(new_flat, values, removed, old_flat, AsciiDocSource.from_dict(source_attribute_index(old_flat)), [], [], "", 0, Ok({ first: AsciiDocSource.from_dict(source_attribute_index(new_flat)), token: "", generation: 0, prefix: "\u(1)", before: [], after: [] }))
			actual.attributes == expected.attributes and actual.removed == expected.removed
		},
	)
}

# An ordinary fork can have the same key while its source value differs.
expect {
	base = AsciiDocSource.new("manual-fork", [{ name: "n", value: "4" }])
	left = AsciiDocSource.set(base, "n", "5")
	right = AsciiDocSource.set(base, "n", "6")
	key = AsciiDocSource.key(left)
	actual = refresh_hint([], AsciiDocDict.empty().insert("n", "99"), AsciiDocDict.empty(), [], AsciiDocSource.index(left), [], [], key.namespace, key.generation, Ok({ first: AsciiDocSource.index(right), token: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(right), before: [], after: [] }))
	actual.attributes.is_empty()
}

# Both the raw reserved name and its revision alias can be dynamic bindings.
expect {
	base = AsciiDocSource.new_parsed("overlay-revision", [{ name: "n", value: "4" }])
	key = AsciiDocSource.key(base)
	actual = refresh_hint([], AsciiDocDict.empty().insert("n", "8").insert("__revision:n", "local"), AsciiDocDict.empty(), [], AsciiDocSource.index(base), [{ name: "__revision:n", value: "1" }], [], key.namespace, key.generation, Ok({ first: AsciiDocSource.index(base), token: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(base), before: [{ name: "__revision:n", value: "2" }], after: [] }))
	actual.attributes.is_empty()
}

# Many unrelated revisions expose only their own names, retaining all live
# overrides; a changed source revision still clears an existing removal.
expect {
	initial = AsciiDocSource.new_parsed("bounded-delta", [{ name: "blocked", value: "same" }])
	var $next = initial
	var $values = AsciiDocDict.empty()
	var $i = 0.U64
	while $i < 128 {
		$values = $values.insert("key${$i.to_str()}", "live")
		$next = AsciiDocSource.define_parsed($next, "unrelated", $i.to_str(), False)
		$i = $i + 1
	}
	old_key = AsciiDocSource.key(initial)
	next_key = AsciiDocSource.key($next)
	actual = refresh_hint([], $values, AsciiDocDict.empty().insert("blocked", True), [], AsciiDocSource.index(initial), [], [], old_key.namespace, old_key.generation, Ok({ first: AsciiDocSource.index($next), token: next_key.namespace, generation: next_key.generation, prefix: AsciiDocSource.prefix($next), before: [], after: [] }))
	changed = AsciiDocSource.define_parsed($next, "blocked", "same", False)
	changed_key = AsciiDocSource.key(changed)
	reset = refresh_hint([], actual.attributes, actual.removed, [], actual.snapshot_index, [], [], next_key.namespace, next_key.generation, Ok({ first: AsciiDocSource.index(changed), token: changed_key.namespace, generation: changed_key.generation, prefix: AsciiDocSource.prefix(changed), before: [], after: [] }))
	(AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index($next)) ?? []).len() == 2 and actual.attributes.len() == 128 and actual.removed.contains("blocked") and reset.attributes.len() == 128 and reset.removed.is_empty()
}

# Caller-supplied dynamic maps keep builtin iteration order across deletion
# and reinsertion, including ordered tag restoration and isolated previews.
expect {
	original = Dict.empty().insert("n", "9").insert("__inline-tag:x", "y").insert("__inline-tag:y", "done").insert("spare", "kept")
	owned = AsciiDocDict.from_dict(original)
	changed = owned.remove("__inline-tag:x").insert("__inline-tag:x", "done")
	model = original.remove("__inline-tag:x").insert("__inline-tag:x", "done")
	view = { ..static_view([]), values: changed, dynamic_tags_possible: True }
	seeded = { ..AsciiDocInline.initial_state, attributes: owned }
	preview = AsciiDocInline.render_state("pass:a[{counter:n}] {counter:n}", [], { subs: "normal" }, seeded)
	changed.to_list() == model.to_list() and owned.to_list() == original.to_list() and materialize_view(view) == model.to_list().map(|(name, value)| { name, value }) and restore_view_tags("x", view) == restore_tag_attributes("x", materialize_view(view)) and preview.html == "11 10" and seeded.attributes.get("n") == Ok("9") and preview.state.attributes.get("n") == Ok("11")
}

# Pass extraction must retain Unicode boundaries across ordinary runs, matches,
# and the later shared-state deferred evaluation.
expect AsciiDocInline.render("é pass:q[*Roc*] 😀 pass:q[_end_]", []) == "é <strong>Roc</strong> 😀 <em>end</em>"
expect AsciiDocInline.render("é pass:a[{counter:n}] 😀 {counter:n}", []) == "é 2 😀 1"

# Escaped macro and role prefixes must retain the surrounding Unicode while
# bounded prefix removal leaves the original matching/capture behavior intact.
expect AsciiDocInline.render("é \\link:x[é] 😀", []) == "é link:x[é] 😀"
expect AsciiDocInline.render("é [.red]++😀 & é++終", []) == "é <span class=\"red\">😀 &amp; é</span>終"
expect AsciiDocInline.render("é & < 😀 {x} 終", [{ name: "x", value: "Roc" }]) == "é &amp; &lt; 😀 Roc 終"

# The macro prefix is already matched ASCII; EOF and a multibyte following
# character cannot be the reserved second colon.
expect AsciiDocInline.render("link:é[終] link: link::é[終]", []) == "<a href=\"é\">終</a> link: link::é[終]"

# Angle prefixes and label searches preserve Unicode, EOF and the boundary
# between a bare URL and a later unrelated bracket.
expect AsciiDocInline.render("é <https://é> 😀 <", []) == "é <a href=\"https://é\" class=\"bare\">https://é</a> 😀 &lt;"
expect AsciiDocInline.render("é \\<https://é> 😀", []) == "é &lt;https://é&gt; 😀"
expect AsciiDocInline.render("https://é tail [later]", []) == "<a href=\"https://é\" class=\"bare\">https://é</a> tail [later]"
expect AsciiDocInline.render("https://é[終] tail", []) == "<a href=\"https://é\">終</a> tail"

# Missing closers stay literal, but whitespace does not terminate a legacy
# angle URL. Separate closers and intervening macro spans advance the cache.
expect AsciiDocInline.render("<https://a <http://b", []) == "&lt;https://a &lt;http://b"
expect AsciiDocInline.render("<https://é tail>", []) == "<a href=\"https://é\" class=\"bare\">https://é</a> tail"
expect AsciiDocInline.render("<https://a> link:x[Label > inside] <http://b>", []) == "<a href=\"https://a\" class=\"bare\">https://a</a> <a href=\"x\">Label &gt; inside</a> <a href=\"http://b\" class=\"bare\">http://b</a>"

# A closer immediately preceded by either one or two backslashes is escaped;
# earlier/repeated absolute queries must match the unchanged independent scan.
expect {
	source = "[a\\]b\\\\]c] [d]"
	first = cached_bracket_end(source, 0, 0, empty_bracket_cursor)
	second = cached_bracket_end(source, 11, 0, first.cursor)
	earlier = cached_bracket_end(source, 0, 0, second.cursor)
	suffix = source.drop_prefix("[a\\]b\\\\]c] ")
	retained = cached_bracket_end(suffix, 0, 11, second.cursor)
	first.end == Ok(9) and second.end == Ok(13) and earlier.end == Ok(9) and retained.end == Ok(2) and first.end == first_bracket_end(source, 0) and retained.end == first_bracket_end(suffix, 0)
}

expect {
	source = "[a\\] [b\\\\]"
	var $cursor = empty_bracket_cursor
	var $same = True
	for open in [0, 5, 0, 5] {
		result = cached_bracket_end(source, open, 0, $cursor)
		$cursor = result.cursor
		$same = $same and result.end == Err(Missing) and result.end == first_bracket_end(source, open)
	}
	$same and parse_macro_view("link:é[missing link:x[also", static_view([]), normal, 0).used == 0
}

# Deferred extraction preserves escaped closing bytes in its raw body; a role
# prefix uses its separate raw ] matcher before the shared body-close cache.
expect {
	prefix = "pass:a[é\\]tail] "
	source = "${prefix}[.red]pass:q[x]"
	deferred = deferred_pass_cached(source, 0, empty_bracket_cursor)
	role = pass_match_cached(source.drop_prefix(prefix), 0, static_view([]), False, False, prefix.count_utf8_bytes(), deferred.cursor, empty_role_cursor)
	preserved = match deferred.result {
		Ok(value) => value.source == "é\\]tail"
		Err(_) => False
	}
	preserved and role.matched.body == "x" and role.matched.used == "[.red]pass:q[x]".count_utf8_bytes()
}

expect {
	source = "pass:a[é\\] pass:m[broken"
	quoted = quote_view(source, static_view([]), normal)
	quoted.source == source and quoted.tags.is_empty() and quoted.deferred.is_empty()
}

expect AsciiDocInline.render("pass:q[é\\]tail]", []) == "é]tail"

# Exact expected substitutions were independently captured from pinned
# Asciidoctor2.0.23; boundary, escape, and Unicode classes are distinct.

# url-boundary-space-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-space-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-tab-bare
expect AsciiDocInline.render("Before\thttps://a.b", []) == "Before\t<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-tab-formal
expect AsciiDocInline.render("Before\thttps://a.b[Link]", []) == "Before\t<a href=\"https://a.b\">Link</a>"

# url-boundary-nbsp-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-nbsp-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-ogham-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-ogham-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-em-space-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-em-space-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-narrow-nbsp-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-narrow-nbsp-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-medium-space-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before <a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-medium-space-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before <a href=\"https://a.b\">Link</a>"

# url-boundary-ideographic-bare
expect AsciiDocInline.render("Before　https://a.b", []) == "Before　<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-ideographic-formal
expect AsciiDocInline.render("Before　https://a.b[Link]", []) == "Before　<a href=\"https://a.b\">Link</a>"

# url-boundary-lf-bare
expect AsciiDocInline.render("Before\nhttps://a.b", []) == "Before\n<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-lf-formal
expect AsciiDocInline.render("Before\nhttps://a.b[Link]", []) == "Before\n<a href=\"https://a.b\">Link</a>"

# url-boundary-cr-bare
expect AsciiDocInline.render("Before\rhttps://a.b", []) == "Before\rhttps://a.b"

# url-boundary-cr-formal
expect AsciiDocInline.render("Before\rhttps://a.b[Link]", []) == "Before\rhttps://a.b[Link]"

# url-boundary-vt-bare
expect AsciiDocInline.render("Before\u(b)https://a.b", []) == "Before\u(b)https://a.b"

# url-boundary-vt-formal
expect AsciiDocInline.render("Before\u(b)https://a.b[Link]", []) == "Before\u(b)https://a.b[Link]"

# url-boundary-ff-bare
expect AsciiDocInline.render("Before\u(c)https://a.b", []) == "Before\u(c)https://a.b"

# url-boundary-ff-formal
expect AsciiDocInline.render("Before\u(c)https://a.b[Link]", []) == "Before\u(c)https://a.b[Link]"

# url-boundary-nul-bare
expect AsciiDocInline.render("Before\u(0)https://a.b", []) == "Before\u(0)https://a.b"

# url-boundary-nul-formal
expect AsciiDocInline.render("Before\u(0)https://a.b[Link]", []) == "Before\u(0)https://a.b[Link]"

# url-boundary-zero-width-bare
expect AsciiDocInline.render("Before​https://a.b", []) == "Before​https://a.b"

# url-boundary-zero-width-formal
expect AsciiDocInline.render("Before​https://a.b[Link]", []) == "Before​https://a.b[Link]"

# url-boundary-line-separator-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before https://a.b"

# url-boundary-line-separator-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before https://a.b[Link]"

# url-boundary-paragraph-separator-bare
expect AsciiDocInline.render("Before https://a.b", []) == "Before https://a.b"

# url-boundary-paragraph-separator-formal
expect AsciiDocInline.render("Before https://a.b[Link]", []) == "Before https://a.b[Link]"

# url-boundary-angle-raw
expect AsciiDocInline.render("<https://a.b>", []) == "<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-angle-http
expect AsciiDocInline.render("<http://a.b>", []) == "<a href=\"http://a.b\" class=\"bare\">http://a.b</a>"

# url-boundary-angle-missing
expect AsciiDocInline.render("<https://a.b", []) == "&lt;https://a.b"

# url-boundary-angle-escaped
expect AsciiDocInline.render("\\<https://a.b>", []) == "&lt;https://a.b&gt;"

# url-boundary-angle-url-escaped
expect AsciiDocInline.render("<\\https://a.b>", []) == "&lt;https://a.b&gt;"

# url-boundary-angle-both-escaped
expect AsciiDocInline.render("\\<\\https://a.b>", []) == "&lt;\\https://a.b&gt;"

# url-boundary-angle-entity
expect AsciiDocInline.render("&lt;https://a.b&gt;", []) == "<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-angle-escaped-entity
expect AsciiDocInline.render("\\&lt;https://a.b&gt;", []) == "&amp;lt;<a href=\"https://a.b&gt\" class=\"bare\">https://a.b&gt</a>;"

# url-boundary-quoted-bare
expect AsciiDocInline.render("\"https://a.b\"", []) == "\"https://a.b\""

# url-boundary-quoted-formal
expect AsciiDocInline.render("\"https://a.b[Link]\"", []) == "\"<a href=\"https://a.b\">Link</a>\""

# url-boundary-single-quoted-bare
expect AsciiDocInline.render("'https://a.b'", []) == "'https://a.b'"

# url-boundary-single-quoted-formal
expect AsciiDocInline.render("'https://a.b[Link]'", []) == "'<a href=\"https://a.b\">Link</a>'"

# url-boundary-word-formal
expect AsciiDocInline.render("xhttps://a.b[Link]", []) == "xhttps://a.b[Link]"

# url-boundary-colon-formal
expect AsciiDocInline.render(":https://a.b[Link]", []) == ":https://a.b[Link]"

# url-boundary-literal-link-valid
expect AsciiDocInline.render("xlink:https://a.b[Link]", []) == "x<a href=\"https://a.b\">Link</a>"

# url-boundary-literal-link-bare
expect AsciiDocInline.render("xlink:https://a.b", []) == "xlink:https://a.b"

# url-boundary-literal-link-escaped-valid
expect AsciiDocInline.render("\\link:https://a.b[Link]", []) == "\\<a href=\"https://a.b\">Link</a>"

# url-boundary-url-escaped-formal
expect AsciiDocInline.render("\\https://a.b[Link]", []) == "https://a.b[Link]"

# url-boundary-unicode-letter
expect AsciiDocInline.render("東京https://a.b", []) == "東京https://a.b"

# url-boundary-emoji
expect AsciiDocInline.render("🙂https://a.b", []) == "🙂https://a.b"

# url-boundary-semicolon
expect AsciiDocInline.render("word;https://a.b", []) == "word;<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# url-boundary-closing-bracket
expect AsciiDocInline.render("word]https://a.b", []) == "word]<a href=\"https://a.b\" class=\"bare\">https://a.b</a>"

# Pinned malformed macro boundary: link-boundary-link-incomplete
expect AsciiDocInline.render("link:https://a.b[", []) == "link:https://a.b["

# Pinned malformed macro boundary: link-boundary-unknown-prefix
expect AsciiDocInline.render("foo:https://a.b[", []) == "foo:https://a.b["

# Pinned malformed macro boundary: link-boundary-word-prefix
expect AsciiDocInline.render("xhttps://a.b[", []) == "xhttps://a.b["

# Pinned malformed macro boundary: link-boundary-double-colon-prefix
expect AsciiDocInline.render("::https://a.b[", []) == "::https://a.b["

# Pinned malformed macro boundary: link-boundary-link-double-colon
expect AsciiDocInline.render("link::https://a.b[", []) == "link::https://a.b["

# Pinned malformed macro boundary: link-boundary-escaped-link
expect AsciiDocInline.render("\\link:https://a.b[", []) == "\\link:https://a.b["

# Pinned malformed macro boundary: link-boundary-image-incomplete
expect AsciiDocInline.render("image:https://a.b[", []) == "image:https://a.b["

# A raw role closes at an escaped ], unlike an escaped passthrough body.
expect {
	source = "[.red\\]++é++"
	raw = cached_role_end(source, 0, 0, empty_role_cursor)
	matched = pass_match(source, 0, static_view([]), False, False)
	raw.end == Ok(6) and first_bracket_end(source, 0) == Err(Missing) and matched.role == ".red\\" and matched.body == "é" and matched.used == source.count_utf8_bytes()
}

# Repeated absent role closers share the scan's raw cursor; a separate direct
# matcher starts fresh and still accepts a later independent closed role.
expect {
	prefix = "[broken pass:q"
	source = "${prefix}[unfinished"
	first = pass_match_cached(source, 0, static_view([]), False, False, 0, empty_bracket_cursor, empty_role_cursor)
	second = pass_match_cached(source.drop_prefix(prefix), 0, static_view([]), False, False, prefix.count_utf8_bytes(), first.cursor, first.role_cursor)
	independent = pass_match("[.red]++ok++", 0, static_view([]), False, False)
	first.matched.used == 0 and second.matched.used == 0 and independent.body == "ok" and independent.role == ".red"
}

# Raw-role lookups also preserve stale-found refresh, earlier query fallback,
# and relative indexes through retained views of the same immutable source.
expect {
	source = "[a] [b] [c]"
	first = cached_role_end(source, 0, 0, empty_role_cursor)
	second = cached_role_end(source, 4, 0, first.cursor)
	earlier = cached_role_end(source, 0, 0, second.cursor)
	last = cached_role_end(source, 8, 0, earlier.cursor)
	suffix = source.drop_prefix("[a] ")
	retained = cached_role_end(suffix, 0, 4, second.cursor)
	first.end == Ok(2) and second.end == Ok(6) and earlier.end == Ok(2) and last.end == Ok(10) and retained.end == Ok(2) and last.end == find(source, "]", 9) and retained.end == find(suffix, "]", 1)
}

# The legacy raw pass scanner still balances nested brackets; the raw-role
# and escaped-body cursors must never replace that separate grammar.
expect scan_view("pass:q[é [inner] tail]", static_view([]), { ..normal, quotes: False, passes_done: False }, 0) == "é [inner] tail"
expect bracket_end("[a\\]tail]", 0) == Ok(3) and first_bracket_end("[a\\]tail]", 0) == Ok(8)

# Reusing a fixed-needle cursor preserves the earliest match for retained
# suffixes, stale found positions, backwards queries and an earlier Missing.
expect {
	source = "é]] x]] tail"
	first = cached_literal_end(source, "]]", 0, 0, empty_bracket_cursor)
	second = cached_literal_end(source, "]]", 4, 0, first.cursor)
	earlier = cached_literal_end(source, "]]", 0, 0, second.cursor)
	missing = cached_literal_end(source, "]]", source.count_utf8_bytes(), 0, second.cursor)
	backwards = cached_literal_end(source, "]]", 0, 0, missing.cursor)
	suffix = source.drop_prefix("é]] ")
	retained = cached_literal_end(suffix, "]]", 0, 5, second.cursor)
	first.end == Ok(2) and second.end == Ok(6) and earlier.end == Ok(2) and backwards.end == Ok(2) and missing.end == Err(Missing) and retained.end == Ok(1) and retained.end == find(suffix, "]]", 0)
}

# A newline invalidates the earlier anchor, while a later anchor in the same
# scan remains valid. The cached newline is not a permanent rejection flag.
expect AsciiDocInline.render("[[bad\nid]] [[ok]]", []) == "[[bad\nid]] <a id=\"ok\"></a>"
expect AsciiDocInline.render("((é)) (((hidden))) <<target,label>>", []) == "é  <a href=\"#target\">label</a>"

# Exact ordinary aliases borrow the stored value without reescaping it.
expect {
	attrs = [{ name: "leaf", value: "value" }, { name: "empty", value: "" }, { name: "unicode", value: "Roc café 東京" }, { name: "lt", value: "literal" }]
	store = AsciiDocSource.new("simple-definition", attrs)
	AsciiDocInline.attribute_value_source("{leaf}", store) == "value" and AsciiDocInline.attribute_value_source("{empty}", store) == "" and AsciiDocInline.attribute_value_source("{unicode}", store) == "Roc café 東京" and AsciiDocInline.attribute_value_source("{lt}", store) == "literal"
}

# List-backed canonical rendering is an independent fallback reference; this
# overload does not use the new persistent-store shortcut.
expect {
	attrs = [{ name: "leaf", value: "value" }, { name: "empty", value: "" }, { name: "nested", value: "{leaf}" }, { name: "markup", value: "<b> &amp;" }, { name: "escaped", value: "\\{leaf}" }, { name: "passleaf", value: "pass:q[*bold*]" }, { name: "control", value: "\u(1)0\u(1)" }, { name: "whitespace", value: "  value  " }, { name: "tab", value: "a\tb" }, { name: "line", value: "a\nb" }]
	store = AsciiDocSource.new("simple-definition-fallback", attrs)
	inputs = ["{leaf}", "{empty}", "{LEAF}", "{unknown}", "{lt}", "{blank}", "{nested}", "{markup}", "{escaped}", "{passleaf}", "{control}", "{whitespace}", "{tab}", "{line}", "\\{leaf}", "{{leaf}}", "{leaf", "{leaf} suffix", "prefix {leaf}", "{counter:n}", "{set:leaf:new}{leaf}", "pass:q[*{leaf}*]", "pass:a[{leaf}]"]
	inputs.all(|input| AsciiDocInline.attribute_value_source(input, store) == AsciiDocInline.attribute_value(input, attrs))
}

expect {
	long = "${Str.join_with(List.repeat("a", 4095), "")}é"
	attrs = [{ name: "long", value: long }]
	store = AsciiDocSource.new("simple-definition-bounds", attrs)
	actual = AsciiDocInline.attribute_value_source("{long}", store)
	actual.to_utf8().len() == 4095 and actual == AsciiDocInline.attribute_value("{long}", attrs)
}

# Attribute-only insertion preserves protected-looking literal values. Later
# special-character stages still run in their declared order.
expect {
	values = ["a+", "<b>&+", "footnote:[literal]+", "{counter:n}+", "\u(1)0\u(1)+"]
	values.all(
		|value| {
			attrs = [{ name: "leaf", value }]
			AsciiDocInline.render_with("{leaf}", attrs, { subs: "attributes" }) == value and AsciiDocInline.render_with("{leaf}", attrs, { subs: "attributes,specialcharacters" }) == escape(value) and AsciiDocInline.render_with("{leaf}", attrs, { subs: "specialcharacters,attributes" }) == value
		},
	)
}

# Literal private-slot bytes survive carried dynamic state and a lone plus.
expect {
	first = AsciiDocInline.render_state("{set:x:\u(1)0\u(1)<b>}", [], { subs: "attributes" }, AsciiDocInline.initial_state)
	literal = AsciiDocInline.render_state("Lead {x} + tail", [], { subs: "normal" }, first.state)
	linebreak = AsciiDocInline.render_state("{x} +", [], { subs: "normal" }, first.state)
	literal.html == "Lead \u(1)0\u(1)<b> + tail" and linebreak.html == "\u(1)0\u(1)<b><br>"
}

# Inserted plain values stay literal after quote and pass extraction.
expect {
	values = ["a+", "a++b", "café+", "x>+", "*word*+", "`code`+"]
	values.all(|value| AsciiDocInline.render_with("{leaf}", [{ name: "leaf", value }], { subs: "normal" }) == value)
}
