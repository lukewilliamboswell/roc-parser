import AsciiDocSource

## Internal indexes for immutable preprocessing snapshots. Public AST nodes
## keep their original source bindings and small block metadata overlays.
AsciiDocAttributes :: [].{
	Attribute : { name : Str, value : Str }

	## Ordered attributes share one immutable preprocessing snapshot. Explicit
	## materialization is the boundary that creates a flat semantic list.
	Attributes :: [Header({ body : Attributes, source : Str, initial : Attributes, final : Attributes }), Flat(List(Attribute)), Shared({ snapshot : SourceSnapshot, before : List(Attribute), after : List(Attribute) })].{
		is_eq : Attributes, Attributes -> Bool
		is_eq = |a, b| AsciiDocAttributes.materialize(a) == AsciiDocAttributes.materialize(b)

		concat : Attributes, Attributes -> Attributes
		concat = |left, right| match (left, right) {
			(Flat(a), Flat(b)) => Flat(if (a.is_empty()) b else if (b.is_empty()) a else a.concat(b))
			(Flat(a), Shared(b)) => Shared({ ..b, before: if (a.is_empty()) b.before else a.concat(b.before) })
			(Shared(a), Flat(b)) => Shared({ ..a, after: if (b.is_empty()) a.after else if (a.after.is_empty()) b else a.after.concat(b) })
			_ => Flat(AsciiDocAttributes.materialize(left).concat(AsciiDocAttributes.materialize(right)))
		}
		append : Attributes, Attribute -> Attributes
		append = |attrs, attr| attrs.concat(Flat([attr]))
		is_empty : Attributes -> Bool
		is_empty = |attrs| match attrs {
			Header(_) => False
			Flat(a) => a.is_empty()
			Shared(a) => a.before.is_empty() and a.after.is_empty() and snapshot_count(a.snapshot) == 0
		}
		len : Attributes -> U64
		len = |attrs| match attrs {
			Header(h) => h.body.len() + 1 + h.initial.len() + h.final.len()
			Flat(a) => a.len()
			Shared(s) => snapshot_count(s.snapshot) + s.before.len() + s.after.len()
		}
		get : Attributes, U64 -> Try(Attribute, [OutOfBounds])
		get = |attrs, index| AsciiDocAttributes.materialize(attrs).get(index)
		map : Attributes, (Attribute -> Attribute) -> Attributes
		map = |attrs, f| Flat(AsciiDocAttributes.materialize(attrs).map(f))
		keep_if : Attributes, (Attribute -> Bool) -> Attributes
		keep_if = |attrs, f| Flat(AsciiDocAttributes.materialize(attrs).keep_if(f))
		all : Attributes, (Attribute -> Bool) -> Bool
		all = |attrs, f| AsciiDocAttributes.materialize(attrs).all(f)
	}

	## Only canonical, collision-free scopes carry typed header snapshots.
	## Arbitrary legacy/manual attributes keep the serialized behavior.
	header : Attributes, Str, Attributes, Attributes -> Attributes
	header = |body, source, initial, final| {
		body_view = indexed_attributes(body, empty).view
		initial_view = indexed_attributes(initial, empty).view
		final_view = indexed_attributes(final, empty).view
		collision = if (body.is_empty()) False else if (body_view.indexed) body_view.semantic.any(|a| header_name(a.name)) or body_view.before.any(|a| header_name(a.name)) or body_view.after.any(|a| header_name(a.name)) or AsciiDocSource.has_header_names(body_view.first) else True
		if collision or !initial_view.indexed or !final_view.indexed {
			body.append({ name: "__header-source", value: source }).concat(initial.map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(final.map(|a| { name: "__header-final:${a.name}", value: a.value }))
		} else {
			Header({ body, source, initial, final })
		}
	}

	header_parts : Attributes -> Try({ body : Attributes, source : Str, initial : Attributes, final : Attributes }, [Missing])
	header_parts = |attrs| match attrs {
		Header(h) => Ok(h)
		_ => Err(Missing)
	}

	without_header : Attributes -> Attributes
	without_header = |attrs| match attrs {
		Header(h) => h.body
		_ => attrs.keep_if(|a| !header_name(a.name))
	}

	flat : List(Attribute) -> Attributes
	flat = |values| Flat(values)

	from_source : List(Attribute) -> Attributes
	from_source = |source| from_source_cached(source, empty).attrs

	from_source_cached : List(Attribute), Cache -> { attrs : Attributes, cache : Cache }
	from_source_cached = |source, cache| {
		indexed_source = indexed(source, cache)
		semantic = if (indexed_source.view.indexed and indexed_source.view.before.is_empty() and indexed_source.view.after.is_empty()) indexed_source.view.semantic else source.keep_if(|a| a.name != "\u(0)roc-source-snapshot")
		{ attrs: Shared({ snapshot: SourceSnapshot([{ semantic, view: indexed_source.view }]), before: [], after: [] }), cache: indexed_source.cache }
	}

	## Only the compiler-fixed standard bindings use this bounded source.
	## Their unique names and control-free values are guarded at the caller.
	## Its separate exact namespace cannot collide with parsed source keys or
	## authenticated length-framed legacy snapshot tokens.
	standard_scope : List(Attribute) -> Attributes
	standard_scope = |semantic| {
		first = AsciiDocSource.from_ordered(semantic, True)
		last = AsciiDocSource.from_ordered(semantic, False)
		view = { indexed: True, first, last, semantic, key: "\u(0)roc-asciidoc-standard-v1", generation: 0, prefix: "\u(1)", before: [], after: [] }
		Shared({ snapshot: SourceSnapshot([{ semantic, view }]), before: [], after: [] })
	}

	from_store : AsciiDocSource.Store -> Attributes
	from_store = |store| Shared({ snapshot: Stored([store]), before: [], after: [] })

	## Recover the immutable source at this block's origin, without local
	## metadata. Flat/manual blocks use their exact legacy inheritance path.
	source_only : Attributes -> Try(Attributes, [Missing])
	source_only = |attrs| match attrs {
		Header(h) => source_only(h.body)
		Shared(s) => Ok(Shared({ ..s, before: [], after: [] }))
		Flat(_) => Err(Missing)
	}

	## Fast exact binding-set proof for bare parsed scopes. Small local
	## overlays and manual scopes retain the conservative semantic fallback.
	matches_baseline : Attributes, List(Attribute) -> Bool
	matches_baseline = |attrs, expected| match attrs {
		Shared({ snapshot: Stored(store), before: [], after: [] }) => AsciiDocSource.matches_baseline(stored_snapshot(store), expected)
		_ => False
	}

	classify_baseline : Attributes, List(Attribute) -> Try(Bool, [Unknown])
	classify_baseline = |attrs, expected| match attrs {
		Shared({ snapshot: Stored(store), before: [], after: [] }) => AsciiDocSource.classify_baseline(stored_snapshot(store), expected)
		_ => Err(Unknown)
	}

	with_after : View, List(Attribute) -> View
	with_after = |view, additions| {
		after = if (view.after.is_empty()) additions else if (additions.is_empty()) view.after else view.after.concat(additions)
		base_run = if (view.prefix == "") 0 else view.prefix.count_utf8_bytes() - 1
		run = longest_run(additions, base_run)
		prefix = if (run == base_run) view.prefix else Str.from_utf8_lossy(List.repeat(1.U8, run + 1))
		{ ..view, after, prefix }
	}

	materialize : Attributes -> List(Attribute)
	materialize = |attrs| match attrs {
		Header(h) => materialize(h.body).append({ name: "__header-source", value: h.source }).concat(materialize(h.initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(materialize(h.final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
		Flat(values) => values.keep_if(|a| a.name != "\u(0)roc-source-snapshot")
		Shared(s) => {
			values = snapshot_values(s.snapshot)
			leading = if (s.before.is_empty()) values else s.before.concat(values)
			if (s.after.is_empty()) leading else leading.concat(s.after)
		}
	}

	indexed_attributes : Attributes, Cache -> { view : View, cache : Cache }
	indexed_attributes = |attrs, cache| match attrs {
		Header(h) => {
			body = indexed_attributes(h.body, cache).view
			initial = indexed_attributes(h.initial, cache).view
			final = indexed_attributes(h.final, cache).view
			source = AsciiDocSource.from_ordered([{ name: "__header-source", value: h.source }], True)
			initial_final = if (initial.prefix.count_utf8_bytes() > final.prefix.count_utf8_bytes()) initial.prefix.count_utf8_bytes() else final.prefix.count_utf8_bytes()
			base_run = (if (body.prefix.count_utf8_bytes() > initial_final) body.prefix.count_utf8_bytes() else initial_final) - 1
			prefix = certificate([{ name: "", value: h.source }], base_run)
			first = AsciiDocSource.compose([{ prefix: "", reader: view_reader(body, True) }, { prefix: "", reader: source }, { prefix: "__header-initial:", reader: view_reader(initial, True) }, { prefix: "__header-final:", reader: view_reader(final, True) }], True, prefix)
			last = AsciiDocSource.compose([{ prefix: "", reader: view_reader(body, False) }, { prefix: "", reader: source }, { prefix: "__header-initial:", reader: view_reader(initial, False) }, { prefix: "__header-final:", reader: view_reader(final, False) }], False, prefix)
			{ view: { indexed: True, first, last, semantic: [], key: "", generation: 0, prefix, before: [], after: [] }, cache }
		}
		Flat(_) => { view: { indexed: False, first: AsciiDocSource.from_dict(Dict.empty()), last: AsciiDocSource.from_dict(Dict.empty()), semantic: materialize(attrs), key: "", generation: 0, prefix: "", before: [], after: [] }, cache }
		Shared(s) => {
			base = { view: snapshot_view(s.snapshot), cache }
			if !base.view.indexed {
				return indexed(materialize(attrs), cache)
			}
			before = if (s.before.is_empty()) base.view.before else s.before.concat(base.view.before)
			after = if (s.after.is_empty()) base.view.after else if (base.view.after.is_empty()) s.after else base.view.after.concat(s.after)
			base_run = base.view.prefix.count_utf8_bytes() - 1
			run = longest_run(after, longest_run(before, base_run))
			prefix = if (run == base_run) base.view.prefix else Str.from_utf8_lossy(List.repeat(1.U8, run + 1))
			{ view: { ..base.view, before, after, prefix }, cache: base.cache }
		}
	}

	lookup_attributes : Attributes, Str -> Try(Str, [Missing])
	lookup_attributes = |attrs, name| match attrs {
		Header(h) => {
			if name == "__header-source" {
				return Ok(h.source)
			}
			if name.starts_with("__header-initial:") {
				return lookup_attributes(h.initial, Str.from_utf8_lossy(name.to_utf8().drop_first(17)))
			}
			if name.starts_with("__header-final:") {
				return lookup_attributes(h.final, Str.from_utf8_lossy(name.to_utf8().drop_first(15)))
			}
			lookup_attributes(h.body, name)
		}
		Flat(values) => last_in(values, name)
		Shared(s) => {
			match last_in(s.after, name) {
				Ok(value) => return Ok(value)
				Err(_) => {}
			}
			match snapshot_lookup_last(s.snapshot, name) {
				Ok(value) => return Ok(value)
				Err(_) => {}
			}
			last_in(s.before, name)
		}
	}

	Cache : { token : Str, first : Dict(Str, Str), last : Dict(Str, Str), semantic : List(Attribute), prefix : Str }
	View : { indexed : Bool, first : AsciiDocSource.Index, last : AsciiDocSource.Index, semantic : List(Attribute), key : Str, generation : U64, prefix : Str, before : List(Attribute), after : List(Attribute) }
	empty : Cache
	empty = { token: "", first: Dict.empty(), last: Dict.empty(), semantic: [], prefix: "" }

	## Attach a fresh exact source token to semantic attributes. Callers have
	## already removed any prior carrier when deriving a changed snapshot.
	with_snapshot : List(Attribute) -> List(Attribute)
	with_snapshot = |attrs| {
		var $token = "${attrs.len().to_str()}:"
		for attr in attrs {
			$token = $token.concat("${attr.name.count_utf8_bytes().to_str()}:${attr.name}${attr.value.count_utf8_bytes().to_str()}:${attr.value}")
		}
		attrs.append({ name: "\u(0)roc-source-snapshot", value: $token })
	}

	indexed : List(Attribute), Cache -> { view : View, cache : Cache }
	indexed = |attrs, cache| {
		fallback = { indexed: False, first: AsciiDocSource.from_dict(Dict.empty()), last: AsciiDocSource.from_dict(Dict.empty()), semantic: attrs, key: "", generation: 0, prefix: "", before: [], after: [] }
		match snapshot(attrs) {
			Err(_) => { view: fallback, cache }
			Ok(s) => {
				var $base = cache
				if cache.token != s.token {
					# Source snapshots are immutable. Authenticate a previously unseen
					# canonical token, then reuse only its source index across overlays.
					var $canonical = "${s.source.len().to_str()}:"
					var $first = Dict.empty()
					var $last = Dict.empty()
					for attr in s.source {
						$canonical = $canonical.concat("${attr.name.count_utf8_bytes().to_str()}:${attr.name}${attr.value.count_utf8_bytes().to_str()}:${attr.value}")
						if !$first.contains(attr.name) {
							$first = $first.insert(attr.name, attr.value)
						}
						$last = $last.insert(attr.name, attr.value)
					}
					if $canonical != s.token {
						return { view: fallback, cache }
					}
					$base = { token: s.token, first: $first, last: $last, semantic: s.source, prefix: certificate(s.source, 0) }
				}
				base_run = $base.prefix.count_utf8_bytes() - 1
				overlay_run = longest_run(s.after, longest_run(s.before, base_run))
				prefix = if (overlay_run == base_run) $base.prefix else Str.from_utf8_lossy(List.repeat(1.U8, overlay_run + 1))
				{ view: { indexed: True, first: AsciiDocSource.from_dict($base.first), last: AsciiDocSource.from_dict($base.last), semantic: $base.semantic, key: $base.token, generation: 0, prefix, before: s.before, after: s.after }, cache: $base }
			}
		}
	}

	lookup_first : View, Str -> Try(Str, [Missing])
	lookup_first = |view, name| {
		if !view.indexed {
			return first_in(view.semantic, name)
		}
		match first_in(view.before, name) {
			Ok(value) => return Ok(value)
			Err(_) => {}
		}
		match view.first.get(name) {
			Ok(value) => return Ok(value)
			Err(_) => {}
		}
		first_in(view.after, name)
	}

	lookup_last : View, Str -> Try(Str, [Missing])
	lookup_last = |view, name| {
		if !view.indexed {
			return last_in(view.semantic, name)
		}
		match last_in(view.after, name) {
			Ok(value) => return Ok(value)
			Err(_) => {}
		}
		match view.last.get(name) {
			Ok(value) => return Ok(value)
			Err(_) => {}
		}
		last_in(view.before, name)
	}

	same_snapshot : List(Attribute), List(Attribute) -> Bool
	same_snapshot = |a, b| match (snapshot(a), snapshot(b)) {
		(Ok(left), Ok(right)) => left.token == right.token and left.before == right.before and left.after == right.after
		_ => False
	}
}

# Both alternatives share one immutable handle. Keeping either large payload
# inline would pad every public Attributes value, and therefore every source line.
LegacySnapshot : { semantic : List(AsciiDocAttributes.Attribute), view : AsciiDocAttributes.View }

SourceSnapshot := [SourceSnapshot(List(LegacySnapshot)), Stored(List(AsciiDocSource.Store))]

# These constructors are private and always create exactly one payload. An
# invalid internal handle must fail explicitly rather than invent missing data.
legacy_snapshot : List(LegacySnapshot) -> LegacySnapshot
legacy_snapshot = |values| match values {
	[value] => value
	_ => crash "invalid internal AsciiDoc legacy snapshot handle"
}

stored_snapshot : List(AsciiDocSource.Store) -> AsciiDocSource.Store
stored_snapshot = |values| match values {
	[value] => value
	_ => crash "invalid internal AsciiDoc stored snapshot handle"
}

snapshot_values : SourceSnapshot -> List(AsciiDocAttributes.Attribute)
snapshot_values = |snapshot_value| match snapshot_value {
	SourceSnapshot(handle) => legacy_snapshot(handle).semantic
	Stored(handle) => AsciiDocSource.materialize(stored_snapshot(handle))
}

snapshot_count : SourceSnapshot -> U64
snapshot_count = |snapshot_value| match snapshot_value {
	SourceSnapshot(handle) => legacy_snapshot(handle).semantic.len()
	Stored(handle) => AsciiDocSource.len(stored_snapshot(handle))
}

snapshot_view : SourceSnapshot -> AsciiDocAttributes.View
snapshot_view = |snapshot_value| match snapshot_value {
	SourceSnapshot(handle) => legacy_snapshot(handle).view
	Stored(handle) => {
		store = stored_snapshot(handle)
		key = AsciiDocSource.key(store)
		reader = AsciiDocSource.index(store)
		{ indexed: True, first: reader, last: reader, semantic: [], key: key.namespace, generation: key.generation, prefix: AsciiDocSource.prefix(store), before: [], after: [] }
	}
}

snapshot_lookup_last : SourceSnapshot, Str -> Try(Str, [Missing])
snapshot_lookup_last = |snapshot_value, name| match snapshot_value {
	SourceSnapshot(handle) => {
		s = legacy_snapshot(handle)
		if (s.view.indexed) AsciiDocAttributes.lookup_last(s.view, name) else last_in(s.semantic, name)
	}
	Stored(handle) => AsciiDocSource.lookup(stored_snapshot(handle), name)
}

first_in : List(AsciiDocAttributes.Attribute), Str -> Try(Str, [Missing])
first_in = |attrs, name| {
	for attr in attrs {
		if attr.name == name {
			return Ok(attr.value)
		}
	}
	Err(Missing)
}

last_in : List(AsciiDocAttributes.Attribute), Str -> Try(Str, [Missing])
last_in = |attrs, name| {
	var $position = attrs.len()
	while $position > 0 {
		$position = $position - 1
		match attrs.get($position) {
			Ok(attr) if attr.name == name => return Ok(attr.value)
			_ => {}
		}
	}
	Err(Missing)
}

certificate : List(AsciiDocAttributes.Attribute), U64 -> Str
certificate = |attrs, initial_run| Str.from_utf8_lossy(List.repeat(1.U8, longest_run(attrs, initial_run) + 1))

longest_run : List(AsciiDocAttributes.Attribute), U64 -> U64
longest_run = |attrs, initial_run| {
	var $longest = initial_run
	for attr in attrs {
		var $run = 0.U64
		for byte in attr.value.to_utf8() {
			if byte == 1 {
				$run = $run + 1
				if $run > $longest {
					$longest = $run
				}
			} else {
				$run = 0
			}
		}
	}
	$longest
}

Snapshot : { token : Str, source : List(AsciiDocAttributes.Attribute), before : List(AsciiDocAttributes.Attribute), after : List(AsciiDocAttributes.Attribute) }

snapshot : List(AsciiDocAttributes.Attribute) -> Try(Snapshot, [Missing])
snapshot = |attrs| {
	var $position = attrs.len()
	var $checked = 0.U64
	# Generated overlays are small. Arbitrary caller ASTs without a carrier
	# retain the conservative lookup path, without a whole-list search here.
	while $position > 0 and $checked < 16 {
		$position = $position - 1
		$checked = $checked + 1
		item = attrs.get($position) ?? { name: "", value: "" }
		if item.name == "\u(0)roc-source-snapshot" {
			bytes = item.value.to_utf8()
			var $end = 0.U64
			while $end < bytes.len() and (bytes.get($end) ?? 0) >= '0' and (bytes.get($end) ?? 255) <= '9' {
				$end = $end + 1
			}
			if bytes.get($end) != Ok(':') {
				return Err(Missing)
			}
			count = U64.from_str(Str.from_utf8_lossy(bytes.take_first($end))) ?? (attrs.len() + 1)
			if count > $position {
				return Err(Missing)
			}
			return Ok({ token: item.value, source: attrs.sublist({ start: $position - count, len: count }), before: attrs.take_first($position - count), after: attrs.drop_first($position + 1) })
		}
	}
	Err(Missing)
}

expect {
	original = [{ name: "x", value: "first" }, { name: "x", value: "last" }]
	attrs = [{ name: "x", value: "local" }].concat(original).append({ name: "\u(0)roc-source-snapshot", value: "2:1:x5:first1:x4:last" }).append({ name: "role", value: "red" })
	indexed = AsciiDocAttributes.indexed(attrs, AsciiDocAttributes.empty)
	indexed.view.indexed and AsciiDocAttributes.lookup_first(indexed.view, "x") == Ok("local") and AsciiDocAttributes.lookup_last(indexed.view, "x") == Ok("last") and AsciiDocAttributes.lookup_last(indexed.view, "role") == Ok("red") and indexed.view.semantic.len() == 2 and !indexed.view.first.contains("\u(0)roc-source-snapshot")
}

expect {
	source = [{ name: "x", value: "one" }, { name: "\u(0)roc-source-snapshot", value: "1:1:x3:one" }]
	first = AsciiDocAttributes.indexed(source, AsciiDocAttributes.empty)
	changed = AsciiDocAttributes.indexed(source.append({ name: "x", value: "two" }), first.cache)
	unchanged = AsciiDocAttributes.indexed(source.append({ name: "x", value: "two" }), changed.cache)
	first.view.last.get("x") == Ok("one") and AsciiDocAttributes.lookup_last(changed.view, "x") == Ok("two") and changed.view.first.get("x") == Ok("one") and first.view.key == changed.view.key and changed.view.after == unchanged.view.after and changed.cache.semantic == first.cache.semantic
}

expect {
	bad = [{ name: "\u(0)roc-source-snapshot", value: "99:" }]
	!AsciiDocAttributes.indexed(bad, AsciiDocAttributes.empty).view.indexed
}

expect {
	forged = [{ name: "x", value: "changed" }, { name: "\u(0)roc-source-snapshot", value: "1:1:x3:old" }]
	!AsciiDocAttributes.indexed(forged, AsciiDocAttributes.empty).view.indexed
}

expect {
	value = "A${Str.from_utf8_lossy(List.repeat(1.U8, 10000))}B"
	token = "1:1:x${value.count_utf8_bytes().to_str()}:${value}"
	view = AsciiDocAttributes.indexed([{ name: "x", value }, { name: "\u(0)roc-source-snapshot", value: token }], AsciiDocAttributes.empty).view
	view.indexed and view.prefix.count_utf8_bytes() == 10001
}

expect {
	attrs = [{ name: "x", value: "first" }, { name: "x", value: "last" }]
	indexed = AsciiDocAttributes.indexed(AsciiDocAttributes.with_snapshot(attrs), AsciiDocAttributes.empty)
	indexed.view.indexed and indexed.view.semantic == attrs and indexed.view.first.get("x") == Ok("first") and AsciiDocAttributes.lookup_last(indexed.view, "x") == Ok("last")
}

expect {
	base = AsciiDocAttributes.with_snapshot([{ name: "x", value: "source-first" }, { name: "x", value: "source-last" }, { name: "empty", value: "" }])
	initial = AsciiDocAttributes.indexed(base, AsciiDocAttributes.empty)
	local = [{ name: "x", value: "before-first" }, { name: "x", value: "before-last" }].concat(base).concat([{ name: "x", value: "after-first" }, { name: "x", value: "after-last" }])
	layered = AsciiDocAttributes.indexed(local, initial.cache)
	AsciiDocAttributes.lookup_first(layered.view, "x") == Ok("before-first") and AsciiDocAttributes.lookup_last(layered.view, "x") == Ok("after-last") and AsciiDocAttributes.lookup_first(layered.view, "empty") == Ok("") and AsciiDocAttributes.lookup_last(layered.view, "absent") == Err(Missing) and layered.cache == initial.cache and layered.view.semantic == initial.view.semantic
}

expect {
	control = Str.from_utf8_lossy(List.repeat(1.U8, 5))
	base = AsciiDocAttributes.with_snapshot([{ name: "x", value: control }])
	initial = AsciiDocAttributes.indexed(base, AsciiDocAttributes.empty)
	shadowed = AsciiDocAttributes.indexed([{ name: "x", value: "shadow" }].concat(base).append({ name: "x", value: "last" }), initial.cache)
	longer = AsciiDocAttributes.indexed(base.append({ name: "local", value: "${control}${control}" }), shadowed.cache)
	shadowed.view.prefix.count_utf8_bytes() == 6 and longer.view.prefix.count_utf8_bytes() == 11 and longer.cache.prefix.count_utf8_bytes() == 6 and shadowed.cache == longer.cache
}

expect {
	base = [{ name: "x", value: "first" }, { name: "x", value: "last" }, { name: "empty", value: "" }]
	shared_scope = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot(base))
	leading = AsciiDocAttributes.flat([{ name: "x", value: "local-first" }])
	trailing = AsciiDocAttributes.flat([{ name: "x", value: "local-last" }])
	attrs = leading.concat(shared_scope).concat(trailing)
	view = AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view
	AsciiDocAttributes.materialize(attrs) == [{ name: "x", value: "local-first" }].concat(base).append({ name: "x", value: "local-last" }) and AsciiDocAttributes.lookup_first(view, "x") == Ok("local-first") and AsciiDocAttributes.lookup_last(view, "x") == Ok("local-last") and AsciiDocAttributes.lookup_first(view, "empty") == Ok("") and attrs == AsciiDocAttributes.flat(AsciiDocAttributes.materialize(attrs))
}

expect {
	base = AsciiDocAttributes.with_snapshot([{ name: "caption", value: "Shared" }])
	scoped = AsciiDocAttributes.from_source_cached(base, AsciiDocAttributes.empty)
	first = scoped.attrs.append({ name: "alt", value: "First" })
	second = scoped.attrs.append({ name: "alt", value: "Second" })
	left = AsciiDocAttributes.indexed_attributes(first, AsciiDocAttributes.empty)
	right = AsciiDocAttributes.indexed_attributes(second, AsciiDocAttributes.empty)
	left.view.semantic == right.view.semantic and left.view.first == right.view.first and left.view.key == right.view.key and left.view.after != right.view.after and AsciiDocAttributes.lookup_first(left.view, "alt") == Ok("First") and AsciiDocAttributes.lookup_first(right.view, "alt") == Ok("Second")
}

expect {
	base = [{ name: "x", value: "source" }]
	input = [{ name: "x", value: "inner-before" }].concat(AsciiDocAttributes.with_snapshot(base)).append({ name: "x", value: "inner-after" })
	scope = AsciiDocAttributes.from_source(input).append({ name: "role", value: "outer" })
	view = AsciiDocAttributes.indexed_attributes(scope, AsciiDocAttributes.empty).view
	AsciiDocAttributes.materialize(scope) == [{ name: "x", value: "inner-before" }, { name: "x", value: "source" }, { name: "x", value: "inner-after" }, { name: "role", value: "outer" }] and AsciiDocAttributes.lookup_attributes(scope, "x") == Ok("inner-after") and AsciiDocAttributes.lookup_first(view, "x") == Ok("inner-before") and AsciiDocAttributes.lookup_last(view, "role") == Ok("outer")
}

expect {
	valid = AsciiDocAttributes.with_snapshot([{ name: "x", value: "original" }])
	cache = AsciiDocAttributes.indexed(valid, AsciiDocAttributes.empty).cache
	forged = AsciiDocAttributes.flat([{ name: "x", value: "changed" }, { name: "\u(0)roc-source-snapshot", value: cache.token }])
	view = AsciiDocAttributes.indexed_attributes(forged, cache).view
	!view.indexed and AsciiDocAttributes.lookup_attributes(forged, "x") == Ok("changed") and view.semantic == [{ name: "x", value: "changed" }]
}

header_name : Str -> Bool
header_name = |name| name == "__header-source" or name.starts_with("__header-initial:") or name.starts_with("__header-final:")

view_reader : AsciiDocAttributes.View, Bool -> AsciiDocSource.Index
view_reader = |view, first| {
	reader = if (!AsciiDocSource.is_persistent(view.first) and !view.semantic.is_empty()) AsciiDocSource.from_ordered(view.semantic, first) else if (first) view.first else view.last
	if view.before.is_empty() and view.after.is_empty() {
		return reader
	}
	AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.from_ordered(view.before, first) }, { prefix: "", reader }, { prefix: "", reader: AsciiDocSource.from_ordered(view.after, first) }], first, view.prefix)
}

# The opaque carrier is publicly identical to the old serialized attributes,
# including duplicate order and nested prefix names. Identification can look
# up every synthetic binding without materializing this list.
expect {
	body = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot([{ name: "x", value: "body-first" }, { name: "x", value: "body-last" }])).append({ name: "role", value: "local" })
	initial = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot([{ name: "x", value: "initial-first" }, { name: "x", value: "initial-last" }, { name: "__header-final:x", value: "nested" }]))
	final = AsciiDocAttributes.from_source(AsciiDocAttributes.with_snapshot([{ name: "x", value: "final-first" }, { name: "x", value: "final-last" }, { name: "empty", value: "" }]))
	attrs = AsciiDocAttributes.header(body, "{x}", initial, final)
	expected = AsciiDocAttributes.materialize(body).append({ name: "__header-source", value: "{x}" }).concat(AsciiDocAttributes.materialize(initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(AsciiDocAttributes.materialize(final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
	view = AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view
	AsciiDocAttributes.header_parts(attrs).is_ok() and AsciiDocAttributes.materialize(attrs) == expected and attrs.len() == expected.len() and AsciiDocSource.semantic(view.first, []) == expected and AsciiDocSource.semantic(view.last, []) == expected and AsciiDocAttributes.lookup_first(view, "__header-initial:x") == Ok("initial-first") and AsciiDocAttributes.lookup_last(view, "__header-initial:x") == Ok("initial-last") and AsciiDocAttributes.lookup_first(view, "__header-final:x") == Ok("final-first") and AsciiDocAttributes.lookup_last(view, "__header-final:x") == Ok("final-last") and AsciiDocAttributes.lookup_first(view, "__header-initial:__header-final:x") == Ok("nested") and AsciiDocAttributes.lookup_attributes(attrs, "__header-final:empty") == Ok("") and AsciiDocAttributes.without_header(attrs) == body and AsciiDocAttributes.source_only(attrs) == AsciiDocAttributes.source_only(body)
}

expect {
	source = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("header-test", [{ name: "x", value: "value" }]))
	control = Str.from_utf8_lossy(List.repeat(1.U8, 7))
	header = AsciiDocAttributes.header(source, control, source, source)
	view = AsciiDocAttributes.indexed_attributes(header, AsciiDocAttributes.empty).view
	flattened = header.map(|a| a)
	filtered = header.keep_if(|a| a.name != "x")
	appended = header.append({ name: "__header-source", value: "manual" })
	view.prefix.count_utf8_bytes() == 8 and AsciiDocAttributes.header_parts(header).is_ok() and AsciiDocAttributes.header_parts(flattened).is_err() and AsciiDocAttributes.materialize(flattened) == AsciiDocAttributes.materialize(header) and AsciiDocAttributes.materialize(filtered) == AsciiDocAttributes.materialize(header).keep_if(|a| a.name != "x") and AsciiDocAttributes.lookup_attributes(appended, "__header-source") == Ok("manual") and AsciiDocAttributes.header_parts(appended).is_err()
}

expect {
	initial = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("header-initial", [{ name: "x", value: "initial" }]))
	var $same = True
	for name in ["__header-source", "__header-initial:x", "__header-final:x", "__header-initial:__header-final:x"] {
		body = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("header-collision", [{ name, value: "body" }]))
		attrs = AsciiDocAttributes.header(body, "title", initial, initial)
		legacy = body.append({ name: "__header-source", value: "title" }).concat(initial.map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(initial.map(|a| { name: "__header-final:${a.name}", value: a.value }))
		$same = $same and AsciiDocAttributes.header_parts(attrs).is_err() and attrs == legacy
	}
	$same
}

# An empty scope has no active names to collide with the encoded header
# namespace. Both representations retain exact legacy ordering and FIRST/LAST
# semantics, including duplicate and nested reserved names in the snapshots.
expect {
	initial = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("empty-header-initial", [{ name: "x", value: "initial-first" }, { name: "__header-final:x", value: "nested" }])).append({ name: "x", value: "initial-last" })
	final = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("empty-header-final", [{ name: "x", value: "final-first" }, { name: "empty", value: "" }])).append({ name: "x", value: "final-last" })
	shared_empty = AsciiDocAttributes.from_store(AsciiDocSource.new_parsed("empty-header-body", []))
	bindings = [{ name: "__header-source", value: "{x}" }].concat(AsciiDocAttributes.materialize(initial).map(|a| { name: "__header-initial:${a.name}", value: a.value })).concat(AsciiDocAttributes.materialize(final).map(|a| { name: "__header-final:${a.name}", value: a.value }))
	var $same = True
	for body in [AsciiDocAttributes.flat([]), shared_empty] {
		attrs = AsciiDocAttributes.header(body, "{x}", initial, final)
		view = AsciiDocAttributes.indexed_attributes(attrs, AsciiDocAttributes.empty).view
		empty_part = match AsciiDocAttributes.header_parts(attrs) {
			Ok(h) => h.body.is_empty() and h.body == body
			Err(_) => False
		}
		$same = $same and empty_part and AsciiDocAttributes.materialize(attrs) == bindings and attrs.len() == bindings.len() and AsciiDocSource.semantic(view.first, []) == bindings and AsciiDocSource.semantic(view.last, []) == bindings and AsciiDocAttributes.lookup_first(view, "__header-initial:x") == Ok("initial-first") and AsciiDocAttributes.lookup_last(view, "__header-initial:x") == Ok("initial-last") and AsciiDocAttributes.lookup_first(view, "__header-final:x") == Ok("final-first") and AsciiDocAttributes.lookup_last(view, "__header-final:x") == Ok("final-last") and AsciiDocAttributes.lookup_attributes(attrs, "__header-initial:__header-final:x") == Ok("nested") and AsciiDocAttributes.without_header(attrs) == body and AsciiDocAttributes.source_only(attrs) == AsciiDocAttributes.source_only(body)
	}
	# Nonempty manual scopes, including reserved duplicates, stay conservative.
	manual = AsciiDocAttributes.flat([{ name: "__header-final:x", value: "manual-first" }, { name: "__header-final:x", value: "manual-last" }])
	manual_header = AsciiDocAttributes.header(manual, "{x}", initial, final)
	$same and AsciiDocAttributes.header_parts(manual_header).is_err() and AsciiDocAttributes.materialize(manual_header) == AsciiDocAttributes.materialize(manual).concat(bindings) and AsciiDocAttributes.header_parts(AsciiDocAttributes.header(AsciiDocAttributes.flat([]), "{x}", AsciiDocAttributes.flat([]), final)).is_err()
}
