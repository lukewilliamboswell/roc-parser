import AsciiDocDict

## Persistent ordered source bindings. Updates copy only balanced tree paths.
AsciiDocSource :: [].{
	Attribute : { name : Str, value : Str }
	Key : { namespace : Str, generation : U64 }
	standard : List(Attribute)
	standard = standard_attributes

	Store :: [Store({ names : Tree, seed_values : Try(List(Attribute), [Unknown]), baseline : Try(List(Attribute), [Unknown]), baseline_equal : Bool, tags : Tree, parsed_history : Bool, next : U64, count : U64, namespace : Str, generation : U64, prefix : Str })]
	Index :: [JoinedIndex({ parts : List({ prefix : Str, reader : Index }), first : Bool, prefix : Str }), FlatIndex(Dict(Str, Str)), OrderedIndex({ values : List(Attribute), first : Bool }), SourceIndex(Store), OverlayIndex({ first : Index, source : List(Attribute), before : List(Attribute), after : List(Attribute), values : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), appended : List(Attribute), prefix : Str, dynamic_tags_possible : Bool })].{
		is_eq : Index, Index -> Bool
		is_eq = |a, b| match (a, b) {
			(JoinedIndex(x), JoinedIndex(y)) => x == y
			(FlatIndex(x), FlatIndex(y)) => x == y
			(OrderedIndex(x), OrderedIndex(y)) => x == y
			(SourceIndex(x), SourceIndex(y)) => if store_trusted(x) and store_trusted(y) AsciiDocSource.key(x) == AsciiDocSource.key(y) else AsciiDocSource.materialize(x) == AsciiDocSource.materialize(y)
			(OverlayIndex(x), OverlayIndex(y)) => x.first == y.first and x.source == y.source and x.before == y.before and x.after == y.after and x.values == y.values and x.removed == y.removed and x.appended == y.appended and x.prefix == y.prefix and x.dynamic_tags_possible == y.dynamic_tags_possible
			_ => False
		}
		get : Index, Str -> Try(Str, [Missing])
		get = |index, name| match index {
			JoinedIndex(joined) => {
				var $position = 0.U64
				while $position < joined.parts.len() {
					position = if joined.first $position else joined.parts.len() - $position - 1
					part = joined.parts.get(position) ?? { prefix: "", reader: FlatIndex(Dict.empty()) }
					if name.starts_with(part.prefix) {
						suffix = if part.prefix == "" name else Str.from_utf8_lossy(name.to_utf8().drop_first(part.prefix.count_utf8_bytes()))
						match part.reader.get(suffix) {
							Ok(value) => return Ok(value)
							Err(_) => {}
						}
					}
					$position = $position + 1
				}
				Err(Missing)
			}
			FlatIndex(values) => values.get(name).map_err(|_| Missing)
			OrderedIndex(ordered) => if ordered.first first_attribute(ordered.values, name) else last_attribute(ordered.values, name)
			SourceIndex(store) => AsciiDocSource.lookup(store, name)
			OverlayIndex(view) => match view.values.get(name) {
				Ok(value) => Ok(value)
				Err(_) => {
					original = if view.removed.contains(name) Err(Missing) else first_overlay(view, name)
					match original {
						Ok(value) => Ok(value)
						Err(_) => first_attribute(view.appended, name)
					}
				}
			}
		}
		contains : Index, Str -> Bool
		contains = |index, name| index.get(name).is_ok()
		is_empty : Index -> Bool
		is_empty = |index| match index {
			JoinedIndex(joined) => joined.parts.all(|part| part.reader.is_empty())
			FlatIndex(values) => values.is_empty()
			OrderedIndex(ordered) => ordered.values.is_empty()
			SourceIndex(Store(s)) => s.count == 0
			OverlayIndex(view) => view.values.is_empty() and view.first.is_empty() and view.source.is_empty() and view.before.is_empty() and view.after.is_empty() and view.appended.is_empty()
		}
	}
	certificate : Index -> Str
	certificate = |reader| match reader {
		JoinedIndex(joined) => joined.prefix
		FlatIndex(_) | OrderedIndex(_) => ""
		SourceIndex(store) => prefix(store)
		OverlayIndex(view) => view.prefix
	}

	## A bounded composition keeps exact order, duplicate winners and virtual
	## name prefixes without allocating the corresponding semantic list.
	compose : List({ prefix : Str, reader : Index }), Bool, Str -> Index
	compose = |parts, first, prefix_value| JoinedIndex({ parts, first, prefix: prefix_value })

	has_header_names : Index -> Bool
	has_header_names = |reader| match reader {
		SourceIndex(Store(s)) => tree_has_header_names(s.names)
		_ => semantic(reader, []).any(|a| header_name(a.name))
	}

	from_view : { first : Index, source : List(Attribute), before : List(Attribute), after : List(Attribute), values : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), appended : List(Attribute), prefix : Str, dynamic_tags_possible : Bool } -> Index
	from_view = |view| OverlayIndex(view)

	## Bounded immutable readers avoid building maps for a fixed small source.
	from_ordered : List(Attribute), Bool -> Index
	from_ordered = |values, first| OrderedIndex({ values, first })

	from_dict : Dict(Str, Str) -> Index
	from_dict = |values| FlatIndex(values)
	index : Store -> Index
	index = |store| SourceIndex(store)

	## List-based readers retain their exact original order and duplicates.
	semantic : Index, List(Attribute) -> List(Attribute)
	semantic = |reader, fallback| match reader {
		JoinedIndex(joined) => {
			var $values = []
			for part in joined.parts {
				$values = $values.concat(semantic(part.reader, []).map(|a| { name: "${part.prefix}${a.name}", value: a.value }))
			}
			$values
		}
		FlatIndex(_) => fallback
		OrderedIndex(ordered) => ordered.values
		SourceIndex(store) => materialize(store)
		OverlayIndex(view) => {
			base = view.before.concat(semantic(view.first, view.source)).concat(view.after)
			view.values.to_list().map(|(name, value)| { name, value }).concat(base.keep_if(|a| !view.removed.contains(a.name) and !view.values.contains(a.name))).concat(view.appended)
		}
	}
	is_persistent : Index -> Bool
	is_persistent = |reader| match reader {
		SourceIndex(_) | OverlayIndex(_) | JoinedIndex(_) => True
		_ => False
	}

	tag_attributes : Index -> List(Attribute)
	tag_attributes = |index_value| match index_value {
		JoinedIndex(joined) => {
			var $tags = []
			for part in joined.parts {
				if part.prefix == "" {
					$tags = $tags.concat(tag_attributes(part.reader))
				}
			}
			$tags
		}
		FlatIndex(_) => []
		OrderedIndex(ordered) => ordered.values.keep_if(|a| a.name.starts_with("__inline-tag:"))
		SourceIndex(Store(s)) => ordered_values(s.tags, [])
		OverlayIndex(view) => {
			dynamic = if view.dynamic_tags_possible view.values.to_list().keep_if(|(name, _)| name.starts_with("__inline-tag:")).map(|(name, value)| { name, value }) else []
			base_tags = match view.first {
				OrderedIndex(_) => tag_attributes(view.first)
				_ => if is_persistent(view.first) tag_attributes(view.first) else view.source.keep_if(|a| a.name.starts_with("__inline-tag:"))
			}
			local = view.before.keep_if(|a| a.name.starts_with("__inline-tag:")).concat(base_tags).concat(view.after.keep_if(|a| a.name.starts_with("__inline-tag:")))
			dynamic.concat(local.keep_if(|a| !view.removed.contains(a.name) and !view.values.contains(a.name))).concat(view.appended.keep_if(|a| a.name.starts_with("__inline-tag:")))
		}
	}

	new : Str, List(Attribute) -> Store
	new = |namespace, attributes| {
		# Normalize once: replacing a name retains its final input position.
		var $unique = Dict.empty()
		var $position = 0.U64
		for attribute in attributes {
			bytes = attribute.name.to_utf8()
			$unique = $unique.insert(attribute.name, { name: attribute.name, name_hash: name_hash(bytes), value: attribute.value, position: $position, active: True, run: value_run(attribute.value) })
			$position = $position + 1
		}
		entries = $unique.to_list().map(|(_, entry)| entry)
		by_name = entries.sort_with(|a, b| ordering(compare_entries(a, b, False)))
		names = balanced_entries(by_name, 0, by_name.len())
		by_position = entries.sort_with(|a, b| ordering(compare_entries(a, b, True)))
		tag_entries = by_position.keep_if(|entry| entry.name.starts_with("__inline-tag:"))
		seed_values = by_position.map(|entry| { name: entry.name, value: entry.value })
		refresh_prefix(Store({ names, seed_values: Ok(seed_values), baseline: if seed_values.len() <= 64 Ok(seed_values) else Err(Unknown), baseline_equal: seed_values.len() <= 64, tags: balanced_entries(tag_entries, 0, tag_entries.len()), parsed_history: False, next: $position, count: by_name.len(), namespace, generation: 0, prefix: "\u(1)" }))
	}

	## Trusted lineage is reserved for the canonical, single threaded source
	## processor. Ordinary Store mutation deliberately invalidates that trust.
	new_parsed : Str, List(Attribute) -> Store
	new_parsed = |namespace, attributes| {
		Store(s) = new(namespace, attributes)
		Store({ ..s, parsed_history: True })
	}

	## Canonical seed only: all Tree buffers and bindings are immutable literals.
	## Dynamic namespaces keep each parsed source lineage exact and separate.
	new_standard_parsed : Str -> Store
	new_standard_parsed = |namespace| Store({ names: standard_tree, seed_values: Ok(standard_attributes), baseline: Ok(standard_attributes), baseline_equal: True, tags: Tree([]), parsed_history: True, next: 54, count: 54, namespace, generation: 0, prefix: "\u(1)" })

	## Canonical initial options share the generic builder with an exact
	## known baseline; empty options retain the immutable standard tree.
	new_standard_parsed_options : Str, List(Attribute) -> Store
	new_standard_parsed_options = |namespace, writes| {
		if writes.len() < 32 {
			var $store = new_standard_parsed(namespace)
			for attribute in writes {
				$store = set_parsed($store, attribute.name, attribute.value)
			}
			return $store
		}
		build_parsed_options(namespace, standard_attributes, writes, Ok(standard_attributes))
	}

	## Initial caller options are ordered literal writes with no observable
	## intermediate scope. Bulk construction retains each final write position,
	## while the original seed remains the baseline for later scope transitions.
	new_parsed_options : Str, List(Attribute), List(Attribute) -> Store
	new_parsed_options = |namespace, seed, writes| {
		if writes.is_empty() {
			return new_parsed(namespace, seed)
		}
		build_parsed_options(namespace, seed, writes, initial_baseline(seed))
	}

	trusted_history : Index -> Bool
	trusted_history = |reader| match reader {
		SourceIndex(store) => store_trusted(store)
		_ => False
	}
	changed_names : Index, Index -> Try(List(Str), [Unknown])
	changed_names = |previous, next| match (previous, next) {
		# Re-entering an unchanged composite scope needs no live-name scan.
		# Equality includes every ordered segment, prefix and source revision;
		# untrusted stores retain their full semantic equality check.
		(JoinedIndex(before), JoinedIndex(after)) if before == after => Ok([])
		(SourceIndex(Store(before)), SourceIndex(Store(after))) if before.parsed_history and after.parsed_history and before.namespace == after.namespace and before.generation <= after.generation => {
			Ok(unique_changed_names(changed_since(after.names, before.next, [])))
		}
		# A bounded ordered source can match the normalized immutable baseline
		# of a parsed Store. The certified proof covers every value binding;
		# actual current order remains in the Store/cache/Tag tree. Inline
		# reconciles small local overlays separately.
		(OrderedIndex(ordered), SourceIndex(Store(source))) if source.parsed_history and ordered.values.len() <= 64 => if source.baseline_equal and source.baseline == Ok(ordered.values) Ok([]) else Err(Unknown)
		(SourceIndex(Store(source)), OrderedIndex(ordered)) if source.parsed_history and ordered.values.len() <= 64 => if source.baseline_equal and source.baseline == Ok(ordered.values) Ok([]) else Err(Unknown)
		_ => Err(Unknown)
	}

	key : Store -> Key
	key = |Store(s)| { namespace: s.namespace, generation: s.generation }
	prefix : Store -> Str
	prefix = |Store(s)| s.prefix
	len : Store -> U64
	len = |Store(s)| s.count
	lookup : Store, Str -> Try(Str, [Missing])
	lookup = |Store(s), name| match find_name(s.names, name) {
		Ok(entry) if entry.active => Ok(entry.value)
		_ => Err(Missing)
	}
	materialize : Store -> List(Attribute)
	materialize = |Store(s)| match s.seed_values {
		Ok(values) => values
		Err(_) => all_entries(s.names, []).sort_with(|a, b| ordering(compare_entries(a, b, True))).map(|entry| { name: entry.name, value: entry.value })
	}

	## A complete bounded baseline is a value-binding proof, not an order cache.
	matches_baseline : Store, List(Attribute) -> Bool
	matches_baseline = |store, expected| classify_baseline(store, expected) == Ok(True)

	## Known unequal bindings need no enumeration either. An unrelated or
	## absent baseline remains Unknown for conservative caller fallbacks.
	classify_baseline : Store, List(Attribute) -> Try(Bool, [Unknown])
	classify_baseline = |Store(s), expected| if s.baseline == Ok(expected) Ok(s.baseline_equal) else Err(Unknown)

	set : Store, Str, Str -> Store
	set = |store, name, value| apply_set(store, name, value, False)
	unset : Store, Str -> Store
	unset = |store, name| apply_unset(store, name, False)
	define : Store, Str, Str, Bool -> Store
	define = |store, name, value, unset_value| apply_definition(store, name, value, unset_value, False)
	set_parsed : Store, Str, Str -> Store
	set_parsed = |store, name, value| apply_set(store, name, value, True)
	unset_parsed : Store, Str -> Store
	unset_parsed = |store, name| apply_unset(store, name, True)
	define_parsed : Store, Str, Str, Bool -> Store
	define_parsed = |store, name, value, unset_value| apply_definition(store, name, value, unset_value, True)

}

build_parsed_options : Str, List(AsciiDocSource.Attribute), List(AsciiDocSource.Attribute), Try(List(AsciiDocSource.Attribute), [Unknown]) -> AsciiDocSource.Store
build_parsed_options = |namespace, seed, writes, baseline| {
	# Final bindings need name order for lookup. Only ordered tag bindings
	# need position order here; the general ordered cache remains deferred.
	var $unique = Dict.empty()
	var $position = 0.U64
	for attribute in seed.concat(writes) {
		bytes = attribute.name.to_utf8()
		$unique = $unique.insert(attribute.name, { name: attribute.name, name_hash: name_hash(bytes), value: attribute.value, position: $position, active: True, run: value_run(attribute.value) })
		$position = $position + 1
	}
	entries = $unique.to_list().map(|(_, entry)| entry)
	by_name = entries.sort_with(|a, b| ordering(compare_entries(a, b, False)))
	names = balanced_entries(by_name, 0, by_name.len())
	tag_entries = entries.keep_if(|entry| entry.name.starts_with("__inline-tag:")).sort_with(|a, b| ordering(compare_entries(a, b, True)))
	certify_baseline(refresh_prefix(Store({ names, seed_values: Err(Unknown), baseline, baseline_equal: False, tags: balanced_entries(tag_entries, 0, tag_entries.len()), parsed_history: True, next: $position, count: by_name.len(), namespace, generation: writes.len(), prefix: "\u(1)" })))
}

# The baseline witness is bounded by unique names, not input length. Reading
# final occurrences backwards preserves exactly the normal constructor's
# last-write ordering without allocating a discarded initial source tree.
initial_baseline : List(AsciiDocSource.Attribute) -> Try(List(AsciiDocSource.Attribute), [Unknown])
initial_baseline = |seed| {
	var $reversed = []
	var $position = seed.len()
	while $position > 0 {
		$position = $position - 1
		attribute = seed.get($position) ?? { name: "", value: "" }
		if !$reversed.any(|previous| previous.name == attribute.name) {
			if $reversed.len() == 64 {
				return Err(Unknown)
			}
			$reversed = $reversed.append(attribute)
		}
	}
	var $normalized = []
	$position = $reversed.len()
	while $position > 0 {
		$position = $position - 1
		$normalized = $normalized.append($reversed.get($position) ?? { name: "", value: "" })
	}
	Ok($normalized)
}

# Ordinary block transitions change one binding and its revision. Keep the
# bounded case allocation-free apart from its result; larger batches retain
# the exact Dict-based order/deduplication algorithm.
unique_changed_names : List(Str) -> List(Str)
unique_changed_names = |changed| {
	if changed.len() <= 8 {
		var $names = []
		for name in changed {
			if !$names.contains(name) {
				$names = $names.append(name)
			}
			if name.starts_with("__revision:") {
				alias = Str.from_utf8_lossy(name.to_utf8().drop_first(11))
				if !$names.contains(alias) {
					$names = $names.append(alias)
				}
			}
		}
		return $names
	}
	var $seen = Dict.empty()
	var $names = []
	for name in changed {
		if !$seen.contains(name) {
			$seen = $seen.insert(name, True)
			$names = $names.append(name)
		}
		if name.starts_with("__revision:") {
			alias = Str.from_utf8_lossy(name.to_utf8().drop_first(11))
			if !$seen.contains(alias) {
				$seen = $seen.insert(alias, True)
				$names = $names.append(alias)
			}
		}
	}
	$names
}

first_attribute : List(AsciiDocSource.Attribute), Str -> Try(Str, [Missing])
first_attribute = |attrs, name| {
	for attr in attrs {
		if attr.name == name {
			return Ok(attr.value)
		}
	}
	Err(Missing)
}

last_attribute : List(AsciiDocSource.Attribute), Str -> Try(Str, [Missing])
last_attribute = |attrs, name| {
	var $i = attrs.len()
	while $i > 0 {
		$i = $i - 1
		match attrs.get($i) {
			Ok(attr) if attr.name == name => return Ok(attr.value)
			_ => {}
		}
	}
	Err(Missing)
}

first_overlay : { first : Index, source : List(Attribute), before : List(Attribute), after : List(Attribute), values : AsciiDocDict.Map(Str), removed : AsciiDocDict.Map(Bool), appended : List(Attribute), prefix : Str, dynamic_tags_possible : Bool }, Str -> Try(Str, [Missing])
first_overlay = |view, name| {
	match first_attribute(view.before, name) {
		Ok(value) => return Ok(value)
		Err(_) => {}
	}
	base = if view.first.is_empty() first_attribute(view.source, name) else view.first.get(name)
	match base {
		Ok(value) => return Ok(value)
		Err(_) => {}
	}
	first_attribute(view.after, name)
}

Entry : { name : Str, name_hash : U64, value : Str, position : U64, active : Bool, run : U64 }

# The single tag contains a zero/one-element immutable node allocation.
# Recursion remains behind List; child handles are fields of that allocation.
# Tree([]) nodes allocate nothing. Constructors maintain the private length <= 1
# invariant, checked by the existing independent tree model below.
Tree := [Tree(List({ entry : Entry, left : Tree, right : Tree, height : U64, longest : U64, max_position : U64 }))]

Children : { left : Tree, right : Tree }

# Canonical ordered defaults moved byte-for-byte from preprocessing.
standard_attributes : List(AsciiDocSource.Attribute)
standard_attributes = [
	{ name: "asciidoctor", value: "" },
	{ name: "asciidoctor-version", value: "2.0.23" },
	{ name: "backend", value: "html5" },
	{ name: "backend-html5", value: "" },
	{ name: "basebackend", value: "html" },
	{ name: "basebackend-html", value: "" },
	{ name: "backend-html5-doctype-article", value: "" },
	{ name: "basebackend-html-doctype-article", value: "" },
	{ name: "doctype", value: "article" },
	{ name: "doctype-article", value: "" },
	{ name: "filetype", value: "html" },
	{ name: "filetype-html", value: "" },
	{ name: "htmlsyntax", value: "html" },
	{ name: "safe-mode-level", value: "20" },
	{ name: "safe-mode-name", value: "secure" },
	{ name: "safe-mode-secure", value: "" },
	{ name: "docdate", value: "2000-01-01" },
	{ name: "docyear", value: "2000" },
	{ name: "doctime", value: "00:00:00 UTC" },
	{ name: "docdatetime", value: "2000-01-01 00:00:00 UTC" },
	{ name: "localdate", value: "2000-01-01" },
	{ name: "localyear", value: "2000" },
	{ name: "localtime", value: "00:00:00 UTC" },
	{ name: "localdatetime", value: "2000-01-01 00:00:00 UTC" },
	{ name: "docdir", value: "" },
	{ name: "outdir", value: "." },
	{ name: "user-home", value: "." },
	{ name: "attribute-missing", value: "skip" },
	{ name: "attribute-undefined", value: "drop-line" },
	{ name: "outfilesuffix", value: ".html" },
	{ name: "sectids", value: "" },
	{ name: "prewrap", value: "" },
	{ name: "embedded", value: "" },
	{ name: "notitle", value: "" },
	{ name: "toc-placement", value: "auto" },
	{ name: "toc-title", value: "Table of Contents" },
	{ name: "stylesdir", value: "." },
	{ name: "iconsdir", value: "./images/icons" },
	{ name: "appendix-caption", value: "Appendix" },
	{ name: "appendix-refsig", value: "Appendix" },
	{ name: "chapter-refsig", value: "Chapter" },
	{ name: "part-refsig", value: "Part" },
	{ name: "section-refsig", value: "Section" },
	{ name: "example-caption", value: "Example" },
	{ name: "figure-caption", value: "Figure" },
	{ name: "table-caption", value: "Table" },
	{ name: "note-caption", value: "Note" },
	{ name: "tip-caption", value: "Tip" },
	{ name: "important-caption", value: "Important" },
	{ name: "warning-caption", value: "Warning" },
	{ name: "caution-caption", value: "Caution" },
	{ name: "untitled-label", value: "Untitled" },
	{ name: "version-label", value: "Version" },
	{ name: "last-update-label", value: "Last updated" },
]

# BEGIN GENERATED STANDARD TREE
standard_tree : Tree
standard_tree = Tree([{ entry: { name: "appendix-refsig", name_hash: 1846151851, value: "Appendix", position: 39, active: True, run: 0 }, left: Tree([{ entry: { name: "note-caption", name_hash: 890389686, value: "Note", position: 46, active: True, run: 0 }, left: Tree([{ entry: { name: "doctype", name_hash: 531439101, value: "article", position: 8, active: True, run: 0 }, left: Tree([{ entry: { name: "safe-mode-level", name_hash: 444112411, value: "20", position: 13, active: True, run: 0 }, left: Tree([{ entry: { name: "filetype-html", name_hash: 200271241, value: "", position: 11, active: True, run: 0 }, left: Tree([{ entry: { name: "backend-html5-doctype-article", name_hash: 145058394, value: "", position: 6, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 6 }]), right: Tree([{ entry: { name: "outdir", name_hash: 324151516, value: ".", position: 25, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 25 }]), height: 2, longest: 0, max_position: 25 }]), right: Tree([{ entry: { name: "doctime", name_hash: 531421578, value: "00:00:00 UTC", position: 18, active: True, run: 0 }, left: Tree([{ entry: { name: "docdate", name_hash: 530838105, value: "2000-01-01", position: 16, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 16 }]), right: Tree([]), height: 2, longest: 0, max_position: 18 }]), height: 3, longest: 0, max_position: 25 }]), right: Tree([{ entry: { name: "tip-caption", name_hash: 788222285, value: "Tip", position: 47, active: True, run: 0 }, left: Tree([{ entry: { name: "notitle", name_hash: 580976676, value: "", position: 33, active: True, run: 0 }, left: Tree([{ entry: { name: "docyear", name_hash: 531596524, value: "2000", position: 17, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 17 }]), right: Tree([{ entry: { name: "outfilesuffix", name_hash: 653023314, value: ".html", position: 29, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 29 }]), height: 2, longest: 0, max_position: 33 }]), right: Tree([{ entry: { name: "filetype", name_hash: 870548263, value: "html", position: 10, active: True, run: 0 }, left: Tree([{ entry: { name: "iconsdir", name_hash: 846005536, value: "./images/icons", position: 37, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 37 }]), right: Tree([]), height: 2, longest: 0, max_position: 37 }]), height: 3, longest: 0, max_position: 47 }]), height: 4, longest: 0, max_position: 47 }]), right: Tree([{ entry: { name: "attribute-missing", name_hash: 1453995968, value: "skip", position: 27, active: True, run: 0 }, left: Tree([{ entry: { name: "version-label", name_hash: 1120047704, value: "Version", position: 52, active: True, run: 0 }, left: Tree([{ entry: { name: "localtime", name_hash: 907356095, value: "00:00:00 UTC", position: 22, active: True, run: 0 }, left: Tree([{ entry: { name: "localdate", name_hash: 906772622, value: "2000-01-01", position: 20, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 20 }]), right: Tree([{ entry: { name: "localyear", name_hash: 907531041, value: "2000", position: 21, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 21 }]), height: 2, longest: 0, max_position: 22 }]), right: Tree([{ entry: { name: "part-refsig", name_hash: 1384067209, value: "Part", position: 41, active: True, run: 0 }, left: Tree([{ entry: { name: "attribute-undefined", name_hash: 1203319768, value: "drop-line", position: 28, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 28 }]), right: Tree([]), height: 2, longest: 0, max_position: 41 }]), height: 3, longest: 0, max_position: 52 }]), right: Tree([{ entry: { name: "embedded", name_hash: 1680730671, value: "", position: 32, active: True, run: 0 }, left: Tree([{ entry: { name: "caution-caption", name_hash: 1571579475, value: "Caution", position: 50, active: True, run: 0 }, left: Tree([{ entry: { name: "docdatetime", name_hash: 1524162536, value: "2000-01-01 00:00:00 UTC", position: 19, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 19 }]), right: Tree([{ entry: { name: "toc-title", name_hash: 1631017370, value: "Table of Contents", position: 35, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 35 }]), height: 2, longest: 0, max_position: 50 }]), right: Tree([{ entry: { name: "basebackend", name_hash: 1813213704, value: "html", position: 4, active: True, run: 0 }, left: Tree([{ entry: { name: "backend", name_hash: 1695229677, value: "html5", position: 2, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 2 }]), right: Tree([]), height: 2, longest: 0, max_position: 4 }]), height: 3, longest: 0, max_position: 50 }]), height: 4, longest: 0, max_position: 52 }]), height: 5, longest: 0, max_position: 52 }]), right: Tree([{ entry: { name: "asciidoctor", name_hash: 3143719609, value: "", position: 0, active: True, run: 0 }, left: Tree([{ entry: { name: "backend-html5", name_hash: 2592962340, value: "", position: 3, active: True, run: 0 }, left: Tree([{ entry: { name: "safe-mode-secure", name_hash: 2044090346, value: "", position: 15, active: True, run: 0 }, left: Tree([{ entry: { name: "asciidoctor-version", name_hash: 1893482028, value: "2.0.23", position: 1, active: True, run: 0 }, left: Tree([{ entry: { name: "basebackend-html-doctype-article", name_hash: 1870153792, value: "", position: 7, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 7 }]), right: Tree([{ entry: { name: "doctype-article", name_hash: 2033431822, value: "", position: 9, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 9 }]), height: 2, longest: 0, max_position: 9 }]), right: Tree([{ entry: { name: "sectids", name_hash: 2332217716, value: "", position: 30, active: True, run: 0 }, left: Tree([{ entry: { name: "last-update-label", name_hash: 2107600022, value: "Last updated", position: 53, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 53 }]), right: Tree([]), height: 2, longest: 0, max_position: 53 }]), height: 3, longest: 0, max_position: 53 }]), right: Tree([{ entry: { name: "appendix-caption", name_hash: 2751639833, value: "Appendix", position: 38, active: True, run: 0 }, left: Tree([{ entry: { name: "section-refsig", name_hash: 2662871015, value: "Section", position: 42, active: True, run: 0 }, left: Tree([{ entry: { name: "safe-mode-name", name_hash: 2616535652, value: "secure", position: 14, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 14 }]), right: Tree([{ entry: { name: "toc-placement", name_hash: 2711544529, value: "auto", position: 34, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 34 }]), height: 2, longest: 0, max_position: 42 }]), right: Tree([{ entry: { name: "stylesdir", name_hash: 3111129352, value: ".", position: 36, active: True, run: 0 }, left: Tree([{ entry: { name: "chapter-refsig", name_hash: 2987870329, value: "Chapter", position: 40, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 40 }]), right: Tree([]), height: 2, longest: 0, max_position: 40 }]), height: 3, longest: 0, max_position: 42 }]), height: 4, longest: 0, max_position: 53 }]), right: Tree([{ entry: { name: "warning-caption", name_hash: 3623748726, value: "Warning", position: 49, active: True, run: 0 }, left: Tree([{ entry: { name: "untitled-label", name_hash: 3468070587, value: "Untitled", position: 51, active: True, run: 0 }, left: Tree([{ entry: { name: "prewrap", name_hash: 3264030566, value: "", position: 31, active: True, run: 0 }, left: Tree([{ entry: { name: "user-home", name_hash: 3164478906, value: ".", position: 26, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 26 }]), right: Tree([{ entry: { name: "htmlsyntax", name_hash: 3372380097, value: "html", position: 12, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 12 }]), height: 2, longest: 0, max_position: 31 }]), right: Tree([{ entry: { name: "figure-caption", name_hash: 3554076066, value: "Figure", position: 44, active: True, run: 0 }, left: Tree([{ entry: { name: "important-caption", name_hash: 3485626046, value: "Important", position: 48, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 48 }]), right: Tree([]), height: 2, longest: 0, max_position: 48 }]), height: 3, longest: 0, max_position: 51 }]), right: Tree([{ entry: { name: "example-caption", name_hash: 3992284140, value: "Example", position: 43, active: True, run: 0 }, left: Tree([{ entry: { name: "localdatetime", name_hash: 3967238301, value: "2000-01-01 00:00:00 UTC", position: 23, active: True, run: 0 }, left: Tree([{ entry: { name: "table-caption", name_hash: 3725961352, value: "Table", position: 45, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 45 }]), right: Tree([]), height: 2, longest: 0, max_position: 45 }]), right: Tree([{ entry: { name: "docdir", name_hash: 4180903034, value: "", position: 24, active: True, run: 0 }, left: Tree([{ entry: { name: "basebackend-html", name_hash: 4062005514, value: "", position: 5, active: True, run: 0 }, left: Tree([]), right: Tree([]), height: 1, longest: 0, max_position: 5 }]), right: Tree([]), height: 2, longest: 0, max_position: 24 }]), height: 3, longest: 0, max_position: 45 }]), height: 4, longest: 0, max_position: 51 }]), height: 5, longest: 0, max_position: 53 }]), height: 6, longest: 0, max_position: 53 }])

# END GENERATED STANDARD TREE

child : Children, U64 -> Tree
child = |children, index| if index == 0 children.left else if index == 1 children.right else Tree([])

set_child : Children, U64, Tree -> Children
set_child = |children, index, value| if index == 0 { ..children, left: value } else if index == 1 { ..children, right: value } else { left: Tree([]), right: Tree([]) }

height : Tree -> U64
height = |tree| match tree {
	Tree([]) => 0
	Tree([n, ..]) => n.height
}

longest : Tree -> U64
longest = |tree| match tree {
	Tree([]) => 0
	Tree([n, ..]) => n.longest
}

max_position : Tree -> U64
max_position = |tree| match tree {
	Tree([]) => 0
	Tree([n, ..]) => n.max_position
}

child_stats : Children, U64 -> { height : U64, longest : U64, max_position : U64 }
child_stats = |children, index| match child(children, index) {
	Tree([]) => { height: 0, longest: 0, max_position: 0 }
	Tree([n, ..]) => { height: n.height, longest: n.longest, max_position: n.max_position }
}

node_children : Entry, Children -> Tree
node_children = |entry, children| {
	left = child_stats(children, 0)
	right = child_stats(children, 1)
	Tree([{ entry, left: children.left, right: children.right, height: U64.max(left.height, right.height) + 1, longest: U64.max(entry.run, U64.max(left.longest, right.longest)), max_position: U64.max(entry.position, U64.max(left.max_position, right.max_position)) }])
}

node : Entry, Tree, Tree -> Tree
node = |entry, left, right| node_children(entry, { left, right })

rotate_left_children : Entry, Children -> Tree
rotate_left_children = |entry, children| {
	selected = child(children, 1)
	parent = set_child(children, 1, Tree([]))
	match selected {
		Tree([]) => node_children(entry, parent)
		Tree([{ entry: pivot, left: pivot_left, right: pivot_right, height: _, longest: _, max_position: _ }, ..]) => {
			pivot_children = { left: pivot_left, right: pivot_right }
			middle = child(pivot_children, 0)
			detached = set_child(pivot_children, 0, Tree([]))
			lower = node_children(entry, set_child(parent, 1, middle))
			node_children(pivot, set_child(detached, 0, lower))
		}
	}
}

rotate_right_children : Entry, Children -> Tree
rotate_right_children = |entry, children| {
	selected = child(children, 0)
	parent = set_child(children, 0, Tree([]))
	match selected {
		Tree([]) => node_children(entry, parent)
		Tree([{ entry: pivot, left: pivot_left, right: pivot_right, height: _, longest: _, max_position: _ }, ..]) => {
			pivot_children = { left: pivot_left, right: pivot_right }
			middle = child(pivot_children, 1)
			detached = set_child(pivot_children, 1, Tree([]))
			lower = node_children(entry, set_child(parent, 0, middle))
			node_children(pivot, set_child(detached, 1, lower))
		}
	}
}

rotate_left : Tree -> Tree
rotate_left = |tree| match tree {
	Tree([]) => Tree([])
	Tree([{ entry, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => rotate_left_children(entry, { left: node_left, right: node_right })
}

rotate_right : Tree -> Tree
rotate_right = |tree| match tree {
	Tree([]) => Tree([])
	Tree([{ entry, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => rotate_right_children(entry, { left: node_left, right: node_right })
}

child_heavy : Children, U64, U64 -> Bool
child_heavy = |children, outer, inner| match child(children, outer) {
	Tree([]) => False
	Tree([n, ..]) => child_stats({ left: n.left, right: n.right }, inner).height > child_stats({ left: n.left, right: n.right }, 1 - inner).height
}

balance_children : Entry, Children -> Tree
balance_children = |entry, children| {
	left_height = child_stats(children, 0).height
	right_height = child_stats(children, 1).height
	if left_height > right_height + 1 {
		if child_heavy(children, 0, 1) {
			selected = child(children, 0)
			detached = set_child(children, 0, Tree([]))
			changed = rotate_left(selected)
			rotate_right_children(entry, set_child(detached, 0, changed))
		} else rotate_right_children(entry, children)
	} else if right_height > left_height + 1 {
		if child_heavy(children, 1, 0) {
			selected = child(children, 1)
			detached = set_child(children, 1, Tree([]))
			changed = rotate_right(selected)
			rotate_left_children(entry, set_child(detached, 1, changed))
		} else rotate_left_children(entry, children)
	} else node_children(entry, children)
}

balance : Entry, Tree, Tree -> Tree
balance = |entry, left, right| balance_children(entry, { left, right })

compare_bytes : List(U8), List(U8) -> I64
compare_bytes = |a, b| {
	var $i = 0.U64
	while $i < a.len() and $i < b.len() {
		x = a.get($i) ?? 0
		y = b.get($i) ?? 0
		if x < y {
			return -1
		}
		if x > y {
			return 1
		}
		$i = $i + 1
	}
	if a.len() < b.len() -1 else if a.len() > b.len() 1 else 0
}

compare_entries : Entry, Entry, Bool -> I64
compare_entries = |a, b, by_position| if by_position {
	if a.position < b.position -1.I64 else if a.position > b.position 1 else 0
} else if a.name_hash < b.name_hash -1 else if a.name_hash > b.name_hash 1 else if a.name == b.name 0 else compare_bytes(a.name.to_utf8(), b.name.to_utf8())

insert : Tree, Entry, Bool -> Tree
insert = |tree, entry, by_position| match tree {
	Tree([]) => node(entry, Tree([]), Tree([]))
	Tree([{ entry: previous, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => {
		children = { left: node_left, right: node_right }
		order = compare_entries(entry, previous, by_position)
		if order < 0 {
			selected = child(children, 0)
			detached = set_child(children, 0, Tree([]))
			changed = insert(selected, entry, by_position)
			balance_children(previous, set_child(detached, 0, changed))
		} else if order > 0 {
			selected = child(children, 1)
			detached = set_child(children, 1, Tree([]))
			changed = insert(selected, entry, by_position)
			balance_children(previous, set_child(detached, 1, changed))
		} else node_children(entry, children)
	}
}

# The selected child is detached before recursion, so retaining its old slot
# cannot force copies of otherwise unique descendant node lists.
update_name : Tree, Entry, List(U8), Bool -> { tree : Tree, prior_active : Bool, prior_position : U64 }
update_name = |tree, replacement, bytes, increment_revision| match tree {
	Tree([]) => { tree: node(replacement, Tree([]), Tree([])), prior_active: False, prior_position: 0 }
	Tree([{ entry: previous, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => {
		children = { left: node_left, right: node_right }
		order = compare_query_key(replacement.name_hash, replacement.name, bytes, previous)
		if order < 0 {
			selected = child(children, 0)
			detached = set_child(children, 0, Tree([]))
			{ tree: changed, prior_active, prior_position } = update_name(selected, replacement, bytes, increment_revision)
			{ tree: balance_children(previous, set_child(detached, 0, changed)), prior_active, prior_position }
		} else if order > 0 {
			selected = child(children, 1)
			detached = set_child(children, 1, Tree([]))
			{ tree: changed, prior_active, prior_position } = update_name(selected, replacement, bytes, increment_revision)
			{ tree: balance_children(previous, set_child(detached, 1, changed)), prior_active, prior_position }
		} else {
			value = if increment_revision ((U64.from_str(previous.value) ?? 0) + 1).to_str() else replacement.value
			entry = { ..replacement, value }
			{ tree: node_children(entry, children), prior_active: previous.active, prior_position: previous.position }
		}
	}
}

# General join preserves the existing two-key AVL behavior while restoring
# through the child handles on the taller path.
join_children : Entry, Children -> Tree
join_children = |entry, children| {
	left_height = child_stats(children, 0).height
	right_height = child_stats(children, 1).height
	if left_height > right_height + 1 {
		selected = child(children, 0)
		parent = set_child(children, 0, Tree([]))
		match selected {
			Tree([]) => node_children(entry, parent)
			Tree([{ entry: pivot, left: pivot_left, right: pivot_right, height: _, longest: _, max_position: _ }, ..]) => {
				pivot_children = { left: pivot_left, right: pivot_right }
				middle = child(pivot_children, 1)
				detached = set_child(pivot_children, 1, Tree([]))
				changed = join_children(entry, set_child(parent, 0, middle))
				balance_children(pivot, set_child(detached, 1, changed))
			}
		}
	} else if right_height > left_height + 1 {
		selected = child(children, 1)
		parent = set_child(children, 1, Tree([]))
		match selected {
			Tree([]) => node_children(entry, parent)
			Tree([{ entry: pivot, left: pivot_left, right: pivot_right, height: _, longest: _, max_position: _ }, ..]) => {
				pivot_children = { left: pivot_left, right: pivot_right }
				middle = child(pivot_children, 0)
				detached = set_child(pivot_children, 0, Tree([]))
				changed = join_children(entry, set_child(parent, 1, middle))
				balance_children(pivot, set_child(detached, 0, changed))
			}
		}
	} else node_children(entry, children)
}

join_name : Entry, Tree, Tree -> Tree
join_name = |entry, left, right| join_children(entry, { left, right })

NameUpdate : { entry : Entry, bytes : List(U8), increment_revision : Bool }

NamePairResult : { tree : Tree, low_active : Bool, low_position : U64, high_active : Bool, high_position : U64 }

replacement_entry : Entry, NameUpdate -> Entry
replacement_entry = |previous, update| {
	value = if update.increment_revision ((U64.from_str(previous.value) ?? 0) + 1).to_str() else update.entry.value
	{ ..update.entry, value }
}

# low and high are distinct exact keys in the existing hash/byte order. Their
# positions retain chronological target/revision order, independently of keys.
# Only their shared search prefix needs the general height-aware join; after
# divergence each child receives at most one ordinary AVL update.
update_name_pair : Tree, NameUpdate, NameUpdate -> NamePairResult
update_name_pair = |tree, low, high| match tree {
	Tree([]) => { tree: node(high.entry, node(low.entry, Tree([]), Tree([])), Tree([])), low_active: False, low_position: 0, high_active: False, high_position: 0 }
	Tree([{ entry: previous, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => {
		children = { left: node_left, right: node_right }
		low_order = compare_query_key(low.entry.name_hash, low.entry.name, low.bytes, previous)
		high_order = compare_query_key(high.entry.name_hash, high.entry.name, high.bytes, previous)
		if high_order < 0 {
			selected = child(children, 0)
			detached = set_child(children, 0, Tree([]))
			{ tree: changed, low_active, low_position, high_active, high_position } = update_name_pair(selected, low, high)
			{ tree: join_children(previous, set_child(detached, 0, changed)), low_active, low_position, high_active, high_position }
		} else if low_order > 0 {
			selected = child(children, 1)
			detached = set_child(children, 1, Tree([]))
			{ tree: changed, low_active, low_position, high_active, high_position } = update_name_pair(selected, low, high)
			{ tree: join_children(previous, set_child(detached, 1, changed)), low_active, low_position, high_active, high_position }
		} else {
			left = child(children, 0)
			without_left = set_child(children, 0, Tree([]))
			right = child(without_left, 1)
			detached = set_child(without_left, 1, Tree([]))
			{ tree: changed_left, prior_active: low_active, prior_position: low_position } = if low_order < 0 update_name(left, low.entry, low.bytes, low.increment_revision) else { tree: left, prior_active: previous.active, prior_position: previous.position }
			{ tree: changed_right, prior_active: high_active, prior_position: high_position } = if high_order > 0 update_name(right, high.entry, high.bytes, high.increment_revision) else { tree: right, prior_active: previous.active, prior_position: previous.position }
			entry = if low_order == 0 replacement_entry(previous, low) else if high_order == 0 replacement_entry(previous, high) else previous
			restored = set_child(detached, 0, changed_left)
			{ tree: balance_children(entry, set_child(restored, 1, changed_right)), low_active, low_position, high_active, high_position }
		}
	}
}

smallest : Tree -> Entry
smallest = |tree| match tree {
	Tree([n, ..]) => match child({ left: n.left, right: n.right }, 0) {
		Tree([]) => n.entry
		next => smallest(next)
	}
	Tree([]) => { name: "", name_hash: 0, value: "", position: 0, active: False, run: 0 }
}

remove : Tree, Entry, Bool -> Tree
remove = |tree, entry, by_position| match tree {
	Tree([]) => Tree([])
	Tree([{ entry: previous, left: node_left, right: node_right, height: _, longest: _, max_position: _ }, ..]) => {
		children = { left: node_left, right: node_right }
		order = compare_entries(entry, previous, by_position)
		if order < 0 {
			selected = child(children, 0)
			detached = set_child(children, 0, Tree([]))
			changed = remove(selected, entry, by_position)
			return balance_children(previous, set_child(detached, 0, changed))
		}
		if order > 0 {
			selected = child(children, 1)
			detached = set_child(children, 1, Tree([]))
			changed = remove(selected, entry, by_position)
			return balance_children(previous, set_child(detached, 1, changed))
		}
		left = child(children, 0)
		without_left = set_child(children, 0, Tree([]))
		right = child(without_left, 1)
		detached = set_child(without_left, 1, Tree([]))
		match (left, right) {
			(Tree([]), other) => other
			(other, Tree([])) => other
			(present_left, present_right) => {
				successor = smallest(present_right)
				changed = remove(present_right, successor, by_position)
				restored = set_child(detached, 0, present_left)
				balance_children(successor, set_child(restored, 1, changed))
			}
		}
	}
}

find_name : Tree, Str -> Try(Entry, [Missing])
find_name = |tree, name| {
	bytes = name.to_utf8()
	find_name_key(tree, name_hash(bytes), name, bytes)
}

find_name_key : Tree, U64, Str, List(U8) -> Try(Entry, [Missing])
find_name_key = |tree, hash, name, bytes| match tree {
	Tree([]) => Err(Missing)
	Tree([n, ..]) => {
		order = compare_query_key(hash, name, bytes, n.entry)
		if order < 0 find_name_key(child({ left: n.left, right: n.right }, 0), hash, name, bytes) else if order > 0 find_name_key(child({ left: n.left, right: n.right }, 1), hash, name, bytes) else Ok(n.entry)
	}
}

# Hashes only order tree keys. Exact bytes distinguish every collision.
name_hash : List(U8) -> U64
name_hash = |bytes| {
	var $hash = 5381.U64
	for byte in bytes {
		$hash = ($hash * 33 + byte.to_u64()) % 4294967296
	}
	$hash
}

# The hash only orders keys. Equal names use exact string equality without
# byte-list allocation; distinct hash collisions retain the original byte lex
# order. Query bytes are transient and reused throughout one tree traversal.
compare_query_key : U64, Str, List(U8), Entry -> I64
compare_query_key = |hash, name, bytes, entry| if hash < entry.name_hash -1 else if hash > entry.name_hash 1 else if name == entry.name 0 else compare_bytes(bytes, entry.name.to_utf8())

ordering : I64 -> [Before, Same, After]
ordering = |comparison| if comparison < 0 Before else if comparison > 0 After else Same

balanced_entries : List(Entry), U64, U64 -> Tree
balanced_entries = |entries, start, end| {
	if start >= end {
		return Tree([])
	}
	middle = start + (end - start) // 2
	match entries.get(middle) {
		Ok(entry) => node(entry, balanced_entries(entries, start, middle), balanced_entries(entries, middle + 1, end))
		Err(_) => Tree([])
	}
}

all_entries : Tree, List(Entry) -> List(Entry)
all_entries = |tree, entries| match tree {
	Tree([]) => entries
	Tree([n, ..]) => {
		before = all_entries(child({ left: n.left, right: n.right }, 0), entries)
		all_entries(child({ left: n.left, right: n.right }, 1), if n.entry.active before.append(n.entry) else before)
	}
}

value_run : Str -> U64
value_run = |value| {
	if !value.contains("\u(1)") {
		return 0
	}
	var $run = 0.U64
	var $max = 0.U64
	for byte in value.to_utf8() {
		if byte == 1 {
			$run = $run + 1
			$max = U64.max($max, $run)
		} else {
			$run = 0
		}
	}
	$max
}

remove_raw : AsciiDocSource.Store, Str -> AsciiDocSource.Store
remove_raw = |store, name| update_raw(store, name, "", False, False)

set_raw : AsciiDocSource.Store, Str, Str -> AsciiDocSource.Store
set_raw = |store, name, value| update_raw(store, name, value, True, False)

# Revision names always start with __revision:, so they cannot be Tag keys.
# The initial replacement value handles an absent/inactive revision as zero.
increment_revision_raw : AsciiDocSource.Store, Str -> AsciiDocSource.Store
increment_revision_raw = |store, name| update_raw(store, name, "1", True, True)

update_raw : AsciiDocSource.Store, Str, Str, Bool, Bool -> AsciiDocSource.Store
update_raw = |Store(s), name, value, active, increment_revision| {
	bytes = name.to_utf8()
	entry = { name, name_hash: name_hash(bytes), value, position: s.next, active, run: value_run(value) }
	{ tree: names, prior_active, prior_position } = update_name(s.names, entry, bytes, increment_revision)
	tags = if name.starts_with("__inline-tag:") {
		remaining = if prior_active remove(s.tags, { ..entry, position: prior_position }, True) else s.tags
		if active insert(remaining, entry, True) else remaining
	} else s.tags
	count = if active {
		if prior_active s.count else s.count + 1
	} else {
		if prior_active s.count - 1 else s.count
	}
	ordered = if active or prior_active Err(Unknown) else s.seed_values
	# Missing-name unsets do not change binding equality. Every other write
	# is certified after the entire logical mutation, including its revision.
	Store({ ..s, names, tags, count, next: s.next + 1, seed_values: ordered, baseline_equal: s.baseline_equal and !active and !prior_active })
}

store_trusted : AsciiDocSource.Store -> Bool
store_trusted = |Store(s)| s.parsed_history

apply_set : AsciiDocSource.Store, Str, Str, Bool -> AsciiDocSource.Store
apply_set = |store, name, value, parsed| {
	generation = AsciiDocSource.key(store).generation + 1
	trusted = parsed and store_trusted(store)
	finish_update(refresh_prefix(set_raw(store, name, value)), generation, trusted)
}

apply_unset : AsciiDocSource.Store, Str, Bool -> AsciiDocSource.Store
apply_unset = |store, name, parsed| {
	generation = AsciiDocSource.key(store).generation + 1
	trusted = parsed and store_trusted(store)
	finish_update(refresh_prefix(remove_raw(store, name)), generation, trusted)
}

apply_definition : AsciiDocSource.Store, Str, Str, Bool, Bool -> AsciiDocSource.Store
apply_definition = |Store(s), name, value, unset_value, parsed| {
	generation = s.generation + 1
	trusted = parsed and s.parsed_history
	bytes = name.to_utf8()
	revision_name = "__revision:${name}"
	revision_bytes = revision_name.to_utf8()
	active = !unset_value
	actual_value = if active value else ""
	entry = { name, name_hash: name_hash(bytes), value: actual_value, position: s.next, active, run: value_run(actual_value) }
	revision_entry = { name: revision_name, name_hash: name_hash(revision_bytes), value: "1", position: s.next + 1, active: True, run: 0 }
	target = { entry, bytes, increment_revision: False }
	revision = { entry: revision_entry, bytes: revision_bytes, increment_revision: True }
	target_first = compare_query_key(entry.name_hash, name, bytes, revision_entry) < 0
	{ tree: names, low_active, low_position, high_active, high_position } = if target_first update_name_pair(s.names, target, revision) else update_name_pair(s.names, revision, target)
	prior_active = if target_first low_active else high_active
	prior_position = if target_first low_position else high_position
	tags = if name.starts_with("__inline-tag:") {
		remaining = if prior_active remove(s.tags, { ..entry, position: prior_position }, True) else s.tags
		if active insert(remaining, entry, True) else remaining
	} else s.tags
	target_count = if active {
		if prior_active s.count else s.count + 1
	} else if prior_active s.count - 1 else s.count
	revision_active = if target_first high_active else low_active
	count = if revision_active target_count else target_count + 1
	updated = Store({ ..s, names, tags, count, next: s.next + 2, seed_values: Err(Unknown), baseline_equal: False })
	finish_update(refresh_prefix(updated), generation, trusted)
}

finish_update : AsciiDocSource.Store, U64, Bool -> AsciiDocSource.Store
finish_update = |store, generation, trusted| {
	Store(s) = certify_baseline(store)
	Store({ ..s, generation, parsed_history: trusted })
}

certify_baseline : AsciiDocSource.Store -> AsciiDocSource.Store
certify_baseline = |store| {
	Store(s) = store
	if s.baseline_equal {
		return store
	}
	match s.baseline {
		Ok(baseline) if baseline.len() == s.count => {
			var $entries = []
			for attribute in baseline {
				match find_name(s.names, attribute.name) {
					Ok(entry) if entry.active and entry.value == attribute.value => {
						$entries = $entries.append(entry)
					}
					_ => return store
				}
			}
			# The baseline is unique and the active count matches, so these
			# exact point reads prove every visible binding. Current positions
			# recover order without visiting arbitrarily many old tombstones.
			ordered = match s.seed_values {
				Ok(values) => Ok(values)
				Err(_) => Ok($entries.sort_with(|a, b| ordering(compare_entries(a, b, True))).map(|entry| { name: entry.name, value: entry.value }))
			}
			Store({ ..s, baseline_equal: True, seed_values: ordered })
		}
		_ => store
	}
}

# The position clock advances on every raw mutation, including tombstones.
# Its gaps do not affect active binding order, while one augmented name tree
# also answers exact deltas without maintaining a second mutation journal.
changed_since : Tree, U64, List(Str) -> List(Str)
changed_since = |tree, cutoff, names| match tree {
	Tree([]) => names
	Tree([n, ..]) => {
		if n.max_position < cutoff {
			return names
		}
		before = changed_since(child({ left: n.left, right: n.right }, 0), cutoff, names)
		current = if n.entry.position >= cutoff before.append(n.entry.name) else before
		changed_since(child({ left: n.left, right: n.right }, 1), cutoff, current)
	}
}

refresh_prefix : AsciiDocSource.Store -> AsciiDocSource.Store
refresh_prefix = |store| {
	Store(s) = store
	size = longest(s.names) + 1
	if s.prefix.count_utf8_bytes() == size store else Store({ ..s, prefix: Str.from_utf8_lossy(List.repeat(1.U8, size)) })
}

ordered_values : Tree, List(AsciiDocSource.Attribute) -> List(AsciiDocSource.Attribute)
ordered_values = |tree, values| match tree {
	Tree([]) => values
	Tree([n, ..]) => ordered_values(child({ left: n.left, right: n.right }, 1), ordered_values(child({ left: n.left, right: n.right }, 0), values).append({ name: n.entry.name, value: n.entry.value }))
}

expect {
	initial = AsciiDocSource.new("source/options/includes", [{ name: "a", value: "first" }, { name: "b", value: "second" }, { name: "a", value: "last" }])
	defined = AsciiDocSource.define(initial, "b", "reset", False)
	removed = AsciiDocSource.define(defined, "a", "", True)
	readded = AsciiDocSource.define(removed, "a", "again", False)
	AsciiDocSource.materialize(initial) == [{ name: "b", value: "second" }, { name: "a", value: "last" }] and AsciiDocSource.materialize(defined) == [{ name: "a", value: "last" }, { name: "b", value: "reset" }, { name: "__revision:b", value: "1" }] and AsciiDocSource.lookup(removed, "a") == Err(Missing) and AsciiDocSource.lookup(readded, "__revision:a") == Ok("2") and AsciiDocSource.key(readded).generation == 3 and AsciiDocSource.lookup(initial, "b") == Ok("second")
}

expect {
	control = Str.from_utf8_lossy(List.repeat(1.U8, 4))
	initial = AsciiDocSource.new("namespace", [{ name: "x", value: control }, { name: "__inline-tag:a", value: "tag one" }, { name: "__inline-tag:b", value: "tag two" }])
	changed = AsciiDocSource.set(initial, "__inline-tag:a", "changed")
	removed = AsciiDocSource.unset(changed, "x")
	AsciiDocSource.prefix(initial).count_utf8_bytes() == 5 and AsciiDocSource.prefix(removed).count_utf8_bytes() == 1 and AsciiDocSource.tag_attributes(AsciiDocSource.index(changed)) == [{ name: "__inline-tag:b", value: "tag two" }, { name: "__inline-tag:a", value: "changed" }] and AsciiDocSource.lookup(initial, "__inline-tag:a") == Ok("tag one")
}

expect {
	var $store = AsciiDocSource.new("balanced", [])
	var $model = []
	var $i = 0.U64
	while $i < 512 {
		name = "key${(($i * 137) % 512).to_str()}"
		value = $i.to_str()
		$store = AsciiDocSource.set($store, name, value)
		$model = $model.append({ name, value })
		$i = $i + 1
	}
	retained = $store
	$i = 0
	while $i < 256 {
		name = "key${(($i * 137) % 512).to_str()}"
		$store = AsciiDocSource.unset($store, name)
		$model = $model.keep_if(|a| a.name != name)
		$i = $i + 1
	}
	Store(final) = $store
	AsciiDocSource.materialize($store) == $model and AsciiDocSource.len(retained) == 512 and AsciiDocSource.len($store) == 256 and $model.all(|a| AsciiDocSource.lookup($store, a.name) == Ok(a.value)) and height(final.names) < 16
}

expect {
	base = AsciiDocSource.new("preview", [{ name: "same", value: "base" }, { name: "gone", value: "source" }, { name: "__inline-tag:base", value: "base-tag" }])
	values = AsciiDocDict.empty().insert("same", "dynamic").insert("__inline-tag:dynamic", "dynamic-tag")
	view = { first: AsciiDocSource.index(base), source: [], before: [{ name: "local", value: "before" }], after: [{ name: "fallback", value: "after" }], values, removed: AsciiDocDict.empty().insert("gone", True), appended: [{ name: "fallback", value: "synthetic" }, { name: "extra", value: "extra" }], prefix: "\u(1)", dynamic_tags_possible: True }
	reader = AsciiDocSource.from_view(view)
	reader.get("same") == Ok("dynamic") and reader.get("gone") == Err(Missing) and reader.get("fallback") == Ok("after") and reader.get("extra") == Ok("extra") and AsciiDocSource.tag_attributes(reader).len() == 2 and AsciiDocSource.semantic(reader, []).any(|a| a.name == "local" and a.value == "before") and AsciiDocSource.lookup(base, "same") == Ok("base")
}

expect {
	reader = AsciiDocSource.from_view({ first: AsciiDocSource.from_dict(Dict.empty()), source: [{ name: "n", value: "4" }], before: [], after: [], values: AsciiDocDict.empty(), removed: AsciiDocDict.empty(), appended: [], prefix: "\u(1)", dynamic_tags_possible: False })
	reader.get("n") == Ok("4") and AsciiDocSource.semantic(reader, []) == [{ name: "n", value: "4" }]
}

expect {
	before = AsciiDocSource.new_parsed("framed-input-and-options", [{ name: "alpha", value: "same" }, { name: "other", value: "value" }])
	redefined = AsciiDocSource.define_parsed(before, "alpha", "same", False)
	removed = AsciiDocSource.define_parsed(redefined, "alpha", "", True)
	restored = AsciiDocSource.define_parsed(removed, "alpha", "again", False)
	delta = AsciiDocSource.changed_names(AsciiDocSource.index(before), AsciiDocSource.index(restored)) ?? []
	delta.len() == 2 and delta.contains("alpha") and delta.contains("__revision:alpha") and AsciiDocSource.lookup(before, "alpha") == Ok("same") and AsciiDocSource.lookup(removed, "alpha") == Err(Missing) and AsciiDocSource.lookup(restored, "__revision:alpha") == Ok("3") and AsciiDocSource.trusted_history(AsciiDocSource.index(restored))
}

expect {
	before = AsciiDocSource.new_parsed("arbitrary-names", [])
	revised = AsciiDocSource.set_parsed(before, "__revision:weird", "changed")
	removed = AsciiDocSource.unset_parsed(revised, "never-present")
	delta = AsciiDocSource.changed_names(AsciiDocSource.index(before), AsciiDocSource.index(removed)) ?? []
	delta.len() == 3 and delta.contains("__revision:weird") and delta.contains("weird") and delta.contains("never-present") and AsciiDocSource.lookup(removed, "never-present") == Err(Missing)
}

expect {
	before = AsciiDocSource.new_parsed("linear", [{ name: "x", value: "one" }])
	after = AsciiDocSource.set_parsed(before, "x", "two")
	fork_a = AsciiDocSource.set(before, "x", "fork-a")
	fork_b = AsciiDocSource.set(before, "x", "fork-b")
	other = AsciiDocSource.new_parsed("other-source", [{ name: "x", value: "one" }])
	AsciiDocSource.changed_names(AsciiDocSource.index(after), AsciiDocSource.index(before)) == Err(Unknown) and AsciiDocSource.changed_names(AsciiDocSource.index(before), AsciiDocSource.index(other)) == Err(Unknown) and AsciiDocSource.changed_names(AsciiDocSource.index(fork_a), AsciiDocSource.index(fork_b)) == Err(Unknown) and !AsciiDocSource.trusted_history(AsciiDocSource.index(fork_a)) and AsciiDocSource.index(fork_a) != AsciiDocSource.index(fork_b) and AsciiDocSource.index(fork_a) == AsciiDocSource.index(fork_a)
}

expect {
	before = AsciiDocSource.new_parsed("repeated-unrelated", [{ name: "stable", value: "kept" }])
	var $current = before
	var $i = 0.U64
	while $i < 512 {
		$current = AsciiDocSource.define_parsed($current, "unrelated", $i.to_str(), False)
		$i = $i + 1
	}
	delta = AsciiDocSource.changed_names(AsciiDocSource.index(before), AsciiDocSource.index($current)) ?? []
	Store(s) = $current
	delta.len() == 2 and !delta.contains("stable") and height(s.names) <= 2 and AsciiDocSource.lookup($current, "stable") == Ok("kept") and AsciiDocSource.lookup(before, "unrelated") == Err(Missing)
}

# Equal ordering hashes are not binding identities.
expect {
	first = "Aa"
	second = "B@"
	initial = AsciiDocSource.new_parsed("collision", [{ name: first, value: "one" }, { name: second, value: "two" }])
	changed = AsciiDocSource.define_parsed(initial, first, "three", False)
	removed = AsciiDocSource.unset_parsed(changed, second)
	delta = AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index(removed)) ?? []
	name_hash(first.to_utf8()) == name_hash(second.to_utf8()) and AsciiDocSource.lookup(initial, first) == Ok("one") and AsciiDocSource.lookup(initial, second) == Ok("two") and AsciiDocSource.lookup(changed, first) == Ok("three") and AsciiDocSource.lookup(changed, second) == Ok("two") and AsciiDocSource.lookup(removed, second) == Err(Missing) and delta.contains(first) and delta.contains(second)
}

expect {
	var $input = []
	var $model = []
	var $i = 0.U64
	while $i < 768 {
		name = "東京${(($i * 137) % 512).to_str()}"
		value = if $i == 767 "\u(1)\u(1)\u(1)" else $i.to_str()
		$input = $input.append({ name, value })
		$model = $model.keep_if(|entry| entry.name != name).append({ name, value })
		$i = $i + 1
	}
	initial = AsciiDocSource.new("bulk-seed", $input)
	changed = AsciiDocSource.set(initial, "extra", "new")
	Store(before) = initial
	Store(after) = changed
	AsciiDocSource.materialize(initial) == $model and AsciiDocSource.materialize(changed) == $model.append({ name: "extra", value: "new" }) and before.seed_values == Ok($model) and after.seed_values == Err(Unknown) and height(before.names) <= 10 and AsciiDocSource.prefix(initial).count_utf8_bytes() == 4 and AsciiDocSource.prefix(AsciiDocSource.unset(changed, "東京${((767.U64 * 137) % 512).to_str()}")) == "\u(1)"
}

expect {
	initial = AsciiDocSource.new_parsed("missing-unset", [{ name: "keep", value: "\u(1)\u(1)" }, { name: "__inline-tag:one", value: "first" }])
	missing = AsciiDocSource.unset_parsed(initial, "missing")
	twice = AsciiDocSource.unset_parsed(missing, "missing")
	empty = AsciiDocSource.unset_parsed(AsciiDocSource.unset_parsed(twice, "keep"), "__inline-tag:one")
	readded = AsciiDocSource.set_parsed(empty, "missing", "")
	Store(original) = initial
	Store(tombstones) = empty
	delta = AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index(twice)) ?? []
	AsciiDocSource.materialize(twice) == AsciiDocSource.materialize(initial) and delta == ["missing"] and tombstones.next == original.next + 4 and tombstones.count == 0 and AsciiDocSource.index(empty).is_empty() and AsciiDocSource.tag_attributes(AsciiDocSource.index(empty)).is_empty() and AsciiDocSource.prefix(empty) == "\u(1)" and AsciiDocSource.materialize(readded) == [{ name: "missing", value: "" }] and AsciiDocSource.lookup(initial, "keep") == Ok("\u(1)\u(1)")
}

# This independent ordered-list model checks skipped-snapshot deltas as well
# as active bindings across arbitrary names, collisions, revisions and tags.
expect {
	names = ["Aa", "B@", "", "__revision:", "__revision:custom", "__inline-tag:slot", "東京", "plain"]
	var $store = AsciiDocSource.new_parsed("mutation-model", [])
	var $before = $store
	var $model = []
	var $changed = []
	var $ok = True
	var $i = 0.U64
	while $i < 320 {
		name = names.get(($i * 7 + $i // 8) % names.len()) ?? ""
		value = if $i % 11 == 0 "\u(1)\u(1)\u(1)" else $i.to_str()
		var $writes = [name]
		if $i % 4 == 0 {
			$store = AsciiDocSource.set_parsed($store, name, value)
			$model = $model.keep_if(|a| a.name != name).append({ name, value })
		} else if $i % 4 == 1 {
			$store = AsciiDocSource.unset_parsed($store, name)
			$model = $model.keep_if(|a| a.name != name)
		} else {
			revision_name = "__revision:${name}"
			revision = (U64.from_str(first_attribute($model, revision_name) ?? "0") ?? 0) + 1
			unset_value = $i % 4 == 3
			$store = AsciiDocSource.define_parsed($store, name, value, unset_value)
			$model = $model.keep_if(|a| a.name != name)
			if !unset_value {
				$model = $model.append({ name, value })
			}
			$model = $model.keep_if(|a| a.name != revision_name).append({ name: revision_name, value: revision.to_str() })
			$writes = $writes.append(revision_name)
		}
		for written in $writes {
			if !$changed.contains(written) {
				$changed = $changed.append(written)
			}
			if written.starts_with("__revision:") {
				alias = Str.from_utf8_lossy(written.to_utf8().drop_first(11))
				if !$changed.contains(alias) {
					$changed = $changed.append(alias)
				}
			}
		}
		Store(snapshot) = $store
		$ok = $ok and (
			match snapshot.seed_values {
				Ok(values) => values == $model
				Err(_) => True
			}
		) and AsciiDocSource.materialize($store) == $model and AsciiDocSource.len($store) == $model.len() and names.all(|key| AsciiDocSource.lookup($store, key) == first_attribute($model, key)) and AsciiDocSource.prefix($store).count_utf8_bytes() == $model.fold(0.U64, |run, a| U64.max(run, value_run(a.value))) + 1 and AsciiDocSource.tag_attributes(AsciiDocSource.index($store)) == $model.keep_if(|a| a.name.starts_with("__inline-tag:"))
		if $i % 19 == 0 {
			delta = AsciiDocSource.changed_names(AsciiDocSource.index($before), AsciiDocSource.index($store)) ?? []
			$ok = $ok and delta.len() == $changed.len() and delta.all(|key| $changed.contains(key))
			$before = $store
			$changed = []
		}
		$i = $i + 1
	}
	$ok
}

expect {
	values = [{ name: "same", value: "first" }, { name: "__inline-tag:one", value: "tag" }, { name: "same", value: "last" }, { name: "empty", value: "" }]
	first = AsciiDocSource.from_ordered(values, True)
	last = AsciiDocSource.from_ordered(values, False)
	borrowed = AsciiDocSource.from_view({ first, source: [], before: [], after: [], values: AsciiDocDict.empty(), removed: AsciiDocDict.empty(), appended: [], prefix: "\u(1)", dynamic_tags_possible: False })
	first.get("same") == Ok("first") and last.get("same") == Ok("last") and first.get("empty") == Ok("") and last.get("absent") == Err(Missing) and AsciiDocSource.semantic(first, []) == values and AsciiDocSource.tag_attributes(borrowed) == [{ name: "__inline-tag:one", value: "tag" }]
}

expect {
	values = [{ name: "a", value: "first" }, { name: "b", value: "second" }, { name: "__revision:custom", value: "7" }]
	parsed = AsciiDocSource.index(AsciiDocSource.new_parsed("ordered-bridge", values))
	first = AsciiDocSource.from_ordered(values, True)
	last = AsciiDocSource.from_ordered(values, False)
	AsciiDocSource.changed_names(parsed, first) == Ok([]) and AsciiDocSource.changed_names(last, parsed) == Ok([]) and AsciiDocSource.changed_names(first, parsed) == Ok([]) and AsciiDocSource.changed_names(parsed, last) == Ok([])
}

expect {
	values = [{ name: "a", value: "one" }, { name: "b", value: "two" }]
	store = AsciiDocSource.new_parsed("conservative-bridge", values)
	parsed = AsciiDocSource.index(store)
	reordered = AsciiDocSource.from_ordered([{ name: "b", value: "two" }, { name: "a", value: "one" }], True)
	duplicates = [{ name: "a", value: "one" }, { name: "a", value: "one" }, { name: "b", value: "two" }]
	manual = AsciiDocSource.index(AsciiDocSource.new("conservative-bridge", values))
	forked = AsciiDocSource.index(AsciiDocSource.set(store, "b", "two"))
	restored = AsciiDocSource.index(AsciiDocSource.set_parsed(store, "b", "two"))
	ordered = AsciiDocSource.from_ordered(values, True)
	AsciiDocSource.changed_names(parsed, reordered) == Err(Unknown) and AsciiDocSource.changed_names(reordered, parsed) == Err(Unknown) and AsciiDocSource.changed_names(parsed, AsciiDocSource.from_ordered(duplicates, True)) == Err(Unknown) and AsciiDocSource.changed_names(AsciiDocSource.from_ordered(duplicates, False), parsed) == Err(Unknown) and AsciiDocSource.changed_names(manual, ordered) == Err(Unknown) and AsciiDocSource.changed_names(ordered, forked) == Err(Unknown) and AsciiDocSource.changed_names(restored, ordered) == Ok([])
}

expect {
	var $values = []
	var $i = 0.U64
	while $i < 65 {
		$values = $values.append({ name: "name${$i.to_str()}", value: $i.to_str() })
		$i = $i + 1
	}
	parsed = AsciiDocSource.index(AsciiDocSource.new_parsed("too-large-bridge", $values))
	AsciiDocSource.changed_names(parsed, AsciiDocSource.from_ordered($values, True)) == Err(Unknown)
}

# Fused revision updates parse only the previous revision leaf. Reserved-name
# writes and inactive revisions must retain the same independent List model.
expect {
	tag = "__inline-tag:one"
	revision = "__revision:${tag}"
	nested_revision = "__revision:${revision}"
	initial = AsciiDocSource.new_parsed("fused-revisions", [{ name: tag, value: "first" }, { name: "__inline-tag:two", value: "second" }, { name: revision, value: "not-a-number" }])
	first = AsciiDocSource.define_parsed(initial, tag, "updated", False)
	nested = AsciiDocSource.define_parsed(first, revision, "12", False)
	controlled = AsciiDocSource.define_parsed(nested, tag, "again\u(1)\u(1)", False)
	removed = AsciiDocSource.unset_parsed(controlled, revision)
	final = AsciiDocSource.define_parsed(removed, tag, "last", False)
	AsciiDocSource.lookup(first, revision) == Ok("1") and AsciiDocSource.lookup(controlled, revision) == Ok("13") and AsciiDocSource.lookup(removed, revision) == Err(Missing) and AsciiDocSource.lookup(final, revision) == Ok("1") and AsciiDocSource.materialize(final) == [{ name: "__inline-tag:two", value: "second" }, { name: nested_revision, value: "1" }, { name: tag, value: "last" }, { name: revision, value: "1" }] and AsciiDocSource.tag_attributes(AsciiDocSource.index(final)) == [{ name: "__inline-tag:two", value: "second" }, { name: tag, value: "last" }] and AsciiDocSource.prefix(controlled).count_utf8_bytes() == 3 and AsciiDocSource.prefix(final) == "\u(1)" and AsciiDocSource.lookup(initial, tag) == Ok("first")
}

expect {
	baseline = [{ name: "Aa", value: "one" }, { name: "B@", value: "two" }, { name: "__inline-tag:a", value: "first\u(1)" }, { name: "__inline-tag:b", value: "second" }, { name: "empty", value: "" }]
	initial = AsciiDocSource.new_parsed("bounded-recovery", baseline)
	reordered = AsciiDocSource.set_parsed(initial, "__inline-tag:a", "first\u(1)")
	changed = AsciiDocSource.set_parsed(reordered, "Aa", "different")
	var $store = AsciiDocSource.set_parsed(changed, "Aa", "one")
	var $i = 0.U64
	while $i < 256 {
		$store = AsciiDocSource.set_parsed($store, "temporary${$i.to_str()}", "\u(1)\u(1)\u(1)\u(1)")
		$i = $i + 1
	}
	Store(expanded) = $store
	$i = 0
	while $i < 256 {
		$store = AsciiDocSource.unset_parsed($store, "temporary${$i.to_str()}")
		$i = $i + 1
	}
	$i = 0
	while $i < 512 {
		$store = AsciiDocSource.unset_parsed($store, "never-present${$i.to_str()}")
		$i = $i + 1
	}
	expected = [{ name: "B@", value: "two" }, { name: "__inline-tag:b", value: "second" }, { name: "empty", value: "" }, { name: "__inline-tag:a", value: "first\u(1)" }, { name: "Aa", value: "one" }]
	Store(final) = $store
	AsciiDocSource.matches_baseline(reordered, baseline) and !AsciiDocSource.matches_baseline(changed, baseline) and expanded.seed_values == Err(Unknown) and !expanded.baseline_equal and final.seed_values == Ok(expected) and AsciiDocSource.matches_baseline($store, baseline) and AsciiDocSource.materialize($store) == expected and AsciiDocSource.materialize(initial) == baseline and AsciiDocSource.tag_attributes(AsciiDocSource.index($store)) == [{ name: "__inline-tag:b", value: "second" }, { name: "__inline-tag:a", value: "first\u(1)" }] and AsciiDocSource.prefix($store).count_utf8_bytes() == 2 and AsciiDocSource.changed_names(AsciiDocSource.from_ordered(baseline, True), AsciiDocSource.index($store)) == Ok([])
}

expect {
	var $values = []
	var $i = 0.U64
	while $i < 65 {
		$values = $values.append({ name: "large${$i.to_str()}", value: "original" })
		$i = $i + 1
	}
	initial = AsciiDocSource.new_parsed("no-arbitrary-certificate", $values)
	removed = AsciiDocSource.unset_parsed(initial, "large64")
	restored = AsciiDocSource.set_parsed(removed, "large64", "original")
	Store(final) = restored
	final.baseline == Err(Unknown) and !final.baseline_equal and !AsciiDocSource.matches_baseline(restored, $values) and AsciiDocSource.materialize(restored) == $values
}

expect {
	baseline = [{ name: "a", value: "one" }, { name: "b", value: "two" }]
	initial = AsciiDocSource.new_parsed("lazy-classification", baseline)
	changed = AsciiDocSource.set_parsed(initial, "a", "different")
	Store(negative) = changed
	restored = AsciiDocSource.set_parsed(changed, "a", "one")
	Store(positive) = restored
	defined = AsciiDocSource.define_parsed(initial, "a", "one", False)
	Store(revised) = defined
	AsciiDocSource.classify_baseline(initial, baseline) == Ok(True) and AsciiDocSource.classify_baseline(changed, baseline) == Ok(False) and negative.seed_values == Err(Unknown) and AsciiDocSource.materialize(changed) == [{ name: "b", value: "two" }, { name: "a", value: "different" }] and AsciiDocSource.classify_baseline(restored, baseline) == Ok(True) and positive.seed_values == Ok([{ name: "b", value: "two" }, { name: "a", value: "one" }]) and revised.seed_values == Err(Unknown) and AsciiDocSource.classify_baseline(defined, baseline) == Ok(False) and AsciiDocSource.classify_baseline(initial, [{ name: "unrelated", value: "value" }]) == Err(Unknown)
}

expect {
	left = "kaz東京é"
	right = "kc8東京é"
	left_long = "kazattribute-name-with-a-long-common-suffix"
	right_long = "kc8attribute-name-with-a-long-common-suffix"
	initial = AsciiDocSource.new_parsed("unicode-byte-collisions", [{ name: left, value: "one" }, { name: right, value: "two" }, { name: left_long, value: "three" }, { name: right_long, value: "four" }])
	updated = AsciiDocSource.set_parsed(initial, right, "changed")
	removed = AsciiDocSource.unset_parsed(updated, left_long)
	name_hash(left.to_utf8()) == name_hash(right.to_utf8()) and name_hash(left_long.to_utf8()) == name_hash(right_long.to_utf8()) and AsciiDocSource.lookup(initial, left) == Ok("one") and AsciiDocSource.lookup(initial, right) == Ok("two") and AsciiDocSource.lookup(removed, left) == Ok("one") and AsciiDocSource.lookup(removed, right) == Ok("changed") and AsciiDocSource.lookup(removed, left_long) == Err(Missing) and AsciiDocSource.lookup(removed, right_long) == Ok("four") and AsciiDocSource.materialize(removed) == [{ name: left, value: "one" }, { name: right_long, value: "four" }, { name: right, value: "changed" }]
}

expect {
	longer = Str.from_utf8_lossy(List.repeat(1.U8, 8192))
	shorter = Str.from_utf8_lossy(List.repeat(1.U8, 4096))
	value_run("") == 0 and value_run("ASCII revision 12345") == 0 and value_run("東京é😀") == 0 and value_run("a\u(1)b\u(1)\u(1)c") == 2 and value_run("${shorter}middle${longer}tail") == 8192 and value_run("\u(1)\u(1)\u(1)") == 3
}

header_name : Str -> Bool
header_name = |name| name == "__header-source" or name.starts_with("__header-initial:") or name.starts_with("__header-final:")

tree_has_header_names : Tree -> Bool
tree_has_header_names = |tree| match tree {
	Tree([]) => False
	Tree([n, ..]) => (n.entry.active and header_name(n.entry.name)) or tree_has_header_names(child({ left: n.left, right: n.right }, 0)) or tree_has_header_names(child({ left: n.left, right: n.right }, 1))
}

# Equal composed scopes preserve live mutations across nested substitutions,
# without granting generic composites a trusted namespace or delta history.
expect {
	store = AsciiDocSource.new_parsed("joined-refresh", [{ name: "x", value: "one" }])
	parts = [{ prefix: "", reader: AsciiDocSource.index(store) }, { prefix: "__header-final:", reader: AsciiDocSource.from_ordered([{ name: "x", value: "first" }, { name: "x", value: "last" }], True) }]
	first = AsciiDocSource.compose(parts, True, "\u(1)")
	same = AsciiDocSource.compose(parts, True, "\u(1)")
	AsciiDocSource.changed_names(first, same) == Ok([]) and !AsciiDocSource.trusted_history(first) and first.get("x") == Ok("one") and first.get("__header-final:x") == Ok("first")
}

expect {
	original = AsciiDocSource.new_parsed("joined-revisions", [{ name: "x", value: "same" }])
	revised = AsciiDocSource.define_parsed(original, "x", "same", False)
	alias = AsciiDocSource.set_parsed(original, "__revision:x", "changed")
	first = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(original) }], True, "\u(1)")
	redefined = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(revised) }], True, "\u(1)")
	aliased = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(alias) }], True, "\u(1)")
	first.get("x") == redefined.get("x") and AsciiDocSource.changed_names(first, redefined) == Err(Unknown) and AsciiDocSource.changed_names(first, aliased) == Err(Unknown)
}

# Equal namespace/generation fields do not authenticate manual forked stores.
expect {
	initial = AsciiDocSource.new("manual-joined-fork", [{ name: "x", value: "initial" }])
	left = AsciiDocSource.set(initial, "x", "left")
	right = AsciiDocSource.set(initial, "x", "right")
	copy = AsciiDocSource.new("other-manual-namespace", [{ name: "x", value: "left" }])
	a = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(left) }], True, "\u(1)")
	b = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(right) }], True, "\u(1)")
	c = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.index(copy) }], True, "\u(1)")
	AsciiDocSource.key(left) == AsciiDocSource.key(right) and AsciiDocSource.changed_names(a, b) == Err(Unknown) and AsciiDocSource.changed_names(a, c) == Ok([])
}

expect {
	# These distinct names have the same tree-order hash. Exact names and
	# ordered duplicate winners must still distinguish composite scopes.
	values = [{ name: "kaz", value: "first" }, { name: "kc8", value: "other" }, { name: "kaz", value: "last" }]
	reader = AsciiDocSource.from_ordered(values, True)
	base = AsciiDocSource.compose([{ prefix: "", reader }], True, "\u(1)")
	reversed = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.from_ordered([{ name: "kaz", value: "last" }, { name: "kc8", value: "other" }, { name: "kaz", value: "first" }], True) }], True, "\u(1)")
	last_reader = AsciiDocSource.compose([{ prefix: "", reader: AsciiDocSource.from_ordered(values, False) }], True, "\u(1)")
	last_segments = AsciiDocSource.compose([{ prefix: "", reader }], False, "\u(1)")
	renamed = AsciiDocSource.compose([{ prefix: "__header-final:", reader }], True, "\u(1)")
	certificate = AsciiDocSource.compose([{ prefix: "", reader }], True, "\u(1)\u(1)")
	name_hash("kaz".to_utf8()) == name_hash("kc8".to_utf8()) and base.get("kaz") == Ok("first") and reversed.get("kaz") == Ok("last") and AsciiDocSource.changed_names(base, reversed) == Err(Unknown) and AsciiDocSource.changed_names(base, last_reader) == Err(Unknown) and AsciiDocSource.changed_names(base, last_segments) == Err(Unknown) and AsciiDocSource.changed_names(base, renamed) == Err(Unknown) and AsciiDocSource.changed_names(base, certificate) == Err(Unknown)
}

# Raw keys and generated aliases may name each other, including nested
# revision names and the empty alias. Keep first encounter order exact.
expect {
	unique_changed_names(["__revision:a", "a", "__revision:__revision:a", "__revision:", ""]) == ["__revision:a", "a", "__revision:__revision:a", "__revision:", ""] and unique_changed_names([]) == []
}

expect {
	inputs = ["__revision:a", "a", "b", "__revision:b", "__revision:__revision:a", "c", "d", "__revision:", "", "e", "f", "a"]
	var $expected = []
	for name in inputs {
		if !$expected.contains(name) {
			$expected = $expected.append(name)
		}
		if name.starts_with("__revision:") {
			alias = Str.from_utf8_lossy(name.to_utf8().drop_first(11))
			if !$expected.contains(alias) {
				$expected = $expected.append(alias)
			}
		}
	}
	unique_changed_names(inputs) == $expected and unique_changed_names(inputs.take_first(8)) == $expected.take_first(unique_changed_names(inputs.take_first(8)).len())
}

expect {
	initial = AsciiDocSource.new_parsed("small-delta-tombstones", [])
	removed = AsciiDocSource.define_parsed(initial, "missing", "", True)
	reserved = AsciiDocSource.define_parsed(removed, "__revision:missing", "7", False)
	match (AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index(initial)), AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index(reserved))) {
		(Ok(empty), Ok(names)) => empty.is_empty() and names.len() == 3 and names.contains("missing") and names.contains("__revision:missing") and names.contains("__revision:__revision:missing") and AsciiDocSource.lookup(reserved, "missing").is_err()
		_ => False
	}
}

# Independent sequential oracle retained only for differential expectations.
# It deliberately rebuilds both paths through the pre-existing raw operations.
sequential_definition_model : AsciiDocSource.Store, Str, Str, Bool, Bool -> AsciiDocSource.Store
sequential_definition_model = |store, name, value, unset_value, parsed| {
	generation = AsciiDocSource.key(store).generation + 1
	trusted = parsed and store_trusted(store)
	defined = if unset_value remove_raw(store, name) else set_raw(store, name, value)
	finish_update(refresh_prefix(increment_revision_raw(defined, "__revision:${name}")), generation, trusted)
}

# Include inactive entries: matching only materialized values would miss lost
# tombstones and changed-name cutoff errors.
model_entries : Tree -> List(Entry)
model_entries = |tree| match tree {
	Tree([]) => []
	Tree([n, ..]) => model_entries(child({ left: n.left, right: n.right }, 0)).append(n.entry).concat(model_entries(child({ left: n.left, right: n.right }, 1)))
}

model_tree_stats : Tree -> { valid : Bool, height : U64, run : U64, position : U64, active : U64 }
model_tree_stats = |tree| match tree {
	Tree([]) => { valid: True, height: 0, run: 0, position: 0, active: 0 }
	Tree([n, .. as extra]) => {
		left = model_tree_stats(child({ left: n.left, right: n.right }, 0))
		right = model_tree_stats(child({ left: n.left, right: n.right }, 1))
		h = U64.max(left.height, right.height) + 1
		run = U64.max(value_run(n.entry.value), U64.max(left.run, right.run))
		position = U64.max(n.entry.position, U64.max(left.position, right.position))
		{ valid: extra.is_empty() and left.valid and right.valid and left.height <= right.height + 1 and right.height <= left.height + 1 and n.height == h and n.longest == run and n.max_position == position and n.entry.run == value_run(n.entry.value), height: h, run, position, active: left.active + right.active + (if n.entry.active 1 else 0) }
	}
}

model_store_equal : AsciiDocSource.Store, AsciiDocSource.Store -> Bool
model_store_equal = |Store(actual), Store(expected)| {
	entries = model_entries(actual.names)
	var $ordered = True
	var $i = 1.U64
	while $i < entries.len() {
		$ordered = $ordered and match (entries.get($i - 1), entries.get($i)) {
			(Ok(left), Ok(right)) => compare_entries(left, right, False) < 0
			_ => False
		}
		$i = $i + 1
	}
	stats = model_tree_stats(actual.names)
	$ordered and stats.valid and stats.active == actual.count and model_tree_stats(actual.tags).valid and entries == model_entries(expected.names) and model_entries(actual.tags) == model_entries(expected.tags) and actual.count == expected.count and actual.next == expected.next and actual.generation == expected.generation and actual.parsed_history == expected.parsed_history and actual.namespace == expected.namespace and actual.prefix == expected.prefix and actual.seed_values == expected.seed_values and actual.baseline == expected.baseline and actual.baseline_equal == expected.baseline_equal
}

expect {
	seed = [{ name: "kaz", value: "one" }, { name: "kc8", value: "two" }, { name: "__inline-tag:slot", value: "first\u(1)\u(1)" }, { name: "__revision:plain", value: "not-a-number" }, { name: "plain", value: "same" }, { name: "__revision:plain", value: "7" }]
	initial = AsciiDocSource.new_parsed("two-key-sequential-model", seed)
	var $actual = initial
	var $expected = initial
	var $checkpoint = initial
	var $reference_checkpoint = initial
	var $ok = True
	var $i = 0.U64
	while $i < 1280 {
		number = ($i * 37 % 113).to_str()
		name = match $i % 12 {
			0 => "kaz${number}"
			1 => "kc8${number}"
			2 => "東京${number}"
			3 => "attribute-with-a-long-common-suffix-${number}"
			4 => ""
			5 => "__revision:"
			6 => "__revision:key${number}"
			7 => "__inline-tag:slot${($i % 5).to_str()}"
			8 => "plain"
			9 => "__revision:plain"
			_ => "key${number}"
		}
		value = match $i % 7 {
			0 => "\u(1)\u(1)\u(1)\u(1)"
			1 => "same"
			2 => "not-numeric"
			3 => ""
			_ => $i.to_str()
		}
		before = $actual
		before_values = AsciiDocSource.materialize(before)
		if $i % 9 == 0 {
			$actual = AsciiDocSource.unset_parsed($actual, name)
			$expected = AsciiDocSource.unset_parsed($expected, name)
		} else if $i % 9 == 1 {
			$actual = AsciiDocSource.set_parsed($actual, name, value)
			$expected = AsciiDocSource.set_parsed($expected, name, value)
		} else {
			unset_value = $i % 11 == 0
			$actual = AsciiDocSource.define_parsed($actual, name, value, unset_value)
			$expected = sequential_definition_model($expected, name, value, unset_value, True)
		}
		$ok = $ok and model_store_equal($actual, $expected) and AsciiDocSource.materialize(before) == before_values and AsciiDocSource.materialize($actual) == AsciiDocSource.materialize($expected) and AsciiDocSource.tag_attributes(AsciiDocSource.index($actual)) == AsciiDocSource.tag_attributes(AsciiDocSource.index($expected)) and AsciiDocSource.changed_names(AsciiDocSource.index($checkpoint), AsciiDocSource.index($actual)) == AsciiDocSource.changed_names(AsciiDocSource.index($reference_checkpoint), AsciiDocSource.index($expected))
		if $i % 29 == 0 {
			$checkpoint = $actual
			$reference_checkpoint = $expected
		}
		$i = $i + 1
	}
	$ok and AsciiDocSource.materialize(initial) == [{ name: "kaz", value: "one" }, { name: "kc8", value: "two" }, { name: "__inline-tag:slot", value: "first\u(1)\u(1)" }, { name: "plain", value: "same" }, { name: "__revision:plain", value: "7" }]
}

expect {
	baseline = [{ name: "x", value: "same" }, { name: "__revision:x", value: "1" }]
	parsed = AsciiDocSource.new_parsed("two-key-baseline-whole-definition", baseline)
	missing_revision = AsciiDocSource.unset_parsed(parsed, "__revision:x")
	restored = AsciiDocSource.define_parsed(missing_revision, "x", "same", False)
	reference = sequential_definition_model(missing_revision, "x", "same", False, True)
	manual = AsciiDocSource.new("two-key-manual", baseline)
	manual_result = AsciiDocSource.define_parsed(manual, "__revision:x", "invalid", False)
	manual_reference = sequential_definition_model(manual, "__revision:x", "invalid", False, True)
	invalidated = AsciiDocSource.define(parsed, "x", "new", False)
	invalidated_reference = sequential_definition_model(parsed, "x", "new", False, False)
	model_store_equal(restored, reference) and AsciiDocSource.classify_baseline(restored, baseline) == Ok(True) and model_store_equal(manual_result, manual_reference) and !AsciiDocSource.trusted_history(AsciiDocSource.index(manual_result)) and model_store_equal(invalidated, invalidated_reference) and !AsciiDocSource.trusted_history(AsciiDocSource.index(invalidated)) and AsciiDocSource.materialize(parsed) == baseline
}

# Start with no pivot, then insert ascending, descending and colliding names.
# This exercises the two-new-leaf case before shared and divergent paths form.
expect {
	initial = AsciiDocSource.new_parsed("two-key-empty-and-growing", [])
	var $actual = initial
	var $reference = initial
	var $ok = True
	var $i = 0.U64
	while $i < 96 {
		number = if $i < 32 $i else if $i < 64 95 - $i else $i - 64
		name = if $i < 64 "key${number.to_str()}" else if $i % 2 == 0 "kaz${number.to_str()}" else "kc8${(number - 1).to_str()}"
		$actual = AsciiDocSource.define_parsed($actual, name, "same", False)
		$reference = sequential_definition_model($reference, name, "same", False, True)
		$ok = $ok and model_store_equal($actual, $reference)
		$i = $i + 1
	}
	$ok and AsciiDocSource.materialize(initial).is_empty()
}

# Every rotation shape retains the two-child invariant and earlier snapshots.
expect {
	var $ok = True
	for names in [["c", "a", "b"], ["a", "c", "b"], ["c", "b", "a"], ["a", "b", "c"]] {
		var $store = AsciiDocSource.new_parsed("buffer-rotations", [])
		var $expected = []
		for name in names {
			$store = AsciiDocSource.set_parsed($store, name, name)
			$expected = $expected.append({ name, value: name })
		}
		checkpoint = $store
		changed = AsciiDocSource.define_parsed($store, "b", "changed", False)
		removed = AsciiDocSource.define_parsed(changed, "a", "", True)
		Store(current) = removed
		$ok = $ok and AsciiDocSource.materialize(checkpoint) == $expected and AsciiDocSource.lookup(removed, "b") == Ok("changed") and AsciiDocSource.lookup(removed, "a").is_err() and model_tree_stats(current.names).valid and model_tree_stats(current.tags).valid
	}
	$ok
}

# Tag removal really deletes AVL nodes, unlike name tombstones. Retain the
# original root while removing internal/minimum/maximum keys from the new one.
expect {
	var $store = AsciiDocSource.new_parsed("buffer-tag-removal", [])
	var $expected = []
	var $i = 0.U64
	while $i < 17 {
		name = "__inline-tag:slot${$i.to_str()}"
		value = $i.to_str()
		$store = AsciiDocSource.set_parsed($store, name, value)
		$expected = $expected.append({ name, value })
		$i = $i + 1
	}
	checkpoint = $store
	original = $expected
	for index in [8.U64, 0, 16, 4, 12, 2, 14] {
		name = "__inline-tag:slot${index.to_str()}"
		$store = AsciiDocSource.unset_parsed($store, name)
		$expected = $expected.keep_if(|attribute| attribute.name != name)
	}
	Store(current) = $store
	AsciiDocSource.materialize(checkpoint) == original and AsciiDocSource.tag_attributes(AsciiDocSource.index(checkpoint)) == original and AsciiDocSource.materialize($store) == $expected and AsciiDocSource.tag_attributes(AsciiDocSource.index($store)) == $expected and model_tree_stats(current.names).valid and model_tree_stats(current.tags).valid
}

# The ordinary constructor is the independent source of default-tree order,
# metadata and binding semantics; the generated literal must agree exactly.
expect {
	literal = AsciiDocSource.new_standard_parsed("static-standard-initial")
	runtime = AsciiDocSource.new_parsed("static-standard-initial", AsciiDocSource.standard)
	names = AsciiDocSource.standard.map(|a| a.name).concat(["", "unknown", "東京", "__revision:outfilesuffix", "__inline-tag:slot"])
	model_store_equal(literal, runtime) and AsciiDocSource.standard.len() == 54 and AsciiDocSource.materialize(literal) == AsciiDocSource.standard and names.all(|name| AsciiDocSource.lookup(literal, name) == AsciiDocSource.lookup(runtime, name)) and AsciiDocSource.tag_attributes(AsciiDocSource.index(literal)).is_empty() and AsciiDocSource.prefix(literal) == "\u(1)" and AsciiDocSource.changed_names(AsciiDocSource.index(runtime), AsciiDocSource.index(literal)) == Ok([])
}

# Exercise every public mutation/trust boundary from the actual static seed,
# including rotations/new keys, tombstones, tags and reserved revision names.
factory_model_transition : AsciiDocSource.Store, U64, Str, Str -> AsciiDocSource.Store
factory_model_transition = |store, operation, name, value| match operation {
	0 => AsciiDocSource.set_parsed(store, name, value)
	1 => AsciiDocSource.unset_parsed(store, name)
	2 => AsciiDocSource.define_parsed(store, name, value, False)
	3 => AsciiDocSource.define_parsed(store, name, value, True)
	4 => AsciiDocSource.set(store, name, value)
	5 => AsciiDocSource.unset(store, name)
	6 => AsciiDocSource.define(store, name, value, False)
	_ => AsciiDocSource.define(store, name, value, True)
}

expect {
	initial = AsciiDocSource.new_standard_parsed("static-standard-first-writes")
	reference = AsciiDocSource.new_parsed("static-standard-first-writes", AsciiDocSource.standard)
	var $valid = True
	for name in ["outfilesuffix", "", "new-name", "東京", "kaz", "kc8", "__inline-tag:slot", "__revision:outfilesuffix"] {
		var $operation = 0.U64
		while $operation < 8 {
			actual = factory_model_transition(initial, $operation, name, "changed\u(1)\u(1)")
			expected = factory_model_transition(reference, $operation, name, "changed\u(1)\u(1)")
			$valid = $valid and model_store_equal(actual, expected) and model_store_equal(initial, reference) and model_store_equal(AsciiDocSource.new_standard_parsed("static-standard-first-writes"), reference) and AsciiDocSource.changed_names(AsciiDocSource.index(initial), AsciiDocSource.index(actual)) == AsciiDocSource.changed_names(AsciiDocSource.index(reference), AsciiDocSource.index(expected))
			$operation = $operation + 1
		}
	}
	$valid
}

expect {
	initial = AsciiDocSource.new_standard_parsed("static-standard-retained-roots")
	reference = AsciiDocSource.new_parsed("static-standard-retained-roots", AsciiDocSource.standard)
	var $actual = initial
	var $expected = reference
	var $valid = True
	var $i = 0.U64
	while $i < 192 {
		name = if $i % 3 == 0 "outfilesuffix" else if $i % 3 == 1 "__inline-tag:slot${($i % 7).to_str()}" else "__revision:key${($i % 11).to_str()}"
		value = if $i % 7 == 0 "\u(1)\u(1)\u(1)" else $i.to_str()
		before = $actual
		before_reference = $expected
		$actual = factory_model_transition($actual, $i % 8, name, value)
		$expected = factory_model_transition($expected, $i % 8, name, value)
		$valid = $valid and model_store_equal($actual, $expected) and model_store_equal(before, before_reference) and model_store_equal(initial, reference) and model_store_equal(AsciiDocSource.new_standard_parsed("static-standard-retained-roots"), reference)
		$i = $i + 1
	}
	$valid and AsciiDocSource.materialize(initial) == AsciiDocSource.standard
}

# Independent reference: the pre-bulk initialization applies every caller
# binding as a complete logical parsed write, including certification/clocks.
model_initial_options : Str, List(AsciiDocSource.Attribute), List(AsciiDocSource.Attribute) -> AsciiDocSource.Store
model_initial_options = |namespace, seed, writes| {
	var $store = AsciiDocSource.new_parsed(namespace, seed)
	for attr in writes {
		$store = AsciiDocSource.set_parsed($store, attr.name, attr.value)
	}
	$store
}

expect {
	seeds = [[], [{ name: "a", value: "first" }, { name: "b", value: "two" }, { name: "a", value: "last" }], [{ name: "__inline-tag:a", value: "first\u(1)\u(1)" }, { name: "__revision:x", value: "9" }, { name: "x", value: "seed" }]]
	sequences = [[], [{ name: "a", value: "last" }], [{ name: "a", value: "changed" }, { name: "a", value: "last" }], [{ name: "__revision:x", value: "not-a-number" }, { name: "x!", value: "" }, { name: "__inline-tag:a", value: "replaced" }, { name: "kaz", value: "collision-one" }, { name: "kc8", value: "collision-two" }, { name: "__revision:__revision:x", value: "literal" }]]
	var $valid = True
	for seed in seeds {
		for writes in sequences {
			actual = AsciiDocSource.new_parsed_options("bulk-options-exact", seed, writes)
			expected = model_initial_options("bulk-options-exact", seed, writes)
			$valid = $valid and model_store_equal(actual, expected)
			$valid = $valid and model_store_equal(AsciiDocSource.define_parsed(actual, "x", "later", False), AsciiDocSource.define_parsed(expected, "x", "later", False))
			$valid = $valid and model_store_equal(AsciiDocSource.unset_parsed(actual, "missing"), AsciiDocSource.unset_parsed(expected, "missing"))
			$valid = $valid and AsciiDocSource.changed_names(AsciiDocSource.index(AsciiDocSource.new_parsed("bulk-options-exact", seed)), AsciiDocSource.index(actual)) == AsciiDocSource.changed_names(AsciiDocSource.index(AsciiDocSource.new_parsed("bulk-options-exact", seed)), AsciiDocSource.index(expected))
		}
	}
	$valid
}

expect {
	var $seed = []
	var $i = 0.U64
	while $i < 96 {
		$seed = $seed.append({ name: "key${$i.to_str()}", value: "seed${$i.to_str()}" })
		$i = $i + 1
	}
	var $writes = []
	$i = 0
	while $i < 384 {
		name = if $i % 11 == 0 "__inline-tag:tag${($i % 7).to_str()}" else if $i % 13 == 0 "__revision:key${($i % 96).to_str()}" else "key${(($i * 37) % 128).to_str()}"
		value = if $i % 17 == 0 "\u(1)\u(1)\u(1)" else $i.to_str()
		$writes = $writes.append({ name, value })
		$i = $i + 1
	}
	actual = AsciiDocSource.new_parsed_options("large-options-exact", $seed, $writes)
	expected = model_initial_options("large-options-exact", $seed, $writes)
	removed_actual = AsciiDocSource.unset_parsed(actual, "key0")
	removed_expected = AsciiDocSource.unset_parsed(expected, "key0")
	restored_actual = AsciiDocSource.set_parsed(removed_actual, "key0", "restored")
	restored_expected = AsciiDocSource.set_parsed(removed_expected, "key0", "restored")
	model_store_equal(actual, expected) and model_store_equal(removed_actual, removed_expected) and model_store_equal(restored_actual, restored_expected) and AsciiDocSource.materialize(actual) == AsciiDocSource.materialize(expected) and AsciiDocSource.tag_attributes(AsciiDocSource.index(actual)) == AsciiDocSource.tag_attributes(AsciiDocSource.index(expected))
}

expect {
	seed = [{ name: "a", value: "one" }, { name: "b", value: "two" }, { name: "__inline-tag:t", value: "safe\u(1)" }]
	writes = [{ name: "a", value: "changed" }, { name: "__inline-tag:t", value: "\u(1)\u(1)\u(1)" }, { name: "b", value: "two" }, { name: "a", value: "one" }, { name: "__inline-tag:t", value: "safe\u(1)" }]
	actual = AsciiDocSource.new_parsed_options("restored-options-exact", seed, writes)
	expected = model_initial_options("restored-options-exact", seed, writes)
	Store(result) = actual
	model_store_equal(actual, expected) and result.baseline_equal and result.generation == 5 and result.next == 8 and result.seed_values == Ok([{ name: "b", value: "two" }, { name: "a", value: "one" }, { name: "__inline-tag:t", value: "safe\u(1)" }]) and AsciiDocSource.changed_names(AsciiDocSource.from_ordered(seed, True), AsciiDocSource.index(actual)) == Ok([]) and AsciiDocSource.prefix(actual).count_utf8_bytes() == 2
}

expect {
	var $seed64 = []
	var $position = 0.U64
	while $position < 64 {
		$seed64 = $seed64.append({ name: "boundary${$position.to_str()}", value: $position.to_str() })
		$position = $position + 1
	}
	# More than 64 input entries can still have exactly 64 unique names.
	duplicates = $seed64.concat([{ name: "boundary0", value: "changed" }, { name: "boundary63", value: "63" }, { name: "boundary0", value: "0" }])
	seed65 = duplicates.append({ name: "boundary64", value: "64" })
	writes = [{ name: "boundary1", value: "temporary" }, { name: "boundary1", value: "1" }]
	actual64 = AsciiDocSource.new_parsed_options("baseline64", duplicates, writes)
	expected64 = model_initial_options("baseline64", duplicates, writes)
	actual65 = AsciiDocSource.new_parsed_options("baseline65", seed65, writes)
	expected65 = model_initial_options("baseline65", seed65, writes)
	Store(before64) = AsciiDocSource.new_parsed("baseline64", duplicates)
	Store(before65) = AsciiDocSource.new_parsed("baseline65", seed65)
	model_store_equal(actual64, expected64) and model_store_equal(actual65, expected65) and initial_baseline(duplicates) == before64.baseline and initial_baseline(seed65) == before65.baseline and before64.baseline.is_ok() and before65.baseline == Err(Unknown) and initial_baseline([]) == Ok([])
}

expect {
	var $valid = True
	for count in [0.U64, 1, 16, 31, 32, 33, 64, 128] {
		var $writes = []
		var $restored = []
		var $position = 0.U64
		while $position < count {
			name = match $position % 5 {
				0 => "key${($position % 17).to_str()}"
				1 => "__inline-tag:tag${($position % 3).to_str()}"
				2 => "__revision:outfilesuffix"
				3 => "literal!"
				_ => "outfilesuffix"
			}
			value = if $position % 7 == 0 "\u(1)\u(1)" else if name == "outfilesuffix" ".html" else $position.to_str()
			$writes = $writes.append({ name, value })
			$restored = $restored.append({ name: "outfilesuffix", value: if $position % 2 == 0 ".changed" else ".html" })
			$position = $position + 1
		}
		for writes in [$writes, $restored] {
			actual = AsciiDocSource.new_standard_parsed_options("standard-bulk-model", writes)
			expected = model_initial_options("standard-bulk-model", standard_attributes, writes)
			changed = AsciiDocSource.define_parsed(actual, "outfilesuffix", ".later", False)
			reference = AsciiDocSource.define_parsed(expected, "outfilesuffix", ".later", False)
			removed = AsciiDocSource.unset_parsed(changed, "missing")
			removed_reference = AsciiDocSource.unset_parsed(reference, "missing")
			$valid = $valid and model_store_equal(actual, expected) and model_store_equal(changed, reference) and model_store_equal(removed, removed_reference) and model_store_equal(actual, expected) and AsciiDocSource.materialize(actual) == AsciiDocSource.materialize(expected) and AsciiDocSource.tag_attributes(AsciiDocSource.index(actual)) == AsciiDocSource.tag_attributes(AsciiDocSource.index(expected)) and AsciiDocSource.changed_names(AsciiDocSource.index(actual), AsciiDocSource.index(removed)) == AsciiDocSource.changed_names(AsciiDocSource.index(expected), AsciiDocSource.index(removed_reference))
		}
	}
	$valid
}
