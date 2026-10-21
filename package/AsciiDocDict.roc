## Owned dense string-keyed maps for inline dynamic bindings. The public
## iteration order matches Dict: insertion appends, replacement keeps a slot,
## and removal moves the last entry into the removed slot.
AsciiDocDict :: [].{
	Map(v) :: { entries : List((Str, v)), positions : Dict(Str, Try(U64, [Missing])) }.{
		get : Map(v), Str -> Try(v, [Missing])
		get = |map, name| {
			{ entries, positions } = map
			match positions.get(name) {
				Ok(Ok(position)) => {
					(_, value) = entries.get(position) ?? crash "invalid internal AsciiDoc map position"
					Ok(value)
				}
				_ => Err(Missing)
			}
		}

		contains : Map(v), Str -> Bool
		contains = |map, name| map.get(name).is_ok()

		len : Map(v) -> U64
		len = |map| map.entries.len()

		is_empty : Map(v) -> Bool
		is_empty = |map| map.entries.is_empty()

		to_list : Map(v) -> List((Str, v))
		to_list = |map| map.entries

		keys : Map(v) -> List(Str)
		keys = |map| map.entries.map(|(name, _)| name)

		insert : Map(v), Str, v -> Map(v)
		insert = |map, name, value| {
			# Decompose before probing; no original aggregate survives a write.
			{ entries, positions } = map
			match positions.get(name) {
				Ok(Ok(position)) => {
					updated = entries.set(position, (name, value)) ?? crash "invalid internal AsciiDoc map insertion position"
					{ entries: updated, positions }
				}
				_ => {
					position = entries.len()
					{ entries: entries.append((name, value)), positions: positions.insert(name, Ok(position)) }
				}
			}
		}

		remove : Map(v), Str -> Map(v)
		remove = |map, name| {
			# Missing removal reconstructs from owned fields too. Returning the
			# original map here would retain its buffers across the success path.
			{ entries, positions } = map
			match positions.get(name) {
				Ok(Ok(position)) => {
					last = entries.len() - 1
					if position == last {
						return { entries: entries.sublist({ start: 0, len: last }), positions: positions.insert(name, Err(Missing)) }
					}
					last_entry = entries.get(last) ?? crash "invalid internal AsciiDoc map final position"
					(moved_name, _) = last_entry
					swapped = entries.set(position, last_entry) ?? crash "invalid internal AsciiDoc map removal position"
					{
						entries: swapped.sublist({ start: 0, len: last }),
						positions: positions.insert(moved_name, Ok(position)).insert(name, Err(Missing)),
					}
				}
				_ => { entries, positions }
			}
		}

		is_eq : Map(v), Map(v) -> Bool where [v.is_eq : v, v -> Bool]
		is_eq = |left, right| {
			if left.len() != right.len() {
				return False
			}
			for (name, value) in left.to_list() {
				if right.get(name) != Ok(value) {
					return False
				}
			}
			True
		}
	}

	empty : () -> Map(v)
	empty = || { entries: [], positions: Dict.empty() }

	from_dict : Dict(Str, v) -> Map(v)
	from_dict = |dict| {
		entries = dict.to_list()
		var $positions = Dict.empty()
		var $position = 0.U64
		for (name, _) in entries {
			$positions = $positions.insert(name, Ok($position))
			$position = $position + 1
		}
		{ entries, positions: $positions }
	}
}

# Exact public iteration order and retained immutable roots are checked against
# the ordinary Dict implementation rather than a second copy of this algorithm.
expect {
	initial_dict = Dict.empty().insert("alpha", "one").insert("beta", "two").insert("gamma", "three")
	initial = AsciiDocDict.from_dict(initial_dict)
	actual = initial.remove("beta").insert("delta", "four").insert("alpha", "updated").remove("absent").insert("beta", "again")
	expected = initial_dict.remove("beta").insert("delta", "four").insert("alpha", "updated").remove("absent").insert("beta", "again")
	actual.to_list() == expected.to_list() and actual.keys() == expected.keys() and actual.len() == expected.len() and initial.to_list() == initial_dict.to_list() and initial.get("alpha") == Ok("one") and actual.get("absent") == Err(Missing)
}

expect {
	var $actual = AsciiDocDict.empty()
	var $expected = Dict.empty()
	var $roots = []
	var $references = []
	var $ok = True
	var $i = 0.U64
	while $i < 512 {
		name = match $i % 7 {
			0 => "kaz${($i % 31).to_str()}"
			1 => "kc8${($i % 31).to_str()}"
			2 => "東京${($i % 19).to_str()}"
			3 => ""
			4 => "__revision:key${($i % 13).to_str()}"
			_ => "key${($i % 41).to_str()}"
		}
		if $i % 5 == 0 {
			$actual = $actual.remove(name)
			$expected = $expected.remove(name)
		} else {
			value = if ($i % 3 == 0) "" else "value${$i.to_str()}"
			$actual = $actual.insert(name, value)
			$expected = $expected.insert(name, value)
		}
		$ok = $ok and $actual.to_list() == $expected.to_list() and $actual.keys() == $expected.keys() and $actual.len() == $expected.len() and $actual.is_empty() == $expected.is_empty()
		for probe in [name, "missing", "", "東京0", "key1"] {
			$ok = $ok and $actual.get(probe).map_err(|_| Missing) == $expected.get(probe).map_err(|_| Missing) and $actual.contains(probe) == $expected.contains(probe)
		}
		if $i % 37 == 0 {
			$roots = $roots.append($actual)
			$references = $references.append($expected)
		}
		$i = $i + 1
	}
	var $r = 0.U64
	while $r < $roots.len() {
		match ($roots.get($r), $references.get($r)) {
			(Ok(actual), Ok(expected)) => {
				$ok = $ok and actual.to_list() == expected.to_list() and actual == AsciiDocDict.from_dict(expected)
			}
			_ => {
				$ok = False
			}
		}
		$r = $r + 1
	}
	$ok
}

expect {
	var $actual = AsciiDocDict.empty()
	var $expected = Dict.empty()
	var $ok = True
	var $i = 0.U64
	while $i < 256 {
		name = "flag${($i * 7 % 29).to_str()}"
		if $i % 4 == 0 {
			$actual = $actual.remove(name)
			$expected = $expected.remove(name)
		} else {
			value = $i % 3 == 0
			$actual = $actual.insert(name, value)
			$expected = $expected.insert(name, value)
		}
		$ok = $ok and $actual.to_list() == $expected.to_list() and $actual.keys() == $expected.keys() and $actual == AsciiDocDict.from_dict($expected)
		$i = $i + 1
	}
	$ok
}

expect {
	left = AsciiDocDict.empty().insert("a", "one").insert("b", "two")
	right = AsciiDocDict.empty().insert("b", "two").insert("a", "one")
	tombstoned = left.remove("a").remove("b")
	restored = tombstoned.insert("b", "two").insert("a", "one")
	left == right and left == restored and left != right.insert("a", "different") and tombstoned.is_empty() and tombstoned.len() == 0 and tombstoned.keys() == [] and tombstoned.get("a") == Err(Missing) and left.to_list() == [("a", "one"), ("b", "two")]
}
