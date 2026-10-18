## Internal persistent radix-32 append vector with a bounded active tail.
## Full tails are archived once per 32 appends. Each archived path contains five lists
## of at most 32 entries under the parser's 1,048,576-token resource budget.
## Retaining parser snapshots copies bounded paths, never the growing output.
## Fixed levels also avoid recursive generic type expansion in the compiler.
RocSyntaxBuffer(a) := { count : U64, blocks : List(List(List(List(List(a))))), tail : List(a) }.{

	## Empty construction buffer; its element type is inferred from appends.
	empty : {} -> RocSyntaxBuffer(a)
	empty = |_| RocSyntaxBuffer.{ count: 0, blocks: [], tail: [] }

	## Number of appended values.
	len : RocSyntaxBuffer(a) -> U64
	len = |buffer| buffer.count

	## Preserve old snapshots and copy only the bounded radix path to a new value.
	append : RocSyntaxBuffer(a), a -> RocSyntaxBuffer(a)
	append = |RocSyntaxBuffer.{ count, blocks, tail }, value| {
		index = count
		if tail.len() < 32 {
			RocSyntaxBuffer.{ count: index + 1, blocks, tail: tail.append(value) }
		} else {
			# Archive a complete leaf once per 32 appends. The active tail only
			# copies its bounded leaf when a parser snapshot still retains it.
			full_start = index - 32
			i4 = full_start // 1048576
			i3 = (full_start // 32768) % 32
			i2 = (full_start // 1024) % 32
			i1 = (full_start // 32) % 32
			{ list: root, prev: b3 } = extract(blocks, i4)
			{ list: rest3, prev: b2 } = extract(b3, i3)
			{ list: rest2, prev: b1 } = extract(b2, i2)
			next1 = put(b1, i1, tail)
			next2 = put(rest2, i2, next1)
			next3 = put(rest3, i3, next2)
			RocSyntaxBuffer.{ count: index + 1, blocks: put(root, i4, next3), tail: List.with_capacity(32).append(value) }
		}
	}

	## Archive one complete 32-entry leaf held outside the buffer. Callers keep the
	## active tail as a direct field of their own state so appends reuse it in place;
	## an active tail nested inside this record is projected and shared before append
	## in native builds, copying it on every append.
	archive : RocSyntaxBuffer(a), List(a) -> RocSyntaxBuffer(a)
	archive = |RocSyntaxBuffer.{ count, blocks, tail }, leaf| {
		full_start = count
		i4 = full_start // 1048576
		i3 = (full_start // 32768) % 32
		i2 = (full_start // 1024) % 32
		i1 = (full_start // 32) % 32
		{ list: root, prev: b3 } = extract(blocks, i4)
		{ list: rest3, prev: b2 } = extract(b3, i3)
		{ list: rest2, prev: b1 } = extract(b2, i2)
		next1 = put(b1, i1, leaf)
		next2 = put(rest2, i2, next1)
		next3 = put(rest3, i3, next2)
		RocSyntaxBuffer.{ count: count + leaf.len(), blocks: put(root, i4, next3), tail }
	}

	## Look up an index across archived leaves and an external active tail.
	get_with : RocSyntaxBuffer(a), List(a), U64 -> Try(a, [OutOfBounds])
	get_with = |buffer, tail, index| if index >= buffer.count tail.get(index - buffer.count) else buffer.get(index)

	## Flatten archived leaves followed by an external active tail.
	to_list_with : RocSyntaxBuffer(a), List(a) -> List(a)
	to_list_with = |buffer, tail| {
		var $out = List.with_capacity(buffer.count + tail.len())
		for b3 in buffer.blocks {
			for b2 in b3 {
				for b1 in b2 {
					for leaf in b1 {
						for value in leaf {
							$out = $out.append(value)
						}
					}
				}
			}
		}
		for value in tail {
			$out = $out.append(value)
		}
		$out
	}

	## Append a list in order, without flattening previous contents.
	concat_list : RocSyntaxBuffer(a), List(a) -> RocSyntaxBuffer(a)
	concat_list = |buffer, values| values.fold(buffer, |acc, value| acc.append(value))

	get : RocSyntaxBuffer(a), U64 -> Try(a, [OutOfBounds])
	get = |buffer, index| {
		if index >= buffer.count {
			return Err(OutOfBounds)
		}
		tail_start = buffer.count - buffer.tail.len()
		if index >= tail_start {
			return buffer.tail.get(index - tail_start)
		}
		b3 = buffer.blocks.get(index // 1048576)?
		b2 = b3.get((index // 32768) % 32)?
		b1 = b2.get((index // 1024) % 32)?
		leaf = b1.get((index // 32) % 32)?
		leaf.get(index % 32)
	}

	to_list : RocSyntaxBuffer(a) -> List(a)
	to_list = |buffer| {
		var $out = List.with_capacity(buffer.count)
		for b3 in buffer.blocks {
			for b2 in b3 {
				for b1 in b2 {
					for leaf in b1 {
						for value in leaf {
							$out = $out.append(value)
						}
					}
				}
			}
		}
		for value in buffer.tail {
			$out = $out.append(value)
		}
		$out
	}
}

# Indices are either an existing active path or exactly the next append slot.
# The placeholder is restored by put before this path can escape to a caller.
extract : List(List(a)), U64 -> { list : List(List(a)), prev : List(a) }
extract = |values, index| {
	if index < values.len() values.replace(index, []) ?? { list: [], prev: [] } else { list: values, prev: [] }
}

put : List(a), U64, a -> List(a)
put = |values, index, value| {
	if index == values.len() values.append(value) else values.set(index, value) ?? []
}

expect {
	buffer = RocSyntaxBuffer.empty({})
	buffer.len() == 0 and buffer.get(0) == Err(OutOfBounds) and buffer.to_list().is_empty()
}

expect {
	before = RocSyntaxBuffer.empty({}).concat_list([10, 20, 30])
	after = before.append(40).append(50)
	before.to_list() == [10, 20, 30] and after.to_list() == [10, 20, 30, 40, 50] and before.get(3) == Err(OutOfBounds) and after.get(4) == Ok(50)
}

expect {
	var $buffer = RocSyntaxBuffer.empty({})
	var $expected = []
	var $i = 0.U64
	while $i < 4097 {
		$buffer = $buffer.append($i)
		$expected = $expected.append($i)
		$i = $i + 1
	}
	actual = $buffer.to_list()
	var $lookups = True
	$i = 0
	while $i < 4097 {
		$lookups = $lookups and $buffer.get($i) == Ok($i)
		$i = $i + 1
	}
	$lookups and actual == $expected and $buffer.len() == 4097 and $buffer.get(0) == Ok(0) and $buffer.get(2048) == Ok(2048) and $buffer.get(4096) == Ok(4096) and $buffer.get(4097) == Err(OutOfBounds)
}

expect {
	# Every saved prefix remains valid across repeated carry boundaries.
	var $buffer = RocSyntaxBuffer.empty({})
	var $prefixes = []
	var $i = 0.U64
	while $i < 129 {
		$prefixes = $prefixes.append($buffer)
		$buffer = $buffer.append($i)
		$i = $i + 1
	}
	final_values = $buffer.to_list()
	$prefixes.all(|prefix| prefix.to_list() == final_values.take_first(prefix.len()) and (prefix.len() == 0 or prefix.get(prefix.len() - 1) == Ok(prefix.len() - 1)))
}

expect {
	# Exercise the next radix level without keeping a flat expected copy alive.
	var $buffer = RocSyntaxBuffer.empty({})
	var $i = 0.U64
	while $i < 32769 {
		$buffer = $buffer.append($i)
		$i = $i + 1
	}
	$buffer.len() == 32769 and $buffer.get(31) == Ok(31) and $buffer.get(32) == Ok(32) and $buffer.get(1023) == Ok(1023) and $buffer.get(1024) == Ok(1024) and $buffer.get(32767) == Ok(32767) and $buffer.get(32768) == Ok(32768) and $buffer.to_list().last() == Ok(32768)
}

expect {
	# Saved paths remain shared across both internal carry boundaries.
	var $buffer = RocSyntaxBuffer.empty({})
	var $prefixes = []
	var $i = 0.U64
	while $i < 32770 {
		if $i == 1023 or $i == 1024 or $i == 32767 or $i == 32768 {
			$prefixes = $prefixes.append($buffer)
		}
		$buffer = $buffer.append($i)
		$i = $i + 1
	}
	final_values = $buffer.to_list()
	$prefixes.all(|prefix| prefix.to_list() == final_values.take_first(prefix.len()) and prefix.get(prefix.len()) == Err(OutOfBounds)) and $buffer.get(32769) == Ok(32769)
}

expect {
	# External active tails archive exactly at leaf boundaries.
	var $buffer = RocSyntaxBuffer.empty({})
	var $tail = []
	var $i = 0.U64
	while $i < 2085 {
		if $tail.len() == 32 {
			$buffer = $buffer.archive($tail)
			$tail = []
		}
		$tail = $tail.append($i)
		$i = $i + 1
	}
	flat = $buffer.to_list_with($tail)
	flat.len() == 2085 and flat.first() == Ok(0) and flat.last() == Ok(2084) and $buffer.get_with($tail, 1023) == Ok(1023) and $buffer.get_with($tail, 2084) == Ok(2084) and $buffer.get_with($tail, 2085) == Err(OutOfBounds) and flat.map_with_index(|v, i| v == i).all(|ok| ok)
}

expect {
	# Ownership transfer preserves nested reference-counted leaf elements too.
	before = RocSyntaxBuffer.empty({}).concat_list(["alpha", "beta"])
	after = before.append("gamma")
	before.to_list() == ["alpha", "beta"] and after.to_list() == ["alpha", "beta", "gamma"]
}
