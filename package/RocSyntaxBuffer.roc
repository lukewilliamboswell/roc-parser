## Internal persistent radix-32 append vector. Each path contains five lists
## of at most 32 entries under the parser's 1,048,576-token resource budget.
## Retaining parser snapshots copies bounded paths, never the growing output.
## Fixed levels also avoid recursive generic type expansion in the compiler.
RocSyntaxBuffer(a) := { count : U64, blocks : List(List(List(List(List(a))))) }.{

	## Empty construction buffer; its element type is inferred from appends.
	empty : {} -> RocSyntaxBuffer(a)
	empty = |_| RocSyntaxBuffer.{ count: 0, blocks: [] }

	## Number of appended values.
	len : RocSyntaxBuffer(a) -> U64
	len = |buffer| buffer.count

	## Preserve old snapshots and copy only the bounded radix path to a new value.
	append : RocSyntaxBuffer(a), a -> RocSyntaxBuffer(a)
	append = |buffer, value| {
		index = buffer.count
		i4 = index // 1048576
		i3 = (index // 32768) % 32
		i2 = (index // 1024) % 32
		i1 = (index // 32) % 32
		b3 = buffer.blocks.get(i4) ?? []
		b2 = b3.get(i3) ?? []
		b1 = b2.get(i2) ?? []
		leaf = b1.get(i1) ?? []
		next1 = put(b1, i1, leaf.append(value))
		next2 = put(b2, i2, next1)
		next3 = put(b3, i3, next2)
		RocSyntaxBuffer.{ count: index + 1, blocks: put(buffer.blocks, i4, next3) }
	}

	## Append a list in order, without flattening previous contents.
	concat_list : RocSyntaxBuffer(a), List(a) -> RocSyntaxBuffer(a)
	concat_list = |buffer, values| values.fold(buffer, |acc, value| acc.append(value))

	get : RocSyntaxBuffer(a), U64 -> Try(a, [OutOfBounds])
	get = |buffer, index| {
		if index >= buffer.count {
			return Err(OutOfBounds)
		}
		b3 = buffer.blocks.get(index // 1048576)?
		b2 = b3.get((index // 32768) % 32)?
		b1 = b2.get((index // 1024) % 32)?
		leaf = b1.get((index // 32) % 32)?
		leaf.get(index % 32)
	}

	to_list : RocSyntaxBuffer(a) -> List(a)
	to_list = |buffer| {
		var $out = []
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
		$out
	}
}

put : List(a), U64, a -> List(a)
put = |values, index, value| {
	if index == values.len() values.append(value) else values.set(index, value) ?? values
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
