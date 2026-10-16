## Internal persistent append buffer for syntax construction. Tree chunks never
## contain the growing flat output, so retaining an old parser state does not
## force a copy of every previously constructed node.
RocSyntaxBuffer(a) :: { count : U64, forest : Forest(a) }.{
	## An empty buffer. Type is inferred from appended values.
	empty : {} -> RocSyntaxBuffer(a)
	empty = |_| { count: 0, forest: Empty }

	## Number of appended values, without traversing the forest.
	len : RocSyntaxBuffer(a) -> U64
	len = |buffer| buffer.count

	## Persistent append: old snapshots remain valid. Binary carry merges equal
	## chunks; each merged pair is ordered older-left, newer-right.
	append : RocSyntaxBuffer(a), a -> RocSyntaxBuffer(a)
	append = |buffer, value| {
		{ count: buffer.count + 1, forest: carry(Leaf(value), buffer.forest) }
	}

	## Append a flat list in order while preserving the original buffer.
	concat_list : RocSyntaxBuffer(a), List(a) -> RocSyntaxBuffer(a)
	concat_list = |buffer, values| values.fold(buffer, |acc, value| acc.append(value))

	## Look up a source-order index in O(log n), including the forest scan.
	get : RocSyntaxBuffer(a), U64 -> Try(a, [OutOfBounds])
	get = |buffer, index| {
		if index >= buffer.count { return Err(OutOfBounds) }
		var $forest = buffer.forest
		var $remaining = buffer.count
		var $result = Err(OutOfBounds)
		var $done = False
		while !$done {
			match $forest {
				Empty => { $done = True }
				Link(tree, rest) => {
					base = $remaining - width(tree)
					if index >= base {
						$result = tree_get(tree, index - base)
						$done = True
					} else {
						$remaining = base
						$forest = rest
					}
				}
			}
		}
		$result
	}

	## Flatten once in source order, using an explicit O(log n) work stack.
	## Only this loop owns the growing output list.
	to_list : RocSyntaxBuffer(a) -> List(a)
	to_list = |buffer| {
		var $forest = buffer.forest
		var $todo = empty_forest({})
		var $done = False
		while !$done {
			match $forest {
				Empty => { $done = True }
				Link(tree, rest) => {
					$todo = Link(tree, $todo)
					$forest = rest
				}
			}
		}
		var $out = []
		$done = False
		while !$done {
			match $todo {
				Empty => { $done = True }
				Link(tree, rest) => {
					match tree {
						Leaf(value) => {
							$out = $out.append(value)
							$todo = rest
						}
						Branch(data) => { $todo = Link(data.left, Link(data.right, rest)) }
					}
				}
			}
		}
		$out
	}
}

# Linked forest avoids copying an O(log n) List spine on every append.
# Occupied trees have strictly increasing powers-of-two widths, newest first.
Forest(a) := [Empty, Link(Tree(a), Forest(a))]
Tree(a) := [Leaf(a), Branch({ size : U64, left : Tree(a), right : Tree(a) })]

empty_forest : {} -> Forest(a)
empty_forest = |_| Empty

width : Tree(a) -> U64
width = |tree| match tree {
	Leaf(_) => 1
	Branch(data) => data.size
}

carry : Tree(a), Forest(a) -> Forest(a)
carry = |tree, forest| match forest {
	Empty => Link(tree, Empty)
	Link(older, rest) => {
		if width(older) == width(tree) {
			carry(Branch({ size: width(tree) * 2, left: older, right: tree }), rest)
		} else { Link(tree, forest) }
	}
}

tree_get : Tree(a), U64 -> Try(a, [OutOfBounds])
tree_get = |tree, index| match tree {
	Leaf(value) => if index == 0 Ok(value) else Err(OutOfBounds)
	Branch(data) => {
		half = data.size // 2
		if index < half tree_get(data.left, index) else tree_get(data.right, index - half)
	}
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
