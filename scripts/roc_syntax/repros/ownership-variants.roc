app [target] { fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst" }
import fuzz.Fuzz

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
 mode = bytes.get(0) ?? 0
 depth = U8.to_u64((bytes.get(1) ?? 10) % 17)
 source = List.repeat(7.U8, 65536)
 measured = Fuzz.measure_allocs!(|{}| run_variant(mode, source, depth))
 crash "OWNERSHIP allocations=${measured.allocations.to_str()} checksum=${checksum(measured.value).to_str()} nodes=${measured.value.nodes.len().to_str()} edges=${measured.value.children.len().to_str()}"
}
target = Fuzz.target_with!({ name: "ownership-proof", generator: Fuzz.raw_bytes, test!, show: Str.inspect })

FlatResult : { cursor : Cursor, nodes : List(TreeNode), children : List(U64), node : U64 }
TreeNode : { kind : U64, start : U64, end : U64, child_start : U64, child_len : U64 }
Builder : { nodes : List(TreeNode), children : List(U64) }
Cursor : { source : List(U8), pos : U64 }
Combined : { source : List(U8), pos : U64, nodes : List(TreeNode), children : List(U64) }
CombinedResult : { state : Combined, node : U64 }
SplitResult : { cursor : Cursor, builder : Builder, node : U64 }

add_leaf : Builder, U64, U64, U64 -> Builder
add_leaf = |{ nodes, children }, kind, start, end| {
	item = { kind, start, end, child_start: children.len(), child_len: 0 }
	{ nodes: nodes.append(item), children }
}

add_branch : Builder, U64, U64, U64, U64, U64 -> Builder
add_branch = |{ nodes, children }, kind, start, end, left, right| {
	item = { kind, start, end, child_start: children.len(), child_len: 2 }
	{ nodes: nodes.append(item), children: children.append(left).append(right) }
}


v0 : Cursor, Builder, U64 -> SplitResult
v0 = |cursor, builder, depth| {
 if depth == 0 {
  index = builder.nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  out = add_leaf({ nodes: builder.nodes, children: builder.children }, kind, cursor.pos, cursor.pos + 1)
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: out, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = v0(cursor, builder, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = v0(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  out = add_branch({ nodes: rn, children: re }, kind, cursor.pos, rc.pos, left, right)
  { cursor: rc, builder: out, node: index }
 }
}

v1 : Cursor, Builder, U64 -> SplitResult
v1 = |cursor, { nodes, children }, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: { nodes: nodes.append(item), children: children }, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = v1(cursor, { nodes, children }, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = v1(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, builder: { nodes: rn.append(item), children: re.append(left).append(right) }, node: index }
 }
}

v2 : Cursor, Builder, U64 -> FlatResult
v2 = |cursor, { nodes, children }, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, nodes: nodes.append(item), children: children, node: index }
 } else {
  { cursor: lc, nodes: ln, children: le, node: left } = v2(cursor, { nodes, children }, depth - 1)
  { cursor: rc, nodes: rn, children: re, node: right } = v2(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, nodes: rn.append(item), children: re.append(left).append(right), node: index }
 }
}

v3 : Cursor, List(TreeNode), List(U64), U64 -> SplitResult
v3 = |cursor, nodes, children, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: { nodes: nodes.append(item), children: children }, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = v3(cursor, nodes, children, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = v3(lc, ln, le, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, builder: { nodes: rn.append(item), children: re.append(left).append(right) }, node: index }
 }
}

v4 : Cursor, List(TreeNode), List(U64), U64 -> FlatResult
v4 = |cursor, nodes, children, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, nodes: nodes.append(item), children: children, node: index }
 } else {
  { cursor: lc, nodes: ln, children: le, node: left } = v4(cursor, nodes, children, depth - 1)
  { cursor: rc, nodes: rn, children: re, node: right } = v4(lc, ln, le, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, nodes: rn.append(item), children: re.append(left).append(right), node: index }
 }
}

v5 : Cursor, Builder, U64 -> FlatResult
v5 = |cursor, builder, depth| {
 if depth == 0 {
  index = builder.nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: builder.children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, nodes: builder.nodes.append(item), children: builder.children, node: index }
 } else {
  { cursor: lc, nodes: ln, children: le, node: left } = v5(cursor, builder, depth - 1)
  { cursor: rc, nodes: rn, children: re, node: right } = v5(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, nodes: rn.append(item), children: re.append(left).append(right), node: index }
 }
}

v6 : Cursor, Builder, U64 -> SplitResult
v6 = |cursor, { nodes, children }, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  out = add_leaf({ nodes: nodes, children: children }, kind, cursor.pos, cursor.pos + 1)
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: out, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = v6(cursor, { nodes, children }, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = v6(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  out = add_branch({ nodes: rn, children: re }, kind, cursor.pos, rc.pos, left, right)
  { cursor: rc, builder: out, node: index }
 }
}

v7 : Cursor, Builder, U64 -> SplitResult
v7 = |cursor, builder, depth| {
 if depth == 0 {
  index = builder.nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: builder.children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: { nodes: builder.nodes.append(item), children: builder.children }, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = v7(cursor, builder, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = v7(lc, { nodes: ln, children: le }, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, builder: { nodes: rn.append(item), children: re.append(left).append(right) }, node: index }
 }
}

run_variant : U8, List(U8), U64 -> Builder
run_variant = |mode, source, depth| {
 cursor = { source, pos: 0 }
 match mode {
0 => { result = v0(cursor, { nodes: [], children: [] }, depth)
 result.builder }
1 => { result = v1(cursor, { nodes: [], children: [] }, depth)
 result.builder }
2 => { result = v2(cursor, { nodes: [], children: [] }, depth)
 { nodes: result.nodes, children: result.children } }
3 => { result = v3(cursor, [], [], depth)
 result.builder }
4 => { result = v4(cursor, [], [], depth)
 { nodes: result.nodes, children: result.children } }
5 => { result = v5(cursor, { nodes: [], children: [] }, depth)
 { nodes: result.nodes, children: result.children } }
6 => { result = v6(cursor, { nodes: [], children: [] }, depth)
 result.builder }
7 => { result = v7(cursor, { nodes: [], children: [] }, depth)
 result.builder }
 _ => { nodes: [], children: [] }
 }
}
checksum : Builder -> U64
checksum = |builder| builder.nodes.fold(0, |n, item| n + item.kind + item.start + item.end + item.child_start + item.child_len) + builder.children.fold(0, |n, item| n + item)

expect {
 source = List.repeat(7.U8, 1024)
 expected = run_variant(4, source, 10)
 [0.U8, 1, 2, 3, 4, 5, 6, 7].all(|mode| run_variant(mode, source, 10) == expected)
}
