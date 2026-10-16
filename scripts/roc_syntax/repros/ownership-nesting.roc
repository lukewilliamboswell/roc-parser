app [target] { fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst" }
import fuzz.Fuzz

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
 mode = bytes.get(0) ?? 0
 depth = U8.to_u64((bytes.get(1) ?? 10) % 17)
 source = List.repeat(7.U8, 65536)
 measured = Fuzz.measure_allocs!(|{}| run_model(mode, source, depth))
 crash "OWNERSHIP allocations=${measured.allocations.to_str()} checksum=${checksum(measured.value).to_str()} nodes=${measured.value.nodes.len().to_str()} edges=${measured.value.children.len().to_str()}"
}
target = Fuzz.target_with!({ name: "ownership-proof", generator: Fuzz.raw_bytes, test!, show: Str.inspect })

# Ownership model: both functions append the same binary tree in postorder.
TreeNode : { kind : U64, start : U64, end : U64, child_start : U64, child_len : U64 }
Builder : { nodes : List(TreeNode), children : List(U64) }
Cursor : { source : List(U8), pos : U64 }
NestedResult : { cursor : Cursor, builder : Builder, node : U64 }
FlatResult : { cursor : Cursor, nodes : List(TreeNode), children : List(U64), node : U64 }
nested : Cursor, List(TreeNode), List(U64), U64 -> NestedResult
nested = |cursor, nodes, children, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, builder: { nodes: nodes.append(item), children: children }, node: index }
 } else {
  { cursor: lc, builder: { nodes: ln, children: le }, node: left } = nested(cursor, nodes, children, depth - 1)
  { cursor: rc, builder: { nodes: rn, children: re }, node: right } = nested(lc, ln, le, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, builder: { nodes: rn.append(item), children: re.append(left).append(right) }, node: index }
 }
}

flat : Cursor, List(TreeNode), List(U64), U64 -> FlatResult
flat = |cursor, nodes, children, depth| {
 if depth == 0 {
  index = nodes.len()
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  item = { kind, start: cursor.pos, end: cursor.pos + 1, child_start: children.len(), child_len: 0 }
  { cursor: { ..cursor, pos: cursor.pos + 1 }, nodes: nodes.append(item), children: children, node: index }
 } else {
  { cursor: lc, nodes: ln, children: le, node: left } = flat(cursor, nodes, children, depth - 1)
  { cursor: rc, nodes: rn, children: re, node: right } = flat(lc, ln, le, depth - 1)
  kind = U8.to_u64(cursor.source.get(cursor.pos) ?? 0)
  index = rn.len()
  item = { kind, start: cursor.pos, end: rc.pos, child_start: re.len(), child_len: 2 }
  { cursor: rc, nodes: rn.append(item), children: re.append(left).append(right), node: index }
 }
}

run_model : U8, List(U8), U64 -> Builder
run_model = |mode, source, depth| {
 cursor = { source, pos: 0 }
 if mode == 0 { nested(cursor, [], [], depth).builder } else {
  result = flat(cursor, [], [], depth)
  { nodes: result.nodes, children: result.children }
 }
}

checksum : Builder -> U64
checksum = |builder| builder.nodes.fold(0, |n, item| n + item.kind + item.start + item.end + item.child_start + item.child_len) + builder.children.fold(0, |n, item| n + item)

expect {
 source = List.repeat(7.U8, 1024)
 before = run_model(0, source, 10)
 after = run_model(1, source, 10)
 before == after and before.nodes.len() == 2047 and before.children.len() == 2046
}
