app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.RocSyntax

## Arbitrary bytes must produce a bounded, lossless and well-formed syntax tree.
## Invalid bytes and incomplete edits are useful input, never rejected.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	parsed = RocSyntax.parse_bytes(bytes)
	if parsed.source != bytes {
		crash "source bytes changed"
	}
	var $cursor = 0
	for token in parsed.tokens {
		if token.start != $cursor or token.end < token.start or token.end > bytes.len() {
			crash "noncontiguous or out-of-bounds token"
		}
		if token.kind != EndOfFile and token.start == token.end {
			crash "empty token must be EOF"
		}
		$cursor = token.end
	}
	if $cursor != bytes.len() {
		crash "token stream omitted source bytes"
	}
	last = parsed.tokens.last() ?? { kind: EndOfFile, start: 0, end: 0 }
	if last.kind != EndOfFile or last.end != bytes.len() {
		crash "missing terminal EOF"
	}
	if parsed.root >= parsed.nodes.len() {
		crash "invalid root index"
	}
	var $node_id = 0
	for node in parsed.nodes {
		if node.start > node.end or node.end > bytes.len() {
			crash "invalid node span"
		}
		if node.children_start + node.children_len > parsed.children.len() {
			crash "invalid children range"
		}
		var $offset = 0
		while $offset < node.children_len {
			child_id = parsed.children.get(node.children_start + $offset) ?? parsed.nodes.len()
			if child_id >= $node_id {
				crash "child does not precede parent; possible cycle"
			}
			match parsed.nodes.get(child_id) {
				Err(_) => crash "invalid child index"
				Ok(child) => if child.start < node.start or child.end > node.end {
					crash "child outside parent span"
				}
			}
			$offset = $offset + 1
		}
		$node_id = $node_id + 1
	}
	if parsed.tokens.keep_if(|token| token.kind == EndOfFile).len() != 1 {
		crash "expected exactly one EOF"
	}
	for diagnostic in parsed.diagnostics {
		if diagnostic.start > diagnostic.end or diagnostic.end > bytes.len() {
			crash "invalid diagnostic span"
		}
	}
	if RocSyntax.parse_bytes(bytes) != parsed {
		crash "parse not deterministic"
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "roc-syntax-raw", generator: Fuzz.raw_bytes, test, show: Str.inspect })
