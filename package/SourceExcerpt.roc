## Owned, bounded original bytes for diagnostics. No source allocation is retained.
SourceExcerpt := { bytes : List(U8), total_bytes : U64 }.{
	is_eq : _
	to_hash : _

	from_bytes : List(U8) -> SourceExcerpt
	from_bytes = |source| {
		maximum = 40
		count = if (source.len() < maximum) source.len() else maximum
		var $bytes = List.with_capacity(count)
		for byte in source.take_first(count).iter() {
			$bytes = $bytes.append(byte)
		}
		{ bytes: $bytes, total_bytes: source.len() }
	}
}

## Excerpts own at most forty bytes and preserve the original byte count.
expect {
	source = List.repeat('x', 100)
	excerpt = SourceExcerpt.from_bytes(source)
	excerpt == { bytes: List.repeat('x', 40), total_bytes: 100 }
}
