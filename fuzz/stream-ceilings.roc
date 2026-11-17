app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Parser
step : U64, List(U8), Parser.InputEnd -> Parser.Step(U64, U8, [])
step = |state, bytes, end| match bytes.first() {
	Ok(byte) => { state: state + 1, consumed: 1, outcome: Item(byte) }
	Err(_) => { state, consumed: 0, outcome: if (end == Final) Done else NeedInput }
}

## Generate distinct buffers within measurement and retain no completed items.
drain : U64 -> U64
drain = |count| {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| if index < count {
			Ok(([index.to_u8_wrap(), 1, 2, 3, 4, 5, 6, 7], index + 1))
		} else {
			Err(NoMore)
		},
	)
	for item in (Parser.decoder({ initial_state: 0, step })).read_iter(chunks) {
		match item {
			Ok(Value(_)) => {}
			Ok(Finished(report)) => return report.emitted_items
			_ => crash "valid source failed"
		}
	}
	crash "missing successful completion"
}

test! = |bytes| {
	count = [1.U64, 32, 256, 2048].get((bytes.first() ?? 0).to_u64() % 4) ?? 1
	measured = Fuzz.measure!("shared-stream", || drain(count))
	if measured.value != count * 8 {
		crash "stream lost items"
	}
	held = measured.stats.peak_live_bytes.minus_saturated(measured.stats.entry_live_bytes)
	if held > 65_536 {
		crash "shared adapter retained completed chunks"
	}
	if measured.stats.requested_bytes > 65_536 + count * 8192 {
		crash "shared allocation traffic exceeded linear ceiling"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "stream-ceilings", examples: [Fuzz.example({ name: "small", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large", revision: "1", make: || [3.U8] })], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
