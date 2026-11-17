app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import parser.Streaming
import fuzz.Fuzz
import parser.QIF

## Distinct chunks are generated inside the measurement; consumed values are discarded.
drain : U64 -> U64
drain = |n| {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| {
			if index == 0 {
				Ok(("!Type:Bank\n".to_utf8(), 1))
			}
			else if index <= n {
				Ok(("D27/08/2024\nT1.00\nPitem${index.to_str()}\n^\n".to_utf8(), index + 1))
			}
			else {
				Err(NoMore)
			}
		},
	)
	var $count = 0.U64
	for result in Streaming.read_iter(chunks, QIF.decoder({})) {
		match result {
			Ok(Value(Record(_))) => {
				$count = $count + 1
			}
			Ok(_) => {}
			Err(_) => crash "generated QIF rejected"
		}
	}
	$count
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	n = [1.U64, 32, 256, 2048].get((bytes.first() ?? 0).to_u64() % 4) ?? 1
	measured = Fuzz.measure!("qif-stream", || drain(n))
	if measured.value != n {
		crash "QIF stream lost generated records"
	}
	held = measured.stats.peak_live_bytes.minus_saturated(measured.stats.entry_live_bytes)
	if held > 65_536 {
		crash "QIF stream retained completed input"
	}
	if measured.stats.requested_bytes > 65_536 + 16_384 * n {
		crash "QIF allocation traffic exceeded linear ceiling"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "qif-ceilings", examples: [Fuzz.example({ name: "small", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large", revision: "1", make: || [3.U8] })], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
