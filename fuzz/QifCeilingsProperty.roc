import parser.Parser
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
	for result in (QIF.decoder({})).read_iter(chunks) {
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

## Return only a small pending decoder after discarding a large source header.
## Shapes retain a long field, a pending Account N, or committed account context.
retain_pending : U8 -> QIF.Decoder
retain_pending = |shape| {
	name = "An account name beyond short string storage"
	value = "A field value beyond short string storage"
	tail = match shape {
		0 => "!Type:LongSectionNameBeyondShortStringStorage\nP${value}\n"
		1 => "!Account\nN${name}\n"
		_ => "!Account\nN${name}\n^\n!Type:Bank\nP${value}\n"
	}
	padding = Str.repeat("X", 262144)
	bytes = "!Comment ${padding}\n${tail}".to_utf8()
	var $state = QIF.decoder({ max_line_bytes: 524288, max_context_bytes: 524288 })
	var $position = 0
	while True {
		result = $state.step({ bytes: bytes.drop_first($position), end: More })
		$state = result.state
		$position = $position + result.consumed
		match result.outcome {
			Item(_) => {}
			NeedInput => {
				if $position != bytes.len() {
					crash "retention source was not consumed"
				}
				return $state
			}
			_ => crash "retention source rejected"
		}
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	if bytes.get(1) == Ok(255) {
		shape = (bytes.get(2) ?? 0) % 3
		# Keep the executable decoder local across the retained-state measurement.
		measurement = Fuzz.start_measure!("qif-pending-retention")
		decoder = retain_pending(shape)
		stats = measurement.finish!()
		if stats.exit_live_bytes > stats.entry_live_bytes + 4096 {
			crash "small pending QIF state retained discarded source allocation"
		}
		if stats.peak_live_bytes > stats.entry_live_bytes + 2097152 {
			crash "QIF retention audit exceeded bounded source peak"
		}
		finished = decoder.step({ bytes: "^\n".to_utf8(), end: Final })
		match finished.outcome {
			Item(Record(record)) => {
				section = if (shape == 0) "LongSectionNameBeyondShortStringStorage" else if (shape == 1) "Account" else "Bank"
				account = if (shape == 2) "An account name beyond short string storage" else ""
				field = if (shape == 1) { code: 'N', value: "An account name beyond short string storage" } else { code: 'P', value: "A field value beyond short string storage" }
				if record.section != section or record.account != account or record.fields != [field] {
					crash "pending QIF retention audit changed fields or context"
				}
			}
			_ => crash "pending QIF record did not complete: ${Str.inspect(finished.outcome)}"
		}
		return Fuzz.keep
	}
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

property_target = Fuzz.target_with!({ name: "qif-ceilings", examples: [Fuzz.example({ name: "small", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large", revision: "1", make: || [3.U8] }), Fuzz.example({ name: "retain-field", revision: "1", make: || [0.U8, 255, 0] }), Fuzz.example({ name: "retain-account-name", revision: "1", make: || [0.U8, 255, 1] }), Fuzz.example({ name: "retain-account-context", revision: "1", make: || [0.U8, 255, 2] })], generator: Fuzz.raw_bytes, test!, show: Str.inspect })

QifCeilingsProperty :: [].{
	target = property_target
}
