app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.CSV
import parser.Streaming

## Generate chunks inside the measured region and discard every completed
## record. Shape 2 retains one early tiny field while later large chunks pass.
drain : U64, U64, Bool -> { records : U64, retained : List(U8) }
drain = |records, field_bytes, retain| {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| {
			if index >= records {
				Err(NoMore)
			} else {
				row = if (retain and index == 0) "tiny,${Str.repeat("y", field_bytes)}\n" else "x,${Str.repeat("y", field_bytes)}\n"
				Ok((row.to_utf8(), index + 1))
			}
		},
	)
	var $retained = []
	for event in Streaming.read_iter(chunks, CSV.decoder({})) {
		match event {
			Ok(Value(row)) => {
				if retain and $retained.is_empty() {
					$retained = row.first() ?? []
				}
			}
			Ok(Finished(report)) => return { records: report.emitted_items, retained: $retained }
			_ => crash "generated CSV did not parse"
		}
	}
	crash "missing completion"
}

## Return only the diagnostic after allocating its large input inside the
## measured region. Excerpts must own their short bytes rather than the source.
retain_error : U64, Bool -> CSV.Error
retain_error = |size, unknown_tag| {
	if unknown_tag {
		result : Try(List({ status : [Active, Inactive] }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
		result = CSV.parse("status\n${Str.repeat("x", size)}")
		match result {
			Err(InvalidCsv(error)) => error
			_ => crash "invalid tag did not yield a CSV error"
		}
	} else {
		result : Try(List({ count : U64 }), [InvalidCsv(CSV.Error), MissingRequiredField(Str)])
		result = CSV.parse("count\n${Str.repeat("x", size)}")
		match result {
			Err(InvalidCsv(error)) => error
			_ => crash "invalid number did not yield a CSV error"
		}
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	choice = (bytes.first() ?? 0).to_u64()
	if choice == 255 {
		for unknown_tag in [False, True] {
			measured = Fuzz.measure!(if (unknown_tag) "csv-unknown-tag-retained" else "csv-expected-value-retained", || retain_error(262_144 + choice, unknown_tag))
			valid = match measured.value.problem {
				ExpectedValue({ expected: U64, bytes: excerpt }) => !unknown_tag and excerpt == Str.repeat("x", 40).to_utf8()
				UnknownTag(excerpt) => unknown_tag and excerpt == Str.repeat("x", 40).to_utf8()
				_ => False
			}
			if !valid {
				crash "CSV diagnostic changed its problem payload"
			}
			if measured.stats.exit_live_bytes > measured.stats.entry_live_bytes + 2048 {
				crash "CSV diagnostic excerpt pinned a large source field"
			}
		}
	}
	shape = choice % 3
	n = choice // 3 + 1
	records = if (shape == 1) 4 else n * 32
	field_bytes = if (shape == 0) 32 else n * 256
	measured = Fuzz.measure!("csv-stream", || drain(records, field_bytes, shape == 2))
	if measured.value.records != records {
		crash "lost records"
	}
	if shape == 2 and measured.value.retained != "tiny".to_utf8() {
		crash "retained field changed"
	}
	if measured.stats.exit_live_bytes > measured.stats.entry_live_bytes + 2048 {
		crash "retaining a tiny field pinned an input chunk"
	}
	held = measured.stats.peak_live_bytes - measured.stats.entry_live_bytes
	if held > 32_768 + 32 * field_bytes {
		crash "CSV parser retained completed records or source chunks: ${held.to_str()}"
	}
	input_bytes = records * (field_bytes + 3)
	if measured.stats.requested_bytes > 65_536 + 64 * input_bytes {
		crash "CSV parser allocation traffic exceeded linear ceiling: ${measured.stats.requested_bytes.to_str()}"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	name: "csv-stream-ceilings",
	generator: Fuzz.raw_bytes,
	test!,
	show: Str.inspect,
	examples: [0.U8, 1, 2, 252, 253, 254, 255].map(|byte| Fuzz.example({ name: if (byte == 255) "retained-error-excerpts-256KiB" else "shape-${byte.to_str()}", revision: "1", make: || [byte] })),
})
