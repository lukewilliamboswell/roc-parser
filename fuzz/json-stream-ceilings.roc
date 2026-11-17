app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.JsonValue
import parser.Parser

## Generate distinct records within the measured region. Keep no source list,
## parsed values, or output chunks after the current pull.
payload : U64 -> Str
payload = |index| Str.repeat(if (index % 2 == 0) "a" else "b", 512)

source : U64, [Ndjson, Array] -> Iter(List(U8))
source = |count, format| Iter.custom(
	0.U64,
	Err(Unknown),
	|index| {
		if format == Array and index == 0 {
			Ok(("[".to_utf8(), 1))
		} else if format == Array and index == count + 1 {
			Ok(("]".to_utf8(), index + 1))
		} else {
			at = if (format == Array) index - 1 else index
			if at >= count {
				return Err(NoMore)
			}
			text = "{\"i\":${at.to_str()},\"payload\":\"${payload(at)}\"}"
			part = if (format == Array) "${if (at == 0) "" else ","}${text}" else "${text}\n"
			Ok((part.to_utf8(), index + 1))
		}
	},
)

values : U64 -> Iter(JsonValue)
values = |count| Iter.custom(
	0.U64,
	Err(Unknown),
	|index| {
		if index >= count {
			Err(NoMore)
		}
		else {
			value = JsonValue.Object([{ key: "i", value: Number(index.to_str()) }, { key: "payload", value: String(payload(index)) }])
			Ok((value, index + 1))
		}
	},
)

read : U64, [Ndjson, Array] -> U64
read = |count, format| {
	var $checksum = 0.U64
	var $records = 0.U64
	var $finished = False
	for result in (JsonValue.record_decoder({ framing: format })).read_iter(source(count, format)) {
		match result {
			Ok(Value(value)) => {
				index = match value.get("i") {
					Ok(number) => number.as_u64() ?? crash "generated index is invalid"
					_ => crash "generated record has no index"
				}
				text = match value.get("payload") {
					Ok(string) => string.as_str() ?? crash "generated payload is invalid"
					_ => crash "generated record has no payload"
				}
				if index != $records or text != payload(index) {
					crash "JSON stream changed record content"
				}
				$checksum = $checksum + index + text.count_utf8_bytes()
				$records = $records + 1
			}
			Ok(Finished(report)) => {
				if report.emitted_items != count or report.rejected_records != 0 {
					crash "JSON stream report lost records"
				}
				$finished = True
			}
			_ => crash "generated JSON records were rejected"
		}
	}
	if !$finished or $records != count {
		crash "JSON stream omitted completion or records"
	}
	$checksum
}

write : U64, [Ndjson, Array] -> U64
write = |count, format| {
	var $bytes = 0.U64
	for part in (JsonValue.record_encoder({ framing: format, chunk_bytes: 97 })).write_iter(values(count)) {
		match part {
			Ok(chunk) => {
				if chunk.is_empty() or chunk.len() > 97 {
					crash "JSON writer exceeded its chunk bound"
				}
				$bytes = $bytes + chunk.len()
			}
			Err(_) => crash "generated JSON values failed to encode"
		}
	}
	$bytes
}

pipeline! : U64, [Ndjson, Array] => U64
pipeline! = |count, format| {
	input : Stream(Try(List(U8), [Injected]))
	input = source(count, format).map(|chunk| Ok(chunk)).stream()
	var $bytes = 0.U64
	for! part in (JsonValue.record_encoder({ framing: format, chunk_bytes: 97 })).write_stream((JsonValue.record_decoder({ framing: format })).values_stream(input)) {
		match part {
			Ok(chunk) => {
				if chunk.is_empty() or chunk.len() > 97 {
					crash "JSON pipeline exceeded its chunk bound"
				}
				$bytes = $bytes + chunk.len()
			}
			Err(_) => crash "generated JSON pipeline failed"
		}
	}
	$bytes
}

run! : U64, [Ndjson, Array], U64 => U64
run! = |count, format, operation| match operation {
	0 => read(count, format)
	1 => write(count, format)
	_ => pipeline!(count, format)
}

expected : U64, [Ndjson, Array], U64 -> U64
expected = |count, format, operation| {
	if operation == 0 {
		return count * 512 + count * count.minus_saturated(1) // 2
	}
	var $digits = 0.U64
	for index in (0.U64).to(count - 1) {
		$digits = $digits + index.to_str().count_utf8_bytes()
	}
	# A record has 19 punctuation/key bytes, its decimal index, 512 payload
	# bytes, and one newline/comma. Array framing adds one further byte.
	count * (19 + 512 + 1) + $digits + if (format == Array) 1 else 0
}

# A retained 32-byte string must keep only its own framed record, even when
# it was read from a much larger source chunk that has since been drained.
retain_sample : [Ndjson, Array], U64 -> Str
retain_sample = |format, salt| {
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| {
			if index > 0 {
				return Err(NoMore)
			}
			# The runtime prefix forces a heap-backed source chunk; a static
			# constant would make source-retention accounting vacuous.
			value = "${salt.to_str()}${Str.repeat("t", 30)}"
			padding = Str.repeat(" ", 262_144)
			wire = if (format == Ndjson) "\"${value}\"\n${padding}\n" else "[\"${value}\",${padding}null]"
			Ok((wire.to_utf8(), 1))
		},
	)
	var $retained = ""
	var $finished = False
	for result in (JsonValue.record_decoder({ framing: format })).read_iter(chunks) {
		match result {
			Ok(Value(String(text))) => {
				$retained = text
			}
			Ok(Value(Null)) => {}
			Ok(Finished(_)) => {
				$finished = True
			}
			_ => crash "retained JSON sample failed"
		}
	}
	if !$finished {
		crash "retained JSON sample omitted completion"
	}
	$retained
}

# One-byte chunks must grow a single record buffer, rather than copy the
# unfinished record on every pull.
fragmented_sample : U64 -> U64
fragmented_sample = |size| {
	bytes = "[\"${Str.repeat("x", size)}\"]".to_utf8()
	chunks = Iter.custom(
		0.U64,
		Err(Unknown),
		|index| if index < bytes.len() {
			Ok(([bytes.get(index) ?? 0], index + 1))
		} else {
			Err(NoMore)
		},
	)
	var $length = 0.U64
	var $finished = False
	for result in (JsonValue.record_decoder({ framing: Array })).read_iter(chunks) {
		match result {
			Ok(Value(String(text))) => {
				$length = $length + text.count_utf8_bytes()
			}
			Ok(Finished(_)) => {
				$finished = True
			}
			_ => crash "fragmented JSON sample failed"
		}
	}
	if !$finished {
		crash "fragmented JSON sample omitted completion"
	}
	$length
}

test! = |bytes| {
	choice = (bytes.first() ?? 0).to_u64()
	count = [1.U64, 32, 64, 2048].get(choice // 6 % 4) ?? 1
	format = if (choice % 2 == 0) Ndjson else Array
	operation = choice // 2 % 3
	small = Fuzz.measure!("json-stream-small", || run!(32, format, operation))
	large = Fuzz.measure!("json-stream-large", || run!(count, format, operation))
	if small.value != expected(32, format, operation) or large.value != expected(count, format, operation) {
		crash "JSON ceiling pipeline lost values or bytes: got ${small.value.to_str()},${large.value.to_str()} expected ${expected(32, format, operation).to_str()},${expected(count, format, operation).to_str()}"
	}
	small_held = small.stats.peak_live_bytes.minus_saturated(small.stats.entry_live_bytes)
	large_held = large.stats.peak_live_bytes.minus_saturated(large.stats.entry_live_bytes)
	if small_held > 131_072 or large_held > 131_072 {
		crash "JSON stream retained completed records or chunks"
	}
	if large_held > small_held + 16_384 {
		crash "JSON live memory grew with completed records"
	}
	if large.stats.exit_live_bytes > large.stats.entry_live_bytes + 2048 {
		crash "JSON stream left input or output retained after completion"
	}
	if large.stats.requested_bytes > 65_536 + count * 32_768 {
		crash "JSON allocation traffic exceeded its linear ceiling"
	}
	if choice == 24 or choice == 25 {
		retained = Fuzz.measure!("json-retained-source", || retain_sample(format, choice))
		if retained.value != "${choice.to_str()}${Str.repeat("t", 30)}" {
			crash "retained JSON value changed"
		}
		if retained.stats.exit_live_bytes > retained.stats.entry_live_bytes + 2048 {
			crash "small JSON value pinned a completed source chunk"
		}
	}
	if choice == 26 {
		size = 4096 + choice
		# Keep the size dependent on runtime input so the pure pipeline is
		# measured rather than evaluated as a compile-time constant.
		fragmented = Fuzz.measure!("json-one-byte-record", || fragmented_sample(size))
		if fragmented.value != size or fragmented.stats.requested_bytes > 65_536 + 64 * size {
			crash "fragmented JSON record allocation grew quadratically"
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with!({
	name: "json-stream-ceilings",
	examples: [0.U8, 1, 2, 3, 4, 5, 18, 19, 20, 21, 22, 23, 24, 25, 26].map(
		|byte| {
			operation = match byte.to_u64() // 2 % 3 {
				0 => "reader"
				1 => "writer"
				_ => "pipeline"
			}
			format = if (byte % 2 == 0) "ndjson" else "array"
			size = if (byte == 26) "one-byte-record" else if (byte >= 24) "retained-source" else if (byte < 18) "small" else "2048-records"
			Fuzz.example({ name: if (byte == 26) "one-byte-record-array-reader" else "${size}-${format}-${operation}", revision: "1", make: || [byte] })
		},
	),
	# Coverage-generated entropy selects at most 64 records. Named examples
	# exercise 2,048-record streams without repeating them in every smoke run.
	generator: Fuzz.map(Fuzz.raw_bytes, |input| [(input.first() ?? 0) % 18]),
	test!,
	show: Str.inspect,
})
