## Shared pull adapters for incremental document decoders.
##
## A decoder consumes byte chunks and returns one item, a safely framed record
## error, or a request for more input. Only source exhaustion supplies `Final`.
## Construction is lazy. Consume every item, including the final `Finished`,
## to establish that the entire document passed validation.
Streaming :: [].{

	## A byte chunk; boundaries have no encoding or record significance.
	Bytes : List(U8)

	## Whether further bytes may follow the supplied input.
	InputEnd : [More, Final]

	## Recover only errors for records whose end is independently established.
	ErrorPolicy : [FailFast, SkipInvalidRecords, ReportInvalidRecords]

	## Error handling. A recovery budget allows exactly `max_errors` rejects.
	ReadProps := { error_policy : ErrorPolicy ?? FailFast, max_errors : U64 ?? 1000 }

	## Counts of successfully emitted items and rejected records.
	Report : { emitted_items : U64, rejected_records : U64 }

	## Successful values, recoverable rejections, and successful EOF validation.
	ReadEvent(item, problem) : [Value(item), Rejected(problem), Finished(Report)]

	## Terminal decoding failure, including the rejection exceeding the budget.
	ReadError(problem) : [ParseErr(problem), ErrorLimitExceeded({ max : U64, error : problem }), DecoderProtocol(Str)]

	## One pure step. `consumed` counts bytes of this call's input only.
	Step(state, item, problem) : {
		state : state,
		consumed : U64,
		outcome : [Item(item), RecordError(problem), NeedInput, Done, Failed(problem)],
	}

	## Supported text encodings. Undefined Windows-1252 bytes are rejected.
	TextEncoding : [Utf8, Windows1252]

	## Decode a complete text token after its byte framing has been established.
	decode_text : Bytes, TextEncoding -> Try(Str, [BadEncoding({ index : U64, message : Str })])
	decode_text = |bytes, encoding| match encoding {
		Utf8 => Str.from_utf8(bytes).map_err(|BadUtf8(problem)| BadEncoding({ index: problem.index, message: "invalid UTF-8" }))
		Windows1252 => {
			var $out = []
			for pair in bytes.iter().with_index() {
				{ index, byte } = { index: pair.0, byte: pair.1 }
				if byte < 0x80 {
					$out = $out.append(byte)
				} else {
					point = if byte >= 0xA0 {
						byte.to_u32()
					} else {
						match byte {
							0x80 => 0x20AC
							0x82 => 0x201A
							0x83 => 0x0192
							0x84 => 0x201E
							0x85 => 0x2026
							0x86 => 0x2020
							0x87 => 0x2021
							0x88 => 0x02C6
							0x89 => 0x2030
							0x8A => 0x0160
							0x8B => 0x2039
							0x8C => 0x0152
							0x8E => 0x017D
							0x91 => 0x2018
							0x92 => 0x2019
							0x93 => 0x201C
							0x94 => 0x201D
							0x95 => 0x2022
							0x96 => 0x2013
							0x97 => 0x2014
							0x98 => 0x02DC
							0x99 => 0x2122
							0x9A => 0x0161
							0x9B => 0x203A
							0x9C => 0x0153
							0x9E => 0x017E
							0x9F => 0x0178
							_ => return Err(BadEncoding({ index, message: "undefined Windows-1252 byte" }))
						}
					}
					if point < 0x800 {
						$out = $out.append((0xC0 + point // 64).to_u8_wrap()).append((0x80 + point % 64).to_u8_wrap())
					} else {
						$out = $out.append((0xE0 + point // 4096).to_u8_wrap()).append((0x80 + (point // 64) % 64).to_u8_wrap()).append((0x80 + point % 64).to_u8_wrap())
					}
				}
			}
			Ok(Str.from_utf8($out) ?? "")
		}
	}

	## Lazily decode pure byte chunks. Empty chunks are data, never EOF.
	iter : Iter(Bytes), state, (state, Bytes, InputEnd -> Step(state, item, problem)), ReadProps -> Iter(Try(ReadEvent(item, problem), ReadError(problem)))
	iter = |chunks, initial, step, props| Iter.custom(
		Reading({ chunks, buffer: [], index: 0, final: False, need_input: False, decoder: initial, report: { emitted_items: 0, rejected_records: 0 } }),
		Err(Unknown),
		|cursor| match cursor {
			Stopped => Err(NoMore)
			Reading(start) => {
				var $cursor = start
				while True {
					if $cursor.need_input {
						match $cursor.chunks.next() {
							Done => {
								$cursor = { ..$cursor, buffer: [], index: 0, final: True, need_input: False }
							}
							Skip({ rest }) => {
								$cursor = { ..$cursor, chunks: rest }
							}
							One({ item, rest }) => {
								$cursor = { ..$cursor, chunks: rest, buffer: item, index: 0, need_input: False }
							}
						}
					} else {
						next = step($cursor.decoder, $cursor.buffer.drop_first($cursor.index), if ($cursor.final) Final else More)
						if next.consumed > $cursor.buffer.len() - $cursor.index {
							return Ok((Err(DecoderProtocol("decoder consumed beyond its input")), Stopped))
						}
						$cursor = { ..$cursor, index: $cursor.index + next.consumed, decoder: next.state }
						match next.outcome {
							Item(value) => {
								$cursor = { ..$cursor, report: { ..$cursor.report, emitted_items: $cursor.report.emitted_items + 1 } }
								return Ok((Ok(Value(value)), Reading($cursor)))
							}
							RecordError(error) => {
								if props.error_policy == FailFast {
									return Ok((Err(ParseErr(error)), Stopped))
								}
								if $cursor.report.rejected_records >= props.max_errors {
									return Ok((Err(ErrorLimitExceeded({ max: props.max_errors, error })), Stopped))
								}
								$cursor = { ..$cursor, report: { ..$cursor.report, rejected_records: $cursor.report.rejected_records + 1 } }
								if props.error_policy == ReportInvalidRecords {
									return Ok((Ok(Rejected(error)), Reading($cursor)))
								}
							}
							NeedInput => {
								if $cursor.final {
									return Ok((Err(DecoderProtocol("decoder requested input after EOF")), Stopped))
								}
								if $cursor.index != $cursor.buffer.len() {
									return Ok((Err(DecoderProtocol("decoder left input before requesting more")), Stopped))
								}
								$cursor = { ..$cursor, buffer: [], index: 0, need_input: True }
							}
							Done => {
								if !$cursor.final or $cursor.index != $cursor.buffer.len() {
									return Ok((Err(DecoderProtocol("decoder completed before consuming EOF")), Stopped))
								}
								return Ok((Ok(Finished($cursor.report)), Stopped))
							}
							Failed(error) => return Ok((Err(ParseErr(error)), Stopped))
						}
					}
				}
			}
		},
	)

	## Lazily decode an effectful source. Source errors stop without finalizing.
	stream : Stream(Try(Bytes, source_error)), state, (state, Bytes, InputEnd -> Step(state, item, problem)), ReadProps -> Stream(Try(ReadEvent(item, problem), [SourceErr(source_error), ParseErr(problem), ErrorLimitExceeded({ max : U64, error : problem }), DecoderProtocol(Str)]))
	stream = |chunks, initial, step, props| Stream.custom(
		Reading({ chunks, buffer: [], index: 0, final: False, need_input: False, decoder: initial, report: { emitted_items: 0, rejected_records: 0 } }),
		Err(Unknown),
		|cursor| match cursor {
			Stopped => Err(NoMore)
			Reading(start) => {
				var $cursor = start
				while True {
					if $cursor.need_input {
						match $cursor.chunks.next!() {
							Done => {
								$cursor = { ..$cursor, buffer: [], index: 0, final: True, need_input: False }
							}
							Skip({ rest }) => {
								$cursor = { ..$cursor, chunks: rest }
							}
							One({ item: Ok(bytes), rest }) => {
								$cursor = { ..$cursor, chunks: rest, buffer: bytes, index: 0, need_input: False }
							}
							One({ item: Err(error), .. }) => return Ok((Err(SourceErr(error)), Stopped))
						}
					} else {
						next = step($cursor.decoder, $cursor.buffer.drop_first($cursor.index), if ($cursor.final) Final else More)
						if next.consumed > $cursor.buffer.len() - $cursor.index {
							return Ok((Err(DecoderProtocol("decoder consumed beyond its input")), Stopped))
						}
						$cursor = { ..$cursor, index: $cursor.index + next.consumed, decoder: next.state }
						match next.outcome {
							Item(value) => {
								$cursor = { ..$cursor, report: { ..$cursor.report, emitted_items: $cursor.report.emitted_items + 1 } }
								return Ok((Ok(Value(value)), Reading($cursor)))
							}
							RecordError(error) => {
								if props.error_policy == FailFast {
									return Ok((Err(ParseErr(error)), Stopped))
								}
								if $cursor.report.rejected_records >= props.max_errors {
									return Ok((Err(ErrorLimitExceeded({ max: props.max_errors, error })), Stopped))
								}
								$cursor = { ..$cursor, report: { ..$cursor.report, rejected_records: $cursor.report.rejected_records + 1 } }
								if props.error_policy == ReportInvalidRecords {
									return Ok((Ok(Rejected(error)), Reading($cursor)))
								}
							}
							NeedInput => {
								if $cursor.final {
									return Ok((Err(DecoderProtocol("decoder requested input after EOF")), Stopped))
								}
								if $cursor.index != $cursor.buffer.len() {
									return Ok((Err(DecoderProtocol("decoder left input before requesting more")), Stopped))
								}
								$cursor = { ..$cursor, buffer: [], index: 0, need_input: True }
							}
							Done => {
								if !$cursor.final or $cursor.index != $cursor.buffer.len() {
									return Ok((Err(DecoderProtocol("decoder completed before consuming EOF")), Stopped))
								}
								return Ok((Ok(Finished($cursor.report)), Stopped))
							}
							Failed(error) => return Ok((Err(ParseErr(error)), Stopped))
						}
					}
				}
			}
		},
	)

	## Collect values, failing rather than silently dropping rejected records.
	collect : Iter(Try(ReadEvent(item, problem), terminal_error)) -> Try(List(item), [ReadFailed(terminal_error), RejectedRecord(problem), MissingFinished, SkippedRecords(Report)])
	collect = |events| {
		var $values = []
		for event in events {
			match event {
				Ok(Value(value)) => {
					$values = $values.append(value)
				}
				Ok(Rejected(error)) => return Err(RejectedRecord(error))
				Ok(Finished(report)) => return if (report.rejected_records == 0) Ok($values) else Err(SkippedRecords(report))
				Err(error) => return Err(ReadFailed(error))
			}
		}
		Err(MissingFinished)
	}

	## Validate and encode each item before emitting its first bounded byte chunk.
	encode_iter : Iter(item), (item -> Try(Bytes, problem)), U64 -> Iter(Try(Bytes, [EncodeErr(problem), InvalidChunkSize]))
	encode_iter = |items, encode, chunk_bytes| Iter.custom(
		Reading({ items, buffer: [], index: 0 }),
		Err(Unknown),
		|cursor| match cursor {
			Stopped => Err(NoMore)
			Reading(start) => {
				if chunk_bytes == 0 {
					return Ok((Err(InvalidChunkSize), Stopped))
				}
				var $cursor = start
				while True {
					if $cursor.index < $cursor.buffer.len() {
						len = U64.min(chunk_bytes, $cursor.buffer.len() - $cursor.index)
						bytes = $cursor.buffer.sublist({ start: $cursor.index, len })
						$cursor = { ..$cursor, index: $cursor.index + len }
						return Ok((Ok(bytes), Reading($cursor)))
					}
					match $cursor.items.next() {
						Done => return Err(NoMore)
						Skip({ rest }) => {
							$cursor = { ..$cursor, items: rest }
						}
						One({ item, rest }) => match encode(item) {
							Ok(bytes) => {
								$cursor = { items: rest, buffer: bytes, index: 0 }
							}
							Err(error) => return Ok((Err(EncodeErr(error)), Stopped))
						}
					}
				}
			}
		},
	)

	## Stream encoded chunks, retaining only the current validated item.
	encode_stream : Stream(Try(item, source_error)), (item -> Try(Bytes, problem)), U64 -> Stream(Try(Bytes, [SourceErr(source_error), EncodeErr(problem), InvalidChunkSize]))
	encode_stream = |items, encode, chunk_bytes| Stream.custom(
		Reading({ items, buffer: [], index: 0 }),
		Err(Unknown),
		|cursor| match cursor {
			Stopped => Err(NoMore)
			Reading(start) => {
				if chunk_bytes == 0 {
					return Ok((Err(InvalidChunkSize), Stopped))
				}
				var $cursor = start
				while True {
					if $cursor.index < $cursor.buffer.len() {
						len = U64.min(chunk_bytes, $cursor.buffer.len() - $cursor.index)
						bytes = $cursor.buffer.sublist({ start: $cursor.index, len })
						$cursor = { ..$cursor, index: $cursor.index + len }
						return Ok((Ok(bytes), Reading($cursor)))
					}
					match $cursor.items.next!() {
						Done => return Err(NoMore)
						Skip({ rest }) => {
							$cursor = { ..$cursor, items: rest }
						}
						One({ item: Ok(item), rest }) => match encode(item) {
							Ok(bytes) => {
								$cursor = { items: rest, buffer: bytes, index: 0 }
							}
							Err(error) => return Ok((Err(EncodeErr(error)), Stopped))
						}
						One({ item: Err(error), .. }) => return Ok((Err(SourceErr(error)), Stopped))
					}
				}
			}
		},
	)

	## Explicitly collect successful values and bounded reported rejections.
	collect_report : Iter(Try(ReadEvent(item, problem), terminal_error)) -> Try({ values : List(item), errors : List(problem), report : Report }, [ReadFailed(terminal_error), MissingFinished])
	collect_report = |events| {
		var $values = []
		var $errors = []
		for event in events {
			match event {
				Ok(Value(value)) => {
					$values = $values.append(value)
				}
				Ok(Rejected(error)) => {
					$errors = $errors.append(error)
				}
				Ok(Finished(report)) => return Ok({ values: $values, errors: $errors, report })
				Err(error) => return Err(ReadFailed(error))
			}
		}
		Err(MissingFinished)
	}

}

expect Streaming.decode_text([0x80, 0xE9], Windows1252) == Ok("€é")
expect Streaming.decode_text([0x81], Windows1252).is_err()

test_step : U64, Streaming.Bytes, Streaming.InputEnd -> Streaming.Step(U64, U8, Str)
test_step = |state, bytes, end| match bytes.first() {
	Ok(0) => { state: state + 1, consumed: 1, outcome: RecordError("zero") }
	Ok(value) => { state: state + 1, consumed: 1, outcome: Item(value) }
	Err(_) => { state, consumed: 0, outcome: if (end == Final) Done else NeedInput }
}

expect List.from_iter(Streaming.iter([[1.U8], [], [2]].iter(), 0, test_step, {})) == [Ok(Value(1)), Ok(Value(2)), Ok(Finished({ emitted_items: 2, rejected_records: 0 }))]
expect List.from_iter(Streaming.iter([[1.U8, 0, 2]].iter(), 0, test_step, {})) == [Ok(Value(1)), Err(ParseErr("zero"))]
expect List.from_iter(Streaming.iter([[1.U8, 0, 2]].iter(), 0, test_step, { error_policy: SkipInvalidRecords })) == [Ok(Value(1)), Ok(Value(2)), Ok(Finished({ emitted_items: 2, rejected_records: 1 }))]
expect List.from_iter(Streaming.iter([[0.U8, 0]].iter(), 0, test_step, { error_policy: ReportInvalidRecords, max_errors: 1 })) == [Ok(Rejected("zero")), Err(ErrorLimitExceeded({ max: 1, error: "zero" }))]

expect Streaming.collect([Ok(Value(1.U64))].iter()) == Err(MissingFinished)
expect Streaming.collect_report(Streaming.iter([[1.U8, 0, 2]].iter(), 0, test_step, { error_policy: ReportInvalidRecords })) == Ok({ values: [1, 2], errors: ["zero"], report: { emitted_items: 2, rejected_records: 1 } })
expect List.from_iter(Streaming.encode_iter(["abcd"].iter(), |text| Ok(text.to_utf8()), 2)) == [Ok("ab".to_utf8()), Ok("cd".to_utf8())]
expect List.from_iter(Streaming.encode_iter(["abc"].iter(), |text| Ok(text.to_utf8()), 0)) == [Err(InvalidChunkSize)]
expect {
	queued = |state, bytes, end| {
		if state == 1 {
			{ state: 2, consumed: 0, outcome: Item(9.U8) }
		}
		else {
			match bytes.first() {
				Ok(value) => { state: 1, consumed: bytes.len(), outcome: Item(value) }
				Err(_) => { state, consumed: 0, outcome: if (end == Final) Done else NeedInput }
			}
		}
	}
	List.from_iter(Streaming.iter([[1.U8]].iter(), 0, queued, {})) == [Ok(Value(1)), Ok(Value(9)), Ok(Finished({ emitted_items: 2, rejected_records: 0 }))]
}
expect List.from_iter(Streaming.iter([].iter(), 0.U64, |state, _, _| { state, consumed: 0, outcome: NeedInput }, {})) == [Err(DecoderProtocol("decoder requested input after EOF"))]
