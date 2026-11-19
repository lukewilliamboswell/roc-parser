import Diagnostic as DiagnosticCore

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
	RecoveryProps := { policy : ErrorPolicy ?? FailFast, max_errors : U64 ?? 1000 }

	## Counts of successfully emitted items and rejected records.
	Report : { emitted_items : U64, rejected_records : U64 }

	## Successful values, recoverable rejections, and successful EOF validation.
	ReadEvent(item, problem) : [Value(item), Rejected([..problem]), Finished(Report)]

	## Terminal decoding failure, including the rejection exceeding the budget.
	ReadError(problem) : [RecoveryLimitExceeded(DiagnosticCore(RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(ProtocolProblem)), ..problem]

	## Rejected-record count and original native error at recovery exhaustion.
	RecoveryProblem(problem) : [RejectedRecords({ max : U64, observed : DiagnosticCore.Observed, cause : [..problem] })]

	## Output chunk sizes must permit at least one byte.
	ChunkSizeProblem : [BelowMinimum({ minimum : U64, actual : U64 })]

	## A decoder violated the shared progress or EOF contract.
	ProtocolProblem : [ConsumedBeyondInput({ consumed : U64, available : U64 }), NeedInputAfterFinal, UnconsumedInput({ remaining : U64 }), PrematureDone({ end : InputEnd, remaining : U64 })]

	## One pure step. `consumed` counts bytes of this call's input only.
	Step(state, item, problem) : {
		state : state,
		consumed : U64,
		outcome : [Item(item), RecordError(problem), NeedInput, Done, Failed(problem)],
	}

	## A configured incremental decoder; use it with either shared reader.
	Decoder(state, item, problem) :: {
		inner : state,
		advance : state, Bytes, InputEnd -> Step(state, item, [..problem]),
		handling : RecoveryProps,
	}.{

		## Lazily consume pure chunks, validating EOF when the source ends.
		read_iter : Decoder(state, item, problem), Iter(Bytes) -> Iter(Try(ReadEvent(item, problem), ReadError(problem)))
		read_iter = |current, chunks| read_chunks(chunks, current.inner, current.advance, current.handling)

		## Consume an effectful source without converting source failure into EOF.
		read_stream : Decoder(state, item, problem), Stream(Try(Bytes, source_error)) -> Stream(Try(ReadEvent(item, problem), [SourceErr(source_error), RecoveryLimitExceeded(DiagnosticCore(RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(ProtocolProblem)), ..problem]))
		read_stream = |current, chunks| read_source(chunks, current.inner, current.advance, current.handling)

		## Collect the complete document, rejecting skipped or reported failures.
		collect : Decoder(state, item, problem), Iter(Bytes) -> Try(List(item), [MissingFinished, SkippedRecords(Report), RecoveryLimitExceeded(DiagnosticCore(RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(ProtocolProblem)), ..problem])
		collect = |current, chunks| collect_events(current.read_iter(chunks))

		## Collect both values and the bounded rejected-record diagnostics.
		collect_report : Decoder(state, item, problem), Iter(Bytes) -> Try({ values : List(item), errors : List([..problem]), report : Report }, [MissingFinished, RecoveryLimitExceeded(DiagnosticCore(RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(ProtocolProblem)), ..problem])
		collect_report = |current, chunks| collect_event_report(current.read_iter(chunks))

		## Extract fail-fast values from an effectful byte source.
		values_stream : Decoder(state, item, problem), Stream(Try(Bytes, source_error)) -> Stream(Try(item, [SourceErr(source_error), MissingFinished, SkippedRecords(Report), RecoveryLimitExceeded(DiagnosticCore(RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(ProtocolProblem)), ..problem]))
		values_stream = |current, chunks| value_events(current.read_stream(chunks))

		## Inspect only the metadata deliberately provided by this format's state.
		metadata : Decoder(state, item, problem) -> metadata where [state.metadata : state -> metadata]
		metadata = |current| current.inner.metadata()

		## Advance manually and retain the returned descriptor.
		step : Decoder(state, item, problem), { bytes : Bytes, end : InputEnd } -> Step(Decoder(state, item, problem), item, [..problem])
		step = |current, input| {
			Decoder.{ inner, advance, handling } = current
			next = advance(inner, input.bytes, input.end)
			{ state: Decoder.{ inner: next.state, advance, handling }, consumed: next.consumed, outcome: next.outcome }
		}
	}

	## Configure a pure state machine without reading input.
	DecoderProps(state, item, problem) := { initial_state : state, step : state, Bytes, InputEnd -> Step(state, item, [..problem]), errors : RecoveryProps ?? {} }
	decoder : DecoderProps(state, item, problem) -> Decoder(state, item, problem)
	decoder = |props| Decoder.{ inner: props.initial_state, advance: props.step, handling: props.errors }

	## Internal composition access; no state getter is attached to descriptors.
	internal_state : Decoder(state, item, problem) -> state
	internal_state = |current| current.inner

	## Translate a scanner failure at an internal format boundary.
	map_step_error : Step(state, item, problem), (problem -> mapped) -> Step(state, item, mapped)
	map_step_error = |current, translate| {
		outcome = match current.outcome {
			Item(value) => Item(value)
			RecordError(error) => RecordError(translate(error))
			NeedInput => NeedInput
			Done => Done
			Failed(error) => Failed(translate(error))
		}
		{ state: current.state, consumed: current.consumed, outcome }
	}

	## A configured record encoder with state and an EOF suffix.
	Encoder(state, item, problem) :: {
		inner : state,
		encode : state, item -> Try({ state : state, bytes : Bytes }, [..problem]),
		finish : state -> Try(Bytes, [..problem]),
		chunk_bytes : U64,
	}.{

		## Encode records lazily with a bounded final suffix.
		write_iter : Encoder(state, item, problem), Iter(item) -> Iter(Try(Bytes, [InvalidChunkSize(DiagnosticCore(ChunkSizeProblem)), ..problem]))
		write_iter = |current, items| write_items(items, current.inner, current.encode, current.finish, current.chunk_bytes)

		## Encode an effectful source without collecting it.
		write_stream : Encoder(state, item, problem), Stream(Try(item, source_error)) -> Stream(Try(Bytes, [SourceErr(source_error), InvalidChunkSize(DiagnosticCore(ChunkSizeProblem)), ..problem]))
		write_stream = |current, items| write_source(items, current.inner, current.encode, current.finish, current.chunk_bytes)
	}

	## Configure record encoding and completion without pulling input.
	EncoderProps(state, item, problem) := { initial_state : state, encode : state, item -> Try({ state : state, bytes : Bytes }, [..problem]), finish : state -> Try(Bytes, [..problem]), chunk_bytes : U64 ?? 65_536 }
	encoder : EncoderProps(state, item, problem) -> Encoder(state, item, problem)
	encoder = |props| Encoder.{ inner: props.initial_state, encode: props.encode, finish: props.finish, chunk_bytes: props.chunk_bytes }

	## Supported text encodings. Undefined Windows-1252 bytes are rejected.
	TextEncoding : [Utf8, Windows1252]

	## An invalid source encoding byte at its zero-based token index.
	EncodingError : { index : U64, problem : [InvalidUtf8, UndefinedWindows1252Byte(U8)] }

	## Decode a complete text token after its byte framing has been established.
	decode_text : Bytes, TextEncoding -> Try(Str, [BadEncoding(EncodingError)])
	decode_text = |bytes, encoding| match encoding {
		Utf8 => Str.from_utf8(bytes).map_err(|BadUtf8(problem)| BadEncoding({ index: problem.index, problem: InvalidUtf8 }))
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
							_ => return Err(BadEncoding({ index, problem: UndefinedWindows1252Byte(byte) }))
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

}

## Collect values, failing rather than silently dropping rejected records.
collect_events : Iter(Try(Streaming.ReadEvent(item, problem), Streaming.ReadError(problem))) -> Try(List(item), [MissingFinished, SkippedRecords(Streaming.Report), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem])
collect_events = |events| {
	var $values = []
	for event in events {
		match event {
			Ok(Value(value)) => {
				$values = $values.append(value)
			}
			Ok(Rejected(error)) => {
				value = Err(error)?
				return Ok(value)
			}
			Ok(Finished(report)) => return if (report.rejected_records == 0) Ok($values) else Err(SkippedRecords(report))
			Err(error) => {
				value = Err(error)?
				return Ok(value)
			}
		}
	}
	Err(MissingFinished)
}

## Explicitly collect successful values and bounded reported rejections.
collect_event_report : Iter(Try(Streaming.ReadEvent(item, problem), Streaming.ReadError(problem))) -> Try({ values : List(item), errors : List([..problem]), report : Streaming.Report }, [MissingFinished, RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem])
collect_event_report = |events| {
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
			Err(error) => {
				value = Err(error)?
				return Ok(value)
			}
		}
	}
	Err(MissingFinished)
}

## Extract successful stream values, requiring explicit successful completion.
## Rejected or skipped records are failures; use a custom loop for recovery.
value_events : Stream(Try(Streaming.ReadEvent(item, problem), [SourceErr(source_error), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem])) -> Stream(Try(item, [SourceErr(source_error), MissingFinished, SkippedRecords(Streaming.Report), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem]))
value_events = |events| Stream.custom(
	Reading(events),
	Err(Unknown),
	|cursor| match cursor {
		Stopped => Err(NoMore)
		Reading(start) => {
			var $events = start
			while True {
				match $events.next!() {
					Done => return Ok((Err(MissingFinished), Stopped))
					Skip({ rest }) => {
						$events = rest
					}
					One({ item, rest }) => match item {
						Ok(Value(value)) => return Ok((Ok(value), Reading(rest)))
						Ok(Finished(report)) => return if report.rejected_records == 0 {
							Err(NoMore)
						} else {
							Ok((Err(SkippedRecords(report)), Stopped))
						}
						Ok(Rejected(error)) => return Ok((value_failure(error), Stopped))
						Err(error) => return Ok((terminal_value_failure(error), Stopped))
					}
				}
			}
		}
	},
)

# Keep input cursors explicit on reader/writer updates for roc-lang/roc#13208.
# Remove the extra fields when the specializing backend passes reader/writer suites.
## Lazily decode pure byte chunks. Empty chunks are data, never EOF.
read_chunks : Iter(Streaming.Bytes), state, (state, Streaming.Bytes, Streaming.InputEnd -> Streaming.Step(state, item, [..problem])), Streaming.RecoveryProps -> Iter(Try(Streaming.ReadEvent(item, problem), Streaming.ReadError(problem)))
read_chunks = |chunks, initial, step, props| Iter.custom(
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
							$cursor = { ..$cursor, chunks: $cursor.chunks, buffer: [], index: 0, final: True, need_input: False }
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
						return Ok((Err(protocol_error(ConsumedBeyondInput({ consumed: next.consumed, available: $cursor.buffer.len() - $cursor.index }))), Stopped))
					}
					$cursor = { ..$cursor, chunks: $cursor.chunks, index: $cursor.index + next.consumed, decoder: next.state }
					match next.outcome {
						Item(value) => {
							$cursor = { ..$cursor, chunks: $cursor.chunks, report: { ..$cursor.report, emitted_items: $cursor.report.emitted_items + 1 } }
							return Ok((Ok(Value(value)), Reading($cursor)))
						}
						RecordError(error) => {
							if props.policy == FailFast {
								return Ok((read_failure(error), Stopped))
							}
							if $cursor.report.rejected_records >= props.max_errors {
								return Ok((Err(recovery_error({ max: props.max_errors, rejected: $cursor.report.rejected_records + 1, cause: error })), Stopped))
							}
							$cursor = { ..$cursor, chunks: $cursor.chunks, report: { ..$cursor.report, rejected_records: $cursor.report.rejected_records + 1 } }
							if props.policy == ReportInvalidRecords {
								return Ok((Ok(Rejected(error)), Reading($cursor)))
							}
						}
						NeedInput => {
							if $cursor.final {
								return Ok((Err(protocol_error(NeedInputAfterFinal)), Stopped))
							}
							if $cursor.index != $cursor.buffer.len() {
								return Ok((Err(protocol_error(UnconsumedInput({ remaining: $cursor.buffer.len() - $cursor.index }))), Stopped))
							}
							$cursor = { ..$cursor, chunks: $cursor.chunks, buffer: [], index: 0, need_input: True }
						}
						Done => {
							if !$cursor.final or $cursor.index != $cursor.buffer.len() {
								return Ok((Err(protocol_error(PrematureDone({ end: if ($cursor.final) Final else More, remaining: $cursor.buffer.len() - $cursor.index }))), Stopped))
							}
							return Ok((Ok(Finished($cursor.report)), Stopped))
						}
						Failed(error) => return Ok((read_failure(error), Stopped))
					}
				}
			}
		}
	},
)

## Lazily decode an effectful source. Source errors stop without finalizing.
read_source : Stream(Try(Streaming.Bytes, source_error)), state, (state, Streaming.Bytes, Streaming.InputEnd -> Streaming.Step(state, item, [..problem])), Streaming.RecoveryProps -> Stream(Try(Streaming.ReadEvent(item, problem), [SourceErr(source_error), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem]))
read_source = |chunks, initial, step, props| read_source_wrapped(chunks, initial, step, props).map(
	|event| match event {
		Ok(value) => Ok(value)
		Err(PrivateNative(error)) => source_failure(error)
		Err(SourceErr(error)) => Err(SourceErr(error))
		Err(RecoveryLimitExceeded(error)) => Err(RecoveryLimitExceeded(error))
		Err(DecoderProtocol(error)) => Err(DecoderProtocol(error))
	},
)

# Keep the scanner error row closed before composing its stream. Merging the
# native row inside Stream.custom causes recursive lambda specialization when
# callers adapt the stream or pass it through an effectful writer.
read_source_wrapped : Stream(Try(Streaming.Bytes, source_error)), state, (state, Streaming.Bytes, Streaming.InputEnd -> Streaming.Step(state, item, [..problem])), Streaming.RecoveryProps -> Stream(Try(Streaming.ReadEvent(item, problem), [SourceErr(source_error), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), PrivateNative([..problem])]))
read_source_wrapped = |chunks, initial, step, props| Stream.custom(
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
							$cursor = { ..$cursor, chunks: $cursor.chunks, buffer: [], index: 0, final: True, need_input: False }
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
						return Ok((Err(protocol_error(ConsumedBeyondInput({ consumed: next.consumed, available: $cursor.buffer.len() - $cursor.index }))), Stopped))
					}
					$cursor = { ..$cursor, chunks: $cursor.chunks, index: $cursor.index + next.consumed, decoder: next.state }
					match next.outcome {
						Item(value) => {
							$cursor = { ..$cursor, chunks: $cursor.chunks, report: { ..$cursor.report, emitted_items: $cursor.report.emitted_items + 1 } }
							return Ok((Ok(Value(value)), Reading($cursor)))
						}
						RecordError(error) => {
							if props.policy == FailFast {
								return Ok((Err(PrivateNative(error)), Stopped))
							}
							if $cursor.report.rejected_records >= props.max_errors {
								return Ok((Err(recovery_error({ max: props.max_errors, rejected: $cursor.report.rejected_records + 1, cause: error })), Stopped))
							}
							$cursor = { ..$cursor, chunks: $cursor.chunks, report: { ..$cursor.report, rejected_records: $cursor.report.rejected_records + 1 } }
							if props.policy == ReportInvalidRecords {
								return Ok((Ok(Rejected(error)), Reading($cursor)))
							}
						}
						NeedInput => {
							if $cursor.final {
								return Ok((Err(protocol_error(NeedInputAfterFinal)), Stopped))
							}
							if $cursor.index != $cursor.buffer.len() {
								return Ok((Err(protocol_error(UnconsumedInput({ remaining: $cursor.buffer.len() - $cursor.index }))), Stopped))
							}
							$cursor = { ..$cursor, chunks: $cursor.chunks, buffer: [], index: 0, need_input: True }
						}
						Done => {
							if !$cursor.final or $cursor.index != $cursor.buffer.len() {
								return Ok((Err(protocol_error(PrematureDone({ end: if ($cursor.final) Final else More, remaining: $cursor.buffer.len() - $cursor.index }))), Stopped))
							}
							return Ok((Ok(Finished($cursor.report)), Stopped))
						}
						Failed(error) => return Ok((Err(PrivateNative(error)), Stopped))
					}
				}
			}
		}
	},
)

## Encode contextual records and validate document completion on source EOF.
write_items : Iter(item), state, (state, item -> Try({ state : state, bytes : Streaming.Bytes }, [..problem])), (state -> Try(Streaming.Bytes, [..problem])), U64 -> Iter(Try(Streaming.Bytes, [InvalidChunkSize(DiagnosticCore(Streaming.ChunkSizeProblem)), ..problem]))
write_items = |items, initial, write, finish, chunk_bytes| Iter.custom(
	Reading({ items, writer: initial, buffer: [], index: 0, finishing: False }),
	Err(Unknown),
	|cursor| match cursor {
		Stopped => Err(NoMore)
		Reading(start) => {
			if chunk_bytes == 0 {
				return Ok((Err(chunk_size_error(chunk_bytes)), Stopped))
			}
			var $cursor = start
			while True {
				if $cursor.index < $cursor.buffer.len() {
					len = U64.min(chunk_bytes, $cursor.buffer.len() - $cursor.index)
					bytes = $cursor.buffer.sublist({ start: $cursor.index, len })
					$cursor = { ..$cursor, items: $cursor.items, index: $cursor.index + len }
					return Ok((Ok(bytes), Reading($cursor)))
				}
				if $cursor.finishing {
					return Err(NoMore)
				}
				match $cursor.items.next() {
					Done => match finish($cursor.writer) {
						Ok(bytes) => {
							$cursor = { ..$cursor, items: $cursor.items, buffer: bytes, index: 0, finishing: True }
						}
						Err(error) => return Ok((write_failure(error), Stopped))
					}
					Skip({ rest }) => {
						$cursor = { ..$cursor, items: rest }
					}
					One({ item, rest }) => match write($cursor.writer, item) {
						Ok({ state, bytes }) => {
							$cursor = { ..$cursor, items: rest, writer: state, buffer: bytes, index: 0 }
						}
						Err(error) => return Ok((write_failure(error), Stopped))
					}
				}
			}
		}
	},
)

## Effectful contextual writer. Source failure never finalizes the writer.
write_source : Stream(Try(item, source_error)), state, (state, item -> Try({ state : state, bytes : Streaming.Bytes }, [..problem])), (state -> Try(Streaming.Bytes, [..problem])), U64 -> Stream(Try(Streaming.Bytes, [SourceErr(source_error), InvalidChunkSize(DiagnosticCore(Streaming.ChunkSizeProblem)), ..problem]))
write_source = |items, initial, write, finish, chunk_bytes| Stream.custom(
	Reading({ items, writer: initial, buffer: [], index: 0, finishing: False }),
	Err(Unknown),
	|cursor| match cursor {
		Stopped => Err(NoMore)
		Reading(start) => {
			if chunk_bytes == 0 {
				return Ok((Err(chunk_size_error(chunk_bytes)), Stopped))
			}
			var $cursor = start
			while True {
				if $cursor.index < $cursor.buffer.len() {
					len = U64.min(chunk_bytes, $cursor.buffer.len() - $cursor.index)
					bytes = $cursor.buffer.sublist({ start: $cursor.index, len })
					$cursor = { ..$cursor, items: $cursor.items, index: $cursor.index + len }
					return Ok((Ok(bytes), Reading($cursor)))
				}
				if $cursor.finishing {
					return Err(NoMore)
				}
				match $cursor.items.next!() {
					Done => match finish($cursor.writer) {
						Ok(bytes) => {
							$cursor = { ..$cursor, items: $cursor.items, buffer: bytes, index: 0, finishing: True }
						}
						Err(error) => return Ok((write_source_failure(error), Stopped))
					}
					Skip({ rest }) => {
						$cursor = { ..$cursor, items: rest }
					}
					One({ item: Ok(item), rest }) => match write($cursor.writer, item) {
						Ok({ state, bytes }) => {
							$cursor = { ..$cursor, items: rest, writer: state, buffer: bytes, index: 0 }
						}
						Err(error) => return Ok((write_source_failure(error), Stopped))
					}
					One({ item: Err(error), .. }) => return Ok((Err(SourceErr(error)), Stopped))
				}
			}
		}
	},
)

# A postfix ? widens an error row without wrapping or renaming its tags.
read_failure : [..problem] -> Try(a, Streaming.ReadError(problem))
read_failure = |error| {
	value = Err(error)?
	Ok(value)
}

source_failure : [..problem] -> Try(a, [SourceErr(source_error), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem])
source_failure = |error| {
	value = Err(error)?
	Ok(value)
}

value_failure : [..problem] -> Try(a, [SourceErr(source_error), MissingFinished, SkippedRecords(Streaming.Report), RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem))), DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem)), ..problem])
value_failure = |error| {
	value = Err(error)?
	Ok(value)
}

terminal_value_failure : [..error] -> Try(a, [SourceErr(source_error), MissingFinished, SkippedRecords(Streaming.Report), ..error])
terminal_value_failure = |error| {
	value = Err(error)?
	Ok(value)
}

write_failure : [..problem] -> Try(a, [InvalidChunkSize(DiagnosticCore(Streaming.ChunkSizeProblem)), ..problem])
write_failure = |error| {
	value = Err(error)?
	Ok(value)
}

write_source_failure : [..problem] -> Try(a, [SourceErr(source_error), InvalidChunkSize(DiagnosticCore(Streaming.ChunkSizeProblem)), ..problem])
write_source_failure = |error| {
	value = Err(error)?
	Ok(value)
}

protocol_error : Streaming.ProtocolProblem -> [DecoderProtocol(DiagnosticCore(Streaming.ProtocolProblem))]
protocol_error = |problem| DecoderProtocol(DiagnosticCore.{ problem, location: Unavailable(NotApplicable), path: [] })

recovery_error : { max : U64, rejected : U64, cause : [..problem] } -> [RecoveryLimitExceeded(DiagnosticCore(Streaming.RecoveryProblem(problem)))]
recovery_error = |detail| RecoveryLimitExceeded(DiagnosticCore.{ problem: RejectedRecords({ max: detail.max, observed: Exact(detail.rejected), cause: detail.cause }), location: Unavailable(NotApplicable), path: [] })

chunk_size_error : U64 -> [InvalidChunkSize(DiagnosticCore(Streaming.ChunkSizeProblem))]
chunk_size_error = |actual| InvalidChunkSize(DiagnosticCore.{ problem: BelowMinimum({ minimum: 1, actual }), location: Unavailable(NotApplicable), path: [Key("chunk_bytes")] })

expect Streaming.decode_text([0x80, 0xE9], Windows1252) == Ok("€é")
expect Streaming.decode_text([0x81], Windows1252).is_err()

test_step : U64, Streaming.Bytes, Streaming.InputEnd -> Streaming.Step(U64, U8, [ZeroByte])
test_step = |state, bytes, end| match bytes.first() {
	Ok(0) => { state: state + 1, consumed: 1, outcome: RecordError(ZeroByte) }
	Ok(value) => { state: state + 1, consumed: 1, outcome: Item(value) }
	Err(_) => { state, consumed: 0, outcome: if (end == Final) Done else NeedInput }
}

test_decoder : Streaming.RecoveryProps -> Streaming.Decoder(U64, U8, [ZeroByte])
test_decoder = |errors| Streaming.decoder({ initial_state: 0, step: test_step, errors })

## Empty chunks do not finalize a document and every complete source emits Finished.
expect List.from_iter(test_decoder({}).read_iter([[1.U8], [], [2]].iter())) == [Ok(Value(1)), Ok(Value(2)), Ok(Finished({ emitted_items: 2, rejected_records: 0 }))]

## Fail-fast preserves the native error tag without an adapter wrapper.
expect List.from_iter(test_decoder({}).read_iter([[1.U8, 0, 2]].iter())) == [Ok(Value(1)), Err(ZeroByte)]

## Skip recovery counts rejected records in the final report.
expect List.from_iter(test_decoder({ policy: SkipInvalidRecords }).read_iter([[1.U8, 0, 2]].iter())) == [Ok(Value(1)), Ok(Value(2)), Ok(Finished({ emitted_items: 2, rejected_records: 1 }))]

## A recovery allowance permits exactly that many rejects and retains the triggering error.
expect List.from_iter(test_decoder({ policy: ReportInvalidRecords, max_errors: 1 }).read_iter([[0.U8, 0]].iter())) == [Ok(Rejected(ZeroByte)), Err(RecoveryLimitExceeded(DiagnosticCore.{ problem: RejectedRecords({ max: 1, observed: Exact(2), cause: ZeroByte }), location: Unavailable(NotApplicable), path: [] }))]

## Collection retains values and a bounded diagnostic report.
expect test_decoder({ policy: ReportInvalidRecords }).collect_report([[1.U8, 0, 2]].iter()) == Ok({ values: [1, 2], errors: [ZeroByte], report: { emitted_items: 2, rejected_records: 1 } })

## A complete successful source can be collected through its configured decoder.
expect test_decoder({}).collect([[1.U8], [], [2]].iter()) == Ok([1, 2])

## Ordinary collection forwards an encountered native rejection directly.
expect test_decoder({ policy: ReportInvalidRecords }).collect([[1.U8, 0]].iter()) == Err(ZeroByte)

## Skipping a record cannot silently make a collected document complete.
expect test_decoder({ policy: SkipInvalidRecords }).collect([[1.U8, 0]].iter()) == Err(SkippedRecords({ emitted_items: 1, rejected_records: 1 }))

## Collection requires an explicit successful completion event.
expect collect_events([Ok(Value(1.U64))].iter()) == Err(MissingFinished)

## A zero chunk bound fails before pulling or encoding any item.
expect List.from_iter(Streaming.encoder({ initial_state: {}, encode: |state, text| Ok({ state, bytes: text.to_utf8() }), finish: |_| Ok([]), chunk_bytes: 0 }).write_iter(["abc"].iter())) == [Err(InvalidChunkSize(DiagnosticCore.{ problem: BelowMinimum({ minimum: 1, actual: 0 }), location: Unavailable(NotApplicable), path: [Key("chunk_bytes")] }))]

## The encoder chunks records and its completion suffix without changing bytes.
expect List.from_iter(Streaming.encoder({ initial_state: 0.U64, encode: |state, text| Ok({ state: state + 1, bytes: text.to_utf8() }), finish: |state| Ok(state.to_str().to_utf8()), chunk_bytes: 1 }).write_iter(["ab", "cd"].iter())) == [Ok("a".to_utf8()), Ok("b".to_utf8()), Ok("c".to_utf8()), Ok("d".to_utf8()), Ok("2".to_utf8())]

## Encoding failures preserve the original problem row.
expect List.from_iter(Streaming.encoder({ initial_state: {}, encode: |_state, _item| Err(CannotEncode), finish: |_| Ok([]) }).write_iter([1.U8].iter())) == [Err(CannotEncode)]

## Pending events are drained before requesting another source chunk.
expect {
	queued = |state, bytes, end| {
		if state == 1 {
			{ state: 2, consumed: 0, outcome: Item(9.U8) }
		} else {
			match bytes.first() {
				Ok(value) => { state: 1, consumed: bytes.len(), outcome: Item(value) }
				Err(_) => { state, consumed: 0, outcome: if (end == Final) Done else NeedInput }
			}
		}
	}
	List.from_iter(Streaming.decoder({ initial_state: 0, step: queued }).read_iter([[1.U8]].iter())) == [Ok(Value(1)), Ok(Value(9)), Ok(Finished({ emitted_items: 2, rejected_records: 0 }))]
}

## A decoder must not request bytes after source exhaustion.
expect List.from_iter(Streaming.decoder({ initial_state: 0.U64, step: |state, _, _| { state, consumed: 0, outcome: NeedInput } }).read_iter([].iter())) == [Err(DecoderProtocol(DiagnosticCore.{ problem: NeedInputAfterFinal, location: Unavailable(NotApplicable), path: [] }))]

## A decoder must not consume bytes beyond the current input.
expect List.from_iter(Streaming.decoder({ initial_state: {}, step: |state, _, _| { state, consumed: 1, outcome: NeedInput } }).read_iter([].iter())) == [Err(DecoderProtocol(DiagnosticCore.{ problem: ConsumedBeyondInput({ consumed: 1, available: 0 }), location: Unavailable(NotApplicable), path: [] }))]

## A decoder must consume the current buffer before requesting more bytes.
expect List.from_iter(Streaming.decoder({ initial_state: {}, step: |state, _, _| { state, consumed: 0, outcome: NeedInput } }).read_iter([[1.U8, 2]].iter())) == [Err(DecoderProtocol(DiagnosticCore.{ problem: UnconsumedInput({ remaining: 2 }), location: Unavailable(NotApplicable), path: [] }))]

## Done before Final is a protocol violation even for an empty buffer.
expect List.from_iter(Streaming.decoder({ initial_state: {}, step: |state, _, _| { state, consumed: 0, outcome: Done } }).read_iter([].iter())) == [Err(DecoderProtocol(DiagnosticCore.{ problem: PrematureDone({ end: More, remaining: 0 }), location: Unavailable(NotApplicable), path: [] }))]

## Manual stepping keeps the configured descriptor and consumes only this call's bytes.
expect {
	first = test_decoder({}).step({ bytes: [1.U8], end: More })
	final = first.state.step({ bytes: [], end: Final })
	{ consumed: first.consumed, first: first.outcome, final: final.outcome } == { consumed: 1, first: Item(1), final: Done }
}

## Every defined Windows-1252 control-range byte decodes independently.
expect Streaming.decode_text([128.U8, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 142, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 158, 159], Windows1252) == Ok("€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ")
