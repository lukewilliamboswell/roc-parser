app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Streaming

## Independent byte-record model: zero rejects, 255 is terminal, others emit.
step : U64, List(U8), Streaming.InputEnd -> Streaming.Step(U64, U8, U64)
step = |offset, bytes, end| match bytes.first() {
	Ok(0) => { state: offset + 1, consumed: 1, outcome: RecordError(offset) }
	Ok(255) => { state: offset + 1, consumed: 1, outcome: Failed(offset) }
	Ok(byte) => { state: offset + 1, consumed: 1, outcome: Item(byte) }
	Err(_) => { state: offset, consumed: 0, outcome: if (end == Final) Done else NeedInput }
}

expected : List(U8), Streaming.ReadProps -> List(Try(Streaming.ReadEvent(U8, U64), Streaming.ReadError(U64)))
expected = |bytes, props| {
	var $out = []
	var $emitted = 0.U64
	var $rejected = 0.U64
	for pair in bytes.iter().with_index() {
		match pair.1 {
			255 => return $out.append(Err(ParseErr(pair.0)))
			0 => {
				if props.error_policy == FailFast {
					return $out.append(Err(ParseErr(pair.0)))
				}
				if $rejected >= props.max_errors {
					return $out.append(Err(ErrorLimitExceeded({ max: props.max_errors, error: pair.0 })))
				}
				$rejected = $rejected + 1
				if props.error_policy == ReportInvalidRecords {
					$out = $out.append(Ok(Rejected(pair.0)))
				}
			}
			value => {
				$out = $out.append(Ok(Value(value)))
				$emitted = $emitted + 1
			}
		}
	}
	$out.append(Ok(Finished({ emitted_items: $emitted, rejected_records: $rejected })))
}

test = |input| {
	policy = match (input.first() ?? 0) % 3 {
		0 => FailFast
		1 => SkipInvalidRecords
		_ => ReportInvalidRecords
	}
	props : Streaming.ReadProps
	props = { error_policy: policy, max_errors: (input.get(1) ?? 0).to_u64() % 8 }
	bytes = input.drop_first(2)
	want = expected(bytes, props)
	whole = List.from_iter(Streaming.iter([bytes].iter(), 0, step, props))
	if whole != want {
		crash "whole chunk differs from independent protocol model"
	}
	pieces = bytes.map(|byte| [byte])
	many = List.from_iter(Streaming.iter([[]].concat(pieces).append([]).iter(), 0, step, props))
	if many != want {
		crash "one-byte/empty chunks changed protocol outcomes"
	}
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [bytes.take_first(split), [], bytes.drop_first(split)]
		if List.from_iter(Streaming.iter(parts.iter(), 0, step, props)) != want {
			crash "split changed protocol outcomes"
		}
	}
	Fuzz.keep
}

test! = |bytes| {
	_ = test(bytes)
	source : Stream(Try(List(U8), [Injected]))
	source = [Ok(bytes), Err(Injected), Ok([1.U8])].iter().stream()
	var $actual = []
	for! item in Streaming.stream(source, 0, step, {}) {
		$actual = $actual.append(item)
	}
	prefix = expected(bytes, {})
	want = match prefix.last() {
		Ok(Ok(Finished(_))) => prefix.drop_last(1).append(Err(SourceErr(Injected)))
		_ => prefix
	}
	if $actual != want {
		crash "source failure finalized or lost its error"
	}

	unfinished : Stream(Try(Streaming.ReadEvent(U8, Str), Str))
	unfinished = [Ok(Value(bytes.first() ?? 0))].iter().stream()
	var $unfinished = []
	for! item in Streaming.values_stream(unfinished) {
		$unfinished = $unfinished.append(item)
	}
	if $unfinished != [Ok(bytes.first() ?? 0), Err(MissingFinished)] {
		crash "value extraction accepted a missing completion event"
	}
	skipped : Stream(Try(Streaming.ReadEvent(U8, Str), Str))
	skipped = [Ok(Finished({ emitted_items: 0, rejected_records: 1 }))].iter().stream()
	var $skipped = []
	for! item in Streaming.values_stream(skipped) {
		$skipped = $skipped.append(item)
	}
	if $skipped != [Err(SkippedRecords({ emitted_items: 0, rejected_records: 1 }))] {
		crash "value extraction silently discarded a rejected record"
	}
	Fuzz.keep
}

target = Fuzz.target_with!({ name: "stream-contract", examples: [], generator: Fuzz.raw_bytes, test!, show: Str.inspect })
