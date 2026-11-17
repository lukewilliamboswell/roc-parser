app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Parser
ByteProblem : [ZeroByte(U64), ForbiddenByte(U64)]

## Independent byte-record model: zero rejects, 255 is terminal, others emit.
step : U64, List(U8), Parser.InputEnd -> Parser.Step(U64, U8, ByteProblem)
step = |offset, bytes, end| match bytes.first() {
	Ok(0) => { state: offset + 1, consumed: 1, outcome: RecordError(ZeroByte(offset)) }
	Ok(255) => { state: offset + 1, consumed: 1, outcome: Failed(ForbiddenByte(offset)) }
	Ok(byte) => { state: offset + 1, consumed: 1, outcome: Item(byte) }
	Err(_) => { state: offset, consumed: 0, outcome: if (end == Final) Done else NeedInput }
}

expected : List(U8), Parser.RecoveryProps -> List(Try(Parser.ReadEvent(U8, ByteProblem), Parser.ReadError(ByteProblem)))
expected = |bytes, props| {
	var $out = []
	var $emitted = 0.U64
	var $rejected = 0.U64
	for pair in bytes.iter().with_index() {
		match pair.1 {
			255 => return $out.append(Err(ForbiddenByte(pair.0)))
			0 => {
				if props.policy == FailFast {
					return $out.append(Err(ZeroByte(pair.0)))
				}
				if $rejected >= props.max_errors {
					return $out.append(Err(RecoveryLimitExceeded({ problem: RejectedRecords({ max: props.max_errors, observed: Exact($rejected + 1), cause: ZeroByte(pair.0) }), location: Unavailable(NotApplicable), path: [] })))
				}
				$rejected = $rejected + 1
				if props.policy == ReportInvalidRecords {
					$out = $out.append(Ok(Rejected(ZeroByte(pair.0))))
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
	props : Parser.RecoveryProps
	props = { policy: policy, max_errors: (input.get(1) ?? 0).to_u64() % 8 }
	bytes = input.drop_first(2)
	want = expected(bytes, props)
	whole = List.from_iter((Parser.decoder({ initial_state: 0, step, errors: props })).read_iter([bytes].iter()))
	if whole != want {
		crash "whole chunk differs from independent protocol model"
	}
	pieces = bytes.map(|byte| [byte])
	many = List.from_iter((Parser.decoder({ initial_state: 0, step, errors: props })).read_iter([[]].concat(pieces).append([]).iter()))
	if many != want {
		crash "one-byte/empty chunks changed protocol outcomes"
	}
	for split in (0.U64).to(bytes.len() + 1) {
		parts = [bytes.take_first(split), [], bytes.drop_first(split)]
		if List.from_iter((Parser.decoder({ initial_state: 0, step, errors: props })).read_iter(parts.iter())) != want {
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
	for! item in (Parser.decoder({ initial_state: 0, step })).read_stream(source) {
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

	# Native failures keep their tags when value extraction is adapted again.
	composed_byte = match (bytes.first() ?? 1) % 3 {
		0 => 0.U8
		1 => 1.U8
		_ => 255.U8
	}
	values_source : Stream(Try(List(U8), [Injected({ code : U64 })]))
	values_source = [Ok([composed_byte]), Err(Injected({ code: bytes.len() })), Ok([1.U8])].iter().stream()
	var $values = []
	for! item in (Parser.decoder({ initial_state: 0, step })).values_stream(values_source).map(|value| value) {
		$values = $values.append(item)
	}
	wanted_values = match composed_byte {
		0 => [Err(ZeroByte(0))]
		255 => [Err(ForbiddenByte(0))]
		_ => [Ok(composed_byte), Err(SourceErr(Injected({ code: bytes.len() })))]
	}
	if $values != wanted_values {
		crash "stream composition changed native or source failure payloads"
	}

	# A faulty custom decoder cannot turn source exhaustion into a successful EOF.
	broken = Parser.decoder({ initial_state: {}, step: broken_step })
	broken_source : Stream(Try(List(U8), [Injected]))
	broken_source = [Ok(bytes)].iter().stream()
	var $broken = []
	for! item in broken.values_stream(broken_source) {
		$broken = $broken.append(item)
	}
	if $broken != [Err(DecoderProtocol({ problem: NeedInputAfterFinal, location: Unavailable(NotApplicable), path: [] }))] {
		crash "value extraction accepted an invalid completion protocol"
	}

	Fuzz.keep
}

target = Fuzz.target_with!({ name: "stream-contract", examples: [], generator: Fuzz.raw_bytes, test!, show: Str.inspect })

## This deliberately invalid state machine always asks for more source bytes.
## Both synthetic machines retain the same closed failure family.
broken_step : {}, List(U8), Parser.InputEnd -> Parser.Step({}, U8, ByteProblem)
broken_step = |state, bytes, _end| { state, consumed: bytes.len(), outcome: NeedInput }
