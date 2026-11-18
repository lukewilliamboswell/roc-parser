import fuzz.Fuzz
import parser.OFX
import parser.Parser
import OfxGen

collect : List(Try(Parser.ReadEvent(OFX.Event, [InvalidOfx(OFX.Error)]), Parser.ReadError([InvalidOfx(OFX.Error)]))) -> Try(OFX, [InvalidOfx(OFX.Error)])
collect = |events| {
	var $statements = []
	var $current = { context: { kind: Bank, currency: "", account: { bank_id: "", branch_id: "", account_id: "", account_type: "", key: "" }, start: "", end: "", transaction_id: "", status: { code: "", severity: "", message: "" }, header: { old_uid: "", new_uid: "", server_date: "", language: "", org: "", fid: "" } }, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }
	for event in events {
		match event {
			Ok(Value(StatementStart(context))) => {
				$current = { context, transactions: [], ledger_balance: Err(Missing), available_balance: Err(Missing) }
			}
			Ok(Value(Transaction(value))) => {
				$current = { ..$current, transactions: $current.transactions.append(value) }
			}
			Ok(Value(LedgerBalance(value))) => {
				$current = { ..$current, ledger_balance: Ok(value) }
			}
			Ok(Value(AvailableBalance(value))) => {
				$current = { ..$current, available_balance: Ok(value) }
			}
			Ok(Value(StatementEnd)) => {
				$statements = $statements.append($current)
			}
			Ok(Finished(_)) => return Ok(OFX.{ statements: $statements })
			Err(InvalidOfx(error)) => return Err(InvalidOfx(error))
			_ => crash "unexpected fail-fast streaming protocol outcome"
		}
	}
	crash "streaming OFX omitted completion"
}

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	results = List.from_iter((OFX.decoder({})).read_iter(OfxGen.chunks(bytes, bytes).iter()))
	if OFX.parse_bytes(bytes, {}) != collect(results) {
		crash "whole-document and chunked OFX models or diagnostics disagree"
	}
	for event in results {
		match event {
			Err(InvalidOfx(error)) => {
				if source_offset(error.location) > bytes.len() or source_position(error.location).line == 0 or source_position(error.location).column == 0 {
					crash "OFX diagnostic outside source"
				}
			}
			_ => {}
		}
	}
	Fuzz.keep
}

property_target = Fuzz.target_with({ name: "ofx-raw", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })

source_position : Parser.Location -> { line : U64, column : U64 }
source_position = |location| match location {
	Text(at) => { line: at.line, column: at.column }
	LineColumn(at) => at
	_ => crash "expected retained source text coordinates"
}

source_offset : Parser.Location -> U64
source_offset = |location| match location {
	InputOffset(offset) | ByteOffset(offset) => offset
	Text(at) => at.offset
	_ => crash "expected retained source byte coordinates"
}

OfxRawProperty :: [].{
	target = property_target
}
