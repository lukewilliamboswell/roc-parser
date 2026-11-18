import parser.Parser
import fuzz.Fuzz
import parser.OFX

## Allocate distinct source chunks inside measurement, drain events immediately.
## Total transaction count varies while chunk/record/depth remain bounded.
drain : U64 -> U64
drain = |count| {
	chunks = Iter.custom(
		(0.U64, count),
		Err(Unknown),
		|state| {
			if state.0 == 0 {
				return Ok(("<OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>001</BANKID><ACCTID>0001</ACCTID></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND>".to_utf8(), (1, state.1)))
			}
			if state.0 == 1 and state.1 > 0 {
				return Ok(("<STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260101</DTPOSTED><TRNAMT>-1.00</TRNAMT><FITID>${state.1.to_str()}</FITID><NAME>Roc</NAME></STMTTRN>".to_utf8(), (1, state.1 - 1)))
			}
			if state.0 == 1 {
				return Ok(("</BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>".to_utf8(), (2, 0)))
			}
			Err(NoMore)
		},
	)
	var $count = 0.U64
	for event in (OFX.decoder({})).read_iter(chunks) {
		match event {
			Ok(Value(Transaction(_))) => {
				$count = $count + 1
			}
			Err(_) => crash "measuring fixture failed"
			_ => {}
		}
	}
	$count
}

## Keep only the first identifier after parsing a large source chunk and record.
## A 32-byte identifier exceeds Roc's inline string size but is small relative
## to the 64-KiB captured record and 256-KiB unrelated source comment.
retain_id : U64 -> { id : Str, count : U64 }
retain_id = |count| {
	chunks = Iter.custom(
		(0.U64, count),
		Err(Unknown),
		|state| {
			if state.0 == 0 {
				pad = Str.repeat("X", 65536)
				tail = Str.repeat("Y", 262144)
				prefix = "<OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>001</BANKID><ACCTID>0001</ACCTID></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND>"
				first = "<STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260101</DTPOSTED><TRNAMT>-1.00</TRNAMT><FITID>01234567890123456789012345678901</FITID><!--${pad}--></STMTTRN>"
				return Ok(("${prefix}${first}<!--${tail}-->".to_utf8(), (1, state.1)))
			}
			if state.0 == 1 and state.1 > 0 {
				return Ok(("<STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260101</DTPOSTED><TRNAMT>-1.00</TRNAMT><FITID>${state.1.to_str()}</FITID></STMTTRN>".to_utf8(), (1, state.1 - 1)))
			}
			if state.0 == 1 {
				return Ok(("</BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>".to_utf8(), (2, 0)))
			}
			Err(NoMore)
		},
	)
	var $id = ""
	var $count = 0.U64
	for event in (OFX.decoder({ max_token_bytes: 524288 })).read_iter(chunks) {
		match event {
			Ok(Value(Transaction(transaction))) => {
				if $count == 0 {
					$id = transaction.id
				}
				$count = $count + 1
			}
			Err(_) => crash "retention audit document failed"
			_ => {}
		}
	}
	{ id: $id, count: $count }
}

## Only the unfinished field and its bounded context may outlive a source
## chunk; a short seamless slice must not retain this unrelated large comment.
pending_leaf : {} -> Parser.Decoder(_, OFX.Event, [InvalidOfx(OFX.Error)])
pending_leaf = |_| {
	pad = Str.repeat("X", 262144)
	bytes = "<!--${pad}--><OFX><BANKMSGSRSV1><STMTTRNRS><TRNUID>0</TRNUID><STATUS><CODE>0</CODE><SEVERITY>INFO</SEVERITY></STATUS><STMTRS><CURDEF>AUD</CURDEF><BANKACCTFROM><BANKID>001</BANKID><ACCTID>0001</ACCTID></BANKACCTFROM><BANKTRANLIST><DTSTART>20260101</DTSTART><DTEND>20260201</DTEND><STMTTRN><TRNTYPE>DEBIT</TRNTYPE><DTPOSTED>20260101</DTPOSTED><TRNAMT>-1.00</TRNAMT><FITID>1</FITID><NAME>unfinished field".to_utf8()
	var $state = OFX.decoder({ max_token_bytes: 524288 })
	var $at = 0.U64
	while True {
		next = $state.step({ bytes: bytes.drop_first($at), end: More })
		$state = next.state
		$at = $at + next.consumed
		match next.outcome {
			NeedInput => return $state
			Item(StatementStart(_)) => {}
			_ => crash "pending field fixture failed before source exhaustion"
		}
	}
}

finish_pending : Parser.Decoder(_, OFX.Event, [InvalidOfx(OFX.Error)]) -> Bool
finish_pending = |state| {
	bytes = "</NAME></STMTTRN></BANKTRANLIST></STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>".to_utf8()
	var $state = state
	var $at = 0.U64
	var $transactions = 0.U64
	var $ended = False
	while True {
		next = $state.step({ bytes: bytes.drop_first($at), end: Final })
		$state = next.state
		$at = $at + next.consumed
		match next.outcome {
			Item(Transaction(value)) => {
				if value.name != "unfinished field" {
					return False
				}
				$transactions = $transactions + 1
			}
			Item(StatementEnd) => {
				$ended = True
			}
			Done => return $transactions == 1 and $ended
			_ => return False
		}
	}
}

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	count = (bytes.first() ?? 0).to_u64() * 2 + 1
	if bytes.get(1) == Ok(254) {
		measured = Fuzz.measure!("ofx-pending-leaf", || pending_leaf({}))
		if measured.stats.exit_live_bytes > measured.stats.entry_live_bytes + 4096 {
			crash "unfinished OFX field retained an unrelated source allocation"
		}
		if !finish_pending(measured.value) {
			crash "detached unfinished OFX field lost document contents"
		}
		return Fuzz.keep
	}
	if bytes.get(1) == Ok(255) {
		retained = Fuzz.measure!("ofx-retain-id", || retain_id(count))
		if retained.value.id != "01234567890123456789012345678901" or retained.value.count != count + 1 {
			crash "retention audit lost identifier or records"
		}
		if retained.stats.exit_live_bytes > retained.stats.entry_live_bytes + 1024 {
			crash "tiny identifier retained discarded source or captured record"
		}
		if retained.stats.peak_live_bytes > 1310720 {
			crash "retention audit exceeded bounded chunk/record peak"
		}
		return Fuzz.keep
	}
	measured = Fuzz.measure!("ofx-drain", || drain(count))
	if measured.value != count {
		crash "OFX ceiling source lost transactions"
	}
	held = measured.stats.peak_live_bytes - measured.stats.entry_live_bytes
	if held > 8192 {
		crash "OFX streaming retained source proportional to document length"
	}
	if measured.stats.requested_bytes > 32768 + count * 20000 {
		crash "OFX allocation traffic exceeded linear ceiling"
	}

	Fuzz.keep
}

property_target = Fuzz.target_with!({ name: "ofx-ceilings", examples: [Fuzz.example({ name: "small", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large", revision: "1", make: || [255.U8] }), Fuzz.example({ name: "retain-small", revision: "1", make: || [0.U8, 255] }), Fuzz.example({ name: "retain-large", revision: "1", make: || [255.U8, 255] }), Fuzz.example({ name: "pending-leaf", revision: "1", make: || [0.U8, 254] })], generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(bytes) })

OfxCeilingsProperty :: [].{
	target = property_target
}
