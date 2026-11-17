app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import parser.Streaming
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
	for event in Streaming.read_iter(chunks, OFX.decoder({})) {
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
	for event in Streaming.read_iter(chunks, OFX.decoder({ max_token_bytes: 524288 })) {
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

test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
	count = (bytes.first() ?? 0).to_u64() * 2 + 1
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

target = Fuzz.target_with!({ name: "ofx-ceilings", examples: [Fuzz.example({ name: "small", revision: "1", make: || [0.U8] }), Fuzz.example({ name: "large", revision: "1", make: || [255.U8] }), Fuzz.example({ name: "retain-small", revision: "1", make: || [0.U8, 255] }), Fuzz.example({ name: "retain-large", revision: "1", make: || [255.U8, 255] })], generator: Fuzz.raw_bytes, test!, show: |bytes| Str.inspect(bytes) })
