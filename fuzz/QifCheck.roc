import parser.QIF
import parser.Parser

## Independent QIF model generation and cross-boundary invariants.
QifCheck :: [].{

	## Preserve all fields while ignoring source locations during a semantic round trip.
	model = |events| events.map(
		|event| match event {
			Header(header) => Header(header)
			Record(record) => Record({ section: record.section, account: record.account, fields: record.fields })
		},
	)

	## Fuzzer bytes select valid fields, Unicode, splits, line endings and row counts.
	generate : List(U8) -> QIF
	generate = |bytes| {
		count = (bytes.first() ?? 0).to_u64() % 8 + 1
		var $events = [Header("!Type:Bank")]
		for i in (0.U64).to(count) {
			choice = (bytes.get(i + 1) ?? 0).to_u64()
			payee = ["Ada", "Café", "€ shop", "a,b", "\"quote\"", "", "日本", "😀"].get(choice % 8) ?? ""
			fields = [{ code: 'D', value: "27/08/2024" }, { code: 'T', value: if (choice % 2 == 0) "-12.3400" else "+0.0001" }, { code: 'P', value: payee }, { code: 'M', value: "memo" }, { code: 'X', value: "unknown" }]
			splits = if (choice % 3 == 0) [{ code: 'S', value: "Food" }, { code: 'E', value: "split memo" }, { code: '$', value: "-12.34" }] else []
			$events = $events.append(Record({ section: "Bank", account: "", fields: fields.concat(splits), at: { offset: 0, line: 1, column: 1 } }))
		}
		QIF.{ events: $events }
	}

	## Compare the same bytes under one-byte, scheduled, empty and whole chunks.
	raw = |bytes| {
		one = List.from_iter((QIF.decoder({})).read_iter([bytes].iter()))
		chunks = bytes.map(|byte| [byte])
		many = List.from_iter((QIF.decoder({})).read_iter(chunks.iter()))
		if one != many {
			crash "QIF chunk scheduling changed results"
		}
		whole = QIF.parse_bytes(bytes, {})
		match Str.from_utf8(bytes) {
			Ok(text) => match (QIF.parse_str(text, {}), whole) {
				(Ok(a), Ok(b)) if a.events == b.events => {}
				(Err(InvalidQif(a)), Err(InvalidQif(b))) if a == b => {}
				_ => crash "QIF Str and UTF-8 byte entrypoints disagree"
			}
			Err(_) => {}
		}
		match whole {
			Ok(doc) => {
				got = (QIF.decoder({})).collect([[], bytes, []].iter()) ?? crash "raw/iter acceptance mismatch"
				if got != doc.events {
					crash "raw/iter event mismatch"
				}
				written = QIF.encode(doc, {}) ?? crash "accepted QIF cannot be written"
				again = QIF.parse_bytes(written, {}) ?? crash "written QIF cannot be read"
				if model(doc.events) != model(again.events) {
					crash "QIF semantic round trip changed fields"
				}
			}
			Err(InvalidQif(error)) => {
				if source_offset(error.location) > bytes.len() {
					crash "QIF diagnostic outside input"
				}
				match one.last() {
					Ok(Err(InvalidQif(again))) if again == error => {}
					_ => crash "whole/stream errors disagree"
				}
			}
		}
	}

	## Generated expectations are the model itself, never the parser's output.
	roundtrip = |bytes| {
		doc = generate(bytes)
		line_ending = match (bytes.last() ?? 0) % 3 {
			0 => Lf
			1 => CrLf
			_ => Cr
		}
		chunk_bytes = (bytes.first() ?? 0).to_u64() % 64 + 1
		var $written = []
		for chunk in (QIF.encoder({ line_ending: line_ending, chunk_bytes: chunk_bytes })).write_iter(doc.events.iter()) {
			encoded = chunk ?? crash "valid generated QIF cannot be written"
			if encoded.len() > chunk_bytes {
				crash "encoder exceeded configured chunk size"
			}
			$written = $written.concat(encoded)
		}
		written = $written
		parsed = (QIF.decoder({})).collect(written.map(|byte| [byte]).iter()) ?? crash "valid generated QIF cannot be read"
		if model(doc.events) != model(parsed) {
			crash "generated QIF differs from independent model"
		}
		decoded = (QIF.transaction_decoder({})).collect([written].iter()) ?? crash "valid generated QIF cannot be decoded"
		if decoded.len() != doc.events.len() {
			crash "typed stream lost records"
		}
		unrepresentable = doc.events.any(
			|event| match event {
				Header(_) => False
				Record(record) => record.fields.any(|field| field.value == "日本" or field.value == "😀")
			},
		)
		match QIF.encode(doc, { encoding: Windows1252 }) {
			Ok(legacy) => {
				if unrepresentable {
					crash "Windows-1252 writer silently changed characters"
				}
				parsed_legacy = (QIF.decoder({ encoding: Windows1252 })).collect(legacy.map(|byte| [byte]).iter()) ?? crash "legacy QIF rejected"
				if model(parsed_legacy) != model(doc.events) {
					crash "Windows-1252 QIF semantic roundtrip changed values"
				}
			}
			Err(_) => if !unrepresentable {
				crash "representable Windows-1252 model rejected"
			}
		}
	}

	## A known malformed terminator never becomes successful EOF, under any split.
	malformed = |bytes| {
		doc = generate(bytes)
		written = QIF.encode(doc, { line_ending: Lf }) ?? crash "generated encoding failed"
		truncated = written.drop_last(2)
		match QIF.parse_bytes(truncated, {}) {
			Ok(_) => crash "missing QIF terminator accepted"
			Err(_) => {}
		}
		raw(truncated)
		bad = "!Type:Bank\nD27/08/2024\nTbad\n^\nD27/08/2024\nT1\n^\n".to_utf8()
		results = List.from_iter((QIF.transaction_decoder({ errors: { policy: ReportInvalidRecords } })).read_iter(bad.map(|byte| [byte]).iter()))
		if results.last() != Ok(Ok(Finished({ emitted_items: 2, rejected_records: 1 }))) {
			crash "recovery report counts differ"
		}
	}
}

source_offset : Parser.Location -> U64
source_offset = |location| match location {
	InputOffset(offset) | ByteOffset(offset) => offset
	Text(at) => at.offset
	_ => crash "expected retained source byte coordinates"
}
