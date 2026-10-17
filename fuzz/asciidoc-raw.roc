app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
	parser: "../package/main.roc",
}

import fuzz.Fuzz
import parser.AsciiDoc

## Arbitrary UTF-8 must return a tree or a located, explained error.
## LF and CRLF documents must agree on successful source-preserving trees.
test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	match Str.from_utf8(bytes) {
		Err(_) => Fuzz.reject
		Ok(source) => {
			result = AsciiDoc.parse_str(source)
			match result {
				Ok(document) => {
					html = AsciiDoc.render(document)
					if AsciiDoc.render(document) != html {
						crash "AsciiDoc conversion is not deterministic"
					}
					if !bytes.contains('\r') {
						crlf = Str.replace_each(source, "\n", "\r\n")
						if AsciiDoc.parse_str(crlf) != Ok(document) {
							crash "CRLF changed an accepted document: ${Str.inspect(source)}"
						}
						match AsciiDoc.parse_str(crlf) {
							Ok(other) => if AsciiDoc.render(other) != html {
								crash "CRLF changed rendered HTML"
							}
							Err(_) => crash "CRLF rejected an accepted document"
						}
					}
				}
				Err(Unsupported(error)) | Err(InvalidAsciiDoc(error)) => {
					if error.offset > bytes.len() or error.line == 0 or error.message.is_empty() {
						crash "invalid AsciiDoc error location: ${Str.inspect(source)}"
					}
					prefix = bytes.sublist({ start: 0, len: error.offset })
					if !Str.from_utf8(prefix).is_ok() {
						crash "AsciiDoc error offset splits a UTF-8 character"
					}
					line = 1 + prefix.fold(0.U64, |count, byte| if byte == '\n' count + 1 else count)
					if error.line != line {
						crash "AsciiDoc byte offset and line disagree"
					}
				}
			}
			Fuzz.keep
		}
	}
}

target = Fuzz.target_with({ name: "asciidoc-raw", generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(Str.from_utf8(bytes)) })
