app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.6.2/3kyiehxxzwAsjM23uCQGJ3Hx7dAAdzSGtK3k9Jq7Egtu.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.OFX
import parser.Streaming
import OfxGen

test : List(U8) -> Fuzz.Outcome
test = |bytes| {
	results = List.from_iter(Streaming.read_iter(OfxGen.chunks(bytes, bytes).iter(), OFX.decoder({})))
	for event in results {
		match event {
			Err(ParseErr(error)) | Err(ErrorLimitExceeded({ error, .. })) => {
				if error.offset > bytes.len() or error.line == 0 or error.column == 0 {
					crash "OFX diagnostic outside source"
				}
			}
			_ => {}
		}
	}
	Fuzz.keep
}

target = Fuzz.target_with({ name: "ofx-raw", examples: [], generator: Fuzz.raw_bytes, test, show: |bytes| Str.inspect(bytes) })
