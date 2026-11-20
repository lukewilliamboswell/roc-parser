app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import JsonStreamCeilingsProperty

# The combined suite crashes in lir_gen on nightly-2026-11-19-54c51c9.
# Keep these allocation ceilings specialized: the unspecialized build exceeds
# the one-byte record-array reader's ceiling.
# Rejoin JsonProperties when its specialized build passes all properties.
target = Fuzz.suite([JsonStreamCeilingsProperty.target])
