app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import OfxCeilingsProperty

# Keep the original allocation ceilings on the specializing backend.
# The combined Records suite aborts during checked type-key normalization on
# nightly-2026-11-18-a520eed (roc-lang/roc#13213); its Boxy build exceeds
# the small OFX ceiling.
# Rejoin RecordsProperties when its specializing build passes every property.
target = Fuzz.suite([OfxCeilingsProperty.target])
