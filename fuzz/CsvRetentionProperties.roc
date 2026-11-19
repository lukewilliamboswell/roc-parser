app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import CsvStreamCeilingsProperty

# Temporary singleton for https://github.com/roc-lang/roc/issues/13191.
# Preserve the specializing backend, opaque ownership fence and every assertion.
# Rejoin RecordsProperties when the combined suite passes diagnostic identity,
# excerpt/original-length checks and the unchanged allocation ceilings.
target = Fuzz.suite([CsvStreamCeilingsProperty.target])
