app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0-rc1/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import DiagnosticLifetimesProperty

target = Fuzz.suite([
	DiagnosticLifetimesProperty.target,
])
