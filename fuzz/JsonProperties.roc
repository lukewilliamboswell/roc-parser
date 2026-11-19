app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import JsonRawProperty
import JsonOptionsProperty
import JsonRoundtripProperty
import JsonStreamProperty
import JsonStreamCeilingsProperty

target = Fuzz.suite([
	JsonRawProperty.target,
	JsonOptionsProperty.target,
	JsonRoundtripProperty.target,
	JsonStreamProperty.target,
	JsonStreamCeilingsProperty.target,
])
