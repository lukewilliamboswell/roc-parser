app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import HttpRequestProperty
import HttpResponseProperty
import HttpRawProperty
import HttpRoundtripProperty
import HttpSmugglingProperty
import HttpStreamProperty
import HttpStreamCeilingsProperty

target = Fuzz.suite([
	HttpRequestProperty.target,
	HttpResponseProperty.target,
	HttpRawProperty.target,
	HttpRoundtripProperty.target,
	HttpSmugglingProperty.target,
	HttpStreamProperty.target,
	HttpStreamCeilingsProperty.target,
])
