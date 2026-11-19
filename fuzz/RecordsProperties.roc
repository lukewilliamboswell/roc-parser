app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import OfxRawProperty
import OfxStreamProperty
import OfxRoundtripProperty
import OfxMalformedProperty
import QifRawProperty
import QifStreamProperty
import QifRoundtripProperty
import QifMalformedProperty
import QifCeilingsProperty
import StreamContractProperty
import StreamCeilingsProperty
import CsvProperty
import CsvRawProperty
import CsvRoundtripProperty
import CsvDecodeProperty
import CsvStreamRawProperty

target = Fuzz.suite([
	OfxRawProperty.target,
	OfxStreamProperty.target,
	OfxRoundtripProperty.target,
	OfxMalformedProperty.target,
	QifRawProperty.target,
	QifStreamProperty.target,
	QifRoundtripProperty.target,
	QifMalformedProperty.target,
	QifCeilingsProperty.target,
	StreamContractProperty.target,
	StreamCeilingsProperty.target,
	CsvProperty.target,
	CsvRawProperty.target,
	CsvRoundtripProperty.target,
	CsvDecodeProperty.target,
	CsvStreamRawProperty.target,
])
