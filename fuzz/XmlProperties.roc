app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import XmlProperty
import XmlRawProperty
import XmlRoundtripProperty
import XmlMalformedProperty
import XmlReaderProperty
import XmlStreamProperty
import XmlStreamCeilingsProperty
import XmlLimitsProperty
import XmlCeilingsProperty
import XmlnsRawProperty
import XmlnsRoundtripProperty

target = Fuzz.suite([
	XmlProperty.target,
	XmlRawProperty.target,
	XmlRoundtripProperty.target,
	XmlMalformedProperty.target,
	XmlReaderProperty.target,
	XmlStreamProperty.target,
	XmlStreamCeilingsProperty.target,
	XmlLimitsProperty.target,
	XmlCeilingsProperty.target,
	XmlnsRawProperty.target,
	XmlnsRoundtripProperty.target,
])
