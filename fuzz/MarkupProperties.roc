app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import SvgPathProperty
import SvgRawProperty
import MathmlRawProperty
import HtmlRawProperty
import MarkupSourceProperty
import DomConstructionProperty
import MarkupStructuredProperty
import MarkupLimitsProperty
import MarkupSegmentsProperty

target = Fuzz.suite([
	SvgPathProperty.target,
	SvgRawProperty.target,
	MathmlRawProperty.target,
	HtmlRawProperty.target,
	MarkupSourceProperty.target,
	DomConstructionProperty.target,
	MarkupStructuredProperty.target,
	MarkupLimitsProperty.target,
	MarkupSegmentsProperty.target,
])
