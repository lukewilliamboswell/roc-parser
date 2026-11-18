app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0-rc1/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import ParserCombinatorsProperty
import ValidateProperty
import StringPrimitivesProperty
import ByteScanProperty
import RocSyntaxRawProperty
import RocSyntaxStructuredProperty
import RocSyntaxSpansProperty
import RocSyntaxSpanModelProperty

target = Fuzz.suite([
	ParserCombinatorsProperty.target,
	ValidateProperty.target,
	StringPrimitivesProperty.target,
	ByteScanProperty.target,
	RocSyntaxRawProperty.target,
	RocSyntaxStructuredProperty.target,
	RocSyntaxSpansProperty.target,
	RocSyntaxSpanModelProperty.target,
])
