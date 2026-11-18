app [target] {
	fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.7.0-rc1/HBigHQhVZWEF2EkLxpYK35u8P6UNsAmPYxpNFTxXK1Br.tar.zst",
	parser: "../package/main.roc",
}
import fuzz.Fuzz
import AsciidocRawProperty
import AsciidocStructuredProperty
import MarkdownDocumentProperty
import MarkdownBlocksProperty
import MarkdownRefdefsProperty
import MarkdownInlineProperty
import MarkdownInlineAstProperty
import MarkdownInlinePathologicalProperty
import YamlProperty
import YamlRawProperty
import YamlSourceProperty
import YamlRoundtripProperty
import YamlBlockProperty

target = Fuzz.suite([
	AsciidocRawProperty.target,
	AsciidocStructuredProperty.target,
	MarkdownDocumentProperty.target,
	MarkdownBlocksProperty.target,
	MarkdownRefdefsProperty.target,
	MarkdownInlineProperty.target,
	MarkdownInlineAstProperty.target,
	MarkdownInlinePathologicalProperty.target,
	YamlProperty.target,
	YamlRawProperty.target,
	YamlSourceProperty.target,
	YamlRoundtripProperty.target,
	YamlBlockProperty.target,
])
