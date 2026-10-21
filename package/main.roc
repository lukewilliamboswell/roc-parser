## roc-parser builds parsers from small, composable pieces and ships ready-made
## parsers for common text formats.
##
## [Parser] holds the generic combinators and [Utf8] the byte-level primitives
## and runners. [CSV], [Yaml], [Xml], [JsonValue], [Markdown] and [HTTP] parse
## their formats into Roc values, following RFC 4180, YAML 1.2, XML 1.0,
## RFC 8259, CommonMark 0.31.2 with GitHub extensions, and RFC 9112.
## [Literal] checks imported constants; each format also supports quotation
## conversion and configurable validation during compilation.
##
## The manual explains when to use each module, with tested examples:
## https://lukewilliamboswell.github.io/roc-parser/
package
	[
		AsciiDoc,
		CSV,
		HTTP,
		JsonValue,
		Literal,
		Markdown,
		Parser,
		RocSyntax,
		Utf8,
		Xml,
		Yaml,
	]
	{
		unicode: "https://github.com/roc-lang/unicode/releases/download/4.3.0/7HGaK8HUHMs5judroSmUoS4TjPGZ1kAjPTyPDSCWiSKM.tar.zst",
	}
