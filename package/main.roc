## roc-parser builds parsers from small, composable pieces and ships ready-made
## parsers for common text formats.
##
## [Parser] holds the generic combinators and [Utf8] the byte-level primitives
## and runners. [CSV], [Yaml], [Xml], [XmlNs], [JsonValue], [Markdown] and
## [HTTP] parse their formats into Roc values, following RFC 4180, YAML 1.2,
## XML 1.0 and Namespaces in XML 1.0, RFC 8259, CommonMark 0.31.2 with GitHub
## extensions, and RFC 9112.
## [Literal] checks imported constants; each format also supports quotation
## conversion and configurable validation during compilation.
##
## The manual explains when to use each module, with tested examples:
## https://lukewilliamboswell.github.io/roc-parser/
package
	[
		AsciiDoc,
		CSV,
		DomName,
		HTTP,
		Html,
		JsonValue,
		Literal,
		Markdown,
		Markup,
		MarkupContext,
		MarkupFacts,
		MarkupTemplate,
		MathML,
		Parser,
		RocSyntax,
		Svg,
		SvgPath,
		Utf8,
		Xml,
		XmlNs,
		Yaml,
	]
	{
		unicode: "https://github.com/roc-lang/unicode/releases/download/4.3.3/AJfFMmhvmbgn7THAF1E1sofC3dZaVWPFmVV7GH8Bc7k7.tar.zst",
	}
