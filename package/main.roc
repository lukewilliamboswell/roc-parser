## roc-parser builds parsers from small, composable pieces and ships ready-made
## parsers for common text formats.
##
## [Parser] holds the generic combinators and [Utf8] the byte-level primitives
## and runners. [CSV], [Yaml], [Xml], [Markdown] and [HTTP] parse their formats
## into Roc values, following RFC 4180, YAML 1.2, XML 1.0, CommonMark 0.31.2
## with GitHub extensions, and RFC 9112.
##
## The manual explains when to use each module, with tested examples:
## https://lukewilliamboswell.github.io/roc-parser/
package
	[
		Parser,
		Utf8,
		CSV,
		HTTP,
		Markdown,
		Xml,
		Yaml,
	]
	{
		unicode: "https://github.com/roc-lang/unicode/releases/download/4.2.0/4W8SHzvwet9hH9qZewJ1J1CVQoH7YKA6zyijWFTB3y1w.tar.zst",
	}
