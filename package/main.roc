## roc-parser builds parsers from small, composable pieces and ships ready-made
## parsers for common text formats.
##
## [Parser] provides generic combinators, UTF-8 primitives and quotation helpers.
## Configured decoders and encoders expose receiver methods for builtin Iter
## and Stream; the owning format supplies their constructors.
## [CSV], [QIF] and [OFX] read banking and tabular records. [JsonValue] reads
## complete JSON values and framed NDJSON or array records. [Xml] and its
## Namespaced API preserve distinct raw-name and expanded-name XML models.
## [HTTP] handles request and response messages and chunked pipelines.
## [Yaml], [Markdown], [AsciiDoc] and [RocSyntax] handle configuration, documents
## and lossless Roc source. [Html], [Svg] and [MathML] provide checked markup.
## [Markup] owns shared names, trees, authoring contexts and template plans;
## Html.RepairContext checks HTML parser repairs independently of authoring rules.
##
## The manual explains when to use each module, with tested examples:
## https://lukewilliamboswell.github.io/roc-parser/
package
	[
		AsciiDoc,
		CSV,
		HTTP,
		Html,
		JsonValue,
		Markdown,
		Markup,
		MathML,
		OFX,
		Parser,
		QIF,
		RocSyntax,
		Svg,
		Xml,
		Yaml,
	]
	{
		unicode: "https://github.com/roc-lang/unicode/releases/download/4.3.3/AJfFMmhvmbgn7THAF1E1sofC3dZaVWPFmVV7GH8Bc7k7.tar.zst",
	}
