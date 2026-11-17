import XmlCore
import XmlNsCore
import Parser

## XML documents, readers and resolved namespace values.
Xml := { declaration : Try(XmlCore.Declaration, [Missing]), root : XmlCore.Node }.{
	is_eq : _
	to_hash : _

	## XML documents with namespace-resolved names.
	Namespaced : XmlNsCore
	Lints : XmlCore.Lints
	ValidationProps : XmlCore.ValidationProps
	Attribute : XmlCore.Attribute
	TextEncoding : XmlCore.TextEncoding
	Declaration : XmlCore.Declaration
	Version : XmlCore.Version
	Node : XmlCore.Node
	Problem : XmlCore.Problem
	Construct : XmlCore.Construct
	Error : XmlCore.Error
	ReaderProps : XmlCore.ReaderProps
	Limit : XmlCore.Limit
	ReadError : XmlCore.ReadError
	StreamProps : XmlCore.StreamProps
	StreamError : XmlCore.StreamError
	Decoder : XmlCore.Decoder
	decoder : _
	decoder = XmlCore.decoder
	SourceSpan : XmlCore.SourceSpan
	SourceStart : XmlCore.SourceStart
	SourceEvent : XmlCore.SourceEvent
	fold_source : _
	fold_source = XmlCore.fold_source
	Event : XmlCore.Event
	Reader : XmlCore.Reader
	reader : _
	reader = XmlCore.reader

	## Parse complete XML with explicit resource limits.
	parse_str : Str, ReaderProps -> Try(Xml, ReadError)
	parse_str = |source, props| XmlCore.parse(source, props).map_ok(|doc| Xml.{ declaration: doc.declaration, root: doc.root })

	## Construct an XML parser for composition.
	parser : {} -> Parser(Parser.Utf8.Bytes, Xml, Problem)
	parser = |_| XmlCore.parser.map(|doc| Xml.{ declaration: doc.declaration, root: doc.root })

	## Check source with quotation lint rules using typed native diagnostics.
	validate : ValidationProps -> Try(Xml, [BadQuotedBytes(Str)])
	validate = |props| XmlCore.validate(props).map_ok(|doc| Xml.{ declaration: doc.declaration, root: doc.root })
	from_quote : Str -> Try(Xml, [BadQuotedBytes(Str)])
	from_quote = |source| validate({ source: source })
}
