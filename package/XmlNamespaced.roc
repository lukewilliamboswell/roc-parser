import XmlCore
import XmlNsCore
import XmlParseProps
import Diagnostic

## Namespace-resolved XML documents and incremental reading.
XmlNamespaced := { declaration : Try(XmlCore.Declaration, [Missing]), root : XmlNsCore.Node }.{
	is_eq : _
	to_hash : _
	Namespace : XmlNsCore.Namespace
	Name : XmlNsCore.Name
	Binding : XmlNsCore.Binding
	Attribute : XmlNsCore.Attribute
	Node : XmlNsCore.Node
	Problem : XmlNsCore.Problem
	NamespaceProblem : XmlNsCore.NamespaceProblem
	ReadError : XmlNsCore.ReadError
	StreamError : XmlNsCore.StreamError
	Event : XmlNsCore.Event
	Reader : XmlNsCore.Reader
	SourceAttribute : XmlNsCore.SourceAttribute
	SourceEvent : XmlNsCore.SourceEvent
	Decoder : XmlNsCore.Decoder
	Metadata : XmlCore.Metadata
	Props : XmlParseProps
	WriteProps : XmlNsCore.WriteProps
	EncodeProblem : XmlNsCore.EncodeProblem
	AttributeOrder : XmlNsCore.AttributeOrder
	parse_str : Str, Props -> Try(XmlNamespaced, ReadError)
	parse_str = |source, props| {
		result = match props.limits {
			Unbounded => XmlNsCore.parse_str(source)
			Bounded(limits) => XmlNsCore.parse(source, limits)
		}
		result.map_ok(|doc| XmlNamespaced.{ declaration: doc.declaration, root: doc.root })
	}
	reader : Str, XmlCore.ReaderProps -> Try(Reader, ReadError)
	reader = XmlNsCore.reader
	decoder : XmlCore.StreamProps -> Decoder
	decoder = XmlNsCore.decoder
	fold_source : _
	fold_source = XmlNsCore.fold_source
	encode : XmlNamespaced, WriteProps -> Try(List(U8), [Unrenderable(Diagnostic(EncodeProblem))])
	encode = |doc, props| XmlNsCore.encode(XmlNsCore.{ declaration: doc.declaration, root: doc.root }, props)
	xml_namespace : Str
	xml_namespace = XmlNsCore.xml_namespace
	xmlns_namespace : Str
	xmlns_namespace = XmlNsCore.xmlns_namespace
}
