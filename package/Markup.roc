import InitialNewline
import ExpandedName
import MarkupCore
import MarkupContext
import MarkupTemplate
import MarkupFacts
import XmlNsCore

## Shared markup identities, source trees, contexts and templates.
Markup :: [].{
	Context : MarkupContext.Context
	Template :: [].{
		Plan : MarkupTemplate.Plan
		Bound(a) : MarkupTemplate.Bound(a)
		Slot : MarkupTemplate.Slot
	}
	ContextProblem : MarkupContext.Problem
	ContextProps : MarkupContext.ContextProps
	ContextChildProps : MarkupContext.ContextChildProps
	Syntax : MarkupCore.Syntax
	Span : MarkupCore.Span
	Token : MarkupCore.Token
	Source : MarkupCore.Source
	ValueGrammar : MarkupCore.ValueGrammar
	ValueLanguage : MarkupCore.ValueLanguage
	StructureReason : MarkupCore.StructureReason
	ChildGrammar : MarkupCore.ChildGrammar
	UnsupportedReason : MarkupCore.UnsupportedReason
	ActualRoot : MarkupCore.ActualRoot
	Resource : MarkupCore.Resource
	InterpolationProblem : MarkupCore.InterpolationProblem
	Problem : MarkupCore.Problem
	Error : MarkupCore.Error
	Props : MarkupCore.Props
	InitialNewlineState : InitialNewline
	InitialNewlineToken : InitialNewline.Token
	Name : ExpandedName
	NameProps : ExpandedName.QualifiedProps
	AttributeProps : ExpandedName.AttributeProps
	NameTextProps : ExpandedName.TextProps
	Attribute : MarkupCore.Attribute
	Origin : MarkupCore.Origin
	Node : MarkupCore.Node
	Tree : MarkupCore.Tree
	Parsed : MarkupCore.Parsed
	parse_xml : _
	parse_xml = MarkupCore.parse_xml
	parse_xml_fragment : _
	parse_xml_fragment = MarkupCore.parse_xml_fragment
	whitespace : _
	whitespace = MarkupCore.whitespace
	html_namespace : Str
	html_namespace = MarkupCore.html_namespace
	svg_namespace : Str
	svg_namespace = MarkupCore.svg_namespace
	mathml_namespace : Str
	mathml_namespace = MarkupCore.mathml_namespace

	## Canonical element spellings for a built-in namespace.
	element_names : XmlNsCore.Namespace -> List(Str)
	element_names = MarkupFacts.element_names
	aria_attribute_names : List(Str)
	aria_attribute_names = MarkupFacts.aria_attribute_names
}
