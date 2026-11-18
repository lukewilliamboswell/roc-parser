import DiagnosticOwnership
import MarkupCore
import ExpandedName

## Native semantic failures own their source-bearing fields at public boundaries.
MarkupDiagnosticOwnership :: [].{
	failure : [InvalidMarkup(MarkupCore.Error)] -> [InvalidMarkup(MarkupCore.Error)]
	failure = |InvalidMarkup(error)| InvalidMarkup({ ..error, problem: problem(error.problem), path: DiagnosticOwnership.own_path(error.path) })

	own_namespace : ExpandedName.Namespace -> ExpandedName.Namespace
	own_namespace = |namespace| match namespace {
		Uri(value) => Uri(DiagnosticOwnership.own_text(value))
		NoNamespace => NoNamespace
	}

	own_name : MarkupCore.Name -> MarkupCore.Name
	own_name = |name| {
		namespace: own_namespace(name.namespace),
		local: DiagnosticOwnership.own_text(name.local),
		prefix: match name.prefix {
			Prefix(value) => Prefix(DiagnosticOwnership.own_text(value))
			Unprefixed => Unprefixed
		},
	}

	own_child_grammar : MarkupCore.ChildGrammar -> MarkupCore.ChildGrammar
	own_child_grammar = owned_children
	own_reason : MarkupCore.StructureReason -> MarkupCore.StructureReason
	own_reason = owned_structure
}

problem : MarkupCore.Problem -> MarkupCore.Problem
problem = |input| match input {
	NonCanonicalName(detail) => NonCanonicalName({ ..detail, authored: DiagnosticOwnership.own_text(detail.authored), expected: DiagnosticOwnership.own_text(detail.expected) })
	HtmlStructure(detail) => HtmlStructure({ ..detail, reason: owned_structure(detail.reason) })
	# XML payloads have already crossed their native ownership boundary.
	SyntaxError(detail) => SyntaxError(detail)
	HtmlSyntax(detail) => HtmlSyntax(detail)
	NamespaceError(detail) => NamespaceError(detail)
	WrongRoot(detail) => WrongRoot({
		expected: { namespace: MarkupDiagnosticOwnership.own_namespace(detail.expected.namespace), local: DiagnosticOwnership.own_text(detail.expected.local) },
		actual: match detail.actual {
			Element(name) => Element(MarkupDiagnosticOwnership.own_name(name))
			Fragment => Fragment
			Text => Text
		},
	})
	UnknownElement(value) => UnknownElement(DiagnosticOwnership.own_text(value))
	UnknownAttribute(detail) => UnknownAttribute({ element: DiagnosticOwnership.own_text(detail.element), attribute: DiagnosticOwnership.own_text(detail.attribute) })
	InvalidValue(detail) => InvalidValue({ ..detail, attribute: DiagnosticOwnership.own_text(detail.attribute), expected: owned_values(detail.expected) })
	InvalidChildren(detail) => InvalidChildren({ element: DiagnosticOwnership.own_text(detail.element), expected: owned_children(detail.expected) })
	Unsupported(detail) => Unsupported(detail)
	LimitExceeded(detail) => LimitExceeded(detail)
	Interpolation(detail) => Interpolation({ ..detail, reason: owned_interpolation(detail.reason) })
	BindingCount(detail) => BindingCount(detail)
}

owned_structure : MarkupCore.StructureReason -> MarkupCore.StructureReason
owned_structure = |input| match input {
	ImpliedElement(value) => ImpliedElement(DiagnosticOwnership.own_text(value))
	MissingEndTag(value) => MissingEndTag(DiagnosticOwnership.own_text(value))
	UnexpectedEndTag(value) => UnexpectedEndTag(DiagnosticOwnership.own_text(value))
	NonVoidSelfClosing(value) => NonVoidSelfClosing(DiagnosticOwnership.own_text(value))
	ReparentedContent => ReparentedContent
	IgnoredMarkup => IgnoredMarkup
	ReconstructedContent => ReconstructedContent
	ChangedAttributes => ChangedAttributes
}

owned_children : MarkupCore.ChildGrammar -> MarkupCore.ChildGrammar
owned_children = |input| match input {
	AllowedParent(value) => AllowedParent(DiagnosticOwnership.own_text(value))
	ImageAlternative => ImageAlternative
	ImageInputAttributes => ImageInputAttributes
	NoNestedAnchor => NoNestedAnchor
	NoInteractiveContent => NoInteractiveContent
	OptionContent => OptionContent
	SelectButtonDescendant => SelectButtonDescendant
	NoChildren => NoChildren
	ElementsOnly => ElementsOnly
	NamedElements(values) => NamedElements(values.map(DiagnosticOwnership.own_text))
	TextOnly => TextOnly
	MathElements => MathElements
	SvgElements => SvgElements
	ChildCount(detail) => ChildCount(detail)
	BaseAndScriptPairs => BaseAndScriptPairs
	ScriptsAndPrescripts => ScriptsAndPrescripts
	ScriptPairs => ScriptPairs
	ExpressionAndAnnotations => ExpressionAndAnnotations
	SvgContentModel => SvgContentModel
}

owned_interpolation : MarkupCore.InterpolationProblem -> MarkupCore.InterpolationProblem
owned_interpolation = |input| match input {
	RawTextHole => RawTextHole
	MissingStaticSegment => MissingStaticSegment
	TextHoleForbidden(value) => TextHoleForbidden(DiagnosticOwnership.own_text(value))
	RecoveryRequired => RecoveryRequired
	SplitCharacterReference => SplitCharacterReference
	InvalidHolePosition => InvalidHolePosition
	MultipleAttributeHoles => MultipleAttributeHoles
	MarkupDelimiterHole => MarkupDelimiterHole
	IncompleteHoleMarkup => IncompleteHoleMarkup
	LostAttributeHole => LostAttributeHole
	CdataHoleNamespace => CdataHoleNamespace
}

owned_values : MarkupCore.ValueGrammar -> MarkupCore.ValueGrammar
owned_values = |input| match input {
	Number => Number
	NonnegativeNumber => NonnegativeNumber
	Length => Length
	NonnegativeLength => NonnegativeLength
	NumberList(detail) => NumberList(detail)
	LengthList => LengthList
	PathData => PathData
	TransformList => TransformList
	Keyword(values) => Keyword(values.map(DiagnosticOwnership.own_text))
	TokenList => TokenList
	Color => Color
	Paint => Paint
	AspectRatio => AspectRatio
	Language(detail) => Language(detail)
}
