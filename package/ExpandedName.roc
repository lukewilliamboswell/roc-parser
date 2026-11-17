import DiagnosticOwnership
import InitialNewline
import Diagnostic
import DomNameCore
import MarkupSyntaxFacts
import MarkupNameTables
import HtmlNameRules
import ForeignNameRules
import MarkupGrammar

## Explicit expanded identity shared by XML and markup; no vocabulary validity is implied.
ExpandedName := { namespace : ExpandedName.Namespace, local : Str, prefix : [Prefix(Str), Unprefixed] }.{
	is_eq : _
	to_hash : _
	Namespace : [Uri(Str), NoNamespace]
	Context : [Element, Attribute]
	Problem : [InvalidCharacter({ spelling : Str, context : Context }), InvalidNamespace({ namespace : Namespace, spelling : Str })]
	QualifiedProps := { namespace : Namespace, spelling : Str, context : Context }
	AttributeProps := { attribute : ExpandedName }
	TextProps := { scripting : [Enabled, Disabled] ?? Enabled }

	## Validate a computed DOM spelling and retain its explicit namespace identity.
	from_qualified : QualifiedProps -> Try(ExpandedName, [InvalidDomName(Diagnostic(Problem))])
	from_qualified = |props| {
		match DomNameCore.validate_and_extract(props.namespace, props.spelling, props.context) {
			Ok(parts) => Ok(ExpandedName.{ namespace: parts.namespace, local: parts.local, prefix: parts.prefix })
			Err(reason) => {
				problem = match reason {
					InvalidCharacterError => InvalidCharacter({ spelling: DiagnosticOwnership.own_text(props.spelling), context: props.context })
					NamespaceError => InvalidNamespace({
						namespace: match props.namespace {
							Uri(value) => Uri(DiagnosticOwnership.own_text(value))
							NoNamespace => NoNamespace
						},
						spelling: DiagnosticOwnership.own_text(props.spelling),
					})
				}
				Err(InvalidDomName(Diagnostic.{ problem, location: Unavailable(GeneratedValue), path: [] }))
			}
		}
	}

	## Format the explicitly supplied prefix; a colon in local remains literal.
	qualified : ExpandedName -> Str
	qualified = |name| DomNameCore.qualified({ namespace: name.namespace, local: name.local, prefix: name.prefix })

	is_known_element : ExpandedName -> Bool
	is_known_element = |name| match name.namespace {
		Uri("http://www.w3.org/1999/xhtml") => MarkupNameTables.html_name(name.local)
		Uri("http://www.w3.org/2000/svg") => MarkupNameTables.svg_name(name.local)
		Uri("http://www.w3.org/1998/Math/MathML") => MarkupNameTables.mathml_name(name.local)
		_ => False
	}

	is_custom_element : ExpandedName -> Bool
	is_custom_element = |name| name.namespace == Uri("http://www.w3.org/1999/xhtml") and HtmlNameRules.custom_element_name(name.local)

	is_known_attribute : ExpandedName, AttributeProps -> Bool
	is_known_attribute = |element, props| match element.namespace {
		Uri("http://www.w3.org/1999/xhtml") => match props.attribute.namespace {
			NoNamespace => HtmlNameRules.allowed(element.local, props.attribute.local)
			Uri("http://www.w3.org/2000/xmlns/") => True
			Uri("http://www.w3.org/XML/1998/namespace") => ["lang", "space", "base", "id"].contains(props.attribute.local)
			_ => False
		}
		Uri("http://www.w3.org/2000/svg") => ForeignNameRules.allowed_attribute(True, element.local, name_parts(props.attribute))
		Uri("http://www.w3.org/1998/Math/MathML") => ForeignNameRules.allowed_attribute(False, element.local, name_parts(props.attribute))
		_ => False
	}

	is_aria_attribute : ExpandedName -> Bool
	is_aria_attribute = |name| name.namespace == NoNamespace and MarkupNameTables.aria_name(name.local)

	is_boolean_attribute : ExpandedName, AttributeProps -> Bool
	is_boolean_attribute = |element, props| element.namespace == Uri("http://www.w3.org/1999/xhtml") and props.attribute.namespace == NoNamespace and HtmlNameRules.boolean_attribute(element.local, props.attribute.local)

	attribute_grammar : ExpandedName, AttributeProps -> MarkupGrammar.ValueGrammar
	attribute_grammar = |element, props| {
		if props.attribute.namespace != NoNamespace {
			return Language(NamespacedAttribute)
		}
		match element.namespace {
			Uri("http://www.w3.org/1999/xhtml") => HtmlNameRules.grammar(element.local, props.attribute.local)
			Uri("http://www.w3.org/2000/svg") | Uri("http://www.w3.org/1998/Math/MathML") =>
				if element.is_known_attribute(props) {
					ForeignNameRules.value_grammar(element.namespace == Uri("http://www.w3.org/2000/svg"), element.local, props.attribute.local)
				} else Language(ExtensionAttribute)
			_ => Language(ExtensionAttribute)
		}
	}

	enumerated_values : ExpandedName, AttributeProps -> Try(List(Str), [NotEnumerated])
	enumerated_values = |element, props| {
		if props.attribute.namespace != NoNamespace or element.is_boolean_attribute(props) {
			return Err(NotEnumerated)
		}
		match element.namespace {
			Uri("http://www.w3.org/1999/xhtml") => HtmlNameRules.enumerated_values(element.local, props.attribute.local)
			Uri("http://www.w3.org/2000/svg") | Uri("http://www.w3.org/1998/Math/MathML") => {
				match MarkupSyntaxFacts.aria_enumerated_values(props.attribute.local) {
					Ok(values) => Ok(values)
					Err(_) => match element.attribute_grammar(props) {
						Keyword(values) => Ok(values)
						_ => Err(NotEnumerated)
					}
				}
			}
			_ => Err(NotEnumerated)
		}
	}

	is_void_element : ExpandedName -> Bool
	is_void_element = |name| name.namespace == Uri("http://www.w3.org/1999/xhtml") and MarkupSyntaxFacts.html_void(name.local)

	text_behavior : ExpandedName, TextProps -> MarkupSyntaxFacts.TextBehavior
	text_behavior = |name, props| if name.namespace == Uri("http://www.w3.org/1999/xhtml") {
		MarkupSyntaxFacts.text_behavior(name.local, props.scripting == Enabled)
	} else Data

	drops_initial_newline : ExpandedName -> Bool
	drops_initial_newline = |name| match name.namespace {
		Uri(namespace) => MarkupSyntaxFacts.drops_initial_newline(namespace, name.local)
		NoNamespace => False
	}

	## Seed the serializer's first-token line-feed rule from this name.
	initial_newline : ExpandedName -> InitialNewline
	initial_newline = |name| InitialNewline.from_behavior(if (name.drops_initial_newline()) DropInitialLf else PreserveInitialLf)

	is_event_handler : ExpandedName, AttributeProps -> Bool
	is_event_handler = |name, props| MarkupSyntaxFacts.event_attribute(name_parts(name), name_parts(props.attribute))
}

name_parts : ExpandedName -> { namespace : ExpandedName.Namespace, local : Str, prefix : [Prefix(Str), Unprefixed] }
name_parts = |name| { namespace: name.namespace, local: name.local, prefix: name.prefix }

## Computed names retain identity and report structured validation failures.
expect {
	name = ExpandedName.from_qualified({ namespace: Uri("http://www.w3.org/2000/svg"), spelling: "s:circle", context: Element })?
	name.qualified() == "s:circle" and name.is_known_element()
}

## A changed namespace never inherits HTML's void or boolean rules.
expect {
	html : ExpandedName
	html = { namespace: Uri("http://www.w3.org/1999/xhtml"), local: "input", prefix: Unprefixed }
	attr : ExpandedName
	attr = { namespace: NoNamespace, local: "disabled", prefix: Unprefixed }
	html.is_void_element() and html.is_boolean_attribute({ attribute: attr }) and !{ ..html, namespace: NoNamespace }.is_void_element()
}
