import MarkupSyntaxFacts
import MarkupCore

MarkupVocabulary :: [].{
	TextBehavior : MarkupSyntaxFacts.TextBehavior
	text_behavior : _
	text_behavior = MarkupSyntaxFacts.text_behavior
	html_void : _
	html_void = MarkupSyntaxFacts.html_void
	html_element_names : _
	html_element_names = MarkupSyntaxFacts.html_element_names
	svg_element_names : _
	svg_element_names = MarkupSyntaxFacts.svg_element_names
	mathml_element_names : _
	mathml_element_names = MarkupSyntaxFacts.mathml_element_names
	aria_attribute_names : _
	aria_attribute_names = MarkupSyntaxFacts.aria_attribute_names
	html_element_name : _
	html_element_name = MarkupSyntaxFacts.html_element_name
	svg_element_name : _
	svg_element_name = MarkupSyntaxFacts.svg_element_name
	mathml_element_name : _
	mathml_element_name = MarkupSyntaxFacts.mathml_element_name
	aria_name : _
	aria_name = MarkupSyntaxFacts.aria_name
	drops_initial_newline : _
	drops_initial_newline = MarkupSyntaxFacts.drops_initial_newline
	aria_enumerated_values : _
	aria_enumerated_values = MarkupSyntaxFacts.aria_enumerated_values
	global_event_name : _
	global_event_name = MarkupSyntaxFacts.global_event_name
	window_event_name : _
	window_event_name = MarkupSyntaxFacts.window_event_name
	legacy_svg_event_name : _
	legacy_svg_event_name = MarkupSyntaxFacts.legacy_svg_event_name
	html_event_name : _
	html_event_name = MarkupSyntaxFacts.html_event_name
	svg_event_name : _
	svg_event_name = MarkupSyntaxFacts.svg_event_name
	event_attribute : MarkupCore.Name, MarkupCore.Name -> Bool
	event_attribute = |element, attribute| MarkupSyntaxFacts.event_attribute({ namespace: element.namespace, local: element.local, prefix: element.prefix }, { namespace: attribute.namespace, local: attribute.local, prefix: attribute.prefix })
}
