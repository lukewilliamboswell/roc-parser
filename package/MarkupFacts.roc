import Markup
import MarkupVocabulary
import HtmlValidate
import MarkupValidate

## Namespace-aware vocabulary and HTML syntax facts for tree consumers.
## Names use their explicit expanded identity; a colon never supplies a namespace.
MarkupFacts :: [].{

	## HTML tokenization behavior. ScriptData and Plaintext retain their distinct rules.
	TextBehavior : MarkupVocabulary.TextBehavior

	## Test the built-in element vocabulary used by the current authoring checks.
	## Valid custom HTML names can be queried separately with is_custom_element_name.
	is_known_element : Markup.Name -> Bool
	is_known_element = |element| match element.namespace {
		Uri("http://www.w3.org/1999/xhtml") => HtmlValidate.element_known(element.local)
		Uri("http://www.w3.org/2000/svg") => MarkupValidate.element_known(True, element.local)
		Uri("http://www.w3.org/1998/Math/MathML") => MarkupValidate.element_known(False, element.local)
		_ => False
	}

	## Recognize the custom-element names accepted by HTML authoring checks.
	is_custom_element_name : Str -> Bool
	is_custom_element_name = HtmlValidate.custom_element_name

	## Test an expanded attribute name against the element's current vocabulary.
	## Extension attributes on custom/unknown elements remain available through parser options.
	is_known_attribute : Markup.Name, Markup.Name -> Bool
	is_known_attribute = |element, attribute| match element.namespace {
		Uri("http://www.w3.org/1999/xhtml") => HtmlValidate.attribute_known(element.local, attribute)
		Uri("http://www.w3.org/2000/svg") => MarkupValidate.attribute_known(True, element.local, attribute)
		Uri("http://www.w3.org/1998/Math/MathML") => MarkupValidate.attribute_known(False, element.local, attribute)
		_ => False
	}

	## Recognize a known unnamespaced ARIA attribute, including retained deprecated names.
	is_aria_attribute : Markup.Name -> Bool
	is_aria_attribute = |attribute| attribute.namespace == NoNamespace and MarkupVocabulary.aria_name(attribute.local)

	## Presence/absence booleans apply only to the relevant unnamespaced HTML attributes.
	## SVG, MathML and ARIA true/false values are string-valued enumerations.
	is_boolean_attribute : Markup.Name, Markup.Name -> Bool
	is_boolean_attribute = |element, attribute| element.namespace == Uri(Markup.html_namespace) and attribute.namespace == NoNamespace and HtmlValidate.boolean_attribute(element.local, attribute.local)

	## Return the value grammar already used by the current format validators.
	attribute_grammar : Markup.Name, Markup.Name -> Markup.ValueGrammar
	attribute_grammar = |element, attribute| {
		if attribute.namespace != NoNamespace {
			return Language("namespaced attribute")
		}
		match element.namespace {
			Uri("http://www.w3.org/1999/xhtml") => HtmlValidate.grammar(element.local, attribute.local)
			Uri("http://www.w3.org/2000/svg") => MarkupValidate.grammar(True, element.local, attribute.local)
			Uri("http://www.w3.org/1998/Math/MathML") => MarkupValidate.grammar(False, element.local, attribute.local)
			_ => Language("extension attribute")
		}
	}

	## Known non-boolean, single-value keywords for this attribute.
	## NotEnumerated means no keyword fact is provided, including for token lists.
	## This does not supply the browser's missing/invalid-value defaults.
	enumerated_values : Markup.Name, Markup.Name -> Try(List(Str), [NotEnumerated])
	enumerated_values = |element, attribute| {
		if attribute.namespace != NoNamespace or is_boolean_attribute(element, attribute) {
			return Err(NotEnumerated)
		}
		supported = match element.namespace {
			Uri("http://www.w3.org/1999/xhtml") | Uri("http://www.w3.org/2000/svg") | Uri("http://www.w3.org/1998/Math/MathML") => True
			_ => False
		}
		if !supported {
			return Err(NotEnumerated)
		}
		match MarkupVocabulary.aria_enumerated_values(attribute.local) {
			Ok(values) => return Ok(values)
			Err(_) => {}
		}
		if element.namespace == Uri(Markup.html_namespace) {
			return HtmlValidate.enumerated_values(element.local, attribute.local)
		}
		match attribute_grammar(element, attribute) {
			Keyword(values) => Ok(values)
			_ => Err(NotEnumerated)
		}
	}

	## HTML void behavior is determined by the namespace and local name.
	## Foreign elements with the same spelling are not HTML void elements.
	is_void_element : Markup.Name -> Bool
	is_void_element = |element| element.namespace == Uri(Markup.html_namespace) and MarkupVocabulary.html_void(element.local)

	## HTML text parsing behavior for this element and scripting setting.
	## Foreign-content elements use Data; XML source retains its XML grammar.
	text_behavior : Markup.Name, Bool -> TextBehavior
	text_behavior = |element, scripting| if element.namespace == Uri(Markup.html_namespace) {
		MarkupVocabulary.text_behavior(element.local, scripting)
	} else Data

	## Recognize known handler content attributes by element namespace and expanded name.
	## Consumers may forbid these in literals; ordinary parsers continue accepting them.
	is_event_handler : Markup.Name, Markup.Name -> Bool
	is_event_handler = MarkupVocabulary.event_attribute
}

html_name = |local| { namespace: Uri(Markup.html_namespace), prefix: Unprefixed, local }

svg_name = |local| { namespace: Uri(Markup.svg_namespace), prefix: Unprefixed, local }

math_name = |local| { namespace: Uri(Markup.mathml_namespace), prefix: Unprefixed, local }

plain_attribute = |local| { namespace: NoNamespace, prefix: Unprefixed, local }

expect MarkupFacts.is_known_element(html_name("input"))
expect MarkupFacts.is_known_element(svg_name("linearGradient"))
expect !MarkupFacts.is_known_element(svg_name("lineargradient"))
expect MarkupFacts.is_known_element(math_name("mfrac"))
expect !MarkupFacts.is_known_element({ namespace: NoNamespace, prefix: Unprefixed, local: "div" })
expect !MarkupFacts.is_known_element(html_name("a-new-widget"))
expect MarkupFacts.is_custom_element_name("a-new-widget")
expect !MarkupFacts.is_custom_element_name("div")
expect MarkupFacts.is_known_attribute(html_name("input"), plain_attribute("disabled"))
expect !MarkupFacts.is_known_attribute(html_name("div"), plain_attribute("disabled"))
expect MarkupFacts.is_known_attribute(svg_name("g"), plain_attribute("oninput"))
expect MarkupFacts.is_known_attribute(math_name("mrow"), plain_attribute("onclick"))
expect MarkupFacts.is_known_attribute(svg_name("use"), { namespace: Uri("http://www.w3.org/1999/xlink"), prefix: Prefix("a"), local: "href" })
expect !MarkupFacts.is_known_attribute(svg_name("use"), plain_attribute("a:href"))
expect MarkupFacts.is_aria_attribute(plain_attribute("aria-label"))
expect !MarkupFacts.is_aria_attribute(plain_attribute("aria-invented"))
expect !MarkupFacts.is_aria_attribute({ namespace: Uri("urn:custom"), prefix: Prefix("a"), local: "aria-label" })
expect MarkupFacts.is_boolean_attribute(html_name("input"), plain_attribute("disabled"))
expect !MarkupFacts.is_boolean_attribute(html_name("div"), plain_attribute("disabled"))
expect !MarkupFacts.is_boolean_attribute(svg_name("g"), plain_attribute("disabled"))
expect !MarkupFacts.is_boolean_attribute(html_name("input"), { namespace: Uri("urn:custom"), prefix: Prefix("a"), local: "disabled" })
expect !MarkupFacts.is_boolean_attribute(html_name("div"), plain_attribute("hidden"))
expect !MarkupFacts.is_boolean_attribute(html_name("div"), plain_attribute("aria-hidden"))
expect MarkupFacts.enumerated_values(html_name("div"), plain_attribute("hidden")) == Ok(["", "hidden", "until-found"])
expect MarkupFacts.enumerated_values(html_name("input"), plain_attribute("disabled")) == Err(NotEnumerated)
expect MarkupFacts.enumerated_values(html_name("input"), plain_attribute("type")).map_ok(|values| values.contains("checkbox")) == Ok(True)
expect MarkupFacts.enumerated_values(svg_name("animate"), plain_attribute("fill")) == Ok(["remove", "freeze"])
expect MarkupFacts.enumerated_values(math_name("mfrac"), plain_attribute("bevelled")) == Ok(["true", "false"])
expect MarkupFacts.enumerated_values(svg_name("rect"), plain_attribute("width")) == Err(NotEnumerated)
expect MarkupFacts.is_void_element(html_name("br"))
expect MarkupFacts.is_void_element(html_name("col"))
expect MarkupFacts.is_void_element(html_name("frame"))
expect !MarkupFacts.is_void_element(html_name("image"))
expect !MarkupFacts.is_void_element(svg_name("br"))
expect !MarkupFacts.is_void_element(html_name("span"))
expect MarkupFacts.text_behavior(html_name("p"), True) == Data
expect MarkupFacts.text_behavior(html_name("title"), True) == Rcdata
expect MarkupFacts.text_behavior(html_name("textarea"), False) == Rcdata
expect MarkupFacts.text_behavior(html_name("script"), True) == ScriptData
expect MarkupFacts.text_behavior(html_name("plaintext"), True) == Plaintext
expect ["style", "xmp", "iframe", "noembed", "noframes"].all(|name| MarkupFacts.text_behavior(html_name(name), False) == RawText)
expect MarkupFacts.text_behavior(html_name("noscript"), True) == RawText
expect MarkupFacts.text_behavior(html_name("noscript"), False) == Data
expect MarkupFacts.text_behavior(svg_name("script"), True) == Data
expect MarkupFacts.text_behavior(math_name("mtext"), True) == Data
expect MarkupFacts.is_event_handler(html_name("button"), plain_attribute("onclick"))
expect MarkupFacts.is_event_handler({ namespace: Uri(Markup.html_namespace), prefix: Prefix("h"), local: "div" }, plain_attribute("onclick"))
expect MarkupFacts.is_event_handler(svg_name("g"), plain_attribute("oninput"))
expect MarkupFacts.is_event_handler(math_name("mrow"), plain_attribute("onclick"))
expect !MarkupFacts.is_event_handler({ namespace: NoNamespace, prefix: Unprefixed, local: "div" }, plain_attribute("onclick"))
expect !MarkupFacts.is_event_handler(html_name("div"), plain_attribute("online"))
expect !MarkupFacts.is_event_handler(html_name("div"), plain_attribute("ONCLICK"))
expect !MarkupFacts.is_event_handler(svg_name("g"), { namespace: Uri("urn:custom"), prefix: Prefix("a"), local: "onclick" })
expect !MarkupFacts.is_event_handler(svg_name("g"), plain_attribute("a:onclick"))
expect MarkupFacts.is_event_handler(html_name("body"), plain_attribute("ononline"))
expect !MarkupFacts.is_event_handler(html_name("div"), plain_attribute("ononline"))
expect MarkupFacts.is_event_handler(svg_name("animate"), plain_attribute("onbegin"))
expect !MarkupFacts.is_event_handler(svg_name("g"), plain_attribute("onbegin"))
expect !MarkupFacts.is_event_handler(svg_name("g"), plain_attribute("onactivate"))
expect !MarkupFacts.is_event_handler(svg_name("g"), plain_attribute("onunload"))
expect MarkupFacts.is_event_handler(svg_name("svg"), plain_attribute("onunload"))

expect MarkupFacts.is_custom_element_name("x-a=b") and MarkupFacts.is_custom_element_name("x:a-b")
expect !MarkupFacts.is_custom_element_name("x-a>b") and !MarkupFacts.is_custom_element_name("x-Ab")
expect MarkupFacts.is_boolean_attribute(html_name("iframe"), plain_attribute("credentialless"))
expect !MarkupFacts.is_boolean_attribute(html_name("audio"), plain_attribute("playsinline"))
expect !MarkupFacts.is_boolean_attribute(html_name("audio"), plain_attribute("disablepictureinpicture"))
expect MarkupFacts.is_boolean_attribute(html_name("video"), plain_attribute("playsinline"))
expect MarkupFacts.enumerated_values(html_name("template"), plain_attribute("shadowrootmode")) == Ok(["open", "closed"])
expect MarkupFacts.enumerated_values(html_name("div"), plain_attribute("inputmode")).map_ok(|values| values.contains("decimal")) == Ok(True)
expect MarkupFacts.enumerated_values(svg_name("g"), plain_attribute("aria-hidden")) == Ok(["true", "false", "undefined"])
expect MarkupFacts.enumerated_values(math_name("mrow"), plain_attribute("aria-checked")) == Ok(["true", "false", "mixed", "undefined"])
expect MarkupFacts.enumerated_values({ namespace: Uri("urn:custom"), prefix: Unprefixed, local: "g" }, plain_attribute("aria-hidden")) == Err(NotEnumerated)
expect MarkupFacts.enumerated_values(svg_name("g"), { namespace: Uri("urn:custom"), prefix: Prefix("a"), local: "aria-hidden" }) == Err(NotEnumerated)

expect MarkupFacts.is_event_handler(html_name("input"), plain_attribute("onsearch"))
expect !MarkupFacts.is_event_handler(html_name("div"), plain_attribute("onsearch"))
expect !MarkupFacts.is_event_handler(svg_name("input"), plain_attribute("onsearch"))
