import XmlNsCore
import DomNameCore
import ExpandedName

DomName :: [].{
	Name : ExpandedName
	Namespace : DomNameCore.Namespace
	Context : DomNameCore.Context
	Problem : DomNameCore.Problem
	validate_and_extract : Namespace, Str, Context -> Try(Name, Problem)
	validate_and_extract = |namespace, spelling, context| DomNameCore.validate_and_extract(namespace, spelling, context).map_ok(|parts| ExpandedName.{ namespace: parts.namespace, local: parts.local, prefix: parts.prefix })
	html_element : Str -> Try(Name, Problem)
	html_element = |spelling| DomNameCore.html_element(spelling).map_ok(|parts| ExpandedName.{ namespace: parts.namespace, local: parts.local, prefix: parts.prefix })
	unqualified_element : Str -> Try(Name, Problem)
	unqualified_element = |spelling| DomNameCore.unqualified_element(spelling).map_ok(|parts| ExpandedName.{ namespace: parts.namespace, local: parts.local, prefix: parts.prefix })
	unqualified_attribute : Str -> Try(Name, Problem)
	unqualified_attribute = |spelling| DomNameCore.unqualified_attribute(spelling).map_ok(|parts| ExpandedName.{ namespace: parts.namespace, local: parts.local, prefix: parts.prefix })
	qualified : Name -> Str
	qualified = |name| name.qualified()
	valid_namespace_prefix : Str -> Bool
	valid_namespace_prefix = DomNameCore.valid_namespace_prefix
	valid_attribute_local_name : Str -> Bool
	valid_attribute_local_name = DomNameCore.valid_attribute_local_name
	valid_element_local_name : Str -> Bool
	valid_element_local_name = DomNameCore.valid_element_local_name
}

expect {
	DomNameCore.validate_and_extract(Uri("http://www.w3.org/2000/svg"), "a=b", Element).is_ok() and XmlNsCore.parse_str("<a=b/>").is_err() and XmlNsCore.parse_str("<p:a:b xmlns:p='urn:x'/>").is_err()
}
