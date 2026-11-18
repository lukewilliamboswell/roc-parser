import MarkupCore
import MarkupValidate
import HtmlValidate

## Internal whole-tree validation. Only parsed template slots may skip values.
MarkupValidation :: [].{
	check : MarkupCore.Parsed, MarkupCore.Props, List(MarkupValidate.Hole) -> Try(MarkupCore.Parsed, [InvalidMarkup(MarkupCore.Error)])
	check = |parsed, props, holes| {
		foreign = MarkupValidate.foreign_with_html(parsed, props, holes)?
		if foreign.has_html {
			HtmlValidate.validate(foreign.parsed, props, holes)
		} else {
			Ok(foreign.parsed)
		}
	}
}
