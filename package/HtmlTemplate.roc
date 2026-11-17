import Diagnostic
import MarkupCore
import MarkupTemplate
import MarkupValidate
import HtmlValidate
import HtmlTree
import HtmlTokenizer

## Shared typed-hole pipeline for HTML syntax, including foreign fragment
## contexts. Projection runs after authored hole nodes exist; bindings fan out
## through exact node-ID provenance and never trigger source-text parsing.
HtmlTemplate :: [].{
	parse_segments : List(Str), HtmlTree.Context, MarkupCore.Props -> Try(MarkupTemplate.Plan, [InvalidMarkup(MarkupCore.Error)])
	parse_segments = |segments, context, props| {
		raw_context = ["script", "style"].contains(context.name) or (props.syntax == HtmlSyntax and (["xmp", "iframe", "noembed", "noframes", "plaintext"].contains(context.name) or (context.name == "noscript" and context.scripting)))
		if segments.len() > 1 and raw_context {
			return Err(InvalidMarkup(Diagnostic.{ problem: Interpolation({ index: 0, reason: RawTextHole }), location: LineColumn({ line: 1, column: 1 }), path: [] }))
		}
		unprojected = MarkupTemplate.parse_segments_with(
			segments,
			props,
			|text| if props.syntax == HtmlSyntax {
				HtmlTree.parse(text, True, { ..context, defer_projection: True }, props)
			} else {
				MarkupCore.parse_xml_fragment(text, context.namespace, props)
			},
			|text| if props.syntax == HtmlSyntax {
				Ok(HtmlTokenizer.normalized(text.to_utf8(), True, False).text)
			} else {
				MarkupCore.decode_xml_text(text)
			},
		)?
		_ = MarkupValidate.text_slots(unprojected)?
		authored_holes = unprojected.metadata().slots.keep_oks(
			|slot| match slot.target {
				AttributeSlot(target) => Ok(target)
				_ => Err(Skip)
			},
		)
		authored_foreign = MarkupValidate.foreign(unprojected.metadata().parsed, props, authored_holes)?
		authored_checked = HtmlValidate.validate(authored_foreign, props, authored_holes)?
		var $plan = MarkupTemplate.rebuild({ ..unprojected.metadata(), parsed: authored_checked })
		if props.syntax == HtmlSyntax {
			projected = HtmlTree.project_parsed($plan.metadata().parsed, props)?
			$plan = MarkupTemplate.fanout($plan, projected.parsed, projected.clones, props)?
		}
		_ = MarkupValidate.text_slots($plan)?
		checked = $plan.metadata().parsed
		slots = $plan.metadata().slots.map(
			|slot| match slot.target {
				AttributeSlot(target) => {
					node = checked.tree.nodes.get(target.node) ?? { kind: Text(""), attributes: [], children: [], origin: Implied(0) }
					match (node.kind, node.attributes.get(target.attribute)) {
						(Element(name), Ok(attribute)) => {
							expected = if attribute.name.namespace != NoNamespace {
								Language(NamespacedAttribute)
							} else if name.namespace == Uri(MarkupCore.svg_namespace) {
								MarkupValidate.grammar(True, name.local, attribute.name.local)
							} else if name.namespace == Uri(MarkupCore.mathml_namespace) {
								MarkupValidate.grammar(False, name.local, attribute.name.local)
							} else {
								HtmlValidate.grammar(name.local, attribute.name.local)
							}
							{ ..slot, expected }
						}
						_ => slot
					}
				}
				_ => slot
			},
		)
		Ok(MarkupTemplate.rebuild({ ..$plan.metadata(), parsed: checked, slots }))
	}
}
