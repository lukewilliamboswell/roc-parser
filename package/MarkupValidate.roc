import Markup
import MarkupTemplate
import MarkupValues
import SvgPath
import MarkupVocabulary

## SVG and Presentation MathML authoring checks shared by the three frontends.
## These checks do not execute or validate embedded CSS and JavaScript.
MarkupValidate :: [].{

	## Whole attribute-value holes whose future values are not yet available.
	Hole : { node : U64, attribute : U64 }

	## Validate foreign vocabularies throughout a mixed-namespace tree.
	foreign : Markup.Parsed, Markup.Props, List(Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	foreign = |parsed, props, holes| foreign_with_html(parsed, props, holes).map_ok(|checked| checked.parsed)

	## Validate foreign vocabularies and report whether HTML checks are needed.
	foreign_with_html : Markup.Parsed, Markup.Props, List(Hole) -> Try({ parsed : Markup.Parsed, has_html : Bool }, [InvalidMarkup(Markup.Error)])
	foreign_with_html = |parsed, props, holes| {
		hole_positions = holes.fold(Dict.empty(), |dict, hole| dict.insert((hole.node, hole.attribute), True))
		var $diagnostics = parsed.diagnostics
		var $has_html = False
		var $pending = parsed.tree.roots.map(|index| { index, extension: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, extension: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup({ problem: Unsupported("invalid tree index"), line: 1, column: 1 }))
			match node.kind {
				Element(name) => {
					$has_html = $has_html or name.namespace == Uri(Markup.html_namespace)
					svg = name.namespace == Uri(Markup.svg_namespace)
					mathml = name.namespace == Uri(Markup.mathml_namespace)
					if !item.extension {
						known = if (svg) svg_elements(name.local) else if (mathml) math_elements(name.local) else True
						if !known or (!svg and !mathml and name.namespace != Uri(Markup.html_namespace)) {
							if !props.allow_unknown_elements {
								return fail(parsed, node, UnknownElement(name.local))
							}
							if $diagnostics.len() >= props.max_diagnostics {
								return fail(parsed, node, ResourceLimit("diagnostics"))
							}
							offset = match node.origin {
								Authored(span) => span.start
								Cloned(copy) => copy.source.start
								Implied(at) => at
							}
							at = Markup.position(parsed.source, offset)
							$diagnostics = $diagnostics.append({ problem: UnknownElement(name.local), line: at.line, column: at.column })
						}
						var $attr_index = 0
						for attr in node.attributes {
							if svg or mathml {
								allowed = allowed_attribute(svg, name.local, attr.name)
								if !allowed {
									problem = UnknownAttribute({ element: name.local, attribute: attr.name.local })
									if !props.allow_unknown_attributes {
										return attr_fail(parsed, attr, problem)
									}
									if $diagnostics.len() >= props.max_diagnostics {
										return fail(parsed, node, ResourceLimit("diagnostics"))
									}
									at = Markup.position(parsed.source, attr.span.start)
									$diagnostics = $diagnostics.append({ problem, line: at.line, column: at.column })
								}
								if props.check_values and allowed and attr.name.namespace == NoNamespace and !hole_positions.contains((item.index, $attr_index)) {
									expected_grammar = value_grammar(svg, name.local, attr.name.local)
									if !valid_value(attr.value, expected_grammar) and !(svg and (svg_presentation(attr.name.local) or member(["x", "y", "cx", "cy", "r", "rx", "ry", "width", "height"], attr.name.local)) and ["inherit", "initial", "unset", "revert", "revert-layer"].contains(ascii_lower(attr.value))) {
										return attr_fail(parsed, attr, InvalidValue({ attribute: attr.name.local, value: attr.value, expected: expected_grammar }))
									}
								}
							}
							$attr_index = $attr_index + 1
						}
						if svg and known {
							check_svg_children(parsed, node, name.local)?
						}
						if mathml and known {
							check_math_children(parsed, node, name.local)?
						}
					}
					extension = item.extension or (svg and name.local == "metadata") or (mathml and name.local == "annotation-xml")
					for index in node.children {
						$pending = $pending.append({ index, extension })
					}
				}
				_ => {}
			}
		}
		Ok({ parsed: { ..parsed, diagnostics: $diagnostics }, has_html: $has_html })
	}

	## Reject text holes where a format permits only element/whitespace content.
	text_slots : MarkupTemplate.Plan -> Try(MarkupTemplate.Plan, [InvalidMarkup(Markup.Error)])
	text_slots = |plan| {
		var $parents = Dict.empty()
		var $index = 0
		for node in plan.parsed.tree.nodes {
			for child in node.children {
				$parents = $parents.insert(child, $index)
			}
			$index = $index + 1
		}
		for slot in plan.slots {
			match slot.target {
				TextSlot(target) => {
					match $parents.get(target.node) {
						Ok(parent) => {
							node = plan.parsed.tree.nodes.get(parent) ?? return Err(InvalidMarkup({ problem: Unsupported("invalid text slot parent"), line: 1, column: 1 }))
							match node.kind {
								Element(name) => {
									copied = match (plan.parsed.tree.nodes.get(target.node) ?? node).origin {
										Cloned(_) => True
										_ => False
									}
									html_nothing = member(["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr", "selectedcontent"], name.local) or (name.local == "option" and node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == "label") and node.attributes.any(|a| a.name.namespace == NoNamespace and a.name.local == "value"))
									allowed = copied or if (name.namespace == Uri(Markup.svg_namespace)) !svg_elements(name.local) or member(["text", "tspan", "textPath", "title", "desc", "metadata", "foreignObject", "script", "style"], name.local) else if (name.namespace == Uri(Markup.mathml_namespace)) !math_elements(name.local) or member(["mi", "mn", "mo", "ms", "mtext", "annotation", "annotation-xml"], name.local) else if (name.namespace == Uri(Markup.html_namespace)) !html_nothing and !member(["selectedcontent", "html", "head", "table", "thead", "tbody", "tfoot", "tr", "colgroup", "ul", "ol", "menu", "dl", "select", "optgroup", "picture"], name.local) else True
									if !allowed {
										at = Markup.position(plan.parsed.source, slot.location.start)
										return Err(InvalidMarkup({ problem: Interpolation({ index: slot.index, reason: "text holes are not allowed inside ${name.local}" }), line: at.line, column: at.column }))
									}
								}
								_ => {}
							}
						}
						Err(_) => {}
					}
				}
				_ => {}
			}
		}
		Ok(plan)
	}

	element_known : Bool, Str -> Bool
	element_known = |svg, local| if (svg) svg_elements(local) else math_elements(local)
	attribute_known : Bool, Str, Markup.Name -> Bool
	attribute_known = allowed_attribute

	## The typed value grammar applied to a known attribute.
	grammar : Bool, Str, Str -> Markup.ValueGrammar
	grammar = |svg, element, attr| if allowed_attribute(svg, element, { namespace: NoNamespace, local: attr, prefix: Unprefixed }) {
		value_grammar(svg, element, attr)
	} else {
		Language("extension attribute")
	}
}

svg_elements = |key| match key {
	"a" | "animate" | "animateMotion" | "animateTransform" | "circle" | "clipPath" | "defs" | "desc" | "discard" | "ellipse" | "feBlend" | "feColorMatrix" | "feComponentTransfer" | "feComposite" | "feConvolveMatrix" | "feDiffuseLighting" | "feDisplacementMap" | "feDistantLight" | "feDropShadow" | "feFlood" | "feFuncA" | "feFuncB" | "feFuncG" | "feFuncR" | "feGaussianBlur" | "feImage" | "feMerge" | "feMergeNode" | "feMorphology" | "feOffset" | "fePointLight" | "feSpecularLighting" | "feSpotLight" | "feTile" | "feTurbulence" | "filter" | "foreignObject" | "g" | "hatch" | "hatchpath" | "image" | "line" | "linearGradient" | "marker" | "mask" | "mesh" | "meshgradient" | "meshpatch" | "meshrow" | "metadata" | "mpath" | "path" | "pattern" | "polygon" | "polyline" | "radialGradient" | "rect" | "script" | "set" | "solidcolor" | "stop" | "style" | "svg" | "switch" | "symbol" | "text" | "textPath" | "title" | "tspan" | "use" | "view" => True
	_ => False
}

math_elements = |key| match key {
	"annotation" | "annotation-xml" | "maction" | "math" | "menclose" | "merror" | "mfenced" | "mfrac" | "mi" | "mlabeledtr" | "mmultiscripts" | "mn" | "mo" | "mover" | "mpadded" | "mphantom" | "mprescripts" | "mroot" | "mrow" | "ms" | "mspace" | "msqrt" | "mstyle" | "msub" | "msubsup" | "msup" | "mtable" | "mtd" | "mtext" | "mtr" | "munder" | "munderover" | "none" | "semantics" => True
	_ => False
}

global_attributes = |key| match key {
	"id" | "class" | "style" | "lang" | "tabindex" | "autofocus" | "role" | "title" => True
	_ => False
}

svg_presentation = |key| match key {
	"alignment-baseline" | "baseline-shift" | "clip" | "clip-path" | "clip-rule" | "color" | "color-interpolation" | "color-interpolation-filters" | "color-rendering" | "cursor" | "direction" | "display" | "dominant-baseline" | "fill" | "fill-opacity" | "fill-rule" | "filter" | "flood-color" | "flood-opacity" | "font-family" | "font-size" | "font-size-adjust" | "font-stretch" | "font-style" | "font-variant" | "font-weight" | "glyph-orientation-horizontal" | "glyph-orientation-vertical" | "image-rendering" | "isolation" | "letter-spacing" | "lighting-color" | "marker-end" | "marker-mid" | "marker-start" | "mask" | "mix-blend-mode" | "opacity" | "overflow" | "paint-order" | "pointer-events" | "shape-rendering" | "stop-color" | "stop-opacity" | "stroke" | "stroke-dasharray" | "stroke-dashoffset" | "stroke-linecap" | "stroke-linejoin" | "stroke-miterlimit" | "stroke-opacity" | "stroke-width" | "text-anchor" | "text-decoration" | "text-rendering" | "transform" | "transform-box" | "transform-origin" | "unicode-bidi" | "vector-effect" | "visibility" | "white-space" | "word-spacing" | "writing-mode" => True
	_ => False
}

svg_events = MarkupVocabulary.svg_event_name

math_global = |key| match key {
	"dir" | "displaystyle" | "scriptlevel" | "mathbackground" | "mathcolor" | "mathsize" | "mathvariant" | "href" => True
	_ => False
}

allowed_attribute : Bool, Str, Markup.Name -> Bool
allowed_attribute = |svg, element, name| {
	match name.namespace {
		Uri("http://www.w3.org/2000/xmlns/") => True
		Uri("http://www.w3.org/XML/1998/namespace") => member(["lang", "space", "base", "id"], name.local)
		Uri("http://www.w3.org/1999/xlink") => svg and member(["href", "type", "role", "arcrole", "title", "show", "actuate"], name.local)
		NoNamespace => {
			key = name.local
			(if (svg) svg_attribute(element, key) else math_attribute(element, key)) or global_attributes(key) or key.starts_with("data-") or aria_attributes(key) or (if (svg) svg_presentation(key) or svg_events(key) else math_global(key) or MarkupVocabulary.global_event_name(key))
		}
		_ => False
	}
}

svg_attribute : Str, Str -> Bool
svg_attribute = |element, key| {
	if key == "requiredExtensions" or key == "systemLanguage" {
		return True
	}
	match element {
		"svg" => match key {
			"x" | "y" | "width" | "height" | "viewBox" | "preserveAspectRatio" | "version" | "baseProfile" | "contentScriptType" | "contentStyleType" | "zoomAndPan" => True
			_ => False
		}
		"view" => match key {
			"viewBox" | "preserveAspectRatio" | "viewTarget" | "zoomAndPan" => True
			_ => False
		}
		"path" | "hatchpath" => match key {
			"d" | "pathLength" => True
			_ => False
		}
		"rect" => match key {
			"x" | "y" | "width" | "height" | "rx" | "ry" | "pathLength" => True
			_ => False
		}
		"circle" => match key {
			"cx" | "cy" | "r" | "pathLength" => True
			_ => False
		}
		"ellipse" => match key {
			"cx" | "cy" | "rx" | "ry" | "pathLength" => True
			_ => False
		}
		"line" => match key {
			"x1" | "y1" | "x2" | "y2" | "pathLength" => True
			_ => False
		}
		"polygon" | "polyline" => match key {
			"points" | "pathLength" => True
			_ => False
		}
		"text" | "tspan" => match key {
			"x" | "y" | "dx" | "dy" | "rotate" | "textLength" | "lengthAdjust" => True
			_ => False
		}
		"textPath" => match key {
			"href" | "startOffset" | "method" | "spacing" | "side" | "textLength" | "lengthAdjust" => True
			_ => False
		}
		"use" | "image" | "foreignObject" => match key {
			"x" | "y" | "width" | "height" | "href" | "preserveAspectRatio" | "crossorigin" | "decoding" => True
			_ => False
		}
		"a" => match key {
			"href" | "target" | "download" | "rel" | "hreflang" | "type" | "referrerpolicy" => True
			_ => False
		}
		"linearGradient" => match key {
			"x1" | "y1" | "x2" | "y2" | "gradientUnits" | "gradientTransform" | "spreadMethod" | "href" => True
			_ => False
		}
		"radialGradient" => match key {
			"cx" | "cy" | "r" | "fx" | "fy" | "fr" | "gradientUnits" | "gradientTransform" | "spreadMethod" | "href" => True
			_ => False
		}
		"stop" => match key {
			"offset" => True
			_ => False
		}
		"pattern" => match key {
			"x" | "y" | "width" | "height" | "patternUnits" | "patternContentUnits" | "patternTransform" | "viewBox" | "preserveAspectRatio" | "href" => True
			_ => False
		}
		"marker" => match key {
			"markerWidth" | "markerHeight" | "markerUnits" | "refX" | "refY" | "orient" | "viewBox" | "preserveAspectRatio" => True
			_ => False
		}
		"mask" => match key {
			"x" | "y" | "width" | "height" | "maskUnits" | "maskContentUnits" | "mask-type" => True
			_ => False
		}
		"clipPath" => match key {
			"clipPathUnits" => True
			_ => False
		}
		"symbol" => match key {
			"x" | "y" | "width" | "height" | "refX" | "refY" | "viewBox" | "preserveAspectRatio" => True
			_ => False
		}
		"filter" => match key {
			"x" | "y" | "width" | "height" | "filterUnits" | "primitiveUnits" | "href" | "filterRes" => True
			_ => False
		}
		"feBlend" => match key {
			"in" | "in2" | "mode" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feColorMatrix" => match key {
			"in" | "type" | "values" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feComponentTransfer" => match key {
			"in" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feFuncA" | "feFuncB" | "feFuncG" | "feFuncR" => match key {
			"type" | "tableValues" | "slope" | "intercept" | "amplitude" | "exponent" | "offset" => True
			_ => False
		}
		"feComposite" => match key {
			"in" | "in2" | "operator" | "k1" | "k2" | "k3" | "k4" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feConvolveMatrix" => match key {
			"in" | "order" | "kernelMatrix" | "divisor" | "bias" | "targetX" | "targetY" | "edgeMode" | "kernelUnitLength" | "preserveAlpha" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feDiffuseLighting" => match key {
			"in" | "surfaceScale" | "diffuseConstant" | "kernelUnitLength" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feSpecularLighting" => match key {
			"in" | "surfaceScale" | "specularConstant" | "specularExponent" | "kernelUnitLength" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feDistantLight" => match key {
			"azimuth" | "elevation" => True
			_ => False
		}
		"fePointLight" => match key {
			"x" | "y" | "z" => True
			_ => False
		}
		"feSpotLight" => match key {
			"x" | "y" | "z" | "pointsAtX" | "pointsAtY" | "pointsAtZ" | "specularExponent" | "limitingConeAngle" => True
			_ => False
		}
		"feDisplacementMap" => match key {
			"in" | "in2" | "scale" | "xChannelSelector" | "yChannelSelector" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feDropShadow" => match key {
			"in" | "dx" | "dy" | "stdDeviation" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feFlood" | "feMerge" | "feTile" => match key {
			"in" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feGaussianBlur" => match key {
			"in" | "stdDeviation" | "edgeMode" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feImage" => match key {
			"href" | "preserveAspectRatio" | "crossorigin" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feMergeNode" => match key {
			"in" => True
			_ => False
		}
		"feMorphology" => match key {
			"in" | "operator" | "radius" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feOffset" => match key {
			"in" | "dx" | "dy" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"feTurbulence" => match key {
			"baseFrequency" | "numOctaves" | "seed" | "stitchTiles" | "type" | "x" | "y" | "width" | "height" | "result" => True
			_ => False
		}
		"animate" | "set" | "animateTransform" | "animateMotion" => match key {
			"attributeName" | "attributeType" | "begin" | "dur" | "end" | "min" | "max" | "restart" | "repeatCount" | "repeatDur" | "fill" | "calcMode" | "values" | "keyTimes" | "keySplines" | "from" | "to" | "by" | "additive" | "accumulate" | "href" | "type" | "path" | "keyPoints" | "rotate" | "origin" => True
			_ => False
		}
		"mpath" => match key {
			"href" => True
			_ => False
		}
		"discard" => match key {
			"href" | "begin" => True
			_ => False
		}
		"script" => match key {
			"href" | "type" | "crossorigin" => True
			_ => False
		}
		"style" => match key {
			"type" | "media" | "title" => True
			_ => False
		}
		_ => False
	}
}

math_attribute : Str, Str -> Bool
math_attribute = |element, key| {
	match element {
		"math" => match key {
			"display" | "overflow" | "altimg" | "altimg-width" | "altimg-height" | "altimg-valign" | "alttext" | "cdgroup" => True
			_ => False
		}
		"mo" => match key {
			"form" | "fence" | "separator" | "stretchy" | "symmetric" | "maxsize" | "minsize" | "largeop" | "movablelimits" | "accent" | "lspace" | "rspace" | "linebreak" | "linebreakstyle" | "lineleading" | "indentalign" | "indentshift" => True
			_ => False
		}
		"mi" => match key {
			"mathvariant" => True
			_ => False
		}
		"ms" => match key {
			"lquote" | "rquote" => True
			_ => False
		}
		"mspace" => match key {
			"width" | "height" | "depth" | "linebreak" => True
			_ => False
		}
		"mpadded" => match key {
			"width" | "height" | "depth" | "lspace" | "voffset" => True
			_ => False
		}
		"mfrac" => match key {
			"linethickness" | "numalign" | "denomalign" | "bevelled" => True
			_ => False
		}
		"msub" | "msup" | "msubsup" | "mmultiscripts" => match key {
			"subscriptshift" | "superscriptshift" => True
			_ => False
		}
		"munder" => match key {
			"accentunder" | "align" => True
			_ => False
		}
		"mover" => match key {
			"accent" | "align" => True
			_ => False
		}
		"munderover" => match key {
			"accent" | "accentunder" | "align" => True
			_ => False
		}
		"mtable" => match key {
			"align" | "rowalign" | "columnalign" | "groupalign" | "alignmentscope" | "columnwidth" | "width" | "rowspacing" | "columnspacing" | "rowlines" | "columnlines" | "frame" | "framespacing" | "equalrows" | "equalcolumns" | "displaystyle" | "side" | "minlabelspacing" => True
			_ => False
		}
		"mtr" | "mlabeledtr" => match key {
			"rowalign" | "columnalign" | "groupalign" => True
			_ => False
		}
		"mtd" => match key {
			"rowspan" | "columnspan" | "rowalign" | "columnalign" | "groupalign" => True
			_ => False
		}
		"menclose" => match key {
			"notation" => True
			_ => False
		}
		"mfenced" => match key {
			"open" | "close" | "separators" => True
			_ => False
		}
		"maction" => match key {
			"actiontype" | "selection" => True
			_ => False
		}
		"annotation" | "annotation-xml" => match key {
			"encoding" | "cd" | "name" | "src" | "definitionURL" => True
			_ => False
		}
		"mstyle" => match key {
			"infixlinebreakstyle" | "decimalpoint" | "scriptsizemultiplier" | "scriptminsize" => True
			_ => False
		}
		_ => False
	}
}

value_grammar : Bool, Str, Str -> Markup.ValueGrammar
value_grammar = |svg, element, attr| {
	if svg {
		match attr {
			"d" | "path" if member(["path", "hatchpath", "animateMotion"], element) => PathData
			"viewBox" => NumberList(4)
			"transform" | "gradientTransform" | "patternTransform" => TransformList
			"preserveAspectRatio" => AspectRatio
			"pathLength" => NonnegativeNumber
			"stroke-dasharray" => Language("dasharray")
			"stroke-miterlimit" => Language("miterlimit")
			"fill" if member(["animate", "set", "animateTransform", "animateMotion"], element) => Keyword(["remove", "freeze"])
			"stdDeviation" | "baseFrequency" | "radius" => Language("nonnegative-number-pair")
			"numOctaves" | "targetX" | "targetY" => Language("nonnegative-integer")
			"seed" | "scale" | "bias" | "divisor" | "surfaceScale" | "diffuseConstant" | "specularConstant" | "specularExponent" | "limitingConeAngle" | "azimuth" | "elevation" | "k1" | "k2" | "k3" | "k4" | "amplitude" | "exponent" | "intercept" | "slope" => Number
			"operator" if element == "feComposite" => Keyword(["over", "in", "out", "atop", "xor", "arithmetic", "lighter"])
			"operator" if element == "feMorphology" => Keyword(["erode", "dilate"])
			"edgeMode" => Keyword(["duplicate", "wrap", "none"])
			"preserveAlpha" => Keyword(["true", "false"])
			"xChannelSelector" | "yChannelSelector" => Keyword(["R", "G", "B", "A"])
			"type" if element == "feColorMatrix" => Keyword(["matrix", "saturate", "hueRotate", "luminanceToAlpha"])
			"type" if element == "feTurbulence" => Keyword(["fractalNoise", "turbulence"])
			"type" if member(["feFuncA", "feFuncR", "feFuncG", "feFuncB"], element) => Keyword(["identity", "table", "discrete", "linear", "gamma"])
			"stitchTiles" => Keyword(["stitch", "noStitch"])
			"points" => Language("coordinate-pairs")
			"x" | "y" | "dx" | "dy" if member(["text", "tspan"], element) => LengthList
			"width" | "height" => Language("auto-or-nonnegative-length")
			"r" | "rx" | "ry" | "markerWidth" | "markerHeight" | "stroke-width" => NonnegativeLength
			"x" | "y" | "cx" | "cy" | "x1" | "x2" | "y1" | "y2" | "fx" | "fy" | "fr" | "dx" | "dy" | "textLength" | "stroke-dashoffset" => Length
			"opacity" | "fill-opacity" | "stroke-opacity" | "stop-opacity" | "offset" => Language("number-or-percentage")
			"rotate" if member(["text", "tspan"], element) => Language("number-list")
			"gradientUnits" | "patternUnits" | "patternContentUnits" | "maskUnits" | "maskContentUnits" | "clipPathUnits" | "filterUnits" | "primitiveUnits" => Keyword(["userSpaceOnUse", "objectBoundingBox"])
			"spreadMethod" => Keyword(["pad", "reflect", "repeat"])
			"fill-rule" | "clip-rule" => Keyword(["nonzero", "evenodd", "inherit"])
			"stroke-linecap" => Keyword(["butt", "round", "square", "inherit"])
			"stroke-linejoin" => Keyword(["miter", "miter-clip", "round", "bevel", "arcs", "inherit"])
			"lengthAdjust" => Keyword(["spacing", "spacingAndGlyphs"])
			"text-anchor" => Keyword(["start", "middle", "end", "inherit"])
			"fill" | "stroke" if !member(["animate", "set", "animateTransform", "animateMotion"], element) => Paint
			_ => Language("opaque")
		}
	} else {
		match attr {
			"display" => Keyword(["block", "inline"])
			"dir" => Keyword(["ltr", "rtl"])
			"displaystyle" | "stretchy" | "symmetric" | "largeop" | "movablelimits" | "fence" | "separator" | "accent" | "accentunder" | "bevelled" | "equalrows" | "equalcolumns" => Keyword(["true", "false"])
			"form" => Keyword(["prefix", "infix", "postfix"])
			"mathvariant" => Keyword(["normal", "bold", "italic", "bold-italic", "double-struck", "bold-fraktur", "script", "bold-script", "fraktur", "sans-serif", "bold-sans-serif", "sans-serif-italic", "sans-serif-bold-italic", "monospace", "initial", "tailed", "looped", "stretched"])
			"columnspan" | "rowspan" | "selection" => Language("positive-integer")
			"scriptlevel" => Language("integer")
			"linethickness" => Language("nonnegative-math-length")
			"minsize" | "maxsize" => Language("math-length")
			"mathsize" | "width" | "height" | "depth" | "lspace" | "rspace" | "voffset" | "subscriptshift" | "superscriptshift" => Language("math-length")
			_ => Language("opaque")
		}
	}
}

valid_value : Str, Markup.ValueGrammar -> Bool
valid_value = |text, grammar| {
	match grammar {
		Number => MarkupValues.scalar(text).is_ok()
		NonnegativeNumber => MarkupValues.scalar(text).map_ok(|v| v >= 0) == Ok(True)
		Length => MarkupValues.length(text).is_ok()
		NonnegativeLength => MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		NumberList(count) => MarkupValues.numbers(text).map_ok(|values| values.len() == count and (count != 4 or ((values.get(2) ?? -1) >= 0 and (values.get(3) ?? -1) >= 0))) == Ok(True)
		LengthList => !Markup.whitespace(text) and words_of(text).all(|v| MarkupValues.length(v).is_ok())
		PathData => text == "none" or SvgPath.validate(text).is_ok()
		Paint => valid_paint(text)
		Color => valid_color(text)
		TransformList => text == "none" or MarkupValues.transforms(text).is_ok()
		Keyword(values) => member(values, text)
		AspectRatio => {
			parts = words_of(text)
			align = parts.first() ?? ""
			parts.len() >= 1 and parts.len() <= 2 and member(["none", "xMinYMin", "xMidYMin", "xMaxYMin", "xMinYMid", "xMidYMid", "xMaxYMid", "xMinYMax", "xMidYMax", "xMaxYMax"], align) and (parts.len() == 1 or [Ok("meet"), Ok("slice")].contains(parts.get(1)))
		}
		Language("auto-or-nonnegative-length") => text == "auto" or MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		Language("integer") => I64.from_str(text).is_ok()
		Language("nonnegative-integer") => U64.from_str(text).is_ok()
		Language("nonnegative-number-pair") => MarkupValues.numbers(text).map_ok(|v| v.len() >= 1 and v.len() <= 2 and v.all(|n| n >= 0)) == Ok(True)
		Language("miterlimit") => MarkupValues.scalar(text).map_ok(|v| v >= 1) == Ok(True)
		Language("dasharray") => text == "none" or (!words_of(text).is_empty() and words_of(text).all(|v| MarkupValues.length(v).map_ok(|n| n.value >= 0) == Ok(True)))
		Language("nonnegative-math-length") => member(["thin", "medium", "thick"], text) or MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		Language("positive-integer") => U64.from_str(text).map_ok(|v| v > 0) == Ok(True)
		Language("coordinate-pairs") => MarkupValues.numbers(text).map_ok(|v| v.len() % 2 == 0) == Ok(True)
		Language("number-list") => MarkupValues.numbers(text).is_ok()
		Language("number-or-percentage") => MarkupValues.scalar(text.drop_suffix("%")).is_ok()
		Language("math-length") => member(["thin", "medium", "thick", "normal", "small", "big", "veryverythinmathspace", "verythinmathspace", "thinmathspace", "mediummathspace", "thickmathspace", "verythickmathspace", "veryverythickmathspace", "infinity"], text) or MarkupValues.length(text).is_ok()
		_ => True
	}
}

check_math_children : Markup.Parsed, Markup.Node, Str -> Try({}, [InvalidMarkup(Markup.Error)])
check_math_children = |parsed, node, name| {
	var $elements = []
	var $count = 0
	needs_names = name == "mmultiscripts" or name == "semantics" or name == "mtable" or name == "mtr" or name == "mlabeledtr"
	text_allowed = name == "mi" or name == "mn" or name == "mo" or name == "ms" or name == "mtext" or name == "annotation"
	var $child_index = 0
	while $child_index < node.children.len() {
		index = node.children.get($child_index) ?? 0
		$child_index = $child_index + 1
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported("invalid child index"))
		match child.kind {
			Element(n) => {
				$count = $count + 1
				if needs_names {
					$elements = $elements.append(n.local)
				}
			}
			Text(text) if !text_allowed and !Markup.whitespace(text) => return fail(parsed, node, InvalidChildren({ element: name, expected: "MathML elements" }))
			_ => {}
		}
	}
	count = $count
	expected = match name {
		"mfrac" | "mroot" | "msub" | "msup" | "munder" | "mover" => 2
		"msubsup" | "munderover" => 3
		"mspace" | "mprescripts" | "none" => 0
		_ => count
	}
	if count != expected {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "${expected.to_str()} child elements" }))
	}
	if name == "mmultiscripts" {
		if count == 0 or $elements.first() == Ok("mprescripts") {
			return fail(parsed, node, InvalidChildren({ element: name, expected: "base followed by script pairs" }))
		}
		var $post = 0
		var $pre = 0
		var $prescripts = False
		for child in $elements.drop_first(1) {
			if child == "mprescripts" {
				if $prescripts or $post % 2 != 0 {
					return fail(parsed, node, InvalidChildren({ element: name, expected: "paired scripts and at most one mprescripts" }))
				}
				$prescripts = True
			} else if $prescripts {
				$pre = $pre + 1
			} else {
				$post = $post + 1
			}
		}
		if $post % 2 != 0 or $pre % 2 != 0 {
			return fail(parsed, node, InvalidChildren({ element: name, expected: "paired scripts" }))
		}
	}
	if name == "semantics" and (count == 0 or [Ok("annotation"), Ok("annotation-xml")].contains($elements.first()) or !$elements.drop_first(1).all(|e| member(["annotation", "annotation-xml"], e))) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "one expression followed by annotations" }))
	}
	if name == "mtable" and !$elements.all(|e| member(["mtr", "mlabeledtr"], e)) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "mtr or mlabeledtr" }))
	}
	if member(["mtr", "mlabeledtr"], name) and !$elements.all(|e| e == "mtd") {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "mtd" }))
	}
	if member(["mi", "mn", "mo", "ms", "mspace", "none", "mprescripts"], name) and count != 0 {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "text only" }))
	}
	Ok({})
}

fail : Markup.Parsed, Markup.Node, Markup.Problem -> Try(a, [InvalidMarkup(Markup.Error)])
fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(span) => span.start
		Cloned(copy) => copy.source.start
		Implied(at) => at
	}
	at = Markup.position(parsed.source, offset)
	Err(InvalidMarkup({ problem, line: at.line, column: at.column }))
}

attr_fail : Markup.Parsed, Markup.Attribute, Markup.Problem -> Try(a, [InvalidMarkup(Markup.Error)])
attr_fail = |parsed, attr, problem| {
	at = Markup.position(parsed.source, attr.span.start)
	Err(InvalidMarkup({ problem, line: at.line, column: at.column }))
}

words_of : Str -> List(Str)
words_of = |text| (Str.from_utf8(text.to_utf8().map(|b| if ([44, 9, 10, 13].contains(b)) 32 else b)) ?? "").split_on(" ").keep_if(|v| !v.is_empty())

check_svg_children : Markup.Parsed, Markup.Node, Str -> Try({}, [InvalidMarkup(Markup.Error)])
check_svg_children = |parsed, node, name| {
	if node.children.is_empty() {
		return Ok({})
	}
	(model, reject_text) = svg_child_model(name)
	var $child_index = 0
	while $child_index < node.children.len() {
		index = node.children.get($child_index) ?? 0
		$child_index = $child_index + 1
		if index >= parsed.tree.nodes.len() {
			return fail(parsed, node, Unsupported("invalid child index"))
		}
		# Unrestricted parents need only the arena-index check. Their children
		# are validated independently by the outer vocabulary traversal.
		if model != Any {
			child = parsed.tree.nodes.get(index) ?? node
			match child.kind {
				Element(n) if !svg_child_allowed(model, n.local) => return fail(parsed, child, InvalidChildren({ element: name, expected: "supported SVG content model" }))
				Text(text) if reject_text and !Markup.whitespace(text) => return fail(parsed, child, InvalidChildren({ element: name, expected: "SVG elements" }))
				_ => {}
			}
		}
	}
	Ok({})
}

svg_child_model = |name| match name {
	"path" | "rect" | "circle" | "ellipse" | "line" | "polygon" | "polyline" | "stop" => (Shape, True)
	"image" | "use" => (Shape, False)
	"linearGradient" | "radialGradient" => (Gradient, True)
	"filter" => (Filter, True)
	"feComponentTransfer" => (ComponentTransfer, False)
	"feMerge" => (Merge, False)
	"feDiffuseLighting" | "feSpecularLighting" => (Lighting, False)
	"text" | "tspan" | "textPath" => (Textual, False)
	_ => (Any, False)
}

svg_child_allowed = |model, child| {
	if match child {
		"title" | "desc" | "metadata" => True
		_ => False
	} {
		return True
	}
	match model {
		Shape => svg_animation_child(child)
		Gradient => child == "stop" or svg_animation_child(child)
		Filter => child.starts_with("fe") or svg_animation_child(child)
		ComponentTransfer => match child {
			"feFuncA" | "feFuncB" | "feFuncG" | "feFuncR" => True
			_ => False
		}
		Merge => child == "feMergeNode"
		Lighting => match child {
			"feDistantLight" | "fePointLight" | "feSpotLight" => True
			_ => False
		}
		Textual => (
			match child {
				"a" | "tspan" | "textPath" => True
				_ => False
			}
		) or svg_animation_child(child)
		Any => True
	}
}

svg_animation_child = |child| match child {
	"animate" | "animateMotion" | "animateTransform" | "set" | "discard" => True
	_ => False
}

valid_paint : Str -> Bool
valid_paint = |text| {
	value = MarkupValues.trim_space(text)
	if member(["none", "context-fill", "context-stroke"], value) {
		return True
	}
	if value.starts_with("url(") {
		match value.split_first(")") {
			Ok({ before, after }) => before.count_utf8_bytes() > 4 and (MarkupValues.trim_space(after).is_empty() or valid_color(MarkupValues.trim_space(after)) or MarkupValues.trim_space(after) == "none")
			Err(_) => False
		}
	} else {
		valid_color(value)
	}
}

valid_color : Str -> Bool
valid_color = |text| {
	value = MarkupValues.trim_space(text).with_ascii_lowercased()
	if color_names(value) or member(["currentcolor", "transparent", "inherit", "initial", "unset", "revert", "revert-layer"], value) {
		return True
	}
	if value.starts_with("#") {
		hex = value.to_utf8().drop_first(1)
		return [3, 4, 6, 8].contains(hex.len()) and hex.all(|b| (b >= '0' and b <= '9') or (b >= 'a' and b <= 'f'))
	}
	# Embedded CSS functions remain CSS; check their lexical envelope here,
	# leaving CSS evaluation and complete CSS syntax validation to consumers.
	match value.split_first("(") {
		Ok({ before, after }) => {
			if !member(["rgb", "rgba", "hsl", "hsla", "hwb", "lab", "lch", "oklab", "oklch", "color", "color-mix", "light-dark", "var"], before) or !after.ends_with(")") {
				return False
			}
			var $depth = 1.I64
			for b in after.to_utf8() {
				if b == '(' {
					$depth = $depth + 1
				}
				if b == ')' {
					$depth = $depth - 1
				}
				if $depth < 0 {
					return False
				}
			}
			$depth == 0 and after.count_utf8_bytes() > 1
		}
		Err(_) => False
	}
}

color_names = |key| match key {
	"aliceblue" | "antiquewhite" | "aqua" | "aquamarine" | "azure" | "beige" | "bisque" | "black" | "blanchedalmond" | "blue" | "blueviolet" | "brown" | "burlywood" | "cadetblue" | "chartreuse" | "chocolate" | "coral" | "cornflowerblue" | "cornsilk" | "crimson" | "cyan" | "darkblue" | "darkcyan" | "darkgoldenrod" | "darkgray" | "darkgreen" | "darkgrey" | "darkkhaki" | "darkmagenta" | "darkolivegreen" | "darkorange" | "darkorchid" | "darkred" | "darksalmon" | "darkseagreen" | "darkslateblue" | "darkslategray" | "darkslategrey" | "darkturquoise" | "darkviolet" | "deeppink" | "deepskyblue" | "dimgray" | "dimgrey" | "dodgerblue" | "firebrick" | "floralwhite" | "forestgreen" | "fuchsia" | "gainsboro" | "ghostwhite" | "gold" | "goldenrod" | "gray" | "green" | "greenyellow" | "grey" | "honeydew" | "hotpink" | "indianred" | "indigo" | "ivory" | "khaki" | "lavender" | "lavenderblush" | "lawngreen" | "lemonchiffon" | "lightblue" | "lightcoral" | "lightcyan" | "lightgoldenrodyellow" | "lightgray" | "lightgreen" | "lightgrey" | "lightpink" | "lightsalmon" | "lightseagreen" | "lightskyblue" | "lightslategray" | "lightslategrey" | "lightsteelblue" | "lightyellow" | "lime" | "limegreen" | "linen" | "magenta" | "maroon" | "mediumaquamarine" | "mediumblue" | "mediumorchid" | "mediumpurple" | "mediumseagreen" | "mediumslateblue" | "mediumspringgreen" | "mediumturquoise" | "mediumvioletred" | "midnightblue" | "mintcream" | "mistyrose" | "moccasin" | "navajowhite" | "navy" | "oldlace" | "olive" | "olivedrab" | "orange" | "orangered" | "orchid" | "palegoldenrod" | "palegreen" | "paleturquoise" | "palevioletred" | "papayawhip" | "peachpuff" | "peru" | "pink" | "plum" | "powderblue" | "purple" | "rebeccapurple" | "red" | "rosybrown" | "royalblue" | "saddlebrown" | "salmon" | "sandybrown" | "seagreen" | "seashell" | "sienna" | "silver" | "skyblue" | "slateblue" | "slategray" | "slategrey" | "snow" | "springgreen" | "steelblue" | "tan" | "teal" | "thistle" | "tomato" | "turquoise" | "violet" | "wheat" | "white" | "whitesmoke" | "yellow" | "yellowgreen" => True
	_ => False
}

aria_attributes = MarkupVocabulary.aria_name

ascii_lower : Str -> Str
ascii_lower = |text| text.with_ascii_lowercased()

# Fixed vocabulary scans use an index so a membership check creates no iterator
# state. Dynamic per-element vocabularies above use direct predicates.
member : List(Str), Str -> Bool
member = |items, item| {
	var $index = 0
	while $index < items.len() {
		if items.get($index) == Ok(item) {
			return True
		}
		$index = $index + 1
	}
	False
}

expect {
	var $passed = True
	for input in ["<g><path/></g>", "<path><title>x</title><animate/></path>", "<linearGradient><stop/></linearGradient>", "<filter><feGaussianBlur/></filter>", "<feComponentTransfer><feFuncA/></feComponentTransfer>", "<feMerge><feMergeNode/></feMerge>", "<feDiffuseLighting><fePointLight/></feDiffuseLighting>", "<text><tspan>x</tspan></text>", "<image>text</image>"] {
		parsed = Markup.parse_xml_fragment(input, Markup.svg_namespace, {})?
		$passed = $passed and MarkupValidate.foreign(parsed, {}, []).is_ok()
	}
	for input in ["<path><rect/></path>", "<linearGradient><rect/></linearGradient>", "<filter><rect/></filter>", "<feComponentTransfer><animate/></feComponentTransfer>", "<feMerge><animate/></feMerge>", "<feDiffuseLighting><feFuncA/></feDiffuseLighting>", "<text><rect/></text>", "<path>text</path>", "<linearGradient>text</linearGradient>", "<filter>text</filter>"] {
		parsed = Markup.parse_xml_fragment(input, Markup.svg_namespace, {})?
		$passed = $passed and MarkupValidate.foreign(parsed, {}, []).is_err()
	}
	$passed
}

expect {
	parsed = Markup.parse_xml_fragment("<g><path/></g>", Markup.svg_namespace, {})?
	group = parsed.tree.nodes.first()?
	nodes = parsed.tree.nodes.set(0, { ..group, children: [999] })?
	invalid = { ..parsed, tree: { ..parsed.tree, nodes } }
	MarkupValidate.foreign(invalid, {}, []).map_ok(|_| {}) == Err(InvalidMarkup({ problem: Unsupported("invalid child index"), line: 1, column: 1 }))
}
