import Markup
import MarkupValues
import SvgPath

## SVG and Presentation MathML authoring checks shared by the three frontends.
## These checks do not execute or validate embedded CSS and JavaScript.
MarkupValidate :: [].{

	## Whole attribute-value holes whose future values are not yet available.
	Hole : { node : U64, attribute : U64 }

	## Validate foreign vocabularies throughout a mixed-namespace tree.
	foreign : Markup.Parsed, Markup.Props, List(Hole) -> Try(Markup.Parsed, [InvalidMarkup(Markup.Error)])
	foreign = |parsed, props, holes| {
		var $pending = parsed.tree.roots.map(|index| { index, extension: False })
		while !$pending.is_empty() {
			item = $pending.last() ?? { index: 0, extension: False }
			$pending = $pending.drop_last(1)
			node = parsed.tree.nodes.get(item.index) ?? return Err(InvalidMarkup({ problem: Unsupported("invalid tree index"), line: 1, column: 1 }))
			match node.kind {
				Element(name) => {
					svg = name.namespace == Uri(Markup.svg_namespace)
					mathml = name.namespace == Uri(Markup.mathml_namespace)
					if !item.extension {
						known = if (svg) svg_elements.contains(name.local) else if (mathml) math_elements.contains(name.local) else True
						if !known and !props.allow_unknown_elements {
							return fail(parsed, node, UnknownElement(name.local))
						}
						var $attr_index = 0
						for attr in node.attributes {
							if svg or mathml {
								allowed = allowed_attribute(svg, name.local, attr.name)
								if !allowed and !props.allow_unknown_attributes {
									return attr_fail(parsed, attr, UnknownAttribute({ element: name.local, attribute: attr.name.local }))
								}
								if props.check_values and allowed and !holes.contains({ node: item.index, attribute: $attr_index }) {
									grammar = value_grammar(svg, name.local, attr.name.local)
									if !valid_value(attr.value, grammar) {
										return attr_fail(parsed, attr, InvalidValue({ attribute: attr.name.local, value: attr.value, expected: grammar }))
									}
								}
							}
							$attr_index = $attr_index + 1
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
		Ok(parsed)
	}

	## The typed value grammar applied to a known attribute.
	grammar : Bool, Str, Str -> Markup.ValueGrammar
	grammar = value_grammar
}

svg_elements = "a animate animateMotion animateTransform circle clipPath defs desc discard ellipse feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence filter foreignObject g hatch hatchpath image line linearGradient marker mask mesh meshgradient meshpatch meshrow metadata mpath path pattern polygon polyline radialGradient rect script set solidcolor stop style svg switch symbol text textPath title tspan use view".split_on(" ")

math_elements = "annotation annotation-xml maction math menclose merror mfenced mfrac mi mlabeledtr mmultiscripts mn mo mover mpadded mphantom mprescripts mroot mrow ms mspace msqrt mstyle msub msubsup msup mtable mtd mtext mtr munder munderover none semantics".split_on(" ")

global_attributes = "id class style lang tabindex autofocus role title".split_on(" ")

svg_presentation = "alignment-baseline baseline-shift clip clip-path clip-rule color color-interpolation color-interpolation-filters color-rendering cursor direction display dominant-baseline fill fill-opacity fill-rule filter flood-color flood-opacity font-family font-size font-size-adjust font-stretch font-style font-variant font-weight glyph-orientation-horizontal glyph-orientation-vertical image-rendering isolation letter-spacing lighting-color marker-end marker-mid marker-start mask mix-blend-mode opacity overflow paint-order pointer-events shape-rendering stop-color stop-opacity stroke stroke-dasharray stroke-dashoffset stroke-linecap stroke-linejoin stroke-miterlimit stroke-opacity stroke-width text-anchor text-decoration text-rendering transform transform-box transform-origin unicode-bidi vector-effect visibility white-space word-spacing writing-mode".split_on(" ")

svg_events = "onabort onactivate onbegin onclick onend onerror onfocusin onfocusout onload onmousedown onmousemove onmouseout onmouseover onmouseup onrepeat onresize onscroll onunload onzoom onkeydown onkeyup onpointerdown onpointermove onpointerup onpointerenter onpointerleave ontouchstart ontouchend".split_on(" ")

math_global = "dir displaystyle scriptlevel mathbackground mathcolor mathsize mathvariant href".split_on(" ")

allowed_attribute : Bool, Str, Markup.Name -> Bool
allowed_attribute = |svg, element, name| {
	match name.namespace {
		Uri("http://www.w3.org/2000/xmlns/") => True
		Uri("http://www.w3.org/XML/1998/namespace") => ["lang", "space", "base", "id"].contains(name.local)
		Uri("http://www.w3.org/1999/xlink") => svg and ["href", "type", "role", "arcrole", "title", "show", "actuate"].contains(name.local)
		NoNamespace => {
			key = name.local
			global_attributes.contains(key) or key.starts_with("data-") or key.starts_with("aria-") or (if (svg) svg_presentation.contains(key) or svg_events.contains(key) or svg_attributes(element).contains(key) else math_global.contains(key) or math_attributes(element).contains(key))
		}
		_ => False
	}
}

svg_attributes : Str -> List(Str)
svg_attributes = |element| {
	text = match element {
		"svg" => "x y width height viewBox preserveAspectRatio version baseProfile contentScriptType contentStyleType zoomAndPan"
		"view" => "viewBox preserveAspectRatio viewTarget zoomAndPan"
		"path" | "hatchpath" => "d pathLength"
		"rect" => "x y width height rx ry pathLength"
		"circle" => "cx cy r pathLength"
		"ellipse" => "cx cy rx ry pathLength"
		"line" => "x1 y1 x2 y2 pathLength"
		"polygon" | "polyline" => "points pathLength"
		"text" | "tspan" => "x y dx dy rotate textLength lengthAdjust"
		"textPath" => "href startOffset method spacing side textLength lengthAdjust"
		"use" | "image" | "foreignObject" => "x y width height href preserveAspectRatio crossorigin decoding"
		"a" => "href target download rel hreflang type referrerpolicy"
		"linearGradient" => "x1 y1 x2 y2 gradientUnits gradientTransform spreadMethod href"
		"radialGradient" => "cx cy r fx fy fr gradientUnits gradientTransform spreadMethod href"
		"stop" => "offset"
		"pattern" => "x y width height patternUnits patternContentUnits patternTransform viewBox preserveAspectRatio href"
		"marker" => "markerWidth markerHeight markerUnits refX refY orient viewBox preserveAspectRatio"
		"mask" => "x y width height maskUnits maskContentUnits mask-type"
		"clipPath" => "clipPathUnits"
		"symbol" => "x y width height refX refY viewBox preserveAspectRatio"
		"filter" => "x y width height filterUnits primitiveUnits href filterRes"
		"feBlend" => "in in2 mode x y width height result"
		"feColorMatrix" => "in type values x y width height result"
		"feComponentTransfer" => "in x y width height result"
		"feFuncA" | "feFuncB" | "feFuncG" | "feFuncR" => "type tableValues slope intercept amplitude exponent offset"
		"feComposite" => "in in2 operator k1 k2 k3 k4 x y width height result"
		"feConvolveMatrix" => "in order kernelMatrix divisor bias targetX targetY edgeMode kernelUnitLength preserveAlpha x y width height result"
		"feDiffuseLighting" => "in surfaceScale diffuseConstant kernelUnitLength x y width height result"
		"feSpecularLighting" => "in surfaceScale specularConstant specularExponent kernelUnitLength x y width height result"
		"feDistantLight" => "azimuth elevation"
		"fePointLight" => "x y z"
		"feSpotLight" => "x y z pointsAtX pointsAtY pointsAtZ specularExponent limitingConeAngle"
		"feDisplacementMap" => "in in2 scale xChannelSelector yChannelSelector x y width height result"
		"feDropShadow" => "in dx dy stdDeviation x y width height result"
		"feFlood" | "feMerge" | "feTile" => "in x y width height result"
		"feGaussianBlur" => "in stdDeviation edgeMode x y width height result"
		"feImage" => "href preserveAspectRatio crossorigin x y width height result"
		"feMergeNode" => "in"
		"feMorphology" => "in operator radius x y width height result"
		"feOffset" => "in dx dy x y width height result"
		"feTurbulence" => "baseFrequency numOctaves seed stitchTiles type x y width height result"
		"animate" | "set" | "animateTransform" | "animateMotion" => "attributeName attributeType begin dur end min max restart repeatCount repeatDur fill calcMode values keyTimes keySplines from to by additive accumulate href type path keyPoints rotate origin"
		"mpath" => "href"
		"discard" => "href begin"
		"script" => "href type crossorigin"
		"style" => "type media title"
		"switch" | "g" | "defs" | "desc" | "title" | "metadata" => ""
		_ => ""
	}
	text.split_on(" ").concat(["requiredExtensions", "systemLanguage"])
}

math_attributes : Str -> List(Str)
math_attributes = |element| {
	text = match element {
		"math" => "display overflow altimg altimg-width altimg-height altimg-valign alttext cdgroup"
		"mo" => "form fence separator stretchy symmetric maxsize minsize largeop movablelimits accent lspace rspace linebreak linebreakstyle lineleading indentalign indentshift"
		"mi" => "mathvariant"
		"ms" => "lquote rquote"
		"mspace" => "width height depth linebreak"
		"mpadded" => "width height depth lspace voffset"
		"mfrac" => "linethickness numalign denomalign bevelled"
		"msub" | "msup" | "msubsup" | "mmultiscripts" => "subscriptshift superscriptshift"
		"munder" => "accentunder align"
		"mover" => "accent align"
		"munderover" => "accent accentunder align"
		"mtable" => "align rowalign columnalign groupalign alignmentscope columnwidth width rowspacing columnspacing rowlines columnlines frame framespacing equalrows equalcolumns displaystyle side minlabelspacing"
		"mtr" | "mlabeledtr" => "rowalign columnalign groupalign"
		"mtd" => "rowspan columnspan rowalign columnalign groupalign"
		"menclose" => "notation"
		"mfenced" => "open close separators"
		"maction" => "actiontype selection"
		"annotation" | "annotation-xml" => "encoding cd name src definitionURL"
		"mstyle" => "infixlinebreakstyle decimalpoint scriptsizemultiplier scriptminsize"
		_ => ""
	}
	text.split_on(" ")
}

value_grammar : Bool, Str, Str -> Markup.ValueGrammar
value_grammar = |svg, element, attr| {
	if svg {
		match attr {
			"d" | "path" if ["path", "hatchpath", "animateMotion"].contains(element) => PathData
			"viewBox" => NumberList(4)
			"transform" | "gradientTransform" | "patternTransform" => TransformList
			"preserveAspectRatio" => AspectRatio
			"pathLength" => NonnegativeNumber
			"points" => Language("coordinate-pairs")
			"x" | "y" | "dx" | "dy" if ["text", "tspan"].contains(element) => LengthList
			"r" | "rx" | "ry" | "width" | "height" | "markerWidth" | "markerHeight" => NonnegativeLength
			"x" | "y" | "cx" | "cy" | "x1" | "x2" | "y1" | "y2" | "fx" | "fy" | "fr" | "dx" | "dy" | "textLength" | "stroke-width" | "stroke-dashoffset" => Length
			"opacity" | "fill-opacity" | "stroke-opacity" | "stop-opacity" | "offset" => Language("number-or-percentage")
			"rotate" if ["text", "tspan"].contains(element) => Language("number-list")
			"gradientUnits" | "patternUnits" | "patternContentUnits" | "maskUnits" | "maskContentUnits" | "clipPathUnits" | "filterUnits" | "primitiveUnits" => Keyword(["userSpaceOnUse", "objectBoundingBox"])
			"spreadMethod" => Keyword(["pad", "reflect", "repeat"])
			"fill-rule" | "clip-rule" => Keyword(["nonzero", "evenodd", "inherit"])
			"stroke-linecap" => Keyword(["butt", "round", "square", "inherit"])
			"stroke-linejoin" => Keyword(["miter", "miter-clip", "round", "bevel", "arcs", "inherit"])
			"lengthAdjust" => Keyword(["spacing", "spacingAndGlyphs"])
			"text-anchor" => Keyword(["start", "middle", "end", "inherit"])
			"fill" | "stroke" if !["animate", "set", "animateTransform", "animateMotion"].contains(element) => Paint
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
			"mathsize" | "width" | "height" | "depth" | "lspace" | "rspace" | "voffset" | "linethickness" | "subscriptshift" | "superscriptshift" => Language("math-length")
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
		NonnegativeLength => text == "auto" or MarkupValues.length(text).map_ok(|v| v.value >= 0) == Ok(True)
		NumberList(count) => MarkupValues.numbers(text).map_ok(|values| values.len() == count and (count != 4 or ((values.get(2) ?? -1) >= 0 and (values.get(3) ?? -1) >= 0))) == Ok(True)
		LengthList => !text.trim().is_empty() and words_of(text).all(|v| MarkupValues.length(v).is_ok())
		PathData => SvgPath.parse_str(text).is_ok()
		TransformList => text == "none" or MarkupValues.transforms(text).is_ok()
		Keyword(values) => values.contains(text)
		AspectRatio => {
			parts = words_of(text)
			align = parts.first() ?? ""
			parts.len() >= 1 and parts.len() <= 2 and ["none", "xMinYMin", "xMidYMin", "xMaxYMin", "xMinYMid", "xMidYMid", "xMaxYMid", "xMinYMax", "xMidYMax", "xMaxYMax"].contains(align) and (parts.len() == 1 or [Ok("meet"), Ok("slice")].contains(parts.get(1)))
		}
		Language("positive-integer") => U64.from_str(text).map_ok(|v| v > 0) == Ok(True)
		Language("coordinate-pairs") => MarkupValues.numbers(text).map_ok(|v| v.len() % 2 == 0) == Ok(True)
		Language("number-list") => MarkupValues.numbers(text).is_ok()
		Language("number-or-percentage") => MarkupValues.scalar(text.drop_suffix("%")).is_ok()
		Language("math-length") => ["thin", "medium", "thick", "normal", "small", "big", "veryverythinmathspace", "verythinmathspace", "thinmathspace", "mediummathspace", "thickmathspace", "verythickmathspace", "veryverythickmathspace", "infinity"].contains(text) or MarkupValues.length(text).is_ok()
		_ => True
	}
}

check_math_children : Markup.Parsed, Markup.Node, Str -> Try({}, [InvalidMarkup(Markup.Error)])
check_math_children = |parsed, node, name| {
	var $elements = []
	for index in node.children {
		child = parsed.tree.nodes.get(index) ?? return fail(parsed, node, Unsupported("invalid child index"))
		match child.kind {
			Element(n) => {
				$elements = $elements.append(n.local)
			}
			Text(text) if !text.trim().is_empty() and !["mi", "mn", "mo", "ms", "mtext", "annotation"].contains(name) => return fail(parsed, node, InvalidChildren({ element: name, expected: "MathML elements" }))
			_ => {}
		}
	}
	count = $elements.len()
	expected = match name {
		"mfrac" | "mroot" | "msub" | "msup" | "munder" | "mover" => 2
		"msubsup" | "munderover" => 3
		"mspace" | "mprescripts" | "none" => 0
		_ => count
	}
	if count != expected {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "${expected.to_str()} child elements" }))
	}
	if name == "mtable" and !$elements.all(|e| ["mtr", "mlabeledtr"].contains(e)) {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "mtr or mlabeledtr" }))
	}
	if ["mtr", "mlabeledtr"].contains(name) and !$elements.all(|e| e == "mtd") {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "mtd" }))
	}
	if ["mi", "mn", "mo", "ms", "mspace", "none", "mprescripts"].contains(name) and count != 0 {
		return fail(parsed, node, InvalidChildren({ element: name, expected: "text only" }))
	}
	Ok({})
}

fail : Markup.Parsed, Markup.Node, Markup.Problem -> Try(a, [InvalidMarkup(Markup.Error)])
fail = |parsed, node, problem| {
	offset = match node.origin {
		Authored(span) => span.start
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
