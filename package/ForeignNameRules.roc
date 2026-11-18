import MarkupSyntaxFacts
import DomNameCore
import MarkupGrammar

## Shared primitive vocabulary rules used by names and validators.
ForeignNameRules :: [].{
	NameParts : { namespace : [Uri(Str), NoNamespace], local : Str, prefix : [Prefix(Str), Unprefixed] }
	svg_elements : _
	svg_elements = MarkupSyntaxFacts.svg_element_name
	math_elements : _
	math_elements = MarkupSyntaxFacts.mathml_element_name
	global_attributes : _
	global_attributes = |key| match key {
		"id" | "class" | "style" | "lang" | "tabindex" | "autofocus" | "role" | "title" => True
		_ => False
	}
	svg_presentation : _
	svg_presentation = |key| match key {
		"alignment-baseline" | "baseline-shift" | "clip" | "clip-path" | "clip-rule" | "color" | "color-interpolation" | "color-interpolation-filters" | "color-rendering" | "cursor" | "direction" | "display" | "dominant-baseline" | "fill" | "fill-opacity" | "fill-rule" | "filter" | "flood-color" | "flood-opacity" | "font-family" | "font-size" | "font-size-adjust" | "font-stretch" | "font-style" | "font-variant" | "font-weight" | "glyph-orientation-horizontal" | "glyph-orientation-vertical" | "image-rendering" | "isolation" | "letter-spacing" | "lighting-color" | "marker-end" | "marker-mid" | "marker-start" | "mask" | "mix-blend-mode" | "opacity" | "overflow" | "paint-order" | "pointer-events" | "shape-rendering" | "stop-color" | "stop-opacity" | "stroke" | "stroke-dasharray" | "stroke-dashoffset" | "stroke-linecap" | "stroke-linejoin" | "stroke-miterlimit" | "stroke-opacity" | "stroke-width" | "text-anchor" | "text-decoration" | "text-rendering" | "transform" | "transform-box" | "transform-origin" | "unicode-bidi" | "vector-effect" | "visibility" | "white-space" | "word-spacing" | "writing-mode" => True
		_ => False
	}
	svg_events : _
	svg_events = MarkupSyntaxFacts.svg_event_name
	math_global : _
	math_global = |key| match key {
		"dir" | "displaystyle" | "scriptlevel" | "mathbackground" | "mathcolor" | "mathsize" | "mathvariant" | "href" => True
		_ => False
	}
	allowed_attribute : Bool, Str, NameParts -> Bool
	allowed_attribute = |svg, element, name| {
		match name.namespace {
			Uri("http://www.w3.org/2000/xmlns/") => True
			Uri("http://www.w3.org/XML/1998/namespace") => member(["lang", "space", "base", "id"], name.local)
			Uri("http://www.w3.org/1999/xlink") => svg and member(["href", "type", "role", "arcrole", "title", "show", "actuate"], name.local)
			NoNamespace => {
				key = name.local
				(if (svg) svg_attribute(element, key) else math_attribute(element, key)) or global_attributes(key) or key.starts_with("data-") or aria_attributes(key) or (if (svg) svg_presentation(key) or svg_events(key) else math_global(key) or MarkupSyntaxFacts.global_event_name(key))
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
	value_grammar : Bool, Str, Str -> MarkupGrammar.ValueGrammar
	value_grammar = |svg, element, attr| {
		if svg {
			match attr {
				"d" | "path" if member(["path", "hatchpath", "animateMotion"], element) => PathData
				"viewBox" => NumberList(4)
				"transform" | "gradientTransform" | "patternTransform" => TransformList
				"preserveAspectRatio" => AspectRatio
				"pathLength" => NonnegativeNumber
				"stroke-dasharray" => Language(Dasharray)
				"stroke-miterlimit" => Language(Miterlimit)
				"fill" if member(["animate", "set", "animateTransform", "animateMotion"], element) => Keyword(["remove", "freeze"])
				"stdDeviation" | "baseFrequency" | "radius" => Language(NonnegativeNumberPair)
				"numOctaves" | "targetX" | "targetY" => Language(NonnegativeInteger)
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
				"points" => Language(CoordinatePairs)
				"x" | "y" | "dx" | "dy" if member(["text", "tspan"], element) => LengthList
				"width" | "height" => Language(AutoOrNonnegativeLength)
				"r" | "rx" | "ry" | "markerWidth" | "markerHeight" | "stroke-width" => NonnegativeLength
				"x" | "y" | "cx" | "cy" | "x1" | "x2" | "y1" | "y2" | "fx" | "fy" | "fr" | "dx" | "dy" | "textLength" | "stroke-dashoffset" => Length
				"opacity" | "fill-opacity" | "stroke-opacity" | "stop-opacity" | "offset" => Language(NumberOrPercentage)
				"rotate" if member(["text", "tspan"], element) => Language(NumberList)
				"gradientUnits" | "patternUnits" | "patternContentUnits" | "maskUnits" | "maskContentUnits" | "clipPathUnits" | "filterUnits" | "primitiveUnits" => Keyword(["userSpaceOnUse", "objectBoundingBox"])
				"spreadMethod" => Keyword(["pad", "reflect", "repeat"])
				"fill-rule" | "clip-rule" => Keyword(["nonzero", "evenodd", "inherit"])
				"stroke-linecap" => Keyword(["butt", "round", "square", "inherit"])
				"stroke-linejoin" => Keyword(["miter", "miter-clip", "round", "bevel", "arcs", "inherit"])
				"lengthAdjust" => Keyword(["spacing", "spacingAndGlyphs"])
				"text-anchor" => Keyword(["start", "middle", "end", "inherit"])
				"fill" | "stroke" if !member(["animate", "set", "animateTransform", "animateMotion"], element) => Paint
				_ => Language(Opaque)
			}
		} else {
			match attr {
				"display" => Keyword(["block", "inline"])
				"dir" => Keyword(["ltr", "rtl"])
				"displaystyle" | "stretchy" | "symmetric" | "largeop" | "movablelimits" | "fence" | "separator" | "accent" | "accentunder" | "bevelled" | "equalrows" | "equalcolumns" => Keyword(["true", "false"])
				"form" => Keyword(["prefix", "infix", "postfix"])
				"mathvariant" => Keyword(["normal", "bold", "italic", "bold-italic", "double-struck", "bold-fraktur", "script", "bold-script", "fraktur", "sans-serif", "bold-sans-serif", "sans-serif-italic", "sans-serif-bold-italic", "monospace", "initial", "tailed", "looped", "stretched"])
				"columnspan" | "rowspan" | "selection" => Language(PositiveInteger)
				"scriptlevel" => Language(Integer)
				"linethickness" => Language(NonnegativeMathLength)
				"minsize" | "maxsize" => Language(MathLength)
				"mathsize" | "width" | "height" | "depth" | "lspace" | "rspace" | "voffset" | "subscriptshift" | "superscriptshift" => Language(MathLength)
				_ => Language(Opaque)
			}
		}
	}
	aria_attributes : _
	aria_attributes = MarkupSyntaxFacts.aria_name
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
}
