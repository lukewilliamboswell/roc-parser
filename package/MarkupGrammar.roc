## Shared typed value-language expectations.
MarkupGrammar :: [].{
	ValueLanguage : [HTMLAttribute, Attribute, AutoOrNonnegativeLength, CoordinatePairs, Dasharray, ExtensionAttribute, Integer, MathLength, Miterlimit, NamespacedAttribute, NonnegativeInteger, NonnegativeMathLength, NonnegativeNumberPair, NumberList, NumberOrPercentage, Opaque, PositiveInteger, Text]
	ValueGrammar : [Number, NonnegativeNumber, Length, NonnegativeLength, NumberList(U64), LengthList, PathData, TransformList, Keyword(List(Str)), TokenList, Color, Paint, AspectRatio, Language(ValueLanguage)]
}
