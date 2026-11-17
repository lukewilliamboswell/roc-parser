import Diagnostic
# Private JSON configuration and problem vocabulary shared by both scanners.
JsonConfig :: [].{
	Limit : [RecordBytes, DocumentBytes, Nesting, ArrayItems, ObjectMembers, StringBytes, NumberCharacters]

	NumberProblem : [NotFinite, UnsafeInteger, SignificantDigits, DoubleOverflow, DoubleUnderflow]

	Problem : [InvalidNumber(Str), IncompleteUtf8, InvalidBom, ExpectedComment, ExpectedArray, ExpectedArrayItem, IncompleteArray, MismatchedDelimiter, LimitExceeded({ limit : Limit, max : U64, observed : Diagnostic.Observed }), TrailingComma, ExpectedArrayDelimiter, ExpectedObjectDelimiter, InvalidLiteral, ExpectedValue, UnexpectedContent, BomNotAllowed, TopLevelRequired([Object, ObjectOrArray]), ExpectedStringKey, ExpectedKeyColon, DuplicateKey(Str), ConflictingDuplicateKey(Str), CommentsNotAllowed, UnterminatedComment, InvalidUtf8, UnterminatedString, InvalidUnicodeEscape, UnpairedSurrogate, InvalidEscape, ControlCharacter(U32), LeadingZero, ExpectedMinusDigit, ExpectedFractionDigit, ExpectedExponentDigit, NonInteroperableNumber({ text : Str, reason : NumberProblem })]

	WriteProps := { sort_keys : Bool ?? False, indent : U64 ?? 0, max_bytes : U64 ?? U64.highest }

	Props := {
		max_bytes : U64 ?? U64.highest,
		max_depth : U64 ?? 128,
		max_items : U64 ?? U64.highest,
		max_string_bytes : U64 ?? U64.highest,
		max_number_digits : U64 ?? U64.highest,
		comments : Bool ?? False,
		trailing_commas : Bool ?? False,
		bom : Bool ?? False,
		non_finite : Bool ?? False,
		control_characters : Bool ?? False,
		lone_surrogates : [Reject, Replace] ?? Reject,
		duplicates : [Reject, CollapseIdentical, KeepFirst, KeepLast] ?? Reject,
		top_level : [Any, ObjectOrArray, Object] ?? Any,
		interoperable_numbers : Bool ?? False,
	}

	WriteError : [InvalidNumber(Str), DuplicateKey(Str), TooDeep, TooLarge(U64)]
}
