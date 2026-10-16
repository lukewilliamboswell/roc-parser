app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Monotonic
import parser.RocSyntax

## In-process optimized full parse with shared input. Input/output and the
## status's display string are outside timing. Consume child IDs as well as
## node/token spans so precedence topology remains observable.
main! : List(OsStr) => Try({}, _)
main! = |args| {
	iterations = match args.last() {
		Ok(arg) => U64.from_str(OsStr.display(arg)) ?? 1
		Err(_) => 1
	}
	input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
	var $checksum = 0.U64
	var $successes = 0.U64
	var $status = Complete
	var $i = 0
	start = Monotonic.now!()
	while $i < iterations {
		parsed = RocSyntax.parse_str(input)
		$checksum = $checksum + parsed.tokens.fold(0, |sum, token| sum + token.start + token.end + token_kind_code(token.kind))
			+ parsed.nodes.fold(0, |sum, node| sum + node.start + node.end + node.children_start + node.children_len + node_kind_code(node.kind))
			+ parsed.children.fold(0, |sum, child| (sum * 31 + child) % 1000000007)
		$status = parsed.status
		if parsed.status == Complete {
			$successes = $successes + 1
		}
		$i = $i + 1
	}
	elapsed = Monotonic.now!() - start
	Stdout.line!(Json.to_str({ iterations, elapsed_ns: elapsed, successes: $successes, checksum: $checksum, status: Str.inspect($status) }))?
	Ok({})
}

token_kind_code : RocSyntax.TokenKind -> U64
token_kind_code = |kind| match kind {
	Whitespace => 1
	Comment => 2
	LowerIdent => 3
	UpperIdent => 4
	Keyword => 5
	Number => 6
	StringStart => 7
	StringPart => 8
	StringEnd => 9
	SingleQuote => 10
	OpenRound => 11
	NoSpaceOpenRound => 12
	CloseRound => 13
	OpenSquare => 14
	CloseSquare => 15
	OpenCurly => 16
	CloseCurly => 17
	Operator => 18
	Punctuation => 19
	Invalid => 20
	EndOfFile => 21
	MultilineStringStart => 22
	InterpolationStart => 23
	InterpolationEnd => 24
}

node_kind_code : RocSyntax.NodeKind -> U64
node_kind_code = |kind| match kind {
	File => 1
	Declaration => 2
	ExpressionStatement => 3
	Lambda => 4
	If => 5
	Match => 6
	MatchBranch => 7
	Pattern => 8
	Block => 9
	Record => 10
	RecordField => 11
	Header => 12
	Import => 13
	TypeAnnotation => 14
	TypeExpression => 15
	Identifier => 16
	NumberLiteral => 17
	StringLiteral => 18
	CharacterLiteral => 19
	Unary => 20
	Binary => 21
	Call => 22
	FieldAccess => 23
	Suffix => 24
	ListLiteral => 25
	Tuple => 26
	Parenthesized => 27
	ErrorNode => 28
	UnsupportedNode => 29
	OptionalFieldAccess => 30
	Debug => 31
	Crash => 32
	Return => 33
	Break => 34
	Ellipsis => 35
	IdentifierPattern => 36
	WildcardPattern => 37
	MutablePattern => 38
	TagPattern => 39
	NominalPattern => 40
	TuplePattern => 41
	ListPattern => 42
	RecordPattern => 43
	PatternField => 44
	RestPattern => 45
	AliasPattern => 46
	AlternativePattern => 47
	VariableDeclaration => 48
	Expect => 49
	For => 50
	ForStream => 51
	While => 52
	MultilineStringLiteral => 53
	StringSegment => 54
	Interpolation => 55
	TypedLiteral => 56
	FileImport => 57
	ImportPath => 58
	ImportAlias => 59
	ExposedItem => 60
	ExposedWildcard => 61
	Dependencies => 62
	DependencyField => 63
	PlatformReference => 64
	PatternStringLiteral => 65
	CapturePattern => 66
	ArrowCall => 67
	MethodArrowCall => 68
	TypeName => 100
	TypeVariable => 101
	TypeWildcard => 102
	TypeApply => 103
	TypeFunction => 104
	TypeTuple => 105
	TypeRecord => 106
	TypeField => 107
	TypeTagUnion => 108
	TypeExtension => 109
	TypeParenthesized => 110
	TypeDeclaration => 111
	TypeHeader => 112
	WhereClause => 113
	WhereMethod => 114
	WhereAlias => 115
	AssociatedBlock => 116
}
