import MarkupValues
import Literal

## Parsed SVG path data. Every command is explicit, including repeated groups
## and implicit line-to groups following a move. Relative case is preserved.
## This validates syntax and arc flags/radii; it does not calculate geometry.
SvgPath := { commands : List(SvgPath.Command) }.{

	## Compare parsed commands and their authored offsets.
	is_eq : _

	## One command and its parameters; offset points to its authored command/group.
	Command : { command : U8, arguments : List(F64), offset : U64 }

	## Structural path-data failures at a UTF-8 byte offset.
	Problem : [MoveRequired, UnknownCommand(U8), ExpectedNumber, ExpectedFlag, NegativeRadius, IncompleteArguments({ command : U8, expected : U64, actual : U64 }), UnexpectedComma]

	## A classified grammar error and its byte offset.
	Error : { problem : Problem, offset : U64 }

	## Parse a complete SVG path, accepting empty path data.
	parse_str : Str -> Try(SvgPath, [InvalidPath(Error)])
	parse_str = |text| parse_commands(text, True).map_ok(|commands| SvgPath.{ commands })

	## Check the same grammar without retaining parsed commands or arguments.
	validate : Str -> Try({}, [InvalidPath(Error)])
	validate = |text| parse_commands(text, False).map_ok(|_| {})

	## Check path data during quotation conversion.
	from_quote : Str -> Try(SvgPath, [BadQuotedBytes(Str)])
	from_quote = |source| parse_str(source).map_err(
		|InvalidPath(error)| {
			at = Literal.position(source, error.offset)
			Literal.error({ format: "SVG path", message: Str.inspect(error.problem), line: at.line, column: at.column })
		},
	)
}

parse_commands : Str, Bool -> Try(List(SvgPath.Command), [InvalidPath(SvgPath.Error)])
parse_commands = |text, retain| {
	bytes = text.to_utf8()
	var $pos = MarkupValues.whitespace(bytes, 0)
	var $commands = []
	var $first = True
	var $command = 0.U8
	while $pos < bytes.len() {
		start = $pos
		byte = bytes.get($pos) ?? 0
		if letter(byte) {
			if !command_letter(byte) {
				return Err(InvalidPath({ problem: UnknownCommand(byte), offset: $pos }))
			}
			$command = byte
			$pos = $pos + 1
		} else if $command == 0 {
			return Err(InvalidPath({ problem: MoveRequired, offset: $pos }))
		}
		if $first and $command != 'M' and $command != 'm' {
			return Err(InvalidPath({ problem: MoveRequired, offset: start }))
		}
		$first = False
		count = arity($command)
		if count == 0 {
			if retain {
				$commands = $commands.append({ command: $command, arguments: [], offset: start })
			}
			$command = 0
			$pos = MarkupValues.whitespace(bytes, $pos)
		} else {
			var $args = []
			var $i = 0
			while $i < count {
				$pos = MarkupValues.whitespace(bytes, $pos)
				if $i > 0 and bytes.get($pos) == Ok(',') {
					$pos = MarkupValues.whitespace(bytes, $pos + 1)
				}
				if bytes.get($pos) == Ok(',') {
					return Err(InvalidPath({ problem: UnexpectedComma, offset: $pos }))
				}
				if $pos >= bytes.len() or letter(bytes.get($pos) ?? 0) {
					return Err(InvalidPath({ problem: IncompleteArguments({ command: $command, expected: count, actual: $i }), offset: $pos }))
				}
				if ($command == 'A' or $command == 'a') and ($i == 3 or $i == 4) {
					flag = bytes.get($pos) ?? 0
					if flag != '0' and flag != '1' {
						return Err(InvalidPath({ problem: ExpectedFlag, offset: $pos }))
					}
					if retain {
						$args = $args.append(if (flag == '1') 1 else 0)
					}
					$pos = $pos + 1
				} else {
					parsed = MarkupValues.number(bytes, $pos).map_err(|InvalidNumber(offset)| InvalidPath({ problem: ExpectedNumber, offset }))?
					if ($command == 'A' or $command == 'a') and $i < 2 and parsed.value < 0 {
						return Err(InvalidPath({ problem: NegativeRadius, offset: $pos }))
					}
					$args = if retain {
						$args.append(parsed.value)
					} else $args
					$pos = parsed.end
				}
				$i = $i + 1
			}
			if retain {
				$commands = $commands.append({ command: $command, arguments: $args, offset: start })
			}
			if $command == 'M' {
				$command = 'L'
			}
			if $command == 'm' {
				$command = 'l'
			}
			$pos = MarkupValues.whitespace(bytes, $pos)
			if bytes.get($pos) == Ok(',') {
				$pos = MarkupValues.whitespace(bytes, $pos + 1)
				if $pos >= bytes.len() or letter(bytes.get($pos) ?? 0) {
					return Err(InvalidPath({ problem: UnexpectedComma, offset: $pos }))
				}
			}
		}
	}
	Ok($commands)
}

letter : U8 -> Bool
letter = |b| (b >= 'a' and b <= 'z') or (b >= 'A' and b <= 'Z')

arity : U8 -> U64
arity = |c| match c {
	'M' | 'm' | 'L' | 'l' | 'T' | 't' => 2
	'H' | 'h' | 'V' | 'v' => 1
	'C' | 'c' => 6
	'S' | 's' | 'Q' | 'q' => 4
	'A' | 'a' => 7
	_ => 0
}

expect SvgPath.parse_str("") == Ok(SvgPath.{ commands: [] })
expect SvgPath.parse_str("M0 0L10-10.5Z").map_ok(|p| p.commands.len()) == Ok(3)
expect SvgPath.parse_str("m1,2 3,4").map_ok(|p| p.commands.map(|c| c.command)) == Ok(['m', 'l'])
expect SvgPath.parse_str("M0 0A5 6 30 0110 20").is_ok()
expect SvgPath.parse_str("M0 0C1 2 3 4 5 6S7 8 9 10Q1 2 3 4T5 6H1V2z").is_ok()
expect SvgPath.parse_str("L0 0") == Err(InvalidPath({ problem: MoveRequired, offset: 0 }))
expect SvgPath.parse_str("M0 0A5 6 0 2 0 1 2").is_err()
expect SvgPath.parse_str("M0 0A-5 6 0 0 0 1 2").is_err()
expect SvgPath.parse_str("M0 0,").is_err()
expect SvgPath.parse_str("M,0 0").is_err()
expect SvgPath.parse_str("M0 0L1e 2").is_err()
expect SvgPath.parse_str("M0 0Z1 2").is_err()
expect SvgPath.parse_str("M0 0L1").is_err()

expect SvgPath.parse_str("M1. 2").is_err()
expect SvgPath.parse_str("M1\u(000C)2").is_ok()

expect {
	[
		"",
		"M0 0L10-10.5Z",
		"m1,2 3,4",
		"M0 0A5 6 30 0110 20",
		"L0 0",
		"M0 0A5 6 0 2 0 1 2",
		"M0 0A-5 6 0 0 0 1 2",
		"M0 0,",
		"M,0 0",
		"M0 0L1e 2",
		"M0 0Z1 2",
		"M0 0L1",
		"M1. 2",
		"M1\u(000C)2",
		"M1e999 2",
		"M0 0X1 2",
		"M0 0A1 2 3 0 1",
	].all(|source| SvgPath.validate(source) == SvgPath.parse_str(source).map_ok(|_| {}))
}

command_letter : U8 -> Bool
command_letter = |byte| match byte {
	'M' | 'm' | 'Z' | 'z' | 'L' | 'l' | 'H' | 'h' | 'V' | 'v' | 'C' | 'c' | 'S' | 's' | 'Q' | 'q' | 'T' | 't' | 'A' | 'a' => True
	_ => False
}
