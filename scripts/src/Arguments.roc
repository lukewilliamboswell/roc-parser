## Parse options before performing any effects. Values remain source strings.
Arguments := [].{
	Parsed : { positional : List(Str), flags : List(Str), values : List((Str, Str)) }
	parse : List(Str), List(Str), List(Str) -> Try(Parsed, [UnknownOption(Str), MissingValue(Str), RepeatedOption(Str)])
	parse = |args, switches, options| {
		var $result = { positional: [], flags: [], values: [] }
		var $rest = args
		while !$rest.is_empty() {
			arg = $rest.first() ?? ""
			$rest = $rest.drop_first(1)
			if arg == "--" {
				$result = { ..$result, positional: $result.positional.concat($rest) }
				break
			}
			if switches.contains(arg) {
				if $result.flags.contains(arg) {
					return Err(RepeatedOption(arg))
				}
				$result = { ..$result, flags: $result.flags.append(arg) }
			} else if options.contains(arg) {
				if $result.values.any(|pair| pair.0 == arg) {
					return Err(RepeatedOption(arg))
				}
				value = $rest.first().map_err(|_| MissingValue(arg))?
				if value.starts_with("--") {
					return Err(MissingValue(arg))
				}
				$rest = $rest.drop_first(1)
				$result = { ..$result, values: $result.values.append((arg, value)) }
			} else if arg.starts_with("--") {
				return Err(UnknownOption(arg))
			} else {
				$result = { ..$result, positional: $result.positional.append(arg) }
			}
		}
		Ok($result)
	}
	value : Parsed, Str, Str -> Str
	value = |args, name, default| args.values.find_first(|pair| pair.0 == name).map_ok(|pair| pair.1) ?? default
	positive : Parsed, Str, U64 -> Try(U64, [InvalidPositiveInteger({ option : Str, value : Str })])
	positive = |args, name, default| {
		text = value(args, name, default.to_str())
		parsed = U64.from_str(text).map_err(|_| InvalidPositiveInteger({ option: name, value: text }))?
		if parsed == 0 {
			Err(InvalidPositiveInteger({ option: name, value: text }))
		} else {
			Ok(parsed)
		}
	}
}

expect Arguments.parse(["--jobs"], [], ["--jobs"]) == Err(MissingValue("--jobs"))
expect Arguments.parse(["--bad"], [], []) == Err(UnknownOption("--bad"))
expect {
	var $passed = True
	for n in List.repeat({}, 2000).map_with_index(|_, index| index + 1) {
		parsed = Arguments.parse(["smoke", "--jobs", n.to_str(), "xml"], [], ["--jobs"]) ?? crash "valid options rejected"
		$passed = $passed and Arguments.positive(parsed, "--jobs", 1) == Ok(n) and parsed.positional == ["smoke", "xml"]
	}
	$passed
}
