## The documentation contract: test explicit expects and compare exact stdout.
DocExamples := [].{
	needs_tests : Str -> Bool
	needs_tests = |source| source.split_on("\n").any(
		|line| {
			text = line.trim()
			text == "expect" or text.starts_with("expect ") or text.starts_with("expect\t")
		},
	)

	check_stdout : Str, Str -> Try({}, Str)
	check_stdout = |actual, expected| if actual == expected Ok({}) else Err("stdout differs from the adjacent .expected file\nexpected: ${Str.inspect(expected)}\nactual: ${Str.inspect(actual)}")
}

expect DocExamples.needs_tests("# no expects here\n\texpect answer == 42\n")
expect DocExamples.needs_tests("expect\tBool.True\n")
expect !DocExamples.needs_tests("# expect False\nexpected = 42\n")
expect DocExamples.check_stdout("answer\n", "answer\n") == Ok({})
expect DocExamples.check_stdout("answer", "answer\n").is_err()
