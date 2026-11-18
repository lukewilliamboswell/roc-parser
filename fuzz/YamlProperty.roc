import fuzz.Fuzz
import parser.Yaml

test : Str -> Fuzz.Outcome
test = |input| {
	match Yaml.parse_str(input, {}) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

property_target = Fuzz.target_with({
	examples: [],
	name: "yaml",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})

YamlProperty :: [].{
	target = property_target
}
