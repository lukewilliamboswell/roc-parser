import fuzz.Fuzz
import parser.Parser
import parser.Xml

test : Str -> Fuzz.Outcome
test = |input| {
	match Xml.parser({}).parse_str(input) {
		Ok(_) => Fuzz.keep
		Err(_) => Fuzz.keep
	}
}

property_target = Fuzz.target_with({
	examples: [],
	name: "xml",
	generator: Fuzz.str,
	test,
	show: |input| Str.inspect(input),
})

XmlProperty :: [].{
	target = property_target
}
