#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-25-d5a4f1e",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}
import weaver.Cli
import cli.OsStr
import cli.Path
import cli.Stdout
import cli.Stderr
import src/CommandLine
import src/Vocabulary
import "markup/vocabulary.json" as vocabulary_json : Str
main! = |raw| match Cli.parse_or_display_message(CommandLine.generate, raw, OsStr.to_raw) {
	Err(Help(message)) | Err(Version(message)) => Stdout.line!(message)
	Err(InvalidUsage(message)) => {
		Stderr.line!(message)?
		Err(Exit(2))
	}
	Ok(args) => match generate!(args) {
		Ok({}) => Ok({})
		Err(error) => {
			Stderr.line!(Str.inspect(error))?
			Err(Exit(1))
		}
	}
}

generate! = |args| {
	if args.generator != "vocabulary" {
		return Err(UnknownGenerator(args.generator))
	}
	output = Path.utf8("package/MarkupNameTables.roc")
	if args.check {
		if Path.read_utf8!(output)? != vocabulary_text {
			return Err(StaleGeneratedSource(Path.display(output)))
		}
	} else {
		Path.write_utf8!(output, vocabulary_text)?
	}
	Ok({})
}

## Validate and render the checked-in inventory while compiling the generator.
vocabulary_text : Str
vocabulary_text = render_vocabulary(vocabulary_json) ?? crash "scripts/markup/vocabulary.json: invalid vocabulary inventory"

render_vocabulary = |json| {
	parsed : Try({ schema_version : U64, tables : Dict(Str, List(Str)) }, _)
	parsed = Json.parse(json)
	document = parsed.map_err(|_| InvalidVocabularyDocument)?
	if document.schema_version != 1 {
		return Err(UnsupportedSchema(document.schema_version))
	}
	if document.tables.len() != 4 {
		return Err(InvalidVocabularyDocument)
	}
	tables = { html: document.tables.get("html").map_err(|_| InvalidVocabularyDocument)?, svg: document.tables.get("svg").map_err(|_| InvalidVocabularyDocument)?, mathml: document.tables.get("mathml").map_err(|_| InvalidVocabularyDocument)?, aria: document.tables.get("aria").map_err(|_| InvalidVocabularyDocument)? }
	Vocabulary.render(tables)
}
