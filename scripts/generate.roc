#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}
import weaver.Cli
import cli.OsStr
import cli.Path
import cli.Stdout
import cli.Stderr
import src/CommandLine
import src/Vocabulary
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
	parsed : Try({ schema_version : U64, tables : Dict(Str, List(Str)) }, _)
	parsed = Json.parse(Path.read_utf8!("scripts/markup/vocabulary.json")?)
	document = parsed.map_err(|_| InvalidVocabularyDocument)?
	if document.schema_version != 1 {
		return Err(UnsupportedSchema(document.schema_version))
	}
	if document.tables.len() != 4 {
		return Err(InvalidVocabularyDocument)
	}
	tables = { html: document.tables.get("html").map_err(|_| InvalidVocabularyDocument)?, svg: document.tables.get("svg").map_err(|_| InvalidVocabularyDocument)?, mathml: document.tables.get("mathml").map_err(|_| InvalidVocabularyDocument)?, aria: document.tables.get("aria").map_err(|_| InvalidVocabularyDocument)? }
	text = Vocabulary.render(tables)?
	output = Path.utf8("package/MarkupNameTables.roc")
	if args.check {
		if Path.read_utf8!(output)? != text {
			return Err(StaleGeneratedSource(Path.display(output)))
		}
	} else {
		Path.write_utf8!(output, text)?
	}
	Ok({})
}
