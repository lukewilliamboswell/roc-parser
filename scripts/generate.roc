#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}
import cli.OsStr
import cli.Path
import cli.Stderr
import src/Arguments
import src/Vocabulary
main! = |raw| {
	match generate!(raw.map(OsStr.display)) {
		Ok({}) => Ok({})
		Err(error) => {
			Stderr.line!(Str.inspect(error))?
			Err(Exit(1))
		}
	}
}

generate! = |raw| {
	args = Arguments.parse(raw, ["--check"], [])?
	if args.positional != ["vocabulary"] {
		return Err(UnknownGenerator(args.positional))
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
	if args.flags.contains("--check") {
		if Path.read_utf8!(output)? != text {
			return Err(StaleGeneratedSource(Path.display(output)))
		}
	} else {
		Path.write_utf8!(output, text)?
	}
	Ok({})
}
