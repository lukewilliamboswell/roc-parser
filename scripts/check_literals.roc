#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "nightly-2026-11-18-a520eed",
	weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}
import cli.Duration
import cli.Cmd
import cli.Env
import weaver.Cli
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/CommandLine
import src/LiteralContract
import "literals/cases.json" as fixture_json : Str

fixtures : List({ name : Str, body : Str, errors : List(Str), assets : Dict(Str, Str) })
fixtures = Json.parse(fixture_json) ?? crash "scripts/literals/cases.json: invalid literal fixtures"

main! = |raw| match Cli.parse_or_display_message(CommandLine.literals, raw, OsStr.to_raw) {
	Err(Help(message)) | Err(Version(message)) => Stdout.line!(message)
	Err(InvalidUsage(message)) => {
		Stderr.line!(message)?
		Err(Exit(2))
	}
	Ok(args) => match run!(args) {
		Ok({}) => Ok({})
		Err(error) => {
			Stderr.line!(Str.inspect(error))?
			Err(Exit(1))
		}
	}
}

run! = |args| {
	filter = args.filter
	root = Path.canonicalize!(".")?
	compiler = if (args.roc == "") (Env.var_str!("ROC") ?? "roc") else args.roc
	roc = if (compiler.contains("/")) Path.display(Path.canonicalize!(Path.utf8(compiler))?) else compiler
	cases = fixtures.keep_if(|case| case.name.contains(filter))
	invalidation = "cache-invalidation".contains(filter)
	if cases.is_empty() and !invalidation {
		return Err(NoMatchingContracts(filter))
	}
	scratch = Path.join(root, ".roc-parser-tmp/literals")
	Path.create_all!(scratch)?
	work = Env.create_temp_dir_in!(scratch, "run-")?
	var $failed = []
	for case in cases {
		result = check!(case, roc, root, Path.join(work, case.name))
		match result {
			Ok({}) => Stdout.line!("PASS ${case.name}").map_err(|_| OutputUnavailable)?
			Err(error) => {
				Stderr.line!("${case.name}: ${Str.inspect(error)}").map_err(|_| OutputUnavailable)?
				$failed = $failed.append(case.name)
			}
		}
	}
	if invalidation {
		match invalidate!(roc, root, Path.join(work, "cache-invalidation")) {
			Ok({}) => Stdout.line!("PASS cache-invalidation").map_err(|_| OutputUnavailable)?
			Err(error) => {
				Stderr.line!(Str.inspect(error)).map_err(|_| OutputUnavailable)?
				$failed = $failed.append("cache-invalidation")
			}
		}
	}
	if !$failed.is_empty() {
		return Err(FailedContracts({ names: $failed, evidence: Path.display(work) }))
	}
	Path.delete_all!(work)?
	Ok({})
}

source = |body, root| {
	var $imports = "import cli.Stdout\n"
	for name in ["Parser", "Yaml", "Xml", "CSV", "QIF", "OFX", "Markdown", "AsciiDoc", "HTTP", "RocSyntax", "JsonValue", "Html", "Svg", "MathML", "Markup"] {
		$imports = "${$imports}import parser.${name}\n"
	}
	"app [main!] { cli: platform \"https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst\", parser: ${Json.to_str("${Path.display(root)}/package/main.roc")} }\n${$imports}\n${body}\nmain! = |_args| Stdout.line!(Str.inspect(value))\n"
}

invoke! = |roc, args, directory, errors| {
	output = Cmd.new(OsStr.from_str(roc)).args(args.map(OsStr.from_str)).cwd(directory).stdout(Capture).stderr(Capture).timeout(Duration.millis(180_000)).manage_tree(True).run!().map_err(|error| CompilerUnavailable(error))?
	text = Str.from_utf8_lossy(output.stdout_bytes.concat(output.stderr_bytes))
	code = match output.status {
		Exited(value) => value
		Signaled(signal) => return Err(CompilerSignaled(signal))
	}
	Path.write_utf8!(Path.join(directory, "last-compiler-output.txt"), text).map_err(|_| EvidenceUnavailable)?
	LiteralContract.check(code, text, errors)?
	Ok(output)
}

check! = |case, roc, root, directory| {
	Path.create_all!(directory)?
	for (name, content) in case.assets.to_list() {
		# The reviewed fixture's assets are relative; refuse traversal before writing.
		if name.starts_with("/") or name.contains("..") {
			return Err(InvalidAssetPath(name))
		}
		path = Path.join(directory, name)
		parent = Path.utf8(Str.join_with(Path.display(path).split_on("/").drop_last(1), "/"))
		Path.create_all!(parent)?
		Path.write_utf8!(path, content)?
	}
	Path.write_utf8!(Path.join(directory, "main.roc"), source(case.body, root))?
	binary = Path.join(directory, "app")
	optimization = "dev"
	for command in [["check", "main.roc"], ["build", "--opt=${optimization}", "--output=${Path.display(binary)}", "main.roc"]] {
		_ = invoke!(roc, command, directory, case.errors)?
	}
	if case.errors.is_empty() {
		for (name, _) in case.assets.to_list() {
			Path.delete!(Path.join(directory, name))?
		}
		output = invoke!(Path.display(binary), [], directory, [])?
		if output.stdout_bytes != "True\n".to_utf8() {
			return Err(IncorrectValue(output.stdout_bytes))
		}
	}
	Ok({})
}

invalidate! = |roc, root, directory| {
	Path.create_all!(directory)?
	body = "import \"settings.yaml\" as text : Str\nvalue = Parser.Quotation.require(Yaml.validate({ source: text, name: \"settings.yaml\" }))"
	Path.write_utf8!(Path.join(directory, "main.roc"), source(body, root))?
	binary = Path.join(directory, "cached")
	for (content, errors) in [("port: 8080\n", []), ("port: [\n", ["compile time crash", "settings.yaml", "YAML"]), ("port: [\n", ["compile time crash", "settings.yaml", "YAML"]), ("port: 9090\n", [])] {
		Path.write_utf8!(Path.join(directory, "settings.yaml"), content)?
		for command in [["check", "main.roc"], ["build", "--opt=dev", "--output=${Path.display(binary)}", "main.roc"]] {
			_ = invoke!(roc, command, directory, errors)?
		}
	}
	Path.delete!(Path.join(directory, "settings.yaml"))?
	output = invoke!(Path.display(binary), [], directory, [])?
	text = Str.from_utf8_lossy(output.stdout_bytes)
	if !text.contains("9090") or text.contains("8080") {
		return Err(StaleImportedValue(text))
	}
	Ok({})
}
