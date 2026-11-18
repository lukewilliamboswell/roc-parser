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
import src/FuzzPlan
import src/Process

main! = |raw| match Cli.parse_or_display_message(CommandLine.fuzz, raw, OsStr.to_raw) {
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
	mode = args.mode
	if !["list", "describe", "prepare", "build", "smoke", "campaign", "show", "replay", "minimize"].contains(mode) {
		return Err(UnknownMode(mode))
	}
	artifact = ["show", "replay", "minimize"].contains(mode)
	names = args.arguments
	if artifact and names.len() != (if (mode == "minimize") 3 else 2) {
		return Err(InvalidArtifactArguments(mode))
	}
	chosen = FuzzPlan.select(if (artifact) names.take_first(1) else names, args.ci)?
	jobs = args.jobs
	runs = args.runs
	seconds = args.seconds
	memory = args.memory_limit
	if mode == "describe" {
		Stdout.line!(Json.to_str(chosen)).map_err(|_| OutputUnavailable)?
		return Ok({})
	}
	if mode == "list" {
		for target in chosen {
			Stdout.line!("${target.name}: ${if (target.scheduled) "CI" else "local"}").map_err(|_| OutputUnavailable)?
		}
		return Ok({})
	}
	root = Path.canonicalize!(".")?
	if mode == "prepare" {
		for target in chosen {
			_ = prepare!(target, args, root)?
		}
		return Ok({})
	}
	roc = if (args.roc == "") (Env.var_str!("ROC") ?? "roc") else args.roc
	# Resolve path-like compiler choices before changing the worker cwd.
	compiler_path = Process.resolve!(roc)?
	compiler = Path.display(compiler_path)
	source_files = Process.run!(["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z", "package", "fuzz"], root, [], 30_000)?
	var $source = []
	for file in Str.from_utf8_lossy(source_files.stdout_bytes).split_on("\u(0)").keep_if(|name| name.ends_with(".roc")) {
		$source = $source.concat(file.to_utf8()).append(0).concat(Path.read_bytes!(Path.utf8(file))?).append(0)
	}
	version = Process.run!([compiler, "version"], root, [], 30_000)?
	compiler_hash = Process.digest!(Path.read_bytes!(compiler_path)?)?
	source_hash = Process.digest!($source.concat(version.stdout_bytes).concat(compiler_hash.to_utf8()))?
	var $execution_args = args
	if jobs > 1 and !args.no_build and chosen.len() > 1 {
		var $remaining = shared_targets(chosen)
		while !$remaining.is_empty() {
			batch = $remaining.take_first(jobs)
			$remaining = $remaining.drop_first(jobs)
			var $workers = []
			for target in batch {
				child = Cmd.new("scripts/fuzz.roc").args(["build", target.name, "--roc", compiler].map(OsStr.from_str)).stdout(Inherit).stderr(Inherit).manage_tree(True).timeout(Duration.millis(600_000)).spawn!().map_err(|error| BuildUnavailable(error))?
				$workers = $workers.append(child)
			}
			var $bad = False
			for child in $workers {
				match child.wait!() {
					Ok(output) => if output.status != Exited(0) {
						$bad = True
					}
					Err(_) => {
						$bad = True
					}
				}
			}
			if $bad {
				return Err(ParallelBuildFailed(batch.map(|target| target.name)))
			}
		}
		$execution_args = { ..args, no_build: True }
	}
	var $failed = []
	for target in chosen {
		binary = Path.join(root, ".roc-parser-tmp/fuzz/bin/${target.family}")
		receipt = Path.utf8("${Path.display(binary)}.identity")
		Path.create_all!(Path.join(root, ".roc-parser-tmp/fuzz/bin"))?
		result = execute!(target, mode, $execution_args, names, compiler, root, binary, receipt, source_hash, runs, seconds, memory)
		match result {
			Ok({}) => {}
			Err(error) => {
				Stderr.line!(Str.inspect(error)).map_err(|_| OutputUnavailable)?
				$failed = $failed.append(target.name)
			}
		}
	}
	if $failed.is_empty() {
		Ok({})
	} else {
		Err(FailedTargets($failed))
	}
}

execute! = |target, mode, args, names, compiler, root, binary, receipt, source_hash, runs, seconds, memory| {
	flags = ["--fuzz"].concat(FuzzPlan.build_flags(target.family))
	signature = Json.to_str(flags)
	current = match (Path.read_bytes!(binary), Path.read_utf8!(receipt)) {
		(Ok(bytes), Ok(saved)) => saved == "${source_hash}\n${signature}\n${Process.digest!(bytes)?}\n"
		_ => False
	}
	if !current {
		if args.no_build {
			return Err(StaleBinary(target.name))
		}
		# Fuzz instrumentation requires LLVM; ordinary checks use dev builds.
		_ = Process.run!([compiler, "build"].concat(flags).concat(["fuzz/${target.family}Properties.roc", "--output=${Path.display(binary)}"]), root, [], 600_000)?
		actual = Process.digest!(Path.read_bytes!(binary)?)?
		Path.write_utf8!(receipt, "${source_hash}\n${signature}\n${actual}\n")?
	}
	if mode == "build" {
		return Ok({})
	}
	work = Path.join(root, ".roc-parser-tmp/fuzz/runs/${target.name}")
	Path.create_all!(work)?
	var $options = []
	if ["show", "replay", "minimize"].contains(mode) {
		input = Path.canonicalize!(Path.utf8(names.get(1) ?? ""))?
		$options = [mode, Path.display(input)]
		if mode == "minimize" {
			$options = $options.append(Path.display(Path.absolute!(Path.utf8(names.get(2) ?? ""))?))
		}
	} else {
		corpus = prepare!(target, args, root)?
		timeout = args.timeout ?? target.timeout
		size = args.max_input_size ?? target.size
		if target.examples {
			_ = Process.run!([Path.display(binary), "--target=${target.name}", "check", "--timeout=${timeout.to_str()}"], work, [], 600_000)?
		}
		$options = ["explore", Path.display(corpus), "--max-input-size=${size.to_str()}", "--timeout=${timeout.to_str()}", "--memory-limit=${memory.to_str()}", if (mode == "smoke") "--runs=${runs.to_str()}" else "--time=${seconds.to_str()}"]
		if target.dictionary != "" {
			$options = $options.append("--dictionary=${Path.display(Path.join(root, target.dictionary))}")
		}
	}
	command = [Path.display(binary), "--target=${target.name}"].concat($options)
	Path.write_utf8!(Path.join(work, "command.json"), Json.to_str(command))?
	output = Process.run!(command, work, [], if (mode == "campaign") (seconds + 120) * 1000 else 600_000)?
	Stdout.line!(Str.from_utf8_lossy(output.stdout_bytes.concat(output.stderr_bytes))).map_err(|_| OutputUnavailable)?
	Ok({})
}

## One build per shared executable, preserving the first selected property.
shared_targets = |selected| {
	var $shared = []
	for target in selected {
		if !$shared.any(|item| item.family == target.family) {
			$shared = $shared.append(target)
		}
	}
	$shared
}

expect shared_targets(FuzzPlan.select(["xml-stream", "xml-reader", "csv"], False) ?? []).map(|entry| entry.name) == ["xml-stream", "csv"]

prepare! = |target, args, root| {
	corpus = Path.absolute!(Path.utf8(if (args.corpus_dir == "") "${Path.display(root)}/.roc-parser-tmp/fuzz/corpus/${target.name}" else args.corpus_dir))?
	Path.create_all!(corpus)?
	if target.seeds != "" {
		seeds = FuzzPlan.seed_values(target.name)
		for seed in seeds {
			bytes = FuzzPlan.encode(seed, target.encoding)?
			hash = Process.digest!(bytes)?
			Path.write_bytes!(Path.join(corpus, "seed-${hash}"), bytes)?
		}
	}
	Ok(corpus)
}
