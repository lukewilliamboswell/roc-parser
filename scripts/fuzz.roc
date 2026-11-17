#!/usr/bin/env roc-stable
app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.25.0/CZsY7tYZwR3rj9kYbpaCfxki2yVAaRL8bBwMLvB2xkbA.tar.zst",
	roc: "nightly-2026-10-09-258ab27",
}
import cli.Cmd
import cli.Env
import cli.OsStr
import cli.Path
import cli.Stderr
import cli.Stdout
import src/Arguments
import src/FuzzPlan
import src/Process

main! = |raw| {
	match run!(raw.map(OsStr.display)) {
		Ok({}) => Ok({})
		Err(error) => {
			Stderr.line!(Str.inspect(error))?
			Err(Exit(1))
		}
	}
}

run! = |raw| {
	args = Arguments.parse(raw, ["--ci", "--no-build"], ["--roc", "--runs", "--seconds", "--timeout", "--max-input-size", "--memory-limit", "--corpus-dir", "--jobs"])?
	mode = args.positional.first() ?? "list"
	if !["list", "describe", "prepare", "build", "smoke", "campaign", "show", "replay", "minimize"].contains(mode) {
		return Err(UnknownMode(mode))
	}
	artifact = ["show", "replay", "minimize"].contains(mode)
	names = args.positional.drop_first(1)
	if artifact and names.len() != (if mode == "minimize" 3 else 2) {
		return Err(InvalidArtifactArguments(mode))
	}
	chosen = FuzzPlan.select(if artifact names.take_first(1) else names, args.flags.contains("--ci"))?
	jobs = Arguments.positive(args, "--jobs", 1)?
	runs = Arguments.positive(args, "--runs", 2000)?
	seconds = Arguments.positive(args, "--seconds", 600)?
	memory = Arguments.positive(args, "--memory-limit", 2048)?
	_ = Arguments.positive(args, "--timeout", 5)?
	_ = Arguments.positive(args, "--max-input-size", 4096)?
	if mode == "describe" {
		Stdout.line!(Json.to_str(chosen)).map_err(|_| OutputUnavailable)?
		return Ok({})
	}
	if mode == "list" {
		for target in chosen {
			Stdout.line!("${target.name}: ${if target.scheduled "CI" else "local"}").map_err(|_| OutputUnavailable)?
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
	roc = Arguments.value(args, "--roc", Env.var_str!("ROC") ?? "roc")
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
	if jobs > 1 and !args.flags.contains("--no-build") and chosen.len() > 1 {
		var $remaining = chosen
		while !$remaining.is_empty() {
			batch = $remaining.take_first(jobs)
			$remaining = $remaining.drop_first(jobs)
			var $workers = []
			for target in batch {
				child = Cmd.new("scripts/fuzz.roc").args(["build", target.name, "--roc", compiler].map(OsStr.from_str)).stdout(Inherit).stderr(Inherit).manage_tree(True).timeout_ms(600_000).spawn!().map_err(|error| BuildUnavailable(error))?
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
		$execution_args = { ..args, flags: args.flags.append("--no-build") }
	}
	var $failed = []
	for target in chosen {
		binary = Path.join(root, ".roc-parser-tmp/fuzz/bin/${target.name}")
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
	if args.flags.contains("--no-build") {
		actual = Process.digest!(Path.read_bytes!(binary)?)?
		saved = Path.read_utf8!(receipt)?
		if saved != "${source_hash}\n${actual}\n" {
			return Err(StaleBinary(target.name))
		}
	} else {
		_ = Process.run!([compiler, "build", "--no-cache", "--opt=speed", "--fuzz", "fuzz/${target.name}.roc", "--output=${Path.display(binary)}"], root, [], 600_000)?
		actual = Process.digest!(Path.read_bytes!(binary)?)?
		Path.write_utf8!(receipt, "${source_hash}\n${actual}\n")?
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
		timeout = Arguments.positive(args, "--timeout", target.timeout)?
		size = Arguments.positive(args, "--max-input-size", target.size)?
		if target.examples {
			_ = Process.run!([Path.display(binary), "check", "--timeout=${timeout.to_str()}"], work, [], 600_000)?
		}
		$options = ["explore", Path.display(corpus), "--max-input-size=${size.to_str()}", "--timeout=${timeout.to_str()}", "--memory-limit=${memory.to_str()}", if mode == "smoke" "--runs=${runs.to_str()}" else "--time=${seconds.to_str()}"]
		if target.dictionary != "" {
			$options = $options.append("--dictionary=${Path.display(Path.join(root, target.dictionary))}")
		}
	}
	Path.write_utf8!(Path.join(work, "command.json"), Json.to_str([Path.display(binary)].concat($options)))?
	output = Process.run!([Path.display(binary)].concat($options), work, [], if mode == "campaign" (seconds + 120) * 1000 else 600_000)?
	Stdout.line!(Str.from_utf8_lossy(output.stdout_bytes.concat(output.stderr_bytes))).map_err(|_| OutputUnavailable)?
	Ok({})
}

prepare! = |target, args, root| {
	corpus = Path.absolute!(Path.utf8(Arguments.value(args, "--corpus-dir", "${Path.display(root)}/.roc-parser-tmp/fuzz/corpus/${target.name}")))?
	Path.create_all!(corpus)?
	if target.seeds != "" {
		parsed : Try(List(Str), _)
		parsed = Json.parse(Path.read_utf8!(Path.utf8(target.seeds))?)
		seeds = parsed.map_err(|_| InvalidSeeds(target.seeds))?
		for seed in seeds {
			bytes = FuzzPlan.encode(seed, target.encoding)?
			hash = Process.digest!(bytes)?
			Path.write_bytes!(Path.join(corpus, "seed-${hash}"), bytes)?
		}
	}
	Ok(corpus)
}
