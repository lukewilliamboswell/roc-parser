import cli.Env
import cli.Path
import cli.Cmd
import cli.OsStr
import cli.Stdout

## Keep failure identity separate from rendered command diagnostics.
Process := [].{
	run! = |command, cwd, input, timeout| {
		match command {
			[program, .. as args] => {
				Stdout.line!("+ ${Str.join_with(command, " ")}").map_err(|_| OutputUnavailable)?
				output = Cmd.new(OsStr.from_str(program)).args(args.map(OsStr.from_str)).cwd(cwd).stdin(Bytes(input)).stdout(Capture).stderr(Capture).manage_tree(True).timeout_ms(timeout).output_limit(50_000_000).run!().map_err(|error| ProcessUnavailable({ command, error }))?
				if output.status != Exited(0) {
					return Err(ProcessFailed({ command, status: output.status, stdout: Str.from_utf8_lossy(output.stdout_bytes), stderr: Str.from_utf8_lossy(output.stderr_bytes) }))
				}
				Ok(output)
			}
			[] => Err(EmptyCommand)
		}
	}
	resolve! = |command| {
		if command.contains("/") {
			return Path.canonicalize!(Path.utf8(command)).map_err(|_| MissingExecutable(command))
		}
		path = Env.var_str!("PATH") ?? ""
		for directory in path.split_on(":").keep_if(|part| part != "") {
			match Path.canonicalize!(Path.join(Path.utf8(directory), command)) {
				Ok(candidate) => if Path.is_file!(candidate) ?? False {
					return Ok(candidate)
				}
				Err(_) => {}
			}
		}
		Err(MissingExecutable(command))
	}
	digest! = |bytes| {
		output = run!(["openssl", "dgst", "-sha256"], ".", bytes, 30_000)?
		Str.from_utf8_lossy(output.stdout_bytes).trim().split_on(" ").last().map_err(|_| InvalidDigest)
	}
}
