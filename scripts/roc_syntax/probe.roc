app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
	parser: "../../package/main.roc",
}

import cli.OsStr
import cli.Stdin
import cli.Stdout
import parser.RocSyntax

## Persistent JSON Lines worker. One request produces exactly one response.
Request : { id : Str, mode : Str, source : Str }

main! : List(OsStr) => Try({}, _)
main! = |_| {
	var $running = True
	while $running {
		match Stdin.line!() {
			Err(EndOfFile) => { $running = False }
			Err(StdinErr(err)) => { return Err(StdinErr(err)) }
			Ok(line) => {
				request : Try(Request, _)
				request = Json.parse(line)
				match request {
					Err(_) => Stdout.line!("{\"version\":1,\"id\":\"\",\"error\":\"invalid_request\"}")?
					Ok(item) => Stdout.line!(respond(item))?
				}
			}
		}
	}
	Ok({})
}

respond : Request -> Str
respond = |request| {
	if request.mode != "file" and request.mode != "expr" {
		return Json.to_str({ version: 1.U64, id: request.id, error: "unsupported_mode" })
	}
	parsed = if request.mode == "expr" { RocSyntax.parse_expr_str(request.source) } else { RocSyntax.parse_str(request.source) }
	tokens = parsed.tokens.map(|token| { kind: Str.inspect(token.kind), start: token.start, end: token.end })
	nodes = parsed.nodes.map(|node| { kind: Str.inspect(node.kind), start: node.start, end: node.end, children_start: node.children_start, children_len: node.children_len })
	diagnostics = parsed.diagnostics.map(|diagnostic| { stage: "parse", kind: Str.inspect(diagnostic.code), start: diagnostic.start, end: diagnostic.end })
	Json.to_str({ version: 1.U64, id: request.id, mode: request.mode, ok: parsed.status == Complete, consumed: parsed.consumed, status: Str.inspect(parsed.status), tokens, nodes, children: parsed.children, root: parsed.root, diagnostics })
}
