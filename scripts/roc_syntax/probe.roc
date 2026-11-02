app [main!] {
	cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
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
			Err(EndOfInput) => {
				$running = False
			}
			Err(problem) => {
				return Err(problem)
			}
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

## The bytes of a file that isn't valid UTF-8, which JSON can't carry.
hex_bytes : Str -> List(U8)
hex_bytes = |text| {
	digits = text.to_utf8()
	var $bytes = []
	var $index = 0
	while $index + 1 < digits.len() {
		high = hex_digit(digits.get($index) ?? 0)
		low = hex_digit(digits.get($index + 1) ?? 0)
		$bytes = $bytes.append(high * 16 + low)
		$index = $index + 2
	}
	$bytes
}

hex_digit : U8 -> U8
hex_digit = |digit| if (digit >= 'a') digit - 'a' + 10 else digit - '0'

respond : Request -> Str
respond = |request| {
	if request.mode != "file" and request.mode != "file-hex" and request.mode != "expr" and request.mode != "statement" and request.mode != "header" and request.mode != "repl" {
		return Json.to_str({ version: 1.U64, id: request.id, error: "unsupported_mode" })
	}
	parsed = match request.mode {
		"expr" => RocSyntax.parse_expr_str(request.source)
		"file-hex" => RocSyntax.parse_bytes(hex_bytes(request.source))
		"statement" => RocSyntax.parse_statement_str(request.source)
		"header" => RocSyntax.parse_header_str(request.source)
		"repl" => {
			file = RocSyntax.parse_str(request.source)
			root = file.nodes.get(file.root) ?? { kind: ErrorNode, start: 0, end: 0, children_start: 0, children_len: 0 }
			if (file.status == Complete and root.children_len == 1) file else RocSyntax.parse_statement_str(request.source)
		}
		_ => RocSyntax.parse_str(request.source)
	}
	tokens = parsed.tokens.map(|token| { kind: Str.inspect(token.kind), start: token.start, end: token.end })
	nodes = parsed.nodes.map(|node| { kind: Str.inspect(node.kind), start: node.start, end: node.end, children_start: node.children_start, children_len: node.children_len })
	diagnostics = parsed.diagnostics.map(|diagnostic| { stage: "parse", kind: Str.inspect(diagnostic.code), start: diagnostic.start, end: diagnostic.end })
	Json.to_str({ version: 1.U64, id: request.id, mode: request.mode, ok: parsed.status == Complete, consumed: parsed.consumed, status: Str.inspect(parsed.status), tokens, nodes, children: parsed.children, root: parsed.root, diagnostics })
}
