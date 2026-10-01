app [main!] {
 cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.23.0/GNN5tt2gKdX4dhawg4915C4YB193woHFdcCkz31fhGxv.tar.zst",
 parser: "../../package/main.roc",
}
import cli.OsStr
import cli.Stdin
import cli.Stdout
import cli.Utc
import parser.Yaml

main! : List(OsStr) => Try({}, _)
main! = |args| {
 iterations = match args.last() { Ok(arg) => U64.from_str(OsStr.display(arg)) ?? 1
 Err(_) => 1 }
 input = Str.from_utf8(Stdin.read_to_end!()?) ?? ""
 repetitions = List.repeat({}, iterations)
 start = Utc.now!()
 var $checksum = 0.U64
 var $successes = 0.U64
 for _ in repetitions {
  match Yaml.parse_str(input) {
   Ok(value) => { $checksum = $checksum + consume(value)
 $successes = $successes + 1 }
   Err(InvalidYaml(error)) => { $checksum = $checksum + error.line + error.column }
  }
 }
 elapsed = Utc.delta_as_nanos(Utc.now!(), start)
 Stdout.line!("{\"iterations\":${iterations.to_str()},\"elapsed_ns\":${elapsed.to_str()},\"successes\":${$successes.to_str()},\"checksum\":${$checksum.to_str()}}")?
 Ok({})
}
consume : Yaml -> U64
consume = |value| match value {
 Null => 1
 Bool(_) => 2
 Int(_) => 3
 Float(_) => 4
 Text(text) => text.count_utf8_bytes().to_u64() + 5
 Sequence(values) => values.fold(6, |total, child| total + consume(child))
 Mapping(entries) => entries.fold(7, |total, entry| total + entry.key.count_utf8_bytes().to_u64() + consume(entry.value))
}
