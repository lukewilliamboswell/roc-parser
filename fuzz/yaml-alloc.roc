app [target] {
 fuzz: platform "https://github.com/lukewilliamboswell/roc-fuzz/releases/download/0.4.3/41NK5FShyGC9Z8HNpUdUeXMWwLmLNfFxsfmLvdPL4Fxb.tar.zst",
 parser: "../package/main.roc",
}
import fuzz.Fuzz
import parser.Yaml

## Diagnostic only: deliberate crash reports counters, not a parser failure.
## Replay one UTF-8 YAML input; never include this target in fuzz campaigns.
test! : List(U8) => Fuzz.Outcome
test! = |bytes| {
 input = Str.from_utf8(bytes) ?? ""
 measured = Fuzz.measure_allocs!(|{}| Yaml.parse_str(input))
 summary = match measured.value {
  Ok(Sequence(values)) => values.len().to_str()
  Ok(Mapping(entries)) => entries.len().to_str()
  Ok(_) => "scalar"
  Err(_) => "error"
 }
 crash "ALLOCATION_DIAGNOSTIC allocations=${measured.allocations.to_str()} result=${summary}"
}

target = Fuzz.target_with!({
 name: "yaml-alloc-diagnostic",
 generator: Fuzz.raw_bytes,
 test!,
 show: |bytes| Str.inspect(Str.from_utf8(bytes)),
})
