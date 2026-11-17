"""Reference-only README comparators for markup, AsciiDoc and Roc source.

No work occurs on import. The caller invokes build/qualification only after
--refresh-references; default runs use ReferenceCache with this module's profiles.
"""
from __future__ import annotations

import hashlib
from html.parser import HTMLParser
import inspect
import json
import os
import re
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

try:
    from . import bench, compare_markup_performance as markup, readme_reference_proxy as proxy
    from .roc_syntax import build_oracle as syntax_build
except ImportError:
    import bench
    import compare_markup_performance as markup
    import readme_reference_proxy as proxy
    from roc_syntax import build_oracle as syntax_build

ROOT = Path(__file__).resolve().parents[1]
EXTRA_FORMATS = ("html", "svg", "mathml", "asciidoc", "roc-syntax")
PROXY = ROOT / "scripts/readme_reference_proxy.py"
RUST = ROOT / "bench/compare/markup-rust"
GO = ROOT / "bench/compare/markup-go"
PYTHON_DRIVER = ROOT / "scripts/markup/compare_python.py"
REQUIREMENTS = ROOT / "bench/compare/markup-python-requirements.txt"
GEM_HOME = ROOT / ".roc-parser-tmp/asciidoc/gems"


def specs(python=None):
    del python  # Commands are constructed only by explicit-refresh build().
    result = []
    for fmt in ("html", "svg", "mathml"):
        case = {"format": fmt, "mode": "fragment" if fmt == "html" else "xml"}
        manifest = {"binaries": {lang: {"path": lang} for lang in ("roc", "rust", "go", "python")}}
        for item in markup.implementations(case, manifest):
            if item["language"] == "roc":
                continue
            result.append(bench.Impl(item["language"], item["name"], fmt, [],
                                     item["semantics"] + " Generic DOM only; Roc additionally retains source and checks authoring rules. Different checksum contract; no speed ratio."))
    result.append(bench.Impl("ruby", "asciidoctor", "asciidoc", [],
                            "Asciidoctor 2.0.23 complete native block document, secure mode, fixed attributes, no rendering; checksum is top-level blocks + title UTF-8 bytes + 1."))
    result.append(bench.Impl("zig", "roc-compiler-parser", "roc-syntax", [],
                            "Pinned compiler tokenizer/parser AST in ReleaseFast; token/node-count checksum differs from RocSyntax lossless tree; no speed ratio."))
    return result


def checksum_semantics(impl):
    if impl.format in ("html", "svg", "mathml"):
        return "namespace-canonical-dom-v1"
    return "asciidoctor-top-level-blocks-title-v1" if impl.format == "asciidoc" else "compiler-node-token-count-v1"


def _paths(out):
    extra = Path(out) / "extra-references"
    return {"root": extra, "rust": extra / "rust-target/release/markup-dom-compare",
            "go": extra / "go-markup", "python": extra / "python/bin/python",
            "syntax": extra / "syntax-oracle/install/bin/roc-syntax-oracle"}


def _run(builder, name, command, cwd=ROOT):
    if not builder.run("readme-extra-" + name, [str(item) for item in command], cwd=cwd):
        raise bench.ReferenceUnavailable(f"supplementary reference build failed: {name}; inspect build logs")


def build(builder, formats):
    """Explicit refresh only: prepare reference artifacts serially, never build Roc."""
    formats = set(formats)
    paths = _paths(builder.out)
    paths["root"].mkdir(parents=True, exist_ok=True)
    languages = set()
    if formats.intersection(("html", "svg", "mathml")):
        _run(builder, "rust", ["cargo", "build", "--locked", "--release", "--jobs", "1", "--manifest-path", RUST / "Cargo.toml", "--target-dir", paths["root"] / "rust-target"], RUST)
        _run(builder, "go", ["env", "GOTOOLCHAIN=local", "go", "build", "-p=1", "-trimpath", "-mod=readonly", "-o", paths["go"], "."], GO)
        python = str(paths["python"])
        if not paths["python"].exists():
            _run(builder, "venv", [builder.python or sys.executable, "-m", "venv", paths["root"] / "python"])
        _run(builder, "python", [python, "-m", "pip", "install", "--require-hashes", "-r", REQUIREMENTS])
        languages.update(("rust", "go", "python"))
    else:
        python = builder.python or sys.executable
    if "asciidoc" in formats:
        _run(builder, "asciidoctor", [sys.executable, ROOT / "scripts/asciidoc/setup_oracle.py", "--gem-home", GEM_HOME])
    if "roc-syntax" in formats:
        _run(builder, "syntax", [sys.executable, ROOT / "scripts/roc_syntax/build_oracle.py", "--optimize", "ReleaseFast", "--cache-dir", paths["root"] / "syntax-oracle"])
    manifest = {"binaries": {key: {"path": str(value)} for key, value in paths.items() if key in ("rust", "go")}}
    manifest["binaries"].update(python={"path": str(python)}, roc={"path": "unused"})
    built = []
    for fmt in ("html", "svg", "mathml"):
        if fmt not in formats:
            continue
        case = {"format": fmt, "mode": "fragment" if fmt == "html" else "xml"}
        for item in markup.implementations(case, manifest):
            if item["language"] in languages:
                spec = next(value for value in specs() if value.format == fmt and value.name == item["name"])
                built.append(bench.Impl(spec.lang, spec.name, fmt, item["command"], spec.notes))
    if "asciidoc" in formats:
        spec = next(value for value in specs() if value.format == "asciidoc")
        built.append(bench.Impl(spec.lang, spec.name, spec.format, [str(python), str(PROXY), "asciidoc", "--worker", "ruby", "--script", str(ROOT / "scripts/asciidoc/performance_oracle.rb"), "--gem-home", str(GEM_HOME)], spec.notes))
    if "roc-syntax" in formats:
        spec = next(value for value in specs() if value.format == "roc-syntax")
        built.append(bench.Impl(spec.lang, spec.name, spec.format, [str(python), str(PROXY), "roc-syntax", "--worker", str(paths["syntax"])], spec.notes))
    return built


def _source_files(impl):
    if impl.lang == "rust":
        return [*RUST.glob("Cargo.*"), *RUST.glob("src/*.rs")]
    if impl.lang == "go":
        return [*GO.glob("*.go"), *GO.glob("go.*")]
    if impl.lang == "python":
        return [PYTHON_DRIVER, REQUIREMENTS]
    if impl.lang == "ruby":
        return [ROOT / "scripts/asciidoc/performance_oracle.rb", ROOT / "scripts/asciidoc/oracle.rb", ROOT / "scripts/asciidoc/setup_oracle.py"]
    return [ROOT / "scripts/roc_syntax/build_oracle.py", *sorted((ROOT / "scripts/roc_syntax/oracle").glob("*.zig"))]


def _ruby_environment():
    env = {key: value for key, value in os.environ.items() if key not in ("RUBYOPT", "RUBYLIB", "GEM_HOME", "GEM_PATH")}
    return {**env, "GEM_HOME": str(GEM_HOME), "GEM_PATH": str(GEM_HOME)}


def profile(impl, args, machine_config):
    sources = {path.relative_to(ROOT).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest() for path in sorted(_source_files(impl))}
    if impl.lang == "rust":
        env = {**os.environ, "RUSTUP_AUTO_INSTALL": "0"}
        runtime = {"compiler": bench.reference_probe(["rustc", "--version", "--verbose"], env=env),
                   "cargo": bench.reference_probe(["cargo", "--version"], env=env), "release": "opt-level=3,lto=thin,codegen-units=1,debug=2,strip=false"}
    elif impl.lang == "go":
        env = {**os.environ, "GOTOOLCHAIN": "local"}
        runtime = {"compiler": bench.reference_probe(["go", "version"], GO, env=env),
                   "configuration": json.loads(bench.reference_probe(["go", "env", "-json", "GOOS", "GOARCH", "GOAMD64", "CGO_ENABLED", "GOFLAGS", "GOEXPERIMENT"], GO, env=env))}
    elif impl.lang == "python":
        runtime = bench.reference_runtime("python", str(_paths(args.work)["python"]))
    elif impl.lang == "ruby":
        code = "require 'json';require 'digest';gem 'asciidoctor','=2.0.23';require 'asciidoctor';r=Gem.loaded_specs.fetch('asciidoctor').full_gem_path;h=Digest::SHA256.new;Dir.glob(File.join(r,'lib/**/*.rb')).sort.each{|p|h.update(p.delete_prefix(r)+\"\\0\");h.update(File.binread(p))};puts JSON.generate(version:RUBY_DESCRIPTION,gem:Asciidoctor::VERSION,code_sha256:h.hexdigest)"
        runtime = json.loads(bench.reference_probe(["ruby", "-e", code], env=_ruby_environment()))
    else:
        runtime = {"zig": bench.reference_probe([os.environ.get("ZIG", "zig"), "version"]),
                   "reference_revision": syntax_build.ROC_REVISION,
                   "optimize": "ReleaseFast", "libc_allocator": True}
    sources[Path(__file__).relative_to(ROOT).as_posix()] = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    sources[PROXY.relative_to(ROOT).as_posix()] = hashlib.sha256(PROXY.read_bytes()).hexdigest()
    if impl.lang == "rust":
        cargo_home = Path(os.environ.get("CARGO_HOME", Path.home() / ".cargo"))
        for folder, label in ((cargo_home, "cargo-home"), (ROOT / ".cargo", "project-cargo"), (RUST / ".cargo", "comparator-cargo")):
            for name in ("config", "config.toml"):
                path = folder / name
                if path.is_file():
                    sources[f"{label}/{name}"] = hashlib.sha256(path.read_bytes()).hexdigest()
    functions = (build, qualification, expected_markup, checksum_semantics, proxy.request, proxy.response, proxy.worker_response)
    sources["readme-reference-adapter"] = hashlib.sha256("\n".join(inspect.getsource(value) for value in functions).encode()).hexdigest()
    protocol = "\n".join(inspect.getsource(value) for value in (bench.run_once, bench.measure, bench.calibrate, bench.summarize, markup.canonical, markup.normalize_tree, markup.model_checksum))
    return {"runtime": runtime, "sources": sources, "machine": machine_config,
            "build_recipe_sha256": hashlib.sha256(inspect.getsource(build).encode()).hexdigest(),
            "measurement_protocol_sha256": hashlib.sha256(protocol.encode()).hexdigest(),
            "checksum_semantics": checksum_semantics(impl), "ownership": impl.notes}


def _expanded(name):
    if name.startswith("{"):
        return name[1:].split("}", 1)
    return "", name


def _canonical_xml(node):
    namespace, name = _expanded(node.tag)
    attributes = [[*_expanded(key), value] for key, value in node.attrib.items()]
    content = [["text", node.text]] if node.text else []
    for child in node:
        content.append(_canonical_xml(child))
        if child.tail:
            content.append(["text", child.tail])
    return ["element", namespace, name, sorted(attributes), content]


class _ExplicitHtml(HTMLParser):
    """Independent projection for the README's explicit, balanced fragment."""
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.roots = []
        self.stack = []

    def handle_starttag(self, tag, attrs):
        if tag in ("svg", "math", "html", "head", "body"):
            raise ValueError("README HTML reference expects an explicit ordinary fragment")
        node = ["element", "http://www.w3.org/1999/xhtml", tag, sorted([["", key, value or ""] for key, value in attrs]), []]
        (self.stack[-1][4] if self.stack else self.roots).append(node)
        self.stack.append(node)

    def handle_endtag(self, tag):
        if not self.stack or self.stack[-1][2] != tag:
            raise ValueError("README fragment requires balanced explicit tags")
        self.stack.pop()

    def handle_data(self, data):
        if data:
            (self.stack[-1][4] if self.stack else self.roots).append(["text", data])


def expected_markup(doc):
    if doc.format != "html":
        root = ET.fromstring(doc.data)
        expected = "{http://www.w3.org/2000/svg}svg" if doc.format == "svg" else "{http://www.w3.org/1998/Math/MathML}math"
        if root.tag != expected:
            raise ValueError("SVG/MathML reference inputs require explicit namespace-root documents")
        return [_canonical_xml(root)]
    parser = _ExplicitHtml()
    parser.feed(doc.data.decode())
    parser.close()
    if parser.stack:
        raise ValueError("unclosed README fragment")
    return parser.roots


def qualification(impl, doc, args, builder):
    """Explicit refresh only: prove reference acceptance on the identical input."""
    if not impl.command:
        raise bench.ReferenceUnavailable("reference artifacts have not been explicitly prepared")
    if impl.format in ("html", "svg", "mathml"):
        expected = expected_markup(doc)
        actual = markup.canonical({"command": impl.command, "name": impl.id}, doc.data)
        if expected != actual:
            return {"status": "different", "notes": ["canonical namespaces/tree/text/attributes differ on the exact README input"], "checksum_semantics": checksum_semantics(impl), "ownership": impl.notes}
        first = bench.run_once(impl.command, doc.data, 1, args.timeout)
        if first["successes"] != 1 or first["checksum"] != markup.model_checksum(actual):
            raise bench.ReferenceUnavailable("reference checksum does not consume its qualified DOM")
        return {"status": "qualified", "notes": [impl.notes], "checksum_semantics": checksum_semantics(impl), "ownership": impl.notes,
                "canonical": actual, "canonical_sha256": hashlib.sha256(json.dumps(actual, ensure_ascii=False, separators=(",", ":")).encode()).hexdigest(), "expected_checksum": first["checksum"]}
    paths = _paths(builder.out)
    if impl.format == "asciidoc":
        result = proxy.worker_response("asciidoc", doc.data, 0, worker="ruby", script=ROOT / "scripts/asciidoc/performance_oracle.rb", gem_home=GEM_HOME, correctness=True)
        projection = result.get("projection", {})
        title = next((line[2:] for line in doc.data.decode().splitlines() if line.startswith("= ")), "")
        sections = [line[3:] for line in doc.data.decode().splitlines() if line.startswith("== ")]
        if result.get("oracle_version") != "2.0.23" or projection.get("title") != title or [block.get("title") for block in projection.get("blocks", [])] != sections:
            raise bench.ReferenceUnavailable("Asciidoctor projection differs from README title/section fixture")
        return {"status": "qualified", "checksum_semantics": checksum_semantics(impl), "ownership": impl.notes,
                "notes": ["Secure native block AST with fixed attributes and no rendering; Roc block representation differs."],
                "projection": projection, "oracle_code_sha256": result.get("oracle_code_sha256"), "expected_checksum": result["operation_checksum"]}
    binary = paths["syntax"]
    try:
        receipt = json.loads(binary.with_suffix(".json").read_text())
    except (OSError, ValueError):
        raise bench.ReferenceUnavailable("pinned compiler worker receipt is unavailable") from None
    worker_hash = hashlib.sha256()
    for path in (ROOT / "scripts/roc_syntax/build_oracle.py", ROOT / "scripts/roc_syntax/oracle/main.zig", ROOT / "scripts/roc_syntax/oracle/build.zig"):
        worker_hash.update(path.name.encode() + b"\0" + path.read_bytes() + b"\0")
    if receipt.get("worker_sha256") != worker_hash.hexdigest() or receipt.get("zig_version") != syntax_build.ZIG_VERSION:
        raise bench.ReferenceUnavailable("compiler worker receipt does not match pinned sources/toolchain")
    if receipt.get("optimize") != "ReleaseFast" or receipt.get("compiler_revision") != syntax_build.ROC_REVISION or receipt.get("binary_sha256") != hashlib.sha256(binary.read_bytes()).hexdigest():
        raise bench.ReferenceUnavailable("compiler reference is not the verified pinned ReleaseFast artifact")
    result = proxy.worker_response("roc-syntax", doc.data, 0, worker=str(binary), correctness=True)
    if not result.get("ok") or not result.get("consumed") or result.get("diagnostics"):
        raise bench.ReferenceUnavailable("compiler reference rejected the exact README input")
    expected_names = re.findall(r"^([a-z][A-Za-z0-9_]*) = ", doc.data.decode(), flags=re.MULTILINE)
    definitions = [fact for fact in result.get("facts", []) if fact.get("kind") == "definition"]
    if len(definitions) != len(expected_names) or any(not doc.data[fact["start"]:fact["end"]].startswith((name + " ").encode()) for name, fact in zip(expected_names, definitions)):
        raise bench.ReferenceUnavailable("compiler definition facts differ from the exact README source fixture")
    return {"status": "qualified", "checksum_semantics": checksum_semantics(impl), "ownership": impl.notes,
            "definitions": expected_names,
            "notes": ["Pinned reference grammar; tokenizer, parser setup, AST and destruction timed. Different syntax-tree checksum; no ratio."],
            "receipt": receipt, "tokens": len(result.get("tokens", [])), "facts": len(result.get("facts", []))}


def fresh_roc_check(impl, doc, args, builder, reference_evidence):
    """Never cache this current-candidate verdict or include it in reference keys."""
    if impl.format not in ("html", "svg", "mathml"):
        return {"status": "not_checked", "notes": ["Fresh Roc acceptance is measured; model correctness is covered by the independent format conformance suites, not inferred from different internal checksums."]}
    binary = Path(builder.out) / "bin/roc-markup-canonical"
    if not binary.is_file():
        raise bench.ReferenceUnavailable("fresh Roc canonical driver is unavailable")
    operation = "html-fragment-full" if impl.format == "html" else impl.format + "-full"
    actual = markup.canonical({"command": [str(binary), operation], "name": "current Roc"}, doc.data)
    if actual != reference_evidence.get("canonical"):
        raise bench.ReferenceUnavailable("fresh Roc canonical DOM differs from cached independent reference")
    return {"status": "match", "canonical_sha256": reference_evidence["canonical_sha256"], "notes": ["DOM equality checked outside timers; Roc additionally performs authoring validation."]}
