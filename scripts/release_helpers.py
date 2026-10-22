#!/usr/bin/env python3
"""Build, test and describe a roc-parser release (.github/workflows/release.yml).

    release_helpers.py validate --event workflow_dispatch --release-version 2.1.0 --default-branch main
    release_helpers.py build --release-version 2.1.0 --output-dir .release/candidate
    release_helpers.py test --release-version 2.1.0 --directory .release/candidate
    release_helpers.py make-release-notes --release-version 2.1.0 --bundle <hash>.tar.zst --output notes.md
    release_helpers.py package-examples --release-version 2.1.0 --bundle-url URL --output-dir dist
    release_helpers.py package-docs --release-version 2.1.0 --output-dir dist
    release_helpers.py assemble-pages --manual .docs-out/site --api .docs-api/2.1.0 --output .pages

A pull request runs `validate`, `build` and `test` as a dry run, with the
version 0.0.1-dry-run and the notes of the highest version under
docs/releases/. The workflow's publish job hands the tested files to
roc-automation's publish-release action; nothing here writes to GitHub.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path
from typing import Sequence

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import release_security_assets  # noqa: E402  (needs the sys.path entry above)
from workflow_helpers import append_github_output, validate_examples  # noqa: E402

DEFAULT_REPO = "lukewilliamboswell/roc-parser"
DRY_RUN_VERSION = "0.0.1-dry-run"
NOTES_DIR = ROOT / "docs" / "releases"
EXAMPLES_SPEC = ROOT / "tests" / "examples.json"
SEMVER_RE = re.compile(r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([0-9A-Za-z.-]+))?$")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subcommands = parser.add_subparsers(dest="command", required=True)

    validate = subcommands.add_parser("validate", help="check that a release may start, or choose the dry run's version")
    validate.add_argument("--event", default=os.environ.get("GITHUB_EVENT_NAME", ""))
    validate.add_argument("--release-version", default="")
    validate.add_argument("--default-branch", default="main")
    validate.add_argument("--ref", default=os.environ.get("GITHUB_REF", ""))
    validate.add_argument("--repo", default="")
    validate.add_argument("--github-output", type=Path)
    validate.set_defaults(func=cmd_validate)

    build = subcommands.add_parser("build", help="build every asset of a release, its SBOM and its notes")
    build.add_argument("--release-version", required=True)
    build.add_argument("--output-dir", type=Path, required=True)
    build.add_argument("--notes", type=Path, help="the release notes (default: docs/releases/<version>.adoc, "
                                                  "or the highest version's for the dry run)")
    build.add_argument("--repo", default="")
    build.add_argument("--github-output", type=Path)
    build.set_defaults(func=cmd_build)

    test = subcommands.add_parser("test", help="test the exact files `build` made")
    test.add_argument("--release-version", required=True)
    test.add_argument("--directory", type=Path, required=True)
    test.add_argument("--repo", default="")
    test.set_defaults(func=cmd_test)

    notes = subcommands.add_parser("make-release-notes", help="render docs/releases/<version>.adoc as the release body")
    notes.add_argument("--release-version", required=True)
    notes.add_argument("--bundle", required=True, help="the bundle's file name, <hash>.tar.zst")
    notes.add_argument("--notes", type=Path, help="the notes to render (default: docs/releases/<version>.adoc)")
    notes.add_argument("--output", type=Path, required=True)
    notes.add_argument("--repo", default="")
    notes.set_defaults(func=cmd_make_release_notes)

    examples = subcommands.add_parser("package-examples", help="zip examples/ for a release, with spec.json")
    examples.add_argument("--release-version", required=True)
    examples.add_argument("--bundle-url", default="", help="the bundle URL to pin; or use --bundle")
    examples.add_argument("--bundle", default="", help="the bundle's file name, to make its release URL")
    examples.add_argument("--repo", default="")
    examples.add_argument("--examples-dir", type=Path, default=ROOT / "examples")
    examples.add_argument("--output", type=Path, help="the zip to write (default: <output-dir>/<asset name>)")
    examples.add_argument("--output-dir", type=Path, default=ROOT / "dist")
    examples.set_defaults(func=cmd_package_examples)

    docs = subcommands.add_parser("package-docs", help="build the manual (HTML and PDF) and the API reference")
    docs.add_argument("--release-version", default="unreleased", help="the version the documentation names")
    docs.add_argument("--output-dir", type=Path, default=ROOT / ".docs-out" / "release")
    docs.add_argument("--site", type=Path, help="also leave the unzipped site here")
    docs.add_argument("--repo", default="")
    docs.add_argument("--roc", default=os.environ.get("ROC", "roc"))
    docs.set_defaults(func=cmd_package_docs)

    pages = subcommands.add_parser("assemble-pages", help="lay out the site: the manual at the root, the API under api/")
    pages.add_argument("--manual", required=True, help="the built manual site (build_manual.py's site/)")
    pages.add_argument("--api", required=True, help="one version's API reference (build_docs.py's <root>/<version>/)")
    pages.add_argument("--repo", default="")
    pages.add_argument("--output", required=True)
    pages.set_defaults(func=cmd_assemble_pages)

    args = parser.parse_args(argv)
    if hasattr(args, "repo"):
        args.repo = args.repo or os.environ.get("GITHUB_REPOSITORY", DEFAULT_REPO)
    try:
        return args.func(args)
    except (RuntimeError, ValueError, release_security_assets.ReleaseSecurityError) as err:
        print(f"error: {err}", file=sys.stderr)
        return 1


# ------------------------------------------------------------------ versions


def semver_key(version: str) -> tuple:
    """SemVer precedence: a prerelease sorts before its release, numeric
    identifiers numerically and before alphanumeric ones."""
    match = SEMVER_RE.match(version)
    if match is None:
        raise ValueError(f"not a SemVer version without a v prefix: {version!r}")
    core = tuple(int(part) for part in match.group(1, 2, 3))
    if match.group(4) is None:
        return (*core, 1, ())
    return (*core, 0, tuple((0, int(p), "") if p.isdigit() else (1, 0, p) for p in match.group(4).split(".")))


def notes_path(version: str, notes_dir: Path = NOTES_DIR) -> Path:
    return notes_dir / f"{version}.adoc"


def latest_notes(notes_dir: Path = NOTES_DIR) -> Path:
    """The notes of the highest version under docs/releases/, which are those
    of the next release whenever a pull request adds or edits them."""
    versions = [path.stem for path in notes_dir.glob("*.adoc") if SEMVER_RE.match(path.stem)]
    if not versions:
        raise RuntimeError(f"no <version>.adoc release notes under {notes_dir}")
    return notes_path(max(versions, key=semver_key), notes_dir)


def is_prerelease(version: str) -> bool:
    semver_key(version)
    return "-" in version


def asset_name(kind: str, version: str) -> str:
    """The file name of one of the assets a release carries besides its bundle."""
    names = {
        "examples-zip": f"roc-parser-examples-{version}.zip",
        "docs-zip": f"roc-parser-docs-{version}.zip",
        "pdf": f"roc-parser-{version}.pdf",
    }
    return names[kind]


def release_asset_url(repo: str, release_version: str, artifact_file: str) -> str:
    return f"https://github.com/{repo}/releases/download/{release_version}/{artifact_file}"


# ------------------------------------------------------------- release notes

PLACEHOLDER = "{package-url}"
UNRESOLVED_PLACEHOLDER_RE = re.compile(r"\{[a-z0-9-]+-url\}")
LINK_RE = re.compile(r"(?<![\w/])(?:link:)?(https?://[^\s\[\]]+)\[([^\]]*)\]")
XREF_RE = re.compile(r"<<[^,>]+,([^>]+)>>")
BOLD_RE = re.compile(r"(?<![\w*`])\*(?=\S)([^*\n]*?\S)\*(?![\w*])")
PASSTHROUGH_CODE_RE = re.compile(r"`\+(.*?)\+`")
LIST_RE = re.compile(r"^(\*{1,3}|\.{1,3})\s+(.*)$")
ADMONITION_RE = re.compile(r"^(NOTE|TIP|IMPORTANT|WARNING|CAUTION):\s+(.*)$")


def inline_markdown(text: str) -> str:
    """AsciiDoc inline markup that release notes use, as Markdown. Code spans
    are left alone."""
    parts = re.split(r"(`[^`]*`)", PASSTHROUGH_CODE_RE.sub(r"`\1`", text))
    for index, part in enumerate(parts):
        if part.startswith("`"):
            continue
        part = LINK_RE.sub(lambda m: f"[{m.group(2) or m.group(1)}]({m.group(1)})", part)
        part = XREF_RE.sub(r"\1", part)
        parts[index] = BOLD_RE.sub(r"**\1**", part)
    return "".join(parts)


def table_markdown(rows: list[list[str]], line: int) -> list[str]:
    if len(rows) < 2 or len({len(row) for row in rows}) != 1:
        raise ValueError(f"line {line}: a table needs a header row and rows of equal width")
    cells = [[inline_markdown(cell).replace("|", "\\|") for cell in row] for row in rows]
    return ["| " + " | ".join(cells[0]) + " |", "|" + "---|" * len(cells[0]),
            *("| " + " | ".join(row) + " |" for row in cells[1:])]


def notes_markdown(adoc: str) -> str:
    """Release notes in AsciiDoc as the Markdown GitHub renders a release body
    with. The notes use headings, paragraphs, lists, source blocks, simple
    tables and links; anything else is refused, so that a dry run finds it."""
    out: list[str] = []
    lines = adoc.splitlines()
    index = 0
    language = None
    while index < len(lines):
        line = lines[index]
        number = index + 1
        index += 1
        if line.startswith("//"):
            continue
        if re.fullmatch(r"\[source(,\s*[\w+-]+)?(,.*)?\]", line):
            language = (line[len("[source"):].strip(",] ").split(",")[0]).strip()
            continue
        if line in ("----", "...."):
            block = []
            while index < len(lines) and lines[index] != line:
                block.append(lines[index])
                index += 1
            if index == len(lines):
                raise ValueError(f"line {number}: unterminated {line} block")
            index += 1
            out += [f"```{language or ''}", *block, "```"]
            language = None
            continue
        if language is not None:
            raise ValueError(f"line {number}: [source] must be followed by a ---- block")
        if re.fullmatch(r"\[(cols=.*|%header|options=\"?header\"?.*)\]", line):
            continue
        if line == "|===":
            rows: list[list[str]] = []
            while index < len(lines) and lines[index] != "|===":
                row = lines[index].strip()
                index += 1
                if not row:
                    continue
                if not row.startswith("|"):
                    raise ValueError(f"line {index}: each table row must be one line of | cells")
                rows.append([cell.strip() for cell in row[1:].split(" |")])
            if index == len(lines):
                raise ValueError(f"line {number}: unterminated table")
            index += 1
            out += table_markdown(rows, number)
            continue
        heading = re.match(r"^(={1,5}) (.*)$", line)
        if heading:
            out.append("#" * len(heading.group(1)) + " " + inline_markdown(heading.group(2)))
            continue
        listing = LIST_RE.match(line)
        if listing:
            marker, text = listing.groups()
            bullet = "-" if marker[0] == "*" else "1."
            out.append("  " * (len(marker) - 1) * (1 if bullet == "-" else 2) + f"{bullet} {inline_markdown(text)}")
            continue
        admonition = ADMONITION_RE.match(line)
        if admonition:
            out.append(f"> **{admonition.group(1).title()}:** {inline_markdown(admonition.group(2))}")
            continue
        if line == "'''":
            out.append("---")
            continue
        if line == "+" or re.fullmatch(r"(====+|\*\*\*\*+|____+|\+\+\+\+?|--)", line) or re.match(r"^:[\w-]+:", line) \
                or re.fullmatch(r"\[[^\]]*\]", line) or re.match(r"^\.[^\s.]", line):
            raise ValueError(f"line {number}: {line!r} is not supported in release notes")
        out.append(inline_markdown(line) if line.strip() else "")
    return "\n".join(out).strip() + "\n"


def compiler_versions() -> str:
    roc = os.environ.get("ROC", "roc")
    completed = subprocess.run([roc, "version"], capture_output=True, text=True, check=False)
    if completed.returncode != 0 or not completed.stdout.strip():
        raise RuntimeError(f"`{roc} version` failed: {completed.stderr.strip()}")
    return completed.stdout.strip()


def render_notes(source: str, package_url: str, compilers: str) -> str:
    """The body of the GitHub release: the authored notes with every
    placeholder resolved, then the compilers that built and tested it."""
    if PLACEHOLDER not in source:
        raise RuntimeError(f"the release notes never use {PLACEHOLDER}, so they cannot say how to use the release")
    rendered = source.replace(PLACEHOLDER, package_url)
    unresolved = sorted(set(UNRESOLVED_PLACEHOLDER_RE.findall(rendered)))
    if unresolved:
        raise RuntimeError(f"the release notes use placeholders this release cannot fill: {', '.join(unresolved)}")
    return f"{notes_markdown(rendered).rstrip()}\n\n## Compilers\n\n```\n{compilers}\n```\n"


def read_notes(path: Path) -> str:
    if not path.is_file():
        raise RuntimeError(f"write the release notes first: {path} is missing")
    text = path.read_text(encoding="utf-8")
    if not text.strip():
        raise RuntimeError(f"release notes are empty: {path}")
    return text


def write_release_notes(version: str, bundle: str, notes: Path, output: Path, repo: str) -> Path:
    url = release_asset_url(repo, version, bundle)
    body = render_notes(read_notes(notes), url, compiler_versions())
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(body, encoding="utf-8")
    return output


def cmd_make_release_notes(args: argparse.Namespace) -> int:
    notes = args.notes or notes_path(args.release_version)
    print(write_release_notes(args.release_version, args.bundle, notes, args.output, args.repo))
    return 0


# ---------------------------------------------------------------------- docs


def zip_tree(source: Path, output: Path, prefix: str) -> None:
    """Zip every file under `source` beneath one top-level `prefix` directory,
    in a stable order, so an unzipped copy is one folder named for the release."""
    files = sorted(path for path in source.rglob("*") if path.is_file())
    if not files:
        raise RuntimeError(f"nothing to package under {source}")
    with zipfile.ZipFile(output, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for path in files:
            archive.write(path, f"{prefix}/{path.relative_to(source).as_posix()}")


PAGES_NOT_FOUND = """<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Page not found · roc-parser</title>
<style>
body {{ font-family: system-ui, sans-serif; max-width: 40rem; margin: 4rem auto; padding: 0 1rem; line-height: 1.5; color: #1a1a1a; background: #ffffff; }}
a {{ color: #6b3f8f; }}
@media (prefers-color-scheme: dark) {{ body {{ color: #ececea; background: #16181a; }} a {{ color: #c9a8ea; }} }}
</style>
</head>
<body>
<h1>Page not found</h1>
<p>This site shows the documentation for the current roc-parser release only:
the <a href="{root}">manual</a> and the <a href="{root}api/">API reference</a>.</p>
<p>The documentation for an earlier release is attached to that release on the
<a href="https://github.com/{repo}/releases">releases page</a>, as a PDF and as
a zipped static site.</p>
</body>
</html>
"""


def assemble_pages(manual: Path, api: Path, output: Path, repo: str) -> Path:
    """The site for one release: the manual is the front page and the API
    reference is under `api/`. Earlier releases' documentation lives in the
    assets attached to each release, so the site never accumulates old
    versions; `404.html` says where an old version's pages went."""
    for source, what in ((manual, "manual"), (api, "API reference")):
        if not (source / "index.html").is_file():
            raise RuntimeError(f"no {what} index.html under {source}")
    if output.exists():
        shutil.rmtree(output)
    shutil.copytree(manual, output)
    if (output / "api").exists():
        raise RuntimeError("the manual already has an api/ directory")
    shutil.copytree(api, output / "api")
    owner, _, name = repo.partition("/")
    root = f"https://{owner}.github.io/{name}/"
    (output / "404.html").write_text(PAGES_NOT_FOUND.format(root=root, repo=repo), encoding="utf-8")
    return output


def cmd_assemble_pages(args: argparse.Namespace) -> int:
    print(assemble_pages(Path(args.manual), Path(args.api), Path(args.output), args.repo))
    return 0


def package_docs(version: str, output_dir: Path, repo: str, roc: str = "roc", site: Path | None = None) -> list[Path]:
    """Build the release's documentation and package it as two assets:

    - `roc-parser-docs-<version>.zip`: the site, opening at `index.html`, with
      the manual at the root, the `roc docs` API reference under `api/` and the
      PDF beside them. It is what GitHub Pages serves for the latest release.
    - `roc-parser-<version>.pdf`: the manual as one PDF.
    """
    if not version.strip() or "/" in version or "\\" in version:
        raise RuntimeError(f"a valid release version is required, got {version!r}")
    output_dir.mkdir(parents=True, exist_ok=True)
    # The manual builds in a container that sees only the checkout, so its
    # output has to be inside it; `.docs-out/` is ignored.
    manual_root = ROOT / ".docs-out" / f"release-{version}"
    with tempfile.TemporaryDirectory(prefix="roc-parser-release-docs-") as scratch:
        try:
            api_root = Path(scratch) / "api"
            subprocess.run(
                [sys.executable, str(ROOT / "scripts" / "build_docs.py"), "--roc", roc,
                 "--docs-root", str(api_root), "--version", version],
                check=True, cwd=ROOT,
            )
            subprocess.run(
                [sys.executable, str(ROOT / "scripts" / "build_manual.py"), "--pdf",
                 "--docs-version", version, "--output", str(manual_root)],
                check=True, cwd=ROOT,
            )
            pdf = output_dir / asset_name("pdf", version)
            shutil.copyfile(manual_root / "roc-parser.pdf", pdf)
            staged = assemble_pages(manual_root / "site", api_root / version, Path(scratch) / "site", repo)
            docs_zip = output_dir / asset_name("docs-zip", version)
            zip_tree(staged, docs_zip, docs_zip.name.removesuffix(".zip"))
            if site is not None:
                if site.exists():
                    shutil.rmtree(site)
                shutil.copytree(staged, site)
        finally:
            shutil.rmtree(manual_root, ignore_errors=True)
    return [docs_zip, pdf]


def cmd_package_docs(args: argparse.Namespace) -> int:
    for asset in package_docs(args.release_version, args.output_dir, args.repo, args.roc, args.site):
        print(asset)
    return 0


# ------------------------------------------------------------------ examples

# The `parser:` entry of an example's app header. In the repository it is the
# package source (`../package/main.roc`); in the release archive, the release bundle's URL.
PACKAGE_DEPENDENCY_RE = re.compile(r'(?m)^(\s*parser:\s*)"([^"]+)"')

EXAMPLES_README = """# roc-parser {tag} examples

These are the roc-parser examples for release {tag}. Each app's `parser`
dependency is pinned to that release's package:

    {url}

Install the latest Roc nightly, then from this directory:

    cd examples
    roc check csv-movies.roc
    roc csv-movies.roc

Every `.roc` file in `examples/` is a complete app; run any of them the same
way. `spec.json` records what each one prints, and is what roc-parser's nightly
compatibility check compares with. The manual and API reference for this
release are attached to the same release: https://github.com/{repo}/releases/tag/{tag}
"""


def rewrite_package_dependency(source: str, url: str, name: str = "example") -> str:
    """Point the one `parser:` dependency in `source` at `url`."""
    rewritten, count = PACKAGE_DEPENDENCY_RE.subn(
        lambda match: f'{match.group(1)}"{url}"', source
    )
    if count != 1:
        raise RuntimeError(f"{name} must declare exactly one parser package dependency, found {count}")
    return rewritten


def examples_spec(version: str, bundle_url: str, spec_path: Path = EXAMPLES_SPEC) -> dict:
    """The archive's spec.json (schema 1 of roc-automation's shared nightly
    compatibility runner), from the expected output in tests/examples.json."""
    apps = json.loads(spec_path.read_text(encoding="utf-8"))["apps"]
    return {"schema_version": 1, "release": version, "dependencies": [bundle_url], "apps": apps}


def package_examples(examples_dir: Path, bundle_url: str, tag: str, output: Path, repo: str = DEFAULT_REPO,
                     spec_path: Path = EXAMPLES_SPEC) -> Path:
    """Stage `examples_dir` with every app's `parser:` pinned to `bundle_url`,
    add a README and spec.json, and zip it as `roc-parser-examples-<tag>/`.
    Returns the zip's path."""
    if not tag.strip() or "/" in tag or "\\" in tag:
        raise RuntimeError(f"a valid release version is required, got {tag!r}")
    if not bundle_url.strip() or any(c in bundle_url for c in '"\r\n'):
        raise RuntimeError(f"a valid bundle URL is required, got {bundle_url!r}")
    apps = sorted(examples_dir.glob("*.roc"))
    if not apps:
        raise RuntimeError(f"no Roc examples found in {examples_dir}")
    prefix = asset_name("examples-zip", tag).removesuffix(".zip")
    with tempfile.TemporaryDirectory(prefix="roc-parser-release-examples-") as scratch:
        staging = Path(scratch) / prefix
        shutil.copytree(examples_dir, staging / "examples")
        for app in sorted((staging / "examples").glob("*.roc")):
            source = app.read_text(encoding="utf-8")
            app.write_text(rewrite_package_dependency(source, bundle_url, app.name), encoding="utf-8")
        (staging / "README.md").write_text(
            EXAMPLES_README.format(tag=tag, url=bundle_url, repo=repo), encoding="utf-8"
        )
        (staging / "spec.json").write_text(
            json.dumps(examples_spec(tag, bundle_url, spec_path), indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
        )
        output.parent.mkdir(parents=True, exist_ok=True)
        zip_tree(staging, output, prefix)
    try:
        validate_examples(archive=output)
    except ValueError as err:
        output.unlink(missing_ok=True)
        raise RuntimeError(str(err)) from err
    return output


def cmd_package_examples(args: argparse.Namespace) -> int:
    """Package the examples as `roc-parser-examples-<tag>.zip`.

    The repository's examples use the package source (`../package/main.roc`);
    the archive a release carries points them at that release's bundle.
    """
    tag = args.release_version
    bundle_url = args.bundle_url
    if not bundle_url:
        if not args.bundle:
            raise RuntimeError("either --bundle-url or --bundle is required")
        bundle_url = release_asset_url(args.repo, tag, Path(args.bundle).name)
    output = args.output or args.output_dir / asset_name("examples-zip", tag)
    print(package_examples(args.examples_dir, bundle_url, tag, output, args.repo))
    return 0


# --------------------------------------------------- validate, build and test


def gh_json(path: str) -> dict | None:
    """One GitHub API read through `gh`; None when the resource does not exist."""
    completed = subprocess.run(["gh", "api", path], capture_output=True, text=True, check=False)
    if completed.returncode != 0:
        if "HTTP 404" in completed.stderr:
            return None
        raise RuntimeError(f"gh api {path} failed: {completed.stderr.strip()}")
    return json.loads(completed.stdout)


def previous_release(repo: str) -> tuple[str, str] | None:
    """The tag and bundle of the latest final release, which `roc bump` compares with."""
    release = gh_json(f"repos/{repo}/releases/latest")
    if release is None:
        return None
    bundles = [asset["browser_download_url"] for asset in release.get("assets", []) if asset["name"].endswith(".tar.zst")]
    if len(bundles) != 1:
        raise RuntimeError(f"release {release.get('tag_name')} has {len(bundles)} bundles, expected one")
    return release["tag_name"], bundles[0]


# What `roc bump` prints when it cannot compare at all, as opposed to finding
# that the version understates the change.
BASELINE_UNREADABLE = ("package does not compile", "cannot extract public api")


def check_bump(previous: str, expect: str | None) -> tuple[int, str]:
    """Run `roc bump`, echoing its output, and return its exit code and output."""
    roc = os.environ.get("ROC", "roc")
    command = [roc, "bump", "--old", previous, *(["--expect", expect] if expect else []), "package/main.roc"]
    print("+ " + " ".join(command), flush=True)
    completed = subprocess.run(command, cwd=ROOT, check=False, capture_output=True, text=True)
    output = completed.stdout + completed.stderr
    print(output, end="" if output.endswith("\n") or not output else "\n", flush=True)
    return completed.returncode, output


def baseline_unreadable(output: str) -> bool:
    lower = output.casefold()
    return any(marker in lower for marker in BASELINE_UNREADABLE)


def is_major_bump(version: str, previous: str) -> bool:
    """Whether `version` moves the major version past `previous`, so that no
    API change can be understated (3.0.0-rc.1 counts, as it releases 3.x)."""
    return semver_key(version)[0] > semver_key(previous)[0]


def cmd_validate(args: argparse.Namespace) -> int:
    """A dispatch releases `--release-version` from the default branch, once,
    with its notes written and a version that `roc bump` agrees with. Any other
    event is a dry run, which names the notes of the highest version."""
    dry_run = args.event != "workflow_dispatch"
    if dry_run:
        version, notes = DRY_RUN_VERSION, latest_notes()
    else:
        version = args.release_version.strip()
        semver_key(version)
        if args.ref != f"refs/heads/{args.default_branch}":
            raise RuntimeError(f"release from refs/heads/{args.default_branch} only, not {args.ref!r}")
        notes = notes_path(version)
    source = read_notes(notes)
    if PLACEHOLDER not in source:
        raise RuntimeError(f"{notes.relative_to(ROOT)} never uses {PLACEHOLDER}")
    notes_markdown(source)
    if not dry_run:
        for path, what in ((f"repos/{args.repo}/git/ref/tags/{version}", "tag"),
                           (f"repos/{args.repo}/releases/tags/{version}", "release")):
            if gh_json(path) is not None:
                raise RuntimeError(f"the {what} {version} already exists; a release is never replaced")

    previous = previous_release(args.repo)
    if previous is None:
        print("No earlier final release: there is nothing for roc bump to compare with.")
    else:
        previous_tag, previous_bundle = previous
        code, output = check_bump(previous_bundle, None if dry_run else version)
        if code != 0 and dry_run:
            print(f"::warning::roc bump could not compare the package with {previous_tag}; a release would stop here")
        elif code != 0 and baseline_unreadable(output) and is_major_bump(version, previous_tag):
            # A major release cannot understate any change, so a baseline that
            # no longer compiles, or whose API roc bump cannot read, does not
            # stop it. Any smaller release still needs the comparison.
            print(f"::warning::roc bump could not read {previous_tag}; {version} is a major release, so no change can be understated")
        elif code != 0:
            raise RuntimeError(f"roc bump refuses {version} against {previous_tag} ({previous_bundle})")

    prerelease = "true" if is_prerelease(version) else "false"
    print(f"version={version} prerelease={prerelease} notes={notes.relative_to(ROOT)}")
    if args.github_output:
        append_github_output(args.github_output, "version", version)
        append_github_output(args.github_output, "prerelease", prerelease)
        append_github_output(args.github_output, "notes", str(notes.relative_to(ROOT)))
    return 0


def bundle_into(directory: Path) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    before = set(directory.glob("*.tar.zst"))
    subprocess.run([sys.executable, str(ROOT / "scripts" / "bundle.py"), "--output-dir", str(directory)],
                   check=True, cwd=ROOT)
    created = sorted(set(directory.glob("*.tar.zst")) - before) or sorted(directory.glob("*.tar.zst"))
    if len(created) != 1:
        raise RuntimeError(f"expected one bundle in {directory}, found {[path.name for path in created]}")
    return created[0]


def release_files(directory: Path, version: str) -> dict[str, Path]:
    """The files `build` makes, by role, checking that each is there once."""
    dist = directory / "dist"
    bundles = sorted(dist.glob("*.tar.zst"))
    if len(bundles) != 1:
        raise RuntimeError(f"expected one bundle in {dist}, found {[path.name for path in bundles]}")
    files = {
        "bundle": bundles[0],
        "sbom": dist / f"{bundles[0].name}.spdx.json",
        "examples": dist / asset_name("examples-zip", version),
        "docs": dist / asset_name("docs-zip", version),
        "pdf": dist / asset_name("pdf", version),
        "notes": directory / "release-notes.md",
    }
    missing = [str(path) for path in files.values() if not path.is_file() or path.stat().st_size == 0]
    if missing:
        raise RuntimeError(f"release files are missing or empty: {', '.join(missing)}")
    unexpected = sorted({path.name for path in dist.iterdir()} - {files[key].name for key in files if key != "notes"})
    if unexpected:
        raise RuntimeError(f"unexpected files in {dist}: {', '.join(unexpected)}")
    return files


def cmd_build(args: argparse.Namespace) -> int:
    """Build every asset into <output-dir>/dist, the site into <output-dir>/site
    and the rendered notes as <output-dir>/release-notes.md."""
    version = args.release_version
    semver_key(version)
    notes = args.notes or (latest_notes() if version == DRY_RUN_VERSION else notes_path(version))
    output = args.output_dir.resolve()
    if output.exists():
        shutil.rmtree(output)
    dist = output / "dist"
    bundle = bundle_into(dist)
    url = release_asset_url(args.repo, version, bundle.name)
    package_examples(ROOT / "examples", url, version, dist / asset_name("examples-zip", version), args.repo)
    package_docs(version, dist, args.repo, os.environ.get("ROC", "roc"), site=output / "site")
    sbom = release_security_assets.generate_sbom(bundle=bundle, release_version=version, output_dir=dist)
    release_security_assets.verify_sbom(sbom=sbom, bundle=bundle, release_version=version)
    write_release_notes(version, bundle.name, notes, output / "release-notes.md", args.repo)
    release_files(output, version)
    for path in sorted(dist.iterdir()):
        print(f"{path.stat().st_size:>10}  {path.name}")
    if args.github_output:
        append_github_output(args.github_output, "bundle", bundle.name)
    return 0


def cmd_test(args: argparse.Namespace) -> int:
    """Test the exact files of a release build: the SBOM describes the bundle,
    the notes are rendered, and the examples archive runs against the bundle."""
    import test_bundle_examples  # noqa: PLC0415  (imports this module)

    version = args.release_version
    files = release_files(args.directory.resolve(), version)
    release_security_assets.verify_sbom(sbom=files["sbom"], bundle=files["bundle"], release_version=version)
    body = files["notes"].read_text(encoding="utf-8")
    if PLACEHOLDER in body or release_asset_url(args.repo, version, files["bundle"].name) not in body:
        raise RuntimeError("the rendered release notes do not name the bundle's release URL")
    with zipfile.ZipFile(files["docs"]) as docs:
        prefix = files["docs"].name.removesuffix(".zip")
        for required in ("index.html", "api/index.html", "404.html"):
            if f"{prefix}/{required}" not in docs.namelist():
                raise RuntimeError(f"{files['docs'].name} has no {required}")
    return test_bundle_examples.main(["--bundle-path", str(files["bundle"]), "--archive", str(files["examples"]),
                                      "--version", version, "--repo", args.repo])


if __name__ == "__main__":
    raise SystemExit(main())
