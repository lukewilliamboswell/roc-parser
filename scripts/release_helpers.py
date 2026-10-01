#!/usr/bin/env python3
"""Repo-specific helpers for the roc-parser release workflow.

    release_helpers.py make-release-notes --release-version 2.0.0 \\
        --release-bundles .release/release-bundles.json --output-file notes.md
    release_helpers.py package-docs --release-version 2.0.0 --output-dir .release
    release_helpers.py assemble-pages --manual .docs-out/site \\
        --api .docs-api/2.0.0 --output .pages
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import zipfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

from workflow_helpers import resolve_bundle_url  # noqa: E402  (needs the sys.path entry above)

DEFAULT_REPO = "lukewilliamboswell/roc-parser"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subcommands = parser.add_subparsers(dest="command", required=True)

    notes = subcommands.add_parser(
        "make-release-notes",
        help="write the GitHub release notes: docs/releases/<version>.md plus the package URL and docs links",
    )
    notes.add_argument("--release-version", default="")
    notes.add_argument("--release-bundles", required=True)
    notes.add_argument("--output-file", required=True)
    notes.add_argument("--docs-url", default="")
    notes.add_argument("--bump-output", default=".release/bump-output.txt")
    notes.add_argument("--notes-dir", default="docs/releases")
    notes.set_defaults(func=cmd_make_release_notes)

    docs = subcommands.add_parser(
        "package-docs",
        help="build the manual (HTML and PDF) and the API reference, and package them as release assets",
    )
    docs.add_argument("--release-version", default="")
    docs.add_argument("--docs-version", default="")
    docs.add_argument("--roc", default="roc")
    docs.add_argument("--output-dir", default=".release")
    docs.set_defaults(func=cmd_package_docs)

    pages = subcommands.add_parser(
        "assemble-pages",
        help="lay out the Pages site: the manual at the root and the API reference under api/",
    )
    pages.add_argument("--manual", required=True, help="the built manual site (build_manual.py's site/)")
    pages.add_argument("--api", required=True, help="one version's API reference (build_docs.py's <root>/<version>/)")
    pages.add_argument("--repo", default="")
    pages.add_argument("--output", required=True)
    pages.set_defaults(func=cmd_assemble_pages)

    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except (RuntimeError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        return 1


# ------------------------------------------------------------- release notes


def read_editorial_notes(notes_dir: Path, release_version: str) -> str:
    """The hand-written notes for a release (`<version>.md`), as Markdown.

    Without a file the release gets a one-line introduction; an empty file is
    a mistake, not a choice, so it fails.
    """
    notes_path = notes_dir / f"{release_version}.md"
    if not notes_path.exists():
        return f"Release {release_version}."
    if not notes_path.is_file():
        raise RuntimeError(f"release notes path is not a file: {notes_path}")
    text = notes_path.read_text(encoding="utf-8").strip()
    if not text:
        raise RuntimeError(f"release notes are empty: {notes_path}")
    return text


def asset_name(kind: str, version: str) -> str:
    """The file name of one of the documentation assets a release carries."""
    names = {
        "manual-zip": f"roc-parser-manual-{version}.zip",
        "manual-pdf": f"roc-parser-manual-{version}.pdf",
        "api-zip": f"roc-parser-api-docs-{version}.zip",
    }
    return names[kind]


def release_asset_url(repo: str, release_version: str, artifact_file: str) -> str:
    return f"https://github.com/{repo}/releases/download/{release_version}/{artifact_file}"


def cmd_make_release_notes(args: argparse.Namespace) -> int:
    release_version = args.release_version or os.environ.get("RELEASE_VERSION", "")
    if not release_version:
        raise RuntimeError("release version is required")
    repo = os.environ.get("GITHUB_REPOSITORY", "")
    if not repo:
        raise RuntimeError("GITHUB_REPOSITORY is required")

    package_url = resolve_bundle_url(Path(args.release_bundles), repo, release_version)
    lines = [
        read_editorial_notes(Path(args.notes_dir), release_version),
        "",
        "## Using this release",
        "",
        "```roc",
        "app [main!] {",
        "    # your platform here",
        f'    parser: "{package_url}",',
        "}",
        "```",
    ]

    bump = Path(args.bump_output)
    if bump.is_file():
        bump_text = bump.read_text(encoding="utf-8").strip()
        if bump_text:
            lines.extend(["", "## Roc API changes", "", "```", bump_text, "```"])

    lines.extend(["", "## Docs", ""])
    docs_url = args.docs_url or os.environ.get("DOCS_URL", "")
    if docs_url:
        lines.append(f"- [The documentation online]({docs_url}), for the current release")
    pdf = release_asset_url(repo, release_version, asset_name("manual-pdf", release_version))
    manual_zip = release_asset_url(repo, release_version, asset_name("manual-zip", release_version))
    api_zip = release_asset_url(repo, release_version, asset_name("api-zip", release_version))
    lines.extend([
        f"- This release's own copy: [the manual as a PDF]({pdf}), and as zipped static sites,",
        f"  [the manual]({manual_zip}) and [the API reference]({api_zip}); unzip and open `index.html`",
    ])

    output = Path(args.output_file)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")
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
zipped static sites.</p>
</body>
</html>
"""


def cmd_assemble_pages(args: argparse.Namespace) -> int:
    """Lay out the Pages site for the current release only.

    The manual is the front page and the API reference is under `api/`.
    Earlier releases' documentation lives in the assets attached to each
    release, so the site never accumulates old versions; `404.html` says where
    an old version's pages (such as the `/<version>/` API snapshots the site
    used to serve) went.
    """
    manual = Path(args.manual)
    api = Path(args.api)
    output = Path(args.output)
    for source, what in ((manual, "manual"), (api, "API reference")):
        if not (source / "index.html").is_file():
            raise RuntimeError(f"no {what} index.html under {source}")
    if output.exists():
        shutil.rmtree(output)
    shutil.copytree(manual, output)
    if (output / "api").exists():
        raise RuntimeError("the manual already has an api/ directory")
    shutil.copytree(api, output / "api")
    repo = args.repo or os.environ.get("GITHUB_REPOSITORY", DEFAULT_REPO)
    owner, _, name = repo.partition("/")
    root = f"https://{owner}.github.io/{name}/"
    (output / "404.html").write_text(PAGES_NOT_FOUND.format(root=root, repo=repo), encoding="utf-8")
    print(output)
    return 0


def cmd_package_docs(args: argparse.Namespace) -> int:
    """Build the release's documentation and package it as release assets.

    Produces three assets, each named for the release:

    - `roc-parser-manual-<tag>.pdf`: the manual as one PDF;
    - `roc-parser-manual-<tag>.zip`: the manual as a static site, opening at
      `index.html`, with the PDF beside it;
    - `roc-parser-api-docs-<tag>.zip`: the `roc docs` API reference.

    Pages carries the latest of each; these keep every release's copy with the
    release itself. Prints the asset paths, one per line, for the publish step.
    """
    tag = args.release_version or os.environ.get("RELEASE_VERSION", "")
    if not tag.strip() or "/" in tag or "\\" in tag:
        raise RuntimeError(f"a valid release version is required, got {tag!r}")
    docs_version = args.docs_version or tag
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="roc-parser-release-docs-") as scratch:
        api_root = Path(scratch) / "api"
        subprocess.run(
            [sys.executable, str(ROOT / "scripts" / "build_docs.py"), "--roc", args.roc,
             "--docs-root", str(api_root), "--version", docs_version],
            check=True, cwd=ROOT,
        )
        api_zip = output_dir / asset_name("api-zip", tag)
        zip_tree(api_root / docs_version, api_zip, f"roc-parser-api-docs-{tag}")

    # The manual builds in a container that sees only the checkout, so its
    # output has to be inside it; `.docs-out/` is ignored.
    manual_root = ROOT / ".docs-out" / f"release-{tag}"
    try:
        subprocess.run(
            [sys.executable, str(ROOT / "scripts" / "build_manual.py"), "--pdf",
             "--docs-version", tag, "--output", str(manual_root)],
            check=True, cwd=ROOT,
        )
        pdf = output_dir / asset_name("manual-pdf", tag)
        shutil.copyfile(manual_root / "roc-parser.pdf", pdf)
        manual_zip = output_dir / asset_name("manual-zip", tag)
        zip_tree(manual_root / "site", manual_zip, f"roc-parser-manual-{tag}")
    finally:
        shutil.rmtree(manual_root, ignore_errors=True)

    for asset in (manual_zip, pdf, api_zip):
        print(asset)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
