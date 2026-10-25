#!/usr/bin/env python3
"""Build roc-parser's documentation site and its release assets.

    release_helpers.py package-docs --release-version 3.1.0 --output-dir dist --site .docs-out/release/site
    release_helpers.py assemble-pages --manual .docs-out/site --api .docs-api/3.1.0 --output .pages

The manual is rendered by Asciidoctor in Docker (scripts/build_manual.py) and
the API reference by `roc docs` (scripts/build_docs.py); `package-docs` lays
them out as one site and packages the site and the PDF. Everything else a
release needs, the bundle, the examples archive, the notes and the publisher,
is roc-automation's shared release workflow (.github/workflows/release.yml).
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

DEFAULT_REPO = "lukewilliamboswell/roc-parser"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    subcommands = parser.add_subparsers(dest="command", required=True)

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
    args.repo = args.repo or os.environ.get("GITHUB_REPOSITORY", DEFAULT_REPO)
    try:
        return args.func(args)
    except (RuntimeError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        return 1


def asset_name(kind: str, version: str) -> str:
    """The file name of one of the documentation assets a release carries."""
    names = {
        "docs-zip": f"roc-parser-docs-{version}.zip",
        "pdf": f"roc-parser-{version}.pdf",
    }
    return names[kind]


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


if __name__ == "__main__":
    raise SystemExit(main())
