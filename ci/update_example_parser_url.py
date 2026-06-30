#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PACKAGE_DEPENDENCY_RE = re.compile(r'(?m)^(\s*parser:\s*)"[^"]+"')
BROKEN_EXAMPLES = {
    "markdown.roc": "latest nightly overflows the compiler stack while checking this example",
    "xml-svg.roc": "missing migrated roc-html dependency",
}


def update_example(example: Path, bundle_url: str) -> bool:
    source = example.read_text(encoding="utf-8")
    rewritten, count = PACKAGE_DEPENDENCY_RE.subn(
        lambda match: f'{match.group(1)}"{bundle_url}"',
        source,
        count=1,
    )

    if count != 1:
        raise SystemExit(f"{example.name} does not declare the expected parser package dependency")

    if rewritten == source:
        return False

    example.write_text(rewritten, encoding="utf-8")
    return True


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("bundle_url", help="Release URL for the parser package bundle")
    args = parser.parse_args()

    changed = False
    for example in sorted((ROOT / "examples").glob("*.roc")):
        if update_example(example, args.bundle_url):
            changed = True
            print(f"Updated {example.relative_to(ROOT)}")
        else:
            print(f"Already current: {example.relative_to(ROOT)}")

        if example.name in BROKEN_EXAMPLES:
            print(f"Note: {example.name} remains skipped in CI: {BROKEN_EXAMPLES[example.name]}.")

    sys.exit(0 if changed else 2)


if __name__ == "__main__":
    main()
