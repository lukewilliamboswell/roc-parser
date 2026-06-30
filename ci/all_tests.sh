#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

ROC_BIN="${ROC:-roc}"

if [[ "$ROC_BIN" == */* ]]; then
    ROC_BIN="$(cd "$(dirname "$ROC_BIN")" && pwd)/$(basename "$ROC_BIN")"
fi

if [ -n "${ROC_PARSER_TMPDIR:-}" ]; then
    tmp_base="$ROC_PARSER_TMPDIR"
else
    tmp_base="$root_dir/.roc-parser-tmp"
fi
export ROC_PARSER_TMPDIR="$tmp_base"
export ROC="$ROC_BIN"

tmp_dir="$tmp_base/roc-parser-ci"
docs_dir="$tmp_dir/docs"
bundle_dir="$tmp_dir/bundle"

rm -rf "$tmp_dir"
mkdir -p "$docs_dir" "$bundle_dir"

echo "$("$ROC_BIN" version)"

echo ""
echo "Checking package..."
"$ROC_BIN" check package/main.roc

echo ""
echo "Running package tests..."
"$ROC_BIN" test package/Parser.roc
"$ROC_BIN" test package/CSV.roc
echo "Skipping package/HTTP.roc tests: latest nightly segfaults in the compiler while running this module's tests."
"$ROC_BIN" test package/Markdown.roc
"$ROC_BIN" test package/String.roc
"$ROC_BIN" test package/Xml.roc

echo ""
echo "Generating package docs..."
"$ROC_BIN" docs package/main.roc --output="$docs_dir"

case "$(uname -s)" in
    MINGW* | MSYS* | CYGWIN*)
        echo ""
        echo "Skipping package bundling on Windows."
        exit 0
        ;;
esac

echo ""
echo "Bundling package..."
BUNDLE_OUTPUT=$(scripts/bundle.sh --output-dir "$bundle_dir" 2>&1)
echo "$BUNDLE_OUTPUT"
BUNDLE_PATH=$(echo "$BUNDLE_OUTPUT" | grep "^Created:" | awk '{print $2}')

if [ -z "$BUNDLE_PATH" ]; then
    echo "Error: could not extract bundle path from roc bundle output"
    exit 1
fi

echo ""
echo "Testing examples against localhost bundle..."
python3 ci/test_bundle_examples.py --bundle-path "$BUNDLE_PATH"
