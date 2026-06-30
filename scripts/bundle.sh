#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_dir="$root_dir/dist"
roc_bin="${ROC:-roc}"
args=()

if [[ "$roc_bin" == */* ]]; then
    roc_bin="$(cd "$(dirname "$roc_bin")" && pwd)/$(basename "$roc_bin")"
fi

while (($# > 0)); do
    case "$1" in
        --output-dir)
            output_dir="$2"
            shift 2
            ;;
        --output-dir=*)
            output_dir="${1#--output-dir=}"
            shift
            ;;
        *)
            args+=("$1")
            shift
            ;;
    esac
done

mkdir -p "$output_dir"
output_dir="$(cd "$output_dir" && pwd)"

cd "$root_dir/package"
roc_files=(main.roc)
for file in *.roc; do
    if [ "$file" != "main.roc" ]; then
        roc_files+=("$file")
    fi
done

if ((${#args[@]} > 0)); then
    "$roc_bin" bundle "${roc_files[@]}" --output-dir "$output_dir" "${args[@]}"
else
    "$roc_bin" bundle "${roc_files[@]}" --output-dir "$output_dir"
fi
