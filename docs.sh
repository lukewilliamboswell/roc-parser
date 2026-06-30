#!/usr/bin/env bash

set -euo pipefail

# Function to validate version number format (x.y.z)
validate_version() {
    if [[ ! $1 =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        echo "Error: Version number must be in format x.y.z (e.g., 0.12.0)"
        exit 1
    fi
}

# Check if version argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 0.11.0"
    exit 1
fi

VERSION="${1#v}"
DOCS_ROOT="${DOCS_ROOT:-www}"
ROC_BIN="${ROC:-roc}"

if [[ "$ROC_BIN" == */* ]]; then
    ROC_BIN="$(cd "$(dirname "$ROC_BIN")" && pwd)/$(basename "$ROC_BIN")"
fi

# Validate version number
validate_version "$VERSION"

rm -rf "$DOCS_ROOT/$VERSION"
mkdir -p "$DOCS_ROOT"

"$ROC_BIN" docs package/main.roc --output="$DOCS_ROOT/$VERSION"

cat > "$DOCS_ROOT/index.html" <<EOF
<!doctype html>
<html lang="en">
    <head>
        <meta charset="UTF-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Redirecting...</title>
        <script>
            window.location.href = "/roc-parser/$VERSION/";
        </script>
    </head>
    <body>
        <noscript>
            <p>
                If you are not automatically redirected, please
                <a href="/roc-parser/$VERSION/">click here</a>.
            </p>
        </noscript>
    </body>
</html>
EOF

echo "Generated docs for $VERSION in $DOCS_ROOT/$VERSION"
