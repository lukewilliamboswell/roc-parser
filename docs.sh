#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

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
    echo "Example: $0 0.12.0"
    exit 1
fi

VERSION=$1

# Validate version number
validate_version "$VERSION"

# Run roc docs with validated version
roc docs --root-dir "/roc-parser/$VERSION/" package/main.roc

# Create new version directory in www/
mkdir www/$VERSION

# Move generated docs to version directory
mv generated-docs/* www/$VERSION
