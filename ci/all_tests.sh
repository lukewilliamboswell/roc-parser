#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

if [ -z "${ROC:-}" ]; then
  echo "INFO: The ROC environment variable is not set, using the default roc command."
  export ROC=$(which roc)
fi

EXAMPLES_DIR='./examples'
PACKAGE_DIR='./package'

echo "test the package"
$ROC test package/Parser.roc
$ROC test package/CSV.roc
$ROC test package/HTTP.roc
$ROC test package/Markdown.roc
$ROC test package/String.roc
$ROC test package/Xml.roc

echo "roc check the examples"
for ROC_FILE in $EXAMPLES_DIR/*.roc; do
    $ROC check $ROC_FILE
done

echo "roc build the examples"
for ROC_FILE in $EXAMPLES_DIR/*.roc; do
    $ROC build $ROC_FILE --linker=legacy
done

echo "test building docs website"
$ROC docs $PACKAGE_DIR/main.roc
