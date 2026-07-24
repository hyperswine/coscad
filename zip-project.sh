#!/usr/bin/env bash

# zip-project.sh
# Zips everything in the current directory except .stack-work/

set -euo pipefail

if [ $# -ge 1 ]; then
  ARCHIVE_NAME="$1"
else
  TIMESTAMP=$(date +%Y%m%d_%H%M%S)
  ARCHIVE_NAME="coscad_${TIMESTAMP}.zip"
fi

echo "Creating: $ARCHIVE_NAME"
echo "Excluding: .stack-work/"

zip -r "$ARCHIVE_NAME" . \
  -x ".stack-work/*" \
  -x "*/.stack-work/*" \
  -x ".stack-work" \
  -x "bin/*" \
  -x ".vscode"

echo "✓ Created $ARCHIVE_NAME"
