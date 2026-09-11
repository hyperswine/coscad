#!/usr/bin/env bash
# Point Formula/coscad.rb at a tagged release and fill in its sha256.
#   scripts/update-formula.sh 1.1.0.0
set -euo pipefail
ver=${1:?usage: update-formula.sh <version, e.g. 1.1.0.0>}
url="https://github.com/hyperswine/coscad/archive/refs/tags/v${ver}.tar.gz"
tmp=$(mktemp)
curl -fsSL "$url" -o "$tmp"
sha=$(shasum -a 256 "$tmp" | cut -d' ' -f1)
rm -f "$tmp"
f=$(dirname "$0")/../Formula/coscad.rb
sed -i.bak -E "s|^  url \".*\"$|  url \"${url}\"|; s|^  sha256 \"[0-9a-f]+\".*$|  sha256 \"${sha}\"|" "$f"
rm -f "$f.bak"
echo "Formula/coscad.rb -> v${ver} (${sha})"
