#!/usr/bin/env bash
# Build the CoScad manual site into OUTDIR (atomically): README and docs/*.md
# as HTML, the man page, and the build-plan companion site for the example
# assemblies under /builds. Needs: pandoc, mandoc, coscad (with OpenSCAD +
# BOSL2 reachable, see `coscad doctor`).
#   scripts/build-manual.sh ~/srv/manual-www
set -euo pipefail
out=${1:?usage: build-manual.sh OUTDIR}
repo=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d "${out%/}.tmp.XXXX")
trap 'rm -rf "$tmp"' EXIT
cd "$repo"
commit=$(git rev-parse --short HEAD 2>/dev/null || echo dev)
stamp=$(date -u +"%Y-%m-%d %H:%MZ")

pages=(README:Overview docs/LANGEXTENSION:Language docs/TOPOLOGICAL:Topological docs/MANUFACTURING:Manufacturing docs/PLAN:Build-plans docs/SYNTAX:Syntax-modes docs/EXAMPLES:Examples CHANGELOG:Changelog)
nav="$tmp/nav.html"
{
  echo '<nav class="top"><a class="brand" href="index.html">CoScad manual</a>'
  for p in "${pages[@]}"; do f=${p%%:*}; t=${p##*:}; n=$(basename "$f" | tr 'A-Z' 'a-z'); [ "$n" = readme ] && n=index; echo "<a href=\"$n.html\">${t//-/ }</a>"; done
  echo '<a href="man.html">man coscad</a><a href="builds/index.html">Builds</a></nav>'
} > "$nav"
cp deploy/manual.css "$tmp/manual.css"
foot="$tmp/foot.html"
echo "<footer>coscad $commit · built $stamp · <a href=\"https://github.com/hyperswine/coscad\">source</a></footer>" > "$foot"

for p in "${pages[@]}"; do
  f=${p%%:*}; t=${p##*:}; n=$(basename "$f" | tr 'A-Z' 'a-z'); [ "$n" = readme ] && n=index
  pandoc -s -f gfm -t html5 --css manual.css --metadata title="${t//-/ } · CoScad" \
    -B "$nav" -A "$foot" "$f.md" -o "$tmp/$n.html"
done
# man page, wrapped in the same chrome
mandoc -T html -O fragment man/coscad.1 > "$tmp/man.body"
{ echo '<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>man coscad</title><link rel="stylesheet" href="manual.css"></head><body>'
  cat "$nav"; cat "$tmp/man.body"; cat "$foot"; echo '</body></html>'; } > "$tmp/man.html"
rm -f "$tmp/man.body" "$tmp/nav.html" "$tmp/foot.html"

# build plans with renders, from a scratch copy of the example assemblies
work=$(mktemp -d)
cp -R examples/assemble/plan "$work/plan"; cp -R examples/assemble/bow3 "$work/bow3"; cp -R examples/assemble/ball "$work/ball"; cp -R examples/assemble/tesseract "$work/tesseract"
coscad site "$tmp/builds" "$work/plan/corner_pair.assemble" "$work/plan/cube.assemble" "$work/bow3/bow3.assemble" "$work/ball/ball.assemble" "$work/tesseract/tesseract.assemble"
rm -rf "$work"
echo "{\"commit\": \"$commit\", \"built\": \"$stamp\"}" > "$tmp/version.json"

rm -rf "${out%/}.old"; [ -d "$out" ] && mv "$out" "${out%/}.old"; mv "$tmp" "$out"; rm -rf "${out%/}.old"
trap - EXIT
echo "manual built at $out ($commit)"
