#!/usr/bin/env bash
# Poll the repo; when main moved (or --force), rebuild the compiler and the
# manual site. Meant for a launchd/cron job on the machine that serves
# manual.cswine.cloud. Environment: REPO, WWW, BIN override the defaults.
set -euo pipefail
REPO=${REPO:-$HOME/srv/coscad}
WWW=${WWW:-$HOME/srv/manual-www}
BIN=${BIN:-$HOME/srv/bin}
# SSH and launchd do not necessarily inherit the same shell environment.
export PATH="$BIN:/opt/homebrew/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH"
lock="$REPO/.update-lock"
mkdir "$lock" 2>/dev/null || { echo "$(date -u +%FT%TZ) another update is running"; exit 0; }
trap 'rmdir "$lock"' EXIT
cd "$REPO"
git fetch -q origin main
local_rev=$(git rev-parse HEAD); remote_rev=$(git rev-parse origin/main)
if [ "$local_rev" = "$remote_rev" ] && [ -f "$WWW/index.html" ] && [ "${1:-}" != "--force" ]; then exit 0; fi
echo "$(date -u +%FT%TZ) updating ${local_rev:0:7} -> ${remote_rev:0:7}"
git reset -q --hard origin/main
mkdir -p "$BIN"
cabal update 2>&1 | tail -1
# Every build links the pinned geometry kernel (needs cmake, pkg-config, a C++17
# compiler); .native/ is git-ignored, so the reset above keeps the built kernel.
python3 scripts/build-manifold.py 2>&1 | tail -2
export PKG_CONFIG_PATH="$REPO/.native/manifold-3.5.2/install/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
cabal install exe:coscad --install-method=copy --overwrite-policy=always --installdir="$BIN" 2>&1 | tail -3
coscad doctor
scripts/build-manual.sh "$WWW"
echo "$(date -u +%FT%TZ) done"
