# Deploying the manual (manual.cswine.cloud)

The manual is a static site built from this repo by `scripts/build-manual.sol`:
README and `docs/*.md` (pandoc), `man coscad` (mandoc), and the build-plan
companion site with renders under `/builds` (`coscad site`).

Build it locally with `sol scripts/build-manual.sol OUTDIR` from the checkout,
with a current `coscad` first on PATH (the builder stages the site beside OUTDIR
and swaps it in with two renames).

**Serving machine** (mac mini, on the tailnet): a checkout at `~/srv/coscad`,
the compiler installed to `~/srv/bin`, the site at `~/srv/manual-www`, nginx
serving it on port 6210 (`manual-nginx-mini.conf`), and a launchd job
(`cloud.cswine.manual-update.plist`, kept in this folder) running
`sol scripts/manual-update.sol` in the checkout every 5 minutes: it fetches
`origin/main`, and when it moved, resets the checkout, rebuilds the compiler
(`cabal install`), and rebuilds the site. `sol scripts/manual-update.sol --force`
(run from `~/srv/coscad`, with the plist's PATH) rebuilds regardless.
Logs: `~/srv/logs/manual-update.log`.

**Host prerequisites** (Homebrew on the mini): `git-lfs` (print photos are LFS
objects; without it the checkout fails and the job never updates), `cmake` and
`pkg-config` (the pinned geometry kernel that every coscad build links), pandoc,
GHC 9.8 with cabal from ghcup, and the FP-RISC 0.13.1 toolchain at
`~/.fpr/0.13.1` for `sol`. The toolchain is self-contained, so a copy of
another Mac's `~/.fpr/0.13.1` works. Run `git lfs install` once. After changing
`cloud.cswine.manual-update.plist`, copy it to `~/Library/LaunchAgents/` and
reload it (`launchctl unload` then `launchctl load` on that path).

**Public host** (Linode): nginx terminates TLS for manual.cswine.cloud and
proxies to the mini over Tailscale (`proxy_pass http://100.107.153.43:6210`).

Pushing to `main` updates https://manual.cswine.cloud within about five minutes plus
build time (a minute or two for the compiler build and the native plan renders;
since 2026-10-29 the site needs no OpenSCAD or BOSL2, only pandoc and mandoc).
