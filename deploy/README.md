# Deploying the manual (manual.cswine.cloud)

The manual is a static site built from this repo by `scripts/build-manual.sh`:
README and `docs/*.md` (pandoc), `man coscad` (mandoc), and the build-plan
companion site with renders under `/builds` (`coscad site`).

**Serving machine** (mac mini, on the tailnet): a checkout at `~/srv/coscad`,
the compiler installed to `~/srv/bin`, the site at `~/srv/manual-www`, nginx
serving it on port 6210 (`manual-nginx-mini.conf`), and a launchd job
(`cloud.cswine.manual-update.plist`) running `scripts/manual-update.sh` every
5 minutes: it fetches `origin/main`, and when it moved, resets the checkout,
rebuilds the compiler (`stack install`), and rebuilds the site atomically.
`bash scripts/manual-update.sh --force` rebuilds regardless. Logs:
`~/srv/logs/manual-update.log`.

**Public host** (Linode): nginx terminates TLS for manual.cswine.cloud and
proxies to the mini over Tailscale (`proxy_pass http://100.107.153.43:6210`).

Pushing to `main` updates https://manual.cswine.cloud within about five minutes plus
build time (a minute or two: the plan renders go through OpenSCAD).
