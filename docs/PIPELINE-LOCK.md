# Pipeline pins and recovery

## Current baseline

`pipeline.lock.json` records the actual working Mac pipeline. Repository pins
use QOS's `{repository, revision, layout}` format, with an additional Git tree
ID. They name clean FP-RISC, `3d-models`/bambu-lan and BOSL2 checkouts. The CoScad
compiler/script/editor inputs have a content fingerprint: the lock does not pin
itself, and editing a CAD design does not require repinning the tools.
`baseline_source_revision` records the CoScad commit observed when capturing; it
is provenance, not a requirement that HEAD remain at the pre-lock commit.

The current source checkout and deployed printer host are **different baselines**:
FP-RISC source is `dab03e396b4005349d0d2c18ff29de578ff3a829`; the running host's
build receipt names `9782b38420bb24c5b61d095adecfaea93abd3be2`. The lock retains
both, plus the deployed executable hash. It does not rebuild or restart the host.

The inventory also captures:

- GHC 9.8.2, Cabal 3.16.1.0, FP-RISC 0.12.0 and Apple clang 21.0.0;
  executable hashes, OS/architecture and the Cabal freeze file.
- OpenSCAD 2025.05.16 and its complete application bundle.
- BOSL2 commit `e173fa0ae45f9e2082e0d8c0382dae98621b7c20` (2.0.766).
- Bambu Studio 02.08.02.61, its complete application bundle and all 2,516 files
  in the currently selected vendor BBL profile directory. Updating profiles
  independently of Studio is detected.
- Node 22.13.0, npm 10.9.2, npm/installed extension module contents, the extension dependency lock and a newly explicit
  `@vscode/vsce` 4.0.0 packaging dependency. Its Node requirement is >=22;
  CI now selects the same exact Node/Cabal versions as the observed host.
- OpenSSL 3.6.5 headers and linked libssl/libcrypto contents, public printer CA,
  public printer configuration and the deployed printer host binary.
- Python 3.15.0 and exact versions of the four packages used by the existing
  LAN/test environment, including their installed distribution contents.
  The native production printer host does not use Python.
- Pandoc, mandoc, Tailscale 1.104.1 and the printer site's public nginx/renewal
  configuration and installed nginx/OpenSSL/Certbot package versions on Linode.

`ci-actions.lock.json` records full GitHub action commit IDs. Workflows use
those IDs directly, fetch the same exact BOSL2 commit, and package with the
locked local VSIX tool rather than downloading an unversioned npx tool.

## Commands

From the CoScad root:

```sh
python3 scripts/pipeline-lock.py --check
python3 scripts/pipeline-lock.py --scope source --scope render --check
python3 scripts/pipeline-lock.py --scope printer --check
python3 scripts/pipeline-lock.py --check-ci
python3 scripts/pipeline-lock.py --scope source --scope render --run coscad doctor
```

The complete check includes SSH to `root@localhost-0`. Version probes have a
30-second deadline. An unavailable dependency is a refusal, not an invitation
to download a replacement or silently accept a different version. Scope checks
are explicit subsets; passing a subset does not certify the remaining scopes.
The inventory is currently a **macOS/arm64 baseline**, not a cross-platform
binary lock. Pure CI policy/refusal tests run without these host dependencies.

Machine paths are resolved from the script's defaults and an optional ignored
`pipeline.paths.local.json`. For example:

```json
{
  "fprisc": "../fprisc",
  "bambu_lan": "../3d-models",
  "bosl2": "../BOSL2",
  "coscad_binary": "bin/coscad",
  "test_python": "/tmp/printer-dashboard-venv/bin/python"
}
```

The root of `bambu_lan` is the Git repository; its actual upload scripts are
under `bambu-lan/`. Relocating identical dependencies is allowed. A working
checkout must match its pinned revision and be clean, as in QOS. Use an isolated
checkout when actively developing FP-RISC alongside a pinned pipeline.

The checked runner supports named tools, passes arguments without a shell and
sets the renderer, BOSL2, Bambu Studio, profiles and LAN-script paths explicitly.
It does not add print commands or retry printer operations. Existing direct
`coscad`/script invocations still work and bypass this guard; use the checked
entry point for the pinned workflow. Hash checking before execution is drift
detection, not filesystem immutability or protection against a malicious updater.

Python environments can be recreated without changing selected package versions:

```sh
python3 -m pip install --require-hashes -r scripts/requirements.lock
# Test environment only:
python3 -m pip install --require-hashes -r scripts/requirements-test.lock
```

These files include published artifact hashes for the pinned versions, including
platform wheels and source archives. Use wheels if reproducible native package
builds are required; a source archive hash does not pin its external compiler.

## Deliberate upgrades and rollback

1. Save the previous lock in version control and retain the matching executable,
   app bundles, vendor profiles and public CA/configuration in an independent
   rollback store. The lock is a receipt and verifier, **not an artifact archive**.
2. Change one dependency deliberately. For source repositories, commit it first.
   Update workflow pins when changing BOSL2 or an action/tool version.
3. Run compiler/geometry, slicing, editor and native printer acceptance checks
   appropriate to that change. A successful lock check is not a geometry or
   physical-print certificate.
4. Run `python3 scripts/pipeline-lock.py --pin`, review its diff and commit it.
   Capture requires two identical complete inventories. A failed capture leaves
   the previous lock intact; concurrent pin writers are refused. The OS-owned
   writer lock releases on process death. Publication uses a flushed temporary
   file, atomic replacement and directory fsync on the baseline host.
5. To roll back, restore the old artifacts/checkouts **and** old lock, then check.
   Do not respond to an unexpected mismatch by automatically repinning.

No access codes, private login keys, environment contents, live session tokens
or TLS private keys are read or hashed by this inventory. The SSH probe is
limited to package versions and three public configuration files for this site.
Auth allowlists and TLS leaf certificates are operational policy/renewable
identity and are not independently hashed here. The whole source-repository pin
still includes its checked-in example allowlist. For independent live key
rotation, move deployed policy outside the pinned checkout using `--auth`; until
then, editing its tracked allowlist requires a new source commit and source pin.
Printer-only checks remain independent of source changes. Certificate renewal
does not require treating an old TLS leaf certificate as a build dependency.

GitHub-hosted runner images and their apt/Chocolatey installation environments
remain managed infrastructure. Exact actions, GHC/Cabal/Node, BOSL2 and dependency
locks do not make those images hermetic. A follow-up should use versioned,
checksum-verified renderer artifacts for each CI platform and record runner image
receipts. Printer firmware is not frozen by this lock; approved firmware and
capability observations need a separate serial-bound compatibility record.

## Hardening and graceful recovery work

The lock guards and tests above are implemented. The following are the next
operational contracts, **not claims that a durable print coordinator exists**:

Geometry regression also now fails on a missing expected baseline instead of
silently recording a new result in an ordinary test run. Baseline updates need
the explicit review flag. The prior Tesseract redesign's three outdated mesh
snapshots were checked against its changed dimensions and corrected; its newly
introduced parts now have explicit geometry entries too.

| Boundary | Required recovery behavior | Acceptance check |
| --- | --- | --- |
| Build or render interrupted | Write into a unique staging directory; publish a complete immutable build bundle only after required checks pass. Retain the previous valid bundle. | Kill the worker during each output; no partial bundle becomes selectable. |
| Preflight | Require `coscad check`, compatible printer/nozzle/material/bed, fresh telemetry and a clear bed confirmation before dispatch. Reject significant slicer warnings. | Deliberate overlap, wrong profile and warning cases never reach upload. |
| Upload times out | Record `upload_unknown`; verify the remote file and local 3MF identity before deciding whether upload can be retried. Do not start from a partial artifact. | Drop FTPS before and after final acknowledgement; neither case starts an unverified file. |
| Start command has no acknowledgement | Persist the intent first; enter `start_unknown` and reconcile fresh printer job identity. Never blindly resend a start after reconnect/restart. | Lose the acknowledgement after a real/simulated start; at most one start is dispatched by the coordinator. |
| Telemetry disconnects | Keep last-known values visibly stale; bound waiting, reconnect with backoff and return `unknown` when completion cannot be established. | Disconnect during PREPARE, RUNNING and FINISH; stale cached FINISH never completes another job. |
| Host/actor fails | Restart the read-only services with bounded backoff; supervise telemetry/camera actors independently, restore job journal and reconcile. Revoke in-memory sessions. | Kill the host and individual services under slow viewers; no automatic printer operation and no permanent lost service. |
| Printer address changes | Rediscover the recorded serial and verify its CA/certificate identity before rebinding. Never trust a broadcast alone. | Another device advertises a known model or wrong serial/certificate; refuse it. |
| Print finishes | Record `printer_finished`, then await physical inspection/bed clearance separately. Partial reprints retain original build/part identities. | A FINISH report cannot silently mark fit, adhesion, quality or bed clearance as verified. |
| Control added to public UI | Retain verified key identity in sessions; enforce viewer/operator permissions, per-printer reservations and an audit trail. Keep raw G-code off the public API. | Viewer keys, duplicate submits and stale approvals cannot dispatch operations. |
| Long operation or shutdown | Deadlines cover slicer, upload, acknowledgements and whole jobs; bound output/frame/mailbox sizes, cancel child process groups and flush durable events. | Hung subprocess, full stderr, oversized frame and power loss produce a bounded, diagnosable outcome. |

The existing print runner still lacks a durable stage journal and bounded
freshness-aware completion/reconciliation. Its old MQTT/camera/FTPS helpers also
use weaker TLS policy than the native dashboard. Those are higher priorities
than adding automatic reprints. Automatic boot supervision and a long real-device
soak remain separate deployment work; this pinning change installs neither.

Run offline lock/refusal tests with `python3 scripts/test-pipeline-lock.py`.
