# Clipper2

The pinned Manifold kernel is built with its CrossSection module, which links
[Clipper2](https://github.com/AngusJohnson/Clipper2) at revision
`46f639177fe418f9689e8ddb74f08a870c71f5b4` (v2.0.1, Manifold 3.5.2's own pin;
see `native-kernel.lock.json`). CoScad uses it for round 2D offsets. Clipper2
is distributed under the Boost Software License 1.0, reproduced in `LICENSE`.

`CPP/Clipper2Lib/include` and `CPP/Clipper2Lib/src` are the upstream trees at
that revision, compiled into the `coscad` library by `cxx-sources` in
`coscad.cabal`. To move the pin: copy those two directories from the new
revision, update `native-kernel.lock.json`, and run `coscad doctor`.
