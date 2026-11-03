# Manifold kernel (vendored)

CoScad links Manifold, licensed under Apache-2.0. Source:
<https://github.com/elalish/manifold>. Exact revision and the configuration the
`-D` flags mirror: [`native-kernel.lock.json`](../../native-kernel.lock.json).

`include/manifold` and `src/` are the upstream trees at that revision, compiled
into the `coscad` library by `cxx-sources` in `coscad.cabal`, so `cabal build`
downloads nothing and needs no CMake. Only what a static, sequential,
CrossSection-enabled build uses is kept: `src/cross_section/cross_section_boolean2.cpp`
(the experimental backend) is omitted. `include/manifold/version.h` is generated
upstream by `cmake/version.h.in` and is checked in here instead, because CMake no
longer runs; `cbits/coscad_manifold.cpp` asserts it against the lock.

To move the pin: copy `include/manifold/` and `src/` from the new revision,
regenerate `version.h` from its `cmake/version.h.in`, update
`native-kernel.lock.json` and the `-D` flags in `coscad.cabal`, then run
`coscad doctor`. Include the accompanying license with distributions that link
the kernel; release archives ship the Manifold, Clipper2 and BOSL2 notices under
`third-party/` together with the native-kernel lock.
