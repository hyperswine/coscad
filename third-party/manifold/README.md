# Optional Manifold kernel

CoScad's default `-fmanifold` build links Manifold, licensed under Apache-2.0.
Source: <https://github.com/elalish/manifold>. Exact revision and build options:
[`native-kernel.lock.json`](../../native-kernel.lock.json).

The source is fetched separately; the owned ABI shim is `cbits/coscad_manifold.cpp`.
No Manifold implementation source is vendored here. Include the accompanying
license with distributions that link the kernel. Release archives include
Manifold, Clipper2 and BOSL2 notices under `third-party/`, together with the
native-kernel lock and build provenance.
