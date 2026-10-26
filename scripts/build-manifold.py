#!/usr/bin/env python3
"""Build the explicit native kernel pin; never modify Cabal/user configuration."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess


def run(args, **kwargs):
    return subprocess.run(args, check=True, text=True, timeout=600, **kwargs)


def main():
    root = Path(__file__).resolve().parents[1]
    lock = json.loads((root / 'native-kernel.lock.json').read_text())
    pin = lock['manifold']
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-dir', type=Path, default=root / pin['layout'])
    parser.add_argument('--build-dir', type=Path, default=root / '.native/manifold-3.5.2/build')
    parser.add_argument('--prefix', type=Path, default=root / '.native/manifold-3.5.2/install')
    args = parser.parse_args()
    source, build, prefix = (p.resolve() for p in (args.source_dir, args.build_dir, args.prefix))
    if source == build or source == prefix or source in build.parents or source in prefix.parents:
        parser.error('build and install directories must be outside the pinned source checkout')
    if not source.exists():
        source.parent.mkdir(parents=True, exist_ok=True)
        run(['git', 'clone', '--depth', '1', '--branch', pin['tag'], pin['repository'], str(source)])
    revision = run(['git', '-C', str(source), 'rev-parse', 'HEAD'], capture_output=True).stdout.strip()
    dirty = run(['git', '-C', str(source), 'status', '--porcelain'], capture_output=True).stdout.strip()
    if revision != pin['revision'] or dirty:
        parser.error('Manifold checkout differs from the clean pinned revision; refusing to build')
    options = [f'-D{k}={v}' for k, v in lock['configuration'].items()]
    run(['cmake', '-S', str(source), '-B', str(build), f'-DCMAKE_INSTALL_PREFIX={prefix}', *options])
    run(['cmake', '--build', str(build), '--parallel', '4'])
    run(['cmake', '--install', str(build)])
    libraries = sorted(prefix.glob('lib*/libmanifold.a'))
    if not libraries:
        libraries = sorted(prefix.glob('lib*/manifold.lib'))
    if len(libraries) != 1:
        parser.error('expected exactly one installed static Manifold library')
    receipt = {'schema': 'coscad.native-kernel-build.v1', 'revision': revision,
               'configuration': lock['configuration'],
               'library_sha256': hashlib.sha256(libraries[0].read_bytes()).hexdigest(),
               'cmake_version': run(['cmake', '--version'], capture_output=True).stdout.splitlines()[0]}
    (prefix / 'coscad-build-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print('Pinned static kernel installed:', prefix)
    print('Use PKG_CONFIG_PATH=' + str(libraries[0].parent / 'pkgconfig')
          + ' with cabal build -fmanifold --builddir=dist-native')


if __name__ == '__main__':
    main()
