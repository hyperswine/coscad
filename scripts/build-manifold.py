#!/usr/bin/env python3
"""Build the explicit native kernel pin; never modify Cabal/user configuration.

Both pinned sources (Manifold and its CrossSection dependency Clipper2) are
checked out at the locked revisions; the kernel build itself never downloads.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess


def run(args, **kwargs):
    return subprocess.run(args, check=True, text=True, timeout=600, **kwargs)


def checkout(parser, pin, path):
    """Clone the pinned revision when missing; refuse a dirty or moved checkout."""
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        if 'tag' in pin:
            run(['git', 'clone', '--depth', '1', '--branch', pin['tag'], pin['repository'], str(path)])
        else:
            path.mkdir()
            run(['git', '-C', str(path), 'init', '-q'])
            run(['git', '-C', str(path), 'remote', 'add', 'origin', pin['repository']])
            run(['git', '-C', str(path), 'fetch', '-q', '--depth', '1', 'origin', pin['revision']])
            run(['git', '-C', str(path), 'checkout', '-q', '--detach', 'FETCH_HEAD'])
    revision = run(['git', '-C', str(path), 'rev-parse', 'HEAD'], capture_output=True).stdout.strip()
    dirty = run(['git', '-C', str(path), 'status', '--porcelain'], capture_output=True).stdout.strip()
    if revision != pin['revision'] or dirty:
        parser.error(f'{path} differs from the clean pinned revision {pin["revision"]}; refusing to build')
    return revision


def main():
    root = Path(__file__).resolve().parents[1]
    lock = json.loads((root / 'native-kernel.lock.json').read_text())
    pin = lock['manifold']
    clipper = lock['clipper2']
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-dir', type=Path, default=root / pin['layout'])
    parser.add_argument('--clipper2-source-dir', type=Path, default=root / clipper['layout'])
    parser.add_argument('--build-dir', type=Path, default=root / '.native/manifold-3.5.2/build')
    parser.add_argument('--prefix', type=Path, default=root / '.native/manifold-3.5.2/install')
    args = parser.parse_args()
    source, clipper_source, build, prefix = (
        p.resolve() for p in (args.source_dir, args.clipper2_source_dir, args.build_dir, args.prefix))
    for tree in (source, clipper_source):
        if tree == build or tree == prefix or tree in build.parents or tree in prefix.parents:
            parser.error('build and install directories must be outside the pinned source checkouts')
    revision = checkout(parser, pin, source)
    clipper_revision = checkout(parser, clipper, clipper_source)
    options = [f'-D{k}={v}' for k, v in lock['configuration'].items()]
    # Manifold's FetchContent step for Clipper2 reads the pre-populated checkout;
    # the disconnected flag turns any attempted download into a configure error.
    options += [f'-DFETCHCONTENT_SOURCE_DIR_CLIPPER2={clipper_source}',
                f'-DFETCHCONTENT_SOURCE_DIR_Clipper2={clipper_source}',
                '-DFETCHCONTENT_FULLY_DISCONNECTED=ON']
    run(['cmake', '-S', str(source), '-B', str(build), f'-DCMAKE_INSTALL_PREFIX={prefix}', *options])
    run(['cmake', '--build', str(build), '--parallel', '4'])
    run(['cmake', '--install', str(build)])
    libraries = sorted(prefix.glob('lib*/libmanifold.a'))
    if not libraries:
        libraries = sorted(prefix.glob('lib*/manifold.lib'))
    if len(libraries) != 1:
        parser.error('expected exactly one installed static Manifold library')
    clipper_libraries = sorted(prefix.glob('lib*/libClipper2.a')) or sorted(prefix.glob('lib*/Clipper2.lib'))
    if len(clipper_libraries) != 1:
        parser.error('expected exactly one installed static Clipper2 library (CrossSection backend)')
    pkgconfig = libraries[0].parent / 'pkgconfig'
    for name in ('manifold.pc', 'Clipper2.pc'):
        if not (pkgconfig / name).exists():
            parser.error(f'missing {name} under {pkgconfig}; the cabal manifold flag needs both')
    receipt = {'schema': 'coscad.native-kernel-build.v2', 'revision': revision,
               'clipper2_revision': clipper_revision,
               'configuration': lock['configuration'],
               'library_sha256': hashlib.sha256(libraries[0].read_bytes()).hexdigest(),
               'clipper2_library_sha256': hashlib.sha256(clipper_libraries[0].read_bytes()).hexdigest(),
               'cmake_version': run(['cmake', '--version'], capture_output=True).stdout.splitlines()[0]}
    (prefix / 'coscad-build-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print('Pinned static kernel installed:', prefix)
    print('Use PKG_CONFIG_PATH=' + str(pkgconfig) + ' with cabal build (the manifold flag is on by default)')


if __name__ == '__main__':
    main()
