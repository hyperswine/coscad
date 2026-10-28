#!/usr/bin/env python3
"""Package a tested executable without retaining source-tree dependencies."""
import argparse
import hashlib
import json
import platform
from pathlib import Path
import shutil
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--binary', type=Path, required=True)
    parser.add_argument('--output', type=Path, default=ROOT / 'dist-release')
    args = parser.parse_args()
    binary = args.binary.resolve()
    version = subprocess.check_output([binary, '--version'], text=True).strip().removeprefix('coscad ')
    system = 'macos' if platform.system() == 'Darwin' else 'linux'
    arch = {'aarch64': 'arm64'}.get(platform.machine(), platform.machine())
    name = f'coscad-{system}-{arch}'
    args.output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='coscad-release-') as tmp:
        stage = Path(tmp) / name
        stage.mkdir()
        shutil.copy2(binary, stage / 'coscad')
        for item in ('README.md', 'LICENSE', 'CHANGELOG.md', 'examples', 'docs', 'man', 'third-party', 'native-kernel.lock.json'):
            source = ROOT / item
            if source.is_dir():
                shutil.copytree(source, stage / item,
                                ignore=shutil.ignore_patterns('dist', '__pycache__', '.DS_Store'))
            else:
                shutil.copy2(source, stage / item)
        clipper = stage / 'third-party' / 'clipper2'
        clipper.mkdir()
        shutil.copy2(ROOT / '.native/clipper2/checkout/LICENSE', clipper / 'LICENSE')
        capabilities = json.loads(subprocess.check_output([binary, 'capabilities', '--json'], text=True))
        if capabilities['default_backend'] != 'native-experimental':
            raise SystemExit('Release must include the default pinned native kernel')
        metadata = {'name': 'coscad', 'version': version, 'system': system, 'arch': arch,
                    'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                    'minimum_macos': '15.0' if system == 'macos' else None}
        metadata['native_kernel'] = json.loads((ROOT / 'native-kernel.lock.json').read_text())
        if system == 'macos':
            linked = subprocess.check_output(['otool', '-L', binary], text=True)
            libraries = [line.strip().split(' (', 1)[0] for line in linked.splitlines()[1:]]
            unexpected = [lib for lib in libraries if not lib.startswith(('/usr/lib/', '/System/Library/'))]
            if unexpected:
                raise SystemExit(f'Unexpected non-system runtime libraries: {unexpected}')
            metadata['runtime_libraries'] = libraries
        (stage / 'release.json').write_text(json.dumps(metadata, indent=2) + '\n')
        archive = args.output / f'{name}.tar.gz'
        with tarfile.open(archive, 'w:gz') as output:
            output.add(stage, arcname=name)
        digest = hashlib.sha256(archive.read_bytes()).hexdigest()
        archive.with_suffix(archive.suffix + '.sha256').write_text(f'{digest}  {archive.name}\n')
        print(archive)


if __name__ == '__main__':
    main()
