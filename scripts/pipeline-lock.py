#!/usr/bin/env python3
"""Explicit pipeline pins. --pin captures; --check refuses drift; --run checks first.
No credential files, environment dumps, printer operations or implicit upgrades.
"""
from __future__ import annotations
import argparse
import contextlib
import hashlib
import json
import os
from pathlib import Path
import platform
import plistlib
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
LOCK = ROOT / 'pipeline.lock.json'
LOCAL = ROOT / 'pipeline.paths.local.json'
SCOPES = ('source', 'build', 'render', 'slice', 'editor', 'printer', 'manual', 'test', 'infrastructure')
PACKAGES = ('paho-mqtt', 'cryptography', 'cffi', 'pycparser')

class Refused(Exception): pass

def output(args, cwd=None):
    try:
        r = subprocess.run([str(a) for a in args], cwd=cwd, capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        raise Refused('cannot run bounded version/source probe: ' + str(args[0])) from None
    if r.returncode:
        raise Refused('version/source probe failed: ' + str(args[0]))
    return (r.stdout or r.stderr).strip()

def sha(path):
    path = Path(path)
    before = path.stat()
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(1024 * 1024), b''): h.update(block)
    after = path.stat()
    if (before.st_size, before.st_mtime_ns, before.st_ino) != (after.st_size, after.st_mtime_ns, after.st_ino):
        raise Refused('file changed during inventory: ' + str(path))
    return h.hexdigest()

def tree(path):
    """Sorted content+relative-name digest, including linked file contents.
    Links resolving outside the declared tree are refused, not silently omitted.
    """
    path = Path(path).resolve()
    if not path.is_dir(): raise Refused('missing dependency directory: ' + str(path))
    h = hashlib.sha256(); count = 0
    def walk(folder, active):
        nonlocal count
        real = folder.resolve()
        if real in active: raise Refused('cyclic dependency symlink: ' + str(folder))
        for entry in sorted(folder.iterdir(), key=lambda p: p.name):
            if entry.name in ('.git', '.DS_Store', '__pycache__'): continue
            target = entry.resolve()
            if not target.is_relative_to(path): raise Refused('external dependency symlink: ' + str(entry))
            relative = entry.relative_to(path).as_posix()
            if entry.is_symlink():
                h.update(b'L\0' + relative.encode() + b'\0' + os.readlink(entry).encode() + b'\0')
            if entry.is_dir(): walk(entry, active | {real})
            elif entry.is_file():
                h.update(b'F\0' + relative.encode() + b'\0' + sha(entry).encode() + b'\0'); count += 1
            else: raise Refused('unsupported dependency file: ' + str(entry))
    walk(path, set())
    return {'sha256':h.hexdigest(), 'files':count}

def git(path, *args): return output(['git', '-C', path, *args])

def repo(path, name):
    if git(path, 'status', '--porcelain', '--untracked-files=normal'):
        raise Refused(name + ' has uncommitted changes; commit or isolate them before pinning/checking')
    return {'repository':name, 'revision':git(path, 'rev-parse', 'HEAD'), 'layout':1,
            'tree':git(path, 'rev-parse', 'HEAD^{tree}')}

def source(path):
    names = git(path, 'ls-files', '--cached', '--others', '--exclude-standard', '-z').split('\0')
    h = hashlib.sha256(); count = 0
    for name in sorted(set(names)):
        # Lock the tools, not the user's evolving CAD designs or generated plans.
        if not (name.startswith(('src/','app/','scripts/','vscode/','test/','.github/')) or name in
                ('coscad.cabal','cabal.project','cabal.project.freeze','ci-actions.lock.json')): continue
        file = Path(path) / name
        if not file.is_file(): raise Refused('missing source file: ' + name)
        h.update(name.encode() + b'\0' + sha(file).encode() + b'\0'); count += 1
    return {'sha256':h.hexdigest(), 'files':count}

def paths(overrides=None):
    home = Path.home()
    def binary(name):
        value = shutil.which(name)
        return value if value else '/missing/' + name
    result = {
        'fprisc':str(ROOT.parent/'fprisc'), 'bambu_lan':str(ROOT.parent/'3d-models'),
        'fpr_compiler':str(ROOT.parent/'fprisc/fpr'),
        'bosl2':os.environ.get('COSCAD_BOSL2', str(home/'Documents/OpenSCAD/libraries/BOSL2')),
        'ghc':binary('ghc'), 'cabal':binary('cabal'), 'clang':binary('clang'),
        'openscad':os.environ.get('COSCAD_OPENSCAD', '/Applications/OpenSCAD.app/Contents/MacOS/openscad'),
        'openscad_bundle':'/Applications/OpenSCAD.app',
        'bambustudio':os.environ.get('BAMBU_STUDIO','/Applications/BambuStudio.app/Contents/MacOS/BambuStudio'),
        'bambustudio_bundle':'/Applications/BambuStudio.app',
        'bambu_profiles':os.environ.get('BAMBU_PROFILES',str(home/'Library/Application Support/BambuStudio/system/BBL')),
        'openssl':'/opt/homebrew/opt/openssl@3', 'node':binary('node'), 'npm':binary('npm'),
        'python':sys.executable, 'test_python':'/tmp/printer-dashboard-venv/bin/python',
        'pandoc':binary('pandoc'), 'mandoc':binary('mandoc'),
        'tailscale':binary('tailscale'),
        'native_host':str(ROOT.parent/'fprisc/build/printer-host'),
        'printer_config':str(ROOT.parent/'fprisc/build/printer-native-runtime/printers.json'),
        'printer_ca':str(ROOT.parent/'fprisc/build/printer-native-runtime/bbl-printer-ca.pem'),
        'native_receipt':str(ROOT.parent/'fprisc/build/printer-native-runtime/native-deployment.json'),
        'coscad_binary':str(ROOT/'bin/coscad'),
    }
    if LOCAL.exists():
        value = json.loads(LOCAL.read_text())
        if not isinstance(value, dict) or set(value)-set(result): raise Refused('unknown local dependency path')
        result.update(value)
    if overrides: result.update(overrides)
    for key,value in result.items():
        if not isinstance(value,str) or not value: raise Refused('invalid dependency path: '+key)
        expanded=Path(value).expanduser()
        result[key]=expanded if expanded.is_absolute() else ROOT/expanded
    return result

def capture(selected, resolved=None):
    p = resolved or paths(); entries = {}
    def add(name,scope,value):
        if scope in selected: entries[name]={'scope':scope, **value}
    if 'source' in selected:
        for name in ('fprisc','bambu_lan','bosl2'):
            add(name,'source',repo(p[name],{'bambu_lan':'3d-models','bosl2':'BOSL2'}.get(name,name)))
        add('coscad_source','source',source(ROOT))
    def file(name,scope,path):
        if scope in selected: add(name,scope,{'sha256':sha(path)})
    def directory(name,scope,path):
        if scope in selected: add(name,scope,tree(path))
    probes={
        'ghc':('build',['--numeric-version']), 'cabal':('build',['--numeric-version']),
        'fpr_compiler':('build',['--version']),
        'clang':('build',['--version']), 'openscad':('render',['--version']),
        'node':('editor',['--version']), 'npm':('editor',['--version']),
        'python':('build',['--version']), 'test_python':('test',['--version']),
        'pandoc':('manual',['--version']), 'mandoc':('manual',['-V']),
        'coscad_binary':('render',['--version']),
    }
    for name,(scope,args) in probes.items():
        if scope not in selected: continue
        if name in ('pandoc','mandoc') and not p[name].is_file():
            add(name,scope,{'available':False}); continue
        file(name,scope,p[name])
        if name != 'mandoc': entries[name]['version']=output([p[name],*args]).splitlines()[0]
    if 'build' in selected:
        add('host','build',{'system':platform.system(),'machine':platform.machine(),'os_version':platform.mac_ver()[0] if sys.platform=='darwin' else platform.release()})
        directory('openssl_headers','build',p['openssl']/'include')
        file('libssl','build',p['openssl']/'lib/libssl.3.dylib')
        file('libcrypto','build',p['openssl']/'lib/libcrypto.3.dylib')
        file('openssl_binary','build',p['openssl']/'bin/openssl')
        add('openssl','build',{'version':output([p['openssl']/'bin/openssl','version'])})
        file('cabal_freeze','build',ROOT/'cabal.project.freeze')
    directory('openscad_bundle','render',p['openscad_bundle'])
    directory('bambustudio_bundle','slice',p['bambustudio_bundle'])
    directory('bambu_profiles','slice',p['bambu_profiles'])
    if 'slice' in selected:
        file('bambustudio','slice',p['bambustudio'])
        with (p['bambustudio_bundle']/'Contents/Info.plist').open('rb') as stream:
            info=plistlib.load(stream)
        add('bambustudio_version','slice',{'version':info['CFBundleShortVersionString']})
    file('npm_lock','editor',ROOT/'vscode/package-lock.json')
    directory('editor_modules','editor',ROOT/'vscode/node_modules')
    directory('npm_distribution','editor',p['npm'].resolve().parent.parent)
    if 'test' in selected:
        code='''import importlib.metadata as m,hashlib,json
result={'versions':{},'contents':{}}
for name in '''+repr(PACKAGES)+''':
 d=m.distribution(name); result['versions'][name]=d.version; h=hashlib.sha256()
 for item in sorted(d.files or [],key=str):
  if str(item).endswith('.pyc'): continue
  path=d.locate_file(item)
  h.update(str(item).encode()+b'\\0'+hashlib.sha256(path.read_bytes()).digest())
 result['contents'][name]=h.hexdigest()
print(json.dumps(result))'''
        add('python_packages','test',json.loads(output([p['test_python'],'-c',code])))
    if 'printer' in selected:
        file('native_host','printer',p['native_host'])
        file('printer_config','printer',p['printer_config'])
        file('printer_ca','printer',p['printer_ca'])
        file('printer_libssl','printer',p['openssl']/'lib/libssl.3.dylib')
        file('printer_libcrypto','printer',p['openssl']/'lib/libcrypto.3.dylib')
        receipt=json.loads(p['native_receipt'].read_text())
        if receipt['binary_sha256'] != entries['native_host']['sha256']:
            raise Refused('deployed binary differs from its recorded source/build receipt')
        add('deployed_source','printer',{'repository':'fprisc','revision':receipt['application_commit'],'layout':1})
    if 'infrastructure' in selected:
        file('tailscale','infrastructure',p['tailscale'])
        add('tailscale_version','infrastructure',{'version':output([p['tailscale'],'version'])})
        # Only the named site's public configuration. Never inspect key files,
        # other virtual hosts, request bodies, access codes or environment files.
        code = '''import hashlib,json,subprocess
from pathlib import Path
files=['/etc/nginx/sites-available/3dprint.cswine.cloud','/etc/nginx/snippets/3dprint-access.conf','/etc/letsencrypt/renewal-hooks/deploy/30-3dprint-nginx']
versions=subprocess.check_output(['dpkg-query','-W','-f=${Package}=${Version}\\n','nginx','openssl','certbot'],text=True)
print(json.dumps({'packages':versions.strip().splitlines(),'files':{p:hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in files}}))'''
        import shlex
        add('linode','infrastructure',json.loads(output(['ssh','-o','BatchMode=yes','-o','ConnectTimeout=10','root@localhost-0','python3 -c '+shlex.quote(code)])))
    return entries

def differences(expected,actual,selected):
    want={k:v for k,v in expected.items() if v.get('scope') in selected}
    return sorted(k for k in want.keys()|actual.keys() if want.get(k)!=actual.get(k))

def check_ci(root,lock):
    import re
    actions=json.loads((root/'ci-actions.lock.json').read_text())
    if actions.get('layout')!=1: raise Refused('unsupported CI action lock layout')
    for name,row in actions['actions'].items():
        if not re.fullmatch('[0-9a-f]{40}',row['revision']): raise Refused('CI action needs a full commit: '+name)
    value=lock if isinstance(lock,dict) else json.loads(lock.read_text())
    bosl=value['pins']['bosl2']['revision']
    for path in sorted((root/'.github/workflows').glob('*.yml')):
        text=path.read_text()
        for name,revision in re.findall(r'uses:\s*([^\s@]+)@([^\s]+)',text):
            if name not in actions['actions'] or revision!=actions['actions'][name]['revision']:
                raise Refused('unpinned/mismatched CI action in '+path.name+': '+name)
        for revision in re.findall(r'fetch --depth 1 origin ([^\s]+)',text):
            if revision!=bosl: raise Refused('CI BOSL2 revision differs from pipeline lock')
        for tool,option in (('ghc','ghc-version'),('cabal','cabal-version'),('node','node-version')):
            if tool not in value['pins']: continue
            expected=value['pins'][tool]['version'].removeprefix('v')
            for observed in re.findall(option+r':\s*[\"\']?([^\s\"\']+)',text):
                if observed!=expected: raise Refused('CI '+option+' differs from pipeline lock')

@contextlib.contextmanager
def writer(lock):
    """OS-owned writer lock releases on process death; stale files do not block."""
    with lock.with_suffix('.writer').open('a') as handle:
        import fcntl
        try: fcntl.flock(handle,fcntl.LOCK_EX|fcntl.LOCK_NB)
        except BlockingIOError: raise Refused('another pin writer is active') from None
        yield

def atomic_json(path,value):
    fd,name=tempfile.mkstemp(prefix='.'+path.name+'-',dir=path.parent)
    try:
        with os.fdopen(fd,'w') as stream:
            stream.write(json.dumps(value,indent=2,sort_keys=True)+'\n'); stream.flush(); os.fsync(stream.fileno())
        os.replace(name,path)
        # Make the rename durable on the host where this baseline is maintained.
        if os.name!='nt':
            fd=os.open(path.parent,os.O_RDONLY)
            try: os.fsync(fd)
            finally: os.close(fd)
    finally: Path(name).unlink(missing_ok=True)

def checked(lock,selected,resolved=None):
    value=json.loads(lock.read_text())
    if value.get('layout')!=1 or not isinstance(value.get('pins'),dict): raise Refused('unsupported pipeline lock layout')
    actual=capture(selected,resolved)
    drift=differences(value['pins'],actual,selected)
    if drift: raise Refused('dependency drift: '+', '.join(drift)+'; restore the pin or review an explicit --pin update')
    check_ci(ROOT,lock)
    return actual

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    mode=parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--pin',action='store_true'); mode.add_argument('--check',action='store_true')
    mode.add_argument('--check-ci',action='store_true')
    mode.add_argument('--run',nargs=argparse.REMAINDER,metavar='TOOL')
    parser.add_argument('--scope',choices=SCOPES,action='append')
    args=parser.parse_args(); selected=set(args.scope or SCOPES)
    try:
        if args.check_ci:
            check_ci(ROOT,LOCK); print('CI action and BOSL2 pins hold'); return 0
        p=paths()
        if args.pin:
            if args.scope: raise Refused('--pin always captures the complete baseline')
            with writer(LOCK):
                pins=capture(selected,p)
                # Require a stable complete inventory, including vendor auto-updaters.
                if capture(selected,p)!=pins:
                    raise Refused('dependency changed during inventory; previous lock preserved')
                check_ci(ROOT,{'pins':pins})
                atomic_json(LOCK,{'repository':'coscad-pipeline','layout':1,'baseline_source_revision':git(ROOT,'rev-parse','HEAD'),'pins':pins})
            print('Pinned '+str(len(pins))+' dependencies; review and commit pipeline.lock.json')
        else:
            pins=checked(LOCK,selected,p)
            print('Pipeline lock holds ('+str(len(pins))+' pins)',flush=True)
            if args.run is not None:
                if not args.run: raise Refused('--run needs a named pinned tool')
                name,*rest=args.run
                allowed={'coscad':'coscad_binary','openscad':'openscad','bambustudio':'bambustudio','cabal':'cabal','node':'node','npm':'npm','python':'test_python'}
                required={'coscad':{'source','render'},'openscad':{'source','render'},'bambustudio':{'slice'},'cabal':{'source','build'},'node':{'editor'},'npm':{'editor'},'python':{'source','test','slice'}}
                if name not in allowed: raise Refused('run supports only named pinned tools')
                if not required[name]<=selected: raise Refused('selected scopes omit required tool dependencies')
                env=os.environ.copy()
                env.update({'COSCAD_OPENSCAD':str(p['openscad']),'COSCAD_BOSL2':str(p['bosl2']),
                    'BAMBU_STUDIO':str(p['bambustudio']),'BAMBU_PROFILES':str(p['bambu_profiles']),
                    'BAMBU_LAN_DIR':str(p['bambu_lan']/'bambu-lan')})
                env['PATH']=os.pathsep.join(dict.fromkeys([str(p[k].resolve().parent) for k in ('ghc','clang','node','cabal')]+[env.get('PATH','')]))
                return subprocess.run([str(p[allowed[name]]),*rest],env=env).returncode
        return 0
    except (Refused,OSError,ValueError,KeyError) as error:
        print('pipeline lock: '+str(error),file=sys.stderr); return 1

if __name__=='__main__': sys.exit(main())
