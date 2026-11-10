#!/usr/bin/env python3
"""Report Bambu sliced mass and assembly CG, including explicit hardware."""
import argparse, hashlib, json, os, tempfile
from pathlib import Path
from coscad_mass import report

def publish(path,value):
    fd,tmp=tempfile.mkstemp(prefix=path.name+'.',dir=path.parent)
    try:
        with os.fdopen(fd,'w') as stream:json.dump(value,stream,indent=2,allow_nan=False);stream.write('\n')
        os.replace(tmp,path)
    finally:
        if os.path.exists(tmp):os.unlink(tmp)

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('manifest',type=Path);p.add_argument('-o','--output',type=Path);p.add_argument('--components-out',type=Path)
    args=p.parse_args()
    try:
        source=args.manifest.read_bytes()
        manifest=json.loads(source.decode('utf-8'))
        protected={args.manifest.resolve()}|{(args.manifest.parent/j['gcode']).resolve() for j in manifest.get('jobs',[])}
        outputs=[x.resolve() for x in (args.output,args.components_out) if x is not None]
        if len(set(outputs))!=len(outputs) or any(x in protected for x in outputs):raise ValueError('outputs must be distinct and must not overwrite inputs')
        value=report(manifest,args.manifest.parent)
        value['source']=str(args.manifest)
        value['source_sha256']=hashlib.sha256(source).hexdigest()
        value['reader_sha256']=hashlib.sha256(Path(__file__).with_name('coscad_mass.py').read_bytes()).hexdigest()
        if args.output:publish(args.output,value)
        else:print(json.dumps(value,indent=2,allow_nan=False))
        if args.components_out:publish(args.components_out,{'schema':'coscad.mass-components/1','components':value['components']})
    except (ValueError,KeyError,TypeError,OSError) as exc:p.exit(1,f'error: {exc}\n')

if __name__=='__main__':main()
