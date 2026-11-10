"""Read status, or send one verified balanced-glider bed with --start --bed-clear."""
import argparse, importlib.util, json, os, sys, time
from pathlib import Path
from provenance import require_geometry, require_slice, require, digest
B=Path(__file__).resolve().parent
ROOT=B.parents[1]
ORIGINAL=B.parent/'bolted-glider'

def main():
    if not __debug__: raise RuntimeError('Python optimization disables required checks')
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('job',choices=['p1s-wing-left','x1c-fuselage'])
    ap.add_argument('--start',action='store_true'); ap.add_argument('--bed-clear',action='store_true')
    ap.add_argument('--accept-failed-state',action='store_true'); ap.add_argument('--spool'); ap.add_argument('--authorization-note')
    args=ap.parse_args();device=args.job.split('-')[0]
    sys.path.insert(0,str(ORIGINAL))
    spec=importlib.util.spec_from_file_location('glider_dispatch',ORIGINAL/'dispatch.py')
    dispatch=importlib.util.module_from_spec(spec);spec.loader.exec_module(dispatch)
    dispatch.B=B;dispatch.NAME='glider'
    dispatch.context.CA=str(ROOT.parent/'fprisc/build/printer-native-runtime/bbl-printer-ca.pem')
    if args.start:
        if not (args.bed_clear and args.spool and args.authorization_note):ap.error('start requires bed clearance, spool and authorization note')
        assert not (B/args.job/'print-run.json').exists(),'Existing dispatch record; inspect instead of resending'
        require_geometry(B);require_slice(B,args.job)
        validation=json.loads((B/'validation.json').read_text());require(B,validation['inputs'])
        balance=json.loads((B/'balance.json').read_text());assert balance['passed'] and balance['ballast_g']==0
        for job,expected in balance['gcode_sha256'].items():assert digest(B/job/'slice/plate_1.gcode')==expected
    _,printer,_=dispatch.context.connect(device)
    try:
        snapshot={'printer':device,'status':printer.summary(),'spools':printer.spools(),'hms':printer.state.get('hms'),
                  'sdcard':printer.state.get('sdcard'),'nozzle_diameter':printer.state.get('nozzle_diameter')}
        print(json.dumps(snapshot),flush=True)
        if not args.start:return
        assert snapshot['sdcard'] is True,'SD card unavailable'
        assert snapshot['status']['gcode_state'] in (('IDLE','FINISH','FAILED') if args.accept_failed_state else ('IDLE','FINISH')),'Printer not idle'
        assert snapshot['status']['print_error']==0 and not snapshot['hms'],'Printer has a fault'
        selected=[s for s in snapshot['spools'] if s[0]==args.spool and s[1].upper()=='PLA']
        assert len(selected)==1,'Selected PLA spool not reported'
        dark=selected[0][2].upper().startswith('000000')
    finally:printer.close()
    os.environ['TZ']='Australia/Sydney';time.tzset()
    (B/args.job/'dispatch-config.json').write_text(json.dumps({'revision':'balanced-v2','job':args.job,
        'dispatcher':'../bolted-glider/dispatch.py','dispatcher_sha256':digest(ORIGINAL/'dispatch.py'),
        'owner_confirmed_bed_clear':True,'authorization_note':args.authorization_note,'spool':args.spool,
        'flow_calibration':device!='x1c','layer_inspection':not(device=='x1c' and dark)},indent=2)+'\n')
    sys.argv=[str(ORIGINAL/'dispatch.py'),args.job,'--spool',args.spool,'--confirm-bed-clear','--authorization-note',args.authorization_note]
    if args.accept_failed_state:sys.argv.append('--accept-failed-state')
    if device=='x1c':sys.argv.append('--no-flow-cali')
    if device=='x1c' and dark:sys.argv.append('--no-layer-inspect')
    dispatch.main()

if __name__=='__main__':main()
