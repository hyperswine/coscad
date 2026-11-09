#!/usr/bin/env python3
"""Read printer status; --start --bed-clear sends its verified cube package exactly once."""
import argparse,json,os,sys,time
from prepare_print import HERE,ROOT,TANK,JOBS,load,digest
from verify_print import verify


def main():
    if not __debug__:raise RuntimeError('Run without Python optimization')
    ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('printer',choices=JOBS)
    ap.add_argument('--start',action='store_true');ap.add_argument('--bed-clear',action='store_true')
    args=ap.parse_args();job=JOBS[args.printer];folder=HERE/job['job']
    if args.start:
        if not args.bed_clear:ap.error('Owner bed-clear confirmation required')
        if (folder/'print-run.json').exists():raise RuntimeError('Dispatch record exists; inspect, never resend')
        verify(args.printer)
    dispatch=load('cube_dispatch',TANK/'dispatch.py');dispatch.B=HERE;dispatch.NAME='cube'
    dispatch.context.CA=str(ROOT.parent/'fprisc/build/printer-native-runtime/bbl-printer-ca.pem')
    _,printer,_=dispatch.context.connect(args.printer)
    try:
        snapshot=dict(printer=args.printer,status=printer.summary(),spools=printer.spools(),hms=printer.state.get('hms'))
        print(json.dumps(snapshot),flush=True)
        if not args.start:return
        if printer.state.get('sdcard') is not True:raise RuntimeError('SD card unavailable')
        if args.printer=='a1':
            if not any(n=='ams0' and m.upper()=='PLA' and c.upper()=='FFFFFFFF' for n,m,c,l in printer.spools()):
                raise RuntimeError('White PLA not reported in physical AMS slot 1')
            if not int(printer.state.get('ams',{}).get('tray_exist_bits','0')) & 1:raise RuntimeError('AMS slot 1 absent')
    finally:printer.close()
    os.environ['TZ']='Australia/Sydney';time.tzset()
    (folder/'dispatch-config.json').write_text(json.dumps(dict(printer=args.printer,spool=job['spool'],
        dispatcher='models/tank-model-2020/dispatch.py',owner_confirmed_bed_clear=True,
        dispatcher_sha256=digest(TANK/'dispatch.py'),
        shared_verifier_sha256=digest(TANK/'panel-fit-v2/verify_print.py'),
        authorization='Owner requests one cube bed on P1S and one on A1, and confirms both beds clear, 2026-11-09.'),indent=2)+'\n')
    sys.argv=[str(TANK/'dispatch.py'),job['job'],'--spool',job['spool'],'--confirm-bed-clear','--authorization-note',
              'Owner requested cube kit on P1S and A1 and confirmed both beds clear, 2026-11-09. A1 uses white PLA physical AMS slot 1; P1S uses loaded external PLA.']
    dispatch.main()


if __name__=='__main__':main()
