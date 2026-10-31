#!/usr/bin/env python3
"""Dispatch owner-authorized tesseract center-only reinforced-support retry on A1 AMS slot 1 exactly once, or read status.

Reuses the existing verified local printer context and LAN API. Credentials stay
inside the process. A durable intent prevents duplicate starts after uncertainty.
"""
import argparse
from contextlib import ExitStack
from datetime import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import sys
import time
import zipfile
from zoneinfo import ZoneInfo
# Every dispatch rechecks the actual package against independent thermal intent.
sys.path.insert(0, str(Path(__file__).resolve().parents[3]/'scripts'))
from bambu_preflight import preset_expectations, checked_package, verify_package


HERE = Path(__file__).resolve().parent/'bed2'/'a1-center-mouse-ears-20261031'
RECORD = HERE/'print-run.json'
spec = importlib.util.spec_from_file_location('printer_context', Path(__file__).resolve().parents[2]/'beach-outrigger/printer-context.py')
context = importlib.util.module_from_spec(spec)
spec.loader.exec_module(context)


def now():
    return datetime.now(ZoneInfo('Australia/Sydney')).isoformat(timespec='seconds')


def save(record):
    tmp = RECORD.with_suffix('.tmp')
    tmp.write_text(json.dumps(record,indent=2)+'\n')
    tmp.replace(RECORD)


def ready(p):
    assert p.state.get('gcode_state') in ('IDLE','FINISH'), 'Printer is busy or failed; inspect before dispatch'
    assert p.state.get('print_error') == 0 and not p.state.get('hms'), 'Printer has an active fault'
    assert str(p.state.get('nozzle_diameter')) == '0.4', 'Nozzle differs from the slice'
    assert p.state.get('sdcard') is True, 'Printer SD card unavailable'
    assert any(name=='ams0' and material.upper()=='PLA' and colour.upper()=='FFFFFFFF'
               for name,material,colour,loaded in p.spools()), 'White PLA not reported in physical AMS slot 1'
    assert int(p.state.get('ams',{}).get('tray_exist_bits','0')) & 1, 'Physical AMS slot 1 not present'


def run(resources):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--start',action='store_true')
    ap.add_argument('--bed-clear',action='store_true')
    args = ap.parse_args()
    record = None
    if args.start:
        assert args.bed_clear, 'Owner bed-clear confirmation required'
        assert not RECORD.exists(), 'Dispatch record exists; read status instead of resending'
        package = HERE/'slice/tesseract-bed2-a1.gcode.3mf'
        verify_package(package, preset_expectations(HERE/'presets', plate='Textured PEI Plate'))
        spec=importlib.util.spec_from_file_location('center_validation',HERE/'verify.py')
        audit=importlib.util.module_from_spec(spec);spec.loader.exec_module(audit)
        audit.verify()
        evidence = [json.loads((HERE/'package-validation.json').read_text())]
        assert evidence[0]['checked'] and evidence[0]['single_plate'] and evidence[0]['normal_objects']==1 and evidence[0]['bed_index']==2
        assert len(evidence)==1 and Path(evidence[0]['package']).name==package.name
        digest = hashlib.sha256(package.read_bytes()).hexdigest()
        assert digest==evidence[0]['package_sha256'], 'Package changed since validation'
        snapshot, thermal_preflight = resources.enter_context(checked_package(package, preset_expectations(HERE/'presets', plate='Textured PEI Plate')))
        package_size = snapshot.seek(0, 2)
        snapshot.seek(0)
        if thermal_preflight['package_sha256'] != digest:
            raise ValueError('Package changed during preflight; refusing to print')
        with zipfile.ZipFile(snapshot) as z:
            settings=json.loads(z.read('Metadata/project_settings.config'))
            assert settings['printer_settings_id']=='Bambu Lab A1 0.4 nozzle'
            assert settings['filament_settings_id']==['Generic PLA']
            assert settings['nozzle_diameter']==['0.4']
            assert settings['curr_bed_type']=='Textured PEI Plate', 'Wrong exported build plate'
            startup=z.read('Metadata/plate_1.gcode').split(b'; CHANGE_LAYER',1)[0]
            targets=re.findall(rb'^M(?:140|190)\s+S([0-9.]+)',startup,re.M);waits=re.findall(rb'^M190\s+S([0-9.]+)',startup,re.M)
            assert targets and waits and float(targets[-1])==55 and float(waits[-1])==55, 'Wrong actual bed temperature'
            assert settings['curr_bed_type']=='Textured PEI Plate'

            layers=int(re.search(r'; total layer number: (\d+)',z.read('Metadata/plate_1.gcode').decode()).group(1))
    run,p,tls = context.connect('a1')
    try:
        if not args.start:
            print(json.dumps({'at':now(),'status':p.summary(),'hms':p.state.get('hms'),'spools':p.spools()}))
            return
        ready(p)
        remote='tesseract-v3-center-trees-ears-a1-'+datetime.now(ZoneInfo('Australia/Sydney')).strftime('%Y%m%d-%H%M%S')+'.gcode.3mf'
        record=dict(created_at=now(),job='tesseract-v3-center-trees-ears',printer='a1',
            authorization='Owner requested printing just the center on A1 with adjusted tree supports and mouse ears, confirmed the A1 plate clear on 2026-10-31. Prior physical AMS slot 1 preference retained; live white PLA verified.',
            bed_clear_owner_confirmed=True,package=package.name,package_sha256=digest,thermal_preflight=thermal_preflight,
            remote_name=remote,spool='ams0',material='PLA',physical_ams_slot=1,ams_mapping=[0],expected_layers=layers,
            before=p.summary(),spools_before=p.spools(),phase='preflight',dispatch_attempted=False,events=[])
        with RECORD.open('x') as f:
            f.write(json.dumps(record,indent=2)+'\n')
        sys.path.insert(0,str(context.ROOT/'3d-models/bambu-lan'))
        import ftp as lanftp

        class FTPS(lanftp.ImplicitFTPS):
            def storbinary(self,cmd,fp,blocksize=32768):
                self.voidcmd('TYPE I')
                with self.transfercmd(cmd) as conn:
                    while data:=fp.read(blocksize):
                        conn.sendall(data)
                    try:
                        conn.settimeout(5)
                        conn.unwrap()
                    except (OSError,TimeoutError):
                        pass
                return self.voidresp()

        ftp=FTPS(context=tls(),timeout=60)
        try:
            ftp.connect(os.environ['BAMBU_HOST'],990)
            ftp.login('bblp',os.environ['BAMBU_ACCESS_CODE'])
            ftp.prot_p()
            record.update(phase='uploading');save(record)
            snapshot.seek(0)
            ftp.storbinary('STOR '+remote,snapshot)
            assert ftp.size(remote)==package_size, 'Upload size differs'
            record.update(phase='verifying-upload');save(record)
            ftp.voidcmd('TYPE I')
            remote_hash=hashlib.sha256()
            with ftp.transfercmd('RETR '+remote) as stream:
                while data:=stream.recv(32768):
                    remote_hash.update(data)
            ftp.voidresp()
            if remote_hash.hexdigest() != digest:
                raise ValueError('Uploaded content differs')
        finally:
            ftp.close()
        record.update(phase='uploaded-verified',remote_content_sha256=digest)
        save(record)
        ready(p)
        seq=str(int(time.time()*1000)%1000000000)
        command={'print':dict(sequence_id=seq,command='project_file',param='Metadata/plate_1.gcode',
            project_id='0',profile_id='0',task_id='0',subtask_id='0',subtask_name=remote.removesuffix('.3mf'),
            url='file:///sdcard/'+remote,md5='',timelapse=False,bed_type='auto',bed_levelling=True,
            flow_cali=True,vibration_cali=True,layer_inspect=False,use_ams=True,ams_mapping=[0])}
        p.replies.clear()
        record.update(phase='start-intent',dispatch_attempted=True,sequence_id=seq)
        save(record)
        p.send(command)  # Exactly one print/control command. Never retry this call.
        deadline=time.monotonic()+35
        reply=None
        while time.monotonic()<deadline:
            reply=next((r for r in p.replies if r.get('command')=='project_file' and str(r.get('sequence_id'))==seq),None)
            if reply:
                break
            time.sleep(.2)
        record['reply']={k:reply.get(k) for k in ('result','reason','command','sequence_id')} if reply else None
        record['phase']=('acknowledged' if str(reply.get('result','')).lower()=='success' else 'rejected') if reply else 'start-unknown'
        save(record)
        print(json.dumps({'phase':record['phase'],'reply':record['reply'],'remote_name':remote}),flush=True)
        if record['phase']=='rejected':
            return
        deadline=time.monotonic()+900
        while time.monotonic()<deadline:
            state=p.summary()
            event={'at':now(),'status':state,'hms':p.state.get('hms'),'ams_tray_now':str(p.state.get('ams',{}).get('tray_now','unknown')),'targets':{k:p.state.get(k) for k in ['bed_target_temper','nozzle_target_temper']}}
            record['events'].append(event)
            same_job=state.get('subtask_name') in (remote,remote.removesuffix('.3mf'))
            if same_job and state.get('gcode_state')=='RUNNING' and state.get('total_layer_num')==layers and 1<=(state.get('layer_num') or 0)<=layers:
                record.update(phase='first-layer',first_layer_reached=True,selected_slot_verified=event['ams_tray_now']=='0')
                if event['ams_tray_now']!='0':record.update(needs_attention=True,slot_verification='Physical AMS slot 1 was requested; recheck live tray status')
            if state.get('print_error') or p.state.get('hms') or (same_job and state.get('total_layer_num')==layers and state.get('gcode_state') in ('FAILED','PAUSE')):
                record.update(needs_attention=True)
            save(record)
            print(json.dumps(event),flush=True)
            if record.get('first_layer_reached') or record.get('needs_attention'):
                break
            time.sleep(15)
    except Exception as exc:
        if record is not None:
            # Avoid serializing exceptions that could contain authentication data.
            record.update(error_type=type(exc).__name__,needs_attention=True)
            save(record)
        raise RuntimeError(type(exc).__name__+' during tesseract center-only reinforced-support retry A1 dispatch; inspect saved phase and live status before any retry') from None
    finally:
        p.close()


def main():
    if not __debug__:raise RuntimeError('Safety assertions require Python without optimization')
    with ExitStack() as resources:
        run(resources)


if __name__=='__main__':
    main()
