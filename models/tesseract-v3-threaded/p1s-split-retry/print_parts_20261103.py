#!/usr/bin/env python3
"""Dispatch one owner-approved November 3 tesseract batch once, or read status.

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


BASE = Path(__file__).resolve().parent
BATCH = BASE/'batch-20261103'
HERE = RECORD = None
spec = importlib.util.spec_from_file_location('printer_context', Path(__file__).resolve().parents[2]/'beach-outrigger/printer-context.py')
context = importlib.util.module_from_spec(spec)
spec.loader.exec_module(context)


def now():
    return datetime.now(ZoneInfo('Australia/Sydney')).isoformat(timespec='seconds')


def save(record):
    tmp = RECORD.with_suffix('.tmp')
    tmp.write_text(json.dumps(record,indent=2)+'\n')
    tmp.replace(RECORD)


def ready(p, device):
    assert p.state.get('gcode_state') in ('IDLE','FINISH'), 'Printer is busy or failed; inspect before dispatch'
    assert p.state.get('print_error') == 0 and not p.state.get('hms'), 'Printer has an active fault'
    assert str(p.state.get('nozzle_diameter')) == '0.4', 'Nozzle differs from the slice'
    assert p.state.get('sdcard') is True, 'Printer SD card unavailable'
    if device == 'a1':
        assert any(n=='ams0' and m.upper()=='PLA' and c.upper()=='FFFFFFFF' for n,m,c,_ in p.spools()), 'White PLA not reported in physical slot 1'
        assert int(p.state.get('ams',{}).get('tray_exist_bits','0')) & 1, 'AMS slot 1 missing'
    else:
        assert any(n=='external' and m.upper()=='PLA' and loaded for n,m,c,loaded in p.spools()), 'Loaded external PLA not reported'



def run(resources):
    global HERE, RECORD
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('trial', choices=('x1c-struts', 'p1s-struts', 'a1-bars'))
    ap.add_argument('--start',action='store_true')
    ap.add_argument('--bed-clear',action='store_true')
    args = ap.parse_args()
    config = {
        'x1c-struts': ('x1c', BATCH/'x1c', 4, ['1-strut','2-strut','3-strut','4-strut']),
        'p1s-struts': ('p1s', BATCH/'p1s', 4, ['1-strut','2-strut','3-strut','4-strut']),
        'a1-bars': ('a1', BATCH/'a1', 4, ['1-bar-print','2-bar-print','3-bar-print']),
    }
    device, HERE, bed, expected_order = config[args.trial]
    sequential = bed == 4
    RECORD = HERE/'print-run.json'
    record = None
    if args.start:
        assert args.bed_clear, 'Owner bed-clear confirmation required'
        assert not RECORD.exists(), 'Dispatch record exists; read status instead of resending'
        package = HERE/('slice/tesseract-bed'+str(bed)+'-'+device+'.gcode.3mf')
        verify_package(package, preset_expectations(HERE/'presets', plate='Textured PEI Plate'))
        import subprocess
        if sequential:
            subprocess.run([sys.executable,str(BASE/'verify.py'),'--bed',str(bed),'--printer',device,'--output-dir',str(HERE)],check=True)
            checkspec=importlib.util.spec_from_file_location('batch_sequence',BASE/'sequential-retry/verify.py')
            checker=importlib.util.module_from_spec(checkspec);checkspec.loader.exec_module(checker)
            checker.verify(HERE,plan_path=HERE/'plan.json',printer=device)
            sequence=json.loads((HERE/'sequential-validation.json').read_text())
            assert sequence['checked'] and sequence['actual_print_sequence']=='by object'
            assert sequence['object_order']==expected_order
            first_object_layers=sequence['per_object'][expected_order[0]]['layers']
        else:
            checkspec=importlib.util.spec_from_file_location('batch_center',HERE/'verify.py')
            checker=importlib.util.module_from_spec(checkspec);checkspec.loader.exec_module(checker)
            sequence=checker.verify();sequence['actual_print_sequence']='by layer'
            first_object_layers=sequence['thermal']['layers_checked']
        evidence = [json.loads((HERE/'package-validation.json').read_text())]
        assert evidence[0]['checked'] and evidence[0]['single_plate'] and evidence[0]['normal_objects']==len(expected_order) and evidence[0]['bed_index']==bed
        assert len(evidence)==1 and Path(evidence[0]['package']).name==package.name
        digest = hashlib.sha256(package.read_bytes()).hexdigest()
        assert digest==evidence[0]['package_sha256']==sequence['package_sha256'], 'Package changed since validation'
        snapshot, thermal_preflight = resources.enter_context(checked_package(package, preset_expectations(HERE/'presets', plate='Textured PEI Plate')))
        package_size = snapshot.seek(0, 2)
        snapshot.seek(0)
        if thermal_preflight['package_sha256'] != digest:
            raise ValueError('Package changed during preflight; refusing to print')
        with zipfile.ZipFile(snapshot) as z:
            settings=json.loads(z.read('Metadata/project_settings.config'))
            assert settings['printer_settings_id']=={'x1c':'Bambu Lab X1 Carbon 0.4 nozzle','p1s':'Bambu Lab P1S 0.4 nozzle','a1':'Bambu Lab A1 0.4 nozzle'}[device]
            assert settings['print_sequence']==sequence['actual_print_sequence']
            assert settings['filament_settings_id']==['Generic PLA']
            assert settings['nozzle_diameter']==['0.4']
            assert settings['curr_bed_type']=='Textured PEI Plate', 'Wrong exported build plate'
            startup=z.read('Metadata/plate_1.gcode').split(b'; CHANGE_LAYER',1)[0]
            targets=re.findall(rb'^M(?:140|190)\s+S([0-9.]+)',startup,re.M);waits=re.findall(rb'^M190\s+S([0-9.]+)',startup,re.M)
            assert targets and waits and float(targets[-1])==55 and float(waits[-1])==55, 'Wrong actual bed temperature'
            assert settings['curr_bed_type']=='Textured PEI Plate'

            layers=int(re.search(r'; total layer number: (\d+)',z.read('Metadata/plate_1.gcode').decode()).group(1))
    run,p,tls = context.connect(device)
    try:
        if not args.start:
            print(json.dumps({'at':now(),'status':p.summary(),'hms':p.state.get('hms'),'spools':p.spools()}))
            return
        ready(p, device)
        remote='tesseract-v3-'+args.trial+'-'+datetime.now(ZoneInfo('Australia/Sydney')).strftime('%Y%m%d-%H%M%S')+'.gcode.3mf'
        record=dict(created_at=now(),job='tesseract-v3-'+args.trial,printer=device,
            authorization='Owner requested four per-object diagonal struts on X1C and P1S, then changed A1 to three per-object long bars; explicitly confirmed all three plates clear on 2026-11-03.',
            bed_clear_owner_confirmed=True,package=package.name,package_sha256=digest,thermal_preflight=thermal_preflight,
            remote_name=remote,spool='ams0' if device=='a1' else 'external',material='PLA',expected_layers=layers,first_object_layers=first_object_layers,expected_order=sequence.get('object_order'),print_sequence=sequence['actual_print_sequence'],
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
        ready(p, device)
        seq=str(int(time.time()*1000)%1000000000)
        command={'print':dict(sequence_id=seq,command='project_file',param='Metadata/plate_1.gcode',
            project_id='0',profile_id='0',task_id='0',subtask_id='0',subtask_name=remote.removesuffix('.3mf'),
            url='file:///sdcard/'+remote,md5='',timelapse=False,bed_type='auto',bed_levelling=True,
            flow_cali=device!='p1s',vibration_cali=True,layer_inspect=False,use_ams=device=='a1',ams_mapping=[0] if device=='a1' else [254])}
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
        record['sequence_validation'] = sequence
        save(record)
        if record['phase']=='rejected':
            return
        deadline=time.monotonic()+900
        seen_zero=False
        while time.monotonic()<deadline:
            state=p.summary()
            event={'at':now(),'status':state,'hms':p.state.get('hms'),'ams_tray_now':str(p.state.get('ams',{}).get('tray_now','unknown')),'targets':{k:p.state.get(k) for k in ['bed_target_temper','nozzle_target_temper']}}
            record['events'].append(event)
            same_job=state.get('subtask_name') in (remote,remote.removesuffix('.3mf'))
            if same_job and state.get('gcode_state')=='RUNNING' and state.get('total_layer_num') in (layers,first_object_layers) and state.get('layer_num')==0:seen_zero=True
            if same_job and seen_zero and state.get('gcode_state')=='RUNNING' and state.get('total_layer_num') in (layers,first_object_layers) and 1<=(state.get('layer_num') or 0)<=layers:
                record.update(phase='first-layer',first_layer_reached=True)
                if device=='a1':
                    record['selected_slot_verified']=event['ams_tray_now']=='0'
                    if not record['selected_slot_verified']:record['needs_attention']=True
            if state.get('print_error') or p.state.get('hms') or (same_job and state.get('total_layer_num') in (layers,first_object_layers) and state.get('gcode_state') in ('FAILED','PAUSE')):
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
        raise RuntimeError(type(exc).__name__+' during November 3 tesseract batch dispatch; inspect saved phase and live status before any retry') from None
    finally:
        p.close()


def main():
    if not __debug__:
        raise RuntimeError('Safety assertions require Python without optimization')
    with ExitStack() as resources:
        run(resources)


if __name__=='__main__':
    main()
