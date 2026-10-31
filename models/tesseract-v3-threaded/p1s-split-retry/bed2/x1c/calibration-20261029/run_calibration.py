#!/usr/bin/env python3
"""One owner-authorized bed-only built-in calibration; never starts a print."""
from pathlib import Path
from datetime import datetime
from zoneinfo import ZoneInfo
import importlib.util,json,threading,time
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[5]
RECORD=HERE/'run.json'
KEYS=('gcode_state','gcode_file','subtask_name','mc_percent','mc_remaining_time','layer_num','total_layer_num','print_error','fail_reason','mc_print_error_code','mc_print_stage','mc_print_sub_stage','stg_cur','stg','nozzle_temper','nozzle_target_temper','bed_temper','bed_target_temper','home_flag','calibration_step','calibration_state','bed_leveling','position','x','y','z','x_pos','y_pos','z_pos','mc_print_line_number')
def now():return datetime.now(ZoneInfo('Australia/Sydney')).isoformat(timespec='seconds')
def save(r):
 tmp=RECORD.with_suffix('.tmp');tmp.write_text(json.dumps(r,indent=2)+'\n');tmp.replace(RECORD)
def main():
 if RECORD.exists():raise RuntimeError('Calibration record exists; inspect it rather than resending')
 s=importlib.util.spec_from_file_location('printer_context',ROOT/'models/beach-outrigger/printer-context.py');c=importlib.util.module_from_spec(s);s.loader.exec_module(c)
 _,p,_=c.connect('x1c');lock=threading.Lock()
 def state():return {k:p.state[k] for k in KEYS if k in p.state}
 try:
  before=state();known='tesseract-v3-split-bed2-pei-x1c-20261029-125659.gcode'
  if not (before.get('gcode_state') in ('IDLE','FINISH') or before.get('gcode_state')=='FAILED' and before.get('subtask_name')==known):raise RuntimeError('Printer is not inactive in the expected job')
  if p.state.get('print_error')!=0 or p.state.get('hms'):raise RuntimeError('Active fault requires inspection before calibration')
  original=p.c.on_message
  def message(cl,ud,msg):
   original(cl,ud,msg)
   try:
    data=json.loads(msg.payload).get('print',{});selected={k:data[k] for k in KEYS if k in data}
    if data:
     event={'at':now(),'reported_keys':sorted(data),'delta':selected,'hms':data.get('hms'),'command':data.get('command'),'sequence_id':data.get('sequence_id'),'result':data.get('result'),'reason':data.get('reason')}
     with lock:
      with (HERE/'telemetry.jsonl').open('a') as f:f.write(json.dumps(event)+'\n')
   except Exception:pass
  p.c.on_message=message
  seq=str(int(time.time()*1000)%1000000000);command={'print':{'command':'calibration','option':2,'sequence_id':seq}}
  r={'created_at':now(),'authorization':'Owner explicitly requested full-bed calibration on the X1C and observation of where it fails.','before':before,'command':command,'source':'https://github.com/bambulab/BambuStudio/blob/master/src/slic3r/GUI/DeviceManager.cpp#L1757-L1777','selected_functions':{'bed_leveling':True,'vibration':False,'camera':False,'motor_noise':False},'phase':'calibration-intent','dispatch_attempted':True,'events':[]}
  with RECORD.open('x') as f:f.write(json.dumps(r,indent=2)+'\n')
  p.replies.clear();p.send(command)  # Exactly one calibration control command.
  deadline=time.monotonic()+40;reply=None
  while time.monotonic()<deadline:
   reply=next((q for q in p.replies if q.get('command')=='calibration' and str(q.get('sequence_id'))==seq),None)
   if reply:break
   time.sleep(.2)
  r['reply']={k:reply.get(k) for k in ('command','sequence_id','result','reason')} if reply else None
  r['phase']='acknowledged' if reply and str(reply.get('result','')).lower()=='success' else 'rejected' if reply else 'acknowledgement-unknown';save(r);print(json.dumps({'phase':r['phase'],'reply':r['reply'],'at':now()}),flush=True)
  if r['phase']=='rejected':return
  seen=False;last=None;deadline=time.monotonic()+1200;seen_leveling=False
  while time.monotonic()<deadline:
   current=state();active=current.get('gcode_state') in ('RUNNING','PREPARE','CALIBRATION')
   if active:seen=True
   if current.get('stg_cur') in (1,40,47,48):seen_leveling=True
   event={'at':now(),'status':current,'hms':p.state.get('hms')}
   if event['status']!=last or event['hms']:
    r['events'].append(event);last=current.copy();(HERE/'status-latest.json').write_text(json.dumps(event,indent=2)+'\n');print(json.dumps(event),flush=True)
   r.update(seen_active=seen,seen_leveling=seen_leveling)
   if seen and current.get('gcode_state') in ('FINISH','FAILED','IDLE'):
    r.update(phase='failed' if current.get('gcode_state')=='FAILED' or current.get('print_error') else 'finished',ended_at=now(),after=current,hms_after=p.state.get('hms'));save(r);break
   if seen and current.get('gcode_state')=='PAUSE':r.update(phase='needs-attention',after=current);save(r);break
   save(r);time.sleep(2)
  else:r.update(phase='observation-timeout',after=state());save(r)
 finally:p.close()
if __name__=='__main__':main()
