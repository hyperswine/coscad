"""One-shot P1S chamber exhaust cooldown; restores its initial off state."""
import importlib.util,json,os,time,pathlib
ROOT=pathlib.Path('/Users/jasonqin/Documents/GitHub/coscad')
s=importlib.util.spec_from_file_location('run',ROOT/'scripts/bambu-print-run.py');r=importlib.util.module_from_spec(s);s.loader.exec_module(r)
os.environ['BAMBU_ENV_FILE']='/Users/jasonqin/.config/bambu/p1s.env';r.load_env_file()
d=json.loads(pathlib.Path('/tmp/coscad-p1s-device.json').read_text());assert d['model']=='C12'
os.environ['BAMBU_HOST']=d['host'];os.environ['BAMBU_SERIAL']=d['serial']
report_path=pathlib.Path(__file__).with_name('chamber-fan-timer.json')
report={}
def save():report_path.write_text(json.dumps(report,indent=2))
def command(p,speed,seq):
 p.send({'print':{'sequence_id':str(seq),'command':'gcode_line','param':f'M106 P3 S{speed}\n'}})
 deadline=time.monotonic()+15
 while time.monotonic()<deadline:
  reply=next((x for x in p.replies if x.get('command')=='gcode_line' and str(x.get('sequence_id'))==str(seq)),None)
  if reply:
   if reply.get('result')!='success':raise RuntimeError('Fan command rejected')
   return
  time.sleep(.2)
 raise RuntimeError('Fan acknowledgement timed out')
p=r.Printer()
try:
 assert p.state.get('gcode_state') in ('IDLE','FINISH'), 'Printer busy'
 assert str(p.state.get('big_fan2_speed'))=='0', 'Initial chamber fan is not off'
 report={'status':'starting','duration_seconds':600,'restore_speed':0};save()
 try:
  command(p,255,601)
  deadline=time.monotonic()+600
  report.update(status='running',started_at=time.strftime('%Y-%m-%dT%H:%M:%S%z'),restore_at=time.strftime('%Y-%m-%dT%H:%M:%S%z',time.localtime(time.time()+600)));save()
  for _ in range(30):
   if str(p.state.get('big_fan2_speed')) not in ('0','None'):break
   time.sleep(1)
  report['reported_chamber_fan_speed']=p.state.get('big_fan2_speed');save()
  print(json.dumps(report),flush=True)
  while time.monotonic()<deadline:time.sleep(min(5,deadline-time.monotonic()))
 finally:
  for attempt in range(4):
   try:
    command(p,0,602+attempt);report.update(status='restored',restored_at=time.strftime('%Y-%m-%dT%H:%M:%S%z'));save();print('Chamber fan restored to off',flush=True);break
   except Exception:
    if attempt==3:report['status']='restore_failed';save();raise
    p.close();time.sleep(2);p=r.Printer()
finally:p.close()
