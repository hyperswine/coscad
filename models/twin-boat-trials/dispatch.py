"""One explicit, deduplicated boat dispatch through the existing Bambu LAN APIs.

Requires owner-confirmed empty bed and exact spool. Never automatically retries
an uncertain print command. No long-running follow-up is scheduled.
"""
from pathlib import Path
import argparse,hashlib,importlib,importlib.util,json,os,socket,ssl,sys,time
B=Path(__file__).resolve().parent
sys.path.insert(0,str(B));context=importlib.import_module('printer-context')
def write(path,record):
 temporary=path.with_suffix('.tmp');temporary.write_text(json.dumps(record,indent=2)+'\n');temporary.replace(path)
def main():
 ap=argparse.ArgumentParser(description=__doc__);ap.add_argument('boat',choices=['monohull','outrigger']);ap.add_argument('--spool',required=True,choices=['external','ams0']);ap.add_argument('--confirm-bed-clear',action='store_true',required=True);ap.add_argument('--authorization-note',required=True);args=ap.parse_args()
 device='p1s' if args.boat=='monohull' else 'a1'
 assert args.spool==('external' if device=='p1s' else 'ams0'),'Unexpected spool for this approved trial'
 folder=B/args.boat;record_path=folder/'print-run.json';assert not record_path.exists(),'Existing dispatch record: inspect it and the printer instead of resending'
 package=folder/'slice'/f'{args.boat}.gcode.3mf';verified=json.loads((folder/'package-validation.json').read_text());assert verified['passed']
 digest=hashlib.sha256(package.read_bytes()).hexdigest();assert digest==verified['package_sha256']
 run,p=context.connect(device)
 record=None
 try:
  assert p.state.get('gcode_state') in ['FINISH','IDLE'],'Printer is busy'
  assert p.state.get('print_error')==0 and not p.state.get('hms'),'Printer has an active fault'
  assert str(p.state.get('nozzle_diameter'))=='0.4'
  assert any(name==args.spool and material=='PLA' for name,material,colour,loaded in p.spools()),'Selected PLA spool is not reported'
  if device=='a1':assert any(name=='ams0' and material=='PLA' and colour.upper()=='FFFFFFFF' for name,material,colour,loaded in p.spools()),'Confirmed white PLA is not reported in physical AMS slot 1'
  if args.spool=='external':assert any(name=='external' and loaded for name,material,colour,loaded in p.spools())
  remote=f'{args.boat}-{device}-{time.strftime("%Y%m%d-%H%M%S")}.gcode.3mf'
  record={'boat':args.boat,'printer':device,'package_sha256':digest,'remote_name':remote,'spool':args.spool,'material':'PLA','bed_clear_owner_confirmed':True,'authorization_note':args.authorization_note,'before':p.summary(),'hms_before':p.state.get('hms'),'phase':'preflight','dispatch_attempted':False,'events':[],'created_at':time.strftime('%Y-%m-%dT%H:%M:%S%z')}
  with record_path.open('x') as f:f.write(json.dumps(record,indent=2)+'\n')
  # Reuse the repository's implicit-FTPS implementation/session reuse, but
  # supply verified serial-name TLS and verify the remote content before start.
  sys.path.insert(0,str(context.ROOT/'3d-models/bambu-lan'));import ftp as lanftp
  serial=os.environ['BAMBU_SERIAL']
  class SerialTLS(ssl.SSLContext):
   def wrap_socket(self,*a,**kw):kw['server_hostname']=serial;return super().wrap_socket(*a,**kw)
  tls=SerialTLS(ssl.PROTOCOL_TLS_CLIENT);tls.load_verify_locations('/private/tmp/boat-printer-ca.pem')
  ftp=lanftp.ImplicitFTPS(context=tls,timeout=20)
  try:
   ftp.connect(os.environ['BAMBU_HOST'],990);ftp.login('bblp',os.environ['BAMBU_ACCESS_CODE']);ftp.prot_p()
   with package.open('rb') as f:ftp.storbinary('STOR '+remote,f)
   assert ftp.size(remote)==package.stat().st_size,'Remote size mismatch'
   # Skip TLS unwrap on RETR for the same printer close_notify issue as STOR.
   ftp.voidcmd('TYPE I');remote_hash=hashlib.sha256();received=0
   with ftp.transfercmd('RETR '+remote) as stream:
    while True:
     data=stream.recv(32768)
     if not data:break
     remote_hash.update(data);received+=len(data)
   ftp.voidresp();assert remote_hash.hexdigest()==digest and received==package.stat().st_size,'Remote content mismatch'
  finally:ftp.close()
  record.update(phase='uploaded-verified',remote_content_sha256=digest);write(record_path,record)
  assert p.state.get('gcode_state') in ['FINISH','IDLE'] and p.state.get('print_error')==0 and not p.state.get('hms'),'Printer changed during upload'
  seq=str(int(time.time()*1000)%1000000000)
  command={'print':{'sequence_id':seq,'command':'project_file','param':'Metadata/plate_1.gcode','project_id':'0','profile_id':'0','task_id':'0','subtask_id':'0','subtask_name':remote.removesuffix('.3mf'),'url':'file:///sdcard/'+remote,'md5':'','timelapse':False,'bed_type':'auto','bed_levelling':True,'flow_cali':True,'vibration_cali':True,'layer_inspect':True,'use_ams':args.spool!='external','ams_mapping':[0] if args.spool!='external' else [254]}}
  # The durable intent precedes exactly one send on an already connected
  # status API client. Its reconnect callback sends status requests only.
  p.replies.clear();record.update(phase='start-intent',dispatch_attempted=True,sequence_id=seq);write(record_path,record);p.send(command)
  until=time.monotonic()+35;reply=None
  while time.monotonic()<until:
   reply=next((r for r in p.replies if r.get('command')=='project_file' and str(r.get('sequence_id'))==seq),None)
   if reply:break
   time.sleep(.2)
  if reply:
   record['reply']={k:reply.get(k) for k in ['result','reason','command','sequence_id']};record['phase']='acknowledged' if reply.get('result')=='success' else 'rejected'
  else:record['phase']='start-unknown';record['acknowledgement_uncertain']=True
  write(record_path,record);print(json.dumps({'printer':device,'boat':args.boat,'phase':record['phase'],'spool':args.spool,'job':remote}),flush=True)
  if record['phase']=='rejected':return
  until=time.monotonic()+600
  while time.monotonic()<until:
   state=p.summary();event={'at':time.strftime('%Y-%m-%dT%H:%M:%S%z'),'status':state,'hms':p.state.get('hms')};record['events'].append(event);write(record_path,record);print(json.dumps(event),flush=True)
   is_job=state.get('subtask_name') in [remote,remote.removesuffix('.3mf')] or state.get('gcode_file')==remote
   if is_job and state.get('gcode_state')=='RUNNING' and state.get('total_layer_num')==verified['layers'] and 1<=(state.get('layer_num') or 0)<=verified['layers']:
    record['phase']='first-layer';record['first_layer_reached']=True;write(record_path,record);break
   if state.get('print_error') or state.get('gcode_state') in ['FAILED','PAUSE']:
    record['needs_attention']=True;write(record_path,record);break
   time.sleep(15)
  try:record['startup_camera']=run.capture(str(B),device+'-'+args.boat+'-start')
  except Exception as error:record['camera_error']=type(error).__name__
  write(record_path,record)
 finally:p.close()
if __name__=='__main__':main()
