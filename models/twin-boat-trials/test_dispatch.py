"""No-network regressions for single-send, durable intent and upload refusal."""
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch
import contextlib,importlib.util,io,json,tempfile,shutil
B=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('boat_dispatch',B/'dispatch.py');d=importlib.util.module_from_spec(spec);spec.loader.exec_module(d)
for scenario in ['acknowledged','lost-ack','corrupt-upload']:
 with tempfile.TemporaryDirectory(prefix='boat-dispatch-test-') as temporary:
  root=Path(temporary);folder=root/'monohull';(folder/'slice').mkdir(parents=True)
  for src,dst in [(B/'monohull/package-validation.json',folder/'package-validation.json'),(B/'monohull/slice/monohull.gcode.3mf',folder/'slice/monohull.gcode.3mf')]:shutil.copyfile(src,dst)
  shutil.copytree(B/'monohull/presets',folder/'presets')
  state={'gcode_state':'FINISH','print_error':0,'hms':[],'nozzle_diameter':'0.4'};calls=[];replies=[];stored={};clock=[0]
  def now():clock[0]+=10;return clock[0]
  def send(command):
   record=json.loads((folder/'print-run.json').read_text());assert record['dispatch_attempted'] and record['phase']=='start-intent','Send preceded durable intent'
   calls.append(command);assert len(calls)==1
   assert record['remote_name']==command['print']['url'].removeprefix('file:///sdcard/')
   state.update(gcode_state='RUNNING',layer_num=1,total_layer_num=175,subtask_name=command['print']['subtask_name'])
   if scenario=='acknowledged':replies.append({'command':'project_file','sequence_id':command['print']['sequence_id'],'result':'success'})
  printer=SimpleNamespace(state=state,replies=replies,summary=lambda:dict(state),spools=lambda:[('external','PLA','yellow',True)],send=send,close=lambda:None)
  class FTP:
   def __init__(self,**kw):pass
   def connect(self,*a):pass
   def login(self,*a):pass
   def prot_p(self):pass
   def storbinary(self,cmd,f):stored['bytes']=f.read()
   def size(self,*a):return len(stored['bytes'])
   def voidcmd(self,*a):pass
   def transfercmd(self,*a):
    class Stream(io.BytesIO):
     recv=io.BytesIO.read
    data=stored['bytes'];return Stream(data if scenario!='corrupt-upload' else bytes([data[0]^1])+data[1:])
   def voidresp(self):pass
   def close(self):pass
  run=SimpleNamespace(capture=lambda *a:'synthetic-camera.png')
  fake_context=SimpleNamespace(ROOT=root,connect=lambda device:(run,printer))
  with patch.object(d,'B',root),patch.object(d,'context',fake_context),patch.object(d.time,'monotonic',now),patch.object(d.time,'sleep',lambda _:None),patch.dict('os.environ',{'BAMBU_SERIAL':'TEST','BAMBU_HOST':'127.0.0.1','BAMBU_ACCESS_CODE':'synthetic'}),patch.dict('sys.modules',{'ftp':SimpleNamespace(ImplicitFTPS=FTP)}),patch('sys.argv',['dispatch.py','monohull','--spool','external','--confirm-bed-clear','--authorization-note','Synthetic test only']),contextlib.redirect_stdout(io.StringIO()):
   if scenario=='corrupt-upload':
    try:d.main();raise AssertionError('Corrupt upload accepted')
    except ValueError as e:assert str(e)=='Remote content mismatch'
    assert not calls
   else:
    d.main();record=json.loads((folder/'print-run.json').read_text());assert record['first_layer_reached'] and len(calls)==1
    if scenario=='lost-ack':assert record['acknowledgement_uncertain']
    try:d.main();raise AssertionError('Duplicate dispatch accepted')
    except AssertionError as e:assert str(e).startswith('Existing dispatch record')
    assert len(calls)==1
  print(scenario,'PASS')
