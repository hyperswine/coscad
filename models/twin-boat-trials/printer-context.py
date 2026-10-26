"""Local credential bridge; no credential values are emitted or recorded."""
from pathlib import Path
import importlib.util, os, socket, ssl, time, struct, io
ROOT=Path(__file__).resolve().parents[3]
def connect(device):
    values={}
    for line in (ROOT/'3d-models/.env').read_text().splitlines():
        if '=' in line and not line.lstrip().startswith('#'):
            k,v=line.split('=',1); values[k.strip()]=v.strip().strip('\"').strip("'")
    prefix='BAMBU_P1S_' if device=='p1s' else 'BAMBU_'
    # The P1S moved from the old .env address; verified dashboard configuration
    # and serial identity pin it to the replacement address.
    host='192.168.0.27' if device=='p1s' else values.get('BAMBU_A1_HOST',values.get('BAMBU_HOST','192.168.0.63'))
    def value(field):
        if device=='a1':return values.get('BAMBU_A1_'+field,values.get('BAMBU_'+field,'03919D4A1103804' if field=='SERIAL' else ''))
        return values.get(prefix+field,'')
    serial=value('SERIAL'); code=value('ACCESS_CODE')
    assert serial and code, 'Printer configuration is incomplete'
    if device=='p1s':assert serial=='01P00A442400974'
    os.environ.update(BAMBU_HOST=host,BAMBU_SERIAL=serial,BAMBU_ACCESS_CODE=code)
    socket.setdefaulttimeout(15)
    spec=importlib.util.spec_from_file_location('bambu_run',ROOT/'coscad/scripts/bambu-print-run.py')
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
    class SerialTLS(ssl.SSLContext):
        def wrap_socket(self,*args,**kw):
            kw['server_hostname']=serial
            return super().wrap_socket(*args,**kw)
    def tls():
        ctx=SerialTLS(ssl.PROTOCOL_TLS_CLIENT)
        ctx.load_verify_locations('/private/tmp/boat-printer-ca.pem')
        return ctx
    class VerifiedPrinter(module.Printer):
        def __init__(self):
            import paho.mqtt.client as mqtt
            self.host,self.serial,self.code=host,serial,code
            self.state,self.replies={},[]
            self.c=mqtt.Client(mqtt.CallbackAPIVersion.VERSION2,client_id=f'coscad-boat-{os.getpid()}')
            self.c.username_pw_set('bblp',code);self.c.tls_set_context(tls())
            self.c.on_connect=self._on_connect;self.c.on_message=self._on_message
            self.c.connect(host,8883,keepalive=60);self.c.loop_start()
            end=time.monotonic()+20
            while 'gcode_state' not in self.state and time.monotonic()<end:time.sleep(.2)
            if 'gcode_state' not in self.state:
                self.close();raise RuntimeError('No fresh status from verified printer identity')
    def capture(outdir,tag):
        from PIL import Image
        auth=struct.pack('<IIII',0x40,0x3000,0,0)+b'bblp'.ljust(32,b'\0')+code.encode().ljust(32,b'\0')
        with tls().wrap_socket(socket.create_connection((host,6000),timeout=15)) as connection:
            connection.sendall(auth)
            def exact(n):
                result=b''
                while len(result)<n:
                    data=connection.recv(n-len(result))
                    if not data:raise ConnectionError('Camera closed early')
                    result+=data
                return result
            size=struct.unpack('<I',exact(16)[:4])[0]
            assert 0<size<10485760,'Invalid camera frame size'
            data=exact(size)
        assert data.startswith(b'\xff\xd8')
        path=Path(outdir)/(time.strftime('%Y%m%d-%H%M%S')+'-'+tag+'-printer-capture.png')
        Image.open(io.BytesIO(data)).convert('RGB').save(path)
        return str(path)
    module.capture=capture
    return module,VerifiedPrinter()
