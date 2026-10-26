"""Local credential bridge for a1 / p1s / x1c; no credential values are emitted or recorded.
Reads 3d-models/.env (BAMBU_<DEV>_HOST / _SERIAL / _ACCESS_CODE), verifies the printer's TLS
identity against Bambu's printer CA with the serial as the peer name."""
from pathlib import Path
import importlib.util, os, socket, ssl, time
ROOT = Path(__file__).resolve().parents[3]
CA = '/private/tmp/boat-printer-ca.pem'
KNOWN = {'a1': ('192.168.0.63', '03919D4A1103804'), 'p1s': ('192.168.0.27', '01P00A442400974'), 'x1c': ('192.168.0.14', '00M09D550701167')}
def connect(device):
    values = {}
    for line in (ROOT/'3d-models/.env').read_text().splitlines():
        if '=' in line and not line.lstrip().startswith('#'):
            k, v = line.split('=', 1); values[k.strip()] = v.strip().strip('"').strip("'")
    pre = 'BAMBU_' + device.upper() + '_'
    host, serial = KNOWN[device]
    assert values.get(pre+'SERIAL', serial) == serial, 'Serial in .env does not match the recorded printer identity'
    code = values.get(pre+'ACCESS_CODE', '')
    assert code, 'Printer configuration is incomplete'
    os.environ.update(BAMBU_HOST=host, BAMBU_SERIAL=serial, BAMBU_ACCESS_CODE=code)
    socket.setdefaulttimeout(15)
    spec = importlib.util.spec_from_file_location('bambu_run', ROOT/'coscad/scripts/bambu-print-run.py')
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    class SerialTLS(ssl.SSLContext):
        def wrap_socket(self, *args, **kw):
            kw['server_hostname'] = serial
            return super().wrap_socket(*args, **kw)
    def tls():
        ctx = SerialTLS(ssl.PROTOCOL_TLS_CLIENT); ctx.load_verify_locations(CA); return ctx
    class VerifiedPrinter(module.Printer):
        def __init__(self):
            import paho.mqtt.client as mqtt
            self.host, self.serial, self.code = host, serial, code
            self.state, self.replies = {}, []
            self.c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f'coscad-beach-{os.getpid()}')
            self.c.username_pw_set('bblp', code); self.c.tls_set_context(tls())
            self.c.on_connect = self._on_connect; self.c.on_message = self._on_message
            self.c.connect(host, 8883, keepalive=60); self.c.loop_start()
            end = time.monotonic() + 20
            while 'gcode_state' not in self.state and time.monotonic() < end: time.sleep(.2)
            if 'gcode_state' not in self.state:
                self.close(); raise RuntimeError('No fresh status from verified printer identity')
    return module, VerifiedPrinter(), tls
