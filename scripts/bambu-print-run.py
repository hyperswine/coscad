#!/usr/bin/env python3
"""Photograph the printer, run an assembly end to end (coscad → next → plan →
slice → print), wait for the print, photograph it again, confirm it worked.

  scripts/bambu-print-run.py examples/assemble/ball/ball.assemble
  scripts/bambu-print-run.py foo.assemble --dry-run      # everything except the printer

Photos land next to the assembly as <timestamp>-a1-printer-capture.png.
A JSON report (<base>_run.json) records both photos, the printer's state
before and after, the bed that was printed and the verdict.

Printer access: BAMBU_ACCESS_CODE (printer screen: Settings > LAN Only
Mode); BAMBU_HOST and BAMBU_SERIAL are discovered from the printer's
SSDP broadcasts when unset. All three may live in $BAMBU_ENV_FILE
(default ~/.config/bambu/a1.env, KEY=VALUE lines), which is read first.
Needs paho-mqtt, Bambu Studio, and the bambu-lan scripts (BAMBU_LAN_DIR).
"""
import argparse, importlib.util, json, os, socket, ssl, struct, subprocess, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
LAN = os.environ.get("BAMBU_LAN_DIR", os.path.expanduser("~/Documents/GitHub/3d-models/bambu-lan"))
FINAL_STATES = {"FINISH", "FAILED", "IDLE"}


def load_env_file():
    f = os.environ.get("BAMBU_ENV_FILE", os.path.expanduser("~/.config/bambu/a1.env"))
    if os.path.exists(f):
        for line in open(f):
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                k, v = line.split("=", 1)
                os.environ.setdefault(k.strip(), v.strip().strip('"'))


def discover(seconds=20):
    """First LAN-mode printer heard on UDP 2021 → (ip, serial, name)."""
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if hasattr(socket, "SO_REUSEPORT"):
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
    s.bind(("", 2021)); s.settimeout(1)
    end = time.time() + seconds
    while time.time() < end:
        try:
            data, (ip, _) = s.recvfrom(4096)
        except socket.timeout:
            continue
        text = data.decode(errors="replace")
        if "bambulab" not in text:
            continue
        h = dict(l.split(": ", 1) for l in text.splitlines() if ": " in l)
        if h.get("DevConnect.bambu.com") == "lan":
            return ip, h.get("USN"), h.get("DevName.bambu.com")
    sys.exit("no LAN-mode Bambu printer heard in %ds; set BAMBU_HOST and BAMBU_SERIAL" % seconds)


def capture(outdir):
    """One camera frame (port 6000, JPEG) → <timestamp>-a1-printer-capture.png."""
    host, code = os.environ["BAMBU_HOST"], os.environ["BAMBU_ACCESS_CODE"]
    auth = struct.pack("<IIII", 0x40, 0x3000, 0, 0) + b"bblp".ljust(32, b"\0") + code.encode().ljust(32, b"\0")
    ctx = ssl.create_default_context(); ctx.check_hostname = False; ctx.verify_mode = ssl.CERT_NONE
    s = ctx.wrap_socket(socket.create_connection((host, 6000), timeout=15), server_hostname=host)
    s.sendall(auth)
    def recv_exact(k):
        b = b""
        while len(b) < k:
            chunk = s.recv(k - len(b))
            if not chunk:
                raise ConnectionError("camera stream closed")
            b += chunk
        return b
    size = struct.unpack("<I", recv_exact(16)[:4])[0]
    jpg = recv_exact(size)
    s.close()
    if jpg[:2] != b"\xff\xd8":
        sys.exit("camera did not return a JPEG")
    stamp = time.strftime("%Y%m%d-%H%M%S")
    jpg_path = os.path.join(outdir, f"{stamp}-a1-printer-capture.jpg")
    png_path = os.path.join(outdir, f"{stamp}-a1-printer-capture.png")
    open(jpg_path, "wb").write(jpg)
    r = subprocess.run(["sips", "-s", "format", "png", jpg_path, "--out", png_path], capture_output=True)
    if r.returncode == 0 and os.path.exists(png_path):
        os.remove(jpg_path)
        return png_path
    return jpg_path  # no sips (not macOS): keep the JPEG


class Printer:
    """Minimal MQTT session: full status on connect, then merged deltas."""
    def __init__(self):
        import paho.mqtt.client as mqtt
        self.host, self.serial, self.code = os.environ["BAMBU_HOST"], os.environ["BAMBU_SERIAL"], os.environ["BAMBU_ACCESS_CODE"]
        self.state, self.replies = {}, []
        self.c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f"coscad-run-{os.getpid()}")
        self.c.username_pw_set("bblp", self.code)
        ctx = ssl.create_default_context(); ctx.check_hostname = False; ctx.verify_mode = ssl.CERT_NONE
        self.c.tls_set_context(ctx)
        self.c.on_connect = self._on_connect
        self.c.on_message = self._on_message
        self.c.connect(self.host, 8883, keepalive=60)
        self.c.loop_start()
        t0 = time.time()
        while "gcode_state" not in self.state and time.time() - t0 < 15:
            time.sleep(0.2)
        if "gcode_state" not in self.state:
            sys.exit("printer did not report its status over MQTT (wrong access code?)")

    def _on_connect(self, cl, ud, flags, rc, props):
        if rc != 0:
            sys.exit(f"MQTT connect failed: {rc} (wrong access code?)")
        cl.subscribe(f"device/{self.serial}/report")
        self.send({"pushing": {"sequence_id": "0", "command": "pushall", "version": 1, "push_target": 1}})

    def _on_message(self, cl, ud, msg):
        data = json.loads(msg.payload)
        p = data.get("print", {})
        if "command" in p and p["command"] not in ("push_status",):
            self.replies.append(p)
        for k, v in p.items():
            if isinstance(v, dict) and isinstance(self.state.get(k), dict):
                self.state[k].update(v)
            else:
                self.state[k] = v

    def send(self, payload):
        self.c.publish(f"device/{self.serial}/request", json.dumps(payload))

    def summary(self):
        s = self.state
        return {k: s.get(k) for k in ("gcode_state", "mc_percent", "layer_num", "total_layer_num", "mc_remaining_time",
                                      "gcode_file", "subtask_name", "nozzle_temper", "bed_temper", "print_error")}

    def spools(self):
        """What filament the printer has: [(spool, type, colour, loaded)]."""
        ams = self.state.get("ams") or {}
        now = str(ams.get("tray_now", "255"))
        out = []
        for unit in ams.get("ams", []):
            for t in unit.get("tray", []):
                if t.get("tray_type"):
                    tid = int(t.get("id", 0))
                    out.append((f"ams{tid}", t["tray_type"], t.get("tray_color", ""), now == str(tid)))
        vt = self.state.get("vt_tray") or {}
        if vt.get("tray_type"):
            out.append(("external", vt["tray_type"], vt.get("tray_color", ""), now == "254"))
        return out

    def wait_until_done(self, started_at, log_every=60):
        """Block until the print leaves RUNNING/PREPARE; log progress."""
        last, seen_running = 0, False
        while True:
            st = self.state.get("gcode_state")
            if st in ("RUNNING", "PREPARE", "PAUSE"):
                seen_running = True
            elif seen_running and st in FINAL_STATES:
                return st
            elif not seen_running and time.time() - started_at > 180:
                return st or "UNKNOWN"
            if time.time() - last >= log_every:
                s = self.summary()
                print(f"  {time.strftime('%H:%M:%S')} {s['gcode_state']} {s['mc_percent']}% layer {s['layer_num']}/{s['total_layer_num']} "
                      f"{s['mc_remaining_time']} min left  nozzle {s['nozzle_temper']}°C bed {s['bed_temper']}°C", flush=True)
                last = time.time()
            time.sleep(2)

    def close(self):
        self.c.loop_stop(); self.c.disconnect()


def error_text(code):
    """Bambu Studio's own error table (hms/hms_en.json) → intro text for print_error."""
    try:
        d = json.load(open(os.path.expanduser("~/Library/Application Support/BambuStudio/hms/hms_en.json")))
        want = "%08X" % int(code)
        stack = [d]
        while stack:
            o = stack.pop()
            if isinstance(o, dict):
                if str(o.get("ecode", "")).upper() == want:
                    return o.get("intro", "")
                stack.extend(o.values())
            elif isinstance(o, list):
                stack.extend(o)
    except Exception:
        pass
    return ""


def run(cmd, cwd):
    print("$", " ".join(cmd), flush=True)
    r = subprocess.run(cmd, cwd=cwd)
    if r.returncode != 0:
        sys.exit(f"step failed ({r.returncode}): {' '.join(cmd)}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("assemble")
    ap.add_argument("--bed", type=int, default=1, help="bed index to print (default 1)")
    ap.add_argument("--dry-run", action="store_true", help="run the modelling and slicing stages only")
    ap.add_argument("--coscad", default=os.environ.get("COSCAD", "coscad"), help="coscad binary")
    ap.add_argument("--slice-arg", action="append", default=[], help="extra argument for bambu-slice.py (repeatable)")
    ap.add_argument("--spool", default=None, help="external or ams0..ams3; default: whatever is loaded, else the one AMS slot with the right material")
    a = ap.parse_args()

    spec = os.path.abspath(a.assemble)
    cwd, base = os.path.dirname(spec), os.path.basename(spec)[:-len(".assemble")]
    report = {"assemble": spec, "started": time.strftime("%Y-%m-%dT%H:%M:%S"), "steps": []}
    report_path = os.path.join(cwd, base + "_run.json")

    load_env_file()
    printer = None
    if not a.dry_run:
        if not os.environ.get("BAMBU_ACCESS_CODE"):
            sys.exit("BAMBU_ACCESS_CODE is not set (printer screen: Settings > LAN Only Mode); put it in ~/.config/bambu/a1.env")
        if not (os.environ.get("BAMBU_HOST") and os.environ.get("BAMBU_SERIAL")):
            ip, serial, name = discover()
            os.environ["BAMBU_HOST"], os.environ["BAMBU_SERIAL"] = ip, serial
            print(f"printer: {name} at {ip}")
        # 1. before: state + photo; refuse to start on a busy printer
        printer = Printer()
        before = printer.summary()
        print("printer before:", json.dumps(before))
        if before["gcode_state"] in ("RUNNING", "PREPARE", "PAUSE"):
            sys.exit(f"printer is busy ({before['gcode_state']}, {before['mc_percent']}%); not starting another print")
        before_photo = capture(cwd)
        print("photo before:", before_photo)
        report.update(before=before, photo_before=before_photo)
        # which spool: the loaded one, else the single AMS slot with the material,
        # else the external spool (the printer will ask for the filament to be fed)
        spools = printer.spools()
        print("spools:", ", ".join(f"{s} {t} {c}{' (loaded)' if l else ''}" for s, t, c, l in spools) or "none reported")
        if not a.spool:
            loaded = [s for s, _, _, l in spools if l]
            material = (a.slice_arg[a.slice_arg.index("--filament") + 1].split()[1] if "--filament" in a.slice_arg else "PLA").upper()
            same = [s for s, t, _, _ in spools if s.startswith("ams") and t.upper().startswith(material)]
            a.spool = loaded[0] if loaded else same[0] if len(same) == 1 else "external"
            if a.spool == "external" and not any(s == "external" for s, _, _, _ in spools):
                sys.exit("no filament loaded, no single matching AMS slot and no external spool reported; pass --spool")
        print("using spool:", a.spool)
        report["spool"] = a.spool

    # 2. the coscad chain
    for stage in (["%s" % base + ".assemble"], ["next", base + ".assemble"], ["plan", base + ".assemble"]):
        run([a.coscad] + stage, cwd)
        report["steps"].append(" ".join(stage))
    slice_cmd = [sys.executable, os.path.join(HERE, "bambu-slice.py"), base + "_manifest.json", "--bed", str(a.bed)] + a.slice_arg
    if not a.dry_run:
        slice_cmd += ["--print", str(a.bed), "--spool", a.spool]
    run(slice_cmd, cwd)
    print_info = json.load(open(os.path.join(cwd, base + "_print.json")))
    bed = next(b for b in print_info["beds"] if b["index"] == a.bed)
    report.update(bed=bed, sliced=print_info)
    print(f"bed {a.bed}: {bed.get('print_time')}, {bed.get('filament_g')} g, parts {', '.join(bed['parts'])}")
    if a.dry_run:
        report["verdict"] = "dry-run: sliced, nothing sent to the printer"
        json.dump(report, open(report_path, "w"), indent=2)
        print("Wrote", report_path)
        return

    # 3. watch the print (bambu-slice.py --print already uploaded and started it)
    started = time.time()
    print("waiting for the printer to start...", flush=True)
    final = printer.wait_until_done(started)
    after = printer.summary()
    took = int(time.time() - started)
    print("printer after:", json.dumps(after))
    after_photo = capture(cwd)
    print("photo after:", after_photo)

    # 4. verdict
    expected = os.path.basename(bed["3mf"])
    ran_ours = expected.split(".")[0] in str(after.get("gcode_file") or "") + str(after.get("subtask_name") or "")
    ok = final == "FINISH" and (after.get("print_error") in (0, None)) and ran_ours
    verdict = ("OK: printed %s in %dm %02ds, finished with no error" % (expected, took // 60, took % 60)) if ok else \
              f"FAILED: final state {final}, print_error {after.get('print_error')} ({error_text(after.get('print_error') or 0) or 'unknown'}), file {after.get('gcode_file') or after.get('subtask_name')} (expected {expected})"
    report.update(after=after, photo_after=after_photo, final_state=final, seconds=took, verdict=verdict,
                  finished=time.strftime("%Y-%m-%dT%H:%M:%S"))
    json.dump(report, open(report_path, "w"), indent=2)
    printer.close()
    print(verdict)
    print("Wrote", report_path)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
