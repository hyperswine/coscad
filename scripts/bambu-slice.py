#!/usr/bin/env python3
"""Slice the beds that `coscad next` packed, with Bambu Studio's CLI, and
optionally send one to a Bambu printer in LAN developer mode.

  scripts/bambu-slice.py foo_manifest.json                 # slice every bed
  scripts/bambu-slice.py foo_manifest.json --bed 2         # one bed
  scripts/bambu-slice.py foo_manifest.json --print 1       # slice, upload bed 1, start it

Each bed becomes `<base>_bedN.gcode.3mf` next to the manifest (the file a
Bambu printer accepts) plus `<base>_print.json` with time and filament per
bed. Presets are Bambu Studio's own system presets, flattened (the CLI does
not resolve `inherits`, which silently leaves filament density, flow and
temperatures at defaults). Printing needs BAMBU_HOST, BAMBU_SERIAL and
BAMBU_ACCESS_CODE and the bambu-lan scripts (BAMBU_LAN_DIR, default
~/Documents/GitHub/3d-models/bambu-lan) for the FTPS upload.
"""
import argparse, json, os, shutil, subprocess, sys, tempfile, time

APP = os.environ.get("BAMBU_STUDIO", "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio")
SYSTEM = os.environ.get("BAMBU_PROFILES", os.path.expanduser("~/Library/Application Support/BambuStudio/system/BBL"))
BED_TYPES = {"textured": "Textured PEI Plate", "cool": "Cool Plate", "smooth": "Cool Plate",
             "engineering": "Engineering Plate", "hightemp": "High Temp Plate", "supertack": "Supertack Plate"}
FILAMENT_BY_MATERIAL = {"pla": "Bambu PLA Basic", "petg": "Bambu PETG Basic", "abs": "Bambu ABS",
                        "asa": "Bambu ASA", "tpu": "Bambu TPU 95A HF", "pa": "Bambu PA-CF"}


def flatten(kind, name):
    """Resolve a system preset's `inherits` chain into one flat dict."""
    path = os.path.join(SYSTEM, kind, name + ".json")
    if not os.path.exists(path):
        sys.exit(f"no {kind} preset '{name}' under {SYSTEM}/{kind}")
    d = json.load(open(path))
    parent = d.pop("inherits", None)
    base = flatten(kind, parent) if parent else {}
    base.update(d)
    base["inherits"] = ""
    return base


def write_presets(printer, process, filament, bed):
    pdir = tempfile.mkdtemp(prefix="coscad-presets-")
    proc = flatten("process", process)
    proc["curr_bed_type"] = bed
    files = {}
    for kind, name, data in (("machine", printer, flatten("machine", printer)),
                             ("process", process, proc),
                             ("filament", filament, flatten("filament", filament))):
        f = os.path.join(pdir, kind + ".json")
        json.dump(data, open(f, "w"), indent=1)
        files[kind] = f
    return files


def parse_gcode_header(path, needle):
    with open(path, errors="replace") as fh:
        for line in fh:
            if line.startswith(needle):
                return (line.split(":", 1)[1] if ":" in line else line).split(";")[0].strip()
            if line.startswith("; CONFIG_BLOCK_END"):
                break
    return None


def slice_bed(stl, out3mf, presets, extra):
    work = tempfile.mkdtemp(prefix="coscad-slice-")
    cmd = [APP, "--debug", "1",
           "--load-settings", presets["machine"] + ";" + presets["process"],
           "--load-filaments", presets["filament"],
           "--slice", "0", "--export-3mf", os.path.basename(out3mf), "--outputdir", work] + extra + [stl]
    r = subprocess.run(cmd, capture_output=True, text=True)
    res_path = os.path.join(work, "result.json")
    res = json.load(open(res_path)) if os.path.exists(res_path) else {}
    if r.returncode != 0 or res.get("return_code", 1) != 0:
        tail = "\n".join(l for l in r.stdout.splitlines() if not l.startswith("[")[-10:])
        sys.exit(f"slicing {stl} failed: {res.get('error_string', r.returncode)}\n{tail}")
    shutil.move(os.path.join(work, os.path.basename(out3mf)), out3mf)
    gcode = os.path.join(work, "plate_1.gcode")
    info = {"3mf": out3mf}
    if os.path.exists(gcode):
        info["print_time"] = parse_gcode_header(gcode, "; model printing time")
        info["filament_g"] = parse_gcode_header(gcode, "; total filament weight [g]")
        info["layers"] = parse_gcode_header(gcode, "; total layer number")
    for plate in res.get("sliced_plates", []):
        info["objects"] = [o["name"] for o in plate.get("objects", [])]
        info["estimated_s"] = plate.get("total_predication")
        if plate.get("warning_message"):
            info["warning"] = plate["warning_message"]
    shutil.rmtree(work, ignore_errors=True)
    return info


def lan_print(threemf, name):
    """Upload to the printer's SD card over FTPS and start it over MQTT."""
    lan = os.environ.get("BAMBU_LAN_DIR", os.path.expanduser("~/Documents/GitHub/3d-models/bambu-lan"))
    sys.path.insert(0, lan)
    for v in ("BAMBU_HOST", "BAMBU_SERIAL", "BAMBU_ACCESS_CODE"):
        if not os.environ.get(v):
            sys.exit(f"set {v} (see {lan}/README.md)")
    import ftp as lanftp  # noqa: the bambu-lan implicit-FTPS client
    import ssl
    import paho.mqtt.client as mqtt
    f = lanftp.connect()
    with open(threemf, "rb") as fh:
        f.storbinary(f"STOR {name}", fh)
    f.quit()
    print(f"uploaded {threemf} -> /{name}")
    serial, host, code = os.environ["BAMBU_SERIAL"], os.environ["BAMBU_HOST"], os.environ["BAMBU_ACCESS_CODE"]
    cmd = {"print": {"sequence_id": "1", "command": "project_file", "param": "Metadata/plate_1.gcode",
                     "project_id": "0", "profile_id": "0", "task_id": "0", "subtask_id": "0",
                     "subtask_name": os.path.splitext(name)[0], "url": f"file:///sdcard/{name}", "md5": "",
                     "timelapse": False, "bed_type": "auto", "bed_levelling": True, "flow_cali": False,
                     "vibration_cali": True, "layer_inspect": False, "use_ams": False, "ams_mapping": [0]}}
    reply = {}
    c = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=f"coscad-{os.getpid()}")
    c.username_pw_set("bblp", code)
    ctx = ssl.create_default_context(); ctx.check_hostname = False; ctx.verify_mode = ssl.CERT_NONE
    c.tls_set_context(ctx)
    c.on_connect = lambda cl, *_: (cl.subscribe(f"device/{serial}/report"), cl.publish(f"device/{serial}/request", json.dumps(cmd)))
    def on_message(cl, ud, msg):
        p = json.loads(msg.payload).get("print", {})
        if p.get("command") == "project_file":
            reply.update(p)
    c.on_message = on_message
    c.connect(host, 8883, keepalive=60)
    c.loop_start()
    t0 = time.time()
    while not reply and time.time() - t0 < 15:
        time.sleep(0.2)
    c.loop_stop(); c.disconnect()
    if not reply:
        sys.exit("printer did not answer the print command (is developer mode on?)")
    print("printer reply:", reply.get("result"), reply.get("reason", ""))
    if reply.get("result") != "success":
        sys.exit(1)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("manifest", help="<base>_manifest.json written by `coscad next`")
    ap.add_argument("--printer", default="Bambu Lab A1 0.4 nozzle", help="machine preset name")
    ap.add_argument("--process", default=None, help="process preset (default: the printer's default profile)")
    ap.add_argument("--filament", default=None, help="filament preset (default: from the parts' material= hint, else the printer's default)")
    ap.add_argument("--plate", default="textured", choices=sorted(BED_TYPES), help="build plate on the printer (default textured)")
    ap.add_argument("--bed", type=int, default=None, help="only this bed index")
    ap.add_argument("--print", dest="print_bed", type=int, metavar="N", help="after slicing, upload bed N and start printing it")
    ap.add_argument("--slicer-arg", action="append", default=[], help="extra Bambu Studio CLI argument (repeatable)")
    a = ap.parse_args()

    if not os.path.exists(APP):
        sys.exit(f"Bambu Studio not found at {APP} (set BAMBU_STUDIO)")
    m = json.load(open(a.manifest))
    if "beds" not in m:
        sys.exit("this manifest has no beds: run `coscad next` first")
    base = a.manifest[: -len("_manifest.json")] if a.manifest.endswith("_manifest.json") else os.path.splitext(a.manifest)[0]
    outdir = os.path.dirname(os.path.abspath(a.manifest))
    machine = flatten("machine", a.printer)
    process = a.process or machine.get("default_print_profile")
    filament = a.filament
    if not filament:
        mats = {v.get("hints", {}).get("material", "").lower() for v in m.get("variants", [])} - {"", "printed"}
        if len(mats) > 1:
            print(f"note: parts ask for several materials ({', '.join(sorted(mats))}); pass --filament to choose", file=sys.stderr)
        elif mats and FILAMENT_BY_MATERIAL.get(next(iter(mats))):
            cand = FILAMENT_BY_MATERIAL[next(iter(mats))] + " @BBL " + machine["printer_model"].split()[-1]
            if os.path.exists(os.path.join(SYSTEM, "filament", cand + ".json")):
                filament = cand
        filament = filament or machine.get("default_filament_profile", [None])[0]
    bed_name = BED_TYPES[a.plate]
    presets = write_presets(a.printer, process, filament, bed_name)
    print(f"printer {a.printer} | process {process} | filament {filament} | {bed_name}")

    pw, pd = m["bed"]["w"], m["bed"]["d"]
    area = machine.get("printable_area") or []
    if area:
        xs = [float(p.split("x")[0]) for p in area]; ys = [float(p.split("x")[1]) for p in area]
        if pw > max(xs) - min(xs) + 1e-6 or pd > max(ys) - min(ys) + 1e-6:
            print(f"warning: the assembly's plate ({pw}x{pd}) is larger than the printer's bed ({max(xs)-min(xs):g}x{max(ys)-min(ys):g})", file=sys.stderr)

    summary = {"source": m.get("source"), "printer": a.printer, "process": process, "filament": filament, "plate": bed_name, "beds": []}
    for bed in m["beds"]:
        if a.bed is not None and bed["index"] != a.bed:
            continue
        stl = bed["stl"] if os.path.isabs(bed["stl"]) else os.path.join(outdir, os.path.basename(bed["stl"]))
        out3mf = f"{base}_bed{bed['index']}.gcode.3mf"
        t0 = time.time()
        info = slice_bed(stl, out3mf, presets, a.slicer_arg)
        info.update({"index": bed["index"], "parts": [p["instance"] for p in bed["placements"]], "slice_s": round(time.time() - t0, 1)})
        summary["beds"].append(info)
        print(f"bed {bed['index']}: {len(info['parts'])} parts, {info.get('print_time')}, {info.get('filament_g')} g -> {out3mf}")
        if info.get("warning"):
            print("  slicer warning:", info["warning"])
    json.dump(summary, open(f"{base}_print.json", "w"), indent=2)
    print(f"Wrote {base}_print.json")
    shutil.rmtree(os.path.dirname(presets["machine"]), ignore_errors=True)

    if a.print_bed is not None:
        hit = [b for b in summary["beds"] if b["index"] == a.print_bed]
        if not hit:
            sys.exit(f"bed {a.print_bed} was not sliced")
        lan_print(hit[0]["3mf"], os.path.basename(hit[0]["3mf"]))


if __name__ == "__main__":
    main()
