#!/usr/bin/env python3
"""Slice the beds that `coscad next` packed, with Bambu Studio's CLI, and
optionally send one to a Bambu printer in LAN developer mode.

  scripts/bambu-slice.py foo_manifest.json                 # slice every bed
  scripts/bambu-slice.py foo_manifest.json --bed 2         # one bed
  scripts/bambu-slice.py foo_manifest.json --print 1       # slice, upload bed 1, start it
  scripts/bambu-slice.py foo_manifest.json --accept-printability   # slice despite reviewed warnings

Each bed becomes `<base>_bedN.gcode.3mf` next to the manifest (the file a
Bambu printer accepts) plus `<base>_print.json` with time and filament per
bed. Presets are Bambu Studio's own system presets, flattened (the CLI does
not resolve `inherits`, which silently leaves filament density, flow and
temperatures at defaults). Printing needs BAMBU_HOST, BAMBU_SERIAL and
BAMBU_ACCESS_CODE and the bambu-lan scripts (BAMBU_LAN_DIR, default
~/Documents/GitHub/3d-models/bambu-lan) for the FTPS upload.

Printability: the warnings `coscad next` recorded for the parts on each
bed are printed, written to `<base>_print.json`, and checked against the
process. Small-contact and tall/narrow parts need an explicit brim,
flat-overhang and elevated-start parts need supports; anything else
needs one or the other. Unaddressed warnings refuse the slice (exit 2)
unless --accept-printability is given. A brim the manifest reserved
(`brim=N` hints, `plate ... brim=N`) is applied as an outer brim when the
process does not set one, and an explicit brim wider than a part's
reserved space is reported.
"""
import argparse, json, os, shutil, subprocess, sys, tempfile, time
from pathlib import Path

# Also works when this script is loaded by a model-specific importlib loader.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from bambu_preflight import checked_package, expectations, verify_package

APP = os.environ.get("BAMBU_STUDIO", "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio")
SYSTEM = os.environ.get("BAMBU_PROFILES", os.path.expanduser("~/Library/Application Support/BambuStudio/system/BBL"))
BED_TYPES = {"textured": "Textured PEI Plate", "cool": "Cool Plate", "smooth": "Cool Plate",
             "engineering": "Engineering Plate", "hightemp": "High Temp Plate", "supertack": "Supertack Plate"}
FILAMENT_BY_MATERIAL = {"pla": "Bambu PLA Basic", "petg": "Bambu PETG Basic", "abs": "Bambu ABS",
                        "asa": "Bambu ASA", "tpu": "Bambu TPU 95A HF", "pa": "Bambu PA-CF"}


def flatten(kind, name, active=()):
    """Resolve a system preset's `inherits` chain into one flat dict."""
    path = os.path.join(SYSTEM, kind, name + ".json")
    if not os.path.exists(path):
        # Vendor filaments (e.g. SUNLU) live in subdirectories, while their
        # inherits fields use unqualified preset names. Refuse ambiguous names.
        hits = [p for p in Path(SYSTEM, kind).rglob('*.json') if p.stem == name]
        if len(hits) != 1:
            sys.exit(f"{'ambiguous' if hits else 'no'} {kind} preset '{name}' under {SYSTEM}/{kind}")
        path = str(hits[0])
    identity = os.path.realpath(path)
    if identity in active:
        sys.exit(f"cycle in {kind} preset inheritance at '{name}'")
    with open(path, encoding="utf-8") as source:
        d = json.load(source)
    parent = d.pop("inherits", None)
    base = flatten(kind, parent, (*active, identity)) if parent else {}
    base.update(d)
    base["inherits"] = ""
    return base


def machine_with_templates(printer):
    """Bambu keeps a printer's start/end/filament-change/layer-change/timelapse
    gcode in `<printer> template <key>.json` files next to the preset, which
    the GUI merges in and the CLI does not. Without them the print runs the
    generic placeholder start gcode, which never loads filament (a print that
    moves but extrudes nothing)."""
    m = flatten("machine", printer)
    tdir = os.path.join(SYSTEM, "machine")
    names = [printer]
    parts = printer.rsplit(" ", 2)  # "... 0.6 nozzle" → also try the 0.4 nozzle templates
    if len(parts) == 3 and parts[2] == "nozzle" and parts[1] != "0.4":
        names.append(parts[0] + " 0.4 nozzle")
    found = 0
    for n in names:
        for f in sorted(os.listdir(tdir)):
            if f.startswith(n + " template ") and f.endswith(".json"):
                d = json.load(open(os.path.join(tdir, f)))
                for k, v in d.items():
                    if k.endswith("_gcode"):
                        m[k] = v
                        found += 1
        if found:
            break
    if not found:
        print(f"warning: no gcode templates found for '{printer}'; using the preset's inline gcode", file=sys.stderr)
    return m


def preset_overrides(path, preset, additional_keys=()):
    """Refuse misspelled or malformed settings instead of silently dropping them."""
    with open(path) as fh:
        data = json.load(fh)
    if not isinstance(data, dict):
        raise ValueError("preset overrides must be a JSON object")
    unknown = sorted(set(data) - set(preset) - set(additional_keys))
    if unknown:
        raise ValueError("unknown preset override settings: " + ", ".join(unknown))
    for key, value in data.items():
        if not (isinstance(value, str) or
                isinstance(value, list) and value and all(isinstance(v, str) for v in value)):
            raise ValueError(f"preset override {key} must be a string or a nonempty list of strings")
    return data


def resolve_process(process, overrides=None, extra=None):
    """The flattened process preset with explicit overrides, then any
    settings this script derives (`extra`, e.g. the manifest's brim)."""
    proc = flatten("process", process)
    if overrides:
        # brim_type can be omitted by presets that inherit the slicer's
        # compiled default; it is still an explicit supported process key.
        proc.update(preset_overrides(overrides, proc, {"brim_type"}))
    proc.update(extra or {})
    return proc


def write_presets(printer, process, filament, bed, overrides=None, filament_overrides=None, extra=None):
    """Write the three flat presets into a fresh temporary directory. The
    directory is removed again if anything fails here; on success the
    caller owns it (see `main`)."""
    proc = resolve_process(process, overrides, extra)
    proc["curr_bed_type"] = bed
    fila = flatten("filament", filament)
    if filament_overrides:
        fila.update(preset_overrides(filament_overrides, fila))
    machine = machine_with_templates(printer)
    pdir = tempfile.mkdtemp(prefix="coscad-presets-")
    try:
        files = {}
        for kind, data in (("machine", machine), ("process", proc), ("filament", fila)):
            f = os.path.join(pdir, kind + ".json")
            with open(f, "w") as fh:
                json.dump(data, fh, indent=1)
            files[kind] = f
        return files
    except BaseException:
        shutil.rmtree(pdir, ignore_errors=True)
        raise


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
    try:
        cmd = [APP, "--debug", "1",
               "--load-settings", presets["machine"] + ";" + presets["process"],
               "--load-filaments", presets["filament"],
               "--slice", "0", "--export-3mf", os.path.basename(out3mf), "--outputdir", work] + extra + [stl]
        r = subprocess.run(cmd, capture_output=True, text=True)
        res_path = os.path.join(work, "result.json")
        res = {}
        if os.path.exists(res_path):
            with open(res_path) as fh:
                res = json.load(fh)
        if r.returncode != 0 or res.get("return_code", 1) != 0:
            tail = "\n".join([l for l in (r.stdout + "\n" + r.stderr).splitlines()
                              if not l.startswith("[")][-10:])
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
        return info
    finally:
        shutil.rmtree(work, ignore_errors=True)


def spool_mapping(spool):
    """'external' → the spool holder (tray 254, no AMS); 'amsN' → AMS slot N (0-3)."""
    if spool in (None, "", "external"):
        return False, [254]
    if spool.startswith("ams") and spool[3:].isdigit():
        return True, [int(spool[3:])]
    sys.exit(f"--spool must be external or ams0..ams3, not '{spool}'")


def lan_print(threemf, name, spool="external", *, expected):
    """Verify a frozen package before any network access, then upload those bytes."""
    with checked_package(threemf, expected) as (package, report):
        _lan_print(package, report, name, spool)
    return report


def _lan_print(package, report, name, spool):
    """Upload to the printer's SD card over FTPS and start it over MQTT."""
    use_ams, mapping = spool_mapping(spool)
    lan = os.environ.get("BAMBU_LAN_DIR", os.path.expanduser("~/Documents/GitHub/3d-models/bambu-lan"))
    sys.path.insert(0, lan)
    for v in ("BAMBU_HOST", "BAMBU_SERIAL", "BAMBU_ACCESS_CODE"):
        if not os.environ.get(v):
            sys.exit(f"set {v} (see {lan}/README.md)")
    import ftp as lanftp  # noqa: the bambu-lan implicit-FTPS client
    import ssl
    import paho.mqtt.client as mqtt
    f = lanftp.connect()
    try:
        f.storbinary(f"STOR {name}", package)
        # The command must reference exactly the content that passed preflight.
        import hashlib
        remote_hash = hashlib.sha256()
        # Bambu's implicit FTPS sessions do not reliably complete TLS unwrap
        # on data connections. Close the read stream like the model dispatchers.
        f.voidcmd('TYPE I')
        with f.transfercmd(f'RETR {name}') as remote:
            while chunk := remote.recv(32768):
                remote_hash.update(chunk)
        f.voidresp()
        if remote_hash.hexdigest() != report['package_sha256']:
            raise ValueError('Uploaded package differs from the verified print file; refusing to start')
    finally:
        f.close()
    print(f"uploaded verified package -> /{name}")
    serial, host, code = os.environ["BAMBU_SERIAL"], os.environ["BAMBU_HOST"], os.environ["BAMBU_ACCESS_CODE"]
    cmd = {"print": {"sequence_id": "1", "command": "project_file", "param": "Metadata/plate_1.gcode",
                     "project_id": "0", "profile_id": "0", "task_id": "0", "subtask_id": "0",
                     "subtask_name": os.path.splitext(name)[0], "url": f"file:///sdcard/{name}", "md5": "",
                     "timelapse": False, "bed_type": "auto", "bed_levelling": True, "flow_cali": True,
                     "vibration_cali": True, "layer_inspect": True, "use_ams": use_ams, "ams_mapping": mapping}}
    print(f"filament from {'AMS slot %d' % (mapping[0] + 1) if use_ams else 'the external spool'}")
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


NEXT_SCHEMA = "coscad.next/1"
DESIGN_SCHEMA = "coscad.design/1"


def manifest_base(path):
    """`<base>` for `<base>_manifest.json`; other names drop only the extension."""
    return path[: -len("_manifest.json")] if path.endswith("_manifest.json") else os.path.splitext(path)[0]


def check_manifest(m, path):
    """Accept a `coscad next` manifest; refuse anything else with a fix.

    Manifests written before schemas existed (coscad 1.2 and earlier) carry
    no `schema`: one with `beds` is a manufacturing manifest and is accepted;
    one with a `plate` is the old design-stage output that shared the
    `_manifest.json` name.
    """
    if not isinstance(m, dict):
        raise ValueError(f"{path}: not a coscad manifest (expected a JSON object)")
    stem = path[: -len("_design.json")] if path.endswith("_design.json") else manifest_base(path)
    src = m.get("source") or stem + ".assemble"
    rerun = f"run `coscad next {src}` and slice the {os.path.basename(stem)}_manifest.json it writes"
    schema = m.get("schema")
    if schema == NEXT_SCHEMA:
        if not isinstance(m.get("beds"), list):
            raise ValueError(f"{path}: {NEXT_SCHEMA} manifest has no beds list")
        return
    if schema == DESIGN_SCHEMA:
        raise ValueError(f"{path} is a design-stage manifest ({DESIGN_SCHEMA}) with plates, not print beds: {rerun}")
    if isinstance(schema, str) and schema.startswith("coscad.next/"):
        raise ValueError(f"{path}: manifest schema {schema} is newer than this script understands ({NEXT_SCHEMA}); update scripts/bambu-slice.py")
    if schema is not None:
        raise ValueError(f"{path}: unknown manifest schema {schema!r}; expected {NEXT_SCHEMA} from `coscad next`")
    if isinstance(m.get("beds"), list):
        return
    if "plate" in m:
        raise ValueError(f"{path} is a design-stage manifest from coscad 1.2 or earlier, when the design stage also wrote "
                         f"<base>_manifest.json (it now writes <base>_design.json): {rerun}")
    raise ValueError(f"{path} has no beds: {rerun}")


# What addresses each printability warning code from `coscad next`.
MITIGATION = {"small_contact": "brim", "tall_narrow": "brim",
              "flat_overhang": "supports", "elevated_start": "supports"}
BRIM_TYPES = ("outer_only", "inner_only", "outer_and_inner")
PRINTABILITY_REFUSED = 2


def _setting(proc, key):
    """A Bambu setting as one string (lists hold one value per extruder)."""
    v = proc.get(key)
    if isinstance(v, list):
        v = v[0] if v else None
    return None if v is None else str(v)


def _number(v):
    try:
        return float(v)
    except (TypeError, ValueError):
        return 0.0


def process_measures(proc):
    """Which mitigations the resolved process turns on. Only an explicit
    brim counts: `auto_brim` decides per object and produced no brim on
    the failed tesseract coupon."""
    brim = _number(_setting(proc, "brim_width")) if _setting(proc, "brim_type") in BRIM_TYPES else 0.0
    return {"brim_mm": brim if brim > 0 else 0.0,
            "supports": _setting(proc, "enable_support") in ("1", "true")}


def selected_beds(m, only=None):
    return [b for b in m["beds"] if only is None or b["index"] == only]


def bed_issues(m, beds):
    """One entry per (bed, variant, warning) for the parts on these beds,
    from each variant's `printability` block. Manifests without
    `warning_codes` (before coscad 1.3) give code None."""
    variants = {v.get("id"): v for v in m.get("variants", [])}
    issues = []
    for bed in beds:
        seen = []
        for p in bed.get("placements", []):
            vid = p.get("variant")
            if vid in seen:
                continue
            seen.append(vid)
            pr = variants.get(vid, {}).get("printability") or {}
            warnings = pr.get("warnings") or []
            codes = pr.get("warning_codes") or []
            for i, w in enumerate(warnings):
                issues.append({"bed": bed["index"], "variant": vid,
                               "instances": [q["instance"] for q in bed["placements"] if q.get("variant") == vid],
                               "code": codes[i] if i < len(codes) else None, "warning": w})
    return issues


def reserved_brim(beds):
    """The widest brim `coscad next` reserved for any part on these beds."""
    return max([_number(p.get("brim_mm")) for b in beds for p in b.get("placements", [])] or [0.0])


def brim_settings(m, beds, overrides):
    """Process settings for the brim the manifest reserved, when the
    explicit overrides leave the brim alone: an outer brim of the widest
    reserved width. Empty when nothing is reserved or the user set one."""
    width = reserved_brim(beds)
    if width <= 0 or set(overrides or {}) & {"brim_type", "brim_width"}:
        return {}
    return {"brim_type": "outer_only", "brim_width": f"{width:g}"}


def brim_conflicts(m, beds, brim_mm):
    """Parts whose reserved space is narrower than the brim being sliced:
    a part's brim may reach its own reservation plus half the margin
    before it can touch a neighbour's (or the plate edge's margin)."""
    if brim_mm <= 0:
        return []
    # manifests before coscad 1.3 reserve nothing and record no brim_mm
    margin = _number((m.get("bed") or {}).get("margin"))
    return [(b["index"], p["instance"], _number(p.get("brim_mm")))
            for b in beds for p in b.get("placements", [])
            if "brim_mm" in p and brim_mm > _number(p.get("brim_mm")) + margin / 2 + 1e-9]


def gate_printability(issues, measures, accept):
    """Classify each warning as addressed by the process or not, and
    decide. Returns (issues annotated with `addressed_by`, refused)."""
    out, open_ = [], []
    for issue in issues:
        need = MITIGATION.get(issue["code"])
        have = [k for k in ("brim", "supports")
                if (need in (None, k)) and (measures["brim_mm"] > 0 if k == "brim" else measures["supports"])]
        out.append(dict(issue, needs=need or "brim or supports", addressed_by=have[0] if have else None))
        if not have:
            open_.append(issue)
    return out, bool(open_) and not accept


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("manifest", help="<base>_manifest.json written by `coscad next` (schema coscad.next/1)")
    ap.add_argument("--printer", default="Bambu Lab A1 0.4 nozzle", help="machine preset name")
    ap.add_argument("--process", default=None, help="process preset (default: the printer's default profile)")
    ap.add_argument("--process-overrides", help="JSON file of explicit changes to the flattened process preset")
    ap.add_argument("--filament-overrides", help="JSON file of explicit changes to the flattened filament preset")
    ap.add_argument("--filament", default=None, help="filament preset (default: from the parts' material= hint, else the printer's default)")
    ap.add_argument("--plate", default="textured", choices=sorted(BED_TYPES), help="build plate on the printer (default textured)")
    ap.add_argument("--bed", type=int, default=None, help="only this bed index")
    ap.add_argument("--print", dest="print_bed", type=int, metavar="N", help="after slicing, upload bed N and start printing it")
    ap.add_argument("--slicer-arg", action="append", default=[], help="extra Bambu Studio CLI argument (repeatable)")
    ap.add_argument("--spool", default="external", help="with --print: external (spool holder, default) or ams0..ams3 (AMS slot 1..4)")
    ap.add_argument("--accept-printability", action="store_true",
                    help="slice even when the manifest's printability warnings are not addressed by a brim or supports "
                         "(recorded in _print.json)")
    a = ap.parse_args()
    spool_mapping(a.spool)

    try:
        with open(a.manifest) as fh:
            m = json.load(fh)
        check_manifest(m, a.manifest)
    except (OSError, ValueError) as error:
        sys.exit(f"error: {error}")
    if not os.path.exists(APP):
        sys.exit(f"Bambu Studio not found at {APP} (set BAMBU_STUDIO)")
    base = manifest_base(a.manifest)
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
    beds = selected_beds(m, a.bed)
    if a.bed is not None and not beds:
        sys.exit(f"error: the manifest has no bed {a.bed}")
    try:
        explicit = preset_overrides(a.process_overrides, flatten("process", process), {"brim_type"}) if a.process_overrides else {}
        derived = brim_settings(m, beds, explicit)
        proc = resolve_process(process, a.process_overrides, derived)
    except (OSError, ValueError) as error:
        sys.exit(f"invalid preset overrides: {error}")
    measures = process_measures(proc)
    issues, refused = gate_printability(bed_issues(m, beds), measures, a.accept_printability)
    for issue in issues:
        state = f"addressed by {issue['addressed_by']}" if issue["addressed_by"] else f"NOT addressed (needs {issue['needs']})"
        print(f"bed {issue['bed']}: {issue['variant']}: {issue['warning']} [{state}]", file=sys.stderr)
    if derived:
        print(f"brim: {derived['brim_width']} mm outer brim, as reserved in the manifest", file=sys.stderr)
    for index, inst, reserved in brim_conflicts(m, beds, measures["brim_mm"]):
        print(f"warning: bed {index}: {inst} has {reserved:g} mm of brim reserved but the process prints "
              f"{measures['brim_mm']:g} mm; brims may merge or reach the plate margin "
              f"(add brim={measures['brim_mm']:g} to the part or plate and rerun `coscad next`)", file=sys.stderr)
    if refused:
        n = sum(1 for i in issues if not i["addressed_by"])
        print(f"error: {n} printability warning{'s' if n != 1 else ''} not addressed by the process: enable an explicit brim "
              f"(brim_type outer_only and brim_width in --process-overrides, or brim=N in the .assemble) or supports "
              f"(enable_support), fix the part, or pass --accept-printability after review", file=sys.stderr)
        sys.exit(PRINTABILITY_REFUSED)
    try:
        presets = write_presets(a.printer, process, filament, bed_name, a.process_overrides, a.filament_overrides, derived)
    except (OSError, ValueError) as error:
        sys.exit(f"invalid preset overrides: {error}")
    try:
        summary = slice_beds(a, m, beds, presets, process, filament, bed_name, machine, outdir, base,
                             issues, measures, derived)
    except (OSError, ValueError) as error:
        sys.exit(f'Print preflight refused: {error}')
    finally:
        shutil.rmtree(os.path.dirname(presets["machine"]), ignore_errors=True)

    if a.print_bed is not None:
        hit = [b for b in summary["beds"] if b["index"] == a.print_bed]
        if not hit:
            sys.exit(f"bed {a.print_bed} was not sliced")
        try:
            lan_print(hit[0]["3mf"], os.path.basename(hit[0]["3mf"]), a.spool,
                      expected=hit[0]['thermal_preflight']['expected'])
        except (OSError, ValueError) as error:
            sys.exit(f'Print preflight refused: {error}')


def printer_fit_warnings(m, beds, machine):
    """Where the manifest's plate is bigger than the printer: its area
    against `printable_area`, its `max_z` (the 4th `plate` argument,
    250 mm by default) against `printable_height`, and any part on these
    beds taller than the printer can build."""
    out = []
    bed = m.get("bed") or {}
    pw, pd = _number(bed.get("w")), _number(bed.get("d"))
    area = machine.get("printable_area") or []
    if area:
        xs = [float(p.split("x")[0]) for p in area]; ys = [float(p.split("x")[1]) for p in area]
        if pw > max(xs) - min(xs) + 1e-6 or pd > max(ys) - min(ys) + 1e-6:
            out.append(f"the assembly's plate ({pw:g}x{pd:g}) is larger than the printer's bed ({max(xs)-min(xs):g}x{max(ys)-min(ys):g})")
    height = _number(_setting(machine, "printable_height"))
    if height > 0:
        max_z = _number(bed.get("max_z"))
        if max_z > height + 1e-6:
            out.append(f"the assembly's plate allows parts up to {max_z:g} mm but the printer builds {height:g} mm; "
                       f"set it with `plate W D margin {height:g}` and rerun `coscad next`")
        used = {p.get("variant") for b in beds for p in b.get("placements", [])}
        for v in m.get("variants", []):
            fp = v.get("footprint") or []
            if v.get("id") in used and len(fp) == 3 and _number(fp[2]) > height + 1e-6:
                out.append(f"{v['id']} is {_number(fp[2]):g} mm tall, over the printer's {height:g} mm build height")
    return out


def slice_beds(a, m, beds, presets, process, filament, bed_name, machine, outdir, base, issues, measures, derived):
    """Slice the selected beds and write `<base>_print.json`."""
    print(f"printer {a.printer} | process {process} | filament {filament} | {bed_name}")
    # Capture intent from flattened INPUT presets, not the exported settings.
    expected = expectations(json.loads(Path(presets['process']).read_text()),
                            json.loads(Path(presets['filament']).read_text()), plate=bed_name)

    for warning in printer_fit_warnings(m, beds, machine):
        print(f"warning: {warning}", file=sys.stderr)

    summary = {"source": m.get("source"), "printer": a.printer, "process": process, "filament": filament, "plate": bed_name, "beds": []}
    if a.process_overrides:
        summary["process_overrides"] = preset_overrides(a.process_overrides, flatten("process", process), {"brim_type"})
    if a.filament_overrides:
        summary["filament_overrides"] = preset_overrides(a.filament_overrides, flatten("filament", filament))
    summary["printability"] = {"accept_printability": bool(a.accept_printability),
                               "unaddressed": sum(1 for i in issues if not i["addressed_by"]),
                               "brim_mm": measures["brim_mm"], "supports": measures["supports"],
                               "brim_from_manifest": derived or None,
                               "brim_conflicts": [{"bed": b, "instance": i, "reserved_mm": r}
                                                  for b, i, r in brim_conflicts(m, beds, measures["brim_mm"])],
                               "warnings": issues}
    for bed in beds:
        stl = bed["stl"] if os.path.isabs(bed["stl"]) else os.path.join(outdir, os.path.basename(bed["stl"]))
        out3mf = f"{base}_bed{bed['index']}.gcode.3mf"
        t0 = time.time()
        info = slice_bed(stl, out3mf, presets, a.slicer_arg)
        try:
            info['thermal_preflight'] = verify_package(out3mf, expected)
        except (OSError, ValueError) as error:
            sys.exit(f'Print preflight refused for bed {bed["index"]}: {error}')
        info.update({"index": bed["index"], "parts": [p["instance"] for p in bed["placements"]], "slice_s": round(time.time() - t0, 1),
                     "printability_warnings": [i["warning"] for i in issues if i["bed"] == bed["index"]]})
        summary["beds"].append(info)
        print(f"bed {bed['index']}: {len(info['parts'])} parts, {info.get('print_time')}, {info.get('filament_g')} g -> {out3mf}")
        if info.get("warning"):
            print("  slicer warning:", info["warning"])
    with open(f"{base}_print.json", "w") as fh:
        json.dump(summary, fh, indent=2)
    print(f"Wrote {base}_print.json")
    return summary


if __name__ == "__main__":
    main()
