#!/usr/bin/env python3
"""Bounded CLI diagnostic probes; no renderer, network, slicer, or printer.

Run with a freshly built executable:
  stack build
  python3 scripts/probe-diagnostics.py --coscad "$(stack path --local-install-root)/bin/coscad" --output /tmp/coscad-probes.json

Curated expectations are assertions. Mutations are observations: deleting a
token can leave a valid program, so a successful mutation is not itself a bug.
"""

import argparse
from collections import Counter
import json
import os
from pathlib import Path
import random
import re
import signal
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
CRASH = re.compile(r"(?:Prelude\.|Exception|invalid argument|resource exhausted|Stack space overflow|Heap exhausted|arithmetic overflow|divide by zero)", re.I)


def mutations(count):
    rng = random.Random(20261011)
    seeds = [
        "!simple\nwidth = 20\nmain = Box width 10 5 |> move 1 2 3\n",
        "!glyph\nmain = (■ 10 ⊖ ◎ 2 20) |> at top 0 0 0 (● 3)\n",
        "main = ⮕ 4 (⭘ 10 ⊖ ⭘ 6)\n",
        "main = loft 0 (⭘ 5) 10 (△ 3)\n",
        "plate = box 30 20 4 |> datum hole top+lft 5 0 0\nmain = zcyl 2 6 |> place plate @hole ctr 0 0 -2\n",
        "main = box 2 2 2 |> grid 3 2 5 6 |> hull |> anchor bot\n",
        "plate = box 30 20 4 |> frame socket top top bak\narm = box 2 4 10 |> frame foot bot bot bak\nmain = arm |> mate plate @socket @foot 90 0.2\n",
    ]
    seen = set()
    punctuation = "()=,|>+-*/$!▽←×;[]{}\x00\u200b\u2212\uff1d"
    while len(seen) < count:
        src = rng.choice(seeds)
        at = rng.randrange(len(src))
        op = rng.choice(["delete", "insert", "replace", "transpose"])
        if op == "delete":
            changed = src[:at] + src[at + 1:]
        elif op == "insert":
            changed = src[:at] + rng.choice(punctuation) + src[at:]
        elif op == "replace":
            changed = src[:at] + rng.choice(punctuation) + src[at + 1:]
        else:
            changed = src[:at] + src[at:at + 2][::-1] + src[at + 2:]
        if changed == src or changed in seen:
            continue
        seen.add(changed)
        yield {"id": f"mutation-{len(seen):03}", "category": "single-edit mutation", "expect": "observe", "source": changed}


def probe(binary, case, timeout):
    with tempfile.TemporaryDirectory(prefix="coscad-probe-") as tmp:
        folder = Path(tmp)
        filename = case.get("filename", "case.coscad")
        if case.get("encoding") == "invalid-utf8":
            (folder / filename).write_bytes(b"main = \xff\n")
        else:
            (folder / filename).write_text(case.get("source", ""), encoding="utf-8")
        # Small, known-valid referenced shapes. Individual cases may override.
        files = {"part.coscad": "main = box 10 10 10\n", **case.get("files", {})}
        for name, src in files.items():
            (folder / name).write_text(src, encoding="utf-8")
        argv = [str(binary), *case.get("args", [filename]), "+RTS", "-N1", "-RTS"]
        started = time.monotonic()
        # Files, not pipes: a broken compiler cannot fill parent memory with output.
        with (folder / "stdout.log").open("wb") as out, (folder / "stderr.log").open("wb") as err:
            process = subprocess.Popen(argv, cwd=folder, stdout=out, stderr=err, start_new_session=True)
            timed_out = False
            try:
                process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                timed_out = True
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
        def output(name):
            with (folder / name).open("rb") as handle:
                data = handle.read(16385)
            return data[:16384].decode("utf-8", errors="replace").replace(tmp + "/", "") + ("\n[truncated]" if len(data) > 16384 else "")
        stdout, stderr = output("stdout.log"), output("stderr.log")
        if timed_out:
            status = "timeout"
        elif process.returncode < 0 or CRASH.search(stderr):
            status = "crash/raw exception"
        elif process.returncode == 0:
            status = "accepted"
        else:
            status = "rejected"
        expected = case["expect"]
        passed = expected == "observe" or status == {"accept": "accepted", "reject": "rejected"}[expected]
        passed = passed and all(s in stdout + stderr for s in case.get("messages", []))
        generated = sorted(p.name for p in folder.iterdir() if p.name not in {filename, "stdout.log", "stderr.log", *files})
        passed = passed and not (case.get("no_generated_files") and generated)
        return {**case, "status": status, "expectation_met": passed, "exit_code": process.returncode,
                "elapsed_seconds": round(time.monotonic() - started, 4), "stdout": stdout, "stderr": stderr,
                "generated_files": generated}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--coscad", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--mutations", type=int, default=250)
    parser.add_argument("--timeout", type=float, default=3)
    args = parser.parse_args()
    if not 0 <= args.mutations <= 2000 or not 0 < args.timeout <= 30:
        parser.error("use 0..2000 mutations and a timeout in (0, 30] seconds")
    binary = args.coscad.resolve(strict=True)
    cases = json.loads((ROOT / "test/diagnostics/cases.json").read_text()) + list(mutations(args.mutations))
    results = []
    for case in cases:
        result = probe(binary, case, args.timeout)
        results.append(result)
        if not result["expectation_met"] or result["status"] in {"timeout", "crash/raw exception"}:
            print(f"{case['id']}: {result['status']} (expected {case['expect']})", flush=True)
    summary = {"total": len(results), "statuses": dict(Counter(r["status"] for r in results)),
               "asserted": dict(Counter(r["status"] for r in results if r["expect"] != "observe")),
               "expectation_failures": sum(not r["expectation_met"] for r in results)}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps({"seed": 20261011, "summary": summary, "results": results}, indent=2, ensure_ascii=False) + "\n")
    print(json.dumps(summary, indent=2))
    print(f"Full inputs and responses: {args.output}")
    return int(summary["expectation_failures"] > 0 or any(r["status"] in {"timeout", "crash/raw exception"} for r in results))


if __name__ == "__main__":
    raise SystemExit(main())
