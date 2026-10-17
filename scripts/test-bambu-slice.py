#!/usr/bin/env python3
"""Offline regression checks for explicit presets, slicer failure reporting,
the printability gate and temporary-directory cleanup. Needs no Bambu
Studio: the slicer and its presets are mocked."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("bambu_slice", Path(__file__).with_name("bambu-slice.py"))
slicer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(slicer)


class SliceTests(unittest.TestCase):
    def override(self, value, preset, additional=()):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "overrides.json"
            p.write_text(json.dumps(value))
            return slicer.preset_overrides(p, preset, additional)

    def test_unknown_setting_is_refused(self):
        with self.assertRaisesRegex(ValueError, "unknown.*brim_wdith"):
            self.override({"brim_wdith": "8"}, {"brim_width": "5"})

    def test_malformed_values_are_refused(self):
        for value in [None, True, 8, [], {}, [True]]:
            with self.subTest(value=value), self.assertRaises(ValueError):
                self.override({"brim_width": value}, {"brim_width": "5"})
        with self.assertRaisesRegex(ValueError, "JSON object"):
            self.override([], {})

    def test_supported_compiled_default_can_be_set(self):
        self.assertEqual(self.override({"brim_type": "outer_only"}, {}, {"brim_type"}),
                         {"brim_type": "outer_only"})

    def test_filament_and_process_changes_stay_separate(self):
        with tempfile.TemporaryDirectory() as d:
            process, filament = Path(d) / "process.json", Path(d) / "filament.json"
            process.write_text('{"brim_width":"8", "brim_type":"outer_only"}')
            filament.write_text('{"close_fan_the_first_x_layers":["3"]}')
            presets = {"process": {"brim_width": "5"},
                       "filament": {"close_fan_the_first_x_layers": ["1"]}}
            with patch.object(slicer, "flatten", side_effect=lambda kind, name: dict(presets[kind])), \
                 patch.object(slicer, "machine_with_templates", return_value={}):
                files = slicer.write_presets("A1", "Optimal", "PLA", "Textured PEI Plate", process, filament)
            try:
                p, f = json.loads(Path(files["process"]).read_text()), json.loads(Path(files["filament"]).read_text())
                self.assertEqual(p["brim_width"], "8")
                self.assertEqual(f["close_fan_the_first_x_layers"], ["3"])
                self.assertNotIn("close_fan_the_first_x_layers", p)
                self.assertNotIn("brim_width", f)
            finally:
                slicer.shutil.rmtree(Path(files["process"]).parent)

    def test_slicer_failure_reports_stderr_without_type_error(self):
        with tempfile.TemporaryDirectory() as d:
            work = os.path.join(d, "coscad-slice-x")
            os.mkdir(work)
            failure = subprocess.CompletedProcess([], 2, "[trace] internal log\n", "unsupported setting")
            with patch.object(slicer.subprocess, "run", return_value=failure), \
                 patch.object(slicer.tempfile, "mkdtemp", return_value=work), \
                 self.assertRaisesRegex(SystemExit, "unsupported setting"):
                slicer.slice_bed("part.stl", "out.3mf", {"machine": "m", "process": "p", "filament": "f"}, [])

    def test_slice_work_dir_is_removed_after_a_slicer_failure(self):
        with tempfile.TemporaryDirectory() as d:
            work = os.path.join(d, "coscad-slice-x")
            os.mkdir(work)
            Path(work, "result.json").write_text('{"return_code": -51, "error_string": "boom"}')
            failure = subprocess.CompletedProcess([], 1, "", "")
            with patch.object(slicer.subprocess, "run", return_value=failure), \
                 patch.object(slicer.tempfile, "mkdtemp", return_value=work), \
                 self.assertRaisesRegex(SystemExit, "boom"):
                slicer.slice_bed("part.stl", "out.3mf", {"machine": "m", "process": "p", "filament": "f"}, [])
            self.assertFalse(os.path.exists(work))

    def test_slice_work_dir_is_removed_when_the_slicer_cannot_start(self):
        with tempfile.TemporaryDirectory() as d:
            work = os.path.join(d, "coscad-slice-x")
            os.mkdir(work)
            with patch.object(slicer.subprocess, "run", side_effect=FileNotFoundError("BambuStudio")), \
                 patch.object(slicer.tempfile, "mkdtemp", return_value=work), \
                 self.assertRaises(FileNotFoundError):
                slicer.slice_bed("part.stl", "out.3mf", {"machine": "m", "process": "p", "filament": "f"}, [])
            self.assertFalse(os.path.exists(work))

    def test_presets_dir_is_removed_when_writing_fails(self):
        with tempfile.TemporaryDirectory() as d:
            pdir = os.path.join(d, "coscad-presets-x")
            os.mkdir(pdir)
            unserialisable = {"x": object()}
            with patch.object(slicer, "flatten", side_effect=lambda kind, name: dict(unserialisable) if kind == "filament" else {}), \
                 patch.object(slicer, "machine_with_templates", return_value={}), \
                 patch.object(slicer.tempfile, "mkdtemp", return_value=pdir), \
                 self.assertRaises(TypeError):
                slicer.write_presets("A1", "Optimal", "PLA", "Textured PEI Plate")
            self.assertFalse(os.path.exists(pdir))


class ManifestTests(unittest.TestCase):
    BED = {"index": 1, "stl": "a_bed1.stl", "placements": []}

    def test_next_manifest_is_accepted(self):
        slicer.check_manifest({"schema": "coscad.next/1", "beds": [self.BED]}, "a_manifest.json")

    def test_pre_schema_next_manifest_is_still_accepted(self):
        slicer.check_manifest({"bed": {"w": 256, "d": 256}, "beds": [self.BED]}, "a_manifest.json")

    def test_design_manifest_is_refused_with_the_next_command(self):
        m = {"schema": "coscad.design/1", "source": "a.assemble", "plate": {}, "placements": []}
        with self.assertRaisesRegex(ValueError, r"design-stage manifest.*coscad next a\.assemble` and slice the a_manifest\.json"):
            slicer.check_manifest(m, "a_design.json")

    def test_old_design_manifest_under_the_old_name_gets_a_migration_message(self):
        m = {"source": "frame.assemble", "plate": {"w": 220}, "placements": []}
        with self.assertRaisesRegex(ValueError, r"coscad 1\.2 or earlier.*_design\.json.*coscad next frame\.assemble"):
            slicer.check_manifest(m, "frame_manifest.json")

    def test_newer_or_foreign_schemas_are_refused(self):
        with self.assertRaisesRegex(ValueError, "newer than this script"):
            slicer.check_manifest({"schema": "coscad.next/2", "beds": []}, "a_manifest.json")
        with self.assertRaisesRegex(ValueError, "unknown manifest schema"):
            slicer.check_manifest({"schema": "other", "beds": []}, "a_manifest.json")
        with self.assertRaisesRegex(ValueError, "no beds list"):
            slicer.check_manifest({"schema": "coscad.next/1"}, "a_manifest.json")
        with self.assertRaisesRegex(ValueError, "no beds"):
            slicer.check_manifest({}, "a_manifest.json")

    def test_cli_refuses_a_design_manifest_before_looking_for_bambu_studio(self):
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "frame_manifest.json"
            p.write_text(json.dumps({"source": "frame.assemble", "plate": {}, "placements": []}))
            r = subprocess.run([slicer.sys.executable, str(Path(__file__).with_name("bambu-slice.py")), str(p)],
                               capture_output=True, text=True, env={"BAMBU_STUDIO": "/nonexistent", "PATH": ""})
            self.assertEqual(r.returncode, 1)
            self.assertIn("design-stage manifest from coscad 1.2 or earlier", r.stderr)
            self.assertNotIn("Traceback", r.stderr)

    def test_tracked_manifests_match_their_names(self):
        root = Path(__file__).resolve().parent.parent
        tracked = subprocess.run(["git", "ls-files", "*_manifest.json", "*_design.json"], cwd=root,
                                 capture_output=True, text=True)
        if tracked.returncode != 0:
            self.skipTest("not a git checkout")
        for name in tracked.stdout.split():
            m = json.loads((root / name).read_text())
            with self.subTest(manifest=name):
                if name.endswith("_design.json"):
                    self.assertEqual(m.get("schema"), "coscad.design/1")
                    self.assertRaises(ValueError, slicer.check_manifest, m, name)
                else:
                    slicer.check_manifest(m, name)


def manifest(warnings=(), codes=None, brim=None, margin=6, schema=True):
    """A one-bed coscad.next/1 manifest with one part carrying `warnings`."""
    pr = {"bed_contact_mm2": 1, "warnings": list(warnings)}
    if codes is not None:
        pr["warning_codes"] = list(codes)
    placement = {"instance": "p_1", "variant": "p_bot", "x": 6, "y": 6, "w": 10, "d": 10}
    if brim is not None:
        placement["brim_mm"] = brim
    m = {"bed": {"w": 256, "d": 256, "margin": margin}, "source": "a.assemble",
         "variants": [{"id": "p_bot", "part": "p", "printability": pr, "hints": {}}],
         "beds": [{"index": 1, "stl": "a_bed1.stl", "placements": [placement]}]}
    if schema:
        m["schema"] = "coscad.next/1"
    return m


class PrintabilityGateTests(unittest.TestCase):
    CONTACT = "only 1 mm^2 of flat bed contact (2 mm^2 of first layer); check the first layer and use a brim or a broader print face"
    START = "elevated downward-facing start at 10 mm above the bed; inspect supports before printing"

    def gate(self, m, proc, accept=False):
        measures = slicer.process_measures(proc)
        return slicer.gate_printability(slicer.bed_issues(m, m["beds"]), measures, accept)

    def test_clean_manifest_passes(self):
        issues, refused = self.gate(manifest(), {})
        self.assertEqual(issues, [])
        self.assertFalse(refused)

    def test_warnings_without_brim_or_supports_are_refused(self):
        issues, refused = self.gate(manifest([self.CONTACT], ["small_contact"]), {"brim_type": "auto_brim", "brim_width": "5"})
        self.assertTrue(refused)
        self.assertEqual(issues[0]["needs"], "brim")
        self.assertIsNone(issues[0]["addressed_by"])
        self.assertEqual(issues[0]["instances"], ["p_1"])

    def test_accept_printability_passes_and_keeps_the_record(self):
        issues, refused = self.gate(manifest([self.CONTACT], ["small_contact"]), {}, accept=True)
        self.assertFalse(refused)
        self.assertEqual([i["warning"] for i in issues], [self.CONTACT])

    def test_each_warning_needs_its_own_measure(self):
        m = manifest([self.CONTACT, self.START], ["small_contact", "elevated_start"])
        brim = {"brim_type": "outer_only", "brim_width": "8"}
        issues, refused = self.gate(m, brim)
        self.assertTrue(refused)
        self.assertEqual([i["addressed_by"] for i in issues], ["brim", None])
        issues, refused = self.gate(m, dict(brim, enable_support="1"))
        self.assertFalse(refused)
        self.assertEqual([i["addressed_by"] for i in issues], ["brim", "supports"])

    def test_manifest_without_codes_needs_a_brim_or_supports(self):
        m = manifest([self.CONTACT])
        self.assertTrue(self.gate(m, {})[1])
        self.assertFalse(self.gate(m, {"enable_support": "1"})[1])
        self.assertFalse(self.gate(m, {"brim_type": "outer_only", "brim_width": ["3"]})[1])
        self.assertTrue(self.gate(m, {"brim_type": "outer_only", "brim_width": "0"})[1])

    def test_reserved_brim_is_applied_unless_the_process_sets_one(self):
        m = manifest([self.CONTACT], ["small_contact"], brim=8)
        self.assertEqual(slicer.brim_settings(m, m["beds"], {}), {"brim_type": "outer_only", "brim_width": "8"})
        self.assertEqual(slicer.brim_settings(m, m["beds"], {"brim_type": "no_brim"}), {})
        self.assertEqual(slicer.brim_settings(manifest(), manifest()["beds"], {}), {})
        _, refused = self.gate(m, slicer.brim_settings(m, m["beds"], {}))
        self.assertFalse(refused)

    def test_a_brim_wider_than_the_reservation_is_reported(self):
        m = manifest(brim=2, margin=6)
        self.assertEqual(slicer.brim_conflicts(m, m["beds"], 5), [])
        self.assertEqual(slicer.brim_conflicts(m, m["beds"], 5.5), [(1, "p_1", 2)])
        self.assertEqual(slicer.brim_conflicts(manifest(), manifest()["beds"], 8), [], "old manifests reserve nothing to check")


class MainTests(unittest.TestCase):
    """bambu-slice.py end to end with the presets and the slicer mocked."""
    CONTACT = PrintabilityGateTests.CONTACT

    def run_main(self, m, *args, overrides=None):
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: slicer.shutil.rmtree(d, ignore_errors=True))
        path = Path(d) / "a_manifest.json"
        path.write_text(json.dumps(m))
        argv = ["bambu-slice.py", str(path)] + list(args)
        if overrides is not None:
            (Path(d) / "process.json").write_text(json.dumps(overrides))
            argv += ["--process-overrides", str(Path(d) / "process.json")]
        presets = Path(d) / "coscad-presets-x"
        seen = {}

        def fake_mkdtemp(prefix=""):
            presets.mkdir()
            return str(presets)

        def fake_slice(stl, out3mf, files, extra):
            seen["process"] = json.loads(Path(files["process"]).read_text())
            return {"3mf": out3mf}

        err = io.StringIO()
        with patch.object(sys, "argv", argv), patch.object(slicer.os.path, "exists", lambda p: True), \
             patch.object(slicer, "flatten", side_effect=lambda kind, name: {"default_print_profile": "P", "brim_width": "5", "enable_support": "0"} if kind != "filament" else {}), \
             patch.object(slicer, "machine_with_templates", return_value={}), \
             patch.object(slicer.tempfile, "mkdtemp", side_effect=fake_mkdtemp), \
             patch.object(slicer, "slice_bed", side_effect=fake_slice), \
             contextlib.redirect_stderr(err), contextlib.redirect_stdout(io.StringIO()):
            try:
                slicer.main()
                code = 0
            except SystemExit as e:
                code = e.code
        printed = Path(d) / "a_print.json"
        return code, err.getvalue(), json.loads(printed.read_text()) if printed.exists() else None, seen, presets

    def test_refusal_exits_2_before_slicing_and_writes_nothing(self):
        code, err, summary, seen, presets = self.run_main(manifest([self.CONTACT], ["small_contact"]))
        self.assertEqual(code, slicer.PRINTABILITY_REFUSED)
        self.assertIn("NOT addressed (needs brim)", err)
        self.assertIn("--accept-printability", err)
        self.assertIsNone(summary)
        self.assertEqual(seen, {})

    def test_accept_printability_slices_and_records_the_warnings(self):
        code, err, summary, _, presets = self.run_main(manifest([self.CONTACT], ["small_contact"]), "--accept-printability")
        self.assertEqual(code, 0, err)
        pr = summary["printability"]
        self.assertTrue(pr["accept_printability"])
        self.assertEqual(pr["unaddressed"], 1)
        self.assertEqual([w["warning"] for w in pr["warnings"]], [self.CONTACT])
        self.assertEqual(summary["beds"][0]["printability_warnings"], [self.CONTACT])
        self.assertFalse(presets.exists(), "the presets directory is removed after slicing")

    def test_reserved_brim_reaches_the_sliced_process(self):
        code, err, summary, seen, _ = self.run_main(manifest([self.CONTACT], ["small_contact"], brim=8))
        self.assertEqual(code, 0, err)
        self.assertEqual((seen["process"]["brim_type"], seen["process"]["brim_width"]), ("outer_only", "8"))
        self.assertEqual(summary["printability"]["brim_from_manifest"], {"brim_type": "outer_only", "brim_width": "8"})
        self.assertEqual(summary["printability"]["warnings"][0]["addressed_by"], "brim")
        self.assertFalse(summary["printability"]["accept_printability"])

    def test_explicit_supports_address_an_elevated_start(self):
        code, err, summary, _, _ = self.run_main(manifest([PrintabilityGateTests.START], ["elevated_start"]),
                                                 overrides={"enable_support": "1"})
        self.assertEqual(code, 0, err)
        self.assertTrue(summary["printability"]["supports"])

    def test_presets_dir_is_removed_when_slicing_fails(self):
        d = tempfile.mkdtemp()
        self.addCleanup(lambda: slicer.shutil.rmtree(d, ignore_errors=True))
        path = Path(d) / "a_manifest.json"
        path.write_text(json.dumps(manifest()))
        presets = Path(d) / "coscad-presets-x"

        def fake_mkdtemp(prefix=""):
            presets.mkdir()
            return str(presets)

        with patch.object(sys, "argv", ["bambu-slice.py", str(path)]), patch.object(slicer.os.path, "exists", lambda p: True), \
             patch.object(slicer, "flatten", return_value={}), patch.object(slicer, "machine_with_templates", return_value={}), \
             patch.object(slicer.tempfile, "mkdtemp", side_effect=fake_mkdtemp), \
             patch.object(slicer, "slice_bed", side_effect=SystemExit("slicing failed")), \
             contextlib.redirect_stdout(io.StringIO()), self.assertRaises(SystemExit):
            slicer.main()
        self.assertFalse(presets.exists())


if __name__ == "__main__":
    unittest.main()
