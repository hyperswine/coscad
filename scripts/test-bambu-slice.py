#!/usr/bin/env python3
"""Offline regression checks for explicit presets and slicer failure reporting."""
import importlib.util
import json
from pathlib import Path
import subprocess
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
            failure = subprocess.CompletedProcess([], 2, "[trace] internal log\n", "unsupported setting")
            with patch.object(slicer.subprocess, "run", return_value=failure), \
                 patch.object(slicer.tempfile, "mkdtemp", return_value=d), \
                 self.assertRaisesRegex(SystemExit, "unsupported setting"):
                slicer.slice_bed("part.stl", "out.3mf", {"machine": "m", "process": "p", "filament": "f"}, [])


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


if __name__ == "__main__":
    unittest.main()
