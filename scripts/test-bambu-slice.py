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


if __name__ == "__main__":
    unittest.main()
