#!/usr/bin/env python3
"""Regress silent slicer substitution: an exit code alone is not evidence."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
import zipfile

spec = importlib.util.spec_from_file_location('workflow', Path(__file__).with_name('shelf-bracket-workflow.py'))
workflow = importlib.util.module_from_spec(spec)
spec.loader.exec_module(workflow)


class ExportedSettings(unittest.TestCase):
    def package(self, path, settings, toolpaths='; FEATURE: Support\nG1 X1 Y1 E1\n'):
        with zipfile.ZipFile(path, 'w') as archive:
            archive.writestr('Metadata/project_settings.config', json.dumps(settings))
            archive.writestr('Metadata/plate_1.gcode', toolpaths)

    def test_silent_enum_substitution_is_refused(self):
        with tempfile.TemporaryDirectory() as d:
            archive = Path(d) / 'part.3mf'
            self.package(archive, {'sparse_infill_pattern': 'cubic'})
            with self.assertRaisesRegex(ValueError, 'slicer changed sparse_infill_pattern'):
                workflow.verify_slice(archive, {'sparse_infill_pattern': 'zig-zag'})

    def test_missing_support_toolpaths_are_refused(self):
        with tempfile.TemporaryDirectory() as d:
            archive = Path(d) / 'part.3mf'
            self.package(archive, {'enable_support': '1'}, 'G1 X1 Y1 E1\n')
            with self.assertRaisesRegex(ValueError, 'expected supports'):
                workflow.verify_slice(archive, {'enable_support': '1'})

    def test_matching_export(self):
        with tempfile.TemporaryDirectory() as d:
            archive = Path(d) / 'part.3mf'
            settings = {'sparse_infill_pattern': 'zig-zag', 'sparse_infill_density': '100%'}
            self.package(archive, settings)
            self.assertEqual(workflow.verify_slice(archive, settings), settings)


class BackendNames(unittest.TestCase):
    def test_old_and_new_native_names_match(self):
        self.assertEqual(workflow.geometry_backend('native-experimental'), 'native')
        self.assertEqual(workflow.geometry_backend('native'), 'native')
        self.assertEqual(workflow.geometry_backend('openscad'), 'openscad')


if __name__ == '__main__':
    unittest.main()
