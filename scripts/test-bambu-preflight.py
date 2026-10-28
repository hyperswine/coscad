"""Offline refusal tests. No credentials, printer libraries or network required."""
import contextlib
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from types import SimpleNamespace
from unittest.mock import patch
import zipfile

sys.path.insert(0, str(Path(__file__).resolve().parent))
import bambu_preflight as gate

spec = importlib.util.spec_from_file_location('bambu_slice', Path(__file__).with_name('bambu-slice.py'))
slicer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(slicer)

EXPECTED = dict(plate='Textured PEI Plate', bed_initial_c=55, bed_c=55,
                nozzle_initial_c=220, nozzle_c=220)
SETTINGS = dict(curr_bed_type='Textured PEI Plate', textured_plate_temp_initial_layer=['55'],
                textured_plate_temp=['55'], nozzle_temperature_initial_layer=['220'],
                nozzle_temperature=['220'])
CODE = '''M83
M140 S55
M190 S55
M109 S250 ; flush
M109 S220
; CHANGE_LAYER
G1 X1 Y1 E1
; CHANGE_LAYER
G2 X2 Y2 I1 J1 E1
; MACHINE_END_GCODE_START
M140 S0
M104 S0
'''


class PreflightTests(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory()
        self.addCleanup(self.folder.cleanup)
        self.package = Path(self.folder.name)/'job.gcode.3mf'

    def write(self, code=CODE, settings=None, extra=None):
        with zipfile.ZipFile(self.package, 'w') as archive:
            archive.writestr('Metadata/project_settings.config', json.dumps(SETTINGS if settings is None else settings))
            archive.writestr('Metadata/plate_1.gcode', code)
            if extra:
                archive.writestr(*extra)
        return self.package

    def check(self, code=CODE, settings=None, expected=None):
        return gate.verify_package(self.write(code, settings), expected or EXPECTED)

    def test_valid_file_calibration_and_shutdown(self):
        report = self.check()
        self.assertEqual(report['layers_checked'], 2)
        self.assertEqual(report['deposition_moves_checked'], 2)
        self.assertEqual(len(report['package_sha256']), 64)

    def test_wrong_exported_plate_even_if_temperatures_match(self):
        with self.assertRaisesRegex(gate.PreflightError, 'Exported plate mismatch'):
            self.check(settings=dict(SETTINGS, curr_bed_type='Cool Plate'))

    def test_wrong_exported_temperature_even_if_gcode_matches(self):
        with self.assertRaisesRegex(gate.PreflightError, 'Exported temperatures'):
            self.check(settings=dict(SETTINGS, textured_plate_temp=['35']))

    def test_wrong_gcode_bed_temperature_even_if_metadata_matches(self):
        with self.assertRaisesRegex(gate.PreflightError, 'Startup bed target'):
            self.check(CODE.replace('S55', 'S35'))

    def test_wrong_bed_wait_or_missing_wait(self):
        for replacement in ('M190 S35\nM140 S55', '; no wait'):
            with self.subTest(replacement=replacement), self.assertRaisesRegex(gate.PreflightError, 'Startup bed wait'):
                self.check(CODE.replace('M190 S55', replacement))

    def test_nozzle_and_late_first_layer_changes_are_checked(self):
        for line in ('M104 S200', 'M140 S35', 'M140 S0'):
            with self.subTest(line=line), self.assertRaisesRegex(gate.PreflightError, 'Layer 1'):
                self.check(CODE.replace('G1 X1', line+'\nG1 X1'))

    def test_later_layer_changes_are_checked(self):
        for line in ('M104 S200', 'M140 S35'):
            with self.subTest(line=line), self.assertRaisesRegex(gate.PreflightError, 'Layer 2'):
                self.check(CODE.replace('G2 X2', line+'\nG2 X2'))

    def test_distinct_initial_and_later_temperatures(self):
        expected = dict(EXPECTED, bed_c=50, nozzle_c=210)
        settings = dict(SETTINGS, textured_plate_temp=['50'], nozzle_temperature=['210'])
        code = CODE.replace('G2 X2', 'M140 S50\nM104 S210\nG2 X2')
        self.assertTrue(self.check(code, settings, expected)['checked'])
        with self.assertRaisesRegex(gate.PreflightError, 'Layer 2'):
            self.check(CODE, settings, expected)

    def test_absolute_extrusion_resets_and_comments(self):
        code = CODE.replace('M83', 'M82\nG92 E100').replace('G1 X1 Y1 E1',
            '; M140 S35\nG1 X1 Y1 E101\nG92 E0')
        self.assertEqual(self.check(code)['deposition_moves_checked'], 2)

    def test_zero_padded_commands_are_checked_and_numbered_commands_refused(self):
        self.assertTrue(self.check(CODE.replace('G1 ', 'G01 ').replace('M140 ', 'M0140 '))['checked'])
        with self.assertRaisesRegex(gate.PreflightError, 'Layer 1'):
            self.check(CODE.replace('G1 X1', 'M0140 S35\nG01 X1'))
        with self.assertRaisesRegex(gate.PreflightError, 'Numbered/checksummed'):
            self.check(CODE.replace('G1 X1', 'N50 M140 S35\nG1 X1'))

    def test_conditional_final_temperature_is_not_trusted(self):
        with self.assertRaisesRegex(gate.PreflightError, 'Startup nozzle'):
            self.check(CODE.replace('M109 S220', 'M622 J1\nM109 S220\nM623'))
        # Earlier calibration branches are fine when overwritten unconditionally.
        self.assertTrue(self.check(CODE.replace('M109 S250', 'M622 J1\nM109 S250\nM623'))['checked'])

    def test_unresolved_or_ambiguous_targets_are_refused(self):
        for value in ('{bed_temperature}', 'nan', '55junk', '55 S35', '55 R35'):
            with self.subTest(value=value), self.assertRaisesRegex(gate.PreflightError, 'ambiguous'):
                self.check(CODE.replace('M140 S55', 'M140 S'+value))

    def test_missing_markers_or_empty_layers_are_refused(self):
        for code in (CODE.replace('; CHANGE_LAYER', '; absent'),
                     CODE.replace('; MACHINE_END_GCODE_START', '; absent'),
                     CODE.replace('G2 X2 Y2 I1 J1 E1', 'G1 E-.8')):
            with self.subTest(code=code), self.assertRaises(gate.PreflightError):
                self.check(code)

    def test_missing_settings_or_corrupt_archive_is_refused(self):
        for content in (b'not a zip', b''):
            self.package.write_bytes(content)
            with self.assertRaises(gate.PreflightError):
                gate.verify_package(self.package, EXPECTED)
        with zipfile.ZipFile(self.package, 'w') as archive:
            archive.writestr('Metadata/plate_1.gcode', CODE)
        with self.assertRaises(gate.PreflightError):
            gate.verify_package(self.package, EXPECTED)

    def test_multi_plate_or_duplicate_member_is_refused(self):
        for name in ('Metadata/plate_2.gcode', 'Metadata/plate_1.gcode'):
            with self.subTest(name=name), contextlib.redirect_stderr(None):
                self.write(extra=(name, CODE))
                with self.assertRaises(gate.PreflightError):
                    gate.verify_package(self.package, EXPECTED)

    def test_missing_invalid_or_mixed_temperature_intent_is_refused(self):
        for value in (None, [], 'nan', 'inf', '-1', '0', True, ['55', '65']):
            with self.subTest(value=value), self.assertRaises(gate.PreflightError):
                gate.expectations({'curr_bed_type': EXPECTED['plate']},
                                  dict(SETTINGS, textured_plate_temp=value))

    def test_independent_intent_does_not_follow_wrong_process_plate(self):
        with self.assertRaisesRegex(gate.PreflightError, 'Process plate'):
            gate.expectations({'curr_bed_type': 'Cool Plate'}, SETTINGS, plate='Textured PEI Plate')

    def test_bad_package_never_reaches_upload_or_network(self):
        self.write(CODE.replace('S55', 'S35'))
        with patch.object(slicer, '_lan_print') as upload:
            with self.assertRaises(gate.PreflightError):
                slicer.lan_print(self.package, 'remote.3mf', expected=EXPECTED)
            upload.assert_not_called()

    def test_missing_intent_cannot_use_the_old_dispatch_api(self):
        self.write()
        with patch.object(slicer, '_lan_print') as upload:
            with self.assertRaises(TypeError):
                slicer.lan_print(self.package, 'remote.3mf')
            upload.assert_not_called()

    def test_uploader_gets_exact_verified_snapshot_even_if_path_is_replaced(self):
        self.write()
        original = self.package.read_bytes()
        def upload(stream, report, name, spool):
            self.package.write_bytes(b'replaced after preflight')
            self.assertEqual(stream.read(), original)
            self.assertTrue(report['checked'])
        with patch.object(slicer, '_lan_print', side_effect=upload):
            slicer.lan_print(self.package, 'remote.3mf', expected=EXPECTED)

    def test_saved_report_does_not_bypass_fresh_dispatch_check(self):
        self.write()
        previous = gate.verify_package(self.package, EXPECTED)
        self.write(CODE.replace('S55', 'S35'))
        with patch.object(slicer, '_lan_print') as upload:
            with self.assertRaises(gate.PreflightError):
                slicer.lan_print(self.package, 'remote.3mf', expected=previous['expected'])
            upload.assert_not_called()

    def test_corrupt_uploaded_bytes_never_reach_mqtt(self):
        self.write()
        stored = []
        class Stream(io.BytesIO):
            recv = io.BytesIO.read
        ftp = SimpleNamespace(storbinary=lambda name, source: stored.append(source.read()),
                              voidcmd=lambda _: None, transfercmd=lambda _: Stream(b'corrupted'),
                              voidresp=lambda: None, close=lambda: None)
        mqtt = SimpleNamespace(Client=unittest.mock.Mock())
        with patch.dict(sys.modules, {'ftp': SimpleNamespace(connect=lambda: ftp),
                                     'paho': SimpleNamespace(mqtt=SimpleNamespace(client=mqtt)),
                                     'paho.mqtt': SimpleNamespace(client=mqtt),
                                     'paho.mqtt.client': mqtt}), \
             patch.dict('os.environ', {'BAMBU_HOST': 'unused', 'BAMBU_SERIAL': 'unused',
                                      'BAMBU_ACCESS_CODE': 'synthetic-test-value'}):
            with self.assertRaisesRegex(ValueError, 'Uploaded package differs'):
                slicer.lan_print(self.package, 'remote.3mf', expected=EXPECTED)
            mqtt.Client.assert_not_called()
        self.assertEqual(stored, [self.package.read_bytes()])

    def test_verified_remote_bytes_reach_one_start_command(self):
        self.write()
        client = unittest.mock.Mock()
        mqtt = SimpleNamespace(Client=lambda *a, **kw: client, CallbackAPIVersion=SimpleNamespace(VERSION2=2))
        client.connect.side_effect = lambda *a, **kw: client.on_connect(client, None, None, 0)
        client.loop_start.side_effect = lambda: client.on_message(client, None, SimpleNamespace(
            payload=json.dumps({'print': {'command': 'project_file', 'result': 'success'}}).encode()))
        stored = []
        class Stream(io.BytesIO):
            recv = io.BytesIO.read
        ftp = SimpleNamespace(storbinary=lambda name, source: stored.append(source.read()),
                              voidcmd=lambda _: None, transfercmd=lambda _: Stream(stored[0]),
                              voidresp=lambda: None, close=lambda: None)
        with patch.dict(sys.modules, {'ftp': SimpleNamespace(connect=lambda: ftp),
                                     'paho': SimpleNamespace(mqtt=SimpleNamespace(client=mqtt)),
                                     'paho.mqtt': SimpleNamespace(client=mqtt), 'paho.mqtt.client': mqtt}), \
             patch.dict('os.environ', {'BAMBU_HOST': 'unused', 'BAMBU_SERIAL': 'unused',
                                      'BAMBU_ACCESS_CODE': 'synthetic-test-value'}), \
             contextlib.redirect_stdout(io.StringIO()):
            report = slicer.lan_print(self.package, 'remote.3mf', expected=EXPECTED)
        client.publish.assert_called_once()
        command = json.loads(client.publish.call_args.args[1])['print']
        self.assertEqual(command['command'], 'project_file')
        self.assertEqual(command['param'], 'Metadata/plate_1.gcode')
        self.assertEqual(report['package_sha256'], hashlib.sha256(stored[0]).hexdigest())

    def test_every_model_dispatch_refuses_wrong_plate_before_connecting(self):
        root = Path(__file__).resolve().parents[1]
        dispatchers = [p for p in (root/'models').rglob('*.py')
                       if not p.name.startswith('test') and 'project_file' in p.read_text()]
        self.assertTrue(dispatchers)
        for index, source in enumerate(sorted(dispatchers)):
            with self.subTest(dispatcher=str(source.relative_to(root))):
                spec = importlib.util.spec_from_file_location(f'dispatch_refusal_{index}', source)
                module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(module)
                folder = Path(self.folder.name)/str(index)
                args = ['--start', '--bed-clear']
                job = folder
                if source.name == 'dispatch.py':
                    name = 'p1s-pla' if source.parent.name == 'beach-outrigger' else 'monohull'
                    args = [name, '--spool', 'external', '--confirm-bed-clear', '--authorization-note', 'Offline test']
                    job = folder/name
                    package = job/'slice'/('beach-outrigger.gcode.3mf' if name == 'p1s-pla' else 'monohull.gcode.3mf')
                elif source.name == 'print_fit.py':
                    package = folder/('thread-fit_bed1.gcode.3mf' if source.parent.name == 'p1s-thread-fit' else 'fit_bed1.gcode.3mf')
                elif source.parent.name == 'p1s-split-retry':
                    package = folder/'slice-corrected/tesseract-bed1-p1s.gcode.3mf'
                elif source.parent.name == 'p1s-full':
                    package = folder/'slice-onebed/tesseract-full-p1s.gcode.3mf'
                elif source.parent.name == 'a1-full-retry':
                    package = folder/'slice/tesseract-full-a1.gcode.3mf'
                else:
                    self.fail(f'Add a refusal fixture for new dispatcher {source}')
                package.parent.mkdir(parents=True, exist_ok=True)
                wrong = self.write(settings=dict(SETTINGS, curr_bed_type='Cool Plate')).read_bytes()
                package.write_bytes(wrong)
                digest = hashlib.sha256(wrong).hexdigest()
                evidence = dict(passed=True, checked=True, single_plate=True, normal_objects=8 if 'split' in str(source) else 37,
                                bed_index=1, package=str(package), file=package.name, sha256=digest, package_sha256=digest)
                (job/'package-validation.json').write_text(json.dumps(evidence))
                (folder/'evidence').mkdir(exist_ok=True)
                (folder/'evidence/slices.json').write_text(json.dumps({'builds': {'fit': {'plates': [evidence]}}}))
                (job/'presets').mkdir(exist_ok=True)
                (job/'presets/process.json').write_text(json.dumps({'curr_bed_type': 'Textured PEI Plate'}))
                (job/'presets/filament.json').write_text(json.dumps(SETTINGS))
                connect = unittest.mock.Mock(side_effect=AssertionError('Network must not be reached'))
                with patch.object(module, 'HERE', folder, create=True), patch.object(module, 'B', folder, create=True), \
                     patch.object(module, 'RECORD', folder/'run.json', create=True), \
                     patch.object(module, 'context', SimpleNamespace(connect=connect)), \
                     patch.object(sys, 'argv', [str(source), *args]):
                    with self.assertRaisesRegex(gate.PreflightError, 'Exported plate mismatch'):
                        module.main()
                    connect.assert_not_called()
                    self.assertFalse((folder/'run.json').exists())
                    self.assertFalse((job/'print-run.json').exists())

    def test_optimized_python_still_refuses(self):
        self.write(CODE.replace('S55', 'S35'))
        program = ('import sys; sys.path.insert(0, sys.argv[1]); '
                   'from bambu_preflight import verify_package; import json; '
                   'verify_package(sys.argv[2], json.loads(sys.argv[3]))')
        result = subprocess.run([sys.executable, '-O', '-c', program,
                                 str(Path(__file__).parent), str(self.package), json.dumps(EXPECTED)],
                                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('Startup bed target', result.stderr)


if __name__ == '__main__':
    unittest.main()
