#!/usr/bin/env python3
"""Offline refusal/atomicity tests; no real packages, printers or credentials."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('pipeline_lock',Path(__file__).with_name('pipeline-lock.py'))
lock=importlib.util.module_from_spec(spec); spec.loader.exec_module(lock)

class LockTests(unittest.TestCase):
    def test_tree_detects_content_rename_addition_and_deletion(self):
        with tempfile.TemporaryDirectory() as d:
            p=Path(d); (p/'preset.json').write_text('one'); first=lock.tree(p)
            (p/'preset.json').write_text('two'); self.assertNotEqual(first,lock.tree(p))
            (p/'preset.json').write_text('one'); self.assertEqual(first,lock.tree(p))
            (p/'preset.json').rename(p/'other.json'); self.assertNotEqual(first,lock.tree(p))
            (p/'other.json').rename(p/'preset.json'); (p/'extra').write_text('x')
            self.assertNotEqual(first,lock.tree(p)); (p/'extra').unlink()
            self.assertEqual(first,lock.tree(p))

    @unittest.skipIf(os.name=='nt','Windows symlink creation requires extra privileges')
    def test_symlink_target_content_is_checked_and_external_links_refused(self):
        with tempfile.TemporaryDirectory() as d, tempfile.TemporaryDirectory() as other:
            p=Path(d); (p/'version').mkdir(); (p/'version/library').write_text('one')
            (p/'current').symlink_to('version',target_is_directory=True)
            first=lock.tree(p); (p/'version/library').write_text('two')
            self.assertNotEqual(first,lock.tree(p))
            (p/'external').symlink_to(other,target_is_directory=True)
            with self.assertRaisesRegex(lock.Refused,'external'): lock.tree(p)
            (p/'external').unlink(); (p/'loop').symlink_to(p,target_is_directory=True)
            with self.assertRaisesRegex(lock.Refused,'cyclic'): lock.tree(p)

    def test_scope_does_not_hide_missing_or_unexpected_entries(self):
        expected={'binary':{'scope':'render','sha256':'abc'},'printer':{'scope':'printer','sha256':'xyz'}}
        self.assertEqual(lock.differences(expected,{'binary':expected['binary']},{'render'}),[])
        self.assertEqual(lock.differences(expected,{}, {'render'}),['binary'])
        self.assertEqual(lock.differences({}, {'unexpected':{'scope':'render'}},{'render'}),['unexpected'])

    def test_failed_atomic_write_preserves_old_lock_and_cleans_temporary_file(self):
        with tempfile.TemporaryDirectory() as d:
            p=Path(d)/'lock.json'; p.write_text('{"old":true}\n')
            with patch.object(lock.os,'replace',side_effect=OSError('injected failure')):
                with self.assertRaises(OSError): lock.atomic_json(p,{'new':True})
            self.assertEqual(p.read_text(),'{"old":true}\n')
            self.assertEqual([f.name for f in p.parent.iterdir()],['lock.json'])
            lock.atomic_json(p,{'new':True}); self.assertEqual(json.loads(p.read_text()),{'new':True})

    @unittest.skipIf(os.name=='nt','POSIX nonblocking lock behavior')
    def test_writer_lock_releases_after_exception_and_refuses_another_writer(self):
        with tempfile.TemporaryDirectory() as d:
            p=Path(d)/'lock.json'
            with lock.writer(p):
                with self.assertRaises(lock.Refused):
                    with lock.writer(p): pass
            with self.assertRaises(RuntimeError):
                with lock.writer(p): raise RuntimeError('injected crash')
            with lock.writer(p): pass

    def test_dirty_dependency_refused_and_revision_tree_recorded(self):
        with patch.object(lock,'git',side_effect=['dirty']):
            with self.assertRaises(lock.Refused): lock.repo(Path('/unused'),'fprisc')
        with patch.object(lock,'git',side_effect=['','a'*40,'b'*40]):
            value=lock.repo(Path('/unused'),'fprisc')
            self.assertEqual(value,{'repository':'fprisc','revision':'a'*40,'tree':'b'*40,'layout':1})

    def test_probe_timeout_fails_closed(self):
        with patch.object(lock.subprocess,'run',side_effect=subprocess.TimeoutExpired('tool',30)):
            with self.assertRaisesRegex(lock.Refused,'bounded'): lock.output(['tool'])

    def test_changing_inventory_preserves_prior_pin_and_releases_writer(self):
        with tempfile.TemporaryDirectory() as d:
            target=Path(d)/'lock.json'; target.write_text('{"prior":true}\n')
            with patch.object(lock,'LOCK',target), patch.object(lock.sys,'argv',['pipeline-lock.py','--pin']), patch.object(lock,'paths',return_value={}), patch.object(lock,'capture',side_effect=[{'x':1},{'x':2}]):
                self.assertEqual(lock.main(),1)
            self.assertEqual(target.read_text(),'{"prior":true}\n')
            with lock.writer(target): pass

    def test_refused_check_never_launches_requested_tool(self):
        with patch.object(lock.sys,'argv',['pipeline-lock.py','--run','coscad','doctor']), patch.object(lock,'paths',return_value={}), patch.object(lock,'checked',side_effect=lock.Refused('drift')), patch.object(lock.subprocess,'run') as run:
            self.assertEqual(lock.main(),1); run.assert_not_called()

    def test_ci_rejects_mutable_actions_and_wrong_bosl_revision(self):
        with tempfile.TemporaryDirectory() as d:
            p=Path(d); (p/'.github/workflows').mkdir(parents=True)
            (p/'ci-actions.lock.json').write_text(json.dumps({'layout':1,'actions':{'actions/checkout':{'revision':'a'*40}}}))
            lockfile=p/'pipeline.lock.json'; lockfile.write_text(json.dumps({'pins':{'bosl2':{'revision':'b'*40}}}))
            workflow=p/'.github/workflows/ci.yml'
            workflow.write_text('uses: actions/checkout@v4\n')
            with self.assertRaises(lock.Refused): lock.check_ci(p,lockfile)
            workflow.write_text('uses: actions/checkout@'+'a'*40+'\nfetch --depth 1 origin '+'c'*40+'\n')
            with self.assertRaises(lock.Refused): lock.check_ci(p,lockfile)
            workflow.write_text('uses: actions/checkout@'+'a'*40+'\nfetch --depth 1 origin '+'b'*40+'\n')
            lock.check_ci(p,lockfile)
            value=json.loads(lockfile.read_text()); value['pins']['node']={'version':'v22.13.0'}
            lockfile.write_text(json.dumps(value)); workflow.write_text(workflow.read_text()+'node-version: "20"\n')
            with self.assertRaisesRegex(lock.Refused,'node-version'): lock.check_ci(p,lockfile)

if __name__=='__main__': unittest.main(verbosity=2)
