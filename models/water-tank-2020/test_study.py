#!/usr/bin/env python3
"""Focused physical invariants for this component experiment."""
import unittest
import numpy as np
from analyse import plate,frame,G

class StudyTests(unittest.TestCase):
    def test_partial_wetting_force_and_moment(self):
        # 50 mm of a 200 mm panel is wetted; no average-pressure shortcut.
        r=plate([-100,100,-100,100],12,'rail_and_tabs',50,112,False)
        self.assertAlmostEqual(r['load_n'],G*200*50**2/2,places=8)
        self.assertLess(r['moment_balance_n_mm'],1e-5)
        self.assertLess(r['force_balance_n'],1e-6)
    def test_dry_panel(self):
        r=plate([-100,100,-100,100],12,'rail_and_tabs',0,112,False)
        self.assertEqual(r['load_n'],0)
        self.assertEqual(r['max_abs_displacement_mm'],0)
    def test_linear_scaling_is_reported_as_out_of_range(self):
        a=plate([-100,100,-100,100],12,'tabs',250,floor=True)
        b=plate([-100,100,-100,100],12,'tabs',500,floor=True)
        self.assertAlmostEqual(b['max_abs_displacement_mm']/a['max_abs_displacement_mm'],2,places=9)
        self.assertFalse(a['linear_geometry_credible'])
    def test_rotated_cantilever_and_stress(self):
        section={'area_mm2':200,'Iy_mm4':8000,'Iz_mm4':8000}
        model={'beams':[{'id':'beam','a':[0,0,0],'b':[0,0,1000]}]}
        for force in ([1,0,0],[0,1,0]):
            r=frame(model,[{'attachment_mm':[0,0,1000],'force_n':force,'moment_n_mm':[0,0,0]}],section,4)
            self.assertAlmostEqual(r['max_displacement_mm'],1000**3/(3*69000*8000),places=9)
            self.assertAlmostEqual(r['max_normal_stress_envelope_mpa'],1000*10/8000,places=9)
            self.assertLess(r['force_balance_n'],1e-8)
    def test_off_frame_load_refused(self):
        section={'area_mm2':200,'Iy_mm4':8000,'Iz_mm4':8000}
        model={'beams':[{'id':'beam','a':[0,0,0],'b':[0,0,1000]}]}
        with self.assertRaisesRegex(ValueError,'unsupported attachment'):
            frame(model,[{'attachment_mm':[50,0,500],'force_n':[1,0,0],'moment_n_mm':[0,0,0]}],section)

if __name__=='__main__':unittest.main()
