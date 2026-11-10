"""Independent analytic checks for extrusion moments and stale-input rejection."""
import json, math, tempfile, unittest
from pathlib import Path
import numpy as np
import trimesh
from toolpath_mass import arc_centroid, read_job
from provenance import require, snapshot

class AnalysisTests(unittest.TestCase):
    def test_arc_centroids(self):
        # A uniform semicircle has centroid 2r/pi above its centre.
        for clockwise,start,end,ij in [(False,[2,0],[-2,0],[-2,0]),(True,[-2,0],[2,0],[2,0])]:
            np.testing.assert_allclose(arc_centroid(np.array(start),np.array(end),np.array(ij),clockwise),[0,4/math.pi],atol=1e-12)
        np.testing.assert_allclose(arc_centroid(np.array([2,0]),np.array([2,0]),np.array([-2,0]),False),[0,0],atol=1e-12)

    def test_toolpath_mass_and_inverse_placement(self):
        with tempfile.TemporaryDirectory() as tmp:
            b=Path(tmp); f=b/'job'; (f/'slice').mkdir(parents=True); (f/'presets').mkdir()
            mesh=trimesh.creation.box(extents=[2,2,2]);mesh.apply_translation([0,0,1])
            mesh.export(b/'part.stl');mesh.export(b/'ears-part.stl')
            (f/'layout.json').write_text(json.dumps([{'part':'part','rotation_deg':90,'centre_mm':[20,30]}]))
            (f/'presets/machine.json').write_text(json.dumps({'extruder_offset':['1x2']}))
            (f/'presets/filament.json').write_text(json.dumps({'filament_density':['1.24'],'filament_diameter':['1.75']}))
            (f/'slice/plate_1.gcode').write_text('G90\nM83\nG1 X19 Y28\n; Z_HEIGHT: 0.2\n; LAYER_HEIGHT: 0.2\n; FEATURE: Outer wall\nG1 X19 Y30 E2\nG1 E-1\nG1 E1\n; FEATURE: Custom\nG1 X100 Y100 E100\n')
            row=read_job(b,'job')['part']
            self.assertAlmostEqual(row['mass_g'],2*1.24/1000*math.pi*(1.75/2)**2)
            np.testing.assert_allclose(row['print_cg_mm'],[1,0,.1],atol=1e-12)

    def test_stale_inputs_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            b=Path(tmp); p=b/'part';p.write_text('first');record=snapshot(b,[p]);require(b,record)
            p.write_text('second')
            with self.assertRaises(AssertionError):require(b,record)

if __name__=='__main__': unittest.main()
