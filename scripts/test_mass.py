"""Analytic regression fixtures for the explicitly supported slice dialect."""
import copy,json,math,subprocess,sys,tempfile,unittest
from pathlib import Path
import numpy as np
from coscad_mass import arc_centroid,deposited_moments,report,rigid_transform

HEADER='G90\nM83\n; Z_HEIGHT: 0.2\n; LAYER_HEIGHT: 0.2\n; FEATURE: Outer wall\n'
class MassTests(unittest.TestCase):
    def read(self,body,header=HEADER):return deposited_moments(header+body,{2:'body'},1.24,1.75)
    def test_line_mass_retractions_and_purge(self):
        row=self.read('M221 S\nM221 R\nG1 X10 E2\nG1 E-1\nG1 E1\n; FEATURE: Custom\nG1 X100 E100\n')['body']
        self.assertAlmostEqual(row['mass_g'],2*1.24/1000*math.pi*(1.75/2)**2)
        np.testing.assert_allclose(row['center_mm'],[5,0,.1])
    def test_semicircle_and_full_circle(self):
        np.testing.assert_allclose(arc_centroid([2,0],[-2,0],[-2,0],False),[0,4/math.pi],atol=1e-12)
        np.testing.assert_allclose(arc_centroid([-2,0],[2,0],[2,0],True),[0,4/math.pi],atol=1e-12)
        np.testing.assert_allclose(arc_centroid([2,0],[2,0],[-2,0],False),[0,0],atol=1e-12)
    def test_arc_in_toolpath(self):
        r=self.read('G1 X2 Y0\nG3 X-2 Y0 I-2 J0 E1\n')['body']
        np.testing.assert_allclose(r['center_mm'],[0,4/math.pi,.1],atol=1e-12)
    def test_unsupported_inputs(self):
        for code in ['M82\nG1 X10 E1','G91\nG1 X10 E1','G20\nG1 X10 E1','G18\nG1 X10 E1',
                     'M221 S90\nG1 X10 E1','M200 D1.75\nG1 X10 E1','T0\nT1\nG1 X10 E1','G5 X10 E1','G92 X1\nG1 X10 E1','G1 Xnan E1','G2 X10 R5 E1','; FEATURE: Unknown\nG1 X10 E1',
                     '; start printing object, unique label id: 9\nG1 X10 E1']:
            with self.subTest(code=code),self.assertRaises(ValueError):self.read(code)
    def test_support_excluded(self):
        r=self.read('G1 X10 E1\n; FEATURE: Support\nG1 X100 E100')['body']
        self.assertAlmostEqual(r['center_mm'][0],5)
    def test_explicit_multi_object_labels(self):
        text=HEADER+'; start printing object, unique label id: 2\nG1 X10 E1\n; stop printing object\nG1 X20\n; start printing object, unique label id: 4\nG1 X30 E2\n'
        r=deposited_moments(text,{2:'a',4:'b'},1.24,1.75)
        self.assertAlmostEqual(r['b']['mass_g'],2*r['a']['mass_g'])
        self.assertEqual(r['b']['center_mm'][0],25)
    def test_rigid_only(self):
        m=np.eye(4);m[0,0]=2
        with self.assertRaises(ValueError):rigid_transform(m)
        m[0,0]=-1;rigid_transform(m)
    def test_cli_preserves_inputs_and_previous_report(self):
        with tempfile.TemporaryDirectory() as tmp:
            b=Path(tmp);source=b/'input.json';output=b/'report.json'
            source.write_text(json.dumps({'schema':'coscad.slice-mass/1','dialect':'bambu-relative-e',
                'hardware':[{'name':'nut','mass_g':1,'center_mm':[0,0,0]}]}))
            command=[sys.executable,str(Path(__file__).with_name('slice-mass.py')),str(source)]
            original=source.read_bytes()
            result=subprocess.run(command+['-o',str(source)],capture_output=True)
            self.assertNotEqual(result.returncode,0);self.assertEqual(source.read_bytes(),original)
            output.write_text('previous report');source.write_text('{bad json')
            result=subprocess.run(command+['-o',str(output)],capture_output=True)
            self.assertNotEqual(result.returncode,0);self.assertEqual(output.read_text(),'previous report')

    def test_report_hardware_override_transform(self):
        with tempfile.TemporaryDirectory() as tmp:
            b=Path(tmp);(b/'part.gcode').write_text(HEADER+'G1 X10 E1\n')
            matrix=np.eye(4);matrix[0,0]=-1;matrix[0,3]=15
            m={'schema':'coscad.slice-mass/1','dialect':'bambu-relative-e','jobs':[{'gcode':'part.gcode','density_g_cm3':1.24,'filament_diameter_mm':1.75,
               'objects':[{'id':2,'name':'body','measured_mass_g':100,'plate_to_assembly':matrix.tolist()}]}],
               'hardware':[{'name':'bolt','mass_g':10,'center_mm':[120,0,.1]}]}
            r=report(m,b);self.assertEqual(r['mass_g'],110);np.testing.assert_allclose(r['center_mm'],[20,0,.1])
            self.assertEqual(r['components'][0]['kind'],'measured');self.assertEqual(len(r['sources'][0]['sha256']),64)
            bad=copy.deepcopy(m);bad['hardware'][0]['name']='body'
            with self.assertRaises(ValueError):report(bad,b)
            bad=copy.deepcopy(m);bad['jobs'][0]['objects'][0]['measured_mass_g']=-1
            with self.assertRaises(ValueError):report(bad,b)

if __name__=='__main__':unittest.main()
