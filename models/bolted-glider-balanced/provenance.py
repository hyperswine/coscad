"""Refuse stale meshes, projects or slices after a design/settings change."""
import hashlib, json

def digest(path): return hashlib.sha256(path.read_bytes()).hexdigest()
def snapshot(base, paths): return {str(p.relative_to(base)):digest(p) for p in paths}
def require(base, record):
    for path,expected in record.items():
        assert digest(base/path)==expected, f'Stale input: {path}; rebuild before using the report/package'
def require_geometry(base):
    require(base,json.loads((base/'meshes.json').read_text()))
def require_slice(base,job):
    record=json.loads((base/job/'slice/input-hashes.json').read_text())
    require(base,record)
