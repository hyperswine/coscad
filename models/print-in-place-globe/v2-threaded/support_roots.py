"""Sampled support continuity check; supplements the slicer's bed-only flag.

0.5 mm raster with 1 mm layer-to-layer lateral allowance. This detects isolated
trees starting on the model, but is not exact continuous toolpath simulation.
"""
from collections import deque
import numpy as np
from scipy import ndimage


class SupportRoots:
    def __init__(self):
        self.grid = np.zeros((520, 520), dtype=bool)
        self.history = deque(maxlen=3)
        self.height = None
        self.anomalies = []
        self.bed_components = 0
        self.layers = 0
        self.components = 0

    def layer(self, height):
        if self.height is not None:
            self.finish_layer()
        self.height = height

    def add(self, points):
        ij = np.rint(points/.5).astype(int)
        assert np.all(ij >= 0) and np.all(ij < 520), 'Support leaves nominal P1S bed'
        self.grid[ij[:, 0], ij[:, 1]] = True

    def finish_layer(self):
        # Adjacent raster cells stand in for the finite extrusion footprint.
        mask = ndimage.binary_dilation(self.grid, structure=np.ones((3, 3)))
        labels, count = ndimage.label(mask, structure=np.ones((3, 3)))
        self.components += count
        if count:
            self.layers += 1
            if self.height <= .21:
                self.bed_components += count
            else:
                previous = np.logical_or.reduce(list(self.history)) if self.history else np.zeros_like(mask)
                nearby = ndimage.binary_dilation(previous, structure=np.ones((5, 5)))
                connected = set(np.unique(labels[nearby]))
                for c in range(1, count+1):
                    if c not in connected:
                        loc = np.argwhere(labels == c).mean(axis=0)*.5
                        self.anomalies.append({'z_mm': self.height, 'xy_mm': loc.tolist(),
                                               'pixels': int((labels == c).sum())})
        self.history.append(mask)
        self.grid.fill(False)

    def report(self):
        if self.height is not None:
            self.finish_layer()
            self.height = None
        return {'raster_mm': .5, 'lateral_allowance_mm': 1,
                'previous_layers_considered': 3, 'support_layers': self.layers,
                'support_components_checked': self.components,
                'bed_components': self.bed_components,
                'components_without_prior_support': self.anomalies,
                'limitation': 'Sampled continuity, not exact extrusion topology or physical support strength'}
