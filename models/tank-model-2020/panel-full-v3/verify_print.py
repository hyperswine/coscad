#!/usr/bin/env python3
"""Verify exactly one full panel, its geometry, paths, actual plate and temperatures."""
import importlib.util
from prepare_print import HERE, FOLDER


def verify():
    spec = importlib.util.spec_from_file_location('fit_verify', HERE.parent / 'panel-fit-v2/verify_print.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.verify(HERE, FOLDER, expected_parts=1)


if __name__ == '__main__':
    verify()
