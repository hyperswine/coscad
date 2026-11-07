#!/usr/bin/env python3
"""Recheck exact V3 geometry, upright poses, plate, paths and temperatures."""
import importlib.util
from prepare_print import HERE, FOLDER


def verify():
    spec = importlib.util.spec_from_file_location('v2_verify_print', HERE.parent / 'panel-fit-v2/verify_print.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.verify(HERE, FOLDER)


if __name__ == '__main__':
    verify()
