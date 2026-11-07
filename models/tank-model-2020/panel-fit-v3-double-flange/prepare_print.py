#!/usr/bin/env python3
"""Slice the three upright V3 coupons using the verified V2 X1C workflow."""
import importlib.util
from pathlib import Path

HERE = Path(__file__).resolve().parent
JOB = 'x1c-double-flange-fit-20261107'
FOLDER = HERE / JOB
PARTS = [f'double-c{c:03d}-{holes}holes' for holes, c in enumerate((20, 25, 30), 1)]


def main():
    spec = importlib.util.spec_from_file_location('v2_prepare_print', HERE.parent / 'panel-fit-v2/prepare_print.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.main(HERE, JOB, PARTS)


if __name__ == '__main__':
    main()
