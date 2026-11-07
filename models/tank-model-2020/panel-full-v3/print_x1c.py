#!/usr/bin/env python3
"""Read status by default. --start --bed-clear sends the full-panel job once."""
import importlib.util
from prepare_print import HERE, JOB, FOLDER
from verify_print import verify


def main():
    spec = importlib.util.spec_from_file_location('fit_dispatch', HERE.parent / 'panel-fit-v2/print_x1c.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.main(HERE, JOB, FOLDER, verify,
                'Owner reports all three bilateral flange clearances work, requests one full panel, and explicitly confirms X1C bed clear, 2026-11-07. Use middle 0.25 mm clearance and loaded external PLA.')


if __name__ == '__main__':
    main()
