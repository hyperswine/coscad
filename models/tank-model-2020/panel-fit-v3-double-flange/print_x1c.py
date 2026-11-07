#!/usr/bin/env python3
"""Read status by default; --start --bed-clear dispatches V3 exactly once."""
import importlib.util
from prepare_print import HERE, JOB, FOLDER
from verify_print import verify


def main():
    spec = importlib.util.spec_from_file_location('v2_print_x1c', HERE.parent / 'panel-fit-v2/print_x1c.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.main(HERE, JOB, FOLDER, verify,
                'Owner requested slicing and printing the three V3 double-flange panel fit samples on X1C and explicitly confirmed its bed clear, 2026-11-07. Use the loaded external PLA.')


if __name__ == '__main__':
    main()
