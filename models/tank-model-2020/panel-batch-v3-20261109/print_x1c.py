#!/usr/bin/env python3
"""Read status by default. --start --bed-clear dispatches the four-panel batch once."""
import importlib.util
from prepare_print import HERE, JOB, FOLDER
from verify_print import verify


def main():
    spec = importlib.util.spec_from_file_location('fit_dispatch', HERE.parent / 'panel-fit-v2/print_x1c.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.main(HERE, JOB, FOLDER, verify,
                'Owner reports the full bilateral panel works very well, requests three more walls plus one notched base, and explicitly confirms X1C bed clear, 2026-11-09. Use the same loaded external PLA and successful saved presets.')


if __name__ == '__main__':
    main()
