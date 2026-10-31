#!/usr/bin/env python3
"""Apply the existing actual-G-code sequential audit with X1C-specific identity."""
from pathlib import Path
import importlib.util
import json

HERE = Path(__file__).resolve().parent


def main():
    spec = importlib.util.spec_from_file_location('sequential_verify', HERE.parent/'verify.py')
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    checker.verify(HERE/'mixed', plan_path=HERE/'plan.json', printer='x1c')
    path = HERE/'plan.json'
    plan = json.loads(path.read_text())
    plan['verified'] = True
    plan['validation_report'] = 'mixed/sequential-validation.json'
    path.write_text(json.dumps(plan, indent=2)+'\n')


if __name__ == '__main__':
    main()
