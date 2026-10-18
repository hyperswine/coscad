#!/usr/bin/env python3
"""Docs drift check (CI): the command list and the exit-status contract in
README.md, man/coscad.1 and `coscad --help` (app/Main.hs) must agree, and
the README module map must name every module under src/Coscad.

Run from the repository root: python3 scripts/check-docs.py
"""
import pathlib, re, sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
read = lambda p: (ROOT / p).read_text(encoding='utf-8')

# Exit 2 cases, each with the word every exit-status text must use.
EXIT2 = {
    'validate/plan design errors': r'design error',
    'check overlaps': r'overlap',
    'next --strict printability warnings': r'(--strict|Fl -strict)',
    'a part that does not fit the plate': r'plate',
    'constraint loops that do not close': r'loop',
    'parts no fixed root reaches': r'fixed root',
    'an instance fixed twice': r'fixed twice',
}
STALE = [
    (r'[Ee]xit(s| code)? 1 (iff|if|when) any (OVERLAP|overlap)', 'check exits 2 on an overlap, not 1'),
    (r'\.assemble` files always resolve in legacy mode', '.assemble files accept the !simple/!glyph pragma'),
]

def section(text, start, end):
    i = text.find(start)
    if i < 0: return None
    j = text.find(end, i + len(start))
    return text[i:j if j >= 0 else len(text)]

def main():
    errors = []
    main_hs, readme, man = read('app/Main.hs'), read('README.md'), read('man/coscad.1')
    usage = section(main_hs, 'usage h =', '\n\n')
    commands = sorted(set(re.findall(r'"  coscad ([a-z]+)', usage or '')))
    if not commands: errors.append('app/Main.hs: no commands found in usage')
    synopsis = section(man, '.Sh SYNOPSIS', '.Sh ') or ''
    for c in commands:
        if not re.search(r'^\.Cm ' + c + r'\b', synopsis, re.M):
            errors.append(f'man/coscad.1: SYNOPSIS does not list `{c}` (in --help)')
        if not re.search(r'coscad ' + c + r'\b', readme):
            errors.append(f'README.md: never shows `coscad {c}` (in --help)')
    for c in re.findall(r'^\.Cm ([a-z]+)', synopsis, re.M):
        if c not in commands:
            errors.append(f'man/coscad.1: SYNOPSIS lists `{c}`, which --help does not')
    texts = {
        'coscad --help (app/Main.hs)': section(main_hs, 'Exit status:', 'Diagnostics go'),
        'man/coscad.1 EXIT STATUS': section(man, '.Sh EXIT STATUS', '\n.Sh '),
        'README.md exit status': section(readme, 'exit 1\nmeans', '\n\n'),
    }
    for where, text in texts.items():
        if text is None:
            errors.append(f'{where}: exit-status text not found'); continue
        flat = ' '.join(text.replace('"', ' ').replace(',', ' ').split())
        for code in ('1', '2'):
            if code not in flat: errors.append(f'{where}: does not mention exit {code}')
        for case, pat in EXIT2.items():
            if not re.search(pat, flat):
                errors.append(f'{where}: exit 2 case missing: {case}')
    docs = [p for p in [ROOT / 'README.md', *sorted((ROOT / 'docs').glob('*.md')), ROOT / 'man/coscad.1']]
    for p in docs:
        t = p.read_text(encoding='utf-8')
        for pat, why in STALE:
            for m in re.finditer(pat, t):
                line = t.count('\n', 0, m.start()) + 1
                errors.append(f'{p.relative_to(ROOT)}:{line}: stale text ({why})')
    module_map = section(readme, '## Module map', '\n## ') or ''
    for f in sorted((ROOT / 'src/Coscad').rglob('*.hs')):
        rel = f.relative_to(ROOT / 'src/Coscad').as_posix()
        if rel not in module_map:
            errors.append(f'README.md: module map does not list src/Coscad/{rel}')
    for e in errors: print('docs: ' + e, file=sys.stderr)
    if errors: return 1
    print(f'docs agree: {len(commands)} commands, exit status in {len(texts)} places, module map complete')
    return 0

if __name__ == '__main__':
    sys.exit(main())
