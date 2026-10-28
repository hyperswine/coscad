# Changelog for the CoScad VS Code extension

## 0.5.0

- Add stress syntax, force/unit hovers, material completion and datum navigation.
- Add Run Stress Analysis and compiler-advertised backend selection/status.
- Validate complete unsaved project snapshots, track recursive dependencies,
  watch external files, cancel superseded checks and reject stale results.
- Save dirty dependencies before output commands; stop on a failed save.
- Refresh capability probes when the command, configuration or direct binary changes.

## Unreleased

- A compiler that predates `support`/`load`/`stress` no longer paints
  `unexpected 'b'; expecting '='` on those lines: the extension recognises the
  directives itself and drops the older compiler's errors on them, warning once
  that the compiler is older than the language support (the visible symptom of
  pointing `coscad.executableCommand` at a stale build).

- With a compiler that has `validate --json --stdin`, compiler diagnostics
  follow the buffer as you type (debounced) as well as open and save, and
  are read from JSON instead of parsed from stderr. While they are live the
  language server publishes no diagnostics of its own, so the compiler is
  the single source of language rules in the editor; with an older or
  missing compiler the server's diagnostics remain the fallback.

- Compiler warnings appear as warning diagnostics on save: `Warning:` lines
  from `coscad validate` are parsed like errors (positioned, file-level or
  unpositioned), and a valid file keeps its warnings instead of clearing
  every diagnostic. With a compiler that reports anchor positions, a
  floating anchor is underlined at its `at`/`on`/`cutat`/`anchor`.

- Frame references accept dotted instance paths (`@q.b`, `@s.q.b`).
- `.assemble` diagnostics accept numeric atoms in `fixed` poses and `mate`
  `roll=` / `gap=` (`w`, `-w`, `(w / 2)`), report undefined or shape names
  there, and know the `tolerance <mm> [<rotation>]` statement (keyword
  highlight, completion and hover).
- `.assemble` diagnostics accept `plate W D [margin [maxZ]] [brim=N|auto]`:
  the 4th max-height argument and the `brim=` option no longer show
  "Unexpected value in plate declaration".

## 0.4.0

- Lives in the compiler repository under `vscode/`, versioned with it.
- Compiler diagnostics probe `coscad --version` once per configuration: a
  missing compiler, a compiler without `validate` (older than 1.2.0.0) or a
  command that answers with something else produces one notice and no red
  error on every file.
- Reads the compiler's 1.1 diagnostics (`file:line:col: reason` with the
  excerpt after it, `file: design: reason`, file-level errors) and still the
  older layout. A leading blank line no longer hides a diagnostic; a validate
  that times out shows a warning instead of silently clearing; a failure with
  no parseable message shows its last lines.
- `coscad.workingDirectory` replaces `coscad.executablePath` (which was a
  working directory despite its name; still honoured when the new one is
  empty). The command is split like a shell command line and run without a
  shell for diagnostics.
- Terminal commands run in a terminal created in the right directory; no
  `cd ... &&`, so they work in PowerShell and cmd as well as POSIX shells.
- Commands: Validate and Build Plan with step images (`plan --png`).
- Requires VS Code 1.82 (the bundled language client's minimum). Publisher
  `hyperswine`, GPL-3.0 like the compiler, repository link, one 256 px icon;
  the package is about a tenth of its former size.

## 0.3.1 and earlier

See the compiler's CHANGELOG.md for the language features each version
tracked (mating frames, assembly constraints, contextual token hover,
numeric types, diagnostics and completion).
