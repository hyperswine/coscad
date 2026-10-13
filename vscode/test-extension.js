// Tests parseCompilerErrors and the compiler probe helpers without VS Code:
// stub the `vscode` module.
const assert = require('assert/strict')
const Module = require('module')
const originalLoad = Module._load
Module._load = function (request, ...rest) {
  if (request === 'vscode') {
    return {
      Range: class { constructor(l0, c0, l1, c1) { Object.assign(this, { start: { line: l0, character: c0 }, end: { line: l1, character: c1 } }) } },
      Diagnostic: class { constructor(range, message, severity) { Object.assign(this, { range, message, severity }) } },
      DiagnosticSeverity: { Error: 0, Warning: 1 }
    }
  }
  if (request === 'vscode-languageclient/node') return { LanguageClient: class {}, TransportKind: {} }
  return originalLoad.call(this, request, ...rest)
}
const { parseCompilerErrors, parseVersion, versionAtLeast, splitCommand } = require('./extension')

// compiler 1.1: reason on the position line, excerpt after it
const block = `Error: /tmp/x/part.coscad:3:14: unexpected "▼ 3"; expecting a shape (glyph or word), a variable, or '(' or a transform
  |
3 | main = ● 5 ⊕ ▼ 3
  |              ^^^
`
let d = parseCompilerErrors(block, '/tmp/x/part.coscad')
assert.equal(d.length, 1)
assert.deepEqual([d[0].range.start.line, d[0].range.start.character], [2, 13])
assert.match(d[0].message, /^unexpected "▼ 3"; expecting a shape/)

// pre-1.1 layout (reason after the excerpt) still parses
const legacy = `Error: /tmp/x/part.coscad:3:14:
  |
3 | main = ● 5 ⊕ ▼ 3
  |              ^^^
unexpected "▼ 3"
expecting a shape
`
d = parseCompilerErrors(legacy, '/tmp/x/part.coscad')
assert.equal(d.length, 1)
assert.match(d[0].message, /^unexpected "▼ 3" expecting a shape/)

const inline = `Error: /tmp/x/part.coscad:2:8: in 'hole': cannot extrude a 3D solid: ⮕ / extrude needs a 2D profile\n`
d = parseCompilerErrors(inline, '/tmp/x/part.coscad')
assert.equal(d.length, 1)
assert.deepEqual([d[0].range.start.line, d[0].range.start.character], [1, 7])
assert.equal(d[0].message, "in 'hole': cannot extrude a 3D solid: ⮕ / extrude needs a 2D profile")

const two = `Error: /tmp/x/part.coscad:1:5: unexpected '▼'
  |
1 | a = ▼
  |     ^
Error: /tmp/x/part.coscad:4:9: unexpected end of input
  |
4 | main = (● 1
  |         ^
`
d = parseCompilerErrors(two, '/tmp/x/part.coscad')
assert.equal(d.length, 2)
assert.equal(d[1].range.start.line, 3)
assert.equal(d[1].message, 'unexpected end of input')

const circular = `Error: circular dependency: these definitions refer to each other and can never be resolved:
  a  (/tmp/x/part.coscad:1:5)
  b  (/tmp/x/part.coscad:2:5)
`
d = parseCompilerErrors(circular, '/tmp/x/part.coscad')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 0)
assert.match(d[0].message, /circular dependency.*a {2}\(\/tmp\/x\/part.coscad:1:5\)/)

// referenced part: shown at the top of the assembly with its own position
d = parseCompilerErrors('Error: /tmp/parts/b.coscad:7:9: unknown frame', '/tmp/test.assemble')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 0)
assert.match(d[0].message, /b.coscad:7:9: unknown frame/)
d = parseCompilerErrors('Error: /other/part.coscad:7:9: invalid solid', '/tmp/part.coscad')
assert.equal(d[0].range.start.line, 0)
assert.match(d[0].message, /other\/part.coscad:7:9/)

// fastener errors carry the assembly's own position (compiler 1.1)
d = parseCompilerErrors('Error: /tmp/test.assemble:8:1: fastener: head seat and host hole frames must be coaxial', '/tmp/test.assemble')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 7)
assert.match(d[0].message, /^fastener: .*must be coaxial/)

// design errors (exit 2) and file-level errors
d = parseCompilerErrors('Error: /tmp/test.assemble: design: screws collide: M5x10 into railA (top @ 360 mm) and M5x10 into railA share the same spot\n', '/tmp/test.assemble')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 0)
assert.match(d[0].message, /^design: screws collide/)
d = parseCompilerErrors('Error: /tmp/gone.coscad: part file not found (referenced from test.assemble)\n', '/tmp/test.assemble')
assert.equal(d.length, 1)
assert.match(d[0].message, /^\/tmp\/gone.coscad: part file not found/)

// a leading blank line or stray output must not hide the diagnostic
d = parseCompilerErrors('\n\nError: /tmp/x/part.coscad:2:8: in \'hole\': bad\n', '/tmp/x/part.coscad')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 1)
assert.deepEqual(parseCompilerErrors('', '/tmp/x/part.coscad'), [])

// Windows drive letters in paths
d = parseCompilerErrors('Error: C:\\work\\part.coscad:2:8: bad\n', 'C:\\work\\part.coscad')
assert.equal(d.length, 1)
assert.equal(d[0].range.start.line, 1)

// version probe helpers
assert.deepEqual(parseVersion('coscad 1.1.0.0\n'), [1, 1, 0, 0])
assert.equal(parseVersion('Error: expected a .coscad file, got --version'), null)
assert.equal(versionAtLeast([1, 2, 0, 0], [1, 2, 0, 0]), true)
assert.equal(versionAtLeast([1, 1, 9, 9], [1, 2, 0, 0]), false)
assert.equal(versionAtLeast([1, 3], [1, 2, 0, 0]), true)
assert.deepEqual(splitCommand('cabal run coscad --'), ['cabal', 'run', 'coscad', '--'])
assert.deepEqual(splitCommand('"C:\\Program Files\\coscad\\coscad.exe"'), ['C:\\Program Files\\coscad\\coscad.exe'])

console.log('All extension tests passed')
