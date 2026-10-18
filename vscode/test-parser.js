const assert = require('assert/strict')
const fs = require('fs')
const path = require('path')
const { COSCADParser } = require('./coscad-parser')

const parser = new COSCADParser()
let checks = 0
const ok = (condition, detail) => { assert.ok(condition, detail); checks++ }

function diagnostics(code, fileType = 'coscad') {
  return parser.getDiagnostics(code, { fileType })
}
const messages = (code, fileType) => diagnostics(code, fileType).map(d => d.message)

// --- glyph / pipeline / attachment ---------------------------------
const glyphCode = `!glyph
base = ▣ 40 40 10 1
post = ⌽ 5 26 2
joined = base ⋈ rt post
main = joined
  |> at top+rt 0 0 2 (⌭ 3 8 0.5)
  |> cutat top 0 0 -2 (zcyl 2.15 30)
`
const glyphAst = parser.parse(glyphCode)
assert.equal(glyphAst.mode, 'glyph')
assert.equal(glyphAst.errors.length, 0)
assert.deepEqual(messages(glyphCode), [])
assert.equal(glyphAst.body.find(node => node.name === 'joined').value.type, 'attachment_operation')
assert.equal(glyphAst.body.find(node => node.name === 'main').value.type, 'pipeline_operation')
checks += 5

// --- simple mode ----------------------------------------------------
const simpleCode = `!simple
ball = Sphere 10
moved = Translate.x 5 ball
scaled = Scale (2, 1, 0.5) ball
mirrored = Mirror.y moved
main = moved * scaled - mirrored
`
assert.equal(parser.parse(simpleCode).errors.length, 0)
assert.deepEqual(messages(simpleCode), [])
checks += 2

// --- assembly -------------------------------------------------------
const assemblyCode = `!glyph
plate 250 300 6
clearance 0.15
center ← center.coscad ×2 ▽top seam=rear
corner ← corner.assemble ×1
premade ← rail.coscad ×0
gap = 2
asm = center ⊕ (corner |> move (55 + gap) -13 -7) ⊕ premade
`
const assemblyAst = parser.parse(assemblyCode, { fileType: 'assemble' })
assert.equal(assemblyAst.errors.length, 0)
assert.deepEqual(messages(assemblyCode, 'assemble'), [])
const center = assemblyAst.body.find(node => node.type === 'part_declaration' && node.name === 'center')
assert.deepEqual(
  { source: center.source, count: center.count, orientation: center.orientation, hints: center.hints },
  { source: 'center.coscad', count: 2, orientation: 'top', hints: [{ key: 'seam', value: 'rear' }] }
)
checks += 3

// --- plate W D [margin [maxZ]] [brim=N|auto] ------------------------
const plateOf = code => parser.parse(code, { fileType: 'assemble' }).body.find(node => node.type === 'plate_declaration')
for (const [line, want] of [
  ['plate 180 180 6 180', [6, 180, null]], ['plate 256 256 6', [6, 250, null]], ['plate 256 256 6 brim=8', [6, 250, 8]],
  ['plate 256 256 brim=auto', [6, 250, 5]], ['plate 256 256 6 220 brim=auto', [6, 220, 5]]]) {
  const code = `${line}\nx ← a.coscad ×1\n`
  assert.deepEqual(messages(code, 'assemble'), [], line)
  const p = plateOf(code)
  assert.deepEqual([p.margin, p.maxZ, p.brim], want, line)
  checks += 2
}
assert.deepEqual(messages('plate 256 256 6 0\nx ← a.coscad ×1\n', 'assemble'), ['plate max height must be a finite distance in mm, greater than zero'])
assert.deepEqual(messages('plate 256 256 6 180 7\nx ← a.coscad ×1\n', 'assemble'), ['Unexpected value in plate declaration'])
checks += 2

// --- fasteners, plan, continuation lines ----------------------------
const planCode = `!glyph
plate 250 250 6
rx ← rail200.coscad ×0 profile=2020 material=aluminium
br ← flat90.coscad ×2
px ← panel.coscad ×1

asm = (rx |> move 100 10 10)
  // brackets
  ⊕ (br |> move 20 20 -3) ⊕ (br |> move 180 20 -3)
  ⊕ (px |> roty 90 |> move 201.5 100 100)

fastener M5x10 br#1 rx bot at=30
fastener M5x10 px rx rt ×2 nut=slidein through=3
plan beam=30 rest=bot,top
`
const planAst = parser.parse(planCode, { fileType: 'assemble' })
assert.equal(planAst.errors.length, 0, JSON.stringify(planAst.errors.map(e => e.message)))
assert.deepEqual(messages(planCode, 'assemble'), [])
const fast = planAst.body.filter(node => node.type === 'fastener_declaration')
assert.equal(fast.length, 2)
assert.deepEqual([fast[0].spec, fast[0].clamped, fast[0].host, fast[0].face, fast[0].options], ['M5x10', 'br#1', 'rx', 'bot', [{ key: 'at', value: '30' }]])
assert.deepEqual([fast[1].count, fast[1].options.map(o => o.key)], [2, ['nut', 'through']])
assert.equal(planAst.body.find(node => node.type === 'plan_declaration').options.length, 2)
assert.equal(planAst.body.find(node => node.name === 'asm').value.type, 'boolean_operation')
ok(messages('a ← a.coscad ×1\nfastener M5x10 zz a top\nasm = a\n', 'assemble').some(m => m.includes("unknown part 'zz'")), 'fastener unknown part')
ok(messages('a ← a.coscad ×1\nfastener M5 a a top\nasm = a\n', 'assemble').some(m => m.includes('cannot read screw spec')), 'bad spec')
ok(messages('a ← a.coscad ×1\nfastener M5x8 a a sideways\nasm = a\n', 'assemble').some(m => m.includes("unknown face 'sideways'")), 'bad face')
checks += 10

// --- numeric atoms in fixed / mate, and tolerance (step 10) ---------
const loopCode = 'w = 8.5\nr = (w * 2)\na ← a.coscad ×1\nb ← b.coscad ×1\ntolerance 0.00001\nfixed a\nfixed b w -w 0 0 0 (r / 2)\nmate b @x a @y roll=(w / 2) gap=-w\n'
assert.deepEqual(messages(loopCode, 'assemble'), [])
const loopAst = parser.parse(loopCode, { fileType: 'assemble' })
const tolNode = loopAst.body.find(node => node.type === 'tolerance_declaration')
assert.deepEqual([tolNode.positionTolerance, tolNode.rotationTolerance], [0.00001, 1e-8])
assert.equal(loopAst.body.filter(node => node.type === 'constraint_declaration')[1].pose[0].type, 'number_reference')
ok(messages('a ← a.coscad ×1\nfixed a q 0 0 0 0 0\n', 'assemble').some(m => m.includes("undefined variable 'q'")), 'fixed unknown binding')
ok(messages('a ← a.coscad ×1\nb ← b.coscad ×1\nfixed a\nmate b @x a @y roll=a\n', 'assemble').some(m => m.includes("'a' is a shape, not a number")), 'mate shape as number')
ok(messages('a ← a.coscad ×1\nfixed a 1 2 3\n', 'assemble').some(m => m.includes('fixed needs six pose numbers')), 'short pose')
ok(messages('a ← a.coscad ×1\ntolerance 0\nfixed a\n', 'assemble').some(m => m.includes('position tolerance must be')), 'zero tolerance')
checks += 8

// --- diagnostics wording (matches the compiler) ---------------------
const modeErrors = diagnostics(`!glyph
main = Translate.x 5 (■ 10)
`)
assert.match(modeErrors[0].message, /not available in !glyph mode/)
assert.equal(modeErrors[0].range.start.line, 1)

const undefinedErrors = diagnostics('main = missing\n')
assert.equal(undefinedErrors.length, 1)
assert.equal(undefinedErrors[0].message, "undefined variable 'missing'")
assert.deepEqual(undefinedErrors[0].range.start, { line: 0, character: 7 })

ok(messages('a = b ⊕ ● 1\nb = a ⊕ ● 2\nmain = a\n').some(m => m.includes('circular dependency')), 'cycle')
ok(messages('part = ■ 10\n').some(m => m.includes("no 'main' definition")), 'missing main')
ok(messages('a = ● 1\na = ● 2\nmain = a\n').some(m => m.startsWith("duplicate definition of 'a'")), 'duplicate')
checks += 5

// --- numeric bindings -----------------------------------------------
const numbersCode = `w = 20
t = w / 5 - 1
half = (w / 2)
neg = -t
main = box w half t
  |> cutat top 0 0 (-t / 2) (zcyl 1 50)
  |> x (w -2)
`
const numbersAst = parser.parse(numbersCode)
assert.equal(numbersAst.errors.length, 0)
assert.deepEqual([...numbersAst.numericNames].sort(), ['half', 'neg', 't', 'w'])
assert.equal(numbersAst.body.find(node => node.name === 't').numericValue, 3)
assert.equal(numbersAst.body.find(node => node.name === 'half').numericValue, 10)
assert.equal(numbersAst.body.find(node => node.name === 'neg').numericValue, -3)
assert.deepEqual(messages(numbersCode), [])
checks += 6

assert.deepEqual(messages('r = 5\nmain = r ⊕ ● 1\n'), ["'r' is a number, not a shape (numbers are used as arguments: ● r, box r ...)"])
assert.deepEqual(messages('plate = box 1 1 1\nmain = ● plate\n'), ["'plate' is a shape, not a number"])
assert.deepEqual(messages('main = 5\n'), ["'main' is a number, not a shape; main must be the shape that gets rendered"])
assert.deepEqual(messages('r = q * 2\nmain = ● r\n'), ["undefined variable 'q'"])
ok(messages('a = b * 2 + 1\nb = 3\nmain = χ -a (● 1)\n').length === 0, 'forward numeric refs + negated name')
const shapeAlias = parser.parse('base = ● 5\nalias = base\nmain = alias\n')
assert.equal(shapeAlias.errors.length, 0)
assert.equal(shapeAlias.numericNames.size, 0)
checks += 7

// --- lofts ----------------------------------------------------------
const loftCode = `r_top = 20
r_bot = 8
h = 40
outer = loft 0 (⭘ r_bot) h (⭘ r_top)
lug = ⭘ 3 |> loft 6 (ω 30 (△ 4)) |> loft 9 (⬠ 2)
glyph = ⟰ 0 (△ 4 ↯ ⭘ 1) 10 (χ 2 (⭘ 3))
main = outer ⊕ lug ⊕ glyph
`
const loftAst = parser.parse(loftCode)
assert.equal(loftAst.errors.length, 0)
assert.deepEqual(messages(loftCode), [])
assert.equal(loftAst.body.find(node => node.name === 'outer').value.type, 'loft')
assert.equal(loftAst.body.find(node => node.name === 'outer').value.profiles.length, 2)
assert.equal(loftAst.body.find(node => node.name === 'lug').value.name, 'loft')
assert.deepEqual(messages('!simple\nmain = Loft 0 (Circle 3) 10 (Triangle 2)\n'), [])
checks += 6

ok(messages('main = loft 0 (● 3) 10 (⭘ 5)\n').some(m => m.includes('loft profiles must be 2D')), 'loft of solid')
ok(messages('main = loft 0 (⭘ 3 ⊕ χ 5 (⭘ 3)) 10 (⭘ 5)\n').some(m => m.includes('single closed outline')), 'boolean profile')
ok(messages('main = loft 0 (⭘ 3)\n').some(m => m.includes('at least two profiles')), 'one profile')
ok(messages('main = loft 0 (θ 90 (⭘ 3)) 10 (⭘ 5)\n').some(m => m.includes('XY plane')), 'out of plane')
ok(messages('p = ζ 3 (⭘ 3)\nmain = loft 0 p 10 (⭘ 5)\n').some(m => m.includes('moved in Z')), 'z inside profile via variable')
checks += 5

// --- dimensionality -------------------------------------------------
ok(messages('main = ⮕ 2 (● 1)\n').some(m => m.startsWith('cannot extrude a 3D solid')), 'extrude solid')
ok(messages('hole = ⮕ 2 (● 1)\nmain = ■ 10 ⊖ hole\n').some(m => m.startsWith('cannot extrude a 3D solid')), 'extrude solid in helper')
ok(messages('main = ■ 10 ↯ ● 2\n').some(m => m.startsWith('offset (↯ / Offset) needs a 2D profile')), 'offset solid')
ok(messages('main = ⭘ 3 ⊕ ■ 2\n').some(m => m.includes('2D profile with a 3D solid')), 'mixed union')
ok(messages('main = △ 8\n').some(m => m.includes("'main' is a 2D profile")), '2D main')
ok(messages('main = box 2 2 2 |> add (⭘ 1)\n').some(m => m.includes('cannot union a 2D profile')), 'mixed pipeline add')
assert.deepEqual(messages('main = ⮕ 5 (△ 8 ↯ ⭘ 2)\n'), [])
checks += 7

// --- optional: every example in a compiler checkout is clean ---------
const examplesRoot = process.env.COSCAD_EXAMPLES
if (examplesRoot) {
  const walk = dir => fs.readdirSync(dir, { withFileTypes: true }).flatMap(entry => {
    const full = path.join(dir, entry.name)
    if (entry.isDirectory()) return entry.name === 'archive' || entry.name === 'haskell' ? [] : walk(full)
    return /\.(coscad|assemble)$/.test(entry.name) ? [full] : []
  })
  const files = walk(examplesRoot)
  let clean = 0
  for (const file of files) {
    const fileType = file.endsWith('.assemble') ? 'assemble' : 'coscad'
    const found = messages(fs.readFileSync(file, 'utf8'), fileType)
    if (found.length) console.log(`  ${path.relative(examplesRoot, file)}: ${found.join(' | ')}`)
    else clean++
  }
  assert.equal(clean, files.length, `${files.length - clean} example file(s) produced diagnostics`)
  checks += files.length
  console.log(`  ${files.length} compiler example files parse clean`)
}

console.log(`All CoScad parser tests passed (${checks} checks)`)
