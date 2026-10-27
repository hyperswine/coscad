// Contextual hover information shared by the server and regression tests.
const { COSCADParser } = require('./coscad-parser')

const syntax = {
  '=': ['Definition', 'Binds a name to a Number or Shape. Definitions may refer forward to other definitions. In assembly options, separates the option name from its value.'],
  '←': ['Part declaration', 'Declares a separate physical part from a .coscad or recursive .assemble file.'],
  '×': ['Print quantity', 'Integer print count. ×0 means premade. Constrained aliases use ×0 or ×1, one alias per physical instance.'],
  '▽': ['Print orientation', 'Names the face placed on the print bed; does not change the assembled pose.'],
  '@': ['Named point or mating frame', 'Selects construction metadata. A datum supplies a point; a frame supplies an origin, outward normal and up direction. Mates require frames.'],
  '#': ['Instance index', 'Selects a repeated placed instance, for example bracket#2. Constraint authoring uses distinct part aliases instead.'],
  '(': ['Grouping', 'Begins a grouped shape or numeric arithmetic expression.'],
  ')': ['Grouping', 'Ends a grouped shape or numeric arithmetic expression.'],
  ',': ['Separator', 'Separates vector components or assembly option values such as at=20,80.'],
  '.': ['Axis or path', 'Selects a simple-mode transform axis (Translate.x), a pocket face (clamped.top), or a filename extension.'],
  '$': ['Shape application', 'Passes the following expression as one shape argument.'],
  '+': ['Addition or combined anchor', 'Adds numeric operands, or combines anchor directions such as top+rt.'],
  '-': ['Negation, subtraction or difference', 'Negates a numeric argument, subtracts numeric operands, or subtracts shapes in simple mode.'],
  '*': ['Multiplication or union', 'Multiplies numeric operands, or unions shapes in simple mode.'],
  '/': ['Division or path separator', 'Divides numeric operands in arithmetic, or separates directories in a part source path.'],
  '|>': ['Pipeline', 'Applies a stage to the shape built so far. A line starting with |> continues the previous definition.'],
  support: ['Clamped support', 'support face or support @datum [face]. Clamps selected exterior nodes; a datum with a face selects its plane. No joint or contact model.'],
  load: ['Applied load · N', 'load face fx fy fz or load @datum [face] fx fy fz. World-axis total force in newtons. Bare @datum is a point load and introduces a stress singularity.'],
  stress: ['Part stress analysis', 'stress material=pla safety=2 cell=1.5. Small-strain isotropic analysis; cell is in mm. Run Stress Analysis explicitly; compare refinements and inspect assumptions.'],
  safety: ['Requested safety factor', 'Dimensionless target from 1 to 100; default 2. A modeled threshold result is not a physical safety certification.'],
  cell: ['Analysis cell size · mm', 'Positive voxel spacing. Use at least two cells through thin walls and compare refined results.'],
  fixed: ['Fixed pose', 'fixed alias [x y z rx ry rz]. Grounds one part: translation in millimetres and XYZ rotation in degrees; omitted pose is identity. Each number may be a numeric binding, -atom or (arithmetic). Every constraint component needs a fixed root.'],
  tolerance: ['Loop closure tolerance', 'tolerance mm [rotation]. How far a mating loop may miss and still close: position in mm (default 1e-6) and rotation-matrix entries (default 1e-8).'],
  mate: ['Rigid mating constraint', 'mate child @own parent @target [roll=degrees gap=mm] (numeric atoms: 90, w, -w, (w / 2)), or child |> mate parent @target @own [roll gap]. Normals oppose, up directions agree at zero roll. Fixes all six degrees of freedom; does not infer hardware or assembly order.'],
  frame: ['MatingFrame', '|> frame name ref normal up [dx dy dz]. Saves an origin, outward +Z normal and projected +Y up direction on a 3D solid. The geometry is unchanged.'],
  datum: ['Point datum', '|> datum name ref [dx dy dz]. Saves a construction point without adding material. It follows transforms and CSG.'],
  place: ['Placement', 'child |> place parent target own [dx dy dz]. Translates the child so its selected point meets the parent point; returns the child only.'],
  grid: ['Rectangular grid', '|> grid n m pitch_x pitch_y. Repeats the shape in an N×M grid. Counts are positive integers and pitches are millimetres.'],
  hull: ['Convex hull', '|> hull takes the convex hull of the whole input group. The binary form also accepts a second shape.'],
  plate: ['Build plate', 'plate width depth [margin]. Dimensions are millimetres; default margin is 6 mm.'],
  clearance: ['Required clearance', 'clearance mm. Required non-negative separation for coscad check.'],
  fastener: ['Assembly joint', 'fastener spec clamped host face|@frame [options]. A screw or peg joint for instruction ordering and access checks; geometric mates do not replace these declarations.'],
  plan: ['Instruction search', 'plan beam=N rest=faces [cost weights] [astar=N]. Sets instruction search width, permitted bench faces and costs.'],
  roll: ['Roll angle · degrees', 'Rotation about the parent frame’s outward normal; zero aligns up directions.'],
  gap: ['Mating gap · mm', 'Separation along the parent’s outward normal. Negative values permit intentional penetration; check interference separately.'],
  inset: ['Peg seating depth · mm', 'Moves the peg seat inward from the host face. Must lie within the host; not supported for screws.'],
  after: ['Placement prerequisites', 'Comma-separated part aliases or placed instance names that must be installed before this part. Used for capture order, independently of geometric mates.'],
  at: ['Fastener distances · mm', 'Distances measured along the host from its marked low end; multiple distances are comma-separated. In a pipeline, at positions and unions a child at a bounding-box anchor.'],
  through: ['Clamped thickness · mm', 'Material under the screw head; overrides the clamped bounding-box thickness for recessed holes.'],
  torque: ['Tightening torque · Nm', 'Finite positive torque override.'],
  material: ['Material', 'Stress materials: pla, petg, abs, asa, pa, tpu, aluminium, steel. Modulus/strength values are illustrative defaults, not measured spool properties. In assemblies this is also a torque/material hint.'],
  profile: ['Rail profile', 'profile=2020 identifies a rail with four long T-slot faces.'],
  ends: ['Rail ends', 'ends=open,blocked marks access at the low and high rail ends.'],
  head: ['Screw head', 'head=button|socket selects the screw head and driver envelope.'],
  nut: ['Nut type', 'nut=dropin|slidein|hex|none selects the preload and thread behavior.'],
  seat: ['Screw head bearing frame', 'seat=@frame selects a frame on the clamped part at the underside of the head. Its outward normal must match the host hole frame.'],
  preload: ['Nut preparation order', 'preload=hand loads the independent part before placing it. preload=placed loads nuts after placement and checks access on the bench.'],
  pocket: ['Hex nut pocket', 'pocket=face or pocket=clamped.face describes the opening. With a host @frame, pocket=@frame (or clamped.@frame) names the nut bearing seat with its outward normal opposite the screw head.'],
  beam: ['Search width · integer', 'Positive number of instruction search states retained per round; default 30.'],
  rest: ['Bench faces', 'Comma-separated permitted rest faces, such as rest=bot,top.'],
  astar: ['Exact-search cap · integer', 'Positive expansion cap for trying exact search before beam search.'],
  mass: ['Mass · grams', 'Finite positive override for the volume-based mass estimate.']
}

for (const name of ['pla','petg','abs','asa','pa','tpu','aluminium','steel']) {
  syntax[name] = ['Stress material', `${name}: homogeneous isotropic material-table assumption. Printed strength depends on the real print; inspect the stress report and refinement. Across-layer assessment currently assumes the part Z axis.`]
}

function walk(node, fn) {
  if (!node || typeof node !== 'object') return
  if (Array.isArray(node)) { node.forEach(n => walk(n, fn)); return }
  fn(node)
  for (const [key, value] of Object.entries(node)) if (!['token','frameToken'].includes(key)) walk(value, fn)
}

function shapeType(node, declarations, seen = new Set()) {
  if (!node) return 'Shape (unresolved)'
  if (node.type === 'shape') return node.dim === 2 ? 'Profile2D' : 'Solid3D'
  if (node.type === 'loft' || node.name === 'extrude' || node.name === 'Extrude') return 'Solid3D'
  if (node.type === 'variable_reference') {
    if (seen.has(node.name)) return 'Shape (cyclic reference)'
    seen.add(node.name)
    return shapeType(declarations.get(node.name)?.value, declarations, seen)
  }
  return shapeType(node.input || node.target || node.left, declarations, seen)
}

function numberRole(token, ast, text) {
  let role = 'Numeric literal. Units depend on its argument position.'
  walk(ast.body, n => {
    const params = n.params || []
    params.forEach((p, i) => {
      let contains = false
      walk(p, child => { if (child.position === token.position) contains = true })
      if (!contains) return
      const name = n.name || n.symbol || n.type
      if (/^(rotx|roty|rotz|Rx|Ry|Rz|θ|ϕ|ω)$/.test(name)) role = `${name}: rotation angle in degrees.`
      else if (['scale','Scale'].includes(name)) role = `${name}: dimensionless scale factor, component ${i+1}.`
      else if (name === 'grid') role = i < 2 ? `grid: positive integer ${i === 0 ? 'column' : 'row'} count.` : `grid: ${i === 2 ? 'X' : 'Y'} pitch in millimetres.`
      else if (name === 'mate') role = i === 0 ? 'mate: roll angle in degrees.' : 'mate: gap in millimetres.'
      else role = `${name}: numeric argument ${i+1}, usually a length in millimetres (counts and direction vectors are dimensionless).`
    })
    for (const p of n.offset || []) walk(p, c => { if (c.position === token.position) role = `${n.name}: local offset in millimetres.` })
  })
  const statement = ast.body.find(n => n.type === 'constraint_declaration' && n.line === token.line)
  if (statement?.name === 'fixed') {
    const values = new COSCADParser().tokenize(text).filter(t => t.line === token.line && t.type === 'number')
    const index = values.findIndex(t => t.position === token.position)
    role = index < 3 ? `fixed: ${['X','Y','Z'][index]} translation in millimetres.` : `fixed: ${['X','Y','Z'][index-3]} rotation in degrees.`
  }
  const analysis = ast.body.find(n => n.type === 'analysis_declaration' && n.line === token.line)
  if (analysis?.name === 'load') role = 'Applied force component in newtons, in world axes; components specify a total force, not pressure.'
  const prefix = (text.split(/\r?\n/)[token.line] || '').slice(0, token.column)
  if (/×\s*$/.test(prefix)) role = 'Integer quantity. Part counts are non-negative; ×0 marks a premade part. Fastener counts are positive.'
  const option = prefix.match(/\b(roll|gap|inset|through|torque|mass|beam|astar|at|profile|cell|safety)\s*=\s*[^=]*$/)?.[1]
  if (option && syntax[option]) role = syntax[option][0] + '. ' + syntax[option][1]
  return role
}

function hoverInfo(text, offset, options = {}) {
  const parser = options.parser || new COSCADParser()
  const tokenAt = options.tokens?.find(t => t.position <= offset && offset < t.position+String(t.raw ?? t.value).length)
  const found = options.tokens ? (tokenAt ? {token:tokenAt} : null) : parser.getSymbolAtPosition(text, offset)
  if (!found || ['comment','newline','unknown'].includes(found.token.type)) return null
  const token = found.token, value = String(token.raw ?? token.value)
  const ast = options.ast || parser.parse(text, {fileType:options.fileType || 'coscad'})
  const declarations = new Map(ast.body.filter(n => ['variable_declaration','part_declaration'].includes(n.type)).map(n => [n.name,n]))
  const declaration = declarations.get(token.value)
  const sourcePart = ast.body.find(n => n.type === 'part_declaration' && offset >= n.sourcePosition && offset < n.sourcePosition+n.source.length)
  let markdown
  if (sourcePart) markdown = `**Part source**: \`${sourcePart.source}\`\n\nSeparate physical part; use Go to Definition to open its source.`
  else if (token.type === 'number') markdown = `**Number** \`${value}\` = ${token.value}\n\n${numberRole(token, ast, text)}${Number.isFinite(token.value) ? '' : '\n\nInvalid: numeric values must be finite.'}`
  else if (token.type === 'assignment') {
    const line = text.split(/\r?\n/)[token.line] || ''
    const firstEqual = line.indexOf('=')
    const owner = ast.body.find(n => n.type === 'variable_declaration' && n.line === token.line)
    const key = line.slice(0,token.column).match(/([A-Za-z_]+)\s*$/)?.[1]
    const optionType = ['roll','gap','inset','through','torque','mass','beam','astar','at','profile'].includes(key) ? 'Number' : ['seat'].includes(key) ? 'MatingFrame' : ['after'].includes(key) ? 'Part instance list' : ['rest','pocket','ends'].includes(key) ? 'Anchor / enum list' : 'option value'
    const type = owner?.kind === 'number' ? 'Number' : owner ? shapeType(owner.value,declarations) : optionType
    markdown = `**= · ${owner && firstEqual === token.column ? 'Definition' : 'Option assignment'}**\n\n${syntax['='][1]}\n\nRight-hand side: **${type}**.`
  } else if (declaration) {
    if (declaration.type === 'part_declaration') markdown = `**Part** \`${value}\`\n\nSource: \`${declaration.source}\`\n\nPrint count: ${declaration.count}${declaration.orientation ? '; bed face: '+declaration.orientation : ''}. Assembled pose is independent of print orientation.`
    else if (declaration.kind === 'number') markdown = `**Number** \`${value}\` = ${Number.isFinite(declaration.numericValue) ? declaration.numericValue : '(unresolved)'}\n\nNumeric binding; usable in arithmetic and numeric argument positions.`
    else markdown = `**${shapeType(declaration.value,declarations)}** \`${value}\`\n\nShape binding defined on line ${declaration.line+1}.`
    const preceding = text.slice(0,declaration.position).split(/\r?\n/)
    if (preceding.at(-1)?.trim() === '') preceding.pop()
    const comments=[]
    while (preceding.at(-1)?.trim().startsWith('//')) comments.unshift(preceding.pop().trim().slice(2).trim())
    if (comments.length) markdown += '\n\n'+comments.join('\n')
  } else {
    const named = token.type === 'reference' ? parser.tokenize(text).find(t => t.position === token.position+1) : text[token.position-1] === '@' ? token : null
    const frames=[]
    walk(ast.body,n => { if (n.frameName === (named?.value || token.value)) frames.push(n) })
    if (named && !frames.length && options.loadPart) {
      const constraint = ast.body.find(n => n.type === 'constraint_declaration' && n.name === 'mate' && n.line === token.line)
      const alias = parser.framePartAt(text, named.position + String(named.value).length) || constraint?.refs[offset < constraint.refs[1].token.position ? 0 : 1]?.text
      const part = declarations.get(alias)
      const loaded = part && options.loadPart(part)
      if (loaded) walk(loaded.ast.body, n => { if (n.frameName === named.value) frames.push({...n,source:part.source}) })
    }
    if (named || frames.length) {
      const name = named?.value || token.value
      const info = frames[0]
      markdown = `**${info?.name === 'datum' ? 'Point datum' : 'Mating frame'}** \`@${name}\`\n\n` + (info && frames.length === 1 ? `Defined ${info.source ? 'in `'+info.source+'` ' : ''}on line ${info.line+1}: origin at \`${info.anchor}\`${info.normal ? `, normal \`${info.normal}\`, up \`${info.up}\`` : ''}. Metadata follows geometry transforms.` : 'Named construction reference on the selected part. Mate requires an oriented frame; point-only datums cannot supply a rotation. Frame existence and loop consistency are checked by the compiler.' + (frames.length > 1 ? ` There are ${frames.length} candidate definitions; selection depends on the shape.` : ''))
    } else if (syntax[token.value]) markdown = `**${syntax[token.value][0]}** \`${value}\`\n\n${syntax[token.value][1]}`
    else if (options.getDocumentation?.(token.value)) {
      const d=options.getDocumentation(token.value)
      markdown=`**${d.name}** \`${value}\`\n\n${d.description}\n\nUsage: \`${d.usage}\`\n\nExample: \`${d.example}\``
    } else if (token.type === 'anchor') markdown = `**Anchor / direction** \`${value}\`\n\nSelects a bounding-box point or a normal/up direction. Combine directions with +; ctr is the box centre and has no direction.`
    else if (token.type === 'pragma') markdown = `**Syntax mode** \`${value}\`\n\nSelects glyph or simple syntax for this file; common pipeline stages work in both.`
    else if (/^M\d+(?:\.\d+)?x\d+(?:\.\d+)?$/i.test(value)) markdown = `**Screw specification** \`${value}\`\n\nMetric thread diameter × screw length, in millimetres.`
    else markdown = `**${token.type === 'identifier' ? 'Symbol / option value' : token.type}** \`${value}\`\n\n${token.type === 'identifier' ? 'Resolved according to the surrounding declaration. Undeclared shape and numeric references are diagnosed; custom part hints remain metadata.' : 'CoScad syntax token.'}`
  }
  return {contents:{kind:'markdown',value:markdown},range:{start:{line:token.line,character:token.column},end:{line:token.line,character:token.column+value.length}}}
}

module.exports = {hoverInfo,shapeType,walk,syntax}
