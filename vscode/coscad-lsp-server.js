// coscad-lsp-server.js
const {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  DidChangeConfigurationNotification,
  TextDocumentSyncKind,
  SymbolKind,
  MarkupKind,
  Location,
  Range,
  TextEdit,
  DiagnosticSeverity,
  SemanticTokensBuilder,
  SemanticTokenTypes,
  SemanticTokenModifiers
} = require('vscode-languageserver/node')

const { TextDocument } = require('vscode-languageserver-textdocument')
const { COSCADParser } = require('./coscad-parser')
const { hoverInfo, walk, syntax: syntaxDocs } = require('./hover-info')
const fs = require('fs')
const path = require('path')
const { fileURLToPath, pathToFileURL } = require('url')

const connection = createConnection(ProposedFeatures.all)
const documents = new TextDocuments(TextDocument)
const parser = new COSCADParser()

let hasConfigurationCapability = false
let hasWorkspaceFolderCapability = false
let hasDiagnosticRelatedInformationCapability = false

// Documentation for COSCAD symbols (kept in step with coscad --help)
const documentation = {
  shapes3d: {
    '■': { name: 'cube', description: 'Cube (OpenSCAD, corner at origin)', usage: '■ size', example: '■ 10 // 10x10x10 cube' },
    '●': { name: 'sphere', description: 'Sphere centered at the origin', usage: '● radius', example: '● 5' },
    '◎': { name: 'cylinder', description: 'Cylinder (OpenSCAD, z from 0 to h)', usage: '◎ radius height', example: '◎ 5 10' },
    '▻': { name: 'cone', description: 'Cone tapering to a point', usage: '▻ radius height', example: '▻ 8 15' },
    '▬': { name: 'box', description: 'Rectangular box (OpenSCAD, corner at origin)', usage: '▬ x y z', example: '▬ 10 20 5' },
    '⎏': { name: 'prism', description: 'Regular n-sided prism', usage: '⎏ sides radius height', example: '⎏ 6 8 12 // hexagonal prism' },
    cube: { name: 'cube', description: 'Centered cube (BOSL2 cuboid)', usage: 'cube size', example: 'cube 10' },
    box: { name: 'box', description: 'Centered box (BOSL2 cuboid)', usage: 'box x y z', example: 'box 40 40 8' },
    sphere: { name: 'sphere', description: 'Sphere centered at the origin', usage: 'sphere r', example: 'sphere 5' },
    cyl: { name: 'cyl', description: 'Centered cylinder (BOSL2 cyl)', usage: 'cyl r h', example: 'cyl 6 20' },
    tube: { name: 'tube', description: 'Centered tube (BOSL2)', usage: 'tube outer_r inner_r h', example: 'tube 10 6 25' },
    torus: { name: 'torus', description: 'Torus (BOSL2)', usage: 'torus r_major r_minor', example: 'torus 20 5' },
    wedge: { name: 'wedge', description: 'Right-triangular prism (BOSL2; profile in the YZ plane)', usage: 'wedge x y z', example: 'wedge 15 15 14' }
  },
  shapes2d: {
    '△': { name: 'triangle', description: 'Equilateral triangle profile (2D; extrude or loft it)', usage: '△ radius', example: '⮕ 10 (△ 8)' },
    '⬠': { name: 'pentagon', description: 'Regular pentagon profile (2D)', usage: '⬠ radius', example: '⮕ 5 (⬠ 8)' },
    '⭘': { name: 'circle', description: 'Circle profile (2D, 100 segments)', usage: '⭘ radius', example: 'loft 0 (⭘ 10) 30 (⭘ 5)' },
    '✎': { name: 'bezier', description: 'Closed piecewise-cubic bezier outline (2D): 3k+1 control points as x y pairs, evaluated by the compiler', usage: '✎ x0 y0 x1 y1 x2 y2 x3 y3 ...', example: '✎ 0 0 10 5 20 -5 30 0 ... |> extrude 4' }
  },
  transformations: {
    'χ': { name: 'Tx', description: 'Translate along X', usage: 'χ distance shape', example: 'χ 5 (● 10)' },
    'ψ': { name: 'Ty', description: 'Translate along Y', usage: 'ψ distance shape', example: 'ψ 3 (■ 10)' },
    'ζ': { name: 'Tz', description: 'Translate along Z', usage: 'ζ distance shape', example: 'ζ 2 (◎ 5 10)' },
    'θ': { name: 'Rx', description: 'Rotate about X (degrees, about the origin)', usage: 'θ angle shape', example: 'θ 45 (■ 10)' },
    'ϕ': { name: 'Ry', description: 'Rotate about Y (degrees, about the origin)', usage: 'ϕ angle shape', example: 'ϕ 90 (◎ 5 10)' },
    'ω': { name: 'Rz', description: 'Rotate about Z (degrees, about the origin)', usage: 'ω angle shape', example: 'ω 30 (▻ 8 15)' },
    '⬈': { name: 'Scale', description: 'Non-uniform scale', usage: '⬈ sx sy sz shape', example: '⬈ 2 1.5 0.5 (● 5)' },
    '⇋': { name: 'Mirror', description: 'Mirror across the plane through the origin with this normal', usage: '⇋ nx ny nz shape', example: '⇋ 1 0 0 (● 5)' },
    '⮕': { name: 'Extrude', description: 'Linear extrusion of a 2D profile (2D → 3D)', usage: '⮕ height profile', example: '⮕ 10 (⭘ 5)' },
    '⟰': { name: 'Loft', description: 'Loft: skin 2D profiles at ascending z into one solid (BOSL2 skin; profiles may differ in vertex count)', usage: '⟰ z0 profile0 z1 profile1 [z2 profile2 ...]', example: '⟰ 0 (⭘ 10) 30 (△ 5)' },
    loft: { name: 'loft', description: 'Loft: skin 2D profiles at ascending z into one solid. Profiles must be single closed outlines (△ ⬠ ⭘ ✎ and their in-plane transforms/offset), no booleans, no ζ/θ/ϕ', usage: 'loft z0 profile0 z1 profile1 [...]', example: 'loft 0 (⭘ 10) 30 (⭘ 5)' },
    Loft: { name: 'Loft', description: 'Loft (simple mode): skin 2D profiles at ascending z into one solid', usage: 'Loft z0 profile0 z1 profile1 [...]', example: 'Loft 0 (Circle 10) 30 (Triangle 5)' },
    '⚓': { name: 'Anchor', description: 'Re-origin a shape at one of its own bounding-box anchors', usage: '⚓ anchor shape', example: '⚓ bot (cyl 5 20) // bottom face at z = 0' }
  },
  shapes3dBosl2: {
    '▣': { name: 'cuboid', description: 'BOSL2 chamfered cuboid (centered)', usage: '▣ x y z chamfer', example: '▣ 20 15 10 2' },
    '◙': { name: 'cuboid', description: 'BOSL2 rounded cuboid (centered; rounding must be < half the smallest dimension)', usage: '◙ x y z rounding', example: '◙ 20 15 10 2' },
    '⌭': { name: 'cyl', description: 'BOSL2 cylinder with chamfered ends (centered)', usage: '⌭ r h chamfer', example: '⌭ 5 20 1' },
    '⌽': { name: 'cyl', description: 'BOSL2 cylinder with rounded ends (centered)', usage: '⌽ r h rounding', example: '⌽ 5 20 2' },
    xcyl: { name: 'xcyl', description: 'BOSL2 cylinder along the X axis (centered)', usage: 'xcyl r l', example: 'xcyl 2.7 12 // M5 clearance hole' },
    ycyl: { name: 'ycyl', description: 'BOSL2 cylinder along the Y axis (centered)', usage: 'ycyl r l', example: 'ycyl 5 20' },
    zcyl: { name: 'zcyl', description: 'BOSL2 cylinder along the Z axis (centered)', usage: 'zcyl r l', example: 'zcyl 2.15 30' },
    '⊚': { name: 'tube', description: 'BOSL2 tube (centered)', usage: '⊚ outer_r inner_r h', example: '⊚ 10 6 25' },
    '⏢': { name: 'prismoid', description: 'BOSL2 prismoid: rectangle-to-rectangle taper (centered)', usage: '⏢ x1 y1 x2 y2 h', example: '⏢ 20 20 10 10 15' },
    '◉': { name: 'torus', description: 'BOSL2 torus', usage: '◉ r_major r_minor', example: '◉ 20 5' },
    '⊿': { name: 'wedge', description: 'BOSL2 wedge: right-triangular prism, profile in the YZ plane, right angle at −Y −Z (rotate/mirror to reorient)', usage: '⊿ x y z', example: '⊿ 15 15 14.2 |> rotz -90' }
  },
  booleanOps: {
    '⊖': { name: 'Diff', description: 'Difference: left minus right', usage: 'a ⊖ b', example: '● 15 ⊖ ◎ 5 10' },
    '⊝': { name: 'Diff', description: 'Difference (alias of ⊖)', usage: 'a ⊝ b', example: '■ 10 ⊝ ● 5' },
    '⊕': { name: 'Union', description: 'Union', usage: 'a ⊕ b', example: '■ 10 ⊕ ● 5' },
    '⊛': { name: 'Union', description: 'Union (alias of ⊕)', usage: 'a ⊛ b', example: '■ 10 ⊛ ● 5' },
    '*': { name: 'Union', description: 'Union (simple mode)', usage: 'a * b', example: 'Box 10 10 10 * Sphere 6' },
    '-': { name: 'Diff', description: 'Difference (simple mode)', usage: 'a - b', example: 'Box 10 10 10 - Sphere 6' }
  },
  advancedOps: {
    '∩': { name: 'Intersection', description: 'Intersection', usage: 'a ∩ b', example: '■ 10 ∩ ● 6' },
    '⇓': { name: 'Hull', description: 'Convex hull of both operands', usage: 'a ⇓ b', example: '■ 10 ⇓ ● 5' },
    '⊞': { name: 'Minkowski', description: 'Minkowski sum', usage: 'a ⊞ b', example: '■ 10 ⊞ ● 1 // rounded cube' },
    '↯': { name: 'Offset', description: 'Offset a 2D profile by the right operand’s radius (sphere, cylinder, or 2D profile; anything else defaults to 1)', usage: 'profile ↯ ● r', example: '△ 8 ↯ ● 2' }
  },
  attachment: {
    '⌖': { name: 'Position', description: 'Position: translate the child so its opposite anchor meets the parent’s anchor (union)', usage: 'parent ⌖ anchor child', example: 'base ⌖ top post' },
    '⋈': { name: 'AttachTo', description: 'Attach: rotate the child’s +Z along the anchor direction and mate its bottom there (union)', usage: 'parent ⋈ anchor child', example: 'plate ⋈ rt (cyl 3 10)' }
  },
  pipeline: {
    '|>': { name: 'pipe', description: 'Pipeline: apply a stage to the shape so far; a line starting with |> continues the previous definition', usage: 'shape |> stage args', example: 'plate |> at top flange |> cutat top 0 0 -2 (zcyl 1.4 10)' },
    x: { name: 'x', description: 'Translate along X', usage: '|> x d', example: 'box 10 10 10 |> x 5' },
    y: { name: 'y', description: 'Translate along Y', usage: '|> y d', example: 'box 10 10 10 |> y 5' },
    z: { name: 'z', description: 'Translate along Z', usage: '|> z d', example: 'box 10 10 10 |> z 5' },
    move: { name: 'move', description: 'Translate by a vector', usage: '|> move dx dy dz', example: 'arch |> move 55 -13 -7' },
    rotx: { name: 'rotx', description: 'Rotate about X (degrees)', usage: '|> rotx deg', example: 'wedge 15 15 14 |> rotx 90' },
    roty: { name: 'roty', description: 'Rotate about Y (degrees)', usage: '|> roty deg', example: 'part |> roty 180' },
    rotz: { name: 'rotz', description: 'Rotate about Z (degrees)', usage: '|> rotz deg', example: 'gusset |> rotz -90' },
    scale: { name: 'scale', description: 'Non-uniform scale', usage: '|> scale sx sy sz', example: 'ball |> scale 2 1 0.5' },
    mirror: { name: 'mirror', description: 'Mirror across the plane with this normal', usage: '|> mirror nx ny nz', example: 'arch |> mirror 1 0 0' },
    extrude: { name: 'extrude', description: 'Linear extrusion of a 2D profile', usage: '|> extrude h', example: 'limb2d |> extrude 14' },
    loft: { name: 'loft', description: 'Loft stage: start a loft from the 2D shape so far (at z = 0), or extend an existing loft with another profile', usage: '|> loft z profile', example: '⭘ 10 |> loft 20 (△ 5) |> loft 35 (⬠ 3)' },
    anchor: { name: 'anchor', description: 'Re-origin at one of the shape’s own anchors', usage: '|> anchor v', example: 'cyl 5 20 |> anchor bot' },
    at: { name: 'at', description: 'Position: union the child with its opposite anchor mated to this anchor, plus an optional offset', usage: '|> at anchor [dx dy dz] child', example: 'plate |> at top 5.5 0 -5 flange' },
    on: { name: 'on', description: 'Attach: rotate the child’s +Z along the anchor and mate its bottom there, plus an optional offset', usage: '|> on anchor [dx dy dz] child', example: 'body |> on rt (cyl 3 10)' },
    cutat: { name: 'cutat', description: 'Subtract the cutter centered at this anchor, plus an optional offset (note: centered, unlike at/on)', usage: '|> cutat anchor [dx dy dz] cutter', example: 'plate |> cutat top 0 0 -2 (zcyl 2.15 30)' },
    add: { name: 'add', description: 'Union', usage: '|> add shape', example: 'plate |> add flange' },
    cut: { name: 'cut', description: 'Difference', usage: '|> cut shape', example: 'plate |> cut (zcyl 2 30)' },
    isect: { name: 'isect', description: 'Intersection', usage: '|> isect shape', example: 'blank |> isect (sphere 20)' },
    hull: { name: 'hull', description: 'Convex hull with another shape', usage: '|> hull shape', example: 'a |> hull b' },
    mink: { name: 'mink', description: 'Minkowski sum with another shape', usage: '|> mink shape', example: 'box 10 10 10 |> mink (sphere 1)' }
  }
}

const documentationCategories = [
  documentation.shapes3d,
  documentation.shapes2d,
  documentation.shapes3dBosl2,
  documentation.transformations,
  documentation.booleanOps,
  documentation.advancedOps,
  documentation.attachment,
  documentation.pipeline
]

function getDocumentation(symbol) {
  if (syntaxDocs[symbol]) return {name:syntaxDocs[symbol][0],description:syntaxDocs[symbol][1],usage:syntaxDocs[symbol][1].split('. ')[0],example:syntaxDocs[symbol][1].split('. ')[0]}
  for (const category of documentationCategories) {
    if (category[symbol]) return category[symbol]
  }
  const info = parser.allSymbols[symbol]
  if (!info) return null
  return {
    name: info.name,
    description: `Built-in ${info.name} ${parser.getSymbolType(symbol).replace('_', ' ')}`,
    usage: symbol,
    example: symbol
  }
}

function getFileType(uri) {
  return path.extname(uri).toLowerCase() === '.assemble' ? 'assemble' : 'coscad'
}

// Semantic token types and modifiers
const tokenTypes = new Map()
const tokenModifiers = new Map()

const legend = {
  tokenTypes: ['function', 'keyword', 'number', 'operator', 'comment', 'variable'],
  tokenModifiers: ['declaration', 'definition', 'readonly', 'static', 'deprecated', 'abstract', 'async', 'modification', 'documentation', 'defaultLibrary']
}

// Initialize token type and modifier maps
legend.tokenTypes.forEach((tokenType, index) => tokenTypes.set(tokenType, index))
legend.tokenModifiers.forEach((tokenModifier, index) => tokenModifiers.set(tokenModifier, index))

connection.onInitialize((params) => {
  const capabilities = params.capabilities

  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  )
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  )
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  )

  const result = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      documentSymbolProvider: true,
      definitionProvider: true,
      semanticTokensProvider: {
        legend,
        range: true,
        full: {
          delta: false
        }
      },
      completionProvider: {
        resolveProvider: false, // Changed to false since we provide full completion info upfront
        triggerCharacters: ['■', '●', '◎', '▻', '▬', '⎏', '△', '⬠', '⭘', '✎', 'χ', 'ψ', 'ζ', 'θ', 'ϕ', 'ω', '⬈', '⇋', '⮕', '⚓', '⊖', '⊝', '⊛', '⊕', '∩', '⇓', '⊞', '↯', '⌖', '⋈', '▣', '◙', '⌭', '⌽', '⊚', '⏢', '◉', '⊿', '⟰', '|', '@']
      }
    }
  }

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true
      }
    }
  }

  return result
})

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    connection.client.register(DidChangeConfigurationNotification.type, undefined)
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders(_event => {
      connection.console.log('Workspace folder change event received.')
    })
  }
})

// Handle $/setTrace notification to avoid error messages
connection.onNotification('$/setTrace', (params) => {
  // This is a standard LSP notification for setting trace levels
  // We can safely ignore it or log it if needed
  connection.console.info('Trace level set to: ' + params.value)
})

// Handle additional LSP notifications to avoid errors
connection.onNotification('$/cancelRequest', () => {
  // Handle cancel request notification
})

connection.onNotification('telemetry/event', () => {
  // Handle telemetry events
})

// Contextual token hover, including literals, punctuation and interfaces.
const hoverCache = new Map()
function loadPart(document, part) {
  if (!document.uri.startsWith('file:')) return null
  const file = path.resolve(path.dirname(fileURLToPath(document.uri)),part.source)
  try {
    const text = documents.get(pathToFileURL(file).href)?.getText() ?? fs.readFileSync(file,'utf8')
    return {file,ast:parser.parse(text,{fileType:getFileType(file)})}
  } catch { return null }
}
connection.onHover(params => {
  const document = documents.get(params.textDocument.uri)
  if (!document) return null
  const text=document.getText(),fileType=getFileType(document.uri)
  let cached=hoverCache.get(document.uri)
  if (!cached || cached.version !== document.version) {
    cached={version:document.version,ast:parser.parse(text,{fileType}),tokens:parser.tokenize(text)}
    hoverCache.set(document.uri,cached)
  }
  return hoverInfo(text, document.offsetAt(params.position), {
    parser, fileType, getDocumentation, ...cached, loadPart:part=>loadPart(document,part)
  })
})

documents.onDidClose(event => hoverCache.delete(event.document.uri))

// Semantic tokens: one classifier for full and range requests. Numeric
// bindings (`w = 20`) are 'variable' + readonly so themes can set them
// apart from shape names.
function classifyToken(token, numericNames) {
  switch (token.type) {
    case 'shape3d':
    case 'shape2d':
      return ['function', ['declaration']]
    case 'transformation':
    case 'boolean':
    case 'advanced':
    case 'composition':
    case 'attachment':
    case 'pipeline_stage':
    case 'anchor':
    case 'pragma':
      return ['keyword', []]
    case 'number':
      return ['number', []]
    case 'comment':
      return ['comment', []]
    case 'identifier':
      return numericNames.has(token.value) ? ['variable', ['readonly']] : ['function', ['declaration']]
    default:
      return ['operator', []]
  }
}

function buildSemanticTokens(document, range) {
  const text = document.getText()
  const numericNames = parser.parse(text, { fileType: getFileType(document.uri) }).numericNames
  const builder = new SemanticTokensBuilder()
  const startOffset = range ? document.offsetAt(range.start) : 0
  const endOffset = range ? document.offsetAt(range.end) : Infinity
  for (const token of parser.tokenize(text)) {
    if (token.type === 'newline' || token.position < startOffset || token.position > endOffset) continue
    const [tokenType, tokenMods] = classifyToken(token, numericNames)
    const length = String(token.raw ?? token.value).length
    const tokenTypeIndex = tokenTypes.get(tokenType) || 0
    const tokenModifiersIndex = tokenMods.reduce((acc, mod) => acc | (1 << (tokenModifiers.get(mod) || 0)), 0)
    builder.push(token.line, token.column, length, tokenTypeIndex, tokenModifiersIndex)
  }
  return builder.build()
}

connection.onRequest('textDocument/semanticTokens/full', (params) => {
  const document = documents.get(params.textDocument.uri)
  return document ? buildSemanticTokens(document) : { data: [] }
})

connection.onRequest('textDocument/semanticTokens/range', (params) => {
  const document = documents.get(params.textDocument.uri)
  return document ? buildSemanticTokens(document, params.range) : { data: [] }
})

// Completion provider
connection.onCompletion((params) => {
  const document = documents.get(params.textDocument.uri)
  const completionItems = []

  // Add variables from the current document
  if (document) {
    const text = document.getText()
    const ast = parser.parse(text, { fileType: getFileType(params.textDocument.uri) })

    const offset = document.offsetAt(params.position)
    const prefix = text.slice(0, offset).match(/@([A-Za-z0-9_]*)$/)
    const alias = prefix && parser.framePartAt(text, offset)
    const part = alias && ast.body.find(n => n.type === 'part_declaration' && n.name === alias)
    const frames = part ? loadPart(document, part)?.ast.body : ast.body
    const seenFrames = new Set()
    walk(frames || [], node => {
      if (!node.frameName || seenFrames.has(node.frameName)) return
      seenFrames.add(node.frameName)
      const item = {label:'@'+node.frameName,kind:5,detail:node.name === 'datum' ? 'Construction point' : 'Oriented mating frame'}
      if (prefix) item.textEdit = TextEdit.replace(Range.create(document.positionAt(offset-prefix[0].length),params.position),'@'+node.frameName)
      else item.insertText = '@'+node.frameName
      completionItems.push(item)
    })
    const variables = ast.body.filter(node => ['variable_declaration', 'part_declaration'].includes(node.type))
    for (const variable of variables) {
      const isNumber = variable.kind === 'number'
      completionItems.push({
        label: variable.name,
        kind: isNumber ? 21 : 6, // Constant : Variable
        detail: variable.type === 'part_declaration'
          ? `Part from ${variable.source}`
          : isNumber ? `Number = ${Number.isFinite(variable.numericValue) ? variable.numericValue : '?'}` : describeValue(variable.value),
        documentation: {
          kind: MarkupKind.Markdown,
          value: isNumber ? `**Number:** \`${variable.name}\`` : `**Variable:** \`${variable.name}\``
        },
        insertText: variable.name
      })
    }
  }

  // Add all COSCAD symbols as completion items
  for (const [symbol, info] of Object.entries(parser.allSymbols)) {
    const docInfo = getDocumentation(symbol)

    if (docInfo) {
      completionItems.push({
        label: `${symbol} (${info.name})`,
        kind: 3, // Function
        detail: docInfo.description,
        documentation: {
          kind: MarkupKind.Markdown,
          value: `**Usage:** \`${docInfo.usage}\`\n\n**Example:** \`${docInfo.example}\``
        },
        insertText: symbol
      })
    }
  }

  for (const keyword of ['!glyph', '!simple', 'plate', 'clearance', 'fastener', 'plan', 'fixed', 'mate', '@', '←', '×', '▽', 'loft']) {
    completionItems.push({
      label: keyword,
      kind: 14,
      detail: keyword.startsWith('!') ? 'Syntax mode pragma' : 'Assembly syntax',
      insertText: keyword
    })
  }

  return completionItems
})

// Go to definition provider
connection.onDefinition((params) => {
  const document = documents.get(params.textDocument.uri)
  if (!document) {
    return null
  }

  const position = params.position
  const offset = document.offsetAt(position)
  const text = document.getText()

  const ast = parser.parse(text, { fileType: getFileType(params.textDocument.uri) })
  const selected=parser.getSymbolAtPosition(text,offset)?.token
  if (selected && (selected.type==='reference' || text[selected.position-1]==='@')) {
    const tokens=parser.tokenize(text),named=selected.type==='reference'?tokens.find(t=>t.position===selected.position+1):selected
    let location=null,candidates=[]
    const constraint=ast.body.find(n=>n.type==='constraint_declaration' && n.name==='mate' && n.line===selected.line)
    const frameAlias = parser.framePartAt(text, named?.position + String(named?.value || '').length)
    if (constraint || frameAlias) {
      const alias=frameAlias || constraint.refs[offset<constraint.refs[1].token.position?0:1].text
      const part=ast.body.find(n=>n.type==='part_declaration' && n.name===alias)
      const loaded=part && loadPart(document,part)
      if (loaded) walk(loaded.ast.body,n=>{if(n.frameName===named?.value)candidates.push({node:n,uri:pathToFileURL(loaded.file).href})})
    } else walk(ast.body,n=>{if(n.frameName===named?.value)candidates.push({node:n,uri:document.uri})})
    if(candidates.length===1){const {node,uri}=candidates[0],t=node.frameToken;location=Location.create(uri,Range.create(t.line,t.column,t.line,t.column+String(t.value).length))}
    return location
  }
  const sourcePart = ast.body.find(node =>
    node.type === 'part_declaration' && offset >= node.sourcePosition && offset < node.sourcePosition + node.source.length
  )
  if (sourcePart && params.textDocument.uri.startsWith('file:')) {
    const target = path.resolve(path.dirname(fileURLToPath(params.textDocument.uri)), sourcePart.source)
    if (fs.existsSync(target)) {
      return Location.create(pathToFileURL(target).href, Range.create(0, 0, 0, 0))
    }
  }

  // Get symbol at current position
  const symbolInfo = parser.getSymbolAtPosition(text, offset)
  if (!symbolInfo || !['identifier', 'anchor', 'pipeline_stage'].includes(symbolInfo.token.type)) {
    return null
  }

  const variableName = symbolInfo.token.value

  // Parse the document to find variable declarations
  const variable = ast.body.find(node =>
    ['variable_declaration', 'part_declaration'].includes(node.type) && node.name === variableName
  )

  if (!variable) {
    return null
  }

  // Create location for the variable declaration
  const startPos = document.positionAt(variable.position || 0)
  const endPos = document.positionAt((variable.position || 0) + variable.name.length)
  const range = Range.create(startPos, endPos)

  return {
    uri: params.textDocument.uri,
    range: range
  }
})

// Document change handler for diagnostics
documents.onDidChangeContent(change => {
  validateTextDocument(change.document)
})

async function validateTextDocument(textDocument) {
  const text = textDocument.getText()
  const fileType = getFileType(textDocument.uri)
  const diagnostics = parser.getDiagnostics(text, { fileType })

  if (fileType === 'assemble' && textDocument.uri.startsWith('file:')) {
    const documentPath = fileURLToPath(textDocument.uri)
    const ast = parser.parse(text, { fileType })
    for (const part of ast.body.filter(node => node.type === 'part_declaration')) {
      const target = path.resolve(path.dirname(documentPath), part.source)
      if (!fs.existsSync(target)) {
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: Range.create(part.sourceLine, part.sourceColumn, part.sourceLine, part.sourceColumn + part.source.length),
          message: `Part file not found: ${part.source}`,
          source: 'coscad'
        })
      } else if (target === documentPath) {
        diagnostics.push({
          severity: DiagnosticSeverity.Error,
          range: Range.create(part.sourceLine, part.sourceColumn, part.sourceLine, part.sourceColumn + part.source.length),
          message: 'Assembly cannot reference itself',
          source: 'coscad'
        })
      }
    }
  }

  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics })
}

function describeValue(value) {
  if (!value) return 'Unknown'
  switch (value.type) {
    case 'shape': return `${value.name} ${value.dim === 2 ? 'profile (2D)' : 'shape'}`
    case 'variable_reference': return `Reference to ${value.name}`
    case 'boolean_operation': return `${value.operator} operation`
    case 'attachment_operation': return `${value.operator} attachment`
    case 'transformation': return `${value.name} transformation`
    case 'loft': return `Loft of ${value.profiles.length} profiles`
    case 'pipeline_operation': {
      const stages = []
      for (let node = value; node && node.type === 'pipeline_operation'; node = node.input) stages.unshift(node.name)
      return `Pipeline: ${stages.join(' |> ')}`
    }
    default: return value.type
  }
}

// Helper function to extract documentation from comments above a variable
function extractVariableDocumentation(text, variablePosition) {
  const lines = text.split('\n')
  let variableLine = 0
  let currentPos = 0

  // Find the line where the variable is declared
  for (let i = 0; i < lines.length; i++) {
    if (currentPos >= variablePosition) {
      variableLine = i
      break
    }
    currentPos += lines[i].length + 1 // +1 for newline
  }

  const commentLines = []
  let i = variableLine - 1

  // Look backwards for consecutive comment lines
  while (i >= 0 && lines[i].trim().startsWith('//')) {
    const commentText = lines[i].trim().substring(2).trim()
    if (commentText.length > 0) {
      commentLines.unshift(commentText) // Add to beginning to maintain order
    }
    i--
  }

  return commentLines.length > 0 ? commentLines.join(' ') : null
}

// Document symbol provider
connection.onDocumentSymbol((params) => {
  const document = documents.get(params.textDocument.uri)
  if (!document) {
    return []
  }

  const text = document.getText()
  const ast = parser.parse(text, { fileType: getFileType(params.textDocument.uri) })
  const symbols = []

  for (const node of ast.body) {
    if (node.type === 'variable_declaration' || node.type === 'part_declaration') {
      // Extract documentation for the variable
      const documentation = extractVariableDocumentation(text, node.position || 0)

      let detail = 'Variable'
      let symbolKind = SymbolKind.Variable

      if (node.type === 'part_declaration') {
        detail = `Part ×${node.count} from ${node.source}`
        symbolKind = SymbolKind.Object
      }

      if (node.kind === 'number') {
        detail = `= ${Number.isFinite(node.numericValue) ? node.numericValue : '?'}`
        symbolKind = SymbolKind.Constant
      } else if (node.value) {
        detail = describeValue(node.value)
        symbolKind = node.value.type === 'shape' ? SymbolKind.Object
          : node.value.type === 'variable_reference' ? SymbolKind.Variable
          : node.value.type === 'transformation' ? SymbolKind.Method
          : SymbolKind.Function
      }

      // Calculate proper range for the variable declaration
      const startPos = document.positionAt(node.position || 0)
      let endPos = document.positionAt((node.position || 0) + node.name.length)

      // Try to include the entire line for better selection
      const line = document.getText().split('\n')[startPos.line]
      if (line) {
        endPos = { line: startPos.line, character: line.length }
      }

      const range = Range.create(startPos, endPos)
      const selectionRange = Range.create(
        startPos,
        document.positionAt((node.position || 0) + node.name.length)
      )

      symbols.push({
        name: node.name,
        detail: detail,
        kind: symbolKind,
        range: range,
        selectionRange: selectionRange
      })
    }
  }

  return symbols
})

// Make the text document manager listen on the connection
documents.listen(connection)

// Listen on the connection
connection.listen()

connection.console.info('COSCAD Language Server started.')
