const assert = require('assert/strict')
const fs = require('fs'), path = require('path')
const { collectShapeSymbols, createShapeVisuals, inlineIconSize, sizeInlineSvg, catalog, MAX_SOURCE_LENGTH, MAX_SYMBOLS } = require('./shape-visuals')

for (const fontSize of [8, 12, 13.5, 24, 40]) {
  const pixels = inlineIconSize({get: (key, fallback) => key === 'fontSize' ? fontSize : fallback})
  assert(pixels <= fontSize)
  const sized = sizeInlineSvg('<svg width="128" height="128" viewBox="0 0 128 128"></svg>', pixels)
  assert(sized.includes(`width="${pixels}"`) && sized.includes(`height="${pixels}"`))
  assert(sized.includes('viewBox="0 0 128 128"'))
}
assert.equal(inlineIconSize({get: key => ({fontSize: 20, lineHeight: 12})[key]}), 12)
assert.equal(inlineIconSize({get: key => ({fontSize: 20, lineHeight: .8})[key]}), 16)

const source = '// xcyl 9 20\r\n// 🧭 Unicode and CRLF\r\nr = 3\r\nside = xcyl r 12 |> add (ycyl 2 9)\r\nmain = side |> add (zcyl 1 8)\r\n'
const found = collectShapeSymbols(source)
assert.deepEqual(found.map(s => s.icon.id), ['xcyl', 'ycyl', 'zcyl'])
for (const symbol of found) assert.equal(source.slice(symbol.offset, symbol.offset + symbol.length), symbol.symbol)
assert.equal(found[0].line, 3)
assert.deepEqual(collectShapeSymbols('main = xcyl2\n'), [])
assert.deepEqual(collectShapeSymbols('part ← xcyl.coscad ×1\nasm = part\n', 'assemble'), [])
assert.deepEqual(collectShapeSymbols('!simple\nmain = ● 3\n'), [], 'mode-invalid glyph is not a shape illustration')
assert.deepEqual(collectShapeSymbols('main = xcyl\n'), [], 'incomplete expression clears its old symbol')
assert.equal(collectShapeSymbols('!simple\nmain = Cylinder 3 8\n')[0].icon.id, 'zcyl')
assert.equal(collectShapeSymbols('!glyph\nmain = ⊚ 3 2 8\n')[0].icon.id, 'tube')
assert.deepEqual(collectShapeSymbols(' '.repeat(MAX_SOURCE_LENGTH + 1)), [])
assert.equal(collectShapeSymbols(Array.from({length: MAX_SYMBOLS + 2}, (_, i) => `s${i} = sphere 2\n`).join('')).length, MAX_SYMBOLS)
assert.equal(new Set(catalog.flatMap(s => s.aliases)).size, catalog.flatMap(s => s.aliases).length)
for (const icon of catalog) {
  const svg = fs.readFileSync(path.join(__dirname, 'assets/shapes', icon.id + '.svg'), 'utf8')
  assert(svg.includes('<polygon') && svg.includes('<title>') && svg.includes('viewBox="0 0 128 128"'))
  assert(!/<script|onload=|href=|<foreignObject/i.test(svg), 'icons have no active/external content')
  assert.equal(collectShapeSymbols('main = ' + icon.source + '\n')[0].icon.id, icon.id)
}
assert.notEqual(fs.readFileSync(path.join(__dirname, 'assets/shapes/xcyl.svg'), 'utf8'), fs.readFileSync(path.join(__dirname, 'assets/shapes/ycyl.svg'), 'utf8'))

const callbacks = {}, types = [], applied = new Map(), commands = new Map()
const settings = {shapeVisuals: 'inline', shapeHoverPreviews: true}
const editorSettings = {fontSize: 14, lineHeight: 0}
const listen = name => callback => {
  (callbacks[name] ??= new Set()).add(callback)
  return {dispose: () => callbacks[name].delete(callback)}
}
const emit = (name, value) => callbacks[name]?.forEach(callback => callback(value))
let text = 'main = xcyl 3 12 |> add (sphere 2)\n'
const document = {languageId: 'coscad', fileName: '/project/part.coscad', version: 1,
  uri: {toString: () => 'file:///project/part.coscad'}, getText: () => text, offsetAt: position => position.character}
const editor = {document, setDecorations: (type, ranges) => applied.set(type, ranges)}
const vscode = {
  Range: class {constructor(l, c, el, ec) {this.start = {line: l, character: c}; this.end = {line: el, character: ec}}},
  Hover: class {constructor(contents, range) {this.contents = contents; this.range = range}},
  MarkdownString: class {constructor() {this.value = ''} appendMarkdown(s) {this.value += s} appendText(s) {this.value += s}},
  Uri: {file: file => ({toString: () => 'file://' + file}), parse: value => ({toString: () => value})}, DecorationRangeBehavior: {ClosedClosed: 1},
  ConfigurationTarget: {Global: 1, Workspace: 2},
  commands: {registerCommand: (name, callback) => {commands.set(name, callback); return {dispose: () => commands.delete(name)}}},
  window: {activeTextEditor: editor, visibleTextEditors: [editor], onDidChangeVisibleTextEditors: listen('visible'),
    createTextEditorDecorationType: options => {const type = {options, disposed: false, dispose() {this.disposed = true}}; types.push(type); return type}},
  workspace: {getConfiguration: (section, resource) => ({get: (key, fallback) =>
    (section === 'editor' ? resource?.fontOverrides?.[key] ?? editorSettings[key] : settings[key]) ?? fallback,
    update: async (key, value) => {settings[key] = value}}),
    onDidChangeTextDocument: listen('change'), onDidCloseTextDocument: listen('close'), onDidChangeConfiguration: listen('config')}
}
const active = kind => types.filter(t => !t.disposed && (kind === 'inline' ? t.options.before : t.options.gutterIconPath)).flatMap(t => applied.get(t) || [])
const controller = createShapeVisuals(vscode, {asAbsolutePath: file => path.join(__dirname, file)})
async function run() {
  try {
    assert.equal(active('inline').length, 2)
    function checkSize(pixels) {
      for (const type of types.filter(type => !type.disposed && applied.get(type)?.length && type.options.before)) {
        const before = type.options.before
        assert.equal(before.width, `${pixels}px`); assert.equal(before.height, `${pixels}px`)
        const data = before.contentIconPath.toString()
        assert(data.startsWith('data:image/svg+xml;base64,'))
        const svg = Buffer.from(data.split(',')[1], 'base64').toString()
        const root = svg.slice(0, svg.indexOf('>'))
        assert(root.includes(`width="${pixels}"`) && root.includes(`height="${pixels}"`), 'intrinsic image must be font-sized')
      }
    }
    checkSize(14)
    const oldTypes = types.filter(type => !type.disposed)
    editorSettings.fontSize = 24; emit('config', {affectsConfiguration: key => key === 'editor.fontSize'})
    checkSize(24); assert(oldTypes.every(type => type.disposed), 'font changes release old image styles')
    document.fontOverrides = {fontSize: 10}
    emit('config', {affectsConfiguration: key => key === '[coscad]'})
    checkSize(10)
    delete document.fontOverrides; editorSettings.fontSize = 14
    emit('config', {affectsConfiguration: () => true})
    await commands.get('coscad.toggleShapeVisuals')()
    assert.equal(settings.shapeVisuals, 'off'); assert.equal(active('inline').length, 0)
    await commands.get('coscad.toggleShapeVisuals')()
    assert.equal(settings.shapeVisuals, 'inline'); assert.equal(active('inline').length, 2)
    assert.equal(active('gutter').length, 0)
    assert.equal(text, 'main = xcyl 3 12 |> add (sphere 2)\n', 'decorations preserve source/copy text')
    const existing = new vscode.Hover(['Existing LSP documentation'], 'original-range')
    const token = {isCancellationRequested: false}
    let hover = await controller.provideHover(document, {character: 8}, token, async () => existing)
    assert.equal(hover.range, 'original-range')
    assert.equal(hover.contents[0], 'Existing LSP documentation')
    assert.equal(existing.contents.length, 1, 'middleware does not mutate cached hover results')
    assert(hover.contents[1].value.includes('xcyl.svg') && hover.contents[1].value.includes('X axis'))
    assert.equal(hover.contents[1].isTrusted, false)
    assert.equal(await controller.provideHover(document, {character: 0}, token, async () => existing), existing)
    assert.equal(await controller.provideHover(document, {character: 8}, {isCancellationRequested: true}, async () => existing), existing)
    settings.shapeVisuals = 'both'; emit('config', {affectsConfiguration: () => true})
    assert.equal(active('inline').length, 2); assert.equal(active('gutter').length, 1, 'one gutter icon per line')
    settings.shapeVisuals = 'gutter'; emit('config', {affectsConfiguration: () => true})
    assert.equal(active('inline').length, 0); assert.equal(active('gutter').length, 1)
    settings.shapeVisuals = 'off'; emit('config', {affectsConfiguration: () => true})
    assert.equal(active('inline').length + active('gutter').length, 0)
    hover = await controller.provideHover(document, {character: 8}, token, async () => existing)
    assert.equal(hover.contents.length, 2, 'hover previews are independently switchable')
    settings.shapeHoverPreviews = false
    assert.equal(await controller.provideHover(document, {character: 8}, token, async () => existing), existing)
    settings.shapeVisuals = 'inline'; emit('config', {affectsConfiguration: () => true})
    text = '// main = xcyl 3 12\n'; document.version++; emit('change', {document})
    await new Promise(resolve => setTimeout(resolve, 130))
    assert.equal(active('inline').length, 0, 'deleting/commenting a shape clears stale decorations')
    text = 'main = ycyl 2 8\n'; document.version++; emit('change', {document})
    await new Promise(resolve => setTimeout(resolve, 130))
    assert.equal(active('inline').length, 1)
    document.lineCount = 1
    document.lineAt = () => ({range: {end: {character: MAX_SOURCE_LENGTH + 1}}})
    const getText = document.getText
    document.getText = () => {throw new Error('Large document must not be copied')}
    document.version++; emit('visible')
    assert.equal(active('inline').length, 0)
    delete document.lineCount; document.getText = getText
    document.fileName = '/project/assembly.assemble'; document.version++; emit('visible')
    assert.equal(active('inline').length, 0, 'switching to assembly clears decorations')
    emit('close', document)
    console.log(`Shape visuals passed (${catalog.length} native icon assets, parsed tokens, inline/gutter modes, hover, edits and lifecycle)`)
  } finally {
    controller.dispose()
    assert(types.every(type => type.disposed))
    assert(Object.values(callbacks).every(listeners => listeners.size === 0))
    assert.equal(commands.size, 0)
  }
}
run().catch(error => {console.error(error); process.exitCode = 1})
