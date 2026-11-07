// Client-side images complement the portable LSP hover; no render subprocesses.
const path = require('path')
const fs = require('fs')
const { COSCADParser } = require('./coscad-parser')
const { walk } = require('./hover-info')
const catalog = require('./shape-icons.json')
const aliases = new Map(catalog.flatMap(icon => icon.aliases.map(alias => [alias, icon])))
const MAX_SOURCE_LENGTH = 256 * 1024
const MAX_SYMBOLS = 1000

function inlineIconSize(editorSettings) {
  const configured = editorSettings.get('fontSize', 14)
  const font = Number.isFinite(configured) && configured > 0 ? configured : 14
  const lineHeight = editorSettings.get('lineHeight', 0)
  const line = Number.isFinite(lineHeight) && lineHeight > 0 ? (lineHeight < 8 ? lineHeight * font : lineHeight) : font
  return Math.max(1, Math.floor(Math.min(font, line)))
}

function sizeInlineSvg(svg, pixels) {
  // contentIconPath uses CSS `content: url(...)`: width/height on the pseudo
  // element do not reliably scale the replaced image. Bound its intrinsic size.
  return svg.replace(/<svg\b([^>]*)>/, (_, attributes) =>
    `<svg${attributes.replace(/\s(?:width|height)="[^"]*"/g, '')} width="${pixels}" height="${pixels}">`)
}

function collectShapeSymbols(text, fileType = 'coscad') {
  if (fileType !== 'coscad' || text.length > MAX_SOURCE_LENGTH) return []
  const found = []
  try {
    const ast = new COSCADParser().parse(text)
    walk(ast.body, node => {
      const icon = node.type === 'shape' && node.dim === 3 && aliases.get(node.symbol)
      if (icon && found.length < MAX_SYMBOLS) found.push({
        icon, symbol: node.symbol, offset: node.position,
        line: node.line, column: node.column, length: node.symbol.length
      })
    })
  } catch (_) {
    // Deep or incomplete input must not break the editor or retain stale images.
    return []
  }
  return found.sort((a, b) => a.offset - b.offset)
}

function createShapeVisuals(vscode, context) {
  const cache = new Map(), types = new Map(), sources = new Map(), subscriptions = []
  let timer, disposed = false
  const settings = document => vscode.workspace.getConfiguration('coscad', document)
  const key = document => document.uri.toString()
  const range = symbol => new vscode.Range(symbol.line, symbol.column, symbol.line, symbol.column + symbol.length)
  const asset = icon => vscode.Uri.file(context.asAbsolutePath(path.join('assets', 'shapes', icon.id + '.svg')))
  const mode = document => {
    const value = settings(document).get('shapeVisuals', 'inline')
    return ['inline', 'gutter', 'both', 'off'].includes(value) ? value : 'inline'
  }
  function symbols(document) {
    if (document.languageId !== 'coscad' || path.extname(document.fileName).toLowerCase() === '.assemble') return []
    const id = key(document)
    let previous = cache.get(id)
    if (previous && Number.isInteger(document.version) && previous.version === document.version) return previous.symbols
    // Check the rope's length without first copying a large generated document.
    if (Number.isInteger(document.lineCount) && document.lineCount > 0 &&
        document.offsetAt(document.lineAt(document.lineCount - 1).range.end) > MAX_SOURCE_LENGTH) {
      cache.set(id, {version: document.version, symbols: []})
      return []
    }
    const text = document.getText()
    if (!previous || previous.version !== document.version || previous.text !== text) {
      previous = { version: document.version, text, symbols: collectShapeSymbols(text) }
      cache.set(id, previous)
    }
    return previous.symbols
  }
  function preview(symbol) {
    const markdown = new vscode.MarkdownString()
    markdown.supportHtml = true
    markdown.isTrusted = false
    const uri = asset(symbol.icon).toString().replace(/&/g, '&amp;').replace(/"/g, '&quot;')
    markdown.appendMarkdown(`<img src="${uri}" width="160" height="160" alt="${symbol.icon.title}">\n\n`)
    markdown.appendText(`${symbol.icon.title}${symbol.icon.axis ? ' · ' + symbol.icon.axis + ' axis' : ''}\n\nPrimitive illustration; dimensions and surrounding transforms are not shown.`)
    return markdown
  }
  function decoration(icon, placement, pixels) {
    const id = placement + ':' + icon.id + (placement === 'inline' ? ':' + pixels : '')
    if (!types.has(id)) {
      const options = { rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed }
      if (placement === 'gutter') {
        options.gutterIconPath = asset(icon)
        options.gutterIconSize = 'contain'
      } else {
        if (!sources.has(icon.id)) sources.set(icon.id, fs.readFileSync(context.asAbsolutePath(path.join('assets', 'shapes', icon.id + '.svg')), 'utf8'))
        const svg = sizeInlineSvg(sources.get(icon.id), pixels)
        const image = vscode.Uri.parse('data:image/svg+xml;base64,' + Buffer.from(svg).toString('base64'))
        options.before = { contentIconPath: image, width: pixels + 'px', height: pixels + 'px', margin: '0 0.2em 0 0' }
      }
      types.set(id, vscode.window.createTextEditorDecorationType(options))
    }
    return types.get(id)
  }
  function refresh() {
    if (disposed) return
    const used = new Set()
    for (const editor of vscode.window.visibleTextEditors) {
      const selected = mode(editor.document)
      const pixels = inlineIconSize(vscode.workspace.getConfiguration('editor', editor.document))
      const groups = new Map(), gutterLines = new Set()
      if (selected !== 'off') for (const symbol of symbols(editor.document)) {
        const placements = selected === 'both' ? ['inline', 'gutter'] : [selected]
        for (const placement of placements) {
          // A gutter has one slot per line; inline mode can show nested primitives.
          if (placement === 'gutter' && gutterLines.has(symbol.line)) continue
          if (placement === 'gutter') gutterLines.add(symbol.line)
          const type = decoration(symbol.icon, placement, pixels)
          used.add(type)
          if (!groups.has(type)) groups.set(type, [])
          groups.get(type).push({ range: range(symbol) })
        }
      }
      // Clear previous types as well, including on mode/document changes.
      for (const type of types.values()) editor.setDecorations(type, groups.get(type) || [])
    }
    // Font changes must release old image styles instead of accumulating them.
    for (const [id, type] of types) if (!used.has(type)) { type.dispose(); types.delete(id) }
  }
  function schedule() {
    clearTimeout(timer)
    timer = setTimeout(refresh, 100)
  }
  subscriptions.push(vscode.window.onDidChangeVisibleTextEditors(() => {
    const visible = new Set(vscode.window.visibleTextEditors.map(editor => key(editor.document)))
    for (const id of cache.keys()) if (!visible.has(id)) cache.delete(id)
    refresh()
  }))
  subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => {
    cache.delete(key(event.document)); schedule()
  }))
  subscriptions.push(vscode.workspace.onDidCloseTextDocument(document => cache.delete(key(document))))
  subscriptions.push(vscode.workspace.onDidChangeConfiguration(event => {
    if (['coscad.shapeVisuals', 'coscad.shapeHoverPreviews', 'editor.fontSize', 'editor.lineHeight', '[coscad]']
      .some(setting => event.affectsConfiguration(setting))) refresh()
  }))
  subscriptions.push(vscode.commands.registerCommand('coscad.toggleShapeVisuals', async () => {
    const document = vscode.window.activeTextEditor?.document
    const config = settings(document)
    const target = vscode.workspace.workspaceFolders?.length ? vscode.ConfigurationTarget.Workspace : vscode.ConfigurationTarget.Global
    await config.update('shapeVisuals', mode(document) === 'off' ? 'inline' : 'off', target)
    refresh()
  }))
  refresh()
  return {
    async provideHover(document, position, cancellation, next) {
      const hover = await next(document, position, cancellation)
      if (disposed || cancellation.isCancellationRequested || !settings(document).get('shapeHoverPreviews', true)) return hover
      const offset = document.offsetAt(position)
      const symbol = symbols(document).find(item => offset >= item.offset && offset < item.offset + item.length)
      if (!symbol) return hover
      return new vscode.Hover([...(hover?.contents || []), preview(symbol)], hover?.range || range(symbol))
    },
    dispose() {
      disposed = true; clearTimeout(timer)
      subscriptions.forEach(subscription => subscription.dispose())
      types.forEach(type => type.dispose())
      types.clear(); cache.clear(); sources.clear()
    }
  }
}

module.exports = { collectShapeSymbols, createShapeVisuals, inlineIconSize, sizeInlineSvg, catalog, MAX_SOURCE_LENGTH, MAX_SYMBOLS }
