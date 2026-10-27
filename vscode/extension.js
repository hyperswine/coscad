// extension.js
const vscode = require('vscode')
const path = require('path')
const fs = require('fs')
const { identity, snapshot, ValidationQueue } = require('./project-state')
const { execFile } = require('child_process')
const { LanguageClient, TransportKind } = require('vscode-languageclient/node')

// The first compiler release whose `validate` command, `file:line:col: reason`
// diagnostics and exit codes (1 = could not run, 2 = design problem) this
// extension relies on.
const MIN_COMPILER = [1, 2, 0, 0]
const VALIDATE_TIMEOUT_MS = 20000
const LIVE_DELAY_MS = 400 // pause after typing before validating the buffer

let client
let compilerDiagnostics
let validationQueue
let backendStatus
let discoveredCapabilities = null

function config() {
  const settings = vscode.workspace.getConfiguration('coscad')
  // `coscad.executablePath` was the working directory in 0.1-0.3 despite
  // its name; it is still honoured when the new setting is empty.
  const cwd = settings.get('workingDirectory', '') || settings.get('executablePath', '')
  return {
    command: settings.get('executableCommand', 'coscad'),
    cwd,
    compilerDiagnostics: settings.get('compilerDiagnostics', true),
    backend: settings.get('backend', 'auto')
  }
}

// `coscad.executableCommand` is a command line ("coscad", "cabal run coscad --"),
// split on whitespace; quoted segments keep their spaces.
function splitCommand(command) {
  const parts = []
  const re = /"([^"]*)"|'([^']*)'|(\S+)/g
  let m
  while ((m = re.exec(command))) parts.push(m[1] ?? m[2] ?? m[3])
  return parts
}

function activeCoscadDocument(allowedExtensions) {
  const editor = vscode.window.activeTextEditor
  if (!editor || editor.document.languageId !== 'coscad') {
    vscode.window.showErrorMessage('No active CoScad file (.coscad or .assemble)')
    return null
  }
  const ext = path.extname(editor.document.fileName).toLowerCase()
  if (allowedExtensions && !allowedExtensions.includes(ext)) {
    vscode.window.showErrorMessage(`This command needs a ${allowedExtensions.join(' or ')} file`)
    return null
  }
  return editor.document
}

function shellQuote(value) {
  if (/^[\w./@%+=:,-]+$/.test(value)) return value
  return "'" + value.replace(/'/g, "'\"'\"'") + "'"
}

// Run `coscad <args...>` in the COSCAD terminal. The terminal is created in
// the right working directory (the file's folder, or coscad.workingDirectory),
// so no `cd` is sent and the command line is the same in every shell.
function runInTerminal(args, filePath) {
  const { command, cwd } = config()
  if (!command) {
    vscode.window.showErrorMessage('coscad.executableCommand is not set', 'Open Settings').then(choice => {
      if (choice === 'Open Settings') vscode.commands.executeCommand('workbench.action.openSettings', 'coscad.executableCommand')
    })
    return
  }
  const workingDirectory = cwd || (filePath ? path.dirname(filePath) : undefined)
  let terminal = vscode.window.terminals.find(t => t.name === 'COSCAD')
  if (terminal && workingDirectory && terminal.creationOptions && terminal.creationOptions.cwd &&
      path.resolve(String(terminal.creationOptions.cwd)) !== path.resolve(workingDirectory)) {
    terminal.dispose()
    terminal = undefined
  }
  if (!terminal) terminal = vscode.window.createTerminal({ name: 'COSCAD', cwd: workingDirectory })
  terminal.show()
  terminal.sendText(`${command} ${args.map(shellQuote).join(' ')}`)
}

function registerFileCommand(context, id, allowedExtensions, makeArgs) {
  context.subscriptions.push(vscode.commands.registerCommand(id, async () => {
    const document = activeCoscadDocument(allowedExtensions)
    if (!document) return
    // Explicit output commands run saved sources; save the entire dependency
    // closure and stop if any save is declined/failed.
    let project
    try { project = snapshot(document, vscode.workspace.textDocuments) }
    catch (error) { vscode.window.showErrorMessage(error.message); return }
    for (const doc of vscode.workspace.textDocuments) {
      if (doc.uri.scheme === 'file' && project.dependencies.has(identity(doc.fileName)) && doc.isDirty && !await doc.save()) {
        vscode.window.showErrorMessage('CoScad command cancelled: a project file was not saved.'); return
      }
    }
    const args = makeArgs(document.fileName)
    const info = await compilerUsable(config().cwd || path.dirname(document.fileName))
    if (!info.ok) return
    if (id === 'coscad.stress' && !info.capabilities?.features?.includes('stress')) {
      vscode.window.showErrorMessage('This compiler does not advertise stress analysis. Update the compiler.'); return
    }
    const backend = config().backend
    if (['stl','stress','check','next'].includes(args[0]) && backend !== 'auto') {
      if (!info.capabilities?.backends.includes(backend)) {
        vscode.window.showErrorMessage(`Backend ${backend} is unavailable in this compiler build.`); return
      }
      args.push(`--backend=${backend}`)
    }
    runInTerminal(args, document.fileName)
  }))
}

// ---------------------------------------------------------------
// Compiler diagnostics. A compiler with `validate --json --stdin` checks the
// buffer as it is on open, save and (debounced) every edit, and its JSON is
// turned into diagnostics (diagnosticsFromJson); the language server's own
// diagnostics are then switched off so only the compiler's rules apply.
// Older compilers are run as `coscad validate <file>` on saved files and
// their stderr is parsed (parseCompilerErrors). The compiler prints one
// diagnostic per line as
//
//   Error: file:line:col: reason          (optionally followed by an excerpt:
//     |                                     "  |", "N | source", "  | ^^^")
//   Error: file: design: reason           (plan design errors; exit 2)
//   Error: file: reason                   (file-level: missing part file)
//   Error: reason                         (unpositioned: circular dependency)
//   Warning: file:line:col: reason        (same layouts; exit status 0, e.g. an
//                                          anchor that misses its geometry)
//
// `isExcerpt` lines are never diagnostics themselves.
function parseCompilerErrors(stderr, filePath) {
  const diagnostics = []
  const lines = stderr.replace(/\r/g, '').split('\n')
  const positioned = /^(.*?):(\d+):(\d+):\s*(.*)$/
  const fileLevel = /^([^:\n]*\.(?:coscad|assemble)):\s+(.*)$/
  const isExcerpt = line => /^\s*(\d+\s*)?\|/.test(line)
  const same = file => path.resolve(file) === path.resolve(filePath)
  const prefixed = /^(Error|Warning):\s/
  let index = 0
  let sawError = false
  while (index < lines.length) {
    const raw = lines[index]
    const hasPrefix = prefixed.test(raw)
    const sev = /^Warning:\s/.test(raw) ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Error
    const line = raw.replace(/^(Error|Warning):\s*/, '')
    if (isExcerpt(raw) || !line.trim()) { index++; continue }
    // Only the first line of a multi-line unpositioned message carries "Error:";
    // continuation lines are folded into it below.
    if (!hasPrefix && sawError) { index++; continue }
    sawError = true
    const m = line.match(positioned)
    if (m) {
      const [, file, lineNo, colNo, rest] = m
      let message = rest.trim()
      let end = index + 1
      while (end < lines.length && isExcerpt(lines[end])) end++
      if (!message) {
        // pre-1.1 layout: the reason came after the excerpt
        const messageLines = []
        while (end < lines.length && lines[end].trim() && !prefixed.test(lines[end]) && !positioned.test(lines[end])) messageLines.push(lines[end++].trim())
        message = messageLines.join(' ')
      }
      if (same(file)) {
        const l = Math.max(0, Number(lineNo) - 1)
        const c = Math.max(0, Number(colNo) - 1)
        diagnostics.push(new vscode.Diagnostic(new vscode.Range(l, c, l, c + 1), message || line, sev))
      } else {
        // A broken referenced part must be visible while editing its assembly,
        // without pretending the child's range belongs here.
        diagnostics.push(new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), `${file}:${lineNo}:${colNo}: ${message || line}`, sev))
      }
      index = end
      continue
    }
    const f = line.match(fileLevel)
    if (f) {
      const [, file, message] = f
      const text = same(file) ? message : `${file}: ${message}`
      diagnostics.push(new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), text, sev))
      index++
      continue
    }
    // unpositioned: take the continuation lines (no Error: prefix) with it
    let end = index + 1
    const block = [line.trim()]
    while (end < lines.length && lines[end].trim() && !prefixed.test(lines[end]) && !isExcerpt(lines[end])) block.push(lines[end++].trim())
    diagnostics.push(new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), block.join(' '), sev))
    index = end
  }
  return diagnostics
}

// `coscad --version` -> [1, 1, 0, 0]; null when the output is not a version.
function parseVersion(text) {
  const m = String(text).match(/coscad\s+(\d+(?:\.\d+)*)/)
  return m ? m[1].split('.').map(Number) : null
}

function versionAtLeast(have, want) {
  for (let i = 0; i < want.length; i++) {
    const h = have[i] ?? 0
    if (h > want[i]) return true
    if (h < want[i]) return false
  }
  return true
}

// `input`, when given, is written to the compiler's stdin (an unsaved buffer
// for `validate --stdin`).
function runCompiler(args, cwd, timeout, input, signal) {
  return new Promise(resolve => {
    const [exe, ...pre] = splitCommand(config().command)
    const child = execFile(exe, [...pre, ...args], { cwd, timeout, signal, maxBuffer: 8 * 1024 * 1024 }, (error, stdout, stderr) => {
      resolve({ error, stdout: String(stdout || ''), stderr: String(stderr || '') })
    })
    if (child.stdin) {
      child.stdin.on('error', () => {}) // the compiler may exit before reading it all
      child.stdin.end(input === undefined ? '' : input)
    }
  })
}

// `coscad validate --json` output -> diagnostics for `filePath`, or null when
// the output is not that JSON document (an older compiler without --json).
function diagnosticsFromJson(stdout, filePath) {
  let doc
  try { doc = JSON.parse(stdout) } catch { return null }
  if (!doc || doc.schema !== 'coscad.validate/1' || !Array.isArray(doc.diagnostics)) return null
  const same = file => path.resolve(file) === path.resolve(filePath)
  const severity = s => s === 'error' ? vscode.DiagnosticSeverity.Error
    : s === 'warning' ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Information
  return doc.diagnostics.map(d => {
    const located = d.file && Number.isInteger(d.line)
    if (located && same(d.file)) {
      const l = Math.max(0, d.line - 1)
      const c = Math.max(0, (Number.isInteger(d.column) ? d.column : 1) - 1)
      return new vscode.Diagnostic(new vscode.Range(l, c, l, c + 1), d.message, severity(d.severity))
    }
    // a referenced file, or no position: show it at the top, named
    const where = d.file && !same(d.file) ? `${d.file}${located ? `:${d.line}:${d.column}` : ''}: ` : ''
    return new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), where + d.message, severity(d.severity))
  })
}

// Whether the compiler understands `validate --json --stdin`: null until the
// first run tells us; reset when settings change. With it, diagnostics follow
// every edit and the language server's own diagnostics are switched off.
let jsonValidate = null

let compilerOwns = false // last state told (or to be told) to the language server

function notifyServer() {
  if (!client || !client.isRunning || !client.isRunning()) return // sent once the client starts
  try {
    Promise.resolve(client.sendNotification('coscad/compilerDiagnostics', { active: compilerOwns })).catch(() => {})
    Promise.resolve(client.sendNotification('coscad/compilerCapabilities', discoveredCapabilities)).catch(() => {})
  } catch {}
}

function setCompilerOwnsDiagnostics(active) {
  if (compilerOwns === active) return
  compilerOwns = active
  notifyServer()
}

let warned = false
let probe = null // Promise for one compiler identity
let probeKey = ''
function compilerIdentity(cwd) {
  const command = config().command, exe = splitCommand(command)[0]
  const candidate = exe && (path.isAbsolute(exe) ? exe : exe.includes(path.sep) ? path.resolve(cwd,exe) : (process.env.PATH || '').split(path.delimiter).map(p=>path.join(p,exe)).find(p=>fs.existsSync(p)))
  let stamp = ''
  try { const stat=fs.statSync(candidate); stamp=`${stat.size}:${stat.mtimeMs}` } catch {}
  return JSON.stringify([command,cwd,candidate,stamp])
}

function warnOnce(message, settingKey) {
  if (warned) return
  warned = true
  vscode.window.showWarningMessage(message, 'Open Settings', 'Disable').then(choice => {
    if (choice === 'Open Settings') vscode.commands.executeCommand('workbench.action.openSettings', settingKey)
    if (choice === 'Disable') vscode.workspace.getConfiguration('coscad').update('compilerDiagnostics', false, vscode.ConfigurationTarget.Global)
  })
}

// Check once per configuration that the compiler runs and is new enough
// for `validate`; otherwise say so once and keep the editor quiet rather
// than painting a usage error on every file.
function compilerUsable(cwd) {
  const key = compilerIdentity(cwd)
  if (key !== probeKey) { probeKey=key; probe=null; jsonValidate=null }
  if (!probe) {
    probe = runCompiler(['--version'], cwd, 10000).then(async ({ error, stdout, stderr }) => {
      const { command } = config()
      if (error && (error.code === 'ENOENT' || error.code === 127 || /not found|is not recognized/i.test(stderr + error.message))) {
        warnOnce(`CoScad compiler not found ("${command}"); compiler diagnostics are off until coscad.executableCommand points at it (a release binary on PATH, or "cabal run coscad --" with coscad.workingDirectory set to the checkout).`, 'coscad.executableCommand')
        return { ok: false }
      }
      const version = parseVersion(stdout)
      if (!version) {
        warnOnce(`"${command} --version" did not answer with a version (${(stderr || stdout).trim().slice(0, 120) || error?.message || 'no output'}); compiler diagnostics are off.`, 'coscad.executableCommand')
        return { ok: false }
      }
      if (!versionAtLeast(version, MIN_COMPILER)) {
        warnOnce(`CoScad compiler ${version.join('.')} has no "validate" command; compiler diagnostics need ${MIN_COMPILER.join('.')} or newer. Update the compiler or disable coscad.compilerDiagnostics.`, 'coscad.compilerDiagnostics')
        return { ok: false }
      }
      const run = await runCompiler(['capabilities','--json'], cwd, 10000)
      let capabilities = null
      try { const value=JSON.parse(run.stdout); if (!run.error && value.schema==='coscad.capabilities/1' && Array.isArray(value.backends) && Array.isArray(value.features)) capabilities=value } catch {}
      if (key === probeKey) { discoveredCapabilities = capabilities; notifyServer() }
      if (backendStatus && key === probeKey) {
        backendStatus.text = `CoScad: ${config().backend === 'auto' ? capabilities?.default_backend || 'compiler default' : config().backend}`
        backendStatus.tooltip = capabilities ? `Compiler ${capabilities.version}; available backends: ${capabilities.backends.join(', ')}. Click to select.` : 'Legacy compiler: capability discovery unavailable.'
      }
      return { ok: true, capabilities }
    })
  }
  return probe
}

async function compileForDiagnostics(document, project, signal, current) {
  const settings = config()
  if (!settings.compilerDiagnostics || !settings.command || !['.coscad','.assemble'].includes(path.extname(document.fileName).toLowerCase())) return
  const show = (message, severity = vscode.DiagnosticSeverity.Warning) => {
    if (current()) compilerDiagnostics.set(document.uri,[new vscode.Diagnostic(new vscode.Range(0,0,0,1),message,severity)])
  }
  if (project.error) { show(project.error.message); return }
  const cwd = settings.cwd || path.dirname(document.fileName)
  const identityAtStart = compilerIdentity(cwd)
  const info = await compilerUsable(cwd)
  if (!current()) return
  if (!info.ok) { setCompilerOwnsDiagnostics(false); return }
  const overlays = info.capabilities?.features.includes('project-buffers')
  const args = overlays ? ['validate','--json','--project-stdin',document.fileName] : ['validate','--json','--stdin',document.fileName]
  let run = await runCompiler(args,cwd,VALIDATE_TIMEOUT_MS,overlays ? project.input : document.getText(),signal)
  if (!current() || compilerIdentity(cwd) !== identityAtStart) return
  if (run.error?.killed || run.error?.signal) { show('CoScad validation timed out; diagnostics are stale.'); return }
  let found = diagnosticsFromJson(run.stdout,document.fileName)
  if (found !== null) {
    jsonValidate=true; setCompilerOwnsDiagnostics(true)
    if (!overlays && project.dependencies.size > 1) found.push(new vscode.Diagnostic(new vscode.Range(0,0,0,1),'This compiler reads referenced files from disk. Update it for unsaved project validation.',vscode.DiagnosticSeverity.Warning))
    compilerDiagnostics.set(document.uri,found)
    const response=JSON.parse(run.stdout)
    return response.valid && Array.isArray(response.dependencies) ? response.dependencies : undefined
  }
  // Older compilers may lack JSON input; never pretend they validated edits.
  jsonValidate=false; setCompilerOwnsDiagnostics(false)
  if (document.isDirty) { show('Compiler lacks live validation; save to validate, or update the compiler.'); return }
  run=await runCompiler(['validate',document.fileName],cwd,VALIDATE_TIMEOUT_MS,undefined,signal)
  if (!current() || compilerIdentity(cwd) !== identityAtStart) return
  found=parseCompilerErrors(run.stderr,document.fileName)
  if (run.error && !found.length) { show(`CoScad validation failed: ${run.error.message}`); return }
  compilerDiagnostics.set(document.uri,found)
}

function activate(context) {
  const serverModule = context.asAbsolutePath(path.join('coscad-lsp-server.js'))
  const serverOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc, options: { execArgv: ['--nolazy', '--inspect=6009'] } }
  }
  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'coscad' }],
    synchronize: { fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{coscad,assemble}') },
    outputChannelName: 'COSCAD Language Server'
  }
  client = new LanguageClient('coscadLanguageServer', 'COSCAD Language Server', serverOptions, clientOptions)

  // commands (coscad.runmain keeps its historical id: compile the active file)
  registerFileCommand(context, 'coscad.runmain', ['.coscad', '.assemble'], file => [file])
  registerFileCommand(context, 'coscad.validate', ['.coscad', '.assemble'], file => ['validate', file])
  registerFileCommand(context, 'coscad.stress', ['.coscad'], file => ['stress', file, '--full'])
  registerFileCommand(context, 'coscad.stl', ['.coscad'], file => ['stl', file])
  registerFileCommand(context, 'coscad.check', ['.assemble'], file => ['check', file])
  registerFileCommand(context, 'coscad.next', ['.assemble'], file => ['next', file])
  registerFileCommand(context, 'coscad.plan', ['.assemble'], file => ['plan', file])
  registerFileCommand(context, 'coscad.planPng', ['.assemble'], file => ['plan', '--png', file])
  context.subscriptions.push(vscode.commands.registerCommand('coscad.doctor', () => {
    const editor = vscode.window.activeTextEditor
    runInTerminal(['doctor'], editor?.document.uri.scheme === 'file' ? editor.document.fileName : undefined)
  }))
  context.subscriptions.push(vscode.commands.registerCommand('coscad.openSettings', () => {
    vscode.commands.executeCommand('workbench.action.openSettings', 'coscad')
  }))

  // compiler diagnostics
  compilerDiagnostics = vscode.languages.createDiagnosticCollection('coscad-compiler')
  context.subscriptions.push(compilerDiagnostics)
  const externalWatchers = new Map()
  validationQueue = new ValidationQueue({
    documents: () => vscode.workspace.textDocuments,
    validate: compileForDiagnostics,
    onDependencies: paths => {
      const external = new Set([...paths].filter(file=>!vscode.workspace.getWorkspaceFolder(vscode.Uri.file(file))))
      for (const [file,watcher] of externalWatchers) if (!external.has(file)) { watcher.dispose(); externalWatchers.delete(file) }
      for (const file of external) if (!externalWatchers.has(file)) {
        const watcher = vscode.workspace.createFileSystemWatcher(new vscode.RelativePattern(path.dirname(file),path.basename(file)))
        watcher.onDidCreate(()=>validationQueue.changed(file)); watcher.onDidChange(()=>validationQueue.changed(file)); watcher.onDidDelete(()=>validationQueue.changed(file))
        externalWatchers.set(file,watcher)
      }
    },
    clear: doc => compilerDiagnostics.delete(doc.uri),
    delay: LIVE_DELAY_MS
  })
  context.subscriptions.push(validationQueue)
  context.subscriptions.push(vscode.workspace.onDidSaveTextDocument(doc => validationQueue.changed(doc.fileName,true)))
  context.subscriptions.push(vscode.workspace.onDidOpenTextDocument(doc => validationQueue.changed(doc.fileName,true)))
  context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(doc => { compilerDiagnostics.delete(doc.uri); validationQueue.close(doc) }))
  context.subscriptions.push(vscode.workspace.onDidChangeTextDocument(event => validationQueue.changed(event.document.fileName)))
  const watcher = vscode.workspace.createFileSystemWatcher('**/*.{coscad,assemble}')
  context.subscriptions.push(watcher, watcher.onDidCreate(uri=>validationQueue.changed(uri.fsPath)), watcher.onDidChange(uri=>validationQueue.changed(uri.fsPath)), watcher.onDidDelete(uri=>validationQueue.changed(uri.fsPath)))
  context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(event => {
    if (event.affectsConfiguration('coscad')) {
      warned=false; probe=null; probeKey=''; jsonValidate=null; discoveredCapabilities=null
      notifyServer()
      setCompilerOwnsDiagnostics(false)
      validationQueue.reset()
    }
  }))
  backendStatus = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 20)
  backendStatus.text='CoScad: detecting compiler'; backendStatus.command='coscad.selectBackend'; backendStatus.show()
  context.subscriptions.push(backendStatus, vscode.commands.registerCommand('coscad.selectBackend', async () => {
    const doc=vscode.window.activeTextEditor?.document
    const info=await compilerUsable(config().cwd || (doc ? path.dirname(doc.fileName) : process.cwd()))
    const choices=['auto',...(info.capabilities?.backends || ['openscad'])]
    const selected=await vscode.window.showQuickPick(choices,{placeHolder:'Geometry backend (auto uses this compiler build’s default)'})
    if (selected) await vscode.workspace.getConfiguration('coscad').update('backend',selected,vscode.ConfigurationTarget.Workspace)
  }))
  validationQueue.reset()

  client.start().then(notifyServer, error => {
    vscode.window.showErrorMessage(`Failed to start COSCAD Language Server: ${error.message}`)
  })
}

function deactivate() {
  validationQueue?.dispose()
  return client ? client.stop() : undefined
}

module.exports = { activate, deactivate, parseCompilerErrors, diagnosticsFromJson, parseVersion, versionAtLeast, splitCommand }
