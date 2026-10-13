// extension.js
const vscode = require('vscode')
const path = require('path')
const { execFile } = require('child_process')
const { LanguageClient, TransportKind } = require('vscode-languageclient/node')

// The first compiler release whose `validate` command, `file:line:col: reason`
// diagnostics and exit codes (1 = could not run, 2 = design problem) this
// extension relies on.
const MIN_COMPILER = [1, 2, 0, 0]
const VALIDATE_TIMEOUT_MS = 20000

let client
let compilerDiagnostics

function config() {
  const settings = vscode.workspace.getConfiguration('coscad')
  // `coscad.executablePath` was the working directory in 0.1-0.3 despite
  // its name; it is still honoured when the new setting is empty.
  const cwd = settings.get('workingDirectory', '') || settings.get('executablePath', '')
  return {
    command: settings.get('executableCommand', 'coscad'),
    cwd,
    compilerDiagnostics: settings.get('compilerDiagnostics', true)
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
  return process.platform === 'win32' ? '"' + value.replace(/"/g, '""') + '"' : "'" + value.replace(/'/g, "'\"'\"'") + "'"
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
    if (document.isDirty) await document.save()
    runInTerminal(makeArgs(document.fileName), document.fileName)
  }))
}

// ---------------------------------------------------------------
// Compiler diagnostics. On save (and open), `coscad validate` checks the
// part or assembly without producing artifacts; its stderr is turned into
// editor diagnostics. The compiler prints one diagnostic per line as
//
//   Error: file:line:col: reason          (optionally followed by an excerpt:
//     |                                     "  |", "N | source", "  | ^^^")
//   Error: file: design: reason           (plan design errors; exit 2)
//   Error: file: reason                   (file-level: missing part file)
//   Error: reason                         (unpositioned: circular dependency)
//
// `isExcerpt` lines are never diagnostics themselves.
function parseCompilerErrors(stderr, filePath) {
  const diagnostics = []
  const lines = stderr.replace(/\r/g, '').split('\n')
  const positioned = /^(.*?):(\d+):(\d+):\s*(.*)$/
  const fileLevel = /^((?:[A-Za-z]:)?[^:\n]*\.(?:coscad|assemble)):\s+(.*)$/
  const isExcerpt = line => /^\s*(\d+\s*)?\|/.test(line)
  const same = file => path.resolve(file) === path.resolve(filePath)
  const sev = vscode.DiagnosticSeverity.Error
  let index = 0
  let sawError = false
  while (index < lines.length) {
    const raw = lines[index]
    const hasPrefix = /^Error:\s/.test(raw)
    const line = raw.replace(/^Error:\s*/, '')
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
        while (end < lines.length && lines[end].trim() && !/^Error:/.test(lines[end]) && !positioned.test(lines[end])) messageLines.push(lines[end++].trim())
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
    while (end < lines.length && lines[end].trim() && !/^Error:/.test(lines[end]) && !isExcerpt(lines[end])) block.push(lines[end++].trim())
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

function runCompiler(args, cwd, timeout) {
  return new Promise(resolve => {
    const [exe, ...pre] = splitCommand(config().command)
    execFile(exe, [...pre, ...args], { cwd, timeout, windowsHide: true, maxBuffer: 8 * 1024 * 1024 }, (error, stdout, stderr) => {
      resolve({ error, stdout: String(stdout || ''), stderr: String(stderr || '') })
    })
  })
}

let warned = false
let probe = null // Promise<{ok: boolean}>; reset when settings change

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
  if (!probe) {
    probe = runCompiler(['--version'], cwd, 10000).then(({ error, stdout, stderr }) => {
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
      return { ok: true }
    })
  }
  return probe
}

async function compileForDiagnostics(document) {
  const { command, cwd, compilerDiagnostics: enabled } = config()
  if (!enabled || !command || document.languageId !== 'coscad' || !['.coscad', '.assemble'].includes(path.extname(document.fileName).toLowerCase())) return
  if (document.uri.scheme !== 'file') return
  const workingDirectory = cwd || path.dirname(document.fileName)
  const { ok } = await compilerUsable(workingDirectory)
  if (!ok) return
  const version = document.version
  const { error, stdout, stderr } = await runCompiler(['validate', document.fileName], workingDirectory, VALIDATE_TIMEOUT_MS)
  if (document.version !== version || document.isClosed || !config().compilerDiagnostics) return
  if (!error) { compilerDiagnostics.set(document.uri, []); return }
  if (error.killed || error.signal) {
    const d = new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), `coscad validate did not finish within ${VALIDATE_TIMEOUT_MS / 1000} s; compiler diagnostics for this file are stale`, vscode.DiagnosticSeverity.Warning)
    compilerDiagnostics.set(document.uri, [d])
    return
  }
  const found = parseCompilerErrors(stderr || stdout, document.fileName)
  if (!found.length) {
    // non-zero exit with nothing we could parse: show the raw tail rather than nothing
    const tail = (stderr || stdout || error.message || '').trim().split('\n').slice(-3).join(' ')
    found.push(new vscode.Diagnostic(new vscode.Range(0, 0, 0, 1), `coscad validate failed (exit ${error.code}): ${tail || 'no output'}`, vscode.DiagnosticSeverity.Error))
  }
  compilerDiagnostics.set(document.uri, found)
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
  context.subscriptions.push(vscode.workspace.onDidSaveTextDocument(compileForDiagnostics))
  context.subscriptions.push(vscode.workspace.onDidOpenTextDocument(compileForDiagnostics))
  context.subscriptions.push(vscode.workspace.onDidCloseTextDocument(document => compilerDiagnostics.delete(document.uri)))
  context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(event => {
    if (event.affectsConfiguration('coscad')) {
      warned = false
      probe = null
      if (!config().compilerDiagnostics) compilerDiagnostics.clear()
      else for (const document of vscode.workspace.textDocuments) compileForDiagnostics(document)
    }
  }))
  for (const document of vscode.workspace.textDocuments) compileForDiagnostics(document)

  client.start().catch(error => {
    vscode.window.showErrorMessage(`Failed to start COSCAD Language Server: ${error.message}`)
  })
}

function deactivate() {
  return client ? client.stop() : undefined
}

module.exports = { activate, deactivate, parseCompilerErrors, parseVersion, versionAtLeast, splitCommand }
