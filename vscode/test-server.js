// Exercise the actual stdio server, including UTF-16 ranges and cross-file frames.
const assert = require('assert/strict')
const {spawn} = require('child_process')
const fs = require('fs'), os = require('os'), path = require('path')
const {pathToFileURL} = require('url')
const {createMessageConnection, StreamMessageReader, StreamMessageWriter} = require('vscode-jsonrpc/node')

async function run() {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'coscad-lsp-protocol-'))
  const child = spawn(process.execPath, [path.join(__dirname,'coscad-lsp-server.js'),'--stdio'], {stdio:['pipe','pipe','pipe']})
  let stderr = ''; child.stderr.on('data', chunk => stderr += chunk)
  const connection = createMessageConnection(new StreamMessageReader(child.stdout),new StreamMessageWriter(child.stdin))
  const timeout = setTimeout(() => { child.kill(); throw new Error('LSP protocol timeout: '+stderr) },15000)
  // the latest diagnostics the server published, per URI, and waiters for the next ones
  const published = new Map(), waiting = new Map()
  connection.onNotification('textDocument/publishDiagnostics', p => {
    published.set(p.uri, p.diagnostics)
    const w = waiting.get(p.uri); if (w) { waiting.delete(p.uri); w(p.diagnostics) }
  })
  const nextDiagnostics = uri => new Promise(resolve => waiting.set(uri, resolve))
  connection.listen()
  try {
    const init = await connection.sendRequest('initialize',{processId:process.pid,rootUri:pathToFileURL(dir).href,capabilities:{}})
    assert(init.capabilities.hoverProvider)
    assert(init.capabilities.completionProvider.triggerCharacters.includes('@'))
    connection.sendNotification('initialized',{})
    connection.sendNotification('$/setTrace',{value:'off'})
    const part = 'main = box 20 30 4 |> frame socket top top bak\n'
    const file = path.join(dir,'part.coscad'),uri=pathToFileURL(file).href
    fs.writeFileSync(file,part)
    connection.sendNotification('textDocument/didOpen',{textDocument:{uri,languageId:'coscad',version:1,text:part}})
    const at = (text, needle,delta=0) => { const before=text.slice(0,text.indexOf(needle)+delta);const lines=before.split('\n');return {line:lines.length-1,character:lines.at(-1).length} }
    const hover = (uri,text,needle,delta=0)=>connection.sendRequest('textDocument/hover',{textDocument:{uri},position:at(text,needle,delta)})
    assert.match((await hover(uri,part,'=')).contents.value,/Solid3D/)
    assert.match((await hover(uri,part,'20')).contents.value,/Number/)
    const assembly='base ← part.coscad ×1\nchild ← part.coscad ×1\nfixed base\nmate child @socket base @socket roll=90 gap=0\n'
    const asmUri=pathToFileURL(path.join(dir,'test.assemble')).href
    connection.sendNotification('textDocument/didOpen',{textDocument:{uri:asmUri,languageId:'coscad',version:1,text:assembly}})
    assert.match((await hover(asmUri,assembly,'@socket')).contents.value,/Mating frame/)
    assert.match((await hover(asmUri,assembly,'90')).contents.value,/degrees/)
    const definition=await connection.sendRequest('textDocument/definition',{textDocument:{uri:asmUri},position:at(assembly,'@socket',2)})
    assert.equal(definition.uri,uri)
    assert.equal(definition.range.start.character,part.indexOf('socket'))
    const completion=await connection.sendRequest('textDocument/completion',{textDocument:{uri:asmUri},position:at(assembly,'@socket',3)})
    const frame=completion.find(c=>c.label==='@socket')
    assert(frame)
    assert.equal(frame.textEdit.newText,'@socket')
    assert.equal(frame.textEdit.range.start.character,assembly.split('\n')[3].indexOf('@'))
    assert.equal(frame.textEdit.range.end.character,frame.textEdit.range.start.character+3)
    const semantic=await connection.sendRequest('textDocument/semanticTokens/full',{textDocument:{uri:asmUri}})
    assert(semantic.data.length>0 && semantic.data.length%5===0)
    // An unsaved part change must be visible to cross-file hover/definition.
    const revised=part.replace('socket','updated')
    connection.sendNotification('textDocument/didChange',{textDocument:{uri,version:2},contentChanges:[{text:revised}]})
    const revisedAsm=assembly.replaceAll('@socket','@updated')
    connection.sendNotification('textDocument/didChange',{textDocument:{uri:asmUri,version:2},contentChanges:[{text:revisedAsm}]})
    assert.match((await hover(asmUri,revisedAsm,'@updated')).contents.value,/Mating frame/)
    const updated=await connection.sendRequest('textDocument/definition',{textDocument:{uri:asmUri},position:at(revisedAsm,'@updated',2)})
    assert.equal(updated.uri,uri)
    const headFile=path.join(dir,'head.coscad'),headUri=pathToFileURL(headFile).href
    const headPart='main = box 20 20 4 |> frame head top top bak\n'
    fs.writeFileSync(headFile,headPart)
    const hw='base ← part.coscad ×1 preload=hand\nchild ← head.coscad ×1\nasm = base ⊕ child\nfastener M4x12 child base @updated seat=@head nut=hex pocket=@updated\n'
    connection.sendNotification('textDocument/didChange',{textDocument:{uri:asmUri,version:3},contentChanges:[{text:hw}]})
    for(const [needle,target,delta] of [['@updated',uri,2],['@head',headUri,2],['pocket=@updated',uri,9]]) {
      const def=await connection.sendRequest('textDocument/definition',{textDocument:{uri:asmUri},position:at(hw,needle,delta)})
      assert.equal(def.uri,target,needle)
    }
    assert.match((await hover(asmUri,hw,'seat=@head',6)).contents.value,/head.coscad/)
    const seats=await connection.sendRequest('textDocument/completion',{textDocument:{uri:asmUri},position:at(hw,'seat=@head',8)})
    assert(seats.some(c=>c.label==='@head'))
    assert(!seats.some(c=>c.label==='@updated'))
    // The compiler owns diagnostics while the extension says so: the server's
    // parser then publishes none, and resumes when switched back.
    const brokenUri = pathToFileURL(path.join(dir,'broken.coscad')).href
    let next = nextDiagnostics(brokenUri)
    connection.sendNotification('textDocument/didOpen',{textDocument:{uri:brokenUri,languageId:'coscad',version:1,text:'main = box 1 1 1 ⊕\n'}})
    assert((await next).length > 0, 'own diagnostics while the compiler is not live')
    next = nextDiagnostics(brokenUri)
    connection.sendNotification('coscad/compilerDiagnostics',{active:true})
    assert.deepEqual(await next, [], 'cleared once the compiler owns diagnostics')
    next = nextDiagnostics(brokenUri)
    connection.sendNotification('textDocument/didChange',{textDocument:{uri:brokenUri,version:2},contentChanges:[{text:'main = box 1 1 ⊕\n'}]})
    assert.deepEqual(await next, [], 'edits stay quiet while the compiler owns diagnostics')
    next = nextDiagnostics(brokenUri)
    connection.sendNotification('coscad/compilerDiagnostics',{active:false})
    assert((await next).length > 0, 'own diagnostics resume as the fallback')
    await connection.sendRequest('shutdown',null)
    connection.sendNotification('exit',null)
    console.log('Stdio LSP integration passed (hover, definitions, completion edits, semantic tokens, unsaved frames, compiler-owned diagnostics)')
  } finally { clearTimeout(timeout); connection.dispose(); child.kill(); fs.rmSync(dir,{recursive:true,force:true}) }
}
run().catch(e=>{console.error(e);process.exitCode=1})
