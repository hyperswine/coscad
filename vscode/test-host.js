// Exercise extension activation and commands with a minimal VS Code host.
const assert=require('assert/strict'), Module=require('module'), fs=require('fs'),os=require('os'),path=require('path')
const original=Module._load, commands=new Map(), errors=[], terminals=[], notifications=[], checks=[]
const events={}, settings={executableCommand:'test-coscad',backend:'auto',compilerDiagnostics:true}
const disposable=()=>({dispose(){}})
const on=name=>fn=>{events[name]=fn;return disposable()}
const dir=fs.mkdtempSync(path.join(os.tmpdir(),'coscad-host-'))
const doc=(name,text)=>({uri:{scheme:'file',fsPath:path.join(dir,name)},fileName:path.join(dir,name),languageId:'coscad',isClosed:false,isDirty:true,getText:()=>text,save:async function(){this.isDirty=false;return true}})
const root=doc('root.assemble','part ← part.coscad ×1\nasm = part\n'),part=doc('part.coscad','main = box 1 2 3\n')
const diagnostics=new Map()
const vscode={
 Range:class {constructor(...args){this.args=args}}, Diagnostic:class {constructor(range,message,severity){Object.assign(this,{range,message,severity})}},
 DiagnosticSeverity:{Error:0,Warning:1,Information:2}, ConfigurationTarget:{Workspace:1,Global:2}, StatusBarAlignment:{Left:1},
 Uri:{file:file=>({fsPath:file})}, RelativePattern:class {},
 languages:{createDiagnosticCollection:()=>({set:(uri,ds)=>diagnostics.set(uri.fsPath,ds),delete:uri=>diagnostics.delete(uri.fsPath),dispose(){}})},
 commands:{registerCommand:(id,fn)=>{commands.set(id,fn);return disposable()},executeCommand:async()=>{}},
 workspace:{textDocuments:[root,part],getConfiguration:()=>({get:(key,def)=>settings[key]??def,update:async(k,v)=>{settings[k]=v}}),getWorkspaceFolder:()=>({}),
 createFileSystemWatcher:()=>({onDidCreate:()=>disposable(),onDidChange:()=>disposable(),onDidDelete:()=>disposable(),dispose(){}}),
 onDidOpenTextDocument:on('open'),onDidSaveTextDocument:on('save'),onDidCloseTextDocument:on('close'),onDidChangeTextDocument:on('change'),onDidChangeConfiguration:on('config')},
 window:{activeTextEditor:{document:root},terminals:[],showErrorMessage:async text=>errors.push(text),showWarningMessage:async text=>errors.push(text),
 createStatusBarItem:()=>({show(){},dispose(){}}),showQuickPick:async()=>undefined,
 createTerminal:options=>({creationOptions:options,show(){},sendText:text=>terminals.push(text)})}
}
const capabilities={schema:'coscad.capabilities/1',version:'1.3.0.0',default_backend:'native-experimental',backends:['openscad','native-experimental'],features:['project-buffers','stress','image-backends','native-images'],materials:[{name:'pla'}]}
Module._load=function(request,...rest){
 if(request==='vscode') return vscode
 if(request==='vscode-languageclient/node') return {TransportKind:{},LanguageClient:class{start(){return Promise.resolve()}isRunning(){return true}sendNotification(...args){notifications.push(args)}stop(){return Promise.resolve()}}}
 if(request==='child_process') return {execFile:(exe,args,options,callback)=>{
   let input=''; const timer=setTimeout(()=>{
     if(options.signal?.aborted){callback({code:'ABORT_ERR'},'','');return}
     if(args[0]==='--version') callback(null,'coscad 1.3.0.0','')
     else if(args[0]==='capabilities') callback(null,JSON.stringify(capabilities),'')
     else {checks.push({args,input});callback(null,JSON.stringify({schema:'coscad.validate/1',valid:true,diagnostics:[],dependencies:JSON.parse(input).files.map(f=>f.path)}),'')}
   },2)
   return {stdin:{on(){},end:text=>{input=text}}}
 }}
 return original.call(this,request,...rest)
}
const extension=require('./extension')
Module._load=original
const waitUntil=async predicate=>{for(let i=0;i<200;i++){if(predicate())return;await new Promise(r=>setTimeout(r,5))}throw new Error('host test timed out')}
async function run(){
 const context={subscriptions:[],asAbsolutePath:f=>path.join(__dirname,f)}
 try{
  extension.activate(context)
  await waitUntil(()=>checks.length>=2)
  const check=checks.find(c=>c.args.at(-1)===root.fileName)
  assert(check.args.includes('--project-stdin'))
  assert.equal(JSON.parse(check.input).files.length,2,'compiler receives both unsaved documents')
  assert(notifications.some(n=>n[0]==='coscad/compilerCapabilities' && n[1]?.materials[0].name==='pla'))
  part.save=async()=>false
  await commands.get('coscad.next')()
  assert.equal(terminals.length,0,'failed dependency save blocks manufacturing')
  part.save=async function(){this.isDirty=false;return true}
  settings.backend='native-experimental'
  await commands.get('coscad.next')()
  assert(terminals.at(-1).includes('--backend=native-experimental'))
  assert(!part.isDirty)
  await commands.get('coscad.planPng')()
  assert(terminals.at(-1).includes('--backend=native-experimental'), 'image commands honor the selected backend')
  capabilities.features=capabilities.features.filter(f=>f!=='image-backends')
  settings.executableCommand='legacy-image-coscad';events.config({affectsConfiguration:()=>true})
  const legacyTerminals=terminals.length
  await commands.get('coscad.planPng')()
  assert.equal(terminals.length,legacyTerminals,'old compilers cannot silently ignore image backend selection')
  assert(errors.some(e=>e.includes('cannot select the planner image backend')))
  capabilities.features.push('image-backends')
  settings.executableCommand='test-coscad';events.config({affectsConfiguration:()=>true})
  vscode.window.activeTextEditor.document=part
  await commands.get('coscad.stress')()
  assert.match(terminals.at(-1),/stress .*--full --backend=native-experimental/)
  capabilities.backends=['openscad'];settings.executableCommand='other-coscad'
  events.config({affectsConfiguration:()=>true})
  const before=terminals.length
  await commands.get('coscad.stress')()
  assert.equal(terminals.length,before,'unavailable backend is refused after configuration change')
  assert(errors.some(e=>e.includes('unavailable')))
  // CoScad 1.6: one native backend; an openscad setting is refused with the reason,
  // and the older native-experimental name is translated.
  capabilities.version='1.6.0.0';capabilities.default_backend='native';capabilities.backends=['native']
  capabilities.features=[...capabilities.features,'openscad-retired'];settings.executableCommand='retired-coscad'
  events.config({affectsConfiguration:()=>true})
  settings.backend='openscad'
  const beforeRetired=terminals.length
  await commands.get('coscad.stress')()
  assert.equal(terminals.length,beforeRetired,'a retired OpenSCAD selection runs nothing')
  assert(errors.some(e=>e.includes('retired the OpenSCAD backend')))
  settings.backend='native-experimental'
  await commands.get('coscad.stress')()
  assert.match(terminals.at(-1),/--backend=native(\s|$)/,'native-experimental is mapped to native on a 1.6 compiler')
  const n=checks.length;settings.compilerDiagnostics=false;events.config({affectsConfiguration:()=>true})
  await new Promise(r=>setTimeout(r,30))
  assert.equal(checks.length,n,'disabled diagnostics launch no new validations')
  console.log('Extension host integration passed (project stdin, capabilities, dependent saves, stress/backend commands, configuration changes)')
 } finally {await extension.deactivate();context.subscriptions.forEach(d=>d.dispose());fs.rmSync(dir,{recursive:true,force:true})}
}
run().catch(e=>{console.error(e);process.exitCode=1})
