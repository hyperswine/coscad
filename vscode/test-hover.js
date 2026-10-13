const assert = require('assert/strict')
const fs = require('fs')
const path = require('path')
const {COSCADParser} = require('./coscad-parser')
const {hoverInfo} = require('./hover-info')
const parser = new COSCADParser()
const code = `// A named plate.
w = next / 2
next = 40
plate = box w 30 4 |> frame socket top top bak 0 0 0
post = box 6 6 20 |> frame foot bot bot bak
holes = zcyl 2 8 |> grid 2 3 10 12 |> hull
main = post |> mate plate @socket @foot (90 / 2) 0.2
`
assert.deepEqual(parser.getDiagnostics(code), [])
let count = 0
for (const token of parser.tokenize(code).filter(t => !['comment','newline'].includes(t.type))) {
  const hover = hoverInfo(code,token.position,{parser})
  assert(hover?.contents.value, `${token.value} at ${token.line}:${token.column}`)
  assert.equal(hover.range.start.character,token.column)
  assert.equal(hover.range.end.character,token.column+String(token.raw ?? token.value).length)
  count++
}
assert.match(hoverInfo(code,code.indexOf('=')).contents.value,/Right-hand side: \*\*Number/)
assert.match(hoverInfo(code,code.indexOf('w =')).contents.value,/= 20/)
assert.match(hoverInfo(code,code.indexOf('90')).contents.value,/degrees/)
assert.match(hoverInfo(code,code.indexOf('@socket')).contents.value,/Mating frame/)
assert.match(hoverInfo(code,code.indexOf('plate =')).contents.value,/Solid3D/)
const assembly=`base ← base.coscad ×1 material=pla
post ← post.coscad ×1 after=base
fixed base
mate post @foot base @socket roll=90 gap=0.2
fastener peg post base top inset=2
`
assert.deepEqual(parser.getDiagnostics(assembly,{fileType:'assemble'}),[])
assert.match(hoverInfo(assembly,assembly.indexOf('roll=')+4,{fileType:'assemble'}).contents.value,/Right-hand side: \*\*Number/)
assert.match(hoverInfo(assembly,assembly.indexOf('90'),{fileType:'assemble'}).contents.value,/degrees/)
for (const t of parser.tokenize(assembly).filter(t=>!['comment','newline'].includes(t.type))) {
  assert(hoverInfo(assembly,t.position,{fileType:'assemble'})?.contents.value, String(t.value));count++
}
for (const [source,pattern] of [
 ['a ← a.coscad ×1\nmate a @own b @target\n',/unknown part 'b'/],
 ['a ← a.coscad ×1\n',null],
 ['a ← a.coscad ×2\nfixed a\n',/×0 or ×1/],
 ['a ← a.coscad ×1\nfixed a\nfixed a\n',/duplicate fixed/],
 ['a ← a.coscad ×1\nmate a @foot a @top\n',/floating component/],
 ['a ← a.coscad ×1\nfixed a\nasm = a\n',/cannot be combined/],
 ['main = box 2 2 2 |> mate (box 2 2 2) @a @b 45\n',/Both roll and gap/],
]) {
 const messages=parser.getDiagnostics(source,{fileType:source.includes('←')?'assemble':'coscad'}).map(d=>d.message)
 if(pattern)assert(messages.some(m=>pattern.test(m)),messages.join('\n'))
 else assert.deepEqual(messages,[])
}
if(process.env.COSCAD_ROOT) {
 const root=process.env.COSCAD_ROOT
 const walk=p=>fs.readdirSync(p,{withFileTypes:true}).flatMap(d=>d.isDirectory()?(['archive','haskell'].includes(d.name)?[]:walk(path.join(p,d.name))):/\.(coscad|assemble)$/.test(d.name)?[path.join(p,d.name)]:[])
 let files=0
 for(const folder of ['examples','examples-next','models']) for(const file of walk(path.join(root,folder))) {
   const text=fs.readFileSync(file,'utf8'),fileType=file.endsWith('.assemble')?'assemble':'coscad'
   const errors=parser.getDiagnostics(text,{fileType})
   assert.deepEqual(errors,[],file+': '+errors.map(d=>d.message).join('; '))
   const tokens=parser.tokenize(text),ast=parser.parse(text,{fileType})
   for(const token of tokens.filter(t=>!['comment','newline','unknown'].includes(t.type))) {
     assert(hoverInfo(text,token.position,{fileType,parser,tokens,ast})?.contents.value,file+': '+token.value);count++
   }
   files++
 }
 console.log(`${files} current compiler files parse clean and have token hovers`)
}
console.log(`Hover coverage passed (${count} token positions)`)

// Fastener frames select the host hole, clamped head seat and nut holder.
const hardware = `base ← base.coscad ×1 preload=hand
post ← post.coscad ×1
asm = base ⊕ post
fastener M4x12 post base @socket seat=@head head=socket nut=hex pocket=@nut
`
assert.deepEqual(parser.getDiagnostics(hardware,{fileType:'assemble'}),[])
for (const [ref,owner] of [['@socket','base'],['@head','post'],['@nut','base']]) {
  const end=hardware.indexOf(ref)+ref.length
  assert.equal(parser.framePartAt(hardware,end),owner)
  const h=hoverInfo(hardware,end-1,{parser,fileType:'assemble',loadPart:p=>({ast:parser.parse(`main = box 20 20 4 |> frame ${ref.slice(1)} top top bak\n`)} )})
  assert.match(h.contents.value,new RegExp('in `'+owner+'.coscad`'))
}
assert.equal(parser.framePartAt(hardware.replace('pocket=@nut','pocket=clamped.@nut'),hardware.indexOf('pocket=')+'pocket=clamped.@nut'.length),'post')
assert.match(hoverInfo(hardware,hardware.indexOf('seat=')+4,{parser,fileType:'assemble'}).contents.value,/MatingFrame/)
for(const t of parser.tokenize(hardware).filter(t=>!['comment','newline'].includes(t.type))) assert(hoverInfo(hardware,t.position,{parser,fileType:'assemble'})?.contents.value,String(t.value))
