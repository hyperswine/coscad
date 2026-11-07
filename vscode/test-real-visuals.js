// Official VS Code extension-test runner. Run in an isolated --user-data-dir:
// code --extensionDevelopmentPath=.../vscode --extensionTestsPath=.../test-real-visuals.js
const assert = require('assert/strict')
const fs = require('fs'), path = require('path')
const vscode = require('vscode')

async function run() {
  const extension = vscode.extensions.getExtension('hyperswine.coscad-lsp')
  assert(extension, 'Development extension must be loaded')
  await extension.activate()
  const document = await vscode.workspace.openTextDocument(path.join(__dirname, 'examples/shape_visuals.coscad'))
  const text = document.getText()
  async function hovers(needle, predicate) {
    const position = document.positionAt(text.indexOf(needle))
    for (let attempt = 0; attempt < 60; attempt++) {
      const results = await vscode.commands.executeCommand('vscode.executeHoverProvider', document.uri, position)
      if (results?.some(predicate)) return results
      await new Promise(resolve => setTimeout(resolve, 100))
    }
    throw new Error('No expected hover: ' + needle)
  }
  const image = (results, id) => results.some(hover => hover.contents.some(item => item.value?.includes(`/${id}.svg`)))
  const checks = []
  for (const [needle, id] of [['xcyl radius', 'xcyl'], ['ycyl radius', 'ycyl'], ['zcyl radius', 'zcyl'], ['sphere 7', 'sphere'], ['◙ 16', 'rounded-box']]) {
    const result = await hovers(needle, hover => hover.contents.some(item => item.value?.includes(`/${id}.svg`)))
    assert(image(result, id))
    assert(result.some(hover => hover.contents.length >= 2), 'Image must accompany existing LSP documentation')
    for (const hover of result) for (const item of hover.contents) if (item.value?.includes(`/${id}.svg`)) {
      assert.equal(item.isTrusted, false)
      assert.equal(item.supportHtml, true)
      assert(item.value.includes('Primitive') && item.value.includes('illustration'), JSON.stringify(item.value))
    }
    checks.push(id)
  }
  const comment = document.positionAt(text.indexOf('xcyl, sphere'))
  const commentResult = await vscode.commands.executeCommand('vscode.executeHoverProvider', document.uri, comment)
  assert(!commentResult?.some(hover => hover.contents.some(item => item.value?.includes('.svg'))))
  assert.equal(document.getText(), text)
  const report = {passed: true, vscode: vscode.version, primitive_hovers: checks,
                  comment_has_no_image: true, original_documentation_preserved: true,
                  source_text_preserved: true}
  if (process.env.COSCAD_VISUAL_TEST_RESULT) fs.writeFileSync(process.env.COSCAD_VISUAL_TEST_RESULT, JSON.stringify(report, null, 2))
  console.log(JSON.stringify(report))
}
module.exports = {run}
