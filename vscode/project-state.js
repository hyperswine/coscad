// Canonical project snapshots and dependency-aware validation scheduling.
// No VS Code dependency: exercised with real files and controlled async jobs.
const fs = require('fs')
const path = require('path')
const { COSCADParser } = require('./coscad-parser')
function identity(file) {
  try { return fs.realpathSync(file) } catch { return path.resolve(file) }
}
function snapshot(root, documents) {
  const open = new Map(documents.filter(d => !d.isClosed && d.uri.scheme === 'file').map(d => [identity(d.fileName), d]))
  const dependencies = new Set(), files = [], visited = new Set()
  function visit(file) {
    const key = identity(file)
    if (visited.has(key)) return
    visited.add(key); dependencies.add(key)
    if (visited.size > 256) throw new Error('Project exceeds 256 source files')
    const doc = open.get(key)
    let text
    if (doc) { text = doc.getText(); files.push({ path: key, text }) }
    else { try { text = fs.readFileSync(file, 'utf8') } catch { return } }
    if (path.extname(file) !== '.assemble') return
    const ast = new COSCADParser().parse(text, { fileType: 'assemble' })
    for (const part of ast.body.filter(n => n.type === 'part_declaration')) visit(path.resolve(path.dirname(file), part.source))
  }
  visit(root.fileName)
  const input = JSON.stringify({ schema: 'coscad.project/1', files })
  if (input.length > 8 * 1024 * 1024) throw new Error('Project snapshot exceeds 8 Mi characters')
  return { dependencies, input }
}
class ValidationQueue {
  constructor({ documents, validate, clear = () => {}, onDependencies = () => {}, delay = 400 }) {
    Object.assign(this, { documents, validate, clear, onDependencies, delay })
    this.jobs = new Map(); this.dependencies = new Map(); this.disposed = false
  }
  graphChanged() { this.onDependencies(new Set([...this.dependencies.values()].flatMap(deps=>[...deps]))) }
  schedule(doc, immediate = false) {
    if (this.disposed || doc.isClosed || doc.uri.scheme !== 'file' || doc.languageId !== 'coscad') return
    const key = identity(doc.fileName), old = this.jobs.get(key)
    if (old) { clearTimeout(old.timer); old.controller.abort() }
    const job = { controller: new AbortController() }
    this.jobs.set(key, job)
    const current = () => !this.disposed && !doc.isClosed && this.jobs.get(key) === job && !job.controller.signal.aborted
    this.clear(doc)
    job.timer = setTimeout(async () => {
      if (!current()) return
      let project
      try {
        project = snapshot(doc, this.documents())
        // Retain previous dependencies until validation succeeds: a temporarily
        // malformed reference must not sever invalidation of its former child.
        this.dependencies.set(key, new Set([...(this.dependencies.get(key) || []), ...project.dependencies]))
        this.graphChanged()
        const deps = await this.validate(doc, project, job.controller.signal, current)
        if (current() && deps) {
          this.dependencies.set(key, new Set([...project.dependencies, ...deps.map(identity)]))
          this.graphChanged()
        }
      } catch (error) {
        if (current()) await this.validate(doc, { error }, job.controller.signal, current)
      }
    }, immediate ? 0 : this.delay)
  }
  changed(file, immediate = false) {
    const key = identity(file)
    for (const doc of this.documents()) {
      if (doc.isClosed || doc.uri.scheme !== 'file' || doc.languageId !== 'coscad') continue
      const root = identity(doc.fileName)
      if (root === key || this.dependencies.get(root)?.has(key)) this.schedule(doc, immediate)
      else if (!this.dependencies.has(root)) this.schedule(doc, immediate)
    }
  }
  close(doc) {
    const key = identity(doc.fileName), job = this.jobs.get(key)
    if (job) { clearTimeout(job.timer); job.controller.abort() }
    this.jobs.delete(key); this.dependencies.delete(key); this.graphChanged()
    this.changed(doc.fileName, true)
  }
  reset() {
    for (const job of this.jobs.values()) { clearTimeout(job.timer); job.controller.abort() }
    this.jobs.clear(); this.dependencies.clear(); this.graphChanged()
    for (const doc of this.documents()) this.schedule(doc, true)
  }
  dispose() {
    this.disposed = true
    for (const job of this.jobs.values()) { clearTimeout(job.timer); job.controller.abort() }
    this.jobs.clear(); this.dependencies.clear(); this.graphChanged()
  }
}
module.exports = { identity, snapshot, ValidationQueue }
