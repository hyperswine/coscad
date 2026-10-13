// CoScad front end for the language server: tokenizer, expression
// parser, numeric-binding classification, and diagnostics. Mirrors the
// Haskell compiler's grammar (coscad/src/Coscad/Parser.hs) and its
// checks (Dim.hs dimensionality, Codegen.hs loft-profile rules) so the
// editor flags what the compiler would reject, with the same wording.

class ParseError extends Error {
  constructor(message, token) {
    super(message)
    this.token = token
  }
}

class ExpressionParser {
  constructor(owner, tokens, mode, numericNames = new Set()) {
    this.owner = owner
    this.tokens = tokens
    this.mode = mode
    this.numericNames = numericNames
    this.index = 0
  }

  current() { return this.tokens[this.index] }
  peek(offset = 1) { return this.tokens[this.index + offset] }
  atEnd() { return this.index >= this.tokens.length }
  last() { return this.tokens.at(-1) }

  match(...values) {
    const token = this.current()
    if (token && values.includes(token.value)) {
      this.index++
      return token
    }
    return null
  }

  expect(value, message = `Expected '${value}'`) {
    const token = this.match(value)
    if (!token) throw new ParseError(message, this.current() || this.last())
    return token
  }

  // ---------------------------------------------------------------
  // numbers
  //   argument position: literal | numeric name | -atom | ( arithmetic )
  //   arithmetic: + - * / with parens and unary minus

  number(message = 'Expected a number') {
    const token = this.current()
    if (token?.type !== 'number') throw new ParseError(message, token || this.last())
    this.index++
    return token.value
  }

  isNumericName(token) {
    return token && ['identifier', 'anchor', 'pipeline_stage'].includes(token.type) && this.numericNames.has(token.value)
  }

  numArg(message = 'Expected a number (literal, numeric binding, or parenthesized arithmetic)') {
    const token = this.current()
    if (!token) throw new ParseError(message, this.last())
    if (token.type === 'number') {
      this.index++
      return this.node('number_literal', token, { value: token.value })
    }
    if (token.value === '-') {
      this.index++
      const operand = this.numArg(message)
      return this.node('number_expression', token, { operator: 'neg', operand })
    }
    if (token.value === '(') {
      const save = this.index
      this.index++
      try {
        const inner = this.numExpr()
        this.expect(')', "Expected ')' to close the arithmetic expression")
        return inner
      } catch (error) {
        if (!(error instanceof ParseError)) throw error
        this.index = save
        throw new ParseError(message, token)
      }
    }
    if (['identifier', 'anchor', 'pipeline_stage'].includes(token.type)) {
      if (this.numericNames.has(token.value)) {
        this.index++
        return this.node('number_reference', token, { name: token.value })
      }
      if (this.owner.declaredShapes?.has(token.value)) {
        throw new ParseError(`'${token.value}' is a shape, not a number`, token)
      }
      throw new ParseError(`undefined variable '${token.value}'`, token)
    }
    throw new ParseError(message, token)
  }

  // Parse a numeric argument if one starts here, else restore and return null.
  tryNumArg() {
    const save = this.index
    const token = this.current()
    if (!token) return null
    const starts = token.type === 'number' || token.value === '-' || token.value === '(' || this.isNumericName(token)
    if (!starts) return null
    try {
      return this.numArg()
    } catch (error) {
      if (!(error instanceof ParseError)) throw error
      this.index = save
      return null
    }
  }

  // Full arithmetic; `syntactic` accepts any identifier (used to classify
  // numeric definitions before names are known).
  numExpr(syntactic = false) {
    let left = this.numTerm(syntactic)
    for (;;) {
      const token = this.current()
      if (!token) break
      if (token.value === '+' || token.value === '-') {
        this.index++
        const right = this.numTerm(syntactic)
        left = this.node('number_expression', token, { operator: token.value, left, right })
      } else if (token.type === 'number' && token.raw?.startsWith('-')) {
        // `w -2` lexes as w, -2: read it as subtraction like the compiler does
        this.index++
        const right = this.node('number_literal', token, { value: -token.value })
        left = this.node('number_expression', token, { operator: '-', left, right })
      } else break
    }
    return left
  }

  numTerm(syntactic) {
    let left = this.numFactor(syntactic)
    while (this.current() && (this.current().value === '*' || this.current().value === '/')) {
      const token = this.current()
      this.index++
      const right = this.numFactor(syntactic)
      left = this.node('number_expression', token, { operator: token.value, left, right })
    }
    return left
  }

  numFactor(syntactic) {
    const token = this.current()
    if (!token) throw new ParseError('Expected a number', this.last())
    if (token.value === '-') {
      this.index++
      return this.node('number_expression', token, { operator: 'neg', operand: this.numFactor(syntactic) })
    }
    if (token.type === 'number') {
      this.index++
      return this.node('number_literal', token, { value: token.value })
    }
    if (token.value === '(') {
      this.index++
      const inner = this.numExpr(syntactic)
      this.expect(')', "Expected ')' to close the arithmetic expression")
      return inner
    }
    if (['identifier', 'anchor', 'pipeline_stage'].includes(token.type)) {
      if (syntactic || this.numericNames.has(token.value)) {
        this.index++
        return this.node('number_reference', token, { name: token.value })
      }
      if (this.owner.declaredShapes?.has(token.value)) throw new ParseError(`'${token.value}' is a shape, not a number`, token)
      throw new ParseError(`undefined variable '${token.value}'`, token)
    }
    throw new ParseError("Expected a number, a numeric binding, or '('", token)
  }

  parseNumericDefinition() {
    const node = this.numExpr(true)
    if (!this.atEnd()) throw new ParseError(`Unexpected '${this.current().value}'`, this.current())
    return node
  }

  // ---------------------------------------------------------------
  // shapes

  parse() {
    const node = this.pipeline()
    if (!this.atEnd()) throw new ParseError(`Unexpected '${this.current().value}'`, this.current())
    return node
  }

  pipeline() {
    let left = this.boolean()
    while (this.match('|>')) left = this.pipeStage(left)
    return left
  }

  pipeStage(input) {
    const stage = this.current()
    if (!stage || !this.owner.pipelineStages[stage.value]) throw new ParseError('Expected a pipeline stage', stage || this.last())
    this.index++
    const node = this.node('pipeline_operation', stage, { name: stage.value, input, params: [] })
    if (['x', 'y', 'z', 'rotx', 'roty', 'rotz', 'extrude'].includes(stage.value)) {
      node.params.push(this.numArg())
    } else if (['move', 'scale', 'mirror'].includes(stage.value)) {
      node.params.push(this.numArg(), this.numArg(), this.numArg())
    } else if (stage.value === 'anchor') {
      node.anchor = this.anchor()
    } else if (['datum', 'frame'].includes(stage.value)) {
      const name = this.current()
      if (!name || !/^[\p{L}_][\p{L}\p{N}_₀-₉]*$/u.test(name.value)) throw new ParseError('Expected an interface name', name || stage)
      this.index++
      node.frameName = name.value
      node.frameToken = name
      node.anchor = this.anchor()
      if (stage.value === 'frame') { node.normal = this.anchor(); node.up = this.anchor() }
      node.offset = this.tryOffset()
    } else if (['place', 'mate'].includes(stage.value)) {
      node.target = this.shapeArg()
      node.targetRef = this.anchor()
      node.ownRef = this.anchor()
      if (stage.value === 'place') node.offset = this.tryOffset()
      else {
        const roll = this.tryNumArg()
        if (roll) node.params.push(roll, this.numArg('Both roll and gap are required'))
      }
    } else if (stage.value === 'grid') {
      node.params.push(this.numArg(), this.numArg(), this.numArg(), this.numArg())
    } else if (stage.value === 'loft') {
      node.params.push(this.numArg())
      node.target = this.primary()
    } else if (['at', 'on', 'cutat'].includes(stage.value)) {
      node.anchor = this.anchor()
      node.offset = this.tryOffset()
      node.target = this.shapeArg()
    } else if (['add', 'cut', 'isect', 'hull', 'mink'].includes(stage.value)) {
      if (!(stage.value === 'hull' && (!this.current() || ['|>', ')'].includes(this.current().value)))) node.target = this.shapeArg()
    } else {
      throw new ParseError(`Unknown pipeline stage '${stage.value}'`, stage)
    }
    return node
  }

  // optional `dx dy dz` before the child of at/on/cutat
  tryOffset() {
    const save = this.index
    const a = this.tryNumArg()
    const b = a && this.tryNumArg()
    const c = b && this.tryNumArg()
    if (a && b && c) return [a, b, c]
    this.index = save
    return null
  }

  boolean() {
    let left = this.attachment()
    while (this.current()?.type === 'boolean') {
      const operator = this.current()
      this.owner.assertMode(operator, this.mode)
      this.index++
      left = this.node('boolean_operation', operator, {
        operator: this.owner.booleanOps[operator.value].name,
        symbol: operator.value,
        left,
        right: this.attachment()
      })
    }
    return left
  }

  attachment() {
    let left = this.transform()
    while (this.current()?.type === 'attachment') {
      const operator = this.current()
      this.owner.assertMode(operator, this.mode)
      this.index++
      left = this.node('attachment_operation', operator, {
        operator: this.owner.attachmentOps[operator.value].name,
        symbol: operator.value,
        anchor: this.anchor(),
        offset: null,
        left,
        right: this.transform()
      })
    }
    return left
  }

  transform() {
    const token = this.current()
    if (token?.type !== 'transformation') return this.primary()
    this.owner.assertMode(token, this.mode)
    this.index++
    const info = this.owner.transformations[token.value]
    if (info.form === 'loft') return this.loftPairs(token)
    const node = this.node('transformation', token, {
      name: info.name,
      symbol: token.value,
      params: [],
      target: null
    })

    if (info.form === 'vector') {
      if (this.match('.')) {
        const axis = this.current()
        if (!axis || !['identifier', 'pipeline_stage'].includes(axis.type) || !['x', 'y', 'z'].includes(axis.value)) {
          throw new ParseError('Expected transform axis x, y, or z', axis || token)
        }
        this.index++
        node.axis = axis.value
        if (token.value !== 'Mirror') node.params.push(this.numArg())
      } else {
        node.params.push(...this.tuple3())
      }
      node.target = this.shapeArg()
      return node
    }
    if (info.form === 'anchor') {
      node.anchor = this.anchor()
      node.target = this.shapeArg()
      return node
    }
    if (info.form === 'binary') {
      node.left = this.primary()
      node.right = this.shapeArg()
      return node
    }
    for (let count = 0; count < info.arity; count++) node.params.push(this.numArg())
    node.target = this.shapeArg()
    return node
  }

  // loft z0 p0 z1 p1 ...
  loftPairs(token) {
    const profiles = []
    for (;;) {
      const z = this.tryNumArg()
      if (!z) break
      profiles.push({ z, shape: this.primary() })
    }
    if (profiles.length < 2) {
      throw new ParseError('loft needs at least two profiles: loft z0 profile0 z1 profile1 ...', token)
    }
    return this.node('loft', token, { symbol: token.value, profiles })
  }

  shapeArg() {
    return this.match('$') ? this.pipeline() : this.primary()
  }

  primary() {
    const token = this.current()
    if (!token) throw new ParseError('Expected a shape expression', this.last())
    if (this.match('(')) {
      const node = this.pipeline()
      this.expect(')', "Expected ')' to close expression")
      return node
    }
    if (token.type === 'shape3d' || token.type === 'shape2d') {
      this.owner.assertMode(token, this.mode)
      this.index++
      const info = this.owner.allSymbols[token.value]
      const node = this.node('shape', token, { name: info.name, symbol: token.value, params: [], dim: token.type === 'shape2d' ? 2 : 3 })
      if (info.variadic) {
        for (;;) {
          const value = this.tryNumArg()
          if (!value) break
          node.params.push(value)
        }
        const points = node.params.length / 2
        if (node.params.length < 8 || node.params.length % 2 || points % 3 !== 1) {
          throw new ParseError(`bezier needs 3k+1 control points as x y pairs (got ${node.params.length} numbers)`, token)
        }
      } else {
        for (let count = 0; count < info.arity; count++) node.params.push(this.numArg())
      }
      return node
    }
    if (['identifier', 'anchor', 'pipeline_stage'].includes(token.type)) {
      this.index++
      return this.node('variable_reference', token, { name: token.value })
    }
    throw new ParseError(`Expected a shape expression, found '${token.value}'`, token)
  }

  tuple3() {
    this.expect('(', "Expected a '(x, y, z)' tuple")
    const values = [this.numArg()]
    this.expect(',', "Expected ',' in tuple")
    values.push(this.numArg())
    this.expect(',', "Expected ',' in tuple")
    values.push(this.numArg())
    this.expect(')', "Expected ')' after tuple")
    return values
  }

  anchor() {
    const words = []
    let token = this.current()
    if (this.match('@')) {
      token = this.current()
      if (!token || !/^[\p{L}_][\p{L}\p{N}_₀-₉]*$/u.test(token.value)) throw new ParseError('Expected a named reference after @', token || this.last())
      this.index++
      return '@' + token.value
    }
    if (token?.type !== 'anchor') throw new ParseError('Expected an anchor (top bot lft rt fwd bak ctr, combinable with +)', token || this.last())
    words.push(token.value)
    this.index++
    while (this.match('+')) {
      token = this.current()
      if (token?.type !== 'anchor') throw new ParseError('Expected an anchor after +', token)
      words.push(token.value)
      this.index++
    }
    return words.join('+')
  }

  node(type, token, values) {
    return { type, ...values, position: token.position, line: token.line, column: token.column }
  }
}

class COSCADParser {
  constructor() {
    const glyph = ['glyph', 'legacy']
    const simple = ['simple', 'legacy']
    this.shapes3d = {
      '■': { name: 'cube', arity: 1, modes: glyph },
      '●': { name: 'sphere', arity: 1, modes: glyph },
      '◎': { name: 'cylinder', arity: 2, modes: glyph },
      '▻': { name: 'cone', arity: 2, modes: glyph },
      '▬': { name: 'box', arity: 3, modes: glyph },
      '⎏': { name: 'prism', arity: 3, modes: glyph },
      '▣': { name: 'cuboid', arity: 4, modes: glyph, bosl2: true },
      '◙': { name: 'cuboid', arity: 4, modes: glyph, bosl2: true },
      '⌭': { name: 'cyl', arity: 3, modes: glyph, bosl2: true },
      '⌽': { name: 'cyl', arity: 3, modes: glyph, bosl2: true },
      '⊚': { name: 'tube', arity: 3, modes: glyph, bosl2: true },
      '⏢': { name: 'prismoid', arity: 5, modes: glyph, bosl2: true },
      '◉': { name: 'torus', arity: 2, modes: glyph, bosl2: true },
      '⊿': { name: 'wedge', arity: 3, modes: glyph, bosl2: true },
      xcyl: { name: 'xcyl', arity: 2 }, ycyl: { name: 'ycyl', arity: 2 }, zcyl: { name: 'zcyl', arity: 2 },
      cube: { name: 'cube', arity: 1 }, box: { name: 'box', arity: 3 }, sphere: { name: 'sphere', arity: 1 },
      cyl: { name: 'cyl', arity: 2 }, tube: { name: 'tube', arity: 3 }, torus: { name: 'torus', arity: 2 }, wedge: { name: 'wedge', arity: 3 },
      Sphere: { name: 'sphere', arity: 1, modes: simple }, Cube: { name: 'cube', arity: 1, modes: simple },
      Box: { name: 'box', arity: 3, modes: simple }, Cylinder: { name: 'cylinder', arity: 2, modes: simple },
      Cone: { name: 'cone', arity: 2, modes: simple }, Tube: { name: 'tube', arity: 3, modes: simple },
      Torus: { name: 'torus', arity: 2, modes: simple }, Wedge: { name: 'wedge', arity: 3, modes: simple },
      Prismoid: { name: 'prismoid', arity: 5, modes: simple }
    }
    this.shapes2d = {
      '△': { name: 'triangle', arity: 1, modes: glyph }, '⬠': { name: 'pentagon', arity: 1, modes: glyph },
      '⭘': { name: 'circle', arity: 1, modes: glyph }, '✎': { name: 'bezier', variadic: true, modes: glyph },
      Circle: { name: 'circle', arity: 1, modes: simple }, Triangle: { name: 'triangle', arity: 1, modes: simple },
      Pentagon: { name: 'pentagon', arity: 1, modes: simple }, Bezier: { name: 'bezier', variadic: true, modes: simple }
    }
    this.transformations = {
      'χ': { name: 'Tx', arity: 1, modes: glyph }, 'ψ': { name: 'Ty', arity: 1, modes: glyph },
      'ζ': { name: 'Tz', arity: 1, modes: glyph }, 'θ': { name: 'Rx', arity: 1, modes: glyph },
      'ϕ': { name: 'Ry', arity: 1, modes: glyph }, 'ω': { name: 'Rz', arity: 1, modes: glyph },
      '⬈': { name: 'Scale', arity: 3, modes: glyph }, '⇋': { name: 'Mirror', arity: 3, modes: glyph },
      '⮕': { name: 'Extrude', arity: 1, modes: glyph }, '⚓': { name: 'Anchor', form: 'anchor', modes: glyph },
      '⟰': { name: 'Loft', form: 'loft', modes: glyph },
      loft: { name: 'Loft', form: 'loft' },
      Translate: { name: 'Translate', form: 'vector', modes: simple }, Rotate: { name: 'Rotate', form: 'vector', modes: simple },
      Scale: { name: 'Scale', form: 'vector', modes: simple }, Mirror: { name: 'Mirror', form: 'vector', modes: simple },
      Extrude: { name: 'Extrude', arity: 1, modes: simple }, Anchor: { name: 'Anchor', form: 'anchor', modes: simple },
      Loft: { name: 'Loft', form: 'loft', modes: simple },
      Hull: { name: 'Hull', form: 'binary', modes: simple }, Union: { name: 'Union', form: 'binary', modes: simple },
      Intersect: { name: 'Intersect', form: 'binary', modes: simple }, Minkowski: { name: 'Minkowski', form: 'binary', modes: simple },
      Offset: { name: 'Offset', arity: 1, modes: simple }
    }
    this.booleanOps = {
      '⊖': { name: 'Diff', modes: glyph }, '⊝': { name: 'Diff', modes: glyph },
      '⊕': { name: 'Union', modes: glyph }, '⊛': { name: 'Union', modes: glyph },
      '∩': { name: 'Intersection', modes: glyph }, '⇓': { name: 'Hull', modes: glyph },
      '⊞': { name: 'Minkowski', modes: glyph }, '↯': { name: 'Offset', modes: glyph },
      '*': { name: 'Union', modes: simple }, '-': { name: 'Diff', modes: simple }
    }
    this.advancedOps = Object.fromEntries(Object.entries(this.booleanOps).filter(([key]) => ['∩', '⇓', '⊞', '↯'].includes(key)))
    this.attachmentOps = { '⌖': { name: 'Position', modes: glyph }, '⋈': { name: 'AttachTo', modes: glyph } }
    this.composition = { '|>': { name: 'pipe' } }
    this.pipelineStages = Object.fromEntries(
      ['x', 'y', 'z', 'rotx', 'roty', 'rotz', 'move', 'scale', 'mirror', 'extrude', 'loft', 'anchor', 'at', 'on', 'cutat', 'add', 'cut', 'isect', 'hull', 'mink', 'datum', 'frame', 'place', 'mate', 'grid']
        .map(name => [name, { name }])
    )
    this.anchors = new Set(['top', 'up', 'bot', 'dn', 'down', 'rt', 'right', 'lft', 'left', 'fwd', 'front', 'bak', 'back', 'ctr', 'center'])
    this.allSymbols = { ...this.shapes3d, ...this.shapes2d, ...this.pipelineStages, ...this.transformations, ...this.booleanOps, ...this.attachmentOps, ...this.composition }
    this.declaredShapes = new Set()
  }

  tokenize(text) {
    const tokens = []
    let index = 0
    let line = 0
    let column = 0
    const push = (type, value, position = index, tokenLine = line, tokenColumn = column, extra = {}) =>
      tokens.push({ type, value, position, line: tokenLine, column: tokenColumn, ...extra })
    while (index < text.length) {
      const char = text[index]
      if (char === '\r' && text[index + 1] === '\n') {
        push('newline', '\n'); index += 2; line++; column = 0; continue
      }
      if (char === '\n' || char === '\r') {
        push('newline', '\n'); index++; line++; column = 0; continue
      }
      if (/\s/.test(char)) { index++; column++; continue }
      if (char === '/' && text[index + 1] === '/') {
        const start = index; const startColumn = column
        while (index < text.length && !['\n', '\r'].includes(text[index])) { index++; column++ }
        push('comment', text.slice(start, index), start, line, startColumn); continue
      }
      if (char === '!' && (text.startsWith('!glyph', index) || text.startsWith('!simple', index))) {
        const value = text.startsWith('!glyph', index) ? '!glyph' : '!simple'
        push('pragma', value); index += value.length; column += value.length; continue
      }
      if (text.startsWith('|>', index)) { push('composition', '|>'); index += 2; column += 2; continue }
      if (/\d/.test(char) || (char === '-' && /[\d.]/.test(text[index + 1] || '')) || (char === '.' && /\d/.test(text[index + 1] || ''))) {
        const start = index; const startColumn = column
        const match = text.slice(index).match(/^-?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?/)
        if (match) {
          index += match[0].length; column += match[0].length
          push('number', Number(match[0]), start, line, startColumn, { raw: match[0] }); continue
        }
      }
      if (/\p{L}|_/u.test(char)) {
        const start = index; const startColumn = column
        while (index < text.length && /[\p{L}\p{N}_₀-₉]/u.test(text[index])) { index++; column++ }
        const value = text.slice(start, index)
        let type = 'identifier'
        if (this.shapes3d[value]) type = 'shape3d'
        else if (this.shapes2d[value]) type = 'shape2d'
        else if (this.transformations[value]) type = 'transformation'
        else if (this.pipelineStages[value]) type = 'pipeline_stage'
        else if (this.anchors.has(value)) type = 'anchor'
        push(type, value, start, line, startColumn); continue
      }
      const punctuation = {
        '=': 'assignment', '←': 'part_arrow', '×': 'count', '▽': 'orientation', '(': 'lparen', ')': 'rparen',
        ',': 'comma', '.': 'dot', '+': 'plus', '$': 'application', '*': 'boolean', '-': 'boolean', '/': 'slash', '#': 'hash', '@': 'reference'
      }
      if (punctuation[char]) { push(punctuation[char], char); index++; column++; continue }
      if (this.allSymbols[char]) { push(this.getSymbolType(char), char); index++; column++; continue }
      push('unknown', char); index++; column++
    }
    return tokens
  }

  getSymbolType(symbol) {
    if (this.shapes3d[symbol]) return 'shape3d'
    if (this.shapes2d[symbol]) return 'shape2d'
    if (this.transformations[symbol]) return 'transformation'
    if (this.attachmentOps[symbol]) return 'attachment'
    if (this.booleanOps[symbol]) return 'boolean'
    if (this.composition[symbol]) return 'composition'
    if (this.pipelineStages[symbol]) return 'pipeline_stage'
    return 'unknown'
  }

  assertMode(token, mode) {
    const modes = this.allSymbols[token.value]?.modes
    if (modes && !modes.includes(mode)) throw new ParseError(`'${token.value}' is not available in !${mode} mode`, token)
  }

  parse(text, options = {}) {
    const ast = { type: 'program', mode: 'legacy', fileType: options.fileType || 'coscad', body: [], errors: [], numericNames: new Set(), failedNames: new Set() }
    const statements = this.logicalStatements(this.tokenize(text), ast)
    const pragma = statements.find(statement => statement[0]?.type === 'pragma')
    if (pragma) ast.mode = pragma[0].value.slice(1)

    // First pass: classify statements; numeric candidates are parsed as
    // arithmetic without knowing any names yet.
    const entries = []
    for (const tokens of statements) {
      const first = tokens[0]
      if (first.type === 'pragma') {
        entries.push({ node: { type: 'pragma', mode: first.value.slice(1), position: first.position, line: first.line, column: first.column } })
        continue
      }
      const identifierLike = ['identifier', 'anchor', 'pipeline_stage'].includes(first.type)
      if (identifierLike && tokens[1]?.type === 'assignment') {
        const entry = { name: first.value, first, tokens }
        try {
          entry.numeric = new ExpressionParser(this, tokens.slice(2), ast.mode).parseNumericDefinition()
          entry.numericRefs = this.collectReferences(entry.numeric).map(reference => reference.name)
        } catch (error) {
          if (!(error instanceof ParseError)) throw error
        }
        entries.push(entry)
      } else {
        entries.push({ tokens, first })
      }
    }

    // A candidate is a number when everything it references is a number
    // (numbers only depend on numbers; they resolve first, like the compiler).
    // An undeclared name does not demote a candidate to a shape: it stays a
    // number and the reference is reported as undefined (as the compiler does).
    const candidates = entries.filter(entry => entry.numeric)
    const declaredNames = new Set(entries.filter(entry => entry.name).map(entry => entry.name)
      .concat(entries.filter(entry => !entry.name && entry.tokens?.[1]?.type === 'part_arrow').map(entry => entry.first.value)))
    const numericNames = new Set()
    for (let changed = true; changed;) {
      changed = false
      for (const entry of candidates) {
        if (numericNames.has(entry.name)) continue
        const arithmetic = entry.numeric.type !== 'number_reference' // `a = b` alone could be a shape alias
        const resolvable = name => numericNames.has(name) || (arithmetic && !declaredNames.has(name))
        if (entry.numericRefs.every(resolvable)) {
          numericNames.add(entry.name)
          changed = true
        }
      }
    }
    ast.numericNames = numericNames
    this.declaredShapes = new Set(
      entries.filter(entry => entry.name && !numericNames.has(entry.name)).map(entry => entry.name)
        .concat(entries.filter(entry => !entry.name && entry.first && entry.tokens?.[1]?.type === 'part_arrow').map(entry => entry.first.value))
    )

    // Second pass: real parse with the numeric names known.
    const numericValues = new Map()
    for (const entry of entries) {
      if (entry.node) { ast.body.push(entry.node); continue }
      const { first, tokens } = entry
      try {
        if (entry.name && numericNames.has(entry.name)) {
          const value = this.evaluate(entry.numeric, numericValues)
          numericValues.set(entry.name, value)
          ast.body.push({
            type: 'variable_declaration', kind: 'number', name: entry.name, value: entry.numeric, numericValue: value,
            position: first.position, line: first.line, column: first.column
          })
        } else {
          ast.body.push(this.parseStatement(tokens, ast.mode, numericNames))
        }
      } catch (error) {
        if (error instanceof ParseError) {
          ast.errors.push(error)
          if (entry.name) ast.failedNames.add(entry.name)
        } else throw error
      }
    }
    // Resolve forward numeric bindings for hover values as well as parsing.
    const numericDefs = new Map(ast.body.filter(n => n.kind === 'number').map(n => [n.name, n]))
    const resolveNumber = (name, seen = new Set()) => {
      if (seen.has(name) || !numericDefs.has(name)) return NaN
      seen.add(name)
      const node = numericDefs.get(name)
      const env = new Map(this.collectReferences(node.value).map(r => [r.name, resolveNumber(r.name, new Set(seen))]))
      return this.evaluate(node.value, env)
    }
    for (const [name, node] of numericDefs) node.numericValue = resolveNumber(name)
    return ast
  }

  evaluate(node, env) {
    if (!node) return NaN
    switch (node.type) {
      case 'number_literal': return node.value
      case 'number_reference': return env.has(node.name) ? env.get(node.name) : NaN
      case 'number_expression': {
        if (node.operator === 'neg') return -this.evaluate(node.operand, env)
        const a = this.evaluate(node.left, env)
        const b = this.evaluate(node.right, env)
        return node.operator === '+' ? a + b : node.operator === '-' ? a - b : node.operator === '*' ? a * b : a / b
      }
      default: return NaN
    }
  }

  logicalStatements(tokens, ast) {
    const lines = []
    let line = []
    for (const token of tokens) {
      if (token.type === 'newline') { lines.push(line); line = [] } else line.push(token)
    }
    if (line.length) lines.push(line)
    // A line starting with |> or a boolean glyph continues the previous
    // statement; comment-only lines in between do not break the chain.
    const continues = token => token.type === 'composition' || (token.type === 'boolean' && token.value !== '*' && token.value !== '-')
    const statements = []
    for (const tokensOnLine of lines) {
      for (const comment of tokensOnLine.filter(token => token.type === 'comment')) {
        ast.body.push({ type: 'comment', value: comment.value, position: comment.position, line: comment.line, column: comment.column })
      }
      const code = tokensOnLine.filter(token => token.type !== 'comment')
      if (!code.length) continue
      if (continues(code[0]) && statements.length) statements.at(-1).push(...code)
      else statements.push(code)
    }
    return statements
  }

  parseStatement(tokens, mode, numericNames = new Set()) {
    const first = tokens[0]
    const identifierLike = ['identifier', 'anchor', 'pipeline_stage'].includes(first.type)
    if (identifierLike && first.value === 'plate' && tokens[1]?.type !== 'assignment') return this.parsePlate(tokens)
    if (identifierLike && first.value === 'clearance' && tokens[1]?.type !== 'assignment') return this.parseClearance(tokens)
    if (identifierLike && first.value === 'fastener' && tokens[1]?.type !== 'assignment') return this.parseFastener(tokens)
    if (identifierLike && first.value === 'plan' && tokens[1]?.type !== 'assignment') return this.parsePlan(tokens)
    if (identifierLike && ['fixed', 'mate'].includes(first.value) && tokens[1]?.type !== 'assignment') return this.parseConstraint(tokens)
    if (identifierLike && tokens[1]?.type === 'part_arrow') return this.parsePart(tokens)
    if (identifierLike && tokens[1]?.type === 'assignment') {
      return {
        type: 'variable_declaration', kind: 'shape', name: first.value,
        value: new ExpressionParser(this, tokens.slice(2), mode, numericNames).parse(),
        position: first.position, line: first.line, column: first.column
      }
    }
    throw new ParseError('Expected a definition (name = expression), part (name ← file), plate, or clearance line', first)
  }

  parsePlate(tokens) {
    const parser = new ExpressionParser(this, tokens.slice(1), 'legacy')
    const values = [parser.number(), parser.number(), parser.atEnd() ? 6 : parser.number()]
    if (!parser.atEnd()) throw new ParseError('Unexpected value in plate declaration', parser.current())
    return { type: 'plate_declaration', width: values[0], depth: values[1], margin: values[2], position: tokens[0].position, line: tokens[0].line, column: tokens[0].column }
  }

  parseConstraint(tokens) {
    const parser = new ExpressionParser(this, tokens.slice(1), 'legacy')
    const readName = () => {
      const t = parser.current()
      if (!t || !/^[\p{L}_][\p{L}\p{N}_₀-₉]*$/u.test(t.value)) throw new ParseError('Expected a part alias', t || tokens[0])
      parser.index++
      return { text: t.value, token: t }
    }
    const child = readName()
    const node = { type: 'constraint_declaration', name: tokens[0].value, refs: [child], position: tokens[0].position, line: tokens[0].line, column: tokens[0].column }
    if (node.name === 'fixed') {
      node.pose = parser.atEnd() ? [0,0,0,0,0,0] : Array.from({length:6}, () => parser.number('fixed needs x y z rx ry rz'))
    } else {
      node.ownRef = parser.anchor()
      node.refs.push(readName())
      node.targetRef = parser.anchor()
      if (![node.ownRef, node.targetRef].every(r => r.startsWith('@'))) throw new ParseError('mate needs oriented @frame references', tokens[0])
      node.options = []
      while (!parser.atEnd()) {
        const key = parser.current(); parser.index++
        if (!['roll','gap'].includes(key.value) || node.options.some(o => o.key === key.value)) throw new ParseError('mate accepts unique roll= and gap= options', key)
        parser.expect('=')
        const value = parser.number()
        if (!Number.isFinite(value)) throw new ParseError('mate options must be finite', key)
        node.options.push({key:key.value,value})
      }
    }
    if (!parser.atEnd()) throw new ParseError('Unexpected value in constraint', parser.current())
    return node
  }

  parseClearance(tokens) {
    const parser = new ExpressionParser(this, tokens.slice(1), 'legacy')
    const value = parser.number()
    if (!parser.atEnd()) throw new ParseError('Unexpected value in clearance declaration', parser.current())
    return { type: 'clearance_declaration', value, position: tokens[0].position, line: tokens[0].line, column: tokens[0].column }
  }

  framePartAt(text, offset) {
    const line = text.slice(text.lastIndexOf('\n', offset - 1) + 1, offset)
    const fast = line.match(/^\s*fastener\s+\S+\s+(\S+)\s+(\S+)\s+(.*)$/u)
    if (fast) {
      const option = fast[3].match(/(seat|pocket)=(clamped\.|host\.)?@[\p{L}\p{N}_]*$/u)
      if (option) return option[1] === 'seat' || option[2] === 'clamped.' ? fast[1] : fast[2]
      if (/^@[\p{L}\p{N}_]*$/u.test(fast[3])) return fast[2]
    }
    return line.match(/([\p{L}][\p{L}\p{N}_]*)\s+@[\p{L}\p{N}_]*$/u)?.[1] || null
  }

  // fastener M5x10 clamped host face [×n] [key=value ...]
  parseFastener(tokens) {
    const word = index => tokens[index] ? String(tokens[index].raw ?? tokens[index].value) : null
    // instance refs may carry `#n`: the tokenizer splits `br#1` into br, #, 1
    const refs = []
    let index = 1
    const readRef = what => {
      if (!tokens[index] || ['count', 'assignment'].includes(tokens[index].type)) throw new ParseError(`fastener: expected ${what}`, tokens[index] || tokens[0])
      const start = tokens[index]
      let text = word(index++)
      if (text === '@' && tokens[index]?.type === 'identifier') text += word(index++)
      if (tokens[index]?.value === '#' && tokens[index + 1]?.type === 'number') { text += '#' + word(index + 1); index += 2 }
      refs.push({ text, token: start })
      return text
    }
    const spec = readRef('a screw spec such as M5x10')
    if (!/^(m\d+(\.\d+)?x\d+(\.\d+)?|peg)$/i.test(spec)) throw new ParseError(`fastener: cannot read screw spec '${spec}' (expected e.g. M5x10, or peg)`, refs[0].token)
    const clamped = readRef('the clamped part')
    const host = readRef('the host part')
    const faceToken = tokens[index]
    const face = readRef('the host face (top bot lft rt fwd bak)')
    if (!this.anchors.has(face) && !/^@[\p{L}][\p{L}\p{N}_]*$/u.test(face)) throw new ParseError(`fastener: unknown face '${face}' (top bot lft rt fwd bak)`, faceToken)
    const node = {
      type: 'fastener_declaration', spec, clamped, host, face, count: 1, options: [],
      faceToken, refs: refs.slice(1, 3), position: tokens[0].position, line: tokens[0].line, column: tokens[0].column
    }
    while (index < tokens.length) {
      if (tokens[index].type === 'count') {
        const count = tokens[index + 1]
        if (count?.type !== 'number' || !Number.isInteger(count.value) || count.value < 1) throw new ParseError('Screw count must be a positive integer', count || tokens[index])
        node.count = count.value; index += 2; continue
      }
      if (tokens[index + 1]?.type === 'assignment') {
        const key = word(index)
        let value = ''
        index += 2
        while (index < tokens.length && tokens[index + 1]?.type !== 'assignment' && tokens[index].type !== 'count') value += word(index++)
        if (!['at', 'nut', 'head', 'seat', 'through', 'torque', 'pocket', 'inset'].includes(key)) throw new ParseError(`Unknown fastener option '${key}' (at nut head through torque pocket inset)`, tokens[index - 2])
        node.options.push({ key, value }); continue
      }
      throw new ParseError(`Unexpected fastener option '${word(index)}'`, tokens[index])
    }
    return node
  }

  // plan key=value ...
  parsePlan(tokens) {
    const node = { type: 'plan_declaration', options: [], position: tokens[0].position, line: tokens[0].line, column: tokens[0].column }
    let index = 1
    while (index < tokens.length) {
      if (tokens[index + 1]?.type !== 'assignment') throw new ParseError('plan takes key=value options (beam rest flip vertical driver shrink sibling loose astar)', tokens[index])
      const key = String(tokens[index].raw ?? tokens[index].value)
      let value = ''
      index += 2
      while (index < tokens.length && tokens[index + 1]?.type !== 'assignment') value += String(tokens[index].raw ?? tokens[index].value), index++
      if (!['beam', 'rest', 'flip', 'vertical', 'driver', 'shrink', 'sibling', 'loose', 'astar'].includes(key)) throw new ParseError(`Unknown plan option '${key}'`, tokens[index - 2])
      node.options.push({ key, value })
    }
    return node
  }

  parsePart(tokens) {
    let index = 2
    const sourceTokens = []
    const startsOption = () => ['count', 'orientation'].includes(tokens[index]?.type) ||
      (tokens[index + 1]?.type === 'assignment')
    while (index < tokens.length && !startsOption()) sourceTokens.push(tokens[index++])
    if (!sourceTokens.length) throw new ParseError('Expected a .coscad or .assemble source path', tokens[index] || tokens[1])
    const node = {
      type: 'part_declaration', name: tokens[0].value, source: sourceTokens.map(token => token.raw ?? token.value).join(''),
      count: 1, orientation: null, hints: [], position: tokens[0].position, line: tokens[0].line, column: tokens[0].column,
      sourcePosition: sourceTokens[0].position, sourceLine: sourceTokens[0].line, sourceColumn: sourceTokens[0].column
    }
    while (index < tokens.length) {
      if (tokens[index].type === 'count') {
        const count = tokens[index + 1]
        if (count?.type !== 'number' || !Number.isInteger(count.value) || count.value < 0) throw new ParseError('Part count must be a non-negative integer', count || tokens[index])
        node.count = count.value; index += 2; continue
      }
      if (tokens[index].type === 'orientation') {
        const parser = new ExpressionParser(this, tokens.slice(index + 1), 'legacy')
        node.orientation = parser.anchor(); index += parser.index + 1; continue
      }
      if (tokens[index + 1]?.type === 'assignment') {
        const key = tokens[index].value
        index += 2
        let value = ''
        while (index < tokens.length && !startsOption()) { value += String(tokens[index].raw ?? tokens[index].value); index++ }
        if (!value) throw new ParseError('Expected a hint value', tokens[index - 1])
        node.hints.push({ key, value }); continue
      }
      throw new ParseError(`Unexpected part option '${tokens[index].value}'`, tokens[index])
    }
    if (!/\.(coscad|assemble)$/i.test(node.source)) throw new ParseError('Part source must end in .coscad or .assemble', sourceTokens[0])
    return node
  }

  getSymbolAtPosition(text, position) {
    const token = this.tokenize(text).find(candidate => candidate.position <= position && position < candidate.position + String(candidate.raw ?? candidate.value).length)
    return token ? { token, info: this.allSymbols[token.value] } : null
  }

  // ---------------------------------------------------------------
  // diagnostics

  getDiagnostics(text, options = {}) {
    const ast = this.parse(text, options)
    const diagnostics = ast.errors.map(error => this.diagnostic(error.message, error.token))
    const declarations = ast.body.filter(node => ['variable_declaration', 'part_declaration'].includes(node.type))
    const byName = new Map(declarations.map(node => [node.name, node]))
    const isNumber = name => ast.numericNames.has(name)

    for (const declaration of declarations) {
      const earlier = declarations.find(other => other !== declaration && other.name === declaration.name && other.position < declaration.position)
      if (earlier) {
        diagnostics.push(this.diagnostic(`duplicate definition of '${declaration.name}' (first defined at line ${earlier.line + 1})`, declaration))
      }
    }

    // references: shape names in shape positions, numbers in numeric positions
    const dependencies = new Map()
    for (const declaration of ast.body.filter(node => node.type === 'variable_declaration')) {
      const references = this.collectReferences(declaration.value)
      dependencies.set(declaration.name, references.map(reference => reference.name))
      for (const reference of references) {
        if (!byName.has(reference.name)) {
          diagnostics.push(this.diagnostic(`undefined variable '${reference.name}'`, reference))
        } else if (reference.type === 'variable_reference' && isNumber(reference.name)) {
          diagnostics.push(this.diagnostic(`'${reference.name}' is a number, not a shape (numbers are used as arguments: ● ${reference.name}, box ${reference.name} ...)`, reference))
        } else if (reference.type === 'number_reference' && !isNumber(reference.name)) {
          diagnostics.push(this.diagnostic(`'${reference.name}' is a shape, not a number`, reference))
        }
      }
    }

    // fastener references must name placed parts (bare name or name#n)
    const partNames = new Set(declarations.filter(node => node.type === 'part_declaration').map(node => node.name))
    const constraints = ast.body.filter(n => n.type === 'constraint_declaration')
    if (constraints.length && byName.has('asm')) diagnostics.push(this.diagnostic('assembly constraints cannot be combined with an asm definition', byName.get('asm')))
    const grounded = new Set(), graph = new Map([...partNames].map(n => [n, new Set()]))
    for (const c of constraints) {
      for (const ref of c.refs) if (!partNames.has(ref.text)) diagnostics.push(this.diagnostic(`unknown part '${ref.text}' in constraint`, ref.token))
      if (c.name === 'fixed') {
        if (grounded.has(c.refs[0].text)) diagnostics.push(this.diagnostic('duplicate fixed pose', c))
        grounded.add(c.refs[0].text)
      } else {
        const [a,b] = c.refs.map(r => r.text)
        graph.get(a)?.add(b); graph.get(b)?.add(a)
      }
    }
    if (constraints.length) {
      const reached = new Set(grounded), queue = [...grounded]
      for (let i=0;i<queue.length;i++) for (const n of graph.get(queue[i]) || []) if (!reached.has(n)) { reached.add(n); queue.push(n) }
      for (const d of declarations.filter(n => n.type === 'part_declaration')) {
        if (d.count > 1) diagnostics.push(this.diagnostic('constrained aliases use ×0 or ×1; declare one alias per physical instance', d))
        if (!reached.has(d.name)) diagnostics.push(this.diagnostic(`floating component containing '${d.name}'; add a fixed root`, d))
      }
    }
    for (const fastener of ast.body.filter(node => node.type === 'fastener_declaration')) {
      for (const ref of fastener.refs) {
        const base = ref.text.split('#')[0]
        if (!partNames.has(base)) diagnostics.push(this.diagnostic(`unknown part '${ref.text}' in fastener (must be a part declared with ←)`, ref.token))
      }
      if (fastener.clamped === fastener.host) diagnostics.push(this.diagnostic('a fastener needs two different parts (clamped and host)', fastener))
    }
    // cycles
    const visiting = new Set(); const visited = new Set()
    const visit = name => {
      if (visiting.has(name)) return name
      if (visited.has(name)) return null
      visiting.add(name)
      for (const dependency of dependencies.get(name) || []) {
        const cycle = visit(dependency)
        if (cycle) return cycle
      }
      visiting.delete(name); visited.add(name); return null
    }
    for (const declaration of ast.body.filter(node => node.type === 'variable_declaration')) {
      const cycle = visit(declaration.name)
      if (cycle) { diagnostics.push(this.diagnostic(`circular dependency: '${declaration.name}' refers back to itself through '${cycle}'`, declaration)); break }
    }

    // 2D / 3D dimensionality and loft profile rules
    const dims = new Map()
    const dimOf = (node, stack = new Set()) => {
      if (!node) return 'any'
      switch (node.type) {
        case 'shape': return node.dim
        case 'variable_reference': {
          const declaration = byName.get(node.name)
          if (!declaration || declaration.type !== 'variable_declaration' || declaration.kind !== 'shape') return 'any'
          if (dims.has(node.name)) return dims.get(node.name)
          if (stack.has(node.name)) return 'any'
          stack.add(node.name)
          const dim = dimOf(declaration.value, stack)
          stack.delete(node.name)
          dims.set(node.name, dim)
          return dim
        }
        case 'loft': {
          for (const profile of node.profiles) {
            if (dimOf(profile.shape, stack) === 3) {
              diagnostics.push(this.diagnostic('loft profiles must be 2D outlines (△ ⬠ ⭘ ✎ ...), not 3D solids', node))
              return 3
            }
            const problem = this.loftProfileProblem(profile.shape, byName)
            if (problem) diagnostics.push(this.diagnostic(`in loft profile: ${problem.message}`, problem.node))
          }
          return 3
        }
        case 'transformation': {
          if (node.name === 'Extrude') return this.extrudeDim(dimOf(node.target, stack), node, diagnostics)
          if (node.name === 'Offset') return this.offsetDim(dimOf(node.target, stack), node, diagnostics)
          if (['Hull', 'Union', 'Intersect', 'Minkowski'].includes(node.name)) return this.sameDim(node.name.toLowerCase(), [dimOf(node.left, stack), dimOf(node.right, stack)], node, diagnostics)
          return dimOf(node.target, stack)
        }
        case 'boolean_operation': {
          if (node.symbol === '↯') return this.offsetDim(dimOf(node.left, stack), node, diagnostics)
          const names = { Diff: 'difference', Union: 'union', Intersection: 'intersection', Hull: 'hull', Minkowski: 'minkowski' }
          return this.sameDim(names[node.operator] || node.operator, [dimOf(node.left, stack), dimOf(node.right, stack)], node, diagnostics)
        }
        case 'attachment_operation':
          return this.sameDim(node.operator === 'Position' ? 'position (⌖ / at)' : 'attach (⋈ / on)', [dimOf(node.left, stack), dimOf(node.right, stack)], node, diagnostics)
        case 'pipeline_operation': {
          const input = dimOf(node.input, stack)
          if (node.name === 'extrude') return this.extrudeDim(input, node, diagnostics)
          if (node.name === 'loft') {
            // `p |> loft z q` starts a loft from profile p; when p is already a
            // loft (directly or through a variable) the stage extends it.
            const extending = this.isLoft(node.input, byName)
            const profiles = extending ? [node.target] : [node.input, node.target]
            if (profiles.some(shape => dimOf(shape, stack) === 3)) {
              diagnostics.push(this.diagnostic('loft profiles must be 2D outlines (△ ⬠ ⭘ ✎ ...), not 3D solids', node))
            } else {
              for (const shape of profiles) {
                const problem = this.loftProfileProblem(shape, byName)
                if (problem) diagnostics.push(this.diagnostic(`in loft profile: ${problem.message}`, problem.node))
              }
            }
            return 3
          }
          if (node.name === 'hull' && !node.target) return input
          if (['datum','frame','place','mate'].includes(node.name) && input === 2) diagnostics.push(this.diagnostic(`${node.name} needs a 3D solid`, node))
          if (['place','mate'].includes(node.name)) dimOf(node.target, stack)
          if (['add', 'cut', 'isect', 'hull', 'mink', 'at', 'on', 'cutat'].includes(node.name)) {
            const names = { add: 'union', cut: 'difference', isect: 'intersection', hull: 'hull', mink: 'minkowski', at: 'position (⌖ / at)', on: 'attach (⋈ / on)', cutat: 'cutat' }
            return this.sameDim(names[node.name], [input, dimOf(node.target, stack)], node, diagnostics)
          }
          return input
        }
        default: return 'any'
      }
    }
    for (const declaration of ast.body.filter(node => node.type === 'variable_declaration' && node.kind === 'shape')) {
      if (!dims.has(declaration.name)) dims.set(declaration.name, dimOf(declaration.value, new Set([declaration.name])))
    }

    if ((options.fileType || 'coscad') === 'coscad') {
      const main = byName.get('main')
      if (!main && ast.failedNames.has('main')) {
        // the main line exists but did not parse; that error is already reported
      } else if (!main) {
        const lines = text.split(/\r?\n/)
        diagnostics.push(this.diagnostic("no 'main' definition found; add a line `main = <shape>` (it is the shape that gets rendered)", { line: Math.max(0, lines.length - 1), column: 0, value: 'main' }))
      } else if (main.kind === 'number') {
        diagnostics.push(this.diagnostic("'main' is a number, not a shape; main must be the shape that gets rendered", main))
      } else if (dims.get('main') === 2) {
        diagnostics.push(this.diagnostic("'main' is a 2D profile; extrude it to get a solid (⮕ h main, or |> extrude h)", main))
      }
    }
    return diagnostics
  }

  isLoft(node, byName, stack = new Set()) {
    if (!node) return false
    if (node.type === 'loft') return true
    if (node.type === 'pipeline_operation' && node.name === 'loft') return true
    if (node.type === 'variable_reference') {
      const declaration = byName.get(node.name)
      if (!declaration || declaration.kind !== 'shape' || stack.has(node.name)) return false
      stack.add(node.name)
      return this.isLoft(declaration.value, byName, stack)
    }
    return false
  }

  extrudeDim(inner, node, diagnostics) {
    if (inner === 3) diagnostics.push(this.diagnostic('cannot extrude a 3D solid: ⮕ / extrude needs a 2D profile (△ ⬠ ⭘ ✎, or Circle/Triangle/Pentagon/Bezier)', node))
    return 3
  }

  offsetDim(inner, node, diagnostics) {
    if (inner === 3) diagnostics.push(this.diagnostic('offset (↯ / Offset) needs a 2D profile, but was given a 3D solid', node))
    return 2
  }

  sameDim(operation, dims, node, diagnostics) {
    const concrete = dims.filter(dim => dim !== 'any')
    if (concrete.includes(2) && concrete.includes(3)) {
      diagnostics.push(this.diagnostic(`cannot ${operation} a 2D profile with a 3D solid; extrude the profile first (⮕ h / |> extrude h)`, node))
      return 3
    }
    return concrete[0] ?? 'any'
  }

  // Mirrors Codegen.pathOf: a loft profile must be one closed outline in
  // the XY plane. Returns { node, message } for the first violation.
  loftProfileProblem(node, byName, stack = new Set()) {
    if (!node) return null
    switch (node.type) {
      case 'shape': return node.dim === 2 ? null : { node, message: 'a loft profile must be a 2D outline (△ ⬠ ⭘ ✎, Circle/Triangle/Pentagon/Bezier), not a 3D solid' }
      case 'variable_reference': {
        const declaration = byName.get(node.name)
        if (!declaration || declaration.type !== 'variable_declaration' || declaration.kind !== 'shape' || stack.has(node.name)) return null
        stack.add(node.name)
        const problem = this.loftProfileProblem(declaration.value, byName, stack)
        stack.delete(node.name)
        return problem ? { node, message: problem.message } : null
      }
      case 'transformation': {
        if (node.name === 'Tz' || (node.name === 'Translate' && node.axis === 'z')) return { node, message: 'a loft profile cannot be moved in Z (ζ); give the loft its z value instead' }
        if (node.name === 'Rx' || node.name === 'Ry' || (node.name === 'Rotate' && node.axis !== 'z')) return { node, message: 'a loft profile must stay in the XY plane (rotate only about Z)' }
        if (['Hull', 'Union', 'Intersect', 'Minkowski'].includes(node.name)) return { node, message: 'a loft profile must be a single closed outline: booleans (⊕ ⊖ ∩ ⇓ ⊞) between profiles are not supported' }
        return this.loftProfileProblem(node.target, byName, stack)
      }
      case 'boolean_operation':
        if (node.symbol === '↯') return this.loftProfileProblem(node.left, byName, stack)
        return { node, message: 'a loft profile must be a single closed outline: booleans (⊕ ⊖ ∩ ⇓ ⊞) between profiles are not supported' }
      case 'attachment_operation': return { node, message: 'a loft profile must be a single closed outline: attachments between profiles are not supported' }
      case 'loft': return { node, message: 'a loft profile must be a 2D outline, not a loft' }
      case 'pipeline_operation': {
        if (['x', 'y', 'rotz', 'scale', 'mirror', 'anchor'].includes(node.name)) return this.loftProfileProblem(node.input, byName, stack)
        if (node.name === 'z') return { node, message: 'a loft profile cannot be moved in Z (ζ); give the loft its z value instead' }
        if (['rotx', 'roty'].includes(node.name)) return { node, message: 'a loft profile must stay in the XY plane (rotate only about Z)' }
        if (node.name === 'move') return this.loftProfileProblem(node.input, byName, stack)
        return { node, message: 'a loft profile must be a single closed outline: booleans (⊕ ⊖ ∩ ⇓ ⊞) between profiles are not supported' }
      }
      default: return null
    }
  }

  collectReferences(node, references = []) {
    if (!node) return references
    if (node.type === 'variable_reference' || node.type === 'number_reference') references.push(node)
    for (const key of ['value', 'input', 'target', 'left', 'right', 'operand']) this.collectReferences(node[key], references)
    for (const key of ['params', 'offset']) for (const child of node[key] || []) this.collectReferences(child, references)
    for (const profile of node.profiles || []) { this.collectReferences(profile.z, references); this.collectReferences(profile.shape, references) }
    return references
  }

  diagnostic(message, token = {}) {
    const line = token.line || 0; const character = token.column || 0
    const length = String(token.raw ?? token.value ?? token.name ?? token.symbol ?? '').length || 1
    return { severity: 1, range: { start: { line, character }, end: { line, character: character + length } }, message, source: 'coscad' }
  }
}

module.exports = { COSCADParser, ParseError }
