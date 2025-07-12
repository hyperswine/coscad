# coscad

Amazing CAD in haskell that compiles to OPENSCAD, by yours truly (Jasen Qin)

## Unicode Glyph Functions

This library provides Unicode glyph aliases for CAD operations. Here's the complete mapping:

### 3D Shapes

- `■` (U+25A0) - `cube` - Creates a cube/rectangular prism
- `●` (U+25CF) - `sphere` - Creates a sphere
- `◎` (U+25CE) - `cylinder` - Creates a cylinder
- `▻` (U+25BB) - `cone` - Creates a cone
- `▬` (U+25AC) - `rect` - Creates a rectangle (alias for cube)
- `⎏` (U+23CF) - `prism` - Creates a regular prism

### 2D Shapes

- `△` (U+25B3) - Equilateral triangle profile
- `⬠` (U+2B20) - Regular pentagon profile
- `⭘` (U+2B58) - Circle profile
- `⟁` (U+27C1) - `poly` - Creates a polygon with points and paths

### Transformations

- `χ` (U+03C7) - `Tx` - Translation along X-axis
- `ψ` (U+03C8) - `Ty` - Translation along Y-axis
- `ζ` (U+03B6) - `Tz` - Translation along Z-axis
- `θ` (U+03B8) - `Rx` - Rotation around X-axis
- `ϕ` (U+03C6) - `Ry` - Rotation around Y-axis
- `ω` (U+03C9) - `Rz` - Rotation around Z-axis
- `⬈` (U+2B08) - `Scale` - Scaling transformation
- `⮕` (U+2B95) - `Extrude` - Linear extrusion (2D → 3D)

### Boolean Operations

- `⊖` (U+2296) - `Diff` - Boolean difference
- `⊝` (U+229D) - `Diff` - Boolean difference (alias)
- `⊛` (U+229B) - `Union` - Boolean union (alias)
- `⊕` (U+2295) - `Union` - Boolean union

### Composition Operations

- `|>` (U+007C U+003E) - Forward pipe operator
- `▷` (U+25B7) - Forward pipe operator (alias)
