# coscad

Amazing CAD in haskell that compiles to OPENSCAD, by yours truly (Jasen Qin)

**Unicode Glyph Functions**

This library provides Unicode glyph aliases for CAD operations. Here's the complete mapping:

3D Shapes

- `■` (U+25A0) - `cube` - Creates a cube/rectangular prism
- `●` (U+25CF) - `sphere` - Creates a sphere
- `◎` (U+25CE) - `cylinder` - Creates a cylinder
- `▻` (U+25BB) - `cone` - Creates a cone
- `▬` (U+25AC) - `rect` - Creates a rectangle (alias for cube)
- `⎏` (U+23CF) - `prism` - Creates a regular prism

2D Shapes

- `△` (U+25B3) - Equilateral triangle profile
- `⬠` (U+2B20) - Regular pentagon profile
- `⭘` (U+2B58) - Circle profile
- `⟁` (U+27C1) - `poly` - Creates a polygon with points and paths

BOSL2 Shapes

These emit [BOSL2](https://github.com/BelfrySCAD/BOSL2) primitives instead of vanilla OpenSCAD. Any `.coscad` file that uses one of these automatically gets `include <BOSL2/std.scad>` added to the generated `.scad` file, so BOSL2 must be installed in your OpenSCAD library path.

- `▣` (U+25A3) - `cuboid` (chamfered) - `▣ x y z chamfer`
- `◙` (U+25D9) - `cuboid` (rounded) - `◙ x y z rounding`
- `⌭` (U+232D) - `cyl` (chamfered) - `⌭ r h chamfer`
- `⌽` (U+233D) - `cyl` (rounded) - `⌽ r h rounding`
- `xcyl` / `ycyl` / `zcyl` - axis-aligned cylinders - `xcyl r l`
- `⊚` (U+229A) - `tube` - `⊚ outer_r inner_r h`
- `⏢` (U+23E2) - `prismoid` - `⏢ x1 y1 x2 y2 h`
- `◉` (U+25C9) - `torus` - `◉ r_major r_minor`
- `⊿` (U+22BF) - `wedge` - `⊿ x y z` (vertical face at -X, hypotenuse in XZ)

Transformations

- `χ` (U+03C7) - `Tx` - Translation along X-axis
- `ψ` (U+03C8) - `Ty` - Translation along Y-axis
- `ζ` (U+03B6) - `Tz` - Translation along Z-axis
- `θ` (U+03B8) - `Rx` - Rotation around X-axis
- `ϕ` (U+03C6) - `Ry` - Rotation around Y-axis
- `ω` (U+03C9) - `Rz` - Rotation around Z-axis
- `⬈` (U+2B08) - `Scale` - Scaling transformation
- `⇋` (U+21CB) - `Mirror` - Mirror across a plane normal - `⇋ mx my mz shape`
- `⮕` (U+2B95) - `Extrude` - Linear extrusion (2D → 3D)

Boolean Operations

- `⊖` (U+2296) - `Diff` - Boolean difference
- `⊝` (U+229D) - `Diff` - Boolean difference (alias)
- `⊛` (U+229B) - `Union` - Boolean union (alias)
- `⊕` (U+2295) - `Union` - Boolean union

Advanced Operations

- `⇓` (U+21D3) - `Hull` - Convex hull operation
- `⊞` (U+229E) - `Minkowski` - Minkowski sum operation
- `↯` (U+21AF) - `Offset` - Offset operation (2D profiles)

Composition Operations

- `|>` (U+007C U+003E) - Forward pipe operator
- `▷` (U+25B7) - Forward pipe operator (alias)

**CLI Usage**

The `coscad` CLI tool converts `.coscad` files to `.scad` (OpenSCAD) format.

Basic Usage

```bash
coscad <input.coscad>
```

This will create a corresponding `.scad` file in the same directory.

**Examples**

Create a simple cube:

```bash
echo "■ 10" > cube.coscad
coscad cube.coscad
```

Create a sphere:

```bash
echo "● 15" > sphere.coscad
coscad sphere.coscad
```

Create a cylinder:

```bash
echo "◎ 5 10" > cylinder.coscad
coscad cylinder.coscad
```

Create a cone:

```bash
echo "▻ 8 15" > cone.coscad
coscad cone.coscad
```

Boolean operations:

```bash
echo "● 15 ⊖ ◎ 5 10" > difference.coscad
coscad difference.coscad
```

```bash
echo "■ 10 ⊕ ● 5" > union.coscad
coscad union.coscad
```

Advanced operations:

```bash
echo "■ 10 ⇓ ● 5" > hull.coscad
coscad hull.coscad
```

```bash
echo "■ 10 ⊞ ● 5" > minkowski.coscad
coscad minkowski.coscad
```

```bash
echo "△ 8 ↯ ● 2" > offset.coscad
coscad offset.coscad
```

BOSL2 shapes:

```bash
echo "▣ 20 15 10 2" > chamfered_cuboid.coscad
coscad chamfered_cuboid.coscad
```

```bash
echo "⌽ 5 20 2" > rounded_cyl.coscad
coscad rounded_cyl.coscad
```

```bash
echo "⊚ 10 6 25" > tube.coscad
coscad tube.coscad
```

Transforming with mirror:

```bash
echo "⇋ 1 0 0 (● 5)" > mirror.coscad
coscad mirror.coscad
```

Using variables (a `main` variable is required and is the shape that gets rendered):

```
c₁ = ■ 10
s₁ = ● 15
main = c₁ ⊕ s₁
```

**Syntax**

The current parser supports:

- Basic shapes: `■ size`, `● radius`, `◎ radius height`, `▻ radius height`, `▬ x y z`, `⎏ sides radius height`
- 2D profiles: `△ radius`, `⬠ radius`, `⭘ radius`
- BOSL2 shapes: `▣ x y z chamfer`, `◙ x y z rounding`, `⌭ r h chamfer`, `⌽ r h rounding`, `xcyl/ycyl/zcyl r l`, `⊚ outer_r inner_r h`, `⏢ x1 y1 x2 y2 h`, `◉ r_major r_minor`, `⊿ x y z`
- Transformations: `χ`/`ψ`/`ζ dist shape` (translate), `θ`/`ϕ`/`ω angle shape` (rotate), `⬈ sx sy sz shape` (scale), `⇋ mx my mz shape` (mirror), `⮕ height shape` (extrude)
- Boolean operations: `shape1 ⊖ shape2` (difference), `shape1 ⊕ shape2` (union)
- Advanced operations: `shape1 ⇓ shape2` (hull), `shape1 ⊞ shape2` (minkowski), `profile ↯ shape` (offset)
- Variables: define with `name = expression` on each line; a `main` variable is required and its value is what gets rendered

BOSL2 shapes automatically add `include <BOSL2/std.scad>` to the generated `.scad` file, so [BOSL2](https://github.com/BelfrySCAD/BOSL2) must be available in your OpenSCAD library path to render the output.

**Building**

To build the project:

```bash
stack build
```

This creates the executable in .stack-work. You can add its path to PATH to call `coscad` directly.

Otherwise, to run the executable with stack:

```bash
stack run -- input.coscad
```
