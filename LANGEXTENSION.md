An extension to the regular coscad
You can have variables like

```
// Simple cube
c₁ = ■ 10

// Sphere with boolean operation
s₁ = ● 15 ⊖ ◎ 5 10

// Union of shapes
ss = ■ 10 ⊕ ● 5

// Transformations
s₂ = χ 5 ● 10
c₃ = ψ 3 ■ 10
cy₁ = ζ 2 ◎ 5 10

// Rotations
c₄ = θ 45 ■ 10
cy₁ = ϕ 90 ◎ 5 10 // later definitions shadow earlier ones
co₁ = ω 30 ▻ 8 15

// 2D shapes with extrusion
ex₁ = ⮕ 10 ⭘ 5
ex₂ = ⮕ 15 △ 8

main = ex₂ ⊕ ex₁
```

the main variable defines the thing that will actually be used to render it

Needed features for coscad lsp
Semantic hover
