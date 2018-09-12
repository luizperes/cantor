# Cantor type rules
  - [Epsilon](#epsilon)
  - [Natural](#natural)
  - [Integer](#integer)
  - [Double](#double)
  - [Char](#char)
  - [Tuple](#tuple)
  - [Set](#set)
  - [Function](#function)
  - [Custom](#custom)
  - [Universe](#universe)

###### Epsilon
```Haskell
# Internal Type
let Ɛ = str ∈ Universe:
  error str
```

###### Natural
```Haskell
let N = x ∈ Universe:
      [ x               (x ∈ N) ]
      [ x       (x ∈ Z), x >= 0 ]
      [ Ɛ "x ∉ N"     otherwise ]
```

###### Integer
```Haskell
let Z = x ∈ Universe:
      [ x           (x ∈ Z) ]
      [ x           (x ∈ N) ]
      [ Ɛ "x ∉ Z" otherwise ]
```

###### Double
```Haskell
let R = x ∈ Universe:
      [ x           (x ∈ R) ]
      [ x           (x ∈ N) ]
      [ x           (x ∈ Z) ]
      [ Ɛ "x ∉ R" otherwise ]
```

###### Char
```Haskell
let Char = x ∈ Universe:
         [ x           (x ∈ Char) ]
         [ Ɛ "x ∉ Char" otherwise ]
```

###### Tuple
```Haskell
let Tuple = x ∈ Universe, ty0 ∈ Universe, ty1 ∈ Universe:
          [ x                            x ∈ (ty0 × ty1) ]
          [ Ɛ "x ∉ ty, invalid Tuple"         otherwise ]
```

###### Set
```Haskell
let Set = s ∈ Universe, ty ∈ Universe:
        [ s               (s ⊆ Universe), (∀x ∈ s, x ∈ ty) ]
        [ Ɛ "s ∉ list"                           otherwise ]
```

###### Function
```Haskell
let Function = f ∈ Universe, ty0 ∈ Universe, ty1 ∈ Universe:
             [ f                   f ∈ (ty0 → ty1) ]
             [ Ɛ "s ∉ function"          otherwise ]
```

###### Custom
```Haskell
let customSet = x ⊆ Universe: x

let Custom = x ∈ Universe:
           [ x           (x ∈ customSet) ]
           [ Ɛ "x ∉ customSet" otherwise ]
```

###### Universe
```Haskell
let Universe = x ∈ Universe: x
```
