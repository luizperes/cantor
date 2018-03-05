# Imperal type rules
  - [Epsilon](#Epsilon)
  - [Boolean](#Boolean)
  - [Natural](#Natural)
  - [Integer](#Integer)
  - [Double](#Double)
  - [Char](#Char)
  - [Tuple](#Tuple)
  - [Set](#Set)
  - [Universe](#Universe)

###### Epsilon
```Haskell
# Internal Type
let Ɛ =
  str ∈ Universe: error str
```

###### Boolean
```Haskell
# Internal Type
let Bool =
  ∀expr ∈ Universe:
                  [ True       expr ]
                  [ False otherwise ]
```

###### Natural
```Haskell
let N =
  ∀x ∈ Universe:
               [ x               (x ∈ N) ]
               [ x       (x ∈ Z), x >= 0 ]
               [ Ɛ "x ∉ N"     otherwise ]
```

###### Integer
```Haskell
let Z =
  ∀x ∈ Universe:
               [ x           (x ∈ Z) ]
               [ x           (x ∈ N) ]
               [ Ɛ "x ∉ Z" otherwise ]
```

###### Double
```Haskell
let R =
  ∀x ∈ Universe:
               [ x           (x ∈ R) ]
               [ x           (x ∈ N) ]
               [ x           (x ∈ Z) ]
               [ Ɛ "x ∉ R" otherwise ]
```

###### Char
```Haskell
let Char =
  ∀x ∈ Universe:
               [ x           (x ∈ Char) ]
               [ Ɛ "x ∉ Char" otherwise ]
```

###### Tuple
```Haskell
let Tuple =
  ∀(ty, x) ∈ Universe:
                     [ x                          (x ∈ ty) ]
                     [ Ɛ "x ∉ ty, invalid Tuple" otherwise ]
```

###### Set
```Haskell
let Set =
 ∀x ∈ list,
 ∀list ⊆ Universe:
                 [ x                (list ⊆ N), (x ∈ N) ]
                 [ x                (list ⊆ Z), (x ∈ Z) ]
                 [ x                (list ⊆ R), (x ∈ R) ]
                 [ x          (list ⊆ Char), (x ∈ Char) ]
                 [ x     (list ⊆ Universe), (x ∈ Tuple) ]
                 [ x  (list ⊆ Universe), (x ∈ Universe) ]
                 [ Ɛ "x ∉ list"               otherwise ]
```

###### Universe
```Haskell
let Universe =
  ∀x ∈ Universe: x
```
