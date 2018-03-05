# Imperal type rules

### Type check
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
Ɛ str: error str
```

###### Boolean
```Haskell
# Internal Type
∀expr ∈ Universe:
                [ True       expr ]
                [ False otherwise ]
```

###### Natural
```Haskell
∀x ∈ Universe:
             [ x               (x ∈ N) ]
             [ x       (x ∈ Z), x >= 0 ]
             [ Ɛ "x ∉ N"     otherwise ]
```

###### Integer
```Haskell
∀x ∈ Universe:
             [ x           (x ∈ Z) ]
             [ x           (x ∈ N) ]
             [ Ɛ "x ∉ Z" otherwise ]
```

###### Double
```Haskell
∀x ∈ Universe:
             [ x           (x ∈ R) ]
             [ x           (x ∈ N) ]
             [ x           (x ∈ Z) ]
             [ Ɛ "x ∉ R" otherwise ]
```

###### Char
```Haskell
∀x ∈ Universe:
             [ x           (x ∈ Char) ]
             [ Ɛ "x ∉ Char" otherwise ]
```

####### Tuple
```Haskell

```

###### Universe
```Haskell
∀x ∈ Universe: x
```
