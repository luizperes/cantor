# Cantor
> Cantor is inspired on set comprehensions and discrete math. It is named after Georg Cantor, creator of set theory.

#### Dependencies
  - `parsec`: Install with the command: `cabal install parsec`
  - `random`: Install with the command: `cabal install random`

#### How to install and run
  - `cabal install`
  - `cabal run </path/to/file.imp>`

#### EBNF and Railroad Diagram
  - Cantor's grammar can be found on the file [EBNF.md](EBNF.md)

#### Type Rules
  - Cantor's type rules can be found on the file [TYPE_RULES.md](TYPE_RULES.md)

#### Example
```Haskell
let Z = -128..127

# simulating head and tail (as if sets were lists) ...
let head => s subset of Universe: any in s
let tail => s subset of Universe: s - (head . s)

let True  = 1 = 1
let False = ~True

# ∀xP(x)
let forall => P in Universe, X subset of Universe, x in X:
           [  True                                              X = {}  ]
           [  forall . (P, X - x, head . (X - x))                P . x  ]
           [  False                                          otherwise  ]

# ∃xP(x)
let exists => P in Universe, X subset of Universe, x in X:
           [  False                                        X = {}  ]
           [  True                                         P . x   ]
           [  exists . (P, X - x, head . (X - x))       otherwise  ]

let map => f in Universe -> Universe, s subset of Universe, x in s, acc subset of Universe:
        [  acc                                                      s = {}  ]
        [  (map . (f, s - x, head . (s - x), (f . x) + acc))     otherwise  ]

let
  square => x in Z: x ^ 2
  allSquared => s subset of Z: map . (square, s, head . s, {})
  pred => x in Z: (x % 2) = 0
  allEven => s subset of Z: s, forall . (pred, s, head . s)
  pred2 => x in Z: x > 10
  ifThereIsANumberGreaterThan10 => s subset of Z: s, exists . (pred2, s, head . s)
do
  allSquared .
  allEven .
  ifThereIsANumberGreaterThan10 .
  {2, 4, 6, 8, 10, 12}
```

The code above can also be written:
```Haskell
let Z = -128..127

# simulating head and tail (as if sets were lists) ...
let head => s ⊆ Universe: any ∈ s
let tail => s ⊆ Universe: s - (head ∘ s)

let True  = 1 = 1
let False = ~True

# ∀xP(x)
let ∀ => P ∈ Universe, X ⊆ Universe, x ∈ X:
      [  True                                              X = {}  ]
      [  ∀ . (P, X - x, head . (X - x))                     P . x  ]
      [  False                                          otherwise  ]

# ∃xP(x)
let ∃ => P ∈ Universe, X ⊆ Universe, x ∈ X:
      [  False                                        X = {}  ]
      [  True                                         P . x   ]
      [  ∃ . (P, X - x, head . (X - x))            otherwise  ]

let map => f ∈ Universe → Universe, s ⊆ Universe, x ∈ s, acc ⊆ Universe:
        [  acc                                                    s = {}  ]
        [  (map . (f, s - x, head . (s - x), (f∘x) + acc))     otherwise  ]

let
  square => x ∈ Z: x ^ 2
  allSquared => s ⊆ Z: map . (square, s, head . s, {})
  pred => x ∈ Z: (x % 2) = 0
  allEven => s ⊆ Z: s, ∀ . (pred, s, head . s)
  pred2 => x ∈ Z: x > 10
  ifThereIsANumberGreaterThan10 => s ⊆ Z: s, ∃ . (pred2, s, head . s)
do
  allSquared ∘
  allEven ∘
  ifThereIsANumberGreaterThan10 ∘
  {2, 4, 6, 8, 10, 12}
```

###### output
```
{4, 16, 36, 64, 100, 144}
```

#### Factorial
```Haskell
let fact => x ∈ N:
         [  1                        x = 0  ]
         [  1                        x = 1  ]
         [  x * fact ∘ (x - 1)   otherwise  ]
```

### Help
Feel free to send your pull requests. :)

### Diclaimer
Use at your own risk.
