# Cantor
> Cantor is inspired on set comprehensions and discrete math. It is named after Georg Cantor, creator of set theory.

#### Dependencies
  - `parsec`: Install with the command: `cabal install parsec`

#### How to install and run
  - `cabal install`
  - `cabal run </path/to/file.imp>`

#### EBNF and Railroad Diagram
  - Cantor's grammar can be found on the file [EBNF.md](EBNF.md)

#### Type Rules
  - Cantor's type rules can be found on the file [TYPE_RULES.md](TYPE_RULES.md)

#### Example
```Haskell
# simulating head and tail (as if sets were lists) ...
let head => y subset of Universe: any
let tail => y subset of Universe: y - (head . y)

let True  = 1 = 1
let False = ~True

# ∀xP(x)
let forall => P in Universe, X subset of Universe, x in X:
           [  True                                              X = {}  ]
           [  True          P . x, forall . (P, X - x, head . (X - x))  ]
           [  False                                          otherwise  ]

# ∃xP(x)
let exists => P in Universe, X subset of Universe, x in X:
           [  False                                        X = {}  ]
           [  True                                         P . x   ]
           [  exists . (P, X - x, head . (X - x))       otherwise  ]

let
  square => x in Z: x ^ 2
  pred => x in Z: (x % 2) = 0
  allEven => s subset of Z: forall . (pred, s, s - (head . s))
  pred2 => x in Z: x > 10
  ifThereIsANumberGreaterThan10 => s subset of Z: exists . (pred2, s, s - (head . s))
do
  square .
  allEven .
  ifThereIsANumberGreaterThan10 .
  {2, 4, 6, 8, 10, 12}
```

The code above can also be written:
```Haskell
# simulating head and tail (as if sets were lists) ...
let head => y ⊆ Universe: any
let tail => y ⊆ Universe: y - (head . y)

let True  = 1 = 1
let False = ~True

# ∀xP(x)
let ∀ => P ∈ Universe, X ⊆ Universe, x ∈ X:
      [  True                                              X = {}  ]
      [  True               P ∘ x, ∀ ∘ (P, X - x, head ∘ (X - x))  ]
      [  False                                          otherwise  ]

# ∃xP(x)
let ∃ => P ∈ Universe, X ⊆ Universe, x ∈ X:
      [  False                                        X = {}  ]
      [  True                                         P ∘ x   ]
      [  ∃ ∘ (P, X - x, head ∘ (X - x))            otherwise  ]

let
  square  => s ⊆ Z: x ^ 2
  pred => x ∈ Z: (x % 2) = 0
  allEven => s ⊆ Z: ∀ ∘ (pred, s, head ∘ s)
  pred2 => x ∈ Z: x > 10
  ifThereIsANumberGreaterThan10 => s ⊆ Z: ∃ ∘ (pred2, s, head ∘ s)
do
  square ∘
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
