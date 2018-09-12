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
let
  square = s subset of Z: x ^ 2, for all x in s
  allEven = s subset of Z: (x % 2) = 0, for all x in s
  ifThereIsANumberGreaterThan10 = s subset of Z: x > 10, there exists x in s
do
  square .
  allEven .
  ifThereIsANumberGreaterThan10 .
  {2, 4, 6, 8, 10, 12}
```

The code above can also be written:
```Haskell
let
  square = s ⊆ Z: x ^ 2, ∀x ∈ s
  allEven = s ⊆ Z: (x % 2) = 0, ∀x ∈ s
  ifThereIsANumberGreaterThan10 = s ⊆ Z: x > 10, ∃x ∈ s
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
let fact = x ∈ N:
         [ 1                        x = 0 ]
         [ 1                        x = 1 ]
         [ x * fact ∘ (x - 1)   otherwise ]
```

### Help
Feel free to send your pull requests. :)

### Diclaimer
Use at your own risk.
