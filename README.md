# Impero 
> Impero is inspired on set comprehensions and discrete math. Its name derives from the so-famous phrase "divide and conquer"
> (from Latin dīvide et imperā, hence impero : to give orders, command / to rule, hold sway).

#### Dependencies
  - `parsec`: Install with the command: `cabal install parsec`

#### EBNF and Railroad Diagram
  - Impero's grammar can be found on the file [EBNF.md](EBNF.md)

#### Type Rules
  - Impero's type rules can be found on the file [TYPE_RULES.md](TYPE_RULES.md)

#### Example
```Haskell
let
  numberSet = s subset of Z
  square = for all x in numberSet: x ^ 2
  allEven = for all n in numberSet: (x % 2) = 0
  ifThereIsANumberGreaterThan10 = there exists n in numberSet: n > 10
do
  square .
  allEven .
  ifTheresIsANumberGreaterThan10 .
  {2, 4, 6, 8, 10, 12}
```

The code above can also be written:
```Haskell
let
  numberSet = s ⊆ Z
  square = ∀x ∈ numberSet: x ^ 2
  allEven = ∀n ∈ numberSet: (x % 2) = 0
  ifThereIsANumberGreaterThan10 = ∃n ∈ numberSet: n > 10
do
  square .
  allEven .
  ifTheresIsANumberGreaterThan10 .
  {2, 4, 6, 8, 10, 12}
```

###### output
```
{4, 16, 36, 64, 100, 144}
```

### Help
Feel free to send your pull requests. :)

### LICENSE
This project extends [GNU GPL v. 3](http://www.gnu.org/licenses/gpl-3.0.en.html), so be aware of that, regarding copying, modifying and (re)destributing.
