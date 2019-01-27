# Cantor EBNF and Railroad diagram

- EBNF
  - [begin_stmt](#begin_stmt)
  - [lib_stmt](#lib_stmt)
  - [use_stmt](#use_stmt)
  - [do_stmt](#do_stmt)
  - [let_stmt](#let_stmt)
  - [pattern_stmt](#pattern_stmt)
  - [simple_stmt](#simple_stmt)
  - [binding](#binding)
  - [case_expr](#case_expr)
  - [expr](#expr)
  - [set_expr](#set_expr)
  - [equality_expr](#equality_expr)
  - [relational_expr](#relational_expr)
  - [range_expr](#range_expr)
  - [additive_expr](#additive_expr)
  - [multiplicative_expr](#multiplicative_expr)
  - [power_expr](#power_expr)
  - [fn_call_expr](#fn_call_expr)
  - [negation_expr](#negation_expr)
  - [primary_expr](#primary_expr)
  - [quant_expr](#quant_expr)
  - [relationship](#relationship)
  - [type](#type)
  - [binding_type](#binding_type)
  - [binding_name](#binding_name)
  - [identifier](#identifier)
  - [constant](#constant)
  - [set](#set)
  - [tuple](#tuple)
  - [range](#range)
  - [float](#float)
  - [digit](#digit)
  - [letter](#letter)
  - [other_symbol](#other_symbol)
  - [start_symbol](#start_symbol)

###### begin\_stmt
```EBNF
begin_stmt ::= lib_stmt
             | use_stmt
             | let_stmt
             | do_stmt
```
<p align="left">
  <a href="">
    <img alt="BeginStmt" src="./img/begin_stmt.png" />
  </a>
</p>

###### lib\_stmt
```EBNF
lib_stmt ::= "lib" binding_name
```
<p align="left">
  <a href="">
    <img alt="LibStmt" src="./img/lib_stmt.png" />
  </a>
</p>

###### use\_stmt
```EBNF
use_stmt ::= "use" binding_name
```
<p align="left">
  <a href="">
    <img alt="UseStmt" src="./img/use_stmt.png" />
  </a>
</p>

###### do\_stmt
```EBNF
do_stmt ::= 'do' ( binding_name ( '.' | '∘' ) )* constant
```
<p align="left">
  <a href="">
    <img alt="DoStmt" src="./img/do_stmt.png" />
  </a>
</p>

###### let\_stmt
```EBNF
let_stmt ::= "let" binding+
```
<p align="left">
  <a href="">
    <img alt="LetStmt" src="./img/let_stmt.png" />
  </a>
</p>

###### binding
```EBNF
binding ::= binding_name '=>' pattern_stmt ':' case_expr
          | binding_name '='  expr
```
<p align="left">
  <a href="">
    <img alt="Binding" src="./img/binding.png" />
  </a>
</p>

###### pattern\_stmt
```EBNF
pattern_stmt ::= simple_stmt (',' pattern_stmt)*
```
<p align="left">
  <a href="">
    <img alt="PatternStmt" src="./img/pattern_stmt.png" />
  </a>
</p>

###### simple\_stmt
```EBNF
simple_stmt ::= binding_type
```
<p align="left">
  <a href="">
    <img alt="SimpleStmt" src="./img/simple_stmt.png" />
  </a>
</p>

###### case\_expr
```EBNF
case_expr ::= expr ( ',' expr )*
            | ( '[' expr ( expr ( ',' expr )* | 'otherwise' ) ']' )+
```
<p align="left">
  <a href="">
    <img alt="CaseExpr" src="./img/case_expr.png" />
  </a>
</p>

###### expr
```EBNF
expr ::= set_expr
       | (expr ':-' set_expr)
```
<p align="left">
  <a href="">
    <img alt="Expr" src="./img/expr.png" />
  </a>
</p>

###### set_expr
```EBNF
set_expr ::= equality_expr
           | (set_expr relationship equality_expr)
```
<p align="left">
  <a href="">
    <img alt="SetExpr" src="./img/set_expr.png" />
  </a>
</p>

###### equality_expr
```EBNF
equality_expr ::= relational_expr
                | (equality_expr '=' relational_expr)
                | (equality_expr '~' relational_expr)
```
<p align="left">
  <a href="">
    <img alt="EqualityExpr" src="./img/equality_expr.png" />
  </a>
</p>

###### relational\_expr
```EBNF
relational_expr ::= range_expr
                  | (relational_expr '>' range_expr)
                  | (relational_expr '>=' range_expr)
                  | (relational_expr '<' range_expr)
                  | (relational_expr '<=' range_expr)
```
<p align="left">
  <a href="">
    <img alt="RelationalExpr" src="./img/relational_expr.png" />
  </a>
</p>

###### range\_expr
```EBNF
range_expr ::= additive_expr
             | (range_expr '..' additive_expr)
```
<p align="left">
  <a href="">
    <img alt="RangeExpr" src="./img/range_expr.png" />
  </a>
</p>


###### additive\_expr
```EBNF
additive_expr ::= multiplicative_expr
                | (additive_expr '+' multiplicative_expr)
                | (additive_expr '-' multiplicative_expr)
```
<p align="left">
  <a href="">
    <img alt="AdditiveExpr" src="./img/additive_expr.png" />
  </a>
</p>

###### multiplicative\_expr
```EBNF
multiplicative_expr ::= power_expr
                      | (multiplicative_expr '*' power_expr)
                      | (multiplicative_expr '/' power_expr)
                      | (multiplicative_expr '%' power_expr)
```
<p align="left">
  <a href="">
    <img alt="MultiplicativeExpr" src="./img/multiplicative_expr.png" />
  </a>
</p>

###### power\_expr
```EBNF
power_expr ::= fn_call_expr
             | (power_expr '^' fn_call_expr)
```
<p align="left">
  <a href="">
    <img alt="PowerExpr" src="./img/power_expr.png" />
  </a>
</p>

###### fn\_call\_expr
```EBNF
fn_call_expr ::= negation_expr
               | (fn_call_expr ("." | "∘") negation_expr)
```
<p align="left">
  <a href="">
    <img alt="FunctionCallExpr" src="./img/fn_call_expr.png" />
  </a>
</p>

###### negation\_expr
```EBNF
negation_expr ::= primary_expr
                | ('~' negation_expr)
```
<p align="left">
  <a href="">
    <img alt="NegationExpr" src="./img/negation_expr.png" />
  </a>
</p>

###### primary\_expr
```EBNF
primary_expr ::= constant
               | type
               | binding_name
               | quant_expr
               | '(' expr ')'
```
<p align="left">
  <a href="">
    <img alt="PrimaryExpr" src="./img/primary_expr.png" />
  </a>
</p>

###### quant\_expr
```EBNF
quant_expr ::= ("for" "all" | '∀') binding_type
             | ("there" "exists" | '∃') binding_type
```
<p align="left">
  <a href="">
    <img alt="QuantExpr" src="./img/quant_expr.png" />
  </a>
</p>

###### relationship
```EBNF
relatioship ::= ("subset" "of" | '⊆')
              | ("in" | '∈')
```
<p align="left">
  <a href="">
    <img alt="Relationship" src="./img/relatioship.png" />
  </a>
</p>

###### type
```EBNF
type ::= 'Z'
       | 'N'
       | 'R'
       | "Char"
       | "Universe"
       | binding_name
       | type ("union" | "∪") type
       | type ("intersection" | "∩") type
       | type ("*'" | "×") type
       | type ("-'" | "⊖") type
       | type ("\") type
       | type ("->" | "→") type
       | "(" type ")"
```
<p align="left">
  <a href="">
    <img alt="Type" src="./img/type.png" />
  </a>
</p>

###### binding\_type
```EBNF
binding_type ::= (binding_name | "(" binding_name ("," binding_name)* ")") relationship type
```
<p align="left">
  <a href="">
    <img alt="BindingType" src="./img/binding_type.png" />
  </a>
</p>

###### binding\_name
```EBNF
binding_name ::= identifier
```
<p align="left">
  <a href="">
    <img alt="BindingName" src="./img/binding_name.png" />
  </a>
</p>

###### identifier
```EBNF
identifier ::= (letter | start_symbol) (letter | digit | start_symbol | other_symbol )*
```
<p align="left">
  <a href="">
    <img alt="Identifier" src="./img/identifier.png" />
  </a>
</p>

###### constant
```EBNF
constant ::= number
           | char
           | tuple
           | set
```
<p align="left">
  <a href="">
    <img alt="Constant" src="./img/constant.png" />
  </a>
</p>

###### set
```EBNF
set ::= '{' ( expr ( ',' expr )* )? '}'
```
<p align="left">
  <a href="">
    <img alt="Set" src="./img/set.png" />
  </a>
</p>

###### tuple
```EBNF
tuple ::= '(' expr (',' expr)+ ')'
```
<p align="left">
  <a href="">
    <img alt="Tuple" src="./img/tuple.png" />
  </a>
</p>

###### range
```EBNF
range ::= expr ".." expr
```
<p align="left">
  <a href="">
    <img alt="Range" src="./img/range.png" />
  </a>
</p>

###### float
```EBNF
float ::= digit+ '.' digit
```
<p align="left">
  <a href="">
    <img alt="Float" src="./img/float.png" />
  </a>
</p>

###### digit
```EBNF
digit ::= '0'
        | '1'
        | "..."
        | '9'
```
<p align="left">
  <a href="">
    <img alt="Digit" src="./img/digit.png" />
  </a>
</p>

###### letter
```EBNF
letter ::= 'a'
         | 'b'
         | "..."
         | 'z'
         | 'A'
         | ".."
         | 'Z'
```
<p align="left">
  <a href="">
    <img alt="Letter" src="./img/letter.png" />
  </a>
</p>

###### other\_symbol
```EBNF
other_symbol ::= '+'
               | '-'
               | '*'
               | '/'
               | '%'
               | '^'
               | '<'
               | '>'
```
<p align="left">
  <a href="">
    <img alt="OtherSymbol" src="./img/other_symbol.png" />
  </a>
</p>

###### start\_symbol
```EBNF
start_symbol ::= '!'
               | '@'
               | '$'
               | '_'
               | '?'
               | '|'
```
<p align="left">
  <a href="">
    <img alt="StartSymbol" src="./img/start_symbol.png" />
  </a>
</p>

###### Powered by Railroad Diagram Generator
<p align="left">
  <a href="">
    <img alt="Railroad Diagram Generator" src="./img/rr-1.48.1593.png" />
  </a>
</p>
