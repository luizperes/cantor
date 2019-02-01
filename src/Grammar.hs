module Grammar where

import System.Environment

data Constant = Epsilon   [Char]
              | BoolLit   Bool
              | CharLit   Char
              | DoubleLit Double
              | IntLit    Double
              | NatLit    Double
              | SetLit    [Expression]
              | TupleLit  [Expression]
              | TypeLit   Type
              deriving Show

type Identifier = String

type BindingName = Identifier

data Type = TUniverse
          | TCustom BindingName
          | TBinOp SetOp Type Type
          | TGroup Type
          deriving Show

data SetOp = Union
           | Intersection
           | CartProduct
           | SymmetricDiff
           | RelativeDiff
           | Function
           deriving Show

data Relationship = SubsetOf
                  | ElementOf
                  deriving Show

data BindingType = BType [BindingName] Relationship Type
                 deriving Show

data PatternStmt = SimpleStmt BindingType
                 | PatternListStmt [PatternStmt]
                 deriving Show

data CaseExpression = CEList [Expression]
                    | CECase [(Expression, [Expression])]
                    deriving Show

data Expression = EBinOp BinaryOp Expression Expression
                | EUnOp UnaryOp Expression
                | EConst Constant
                | EBind BindingName
                | EQtOp Quantif BindingType
                deriving Show

data Quantif = ForAll
             | ThereExists
             deriving Show

data UnaryOp = Negation
             | Negative
             deriving Show

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | Mod
              | Exp
              | Eq
              | NEq
              | Gt
              | GtE
              | Lt
              | LtE
              | FCall
              | Range
              | In
              | Subset
              | Def
              | And -- implicit
              deriving Show

data Binding = BBind BindingName PatternStmt CaseExpression
             | BExpr BindingName Expression
             deriving Show

data FunctionCall = FCSingle Constant
                  | FCNested BindingName FunctionCall
                  deriving Show

data BeginStmt = LetStmt [Binding]
               | DoStmt  FunctionCall
               deriving Show

data Program = Prog [BeginStmt]
             deriving Show

