module Grammar where

import System.Environment

data Constant = Epsilon   [Char]
              | BoolLit   Bool
              | CharLit   Char
              | NumLit    Double
              | SetLit    [Expression]
              | TupleLit  [Expression]
              | AnyLit
              | LambdaLit (BindingName, [BindingType], CaseExpression)
              | SProdLit  [(Constant, Constant)] -- implicit
              | UniverseLit -- implicit
              deriving (Show, Ord, Eq)

type Identifier = String

type BindingName = Identifier

data Type = TUniverse
          | TCustom BindingName
          | TBinOp SetOp Type Type
          | TGroup Type
          deriving (Show, Ord, Eq) 

data SetOp = Union
           | Intersection
           | CartProduct
           | Difference
           | Function
           deriving (Show, Ord, Eq)

data Relationship = SubsetOf
                  | ElementOf
                  deriving (Show, Ord, Eq) 

data BindingType = BType [BindingName] Relationship Type
                 deriving (Show, Ord, Eq)

data PatternStmt = SimpleStmt BindingType
                 | PatternListStmt [PatternStmt]
                 deriving Show

data CaseExpression = CEList [Expression]
                    | CECase [(Expression, [Expression])]
                    deriving (Show, Ord, Eq)

data Expression = EBinOp BinaryOp Expression Expression
                | EUnOp UnaryOp Expression
                | EConst Constant
                | EBind BindingName
                deriving (Show, Ord, Eq)

data UnaryOp = Negation
             | Negative
             deriving (Show, Ord, Eq)

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
              | And -- implicit
              deriving (Show, Ord, Eq)

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

