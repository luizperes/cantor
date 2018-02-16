module Grammar where

import System.Environment

data Constant = StringLit [Char]
              | FloatLit  Double
              | IntLit    Integer
              | NatLit    Integer
              | SetLit    [Constant]
              | TupleLit  [Constant]
              deriving Show

data Identifier = IId String
                deriving Show

data BindingName = BId Identifier
                 deriving Show

data Type = N
          | Z
          | R
          | Universe
          | CustomType BindingName
          deriving Show

data Relationship = SubsetOf
                  | ElementOf
                  deriving Show

data BindingType = BType BindingName Relationship Type
                 deriving Show

data PatternStmt = ForAllStmt [BindingType]
                 | ThereExistsStmt [BindingType]
                 deriving Show

data Expression = EBinOp BinaryOp Expression Expression
                | EConst Constant
                | EFCall FunctionCall
                | EBind BindingName
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
              deriving Show

data Binding = BBind BindingName PatternStmt [Expression]
             deriving Show

data FunctionCall = FCSingle Constant
                  | FCExpr   BindingName Expression
                  | FCNested BindingName FunctionCall
                  deriving Show

data BeginStmt = LetStmt [Binding]
               | DoStmt  FunctionCall
               deriving Show

data Program = Epsilon
             | Prog [BeginStmt]
             deriving Show

