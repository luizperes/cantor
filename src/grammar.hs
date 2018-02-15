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

data Factor = FConst  Constant
            | FParens Expression
            deriving Show

data AddOp = Add
           | Sub
           deriving Show

data MulOp = Mul
           | Div
           | Mod
           | Exp
           deriving Show

data Term = TFactor Factor
          | TMFactor Factor MulOp Factor
          deriving Show

data Expression = ETerm Term
                | EMTerm Term AddOp Expression
                deriving Show

data Binding = BBind BindingName PatternStmt [Expression]
             deriving Show

data FunctionCall = FCSingle Constant
                  | FCNested BindingName FunctionCall
                  deriving Show

data BeginStmt = LetStmt [Binding]
               | DoStmt  FunctionCall
               deriving Show

data Program = Epsilon
             | Prog [BeginStmt]
             deriving Show

