module Grammar where

import System.Environment

data Constant = StringLit [Char]
              | FloatLit Float
              | IntLit   Int
              deriving Show

data Identifier = IId String
                deriving Show

data BindingName = BId Identifier
                 deriving Show

data Type = N
          | Z
          | R
          | Universe
          | Custom Identifier
          deriving Show

data PatternStmt = ForAllStmt BindingName Type
                 | ThereExistsStmt BindingName Type
                 | BindingStmt BindingName
                 deriving Show

data Factor = FConst  Constant
            | FBind   BindingName Factor
            | FParens Factor
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

data BeginStmt = LetStmt [Binding]
               | DoStmt  Expression
               deriving Show

data Grammar = Epsilon
             | GConstant Constant
             | GIdentifier Identifier
             | GBindingName BindingName
             | GType Type
             | GPatternStmt PatternStmt
             | GFactor Factor
             | GAddOp AddOp
             | GMulOp MulOp
             | GTerm Term
             | GExpression Expression
             | GBinding Binding
             | GBeginStmt BeginStmt
             | GProgram [BeginStmt]
             deriving Show

