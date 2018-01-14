module Grammar where

import System.Environment

data Constant = StringLit [Char]
              | FloatLit Float
              | IntLit   Int

data Identifier = IId String

data BindingName = BId Identifier

data Type = N
          | Z
          | R
          | Universe
          | Custom Identifier

data PatternStmt = ForAllStmt BindingName Type
                 | ThereExistsStmt BindingName Type
                 | BindingStmt BindingName

data Factor = FConst  Constant
            | FBind   BindingName Factor
            | FParens Factor

data AddOp = Add
           | Sub

data MulOp = Mul
           | Div
           | Mod
           | Exp

data Term = TFactor Factor
          | TMFactor Factor MulOp Factor

data Expression = ETerm Term
                | EMTerm Term AddOp Expression

data Binding = BBind BindingName PatternStmt [Expression]

data BeginStmt = LetStmt [Binding]
               | DoStmt  Expression

data Grammar = GConstant Constant
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

