module Grammar where

import System.Environment

data Constant = Epsilon   [Char]
              | BoolLit   Bool
              | CharLit   Char
              | DoubleLit Double
              | IntLit    Integer
              | NatLit    Integer
              | SetLit    [Expression]
              | TupleLit  [Expression]
              deriving Show

data Identifier = IId String
                deriving (Show, Eq)

data BindingName = BId Identifier
                 deriving (Show, Eq)

data Type = TN
          | TZ
          | TR
          | TChar
          | TUniverse
          | TCustom BindingName
          deriving Show

data Relationship = SubsetOf
                  | ElementOf
                  deriving Show

data BindingType = BType BindingName Relationship Type
                 | BMultType [BindingName] Relationship Type
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
                | EFCall FunctionCall
                | EBind BindingName
                | EType Type
                | EQtOp Quantif BindingType
                deriving Show

data Quantif = ForAll
             | ThereExists
             deriving Show

data UnaryOp = Neg
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
              | Range
              | In
              | Subset
              | Def
              deriving Show

data Binding = BBind BindingName PatternStmt CaseExpression
             | BExpr BindingName Expression
             deriving Show

data FunctionCall = FCSingle Constant
                  | FCExpr   BindingName Expression
                  | FCNested BindingName FunctionCall
                  deriving Show

data BeginStmt = LetStmt [Binding]
               | DoStmt  FunctionCall
               deriving Show

data Program = Prog [BeginStmt]
             deriving Show

