module Unparsing where

import System.Environment
import Data.List
import Grammar

unparseConstList' :: [Constant] -> [String] -> String
unparseConstList' [] strs = concat (intersperse "\n" strs)
unparseConstList' (x:xs) strs = unparseConstList' xs (strs ++ ((unparseConst' x):[]))

unparseConst' :: Constant -> String
unparseConst' (SetLit cs) = "{" ++ (unparseList' cs []) ++ "}"
unparseConst' (TupleLit cs) = "(" ++ (unparseList' cs []) ++ ")"
unparseConst' (Epsilon c) = c
unparseConst' (NatLit c) = show (ceiling c)
unparseConst' (IntLit c) = show (ceiling c)
unparseConst' (DoubleLit c) = show c
unparseConst' (CharLit c) = "'" ++ [c] ++ "'"
unparseConst' (BoolLit c) = show c
unparseConst' (TypeLit c) = unparseType' c

unparseList' :: [Expression] -> [String] -> String
unparseList' [] strs = concat (intersperse ", " strs)
unparseList' (x:xs) strs = unparseList' xs (strs ++ ((unparseExpr' x):[]))

unparseRel' :: Relationship -> String
unparseRel' (SubsetOf) = "subset of"
unparseRel' (ElementOf) = "in"

unparseBind' :: BindingName -> String
unparseBind' id = id

unparseType' :: Type -> String
unparseType' (TUniverse) = "Universe"
unparseType' t = show t

unparseExpr' :: Expression -> String
unparseExpr' (EConst c) = unparseConst' c
unparseExpr' expr = show expr

-- TODO: write the unparsing for the expressions
-- EBinOp BinaryOp Expression Expression
--                | EUnOp UnaryOp Expression
--                | EConst Constant
--                | EBind BindingName
--                | EQtOp Quantif BindingType

unparseCaseExpr' :: CaseExpression -> String
unparseCaseExpr' expr = show expr
