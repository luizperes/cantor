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
unparseConst' (NumLit c) =
  case (ceiling c == floor c) of
    True -> show (ceiling c)
    False -> show c
unparseConst' (CharLit c) = "'" ++ [c] ++ "'"
unparseConst' (BoolLit c) = show c
unparseConst' (AnyLit) = "any"
unparseConst' (UniverseLit) = unparseType' TUniverse
unparseConst' (LambdaLit (id, _, _)) = id

unparseList' :: [Expression] -> [String] -> String
unparseList' [] strs = concat (intersperse ", " strs)
unparseList' (x:xs) strs = unparseList' xs (strs ++ ((unparseExpr' x):[]))

unparseRel' :: Relationship -> String
unparseRel' (SubsetOf) = unparseBinOp' Subset
unparseRel' (ElementOf) = unparseBinOp' In

unparseType' :: Type -> String
unparseType' (TUniverse) = "Universe"
unparseType' (TGroup ty) = "(" ++ (unparseType' ty) ++ ")"
unparseType' (TCustom binding) = binding
unparseType' (TBinOp op t1 t2) =
  case op of
   Union -> (unparseType' t1) ++ " ∪ " ++ (unparseType' t2)
   Intersection -> (unparseType' t1) ++ " \\ " ++ (unparseType' t2)
   CartProduct -> (unparseType' t1) ++ " *' " ++ (unparseType' t2)
   Difference -> (unparseType' t1) ++ " \\ " ++ (unparseType' t2)
   Function -> (unparseType' t1) ++ " -> " ++ (unparseType' t2)

unparseBType' :: BindingType -> String
unparseBType' (BType binds rel ty) =
  concat (intersperse ", " binds) ++
  " " ++ (unparseRel' rel) ++ " " ++
  (unparseType' ty)

unparseExpr' :: Expression -> String
unparseExpr' (EConst c) = unparseConst' c
unparseExpr' (EUnOp op expr) = (unparseUnOp' op) ++ (unparseExpr' expr)
unparseExpr' (EBinOp op e1 e2) =
  (unparseExpr' e1) ++ " " ++ (unparseBinOp' op) ++ " " ++ (unparseExpr' e2)
unparseExpr' (EBind binding) = binding

-- TODO: check whether or not case expressions are needed
unparseCaseExpr' :: CaseExpression -> String
unparseCaseExpr' expr = show expr

unparseUnOp' :: UnaryOp -> String
unparseUnOp' op =
  case op of
    Negation -> "~"
    Negative -> "-"

unparseBinOp' :: BinaryOp -> String
unparseBinOp' op =
  case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Exp -> "^"
    Eq  -> "="
    NEq -> "~"
    Gt  -> ">"
    GtE -> ">="
    Lt  -> "<"
    LtE -> "<="
    FCall -> "."
    Range -> ".."
    In -> "in"
    Subset -> "subset of"
    And -> ","
