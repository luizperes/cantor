module Interpreter where

import System.Environment
import Grammar

exec' :: Program -> [Constant]
exec' (Prog stmts) =
  interpret'
    (filter isDoStmt stmts)
    (flattenLetStmt (filter (\x -> not (isDoStmt x)) stmts) (LetStmt []))

flattenLetStmt :: [BeginStmt] -> BeginStmt -> BeginStmt
flattenLetStmt [] (LetStmt []) = LetStmt []
flattenLetStmt ((LetStmt x):[]) (LetStmt y) = LetStmt (y ++ x)
flattenLetStmt ((LetStmt x):xs) (LetStmt y) = flattenLetStmt xs (LetStmt (y ++ x))

isDoStmt :: BeginStmt -> Bool
isDoStmt (DoStmt _) = True
isDoStmt _ = False

interpret' :: [BeginStmt] -> BeginStmt -> [Constant]
interpret' doStmts letStmt =
  (map
    (\doStmt -> interpretDo' doStmt letStmt)
    doStmts)

interpretDo' :: BeginStmt -> BeginStmt -> Constant
interpretDo' (DoStmt fc) letStmt = interpretFCall' fc letStmt

interpretFCall' :: FunctionCall -> BeginStmt -> Constant
interpretFCall' (FCSingle x) _ = x
interpretFCall' (FCNested n fc) (LetStmt binds) =
  apply'
    n
    binds
    (filter
      (\bind -> case bind of
        BBind x _ _ -> x == n)
      binds)
    (interpretFCall' fc (LetStmt binds))

apply' :: BindingName -> [Binding] -> [Binding] -> Constant -> Constant
apply' (BId(IId id)) _ [] _ =
  Epsilon ("Binding `" ++ id ++ "' does not exist")
apply' _ binds (f:[]) c = applyFCall' f binds c
apply' _ _ ((BBind (BId(IId id)) _ _):_:[]) _ =
  Epsilon ("Binding `" ++ id ++ "' is duplicated")

applyFCall' :: Binding -> [Binding] -> Constant -> Constant
applyFCall' (BBind bind pattern expr) binds c =
  case matchType' (flattenType' pattern []) c of
    Left True -> applyExpr' expr pattern binds c
    Right r -> Epsilon
      ("Constant `" ++ (show c)  ++
       "' does not match type `" ++
       (show r) ++ "' in " ++ (show bind))

flattenType' :: PatternStmt -> [Type] -> [Type]
flattenType' (ForAllStmt []) ty = ty
flattenType' (ThereExistsStmt []) ty = ty
flattenType' (ForAllStmt ((BType _ _ x):xs)) ty =
  flattenType' (ForAllStmt xs) (ty ++ (x:[]))
flattenType' (ThereExistsStmt ((BType _ _ x):xs)) ty =
  flattenType' (ThereExistsStmt xs) (ty ++ (x:[]))

matchType' :: [Type] -> Constant -> Either Bool [Char]
matchType' [] _ = Left True
matchType' ((Universe):[]) _ = Left True
matchType' ((N):[]) (NatLit _) = Left True
matchType' ((Z):[]) (NatLit _) = Left True
matchType' ((Z):[]) (IntLit _) = Left True
matchType' ((R):[]) (FloatLit _) = Left True
matchType' _ _ = Right ("Not implemented")

applyExpr' :: [Expression] -> PatternStmt -> [Binding] -> Constant -> Constant
applyExpr' expr pattern binds c = c
