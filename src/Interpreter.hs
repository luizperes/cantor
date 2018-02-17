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
applyFCall' _ binds c = c
