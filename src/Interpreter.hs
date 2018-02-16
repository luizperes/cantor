module Interpreter where

import System.Environment
import Grammar

exec' :: Program -> [Constant]
exec' (Prog stmts) =
  interpret'
    (filter isDoStmt stmts)
    (filter (\x -> not (isDoStmt x)) stmts)

isDoStmt :: BeginStmt -> Bool
isDoStmt (DoStmt _) = True
isDoStmt _ = False

interpret' :: [BeginStmt] -> [BeginStmt] -> [Constant]
interpret' doStmts letStmts =
  (map
    (\doStmt -> interpretDo' doStmt letStmts)
    doStmts)

interpretDo' :: BeginStmt -> [BeginStmt] -> Constant
interpretDo' doStmt letStmts = SetLit [IntLit 1]
