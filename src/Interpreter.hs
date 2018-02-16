module Interpreter where

import System.Environment
import Grammar

exec' :: Program -> String
exec' (Prog stmts) = interpret' (filter isDoStmt stmts) (filter (\x -> not (isDoStmt x)) stmts)

isDoStmt :: BeginStmt -> Bool
isDoStmt (DoStmt _) = True
isDoStmt _ = False

interpret' :: [BeginStmt] -> [BeginStmt] -> String
interpret' doStmts letStmts = show doStmts
