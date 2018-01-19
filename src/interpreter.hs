module Interpreter where

import System.Environment
import Grammar

data Result a = Success a
              | Failure [Char]

parseMain :: [Char] -> Result BeginStmt
parseMain line = case line of
                     'l':'e':'t':xs -> Success (LetStmt [])
                     'd':'o':xs -> Success (DoStmt (ETerm (TFactor (FConst (StringLit "Blah")))))
                     _ -> Failure "Only `let' and `do' are valid initial statements"

parse :: [[Char]] -> [BeginStmt]
parse lines = case lines of
                   [] -> []
                   (x:xs) -> case (parseMain x) of
                                  Success l -> l : (parse xs)
                                  Failure c -> []
  

exec :: Program -> Bool
exec p = True

matching :: [[Char]] -> Bool
matching lines = case parse lines of
                      [] -> False
                      p -> exec (Prog p)
