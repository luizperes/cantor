module Interpreter where

import System.Environment
import Grammar

parseMain :: [Char] -> Maybe BeginStmt
parseMain line = case line of
                      _ -> Nothing

parse :: [[Char]] -> [BeginStmt]
parse lines = case lines of
                   [] -> []
                   (x:xs) -> case (parseMain x) of
                                  Just l -> l : (parse xs)
                                  _ -> []
  

exec :: Program -> Bool
exec p = True

matching :: [[Char]] -> Bool
matching lines = case parse lines of
                      [] -> False
                      p -> exec (Prog p)
