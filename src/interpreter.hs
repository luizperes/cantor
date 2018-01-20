module Interpreter where

import System.Environment
import Grammar

data Fail = Failure [Char]
type Result a = Either a Fail 

parseMain :: [Char] -> Result BeginStmt
parseMain line =
  case line of
    'l':'e':'t':xs -> Left(LetStmt [])
    'd':'o':xs -> Left(DoStmt (ETerm (TFactor (FConst (StringLit "Blah")))))
    _ -> Right(Failure "Only `let' and `do' are valid initial statements")

parse :: [[Char]] -> Result [BeginStmt]
parse [] = Left([])
parse (x:xs) =
  case (parseMain x) of
    Left l -> Left $ l : (parse xs)
    Right (Failure f) -> Right (Failure f)

exec :: Program -> Bool
exec p = True

matching :: [[Char]] -> Bool
matching lines = case parse lines of
                      Right c -> False
                      Left  p -> exec (Prog p)
