module Interpreter where

import System.Environment
import Data.Char
import Grammar

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
  | isSpace x = dropSpaceTail (x:maybeStuff) xs
  | null maybeStuff = x : dropSpaceTail "" xs
  | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

matchStr :: [Char] -> [Char] -> Bool
matchStr _ [] = False
matchStr [] _ = False
matchStr (' ':xs) (y:ys)
  | isSpace y =  matchStr xs (trim ys)
  | otherwise = False
matchStr (x:xs) (y:ys)
  | x == y = matchStr xs ys
  | otherwise = False

parseMain :: [Char] -> Result BeginStmt
parseMain line =
  case line of
    'l':'e':'t':xs -> Left(LetStmt [])
    'd':'o':xs -> Left(DoStmt (ETerm (TFactor (FConst (StringLit "Blah")))))
    _ -> Right(Failure "Only `let' and `do' are valid initial statements")

parse :: [[Char]] -> [Result BeginStmt]
parse [] = []
parse (x:xs) =
  case (parseMain x) of
    Left l -> Left (l) : (parse xs)
    Right (Failure f) -> Right (Failure f) : parse (xs)

exec :: Program -> Bool
exec p = True

matching :: [[Char]] -> Bool
matching lines =
  case parse lines of
    [] -> False
    p  ->  exec (Prog p)
