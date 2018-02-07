module Interpreter where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Grammar

data Result a = Success a
              | Failure [Char]
              deriving Show

languageDef =
  emptyDef { Token.commentStart    = "/'"
           , Token.commentEnd      = "'/"
           , Token.commentLine     = "#"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "let"
                                     , "do"
                                     , "for"
                                     , "all"
                                     , "there"
                                     , "exists"
                                     , "true"
                                     , "subset"
                                     , "set"
                                     , "in"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "%"
                                     , "<", ">", ">=", "<="
                                     , "=", "~"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parse' :: Parser [Result BeginStmt]
parse' = whiteSpace >> many1 statement'

letStmt :: Parser (Result BeginStmt)
letStmt =
  do reserved "let"
     return $ Success(LetStmt [])

doStmt :: Parser (Result BeginStmt)
doStmt =
  do reserved "do"
     return $ Success(DoStmt(ETerm(TFactor(FConst(StringLit "Blah")))))

endOrFail :: [Char] -> Parser (Result a)
endOrFail what =
  do return $ Failure (what)

statement' :: Parser (Result BeginStmt)
statement' =   letStmt
           <|> doStmt

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseFile :: String -> IO () -- Program
parseFile file =
  case parseWithEof parse' file of
    Left e  -> print e
    Right r -> print r -- return (Prog r)
