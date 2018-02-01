module Interpreter where

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Grammar

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

parse' :: Parser [BeginStmt]
parse' = whiteSpace >> (many statement')

letStmt :: Parser BeginStmt
letStmt =
  do reserved "let"
     return $ LetStmt []

doStmt :: Parser BeginStmt
doStmt =
  do reserved "do"
     return $ DoStmt (ETerm (TFactor (FConst (StringLit "Blah"))))

statement' :: Parser BeginStmt
statement' =   letStmt
           <|> doStmt

parseFile :: String -> IO () -- Program
parseFile file =
  case parse parse' "" file of
    Left e  -> print e >> fail "parse error"
    Right r -> print r -- return (Prog r)
