module Interpreter where

import System.Environment
import Control.Monad
import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Grammar

languageDef =
  emptyDef { Token.commentStart    = "/'"
           , Token.commentEnd      = "'/"
           , Token.commentLine     = "#"
           , Token.identStart      =   letter
                                   <|> start_symbol
           , Token.identLetter     =   alphaNum
                                   <|> start_symbol
                                   <|> other_symbol
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
           , Token.reservedOpNames = [ "+", "-", "*", "/", "%", "^"
                                     , "<", ">", ">=", "<="
                                     , "=", "~"
                                     ]
           }

start_symbol = oneOf "!@#$_?"
other_symbol = oneOf "+-*/%^<>=~"

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    -- parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer number
natural    = Token.natural    lexer -- parses a natural number
float      = Token.float      lexer -- parses a floating number
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parse' :: Parser [BeginStmt]
parse' =  whiteSpace
       >> many1 statement'

letStmt :: Parser BeginStmt
letStmt = do
  reserved "let"
  binds <- (many1 binding')
  return $ LetStmt binds

doStmt :: Parser BeginStmt
doStmt = do
  reserved "do"
  return $ DoStmt(ETerm(TFactor(FConst(StringLit "Blah"))))

statement' :: Parser BeginStmt
statement' =   letStmt
           <|> doStmt

binding' :: Parser Binding
binding' = do
  id <- identifier
  reserved "="
  return $ BBind (BId (IId id)) (BindingStmt (BId (IId "blau"))) []

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseFile :: String -> IO () -- Program
parseFile file =
  case parseWithEof parse' file of
    Left e  -> print e
    Right r -> print r -- return (Prog r)
