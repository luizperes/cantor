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
                                     , "subset"
                                     , "of"
                                     , "in"
                                     , "N"
                                     , "Z"
                                     , "R"
                                     , "Universe"
                                     ]
           , Token.reservedOpNames = [ "+", "-", "*", "/", "%", "^"
                                     , "<", ">", ">=", "<="
                                     , "=", "~"
                                     , "∀", "∃", "∈", "⊆"
                                     ]
           , caseSensitive         = True
           }

start_symbol = oneOf "!@$_?|"
other_symbol = oneOf "+-*/%^<>~"

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
symbol     = Token.symbol     lexer -- parses symbols

commaSep p = p `sepBy` (symbol ",")
braces p   = between (symbol "{") (symbol "}") p

parse' :: Parser [BeginStmt]
parse' =  whiteSpace
       >> many1 statement'

letStmt' :: Parser BeginStmt
letStmt' = do
  reserved "let"
  binds <- (many1 binding')
  return $ LetStmt binds

doStmt' :: Parser BeginStmt
doStmt' = do
  reserved "do"
  fcall <- functionCall'
  return $ DoStmt fcall

functionCall' :: Parser FunctionCall
functionCall' =   singleFunctionCall'
              <|> nestedFunctionCall'

singleFunctionCall' :: Parser FunctionCall
singleFunctionCall' = do
  set <- set'
  return $ FCSingle set

nestedFunctionCall' :: Parser FunctionCall
nestedFunctionCall' = do
  bindName <- bindingName'
  symbol "."
  result <- (singleFunctionCall' <|> nestedFunctionCall')
  return $ FCNested bindName result

set' :: Parser Set
set' = do
  list <- braces (commaSep constant')
  return $ SetLit list

constant' :: Parser Constant
constant' = try
              floatLit'
          <|> naturalLit'
          <|> intLit'

floatLit' :: Parser Constant
floatLit' = do
  f <- float
  return $ FloatLit f

naturalLit' :: Parser Constant
naturalLit' = (natural >>= (\n -> return $ NatLit n))

intLit' :: Parser Constant
intLit' = (integer >>= (\i -> return $ IntLit i))

statement' :: Parser BeginStmt
statement' =   letStmt'
           <|> doStmt'

binding' :: Parser Binding
binding' = do
  bindName <- bindingName'
  symbol "="
  pattern <- patternStmt'
  symbol ":"
  return $ BBind bindName pattern []

bindingName' :: Parser BindingName
bindingName' = do
  id <- identifier
  return $ BId (IId id)

patternStmt' :: Parser PatternStmt
patternStmt' =   forAllStmt'
             <|> thereExistsStmt'

forAllStmt' :: Parser PatternStmt
forAllStmt' = do
  ((reserved "for" >> reserved "all") <|> reservedOp "∀")
  bindName <- bindingName'
  relation <- relationship'
  ty <- type'
  return $ ForAllStmt bindName relation ty

thereExistsStmt' :: Parser PatternStmt
thereExistsStmt' = do
  ((reserved "there" >> reserved "exists") <|> reservedOp "∃")
  bindName <- bindingName'
  relation <- relationship'
  ty <- type'
  return $ ThereExistsStmt bindName relation ty

relationship' :: Parser Relationship
relationship' =   subsetOf'
              <|> elementOf'

subsetOf' =  ((reserved "subset" >> reserved "of")
          <|>  reservedOp "⊆")
          >> return SubsetOf
elementOf' = (reserved "in" <|> reservedOp "∈") >> return ElementOf

type' :: Parser Type
type' =   typeN'
      <|> typeZ'
      <|> typeR'
      <|> typeUniverse'
      <|> typeCustom'

typeN' = reserved "N" >> return N
typeZ' = reserved "Z" >> return Z
typeR' = reserved "R" >> return R
typeUniverse' = reserved "Universe" >> return Universe
typeCustom' = do
  bindName <- bindingName'
  return $ CustomType bindName

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseFile :: String -> IO () -- Program
parseFile file =
  case parseWithEof parse' file of
    Left e  -> print e
    Right r -> print r -- return (Prog r)
