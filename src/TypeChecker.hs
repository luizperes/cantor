module TypeChecker where

import System.Environment
import Data.List
import qualified Data.Map.Strict as Map
import Grammar

--
-- lookup in map is O(logn)
--
type Map = Map.Map
type FunEnv = (Identifier, ([BindingType], CaseExpression))
type FunEnvMap = Map Identifier ([BindingType], CaseExpression)
type BindEnv = (Identifier, Constant)
type BindEnvMap = Map Identifier Constant

isNumber' :: Constant -> Maybe Double
isNumber' (NatLit c) = Just c
isNumber' (IntLit c) = Just c
isNumber' (DoubleLit c) = Just c
isNumber' _ = Nothing

isChar' :: Constant -> Maybe Char
isChar' (CharLit c) = Just c
isChar' _ = Nothing

arithmType' fn (NatLit a) (NatLit b) = NatLit (fn a b)
arithmType' fn (IntLit a) (IntLit b) = IntLit (fn a b)
arithmType' fn (IntLit a) (NatLit b) = IntLit (fn a b)
arithmType' fn (NatLit a) (IntLit b) = IntLit (fn a b)
arithmType' fn (DoubleLit a) (NatLit b) = DoubleLit (fn a b)
arithmType' fn (NatLit a) (DoubleLit b) = DoubleLit (fn a b)
arithmType' fn (DoubleLit a) (IntLit b) = DoubleLit (fn a b)
arithmType' fn (IntLit a) (DoubleLit b) = DoubleLit (fn a b)
arithmType' fn (DoubleLit a) (DoubleLit b) = DoubleLit (fn a b)

{-
 - Type rules are as described on TYPE_RULES.md file
 -}

matchType' :: BindingName -> Relationship -> Type -> Constant -> FunEnvMap -> BindEnvMap -> Bool

-- Tuple
-- TODO:

-- List
-- TODO:

-- Custom
-- TODO:

-- Universe
matchType' _ ElementOf TUniverse _ _ _ = True
matchType' _ SubsetOf TUniverse _ _ _= True
matchType' _ _ _ _ _ _ = False
