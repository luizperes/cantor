module TypeChecker where

import System.Environment
import Data.List
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Grammar
import Unparsing

--
-- lookup in map is O(logn)
--
type Map = Map.Map
type FunEnv = (Identifier, ([BindingType], CaseExpression))
type FunEnvMap = Map Identifier ([BindingType], CaseExpression)
type BindEnv = (Identifier, Constant)
type BindEnvMap = Map Identifier Constant

isChar' :: Constant -> Maybe Char
isChar' (CharLit c) = Just c
isChar' _ = Nothing

allTysExist' :: [BindingType] -> BindEnvMap -> Constant
allTysExist' [] _ = BoolLit True
allTysExist' [(BType _ _ TUniverse)] _ = BoolLit True
allTysExist' [(BType _ _ (TCustom tc))] bEnv =
  case (Map.lookup tc bEnv) of
    Just v -> BoolLit True
    _ -> Epsilon ("Type " ++ (unparseType' (TCustom tc)) ++ " could not be found")
allTysExist' [(BType _ _ (TGroup group))] bEnv =
  allTysExist' [(BType [""] ElementOf group)] bEnv
allTysExist' [(BType _ _ (TBinOp _ t1 t2))] bEnv =
  case (allTysExist' [(BType [""] ElementOf  t1)] bEnv, allTysExist' [(BType [""] ElementOf t2)] bEnv) of
    (BoolLit True, BoolLit True) -> BoolLit True
    (Epsilon s, BoolLit True) -> Epsilon s
    (BoolLit True, Epsilon s) -> Epsilon s
allTysExist' (x:xs) bEnv =
  case (allTysExist' [x] bEnv) of
    BoolLit True -> allTysExist' xs bEnv
    Epsilon s -> Epsilon s

{-
 - Type rules are as described on TYPE_RULES.md file
 -}

cartProd xs ys = [(x,y) | x <- xs, y <- ys]

matchType' :: Relationship -> Type -> Maybe (Set Constant) -> Constant -> Bool
matchType' ElementOf TUniverse _ _ = True
matchType' SubsetOf TUniverse _ _ = True
matchType' ElementOf (TCustom ty) (Just s) c =
  case c of
    NumLit n -> Set.member (NumLit n) s
    _ -> Set.member c s
matchType' _ _ _ _ = False
-- Tuple
-- TODO:

-- List
-- TODO:

-- Custom
-- TODO:
