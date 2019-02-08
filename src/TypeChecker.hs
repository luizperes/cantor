module TypeChecker where
  
import Grammar

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

matchType' :: Type -> Relationship -> Constant -> Bool

-- Tuple
-- TODO:

-- List
-- TODO:

-- Custom
-- TODO:

-- Universe
matchType' (TUniverse) (ElementOf) (_) = True
matchType' (TUniverse) (SubsetOf) (_) = True
