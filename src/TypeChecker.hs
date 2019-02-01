module TypeChecker where
  
import Grammar

impNumber' :: Constant -> Maybe Double
impNumber' (NatLit c) = Just c
impNumber' (IntLit c) = Just c
impNumber' (DoubleLit c) = Just c
impNumber' _ = Nothing

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
