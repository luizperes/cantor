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

-- Natural
matchType' (TN) (ElementOf) (NatLit _) = True
matchType' (TN) (ElementOf) (IntLit n)
  | n >= 0    = True
  | otherwise = False
matchType' (TN) (ElementOf) (_) = False

-- Integer
matchType' (TZ) (ElementOf) (NatLit _) = True
matchType' (TZ) (ElementOf) (IntLit _) = True
matchType' (TZ) (ElementOf) (_) = False

-- Double
matchType' (TR) (ElementOf) (NatLit _) = True
matchType' (TR) (ElementOf) (IntLit _) = True
matchType' (TR) (ElementOf) (DoubleLit _) = True
matchType' (TR) (ElementOf) (_) = False

-- Char
matchType' (TChar) (ElementOf) (CharLit _) = True
matchType' (TChar) (ElementOf) (_) = False

-- Tuple
-- TODO:

-- List
-- TODO:

-- Custom
-- TODO:

-- Universe
matchType' (TUniverse) (ElementOf) (_) = True
matchType' (TUniverse) (SubsetOf) (_) = True
