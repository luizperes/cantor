module TypeChecker where
  
import Grammar

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
