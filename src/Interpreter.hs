module Interpreter where

import System.Environment
import Grammar
import Unparsing

exec' :: Program -> [Constant]
exec' (Prog stmts) =
  interpret'
    (filter isDoStmt stmts)
    (flattenLetStmt (filter (\x -> not (isDoStmt x)) stmts) (LetStmt []))

flattenLetStmt :: [BeginStmt] -> BeginStmt -> BeginStmt
flattenLetStmt [] (LetStmt []) = LetStmt []
flattenLetStmt ((LetStmt x):[]) (LetStmt y) = LetStmt (y ++ x)
flattenLetStmt ((LetStmt x):xs) (LetStmt y) = flattenLetStmt xs (LetStmt (y ++ x))

isDoStmt :: BeginStmt -> Bool
isDoStmt (DoStmt _) = True
isDoStmt _ = False

interpret' :: [BeginStmt] -> BeginStmt -> [Constant]
interpret' doStmts letStmt =
  (map
    (\doStmt -> interpretDo' doStmt letStmt)
    doStmts)

interpretDo' :: BeginStmt -> BeginStmt -> Constant
interpretDo' (DoStmt fc) letStmt = interpretFCall' fc letStmt

interpretFCall' :: FunctionCall -> BeginStmt -> Constant
interpretFCall' (FCSingle x) _ = x
interpretFCall' (FCNested n fc) (LetStmt binds) =
  apply'
    n
    binds
    (filter
      (\bind -> case bind of
        BBind x _ _ -> x == n)
      binds)
    (interpretFCall' fc (LetStmt binds))

apply' :: BindingName -> [Binding] -> [Binding] -> Constant -> Constant
apply' (BId(IId id)) _ [] _ =
  Epsilon ("Binding `" ++ id ++ "' does not exist")
apply' _ binds (f:[]) c = applyFCall' f binds c
apply' _ _ ((BBind (BId(IId id)) _ _):_:[]) _ =
  Epsilon ("Binding `" ++ id ++ "' is duplicated")

applyFCall' :: Binding -> [Binding] -> Constant -> Constant
applyFCall' (BBind bind pattern expr) binds c =
  case checkTypes' (flattenType' pattern [] []) c of
    Left True -> applyExpr' expr pattern binds c
    Right r -> Epsilon (r ++ " in " ++ (unparseBind' bind))

flattenType' :: PatternStmt -> [Type] -> [Relationship] -> ([Type], [Relationship])
flattenType' (ForAllStmt []) ty rel = (ty, rel)
flattenType' (ThereExistsStmt []) ty rel = (ty, rel)
flattenType' (ForAllStmt ((BType _ r t):xs)) ty rel =
  flattenType' (ForAllStmt xs) (ty ++ (t:[])) (rel ++ (r:[]))
flattenType' (ThereExistsStmt ((BType _ r t):xs)) ty rel =
  flattenType' (ThereExistsStmt xs) (ty ++ (t:[])) (rel ++ (r:[]))

checkTypes' :: ([Type], [Relationship]) -> Constant -> Either Bool [Char]
checkTypes' ([], []) _ = Left True
checkTypes' (t:[], r:[]) c =
  case matchType' t r c of
    True -> Left True
    _ -> Right ("`" ++ (unparseConst' c) ++ "' is not `" ++
         (unparseRel' r) ++ " " ++ (unparseType' t) ++ "'")
checkTypes' (tys, rels) cs = Right "Blah"

matchType' :: Type -> Relationship -> Constant -> Bool
matchType' (Universe) _ _ = True
matchType' (N) _ (NatLit _) = True
matchType' (Z) _ (NatLit _) = True
matchType' (Z) _ (IntLit _) = True
matchType' (R) _ (FloatLit _) = True
matchType' _ _ _= False

applyExpr' :: [Expression] -> PatternStmt -> [Binding] -> Constant -> Constant
applyExpr' expr pattern binds c = c
