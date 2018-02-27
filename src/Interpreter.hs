module Interpreter where

import System.Environment
import Grammar
import Unparsing

--exec' :: Program -> [Constant]
--exec' (Prog stmts) = interpret' (sepBeginStmts stmts ([], []))

exec' :: Program -> [Char]
exec' (Prog stmts) =
  case sepBeginStmts stmts([],[]) of
    (dos, lets) -> (show lets) ++ "\n\n" ++ (show dos)

sepBeginStmts :: [BeginStmt] -> ([BeginStmt], [BeginStmt]) -> ([BeginStmt], [BeginStmt])
sepBeginStmts [] tp = tp
sepBeginStmts (x:xs) (dos, lets) =
  case x of
    (DoStmt _) -> sepBeginStmts xs (dos ++ [x], lets)
    _ -> sepBeginStmts xs (dos, lets ++ [x])

flattenLetStmt :: [BeginStmt] -> BeginStmt -> BeginStmt
flattenLetStmt [] (LetStmt []) = LetStmt []
flattenLetStmt ((LetStmt x):[]) (LetStmt y) = LetStmt (y ++ x)
flattenLetStmt ((LetStmt x):xs) (LetStmt y) = flattenLetStmt xs (LetStmt (y ++ x))

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
        BBind x _ _ -> x == n
        BExpr x _ -> x == n)
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

-- TODO: fix flattenType, think in a better approach
flattenType' :: PatternStmt -> [Type] -> [Relationship] -> ([Type], [Relationship])
flattenType' (SimpleStmt (BType _ r t)) ty rel = (ty ++ [t], rel ++ [r])
flattenType' (ForAllStmt []) ty rel = (ty, rel)
flattenType' (ThereExistsStmt []) ty rel = (ty, rel)
flattenType' (ForAllStmt ((BType _ r t):xs)) ty rel =
  flattenType' (ForAllStmt xs) (ty ++ (t:[])) (rel ++ (r:[]))
flattenType' (ThereExistsStmt ((BType _ r t):xs)) ty rel =
  flattenType' (ThereExistsStmt xs) (ty ++ (t:[])) (rel ++ (r:[]))
flattenType' _ ty rel = (ty, rel)

checkTypes' :: ([Type], [Relationship]) -> Constant -> Either Bool [Char]
checkTypes' ([], []) _ = Left True
checkTypes' (t:[], r:[]) c =
  case matchType' t r c of
    True -> Left True
    _ -> Right ("`" ++ (unparseConst' c) ++ "' is not `" ++
         (unparseRel' r) ++ " " ++ (unparseType' t) ++ "'")
checkTypes' (tys, rels) cs = Right "Blah"

matchType' :: Type -> Relationship -> Constant -> Bool
matchType' (TUniverse) _ _ = True
matchType' (TN) (ElementOf) (NatLit _) = True
matchType' (TZ) (ElementOf) (NatLit _) = True
matchType' (TZ) (ElementOf) (IntLit _) = True
matchType' (TR) (ElementOf) (FloatLit _) = True
matchType' _ _ _= False

applyExpr' :: [Expression] -> PatternStmt -> [Binding] -> Constant -> Constant
applyExpr' expr pattern binds c = c


