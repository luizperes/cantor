module Interpreter where

import System.Environment
import qualified Data.Map.Strict as Map
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

exec' :: Program -> [Constant]
exec' (Prog stmts) =
  case sepBeginStmts stmts([],[]) of
    (dos, lets) ->
      case (flattenLets' lets (LetStmt [])) of
        LetStmt bindings -> interpret' dos (setupEnvs' bindings ([], []))

flattenLets' :: [BeginStmt] -> BeginStmt -> BeginStmt
flattenLets' [] (LetStmt acc) = LetStmt acc
flattenLets' ((LetStmt x):xs) (LetStmt acc) = flattenLets' xs (LetStmt (acc ++ x))

sepBeginStmts :: [BeginStmt] -> ([BeginStmt], [BeginStmt]) -> ([BeginStmt], [BeginStmt])
sepBeginStmts [] tp = tp
sepBeginStmts (x:xs) (dos, lets) =
  case x of
    (DoStmt _) -> sepBeginStmts xs (dos ++ [x], lets)
    _ -> sepBeginStmts xs (dos, lets ++ [x])

setupEnvs' :: [Binding] -> ([FunEnv], [BindEnv]) -> (FunEnvMap, BindEnvMap)
setupEnvs' [] (fnEnv, varEnv) = (Map.fromList fnEnv, Map.fromList varEnv)
setupEnvs' (x:xs) (fnEnv, varEnv) =
  case x of
    BExpr bname expr ->
      setupEnvs' xs (fnEnv, varEnv ++ [(bname, eval' expr (Map.fromList fnEnv) (Map.fromList varEnv))])
    BBind bname params expr ->
      case extractBTypes' params of
        btys -> setupEnvs' xs (fnEnv ++ [(bname, (btys, expr))], varEnv)

extractBTypes' :: PatternStmt -> [BindingType]
extractBTypes' (SimpleStmt bty) = [bty]
extractBTypes' (PatternListStmt []) = []
extractBTypes' (PatternListStmt (x:xs)) =
  (extractBTypes' x) ++ (extractBTypes' (PatternListStmt xs))

interpret' :: [BeginStmt] -> (FunEnvMap, BindEnvMap) -> [Constant]
interpret' doStmts (fEnv, bEnv) =
  (map
    (\doStmt -> interpretDo' doStmt (fEnv, bEnv))
    doStmts)

eval' :: Expression -> FunEnvMap -> BindEnvMap -> Constant
eval' expr fEnv bEnv = BoolLit False 

interpretDo' :: BeginStmt -> (FunEnvMap, BindEnvMap) -> Constant
interpretDo' (DoStmt fc) (fEnv, bEnv) = interpretFCall' fc (fEnv, bEnv)

interpretFCall' :: FunctionCall -> (FunEnvMap, BindEnvMap) -> Constant
interpretFCall' (FCSingle x) _ = x
interpretFCall' (FCNested bname fc) (fEnv, bEnv) =
  eval'
    (EBinOp FCall (EBind bname) (EConst (interpretFCall' fc (fEnv, bEnv))))
    fEnv
    bEnv

apply' :: BindingName -> [Binding] -> [Binding] -> Constant -> Constant
apply' id _ [] _ =
  Epsilon ("Binding `" ++ id ++ "' does not exist")
apply' _ binds (f:[]) c = applyFCall' f binds c
apply' _ _ ((BBind id _ _):_:[]) _ =
  Epsilon ("Binding `" ++ id ++ "' is duplicated")

applyFCall' :: Binding -> [Binding] -> Constant -> Constant
applyFCall' (BBind bind pattern expr) binds c = c
--  case checkTypes' pattern c of
--    Left True -> applyExpr' expr pattern binds c
--    Right r -> Epsilon (r ++ " in " ++ (unparseBind' bind))

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
matchType' (TR) (ElementOf) (DoubleLit _) = True
matchType' _ _ _= False

applyExpr' :: CaseExpression -> PatternStmt -> [Binding] -> Constant -> Constant
applyExpr' expr pattern binds c = c
