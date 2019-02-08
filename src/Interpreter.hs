module Interpreter where

import System.Environment
import qualified Data.Map.Strict as Map
import Data.List
import Grammar
import Unparsing
import TypeChecker

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
setupEnvs' [] (fEnv, bEnv) = (Map.fromList fEnv, Map.fromList bEnv)
setupEnvs' (x:xs) (fEnv, bEnv) =
  case x of
    BExpr bname expr ->
      setupEnvs' xs (fEnv, bEnv ++ [(bname, eval' expr (Map.fromList fEnv) (Map.fromList bEnv))])
    BBind bname params expr ->
      case extractBTypes' params of
        btys -> setupEnvs' xs (fEnv ++ [(bname, (btys, expr))], bEnv)

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
eval' (EBind v) _ bEnv =
  case Map.lookup v bEnv of
    Just value -> value
    _ -> Epsilon (v ++ " is not a valid binding name")
eval' (EUnOp Negation expr) fEnv bEnv =
  case eval' expr fEnv bEnv of
    BoolLit b -> BoolLit (not b)
    _ -> Epsilon ("Can't eval " ++ (unparseExpr' expr))
eval' (EUnOp Negative expr) fEnv bEnv =
  case (eval' expr fEnv bEnv) of
    n -> case isNumber' n of
      Just _ -> arithmType' (*) (IntLit (-1)) n
      _ -> Epsilon ("Can't eval " ++ (unparseExpr' expr))
eval' (EBinOp FCall (EBind bname) (EConst expr)) fEnv bEnv =
  case expr of
    SetLit [] -> eval' (EBind bname) fEnv bEnv
    _ -> case (eval' (EConst expr) fEnv bEnv) of
      evalExpr -> case (apply' bname evalExpr fEnv bEnv) of
        Just c -> c
        _ -> Epsilon ("Can't apply " ++ bname ++ " to " ++ (unparseExpr' (EConst expr)))
eval' (EBinOp FCall (EBind bname) expr) fEnv bEnv =
  eval'
    (EBinOp FCall (EBind bname) (EConst (eval' expr fEnv bEnv)))
    fEnv
    bEnv
eval' (EBinOp op expr1 expr2) fEnv bEnv =
  case (applyBinOp' op (eval' expr1 fEnv bEnv) (eval' expr2 fEnv bEnv)) of
    Epsilon r -> Epsilon r
    c -> c
eval' (EConst (SetLit list)) fEnv bEnv = SetLit (map (\x -> EConst(eval' x fEnv bEnv)) list)
eval' (EConst (TupleLit list)) fEnv bEnv = TupleLit (map (\x -> EConst(eval' x fEnv bEnv)) list)
eval' (EConst const) _ _ = const
eval' expr fEnv bEnv = Epsilon ("Can't eval " ++ (unparseExpr' expr))

evalCaseExpr' :: CaseExpression -> FunEnvMap -> BindEnvMap -> Constant
evalCaseExpr' (CEList [expr]) fEnv bEnv = eval' expr fEnv bEnv
evalCaseExpr' (CEList (x:xs)) fEnv bEnv =
  case (map (\expr -> eval' expr fEnv bEnv) xs) of
    consts ->
      case (evalConstListToBool' consts fEnv bEnv) of
        BoolLit True  -> eval' x fEnv bEnv
        BoolLit False -> BoolLit False
        _ ->
          Epsilon ("Expression(s): " ++
          (intercalate ", " (map (\expr -> unparseExpr' expr) xs)) ++
          " should eval to boolean, however it failed with: " ++
          (intercalate ", " (map (\c -> unparseConst' c) consts)))
evalCaseExpr' (CECase cases) fEnv bEnv = applyCond' cases fEnv bEnv

applyCond' :: [(Expression, [Expression])] -> FunEnvMap -> BindEnvMap -> Constant
applyCond' [] _ _ = BoolLit False
applyCond' ((expr, condExprs):conds) fEnv bEnv =
  case (map (\e -> eval' e fEnv bEnv) condExprs) of
    consts ->
      case (evalConstListToBool' consts fEnv bEnv) of
        BoolLit True -> eval' expr fEnv bEnv
        BoolLit False -> applyCond' conds fEnv bEnv
        _ ->
          Epsilon ("Expression(s): " ++
          (intercalate ", " (map (\e -> unparseExpr' e) condExprs)) ++
          " should eval to boolean, however it failed with: " ++
          (intercalate ", " (map (\c -> unparseConst' c) consts)))

evalConstListToBool' :: [Constant] -> FunEnvMap -> BindEnvMap -> Constant
evalConstListToBool' consts fEnv bEnv =
  case (foldl (\c1 c2 -> applyBinOp' And c1 c2) (BoolLit True) consts) of
    BoolLit True  -> BoolLit True
    BoolLit False -> BoolLit False
    sth_else -> sth_else

interpretDo' :: BeginStmt -> (FunEnvMap, BindEnvMap) -> Constant
interpretDo' (DoStmt fc) (fEnv, bEnv) = interpretFCall' fc (fEnv, bEnv)

interpretFCall' :: FunctionCall -> (FunEnvMap, BindEnvMap) -> Constant
interpretFCall' (FCSingle x) (fEnv, bEnv) = (eval' (EConst x) fEnv bEnv)
interpretFCall' (FCNested bname fc) (fEnv, bEnv) =
  eval'
    (EBinOp FCall (EBind bname) (EConst (interpretFCall' fc (fEnv, bEnv))))
    fEnv
    bEnv

applyBinOp' :: BinaryOp -> Constant -> Constant -> Constant
applyBinOp' op (BoolLit b1) (BoolLit b2) =
  case op of
    Eq  -> BoolLit (b1 == b2)
    NEq -> BoolLit (b1 /= b2)
    And -> BoolLit (b1 && b2) -- implicit case for expr list
applyBinOp' op c1 c2 =
  case (isNumber' c1, isNumber' c2) of
    (Just b1, Just b2) ->
      case op of
        Eq  -> BoolLit (b1 == b2)
        NEq -> BoolLit (b1 /= b2)
        Gt  -> BoolLit (b1 >  b2)
        GtE -> BoolLit (b1 >= b2)
        Lt  -> BoolLit (b1 <  b2)
        LtE -> BoolLit (b1 <= b2)
        Add -> arithmType' (+) c1 c2
        Sub -> arithmType' (-) c1 c2
        Mul -> arithmType' (*) c1 c2
        Div -> DoubleLit ((/) b1 b2)
        Mod -> case (c1, c2) of
          (DoubleLit _, _) -> Epsilon ("Can't apply mod to doubles!")
          (_, DoubleLit _) -> Epsilon ("Can't apply mod to doubles!")
          _ -> IntLit (fromIntegral ((mod) (ceiling b1) (ceiling b2)))
        Exp -> DoubleLit ((**) b1 b2)
        Range -> SetLit (map (\x -> EConst (DoubleLit x)) [b1..b2])
        _ ->
          Epsilon ("Can't apply (" ++ (unparseBinOp' op) ++ ") to " ++
          (unparseConst' c1) ++ " and " ++ (unparseConst' c2))
    _ ->
      case (isChar' c1, isChar' c2) of
        (Just cc1, Just cc2) ->
          case op of
            Eq  -> BoolLit (cc1 == cc2)
            NEq -> BoolLit (cc1 /= cc2)
            Range -> SetLit (map (\x -> EConst (CharLit x)) [cc1..cc2])
            _ ->
              Epsilon ("Can't apply (" ++ (unparseBinOp' op) ++ ") to " ++
              (unparseConst' c1) ++ " and " ++ (unparseConst' c2))
        _ ->
          Epsilon ("Can't apply (" ++ (unparseBinOp' op) ++ ") to " ++
          (unparseConst' c1) ++ " and " ++ (unparseConst' c2))

apply' :: BindingName -> Constant -> FunEnvMap -> BindEnvMap -> Maybe Constant
apply' bname input fEnv bEnv =
  case (Map.lookup bname fEnv) of
    Just (btys, expr) ->
      case (joinTysAndBinds' btys fEnv bEnv input) of
        Just paramsEnv ->
          Just (evalCaseExpr'
            expr
            fEnv
            (Map.union (Map.fromList paramsEnv) bEnv))
        _ -> Nothing
    _ -> Nothing

joinTysAndBinds' :: [BindingType] -> FunEnvMap -> BindEnvMap -> Constant -> Maybe [BindEnv]
joinTysAndBinds' [(BType [bname] rel ty)] fEnv bEnv c =
  joinTyAndBind' (BType [bname] rel ty) fEnv bEnv c
joinTysAndBinds' [(BType bnames rel ty)] fEnv bEnv (TupleLit ts) =
  joinTyAndBind' (BType bnames rel ty) fEnv bEnv (TupleLit ts)
joinTysAndBinds' btys fEnv bEnv (TupleLit ts) =
  case (map (\expr -> eval' expr fEnv bEnv) ts) of
    res ->
      case sequence (zipWith (\bty c -> joinTyAndBind' bty fEnv bEnv c) btys res) of
        Just lst -> Just (concat lst)
        _ -> Nothing
joinTysAndBinds' _ _ _ _ = Nothing

-- TODO: check types
-- TODO: implement tuples and sets
joinTyAndBind' :: BindingType -> FunEnvMap -> BindEnvMap -> Constant -> Maybe [BindEnv]
joinTyAndBind' (BType [bname] rel ty) _ _ c = Just [(bname, c)] -- check type
joinTyAndBind' (BType tnames rel ty) fEnv bEnv (TupleLit ts) =
  case (map (\expr -> eval' expr fEnv bEnv) ts) of
    res -> Just (zipWith (\x y -> (x, y)) tnames res)
joinTyAndBind' _ _ _ _ = Nothing
