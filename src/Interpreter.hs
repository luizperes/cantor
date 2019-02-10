module Interpreter where

import System.Environment
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import System.IO.Unsafe
import System.Random
import Grammar
import Unparsing
import TypeChecker

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
    n -> case n of
      NumLit v -> NumLit ((*) (-1) v)
      _ -> Epsilon ("Can't eval " ++ (unparseExpr' expr))
eval' (EBinOp FCall (EBind bname) (EConst expr)) fEnv bEnv =
  case expr of
    SetLit [] -> eval' (EBind bname) fEnv bEnv
    _ -> case (eval' (EConst expr) fEnv bEnv) of
      evalExpr -> case (apply' bname evalExpr fEnv bEnv) of
        Just (Epsilon s) -> Epsilon s
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

-- TODO: think of a better way to do it later...
nRandom l = unsafePerformIO (getStdRandom (randomR (0, length l)))

applyBinOp' :: BinaryOp -> Constant -> Constant -> Constant
applyBinOp' op (BoolLit b1) (BoolLit b2) =
  case op of
    Eq  -> BoolLit (b1 == b2)
    NEq -> BoolLit (b1 /= b2)
    And -> BoolLit (b1 && b2) -- implicit case for expr list
applyBinOp' In AnyLit c2 =
  case c2 of
    SetLit s ->
      case (s > [], s) of
      (True, (EConst x):xs) -> x
      _ -> Epsilon ((unparseConst' (SetLit s)) ++ " is empty")
    _ -> Epsilon ("Can't apply any in " ++ (unparseConst' c2))
applyBinOp' Subset AnyLit c2 =
  case c2 of
    SetLit s -> SetLit (take (nRandom s) s)
    _ -> Epsilon ("Can't apply any in " ++ (unparseConst' c2))
applyBinOp' op c1@(SetLit s1) c2@(SetLit s2) =
  case op of
    Eq  -> BoolLit (s1 == s2)
    NEq -> BoolLit (s1 /= s2)
    Gt  -> BoolLit (s1 >  s2)
    GtE -> BoolLit (s1 >= s2)
    Lt  -> BoolLit (s1 <  s2)
    LtE -> BoolLit (s1 <= s2)
    Add -> SetLit (Set.toList (Set.union (Set.fromList s1) (Set.fromList s2)))
    Sub -> SetLit (Set.toList (Set.difference (Set.fromList s1) (Set.fromList s2)))
    _ -> Epsilon ("Can't apply (" ++ (unparseBinOp' op) ++ ") to " ++
         (unparseConst' c1) ++ " and " ++ (unparseConst' c2))
applyBinOp' op (SetLit s1) c2 = applyBinOp' op (SetLit s1) (SetLit [(EConst c2)])
applyBinOp' op c1 (SetLit s2) = applyBinOp' op (SetLit [(EConst c1)]) (SetLit s2)
applyBinOp' op c1@(TupleLit t1) c2@(TupleLit t2) =
  case op of
    Eq  -> BoolLit (t1 == t2)
    NEq -> BoolLit (t1 /= t2)
    _ -> Epsilon("Can't apply (" ++ (unparseBinOp' op) ++ ") to " ++
         (unparseConst' c1) ++ " and " ++ (unparseConst' c2))
applyBinOp' op c1 c2 =
  case (c1, c2) of
    (NumLit b1, NumLit b2) ->
      case op of
        Eq  -> BoolLit (b1 == b2)
        NEq -> BoolLit (b1 /= b2)
        Gt  -> BoolLit (b1 >  b2)
        GtE -> BoolLit (b1 >= b2)
        Lt  -> BoolLit (b1 <  b2)
        LtE -> BoolLit (b1 <= b2)
        Add -> NumLit ((+) b1 b2)
        Sub -> NumLit ((-) b1 b2)
        Mul -> NumLit ((*) b1 b2)
        Div -> NumLit ((/) b1 b2)
        Mod -> case (ceiling(b1) == floor(b1), ceiling(b2) == floor(b2)) of
          (False, _) -> Epsilon ("Can't apply mod to doubles!")
          (_, False) -> Epsilon ("Can't apply mod to doubles!")
          _ -> NumLit (fromIntegral ((mod) (ceiling b1) (ceiling b2)))
        Exp -> NumLit ((**) b1 b2)
        Range -> SetLit (map (\x -> EConst (NumLit x)) [b1..b2])
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
      case (allTysExist' btys bEnv) of
        Epsilon s -> Just (Epsilon s)
        _ -> applyJoinTysAndBinds' btys expr fEnv bEnv input
    _ -> Nothing

applyJoinTysAndBinds' :: [BindingType] -> CaseExpression -> FunEnvMap -> BindEnvMap -> Constant -> Maybe Constant
applyJoinTysAndBinds' btys expr fEnv bEnv input =
  case (joinTysAndBinds' btys fEnv bEnv input) of
    Just paramsEnv ->
      Just (evalCaseExpr'
            expr
            fEnv
            (Map.union (Map.fromList paramsEnv) bEnv))
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
joinTysAndBinds' btys fEnv bEnv c = Nothing

joinTyAndBind' :: BindingType -> FunEnvMap -> BindEnvMap -> Constant -> Maybe [BindEnv]
joinTyAndBind' (BType [bname] rel ty) fEnv bEnv c =
  case (matchType' rel ty (makeSet' (BType [bname] rel ty) fEnv bEnv) c) of
    True -> Just [(bname, c)]
    _ -> Nothing
joinTyAndBind' (BType tnames rel ty) fEnv bEnv (TupleLit ts) =
  case (map (\expr -> eval' expr fEnv bEnv) ts) of
    res -> Just (zipWith (\x y -> (x, y)) tnames res)

makeSet' :: BindingType -> FunEnvMap -> BindEnvMap -> Maybe (Set Constant)
makeSet' (BType b r TUniverse) _ _ = Just (Set.singleton UniverseLit)
makeSet' (BType b r (TGroup g)) fEnv bEnv =
  makeSet' (BType b r (TGroup g)) fEnv bEnv
makeSet' (BType b r (TCustom tc)) fEnv bEnv =
  case (Map.lookup tc bEnv) of
    Just v ->
      case v of
        SetLit s -> Just (Set.fromList (map (\e -> eval' e fEnv bEnv) s))
        _ -> Nothing
    _ -> Nothing
makeSet' (BType b r (TBinOp op t1 t2)) fEnv bEnv =
  case (makeSet' (BType b r t1) fEnv bEnv, makeSet' (BType b r t2) fEnv bEnv) of
    (Just s1, Just s2) ->
      case op of
        Function ->
          Just (Set.singleton (SProdLit (cartProd (Set.toList s1) (Set.toList s2))))
        CartProduct ->
          Just (Set.singleton (SProdLit (cartProd (Set.toList s1) (Set.toList s2))))
        Union -> Just (Set.union s1 s2)
        Intersection -> Just (Set.intersection s1 s2)
        Difference -> Just (Set.difference s1 s2)
    _ -> Nothing
