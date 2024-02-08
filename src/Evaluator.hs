{-# LANGUAGE NamedFieldPuns #-}

module Evaluator (evaluateExpr, evalBinOp, appStep) where

import AbstractSyntax
import Data.Map.Strict qualified as Map
import Text.Printf (printf)

evaluateExpr :: Context -> Expression -> Expression
-- evaluateExpr _ (ExError str) = ExError str
evaluateExpr _ (ExNum n) = ExNum n
evaluateExpr (Context st) (ExVar v) =
  case Map.lookup v st of
    -- Nothing -> ExError $ "Variable not found: " ++ show v
    Nothing -> error $ "Variable not found: " ++ show v
    (Just n) -> ExNum n
evaluateExpr st (ExBinOp binOp expr0 expr1) =
  case (evaluateExpr st expr0, evaluateExpr st expr1) of
    -- (ExError msg, _) -> ExError msg
    -- (_, ExError msg) -> ExError msg
    (ExNum n0, ExNum n1) -> ExNum $ evalBinOp binOp n0 n1
    -- (e1, e2) -> ExError $ printf "incorrect operation: operation = %s, expr1 = %s, expr2 = %s" (show binOp) (show e1) (show e2)
    (e1, e2) -> error $ printf "incorrect operation: operation = %s, expr1 = %s, expr2 = %s" (show binOp) (show e1) (show e2)
-- evaluateExpr st (ExElvis exPredicate exIfTrue exIfFalse) =
--   case evaluateExpr st exPredicate of
--     ExError msg -> ExError msg
--     ExNum (Numbr n) | n /= 0 -> evaluateExpr st exIfTrue
--     ExNum (Numbr n) | n == 0 -> evaluateExpr st exIfFalse
--     e -> ExError $ printf "incorrect elvis operation: %s" (show e)
-- evaluateExpr st (ExWhile exPredicate exLoop exAfterLoop) = evaluateExpr st (ExElvis exPredicate exLoop exAfterLoop)

evalBinOp :: BinOperation -> Numbr -> Numbr -> Numbr
evalBinOp OpPlus (Numbr n0) (Numbr n1) = Numbr (n0 + n1)
evalBinOp OpMinus (Numbr n0) (Numbr n1) = Numbr (n0 - n1)
evalBinOp OpMult (Numbr n0) (Numbr n1) = Numbr (n0 * n1)
evalBinOp OpDiv (Numbr n0) (Numbr n1) = Numbr (n0 `div` n1)
evalBinOp OpMod (Numbr n0) (Numbr n1) = Numbr (n0 `mod` n1)
evalBinOp OpLs (Numbr n0) (Numbr n1) = Numbr (if n0 < n1 then 1 else 0)
evalBinOp OpLEq (Numbr n0) (Numbr n1) = Numbr (if n0 <= n1 then 1 else 0)
evalBinOp OpGt (Numbr n0) (Numbr n1) = Numbr (if n0 > n1 then 1 else 0)
evalBinOp OpGEq (Numbr n0) (Numbr n1) = Numbr (if n0 >= n1 then 1 else 0)
evalBinOp OpEq (Numbr n0) (Numbr n1) = Numbr (if n0 == n1 then 1 else 0)
evalBinOp OpNEq (Numbr n0) (Numbr n1) = Numbr (if n0 /= n1 then 1 else 0)
evalBinOp OpAnd (Numbr n0) (Numbr n1) = Numbr (if (n0 /= 0) && (n1 /= 0) then 1 else 0)
evalBinOp OpOr (Numbr n0) (Numbr n1) = Numbr (if (n0 /= 0) || (n1 /= 0) then 1 else 0)

appStep :: Statement -> Interpreter -> Interpreter
appStep _ app@Interpreter {appError = Just _} = app
appStep (StmAssign var expr) app@Interpreter {appContext} =
  let expr' = evaluateExpr appContext expr
   in case expr' of
        -- ExError msg -> app {appError = Just msg}
        ExNum n -> app {appContext = Context (Map.insert var n (unContext appContext))}
        _ -> app {appError = Just $ "appStep incorrect StmAssign evaluateExpr: " ++ show expr}
appStep (StmRead var) app@Interpreter {appContext, appInput} =
  case readInput appInput of
    Just (n, newInput) ->
      let newContext = Map.insert var n (unContext appContext)
       in app {appContext = Context newContext, appInput = newInput}
    Nothing -> app {appError = Just "readInput error - empty Input"}
appStep (StmWrite expr) app@Interpreter {appContext, appOutput} =
  case evaluateExpr appContext expr of
    ExNum n -> let newOutput = writeOutput n appOutput in app {appOutput = newOutput}
    _ -> app {appError = Just $ "appStep incorrect StmWrite evaluateExpr: " ++ show expr}
appStep (StmSemicolon stm0 stm1) app = appStep stm1 (appStep stm0 app)

