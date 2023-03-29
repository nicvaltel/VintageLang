{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Evaluator where

import BinaryOperations (evalBinOp)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT)
import Control.Monad.State.Strict (State, evalState, gets)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Types
import Control.Exception (catch, SomeException)

type VarState a = State VarDict a

type ErrorType = String

type App a = ExceptT ErrorType  (State VarDict) a


evaluator :: Expression -> App Value
evaluator (ExprVal val) = pure val
evaluator (ExprVar var) = do
  val <- gets (Map.lookup var)
  case val of
    Just n -> pure n
    Nothing -> error $ "udefined variable " ++ show var
evaluator (ExprApp expr0 binOp expr1) = do
  a <- evaluator expr0
  b <- evaluator expr1
  pure $ evalBinOp a binOp b

runEvaluator :: Expression -> VarDict -> Either ErrorType Value
runEvaluator expr dict = evalState (runExceptT (evaluator expr)) dict 

runEvaluatorIO :: Expression -> VarDict -> IO Value
runEvaluatorIO expr dict = do
  let result = evalState (runExceptT (evaluator expr)) dict 
  case result of
    Right r -> pure r
    Left e -> error e


testExpr1 :: Expression
testExpr1 = ExprApp (ExprApp (ExprVal 7) (BinaryOperator "*") (ExprVal 3)) (BinaryOperator "+") (ExprVal 100) -- 121

testExpr2 :: Expression
testExpr2 = ExprApp (ExprVar "x") (BinaryOperator "-") (ExprApp (ExprVar "y") (BinaryOperator "*") (ExprVar "Zorro")) -- -89

testExprErr :: Expression
testExprErr = ExprApp (ExprVal 7) (BinaryOperator "?") (ExprVal 3)

testState :: Map Variable Int
testState = Map.fromList [("x", 7), ("y", 8), ("Zorro", 12)]

runTest :: IO ()
runTest = do
  let runOnce expr = runEvaluatorIO expr testState `catch` (\(e :: SomeException) -> pure (-1))
  r1 <- runOnce testExpr1
  print $ r1 == 121
  r2 <- runOnce testExpr2
  print $ r2 == -89
  rErr <- runOnce testExprErr
  print $ rErr

