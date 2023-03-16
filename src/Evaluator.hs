module Evaluator where

import Types
import Control.Monad.State.Strict (State, gets, evalState)
import qualified Data.Map.Strict as Map
import BinaryOperations (evalBinOp)
import Data.Map.Strict (Map)


type VarState a = State VarDict a

evaluator :: Expression -> VarState Value
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


runEvaluator :: Expression -> VarDict -> Value
runEvaluator expr = evalState (evaluator expr) 

testExpr1 :: Expression
testExpr1 = ExprApp (ExprApp (ExprVal 7) (BinaryOperator "*") (ExprVal 3)) (BinaryOperator "+") (ExprVal 100) -- 121

testExpr2 :: Expression
testExpr2 = ExprApp (ExprVar "x") (BinaryOperator "-") (ExprApp (ExprVar "y") (BinaryOperator "*") (ExprVar "Zorro")) -- -89

testState :: Map Variable Int
testState = Map.fromList [("x", 7), ("y", 8), ("Zorro", 12)]

runTest :: IO ()
runTest = do
  print $ runEvaluator testExpr1 testState == 121
  print $ runEvaluator testExpr2 testState == -89