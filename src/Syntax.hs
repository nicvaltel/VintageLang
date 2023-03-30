module Syntax where

import Types ( Expression, Value, VarDict, Variable )
import qualified Data.Map.Strict as Map
import Evaluator qualified
import Control.Monad.State.Strict (State, get, put, evalState)

type Config = (VarDict, [Value], [Value])

type App a = State Config a


data Stmt = -- Simple statments: syntax and semantics
    Read Variable
  | Write Expression
  | Assign Variable Expression
  | Seq Stmt Stmt
  deriving (Eq, Show)


evalExpr :: Expression -> VarDict -> Value
evalExpr expr dict = 
  case Evaluator.runEvaluator expr dict of
    Left err -> error err
    Right val -> val  


eval :: Stmt -> App Config 
eval (Read var) = do
  conf <- get 
  case conf of
    (_,[],_) -> error $ "empty input on Read " ++ show var
    (dict, i:inp, out) -> pure (Map.insert var i dict, inp, out)
eval (Write expr) = do
  (dict,i,o) <- get
  let newConf = (dict,i,o ++ [evalExpr expr dict])
  put newConf
  pure newConf
eval (Assign var expr) = do
  (dict,i,o) <- get
  let newDict = Map.insert var (evalExpr expr dict) dict
  let newConf = (newDict, i, o)
  put newConf
  pure newConf
eval (Seq stmt0 stmt1) = eval stmt0 >> eval stmt1


runEval :: Stmt -> Config -> Config
runEval stmt = evalState (eval stmt)

testStmtWrite :: Stmt
testStmtWrite = Write Evaluator.testExpr1 -- 121

testStmtRead :: Stmt
testStmtRead = Read "x"

testStmtAssign :: Stmt
testStmtAssign = Assign "testVar" Evaluator.testExpr2 -- -89

testStmtSeq :: Stmt
testStmtSeq = Seq testStmtWrite testStmtRead


runTest :: IO ()
runTest = do
  let r1 = runEval testStmtWrite (Map.empty,[],[])
  print $ r1 == (Map.empty ,[],[121])
  let r2 = runEval testStmtRead (Map.singleton "y" 777,[555],[]) 
  print $ r2 == (Map.fromList [("x",555),("y",777)],[],[])
  let r3 = runEval testStmtAssign (Evaluator.testState ,[],[])
  print $ r3 == (Map.insert "testVar" (-89) Evaluator.testState,[],[])
  let r4 = runEval testStmtSeq (Map.empty,[100500],[])
  print $ r4 == (Map.fromList [("x",100500)],[],[121])
  



