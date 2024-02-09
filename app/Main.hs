module Main (main) where

import Evaluator
import AbstractSyntax
import StackMachine
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
    runTestEvaluate
    putStrLn ""
    runTestStackMachine


testContext = Context $ Map.fromList [(Variable "x", Numbr 7), (Variable "y", Numbr 8), (Variable "z", Numbr 12)]

testApp = Interpreter {appContext = testContext, appInput = Input [Numbr 555], appOutput = Output [], appError = Nothing}

testSMApp = SMApp {smStack = Stack [], smContext = testContext, smInput = Input [Numbr 555], smOutput = Output []}

testExpr1 = evaluateExpr
                testContext
                ( ExBinOp
                    OpMinus
                    ( evaluateExpr
                        testContext
                        ( ExBinOp
                            OpPlus
                            (evaluateExpr testContext (ExBinOp OpMult (ExVar (Variable "x")) (ExNum (Numbr 3))))
                            (evaluateExpr testContext (ExBinOp OpDiv (ExVar (Variable "y")) (ExNum (Numbr 3))))
                        )
                    )
                    (ExVar (Variable "z"))
                )

runTestEvaluate :: IO ()
runTestEvaluate = do
  let st = testContext
  print $ evaluateExpr st (ExVar (Variable "z")) == ExNum (Numbr 12)
  print $ evaluateExpr st (ExNum (Numbr 717)) == ExNum (Numbr 717)
  print $ evaluateExpr st (ExNum (Numbr 717)) == ExNum (Numbr 717)
--   print $ evaluateExpr st (ExError "Test Error") == ExError "Test Error"
  print $ evaluateExpr st (ExBinOp OpPlus (ExVar (Variable "x")) (ExNum (Numbr 20))) == ExNum (Numbr 27)
  print $ evaluateExpr st (ExBinOp OpMinus (ExNum (Numbr 30)) (ExVar (Variable "z"))) == ExNum (Numbr 18)
  let n11 = testExpr1
   in print $ n11 == ExNum (Numbr 11)
--   print $
--     evaluateExpr
--       st
--       ( ExElvis
--           (ExBinOp OpGEq (ExVar (Variable "x")) (ExNum (Numbr 100)))
--           (ExVar (Variable "y"))
--           (ExVar (Variable "z"))
--       )
--       == ExNum (Numbr 12)

  let app1 = appStep (StmWrite $ ExVar $ Variable "x") testApp
  let app2 = appStep (StmRead $ Variable "hello") app1
  let appStepResult = appStep (StmWrite $ ExVar $ Variable "y") app2

  let steps = StmSemicolon (StmWrite $ ExVar $ Variable "x") (StmSemicolon (StmRead $ Variable "hello") (StmWrite $ ExVar $ Variable "y"))
  print $ appStep steps testApp == appStepResult





runTestStackMachine :: IO ()
runTestStackMachine = do
    let app0 = testSMApp
    let app1 = stackMachineStep (SOConst $ Numbr 2) app0
    let app2 = stackMachineStep (SOConst $ Numbr 3) app1
    let app3 = stackMachineStep (SOBinop OpMult) app2
    let app4 = stackMachineStep (SOLd $ Variable "y") app3
    let app5 = stackMachineStep (SOBinop OpPlus) app4
    let app6 = stackMachineStep SOWrite app5

    let smProgramm = [
             SOConst $ Numbr 2,
             SOConst $ Numbr 3,
             SOBinop OpMult,
             SOLd $ Variable "y",
             SOBinop OpPlus,
             SOWrite ]

    print $ foldl (flip stackMachineStep) testSMApp smProgramm == app6

    let expr = testExpr1

    let smProgramm1 = compileExpressionToSM expr
    print $ unStack (smStack $ foldl (flip stackMachineStep) testSMApp smProgramm1) == [Numbr 11]

    let steps = StmSemicolon (StmWrite $ ExVar $ Variable "x") (StmSemicolon (StmRead $ Variable "hello") (StmWrite $ ExVar $ Variable "hello"))
    let smProgramm2 = compileStatementToSM steps
    print $ smOutput (foldl (flip stackMachineStep) testSMApp smProgramm2) == Output [Numbr 555, Numbr 7]
    putStrLn ""
    pure ()

