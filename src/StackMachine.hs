{-# LANGUAGE NamedFieldPuns #-}

module StackMachine (
    Stack(..),
    StackOp(..),
    StackMachineError(..),
    SMApp(..),
    stackMachineStep,
    compileStatementToSM,
    compileExpressionToSM
) where

import AbstractSyntax
import Evaluator (evalBinOp)
import Data.Map.Strict qualified as Map
import Text.Printf (printf)

newtype Stack = Stack {unStack :: [Numbr]}
    deriving (Eq, Ord, Show)

data StackOp
  = SOBinop BinOperation
  | SOConst Numbr
  | SORead
  | SOWrite
  | SOLd Variable
  | SOSt Variable
  | SOStop
  deriving (Eq, Ord, Show)

data StackMachineError
  = StackIsEmpty
  | OnlyOneElementInStack
  | ReadError
  | VariableNotFound Variable
  deriving (Eq, Ord, Show)

data SMApp = SMApp {smStack :: Stack, smContext :: Context, smInput :: Input, smOutput :: Output} | SMError StackMachineError SMApp
  deriving (Eq, Ord, Show)


stackMachineStep :: StackOp -> SMApp -> SMApp
stackMachineStep SOStop st = st
stackMachineStep _ err@(SMError _ _) = err
stackMachineStep (SOBinop binOp) sm@SMApp{smStack} =
    case unStack smStack of
        (y : x : rest) -> sm{smStack = Stack $ evalBinOp binOp x y : rest }
        [] -> SMError StackIsEmpty sm
        [_] -> SMError OnlyOneElementInStack sm
stackMachineStep (SOConst n) sm@SMApp{smStack} = sm{smStack = Stack $ n : unStack smStack }
stackMachineStep SORead sm@SMApp{smStack, smInput} =
    case readInput smInput of
        Just (n, newInput) -> sm {smStack = Stack $ n : unStack smStack, smInput = newInput}
        Nothing -> SMError ReadError sm
stackMachineStep SOWrite sm@SMApp{smStack, smOutput = Output out} =
    case unStack smStack of
        n:rest -> sm{smStack = Stack rest, smOutput = Output (n:out)}
        [] -> SMError StackIsEmpty sm
stackMachineStep (SOLd varX) sm@SMApp{smStack, smContext} =
    case Map.lookup varX (unContext smContext) of
        Just n -> sm{smStack = Stack $ n : unStack smStack}
        Nothing -> SMError (VariableNotFound varX) sm
stackMachineStep (SOSt varX) sm@SMApp{smStack, smContext} =
    case unStack smStack of
        (n : rest) -> sm{smStack = Stack rest, smContext = Context $ Map.insert varX n (unContext smContext) }
        [] -> SMError StackIsEmpty sm

compileExpressionToSM :: Expression -> [StackOp]
compileExpressionToSM (ExVar varX) = [SOLd varX]
compileExpressionToSM (ExNum numbr) = [SOConst numbr]
compileExpressionToSM (ExBinOp binOp expr0 expr1) = compileExpressionToSM expr0 ++ compileExpressionToSM expr1 ++ [SOBinop binOp]
-- compileExpressionToSM (ExElvis exprPredicate exprIfTrue exprIfFalse) = [SOStop]
-- compileExpressionToSM (ExError _) = [SOStop]



compileStatementToSM :: Statement -> [StackOp]
compileStatementToSM (StmAssign var expr) = compileExpressionToSM expr ++ [SOSt var]
compileStatementToSM (StmRead var) = [SORead, SOSt var]
compileStatementToSM (StmWrite expr) = compileExpressionToSM expr ++ [SOWrite]
compileStatementToSM (StmSemicolon stm0 stm1) = compileStatementToSM stm0 ++ compileStatementToSM stm1



-- runTestStackMachine :: IO ()
-- runTestStackMachine = do
--     let app0 = testSMApp
--     let app1 = stackMachineStep (SOConst $ Numbr 2) app0
--     let app2 = stackMachineStep (SOConst $ Numbr 3) app1
--     let app3 = stackMachineStep (SOBinop OpMult) app2
--     let app4 = stackMachineStep (SOLd $ Variable "y") app3
--     let app5 = stackMachineStep (SOBinop OpPlus) app4
--     let app6 = stackMachineStep SOWrite app5
--     print app6

--     putStrLn ""

--     let smProgramm = [
--              SOConst $ Numbr 2,
--              SOConst $ Numbr 3,
--              SOBinop OpMult,
--              SOLd $ Variable "y",
--              SOBinop OpPlus,
--              SOWrite ]

--     print $ foldl (flip stackMachineStep) testSMApp smProgramm
--     pure ()