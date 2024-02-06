{-# LANGUAGE NamedFieldPuns #-}
module Evaluator (evaluateExpr, runTestEvaluate) where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import AbstractSyntax

evaluateExpr :: Context -> Expression -> Expression
evaluateExpr _ (ExError str) = ExError str
evaluateExpr _ (ExNum n) = ExNum n
evaluateExpr (Context st) (ExVar v) =
    case Map.lookup v st of
        Nothing -> ExError $ "Variable not found: " ++ show v
        (Just n) -> ExNum n
evaluateExpr st (ExBinOp binOp expr0 expr1) =
        case (evaluateExpr st expr0, evaluateExpr st expr1) of
            (ExError msg, _) -> ExError msg
            (_, ExError msg) -> ExError msg
            (ExNum n0,ExNum n1) -> ExNum $ evalBinOp binOp n0 n1
            (e1,e2) -> ExError $ printf "incorrect operation: operation = %s, expr1 = %s, expr2 = %s" (show binOp) (show e1) (show e2)
evaluateExpr st (ExElvis exPredicate exIfTrue exIfFalse) = 
    case evaluateExpr st exPredicate of
        ExError msg -> ExError msg
        ExNum (Numbr n) | n /= 0 -> evaluateExpr st exIfTrue
        ExNum (Numbr n) | n == 0 -> evaluateExpr st exIfFalse
        e -> ExError $ printf "incorrect elvis operation: %s" (show e)
evaluateExpr st (ExWhile exPredicate exLoop exAfterLoop) = evaluateExpr st (ExElvis exPredicate exLoop exAfterLoop)


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



appStep :: Statement -> App -> App
appStep _ app@App{appError = Just _} = app
appStep (StmAssign var expr) app@App{appContext} =
    let expr' = evaluateExpr appContext expr
     in case expr' of
            ExError msg -> app{appError = Just msg}
            ExNum n -> app{appContext = Context (Map.insert var n (unContext appContext))}
            _ -> app{appError = Just $ "appStep incorrect StmAssign evaluateExpr: " ++ show expr}
appStep (StmRead var) app@App{appContext, appInput} =
    case readInput appInput of
        Just (n, newInput) -> 
            let newContext = Map.insert var n (unContext appContext)
             in app{appContext = Context newContext, appInput = newInput}
        Nothing -> app{appError = Just "readInput error - empty Input"}
appStep (StmWrite expr) app@App{appContext, appOutput} =
    case evaluateExpr appContext expr of
        ExNum n -> let newOutput = writeOutput n appOutput in app{appOutput = newOutput}
        _ -> app{appError = Just $ "appStep incorrect StmWrite evaluateExpr: " ++ show expr}    
appStep (StmSemicolon stm0 stm1) app = appStep stm1 (appStep stm0 app)


testContext = Context $ Map.fromList [(Variable "x",Numbr 7),(Variable "y",Numbr 8),(Variable "z",Numbr 12)]
testApp = App {appContext = testContext, appInput = Input [Numbr 555] , appOutput = Output [], appError = Nothing}


runTestEvaluate :: IO ()
runTestEvaluate = do
    let st = testContext
    print $ evaluateExpr st (ExVar (Variable "z")) == ExNum (Numbr 12)
    print $ evaluateExpr st (ExNum (Numbr 717)) == ExNum (Numbr 717)
    print $ evaluateExpr st (ExNum (Numbr 717)) == ExNum (Numbr 717)
    print $ evaluateExpr st (ExError "Test Error") == ExError "Test Error"
    print $ evaluateExpr st (ExBinOp OpPlus (ExVar (Variable "x")) (ExNum (Numbr 20))) == ExNum (Numbr 27)
    print $ evaluateExpr st (ExBinOp OpMinus (ExNum (Numbr 30)) (ExVar (Variable "z"))) == ExNum (Numbr 18)
    let x = evaluateExpr st ( ExBinOp OpMinus
                (evaluateExpr st ( ExBinOp OpPlus
                    (evaluateExpr st (ExBinOp OpMult (ExVar (Variable "x")) (ExNum (Numbr 3)))) 
                    (evaluateExpr st (ExBinOp OpDiv (ExVar (Variable "y")) (ExNum (Numbr 3))))
                ))
                (ExVar (Variable "z")))
     in print $ x == ExNum (Numbr 11)
    print $ evaluateExpr st (ExElvis 
        (ExBinOp OpGEq (ExVar (Variable "x")) (ExNum (Numbr 100))) 
        (ExVar (Variable "y")) 
        (ExVar (Variable "z"))) == ExNum (Numbr 12)
    let app1 = appStep (StmWrite $ ExVar $ Variable "x") testApp
    let app2 = appStep (StmRead $ Variable "hello") app1
    print $ appStep (StmWrite $ ExVar $ Variable "y") app2

