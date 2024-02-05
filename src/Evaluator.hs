module Evaluator (evaluate, runTestEvaluate) where

import qualified Data.Map.Strict as Map
import Text.Printf (printf)
import AbstractSyntax

evaluate :: EvalState -> Expression -> Expression
evaluate _ (ExError str) = ExError str
evaluate _ (ExNum n) = ExNum n
evaluate (EvalState st) (ExVar v) =
    case Map.lookup v st of
        Nothing -> ExError $ "Variable not found: " ++ show v
        (Just n) -> ExNum n
evaluate st (ExBinOp binOp expr0 expr1) =
        case (evaluate st expr0, evaluate st expr1) of
            (ExError msg, _) -> ExError msg
            (_, ExError msg) -> ExError msg
            (ExNum n0,ExNum n1) -> ExNum $ evalBinOp binOp n0 n1
            (e1,e2) -> ExError $ printf "incorrect operation: operation = %s, expr1 = %s, expr2 = %s" (show binOp) (show e1) (show e2)
evaluate st (ExElvis exPredicate exIfTrue exIfFalse) = 
    case evaluate st exPredicate of
        ExError msg -> ExError msg
        ExNum (Numbr n) | n /= 0 -> evaluate st exIfTrue
        ExNum (Numbr n) | n == 0 -> evaluate st exIfFalse
        e -> ExError $ printf "incorrect elvis operation: %s" (show e)
evaluate st (ExWhile exPredicate exLoop exAfterLoop) = evaluate st (ExElvis exPredicate exLoop exAfterLoop)
evaluate st (ExAssign var expr) = undefined


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


runTestEvaluate :: IO ()
runTestEvaluate = do
    let st = EvalState $ Map.fromList [(Variable "x",Numbr 7),(Variable "y",Numbr 8),(Variable "z",Numbr 12)]
    print $ evaluate st (ExVar (Variable "z")) == ExNum (Numbr 12)
    print $ evaluate st (ExNum (Numbr 717)) == ExNum (Numbr 717)
    print $ evaluate st (ExNum (Numbr 717)) == ExNum (Numbr 717)
    print $ evaluate st (ExError "Test Error") == ExError "Test Error"
    print $ evaluate st (ExBinOp OpPlus (ExVar (Variable "x")) (ExNum (Numbr 20))) == ExNum (Numbr 27)
    print $ evaluate st (ExBinOp OpMinus (ExNum (Numbr 30)) (ExVar (Variable "z"))) == ExNum (Numbr 18)
    let x = evaluate st ( ExBinOp OpMinus
                (evaluate st ( ExBinOp OpPlus
                    (evaluate st (ExBinOp OpMult (ExVar (Variable "x")) (ExNum (Numbr 3)))) 
                    (evaluate st (ExBinOp OpDiv (ExVar (Variable "y")) (ExNum (Numbr 3))))
                ))
                (ExVar (Variable "z")))
     in print $ x == ExNum (Numbr 11)
    print $ evaluate st (ExElvis 
        (ExBinOp OpGEq (ExVar (Variable "x")) (ExNum (Numbr 100))) 
        (ExVar (Variable "y")) 
        (ExVar (Variable "z"))) == ExNum (Numbr 12)


-- newtype Variable = Variable String
--   deriving (Eq, Ord, Show)

-- newtype Numbr = Numbr Int
--   deriving (Eq, Ord, Show)

-- data BinOperation = OpPlus | OpMinus | OpMult | OpDiv | OpMod | OpLs | OpLEq | OpGt | OpGEq | OpEq | OpNEq | OpAnd | OpOr
--   deriving (Eq, Ord, Show)

-- data Expression = ExVar Variable | ExNum Numbr | ExBinOp BinOperation Expression Expression | ExElvis Expression Expression Expression | ExError String
--   deriving (Eq, Ord, Show)

-- newtype EvalState = EvalState (Map Variable Numbr)
--   deriving (Eq, Ord, Show)