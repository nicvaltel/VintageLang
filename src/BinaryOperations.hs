module BinaryOperations (evalBinOp) where

import Types
import Control.Exception (throw, ErrorCall (ErrorCall))


evalBinOp :: Value -> BinaryOperator -> Value -> Value
evalBinOp a (BinaryOperator "+") b = a + b
evalBinOp a (BinaryOperator "-") b = a - b
evalBinOp a (BinaryOperator "*") b = a * b
evalBinOp a (BinaryOperator "/") b = a `div` b
evalBinOp a (BinaryOperator "%") b = a `mod` b
evalBinOp a (BinaryOperator "<") b = evalBool (a < b)
evalBinOp a (BinaryOperator ">") b = evalBool $ a > b
evalBinOp a (BinaryOperator "<=") b = evalBool $ a <= b
evalBinOp a (BinaryOperator ">=") b = evalBool $ a >= b
evalBinOp a (BinaryOperator "==") b = evalBool $ a == b
evalBinOp a (BinaryOperator "&&") b = boolToInt (intToBool a && intToBool b)
evalBinOp a (BinaryOperator "!!") b = boolToInt (intToBool a || intToBool b)
evalBinOp _ bo _ = throw $ ErrorCall $ "incorrect binary operator: " ++ show bo



evalBool :: Bool -> BoolVal
evalBool = boolToInt

boolToInt :: Bool -> BoolVal 
boolToInt True = 1
boolToInt False = 0

intToBool :: BoolVal -> Bool
intToBool 0 = False 
intToBool _ = True