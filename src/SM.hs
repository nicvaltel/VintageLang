module SM where
import Types (Value, Variable, Config, SMConfig, BinaryOperator (BinaryOperator))
import Control.Monad.State.Strict (State, MonadState (get, put), modify, evalState)
import BinaryOperations (evalBinOp)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map



data StackOperation = 
  Binop String
  | Const Value
  | Read
  | Write
  | LD Variable 
  | ST Variable
  deriving (Eq, Show)


type App a = State SMConfig a


eval :: [StackOperation] -> App SMConfig
eval operations = do 
  traverse_ evalOper operations
  get


evalOper :: StackOperation -> App ()
evalOper (Binop op) = do
  (stack, conf) <- get
  case stack of
    (y:x:st) -> let z = evalBinOp x (BinaryOperator op) y in put (z:st, conf) 
    _ -> error $ "Not enougth elements in stack in Binop " ++ show (Binop op) 
evalOper (Const val) = modify (\(st, conf) -> (val:st,conf))
evalOper Read = do
  (stack, conf) <- get
  case conf of
    (_,[],_) -> error "Empty input on Read operation"
    (s,z:i,o) -> put (z:stack,(s,i,o))
evalOper Write = do
  (stack, (s,i,o)) <- get
  case stack of
    [] -> error "Empty stack on Write operation"
    z:st -> put (st,(s,i,o ++ [z]))
evalOper (LD var) = do
    (st, (s,i,o)) <- get
    case Map.lookup var s of
      Nothing -> error $ "No variable in state: " ++ var
      Just x -> put (x:st,(s,i,o))
evalOper (ST var) = do
    (stack, (s,i,o)) <- get
    case stack of
      [] -> error "Empty stack on ST operation"
      x:st -> put (st,(Map.insert var x s,i,o))


runEval :: [StackOperation] -> SMConfig -> SMConfig
runEval operations = evalState (eval operations)

testSM :: [StackOperation]
testSM = 
  [
    Const 10,
    Const 20,
    ST "x",
    Read,
    Binop "+",
    Write,
    Const 30,
    LD "x",
    Binop "*",
    Write
  ]


runTest :: IO ()
runTest = do
  let res1 = runEval testSM ([],(Map.empty, [1], [])) 
  print $ res1 == ([],(Map.fromList [("x",20)],[],[11,600]))