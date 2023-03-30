module Types where


import Data.Map.Strict (Map)
import Control.Monad.State.Strict (State)
import Control.Monad.Except (ExceptT)


type Variable = String

type Value = Int

type BoolVal = Int

type VarDict = Map Variable Value

newtype BinaryOperator = BinaryOperator String
  deriving (Eq, Show)

data Expression =
  ExprVar Variable
  | ExprVal Value
  | ExprApp Expression BinaryOperator Expression
  deriving (Eq, Show)


type VarState a = State VarDict a

type ErrorType = String

type Config = (VarDict, [Value], [Value])

type Stack = [Value]

type SMConfig = (Stack, Config)

