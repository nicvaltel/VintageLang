module Types where


import Data.Map.Strict (Map)


type Variable = String

type Value = Int

type BoolVal = Int

type VarDict = Map Variable Value

newtype BinaryOperator = BinaryOperator String
  deriving (Show)

data Expression =
  ExprVar Variable
  | ExprVal Value
  | ExprApp Expression BinaryOperator Expression


