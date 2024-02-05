module AbstractSyntax
  ( Variable (..),
    Numbr (..),
    BinOperation (..),
    Expression (..),
    EvalState (..),
  )
where

import Data.Map (Map)

newtype Variable = Variable String
  deriving (Eq, Ord, Show)

newtype Numbr = Numbr Int
  deriving (Eq, Ord, Show)

data BinOperation = OpPlus | OpMinus | OpMult | OpDiv | OpMod | OpLs | OpLEq | OpGt | OpGEq | OpEq | OpNEq | OpAnd | OpOr
  deriving (Eq, Ord, Show)

data Expression
  = ExVar Variable
  | ExNum Numbr
  | ExBinOp BinOperation Expression Expression
  | ExElvis Expression Expression Expression -- predicate ifTrue ifFalse
  | ExWhile Expression Expression Expression -- predicate loop expressionAfterLoop
  | ExAssign Variable Expression
  | ExError String
  deriving (Eq, Ord, Show)

newtype EvalState = EvalState (Map Variable Numbr)
  deriving (Eq, Ord, Show)
