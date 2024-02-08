module AbstractSyntax
  ( Variable (..),
    Numbr (..),
    BinOperation (..),
    Expression (..),
    Context (..),
    Statement (..),
    Input (..),
    Output (..),
    Interpreter (..),
    readInput,
    writeOutput,
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
  -- | ExElvis Expression Expression Expression -- predicate ifTrue ifFalse
  -- | ExWhile Expression Expression Expression -- predicate loop expressionAfterLoop
  -- | ExError String
  deriving (Eq, Ord, Show)

newtype Context = Context {unContext :: Map Variable Numbr}
  deriving (Eq, Ord, Show)

data Statement
  = StmAssign Variable Expression
  | StmRead Variable
  | StmWrite Expression
  | StmSemicolon Statement Statement
  deriving (Eq, Ord, Show)

newtype Input = Input [Numbr]
  deriving (Eq, Ord, Show)

newtype Output = Output [Numbr]
  deriving (Eq, Ord, Show)

data Interpreter = Interpreter {appContext :: Context, appInput :: Input, appOutput :: Output, appError :: Maybe String}
  deriving (Eq, Ord, Show)





readInput :: Input -> Maybe (Numbr, Input)
readInput (Input (n : nx)) = Just (n, Input nx)
readInput (Input []) = Nothing

writeOutput :: Numbr -> Output -> Output
writeOutput numbr (Output out) = Output (numbr : out)
