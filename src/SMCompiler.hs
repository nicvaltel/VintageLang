module SMCompiler where


import SM ( StackOperation(Binop, LD, Const) )
import SM qualified
import Types
import Syntax (Stmt) 
import Syntax qualified


compile :: Stmt -> [StackOperation]
compile (Syntax.Read var) = [SM.Read, SM.ST var] 
compile (Syntax.Write expr) = compileExpr expr ++ [SM.Write]
compile (Syntax.Assign var expr) = compileExpr expr ++ [SM.ST var]
compile (Syntax.Seq stm0 stm1) = compile stm0 ++ compile stm1 

compileExpr :: Expression -> [StackOperation]
compileExpr (ExprVar var) = [LD var] 
compileExpr (ExprVal val) = [Const val] 
compileExpr (ExprApp ex0 (BinaryOperator op) ex1) = compileExpr ex0 ++ compileExpr ex1 ++ [Binop op]


testStmt :: Stmt
testStmt = Syntax.testStmtSeq -- compile testStmt == [Const 7,Const 3,Binop "*",Const 100,Binop "+",Write,Read,ST "x"] 

