module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T 
    | Skip | BeginEnds Statements | IfElse Expr.T Statement Statement | While Expr.T Statement | Read String | Write Expr.T 
    deriving Show

type Statements = [Statement]


assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip a = Skip

beginends = accept "begin" -# parseStatements #- require "ends" >-> buildBeginEnds
buildBeginEnds s = BeginEnds s 

ifelse = accept "if" -# Expr.parse #- require "then" # parseStatement #- require "else" # parseStatement >-> buildIfElse
buildIfElse (e, s1, s2) = IfElse e s1 s2  

whileparser = accept "while" -# Expr.parse #- require "do" # parseStatement >-> buildWhile
buildWhile (e, s) = While e s

readparser = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writeParser = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e 


parseStatements = iter parseStatement

parseStatement = assignment ! skip ! beginends ! ifelse ! whileparser ! readparser ! writeParser



exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"


