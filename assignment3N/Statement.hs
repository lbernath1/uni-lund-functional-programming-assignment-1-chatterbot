module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T 
    | Skip | BeginEnds Statements | If Expr.T Statement Statement | While Expr.T Statement | Read String | Write Expr.T 
    deriving Show

type Statements = [Statement]


assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip a = Skip

beginends = accept "begin" -# parseStatements #- require "end" >-> buildBeginEnds
buildBeginEnds s = BeginEnds s 

ifparser = accept "if" -# Expr.parse #- require "then" # parseStatement #- require "else" # parseStatement >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2  

whileparser = accept "while" -# Expr.parse #- require "do" # parseStatement >-> buildWhile
buildWhile (e, s) = While e s

readparser = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writeParser = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e 


parseStatements = iter parseStatement

parseStatement = assignment ! skip ! beginends ! ifparser ! whileparser ! readparser ! writeParser


first :: (a, b, c) -> a
first (x, _, _) = x 

second :: (a, b, c) -> b
second (_, y, _) = y 

third :: (a, b, c) -> c
third (_, _, z) = z 


exec' :: [T] -> Dictionary.T String Integer -> [Integer] -> (Dictionary.T String Integer, [Integer], [Integer])
exec' [] dict input = (dict, input, [])
exec' (Assignment varname value : stmnts) dict input = exec' stmnts (Dictionary.insert (varname, (Expr.value value)) dict) input  
exec' (Skip:stmnts) dict input = exec' stmnts dict input
exec' (If cond thenStmts elseStmts : stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec' (thenStmts: stmts) dict input
    else exec' (elseStmts: stmts) dict input
exec' ((BeginEnds listofStatements) : stmnts) dict input = (first b, second b, third a : third b)
    where a = exec' listofStatements dict input  
          b = exec' stmnts (first a)  (second a)


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
--exec [] _ _ = []
--exec (Assignment varname value : stmnts) dict input = exec stmnts (Dictionary.insert (varname, value) dict) input  
--exec (Skip:stmnts) = exec stmnts

--exec ((BeginEnds listofStatements) : stmnts) dict input = exec listofStatements dict input : exec stmnts dict input   

--exec (If cond thenStmts elseStmts : stmts) dict input = 
--    if (Expr.value cond dict)>0 
--    then exec (thenStmts: stmts) dict input
--    else exec (elseStmts: stmts) dict input

--exec (While cond statement : stmts) dict input =
--    if (Expr.value cond dict)>0
--    then exec  (BeginEnds)

exec stmnts dict input = third  $ exec' stmnts dict input



instance Parse Statement where
  parse = parseStatement
  toString = error "Statement.toString not implemented"


