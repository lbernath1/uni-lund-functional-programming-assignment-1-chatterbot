
module Statement(T, parse, toString, fromString, exec, Statement, Statements) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T| Skip | BeginEnds Statements | If Expr.T Statement Statement | While Expr.T Statement | Read String | Write Expr.T 
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


--commentParser = spaces -# accept "--" -# iter notnewline -# lit '\n' >-> buildSkip

parseStatement = assignment ! skip ! beginends ! ifparser ! whileparser ! readparser ! writeParser
parseStatements = iter parseStatement


first :: (a, b, c) -> a
first (x, _, _) = x 

second :: (a, b, c) -> b
second (_, y, _) = y 

third :: (a, b, c) -> c
third (_, _, z) = z 


exec' :: [T] -> Dictionary.T String Integer -> [Integer] -> (Dictionary.T String Integer, [Integer], [Integer])
exec' [] dict input = (dict, input, [])
exec' (Assignment varname valueExpr : stmts) dict input = exec' stmts (Dictionary.insert (varname, (Expr.value valueExpr dict)) dict) input  
exec' (If cond thenStmts elseStmts : stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec' (thenStmts: stmts) dict input
    else exec' (elseStmts: stmts) dict input
exec' ((BeginEnds listofStatements) : stmts) dict input = (first b, second b,  (third a) ++ (third b))
    where a = exec' listofStatements dict input  
          b = exec' stmts (first a)  (second a)

exec' (While cond statement : stmts) dict input =
    if (Expr.value cond dict)>0
    then exec' (While cond statement : stmts) (first doWhileOnce) (second doWhileOnce)
    else exec' stmts dict input
       where doWhileOnce = exec' [statement] dict input

exec' (Read varname : stmts) dict (val:input) = exec' stmts (Dictionary.insert (varname, val) dict) input

exec' (Write expr : stmnts) dict input = (first a, second a, (Expr.value expr dict): third a)
    where a = exec' stmnts dict input



exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec stmts dict input = third  $ exec' stmts dict input



instance Parse Statement where
  parse = parseStatement
  toString statement = case statement of
    --Assignment var expr-> var ++ ":=" ++ expr ++ "\n"
    Skip -> "skip;" ++ "\n"
    --BeginEnds [stmt:stmts] -> "begin;"-- ++ (toString stmt) ++ "end;"
    If expr stmt1 stmt2 -> "if else..."
    While expr stmt -> "while loop"
    Read str -> "read"
    Write expr -> "write"
