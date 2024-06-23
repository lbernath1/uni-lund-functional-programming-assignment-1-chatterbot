
--Ellen Hedberg & Leon Bernáth
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
module Statement(T, parse, toString, fromString, exec, Statement, Statements, parseStatements, statementToString, statementsToString) where
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


exec' :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer] -> (Dictionary.T String Integer, [Integer], [Integer])
exec' [] dict input output = (dict, input, output)
exec' (Assignment varname valueExpr : stmts) dict input output = exec' stmts (Dictionary.insert (varname, Expr.value valueExpr dict) dict) input output
exec' (Skip:stmnts) dict input output = exec' stmnts dict input output
exec' (If cond thenStmts elseStmts : stmts) dict input output =
    if Expr.value cond dict>0
    then exec' (thenStmts: stmts) dict input output
    else exec' (elseStmts: stmts) dict input output

exec' ((BeginEnds listofStatements) : stmts) dict input output = (first b, second b,  (third a) ++ (third b))
    where a = exec' listofStatements dict input output
          b = exec' stmts (first a)  (second a) []

exec' (While cond statement : stmts) dict input output =
    if (Expr.value cond dict)>0
    then exec' (While cond statement : stmts) (first doWhileOnce) (second doWhileOnce) (output ++ (reverse (third doWhileOnce)))
    else exec' stmts dict input output
       where doWhileOnce = exec' [statement] dict input []
             
      
exec' (Read varname : stmts) dict (val:input) output = exec' stmts (Dictionary.insert (varname, val) dict) input output

exec' (Write expr : stmnts) dict input output = (first a, second a, third a ++[(Expr.value expr dict)] )
    where a = exec' stmnts dict input output



exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec stmts dict input = third  $ exec' stmts dict input []


indentString = "  "

indentf :: Int -> String -> String
indentf 0 s = s
indentf n s = indentf (n-1) (indentString++s)


statementToString :: Statement -> Int -> String
statementToString statement n = case statement of
    Assignment var expr -> indentf n $ var ++ ":=" ++ (Expr.toString expr) ++ ";"  ++ "\n"
    Skip -> indentf n $ "skip;" ++ "\n"
    BeginEnds stmnts -> (indentf n "begin\n") ++ statementsToString stmnts (n+1)  ++ (indentf n "end\n")
    If  expr sm1 sm2 -> (indentf n $ "if " ++ (Expr.toString expr) ++ " then\n") ++ statementToString sm1 (n+1)  ++ (indentf n "else\n") ++ statementToString sm2 (n+1)
    While expr sm -> (indentf n $ "while " ++ (Expr.toString expr) ++ " do\n") ++ statementToString sm (n+1)
    Read var -> indentf n $ "read "++ var ++";\n"
    Write expr -> indentf n $ "write "++  (Expr.toString expr) ++";\n"


statementsToString :: Statements -> Int  -> String
statementsToString [] _ = ""
statementsToString (sm:stmnts) n = (statementToString sm n) ++ (statementsToString stmnts n)



instance Parse Statement where
  parse = parseStatement
  toString statement = statementToString statement 0
