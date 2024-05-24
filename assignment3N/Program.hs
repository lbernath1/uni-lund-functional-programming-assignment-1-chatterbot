module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program Statement.Statements


programToStatements :: T -> Statement.Statements
programToStatements (Program program) = program  

tupleStatementToProgram :: Maybe (Statement.Statements, String ) -> Maybe (Statement.Statements, String)
tupleStatementToProgram (a, b) = (Program a, b)

instance Parse T where
  parse program = tupleStatementToProgram $ Statement.parseStatements (programToStatements program) 
  toString program = Statement.statementsToString (programToStatements program) 0


exec program input = Statement.exec program dict input  --make point free-er later
  where dict = Dictionary.empty
