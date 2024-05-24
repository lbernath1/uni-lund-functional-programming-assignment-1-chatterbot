module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import Data.Maybe (Maybe(Nothing), fromJust, isJust, isNothing)
newtype T = Program Statement.Statements


programToStatements :: T -> Statement.Statements
programToStatements (Program program) = program  

instance Parse T where --parse takes string, returns (program, string), toString takes program, returns string
  parse s = Just ((Program stmts), "")
    where stmts = fst (fromJust (Statement.parseStatements s))

  toString (Program program) = Statement.statementsToString program 0


exec program input = Statement.exec program dict input  --make point free-er later
  where dict = Dictionary.empty
