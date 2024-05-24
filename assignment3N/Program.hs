module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
import Data.Maybe (Maybe(Nothing), fromJust, isJust, isNothing)
newtype T = Program Statement.Statements deriving (Show)



instance Parse T where 
  parse = Statement.parseStatements >-> Program
  toString (Program program) = Statement.statementsToString program 0


exec (Program program) input = Statement.exec program dict input  --make point free-er later
  where dict = Dictionary.empty
