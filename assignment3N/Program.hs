module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program Statement.Statements
instance Parse T where
  parse program = ((iter Statement.parse) program) --TODOss
  toString program = concat (map (\ statement -> toString statement) program)
             
exec program input = Statement.exec program dict input  --make point free-er later
  where dict = Dictionary.empty
