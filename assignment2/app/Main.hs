-- Ellen Hedberg el1185he-s, Leon Bernáth le1327be-s
-- 



module Main where
import Lib
import Test.HUnit
main =
  runTestTT $
    test
      [ "maximaBy" ~: maximaByTest
      , "similarityScore" ~: similarityScoreTest
      , "optAliggments" ~: optAlignmentsTest
      , "newSimilarityScore" ~: newSimilarityScoreTest
      , "newOptAlignments" ~: newOptAlignmentsTest
      , "mscLength" ~: mcsLengthTest
      ]

todo = 42
similarityScore string1 string2 = todo
maximaBy valueFcn xs = xs -- todo
optAlignments string1 string2 = [("todo", "todo")]
outputOptAlignments string1 string2 = putStrLn ("TODO")
newSimilarityScore string1 string2 = todo
newOptAlignments string1 string2 = [("todo", "todo")]



similarityScore :: String -> String -> Int
similarityScore string1 string2



score :: Char -> Int
score(x,'-') = scoreSpace
score('-',y) = scoreSpace
score(x,y) = scoreMatch, if x == y
             scoreMismatch, if x /= y