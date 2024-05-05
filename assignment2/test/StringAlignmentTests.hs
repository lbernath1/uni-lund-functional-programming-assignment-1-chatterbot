module Main where
import StringAlignments
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
    