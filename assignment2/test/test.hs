
module Main where

  import Data.List
  similarityScoreTest = test [similarityScore "writers" "vintner" ~=? -5] 
  maximaByTest = test [sort (maximaBy length ["cs", "efd", "lth", "it"]) ~=? sort ["efd", "lth"]]
  optAlignmentsTest = test [sort (optAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
  newSimilarityScoreTest = test [newSimilarityScore "writers" "vintner" ~=? -5]
  newOptAlignmentsTest = test [sort(newOptAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
  mcsLengthTest = test [mcsLength [3, 2, 8, 2, 3, 9, 4, 3, 9] [1, 3, 2, 3, 7, 9] ~=? 4] -- [3, 2, 3, 9]

