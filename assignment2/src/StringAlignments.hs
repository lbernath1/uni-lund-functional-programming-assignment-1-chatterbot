
module StringAlignments where


import Test.HUnit

import Control.Arrow (Arrow (second))
import Data.List (maximumBy, sort)
import Data.Tuple
import GHC.Base (maxInt)
-- import Data.ByteString (length)



scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1


mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1))
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)


type AlignmentType = (String,String)

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\ x -> valueFcn x >= a  ) xs
  where
    a = maximum (map valueFcn xs)



optAlignments :: String -> String -> [AlignmentType]


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]



similarityScore :: String -> String -> Int
similarityScore s1 s2 = simScores (length s1) (length s2)
  where
    simScores i j = simScoreTable!!i!!j
    simScoreTable = [[ simsEntry i j | j<-[0..]] | i<-[0..] ]

    simsEntry :: Int -> Int -> Int
    simsEntry a 0 = a*scoreSpace
    simsEntry 0 a = a*scoreSpace
    simsEntry i j = maximum ([
        simsEntry i (j-1) + scoreSpace,
        simsEntry (i-1) j + scoreSpace,
        if x == y then simsEntry (i-1) (j-1) + scoreMatch else simsEntry (i-1) (j-1) + scoreMismatch])
      where
         x = s1!!(i-1)
         y = s2!!(j-1)




todo = 42
optAlignments string1 string2 = [("todo", "todo")]
outputOptAlignments string1 string2 = putStrLn ("TODO")
newSimilarityScore string1 string2 = todo
newOptAlignments string1 string2 = [("todo", "todo")]





similarityScoreTest = test [similarityScore "writers" "vintner" ~=? -5]
maximaByTest = test [sort (maximaBy length ["cs", "efd", "lth", "it"]) ~=? sort ["efd", "lth"]]
optAlignmentsTest = test [sort (optAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
newSimilarityScoreTest = test [newSimilarityScore "writers" "vintner" ~=? -5]
newOptAlignmentsTest = test [sort (newOptAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
mcsLengthTest = test [mcsLength [3, 2, 8, 2, 3, 9, 4, 3, 9] [1, 3, 2, 3, 7, 9] ~=? 4] -- [3, 2, 3, 9]



