
module StringAlignments where


import Test.HUnit

import Control.Arrow (Arrow (second))
import Data.List (maximumBy, sort)
import Data.Tuple
import GHC.Base (maxInt)






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
maximaBy valueFcn xs = xs -- todo



optAlignments :: String -> String -> [AlignmentType]

similarityScore :: String -> String -> Int

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList] 

todo = 42
similarityScore string1 string2 = todo
optAlignments string1 string2 = [("todo", "todo")]
outputOptAlignments string1 string2 = putStrLn ("TODO")
newSimilarityScore string1 string2 = todo
newOptAlignments string1 string2 = [("todo", "todo")]





similarityScoreTest = test [similarityScore "writers" "vintner" ~=? -5] 
maximaByTest = test [sort (maximaBy length ["cs", "efd", "lth", "it"]) ~=? sort ["efd", "lth"]]
optAlignmentsTest = test [sort (optAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
newSimilarityScoreTest = test [newSimilarityScore "writers" "vintner" ~=? -5]
newOptAlignmentsTest = test [sort(newOptAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]
mcsLengthTest = test [mcsLength [3, 2, 8, 2, 3, 9, 4, 3, 9] [1, 3, 2, 3, 7, 9] ~=? 4] -- [3, 2, 3, 9]



