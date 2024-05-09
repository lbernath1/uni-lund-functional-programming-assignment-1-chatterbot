
module StringAlignments where


import Test.HUnit

import Data.List (sort)
import Data.Char (toUpper)
import Data.Bifunctor (bimap)

scoreMatch :: Int
scoreMatch = 0
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
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

similarityScore :: String -> String -> Int
similarityScore s1 s2 = simScores (length s1) (length s2)
  where
    simScores i j = simScoreTable!!i!!j
    simScoreTable = [[ simsEntry i j | j<-[0..]] | i<-[0..] ]

    simsEntry :: Int -> Int -> Int
    simsEntry a 0 = a*scoreSpace
    simsEntry 0 a = a*scoreSpace
    simsEntry i j = maximum [
        simsEntry i (j-1) + scoreSpace,
        simsEntry (i-1) j + scoreSpace,
        if x == y then simsEntry (i-1) (j-1) + scoreMatch else simsEntry (i-1) (j-1) + scoreMismatch]
      where
         x = s1!!(i-1)
         y = s2!!(j-1)

newSimilarityScore :: String -> String -> Int
newSimilarityScore s1 s2 = simScores (length s1) (length s2)
  where
    simScores i j = simScoreTable!!i!!j
    simScoreTable = [[ simsEntry i j | j<-[0..]] | i<-[0..] ]

    simsEntry :: Int -> Int -> Int
    simsEntry a 0 = a*scoreSpace
    simsEntry 0 a = a*scoreSpace
    simsEntry i j = maximum [
        simScores i (j-1) + scoreSpace,
        simScores (i-1) j + scoreSpace,
        if x == y then simScores (i-1) (j-1) + scoreMatch else simScores (i-1) (j-1) + scoreMismatch]
      where
         x = s1!!(i-1)
         y = s2!!(j-1)




attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\ x -> valueFcn x >= a  ) xs
  where
    a = maximum (map valueFcn xs)



type AlignmentType = (String,String)
optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2 = reverseStrings $ snd $ optAligns (length s1) (length s2)
  where
    optAligns i j = optAlignTable!!i!!j
    optAlignTable = [[ oaEntry i j | j<-[0..]] | i<-[0..] ]

    oaEntry :: Int -> Int -> (Int, [AlignmentType])
    oaEntry 0 i = (i*scoreSpace, [(['-'|_<-[1..i]], take i s2)])
    oaEntry j 0 = (j*scoreSpace, [(take j s1, ['-'|_<-[1..j]])])

    oaEntry i j = changeType  (maximaBy fst [
      (fst (oaEntry i (j-1)) + scoreSpace, attachHeads '-' y (snd $ oaEntry i (j-1))),
      (fst (oaEntry (i-1) j) + scoreSpace, attachHeads x '-' (snd $ oaEntry (i-1) j)),
      (if x == y then fst  (oaEntry (i-1) (j-1)) + scoreMatch else fst (oaEntry (i-1) (j-1)) + scoreMismatch, attachHeads x y (snd $ oaEntry (i-1) (j-1)))
      ])
      where
         x = s1!!(i-1)
         y = s2!!(j-1)

newOptAlignments :: String -> String -> [AlignmentType]
newOptAlignments s1 s2 = reverseStrings $ snd $ optAligns (length s1) (length s2)
  where
    optAligns i j = optAlignTable!!i!!j
    optAlignTable = [[ oaEntry i j | j<-[0..]] | i<-[0..] ]

    oaEntry :: Int -> Int -> (Int, [AlignmentType])
    oaEntry 0 i = (i*scoreSpace, [(['-'|_<-[1..i]], take i s2)])
    oaEntry j 0 = (j*scoreSpace, [(take j s1, ['-'|_<-[1..j]])])

    oaEntry i j = changeType  (maximaBy fst [
      (fst (optAligns i (j-1)) + scoreSpace, attachHeads '-' y (snd $ optAligns i (j-1))),
      (fst (optAligns (i-1) j) + scoreSpace, attachHeads x '-' (snd $ optAligns (i-1) j)),
      (if x == y then fst  (optAligns (i-1) (j-1)) + scoreMatch else fst (optAligns (i-1) (j-1)) + scoreMismatch, attachHeads x y (snd $ optAligns (i-1) (j-1)))
      ])
      where
         x = s1!!(i-1)
         y = s2!!(j-1)



changeType :: [(Int, [AlignmentType])] -> (Int, [AlignmentType])
changeType listOfTuples = (fst . head $ listOfTuples, concatMap snd listOfTuples )

reverseStrings :: [AlignmentType] -> [AlignmentType]
reverseStrings = map (Data.Bifunctor.bimap reverse reverse )


outputOptAlignments :: String -> String -> IO()
outputOptAlignments s1 s2 = do
  putStrLn "number of opt aligns: "
  print (length optAligns)
  printListofAlignments optAligns
  where optAligns = optAlignments s1 s2


printListofAlignments :: [AlignmentType] -> IO()
printListofAlignments [] = putStrLn ""
printListofAlignments (tuple:listofTuples) = do
  putStrLn $ concatMap (\ c -> toUpper c : " ") (fst tuple)
  putStrLn $ concatMap (\ c-> toUpper c : " ") (snd tuple) ++ "\n"
  printListofAlignments listofTuples


-- Credit for the tests goes to Coffee from the Discord Server.
similarityScoreTest :: Test
similarityScoreTest = test [similarityScore "writers" "vintner" ~=? -5]

maximaByTest :: Test
maximaByTest = test [sort (maximaBy length ["cs", "efd", "lth", "it"]) ~=? sort ["efd", "lth"]]

optAlignmentsTest :: Test
optAlignmentsTest = test [sort (optAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]

newSimilarityScoreTest :: Test
newSimilarityScoreTest = test [newSimilarityScore "writers" "vintner" ~=? -5]

newOptAlignmentsTest :: Test
newOptAlignmentsTest = test [sort (newOptAlignments "writers" "vintner") ~=? sort [("writ-ers", "vintner-"), ("wri-t-ers", "-vintner-"), ("wri-t-ers", "v-intner-")]]

mcsLengthTest :: Test
mcsLengthTest = test [mcsLength [3, 2, 8, 2, 3, 9, 4, 3, 9] [1, 3, 2, 3, 7, 9] ~=? 4] -- [3, 2, 3, 9]