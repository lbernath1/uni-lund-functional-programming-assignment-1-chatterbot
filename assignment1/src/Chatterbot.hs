{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.List
import Data.Maybe (Maybe(Nothing), fromJust)

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase

--Input for transformationsApply: wc f (first:listOfTuples) text
rulesApply phrasePairs phrase = fromJust (orElse (transformationsApply "*" reflect phrasePairs phrase) (Just []))

reflect :: Phrase -> Phrase

reflect phrase = map (\ word -> fromJust (orElse (keyToValue word reflections) (Just word))) phrase

keyToValue :: Eq a => a -> [(a, b)] -> Maybe b
keyToValue key [] = Nothing
keyToValue key (pair:listOfPairs) = if (fst pair) == key then Just (snd pair) else keyToValue key listOfPairs

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
-- x wildcard, y list to change, z string to insert,
substitute x y z = concat (map (\char -> if char /= x then [char] else z) y)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- wildcard, pattern, series of things
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] (x:xs) = Nothing
match _ (x:xs) [] = Nothing
match wildcard (p:ps) (s:ss) =
  (if wildcard /= p then
    (if (p == s && (match wildcard ps ss) /= Nothing)
      then (match wildcard ps ss) 
      else (Nothing)
    )
        else (orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))))


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = (if (match wc ps xs) /= Nothing then Just [x] else Nothing )    


-- longerWildcardMatch (wc:ps) (x:xs) = Nothing
longerWildcardMatch (wc:ps) (x:xs) = 
  (if match wc (wc:ps) xs == Nothing || singleWildcardMatch (wc:ps) (x:xs) /= Nothing 
    then Nothing
  else 
  (if (singleWildcardMatch (wc:ps) xs) /= Nothing 
    then Just [x, (head xs)]   
   else Just ( x:(fromJust (longerWildcardMatch (wc:ps) xs))))
  )
      


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]

transformationApply wc f text (s1, s2) = if  (match wc s1 text) /= Nothing then Just (substitute wc s2 (f (fromJust  (match wc s1 text)))) else Nothing



-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
-- wc, function, listOfTuples, text 
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (first:listOfTuples) text = orElse (transformationApply wc f text first) (transformationsApply wc f listOfTuples text)


