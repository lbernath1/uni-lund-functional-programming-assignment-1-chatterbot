-- Ellen Hedberg el1185he-s, Leon Bernáth le1327be-s

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.List
import Data.Maybe (Maybe(Nothing), fromJust, isJust, isNothing)
import Data.Bifunctor (second, bimap)

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

comp12 = (.).(.) -- comp12 f(z) g(x,y) -> f(g(x,y))
comp13 = (.).(.).(.)  -- comp13 f(d) g (a,b,c) -> f(g(a,b,c))

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind input = do
  r <- randomIO :: IO Float
  (((.).(.)) (return . rulesApply) map) (Data.Bifunctor.second (pick r)) input --TODO: not yet point free

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = (flip (comp12 fromJust orElse) (Just ([])) .) . transformationsApply "*" reflect 

reflect :: Phrase -> Phrase

reflect = map (\ word -> fromJust (orElse (lookup word reflections) (Just word)))

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
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map (Data.Bifunctor.bimap prepare (map prepare))

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

reductionsApply :: [PhrasePair] -> Phrase -> Phrase --TODO: not yet point free
reductionsApply listOfPairs input = maybe input (reductionsApply listOfPairs) (transformationsApply "*" id listOfPairs input) 


-------------------------------------------------------
-- Match and substitute
-------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
-- x wildcard, y list to change, z string to insert,
substitute x y z = concatMap (\ char -> if char /= x then [char] else z) y --TODO: not yet point free


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- wildcard, pattern, series of things
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] (x:xs) = Nothing
match _ (x:xs) [] = Nothing
match wildcard (p:ps) (s:ss) =
  (if wildcard /= p then
    (if (p == s && Data.Maybe.isJust (match wildcard ps ss))
      then (match wildcard ps ss) 
      else (Nothing)
    )
        else (orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss)))) --TODO: not yet point free


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = (if Data.Maybe.isJust (match wc ps xs) then Just [x] else Nothing )  --TODO: not yet point free  


-- longerWildcardMatch (wc:ps) (x:xs) = Nothing
longerWildcardMatch (wc : ps) (x : xs)
  | isNothing (match wc (wc : ps) xs)
      || Data.Maybe.isJust (singleWildcardMatch (wc : ps) (x : xs))
  = Nothing
  | Data.Maybe.isJust (singleWildcardMatch (wc : ps) xs)
  = Just [x, (head xs)]
  | otherwise
  = Just (x : (fromJust (longerWildcardMatch (wc : ps) xs))) --TODO: not yet point free
      


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
transformationApply wc f text (s1, s2) = if  Data.Maybe.isJust (match wc s1 text) then Just (substitute wc s2 (f (fromJust  (match wc s1 text)))) else Nothing --TODO: not yet point free



-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
-- wc, function, listOfTuples, text 
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (first:listOfTuples) text = orElse (transformationApply wc f text first) (transformationsApply wc f listOfTuples text) --TODO: not yet point free


