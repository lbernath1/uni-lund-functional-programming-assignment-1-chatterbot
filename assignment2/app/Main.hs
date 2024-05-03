-- Ellen Hedberg el1185he-s, Leon Bernáth le1327be-s
-- 

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
    print "Result of similarityScore: "
    print (similarityScore "hej" "hello")

negDistance :: Int -> Int -> Int
negDistance i1 i2 = min i1 i2 - max i1 i2

similarityScore :: String -> String -> Int
similarityScore s1 s2 = negDistance (length s1) (length s2)



