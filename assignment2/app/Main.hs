-- Ellen Hedberg el1185he-s, Leon Bernáth le1327be-s
-- 

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
    print "Result of similarityScore: "
    print (similarityScore "hej" "hello")

similarityScore :: String -> String -> Int
similarityScore s1 s2 = if s1 == s2 then length s1 else 5

