-- Ellen Hedberg el1185he-s, Leon Bernáth le1327be-s
-- 

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
    print "Result of similarityScore: "

negDistance :: Int -> Int -> Int
negDistance i1 i2 = min i1 i2 - max i1 i2




