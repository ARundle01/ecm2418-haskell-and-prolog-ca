import Data.List
main :: IO()

-- Qu 1.1 --

rule1 :: (Int, Int, Int, Int, Int, Int) -> Bool
-- checks if all digits are different
rule1 (a,b,c,d,e,f)
  | length (filter (==a) ts) > 1 = False
  | length (filter (==b) ts) > 1 = False
  | length (filter (==c) ts) > 1 = False
  | length (filter (==d) ts) > 1 = False
  | length (filter (==e) ts) > 1 = False
  | length (filter (==f) ts) > 1 = False
  | otherwise                    = True
  where
    ts = [a,b,c,d,e,f]




-- Qu 1.2 --

isEven :: Int -> Bool
isEven n
  = n `mod` 2 == 0

isOdd :: Int -> Bool
isOdd n
  = n `mod` 2 /= 0

toPairs :: (Int, Int, Int, Int, Int, Int) -> [(Int, Int)]
-- converts a 6-tuple to a list of pairs
toPairs (a,b,c,d,e,f)
  = [(a,b), (c,d), (e,f)]

rule2 :: (Int, Int, Int, Int, Int, Int) -> Bool
-- checks if alternate digits are odd and even or vice versa
rule2 sixtuple
  | length (filter (\(a,b) -> isEven a && isOdd b) pairlist) == 3 = True
  | length (filter (\(a,b) -> isOdd a && isEven b) pairlist) == 3 = True
  | otherwise                                                     = False
  where
    pairlist = toPairs sixtuple




-- Qu 1.3 --

rule3 :: (Int, Int, Int, Int, Int, Int) -> Bool
-- checks that alternate digits differ by more than 2
rule3 sixtuple
  | length (filter (\(a,b) -> (a-b>2) || (b-a>2)) pairlist) == 3 = True
  | otherwise                                                    = False
  where
    pairlist = toPairs sixtuple




-- Qu 1.4 --

pairToInt :: (Int, Int) -> Int
-- converts a pair into a base-10 Int
pairToInt pair
  = (fst pair * 10) + snd pair

rule4 :: (Int, Int, Int, Int, Int, Int) -> Bool
-- checks that the first and middle pair form numbers that are both multiples of the last pair
rule4 sixtuple
  | firstInt `mod` lastInt == 0 && middleInt `mod` lastInt == 0 = True
  | otherwise                                                   = False
  where
    pairlist = toPairs sixtuple
    firstInt  = pairToInt (pairlist!!0)
    middleInt = pairToInt (pairlist!!1)
    lastInt   = pairToInt (pairlist!!2)




-- Qu 1.5 --

cartesianProduct :: [Int] -> [(Int, Int, Int, Int, Int, Int)]
-- generates all permutations of a 6-tuple containing only elements of the input list
cartesianProduct n
  = [(a,b,c,d,e,f) | a <- n, b <- n, c <- n, d <- n, e <- n, f <- n]
  
possibles :: [(Int, Int, Int, Int, Int, Int)]
-- generates all possible 6-tuples with digits [0..9]
possibles
  = cartesianProduct [0..9]
  
  
  
  
-- Qu 1.6 --
  
isSolution :: (Int, Int, Int, Int, Int, Int) -> Bool
-- checks that a given 6-tuple is a solution to the puzzle
isSolution sixtuple
  |  rule1 sixtuple
  && rule2 sixtuple
  && rule3 sixtuple
  && rule4 sixtuple = True
  | otherwise       = False



-- Main --

main
    = putStrLn(show (filter isSolution possibles))