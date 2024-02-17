import Data.String
import qualified Data.List as List
import qualified Data.Ord as Ord
main :: IO()

-- Qu 2.1 --

pretty :: [[[Char]]] -> String
-- Formats a list of strings into a pretty string
pretty []
  = ""
pretty xss
  = Data.String.unlines xs
  where
    xs = concat xss




-- Qu 2.2 --

type Point
  = (Int, Int)

glider :: [Point]
glider
  = [(0,2), (1,3), (2,1), (2,2), (2,3)]
  
convertToInt :: Int -> Point -> Int
-- Converts a Point into a position on a grid of width w.
convertToInt w p
  = (fst p) + (snd p * w)

mapConversion :: [Point] -> Int -> [Int]
-- sorts a map of conversion to Int onto a list of Points
mapConversion xs w
  = List.sort (map (convertToInt w) xs)

assembleString :: [Int] -> Int -> [Char]
-- assembles a string depending on whether the index equals the value in list
assembleString [] n
  = []
assembleString (x:xs) n
  | x == n    = '#' : assembleString xs (n+1)
  | otherwise = '.' : assembleString (x:xs) (n+1)

chunkOf :: Int -> [a] -> [[a]]
-- Splits a list into a list of lists of size n
chunkOf _ []
  = []
chunkOf n xs
  = (take n xs) : (chunkOf n (drop n xs))

setHeight :: String -> Int -> String
-- adds remaining periods to fill size of grid
setHeight [] n
  = []
setHeight xs n
  | length xs < n = xs ++ (replicate r '.')
  | otherwise = xs
  where
    r = n - length xs

visualise :: Int -> Int -> [Point] -> [[Char]]
-- creates a string representation of a grid based on the list of Points input
visualise _ _ []
  = []
visualise w h xs
  = chunkOf w (setHeight assembled p)
  where
    assembled = assembleString (mapConversion xs w) 0
    p = w * h
    
visualisation :: Int -> Int -> [[Point]] -> [[[Char]]]
-- maps visualise onto a list of list of Points
visualisation w h xss
  = map (visualise w h) xss




-- Qu 2.3 --

filterByRow :: [Point] -> Int -> [Point]
-- filters Points if they are in the same row as the input n
filterByRow xs n
  = filter (\(_,b) -> b == n) xs

filterByColumn :: [Point] -> Int -> [Point]
-- filters Points if they are in same column as input n
filterByColumn xs n
  = filter (\(a,_) -> a == n) xs

getLeftRight :: [Point] -> Point -> [Point]
-- retrieves any Points that are spatially to the left or right of input
getLeftRight [] _ 
  = []
getLeftRight xs (a,b)
  = filterByColumn (filterByRow xs b) (a+1) ++ filterByColumn (filterByRow xs b) (a-1)

getTopBottom :: [Point] -> Point -> [Point]
-- retrieves Points that are spatially above or below the input
getTopBottom [] _
  = []
getTopBottom xs (a,b)
  = filterByRow (filterByColumn xs a) (b+1) ++ filterByRow (filterByColumn xs a) (b-1)

getTopCorners :: [Point] -> Point -> [Point]
-- retrieves Points that are spatially and diagonally above the input
getTopCorners [] _
  = []
getTopCorners xs (a,b)
  = filterByColumn (filterByRow xs (b-1)) (a+1) ++ filterByColumn (filterByRow xs (b-1)) (a-1)

getBottomCorners :: [Point] -> Point -> [Point]
-- retrieves Points that are spatially and diagonally below the input
getBottomCorners [] _
  = []
getBottomCorners xs (a,b)
  = filterByColumn (filterByRow xs (b+1)) (a+1) ++ filterByColumn (filterByRow xs (b+1)) (a-1)

filterNextTo :: [Point] -> Point -> [Point]
-- retrieves any Points that may be in any of the 8 positions around the input
filterNextTo xs p
  = leftright ++ topbottom ++ topcorners ++ bottomcorners
  where
    leftright     = getLeftRight xs p
    topbottom     = getTopBottom xs p
    topcorners    = getTopCorners xs p
    bottomcorners = getBottomCorners xs p

numOfNeighbours :: [Point] -> Point -> Int
-- counts the number of Points that neighbour (in any direction) the input
numOfNeighbours xs p
  = length (filterNextTo xs p)

becomesAlive :: [Point] -> Point -> Bool
-- if a Point has exactly 3 neighbours it becomes alive
becomesAlive xs p
  | numOfNeighbours xs p == 3 = True
  | otherwise                 = False

filterStaysAlive :: [Point] -> [Point]
-- filters all points that have 2 or 3 neighbours
filterStaysAlive ps
  = filter (\p -> numOfNeighbours ps p == 2 || numOfNeighbours ps p == 3 ) ps

maximumFst :: [Point] -> Int
-- finds the maximum column in a list of Points
maximumFst ps
  = fst (List.maximumBy (Ord.comparing fst) ps)

maximumSnd :: [Point] -> Int
-- finds the maximum row in a list of Points
maximumSnd ps
  = snd (List.maximumBy (Ord.comparing snd) ps)

grid :: Int -> Int -> [Point]
-- creates a list of Points of [(0,0)..(b, a)]
grid b a
  = [(y, x) | y <- [0..b], x <- [0..a]]

genGrid :: [Point] -> [Point]
-- generates a grid of Points that would fully contain a given list of Points
genGrid ps
  = grid y x
  where
    y = (maximumFst ps) + 1
    x = (maximumSnd ps) + 1

deadBecomeAlive :: [Point] -> [Point] -> [Point]
-- returns a list of currently dead cells that will become alive
deadBecomeAlive [] _
  = []
deadBecomeAlive (d:ds) liveCells
  | becomesAlive liveCells d = d : deadBecomeAlive ds liveCells
  | otherwise                = deadBecomeAlive ds liveCells

nextGen :: [Point] -> [Point]
-- generates a list of the next generation of a list of Points as per the 3 rules of evolution
nextGen ps
  = filterStaysAlive liveCells ++ deadBecomeAlive deadCells liveCells
  where
    liveCells = ps
    deadCells = (genGrid ps) List.\\ liveCells

evolution :: [Point] -> [[Point]]
-- generates an infinite list of all generations of living cells
evolution []
  = []
evolution ps
  = iterate (nextGen) glider




-- Main --

main
  = putStrLn(pretty (take 8 (visualisation 5 5 (evolution glider))))