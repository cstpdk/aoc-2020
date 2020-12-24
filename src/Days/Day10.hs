module Days.Day10 where

import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Data.IntMap.Strict as Map

parse :: String -> [Int]
parse = fmap read . lines

type OneDiffs = Int
type ThreeDiffs = Int

count :: [Int] -> (OneDiffs, ThreeDiffs)
count ints =
  let (ones, threes, _) = foldl inner (0,0,0) $ sort ints
  in (ones, threes + 1) -- + 1 to include the device 
  where inner (ones, threes, prev) i
          | i == prev + 1 = (ones + 1, threes, i)
          | i == prev + 3 = (ones, threes + 1, i)
          | otherwise = (ones, threes, i)

findCombinations :: [Int] -> Map.IntMap Int -> Map.IntMap Int
findCombinations [] m = m
findCombinations ints@(x:xs) m = 
  let successors = validSuccessors ints
      paths = fromMaybe 0 (Map.lookup x m)
      m' = foldl (\m'' k -> Map.insertWith (+) k paths m'') m successors
  in findCombinations xs m'

validSuccessors :: [Int] -> [Int]
validSuccessors (x:xs) = takeWhile (\i -> i <= x + 3) xs

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day10.txt"
  let input = parse content
  let (ones, threes) = count input
  print $ ones * threes

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day10.txt"
  let input = parse content
  let m = Map.fromList [(0,1)]
  let l = 0 : sort input ++ [maximum input + 3]
  print $ Map.lookup (maximum input) $ findCombinations l m 
