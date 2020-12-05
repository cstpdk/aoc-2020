module Days.Day01 where

import Data.List (find, tails)

{- Read the file with an int per line and collect into list -}
readInput :: IO [Int]
readInput = do
  content <- readFile "inputs/day01.txt"
  return . fmap read $ lines content

{- Find a pair of integers that sum to 2020 in the list
 - Naive solution by exhaustive search, O(n*n!) -}
findPair :: [Int] -> Maybe (Int, Int)
findPair ints = find is2020Pair pairs

  where pairs :: [(Int, Int)]
        pairs = toPairs [] ints
        toPairs :: [(Int, Int)] -> [Int] ->  [(Int, Int)]
        toPairs acc (x : xs) = toPairs (acc ++ zip (repeat x) xs) xs
        toPairs acc [] = acc


{- Find a triple of integers that sum to 2020 in the list
 - has cubic performance, which is not great -}
findTriple :: [Int] -> Maybe (Int, Int, Int)
findTriple ints = find is2020Triple triples
  where triples :: [(Int, Int, Int)]
        -- I'm surrendering and doing the list comprehension thing
        triples = [(x, y, z) |  (x : ys) <- tails ints,
                                (y : zs) <- tails ys,
                                z <- zs]

is2020Pair :: (Int, Int) -> Bool
is2020Pair (a,b) = (a + b) == 2020

is2020Triple :: (Int, Int, Int) -> Bool
is2020Triple (a,b,c) = (a + b + c) == 2020

part1 :: IO ()
part1 = do
  is <- readInput
  maybe (putStrLn "No result") (
        print . uncurry (*)) (findPair is)

part2 :: IO ()
part2 = do
  is <- readInput

  maybe (putStrLn "No result") (
        print . (\(a,b,c) -> a * b * c)) (findTriple is)
