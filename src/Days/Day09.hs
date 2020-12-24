module Days.Day09 where

import Data.List (find, tails)

parse :: String -> [Int]
parse = fmap read . lines

findN :: [Int] -> [Int] -> Int
findN window (x : xs) =
  case find (\(y1,y2) -> y1 + y2 == x) pairs of
    Nothing -> x
    Just _ -> findN (drop 1 window ++ [x]) xs
  where pairs = [(x,y) | (x : ys) <- tails window, y <- ys]
  
findConsecutive :: Int -> Int -> [Int] -> Maybe [Int]
findConsecutive _ _ [] = Nothing
findConsecutive n m xs = case sum (take m xs) of
  s | s > n -> findConsecutive n 2 (drop 1 xs)
  s | s < n -> findConsecutive n (m + 1) xs
  s | s == n -> Just (take m xs)

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day09.txt"
  let input = parse content
  print $ findN (take 25 input) (drop 25 input)

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day09.txt"
  let input = parse content
  let n = 22406676

  let range = findConsecutive n 2 input
  case range of
    Just r | length r > 1 -> print (minimum r + maximum r)
    _ -> print "No solution"
