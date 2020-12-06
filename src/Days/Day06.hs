module Days.Day06 where

import Util (splitByEmptyLines)
import Data.List (nub, intersect)

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day06.txt"
  print . sum $ length . filter (/= ' ') . nub <$> splitByEmptyLines content

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day06.txt"
  print . sum $ length . foldl1 intersect . words <$> splitByEmptyLines content
