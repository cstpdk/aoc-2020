module Days.Day05 where

import Data.List (nub, sort)

-- Binary format: 
-- 7 chars of F or B, F is lower, B is higher
-- 3 chars of R or L, L is lower, R is higher

type Row = Int
type Column = Int
type SeatId = Int

position :: String -> (Row, Column)
position s = 
  ( lowHigh 'F' (0,127) $ take 7 s, 
    lowHigh 'L' (0, 7) $ drop 7 s)

lowHigh :: Char -> (Int, Int) -> String -> Row
lowHigh lowerChar acc [c]
  | c == lowerChar = minimum (takeLower acc)
  | otherwise = maximum (takeUpper acc)
lowHigh lowerChar (mi, ma) (c : cs)
  | c == lowerChar = lowHigh lowerChar (takeLower (mi, ma)) cs
  | otherwise = lowHigh lowerChar (takeUpper (mi, ma)) cs

takeLower (mi,ma) = (mi, (ma - mi) `div` 2 + mi)
takeUpper (mi,ma) = (((ma - mi) `div` 2) + 1 + mi, ma)

seatId :: (Row, Column) -> SeatId
seatId (row, column) = row * 8 + column

findCandidateIds :: [SeatId] -> [SeatId]
findCandidateIds = inner []
  where inner acc (x : xs@(y : z : _))
          | x + 1 /= y = inner ( (x + 1) : acc) xs
          | z - 1 /= y = inner ( (z - 1) : acc) xs
          | otherwise = inner acc xs
        inner acc [] = acc
        inner acc (_ : xs) = inner acc xs

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day05.txt"
  print . maximum $ seatId . position <$> lines content

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day05.txt"
  print . nub . findCandidateIds . sort $ seatId . position <$> lines content
