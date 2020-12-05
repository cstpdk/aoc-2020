module Days.Day03 where

formatInput :: String -> [String]
formatInput s = cycle <$> lines s

solveFor :: (Int -> Int) -> Int -> [String] -> Int
solveFor inc skip = inner 0 0
  where inner :: Int -> Int -> [String] -> Int
        inner i c (x : xs)
          | x !! i == '#' = inner (inc i) (c + 1) (drop skip xs)
          | otherwise = inner (inc i) c (drop skip xs)
        inner _ c [] = c


part1 :: IO ()
part1 = do
  content <- readFile "inputs/day03/1.txt"
  print (solveFor (3 +) 0 $ formatInput content)

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day03/1.txt"
  let res inp = solveFor (1 +) 0 inp * 
                solveFor (3 +) 0 inp * 
                solveFor (5 +) 0 inp * 
                solveFor (7 +) 0 inp *
                solveFor (1 +) 1 inp
                in print (res $ formatInput content)
