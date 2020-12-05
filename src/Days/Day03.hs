module Days.Day03 where

formatInput :: String -> [String]
formatInput s = cycle <$> lines s

solveFor :: [String] -> (Int -> Int) -> Int -> Int
solveFor lines inc skip = inner 0 0 lines
  where inner :: Int -> Int -> [String] -> Int
        inner i c (x : xs)
          | x !! i == '#' = inner (inc i) (c + 1) (drop skip xs)
          | otherwise = inner (inc i) c (drop skip xs)
        inner _ c [] = c


part1 :: IO ()
part1 = do
  content <- readFile "inputs/day03.txt"
  print (solveFor (formatInput content) (3 +) 0)

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day03.txt"
  let inp = formatInput content
      in print $ product $ uncurry (solveFor inp) <$> [ ((1 +), 0), 
                                                        ((3 +), 0), 
                                                        ((5 +), 0), 
                                                        ((7 +), 0), 
                                                        ((1 +), 1)]
