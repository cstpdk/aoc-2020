{-# LANGUAGE NamedFieldPuns #-}
module Days.Day02 where

import Data.Maybe

data Policy = Policy { character :: Char, nos :: (Int, Int) }
data Line = Line { policy :: Policy, password :: String }

parse :: String -> Maybe Line
parse s = case words s of
  [range, char, password] -> Just $ inner range char password
  _ -> Nothing
  where inner :: String -> String -> String -> Line
        inner r c password =
          let -- This is your daily reminder that fmap for tuple affects snd
              nos = mapTuple read $ drop 1 <$> break ('-' ==) r
              character = head c
          in Line { policy = Policy { nos, character }, password }

solveMinMax :: [Line] -> Int
solveMinMax lines = length $ filter inner lines
  where inner :: Line -> Bool
        inner Line { policy = Policy { character, nos = (mi,ma) },
                      password } =

          let occurrences = length $ filter (character ==) password
          in occurrences >= mi && occurrences <= ma

solvePosition :: [Line] -> Int
solvePosition lines = length $ filter inner lines
  where inner :: Line -> Bool
        inner Line { policy = Policy { character, nos = (mi,ma) },
                      password } =
          let pos1 = listToMaybe $ drop (mi - 1) password
              pos2 = listToMaybe $ drop (ma - 1) password
          in  (pos1 == Just character && pos2 /= Just character) ||
              (pos2 == Just character && pos1 /= Just character)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a,b) = (f a, f b)

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day02/1.txt"
  maybe (putStrLn "No solution :(")
        (print . solveMinMax)
        $ mapM parse (lines content)

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day02/1.txt"
  maybe (putStrLn "No solution :(")
        (print . solvePosition)
        $ mapM parse (lines content)
