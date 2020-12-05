{-# LANGUAGE TupleSections #-}
module Days.Day04 where

import Data.Maybe (isJust)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data Unvalidated = Unvalidated {
  byr :: Maybe String, iyr :: Maybe String, eyr :: Maybe String,
  hgt :: Maybe String, hcl :: Maybe String, ecl :: Maybe String,
  pid :: Maybe String, cid :: Maybe String }
  deriving Show

u :: Unvalidated
u = Unvalidated (Just "") (Just "") (Just "") (Just "") (Just "") (Just "")
                (Just "") (Just "")

data Length = Cm | In

parse :: String -> Unvalidated
parse s =
  let kvs = break (== ':') <$> words s
      g k = strip <$> lookup k kvs
      in Unvalidated  (g "byr") (g "iyr") (g "eyr") (g "hgt")
                      (g "hcl") (g "ecl") (g "pid") (g "cid")

split :: String -> [String]
split s = build "" [] $ lines s
  where build :: String -> [String] -> [String] -> [String]
        build accS accL (x : xs)
          | x == "" = build "" (accS : accL) xs
          | otherwise = build (accS ++ " " ++ x) accL xs
        build accS accL [] = accS : accL

validSimple :: Unvalidated -> Bool
validSimple u =
  isJust $ byr u >> iyr u >> eyr u >> hgt u
        >> hcl u >> ecl u >> pid u

validAdvanced :: Unvalidated -> Bool
validAdvanced u =
  isJust $ byr u >>= readMaybe >>= validateIntRange (1920,2002)
        >> iyr u >>= readMaybe >>= validateIntRange (2010,2020)
        >> eyr u >>= readMaybe >>= validateIntRange (2020,2030)
        >> hgt u >>= validateHeight
        >> hcl u >>= parseHairColor
        >> ecl u >>= validateEyeColor
        >> pid u >>= isDigitsOfLength 9

validateIntRange :: (Int, Int) -> Int -> Maybe Int
validateIntRange (mi,ma) i
  | i >= mi && i <= ma = Just i
  | otherwise = Nothing

validateHeight :: String -> Maybe (Int, Length)
validateHeight s =
  case span isDigit s of
    (numbers, "cm") | not (null numbers) ->
      (, Cm) <$> (readMaybe numbers >>= validateIntRange (150,193))
    (numbers, "in") | not (null numbers) ->
      (, In) <$> (readMaybe numbers >>= validateIntRange (59,76))
    _ -> Nothing

parseHairColor :: String -> Maybe String
parseHairColor s =
  case splitAt 1 s of
    ("#", code) | length code == 6 && isHex code-> Just code
    _ -> Nothing

isHex :: String -> Bool
isHex = all (\c -> isDigit c || isInRange c)
  where isInRange c = c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e'
                    || c == 'f'

isDigitsOfLength :: Int -> String -> Maybe String
isDigitsOfLength l s
  | length s == l && all isDigit s = Just s
  | otherwise = Nothing

validateEyeColor :: String -> Maybe String
validateEyeColor s
  | s == "amb" || s == "blu" || s == "brn" || s == "gry" || s == "grn"
    || s == "hzl" || s == "oth" = Just s
  | otherwise = Nothing

strip :: String -> String
strip = filter (\c -> c /= ':' && c /= ' ')

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day04.txt"
  print . length . filter validSimple $ parse <$> split content

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day04.txt"
  print . length . filter validAdvanced $ parse <$> split content
