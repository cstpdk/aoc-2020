module Util where

splitByEmptyLines :: String -> [String]
splitByEmptyLines s = build "" [] $ lines s
  where build :: String -> [String] -> [String] -> [String]
        build accS accL (x : xs)
          | x == "" = build "" (accS : accL) xs
          | otherwise = build (accS ++ " " ++ x) accL xs
        build accS accL [] = accS : accL
