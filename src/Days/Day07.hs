module Days.Day07 where

import Data.List (sortOn, nub, elemIndex, sort)
import Data.Graph (dfs, indegree, outdegree, edges, vertices, path, Graph, transposeG, reachable, graphFromEdges)
import Data.Maybe (fromJust)

import qualified Data.Map.Strict as Map

type Color = String
data BagSpec = BagSpec { color :: Color, contents :: [(Color, Int)] }
  deriving Show

colorToInt :: [Color] -> Color -> Maybe Int
colorToInt colors = flip elemIndex (sort colors)

findEdges :: [BagSpec] -> [(Color, Color)]
findEdges = concatMap (\s -> zip (repeat $ color s)
                                 (buildOutEdges $ contents s))
  where buildOutEdges :: [(Color,Int)] -> [Color]
        buildOutEdges = concatMap (\(c, i) -> replicate i c)

--edgeToInt :: [(Color, Color)] -> [(Int, Int)]
--edgeToInt = fmap (\(c1, c2) -> in

allColors :: [BagSpec] -> [Color]
allColors = nub . sort . fmap color

parse :: String -> [BagSpec]
parse s = parse' <$> lines s

parse' :: String -> BagSpec
parse' s =
  let color = unwords . take 2 . words $ s
      contents = inner [] $ drop 4 $ words s
  in BagSpec { color = color, contents = contents }
  where inner :: [(Color, Int)] -> [String] -> [(Color, Int)]
        inner acc [] = acc
        inner acc words
          | head words == "no" = []
          | otherwise = inner ((unwords (drop 1 . take 3 $ words),
                                read $ head words) : acc)
                              (drop 4 words)
buildGraph :: String -> (Graph, Color -> Maybe Int)
buildGraph content =
  let specs = parse content
      es = findEdges specs
      cs = allColors specs
      toI = colorToInt cs
      outEdges = (\c -> (c, snd <$> filter ((== c) . fst) es)) <$> cs
      (graph,_,_) = graphFromEdges
                    $ fmap (\(c, edges) -> (c, toI c, toI <$> edges)) outEdges
  in (transposeG graph, toI)

type M = Map.Map Color [(Color, Int)]
buildMap :: [BagSpec] -> M
buildMap = 
  foldl f Map.empty . sortOn color
  where f :: (M -> BagSpec -> M)
        f m bs = Map.insert (color bs) (contents bs) m

countBags :: M -> (Color, Int) -> Int
countBags m (c,i) =
  i * (1 + sum (countBags m <$> children))
  where (Just children) = Map.lookup c m

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day07.txt"

  let (graph, toI) = buildGraph content
  print $ ((\i -> i - 1) <$> length)
        . reachable graph <$> toI "shiny gold"


part2 :: IO ()
part2 = do
  content <- readFile "inputs/day07.txt"
  let specs = parse content
  let m = buildMap specs
  print $ countBags m ("shiny gold", 1) - 1
