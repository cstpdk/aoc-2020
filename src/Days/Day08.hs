module Days.Day08 where

import qualified Data.Map.Strict as Map

data Inst = Nop Int | Acc Int | Jmp Int
data Result = Looped Int | Terminated Int
  deriving Show

type LineNumber = Int

type Input = [Inst]

parse :: String -> Input
parse s = inner . words <$> lines s
  where inner :: [String] -> Inst
        inner ("nop" : n : _) = Nop (r n)
        inner ("acc" : n : _) = Acc (r n)
        inner ("jmp" : n : _) = Jmp (r n)
        r = read . filter (/= '+')

eval :: Int -> LineNumber -> Input -> Map.Map Int Bool -> Result
eval acc line inp m =
  if line >= length inp then Terminated acc
  else
  case Map.lookup line m of
    Just _ -> Looped acc
    Nothing ->  let (acc', line') = eval' acc line (inp !! line)
                in eval acc' line' inp (Map.insert line True m)

eval' :: Int -> LineNumber -> Inst -> (Int, LineNumber)
eval' acc line (Nop _) = (acc, line + 1)
eval' acc line (Acc i) = (acc + i, line + 1)
eval' acc line (Jmp i) = (acc, line + i)

-- This pretty naively done, we run also for all acc instructions for one
-- but it terminates, and it's christmas, so why not keep it
evalWithCorrection :: LineNumber -> Input -> Int
evalWithCorrection line inp = 
  case eval 0 0 flipped Map.empty of 
    Looped _ -> evalWithCorrection (line + 1) inp
    Terminated i -> i
  where flipped = case splitAt line inp of
          ([],ys) -> ys
          (xs, ys) -> case last xs of
                        Nop i -> init xs ++ [Jmp i] ++ ys
                        Acc i -> init xs ++ [Acc i] ++ ys
                        Jmp i -> init xs ++ [Nop i] ++ ys
                    

part1 :: IO ()
part1 = do
  content <- readFile "inputs/day08.txt"
  let input = parse content
  print $ eval 0 0 input Map.empty

part2 :: IO ()
part2 = do
  content <- readFile "inputs/day08.txt"
  let input = parse content
  print $ evalWithCorrection 0 input
