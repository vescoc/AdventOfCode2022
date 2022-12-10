module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe
import Data.List (foldl', intercalate)

data Instruction = Noop | Addx Int
  deriving Show

type Input = [Instruction]

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- parse <$> input
  let p1 = solve1 i
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i
  putStrLn $ "part 2: " ++ p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

parse :: String -> Input
parse = fmap (parseL . words) . lines
  where parseL ["noop"] = Noop
        parseL ["addx", x] = Addx $ read x
        parseL _ = error "invalid instruction"

solve1 :: Input -> Int
solve1 = sum . mapMaybe f . zip [1..] . foldl' (\b a -> b ++ eval a (last b)) [1]
  where f (i, v)
          | i `elem` [20, 60, 100, 140, 180, 220] = Just $ i * v
          | otherwise = Nothing

solve2 :: Input -> String
solve2 = intercalate "\n" . chunks 40 . zipWith f [0..] . take (40 * 6) . foldl' (\b a -> b ++ eval a (last b)) [1]
  where f i v
          | i' >= v - 1 && i' <= v + 1 = '#'
          | otherwise = '.'
          where i' = i `mod` 40
  
eval :: Instruction -> Int -> [Int]
eval Noop v = [v]
eval (Addx x) v = [v, v + x]

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunks n (drop n xs)
