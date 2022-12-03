module Lib
    ( someFunc, solve1, solve2, parse, input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (sortBy)

someFunc :: IO ()
someFunc = do
  start <- getCurrentTime
  i <- parse <$> input
  let p1 = solve1 i
  putStrLn $ "part 1: " ++ show p1
  let p2 = solve2 i
  putStrLn $ "part 2: " ++ show p2
  end <- getCurrentTime
  let delta = diffUTCTime end start
  putStrLn $ "delta: " ++ show delta

input :: IO String
input = readFile "../input"

solve1 :: [String] -> Int
solve1 = maximum . fmap (sum . fmap read) . split ""

solve2 :: [String] -> Int
solve2 = sum . take 3 . sortBy (flip compare) . fmap (sum . fmap read) . split ""

parse :: String -> [String]
parse = lines

split :: Eq a => a -> [a] -> [[a]]
split v = split' []
  where split' l [] = [l]
        split' l (x:xs)
          | v == x = l:split' [] xs
          | otherwise = split' (l ++ [x]) xs
