module Lib
    ( someFunc, solve1, solve2, parse, input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe
import Data.List (find, nub)

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

parse :: String -> String
parse = id

solve :: Int -> String -> Int
solve l = fst . fromMaybe (error "not found") . find test . zip [l..] . windows l
  where test = (== l) . length . nub . snd

solve1 :: String -> Int
solve1 = solve 4

solve2 :: String -> Int
solve2 = solve 14

windows :: Int -> [a] -> [[a]]
windows l [] = []
windows l xs = take l xs : windows l (drop 1 xs)
