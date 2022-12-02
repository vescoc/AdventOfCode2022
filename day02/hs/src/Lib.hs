module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

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

solve :: ((Char, Char) -> Int) -> [(Char, Char)] -> Int
solve f = sum . map f 

solve1 :: [(Char, Char)] -> Int
solve1 = solve play1

solve2 :: [(Char, Char)] -> Int
solve2 = solve play2

parse :: String -> [(Char, Char)]
parse = fmap parseL . lines
  where parseL [a, ' ', b] = (a, b)
        parseL _ = error "invalid line"

play1 :: (Char, Char) -> Int
play1 ('A', 'X') = 4
play1 ('A', 'Y') = 8
play1 ('A', 'Z') = 3
play1 ('B', 'X') = 1
play1 ('B', 'Y') = 5
play1 ('B', 'Z') = 9
play1 ('C', 'X') = 7
play1 ('C', 'Y') = 2
play1 ('C', 'Z') = 6
play1 _ = error "invalid pair"

play2 :: (Char, Char) -> Int
play2 ('A', 'X') = 3
play2 ('A', 'Y') = 4
play2 ('A', 'Z') = 8
play2 ('B', 'X') = 1
play2 ('B', 'Y') = 5
play2 ('B', 'Z') = 9
play2 ('C', 'X') = 2
play2 ('C', 'Y') = 6
play2 ('C', 'Z') = 7
play2 _ = error "invalid pair"
