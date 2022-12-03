module Lib
    ( someFunc, solve1, solve2
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char
import Data.List (intersect)

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
solve1 = sum . fmap (convert . eval)
  where eval l = let hl = length l `div` 2
                 in take hl l `intersect` drop hl l

solve2 :: [String] -> Int
solve2 = sum . fmap (convert . eval) . chunks3
  where eval = foldr1 intersect

parse :: String -> [String]
parse = lines

convert :: String -> Int
convert (x:_)
  | isAsciiLower x = ord x - ord 'a' + 1
  | otherwise = ord x - ord 'A' + 27
convert _ = error "invalid"

chunks3 :: [a] -> [[a]]
chunks3 (x:y:z:xs) = [x, y, z]:chunks3 xs
chunks3 [] = []
chunks3 xs = [xs]
