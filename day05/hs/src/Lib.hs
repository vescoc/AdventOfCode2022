module Lib
    ( someFunc, solve1, solve2, parse, input, Data
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Bifunctor
import Data.Array.IArray
import Data.List (transpose, foldl')
import Data.List.Extra (breakOn)
import Data.Maybe

type Move = (Int, Int, Int)
type Data = (Array Int String, [Move])

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

parse :: String -> Data
parse = bimap parseS (fmap (parseM . words) . lines . drop 2) . breakOn "\n\n"
  where parseM ["move", m, "from", f, "to", t] = (read m, read f, read t)
        parseM _ = error "invalid move"
        parseS = array' . fmap (dropWhile (== ' ') . (!!1)) . chunks4 . transpose . init . lines
        array' l = array (1, length l) $ zip [1..] l
  
solve :: (Array Int String -> Move -> Array Int String) -> Data -> String
solve move = mapMaybe listToMaybe . elems . uncurry (foldl' move)

solve1 :: Data -> String
solve1 = solve move
  where move :: Array Int String -> Move -> Array Int String
        move arr (q, f, t) = arr // [(f, drop q $ arr!f), (t, reverse (take q $ arr!f) ++ arr!t)]

solve2 :: Data -> String
solve2 = solve move
  where move :: Array Int String -> Move -> Array Int String
        move arr (q, f, t) = arr // [(f, drop q $ arr!f), (t, take q (arr!f) ++ arr!t)]

chunks4 :: [a] -> [[a]]
chunks4 (a:b:c:d:xs) = [a, b, c, d] : chunks4 xs
chunks4 [] = []
chunks4 xs = [xs]
