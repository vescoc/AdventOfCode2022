module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Array.IArray
import Data.Char
import Control.Monad

type Input = Array (Int, Int) Int

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

parse :: String -> Input
parse = array' . (parseR =<<) . zip [0..] . lines
  where parseR :: (Int, String) -> [((Int, Int), Int)]
        parseR (y, row) = [((x, y), v) | (x, c) <- zip [0..] row, let v = ord c]
        array' xs = let mx = maximum $ map (fst . fst) xs
                        my = maximum $ map (snd . fst) xs
                    in array ((0, 0), (mx, my)) xs

solve1 :: Input -> Int
solve1 arr = length $ do
  (x, y) <- indices arr
  let v = arr!(x, y)
  guard (all (v >) (arr `top` (x, y)) || all (v >) (arr `bottom` (x, y)) || all (v >) (arr `left` (x, y)) || all (v >) (arr `right` (x, y)))
  return (x, y)

solve2 :: Input -> Int
solve2 arr = maximum $ do
  (x, y) <- indices arr
  let v = arr!(x, y)
  let t = length $ break' v $ top arr (x, y)
  let b = length $ break' v $ bottom arr (x, y)
  let l = length $ break' v $ left arr (x, y)
  let r = length $ break' v $ right arr (x, y)
  return (t * b * l * r)

top :: Input -> (Int, Int) -> [Int]
top arr (x, y)
  | y == miny = []
  | otherwise = [arr!(x, y') | y' <- reverse [miny..y - 1]]
  where ((_, miny), _) = bounds arr

bottom :: Input -> (Int, Int) -> [Int]
bottom arr (x, y)
  | y == maxy = []
  | otherwise = [arr!(x, y') | y' <- [y + 1..maxy]]
  where (_, (_, maxy)) = bounds arr

left :: Input -> (Int, Int) -> [Int]
left arr (x, y)
  | x == minx = []
  | otherwise = [arr!(x', y) | x' <- reverse [minx..x - 1]]
  where ((minx, _), _) = bounds arr

right :: Input -> (Int, Int) -> [Int]
right arr (x, y)
  | x == maxx = []
  | otherwise = [arr!(x', y) | x' <- [x + 1..maxx]]
  where (_, (maxx, _)) = bounds arr

break' :: Ord a => a -> [a] -> [a]
break' _ [] = []
break' v (x:xs)
  | v > x = x : break' v xs
  | otherwise = [x]
