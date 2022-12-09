module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (nub)

data Move = U Int | D Int | R Int | L Int
  deriving Show

type Input = [Move]

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
parse = fmap (parseL . words) . lines
  where parseL ["U", m] = U $ read m
        parseL ["D", m] = D $ read m
        parseL ["L", m] = L $ read m
        parseL ["R", m] = R $ read m
        parseL _ = error "invalid direction"

solve :: [(Int, Int)] -> Input -> Int
solve r = length . nub . fmap last . (r:) . moveRope r . (toSteps =<<)
  where moveRope _ [] = []
        moveRope r' (m:ms) = let r'' = moveHead r' m
                             in r'' : moveRope r'' ms
        moveHead ((x, y):rs) (dx, dy) = adjustRope ((x + dx, y + dy) : rs)
        moveHead _ _ = error "invalid rope"
        adjustRope (a:b:rs) = a : adjustRope (follow a b : rs)
        adjustRope rs = rs
        toSteps m = let (d, ss) = case m of
                                    (U s) -> ((0, -1), s)
                                    (D s) -> ((0, 1), s)
                                    (L s) -> ((-1, 0), s)
                                    (R s) -> ((1, 0), s)
                    in replicate ss d
        
solve1 :: Input -> Int
solve1 = solve $ replicate 2 (0, 0)

solve2 :: Input -> Int
solve2 = solve $ replicate 10 (0, 0)

follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty) = let (dx, dy) = (hx - tx, hy - ty)
                           in case (abs dx, abs dy) of
                                (a, 0) | a <= 1 -> (tx, ty)
                                (_, 0) -> (tx + signum dx, ty)
                                (0, a) | a <= 1 -> (tx, ty)
                                (0, _) -> (tx, ty + signum dy)
                                (a, b) | a + b <= 2 -> (tx, ty)
                                _ -> (tx + signum dx, ty + signum dy)
