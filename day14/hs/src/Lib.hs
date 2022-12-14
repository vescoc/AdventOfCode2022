module Lib
    ( someFunc, solve1, solve2, input, parse, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Array.IArray
import Data.List.Split
import Data.Bifunctor
import Data.List (foldl')
import Control.Monad

type Input = Array (Int, Int) Bool

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
parse = draw . second (array' . minMax . join) . dupe . fmap parseL . lines
  where parseL = fmap (bimap read (read . drop 1) . break (== ',')) . splitOn " -> "
        minMax l = let (x, y) = unzip l
                   in ((minimum x, 0), (maximum x, maximum y))
        array' :: ((Int, Int), (Int, Int)) -> Input
        array' b@((minx, miny), (maxx, maxy)) = array b [((x, y), False) | x <- [minx..maxx], y <- [miny..maxy]]
        draw :: ([[(Int, Int)]], Input) -> Input
        draw (lss, display) = foldl' drawLines display lss
        drawLines display ls = foldl' drawLine display $ windows ls
        drawLine :: Input -> ((Int, Int), (Int, Int)) -> Input
        drawLine display (a, b)
          | x1' == x2' = display // [((x1', y), True) | y <- [y1'..y2']]
          | y1' == y2' = display // [((x, y1'), True) | x <- [x1'..x2']]
          | otherwise = error "invalid line"
          where ((x1', y1'), (x2', y2')) = (minimum [a, b], maximum [a, b])

solve1 :: Input -> Int
solve1 = error "todo!"

solve2 :: Input -> Int
solve2 = error "todo!"

dupe :: a -> (a, a)
dupe x = (x, x)

windows :: [a] -> [(a, a)]
windows (x:y:xs) = (x, y) : windows (y:xs)
windows _ = []
