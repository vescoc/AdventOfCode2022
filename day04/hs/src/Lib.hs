module Lib
    ( someFunc, solve1, solve2, parse, input, Data
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Bifunctor

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

type Data = [((Int, Int), (Int, Int))]

input :: IO String
input = readFile "../input"

parse :: String -> Data
parse = fmap parseL . lines
  where parseL = bimap parseS (parseS . drop 1) . break (== ',')
        parseS = bimap read (read . drop 1) . break (== '-')

solve :: (((Int, Int), (Int, Int)) -> Bool) -> Data -> Int
solve test = length . filter test

solve1 :: Data -> Int
solve1 = solve contains'
  where contains' (a, b) = a `contains` b || b `contains` a

solve2 :: Data -> Int
solve2 = solve overlap'
  where overlap' (a, b) = a `overlap` b || b `overlap` a

contains :: Ord a => (a, a) -> (a, a) -> Bool
contains (al, ah) (bl, bh) = bl >= al && bl <= ah && bh >= al && bh <= ah

overlap :: Ord a => (a, a) -> (a, a) -> Bool
overlap  (al, ah) (bl, bh) = bl >= al && bl <= ah || bh >= al && bh <= ah
