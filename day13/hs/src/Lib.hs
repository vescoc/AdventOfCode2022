{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Vector (toList)
import Data.List (sort)

data List = I Int | L [List]
  deriving (Show, Eq)

type Input = [List]

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
parse = mapMaybe (parseL . B.pack) . lines
  where parseL = decode'

solve1 :: Input -> Int
solve1 = sum . map fst . filter ((/= GT) . snd) . zip [1..] . map (uncurry compare) . chunks
  where chunks [] = []
        chunks (a:b:xs) = (a, b) : chunks xs
        chunks _ = error "invalid chunk"

solve2 :: Input -> Int
solve2 = product . map fst . filter ((`elem` [k2, k6]) . snd) . zip [1..] . sort . (k2 :) . (k6 :)
  where k2 = L [L [I 2]]
        k6 = L [L [I 6]]

instance FromJSON List where
  parseJSON (Array a) = L <$> mapM parseJSON (toList a)
  parseJSON n = I <$> parseJSON n

instance Ord List where
  (I a) `compare` (I b) = a `compare` b
  a@(I _) `compare` b@(L _) = L [a] `compare` b
  a@(L _) `compare` b@(I _) = a `compare` L [b]
  (L a) `compare` (L b) = mconcat (zipWith compare a b) <> (length a `compare` length b)
