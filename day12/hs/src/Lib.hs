{-# LANGUAGE TupleSections #-}

module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Char
import Data.Array.IArray
import Data.List (unzip4)
import Data.Maybe
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

type Coord = (Int, Int)
type Input = (Array Coord Int, Coord, Coord, [Coord])

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
parse = makeInput . unzip4 . (parseRow =<<) . zip [0..] . lines
  where parseRow (r, row) = [(((c, r), b'), s, e, a) | (c, b) <- zip [0..] row, let (b', s, e, a) = cell (c, r) b]
        cell p 'a' = (ord 'a', Nothing, Nothing, Just p)
        cell p 'S' = (ord 'a', Just p, Nothing, Just p)
        cell p 'E' = (ord 'z', Nothing, Just p, Nothing)
        cell _ c = (ord c, Nothing, Nothing, Nothing)
        makeInput :: ([(Coord, Int)], [Maybe Coord], [Maybe Coord], [Maybe Coord]) -> Input
        makeInput (cells, starts, ends, as) = (array ((0, 0), (maximum $ map (fst . fst) cells, maximum $ map (snd . fst) cells)) cells,
                                               head $ catMaybes starts,
                                               head $ catMaybes ends,
                                               catMaybes as)

solve1 :: Input -> Int
solve1 (area, start, end, _) = bfs (Seq.singleton (start, 0)) (Set.singleton start)
  where bfs Seq.Empty _ = error "not found"
        bfs ((current@(x, y), steps) :<| queue) visited = if current == end
                                                          then steps
                                                          else bfs (queue >< Seq.fromList (map (,steps + 1) neighbors)) (visited `Set.union` Set.fromList neighbors)
          where neighbors = calcNeighbors ((<= 1) . subtract (area!(x, y))) area (x, y) visited
                                          
solve2 :: Input -> Int
solve2 (area, end, start, as) = bfs (Seq.singleton (start, 0)) (Set.singleton start)
  where 
        ends = end:as
        bfs Seq.Empty _ = error "not found"
        bfs ((current@(x, y), steps) :<| queue) visited = if current `elem` ends
                                                          then steps
                                                          else bfs (queue >< Seq.fromList (map (,steps + 1) neighbors)) (visited `Set.union` Set.fromList neighbors)
          where neighbors = calcNeighbors ((<= 1) . (area!(x, y) -)) area (x, y) visited

calcNeighbors :: (Int -> Bool) -> Array Coord Int -> Coord -> Set Coord -> [Coord]
calcNeighbors test area (x, y) visited = let ((minx, miny), (maxx, maxy)) = bounds area
                                         in do
  (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
  let (x', y') = (x + dx, y + dy)
  guard (x' `elem` [minx..maxx] && y' `elem` [miny..maxy])
  guard ((x', y') `Set.notMember` visited)
  guard (test $ area!(x', y'))
  return (x', y')
