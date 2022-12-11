module Lib
    ( someFunc, solve1, solve2, parse, input, Input
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Bifunctor
import Data.Array.IArray
import Data.List (partition, sortBy)

data Operation = Add Int | Multiply Int | Square
  deriving Show

type MonkeyState = ([Int], Int)
type MonkeyInfo = (Operation, Int, Int, Int)
type Input = (Array Int MonkeyInfo, Array Int MonkeyState)

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
parse = bimap array' array' . unzip . parse' . lines
  where parse' [] = []
        parse' xs = (uncurry (:) . bimap (parseM . fmap words) (parse' . drop 1) . break (== "")) xs
        array' xs = array (0, length xs - 1) $ zip [0..] xs
        parseM [["Monkey", _],
                "Starting":"items:":items,
                ["Operation:", "new", "=", "old", op, value],
                ["Test:", "divisible", "by", divisor],
                ["If", "true:", "throw", "to", "monkey", tt],
                ["If", "false:", "throw", "to", "monkey", ft]] = ((readOp op value, read divisor, read tt, read ft), (readItems items, 0))
        parseM _ = error "invalid monkey"
        readItems = map (read . takeWhile (/= ','))
        readOp "+" value = Add $ read value
        readOp "*" "old" = Square
        readOp "*" value = Multiply $ read value
        readOp _ _ = error "invalid operation"

solve :: Int -> (Int -> Int) -> Input -> Int
solve n d (info, state) = product $ take 2 $ sortBy (flip compare) $ map snd $ elems $ (!!n) $ iterate (round' d info) state

solve1 :: Input -> Int
solve1 = solve 20 (`div` 3)

solve2 :: Input -> Int
solve2 (info, state) = let d = product $ map (\(_, t, _, _) -> t) $ elems info
                       in solve 10000 (`mod` d) (info, state)

round' :: (Int -> Int) -> Array Int MonkeyInfo -> Array Int MonkeyState -> Array Int MonkeyState
round' d info = step (assocs info)
  where step :: [(Int, MonkeyInfo)] -> Array Int MonkeyState -> Array Int MonkeyState
        step [] state = state
        step ((i, (op, test, tt, ft)):xs) state = step xs state'
          where (items, c) = state!i
                (ts, fs) = partition ((== 0) . (`mod` test)) $ map (d . eval op) items
                state' = state // [(i, ([], c + length items)), (tt, append tt ts), (ft, append ft fs)]
                append t ss = let (ss', c') = state!t
                              in (ss' ++ ss, c')

eval :: Operation -> Int -> Int
eval (Add value) item = value + item
eval (Multiply value) item = value * item
eval Square item = item * item
