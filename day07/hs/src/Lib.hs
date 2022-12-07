module Lib
    ( someFunc, solve1, solve2, parse, input, Data
    ) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M
import Control.Monad

data Node = Dir String | File String Int

type Data = M.Map [String] Int

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

solve1 :: Data -> Int
solve1 = sum . filter (<= 100000) . M.elems

solve2 :: Data -> Int
solve2 m = let usedSpace = m M.! []
               unusedSpace = 70000000 - usedSpace
               neededSpace = 30000000 - unusedSpace
           in minimum $ filter (>= neededSpace) $ M.elems m

parse :: String -> Data
parse = visit . fst . foldM parseL [] . lines
  where parseL cwd line = parseL' $ words line
          where parseL' ["$", "cd", ".."] = ([], init cwd)
                parseL' ["$", "cd", dir] = ([], cwd ++ [dir])
                parseL' ["$", "ls"] = ([], cwd)
                parseL' ["dir", dir] = ([(cwd, Dir dir)], cwd)
                parseL' [size, file] = ([(cwd, File file $ read size)], cwd)
                parseL' _ = error "invalid line"
        visit = foldr add M.empty
          where add (_, Dir _) m = m
                add (p, File _ s) m = add' p s m
                add' [] s = M.insertWith (+) [] s
                add' p s = add' (init p) s . M.insertWith (+) p s
