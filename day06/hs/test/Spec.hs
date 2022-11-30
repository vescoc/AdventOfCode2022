import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2)

main :: IO ()
main = hspecWith defaultConfig { configFastFail = False } specs

specs :: Spec
specs = do
  describe "day06" $ do
    it "solve1" $ do
      solve1 input `shouldBe` 666
    it "solve2" $ do
      solve2 input `shouldBe` 666

input :: String
input = error "todo!"
