import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2, Input)
import qualified Lib

main :: IO ()
main = do
  input <- Lib.parse <$> Lib.input
  hspecWith defaultConfig { configFastFail = False } $ specs input

specs :: Input -> Spec
specs input = do
  describe "day13" $ do
    it "solve1" $ do
      solve1 input `shouldBe` 5623
    it "solve2" $ do
      solve2 input `shouldBe` 20570
