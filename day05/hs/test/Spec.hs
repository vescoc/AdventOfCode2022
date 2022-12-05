import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib (solve1, solve2, Data)
import qualified Lib

main :: IO ()
main = do
  input <- Lib.parse <$> Lib.input
  hspecWith defaultConfig { configFastFail = False } $ specs input

specs :: Data -> Spec
specs input = do
  describe "day05" $ do
    it "solve1" $ do
      solve1 input `shouldBe` "SHMSDGZVC"
    it "solve2" $ do
      solve2 input `shouldBe` "VRZGHDFBQ"
