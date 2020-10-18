module LibSpec where

import Lib
import Test.Hspec
import Control.Monad.State


spec :: Spec
spec = do
  describe "splitCount" do
    let split :: Eq a => a -> [a] -> ([[a]],[Int])
        split s cs = runState (splitCount s cs) []

    it "correct number of groups" $
      split ' ' "ab cd efg" `shouldBe` (["ab","cd","efg"],[3,3,4])

    it "multiple seperator chars" $
      split ' ' "ab    cd  efg" `shouldBe` (["ab","cd","efg"],[3,3,4])

    it "trailing seperators" $
      split ' ' "ab cd     " `shouldBe` (["ab","cd"],[3,3])

    describe "chooses max" do
      describe "first" do
        it "increases" $
          runState (splitCount ' ' "abc") [1] `shouldBe` (["abc"],[4])

        it "holds" $
          runState (splitCount ' ' "abc") [5] `shouldBe` (["abc"],[5])

      it "mid values" $
        runState (splitCount ' ' "a bc def ghij klmno") [2,2,2,2,2] `shouldBe` (["a","bc","def","ghij","klmno"],[2,3,4,5,6])

    it "keeps trailing counts" $
      runState (splitCount ' ' "abc") [1,2,3,4,5] `shouldBe` (["abc"],[4,2,3,4,5])

  describe "rightPad" do
    it "pads only internal" $
      rightPad ' ' [2, 3, 4, 5] ["a","b","cd","ef"] `shouldBe` "a b  cd  ef"

