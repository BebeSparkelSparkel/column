module LibSpec where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "split" do
    it "correct number of groups" $ split ' ' "ab cd efg" `shouldBe` ["ab", "cd", "efg"]
    it "multiple seperator chars" $ split ' ' "ab    cd  efg" `shouldBe` ["ab", "cd", "efg"]
    it "trailing seperators" $ split ' ' "ab cd     " `shouldBe` ["ab", "cd"]

