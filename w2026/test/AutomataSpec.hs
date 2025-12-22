module AutomataSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Automata

spec :: Spec
spec = do
  describe "int2bin and bin2int" $ do
    
    it "converts 0 correctly" $ do
      int2bin 0 `shouldBe` "0"
      bin2int "0" `shouldBe` 0
    
    it "converts some specific values correctly" $ do
      int2bin 5 `shouldBe` "101"
      bin2int "101" `shouldBe` 5
      int2bin 255 `shouldBe` "11111111"
      bin2int "11111111" `shouldBe` 255
    
    it "round-trips from Int to binary and back" $ 
      property $ \(NonNegative n) -> 
        bin2int (int2bin n) == n
    
    it "round-trips from binary string and back (normalized)" $
      property $ \(NonNegative n) -> n > 0 ==>
        let binStr = int2bin n
        in int2bin (bin2int binStr) == binStr
